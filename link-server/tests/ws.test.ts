import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import type WebSocket from "ws";
import {
  authenticateAgent,
  collectMessages,
  createSignedLink,
  createTestAgent,
  getJson,
  openWs,
  postJson,
  startTestServer,
  waitFor,
  waitForOpen,
  type TestServerHandle,
} from "./helpers.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

function closeAll(...sockets: WebSocket[]): void {
  for (const s of sockets) {
    try {
      s.close();
    } catch {
      // best-effort
    }
  }
}

test("ws upgrade is rejected without a valid token", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const socket = openWs(server.wsUrl, roomId, "not-a-real-token");
    const statusCode = await new Promise<number>((resolve, reject) => {
      socket.once("unexpected-response", (_req, res) => resolve(res.statusCode));
      socket.once("open", () => reject(new Error("socket should not have opened")));
      socket.once("error", () => {
        /* some ws versions also raise error after unexpected-response */
      });
    });
    assert.equal(statusCode, 401);
  });
});

test("on connect, agent receives an online-agents snapshot; a second connect triggers peer-joined", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    // Attach the collector before awaiting "open": the server sends
    // online-agents synchronously as part of completing the upgrade, and it
    // can otherwise arrive before a listener attached after "open" resolves.
    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const collectorA = collectMessages(wsA);
    await waitForOpen(wsA);
    await collectorA.waitForType("online-agents");

    const wsB = openWs(server.wsUrl, roomId, tokenB);
    await waitForOpen(wsB);

    const joinedMsg = await collectorA.waitForType("peer-joined");
    assert.equal(joinedMsg.did, agentB.did);

    closeAll(wsA, wsB);
  });
});

test("committing a diff over HTTP broadcasts it to other WS-connected agents, excluding the committer", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    await Promise.all([waitForOpen(wsA), waitForOpen(wsB)]);
    const collectorA = collectMessages(wsA);
    const collectorB = collectMessages(wsB);

    const link = await createSignedLink(agentA, { source: "a", predicate: "rel", target: "b" });
    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      tokenA
    );
    assert.equal(commitRes.status, 200);

    const diffMsg = await collectorB.waitForType("diff");
    assert.equal(diffMsg.sequence, commitRes.body.sequence);
    assert.equal(diffMsg.revision, commitRes.body.revision);
    assert.equal((diffMsg.payload as { additions: unknown[] }).additions.length, 1);

    assert.ok(
      !collectorA.messages.some((m) => m.type === "diff"),
      "the committer's own socket should not receive its own diff"
    );

    closeAll(wsA, wsB);
  });
});

test("telepresence-signal routes only to the target DID's sockets", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const agentC = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentC.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);
    const tokenC = await authenticateAgent(server.url, roomId, agentC);

    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    const wsC = openWs(server.wsUrl, roomId, tokenC);
    await Promise.all([waitForOpen(wsA), waitForOpen(wsB), waitForOpen(wsC)]);
    const collectorB = collectMessages(wsB);
    const collectorC = collectMessages(wsC);

    wsA.send(JSON.stringify({ type: "telepresence-signal", toDid: agentB.did, payload: { sdp: "offer" } }));

    const signalMsg = await collectorB.waitForType("telepresence-signal");
    assert.equal(signalMsg.fromDid, agentA.did);
    assert.deepEqual(signalMsg.payload, { sdp: "offer" });

    assert.ok(!collectorC.messages.some((m) => m.type === "telepresence-signal"));

    closeAll(wsA, wsB, wsC);
  });
});

test("telepresence-broadcast fans out to all other connected agents", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const agentC = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentC.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);
    const tokenC = await authenticateAgent(server.url, roomId, agentC);

    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    const wsC = openWs(server.wsUrl, roomId, tokenC);
    await Promise.all([waitForOpen(wsA), waitForOpen(wsB), waitForOpen(wsC)]);
    const collectorB = collectMessages(wsB);
    const collectorC = collectMessages(wsC);

    wsA.send(JSON.stringify({ type: "telepresence-broadcast", payload: { cursor: [1, 2] } }));

    const msgB = await collectorB.waitForType("telepresence-broadcast");
    const msgC = await collectorC.waitForType("telepresence-broadcast");
    assert.equal(msgB.fromDid, agentA.did);
    assert.equal(msgC.fromDid, agentA.did);

    closeAll(wsA, wsB, wsC);
  });
});

test("set-online-status updates status and is visible via HTTP peers + pushed online-agents", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    await Promise.all([waitForOpen(wsA), waitForOpen(wsB)]);
    const collectorB = collectMessages(wsB);

    wsA.send(JSON.stringify({ type: "set-online-status", status: { mood: "focused" } }));

    await waitFor(() =>
      collectorB.messages.some(
        (m) =>
          m.type === "online-agents" &&
          (m.agents as { did: string; status?: unknown }[]).some(
            (a) => a.did === agentA.did && (a.status as { mood?: string })?.mood === "focused"
          )
      )
    );

    const peersRes = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(peersRes.body.peers.includes(agentA.did));
    assert.ok(peersRes.body.peers.includes(agentB.did));

    closeAll(wsA, wsB);
  });
});

test("disconnect marks an agent offline (peer-left) only after the grace period elapses", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    await Promise.all([waitForOpen(wsA), waitForOpen(wsB)]);
    const collectorB = collectMessages(wsB);

    wsA.close();

    // Immediately after close, agent A should still be considered online
    // (grace period from startTestServer is 300ms).
    const immediate = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(immediate.body.peers.includes(agentA.did));

    const leftMsg = await collectorB.waitForType("peer-left", 3000);
    assert.equal(leftMsg.did, agentA.did);

    const after = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(!after.body.peers.includes(agentA.did));

    closeAll(wsB);
  });
});

test("reconnecting within the grace period does not trigger peer-left", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA1 = openWs(server.wsUrl, roomId, tokenA);
    const wsB = openWs(server.wsUrl, roomId, tokenB);
    await Promise.all([waitForOpen(wsA1), waitForOpen(wsB)]);
    const collectorB = collectMessages(wsB);

    wsA1.close();
    // Reconnect well within the 300ms grace period.
    await new Promise((resolve) => setTimeout(resolve, 50));
    const wsA2 = openWs(server.wsUrl, roomId, tokenA);
    await waitForOpen(wsA2);

    // Give the grace timer time to have fired if it were going to.
    await new Promise((resolve) => setTimeout(resolve, 500));
    assert.ok(!collectorB.messages.some((m) => m.type === "peer-left"));

    const peersRes = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(peersRes.body.peers.includes(agentA.did));

    closeAll(wsA2, wsB);
  });
});
