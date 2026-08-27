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
  openAuthenticatedWs,
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

test("ws auth: sending an invalid token as the first message closes the socket with auth-error", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    // Create the room first so the roomId exists.
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const socket = openWs(server.wsUrl, roomId);
    await waitForOpen(socket);
    const collector = collectMessages(socket);
    socket.send(JSON.stringify({ type: "auth", token: "not-a-real-token" }));

    const closeCode = await new Promise<number>((resolve) => {
      socket.on("close", (code) => resolve(code));
    });
    assert.equal(closeCode, 4004);
    const authErr = collector.messages.find((m) => m.type === "auth-error");
    assert.ok(authErr, "should receive an auth-error message");
  });
});

test("ws auth: no auth message within timeout closes the socket", async () => {
  const server = await startTestServer({ wsOptions: { authTimeoutMs: 200 } });
  try {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const socket = openWs(server.wsUrl, roomId);
    await waitForOpen(socket);
    const collector = collectMessages(socket);

    const closeCode = await new Promise<number>((resolve) => {
      socket.on("close", (code) => resolve(code));
    });
    assert.equal(closeCode, 4001);
    const authErr = collector.messages.find((m) => m.type === "auth-error");
    assert.ok(authErr, "should receive auth-error with timeout reason");
  } finally {
    await server.close();
  }
});

test("on connect, agent receives an online-agents snapshot; a second connect triggers peer-joined", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const collectorA = collectMessages(wsA);

    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);

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

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
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

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const wsC = await openAuthenticatedWs(server.wsUrl, roomId, tokenC);
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

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const wsC = await openAuthenticatedWs(server.wsUrl, roomId, tokenC);
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

test("set-online-status broadcasts a status-changed delta (not the full roster)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collectorB = collectMessages(wsB);

    wsA.send(JSON.stringify({ type: "set-online-status", status: { mood: "focused" } }));

    const statusMsg = await collectorB.waitForType("status-changed");
    assert.equal(statusMsg.did, agentA.did);
    assert.deepEqual(statusMsg.status, { mood: "focused" });

    const peersRes = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(peersRes.body.peers.includes(agentA.did));
    assert.ok(peersRes.body.peers.includes(agentB.did));

    closeAll(wsA, wsB);
  });
});

test("disconnect marks an agent offline (peer-left) only after the grace period elapses", async () => {
  // Use a longer grace period (1000ms) so the HTTP round trip for the
  // "still online immediately after close" assertion doesn't race the
  // timer on loaded CI runners (default 300ms can be tight).
  const server = await startTestServer({ telepresenceGraceMs: 1000 });
  try {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collectorB = collectMessages(wsB);

    wsA.close();

    // Immediately after close, agent A should still be considered online
    // (the 1000ms grace period gives this round trip plenty of room).
    const immediate = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(immediate.body.peers.includes(agentA.did));

    const leftMsg = await collectorB.waitForType("peer-left", 3000);
    assert.equal(leftMsg.did, agentA.did);

    const after = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(!after.body.peers.includes(agentA.did));

    closeAll(wsB);
  } finally {
    await server.close();
  }
});

test("reconnecting within the grace period does not trigger peer-left", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA1 = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collectorB = collectMessages(wsB);

    wsA1.close();
    // Reconnect well within the 300ms grace period.
    await new Promise((resolve) => setTimeout(resolve, 50));
    const wsA2 = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);

    // Give the grace timer time to have fired if it were going to.
    await new Promise((resolve) => setTimeout(resolve, 500));
    assert.ok(!collectorB.messages.some((m) => m.type === "peer-left"));

    const peersRes = await getJson<{ peers: string[] }>(`${server.url}/rooms/${roomId}/peers`, tokenB);
    assert.ok(peersRes.body.peers.includes(agentA.did));

    closeAll(wsA2, wsB);
  });
});

test("ws rate limiting: excess messages are silently dropped", async () => {
  const server = await startTestServer({
    wsOptions: { wsMessageLimit: 3, wsMessageWindowMs: 5000 },
  });
  try {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: agentB.did }, tokenA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collectorB = collectMessages(wsB);

    // Send 6 broadcasts — only 3 should get through.
    for (let i = 0; i < 6; i++) {
      wsA.send(JSON.stringify({ type: "telepresence-broadcast", payload: { n: i } }));
    }

    // Wait a bit for all to arrive, then count.
    await new Promise((resolve) => setTimeout(resolve, 200));
    const broadcasts = collectorB.messages.filter((m) => m.type === "telepresence-broadcast");
    assert.equal(broadcasts.length, 3, "only 3 of 6 messages should pass the rate limiter");

    closeAll(wsA, wsB);
  } finally {
    await server.close();
  }
});

test("commit rejects links missing required structural fields", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    // Missing data
    const res1 = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [{ author: agent.did, timestamp: new Date().toISOString() }], removals: [] },
      token
    );
    assert.equal(res1.status, 400);

    // Missing timestamp
    const res2 = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [{ author: agent.did, data: { source: "a", target: "b" } }], removals: [] },
      token
    );
    assert.equal(res2.status, 400);

    // Non-object data
    const res3 = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [{ author: agent.did, timestamp: new Date().toISOString(), data: "not-an-object" }], removals: [] },
      token
    );
    assert.equal(res3.status, 400);
  });
});

test("session sweep removes expired sessions from the database", async () => {
  const server = await startTestServer({
    jwtExpirySeconds: 1, // 1s expiry
    sessionSweepIntervalMs: 100, // sweep every 100ms for test speed
  });
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    await authenticateAgent(server.url, roomId, agent);

    // Confirm there are sessions
    const before = server.built.db.raw
      .prepare("SELECT COUNT(*) as c FROM sessions")
      .get() as { c: number };
    assert.ok(before.c > 0, "session should exist after auth");

    // Wait for JWT to expire + sweep to fire
    await new Promise((resolve) => setTimeout(resolve, 1500));

    const after = server.built.db.raw
      .prepare("SELECT COUNT(*) as c FROM sessions")
      .get() as { c: number };
    assert.equal(after.c, 0, "expired sessions should have been swept");
  } finally {
    await server.close();
  }
});

test("render response includes sequence number", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link = await createSignedLink(agent, { source: "a", predicate: "p", target: "b" });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      token
    );

    const res = await getJson<{ links: unknown[]; revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/render`,
      token
    );
    assert.equal(res.status, 200);
    assert.equal(typeof res.body.sequence, "number");
    assert.ok(res.body.sequence > 0, "sequence should be positive after a commit");
  });
});
