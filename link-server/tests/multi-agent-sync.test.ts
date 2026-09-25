/**
 * Multi-agent sync integration tests.
 *
 * Each test boots a real link-server and exercises the full sync contract
 * with two (or more) agents: auth, commit, HTTP sync, WebSocket push,
 * bootstrap, catch-up after disconnect, and bidirectional OR-Set
 * convergence.
 *
 * These close the gap between "each endpoint works in isolation" (the
 * existing test files) and "two agents syncing through the server
 * actually converge."
 */

import { test } from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import type WebSocket from "ws";

import {
  startTestServer,
  createTestAgent,
  authenticateAgent,
  createSignedLink,
  openAuthenticatedWs,
  collectMessages,
  postJson,
  getJson,
  waitFor,
  type TestServerHandle,
  type TestAgent,
} from "./helpers.js";
import type { LinkExpression, PerspectiveDiff } from "../src/types.js";

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

async function withServer(fn: (s: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer({ autoAdmit: true });
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

function closeAll(...sockets: WebSocket[]): void {
  for (const ws of sockets) ws.close();
}

/** Authenticate both agents against the same room (autoAdmit handles ACL). */
async function authPair(
  url: string,
  roomId: string,
  a: TestAgent,
  b: TestAgent
): Promise<[string, string]> {
  const tokenA = await authenticateAgent(url, roomId, a);
  const tokenB = await authenticateAgent(url, roomId, b);
  return [tokenA, tokenB];
}

// ---------------------------------------------------------------------------
// 1. Late-joiner bootstrap: A commits, then B joins and sees everything
// ---------------------------------------------------------------------------

test("late joiner receives all prior links via /render bootstrap", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();

    // A authenticates and commits 3 links
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    const links: LinkExpression[] = [];
    for (let i = 0; i < 3; i++) {
      links.push(
        await createSignedLink(agentA, {
          source: `test://source-${i}`,
          predicate: "test://p",
          target: `test://target-${i}`,
        })
      );
    }
    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: links, removals: [] },
      tokenA
    );
    assert.equal(commitRes.status, 200);
    assert.equal(commitRes.body.sequence, 1);

    // B authenticates later — no WebSocket, pure HTTP bootstrap
    const tokenB = await authenticateAgent(server.url, roomId, agentB);

    // B calls /render (the bootstrap path a real language uses)
    const render = await getJson<{ links: LinkExpression[]; revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/render`,
      tokenB
    );
    assert.equal(render.status, 200);
    assert.equal(render.body.links.length, 3, "B must see all 3 of A's links");
    assert.equal(render.body.revision, commitRes.body.revision);
    assert.equal(render.body.sequence, commitRes.body.sequence);

    // Verify the link data round-tripped correctly
    const sources = render.body.links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://source-0", "test://source-1", "test://source-2"]);
  });
});

// ---------------------------------------------------------------------------
// 2. Real-time WebSocket push: A commits while B is connected via WS
// ---------------------------------------------------------------------------

test("real-time WS push: A commits, B receives the diff via WebSocket", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // B connects via WebSocket
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collector = collectMessages(wsB);

    // A commits a link over HTTP
    const link = await createSignedLink(agentA, {
      source: "test://ws-push",
      predicate: "test://p",
      target: "test://t",
    });
    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      tokenA
    );
    assert.equal(commitRes.status, 200);

    // B receives the diff via WebSocket
    const diffMsg = await collector.waitForType("diff", 3000);
    assert.equal(diffMsg.type, "diff");
    assert.equal(diffMsg.sequence, commitRes.body.sequence);
    assert.equal(diffMsg.revision, commitRes.body.revision);
    const payload = diffMsg.payload as PerspectiveDiff;
    assert.equal(payload.additions.length, 1);
    assert.equal((payload.additions[0].data as { source: string }).source, "test://ws-push");

    closeAll(wsB);
  });
});

// ---------------------------------------------------------------------------
// 3. Bidirectional sync: A → B and B → A both work
// ---------------------------------------------------------------------------

test("bidirectional sync: both agents commit and receive each other's links", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // Both connect via WebSocket
    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collectorA = collectMessages(wsA);
    const collectorB = collectMessages(wsB);

    // A commits
    const linkA = await createSignedLink(agentA, {
      source: "test://from-a",
      predicate: "test://p",
      target: "test://to-a",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [linkA], removals: [] },
      tokenA
    );

    // B should receive A's diff
    const bGotA = await collectorB.waitForType("diff", 3000);
    assert.equal(
      ((bGotA.payload as PerspectiveDiff).additions[0].data as { source: string }).source,
      "test://from-a"
    );

    // B commits
    const linkB = await createSignedLink(agentB, {
      source: "test://from-b",
      predicate: "test://p",
      target: "test://to-b",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [linkB], removals: [] },
      tokenB
    );

    // A should receive B's diff
    const aGotB = await collectorA.waitForType("diff", 3000);
    assert.equal(
      ((aGotB.payload as PerspectiveDiff).additions[0].data as { source: string }).source,
      "test://from-b"
    );

    // Final render: both agents see both links
    const render = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${roomId}/render`,
      tokenA
    );
    assert.equal(render.body.links.length, 2);
    const sources = render.body.links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://from-a", "test://from-b"]);

    closeAll(wsA, wsB);
  });
});

// ---------------------------------------------------------------------------
// 4. Offline catch-up: B disconnects, A commits, B reconnects + syncs
// ---------------------------------------------------------------------------

test("offline catch-up: B disconnects, A commits, B catches up via /sync?since=N", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // A commits link-1 while B is connected
    const link1 = await createSignedLink(agentA, {
      source: "test://before-disconnect",
      predicate: "test://p",
      target: "test://t",
    });
    const commit1 = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link1], removals: [] },
      tokenA
    );
    assert.equal(commit1.status, 200);

    // B connects, receives the first link via WS, then disconnects
    const wsB1 = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collector1 = collectMessages(wsB1);
    // The existing link was committed before B connected, so B must HTTP-sync
    // to see it. Just verify B can reach the server via WS:
    await waitFor(() => wsB1.readyState === wsB1.OPEN, 2000);

    // B records its sequence cursor (like a real language would from /render)
    const seq1 = commit1.body.sequence;
    wsB1.close();
    // Wait for the close to complete
    await waitFor(() => wsB1.readyState === wsB1.CLOSED, 2000);

    // A commits link-2 and link-3 while B is offline
    const link2 = await createSignedLink(agentA, {
      source: "test://while-offline-1",
      predicate: "test://p",
      target: "test://t",
    });
    const link3 = await createSignedLink(agentA, {
      source: "test://while-offline-2",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link2], removals: [] },
      tokenA
    );
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link3], removals: [] },
      tokenA
    );

    // B reconnects and catches up via HTTP (the belt-and-braces path)
    const syncRes = await getJson<{
      diffs: Array<{ additions: LinkExpression[]; removals: LinkExpression[] }>;
      revision: string;
      sequence: number;
    }>(`${server.url}/rooms/${roomId}/sync?since=${seq1}`, tokenB);

    assert.equal(syncRes.status, 200);
    assert.equal(syncRes.body.diffs.length, 2, "B must receive the 2 diffs committed while offline");
    assert.equal(syncRes.body.sequence, seq1 + 2);

    const catchUpSources = syncRes.body.diffs
      .flatMap((d) => d.additions)
      .map((l) => (l.data as { source: string }).source)
      .sort();
    assert.deepEqual(catchUpSources, ["test://while-offline-1", "test://while-offline-2"]);
  });
});

// ---------------------------------------------------------------------------
// 5. OR-Set convergence: interleaved adds and removes from both agents
// ---------------------------------------------------------------------------

test("OR-Set convergence: interleaved adds/removes from both agents yield same revision", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // A adds link-X and link-Y
    const linkX = await createSignedLink(agentA, {
      source: "test://x",
      predicate: "test://p",
      target: "test://x-target",
    });
    const linkY = await createSignedLink(agentA, {
      source: "test://y",
      predicate: "test://p",
      target: "test://y-target",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [linkX, linkY], removals: [] },
      tokenA
    );

    // B adds link-Z
    const linkZ = await createSignedLink(agentB, {
      source: "test://z",
      predicate: "test://p",
      target: "test://z-target",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [linkZ], removals: [] },
      tokenB
    );

    // A removes link-X (its own link — must send the exact original)
    const removeRes = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [linkX] },
      tokenA
    );
    assert.equal(removeRes.status, 200);

    // Both query revision — must match
    const revA = await getJson<{ revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/revision`,
      tokenA
    );
    const revB = await getJson<{ revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/revision`,
      tokenB
    );
    assert.equal(revA.body.revision, revB.body.revision, "revisions must converge");
    assert.equal(revA.body.sequence, revB.body.sequence);

    // Render: only Y and Z should remain
    const render = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${roomId}/render`,
      tokenA
    );
    assert.equal(render.body.links.length, 2);
    const sources = render.body.links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://y", "test://z"]);
  });
});

// ---------------------------------------------------------------------------
// 6. WS push + HTTP sync consistency: same data from both paths
// ---------------------------------------------------------------------------

test("WS push and HTTP sync return identical data for the same diffs", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // B connects via WS
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collector = collectMessages(wsB);

    // A commits
    const link = await createSignedLink(agentA, {
      source: "test://consistency",
      predicate: "test://p",
      target: "test://t",
    });
    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      tokenA
    );

    // Wait for WS push
    const wsMsg = await collector.waitForType("diff", 3000);

    // Also fetch via HTTP sync
    const syncRes = await getJson<{
      diffs: Array<{ additions: LinkExpression[]; removals: LinkExpression[] }>;
      revision: string;
      sequence: number;
    }>(`${server.url}/rooms/${roomId}/sync?since=0`, tokenB);

    // Compare: WS diff payload must match the HTTP diff
    const wsPayload = wsMsg.payload as PerspectiveDiff;
    const httpDiff = syncRes.body.diffs[0];

    assert.equal(wsPayload.additions.length, httpDiff.additions.length);
    assert.equal(
      (wsPayload.additions[0].data as { source: string }).source,
      (httpDiff.additions[0].data as { source: string }).source
    );
    assert.equal(wsMsg.revision, syncRes.body.revision);
    assert.equal(wsMsg.sequence, syncRes.body.sequence);

    closeAll(wsB);
  });
});

// ---------------------------------------------------------------------------
// 7. Multiple rooms: agents in different rooms don't cross-contaminate
// ---------------------------------------------------------------------------

test("room isolation: commits in room-1 do not leak into room-2", async () => {
  await withServer(async (server) => {
    const room1 = randomUUID();
    const room2 = randomUUID();
    const agentA = await createTestAgent();

    // A authenticates in both rooms
    const token1 = await authenticateAgent(server.url, room1, agentA);
    const token2 = await authenticateAgent(server.url, room2, agentA);

    // Commit to room-1
    const link1 = await createSignedLink(agentA, {
      source: "test://room1-only",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${room1}/commit`,
      { additions: [link1], removals: [] },
      token1
    );

    // Commit a different link to room-2
    const link2 = await createSignedLink(agentA, {
      source: "test://room2-only",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${room2}/commit`,
      { additions: [link2], removals: [] },
      token2
    );

    // Each room sees only its own link
    const render1 = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${room1}/render`,
      token1
    );
    const render2 = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${room2}/render`,
      token2
    );
    assert.equal(render1.body.links.length, 1);
    assert.equal((render1.body.links[0].data as { source: string }).source, "test://room1-only");
    assert.equal(render2.body.links.length, 1);
    assert.equal((render2.body.links[0].data as { source: string }).source, "test://room2-only");
  });
});

// ---------------------------------------------------------------------------
// 8. WS push excludes the committer but reaches all other agents
// ---------------------------------------------------------------------------

test("WS push reaches all connected agents except the committer", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const agentC = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomId, agentA);
    const tokenB = await authenticateAgent(server.url, roomId, agentB);
    const tokenC = await authenticateAgent(server.url, roomId, agentC);

    const wsA = await openAuthenticatedWs(server.wsUrl, roomId, tokenA);
    const wsB = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const wsC = await openAuthenticatedWs(server.wsUrl, roomId, tokenC);
    const cA = collectMessages(wsA);
    const cB = collectMessages(wsB);
    const cC = collectMessages(wsC);

    // B commits
    const link = await createSignedLink(agentB, {
      source: "test://from-b",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      tokenB
    );

    // A and C should receive the diff
    await cA.waitForType("diff", 3000);
    await cC.waitForType("diff", 3000);

    // B should NOT receive its own diff
    // Give it a moment to ensure no late delivery
    await new Promise((r) => setTimeout(r, 200));
    assert.ok(
      !cB.messages.some((m) => m.type === "diff"),
      "committer must not receive its own diff"
    );

    closeAll(wsA, wsB, wsC);
  });
});

// ---------------------------------------------------------------------------
// 9. Burst commit + sync: many links committed rapidly all arrive
// ---------------------------------------------------------------------------

test("burst commit: 50 links committed in rapid succession all sync correctly", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // A commits 50 links in a single batch
    const links: LinkExpression[] = [];
    for (let i = 0; i < 50; i++) {
      links.push(
        await createSignedLink(agentA, {
          source: `test://burst-${i}`,
          predicate: "test://p",
          target: `test://target-${i}`,
        })
      );
    }
    const commitRes = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: links, removals: [] },
      tokenA
    );
    assert.equal(commitRes.status, 200);

    // B syncs via HTTP
    const render = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${roomId}/render`,
      tokenB
    );
    assert.equal(render.body.links.length, 50, "all 50 links must arrive via render");
  });
});

// ---------------------------------------------------------------------------
// 10. Sequence monotonicity across agents
// ---------------------------------------------------------------------------

test("sequence numbers increase monotonically across interleaved commits from both agents", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    const sequences: number[] = [];

    for (let i = 0; i < 6; i++) {
      const agent = i % 2 === 0 ? agentA : agentB;
      const token = i % 2 === 0 ? tokenA : tokenB;
      const link = await createSignedLink(agent, {
        source: `test://seq-${i}`,
        predicate: "test://p",
        target: "test://t",
      });
      const res = await postJson<{ sequence: number }>(
        `${server.url}/rooms/${roomId}/commit`,
        { additions: [link], removals: [] },
        token
      );
      assert.equal(res.status, 200);
      sequences.push(res.body.sequence);
    }

    // Sequences must be strictly increasing
    for (let i = 1; i < sequences.length; i++) {
      assert.ok(
        sequences[i] > sequences[i - 1],
        `sequence[${i}]=${sequences[i]} must exceed sequence[${i - 1}]=${sequences[i - 1]}`
      );
    }

    // /sync?since=3 should return only diffs 4, 5, 6
    const partialSync = await getJson<{
      diffs: unknown[];
      sequence: number;
    }>(`${server.url}/rooms/${roomId}/sync?since=3`, tokenA);
    assert.equal(partialSync.body.diffs.length, 3);
    assert.equal(partialSync.body.sequence, 6);
  });
});

// ---------------------------------------------------------------------------
// 11. WS reconnect after offline: new WS session receives diffs committed
//     during the gap (simulates the language's onOpen catch-up)
// ---------------------------------------------------------------------------

test("WS reconnect: second WS session receives diffs committed during the gap", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agentA = await createTestAgent();
    const agentB = await createTestAgent();
    const [tokenA, tokenB] = await authPair(server.url, roomId, agentA, agentB);

    // A commits link-1
    const link1 = await createSignedLink(agentA, {
      source: "test://pre-ws",
      predicate: "test://p",
      target: "test://t",
    });
    const c1 = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link1], removals: [] },
      tokenA
    );

    // B connects WS, then disconnects
    const ws1 = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    ws1.close();
    await waitFor(() => ws1.readyState === ws1.CLOSED, 2000);

    // A commits link-2 while B is offline
    const link2 = await createSignedLink(agentA, {
      source: "test://during-offline",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link2], removals: [] },
      tokenA
    );

    // B reconnects WS
    const ws2 = await openAuthenticatedWs(server.wsUrl, roomId, tokenB);
    const collector2 = collectMessages(ws2);

    // B does catch-up HTTP sync (simulating the language's onOpen handler)
    const catchUp = await getJson<{
      diffs: Array<{ additions: LinkExpression[] }>;
      sequence: number;
    }>(`${server.url}/rooms/${roomId}/sync?since=${c1.body.sequence}`, tokenB);

    assert.equal(catchUp.body.diffs.length, 1, "catch-up must return the 1 missed diff");
    assert.equal(
      (catchUp.body.diffs[0].additions[0].data as { source: string }).source,
      "test://during-offline"
    );

    // Now A commits link-3 — B's new WS should receive it live
    const link3 = await createSignedLink(agentA, {
      source: "test://after-reconnect",
      predicate: "test://p",
      target: "test://t",
    });
    await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link3], removals: [] },
      tokenA
    );

    const liveMsg = await collector2.waitForType("diff", 3000);
    assert.equal(
      ((liveMsg.payload as PerspectiveDiff).additions[0].data as { source: string }).source,
      "test://after-reconnect"
    );

    closeAll(ws2);
  });
});
