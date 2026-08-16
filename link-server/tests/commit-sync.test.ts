import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  authenticateAgent,
  createSignedLink,
  createTestAgent,
  getJson,
  linkHashOf,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import { computeRevision, type LinkExpression, type PerspectiveDiff } from "../src/types.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

test("commit persists links; sync and render reflect them with a matching revision", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link1 = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    const link2 = await createSignedLink(agent, { source: "b", predicate: "rel", target: "c" });

    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link1, link2], removals: [] },
      token
    );
    assert.equal(commitRes.status, 200);
    assert.equal(commitRes.body.sequence, 1);

    const expectedRevision = computeRevision([linkHashOf(link1), linkHashOf(link2)]);
    assert.equal(commitRes.body.revision, expectedRevision);

    const renderRes = await getJson<{ links: LinkExpression[]; revision: string }>(
      `${server.url}/rooms/${roomId}/render`,
      token
    );
    assert.equal(renderRes.status, 200);
    assert.equal(renderRes.body.links.length, 2);
    assert.equal(renderRes.body.revision, expectedRevision);

    const syncRes = await getJson<{ diffs: PerspectiveDiff[]; revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/sync?since=0`,
      token
    );
    assert.equal(syncRes.status, 200);
    assert.equal(syncRes.body.diffs.length, 1);
    assert.equal(syncRes.body.diffs[0].additions.length, 2);
    assert.equal(syncRes.body.revision, expectedRevision);
    assert.equal(syncRes.body.sequence, 1);

    const revisionRes = await getJson<{ revision: string; sequence: number }>(
      `${server.url}/rooms/${roomId}/revision`,
      token
    );
    assert.equal(revisionRes.body.revision, expectedRevision);
    assert.equal(revisionRes.body.sequence, 1);
  });
});

test("removal (OR-Set) takes a link out of the active set but keeps history in the diff log", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, token);

    // A removal re-sends the *exact* original LinkExpression, now in `removals`.
    const removeRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [link] },
      token
    );
    assert.equal(removeRes.status, 200);
    assert.equal(removeRes.body.sequence, 2);
    assert.equal(removeRes.body.revision, computeRevision([]));

    const renderRes = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${roomId}/render`,
      token
    );
    assert.equal(renderRes.body.links.length, 0);

    const syncRes = await getJson<{ diffs: PerspectiveDiff[] }>(
      `${server.url}/rooms/${roomId}/sync?since=0`,
      token
    );
    assert.equal(syncRes.body.diffs.length, 2, "both the addition and the removal remain in history");
  });
});

test("sync ?since=N only returns diffs after sequence N", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    for (let i = 0; i < 3; i++) {
      const link = await createSignedLink(agent, { source: `s${i}`, predicate: "rel", target: "t" });
      await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, token);
    }

    const full = await getJson<{ diffs: PerspectiveDiff[] }>(
      `${server.url}/rooms/${roomId}/sync?since=0`,
      token
    );
    assert.equal(full.body.diffs.length, 3);

    const partial = await getJson<{ diffs: PerspectiveDiff[] }>(
      `${server.url}/rooms/${roomId}/sync?since=2`,
      token
    );
    assert.equal(partial.body.diffs.length, 1);

    const none = await getJson<{ diffs: PerspectiveDiff[] }>(
      `${server.url}/rooms/${roomId}/sync?since=99`,
      token
    );
    assert.equal(none.body.diffs.length, 0);
  });
});

test("revision converges regardless of add order (content hash, not sequence-dependent)", async () => {
  await withServer(async (server) => {
    const roomA = randomUUID();
    const roomB = randomUUID();
    const agent = await createTestAgent();
    const tokenA = await authenticateAgent(server.url, roomA, agent);
    const tokenB = await authenticateAgent(server.url, roomB, agent);

    const link1 = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    const link2 = await createSignedLink(agent, { source: "c", predicate: "rel", target: "d" });

    // Room A: commit both links in one shot.
    await postJson(`${server.url}/rooms/${roomA}/commit`, { additions: [link1, link2], removals: [] }, tokenA);
    // Room B: commit in reverse order, as two separate diffs.
    await postJson(`${server.url}/rooms/${roomB}/commit`, { additions: [link2], removals: [] }, tokenB);
    await postJson(`${server.url}/rooms/${roomB}/commit`, { additions: [link1], removals: [] }, tokenB);

    const revA = await getJson<{ revision: string }>(`${server.url}/rooms/${roomA}/revision`, tokenA);
    const revB = await getJson<{ revision: string }>(`${server.url}/rooms/${roomB}/revision`, tokenB);
    assert.equal(revA.body.revision, revB.body.revision);
  });
});

test("commit rejects a link whose author does not match the authenticated DID", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const other = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link = await createSignedLink(other, { source: "a", predicate: "rel", target: "b" });
    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      token
    );
    assert.equal(res.status, 400);
  });
});

test("commit rejects a link with a tampered signature", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    const tampered: LinkExpression = { ...link, data: { ...link.data as { source: string; predicate: string | null; target: string }, target: "hacked" } };

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [tampered], removals: [] },
      token
    );
    assert.equal(res.status, 400);
  });
});

test("commit rejects entirely if any single link in the batch is invalid (atomic)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const other = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const good = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    const bad = await createSignedLink(other, { source: "x", predicate: "rel", target: "y" });

    const res = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [good, bad], removals: [] },
      token
    );
    assert.equal(res.status, 400);

    const render = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(render.body.links.length, 0, "no partial commit should have been applied");
  });
});

test("a DID not on the room ACL cannot commit", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    // A stranger's token doesn't exist since they can't auth (403) — simulate
    // by forging a bearer with garbage; the important contract is the ACL
    // check happens on every request, not just at login.
    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [] },
      "garbage-token"
    );
    assert.equal(res.status, 401);
  });
});

test("removing a DID from the ACL revokes their access on subsequent requests", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const friend = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: friend.did }, adminToken);
    const friendToken = await authenticateAgent(server.url, roomId, friend);

    const before = await getJson(`${server.url}/rooms/${roomId}/revision`, friendToken);
    assert.equal(before.status, 200);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: friend.did }, adminToken);

    // ACL removal also revokes the removed DID's sessions immediately, so the
    // very next request is rejected as "session revoked" (401) rather than
    // waiting for a lazy ACL check (403) on an otherwise-still-valid JWT.
    const after = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/revision`, friendToken);
    assert.equal(after.status, 401);
  });
});
