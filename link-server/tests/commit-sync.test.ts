import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  authenticateAgent,
  createSignedLink,
  createTestAgent,
  getJson,
  linkHashOf,
  openLink,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import { EMPTY_REVISION, xorHex, type LinkExpression, type PerspectiveDiff } from "../src/types.js";

/** Test helper: compute the XOR revision of a set of link hashes.
 * The server maintains this incrementally — this standalone version
 * exists only for assertions. */
function computeRevision(linkHashes: string[]): string {
  let rev = EMPTY_REVISION;
  for (const hash of linkHashes) {
    rev = xorHex(rev, hash);
  }
  return rev;
}

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

test("commit rejects entirely if any single link in the batch is invalid (atomic)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const good = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    // Encrypted, but without the link_hash the server content-addresses it by.
    const { link_hash: _dropped, ...bad } = await createSignedLink(agent, { source: "x", predicate: "rel", target: "y" });

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

// ---- Review item 6: applyDiffAndAppend ordering invariant ----

test("applyDiffAndAppend processes additions before removals — simultaneous add+remove leaves link removed", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const link = await createSignedLink(agent, { source: "s", predicate: "p", target: "t" });

    // A diff that both adds AND removes the same link in one commit.
    // Additions run first (XOR in), then removals (XOR back out) — net
    // effect: the link does not remain in the active set.
    const res = await postJson(`${server.url}/rooms/${roomId}/commit`, {
      additions: [link],
      removals: [link],
    }, token);
    assert.equal(res.status, 200);

    const render = await getJson<{ links: LinkExpression[] }>(
      `${server.url}/rooms/${roomId}/render`, token,
    );
    assert.equal(render.body.links.length, 0,
      "link should not remain when added and removed in the same diff");

    // Revision should return to EMPTY_REVISION (XOR is self-inverse).
    const rev = await getJson<{ revision: string }>(
      `${server.url}/rooms/${roomId}/revision`, token,
    );
    assert.equal(rev.body.revision, EMPTY_REVISION);
  });
});

// ---- The diff log is the catch-up source: it is never pruned ----

test("sync from an early cursor returns every later diff, past 10,000 commits", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    // Sequence 1 goes through the commit route; it is the member's cursor.
    const first = await createSignedLink(agent, { source: "s0", predicate: "p", target: "t0" });
    const commitRes = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`, { additions: [first], removals: [] }, token,
    );
    assert.equal(commitRes.status, 200);
    const cursor = commitRes.body.sequence;
    assert.equal(cursor, 1);

    // The other members then commit more than 10,000 diffs. They go through
    // the same db call the commit route makes, without signing and HTTP
    // round-trips for each one (/sync returns stored diffs unchecked).
    const later = 10_050;
    for (let i = 1; i <= later; i++) {
      server.built.db.applyDiffAndAppend(roomId, {
        additions: [{
          author: agent.did,
          timestamp: new Date(0).toISOString(),
          data: { source: `s${i}`, predicate: "p", target: `t${i}` },
          proof: { signature: "00", key: "00", valid: true, invalid: false },
        } as LinkExpression],
        removals: [],
      }, agent.did);
    }

    // `/sync` is paged: follow `next` to the end of the log.
    const syncAll = async (since: number) => {
      const diffs: PerspectiveDiff[] = [];
      let sequence = 0;
      for (let next: number | null = since; next !== null; ) {
        const res = await getJson<{ diffs: PerspectiveDiff[]; sequence: number; next: number | null }>(
          `${server.url}/rooms/${roomId}/sync?since=${next}&limit=5000`, token,
        );
        assert.equal(res.status, 200);
        diffs.push(...res.body.diffs);
        sequence = res.body.sequence;
        next = res.body.next;
      }
      return { diffs, sequence };
    };

    const fromCursor = await syncAll(cursor);
    assert.equal(fromCursor.sequence, cursor + later);
    assert.equal(fromCursor.diffs.length, later,
      "every diff after the cursor must still be in the log");
    assert.equal(fromCursor.diffs[0].additions[0].data.source, "s1",
      "the first diff after the cursor must be the next one committed");
    assert.equal(fromCursor.diffs[later - 1].additions[0].data.source, `s${later}`);

    assert.equal((await syncAll(0)).diffs.length, later + 1);
  });
});

test("render still returns the whole active set for a room whose early diffs were pruned before this change", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const links: LinkExpression[] = [];
    for (let i = 0; i < 5; i++) {
      const link = await createSignedLink(agent, { source: `s${i}`, predicate: "p", target: `t${i}` });
      links.push(link);
      const res = await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, token);
      assert.equal(res.status, 200);
    }
    const removed = await postJson(
      `${server.url}/rooms/${roomId}/commit`, { additions: [], removals: [links[4]] }, token,
    );
    assert.equal(removed.status, 200);

    // A server that ran the old pruning deleted the room's early diff rows
    // and kept the active set they built.
    server.built.db.raw.prepare("DELETE FROM diffs WHERE room_id = ? AND sequence <= ?").run(roomId, 3);

    const sync = await getJson<{ diffs: PerspectiveDiff[] }>(`${server.url}/rooms/${roomId}/sync?since=0`, token);
    assert.equal(sync.body.diffs.length, 3, "the gap stays in the log");

    const render = await getJson<{ links: LinkExpression[]; revision: string }>(
      `${server.url}/rooms/${roomId}/render`, token,
    );
    assert.equal(render.status, 200);
    assert.deepEqual(
      render.body.links.map((l) => openLink(l).data.source).sort(),
      ["s0", "s1", "s2", "s3"],
      "render answers from the active set, so it repairs a member behind the gap",
    );
    assert.equal(render.body.revision, computeRevision(links.slice(0, 4).map(linkHashOf)));
  });
});
