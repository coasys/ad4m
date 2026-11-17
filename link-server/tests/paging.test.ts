/**
 * Paged /render (a snapshot pinned at `upto`, keyset-paged on
 * (sequence, link_hash)), paged /sync, and compressed answers. A request
 * without `limit` gets a page of DEFAULT_PAGE_LIMIT.
 */
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { get as httpGet } from "node:http";
import { test } from "node:test";
import { gunzipSync } from "node:zlib";
import {
  authenticateAgent,
  createSignedLink,
  createTestAgent,
  getJson,
  linkHashOf,
  startTestServer,
  type TestAgent,
  type TestServerHandle,
} from "./helpers.js";
import { DEFAULT_PAGE_LIMIT } from "../src/routes.js";
import type { LinkExpression, PerspectiveDiff } from "../src/types.js";

interface RenderPage {
  links: LinkExpression[];
  revision: string;
  sequence: number;
  upto: number;
  next: string | null;
}

interface SyncPage {
  diffs: Array<PerspectiveDiff & { sequence: number }>;
  revision: string;
  sequence: number;
  next: number | null;
}

async function withRoom(
  fn: (ctx: { server: TestServerHandle; roomId: string; agent: TestAgent; token: string }) => Promise<void>
): Promise<void> {
  const server = await startTestServer();
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);
    await fn({ server, roomId, agent, token });
  } finally {
    await server.close();
  }
}

/** Commits straight into the DB: the commit rate limit would make 60 HTTP commits take a minute. */
function commit(server: TestServerHandle, roomId: string, did: string, diff: PerspectiveDiff): number {
  return server.built.db.applyDiffAndAppend(roomId, diff, did).sequence;
}

async function makeLinks(agent: TestAgent, n: number, tag: string): Promise<LinkExpression[]> {
  const links: LinkExpression[] = [];
  for (let i = 0; i < n; i++) {
    links.push(await createSignedLink(agent, { source: `${tag}-${i}`, predicate: "p", target: "t" }));
  }
  return links;
}

async function renderAll(server: TestServerHandle, roomId: string, token: string, limit: number): Promise<RenderPage[]> {
  const pages: RenderPage[] = [];
  let next: string | null = null;
  do {
    const query: string = next === null ? `limit=${limit}` : `limit=${limit}&next=${encodeURIComponent(next)}`;
    const res = await getJson<RenderPage>(`${server.url}/rooms/${roomId}/render?${query}`, token);
    assert.equal(res.status, 200);
    pages.push(res.body);
    next = res.body.next;
  } while (next !== null);
  return pages;
}

/** The hashes of the room's active links, read straight from the DB. */
function activeHashes(server: TestServerHandle, roomId: string): string[] {
  return server.built.db
    .getActiveLinkPage(roomId, Number.MAX_SAFE_INTEGER, { sequence: 0, hash: "" }, Number.MAX_SAFE_INTEGER)
    .map((row) => row.link_hash);
}

/** A raw GET, so the test decides the Accept-Encoding header (fetch always sends one). */
function rawGet(url: string, headers: Record<string, string>): Promise<{ headers: Record<string, unknown>; body: Buffer }> {
  return new Promise((resolve, reject) => {
    httpGet(url, { headers }, (res) => {
      const chunks: Buffer[] = [];
      res.on("data", (c: Buffer) => chunks.push(c));
      res.on("end", () => resolve({ headers: res.headers, body: Buffer.concat(chunks) }));
      res.on("error", reject);
    }).on("error", reject);
  });
}

test("render and sync without limit answer one page of DEFAULT_PAGE_LIMIT, in the paged shape", async () => {
  await withRoom(async ({ server, roomId, agent, token }) => {
    const links = await makeLinks(agent, DEFAULT_PAGE_LIMIT + 1, "default");
    for (const link of links) commit(server, roomId, agent.did, { additions: [link], removals: [] });

    const render = await getJson<RenderPage>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(render.status, 200);
    assert.deepEqual(Object.keys(render.body).sort(), ["links", "next", "revision", "sequence", "upto"]);
    assert.equal(render.body.links.length, DEFAULT_PAGE_LIMIT);
    assert.equal(render.body.upto, DEFAULT_PAGE_LIMIT + 1);
    assert.equal(typeof render.body.next, "string");
    const rest = await getJson<RenderPage>(
      `${server.url}/rooms/${roomId}/render?next=${encodeURIComponent(render.body.next!)}`, token);
    assert.equal(rest.body.links.length, 1);
    assert.equal(rest.body.next, null);

    const sync = await getJson<SyncPage>(`${server.url}/rooms/${roomId}/sync?since=0`, token);
    assert.equal(sync.status, 200);
    assert.deepEqual(Object.keys(sync.body).sort(), ["diffs", "next", "revision", "sequence"]);
    assert.equal(sync.body.diffs.length, DEFAULT_PAGE_LIMIT);
    assert.equal(sync.body.next, DEFAULT_PAGE_LIMIT);
    assert.deepEqual(sync.body.diffs[0], { additions: [links[0]], removals: [], sequence: 1 });
  });
});

test("paged render returns every active link once, in pages of at most limit, pinned at one upto", async () => {
  await withRoom(async ({ server, roomId, agent, token }) => {
    const single = await makeLinks(agent, 13, "single");
    for (const link of single) commit(server, roomId, agent.did, { additions: [link], removals: [] });
    // Many links at one sequence: the link_hash half of the keyset orders them.
    const batch = await makeLinks(agent, 12, "batch");
    commit(server, roomId, agent.did, { additions: batch, removals: [] });

    const pages = await renderAll(server, roomId, token, 10);
    assert.deepEqual(pages.map((p) => p.links.length), [10, 10, 5]);
    assert.ok(pages.every((p) => p.upto === 14), "every page names the snapshot's upto");
    assert.equal(pages[2].next, null);

    const hashes = pages.flatMap((p) => p.links.map(linkHashOf));
    assert.equal(new Set(hashes).size, 25, "no link is sent twice");
    assert.deepEqual(new Set(hashes), new Set([...single, ...batch].map(linkHashOf)));
  });
});

test("a paged render stays a snapshot under concurrent writes, and the tail from upto completes it", async () => {
  await withRoom(async ({ server, roomId, agent, token }) => {
    const initial = await makeLinks(agent, 30, "init");
    for (const link of initial) commit(server, roomId, agent.did, { additions: [link], removals: [] });

    const first = await getJson<RenderPage>(`${server.url}/rooms/${roomId}/render?limit=10`, token);
    assert.equal(first.body.upto, 30);
    const sentFirst = new Set(first.body.links.map(linkHashOf));
    const notYetSent = initial.find((l) => !sentFirst.has(linkHashOf(l)))!;
    const alreadySent = first.body.links[0];

    // Writes between pages: one add, a removal of a link the next pages would
    // have carried, and a removal of one already sent.
    const [added] = await makeLinks(agent, 1, "late");
    commit(server, roomId, agent.did, { additions: [added], removals: [] });
    commit(server, roomId, agent.did, { additions: [], removals: [notYetSent] });
    commit(server, roomId, agent.did, { additions: [], removals: [alreadySent] });

    const pages = [first.body];
    let next = first.body.next;
    while (next !== null) {
      const res = await getJson<RenderPage>(
        `${server.url}/rooms/${roomId}/render?limit=10&next=${encodeURIComponent(next)}`, token);
      assert.equal(res.body.upto, 30, "later pages keep the first page's upto");
      pages.push(res.body);
      next = res.body.next;
    }
    const later = pages.slice(1).flatMap((p) => p.links.map(linkHashOf));
    assert.ok(!later.includes(linkHashOf(added)), "a link added after upto is not in the snapshot");
    assert.ok(!later.includes(linkHashOf(notYetSent)), "a link removed before its page was read is not sent");

    // Client view: snapshot, then the diff log after upto.
    const state = new Map<string, LinkExpression>();
    for (const page of pages) for (const l of page.links) state.set(linkHashOf(l), l);
    const tail = await getJson<SyncPage>(`${server.url}/rooms/${roomId}/sync?since=30&limit=100`, token);
    for (const d of tail.body.diffs) {
      for (const l of d.additions) state.set(linkHashOf(l), l);
      for (const l of d.removals) state.delete(linkHashOf(l));
    }
    assert.deepEqual(new Set(state.keys()), new Set(activeHashes(server, roomId)));
    assert.equal(tail.body.next, null);
    assert.equal(tail.body.revision, server.built.db.getRoomRevision(roomId));
  });
});

test("paged sync returns at most limit diffs, each with its sequence, and next until the log ends", async () => {
  await withRoom(async ({ server, roomId, agent, token }) => {
    const links = await makeLinks(agent, 7, "s");
    for (const link of links) commit(server, roomId, agent.did, { additions: [link], removals: [] });

    const seen: SyncPage[] = [];
    let since = 0;
    for (;;) {
      const res = await getJson<SyncPage>(`${server.url}/rooms/${roomId}/sync?since=${since}&limit=3`, token);
      assert.equal(res.status, 200);
      assert.equal(res.body.sequence, 7, "sequence is still the room's latest");
      seen.push(res.body);
      if (res.body.next === null) break;
      since = res.body.next;
    }
    assert.deepEqual(seen.map((p) => p.diffs.map((d) => d.sequence)), [[1, 2, 3], [4, 5, 6], [7]]);
    assert.deepEqual(seen.map((p) => p.next), [3, 6, null]);

    assert.deepEqual(
      seen.flatMap((p) => p.diffs.map(({ additions, removals }) => ({ additions, removals }))),
      links.map((link) => ({ additions: [link], removals: [] })),
    );

    const empty = await getJson<SyncPage>(`${server.url}/rooms/${roomId}/sync?since=7&limit=3`, token);
    assert.deepEqual(empty.body.diffs, []);
    assert.equal(empty.body.next, null);
  });
});

test("render and sync refuse an unusable limit or cursor", async () => {
  await withRoom(async ({ server, roomId, token }) => {
    for (const q of ["limit=0", "limit=-1", "limit=abc", "limit=5001"]) {
      assert.equal((await getJson(`${server.url}/rooms/${roomId}/render?${q}`, token)).status, 400, q);
      assert.equal((await getJson(`${server.url}/rooms/${roomId}/sync?since=0&${q}`, token)).status, 400, q);
    }
    for (const next of ["garbage", Buffer.from("[1,2]").toString("base64url"), Buffer.from('["a",1,"h"]').toString("base64url")]) {
      const res = await getJson(`${server.url}/rooms/${roomId}/render?limit=10&next=${next}`, token);
      assert.equal(res.status, 400, next);
    }
  });
});

test("JSON answers are compressed for a client that accepts it, and plain for one that does not", async () => {
  await withRoom(async ({ server, roomId, agent, token }) => {
    const links = await makeLinks(agent, 50, "big");
    commit(server, roomId, agent.did, { additions: links, removals: [] });
    const url = `${server.url}/rooms/${roomId}/render`;
    const auth = { authorization: `Bearer ${token}` };

    const plain = await rawGet(url, auth);
    assert.equal(plain.headers["content-encoding"], undefined);
    const plainBody = JSON.parse(plain.body.toString("utf8"));
    assert.equal(plainBody.links.length, 50);

    const gz = await rawGet(url, { ...auth, "accept-encoding": "gzip" });
    assert.equal(gz.headers["content-encoding"], "gzip");
    assert.ok(gz.body.length < plain.body.length, "the gzip body is smaller");
    assert.deepEqual(JSON.parse(gunzipSync(gz.body).toString("utf8")), plainBody);
  });
});
