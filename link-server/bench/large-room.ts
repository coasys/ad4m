/**
 * Large-room benchmark for the paged /render and /sync: bytes and time for
 * the whole room and for its largest and slowest page, with and without
 * compression. Then a room of MEMBERS membership links (protocol 2): the
 * time to judge and store them, to read them back, to rebuild the room's
 * replay from storage (as after a restart) and to authenticate in it.
 *
 *   pnpm run bench                 # 30 000 links, one link per diff; 1 000 membership links
 *   LINKS=10000 MEMBERS=5000 pnpm run bench
 *
 * Each link is a real AES-256-GCM encryption of a ~575-byte link JSON (the
 * mean size of a team-room link), in base64 as the language writes it.
 * Requests go over loopback HTTP, so times are the server's work plus local
 * transfer, not a WAN round trip.
 */
import { createCipheriv, createHash, randomBytes } from "node:crypto";
import { request } from "node:http";
import { performance } from "node:perf_hooks";
import { brotliDecompressSync, gunzipSync } from "node:zlib";
import { MembershipRooms } from "../src/membership.js";
import {
  authAs,
  authenticateAgent,
  createRoom,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "../tests/helpers.js";
import { links, personAgent, signLink } from "../tests/identity.js";

const LINKS = Number(process.env.LINKS ?? 30_000);
const RENDER_PAGE = Number(process.env.RENDER_PAGE ?? 500);
const SYNC_PAGE = Number(process.env.SYNC_PAGE ?? 100);
const MEMBERS = Number(process.env.MEMBERS ?? 1_000);
const MEMBERSHIP_BATCH = Number(process.env.MEMBERSHIP_BATCH ?? 100);
/** Mean decrypted link size in the team room (DEFECTS PERF-48). */
const PLAINTEXT_BYTES = 575;

function plaintextLink(i: number): string {
  const did = "did:key:z6Mk" + createHash("sha256").update(`agent-${i % 7}`).digest("base64url").slice(0, 44);
  const link = {
    author: did,
    timestamp: new Date(Date.UTC(2026, 9, 1) + i * 1000).toISOString(),
    proof: { signature: randomBytes(64).toString("hex"), key: `${did}#primary` },
    data: {
      source: `ad4m://self/space/${createHash("sha1").update(`s${i % 300}`).digest("hex")}`,
      predicate: "flux://has_message",
      target: `literal://string:message-${i}-`,
    },
  };
  const pad = PLAINTEXT_BYTES - JSON.stringify(link).length;
  link.data.target += "lorem-ipsum-dolor-sit-amet-".repeat(Math.ceil(pad / 27)).slice(0, Math.max(pad, 0));
  return JSON.stringify(link);
}

function encryptedWireLink(i: number, key: Buffer) {
  const nonce = randomBytes(12);
  const cipher = createCipheriv("aes-256-gcm", key, nonce);
  const ct = Buffer.concat([cipher.update(plaintextLink(i), "utf8"), cipher.final(), cipher.getAuthTag()]);
  return {
    data: { ciphertext: ct.toString("base64"), nonce: nonce.toString("base64") },
    link_hash: createHash("sha256").update(`link-${i}`).digest("hex"),
    key_version: 1,
  };
}

interface Measured {
  bytes: number;
  ms: number;
  body: Buffer;
  encoding: string;
}

function get(url: string, token: string, acceptEncoding?: string): Promise<Measured> {
  return new Promise((resolve, reject) => {
    const started = performance.now();
    const headers: Record<string, string> = { authorization: `Bearer ${token}` };
    if (acceptEncoding) headers["accept-encoding"] = acceptEncoding;
    const req = request(url, { headers }, (res) => {
      const chunks: Buffer[] = [];
      res.on("data", (c: Buffer) => chunks.push(c));
      res.on("end", () => {
        const body = Buffer.concat(chunks);
        if (res.statusCode !== 200) return reject(new Error(`${url}: ${res.statusCode} ${body}`));
        resolve({ bytes: body.length, ms: performance.now() - started, body, encoding: String(res.headers["content-encoding"] ?? "identity") });
      });
      res.on("error", reject);
    });
    req.on("error", reject);
    req.end();
  });
}

const mb = (n: number) => `${(n / 1e6).toFixed(2)} MB`;
const kb = (n: number) => `${(n / 1e3).toFixed(0)} kB`;
const ms = (n: number) => `${n.toFixed(0)} ms`;

async function seedRoom(server: TestServerHandle): Promise<{ roomId: string; token: string }> {
  const agent = personAgent("alice");
  const roomId = await createRoom(server.url, { creator: agent });
  const token = await authenticateAgent(server.url, roomId, agent);
  const key = randomBytes(32);
  const db = server.built.db;
  const seed = db.raw.transaction(() => {
    for (let i = 0; i < LINKS; i++) {
      db.applyDiffAndAppend(roomId, { additions: [encryptedWireLink(i, key)], removals: [] }, agent.did);
    }
  });
  seed();
  return { roomId, token };
}

function decoded(res: Measured): Buffer {
  if (res.encoding === "gzip") return gunzipSync(res.body);
  if (res.encoding === "br") return brotliDecompressSync(res.body);
  return res.body;
}

async function pagedRender(server: TestServerHandle, roomId: string, token: string, acceptEncoding?: string) {
  let next: string | null = null;
  let pages = 0, maxBytes = 0, maxMs = 0, totalBytes = 0;
  const started = performance.now();
  do {
    const q: string = `limit=${RENDER_PAGE}` + (next ? `&next=${encodeURIComponent(next)}` : "");
    const res = await get(`${server.url}/rooms/${roomId}/render?${q}`, token, acceptEncoding);
    const plain = decoded(res);
    next = (JSON.parse(plain.toString("utf8")) as { next: string | null }).next;
    pages++;
    totalBytes += res.bytes;
    maxBytes = Math.max(maxBytes, res.bytes);
    maxMs = Math.max(maxMs, res.ms);
  } while (next);
  return { pages, maxBytes, maxMs, totalBytes, totalMs: performance.now() - started };
}

async function pagedSync(server: TestServerHandle, roomId: string, token: string, acceptEncoding?: string) {
  let since = 0;
  let pages = 0, maxBytes = 0, maxMs = 0, totalBytes = 0;
  const started = performance.now();
  for (;;) {
    const res = await get(`${server.url}/rooms/${roomId}/sync?since=${since}&limit=${SYNC_PAGE}`, token, acceptEncoding);
    const plain = decoded(res);
    const next = (JSON.parse(plain.toString("utf8")) as { next: number | null }).next;
    pages++;
    totalBytes += res.bytes;
    maxBytes = Math.max(maxBytes, res.bytes);
    maxMs = Math.max(maxMs, res.ms);
    if (next === null) break;
    since = next;
  }
  return { pages, maxBytes, maxMs, totalBytes, totalMs: performance.now() - started };
}

/**
 * MEMBERS member links, each the room creator (an admin) adding a person, posted in batches of
 * MEMBERSHIP_BATCH: every link is judged by the membership core and stored.
 */
async function membershipRoom(server: TestServerHandle): Promise<void> {
  const alice = personAgent("alice");
  const roomId = await createRoom(server.url, { creator: alice, add: "admins" });
  const token = await authenticateAgent(server.url, roomId, alice);
  // Dated after the room's rules, one millisecond apart, so each link has its own place.
  const start = Date.now() + 1;
  const all = Array.from({ length: MEMBERS }, (_, i) =>
    signLink(alice, links.member(`did:scid:coasys:1:bench-member-${i}`), new Date(start + i).toISOString())
  );

  let maxBatchMs = 0;
  const postStarted = performance.now();
  for (let i = 0; i < all.length; i += MEMBERSHIP_BATCH) {
    const started = performance.now();
    const res = await postJson<{ results: Array<{ counted: boolean; reason?: string }> }>(
      `${server.url}/rooms/${roomId}/membership`,
      { links: all.slice(i, i + MEMBERSHIP_BATCH) },
      token
    );
    maxBatchMs = Math.max(maxBatchMs, performance.now() - started);
    if (res.status !== 200) throw new Error(`membership batch ${i}: ${res.status} ${JSON.stringify(res.body)}`);
    const refused = res.body.results.find((r) => !r.counted);
    if (refused) throw new Error(`membership batch ${i}: a link did not count (${refused.reason})`);
  }
  const postMs = performance.now() - postStarted;

  let since = 0, pages = 0, bytes = 0, read = 0;
  const readStarted = performance.now();
  for (;;) {
    const res = await get(`${server.url}/rooms/${roomId}/membership?since=${since}&limit=500`, token);
    const page = JSON.parse(res.body.toString("utf8")) as { links: unknown[]; seq: number };
    pages++;
    bytes += res.bytes;
    read += page.links.length;
    if (page.links.length < 500) break;
    since = page.seq;
  }
  const readMs = performance.now() - readStarted;

  // A cold replay, as the server builds it on first use after a restart.
  const cold = new MembershipRooms(server.built.db);
  const replayStarted = performance.now();
  const members = cold.members(roomId).length;
  const replayMs = performance.now() - replayStarted;

  const authStarted = performance.now();
  const auth = await authAs(server.url, roomId, alice);
  const authMs = performance.now() - authStarted;
  if (auth.status !== 200 || !auth.body.admitted) throw new Error(`auth: ${auth.status} ${JSON.stringify(auth.body)}`);

  if (members !== MEMBERS + 1) throw new Error(`replay: ${members} members, expected ${MEMBERS + 1}`);
  if (read !== MEMBERS + 1) throw new Error(`read back ${read} links, expected ${MEMBERS + 1} (rules + members)`);

  console.log(`\nmembership: ${MEMBERS} member links by the creator, posted ${MEMBERSHIP_BATCH} per request\n`);
  const header = ["post all", "per link", "max batch", "read all", "read pages", "read bytes", "cold replay", "members", "auth"];
  console.log(`| ${header.join(" | ")} |`);
  console.log(`|${header.map(() => "---").join("|")}|`);
  console.log(
    `| ${[
      ms(postMs), `${(postMs / MEMBERS).toFixed(2)} ms`, ms(maxBatchMs), ms(readMs), `${pages}`, kb(bytes),
      ms(replayMs), `${members}`, ms(authMs),
    ].join(" | ")} |`
  );
}

async function main(): Promise<void> {
  // A high rate limit: the bench sends far more requests than a member would.
  const server = await startTestServer({
    rateLimits: { roomJwt: { limit: 100_000, windowMs: 60_000 }, authIp: { limit: 100_000, windowMs: 60_000 } },
    autoAdmit: false,
  });
  try {
    console.log(
      `room: ${LINKS} encrypted links of ${plaintextLink(0).length} plaintext bytes, one per diff; ` +
      `render page ${RENDER_PAGE} links, sync page ${SYNC_PAGE} diffs\n`,
    );
    const rows: string[][] = [];
    const { roomId, token } = await seedRoom(server);
    for (const accept of [undefined, "gzip", "br"]) {
      const pr = await pagedRender(server, roomId, token, accept);
      const ps = await pagedSync(server, roomId, token, accept);
      rows.push([
        accept ?? "none",
        `${pr.pages}`, mb(pr.totalBytes), ms(pr.totalMs), kb(pr.maxBytes), ms(pr.maxMs),
        `${ps.pages}`, mb(ps.totalBytes), ms(ps.totalMs), kb(ps.maxBytes), ms(ps.maxMs),
      ]);
    }
    const header = [
      "accept-encoding",
      "render pages", "render bytes", "render time", "max render page", "max render page time",
      "sync pages", "sync bytes", "sync time", "max sync page", "max sync page time",
    ];
    console.log(`| ${header.join(" | ")} |`);
    console.log(`|${header.map(() => "---").join("|")}|`);
    for (const r of rows) console.log(`| ${r.join(" | ")} |`);
    await membershipRoom(server);
  } finally {
    await server.close();
  }
}

await main();
