/**
 * Large-room benchmark for /render and /sync: response bytes with and
 * without compression, hex vs base64 ciphertext, and the largest page and
 * slowest request when paged.
 *
 *   npm run bench                 # 30 000 links, one link per diff
 *   LINKS=10000 npm run bench
 *
 * Each link is a real AES-256-GCM encryption of a ~575-byte link JSON (the
 * mean size of a team-room link), written as the language writes it: hex
 * before PERF-48, base64 after. Requests go over loopback HTTP, so times
 * are the server's work plus local transfer, not a WAN round trip.
 */
import { createCipheriv, createHash, randomBytes } from "node:crypto";
import { request } from "node:http";
import { performance } from "node:perf_hooks";
import { brotliDecompressSync, gunzipSync } from "node:zlib";
import { authenticateAgent, createTestAgent, startTestServer, type TestServerHandle } from "../tests/helpers.js";

const LINKS = Number(process.env.LINKS ?? 30_000);
const RENDER_PAGE = Number(process.env.RENDER_PAGE ?? 500);
const SYNC_PAGE = Number(process.env.SYNC_PAGE ?? 100);
/** Mean decrypted link size in the team room (DEFECTS PERF-48). */
const PLAINTEXT_BYTES = 575;

type Encoding = "hex" | "base64";

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

function encryptedWireLink(i: number, key: Buffer, encoding: Encoding) {
  const nonce = randomBytes(12);
  const cipher = createCipheriv("aes-256-gcm", key, nonce);
  const ct = Buffer.concat([cipher.update(plaintextLink(i), "utf8"), cipher.final(), cipher.getAuthTag()]);
  return {
    data: { ciphertext: ct.toString(encoding), nonce: nonce.toString(encoding) },
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

async function seedRoom(server: TestServerHandle, encoding: Encoding): Promise<{ roomId: string; token: string }> {
  const roomId = `bench-${encoding}`;
  const agent = await createTestAgent();
  const token = await authenticateAgent(server.url, roomId, agent);
  const key = randomBytes(32);
  const db = server.built.db;
  const seed = db.raw.transaction(() => {
    for (let i = 0; i < LINKS; i++) {
      db.applyDiffAndAppend(roomId, { additions: [encryptedWireLink(i, key, encoding)], removals: [] }, agent.did);
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

async function main(): Promise<void> {
  // A high rate limit: the bench sends far more requests than a member would.
  // maxDiffsPerRoom 0 keeps the whole diff log (the default prunes to 10 000, BUG-118).
  const server = await startTestServer({
    rateLimits: { roomJwt: { limit: 100_000, windowMs: 60_000 } },
    maxDiffsPerRoom: 0,
  });
  try {
    console.log(
      `room: ${LINKS} encrypted links of ${plaintextLink(0).length} plaintext bytes, one per diff; ` +
      `render page ${RENDER_PAGE} links, sync page ${SYNC_PAGE} diffs\n`,
    );
    const rows: string[][] = [];
    for (const encoding of ["hex", "base64"] as const) {
      const { roomId, token } = await seedRoom(server, encoding);
      for (const accept of [undefined, "gzip", "br"]) {
        const label = accept ?? "none";
        const render = await get(`${server.url}/rooms/${roomId}/render`, token, accept);
        const sync = await get(`${server.url}/rooms/${roomId}/sync?since=0`, token, accept);
        const pr = await pagedRender(server, roomId, token, accept);
        const ps = await pagedSync(server, roomId, token, accept);
        rows.push([
          encoding, label,
          mb(render.bytes), ms(render.ms),
          mb(sync.bytes), ms(sync.ms),
          `${pr.pages}`, kb(pr.maxBytes), ms(pr.maxMs), ms(pr.totalMs),
          `${ps.pages}`, kb(ps.maxBytes), ms(ps.maxMs),
        ]);
      }
    }
    const header = [
      "ciphertext", "accept-encoding",
      "/render bytes", "/render time",
      "/sync?since=0 bytes", "/sync time",
      "render pages", "max page bytes", "max page time", "all pages time",
      "sync pages", "max sync page bytes", "max sync page time",
    ];
    console.log(`| ${header.join(" | ")} |`);
    console.log(`|${header.map(() => "---").join("|")}|`);
    for (const r of rows) console.log(`| ${r.join(" | ")} |`);
  } finally {
    await server.close();
  }
}

await main();
