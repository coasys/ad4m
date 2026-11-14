import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import { Writable } from "node:stream";
import { test } from "node:test";
import { fileURLToPath } from "node:url";
import { isLoopbackHost, readOperatorToken } from "../src/operator.js";
import { OPERATOR_PAGE_HTML, OPERATOR_PAGE_JS } from "../src/operator-page.js";
import type { ServerOptions } from "../src/server.js";
import {
  authAs,
  authenticateAgent,
  createRoom,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import { links, newSalt, personAgent, personDid, roomIdOf, signLink } from "./identity.js";

const TOKEN = randomBytes(32).toString("hex");
const ORIGIN = "https://prs.ad4m.dev";
/** What nginx adds after the oauth2-proxy subrequest, plus what a same-origin browser POST carries. */
const SIGNED_IN = { "x-forwarded-user": "lucksus", origin: ORIGIN };
const here = path.dirname(fileURLToPath(import.meta.url));

const alice = personAgent("alice");
const bob = personAgent("bob");

interface OperatorHandle {
  server: TestServerHandle;
  opUrl: string;
  logs: string[];
}

/** A membership server (no auto-admit) with the operator listener on. */
async function withOperator(fn: (h: OperatorHandle) => Promise<void>, opts: Partial<ServerOptions> = {}): Promise<void> {
  const logs: string[] = [];
  const stream = new Writable({
    write(chunk, _enc, done) {
      logs.push(chunk.toString());
      done();
    },
  });
  const server = await startTestServer({
    autoAdmit: false,
    operator: { token: TOKEN, allowedOrigins: [ORIGIN], label: "staging", logger: { level: "info", stream } },
    ...opts,
  });
  try {
    const opApp = server.built.operatorApp!;
    await opApp.listen({ port: 0, host: "127.0.0.1" });
    const opUrl = `http://127.0.0.1:${(opApp.server.address() as AddressInfo).port}`;
    await fn({ server, opUrl, logs });
  } finally {
    await server.close();
  }
}

function op<T = any>(url: string, body?: unknown, headers: Record<string, string> = {}) {
  return fetch(url, {
    method: body === undefined ? "GET" : "POST",
    headers: {
      authorization: `Bearer ${TOKEN}`,
      ...SIGNED_IN,
      ...(body === undefined ? {} : { "content-type": "application/json" }),
      ...headers,
    },
    body: body === undefined ? undefined : JSON.stringify(body),
  }).then(async (res) => ({ status: res.status, body: (await res.json().catch(() => undefined)) as T }));
}

/** alice's room, bob joined: two members. */
async function roomWithBob(server: TestServerHandle) {
  const roomId = await createRoom(server.url, { join: "open" });
  const aliceToken = await authenticateAgent(server.url, roomId, alice);
  const bobToken = (await authAs(server.url, roomId, bob)).body.token!;
  const joined = await postJson(
    `${server.url}/rooms/${roomId}/membership`,
    { links: [signLink(bob, links.member(personDid("bob")))] },
    bobToken
  );
  assert.equal(joined.status, 200, JSON.stringify(joined.body));
  return { roomId, aliceToken, bobToken };
}

test("operator API refuses requests without the right token", async () => {
  await withOperator(async ({ opUrl }) => {
    assert.equal((await fetch(`${opUrl}/api/rooms`)).status, 401);
    const wrong = await fetch(`${opUrl}/api/rooms`, { headers: { authorization: `Bearer ${"x".repeat(64)}` } });
    assert.equal(wrong.status, 401);
    const wrongPost = await fetch(`${opUrl}/api/rooms/r/suspend`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: "{}",
    });
    assert.equal(wrongPost.status, 401);
    assert.equal((await op(`${opUrl}/api/rooms`)).status, 200);
  });
});

test("operator API is not served on the public listener", async () => {
  await withOperator(async ({ server }) => {
    const res = await fetch(`${server.url}/api/rooms`, { headers: { authorization: `Bearer ${TOKEN}` } });
    assert.equal(res.status, 404);
  });
});

test("operator lists rooms with their creator, state and member count", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const { roomId } = await roomWithBob(server);

    const res = await op<{
      rooms: Array<{ id: string; creator: string; legacy: boolean; suspended: boolean; memberCount: number }>;
    }>(`${opUrl}/api/rooms`);
    assert.equal(res.status, 200);
    const room = res.body.rooms.find((r) => r.id === roomId);
    assert.ok(room, "room is listed");
    assert.equal(room.creator, personDid("alice"));
    assert.equal(room.legacy, false);
    assert.equal(room.suspended, false);
    assert.equal(room.memberCount, 2);
  });
});

test("operator room detail shows the members from the membership and each key with its standing", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const { roomId, bobToken } = await roomWithBob(server);
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, bobToken);
    // A key that signed in but is no member.
    const frank = personAgent("frank");
    await authAs(server.url, roomId, frank);

    const detail = await op<{
      members: Array<{ person: string; role: string; online: boolean }>;
      keys: Array<{ did: string; person: string; admitted: boolean; online: boolean; revoked: boolean }>;
    }>(`${opUrl}/api/rooms/${roomId}`);
    assert.equal(detail.status, 200);
    assert.deepEqual(
      detail.body.members.map((m) => [m.person, m.role, m.online]),
      [
        [personDid("alice"), "admin", false],
        [personDid("bob"), "member", true],
      ]
    );
    const keys = new Map(detail.body.keys.map((k) => [k.did, k]));
    assert.deepEqual(
      [alice.did, bob.did, frank.did].map((d) => [keys.get(d)?.admitted, keys.get(d)?.online, keys.get(d)?.revoked]),
      [
        [true, false, false],
        [true, true, false],
        [false, false, false],
      ]
    );
    assert.equal(keys.get(frank.did)?.person, personDid("frank"));
    socket.close();
  });
});

test("suspending a room closes its sockets (4006) and refuses it (423) until resumed", async () => {
  await withOperator(async ({ server, opUrl, logs }) => {
    const { roomId, aliceToken, bobToken } = await roomWithBob(server);
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, bobToken);
    const closed = new Promise<number>((resolve) => socket.once("close", (code) => resolve(code)));

    const res = await op<{ suspended: boolean }>(`${opUrl}/api/rooms/${roomId}/suspend`, {});
    assert.equal(res.status, 200);
    assert.equal(res.body.suspended, true);
    assert.equal(await closed, 4006);
    // The sessions are gone, and a new auth is refused while the room is suspended.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/sync`, aliceToken)).status, 401);
    assert.equal((await authAs(server.url, roomId, alice)).status, 423);
    // Suspending again changes nothing and logs nothing.
    assert.equal((await op(`${opUrl}/api/rooms/${roomId}/suspend`, {})).status, 200);
    const suspendLines = () => logs.filter((l) => l.includes('"msg":"operator suspend"')).length;
    assert.equal(suspendLines(), 1);

    const resumed = await op<{ suspended: boolean }>(`${opUrl}/api/rooms/${roomId}/resume`, {});
    assert.equal(resumed.status, 200);
    assert.equal(resumed.body.suspended, false);
    const back = await authAs(server.url, roomId, alice);
    assert.equal(back.status, 200);
    assert.equal(back.body.admitted, true, "the membership is untouched by a suspension");
  });
});

test("the operator manages no membership: the ACL route is gone and an unknown room is 404", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = await createRoom(server.url);
    assert.equal((await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: bob.did })).status, 404);

    const unknown = roomIdOf(personDid("alice"), newSalt());
    assert.equal((await op(`${opUrl}/api/rooms/${unknown}`)).status, 404);
    assert.equal((await op(`${opUrl}/api/rooms/${unknown}/suspend`, {})).status, 404);
    assert.equal(server.built.db.getRoom(unknown), undefined, "the operator creates no room");
  });
});

test("operator mutations refuse non-JSON bodies and cross-site requests", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = await createRoom(server.url);

    const form = await fetch(`${opUrl}/api/rooms/${roomId}/suspend`, {
      method: "POST",
      headers: { authorization: `Bearer ${TOKEN}`, ...SIGNED_IN, "content-type": "text/plain" },
      body: "{}",
    });
    assert.equal(form.status, 415);
    const cross = await op(`${opUrl}/api/rooms/${roomId}/suspend`, {}, { "sec-fetch-site": "cross-site" });
    assert.equal(cross.status, 403);
    assert.equal(server.built.db.getRoom(roomId)?.suspended, 0);
  });
});

test("the operator token never reaches the log; suspensions are logged with the signed-in user", async () => {
  await withOperator(async ({ server, opUrl, logs }) => {
    const roomId = await createRoom(server.url);

    await fetch(`${opUrl}/api/rooms`, { headers: { authorization: "Bearer wrong" } });
    await op(`${opUrl}/api/rooms/${roomId}/suspend`, {});
    const who = await op<{ operator: string; instance: string; admission: string }>(`${opUrl}/api/whoami`);
    assert.equal(who.body.operator, "lucksus");
    assert.equal(who.body.instance, "staging");
    assert.equal(who.body.admission, "membership");

    const text = logs.join("");
    assert.ok(text.length > 0, "the operator listener logged requests");
    assert.ok(!text.includes(TOKEN), "token must not appear in the log");
    const change = logs.map((l) => JSON.parse(l)).find((l) => l.msg === "operator suspend");
    assert.ok(change, "the suspension is logged");
    assert.equal(change.operator, "lucksus");
    assert.equal(change.roomId, roomId);
    assert.equal(change.action, "suspend");
    assert.equal(change.instance, "staging");
  });
});

test("operator API refuses a request without a signed-in login", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = await createRoom(server.url);

    const anonymousGet = await fetch(`${opUrl}/api/rooms`, { headers: { authorization: `Bearer ${TOKEN}` } });
    assert.equal(anonymousGet.status, 401);
    const anonymousPost = await fetch(`${opUrl}/api/rooms/${roomId}/suspend`, {
      method: "POST",
      headers: { authorization: `Bearer ${TOKEN}`, origin: ORIGIN, "content-type": "application/json" },
      body: "{}",
    });
    assert.equal(anonymousPost.status, 401);
    const malformed = await op(`${opUrl}/api/rooms/${roomId}/suspend`, {}, { "x-forwarded-user": "a b<script>" });
    assert.equal(malformed.status, 401);
    assert.equal(server.built.db.getRoom(roomId)?.suspended, 0);
  });
});

test("state changes need an Origin, or a Referer, from the admin page", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = await createRoom(server.url);
    const post = (action: "suspend" | "resume", headers: Record<string, string>) =>
      fetch(`${opUrl}/api/rooms/${roomId}/${action}`, {
        method: "POST",
        headers: {
          authorization: `Bearer ${TOKEN}`,
          "x-forwarded-user": "lucksus",
          "content-type": "application/json",
          ...headers,
        },
        body: "{}",
      });
    const suspended = () => server.built.db.getRoom(roomId)?.suspended;

    // Refused: no Origin and no Referer, a foreign Origin, a foreign Referer, an opaque Origin.
    assert.equal((await post("suspend", {})).status, 403);
    assert.equal((await post("suspend", { origin: "https://evil.example" })).status, 403);
    assert.equal((await post("suspend", { origin: "https://prs.ad4m.dev.evil.example" })).status, 403);
    assert.equal((await post("suspend", { referer: "https://evil.example/link-admin/staging/" })).status, 403);
    assert.equal((await post("suspend", { origin: "null" })).status, 403);
    assert.equal((await post("suspend", { origin: "null", referer: `${ORIGIN}/link-admin/staging/` })).status, 403);
    // A foreign Origin is not rescued by a good Referer.
    assert.equal(
      (await post("suspend", { origin: "https://evil.example", referer: `${ORIGIN}/link-admin/staging/` })).status,
      403
    );
    assert.equal(suspended(), 0);

    // Accepted: the page's Origin, or (no Origin) the page as Referer.
    assert.equal((await post("suspend", { origin: ORIGIN })).status, 200);
    assert.equal(suspended(), 1);
    assert.equal((await post("resume", { referer: `${ORIGIN}/link-admin/staging/` })).status, 200);
    assert.equal(suspended(), 0);
  });
});

test("the operator page is served with a strict CSP and builds no HTML from data", async () => {
  await withOperator(async ({ opUrl }) => {
    const page = await fetch(`${opUrl}/`);
    assert.equal(page.status, 200);
    assert.match(page.headers.get("content-security-policy") ?? "", /default-src 'self'/);
    assert.match(await page.text(), /<script src="admin.js"><\/script>/);
    const js = await fetch(`${opUrl}/admin.js`);
    assert.equal(js.status, 200);
    assert.match(js.headers.get("content-type") ?? "", /javascript/);
  });
});

test("the page's styles come from admin.css on the same origin, so the strict CSP does not block them", async () => {
  await withOperator(async ({ opUrl }) => {
    const page = await fetch(`${opUrl}/`);
    const csp = page.headers.get("content-security-policy") ?? "";
    assert.doesNotMatch(csp, /unsafe-inline/);
    const html = await page.text();
    // Relative href: works under /link-admin/<instance>/.
    assert.match(html, /<link rel="stylesheet" href="admin.css">/);
    const css = await fetch(`${opUrl}/admin.css`);
    assert.equal(css.status, 200);
    assert.match(css.headers.get("content-type") ?? "", /^text\/css/);
    assert.match(await css.text(), /font: 15px/);
  });
  // CSP default-src 'self' blocks inline styles; keep them out of the page and the script.
  assert.doesNotMatch(OPERATOR_PAGE_HTML, /<style|style=/);
  assert.doesNotMatch(OPERATOR_PAGE_JS, /\.style\b|setAttribute\("style"/);
});

test("the operator page builds no HTML from data, uses relative URLs and calls no ACL route", () => {
  assert.doesNotMatch(OPERATOR_PAGE_JS, /innerHTML|outerHTML|insertAdjacentHTML|document\.write/);
  // Relative paths, so the page also works mounted under a path prefix.
  assert.doesNotMatch(OPERATOR_PAGE_JS, /fetch\("\//);
  assert.doesNotMatch(OPERATOR_PAGE_JS, /\/acl\b/);
});

test("readOperatorToken requires an owner-only file and a long token", () => {
  const dir = mkdtempSync(path.join(tmpdir(), "link-server-op-"));
  try {
    const file = path.join(dir, "token");
    writeFileSync(file, `${TOKEN}\n`);
    chmodSync(file, 0o644);
    assert.throws(() => readOperatorToken(file), /mode 644/);
    chmodSync(file, 0o600);
    assert.equal(readOperatorToken(file), TOKEN);
    writeFileSync(file, "short\n");
    assert.throws(() => readOperatorToken(file), (err: Error) => /shorter/.test(err.message) && !err.message.includes("short\n"));
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test("isLoopbackHost accepts loopback names only", () => {
  for (const host of ["127.0.0.1", "::1", "localhost"]) assert.equal(isLoopbackHost(host), true);
  for (const host of ["0.0.0.0", "::", "192.0.2.1", "example.org"]) assert.equal(isLoopbackHost(host), false);
});

test("CLI refuses an operator port without a token file, or on a non-loopback address", () => {
  const run = (args: string[]) =>
    spawnSync(process.execPath, ["--import", "tsx", path.join(here, "../src/index.ts"), ...args], {
      encoding: "utf8",
      env: { ...process.env, OPERATOR_TOKEN_FILE: "", OPERATOR_PORT: "", OPERATOR_ORIGINS: "" },
      timeout: 30_000,
    });
  const ephemeral = run([
    "--port", "0", "--operator-port", "0", "--operator-token-file", "/nonexistent", "--operator-origin", ORIGIN,
  ]);
  assert.equal(ephemeral.status, 1);
  assert.match(ephemeral.stderr, /Invalid --operator-port value: 0/);
  const noOrigin = run(["--port", "0", "--operator-port", "13999", "--operator-token-file", "/nonexistent"]);
  assert.equal(noOrigin.status, 1);
  assert.match(noOrigin.stderr, /needs --operator-origin/);
  const noToken = run(["--port", "0", "--operator-port", "13999"]);
  assert.equal(noToken.status, 1);
  assert.match(noToken.stderr, /needs --operator-token-file/);
  const exposed = run([
    "--port", "0", "--operator-port", "13999", "--operator-token-file", "/nonexistent",
    "--operator-origin", ORIGIN, "--operator-host", "0.0.0.0",
  ]);
  assert.equal(exposed.status, 1);
  assert.match(exposed.stderr, /must be a loopback address/);
});
