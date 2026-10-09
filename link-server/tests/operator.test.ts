import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { randomBytes, randomUUID } from "node:crypto";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import { Writable } from "node:stream";
import { test } from "node:test";
import { fileURLToPath } from "node:url";
import { isLoopbackHost, readOperatorToken } from "../src/operator.js";
import { OPERATOR_PAGE_JS } from "../src/operator-page.js";
import {
  authenticateAgent,
  createTestAgent,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

const TOKEN = randomBytes(32).toString("hex");
const ORIGIN = "https://prs.ad4m.dev";
/** What nginx adds after the oauth2-proxy subrequest, plus what a same-origin browser POST carries. */
const SIGNED_IN = { "x-forwarded-user": "lucksus", origin: ORIGIN };
const here = path.dirname(fileURLToPath(import.meta.url));

interface OperatorHandle {
  server: TestServerHandle;
  opUrl: string;
  logs: string[];
}

async function withOperator(fn: (h: OperatorHandle) => Promise<void>): Promise<void> {
  const logs: string[] = [];
  const stream = new Writable({
    write(chunk, _enc, done) {
      logs.push(chunk.toString());
      done();
    },
  });
  const server = await startTestServer({
    operator: { token: TOKEN, allowedOrigins: [ORIGIN], label: "staging", logger: { level: "info", stream } },
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

test("operator API refuses requests without the right token", async () => {
  await withOperator(async ({ opUrl }) => {
    assert.equal((await fetch(`${opUrl}/api/rooms`)).status, 401);
    const wrong = await fetch(`${opUrl}/api/rooms`, { headers: { authorization: `Bearer ${"x".repeat(64)}` } });
    assert.equal(wrong.status, 401);
    const wrongPost = await fetch(`${opUrl}/api/rooms/r/acl`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ action: "add", did: "did:key:z6Mk" }),
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

test("operator lists rooms with their admin and member count", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const res = await op<{ rooms: Array<{ id: string; admin: string; memberCount: number }> }>(`${opUrl}/api/rooms`);
    assert.equal(res.status, 200);
    const room = res.body.rooms.find((r) => r.id === roomId);
    assert.ok(room, "room is listed");
    assert.equal(room.admin, admin.did);
    assert.equal(room.memberCount, 1);
  });
});

test("operator admits a DID, which can then authenticate in an allowlist room", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const joiner = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);
    await assert.rejects(() => authenticateAgent(server.url, roomId, joiner), /403/);

    const res = await op<{ admin: string; members: Array<{ did: string; hasX25519Key: boolean }> }>(
      `${opUrl}/api/rooms/${roomId}/acl`,
      { action: "add", did: joiner.did }
    );
    assert.equal(res.status, 200);
    assert.equal(res.body.admin, admin.did);
    assert.ok(res.body.members.some((m) => m.did === joiner.did && !m.hasX25519Key));

    const token = await authenticateAgent(server.url, roomId, joiner);
    const acl = await getJson<{ members: Array<{ did: string }> }>(`${server.url}/rooms/${roomId}/acl`, token);
    assert.equal(acl.status, 200);
    const detail = await op<{ members: Array<{ did: string; hasX25519Key: boolean }> }>(`${opUrl}/api/rooms/${roomId}`);
    assert.ok(detail.body.members.some((m) => m.did === joiner.did && m.hasX25519Key));
  });
});

test("operator removal ends the member's sessions and sockets at once", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, memberToken);
    const closed = new Promise<number>((resolve) => socket.once("close", (code) => resolve(code)));

    const res = await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "remove", did: member.did });
    assert.equal(res.status, 200);

    assert.equal(await closed, 4005);
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/sync`, memberToken)).status, 401);
    await assert.rejects(() => authenticateAgent(server.url, roomId, member), /403/);
  });
});

test("operator cannot remove the room admin, add a malformed DID or create rooms", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const someone = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    assert.equal((await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "remove", did: admin.did })).status, 400);
    assert.equal((await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: "did:key:nope" })).status, 400);
    assert.equal((await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "grant", did: someone.did })).status, 400);

    const unknown = randomUUID();
    assert.equal((await op(`${opUrl}/api/rooms/${unknown}/acl`, { action: "add", did: someone.did })).status, 404);
    assert.equal((await op(`${opUrl}/api/rooms/${unknown}`)).status, 404);
    assert.equal(server.built.db.getRoom(unknown), undefined);
  });
});

test("operator mutations refuse non-JSON bodies and cross-site requests", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const someone = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const form = await fetch(`${opUrl}/api/rooms/${roomId}/acl`, {
      method: "POST",
      headers: { authorization: `Bearer ${TOKEN}`, ...SIGNED_IN, "content-type": "text/plain" },
      body: JSON.stringify({ action: "add", did: someone.did }),
    });
    assert.equal(form.status, 415);
    const cross = await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: someone.did }, {
      "sec-fetch-site": "cross-site",
    });
    assert.equal(cross.status, 403);
    assert.equal(server.built.db.isMember(roomId, someone.did), false);
  });
});

test("the operator token never reaches the log; ACL changes are logged with the signed-in user", async () => {
  await withOperator(async ({ server, opUrl, logs }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const joiner = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    await fetch(`${opUrl}/api/rooms`, { headers: { authorization: "Bearer wrong" } });
    await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: joiner.did });
    const who = await op<{ operator: string; instance: string }>(`${opUrl}/api/whoami`);
    assert.equal(who.body.operator, "lucksus");
    assert.equal(who.body.instance, "staging");

    const text = logs.join("");
    assert.ok(text.length > 0, "the operator listener logged requests");
    assert.ok(!text.includes(TOKEN), "token must not appear in the log");
    const change = logs.map((l) => JSON.parse(l)).find((l) => l.msg === "operator acl change");
    assert.ok(change, "acl change is logged");
    assert.equal(change.operator, "lucksus");
    assert.equal(change.did, joiner.did);
    assert.equal(change.action, "add");
    assert.equal(change.instance, "staging");
  });
});

test("operator API refuses a request without a signed-in login", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const joiner = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const anonymousGet = await fetch(`${opUrl}/api/rooms`, { headers: { authorization: `Bearer ${TOKEN}` } });
    assert.equal(anonymousGet.status, 401);
    const anonymousPost = await fetch(`${opUrl}/api/rooms/${roomId}/acl`, {
      method: "POST",
      headers: { authorization: `Bearer ${TOKEN}`, origin: ORIGIN, "content-type": "application/json" },
      body: JSON.stringify({ action: "add", did: joiner.did }),
    });
    assert.equal(anonymousPost.status, 401);
    const malformed = await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: joiner.did }, {
      "x-forwarded-user": "a b<script>",
    });
    assert.equal(malformed.status, 401);
    assert.equal(server.built.db.isMember(roomId, joiner.did), false);
    assert.deepEqual(server.built.db.getAclChanges(roomId), []);
  });
});

test("every admit and remove is stored with the signed-in login, and the room admin's own changes with its DID", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const joiner = await createTestAgent();
    const other = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "add", did: joiner.did }, { "x-forwarded-user": "HexaField" });
    await op(`${opUrl}/api/rooms/${roomId}/acl`, { action: "remove", did: joiner.did }, { "x-forwarded-user": "jhweir" });
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: other.did }, adminToken);

    const stored = server.built.db.getAclChanges(roomId).reverse();
    assert.deepEqual(
      stored.map((c) => [c.action, c.did, c.actor, c.source]),
      [
        ["add", joiner.did, "HexaField", "operator"],
        ["remove", joiner.did, "jhweir", "operator"],
        ["add", other.did, admin.did, "room-admin"],
      ]
    );

    const detail = await op<{
      members: Array<{ did: string; addedBy: { actor: string; source: string } | null }>;
      history: Array<{ action: string; actor: string }>;
    }>(`${opUrl}/api/rooms/${roomId}`);
    assert.deepEqual(detail.body.members.find((m) => m.did === other.did)?.addedBy?.actor, admin.did);
    assert.equal(detail.body.members.find((m) => m.did === admin.did)?.addedBy, null);
    assert.deepEqual(detail.body.history.map((h) => [h.action, h.actor]), [
      ["add", admin.did],
      ["remove", "jhweir"],
      ["add", "HexaField"],
    ]);
  });
});

test("state changes need an Origin, or a Referer, from the admin page", async () => {
  await withOperator(async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const a = await createTestAgent();
    const b = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);
    const post = (did: string, headers: Record<string, string>) =>
      fetch(`${opUrl}/api/rooms/${roomId}/acl`, {
        method: "POST",
        headers: {
          authorization: `Bearer ${TOKEN}`,
          "x-forwarded-user": "lucksus",
          "content-type": "application/json",
          ...headers,
        },
        body: JSON.stringify({ action: "add", did }),
      });

    // Refused: no Origin and no Referer, a foreign Origin, a foreign Referer, an opaque Origin.
    assert.equal((await post(a.did, {})).status, 403);
    assert.equal((await post(a.did, { origin: "https://evil.example" })).status, 403);
    assert.equal((await post(a.did, { origin: "https://prs.ad4m.dev.evil.example" })).status, 403);
    assert.equal((await post(a.did, { referer: "https://evil.example/link-admin/staging/" })).status, 403);
    assert.equal((await post(a.did, { origin: "null" })).status, 403);
    // A foreign Origin is not rescued by a good Referer.
    assert.equal((await post(a.did, { origin: "https://evil.example", referer: `${ORIGIN}/link-admin/staging/` })).status, 403);
    assert.equal(server.built.db.isMember(roomId, a.did), false);

    // Accepted: the page's Origin, or (no Origin) the page as Referer.
    assert.equal((await post(a.did, { origin: ORIGIN })).status, 200);
    assert.equal((await post(b.did, { referer: `${ORIGIN}/link-admin/staging/` })).status, 200);
    assert.equal(server.built.db.isMember(roomId, a.did), true);
    assert.equal(server.built.db.isMember(roomId, b.did), true);
  });
});

test("the admission page is served with a strict CSP and builds no HTML from data", async () => {
  await withOperator(async ({ opUrl }) => {
    const page = await fetch(`${opUrl}/`);
    assert.equal(page.status, 200);
    assert.match(page.headers.get("content-security-policy") ?? "", /default-src 'self'/);
    assert.match(await page.text(), /<script src="admin.js"><\/script>/);
    const js = await fetch(`${opUrl}/admin.js`);
    assert.equal(js.status, 200);
    assert.match(js.headers.get("content-type") ?? "", /javascript/);
  });
  assert.doesNotMatch(OPERATOR_PAGE_JS, /innerHTML|outerHTML|insertAdjacentHTML|document\.write/);
  // Relative paths, so the page also works mounted under a path prefix.
  assert.doesNotMatch(OPERATOR_PAGE_JS, /fetch\("\//);
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
  const noOrigin = run(["--port", "0", "--operator-port", "0", "--operator-token-file", "/nonexistent"]);
  assert.equal(noOrigin.status, 1);
  assert.match(noOrigin.stderr, /needs --operator-origin/);
  const noToken = run(["--port", "0", "--operator-port", "0"]);
  assert.equal(noToken.status, 1);
  assert.match(noToken.stderr, /needs --operator-token-file/);
  const exposed = run([
    "--port", "0", "--operator-port", "0", "--operator-token-file", "/nonexistent",
    "--operator-origin", ORIGIN, "--operator-host", "0.0.0.0",
  ]);
  assert.equal(exposed.status, 1);
  assert.match(exposed.stderr, /must be a loopback address/);
});
