import assert from "node:assert/strict";
import { randomBytes, randomUUID } from "node:crypto";
import type { AddressInfo } from "node:net";
import { test } from "node:test";
import type { ServerOptions } from "../src/server.js";
import { MAX_JOIN_REQUESTS_PER_ROOM } from "../src/db.js";
import {
  authenticateAgent,
  createTestAgent,
  postJson,
  signChallenge,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

const TOKEN = randomBytes(32).toString("hex");
const ORIGIN = "https://link-admin.example.org";
const SIGNED_IN = { authorization: `Bearer ${TOKEN}`, "x-forwarded-user": "lucksus" };

async function withServer(
  opts: Partial<ServerOptions>,
  fn: (h: { server: TestServerHandle; opUrl: string }) => Promise<void>
): Promise<void> {
  const server = await startTestServer({ ...opts, operator: { token: TOKEN, allowedOrigins: [ORIGIN] } });
  try {
    const opApp = server.built.operatorApp!;
    await opApp.listen({ port: 0, host: "127.0.0.1" });
    const opUrl = `http://127.0.0.1:${(opApp.server.address() as AddressInfo).port}`;
    await fn({ server, opUrl });
  } finally {
    await server.close();
  }
}

async function get<T = any>(url: string): Promise<T> {
  const res = await fetch(url, { headers: SIGNED_IN });
  assert.equal(res.status, 200, `${url}: ${res.status}`);
  return (await res.json()) as T;
}

/** Runs the challenge-response for `agent`; returns the HTTP status of step 2. */
async function tryJoin(serverUrl: string, roomId: string, agent: Awaited<ReturnType<typeof createTestAgent>>) {
  const step1 = await postJson<{ challenge: string }>(`${serverUrl}/rooms/${roomId}/auth`, { did: agent.did });
  const signature = await signChallenge(agent, step1.body.challenge);
  const step2 = await postJson(`${serverUrl}/rooms/${roomId}/auth`, {
    did: agent.did,
    challenge: step1.body.challenge,
    signature,
  });
  return step2.status;
}

test("whoami reports the admission mode: allowlist by default, auto with autoAdmit", async () => {
  await withServer({}, async ({ opUrl }) => {
    assert.equal((await get(`${opUrl}/api/whoami`)).admission, "allowlist");
  });
  await withServer({ autoAdmit: true }, async ({ opUrl }) => {
    assert.equal((await get(`${opUrl}/api/whoami`)).admission, "auto");
  });
});

test("the server logs its admission mode at startup", async () => {
  const lines: string[] = [];
  await withServer({ logger: true, logDestination: { write: (l: string) => lines.push(l) } }, async ({ server }) => {
    const entry = server.built.logTail!.since().find((e) => e.msg.startsWith("admission mode"));
    assert.ok(entry, "an admission mode line");
    assert.equal(entry.admission, "allowlist");
  });
});

test("a refused join waits for the operator, who can admit it; the request then goes", async () => {
  await withServer({}, async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const joiner = await createTestAgent();
    assert.equal(await tryJoin(server.url, roomId, joiner), 403);
    assert.equal(await tryJoin(server.url, roomId, joiner), 403);

    const detail = await get(`${opUrl}/api/rooms/${roomId}`);
    assert.equal(detail.pending.length, 1);
    assert.equal(detail.pending[0].did, joiner.did);
    assert.equal(detail.pending[0].attempts, 2);
    assert.ok(detail.pending[0].firstSeen && detail.pending[0].lastSeen);
    const rooms = await get(`${opUrl}/api/rooms`);
    assert.equal(rooms.rooms.find((r: any) => r.id === roomId).pendingCount, 1);

    const added = await fetch(`${opUrl}/api/rooms/${roomId}/acl`, {
      method: "POST",
      headers: { ...SIGNED_IN, origin: ORIGIN, "content-type": "application/json" },
      body: JSON.stringify({ action: "add", did: joiner.did }),
    });
    assert.equal(added.status, 200);
    assert.deepEqual((await added.json()).pending, []);
    assert.equal(await tryJoin(server.url, roomId, joiner), 200);
  });
});

test("a join with a bad signature, or to a room the server does not have, is not recorded", async () => {
  await withServer({}, async ({ server, opUrl }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);
    const stranger = await createTestAgent();

    const step1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, { did: stranger.did });
    const bad = await postJson(`${server.url}/rooms/${roomId}/auth`, {
      did: stranger.did,
      challenge: step1.body.challenge,
      signature: "00".repeat(64),
    });
    assert.equal(bad.status, 401);
    assert.deepEqual((await get(`${opUrl}/api/rooms/${roomId}`)).pending, []);
  });
});

test(`at most ${MAX_JOIN_REQUESTS_PER_ROOM} requests wait per room; the oldest go first`, async () => {
  await withServer({}, async ({ server }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);
    const db = server.built.db;
    for (let i = 0; i < MAX_JOIN_REQUESTS_PER_ROOM + 5; i++) db.recordJoinRequest(roomId, `did:key:z6Mk${i}`);
    const pending = db.getJoinRequests(roomId);
    assert.equal(pending.length, MAX_JOIN_REQUESTS_PER_ROOM);
    assert.ok(!pending.some((p) => p.did === "did:key:z6Mk0"), "the oldest was dropped");
  });
});

test("with autoAdmit nothing waits: the join is admitted", async () => {
  await withServer({ autoAdmit: true }, async ({ server, opUrl }) => {
    const roomId = randomUUID();
    await authenticateAgent(server.url, roomId, await createTestAgent());
    assert.equal(await tryJoin(server.url, roomId, await createTestAgent()), 200);
    assert.deepEqual((await get(`${opUrl}/api/rooms/${roomId}`)).pending, []);
  });
});
