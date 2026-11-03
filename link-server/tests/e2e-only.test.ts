/**
 * Rooms are end-to-end encrypted from creation.
 *
 * The current server link language asks for an E2E room in the auth call
 * that creates it (`e2e: true`), so the room refuses plaintext before any
 * key exists: no member can commit a plaintext link while the admin has not
 * rotated in the first key yet. Rooms older builds created (no flag) keep
 * the legacy contract: plaintext until their admin's first rotation.
 */

import assert from "node:assert/strict";
import { randomBytes, randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  authenticateAgent,
  clientSideRotate,
  createSignedLink,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  type TestAgent,
  type TestServerHandle,
} from "./helpers.js";
import type { LinkExpression } from "../src/types.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

function encryptedLink(): LinkExpression {
  return {
    data: { ciphertext: randomBytes(48).toString("hex"), nonce: randomBytes(12).toString("hex") },
    link_hash: randomBytes(32).toString("hex"),
    key_version: 1,
  };
}

async function plaintextCommit(server: TestServerHandle, roomId: string, agent: TestAgent, token: string, source: string) {
  const link = await createSignedLink(agent, { source, predicate: "test://p", target: "test://t" });
  return postJson<{ error?: string; sequence?: number }>(
    `${server.url}/rooms/${roomId}/commit`,
    { additions: [link], removals: [] },
    token
  );
}

/** Admin creates the room, admits `member`, and both sign in. */
async function roomWithMember(server: TestServerHandle, opts: { e2e: boolean }) {
  const roomId = randomUUID();
  const admin = await createTestAgent();
  const member = await createTestAgent();
  const adminToken = await authenticateAgent(server.url, roomId, admin, { e2e: opts.e2e });
  const acl = await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
  assert.equal(acl.status, 200);
  const memberToken = await authenticateAgent(server.url, roomId, member, { e2e: opts.e2e });
  return { roomId, admin, member, adminToken, memberToken };
}

test("a room created with e2e refuses plaintext from its first commit, before any key exists", async () => {
  await withServer(async (server) => {
    const { roomId, admin, member, adminToken, memberToken } = await roomWithMember(server, { e2e: true });

    // No rotation yet: the window in which members used to commit plaintext.
    const fromMember = await plaintextCommit(server, roomId, member, memberToken, "test://member-first");
    assert.equal(fromMember.status, 400, "a member's plaintext commit before the admin's rotation is refused");
    const fromAdmin = await plaintextCommit(server, roomId, admin, adminToken, "test://admin-first");
    assert.equal(fromAdmin.status, 400, "the admin's plaintext commit is refused too");

    const render = await getJson<{ links: unknown[] }>(`${server.url}/rooms/${roomId}/render`, memberToken);
    assert.deepEqual(render.body.links, [], "nothing plaintext was stored");
    const sync = await getJson<{ diffs: unknown[] }>(`${server.url}/rooms/${roomId}/sync?since=0`, memberToken);
    assert.deepEqual(sync.body.diffs, []);
  });
});

test("a room created with e2e tells a member without a key to wait, and accepts encrypted commits", async () => {
  await withServer(async (server) => {
    const { roomId, adminToken, memberToken } = await roomWithMember(server, { e2e: true });

    const before = await getJson<{ keys: unknown[]; e2e_enabled: boolean }>(`${server.url}/rooms/${roomId}/keys`, memberToken);
    assert.equal(before.status, 200, "an E2E room answers 200 with no keys, not the legacy 404");
    assert.deepEqual(before.body, { keys: [], e2e_enabled: true });

    const { version } = await clientSideRotate(server.url, roomId, adminToken);
    assert.equal(version, 1, "the admin's first rotation is key version 1");

    const commit = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [encryptedLink()], removals: [] },
      memberToken
    );
    assert.equal(commit.status, 200);
    assert.equal(commit.body.sequence, 1);
  });
});

test("the e2e flag applies only to the auth call that creates the room", async () => {
  await withServer(async (server) => {
    // An older build created the room; a client sending the flag later does not convert it.
    const { roomId, member, memberToken } = await roomWithMember(server, { e2e: false });
    assert.equal(server.built.db.getRoom(roomId)!.e2e_enabled, 0);
    const commit = await plaintextCommit(server, roomId, member, memberToken, "test://legacy");
    assert.equal(commit.status, 200);

    const fresh = randomUUID();
    await authenticateAgent(server.url, fresh, await createTestAgent(), { e2e: true });
    assert.equal(server.built.db.getRoom(fresh)!.e2e_enabled, 1, "the creating call's flag makes the room E2E");
  });
});

test("a room an older build created keeps the legacy contract: plaintext until its admin rotates", async () => {
  await withServer(async (server) => {
    const { roomId, admin, member, adminToken, memberToken } = await roomWithMember(server, { e2e: false });

    const keys = await getJson(`${server.url}/rooms/${roomId}/keys`, memberToken);
    assert.equal(keys.status, 404, "an old room without keys still answers 404, as old builds expect");

    assert.equal((await plaintextCommit(server, roomId, admin, adminToken, "test://a")).status, 200);
    assert.equal((await plaintextCommit(server, roomId, member, memberToken, "test://b")).status, 200);

    const render = await getJson<{ links: Array<LinkExpression> }>(`${server.url}/rooms/${roomId}/render`, memberToken);
    const sources = render.body.links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://a", "test://b"], "plaintext links are served as stored");
    const sync = await getJson<{ diffs: unknown[] }>(`${server.url}/rooms/${roomId}/sync?since=0`, memberToken);
    assert.equal(sync.body.diffs.length, 2);

    await clientSideRotate(server.url, roomId, adminToken);
    assert.equal((await plaintextCommit(server, roomId, member, memberToken, "test://c")).status, 400,
      "after the admin's rotation the old room refuses plaintext, as before");
  });
});

test("auth refuses an e2e flag that is not a boolean", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const step1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, { did: agent.did });
    const { signHex, hashMessageForVerify } = await import("../src/auth.js");
    const signature = await signHex(agent.privateKey, hashMessageForVerify(step1.body.challenge));
    const res = await postJson(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
      challenge: step1.body.challenge,
      signature,
      e2e: "yes",
    });
    assert.equal(res.status, 400);
    assert.equal(server.built.db.getRoom(roomId), undefined, "no room is created from a malformed request");
  });
});
