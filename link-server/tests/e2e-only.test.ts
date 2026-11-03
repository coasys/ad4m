/**
 * Every room is end-to-end encrypted from creation.
 *
 * The commit route refuses plaintext links in every room, before any key
 * exists: no member can commit a plaintext link while the admin has not
 * rotated in the first key yet. `GET /keys` answers a member without a key
 * with an empty list, never a "no E2E" 404.
 */

import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { test } from "node:test";
import Database from "better-sqlite3";
import { LinkServerDB } from "../src/db.js";
import {
  authenticateAgent,
  clientSideRotate,
  createSignedLink,
  createTestAgent,
  getJson,
  openLink,
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

/** A plaintext link, as no current client sends one: author, timestamp and data in the clear. */
function plaintextLink(agent: TestAgent, source: string): LinkExpression {
  return {
    author: agent.did,
    timestamp: new Date().toISOString(),
    data: { source, predicate: "test://p", target: "test://t" },
  };
}

async function plaintextCommit(server: TestServerHandle, roomId: string, agent: TestAgent, token: string, source: string) {
  return postJson<{ error?: string; sequence?: number }>(
    `${server.url}/rooms/${roomId}/commit`,
    { additions: [plaintextLink(agent, source)], removals: [] },
    token
  );
}

/** Admin creates the room, admits `member`, and both sign in. */
async function roomWithMember(server: TestServerHandle) {
  const roomId = randomUUID();
  const admin = await createTestAgent();
  const member = await createTestAgent();
  const adminToken = await authenticateAgent(server.url, roomId, admin);
  const acl = await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
  assert.equal(acl.status, 200);
  const memberToken = await authenticateAgent(server.url, roomId, member);
  return { roomId, admin, member, adminToken, memberToken };
}

test("a room refuses plaintext from its first commit, before any key exists", async () => {
  await withServer(async (server) => {
    const { roomId, admin, member, adminToken, memberToken } = await roomWithMember(server);

    // No rotation yet: no key exists in the room.
    const fromMember = await plaintextCommit(server, roomId, member, memberToken, "test://member-first");
    assert.equal(fromMember.status, 400, "a member's plaintext commit before the admin's rotation is refused");
    assert.match(fromMember.body.error ?? "", /end-to-end encryption/);
    const fromAdmin = await plaintextCommit(server, roomId, admin, adminToken, "test://admin-first");
    assert.equal(fromAdmin.status, 400, "the admin's plaintext commit is refused too");

    const render = await getJson<{ links: unknown[] }>(`${server.url}/rooms/${roomId}/render`, memberToken);
    assert.deepEqual(render.body.links, [], "nothing plaintext was stored");
    const sync = await getJson<{ diffs: unknown[] }>(`${server.url}/rooms/${roomId}/sync?since=0`, memberToken);
    assert.deepEqual(sync.body.diffs, []);
  });
});

test("a room still refuses plaintext after its admin rotates", async () => {
  await withServer(async (server) => {
    const { roomId, member, adminToken, memberToken } = await roomWithMember(server);
    await clientSideRotate(server.url, roomId, adminToken);
    assert.equal((await plaintextCommit(server, roomId, member, memberToken, "test://after")).status, 400);
  });
});

test("a commit mixing an encrypted and a plaintext link is refused whole", async () => {
  await withServer(async (server) => {
    const { roomId, member, adminToken, memberToken } = await roomWithMember(server);
    const { roomKey } = await clientSideRotate(server.url, roomId, adminToken);
    const encrypted = await createSignedLink(member, { source: "test://ok", predicate: "p", target: "t" }, undefined, roomKey);
    const res = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [encrypted, plaintextLink(member, "test://plain")], removals: [] },
      memberToken
    );
    assert.equal(res.status, 400);
    const render = await getJson<{ links: unknown[] }>(`${server.url}/rooms/${roomId}/render`, memberToken);
    assert.deepEqual(render.body.links, [], "neither link was stored");
  });
});

test("GET /keys answers a member without a key with 200 and an empty list", async () => {
  await withServer(async (server) => {
    const { roomId, adminToken, memberToken } = await roomWithMember(server);

    for (const token of [adminToken, memberToken]) {
      const before = await getJson<{ keys: unknown[] }>(`${server.url}/rooms/${roomId}/keys`, token);
      assert.equal(before.status, 200, "no key yet is an empty list, not a 404");
      assert.deepEqual(before.body, { keys: [] });
    }
  });
});

test("after the admin's first rotation the room accepts an encrypted commit", async () => {
  await withServer(async (server) => {
    const { roomId, member, adminToken, memberToken } = await roomWithMember(server);

    const { version, roomKey } = await clientSideRotate(server.url, roomId, adminToken);
    assert.equal(version, 1, "the admin's first rotation is key version 1");

    const link = await createSignedLink(member, { source: "test://enc", predicate: "p", target: "t" }, undefined, roomKey);
    const commit = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      memberToken
    );
    assert.equal(commit.status, 200);
    assert.equal(commit.body.sequence, 1);

    const render = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, adminToken);
    assert.equal(render.body.links.length, 1);
    assert.equal(render.body.links[0].author, undefined, "the server holds no cleartext author");
    assert.equal(openLink(render.body.links[0], roomKey).data.source, "test://enc");
  });
});

test("a database file whose rooms table still has the e2e_enabled column opens and creates rooms", () => {
  // SCHEMA is CREATE TABLE IF NOT EXISTS: a file from the earlier schema keeps the column the INSERT no longer names.
  const dir = mkdtempSync(path.join(tmpdir(), "link-server-schema-"));
  try {
    const file = path.join(dir, "data.sqlite");
    const earlier = new Database(file);
    earlier.exec(`CREATE TABLE rooms (
      id TEXT PRIMARY KEY,
      admin_did TEXT NOT NULL,
      created_at TEXT NOT NULL,
      e2e_enabled INTEGER DEFAULT 0,
      revision TEXT
    )`);
    earlier.close();

    const db = new LinkServerDB(file);
    try {
      const roomId = randomUUID();
      db.createRoom(roomId, "did:key:z6MkAdmin");
      const room = db.getRoom(roomId);
      assert.equal(room?.admin_did, "did:key:z6MkAdmin");
      assert.equal(db.listRooms().length, 1);
    } finally {
      db.close();
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
