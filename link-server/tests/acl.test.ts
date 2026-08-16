import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  authenticateAgent,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

test("GET acl lists the admin and all members", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const res = await getJson<{ admin: string; members: string[] }>(
      `${server.url}/rooms/${roomId}/acl`,
      adminToken
    );
    assert.equal(res.status, 200);
    assert.equal(res.body.admin, admin.did);
    assert.deepEqual(new Set(res.body.members), new Set([admin.did, member.did]));
  });
});

test("non-admin members cannot modify the ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const outsider = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: outsider.did },
      memberToken
    );
    assert.equal(res.status, 403);

    const acl = await getJson<{ members: string[] }>(`${server.url}/rooms/${roomId}/acl`, adminToken);
    assert.ok(!acl.body.members.includes(outsider.did));
  });
});

test("acl rejects a missing/invalid action", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "not-a-real-action", did: "did:key:zXYZ" },
      adminToken
    );
    assert.equal(res.status, 400);
  });
});

test("admin cannot remove themselves from the ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "remove", did: admin.did },
      adminToken
    );
    assert.equal(res.status, 400);

    const acl = await getJson<{ members: string[] }>(`${server.url}/rooms/${roomId}/acl`, adminToken);
    assert.ok(acl.body.members.includes(admin.did));
  });
});

test("removed member can no longer re-authenticate without being re-added", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);

    await assert.rejects(() => authenticateAgent(server.url, roomId, member));
  });
});

test("member added back to the ACL can authenticate again", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "remove", did: member.did }, adminToken);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const token = await authenticateAgent(server.url, roomId, member);
    assert.equal(typeof token, "string");
  });
});

test("acl endpoints require authentication", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/acl`);
    assert.equal(res.status, 401);
  });
});
