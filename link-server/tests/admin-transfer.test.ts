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

test("admin can transfer admin role to another member", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson<{ admin: string; members: string[] }>(
      `${server.url}/rooms/${roomId}/admin/transfer`,
      { newAdminDid: member.did },
      adminToken,
    );
    assert.equal(res.status, 200);
    assert.equal(res.body.admin, member.did);

    // Old admin can no longer perform admin actions
    const thirdAgent = await createTestAgent();
    const oldAdminAcl = await postJson(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: thirdAgent.did },
      adminToken,
    );
    assert.equal(oldAdminAcl.status, 403);

    // New admin CAN perform admin actions
    const newAdminAcl = await postJson(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: thirdAgent.did },
      memberToken,
    );
    assert.equal(newAdminAcl.status, 200);
  });
});

test("transfer to non-member gets rejected with 400", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const nonMember = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson(
      `${server.url}/rooms/${roomId}/admin/transfer`,
      { newAdminDid: nonMember.did },
      adminToken,
    );
    assert.equal(res.status, 400);
  });
});

test("transfer to self gets rejected with 400", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson(
      `${server.url}/rooms/${roomId}/admin/transfer`,
      { newAdminDid: admin.did },
      adminToken,
    );
    assert.equal(res.status, 400);
  });
});

test("non-admin cannot transfer", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson(
      `${server.url}/rooms/${roomId}/admin/transfer`,
      { newAdminDid: admin.did },
      memberToken,
    );
    assert.equal(res.status, 403);
  });
});

test("new admin can rotate keys after transfer", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    // Transfer admin to member
    await postJson(
      `${server.url}/rooms/${roomId}/admin/transfer`,
      { newAdminDid: member.did },
      adminToken,
    );

    // New admin can access admin-only key endpoints
    const missingRes = await getJson(`${server.url}/rooms/${roomId}/keys/missing`, memberToken);
    assert.equal(missingRes.status, 200);

    // Old admin cannot
    const oldMissingRes = await getJson(`${server.url}/rooms/${roomId}/keys/missing`, adminToken);
    assert.equal(oldMissingRes.status, 403);
  });
});
