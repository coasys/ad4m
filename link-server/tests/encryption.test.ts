import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  authenticateAgent,
  createSignedLink,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  testAgentX25519PrivateKey,
  type TestServerHandle,
} from "./helpers.js";
import {
  decryptRoomKeyWithX25519,
  type EncryptedKeyPayload,
} from "../src/encryption.js";
import type { EncryptedLinkData, LinkExpression } from "../src/types.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

test("rotate generates a room key sealed to every current ACL member", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const rotateRes = await postJson<{ version: number; recipients: string[] }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      {},
      adminToken
    );
    assert.equal(rotateRes.status, 200);
    assert.equal(rotateRes.body.version, 1);
    assert.deepEqual(new Set(rotateRes.body.recipients), new Set([admin.did, member.did]));

    const adminKey = await getJson<{ encryptedKey: EncryptedKeyPayload; version: number }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken
    );
    const memberKey = await getJson<{ encryptedKey: EncryptedKeyPayload; version: number }>(
      `${server.url}/rooms/${roomId}/keys`,
      memberToken
    );
    assert.equal(adminKey.status, 200);
    assert.equal(memberKey.status, 200);
    assert.equal(adminKey.body.version, 1);
    assert.equal(memberKey.body.version, 1);

    const adminRoomKey = decryptRoomKeyWithX25519(adminKey.body.encryptedKey, testAgentX25519PrivateKey(admin));
    const memberRoomKey = decryptRoomKeyWithX25519(memberKey.body.encryptedKey, testAgentX25519PrivateKey(member));
    assert.deepEqual(adminRoomKey, memberRoomKey, "both members recover the same underlying room key");
  });
});

test("only the admin can rotate the room key", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      {},
      memberToken
    );
    assert.equal(res.status, 403);
  });
});

test("agent with no granted key yet gets 404 on GET /keys", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/keys`, adminToken);
    assert.equal(res.status, 404, "room has no key yet, no rotation has occurred");
  });
});

test("member added after a rotation has no key until the next rotation", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const lateMember = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: lateMember.did }, adminToken);
    const lateToken = await authenticateAgent(server.url, roomId, lateMember);

    const before = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/keys`, lateToken);
    assert.equal(before.status, 404);

    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);
    const after = await getJson<{ encryptedKey: EncryptedKeyPayload; version: number }>(
      `${server.url}/rooms/${roomId}/keys`,
      lateToken
    );
    assert.equal(after.status, 200);
    assert.equal(after.body.version, 2);
  });
});

test("server stores link data opaquely — encrypted-shaped data round-trips unchanged", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    // The server does not validate or transform the data field — it stores
    // whatever shape the client sends. Verify that encrypted-shaped data
    // round-trips through commit → render without alteration.
    const encryptedData: EncryptedLinkData = { ciphertext: "deadbeef", nonce: "cafebabe" };
    const link = await createSignedLink(agent, encryptedData);

    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      token
    );
    assert.equal(commitRes.status, 200);

    const renderRes = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(renderRes.status, 200);
    assert.equal(renderRes.body.links.length, 1);
    const storedData = renderRes.body.links[0].data as EncryptedLinkData;
    assert.equal(storedData.ciphertext, "deadbeef");
    assert.equal(storedData.nonce, "cafebabe");
  });
});

test("removing an encrypted-shaped link works by resending the exact original LinkExpression", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const encryptedData: EncryptedLinkData = { ciphertext: "aabb", nonce: "ccdd" };
    const link = await createSignedLink(agent, encryptedData);
    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, token);

    const removeRes = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [link] },
      token
    );
    assert.equal(removeRes.status, 200);

    const renderRes = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(renderRes.body.links.length, 0);
  });
});
