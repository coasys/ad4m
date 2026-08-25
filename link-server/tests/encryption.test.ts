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
  type TestServerHandle,
} from "./helpers.js";
import {
  decryptLinkData,
  decryptRoomKeyForDid,
  encryptLinkData,
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

    const adminRoomKey = decryptRoomKeyForDid(adminKey.body.encryptedKey, admin.privateKey);
    const memberRoomKey = decryptRoomKeyForDid(memberKey.body.encryptedKey, member.privateKey);
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
    assert.equal(res.status, 404, "room isn't E2E-enabled yet, no key exists");
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

test("end to end: commit encrypted link data, sync/render return ciphertext, client decrypts locally", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, token);
    const keyRes = await getJson<{ encryptedKey: EncryptedKeyPayload }>(
      `${server.url}/rooms/${roomId}/keys`,
      token
    );
    const roomKey = decryptRoomKeyForDid(keyRes.body.encryptedKey, agent.privateKey);

    const plaintext = { source: "secret-a", predicate: "knows", target: "secret-b" };
    const encryptedData = encryptLinkData(roomKey, plaintext);
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
    assert.equal(typeof storedData.ciphertext, "string");
    assert.notEqual(
      JSON.stringify(storedData),
      JSON.stringify(plaintext),
      "server storage must not contain plaintext source/predicate/target"
    );

    const decrypted = decryptLinkData(roomKey, storedData);
    assert.deepEqual(decrypted, plaintext);
  });
});

test("room requires E2E-encrypted data once enabled: plaintext commits are rejected", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, token);

    const plainLink = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [plainLink], removals: [] },
      token
    );
    assert.equal(res.status, 400);
  });
});

test("room without E2E enabled rejects encrypted-shaped link data", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const fakeEncrypted = await createSignedLink(agent, { ciphertext: "aabb", nonce: "ccdd" });
    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [fakeEncrypted], removals: [] },
      token
    );
    assert.equal(res.status, 400);
  });
});

test("removing an encrypted link works by resending the exact original LinkExpression", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, token);
    const keyRes = await getJson<{ encryptedKey: EncryptedKeyPayload }>(
      `${server.url}/rooms/${roomId}/keys`,
      token
    );
    const roomKey = decryptRoomKeyForDid(keyRes.body.encryptedKey, agent.privateKey);

    const encryptedData = encryptLinkData(roomKey, { source: "a", predicate: "rel", target: "b" });
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
