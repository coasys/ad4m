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

    const adminKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken
    );
    const memberKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      memberToken
    );
    assert.equal(adminKeys.status, 200);
    assert.equal(memberKeys.status, 200);
    assert.equal(adminKeys.body.keys.length, 1);
    assert.equal(adminKeys.body.keys[0].version, 1);
    assert.equal(memberKeys.body.keys.length, 1);
    assert.equal(memberKeys.body.keys[0].version, 1);

    const adminRoomKey = decryptRoomKeyWithX25519(adminKeys.body.keys[0].encryptedKey, testAgentX25519PrivateKey(admin));
    const memberRoomKey = decryptRoomKeyWithX25519(memberKeys.body.keys[0].encryptedKey, testAgentX25519PrivateKey(member));
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
    const after = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      lateToken
    );
    assert.equal(after.status, 200);
    assert.equal(after.body.keys.length, 1);
    assert.equal(after.body.keys[0].version, 2);
  });
});

test("server stores link data opaquely — encrypted-shaped data round-trips unchanged", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const encryptedData: EncryptedLinkData = { ciphertext: "deadbeef", nonce: "cafebabe" };
    const link = await createSignedLink(agent, encryptedData);
    (link as any).link_hash = "opaque-test-hash-1";

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
    (link as any).link_hash = "opaque-test-hash-2";
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

test("commit accepts fully-encrypted links (no author/timestamp/proof, only data+link_hash)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const wireLink = {
      data: { ciphertext: "deadbeef1234", nonce: "cafebabe5678" },
      link_hash: "abc123def456",
    };

    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [wireLink], removals: [] },
      token
    );
    assert.equal(commitRes.status, 200);
    assert.equal(commitRes.body.sequence, 1);

    const renderRes = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(renderRes.status, 200);
    assert.equal(renderRes.body.links.length, 1);
    const stored = renderRes.body.links[0].data as EncryptedLinkData;
    assert.equal(stored.ciphertext, "deadbeef1234");
    assert.equal(stored.nonce, "cafebabe5678");
  });
});

test("commit rejects encrypted links that lack a link_hash", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const wireLink = {
      data: { ciphertext: "deadbeef", nonce: "cafebabe" },
    };

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [wireLink], removals: [] },
      token
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /link_hash/);
  });
});

test("encrypted link removal by link_hash removes the correct link from the active set", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const hash = "e2e-removal-test-hash";
    const wireAdd = {
      data: { ciphertext: "aabbccdd", nonce: "11223344" },
      link_hash: hash,
    };

    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [wireAdd], removals: [] }, token);

    const renderBefore = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(renderBefore.body.links.length, 1);

    // Removal uses a DIFFERENT ciphertext (re-encrypted after key rotation)
    // but the SAME link_hash — the server must match by link_hash.
    const wireRemove = {
      data: { ciphertext: "eeff0011", nonce: "55667788" },
      link_hash: hash,
    };
    const removeRes = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [wireRemove] },
      token
    );
    assert.equal(removeRes.status, 200);

    const renderAfter = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, token);
    assert.equal(renderAfter.body.links.length, 0, "link must be removed by link_hash match");
  });
});

test("any authenticated member can remove encrypted links (author check bypassed)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const alice = await createTestAgent();
    const bob = await createTestAgent();
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: bob.did }, aliceToken);
    const bobToken = await authenticateAgent(server.url, roomId, bob);

    // Alice adds an encrypted link
    const hash = "cross-member-removal-hash";
    const wireAdd = {
      data: { ciphertext: "alice-data", nonce: "alice-nonce" },
      link_hash: hash,
    };
    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [wireAdd], removals: [] }, aliceToken);

    const renderBefore = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, bobToken);
    assert.equal(renderBefore.body.links.length, 1);

    // Bob removes it using the same link_hash — should succeed because
    // encrypted links skip the author === claims.did check.
    const wireRemove = {
      data: { ciphertext: "bob-reencrypted", nonce: "bob-nonce" },
      link_hash: hash,
    };
    const removeRes = await postJson<{ sequence: number }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [wireRemove] },
      bobToken
    );
    assert.equal(removeRes.status, 200);

    const renderAfter = await getJson<{ links: LinkExpression[] }>(`${server.url}/rooms/${roomId}/render`, bobToken);
    assert.equal(renderAfter.body.links.length, 0, "Bob must be able to remove Alice's encrypted link");
  });
});

test("plaintext removal by non-author is rejected (contrast with encrypted mode)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const alice = await createTestAgent();
    const bob = await createTestAgent();
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: bob.did }, aliceToken);
    const bobToken = await authenticateAgent(server.url, roomId, bob);

    // Alice adds a plaintext link
    const link = await createSignedLink(alice, { source: "a", predicate: "p", target: "b" });
    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, aliceToken);

    // Bob tries to remove it — should fail because link.author is Alice's DID,
    // not Bob's, and plaintext validation requires author === claims.did.
    const removeRes = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [], removals: [link] },
      bobToken
    );
    assert.equal(removeRes.status, 400, "plaintext cross-member removal must be rejected");
    assert.match(removeRes.body.error, /author/);
  });
});

test("encrypted links with key_version round-trip through sync unchanged", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const wireLink = {
      data: { ciphertext: "versioned-ct", nonce: "versioned-nonce" },
      link_hash: "versioned-hash",
      key_version: 3,
    };

    await postJson(`${server.url}/rooms/${roomId}/commit`, { additions: [wireLink], removals: [] }, token);

    const syncRes = await getJson<{ diffs: Array<{ additions: LinkExpression[] }> }>(
      `${server.url}/rooms/${roomId}/sync?since=0`,
      token
    );
    assert.equal(syncRes.status, 200);
    assert.equal(syncRes.body.diffs.length, 1);
    const synced = syncRes.body.diffs[0].additions[0];
    assert.equal((synced.data as EncryptedLinkData).ciphertext, "versioned-ct");
    assert.equal(synced.key_version, 3, "key_version must survive the commit → sync round-trip");
    assert.equal(synced.link_hash, "versioned-hash", "link_hash must survive the round-trip");
  });
});
