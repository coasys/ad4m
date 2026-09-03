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
  encryptRoomKeyForRecipient,
  type EncryptedKeyPayload,
  type MemberKeyGap,
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

// ---- rotate: membersNeedingHistoricalKeys ----

test("rotate response reports members missing historical key versions", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    // Version 1 — only admin present
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    // Add a late member
    const lateMember = await createTestAgent();
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: lateMember.did }, adminToken);
    await authenticateAgent(server.url, roomId, lateMember);

    // Version 2 — late member now in ACL, gets version 2 but lacks version 1
    const rotateRes = await postJson<{
      version: number;
      recipients: string[];
      membersNeedingHistoricalKeys: Array<{ did: string; missingVersions: number[]; x25519PublicKey: string }>;
    }>(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    assert.equal(rotateRes.status, 200);
    assert.equal(rotateRes.body.version, 2);
    assert.equal(rotateRes.body.membersNeedingHistoricalKeys.length, 1);
    assert.equal(rotateRes.body.membersNeedingHistoricalKeys[0].did, lateMember.did);
    assert.deepEqual(rotateRes.body.membersNeedingHistoricalKeys[0].missingVersions, [1]);
    assert.equal(typeof rotateRes.body.membersNeedingHistoricalKeys[0].x25519PublicKey, "string");
  });
});

// ---- grant endpoint ----

test("admin can grant historical key versions to a late member", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    // Rotate once (version 1)
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    // Add late member
    const lateMember = await createTestAgent();
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: lateMember.did }, adminToken);
    const lateToken = await authenticateAgent(server.url, roomId, lateMember);

    // Rotate again (version 2) — late member gets version 2
    const rotateRes = await postJson<{
      membersNeedingHistoricalKeys: Array<{ did: string; missingVersions: number[]; x25519PublicKey: string }>;
    }>(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    // Admin re-seals version 1 for the late member
    const adminKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken
    );
    const adminV1 = adminKeys.body.keys.find((k) => k.version === 1)!;
    const adminPriv = testAgentX25519PrivateKey(admin);
    const roomKeyV1 = decryptRoomKeyWithX25519(adminV1.encryptedKey, adminPriv);

    const gap = rotateRes.body.membersNeedingHistoricalKeys[0];
    const { hexToBytes } = await import("@noble/ed25519").then((m) => m.etc);
    const latePub = hexToBytes(gap.x25519PublicKey);
    const resealedV1 = encryptRoomKeyForRecipient(roomKeyV1, latePub);

    const grantRes = await postJson<{ granted: number[] }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: lateMember.did, keys: [{ version: 1, encryptedKey: resealedV1 }] },
      adminToken
    );
    assert.equal(grantRes.status, 200);
    assert.deepEqual(grantRes.body.granted, [1]);

    // Late member now has both versions
    const lateKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      lateToken
    );
    assert.equal(lateKeys.body.keys.length, 2);
    const versions = lateKeys.body.keys.map((k) => k.version).sort();
    assert.deepEqual(versions, [1, 2]);

    // Verify the late member can decrypt both and they match the admin's
    const latePriv = testAgentX25519PrivateKey(lateMember);
    const lateV1 = decryptRoomKeyWithX25519(lateKeys.body.keys.find((k) => k.version === 1)!.encryptedKey, latePriv);
    const lateV2 = decryptRoomKeyWithX25519(lateKeys.body.keys.find((k) => k.version === 2)!.encryptedKey, latePriv);
    const adminV2Key = decryptRoomKeyWithX25519(
      adminKeys.body.keys.find((k) => k.version === 2)!.encryptedKey,
      adminPriv
    );
    assert.deepEqual(lateV1, roomKeyV1, "late member's decrypted v1 must match admin's v1");
    assert.deepEqual(lateV2, adminV2Key, "late member's decrypted v2 must match admin's v2");
  });
});

test("grant rejects non-admin callers", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: member.did, keys: [{ version: 1, encryptedKey: { ephemeralPublicKey: "aa", nonce: "bb", ciphertext: "cc" } }] },
      memberToken
    );
    assert.equal(res.status, 403);
  });
});

test("grant rejects when target is not a room member", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const outsider = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: outsider.did, keys: [{ version: 1, encryptedKey: { ephemeralPublicKey: "aa", nonce: "bb", ciphertext: "cc" } }] },
      adminToken
    );
    assert.equal(res.status, 404);
    assert.match(res.body.error, /not a member/);
  });
});

test("grant is idempotent — already-stored versions are skipped", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    const member = await createTestAgent();
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    // Rotate — both get version 1
    await postJson(`${server.url}/rooms/${roomId}/keys/rotate`, {}, adminToken);

    // Grant version 1 for member again (already stored)
    const fakeKey = { ephemeralPublicKey: "aa".repeat(32), nonce: "bb".repeat(12), ciphertext: "cc".repeat(24) };
    const res = await postJson<{ granted: number[] }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: member.did, keys: [{ version: 1, encryptedKey: fakeKey }] },
      adminToken
    );
    assert.equal(res.status, 200);
    assert.deepEqual(res.body.granted, [], "already-stored version must not be re-inserted");
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
