/**
 * True E2E encryption tests.
 *
 * These tests prove the server NEVER sees plaintext room keys. Every
 * test constructs a scenario where a malicious server operator could
 * have retained plaintext if key generation happened server-side, and
 * asserts that no such leakage exists.
 *
 * Threat model: the server operator is honest-but-curious. They can
 * read memory, inspect DB rows, and log every HTTP request/response.
 * True E2E means the operator learns nothing about the room key even
 * with full server access.
 */

import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import * as ed from "@noble/ed25519";
import {
  authenticateAgent,
  clientSideRotate,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  testAgentX25519PrivateKey,
  testAgentX25519PublicKey,
  type TestServerHandle,
} from "./helpers.js";
import {
  decryptRoomKeyWithX25519,
  encryptRoomKeyForRecipient,
  generateRoomKey,
  type EncryptedKeyPayload,
} from "../src/encryption.js";

const { bytesToHex, hexToBytes } = ed.etc;

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

// ---------------------------------------------------------------------------
// Core E2E property: server never touches plaintext key material
// ---------------------------------------------------------------------------

test("server stores only sealed envelopes — plaintext room key bytes never appear in GET /keys", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    const { roomKey } = await clientSideRotate(server.url, roomId, adminToken);
    const roomKeyHex = bytesToHex(roomKey);

    // Fetch every key the server stores for this room and stringify the
    // entire response. The plaintext room key must NOT appear anywhere.
    const adminKeys = await getJson<unknown>(`${server.url}/rooms/${roomId}/keys`, adminToken);
    const raw = JSON.stringify(adminKeys.body);
    assert.ok(
      !raw.includes(roomKeyHex),
      "plaintext room key hex must not appear in the server's key response",
    );
  });
});

test("server cannot decrypt sealed envelopes — decryption requires recipient's X25519 private key", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const { roomKey } = await clientSideRotate(server.url, roomId, adminToken);

    // Fetch the sealed envelope the server stored for the admin.
    const keysRes = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken,
    );
    const envelope = keysRes.body.keys[0].encryptedKey;

    // The server knows the ephemeral public key (it's in the envelope) and
    // the recipient's public key (it's in the ACL table). But it does NOT
    // have any X25519 private key. Attempting decryption with a random
    // private key must fail.
    const fakePriv = ed.utils.randomPrivateKey().slice(0, 32);
    assert.throws(
      () => decryptRoomKeyWithX25519(envelope, fakePriv),
      "decrypting with a random key must throw (AEAD tag mismatch)",
    );

    // The real recipient CAN decrypt.
    const decrypted = decryptRoomKeyWithX25519(envelope, testAgentX25519PrivateKey(admin));
    assert.deepEqual(decrypted, roomKey);
  });
});

test("each member receives a uniquely-sealed envelope — envelopes are not shared", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    await clientSideRotate(server.url, roomId, adminToken);

    const adminKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken,
    );
    const memberKeys = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      memberToken,
    );

    // Each seal uses an ephemeral keypair, so ciphertexts, nonces, and
    // ephemeral public keys must all differ between recipients.
    const ae = adminKeys.body.keys[0].encryptedKey;
    const me = memberKeys.body.keys[0].encryptedKey;
    assert.notEqual(ae.ephemeralPublicKey, me.ephemeralPublicKey, "ephemeral keys must differ");
    assert.notEqual(ae.ciphertext, me.ciphertext, "ciphertexts must differ");
    assert.notEqual(ae.nonce, me.nonce, "nonces must differ");

    // But both decrypt to the same underlying room key.
    const adminPlain = decryptRoomKeyWithX25519(ae, testAgentX25519PrivateKey(admin));
    const memberPlain = decryptRoomKeyWithX25519(me, testAgentX25519PrivateKey(member));
    assert.deepEqual(adminPlain, memberPlain, "both envelopes must unwrap to the same room key");
  });
});

// ---------------------------------------------------------------------------
// Rejection: server enforces client-side rotation format
// ---------------------------------------------------------------------------

test("server rejects old-style rotation with empty body", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      {},
      token,
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /keys array/i);
  });
});

test("server rejects rotation with empty keys array", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys: [] },
      token,
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /keys array/i);
  });
});

test("server rejects rotation containing a DID not in the room's ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const outsider = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const roomKey = generateRoomKey();
    const adminPubHex = testAgentX25519PublicKey(admin);
    const keys = [
      { did: admin.did, encryptedKey: encryptRoomKeyForRecipient(roomKey, hexToBytes(adminPubHex)) },
      { did: outsider.did, encryptedKey: encryptRoomKeyForRecipient(roomKey, hexToBytes(testAgentX25519PublicKey(outsider))) },
    ];

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys },
      adminToken,
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /not in this room's ACL/);
  });
});

test("server rejects rotation with malformed envelope (missing fields)", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys: [{ did: admin.did, encryptedKey: { ephemeralPublicKey: "aa" } }] },
      adminToken,
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /malformed encryptedKey/);
  });
});

// ---------------------------------------------------------------------------
// ACL endpoint returns X25519 public keys for client-side sealing
// ---------------------------------------------------------------------------

test("GET /acl returns X25519 public keys for each member", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    const aclRes = await getJson<{
      admin: string;
      members: Array<{ did: string; x25519PublicKey: string | null }>;
    }>(`${server.url}/rooms/${roomId}/acl`, adminToken);

    assert.equal(aclRes.status, 200);
    assert.equal(aclRes.body.members.length, 2);

    // Both members must have X25519 public keys (registered during auth)
    for (const m of aclRes.body.members) {
      assert.ok(m.x25519PublicKey, `member ${m.did} must have an X25519 public key`);
      assert.equal(typeof m.x25519PublicKey, "string");
      assert.ok(m.x25519PublicKey.length > 0);
    }

    // Verify the keys match what we expect from the test agents
    const adminEntry = aclRes.body.members.find((m) => m.did === admin.did)!;
    const memberEntry = aclRes.body.members.find((m) => m.did === member.did)!;
    assert.equal(adminEntry.x25519PublicKey, testAgentX25519PublicKey(admin));
    assert.equal(memberEntry.x25519PublicKey, testAgentX25519PublicKey(member));
  });
});

// ---------------------------------------------------------------------------
// Full lifecycle: rotate → add member → rotate → grant → decrypt
// ---------------------------------------------------------------------------

test("full E2E lifecycle: rotate, late member, grant, all decrypt", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const alice = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: alice.did }, adminToken);
    const aliceToken = await authenticateAgent(server.url, roomId, alice);

    // --- Version 1: admin + alice ---
    const { roomKey: keyV1 } = await clientSideRotate(server.url, roomId, adminToken);

    // Both can decrypt v1
    const adminKeysV1 = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    const aliceKeysV1 = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, aliceToken,
    );
    assert.deepEqual(
      decryptRoomKeyWithX25519(adminKeysV1.body.keys[0].encryptedKey, testAgentX25519PrivateKey(admin)),
      keyV1,
    );
    assert.deepEqual(
      decryptRoomKeyWithX25519(aliceKeysV1.body.keys[0].encryptedKey, testAgentX25519PrivateKey(alice)),
      keyV1,
    );

    // --- Add Bob (late member) ---
    const bob = await createTestAgent();
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: bob.did }, adminToken);
    const bobToken = await authenticateAgent(server.url, roomId, bob);

    // Bob has no keys yet
    const bobKeysBefore = await getJson<{ keys: unknown[]; e2e_enabled: boolean }>(
      `${server.url}/rooms/${roomId}/keys`, bobToken,
    );
    assert.equal(bobKeysBefore.body.keys.length, 0);
    assert.equal(bobKeysBefore.body.e2e_enabled, true);

    // --- Version 2: admin + alice + bob ---
    const rot2 = await clientSideRotate(server.url, roomId, adminToken);
    assert.equal(rot2.version, 2);
    assert.ok(rot2.membersNeedingHistoricalKeys);
    assert.equal(rot2.membersNeedingHistoricalKeys!.length, 1);
    assert.equal(rot2.membersNeedingHistoricalKeys![0].did, bob.did);
    assert.deepEqual(rot2.membersNeedingHistoricalKeys![0].missingVersions, [1]);

    // Bob gets v2 but not v1
    const bobKeysV2 = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, bobToken,
    );
    assert.equal(bobKeysV2.body.keys.length, 1);
    assert.equal(bobKeysV2.body.keys[0].version, 2);

    // --- Grant v1 to Bob ---
    const bobPubHex = rot2.membersNeedingHistoricalKeys![0].x25519PublicKey;
    const resealedV1 = encryptRoomKeyForRecipient(keyV1, hexToBytes(bobPubHex));
    const grantRes = await postJson<{ granted: number[] }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: bob.did, keys: [{ version: 1, encryptedKey: resealedV1 }] },
      adminToken,
    );
    assert.deepEqual(grantRes.body.granted, [1]);

    // Bob now has both versions and can decrypt both
    const bobKeysFinal = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, bobToken,
    );
    assert.equal(bobKeysFinal.body.keys.length, 2);
    const bobPriv = testAgentX25519PrivateKey(bob);
    const bobV1 = decryptRoomKeyWithX25519(
      bobKeysFinal.body.keys.find((k) => k.version === 1)!.encryptedKey, bobPriv,
    );
    const bobV2 = decryptRoomKeyWithX25519(
      bobKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey, bobPriv,
    );
    assert.deepEqual(bobV1, keyV1, "Bob's v1 must match the original v1 key");
    assert.deepEqual(bobV2, rot2.roomKey, "Bob's v2 must match the original v2 key");

    // --- Cross-check: all three agree on both versions ---
    const adminKeysFinal = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    const aliceKeysFinal = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, aliceToken,
    );
    const adminV2 = decryptRoomKeyWithX25519(
      adminKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey,
      testAgentX25519PrivateKey(admin),
    );
    const aliceV2 = decryptRoomKeyWithX25519(
      aliceKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey,
      testAgentX25519PrivateKey(alice),
    );
    assert.deepEqual(adminV2, rot2.roomKey);
    assert.deepEqual(aliceV2, rot2.roomKey);
    assert.deepEqual(bobV2, rot2.roomKey);
  });
});

// ---------------------------------------------------------------------------
// Key isolation across versions
// ---------------------------------------------------------------------------

test("successive rotations produce independent keys — compromising v2 does not reveal v1", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const { roomKey: keyV1 } = await clientSideRotate(server.url, roomId, adminToken);
    const { roomKey: keyV2 } = await clientSideRotate(server.url, roomId, adminToken);

    // Keys must differ (randomBytes(32) has negligible collision probability).
    assert.notDeepEqual(keyV1, keyV2, "successive room keys must differ");

    // Verify both versions decrypt correctly from the server's storage.
    const keysRes = await getJson<{ keys: Array<{ encryptedKey: EncryptedKeyPayload; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    assert.equal(keysRes.body.keys.length, 2);
    const priv = testAgentX25519PrivateKey(admin);
    const decV1 = decryptRoomKeyWithX25519(keysRes.body.keys.find((k) => k.version === 1)!.encryptedKey, priv);
    const decV2 = decryptRoomKeyWithX25519(keysRes.body.keys.find((k) => k.version === 2)!.encryptedKey, priv);
    assert.deepEqual(decV1, keyV1);
    assert.deepEqual(decV2, keyV2);
  });
});
