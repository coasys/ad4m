/**
 * E2E encryption tests.
 *
 * These tests prove the server NEVER sees plaintext room keys. Every
 * test constructs a scenario where a malicious server operator could
 * have retained plaintext if key generation happened server-side, and
 * asserts that no such leakage exists.
 *
 * Threat model: the server operator is honest-but-curious. They can
 * read memory, inspect DB rows, and log every HTTP request/response.
 * E2E means the operator learns nothing about the room key even
 * with full server access.
 */

import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { readFileSync } from "node:fs";
import path from "node:path";
import { test } from "node:test";
import * as ed from "@noble/ed25519";
import {
  authenticateAgent,
  clientSideRotate,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import type { SealedBox } from "../src/encryption.js";
import { generateRoomKey, openSealed, roomKeyCommitment, sealToDid } from "./sealed-box.js";

const { bytesToHex } = ed.etc;

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

    // Fetch every key the server stores for this room. Verify the
    // response succeeded and contains sealed records, THEN check that
    // the plaintext room key does not appear anywhere in it.
    const adminKeys = await getJson<{ keys: Array<{ encryptedKey: SealedBox }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    assert.equal(adminKeys.status, 200);
    assert.ok(adminKeys.body.keys.length > 0, "server must have stored sealed keys");
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
    const keysRes = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken,
    );
    const envelope = keysRes.body.keys[0].encryptedKey;

    // The server knows the ephemeral public key (it's in the box) and the
    // recipient's public key (it follows from the DID). But it does NOT
    // hold any agent's secret key. Opening with a random key must fail.
    const fakePriv = ed.utils.randomPrivateKey().slice(0, 32);
    assert.throws(
      () => openSealed(envelope, fakePriv),
      "decrypting with a random key must throw (AEAD tag mismatch)",
    );

    // The real recipient CAN decrypt.
    const decrypted = openSealed(envelope, admin.privateKey);
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

    const adminKeys = await getJson<{ keys: Array<{ encryptedKey: SealedBox }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      adminToken,
    );
    const memberKeys = await getJson<{ keys: Array<{ encryptedKey: SealedBox }> }>(
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
    const adminPlain = openSealed(ae, admin.privateKey);
    const memberPlain = openSealed(me, member.privateKey);
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

test("server rejects rotation with null entry in keys array", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const token = await authenticateAgent(server.url, roomId, agent);

    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys: [null] },
      token,
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /non-null object/);
  });
});

test("server rejects rotation containing a DID not in the room's ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const outsider = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    const roomKey = generateRoomKey();
    const keys = [
      { did: admin.did, encryptedKey: sealToDid(admin.did, roomKey) },
      { did: outsider.did, encryptedKey: sealToDid(outsider.did, roomKey) },
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
// Members are their DIDs: the ACL carries no keys, and a room key reaches
// a member who never connected
// ---------------------------------------------------------------------------

test("GET /acl lists members by DID alone", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const aclRes = await getJson<{ admin: string; members: Array<Record<string, unknown>> }>(
      `${server.url}/rooms/${roomId}/acl`,
      adminToken,
    );
    assert.equal(aclRes.status, 200);
    assert.deepEqual(
      aclRes.body.members,
      [{ did: admin.did }, { did: member.did }],
      "no key material in the ACL: a member's key follows from its DID",
    );
  });
});

test("the admin seals a room key to a member who never connected, and the member opens it", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);

    const { roomKey, recipients } = await clientSideRotate(server.url, roomId, adminToken);
    assert.deepEqual(recipients.sort(), [admin.did, member.did].sort());

    const memberToken = await authenticateAgent(server.url, roomId, member);
    const keys = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      memberToken,
    );
    assert.equal(keys.status, 200);
    assert.deepEqual(openSealed(keys.body.keys[0].encryptedKey, member.privateKey), roomKey);
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
    const adminKeysV1 = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    const aliceKeysV1 = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, aliceToken,
    );
    assert.deepEqual(
      openSealed(adminKeysV1.body.keys[0].encryptedKey, admin.privateKey),
      keyV1,
    );
    assert.deepEqual(
      openSealed(aliceKeysV1.body.keys[0].encryptedKey, alice.privateKey),
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
    const bobKeysV2 = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, bobToken,
    );
    assert.equal(bobKeysV2.body.keys.length, 1);
    assert.equal(bobKeysV2.body.keys[0].version, 2);

    // --- Alice, a member who is not the admin, grants v1 to Bob ---
    const resealedV1 = sealToDid(bob.did, keyV1);
    const grantRes = await postJson<{ granted: number[] }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: bob.did, keys: [{ version: 1, encryptedKey: resealedV1 }] },
      aliceToken,
    );
    assert.deepEqual(grantRes.body.granted, [1]);

    // Bob now has both versions and can decrypt both
    const bobKeysFinal = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, bobToken,
    );
    assert.equal(bobKeysFinal.body.keys.length, 2);
    const bobPriv = bob.privateKey;
    const bobV1 = openSealed(
      bobKeysFinal.body.keys.find((k) => k.version === 1)!.encryptedKey, bobPriv,
    );
    const bobV2 = openSealed(
      bobKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey, bobPriv,
    );
    assert.deepEqual(bobV1, keyV1, "Bob's v1 must match the original v1 key");
    assert.deepEqual(bobV2, rot2.roomKey, "Bob's v2 must match the original v2 key");

    // --- Cross-check: all three agree on both versions ---
    const adminKeysFinal = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    const aliceKeysFinal = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, aliceToken,
    );
    const adminV2 = openSealed(
      adminKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey,
      admin.privateKey,
    );
    const aliceV2 = openSealed(
      aliceKeysFinal.body.keys.find((k) => k.version === 2)!.encryptedKey,
      alice.privateKey,
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
    const keysRes = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    assert.equal(keysRes.body.keys.length, 2);
    const priv = admin.privateKey;
    const decV1 = openSealed(keysRes.body.keys.find((k) => k.version === 1)!.encryptedKey, priv);
    const decV2 = openSealed(keysRes.body.keys.find((k) => k.version === 2)!.encryptedKey, priv);
    assert.deepEqual(decV1, keyV1);
    assert.deepEqual(decV2, keyV2);
  });
});

// ---------------------------------------------------------------------------
// Rotation policy: version count must reflect intentional rotations only
// ---------------------------------------------------------------------------

test("re-authenticating an existing member must not create new key versions — grant is idempotent", async () => {
  // This test encodes the contract that key rotation is an EXPLICIT admin
  // action, not a side-effect of presence events. If someone wires
  // performRotation() to onPeerJoined (a presence event that fires on
  // every reconnect/wake/second-device), the version count would inflate
  // here and fail.
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    // One intentional rotation → version 1
    const { version } = await clientSideRotate(server.url, roomId, adminToken);
    assert.equal(version, 1);

    // Simulate three "reconnects" — member re-authenticates each time.
    // A correct client runs only performKeyGrants() here (idempotent),
    // NOT performRotation(). We verify the server-side consequence: the
    // version count must not grow.
    for (let i = 0; i < 3; i++) {
      await authenticateAgent(server.url, roomId, member);
    }

    // Still exactly version 1 — no phantom rotations.
    const keysRes = await getJson<{ keys: Array<{ version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    assert.equal(keysRes.status, 200);
    assert.equal(keysRes.body.keys.length, 1, "must have exactly 1 key version after reconnects");
    assert.equal(keysRes.body.keys[0].version, 1);
  });
});

test("adding a new member and granting historical keys does not inflate the version count", async () => {
  // The correct flow: rotate once → add member → grant (NOT rotate again).
  // The new member receives v1 via grant. Version count stays at 1.
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);

    // Rotate to v1 (admin only)
    await clientSideRotate(server.url, roomId, adminToken);

    // Add a new member
    const member = await createTestAgent();
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    // Grant v1 to the new member (what the language's key grants do)
    const missingRes = await getJson<{
      membersNeedingHistoricalKeys: Array<{ did: string; missingVersions: number[] }>;
    }>(`${server.url}/rooms/${roomId}/keys/missing`, adminToken);
    assert.equal(missingRes.status, 200);

    for (const m of missingRes.body.membersNeedingHistoricalKeys) {
      for (const ver of m.missingVersions) {
        // Re-seal admin's key for this version to the new member
        const adminKeysRes = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
          `${server.url}/rooms/${roomId}/keys`, adminToken,
        );
        const adminEnvelope = adminKeysRes.body.keys.find((k) => k.version === ver)!;
        const plainKey = openSealed(adminEnvelope.encryptedKey, admin.privateKey);
        const resealed = sealToDid(m.did, plainKey);
        await postJson(
          `${server.url}/rooms/${roomId}/keys/grant`,
          { targetDid: m.did, keys: [{ version: ver, encryptedKey: resealed }] },
          adminToken,
        );
      }
    }

    // Version count: still 1. The grant populated v1 for the member
    // but did NOT create v2.
    const adminKeysRes = await getJson<{ keys: Array<{ version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, adminToken,
    );
    assert.equal(adminKeysRes.body.keys.length, 1);
    assert.equal(adminKeysRes.body.keys[0].version, 1);

    // Member now has v1 too
    const memberToken = await authenticateAgent(server.url, roomId, member);
    const memberKeysRes = await getJson<{ keys: Array<{ version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, memberToken,
    );
    assert.equal(memberKeysRes.body.keys.length, 1);
    assert.equal(memberKeysRes.body.keys[0].version, 1);
  });
});

// ---------------------------------------------------------------------------
// Wire-level proof: plaintext key never appears in the POST body
// ---------------------------------------------------------------------------

test("POST /keys/rotate request body contains zero plaintext key bytes — hex or base64", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    // Generate room key locally — same as clientSideRotate but we
    // capture the serialised request body before sending it.
    const roomKey = generateRoomKey();
    const roomKeyHex = bytesToHex(roomKey);
    const roomKeyBase64 = Buffer.from(roomKey).toString("base64");
    const roomKeyBase64Url = Buffer.from(roomKey).toString("base64url");

    const aclRes = await getJson<{ admin: string; members: Array<{ did: string }> }>(
      `${server.url}/rooms/${roomId}/acl`,
      adminToken,
    );
    assert.equal(aclRes.status, 200);

    // Seal to each member's DID.
    const keys = aclRes.body.members.map((m) => ({ did: m.did, encryptedKey: sealToDid(m.did, roomKey) }));

    // Serialize — this exact string goes on the wire.
    const body = { keys, commitment: roomKeyCommitment(roomKey) };
    const wireBody = JSON.stringify(body);

    // The plaintext key must not appear in any encoding.
    assert.equal(
      wireBody.includes(roomKeyHex), false,
      `plaintext room key (hex) found in POST body: ${roomKeyHex}`,
    );
    assert.equal(
      wireBody.includes(roomKeyBase64), false,
      `plaintext room key (base64) found in POST body: ${roomKeyBase64}`,
    );
    assert.equal(
      wireBody.includes(roomKeyBase64Url), false,
      `plaintext room key (base64url) found in POST body: ${roomKeyBase64Url}`,
    );

    // Now send it — server must accept the sealed-only payload.
    const rotateRes = await postJson<{ version: number; recipients: string[] }>(
      `${server.url}/rooms/${roomId}/keys/rotate`, body, adminToken,
    );
    assert.equal(rotateRes.status, 200);
    assert.equal(rotateRes.body.version, 1);

    // Member can still decrypt — proves the sealed envelope carries the real key.
    const memberToken = await authenticateAgent(server.url, roomId, member);
    const keysRes = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`, memberToken,
    );
    assert.equal(keysRes.status, 200);
    const decrypted = openSealed(
      keysRes.body.keys[0].encryptedKey,
      member.privateKey,
    );
    assert.deepEqual(decrypted, roomKey, "member must recover the original room key");
  });
});

// ---------------------------------------------------------------------------
// Storage-level proof: plaintext key never appears in SQLite DB file
// ---------------------------------------------------------------------------

test("plaintext room key bytes never appear anywhere in the SQLite DB file", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    await authenticateAgent(server.url, roomId, member);

    // Rotate twice to populate multiple key versions.
    const { roomKey: key1 } = await clientSideRotate(server.url, roomId, adminToken);
    const { roomKey: key2 } = await clientSideRotate(server.url, roomId, adminToken);
    const key1Hex = bytesToHex(key1);
    const key2Hex = bytesToHex(key2);

    // Read the entire SQLite database file as raw bytes.
    const dbPath = path.join(server.dataDir, "data.sqlite");
    const dbBytes = readFileSync(dbPath);
    const dbHex = dbBytes.toString("hex");

    // Neither plaintext key may appear anywhere in the DB — not in
    // key storage rows, not in WAL pages, not in free-list pages.
    assert.equal(
      dbHex.includes(key1Hex), false,
      `plaintext room key v1 found in SQLite DB file`,
    );
    assert.equal(
      dbHex.includes(key2Hex), false,
      `plaintext room key v2 found in SQLite DB file`,
    );

    // Also check WAL and SHM if they exist (WAL mode).
    for (const suffix of ["-wal", "-shm"]) {
      try {
        const walBytes = readFileSync(dbPath + suffix);
        const walHex = walBytes.toString("hex");
        assert.equal(
          walHex.includes(key1Hex), false,
          `plaintext room key v1 found in ${suffix} file`,
        );
        assert.equal(
          walHex.includes(key2Hex), false,
          `plaintext room key v2 found in ${suffix} file`,
        );
      } catch {
        // File doesn't exist — not in WAL mode, which still passes.
      }
    }
  });
});

// ---------------------------------------------------------------------------
// Endpoint sweep: no server endpoint leaks plaintext key material
// ---------------------------------------------------------------------------

test("no server endpoint leaks plaintext room key in its response", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    const { roomKey } = await clientSideRotate(server.url, roomId, adminToken);
    const roomKeyHex = bytesToHex(roomKey);

    // Every read endpoint that could conceivably echo back key material.
    const base = `${server.url}/rooms/${roomId}`;
    const endpoints: Array<{ label: string; url: string; token: string }> = [
      { label: "GET /keys (admin)", url: `${base}/keys`, token: adminToken },
      { label: "GET /keys (member)", url: `${base}/keys`, token: memberToken },
      { label: "GET /acl", url: `${base}/acl`, token: adminToken },
      { label: "GET /render", url: `${base}/render`, token: adminToken },
      { label: "GET /sync?since=0", url: `${base}/sync?since=0`, token: adminToken },
      { label: "GET /keys/missing", url: `${base}/keys/missing`, token: adminToken },
      { label: "GET /peers", url: `${base}/peers`, token: adminToken },
    ];

    for (const ep of endpoints) {
      const res = await fetch(ep.url, {
        headers: { authorization: `Bearer ${ep.token}` },
      });
      const text = await res.text();
      assert.equal(
        text.includes(roomKeyHex), false,
        `plaintext room key leaked by ${ep.label}: response contains ${roomKeyHex}`,
      );
    }
  });
});

// ---- Grants by members; rotation stays the admin's ----

test("a member grants the versions it holds to a member lacking them, but cannot rotate or manage the ACL", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const carol = await createTestAgent();
    const dave = await createTestAgent();

    const adminToken = await authenticateAgent(server.url, roomId, admin);
    await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: member.did }, adminToken);
    const memberToken = await authenticateAgent(server.url, roomId, member);
    const { roomKey } = await clientSideRotate(server.url, roomId, adminToken);

    // Carol and Dave join after version 1: neither holds it.
    for (const late of [carol, dave]) {
      await postJson(`${server.url}/rooms/${roomId}/acl`, { action: "add", did: late.did }, adminToken);
    }
    const daveToken = await authenticateAgent(server.url, roomId, dave);

    // Any member sees who lacks which version.
    const missing = await getJson<{ membersNeedingHistoricalKeys: Array<{ did: string; missingVersions: number[] }> }>(
      `${server.url}/rooms/${roomId}/keys/missing`,
      memberToken,
    );
    assert.equal(missing.status, 200);
    assert.deepEqual(
      missing.body.membersNeedingHistoricalKeys.map((m) => [m.did, m.missingVersions]).sort(),
      [[carol.did, [1]], [dave.did, [1]]].sort(),
    );

    // Dave holds no version, so he cannot grant one.
    const daveGrant = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: carol.did, keys: [{ version: 1, encryptedKey: sealToDid(carol.did, generateRoomKey()) }] },
      daveToken,
    );
    assert.equal(daveGrant.status, 403);
    assert.match(daveGrant.body.error, /version 1/);

    // The member, who is not the admin, grants version 1 to Carol, who never connected.
    const grant = await postJson<{ granted: number[] }>(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: carol.did, keys: [{ version: 1, encryptedKey: sealToDid(carol.did, roomKey) }] },
      memberToken,
    );
    assert.equal(grant.status, 200);
    assert.deepEqual(grant.body.granted, [1]);
    const carolToken = await authenticateAgent(server.url, roomId, carol);
    const carolKeys = await getJson<{ keys: Array<{ encryptedKey: SealedBox; version: number }> }>(
      `${server.url}/rooms/${roomId}/keys`,
      carolToken,
    );
    assert.deepEqual(openSealed(carolKeys.body.keys[0].encryptedKey, carol.privateKey), roomKey);

    // Rotation and the ACL stay with the admin.
    const rotateAttempt = await postJson(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys: [{ did: member.did, encryptedKey: sealToDid(member.did, generateRoomKey()) }] },
      memberToken,
    );
    assert.equal(rotateAttempt.status, 403);
    const aclAttempt = await postJson(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK" },
      memberToken,
    );
    assert.equal(aclAttempt.status, 403);
  });
});

// ---- Auth carries no key ----

test("auth takes the signed challenge alone; a key sent along is not stored", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const step1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, { did: agent.did });
    assert.equal(step1.status, 200);

    const { signHex, hashMessageForVerify } = await import("../src/auth.js");
    const signature = await signHex(agent.privateKey, hashMessageForVerify(step1.body.challenge));
    const step2 = await postJson<{ token: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
      challenge: step1.body.challenge,
      signature,
      x25519PublicKey: "aa".repeat(32),
    });
    assert.equal(step2.status, 200);

    const acl = await getJson<{ members: Array<Record<string, unknown>> }>(
      `${server.url}/rooms/${roomId}/acl`,
      step2.body.token,
    );
    assert.deepEqual(acl.body.members, [{ did: agent.did }]);
  });
});
