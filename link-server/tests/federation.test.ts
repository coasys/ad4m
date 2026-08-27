import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import * as ed from "@noble/ed25519";
import { signHex } from "../src/auth.js";
import { canonicalFederationPayload, type LinkExpression } from "../src/types.js";
import {
  authenticateAgent,
  createSignedLink,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  waitFor,
  type TestServerHandle,
} from "./helpers.js";

async function pairedServers(
  fn: (a: TestServerHandle, b: TestServerHandle) => Promise<void>
): Promise<void> {
  const serverA = await startTestServer();
  const serverB = await startTestServer();
  try {
    // selfUrl has to be set post-boot since each server needs to know its own
    // randomly-assigned port; buildServer captures it in the FederationManager
    // constructor, so rebuild-free tests instead pass selfUrl explicitly via
    // startTestServer's opts where a test needs outbound advertisement.
    await fn(serverA, serverB);
  } finally {
    await Promise.all([serverA.close(), serverB.close()]);
  }
}

test("GET /server/identity exposes the persisted ed25519 public key", async () => {
  await pairedServers(async (serverA) => {
    const res = await getJson<{ publicKey: string }>(`${serverA.url}/server/identity`);
    assert.equal(res.status, 200);
    assert.equal(res.body.publicKey, serverA.built.identity.publicKey);
    assert.equal(res.body.publicKey.length, 64, "32-byte ed25519 public key, hex-encoded");
  });
});

test("committed diffs propagate to a federated peer automatically", async () => {
  const serverA = await startTestServer();
  const serverB = await startTestServer({ selfUrl: undefined });
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const tokenA = await authenticateAgent(serverA.url, roomId, agent);
    const tokenB = await authenticateAgent(serverB.url, roomId, agent);

    // Federation trust is bidirectional by design: A must know B to push to
    // it, and B must independently know A to accept pushes from it.
    const addPeerOnA = await postJson<{ peers: string[] }>(
      `${serverA.url}/rooms/${roomId}/federation`,
      { action: "add", peerUrl: serverB.url },
      tokenA
    );
    assert.equal(addPeerOnA.status, 200);
    assert.deepEqual(addPeerOnA.body.peers, [serverB.url]);
    await postJson(`${serverB.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverA.url }, tokenB);

    const listOnA = await getJson<{ peers: string[] }>(`${serverA.url}/rooms/${roomId}/federation`, tokenA);
    assert.deepEqual(listOnA.body.peers, [serverB.url]);

    const link = await createSignedLink(agent, { source: "x", predicate: "knows", target: "y" });
    const commitRes = await postJson<{ sequence: number; revision: string }>(
      `${serverA.url}/rooms/${roomId}/commit`,
      { additions: [link], removals: [] },
      tokenA
    );
    assert.equal(commitRes.status, 200);

    await waitFor(async () => {
      const render = await getJson<{ links: LinkExpression[] }>(`${serverB.url}/rooms/${roomId}/render`, tokenB);
      return render.body.links.length === 1;
    });

    const renderB = await getJson<{ links: LinkExpression[]; revision: string }>(
      `${serverB.url}/rooms/${roomId}/render`,
      tokenB
    );
    assert.equal(renderB.body.links[0].proof.signature, link.proof.signature);
    assert.equal(renderB.body.revision, commitRes.body.revision);
  } finally {
    await Promise.all([serverA.close(), serverB.close()]);
  }
});

test("federate is rejected from a server that was never added as a peer", async () => {
  await pairedServers(async (serverA, serverB) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    await authenticateAgent(serverA.url, roomId, agent); // ensures the room exists on A

    // B was never registered as A's peer. Have B sign a legitimate federate
    // request with its own real identity and send it directly to A.
    const diff = { additions: [], removals: [] };
    const sequence = 1;
    const revision = "deadbeef";
    const timestamp = new Date().toISOString();
    const payload = canonicalFederationPayload("federate", roomId, { diff, sequence, revision }, timestamp);
    const serverSignature = await signHex(
      Buffer.from(serverB.built.identity.privateKey, "hex"),
      payload
    );

    const res = await postJson<{ error: string }>(`${serverA.url}/rooms/${roomId}/federate`, {
      diff,
      sequence,
      revision,
      timestamp,
      serverPublicKey: serverB.built.identity.publicKey,
      serverSignature,
    });
    assert.equal(res.status, 403);
  });
});

test("federate with a forged signature from a known peer is rejected", async () => {
  await pairedServers(async (serverA, serverB) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const tokenA = await authenticateAgent(serverA.url, roomId, agent);
    await postJson(`${serverA.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverB.url }, tokenA);

    const diff = { additions: [], removals: [] };
    const res = await postJson<{ error: string }>(`${serverA.url}/rooms/${roomId}/federate`, {
      diff,
      sequence: 1,
      revision: "deadbeef",
      timestamp: new Date().toISOString(),
      serverPublicKey: serverB.built.identity.publicKey,
      serverSignature: "00".repeat(64), // garbage
    });
    assert.equal(res.status, 403);
  });
});

test("federate accepts a diff with a tampered link signature (server relays signatures as metadata)", async () => {
  // The server does not verify individual link signatures — it trusts the
  // peer's server-level ed25519 signature on the federation payload.
  // Links and their proofs travel as metadata; downstream consumers can
  // verify if they choose.
  await pairedServers(async (serverA, serverB) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const impostor = await createTestAgent();
    const tokenA = await authenticateAgent(serverA.url, roomId, agent);
    await postJson(`${serverA.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverB.url }, tokenA);

    const link = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    // Tamper with the author after signing -- signature no longer matches.
    const tampered = { ...link, author: impostor.did };
    const diff = { additions: [tampered], removals: [] };
    const timestamp = new Date().toISOString();
    const payload = canonicalFederationPayload("federate", roomId, { diff, sequence: 1, revision: "x" }, timestamp);
    const serverSignature = await signHex(Buffer.from(serverB.built.identity.privateKey, "hex"), payload);

    const res = await postJson<{ error: string }>(`${serverA.url}/rooms/${roomId}/federate`, {
      diff,
      sequence: 1,
      revision: "x",
      timestamp,
      serverPublicKey: serverB.built.identity.publicKey,
      serverSignature,
      serverUrl: serverB.url,
    });
    assert.equal(res.status, 200);
  });
});

test("reconciliation pulls diffs committed before peering was established", async () => {
  const serverA = await startTestServer({ selfUrl: undefined });
  const serverB = await startTestServer({ selfUrl: undefined });
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const tokenA = await authenticateAgent(serverA.url, roomId, agent);
    const tokenB = await authenticateAgent(serverB.url, roomId, agent);

    // Commit on A *before* any peering exists -- B never gets pushed this.
    const link = await createSignedLink(agent, { source: "p", predicate: "rel", target: "q" });
    await postJson(`${serverA.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, tokenA);

    const stillEmpty = await getJson<{ links: LinkExpression[] }>(`${serverB.url}/rooms/${roomId}/render`, tokenB);
    assert.equal(stillEmpty.body.links.length, 0);

    // Now establish bidirectional peering...
    await postJson(`${serverA.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverB.url }, tokenA);
    await postJson(`${serverB.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverA.url }, tokenB);

    // ...and have B actively reconcile against A to pull what it's missing.
    await serverB.built.federation.reconcileRoom(roomId);

    const renderB = await getJson<{ links: LinkExpression[] }>(`${serverB.url}/rooms/${roomId}/render`, tokenB);
    assert.equal(renderB.body.links.length, 1);
    assert.equal(renderB.body.links[0].proof.signature, link.proof.signature);
  } finally {
    await Promise.all([serverA.close(), serverB.close()]);
  }
});

test("peer public key is pinned on first verified contact if it wasn't captured at add-time", async () => {
  const serverA = await startTestServer();
  const serverB = await startTestServer();
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    await authenticateAgent(serverA.url, roomId, agent);

    // Simulate "admin added the peer while it was unreachable": register the
    // peer URL directly at the DB layer with no public key captured.
    serverA.built.db.addFederationPeer(roomId, serverB.url);

    const diff = { additions: [], removals: [] };
    const sequence = 1;
    const revision = "deadbeef";
    const timestamp = new Date().toISOString();
    // The server now fetches ${serverUrl}/server/identity to verify the
    // claimed key matches before pinning, instead of blindly trusting the
    // first contact. Since serverB runs, the fetch succeeds and the key
    // gets verified + pinned.
    const payload = canonicalFederationPayload("federate", roomId, { diff, sequence, revision }, timestamp);
    const serverSignature = await signHex(Buffer.from(serverB.built.identity.privateKey, "hex"), payload);

    const first = await postJson<{ applied: number }>(`${serverA.url}/rooms/${roomId}/federate`, {
      diff,
      sequence,
      revision,
      timestamp,
      serverPublicKey: serverB.built.identity.publicKey,
      serverSignature,
      serverUrl: serverB.url,
    });
    assert.equal(first.status, 200, "first contact verifies + pins the key and succeeds");

    // A forged signature from an *unrelated* key claiming the same peerUrl
    // must now be rejected, proving the key really was pinned (not just
    // trusted-by-URL forever).
    const rogueEd = ed.utils.randomPrivateKey();
    const roguePub = ed.etc.bytesToHex(await ed.getPublicKeyAsync(rogueEd));
    const timestamp2 = new Date().toISOString();
    const roguePayload = canonicalFederationPayload("federate", roomId, {
      diff,
      sequence: 2,
      revision: "cafebabe",
    }, timestamp2);
    const rogueSig = await signHex(rogueEd, roguePayload);
    const second = await postJson<{ error: string }>(`${serverA.url}/rooms/${roomId}/federate`, {
      diff,
      sequence: 2,
      revision: "cafebabe",
      timestamp: timestamp2,
      serverPublicKey: roguePub,
      serverSignature: rogueSig,
      serverUrl: serverB.url,
    });
    assert.equal(second.status, 403);
  } finally {
    await Promise.all([serverA.close(), serverB.close()]);
  }
});

test("removing a federation peer stops further propagation", async () => {
  const serverA = await startTestServer();
  const serverB = await startTestServer();
  try {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const tokenA = await authenticateAgent(serverA.url, roomId, agent);
    const tokenB = await authenticateAgent(serverB.url, roomId, agent);

    await postJson(`${serverA.url}/rooms/${roomId}/federation`, { action: "add", peerUrl: serverB.url }, tokenA);
    await postJson(
      `${serverA.url}/rooms/${roomId}/federation`,
      { action: "remove", peerUrl: serverB.url },
      tokenA
    );
    const list = await getJson<{ peers: string[] }>(`${serverA.url}/rooms/${roomId}/federation`, tokenA);
    assert.deepEqual(list.body.peers, []);

    const link = await createSignedLink(agent, { source: "a", predicate: "rel", target: "b" });
    await postJson(`${serverA.url}/rooms/${roomId}/commit`, { additions: [link], removals: [] }, tokenA);

    // Give any (incorrect) propagation a moment, then confirm B never got it.
    await new Promise((resolve) => setTimeout(resolve, 200));
    const renderB = await getJson<{ links: LinkExpression[] }>(`${serverB.url}/rooms/${roomId}/render`, tokenB);
    assert.equal(renderB.body.links.length, 0);
  } finally {
    await Promise.all([serverA.close(), serverB.close()]);
  }
});
