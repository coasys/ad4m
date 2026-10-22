/**
 * Integration tests: server-link-language client modules against a real link-server.
 *
 * Wires the pure client modules (auth, api, sync, store, ws-client) against
 * a real link-server instance (booted per test from ../../link-server), with
 * real ed25519 signing matching the AD4M executor convention. No mocks of
 * the server. Closes the gap between "client logic works against mock
 * transport" and "client logic works against a real server."
 *
 * Tests validate:
 *   - Real DID challenge-response auth flow
 *   - Commit → HTTP sync round-trip
 *   - Bootstrap (cold-start /render pull)
 *   - Catch-up sync (/sync?since=N)
 *   - Inbound WebSocket diff push
 *   - Two-agent sync through the same server
 */

import { describe, it, afterEach } from "node:test";
import assert from "node:assert/strict";
import { ed25519 } from "@noble/curves/ed25519";

import type {
  AgentAdapter,
  StorageAdapter,
  Transport,
  TransportResponse,
  WebSocketFactory,
  WSConnection,
} from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import * as auth from "../src/auth.js";
import * as store from "../src/store.js";
import * as syncModule from "../src/sync.js";
import * as api from "../src/api.js";
import { buildKeyRing, bytesToHex, generateRoomKey, hexToBytes, sealToDid } from "../src/encryption.js";
import { grantMissingRoomKeys, rotateRoomKey } from "../src/room-keys.js";
import type { LinkExpression, PerspectiveDiff, SealedBox } from "../src/types.js";
import { openSealed } from "./sealed-box.js";

// Link-server imports — test helpers to boot a real server, and auth
// utilities for DID construction + the SHA-256 pre-hash convention.
import {
  startTestServer,
  type TestServerHandle,
} from "../../../link-server/tests/helpers.js";
import { sha256Hex } from "../../../link-server/src/types.js";
import {
  publicKeyToDid,
  hashMessageForVerify,
} from "../../../link-server/src/auth.js";

// ---------------------------------------------------------------------------
// Real adapters (no mocks — talk to the real server)
// ---------------------------------------------------------------------------

class FetchTransport implements Transport {
  async fetch(
    url: string,
    method: string,
    headers: Record<string, string>,
    body: string,
  ): Promise<TransportResponse> {
    const res = await globalThis.fetch(url, {
      method,
      headers,
      body: method !== "GET" && method !== "HEAD" ? body : undefined,
    });
    const text = await res.text();
    const resHeaders: Record<string, string> = {};
    res.headers.forEach((v, k) => { resHeaders[k] = v; });
    return { status: res.status, headers: resHeaders, body: text };
  }
}

class MemoryStorage implements StorageAdapter {
  private data = new Map<string, string>();
  get(key: string): string | null { return this.data.get(key) ?? null; }
  put(key: string, value: string): void { this.data.set(key, value); }
  delete(key: string): void { this.data.delete(key); }
  listKeys(prefix?: string): string[] {
    return [...this.data.keys()].filter((k) => !prefix || k.startsWith(prefix));
  }
}

/** Real ed25519 agent that signs using the AD4M executor convention:
 *  sign(SHA-256(payload)), matching what the server verifies.
 *  Uses @noble/curves/ed25519 (sync API) — already a devDep of this package. */
class RealAgent implements AgentAdapter {
  readonly didValue: string;
  private readonly privateKey: Uint8Array;

  constructor(didValue: string, privateKey: Uint8Array) {
    this.didValue = didValue;
    this.privateKey = privateKey;
  }

  did(): string { return this.didValue; }

  signStringHex(payload: string): string {
    // The AD4M executor convention: sign(SHA-256(message)).
    // hashMessageForVerify returns the SHA-256 digest as Uint8Array.
    const hashed = hashMessageForVerify(payload);
    const sig = ed25519.sign(hashed, this.privateKey);
    return Buffer.from(sig).toString("hex");
  }

  /** The executor's `agentOpenSealed`, with this agent's key. */
  openSealed(sealed: SealedBox): string {
    return bytesToHex(openSealed(sealed, this.privateKey));
  }

  static create(): RealAgent {
    const privateKey = ed25519.utils.randomPrivateKey();
    const publicKey = ed25519.getPublicKey(privateKey);
    const didValue = publicKeyToDid(publicKey);
    return new RealAgent(didValue, privateKey);
  }
}

/** No-op WS factory — the HTTP-only integration tests don't open WebSockets.
 *  A real WS test would import `ws` from the link-server's devDeps. */
class NullWsFactory implements WebSocketFactory {
  connect(_url: string): WSConnection {
    throw new Error("NullWsFactory: WebSocket not available in this test suite");
  }
}

// ---------------------------------------------------------------------------
// Test harness
// ---------------------------------------------------------------------------

let _server: TestServerHandle | null = null;
const _cleanups: Array<() => void> = [];

async function bootServer(): Promise<TestServerHandle> {
  if (_server) await _server.close();
  _server = await startTestServer({ autoAdmit: true });
  return _server;
}

/** Wire the server-link-language adapters against a real server + agent. */
function wireClient(
  serverUrl: string,
  roomId: string,
  agent: RealAgent,
): { emittedDiffs: PerspectiveDiff[]; syncStates: string[] } {
  resetAdapters();
  syncModule._resetBatchStateForTests();
  auth.resetAuth();

  const emittedDiffs: PerspectiveDiff[] = [];
  const syncStates: string[] = [];

  initAdapters({
    storage: new MemoryStorage(),
    transport: new FetchTransport(),
    agent,
    wsFactory: new NullWsFactory(),
    config: { serverUrl, roomId },
  });

  store.initStore(sha256Hex);

  syncModule.initSync({
    config: { serverUrl, roomId },
    getToken: () => auth.getValidToken(),
    invalidateToken: () => auth.invalidateSession(),
    emitDiff: (diff) => emittedDiffs.push(diff),
    emitSyncState: (state) => syncStates.push(state),
    getKeyRing: () => null,
  });

  return { emittedDiffs, syncStates };
}

afterEach(async () => {
  for (const fn of _cleanups.splice(0)) fn();
  resetAdapters();
  syncModule._resetBatchStateForTests();
  auth.resetAuth();
  if (_server) {
    await _server.close();
    _server = null;
  }
});

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("integration: auth against real server", () => {
  it("authenticate() returns a valid JWT via real DID challenge-response", async () => {
    const server = await bootServer();
    const agent = RealAgent.create();
    wireClient(server.url, "auth-test-room", agent);

    const session = await auth.authenticate();

    assert.ok(session.token, "must receive a token");
    assert.ok(session.token.split(".").length === 3, "token must look like a JWT");
    assert.ok(session.expiresAt! > Date.now(), "token must expire in the future");
  });

  it("getValidToken() caches and returns the same token on subsequent calls", async () => {
    const server = await bootServer();
    const agent = RealAgent.create();
    wireClient(server.url, "cache-test-room", agent);

    const t1 = await auth.getValidToken();
    const t2 = await auth.getValidToken();
    assert.equal(t1, t2, "cached token must match");
  });
});

describe("integration: commit + HTTP sync", () => {
  it("committed links appear in /render and /sync", async () => {
    const server = await bootServer();
    const agent = RealAgent.create();
    const { emittedDiffs } = wireClient(server.url, "commit-sync-room", agent);

    const link: LinkExpression = {
      author: agent.did(),
      timestamp: new Date().toISOString(),
      data: { source: "test://int-source", predicate: "test://p", target: "test://int-target" },
      proof: { signature: "test-sig", key: `${agent.did()}#primary` },
    };

    // Commit via the sync module
    await syncModule.commit({ additions: [link], removals: [] });

    // Verify via direct HTTP (bypass the client — confirm server state)
    const token = await auth.getValidToken();
    const transport = new FetchTransport();
    const renderRes = await transport.fetch(
      `${server.url}/rooms/commit-sync-room/render`,
      "GET",
      { Authorization: `Bearer ${token}` },
      "",
    );
    const rendered = JSON.parse(renderRes.body) as { links: LinkExpression[] };
    assert.equal(rendered.links.length, 1);
    assert.equal((rendered.links[0].data as { source: string }).source, "test://int-source");
  });
});

describe("integration: a rejected session is replaced", () => {
  // The server drops sessions (ACL change, expiry sweep, data reset). The
  // cached JWT then 401s on every call; the language must re-authenticate
  // instead of retrying the dead token until the executor restarts.
  it("commit and catchUp re-authenticate after the server revokes the session", async () => {
    const server = await bootServer();
    const agent = RealAgent.create();
    wireClient(server.url, "revoked-session-room", agent);

    const link = (source: string): LinkExpression => ({
      author: agent.did(),
      timestamp: new Date().toISOString(),
      data: { source, predicate: "test://p", target: "test://t" },
      proof: { signature: "test-sig", key: `${agent.did()}#primary` },
    });

    await syncModule.commit({ additions: [link("test://before")], removals: [] });
    server.built.db.deleteSessionsForDid("revoked-session-room", agent.did());
    await syncModule.commit({ additions: [link("test://after")], removals: [] });

    server.built.db.deleteSessionsForDid("revoked-session-room", agent.did());
    await syncModule.catchUp();

    server.built.db.deleteSessionsForDid("revoked-session-room", agent.did());
    await syncModule.bootstrap();
    const sources = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://after", "test://before"], "both commits must be on the server");
  });
});

describe("integration: bootstrap (cold-start /render pull)", () => {
  it("bootstrap() pulls existing links from a populated room", async () => {
    const server = await bootServer();
    const agentA = RealAgent.create();
    const roomId = "bootstrap-room";

    // Agent A commits links
    wireClient(server.url, roomId, agentA);
    for (let i = 0; i < 3; i++) {
      await syncModule.commit({
        additions: [{
          author: agentA.did(),
          timestamp: new Date().toISOString(),
          data: { source: `test://boot-${i}`, predicate: "test://p", target: "test://t" },
          proof: { signature: `sig-${i}`, key: `${agentA.did()}#primary` },
        }],
        removals: [],
      });
    }

    // Agent B bootstraps into the same room
    const agentB = RealAgent.create();
    const { emittedDiffs: bDiffs } = wireClient(server.url, roomId, agentB);

    await syncModule.bootstrap();

    // bootstrap() does NOT emit diffs (it populates the store silently)
    assert.equal(bDiffs.length, 0, "bootstrap must not emit diffs");

    // But the store should have all 3 links
    const links = store.allLinks().links;
    assert.equal(links.length, 3, "B's store must contain all 3 of A's links");

    const sources = links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://boot-0", "test://boot-1", "test://boot-2"]);
  });
});

describe("integration: catch-up sync", () => {
  it("catchUp() fetches diffs committed after the stored sequence cursor", async () => {
    const server = await bootServer();
    const agentA = RealAgent.create();
    const roomId = "catchup-room";

    // A commits 2 links
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({
      additions: [{
        author: agentA.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://early", predicate: "test://p", target: "test://t" },
        proof: { signature: "sig-early", key: `${agentA.did()}#primary` },
      }],
      removals: [],
    });
    await syncModule.commit({
      additions: [{
        author: agentA.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://late", predicate: "test://p", target: "test://t" },
        proof: { signature: "sig-late", key: `${agentA.did()}#primary` },
      }],
      removals: [],
    });

    // B bootstraps (gets both links), then A commits a third
    const agentB = RealAgent.create();
    const { emittedDiffs: bDiffs } = wireClient(server.url, roomId, agentB);
    await syncModule.bootstrap();
    assert.equal(store.allLinks().links.length, 2);
    const seqAfterBootstrap = store.getSequence();

    // Switch back to A and commit a third link
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({
      additions: [{
        author: agentA.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://missed", predicate: "test://p", target: "test://t" },
        proof: { signature: "sig-missed", key: `${agentA.did()}#primary` },
      }],
      removals: [],
    });

    // Switch to B, set the sequence cursor to where bootstrap left off, and catch up
    const bDiffs2: PerspectiveDiff[] = [];
    wireClient(server.url, roomId, agentB);
    // Re-bootstrap to restore B's store state
    await syncModule.bootstrap();
    // Manually set sequence to simulate B having bootstrapped earlier
    store.setSequence(seqAfterBootstrap);

    syncModule.initSync({
      config: { serverUrl: server.url, roomId },
      getToken: () => auth.getValidToken(),
      emitDiff: (diff) => bDiffs2.push(diff),
      emitSyncState: () => {},
      getKeyRing: () => null,
    });

    const lastDiff = await syncModule.catchUp();

    assert.ok(bDiffs2.length >= 1, "catch-up must emit the missed diff");
    const sources = bDiffs2.flatMap((d) => d.additions).map((l) => (l.data as { source: string }).source);
    assert.ok(sources.includes("test://missed"), "missed link must appear in emitted diffs");
  });
});

describe("integration: two-agent commit + sync through real server", () => {
  it("agent A commits, agent B syncs and sees A's links; B commits, A syncs and sees B's", async () => {
    const server = await bootServer();
    const agentA = RealAgent.create();
    const agentB = RealAgent.create();
    const roomId = "two-agent-room";

    // A commits
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({
      additions: [{
        author: agentA.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://from-a", predicate: "test://p", target: "test://to-a" },
        proof: { signature: "sig-a", key: `${agentA.did()}#primary` },
      }],
      removals: [],
    });

    // B bootstraps — should see A's link
    const bDiffs: PerspectiveDiff[] = [];
    wireClient(server.url, roomId, agentB);
    syncModule.initSync({
      config: { serverUrl: server.url, roomId },
      getToken: () => auth.getValidToken(),
      emitDiff: (diff) => bDiffs.push(diff),
      emitSyncState: () => {},
      getKeyRing: () => null,
    });
    await syncModule.bootstrap();
    assert.equal(store.allLinks().links.length, 1);
    assert.equal(
      (store.allLinks().links[0].data as { source: string }).source,
      "test://from-a"
    );

    // B commits
    await syncModule.commit({
      additions: [{
        author: agentB.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://from-b", predicate: "test://p", target: "test://to-b" },
        proof: { signature: "sig-b", key: `${agentB.did()}#primary` },
      }],
      removals: [],
    });

    // A syncs — should see B's link
    wireClient(server.url, roomId, agentA);
    syncModule.initSync({
      config: { serverUrl: server.url, roomId },
      getToken: () => auth.getValidToken(),
      emitDiff: () => {},
      emitSyncState: () => {},
      getKeyRing: () => null,
    });
    await syncModule.bootstrap();
    const aLinks = store.allLinks().links;
    assert.equal(aLinks.length, 2);
    const aSources = aLinks.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(aSources, ["test://from-a", "test://from-b"]);
  });
});

describe("integration: performSync", () => {
  it("performSync() reports Synced on success", async () => {
    const server = await bootServer();
    const agent = RealAgent.create();
    const { syncStates } = wireClient(server.url, "perfsync-room", agent);

    const result = await syncModule.performSync();
    assert.deepEqual(result, { additions: [], removals: [] });
    assert.ok(syncStates.includes("Synced"), "must report Synced state");
  });
});

// ---------------------------------------------------------------------------
// Room keys: sealed to DIDs, rotated by the admin, granted by any member
// ---------------------------------------------------------------------------

describe("integration: room keys", () => {
  const roomId = "room-keys-room";

  /** Authenticates `agent` as the language would, and returns its token. */
  async function signIn(serverUrl: string, agent: RealAgent): Promise<string> {
    wireClient(serverUrl, roomId, agent);
    return auth.getValidToken();
  }

  async function admit(serverUrl: string, adminToken: string, did: string): Promise<void> {
    const res = await fetch(`${serverUrl}/rooms/${roomId}/acl`, {
      method: "POST",
      headers: { "content-type": "application/json", authorization: `Bearer ${adminToken}` },
      body: JSON.stringify({ action: "add", did }),
    });
    assert.equal(res.status, 200);
  }

  async function ringOf(serverUrl: string, agent: RealAgent) {
    const token = await signIn(serverUrl, agent);
    const keys = await api.fetchRoomKeys({ serverUrl, roomId }, token);
    return buildKeyRing(keys?.keys ?? [], agent).ring;
  }

  it("the admin seals a new version to every ACL member, including one that never connected", async () => {
    const server = await bootServer();
    const admin = RealAgent.create();
    const member = RealAgent.create();
    const adminToken = await signIn(server.url, admin);
    await admit(server.url, adminToken, member.did());

    const rotated = await rotateRoomKey({ serverUrl: server.url, roomId }, adminToken);
    assert.equal(rotated?.version, 1);
    assert.deepEqual(rotated?.recipients.sort(), [admin.did(), member.did()].sort());

    const adminRing = await ringOf(server.url, admin);
    const memberRing = await ringOf(server.url, member);
    assert.deepEqual([...memberRing.keys()], [1]);
    assert.deepEqual(memberRing.get(1), adminRing.get(1), "both hold the same room key");
  });

  it("a member who is not the admin grants the versions it holds, and only those", async () => {
    const server = await bootServer();
    const admin = RealAgent.create();
    const bob = RealAgent.create();
    const carol = RealAgent.create();
    const config = { serverUrl: server.url, roomId };

    const adminToken = await signIn(server.url, admin);
    await rotateRoomKey(config, adminToken); // version 1: the admin alone
    await admit(server.url, adminToken, bob.did());
    await rotateRoomKey(config, adminToken); // version 2: the admin and Bob
    await admit(server.url, adminToken, carol.did()); // Carol: no version, never connected

    const bobRing = await ringOf(server.url, bob);
    assert.deepEqual([...bobRing.keys()], [2]);
    const bobToken = await signIn(server.url, bob);
    const bobGrants = await grantMissingRoomKeys(config, bobToken, bobRing, bob.did());
    assert.deepEqual(bobGrants, [{ did: carol.did(), versions: [2] }], "Bob grants Carol the version he holds");

    const adminRing = await ringOf(server.url, admin);
    const adminGrants = await grantMissingRoomKeys(config, await signIn(server.url, admin), adminRing, admin.did());
    assert.deepEqual(
      adminGrants.map((g) => [g.did, g.versions]).sort(),
      [[bob.did(), [1]], [carol.did(), [1]]].sort(),
      "the admin fills in version 1",
    );

    const carolRing = await ringOf(server.url, carol);
    assert.deepEqual([...carolRing.keys()].sort(), [1, 2]);
    assert.deepEqual(carolRing.get(1), adminRing.get(1));
    assert.deepEqual(carolRing.get(2), adminRing.get(2));

    const again = await grantMissingRoomKeys(config, await signIn(server.url, bob), await ringOf(server.url, bob), bob.did());
    assert.deepEqual(again, [], "nobody lacks a version any more");
  });

  it("a wrong key a member grants is spoiled, dropped, and replaced by an honest grant", async () => {
    const server = await bootServer();
    const admin = RealAgent.create();
    const mallory = RealAgent.create();
    const late = RealAgent.create();
    const config = { serverUrl: server.url, roomId };

    const adminToken = await signIn(server.url, admin);
    await admit(server.url, adminToken, mallory.did());
    await rotateRoomKey(config, adminToken); // version 1: the admin and Mallory
    await admit(server.url, adminToken, late.did());

    // Mallory holds version 1, so the server lets her grant it: she seals a key of her own.
    await api.grantKeys(config, await signIn(server.url, mallory), late.did(), [
      { version: 1, encryptedKey: sealToDid(late.did(), generateRoomKey()) },
    ]);
    const lateToken = await signIn(server.url, late);
    const first = buildKeyRing((await api.fetchRoomKeys(config, lateToken))?.keys ?? [], late);
    assert.equal(first.ring.size, 0, "the wrong key is not taken");
    assert.deepEqual(first.spoiled, [1]);

    await api.deleteRoomKey(config, lateToken, 1);
    const adminRing = await ringOf(server.url, admin);
    const grants = await grantMissingRoomKeys(config, await signIn(server.url, admin), adminRing, admin.did());
    assert.deepEqual(grants, [{ did: late.did(), versions: [1] }], "the dropped version is missing again");

    const lateRing = await ringOf(server.url, late);
    assert.deepEqual(lateRing.get(1), adminRing.get(1), "the newcomer now holds the room's key");
  });
});
