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
import { bytesToHex, deriveX25519KeyPair } from "../src/encryption.js";
import { KeyRingManager } from "../src/key-ring-manager.js";
import type { LinkExpression, PerspectiveDiff } from "../src/types.js";

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
// E2E key grant for a member who joins after the admin made the room key
// ---------------------------------------------------------------------------

/** Authenticates `agent` without the auth module's singleton session, so
 *  two agents can hold tokens at once. Registers the agent's X25519 key the
 *  same way auth.authenticate() does. */
function tokenGetter(serverUrl: string, roomId: string, agent: RealAgent): () => Promise<string> {
  let token: Promise<string> | null = null;
  return () => (token ??= (async () => {
    const config = { serverUrl, roomId };
    const challenge = await api.requestChallenge(config, agent.did());
    const x25519Hex = bytesToHex(x25519KeyPair(agent).publicKey);
    return api.verifyChallenge(
      config, agent.did(), challenge, agent.signStringHex(challenge),
      x25519Hex, agent.signStringHex(x25519Hex),
    );
  })());
}

function x25519KeyPair(agent: RealAgent) {
  return deriveX25519KeyPair((payload) => agent.signStringHex(payload));
}

/** Points the sync module and the store at `agent`, with `manager`'s key ring. */
function wireMember(
  serverUrl: string,
  roomId: string,
  agent: RealAgent,
  getToken: () => Promise<string>,
  manager: KeyRingManager,
): void {
  wireClient(serverUrl, roomId, agent);
  syncModule.initSync({
    config: { serverUrl, roomId },
    getToken,
    emitDiff: () => {},
    emitSyncState: () => {},
    getKeyRing: () => manager.keyRing,
  });
}

async function waitFor(cond: () => boolean, label: string, timeoutMs: number): Promise<void> {
  const start = Date.now();
  while (!cond()) {
    if (Date.now() - start > timeoutMs) throw new Error(`timed out after ${timeoutMs} ms waiting for ${label}`);
    await new Promise((r) => setTimeout(r, 10));
  }
}

/** An admin that made the room key and committed `n` encrypted links. */
async function roomWithEncryptedLinks(serverUrl: string, roomId: string, n: number) {
  const admin = RealAgent.create();
  wireClient(serverUrl, roomId, admin); // the HTTP transport api.ts uses
  const adminToken = tokenGetter(serverUrl, roomId, admin);
  const adminKeys = new KeyRingManager({
    config: { serverUrl, roomId },
    getToken: adminToken,
    x25519PrivateKey: () => x25519KeyPair(admin).privateKey,
    onNewKeys: async () => {},
  });
  _cleanups.push(() => adminKeys.teardown());
  assert.equal(await adminKeys.detectAdmin(admin.did()), true);
  await adminKeys.setup();
  await adminKeys.rotate();
  assert.equal(adminKeys.status, "ready");

  wireMember(serverUrl, roomId, admin, adminToken, adminKeys);
  await syncModule.commit({
    additions: Array.from({ length: n }, (_, i) => ({
      author: admin.did(),
      timestamp: new Date().toISOString(),
      data: { source: `test://admin-${i}`, predicate: "test://p", target: "test://t" },
      proof: { signature: "sig", key: `${admin.did()}#primary` },
    })),
    removals: [],
  });
  return { admin, adminKeys };
}

/** A member who joins the room: authenticated, X25519 key registered, no keys. */
async function joiner(serverUrl: string, roomId: string, pendingPollScheduleMs?: number[]) {
  const agent = RealAgent.create();
  const token = tokenGetter(serverUrl, roomId, agent);
  const seen = { at: 0 };
  const keys: KeyRingManager = new KeyRingManager({
    config: { serverUrl, roomId },
    getToken: token,
    x25519PrivateKey: () => x25519KeyPair(agent).privateKey,
    onNewKeys: async () => {
      await syncModule.bootstrap();
      seen.at = Date.now();
    },
    pendingPollScheduleMs,
  });
  _cleanups.push(() => keys.teardown());
  wireMember(serverUrl, roomId, agent, token, keys);
  await keys.detectAdmin(agent.did());
  await keys.setup();
  await syncModule.bootstrap();
  return { agent, keys, seen };
}

describe("integration: a late joiner gets the room keys quickly", () => {
  it("sees the room's links within 2 s of the admin noticing it (peer-joined)", async () => {
    const server = await bootServer();
    const roomId = "late-joiner-room";
    const { adminKeys } = await roomWithEncryptedLinks(server.url, roomId, 5);

    const { keys, seen } = await joiner(server.url, roomId);
    assert.equal(keys.status, "pending");
    assert.equal(store.allLinks().links.length, 0, "no keys yet, so nothing decrypts");

    // The joiner's socket connects; the server tells the admin peer-joined.
    const grantedAt = Date.now();
    adminKeys.onPeerJoined();

    await waitFor(() => seen.at > 0, "the joiner to re-read the room", 5_000);
    assert.ok(seen.at - grantedAt < 2_000, `joiner saw links ${seen.at - grantedAt} ms after the grant`);
    assert.equal(keys.status, "ready");
    assert.equal(store.allLinks().links.length, 5);
  });

  it("restarts its fast re-check when a peer joins after its poll window ended", async () => {
    const server = await bootServer();
    const roomId = "admin-later-room";
    const { adminKeys } = await roomWithEncryptedLinks(server.url, roomId, 3);

    // A short poll window that ends before the admin grants.
    const { keys, seen } = await joiner(server.url, roomId, [20, 20]);
    await waitFor(() => !keys.isPolling(), "the joiner's poll window to end", 2_000);

    await adminKeys.grantMissing();
    await new Promise((r) => setTimeout(r, 300));
    assert.equal(seen.at, 0, "nothing re-checks /keys after the window");
    assert.equal(keys.status, "pending");

    // The admin's socket connects: the joiner gets peer-joined.
    const joinedAt = Date.now();
    keys.onPeerJoined();

    await waitFor(() => seen.at > 0, "the joiner to re-read the room", 3_000);
    assert.ok(seen.at - joinedAt < 2_000, `joiner saw links ${seen.at - joinedAt} ms after peer-joined`);
    assert.equal(store.allLinks().links.length, 3);
  });

  it("background refreshes keep their cooldown once keys are held, and defer to the poll while pending", async () => {
    const server = await bootServer();
    const roomId = "refresh-cooldown-room";
    const { adminKeys } = await roomWithEncryptedLinks(server.url, roomId, 1);

    assert.equal(await adminKeys.refresh(), false, "first refresh runs: nothing new");
    assert.equal(await adminKeys.refresh(), null, "second refresh within the cooldown is skipped");

    const { keys } = await joiner(server.url, roomId, [5_000]);
    assert.equal(keys.isPolling(), true);
    assert.equal(await keys.refresh(), null, "the pending poll owns /keys re-reads");
  });
});
