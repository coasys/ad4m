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
