/**
 * Integration tests: server-link-language client modules against a real link-server.
 *
 * Wires the pure client modules (auth, api, sync, store, room-keys) against
 * a real link-server instance (booted per test from ../../link-server, with its
 * fake identity store), with real ed25519 signing matching the AD4M executor
 * convention. No mocks of the server.
 *
 * Rooms are protocol 2: a room exists once its creator's genesis (`POST /membership`
 * with `{creator, salt}` and the rules link) made it, and a key authenticates with
 * one signed request carrying its X25519 key. The server auto-admits device-only
 * keys (`startTestServer`'s default), so the sync tests use fresh keys; the
 * membership tests sign with the fixture's persons.
 *
 * Tests validate:
 *   - Protocol 2 auth, and the room's genesis through `commit`
 *   - Membership links: `/membership`, never `/commit`; catch-up hands them to the executor
 *   - Commit → HTTP sync round-trip
 *   - Bootstrap (cold-start /render pull), paged render and tail
 *   - Catch-up sync (/sync?since=N)
 *   - Room keys sealed to members' X25519 keys, rotated and granted by any member
 *   - Two-agent sync through the same server
 */

import { describe, it, afterEach, mock } from "node:test";
import assert from "node:assert/strict";
import { ed25519 } from "@noble/curves/ed25519";

import type {
  RoomConfig,
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
import {
  buildKeyRing,
  bytesToHex,
  decryptLinkFromWire,
  encryptLinkForWire,
  generateRoomKey,
  linkHashKeyOf,
  sealRoomKeyTo,
  type KeyRing,
} from "../src/encryption.js";
import { fromWireMembershipLink, roomIdOf as languageRoomIdOf } from "../src/room.js";
import { grantMissingRoomKeys, rotateRoomKey } from "../src/room-keys.js";
import type { LinkExpression, MembershipLinkResult, PerspectiveDiff, WireLinkExpression } from "../src/types.js";
import { isEncryptedLinkData } from "../src/types.js";
import { x25519Public } from "../keyring/keyring.js";
import { TestAgent } from "./agent.js";

// Link-server imports — test helpers to boot a real server and make rooms the protocol 2 way.
import {
  createRoom,
  startTestServer,
  type TestServerHandle,
} from "../../../link-server/tests/helpers.js";
import {
  links as membership,
  newSalt,
  personAgent,
  personDid,
  roomIdOf,
  signLink,
  type TestAgent as ServerTestAgent,
} from "../../../link-server/tests/identity.js";
import { sha256Hex } from "../../../link-server/src/types.js";

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

/** A fresh device-only key (no log lists it): the server auto-admits it. */
function newAgent(): TestAgent {
  return new TestAgent(ed25519.utils.randomPrivateKey());
}

/** The language's agent for a fixture person's device key: same seed, so the same did:key. */
function agentOf(key: ServerTestAgent): TestAgent {
  const agent = new TestAgent(key.privateKey);
  assert.equal(agent.did(), key.did);
  return agent;
}

/** No-op WS factory — the HTTP-only integration tests don't open WebSockets. */
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

async function bootServer(autoAdmit = true): Promise<TestServerHandle> {
  if (_server) await _server.close();
  _server = await startTestServer({ autoAdmit });
  return _server;
}

/** A room alice made (genesis with open rules), for the sync tests. */
function newRoom(server: TestServerHandle): Promise<string> {
  return createRoom(server.url);
}

/** The room key every agent of these tests holds, as members do once a rotation reached them.
 *  Rooms refuse plaintext, so every commit is encrypted. */
const SHARED_RING: KeyRing = new Map([[1, generateRoomKey()]]);

/** Wire the server-link-language adapters against a real server + agent. */
function wireClient(
  serverUrl: string,
  roomId: string,
  agent: TestAgent,
  storage: MemoryStorage = new MemoryStorage(),
  transport: Transport = new FetchTransport(),
  ring: KeyRing | null = SHARED_RING,
  extra: { config?: Partial<RoomConfig>; membershipLinks?: (links: LinkExpression[]) => Promise<MembershipLinkResult[]> } = {},
): { emittedDiffs: PerspectiveDiff[]; syncStates: string[] } {
  resetAdapters();
  syncModule._resetRenderStateForTests();
  auth.resetAuth();

  const emittedDiffs: PerspectiveDiff[] = [];
  const syncStates: string[] = [];
  const config: RoomConfig = { serverUrl, roomId, ...extra.config };

  initAdapters({
    storage,
    transport,
    agent,
    wsFactory: new NullWsFactory(),
    config,
  });

  store.initStore(sha256Hex);

  syncModule.initSync({
    config,
    getToken: () => auth.getValidToken(),
    invalidateToken: () => auth.invalidateSession(),
    emitDiff: (diff) => emittedDiffs.push(diff),
    emitSyncState: (state) => syncStates.push(state),
    getKeyRing: () => ring,
    membershipLinks: extra.membershipLinks,
  });

  return { emittedDiffs, syncStates };
}

afterEach(async () => {
  for (const fn of _cleanups.splice(0)) fn();
  syncModule._setPageLimitsForTests();
  resetAdapters();
  syncModule._resetRenderStateForTests();
  auth.resetAuth();
  if (_server) {
    await _server.close();
    _server = null;
  }
});

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("integration: auth against real server (protocol 2)", () => {
  it("authenticate() returns a JWT and whether the room admits the key, and publishes its X25519 key", async () => {
    const server = await bootServer();
    const roomId = await newRoom(server);
    const agent = newAgent();
    wireClient(server.url, roomId, agent);

    const session = await auth.authenticate();

    assert.ok(session.token.split(".").length === 3, "token must look like a JWT");
    assert.ok(session.expiresAt! > Date.now(), "token must expire in the future");
    assert.equal(session.admitted, true, "the test server auto-admits a device-only key");
    const members = await api.fetchMembers({ serverUrl: server.url, roomId }, session.token);
    const mine = members.keys.find((k) => k.did === agent.did());
    assert.equal(mine?.x25519PublicKey, bytesToHex(x25519Public(await agent.roomKeySeed())));
  });

  it("getValidToken() caches and returns the same token on subsequent calls", async () => {
    const server = await bootServer();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, newAgent());

    const t1 = await auth.getValidToken();
    const t2 = await auth.getValidToken();
    assert.equal(t1, t2, "cached token must match");
  });

  it("a room nobody made answers 404: auth never creates a room", async () => {
    const server = await bootServer();
    wireClient(server.url, roomIdOf(personDid("alice"), newSalt()), newAgent());
    await assert.rejects(() => auth.getValidToken(), (e: unknown) => e instanceof api.ApiError && e.status === 404);
  });
});

describe("integration: membership links (K5)", () => {
  it("the creator's first commit makes the room through its genesis; a person joins; catch-up hands the links to the executor", async () => {
    const server = await bootServer(false);
    const salt = newSalt();
    const creatorKey = personAgent("alice");
    const roomId = languageRoomIdOf(creatorKey.person!, salt);
    assert.equal(roomId, roomIdOf(creatorKey.person!, salt), "the language and the server agree on the room id");
    const genesis = { config: { creator: creatorKey.person!, uid: salt } };
    const rules = fromWireMembershipLink(signLink(creatorKey, membership.rules({ join: "open" })));
    const plain: LinkExpression = {
      author: creatorKey.did,
      timestamp: new Date().toISOString(),
      data: { source: "test://first", predicate: "test://p", target: "test://t" },
      proof: { signature: "sig", key: `${creatorKey.did}#primary` },
    };

    // Alice's executor commits the rules link and a plain link in one diff, before the room exists.
    wireClient(server.url, roomId, agentOf(creatorKey), new MemoryStorage(), new FetchTransport(), SHARED_RING, genesis);
    await syncModule.commit({ additions: [rules, plain], removals: [] });
    const aliceToken = await auth.getValidToken();
    const render = await api.fetchRender({ serverUrl: server.url, roomId }, aliceToken, { limit: 500, next: null });
    assert.equal(render.links.length, 1, "the plain link went to /commit");
    assert.deepEqual(decryptLinkFromWire(render.links[0], SHARED_RING), plain);

    // Bob, a person not yet admitted, joins the open room with his self-join.
    const bobKey = personAgent("bob");
    wireClient(server.url, roomId, agentOf(bobKey), new MemoryStorage(), new FetchTransport(), null, genesis);
    assert.equal((await auth.authenticate()).admitted, false);
    const join = fromWireMembershipLink(signLink(bobKey, membership.member(personDid("bob"))));
    await syncModule.commit({ additions: [join], removals: [] });
    auth.resetAuth();
    assert.equal((await auth.authenticate()).admitted, true, "the self-join counted");

    // Alice's executor, on a fresh store, reads the room's membership log: the rules and the join, in order.
    const handed: LinkExpression[][] = [];
    wireClient(server.url, roomId, agentOf(creatorKey), new MemoryStorage(), new FetchTransport(), SHARED_RING, {
      ...genesis,
      membershipLinks: async (links) => {
        handed.push(links);
        return links.map(() => ({ hash: "", counted: true }));
      },
    });
    await syncModule.catchUp();
    assert.deepEqual(handed.flat().map((l) => l.data.predicate), [
      "coasys://membership/rules",
      "coasys://membership/member",
    ]);
    assert.deepEqual(handed.flat()[1], join, "a link reaches the executor as it was signed");
    assert.equal(store.getMembershipSeq(), 2);
  });

  it("a refused membership link is logged, not thrown, and nothing is counted", async () => {
    const server = await bootServer(false);
    const roomId = await createRoom(server.url, { join: "invite" });
    const bobKey = personAgent("bob");
    wireClient(server.url, roomId, agentOf(bobKey), new MemoryStorage(), new FetchTransport(), null);
    const errors = mock.method(console, "error", () => undefined);
    try {
      await syncModule.commit({
        additions: [fromWireMembershipLink(signLink(bobKey, membership.member(personDid("bob"))))],
        removals: [],
      });
      assert.ok(errors.mock.calls.length >= 1, "the refusal is logged");
    } finally {
      errors.mock.restore();
    }
    auth.resetAuth();
    assert.equal((await auth.authenticate()).admitted, false);
  });
});

describe("integration: a room is end-to-end encrypted from creation", () => {
  it("the server refuses plaintext before any key exists, and a member with no key commits nothing", async () => {
    const server = await bootServer();
    const roomId = await newRoom(server);
    const first = newAgent();
    wireClient(server.url, roomId, first, new MemoryStorage(), new FetchTransport(), null);
    const firstToken = await auth.getValidToken();
    // Plaintext sent straight to the server, past the language: refused before any key or commit exists.
    await assert.rejects(
      () => api.commitDiff({ serverUrl: server.url, roomId }, firstToken, {
        additions: [{
          author: first.did(),
          timestamp: new Date().toISOString(),
          data: { source: "test://plain", predicate: "test://p", target: "test://t" },
        }],
        removals: [],
      }),
      (err) => err instanceof api.ApiError && err.status === 400,
    );

    // A member signs in before anyone rotated a key in: it holds none.
    const member = newAgent();
    wireClient(server.url, roomId, member, new MemoryStorage(), new FetchTransport(), null);
    const memberToken = await auth.getValidToken();
    assert.deepEqual(await api.fetchRoomKeys({ serverUrl: server.url, roomId }, memberToken), { keys: [] });
    await assert.rejects(() => syncModule.commit({
      additions: [{
        author: member.did(),
        timestamp: new Date().toISOString(),
        data: { source: "test://first", predicate: "test://p", target: "test://t" },
        proof: { signature: "sig", key: `${member.did()}#primary` },
      }],
      removals: [],
    }));
    const render = await api.fetchRender({ serverUrl: server.url, roomId }, memberToken, { limit: 500, next: null });
    assert.deepEqual(render.links, [], "nothing reached the server");
  });
});

describe("integration: commit + HTTP sync", () => {
  it("committed links appear in /render and /sync", async () => {
    const server = await bootServer();
    const agent = newAgent();
    const roomId = await newRoom(server);
    const { emittedDiffs } = wireClient(server.url, roomId, agent);

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
      `${server.url}/rooms/${roomId}/render`,
      "GET",
      { Authorization: `Bearer ${token}` },
      "",
    );
    const rendered = JSON.parse(renderRes.body) as { links: WireLinkExpression[] };
    assert.equal(rendered.links.length, 1);
    assert.ok(isEncryptedLinkData(rendered.links[0].data), "the server holds only ciphertext");
    assert.deepEqual(decryptLinkFromWire(rendered.links[0], SHARED_RING), link);
  });
});

describe("integration: a rejected session is replaced", () => {
  // The server drops sessions (ACL change, expiry sweep, data reset). The
  // cached JWT then 401s on every call; the language must re-authenticate
  // instead of retrying the dead token until the executor restarts.
  it("commit and catchUp re-authenticate after the server revokes the session", async () => {
    const server = await bootServer();
    const agent = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agent);

    const link = (source: string): LinkExpression => ({
      author: agent.did(),
      timestamp: new Date().toISOString(),
      data: { source, predicate: "test://p", target: "test://t" },
      proof: { signature: "test-sig", key: `${agent.did()}#primary` },
    });

    await syncModule.commit({ additions: [link("test://before")], removals: [] });
    server.built.db.deleteSessionsForDid(roomId, agent.did());
    await syncModule.commit({ additions: [link("test://after")], removals: [] });

    server.built.db.deleteSessionsForDid(roomId, agent.did());
    await syncModule.catchUp();

    server.built.db.deleteSessionsForDid(roomId, agent.did());
    await syncModule.bootstrap();
    const sources = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://after", "test://before"], "both commits must be on the server");
  });
});

describe("integration: bootstrap (cold-start /render pull)", () => {
  it("bootstrap() pulls existing links from a populated room", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);

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
    const agentB = newAgent();
    const { emittedDiffs: bDiffs } = wireClient(server.url, roomId, agentB);

    await syncModule.bootstrap();

    // The executor only learns of links from emitted diffs: bootstrap emits
    // the room's links, each once.
    const emitted = bDiffs.flatMap((d) => d.additions).map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(emitted, ["test://boot-0", "test://boot-1", "test://boot-2"]);

    const links = store.allLinks().links;
    assert.equal(links.length, 3, "B's store must contain all 3 of A's links");

    const sources = links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(sources, ["test://boot-0", "test://boot-1", "test://boot-2"]);
  });
});

describe("integration: catch-up sync", () => {
  it("catchUp() fetches diffs committed after the stored sequence cursor", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);

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
    const agentB = newAgent();
    const bStorage = new MemoryStorage();
    wireClient(server.url, roomId, agentB, bStorage);
    await syncModule.bootstrap();
    assert.equal(store.allLinks().links.length, 2);

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

    // B, with its store and cursor from the bootstrap, catches up
    const { emittedDiffs: bDiffs2 } = wireClient(server.url, roomId, agentB, bStorage);
    await syncModule.catchUp();

    const sources = bDiffs2.flatMap((d) => d.additions).map((l) => (l.data as { source: string }).source);
    assert.deepEqual(sources, ["test://missed"], "catch-up emits only the missed link");
  });
});

describe("integration: two-agent commit + sync through real server", () => {
  it("agent A commits, agent B syncs and sees A's links; B commits, A syncs and sees B's", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const agentB = newAgent();
    const roomId = await newRoom(server);

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
      getKeyRing: () => SHARED_RING,
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
      getKeyRing: () => SHARED_RING,
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
    const agent = newAgent();
    const roomId = await newRoom(server);
    const { syncStates } = wireClient(server.url, roomId, agent);

    const result = await syncModule.performSync();
    assert.deepEqual(result, { additions: [], removals: [] });
    assert.ok(syncStates.includes("Synced"), "must report Synced state");
  });
});

// ---------------------------------------------------------------------------
// Room keys: sealed to members' X25519 keys, rotated and granted by any member
// ---------------------------------------------------------------------------

describe("integration: room keys", () => {
  let roomId = "";
  const admitsAll = async () => true;

  /** Authenticates `agent` as the language would (publishing its X25519 key), and returns its token. */
  async function signIn(serverUrl: string, agent: TestAgent): Promise<string> {
    wireClient(serverUrl, roomId, agent);
    return auth.getValidToken();
  }

  async function ringOf(serverUrl: string, agent: TestAgent) {
    const token = await signIn(serverUrl, agent);
    const keys = await api.fetchRoomKeys({ serverUrl, roomId }, token);
    return buildKeyRing(keys?.keys ?? [], await agent.roomKeySeed()).ring;
  }

  async function boot(): Promise<{ server: TestServerHandle; config: RoomConfig }> {
    const server = await bootServer();
    roomId = await newRoom(server);
    return { server, config: { serverUrl: server.url, roomId } };
  }

  it("any member seals a new version to every admitted key, and each opens it with its own seed", async () => {
    const { server, config } = await boot();
    const first = newAgent();
    const member = newAgent();
    await signIn(server.url, member);
    const firstToken = await signIn(server.url, first);

    const rotated = await rotateRoomKey(config, firstToken, admitsAll);
    assert.equal(rotated?.result.version, 1);
    assert.deepEqual(rotated?.result.recipients.sort(), [first.did(), member.did()].sort());
    assert.deepEqual(rotated?.skipped, []);

    const firstRing = await ringOf(server.url, first);
    const memberRing = await ringOf(server.url, member);
    assert.deepEqual([...memberRing.keys()], [1]);
    assert.deepEqual(memberRing.get(1), firstRing.get(1), "both hold the same room key");
  });

  it("a key the space does not admit gets no box, and the server refuses the rotation without it (409, then gives up)", async () => {
    const { server, config } = await boot();
    const first = newAgent();
    const stranger = newAgent();
    await signIn(server.url, stranger);
    const firstToken = await signIn(server.url, first);
    const errors = mock.method(console, "error", () => undefined);
    const logs = mock.method(console, "log", () => undefined);
    try {
      await assert.rejects(
        () => rotateRoomKey(config, firstToken, async (did) => did !== stranger.did()),
        (e: unknown) => e instanceof api.RotateMismatch && e.missing.includes(stranger.did()),
      );
    } finally {
      errors.mock.restore();
      logs.mock.restore();
    }
    assert.deepEqual(await api.fetchRoomKeys(config, firstToken), { keys: [] }, "no version was made");
  });

  it("a member grants the versions it holds, and only those", async () => {
    const { server, config } = await boot();
    const first = newAgent();
    const bob = newAgent();
    const carol = newAgent();

    const firstToken = await signIn(server.url, first);
    await rotateRoomKey(config, firstToken, admitsAll); // version 1: the first member alone
    await signIn(server.url, bob);
    await rotateRoomKey(config, await signIn(server.url, first), admitsAll); // version 2: the first member and Bob
    await signIn(server.url, carol); // Carol: admitted, no version

    const bobRing = await ringOf(server.url, bob);
    assert.deepEqual([...bobRing.keys()], [2]);
    const bobToken = await signIn(server.url, bob);
    const bobGrants = await grantMissingRoomKeys(config, bobToken, bobRing, bob.did(), admitsAll);
    assert.deepEqual(bobGrants, [{ did: carol.did(), versions: [2] }], "Bob grants Carol the version he holds");

    const firstRing = await ringOf(server.url, first);
    const firstGrants = await grantMissingRoomKeys(config, await signIn(server.url, first), firstRing, first.did(), admitsAll);
    assert.deepEqual(
      firstGrants.map((g) => [g.did, g.versions]).sort(),
      [[bob.did(), [1]], [carol.did(), [1]]].sort(),
      "the first member fills in version 1",
    );

    const carolRing = await ringOf(server.url, carol);
    assert.deepEqual([...carolRing.keys()].sort(), [1, 2]);
    assert.deepEqual(carolRing.get(1), firstRing.get(1));
    assert.deepEqual(carolRing.get(2), firstRing.get(2));

    const again = await grantMissingRoomKeys(config, await signIn(server.url, bob), await ringOf(server.url, bob), bob.did(), admitsAll);
    assert.deepEqual(again, [], "nobody lacks a version any more");
  });

  it("a wrong key a member grants is spoiled, dropped, and replaced by an honest grant", async () => {
    const { server, config } = await boot();
    const first = newAgent();
    const mallory = newAgent();
    const late = newAgent();

    await signIn(server.url, mallory);
    await rotateRoomKey(config, await signIn(server.url, first), admitsAll); // version 1: the first member and Mallory
    const lateToken = await signIn(server.url, late);
    const lateX25519 = bytesToHex(x25519Public(await late.roomKeySeed()));

    // Mallory holds version 1, so the server lets her grant it: she seals a key of her own.
    await api.grantKeys(config, await signIn(server.url, mallory), late.did(), [
      { version: 1, encryptedKey: sealRoomKeyTo(lateX25519, generateRoomKey()) },
    ]);
    const first1 = buildKeyRing((await api.fetchRoomKeys(config, lateToken))?.keys ?? [], await late.roomKeySeed());
    assert.equal(first1.ring.size, 0, "the wrong key is not taken");
    assert.deepEqual(first1.spoiled, [1]);

    await api.deleteRoomKey(config, await signIn(server.url, late), 1);
    const firstRing = await ringOf(server.url, first);
    const grants = await grantMissingRoomKeys(config, await signIn(server.url, first), firstRing, first.did(), admitsAll);
    assert.deepEqual(grants, [{ did: late.did(), versions: [1] }], "the dropped version is missing again");

    const lateRing = await ringOf(server.url, late);
    assert.deepEqual(lateRing.get(1), firstRing.get(1), "the newcomer now holds the room's key");
  });
});

// ---------------------------------------------------------------------------
// Paged snapshot, then tail (link-server README, "Paged snapshot, then tail")
// ---------------------------------------------------------------------------

/** A FetchTransport that records each GET path and can fail chosen requests. */
class RecordingTransport extends FetchTransport {
  gets: string[] = [];
  failIf: ((url: string) => boolean) | null = null;

  async fetch(url: string, method: string, headers: Record<string, string>, body: string): Promise<TransportResponse> {
    if (method === "GET") this.gets.push(new URL(url).pathname.replace(/^.*\/rooms\/[^/]+/, "") + new URL(url).search);
    if (this.failIf?.(url)) throw new Error("httpFetch GET timed out after 10000 ms");
    return super.fetch(url, method, headers, body);
  }
}

function linkOf(agent: TestAgent, source: string): LinkExpression {
  return {
    author: agent.did(),
    timestamp: new Date().toISOString(),
    data: { source, predicate: "test://p", target: "test://t" },
    proof: { signature: `sig-${source}`, key: `${agent.did()}#primary` },
  };
}

function sourcesOf(diffs: PerspectiveDiff[], kind: "additions" | "removals"): string[] {
  return diffs.flatMap((d) => d[kind]).map((l) => (l.data as { source: string }).source);
}

function activeSources(server: TestServerHandle, roomId: string): string[] {
  return server.built.db
    .getActiveLinkPage(roomId, Number.MAX_SAFE_INTEGER, { sequence: 0, hash: "" }, Number.MAX_SAFE_INTEGER)
    .map((row) => (decryptLinkFromWire(JSON.parse(row.link_data) as WireLinkExpression, SHARED_RING).data as { source: string }).source)
    .sort();
}

describe("integration: paged render, then tail", () => {
  it("loads a room larger than a page page by page, each emitted as its own diff, then tails /sync in pages", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({ additions: Array.from({ length: 23 }, (_, i) => linkOf(agentA, `bulk-${i}`)), removals: [] });
    for (let i = 0; i < 4; i++) await syncModule.commit({ additions: [linkOf(agentA, `one-${i}`)], removals: [] });

    const agentB = newAgent();
    const transport = new RecordingTransport();
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, new MemoryStorage(), transport);
    syncModule._setPageLimitsForTests(10, 2);
    await syncModule.bootstrap();

    assert.deepEqual(emittedDiffs.map((d) => d.additions.length), [10, 10, 7], "one emitted diff per page");
    assert.equal(new Set(sourcesOf(emittedDiffs, "additions")).size, 27, "each link emitted once");
    assert.deepEqual(store.allLinks().links.map((l) => (l.data as { source: string }).source).sort(), activeSources(server, roomId));
    assert.equal(store.getSequence(), 5);
    assert.equal(store.getRevision(), server.built.db.getRoomRevision(roomId));
    assert.equal(store.getRenderProgress(), null, "a finished render leaves no cursor");
    assert.equal(syncModule.render()?.links.length, 27, "the rendered store hashes to the server's revision");

    const renders = transport.gets.filter((g) => g.startsWith("/render"));
    assert.equal(renders.length, 3);
    assert.ok(renders.every((g) => g.includes("limit=10")));
    assert.ok(!renders[0].includes("next=") && renders.slice(1).every((g) => g.includes("next=")));
    assert.deepEqual(transport.gets.filter((g) => g.startsWith("/sync")), ["/sync?since=5&limit=2"]);
  });

  it("a render cut off mid-way resumes at its stored cursor after a restart, and the tail brings what changed meanwhile", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    const initial = Array.from({ length: 25 }, (_, i) => linkOf(agentA, `init-${i}`));
    await syncModule.commit({ additions: initial, removals: [] });

    const agentB = newAgent();
    const bStorage = new MemoryStorage();
    const transport = new RecordingTransport();
    wireClient(server.url, roomId, agentB, bStorage, transport);
    syncModule._setPageLimitsForTests(10, 2);
    transport.failIf = (url) => url.includes("/render") && url.includes("next=");
    await assert.rejects(syncModule.bootstrap(), /timed out/);
    const progress = store.getRenderProgress();
    assert.equal(progress?.page, 1, "the first page is applied and its cursor stored");
    assert.equal(progress?.upto, 1);

    // Meanwhile A removes one link B already has and one it has not got yet, and adds two.
    const page1 = new Set(store.allLinks().links.map((l) => (l.data as { source: string }).source));
    const seenGone = initial.find((l) => page1.has(l.data.source))!;
    const unseenGone = initial.find((l) => !page1.has(l.data.source))!;
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({ additions: [linkOf(agentA, "late-1")], removals: [] });
    await syncModule.commit({ additions: [linkOf(agentA, "late-2")], removals: [seenGone, unseenGone] });

    // B restarts on its storage and resumes.
    const restarted = new RecordingTransport();
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, bStorage, restarted);
    syncModule._setPageLimitsForTests(10, 2);
    await syncModule.bootstrap();

    const renders = restarted.gets.filter((g) => g.startsWith("/render"));
    assert.ok(renders[0].includes("next="), "the restart resumes at the stored cursor, not page one");
    assert.equal(renders.length, 2, "pages two and three only");
    assert.deepEqual(restarted.gets.filter((g) => g.startsWith("/sync")), ["/sync?since=1&limit=2"], "the tail starts at upto");

    const local = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(local, activeSources(server, roomId), "B ends with the server's link set");
    assert.deepEqual(sourcesOf(emittedDiffs, "removals"), [seenGone.data.source],
      "the tail removes the link B had; the other never reached B");
    assert.ok(!sourcesOf(emittedDiffs, "additions").includes(unseenGone.data.source));
    assert.equal(store.getSequence(), 3);
    assert.equal(store.getRenderProgress(), null);
    assert.deepEqual(bStorage.listKeys("render/"), [], "the render's bookkeeping is gone");
  });

  it("catchUp does not tail while a render is pending; after the backoff it resumes the render, which then tails", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({ additions: Array.from({ length: 15 }, (_, i) => linkOf(agentA, `r-${i}`)), removals: [] });

    const agentB = newAgent();
    const transport = new RecordingTransport();
    wireClient(server.url, roomId, agentB, new MemoryStorage(), transport);
    syncModule._setPageLimitsForTests(10, 2);
    mock.timers.enable({ apis: ["Date"], now: Date.now() });
    try {
      transport.failIf = (url) => url.includes("/render");
      await assert.rejects(syncModule.bootstrap());
      assert.equal(store.allLinks().links.length, 0);

      transport.failIf = null;
      transport.gets = [];
      await syncModule.catchUp();
      await syncModule._awaitRenderRetryForTests();
      assert.deepEqual(transport.gets, [], "inside the backoff: no render, and no /sync from cursor 0");

      mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
      await syncModule.catchUp();
      await syncModule._awaitRenderRetryForTests();
      assert.equal(store.allLinks().links.length, 15);
      assert.equal(store.getSequence(), 1);
      assert.equal(store.getRenderProgress(), null);
      const renders = transport.gets.filter((g) => g.startsWith("/render"));
      assert.equal(renders.length, 2, "both pages, once each");
      assert.deepEqual(transport.gets.filter((g) => g.startsWith("/sync")), ["/sync?since=1&limit=2"], "the tail starts at upto");
      assert.ok(transport.gets.indexOf("/sync?since=1&limit=2") > transport.gets.indexOf(renders[1]), "after the last page");
    } finally {
      mock.timers.reset();
    }
  });

  it("a link committed or pushed during the render ends in the store, and nothing is removed", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    await syncModule.commit({ additions: Array.from({ length: 15 }, (_, i) => linkOf(agentA, `r-${i}`)), removals: [] });

    const agentB = newAgent();
    const bStorage = new MemoryStorage();
    const transport = new RecordingTransport();
    wireClient(server.url, roomId, agentB, bStorage, transport);
    syncModule._setPageLimitsForTests(10, 2);
    transport.failIf = (url) => url.includes("/render") && url.includes("next=");
    await assert.rejects(syncModule.bootstrap());

    // B's own commit, and A's link arriving as a WebSocket push, while the render is unfinished.
    await syncModule.commit({ additions: [linkOf(agentB, "own")], removals: [] });
    const pushed = encryptLinkForWire(linkOf(agentA, "pushed"), {
      roomKey: SHARED_RING.get(1)!,
      version: 1,
      hashKey: linkHashKeyOf(SHARED_RING)!,
    });
    const { sequence, revision } = server.built.db.applyDiffAndAppend(roomId, { additions: [pushed], removals: [] }, agentA.did());
    syncModule.applyInboundWireDiff({ additions: [pushed], removals: [] }, sequence, revision);

    transport.failIf = null;
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, bStorage, transport);
    syncModule._setPageLimitsForTests(10, 2);
    await syncModule.bootstrap();

    assert.deepEqual(sourcesOf(emittedDiffs, "removals"), [], "nothing is removed and re-added");
    assert.equal(sourcesOf(emittedDiffs, "additions").filter((s) => s === "pushed").length, 1,
      "the push, left to the render, arrives once with its tail");
    assert.ok(!sourcesOf(emittedDiffs, "additions").includes("own"), "B's own commit is not emitted back");
    const local = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(local, activeSources(server, roomId));
    assert.ok(local.includes("own") && local.includes("pushed"));
  });

  it("a restart with a cursor above 0 sends no /render: it tails /sync from its cursor", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    const first = Array.from({ length: 12 }, (_, i) => linkOf(agentA, `first-${i}`));
    await syncModule.commit({ additions: first, removals: [] });

    const agentB = newAgent();
    const bStorage = new MemoryStorage();
    wireClient(server.url, roomId, agentB, bStorage, new RecordingTransport());
    syncModule._setPageLimitsForTests(10, 2);
    await syncModule.bootstrap();
    assert.equal(store.getSequence(), 1);

    // While B is down, A adds three diffs and removes a link B holds.
    wireClient(server.url, roomId, agentA);
    for (let i = 0; i < 3; i++) await syncModule.commit({ additions: [linkOf(agentA, `later-${i}`)], removals: [] });
    await syncModule.commit({ additions: [], removals: [first[0]] });

    const restarted = new RecordingTransport();
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, bStorage, restarted);
    syncModule._setPageLimitsForTests(10, 2);
    syncModule.needRender();
    await syncModule.bootstrap();

    assert.deepEqual(restarted.gets.filter((g) => g.startsWith("/render")), [], "no render");
    assert.deepEqual(restarted.gets.filter((g) => g.startsWith("/sync")), ["/sync?since=1&limit=2", "/sync?since=3&limit=2"]);
    assert.deepEqual(sourcesOf(emittedDiffs, "additions"), ["later-0", "later-1", "later-2"]);
    assert.deepEqual(sourcesOf(emittedDiffs, "removals"), [first[0].data.source]);
    const local = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(local, activeSources(server, roomId));
    assert.equal(store.getSequence(), 5);
    assert.equal(syncModule.render()?.links.length, local.length, "the store hashes to the server's revision");
  });

  it("a restart with a cursor above 0 and a store short of the room renders the room again, then answers render()", async () => {
    // The staging state: the cursor is at the room's head, the store holds a
    // part of the room (a build before #1641 moved the cursor to every push).
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    const all = Array.from({ length: 23 }, (_, i) => linkOf(agentA, `room-${i}`));
    await syncModule.commit({ additions: all, removals: [] });
    await syncModule.commit({ additions: [linkOf(agentA, "last")], removals: [] });

    const agentB = newAgent();
    const bStorage = new MemoryStorage();
    wireClient(server.url, roomId, agentB, bStorage, new RecordingTransport());
    await syncModule.bootstrap();
    assert.notEqual(syncModule.render(), null);
    for (const link of all.slice(0, 20)) store.removeLink(link);
    assert.equal(syncModule.render(), null, "a short store is not the room");

    const restarted = new RecordingTransport();
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, bStorage, restarted);
    syncModule._setPageLimitsForTests(10, 2);
    syncModule.needRender();
    await syncModule.bootstrap();
    await syncModule._awaitRenderRetryForTests();

    assert.deepEqual(restarted.gets.filter((g) => g.startsWith("/render")).length, 3, "24 links in pages of 10");
    assert.equal(new Set(sourcesOf(emittedDiffs, "additions")).size, 20, "the links the store lacked, each once");
    const local = store.allLinks().links.map((l) => (l.data as { source: string }).source).sort();
    assert.deepEqual(local, activeSources(server, roomId));
    assert.equal(store.getSequence(), 2);
    assert.equal(syncModule.render()?.links.length, 24);

    await syncModule.catchUp();
    assert.deepEqual(restarted.gets.filter((g) => g.startsWith("/render")).length, 3, "healed: no further render");
  });

  it("catchUp reads a long diff log a page at a time", async () => {
    const server = await bootServer();
    const agentA = newAgent();
    const roomId = await newRoom(server);
    wireClient(server.url, roomId, agentA);
    for (let i = 0; i < 5; i++) await syncModule.commit({ additions: [linkOf(agentA, `d-${i}`)], removals: [] });

    const agentB = newAgent();
    const transport = new RecordingTransport();
    const { emittedDiffs } = wireClient(server.url, roomId, agentB, new MemoryStorage(), transport);
    syncModule._setPageLimitsForTests(10, 2);
    await syncModule.catchUp();

    assert.deepEqual(transport.gets, ["/sync?since=0&limit=2", "/sync?since=2&limit=2", "/sync?since=4&limit=2"]);
    assert.deepEqual(sourcesOf(emittedDiffs, "additions"), ["d-0", "d-1", "d-2", "d-3", "d-4"]);
    assert.equal(store.getSequence(), 5);
    assert.equal(store.getRevision(), server.built.db.getRoomRevision(roomId));
  });
});
