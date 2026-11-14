/**
 * What `POST /membership` refuses to a caller who games the replay:
 *
 * - a self-join dated off the server's clock (`stale`): the replay judges a self-join, an
 *   invite's expiry and its first use at the link's own timestamp, so a backdated self-join
 *   would count in an earlier open period, before an invite expired, or before the person who
 *   used the invite;
 * - batches past `MAX_MEMBERSHIP_BATCH` (400, before any work), and forged signatures costing
 *   identity-store lookups;
 * - refused links costing a rebuild of the room's replay from the database;
 * - a link already stored is answered `repeated`, not counted again.
 */
import assert from "node:assert/strict";
import { randomBytes } from "node:crypto";
import { test } from "node:test";
import { ed25519 } from "@noble/curves/ed25519.js";
import { AUTH_SKEW_MS, publicKeyToDid } from "../src/auth.js";
import type { WireMembershipLink } from "../src/identity-core.js";
import * as membership from "../src/membership.js";
import {
  authAs,
  authenticateAgent,
  collectMessages,
  createRoom,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import { links, newSalt, personAgent, personDid, roomIdOf, signLink } from "./identity.js";

interface Results {
  results?: Array<{ hash: string; counted: boolean; stored?: boolean; reason?: string }>;
  reason?: string;
  error?: string;
}

const alice = personAgent("alice");
const bob = personAgent("bob");
const carol = personAgent("carol");
const iso = (ms: number) => new Date(ms).toISOString();
const HOUR = 60 * 60_000;
/** The batch cap of `POST /membership`, as the protocol states it. */
const MAX_BATCH = 100;

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const generous = { limit: 100_000, windowMs: 60_000 };
  const server = await startTestServer({
    autoAdmit: false,
    rateLimits: { authIp: generous, roomJwt: generous, commitJwt: generous },
  });
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

function post(server: TestServerHandle, roomId: string, body: unknown, token?: string) {
  return postJson<Results>(`${server.url}/rooms/${roomId}/membership`, body, token);
}

/** A room alice made at `createdAt` with `rules`, alice signed in. */
async function roomMadeAt(server: TestServerHandle, createdAt: number, join: "open" | "invite") {
  const salt = newSalt();
  const roomId = roomIdOf(alice.person!, salt);
  const genesis = await post(server, roomId, {
    genesis: { creator: alice.person, salt },
    links: [signLink(alice, links.rules({ join }), iso(createdAt))],
  });
  assert.equal(genesis.status, 200, JSON.stringify(genesis.body));
  return { roomId, aliceToken: await authenticateAgent(server.url, roomId, alice) };
}

async function storedSources(server: TestServerHandle, roomId: string, token: string): Promise<string[]> {
  const page = await getJson<{ links: WireMembershipLink[] }>(`${server.url}/rooms/${roomId}/membership`, token);
  assert.equal(page.status, 200);
  return page.body.links.map((l) => `${l.data.predicate.split("/").pop()}:${l.data.source}`);
}

async function tokenOf(server: TestServerHandle, roomId: string, agent = bob): Promise<string> {
  const auth = await authAs(server.url, roomId, agent);
  assert.equal(auth.status, 200, JSON.stringify(auth.body));
  return auth.body.token!;
}

test("a self-join backdated into an earlier open period is refused as stale: not stored, not relayed", async () => {
  await withServer(async (server) => {
    const t0 = Date.now() - HOUR;
    const { roomId, aliceToken } = await roomMadeAt(server, t0, "open");
    const switched = await post(server, roomId, { links: [signLink(alice, links.joinMode("invite"), iso(t0 + 30 * 60_000))] }, aliceToken);
    assert.equal(switched.status, 200, JSON.stringify(switched.body));
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, aliceToken);
    const heard = collectMessages(socket);
    const bobToken = await tokenOf(server, roomId);

    const now = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob")))] }, bobToken);
    assert.equal(now.status, 403);
    assert.equal(now.body.reason, "inviteOnly");

    const backdated = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob")), iso(t0 + 60_000))] }, bobToken);
    assert.equal(backdated.status, 403, JSON.stringify(backdated.body));
    assert.equal(backdated.body.reason, "stale");
    assert.equal(backdated.body.results![0].counted, false);
    assert.notEqual(backdated.body.results![0].stored, true);

    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, false);
    assert.ok(!(await storedSources(server, roomId, aliceToken)).includes(`member:${personDid("bob")}`));
    await new Promise((resolve) => setTimeout(resolve, 150));
    assert.ok(!heard.messages.some((m) => m.type === "membership"), "nothing was relayed");
    socket.close();
  });
});

test("an expired invite admits no self-join dated before its expiry", async () => {
  await withServer(async (server) => {
    const t0 = Date.now() - HOUR;
    const { roomId, aliceToken } = await roomMadeAt(server, t0, "invite");
    const invite = `invite://${randomBytes(4).toString("hex")}`;
    const made = await post(server, roomId, { links: [signLink(alice, links.invite(invite, t0 + 10 * 60_000), iso(t0 + 60_000))] }, aliceToken);
    assert.equal(made.status, 200, JSON.stringify(made.body));
    const bobToken = await tokenOf(server, roomId);

    const fresh = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob"), invite))] }, bobToken);
    assert.equal(fresh.body.reason, "inviteExpired");
    const backdated = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob"), invite), iso(t0 + 2 * 60_000))] }, bobToken);
    assert.equal(backdated.status, 403);
    assert.equal(backdated.body.reason, "stale");
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, false);
  });
});

test("a self-join backdated before another person's use of an invite neither steals it nor evicts them", async () => {
  await withServer(async (server) => {
    const t0 = Date.now() - HOUR;
    const { roomId, aliceToken } = await roomMadeAt(server, t0, "invite");
    const invite = `invite://${randomBytes(4).toString("hex")}`;
    await post(server, roomId, { links: [signLink(alice, links.invite(invite, Date.now() + 86_400_000), iso(t0 + 60_000))] }, aliceToken);
    const bobToken = await tokenOf(server, roomId);
    const bobJoin = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob"), invite))] }, bobToken);
    assert.equal(bobJoin.status, 200, JSON.stringify(bobJoin.body));

    const carolToken = await tokenOf(server, roomId, carol);
    const stolen = await post(server, roomId, { links: [signLink(carol, links.member(personDid("carol"), invite), iso(t0 + 2 * 60_000))] }, carolToken);
    assert.equal(stolen.status, 403);
    assert.equal(stolen.body.reason, "stale");
    assert.equal((await authAs(server.url, roomId, carol)).body.admitted, false);
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, true);
  });
});

test("a self-join is stale whoever relays it, and dated in the future too; within the skew it counts", async () => {
  await withServer(async (server) => {
    const { roomId, aliceToken } = await roomMadeAt(server, Date.now() - HOUR, "open");

    // alice, an admitted admin, relays bob's backdated and carol's future self-join.
    const relayed = await post(
      server,
      roomId,
      {
        links: [
          signLink(bob, links.member(personDid("bob")), iso(Date.now() - AUTH_SKEW_MS - 60_000)),
          signLink(carol, links.member(personDid("carol")), iso(Date.now() + AUTH_SKEW_MS + 60_000)),
        ],
      },
      aliceToken
    );
    assert.equal(relayed.status, 403, JSON.stringify(relayed.body));
    assert.deepEqual(relayed.body.results!.map((r) => r.reason), ["stale", "stale"]);

    // Two minutes off the clock is within the skew.
    const bobToken = await tokenOf(server, roomId);
    const recent = await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob")), iso(Date.now() - 2 * 60_000))] }, bobToken);
    assert.equal(recent.status, 200, JSON.stringify(recent.body));
    assert.equal(recent.body.results![0].counted, true);
  });
});

test("a genesis batch refuses a stale self-join as well", async () => {
  await withServer(async (server) => {
    const salt = newSalt();
    const roomId = roomIdOf(alice.person!, salt);
    const genesis = await post(server, roomId, {
      genesis: { creator: alice.person, salt },
      links: [
        signLink(alice, links.rules({ join: "open" }), iso(Date.now() - HOUR)),
        signLink(bob, links.member(personDid("bob")), iso(Date.now() - HOUR + 60_000)),
      ],
    });
    assert.equal(genesis.status, 200, JSON.stringify(genesis.body));
    assert.deepEqual(genesis.body.results!.map((r) => r.reason ?? "counted"), ["counted", "stale"]);
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, false);
  });
});

test("an admin's links keep their own timestamps (written offline, synced later)", async () => {
  await withServer(async (server) => {
    const t0 = Date.now() - HOUR;
    const { roomId, aliceToken } = await roomMadeAt(server, t0, "invite");
    // alice adds bob, dated 50 minutes ago: an admin's add is not a self-join.
    const added = await post(server, roomId, { links: [signLink(alice, links.member(personDid("bob")), iso(t0 + 10 * 60_000))] }, aliceToken);
    assert.equal(added.status, 200, JSON.stringify(added.body));
    assert.equal(added.body.results![0].counted, true);
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, true);
  });
});

test("a link already stored is answered repeated (stored, not counted now), even once its self-join is off the clock", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const bobToken = await tokenOf(server, roomId);
    const join = signLink(bob, links.member(personDid("bob")));
    const first = await post(server, roomId, { links: [join] }, bobToken);
    assert.deepEqual(first.body.results![0], { hash: first.body.results![0].hash, counted: true, stored: true });

    const again = await post(server, roomId, { links: [join, join] }, bobToken);
    assert.equal(again.status, 200, JSON.stringify(again.body));
    for (const result of again.body.results!) {
      assert.equal(result.counted, false);
      assert.equal(result.stored, true);
      assert.equal(result.reason, "repeated");
    }

    // The stored self-join sent again an hour later: still repeated, never stale.
    const rooms = new membership.MembershipRooms(server.built.db);
    try {
      const judged = server.built.db.getMembershipJudged(roomId).at(-1)!;
      const later = rooms.apply(roomId, [{ link: join, hash: first.body.results![0].hash, judged }], Date.now() + HOUR);
      assert.deepEqual(later.results.map((r) => r.reason), ["repeated"]);
      assert.equal(later.stored.length, 0);
    } finally {
      rooms.close();
    }
  });
});

test(`a batch of more than ${MAX_BATCH} links is refused with 400 before any lookup, genesis or not`, async () => {
  await withServer(async (server) => {
    assert.equal(membership.MAX_MEMBERSHIP_BATCH, MAX_BATCH);
    const salt = newSalt();
    const roomId = roomIdOf(alice.person!, salt);
    const tooMany = Array.from({ length: MAX_BATCH + 1 }, (_, i) =>
      signLink(alice, links.invite(`invite://cap-${i}`, Date.now() + HOUR))
    );
    const before = { ...server.store.hits };
    const genesis = await post(server, roomId, {
      genesis: { creator: alice.person, salt },
      links: [signLink(alice, links.rules({ join: "open" })), ...tooMany],
    });
    assert.equal(genesis.status, 400, JSON.stringify(genesis.body));
    assert.deepEqual(server.store.hits, before, "no store lookup");

    const { roomId: room, aliceToken } = await roomMadeAt(server, Date.now() - HOUR, "open");
    const over = await post(server, room, { links: tooMany }, aliceToken);
    assert.equal(over.status, 400);
    const atCap = await post(server, room, { links: tooMany.slice(0, MAX_BATCH) }, aliceToken);
    assert.equal(atCap.status, 200, JSON.stringify(atCap.body));
  });
});

test("forged signatures cost no identity-store lookup and count for no one", async () => {
  await withServer(async (server) => {
    const salt = newSalt();
    const roomId = roomIdOf(alice.person!, salt);
    const forged = Array.from({ length: MAX_BATCH - 1 }, () => {
      const did = publicKeyToDid(ed25519.getPublicKey(ed25519.utils.randomPrivateKey()));
      return {
        author: did,
        timestamp: new Date().toISOString(),
        data: { source: personDid("bob"), predicate: "coasys://membership/member", target: "coasys://membership/space" },
        proof: { key: did, signature: "00" },
      };
    });
    // alice's own key, with a signature that is not hers.
    const rules = signLink(alice, links.rules({ join: "open" }));
    const forgedRules = { ...rules, proof: { ...rules.proof, signature: "11".repeat(64) } };
    const before = server.store.hits.keys;
    const genesis = await post(server, roomId, { genesis: { creator: alice.person, salt }, links: [forgedRules, ...forged] });
    assert.equal(genesis.status, 403, JSON.stringify(genesis.body).slice(0, 300));
    assert.equal(server.store.hits.keys, before, "a forged link is judged without asking the store");
    assert.ok(genesis.body.results!.every((r) => !r.counted));
  });
});

test("refused links never rebuild the room's replay from the database", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const invites = Array.from({ length: 50 }, (_, i) => signLink(alice, links.invite(`invite://r-${i}`, Date.now() + HOUR)));
    assert.equal((await post(server, roomId, { links: invites }, aliceToken)).status, 200);
    const bobToken = await tokenOf(server, roomId);
    assert.equal((await post(server, roomId, { links: [signLink(bob, links.member(personDid("bob")))] }, bobToken)).status, 200);

    const db = server.built.db;
    const original = db.getMembershipJudged.bind(db);
    let rebuilds = 0;
    db.getMembershipJudged = (id: string) => {
      rebuilds++;
      return original(id);
    };
    try {
      // bob is no admin: each of his `admin` links is refused (`notAdmin`), a refusal the
      // core's replay keeps.
      const junk = Array.from({ length: MAX_BATCH }, (_, i) =>
        signLink(bob, links.admin(personDid("frank")), iso(Date.now() + i))
      );
      const refused = await post(server, roomId, { links: junk }, bobToken);
      assert.equal(refused.status, 403);
      assert.equal(refused.body.reason, "notAdmin");
      assert.equal(rebuilds, 0, "no rebuild per refused link");
    } finally {
      db.getMembershipJudged = original;
    }
  });
});

test("a refused link leaves no trace: a link later in the batch that sorts before it does not revive it", async () => {
  await withServer(async (server) => {
    const { roomId, aliceToken } = await roomMadeAt(server, Date.now() - HOUR, "invite");
    const invite = `invite://${randomBytes(4).toString("hex")}`;
    const bobJoin = signLink(bob, links.member(personDid("bob"), invite));
    // bob's self-join comes first and is refused (no such invite yet); alice's invite, dated
    // before it, comes next in the same batch.
    const batch = await post(
      server,
      roomId,
      { links: [bobJoin, signLink(alice, links.invite(invite, Date.now() + HOUR), iso(Date.now() - 60_000))] },
      aliceToken
    );
    assert.equal(batch.status, 200, JSON.stringify(batch.body));
    assert.deepEqual(batch.body.results!.map((r) => r.reason ?? "counted"), ["inviteOnly", "counted"]);
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, false);
    const members = await getJson<{ members: Array<{ person: string }> }>(`${server.url}/rooms/${roomId}/members`, aliceToken);
    assert.deepEqual(members.body.members.map((m) => m.person), [personDid("alice")]);

    // Sent again, the self-join counts through the invite.
    const bobToken = await tokenOf(server, roomId);
    const again = await post(server, roomId, { links: [bobJoin] }, bobToken);
    assert.equal(again.status, 200, JSON.stringify(again.body));
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, true);
  });
});
