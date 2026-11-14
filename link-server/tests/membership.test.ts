import assert from "node:assert/strict";
import { test } from "node:test";
import type { WireMembershipLink } from "../src/identity-core.js";
import {
  authAs,
  authenticateAgent,
  collectMessages,
  createRoom,
  createSignedLink,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";
import {
  deviceOnlyAgent,
  links,
  newSalt,
  personAgent,
  personDid,
  roomIdOf,
  signLink,
} from "./identity.js";

interface Results {
  results?: Array<{ hash: string; counted: boolean; stored?: boolean; reason?: string }>;
  reason?: string;
  error?: string;
}

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer({ autoAdmit: false });
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

function postMembership(server: TestServerHandle, roomId: string, body: unknown, token?: string) {
  return postJson<Results>(`${server.url}/rooms/${roomId}/membership`, body, token);
}

const alice = personAgent("alice");
const bob = personAgent("bob");
const carol = personAgent("carol");
const carolBot = personAgent("carol", "d2");

test("genesis creates the room with its creator as the first admin", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const auth = await authAs(server.url, roomId, alice);
    assert.equal(auth.status, 200, JSON.stringify(auth.body));
    assert.equal(auth.body.admitted, true);

    const members = await getJson<{ members: Array<{ person: string; role: string }> }>(
      `${server.url}/rooms/${roomId}/members`,
      auth.body.token
    );
    assert.equal(members.status, 200);
    assert.deepEqual(members.body.members.map((m) => [m.person, m.role]), [[personDid("alice"), "admin"]]);

    const page = await getJson<{ links: WireMembershipLink[]; seq: number }>(
      `${server.url}/rooms/${roomId}/membership?since=0`,
      auth.body.token
    );
    assert.equal(page.status, 200);
    assert.equal(page.body.seq, 1);
    assert.equal(page.body.links.length, 1);
    assert.equal(page.body.links[0].data.predicate, "coasys://membership/rules");
  });
});

test("auth to an unknown room answers 404 and creates nothing", async () => {
  await withServer(async (server) => {
    const roomId = roomIdOf(personDid("alice"), newSalt());
    assert.equal((await authAs(server.url, roomId, alice)).status, 404);
    assert.equal((await authAs(server.url, roomId, alice)).status, 404);
    assert.equal(server.built.db.getRoom(roomId), undefined);
  });
});

test("genesis is refused when the room id does not derive from creator and salt", async () => {
  await withServer(async (server) => {
    const salt = newSalt();
    const wrongId = roomIdOf(personDid("alice"), salt + "x");
    const res = await postMembership(server, wrongId, {
      genesis: { creator: personDid("alice"), salt },
      links: [signLink(alice, links.rules())],
    });
    assert.equal(res.status, 400);
    assert.equal((await authAs(server.url, wrongId, alice)).status, 404);
  });
});

test("genesis is refused when the rules link is not the creator's", async () => {
  await withServer(async (server) => {
    const salt = newSalt();
    const roomId = roomIdOf(personDid("alice"), salt);
    const res = await postMembership(server, roomId, {
      genesis: { creator: personDid("alice"), salt },
      links: [signLink(bob, links.rules())],
    });
    assert.equal(res.status, 403);
    assert.equal(res.body.reason, "notCreator");
    assert.equal((await authAs(server.url, roomId, alice)).status, 404);
  });
});

test("an open room admits a person's self-join, and members hear of it", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const socket = await openAuthenticatedWs(server.wsUrl, roomId, aliceToken);
    const heard = collectMessages(socket);

    const before = await authAs(server.url, roomId, bob);
    assert.equal(before.status, 200);
    assert.equal(before.body.admitted, false);
    const bobToken = before.body.token!;
    // Not yet admitted: the room's links stay closed to this token.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, bobToken)).status, 403);

    const join = signLink(bob, links.member(personDid("bob")));
    const res = await postMembership(server, roomId, { links: [join] }, bobToken);
    assert.equal(res.status, 200, JSON.stringify(res.body));
    assert.equal(res.body.results!.length, 1);
    assert.equal(res.body.results![0].counted, true);
    assert.match(res.body.results![0].hash, /^[0-9a-f]{64}$/);

    // The same token is admitted now: admission follows the membership, not the token.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, bobToken)).status, 200);
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, true);

    const membership = await heard.waitForType("membership");
    assert.deepEqual((membership.links as WireMembershipLink[]).map((l) => l.data.source), [personDid("bob")]);
    assert.equal(membership.seq, 2);
    const rotate = await heard.waitForType("rotate-needed");
    assert.equal(rotate.reason, "joined");
    assert.deepEqual(rotate.removedKeys, []);

    // The same link again counts once: it is stored already, not counted now.
    const again = await postMembership(server, roomId, { links: [join] }, bobToken);
    assert.equal(again.status, 200);
    assert.equal(again.body.results![0].counted, false);
    assert.equal(again.body.results![0].stored, true);
    assert.equal(again.body.results![0].reason, "repeated");
    const page = await getJson<{ links: unknown[]; seq: number }>(
      `${server.url}/rooms/${roomId}/membership?since=1`,
      aliceToken
    );
    assert.equal(page.body.links.length, 1);
    assert.equal(page.body.seq, 2);
    socket.close();
  });
});

test("an invite-only room refuses a self-join without an invite, and takes one with it", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "invite" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const bobToken = (await authAs(server.url, roomId, bob)).body.token!;

    const refused = await postMembership(server, roomId, { links: [signLink(bob, links.member(personDid("bob")))] }, bobToken);
    assert.equal(refused.status, 403);
    assert.equal(refused.body.reason, "inviteOnly");
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, false);
    // A refused link is not kept: the membership log holds the rules alone.
    const page = await getJson<{ links: unknown[] }>(`${server.url}/rooms/${roomId}/membership?since=0`, aliceToken);
    assert.equal(page.body.links.length, 1);

    const invite = "invite://room-test-1";
    const made = await postMembership(
      server,
      roomId,
      { links: [signLink(alice, links.invite(invite, Date.now() + 3_600_000))] },
      aliceToken
    );
    assert.equal(made.status, 200, JSON.stringify(made.body));
    const joined = await postMembership(
      server,
      roomId,
      { links: [signLink(bob, links.member(personDid("bob"), invite))] },
      bobToken
    );
    assert.equal(joined.status, 200, JSON.stringify(joined.body));
    assert.equal((await authAs(server.url, roomId, bob)).body.admitted, true);
  });
});

test("a key no log lists cannot join: needs an identity", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const eve = deviceOnlyAgent();
    const auth = await authAs(server.url, roomId, eve);
    assert.equal(auth.status, 200);
    assert.equal(auth.body.admitted, false);
    const res = await postMembership(server, roomId, { links: [signLink(eve, links.member(eve.did))] }, auth.body.token);
    assert.equal(res.status, 403);
    assert.equal(res.body.reason, "needsIdentity");
  });
});

test("a bot key's self-join does not count, though its owner's does", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const botAuth = await authAs(server.url, roomId, carolBot);
    assert.equal(botAuth.status, 200);
    assert.equal(botAuth.body.admitted, false);
    const res = await postMembership(
      server,
      roomId,
      { links: [signLink(carolBot, links.member(personDid("carol")))] },
      botAuth.body.token
    );
    assert.equal(res.status, 403);
    assert.equal(res.body.reason, "needsIdentity");

    const carolToken = (await authAs(server.url, roomId, carol)).body.token!;
    const own = await postMembership(server, roomId, { links: [signLink(carol, links.member(personDid("carol")))] }, carolToken);
    assert.equal(own.status, 200, JSON.stringify(own.body));
    // The bot key itself is still no member: only its bot and approved-by links make it one.
    assert.equal((await authAs(server.url, roomId, carolBot)).body.admitted, false);
  });
});

test("a link timestamped before its author's removal is refused as backdated", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const bobToken = (await authAs(server.url, roomId, bob)).body.token!;
    // Each step a little later than the one before: the join after the rules, the removal
    // after the join, so a link dated between join and removal predates the removal.
    await new Promise((resolve) => setTimeout(resolve, 20));
    const joinedAt = Date.now();
    const join = signLink(bob, links.member(personDid("bob")), new Date(joinedAt).toISOString());
    assert.equal((await postMembership(server, roomId, { links: [join] }, bobToken)).status, 200);
    await new Promise((resolve) => setTimeout(resolve, 20));

    const removed = await postMembership(server, roomId, { links: [signLink(alice, links.removed(personDid("bob")))] }, aliceToken);
    assert.equal(removed.status, 200, JSON.stringify(removed.body));
    // Removal ends the removed person's sessions at once.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, bobToken)).status, 401);

    // A member relays a link bob signed before his removal reached the server, dated before it.
    const backdated = signLink(bob, links.member(personDid("frank")), new Date(joinedAt + 1).toISOString());
    const res = await postMembership(server, roomId, { links: [backdated] }, aliceToken);
    assert.equal(res.status, 403);
    assert.equal(res.body.reason, "backdated");
  });
});

test("a token not yet admitted may send only its own self-join", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const bobToken = (await authAs(server.url, roomId, bob)).body.token!;

    // Relaying someone else's link, or naming someone else, is refused before any judgement.
    for (const link of [
      signLink(alice, links.member(personDid("frank"))),
      signLink(bob, links.member(personDid("frank"))),
      signLink(bob, links.admin(personDid("bob"))),
    ]) {
      const res = await postMembership(server, roomId, { links: [link] }, bobToken);
      assert.equal(res.status, 403, JSON.stringify(res.body));
      assert.equal(res.body.reason, "notAdmitted");
    }
    // Nor may it commit or read the room.
    const commit = await postJson(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [await createSignedLink(bob, { source: "a", predicate: "b", target: "c" })], removals: [] },
      bobToken
    );
    assert.equal(commit.status, 403);
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/sync`, bobToken)).status, 403);
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/membership`, bobToken)).status, 403);
  });
});

test("membership links are refused on /commit", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const res = await postJson<{ error: string }>(
      `${server.url}/rooms/${roomId}/commit`,
      { additions: [signLink(alice, links.member(personDid("frank")))], removals: [] },
      aliceToken
    );
    assert.equal(res.status, 400);
    assert.match(res.body.error, /membership/);
  });
});

test("POST /membership without a token answers 401 for a room that exists", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const res = await postMembership(server, roomId, { links: [signLink(bob, links.member(personDid("bob")))] });
    assert.equal(res.status, 401);
  });
});
