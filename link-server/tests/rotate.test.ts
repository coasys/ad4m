/**
 * Room key rotation under protocol 2 (K3): any admitted key rotates, and the envelopes must go
 * to exactly the keys admitted now. A removal ends the removed person's sockets (4005) and tells
 * the room `rotate-needed`, naming the keys the next room key must leave out. A rotation names
 * the version its boxes are sealed for (K9): the room's latest plus one, as `GET /members` says.
 */
import assert from "node:assert/strict";
import { test } from "node:test";
import type WebSocket from "ws";
import {
  admittedKeys,
  authAs,
  authenticateAgent,
  clientSideRotate,
  collectMessages,
  createRoom,
  getJson,
  openAuthenticatedWs,
  postJson,
  startTestServer,
  type TestAgent,
  type TestServerHandle,
} from "./helpers.js";
import { links, personAgent, personDid, signLink, type PersonName } from "./identity.js";
import { generateRoomKey, roomKeyCommitment, sealToDid } from "./sealed-box.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer({ autoAdmit: false });
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

const alice = personAgent("alice");
const bob = personAgent("bob");

/** Signs in `agent` and self-joins its person to an open room: its token. */
async function join(server: TestServerHandle, roomId: string, agent: TestAgent, name: PersonName): Promise<string> {
  const token = (await authAs(server.url, roomId, agent)).body.token!;
  const res = await postJson(
    `${server.url}/rooms/${roomId}/membership`,
    { links: [signLink(agent, links.member(personDid(name)))] },
    token
  );
  assert.equal(res.status, 200, JSON.stringify(res.body));
  return token;
}

async function latestKeyVersion(server: TestServerHandle, roomId: string, token: string): Promise<number> {
  const res = await getJson<{ latestKeyVersion: number }>(`${server.url}/rooms/${roomId}/members`, token);
  assert.equal(res.status, 200, JSON.stringify(res.body));
  return res.body.latestKeyVersion;
}

/** A rotation sealed to `dids`, for `version` (default: the room's latest plus one). */
async function rotateTo(server: TestServerHandle, roomId: string, token: string, dids: string[], version?: number) {
  const roomKey = generateRoomKey();
  const forVersion = version ?? (await latestKeyVersion(server, roomId, token)) + 1;
  return postJson<{ error?: string; missing?: string[]; extra?: string[]; version?: number; latestKeyVersion?: number }>(
    `${server.url}/rooms/${roomId}/keys/rotate`,
    {
      keys: dids.map((did) => ({ did, encryptedKey: sealToDid(did, roomKey) })),
      commitment: roomKeyCommitment(roomKey),
      version: forVersion,
    },
    token
  );
}

function closeCode(socket: WebSocket): Promise<number> {
  return new Promise((resolve) => socket.once("close", (code) => resolve(code)));
}

test("a rotation sealed to fewer keys than are admitted is refused with 409 and stores nothing", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    await join(server, roomId, bob, "bob");

    const short = await rotateTo(server, roomId, aliceToken, [alice.did]);
    assert.equal(short.status, 409, JSON.stringify(short.body));
    assert.deepEqual(short.body.missing, [bob.did]);
    assert.deepEqual(short.body.extra, []);
    assert.equal(server.built.db.getLatestKeyVersion(roomId), 0, "no key version was stored");

    // A key that signed in but is no member is extra.
    const frank = personAgent("frank");
    await authAs(server.url, roomId, frank);
    const long = await rotateTo(server, roomId, aliceToken, [alice.did, bob.did, frank.did]);
    assert.equal(long.status, 409);
    assert.deepEqual(long.body.missing, []);
    assert.deepEqual(long.body.extra, [frank.did]);
    assert.equal(server.built.db.getLatestKeyVersion(roomId), 0);
  });
});

test("any member rotates, not only the room's creator; a key that is not admitted cannot", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    await authenticateAgent(server.url, roomId, alice);
    const bobToken = await join(server, roomId, bob, "bob");

    const rotated = await clientSideRotate(server.url, roomId, bobToken);
    assert.equal(rotated.version, 1);
    assert.deepEqual([...rotated.recipients].sort(), [alice.did, bob.did].sort());

    const frankToken = (await authAs(server.url, roomId, personAgent("frank"))).body.token!;
    const outsider = await rotateTo(server, roomId, frankToken, [alice.did, bob.did], 2);
    assert.equal(outsider.status, 403);
    assert.equal(server.built.db.getLatestKeyVersion(roomId), 1);
  });
});

test("GET /members names the room's latest key version, and a rotation is stored as the version it names (K9)", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    assert.equal(await latestKeyVersion(server, roomId, aliceToken), 0);

    const first = await rotateTo(server, roomId, aliceToken, [alice.did], 1);
    assert.equal(first.status, 200, JSON.stringify(first.body));
    assert.equal(first.body.version, 1);
    assert.equal(await latestKeyVersion(server, roomId, aliceToken), 1);

    const second = await rotateTo(server, roomId, aliceToken, [alice.did], 2);
    assert.equal(second.status, 200, JSON.stringify(second.body));
    assert.equal(second.body.version, 2);
    assert.equal(await latestKeyVersion(server, roomId, aliceToken), 2);
  });
});

test("a rotation sealed for a version other than the next is refused with 409 and stores nothing (K9)", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    assert.equal((await rotateTo(server, roomId, aliceToken, [alice.did], 1)).status, 200);

    // Another member's rotation got there first (sealed for 1 again), or one sealed ahead.
    for (const version of [1, 3]) {
      const res = await rotateTo(server, roomId, aliceToken, [alice.did], version);
      assert.equal(res.status, 409, JSON.stringify(res.body));
      assert.match(res.body.error!, /version/);
      assert.equal(res.body.latestKeyVersion, 1);
      assert.equal(server.built.db.getLatestKeyVersion(roomId), 1, "no key version was stored");
    }
  });
});

test("a rotation that does not name its version is refused with 400 (K9)", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    const roomKey = generateRoomKey();
    for (const version of [undefined, 0, -1, 1.5, "1"]) {
      const res = await postJson<{ error: string }>(
        `${server.url}/rooms/${roomId}/keys/rotate`,
        { keys: [{ did: alice.did, encryptedKey: sealToDid(alice.did, roomKey) }], commitment: roomKeyCommitment(roomKey), version },
        aliceToken
      );
      assert.equal(res.status, 400, `${version}: ${JSON.stringify(res.body)}`);
      assert.match(res.body.error, /version/);
    }
    assert.equal(server.built.db.getLatestKeyVersion(roomId), 0);
  });
});

test("removing a person closes each of their keys' sockets (4005) and asks the room to rotate without them", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url, { join: "open" });
    const aliceToken = await authenticateAgent(server.url, roomId, alice);
    // Dave with two standing device keys, both signed in.
    server.store.showUpTo("dave", 0);
    const dave1 = personAgent("dave");
    const dave2 = personAgent("dave", "d2");
    const dave1Token = await join(server, roomId, dave1, "dave");
    const dave2Token = await authenticateAgent(server.url, roomId, dave2);
    assert.deepEqual((await admittedKeys(server.url, roomId, aliceToken)).sort(), [alice.did, dave1.did, dave2.did].sort());

    const aliceSocket = await openAuthenticatedWs(server.wsUrl, roomId, aliceToken);
    const heard = collectMessages(aliceSocket);
    const closed = [
      closeCode(await openAuthenticatedWs(server.wsUrl, roomId, dave1Token)),
      closeCode(await openAuthenticatedWs(server.wsUrl, roomId, dave2Token)),
    ];

    const removed = await postJson(
      `${server.url}/rooms/${roomId}/membership`,
      { links: [signLink(alice, links.removed(personDid("dave")))] },
      aliceToken
    );
    assert.equal(removed.status, 200, JSON.stringify(removed.body));
    assert.deepEqual(await Promise.all(closed), [4005, 4005]);

    const rotate = await heard.waitForType("rotate-needed");
    assert.equal(rotate.reason, "removed");
    assert.deepEqual([...(rotate.removedKeys as string[])].sort(), [dave1.did, dave2.did].sort());

    // Their tokens are dead, and the next key must leave them out.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, dave1Token)).status, 401);
    assert.deepEqual(await admittedKeys(server.url, roomId, aliceToken), [alice.did]);
    const withDave = await rotateTo(server, roomId, aliceToken, [alice.did, dave1.did]);
    assert.equal(withDave.status, 409);
    assert.deepEqual(withDave.body.extra, [dave1.did]);
    assert.equal((await clientSideRotate(server.url, roomId, aliceToken)).version, 1);
    aliceSocket.close();
  });
});
