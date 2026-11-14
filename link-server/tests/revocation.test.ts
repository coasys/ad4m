/**
 * Keys revoked in their log (R24): the server asks the store again about every key with an open
 * socket, every 10 minutes. A key its log no longer lists as standing loses its sockets (4005)
 * and sessions, and the room hears `rotate-needed` (`revoked`). A store outage ends nobody's
 * access.
 */
import assert from "node:assert/strict";
import { mock, test } from "node:test";
import type WebSocket from "ws";
import {
  admittedKeys,
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
import { links, personAgent, personDid, signLink } from "./identity.js";

const alice = personAgent("alice");
const daveOld = personAgent("dave", "d2");

/** alice's open room with dave joined through d2, a key his log revokes at seq 1. */
async function roomWithDave(server: TestServerHandle) {
  server.store.showUpTo("dave", 0);
  const roomId = await createRoom(server.url, { join: "open" });
  const aliceToken = await authenticateAgent(server.url, roomId, alice);
  const daveToken = (await authAs(server.url, roomId, daveOld)).body.token!;
  const joined = await postJson(
    `${server.url}/rooms/${roomId}/membership`,
    { links: [signLink(daveOld, links.member(personDid("dave")))] },
    daveToken
  );
  assert.equal(joined.status, 200, JSON.stringify(joined.body));
  const aliceSocket = await openAuthenticatedWs(server.wsUrl, roomId, aliceToken);
  const daveSocket = await openAuthenticatedWs(server.wsUrl, roomId, daveToken);
  return { roomId, aliceToken, daveToken, aliceSocket, daveSocket };
}

function closeCode(socket: WebSocket): Promise<number> {
  return new Promise((resolve) => socket.once("close", (code) => resolve(code)));
}

test("a refresh closes a revoked key's socket with 4005 and asks the room to rotate without it", async () => {
  const server = await startTestServer({ autoAdmit: false });
  try {
    const { roomId, aliceToken, daveToken, aliceSocket, daveSocket } = await roomWithDave(server);
    const heard = collectMessages(aliceSocket);
    const closed = closeCode(daveSocket);

    server.store.showUpTo("dave", 1);
    assert.deepEqual(await server.built.refreshKeys(), [daveOld.did]);
    assert.equal(await closed, 4005);
    const rotate = await heard.waitForType("rotate-needed");
    assert.equal(rotate.reason, "revoked");
    assert.deepEqual(rotate.removedKeys, [daveOld.did]);

    // The session is gone, the key is not sealed to again, and it cannot sign in anew.
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, daveToken)).status, 401);
    assert.deepEqual(await admittedKeys(server.url, roomId, aliceToken), [alice.did]);
    const again = await authAs(server.url, roomId, daveOld);
    assert.equal(again.status, 403);
    assert.match(again.body.error!, /revoked/);
    // A key still standing is untouched.
    assert.equal(aliceSocket.readyState, aliceSocket.OPEN);
    aliceSocket.close();
  } finally {
    await server.close();
  }
});

test("while the store is down a refresh keeps every socket open", async () => {
  const server = await startTestServer({ autoAdmit: false });
  try {
    const { aliceSocket, daveSocket } = await roomWithDave(server);
    server.store.showUpTo("dave", 1);
    server.store.down = true;
    assert.deepEqual(await server.built.refreshKeys(), []);
    await new Promise((resolve) => setTimeout(resolve, 100));
    assert.equal(daveSocket.readyState, daveSocket.OPEN);
    assert.equal(aliceSocket.readyState, aliceSocket.OPEN);

    // Once the store answers again, the next refresh ends the revoked key.
    server.store.down = false;
    const closed = closeCode(daveSocket);
    assert.deepEqual(await server.built.refreshKeys(), [daveOld.did]);
    assert.equal(await closed, 4005);
    aliceSocket.close();
  } finally {
    await server.close();
  }
});

test("the server refreshes on its own every 10 minutes (fake timers)", async () => {
  // Only intervals are faked: the server's timers (refresh, sweeps, socket pings) wait for the
  // clock to tick, while I/O and the helpers' timeouts run on real time.
  mock.timers.enable({ apis: ["setInterval"] });
  const server = await startTestServer({ autoAdmit: false });
  try {
    const { aliceSocket, daveSocket } = await roomWithDave(server);
    let code: number | undefined;
    daveSocket.once("close", (c) => {
      code = c;
    });
    server.store.showUpTo("dave", 1);

    // Ticks of 30 s, each followed by real time for the pings' pongs to come back.
    const advance = async (ms: number) => {
      for (let t = 0; t < ms; t += 30_000) {
        mock.timers.tick(30_000);
        await new Promise((resolve) => setTimeout(resolve, 20));
      }
    };
    await advance(9 * 60_000 + 30_000);
    assert.equal(code, undefined, "no refresh before 10 minutes");
    assert.equal(daveSocket.readyState, daveSocket.OPEN);

    await advance(30_000);
    const deadline = Date.now() + 3000;
    while (code === undefined && Date.now() < deadline) await new Promise((resolve) => setTimeout(resolve, 20));
    assert.equal(code, 4005, "the 10-minute refresh closed the revoked key's socket");
    assert.equal(aliceSocket.readyState, aliceSocket.OPEN, "the standing key's socket survived the pings");
    aliceSocket.close();
  } finally {
    await server.close();
    mock.timers.reset();
  }
});
