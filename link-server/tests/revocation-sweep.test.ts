/**
 * The key refresh reaches every key a room key is sealed to, not only keys with an open socket;
 * a session of a key revoked in a room is refused; one key the store's answer breaks on does not
 * stop the sweep for the others.
 */
import assert from "node:assert/strict";
import { createServer, request as httpRequest, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { test } from "node:test";
import { generateRoomKey, roomKeyCommitment, sealToDid } from "./sealed-box.js";
import {
  admittedKeys,
  authAs,
  authenticateAgent,
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

/** alice's open room with dave joined through d2 (a key his log revokes at seq 1), no socket. */
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
  return { roomId, aliceToken, daveToken };
}

test("a revoked key with no socket is revoked by the refresh: its session ends and no room key is sealed to it", async () => {
  const server = await startTestServer({ autoAdmit: false });
  try {
    const { roomId, aliceToken, daveToken } = await roomWithDave(server);
    assert.deepEqual((await admittedKeys(server.url, roomId, aliceToken)).sort(), [alice.did, daveOld.did].sort());

    server.store.showUpTo("dave", 1);
    assert.deepEqual(await server.built.refreshKeys(), [daveOld.did]);

    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, daveToken)).status, 401);
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/keys`, daveToken)).status, 401);
    assert.deepEqual(await admittedKeys(server.url, roomId, aliceToken), [alice.did]);

    // A rotation sealed to alice alone is complete.
    const roomKey = generateRoomKey();
    const rotated = await postJson<{ version: number }>(
      `${server.url}/rooms/${roomId}/keys/rotate`,
      { keys: [{ did: alice.did, encryptedKey: sealToDid(alice.did, roomKey) }], commitment: roomKeyCommitment(roomKey) },
      aliceToken
    );
    assert.equal(rotated.status, 200, JSON.stringify(rotated.body));
    // And no grant reaches the revoked key.
    const grant = await postJson(
      `${server.url}/rooms/${roomId}/keys/grant`,
      { targetDid: daveOld.did, keys: [{ version: rotated.body.version, encryptedKey: sealToDid(daveOld.did, roomKey) }] },
      aliceToken
    );
    assert.equal(grant.status, 404, JSON.stringify(grant.body));
  } finally {
    await server.close();
  }
});

test("a session of a key marked revoked in the room is refused, though its session row is still there", async () => {
  const server = await startTestServer({ autoAdmit: false });
  try {
    const { roomId, daveToken } = await roomWithDave(server);
    assert.equal((await getJson(`${server.url}/rooms/${roomId}/members`, daveToken)).status, 200);
    server.built.db.markKeyRevoked(daveOld.did, roomId);
    const refused = await getJson<{ error?: string }>(`${server.url}/rooms/${roomId}/members`, daveToken);
    assert.equal(refused.status, 401);
    assert.match(refused.body.error!, /revoked/);
    const membership = await postJson(
      `${server.url}/rooms/${roomId}/membership`,
      { links: [signLink(daveOld, links.member(personDid("dave")))] },
      daveToken
    );
    assert.equal(membership.status, 401);
  } finally {
    await server.close();
  }
});

/**
 * A store in front of the fake one (`state.target`) that, once `state.broken` is set, serves
 * `victim`'s key entry with a head the core cannot read (`time` not a number).
 */
async function breakingProxy(victimMultikey: string) {
  const state = { broken: false, target: "" };
  const server: Server = createServer((req, res) => {
    const upstream = httpRequest(`${state.target}${req.url}`, (up) => {
      const chunks: Buffer[] = [];
      up.on("data", (c: Buffer) => chunks.push(c));
      up.on("end", () => {
        let body = Buffer.concat(chunks).toString();
        if (state.broken && req.url === `/v1/keys/${encodeURIComponent(victimMultikey)}` && up.statusCode === 200) {
          const entry = JSON.parse(body);
          entry.head = { ...entry.head, time: "not a number" };
          body = JSON.stringify(entry);
        }
        res.writeHead(up.statusCode ?? 502, { "content-type": "application/json" });
        res.end(body);
      });
    });
    upstream.on("error", () => {
      res.writeHead(502);
      res.end();
    });
    upstream.end();
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return {
    url: `http://127.0.0.1:${(server.address() as AddressInfo).port}`,
    state,
    close: () =>
      new Promise<void>((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
}

test("one key the store's answer breaks on keeps its access; the sweep goes on to revoke the next", async () => {
  // The broken key must come first in the sweep: alice's key sorts before dave's d2.
  assert.ok(alice.did < daveOld.did);
  const proxy = await breakingProxy(alice.did.slice("did:key:".length));
  const server = await startTestServer({ autoAdmit: false, storeUrl: proxy.url });
  proxy.state.target = server.store.url;
  try {
    const { roomId, aliceToken } = await roomWithDave(server);
    // Sockets for both, so a sweep over socket keys alone meets the broken key too.
    const aliceSocket = await openAuthenticatedWs(server.wsUrl, roomId, aliceToken);
    const daveToken = (await authAs(server.url, roomId, daveOld)).body.token!;
    const daveSocket = await openAuthenticatedWs(server.wsUrl, roomId, daveToken);

    server.store.showUpTo("dave", 1);
    proxy.state.broken = true;
    assert.deepEqual(await server.built.refreshKeys(), [daveOld.did]);
    assert.equal(aliceSocket.readyState, aliceSocket.OPEN, "the key the store broke on keeps its socket");
    assert.deepEqual(await admittedKeys(server.url, roomId, aliceToken), [alice.did]);
    aliceSocket.close();
    daveSocket.close();
  } finally {
    await server.close();
    await proxy.close();
  }
});
