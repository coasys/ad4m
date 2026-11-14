/**
 * Protocol 2 auth: a key signs `coasys-link-server:auth:2:<roomId>:<timestamp>` and its X25519
 * key, the server asks the identity store whom the key speaks for, and answers
 * `{token, expiresAt, admitted}`. Auth never creates a room.
 */
import assert from "node:assert/strict";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { test } from "node:test";
import Database from "better-sqlite3";
import {
  authAs,
  createRoom,
  createTestAgent,
  getJson,
  postJson,
  startTestServer,
  type AuthAnswer,
  type TestServerHandle,
} from "./helpers.js";
import { authBody, personAgent, personDid } from "./identity.js";

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

test("the creator's key authenticates admitted; another person's key gets a token, not admitted", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const own = await authAs(server.url, roomId, alice);
    assert.equal(own.status, 200, JSON.stringify(own.body));
    assert.equal(typeof own.body.token, "string");
    assert.ok(!Number.isNaN(Date.parse(own.body.expiresAt!)));
    assert.equal(own.body.admitted, true);

    const other = await authAs(server.url, roomId, bob);
    assert.equal(other.status, 200);
    assert.equal(other.body.admitted, false);
    // The key is recorded with the person it speaks for.
    const key = server.built.db.getMemberKey(roomId, bob.did);
    assert.equal(key?.person, personDid("bob"));
  });
});

test("a timestamp more than five minutes from the server's clock is refused", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    for (const skew of [-6 * 60_000, 6 * 60_000]) {
      const res = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, roomId, Date.now() + skew));
      assert.equal(res.status, 401, `skew ${skew}`);
      assert.match(res.body.error!, /clock/);
    }
    const near = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, roomId, Date.now() - 60_000));
    assert.equal(near.status, 200, "a minute off is within the window");
  });
});

test("an auth request is good once: the same body again is refused", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const body = authBody(alice, roomId);
    assert.equal((await postJson(`${server.url}/rooms/${roomId}/auth`, body)).status, 200);
    const again = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, body);
    assert.equal(again.status, 401);
    assert.match(again.body.error!, /already used/);
  });
});

test("a signature by another key, or over another room, is refused", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const otherRoom = await createRoom(server.url);
    const forged = { ...authBody(bob, roomId), did: alice.did };
    assert.equal((await postJson(`${server.url}/rooms/${roomId}/auth`, forged)).status, 401);
    // Signed for one room, sent to another.
    assert.equal((await postJson(`${server.url}/rooms/${otherRoom}/auth`, authBody(alice, roomId))).status, 401);
  });
});

test("a malformed body is refused with 400", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const good = authBody(alice, roomId);
    for (const bad of [
      { did: alice.did },
      { ...good, did: "did:key:notavalidkey" },
      { ...good, signature: "not hex" },
      { ...good, timestamp: "now" },
      { ...good, x25519PublicKey: "AA".repeat(32) },
      { ...good, x25519PublicKey: "aa".repeat(31) },
      { ...good, x25519Signature: undefined },
    ]) {
      const res = await postJson(`${server.url}/rooms/${roomId}/auth`, bad);
      assert.equal(res.status, 400, JSON.stringify(bad));
    }
  });
});

test("auth to a suspended room answers 423", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    server.built.db.setSuspended(roomId, true);
    assert.equal((await authAs(server.url, roomId, alice)).status, 423);
    server.built.db.setSuspended(roomId, false);
    assert.equal((await authAs(server.url, roomId, alice)).status, 200);
  });
});

test("a room from before protocol 2 answers 410", async () => {
  const dataDir = mkdtempSync(path.join(tmpdir(), "link-server-legacy-"));
  try {
    const earlier = new Database(path.join(dataDir, "data.sqlite"));
    earlier.exec(`CREATE TABLE rooms (id TEXT PRIMARY KEY, admin_did TEXT NOT NULL, created_at TEXT NOT NULL, revision TEXT)`);
    earlier.prepare("INSERT INTO rooms (id, admin_did, created_at, revision) VALUES (?, ?, ?, ?)").run(
      "legacy-room",
      alice.did,
      new Date().toISOString(),
      ""
    );
    earlier.close();
    const server = await startTestServer({ autoAdmit: false, dataDir });
    try {
      const res = await authAs(server.url, "legacy-room", alice);
      assert.equal(res.status, 410);
      assert.match(res.body.error!, /protocol 2/);
    } finally {
      await server.close();
    }
  } finally {
    rmSync(dataDir, { recursive: true, force: true });
  }
});

test("a key its log has revoked is refused with 403; before the revocation it was let in", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const daveOld = personAgent("dave", "d2");
    server.store.showUpTo("dave", 0);
    assert.equal((await authAs(server.url, roomId, daveOld)).status, 200);

    server.store.showUpTo("dave", 1);
    const res = await authAs(server.url, roomId, daveOld);
    assert.equal(res.status, 403);
    assert.match(res.body.error!, /revoked/);
    // The same person's standing key still authenticates.
    assert.equal((await authAs(server.url, roomId, personAgent("dave"))).status, 200);
  });
});

test("auth answers 503 while the identity store is down, and works once it is back", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    server.store.down = true;
    const res = await authAs(server.url, roomId, alice);
    assert.equal(res.status, 503);
    assert.equal(server.built.db.getMemberKey(roomId, alice.did), undefined, "nothing is recorded");
    server.store.down = false;
    assert.equal((await authAs(server.url, roomId, alice)).status, 200);
  });
});

test("a key no log lists authenticates, not admitted, on a membership server", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const res = await authAs(server.url, roomId, await createTestAgent());
    assert.equal(res.status, 200);
    assert.equal(res.body.admitted, false);
  });
});

test("protected room endpoints require a bearer token", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/revision`);
    assert.equal(res.status, 401);
  });
});

test("a JWT issued for one room is rejected on a different room", async () => {
  await withServer(async (server) => {
    const roomA = await createRoom(server.url);
    const roomB = await createRoom(server.url);
    const auth = await authAs(server.url, roomA, alice);
    assert.equal(auth.body.admitted, true);
    const crossRoom = await getJson<{ error: string }>(`${server.url}/rooms/${roomB}/revision`, auth.body.token);
    assert.equal(crossRoom.status, 401);
  });
});

test("a garbage bearer token is rejected with 401", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/revision`, "not-a-jwt");
    assert.equal(res.status, 401);
  });
});
