/**
 * Protocol 2 auth: a key signs `coasys-link-server:auth:2:<origin>:<roomId>:<timestamp>` and its
 * X25519 key, the server asks the identity store whom the key speaks for, and answers
 * `{token, expiresAt, admitted}`. Auth never creates a room. The origin is the server's own
 * (`PUBLIC_ORIGIN`): a signature made for another server is refused.
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
import { buildServer } from "../src/server.js";
import { publicOrigin } from "../src/auth.js";
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
      const res = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, server.url, roomId, Date.now() + skew));
      assert.equal(res.status, 401, `skew ${skew}`);
      assert.match(res.body.error!, /clock/);
    }
    const near = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, server.url, roomId, Date.now() - 60_000));
    assert.equal(near.status, 200, "a minute off is within the window");
  });
});

test("an auth request is good once: the same body again is refused", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const body = authBody(alice, server.url, roomId);
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
    const forged = { ...authBody(bob, server.url, roomId), did: alice.did };
    assert.equal((await postJson(`${server.url}/rooms/${roomId}/auth`, forged)).status, 401);
    // Signed for one room, sent to another.
    assert.equal((await postJson(`${server.url}/rooms/${otherRoom}/auth`, authBody(alice, server.url, roomId))).status, 401);
  });
});

test("a malformed body is refused with 400", async () => {
  await withServer(async (server) => {
    const roomId = await createRoom(server.url);
    const good = authBody(alice, server.url, roomId);
    for (const bad of [
      { did: alice.did },
      { ...good, origin: undefined },
      { ...good, origin: 7 },
      { ...good, origin: `${server.url}/` },
      { ...good, origin: server.url.toUpperCase() },
      { ...good, origin: `${server.url}/rooms` },
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

// ---- the origin (the auth message names the server it is for) ----

const ORIGIN = "https://links.example.org";

/** A membership server that takes auth signed for `publicOrigins` only (no Host-header origin). */
async function withOrigins(publicOrigins: string[], fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer({ autoAdmit: false, hostOrigin: false, publicOrigins });
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

test("a signature for the server's public origin passes; one for another origin is refused", async () => {
  await withOrigins([ORIGIN], async (server) => {
    const roomId = await createRoom(server.url);
    const auth = (body: unknown) => postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, body);

    const ok = await auth(authBody(alice, ORIGIN, roomId));
    assert.equal(ok.status, 200, JSON.stringify(ok.body));
    assert.equal(ok.body.admitted, true);

    // Signed for another server, and sent here naming that server.
    const elsewhere = await auth(authBody(alice, "https://other.example.org", roomId));
    assert.equal(elsewhere.status, 401, JSON.stringify(elsewhere.body));
    assert.match(elsewhere.body.error!, /origin/);

    // Signed for another server, and sent here naming this one: the signature does not verify.
    const relayed = { ...authBody(alice, "https://other.example.org", roomId), origin: ORIGIN };
    const res = await auth(relayed);
    assert.equal(res.status, 401, JSON.stringify(res.body));
    assert.match(res.body.error!, /invalid signature/);

    // The address it was reached at is not an origin it was configured with.
    const byHost = await auth(authBody(alice, server.url, roomId));
    assert.equal(byHost.status, 401, JSON.stringify(byHost.body));
    assert.match(byHost.body.error!, /origin/);
  });
});

test("a server with several public origins takes a signature for each", async () => {
  await withOrigins(["https://links.example.org", "http://10.0.0.5:3456"], async (server) => {
    const roomId = await createRoom(server.url);
    for (const origin of ["https://links.example.org", "http://10.0.0.5:3456"]) {
      const res = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, origin, roomId));
      assert.equal(res.status, 200, `${origin}: ${JSON.stringify(res.body)}`);
    }
  });
});

test("a configured origin is normalised: case, a trailing slash and a default port do not matter", async () => {
  await withOrigins(["HTTPS://Links.Example.org:443/"], async (server) => {
    const roomId = await createRoom(server.url);
    const res = await postJson<AuthAnswer>(`${server.url}/rooms/${roomId}/auth`, authBody(alice, ORIGIN, roomId));
    assert.equal(res.status, 200, JSON.stringify(res.body));
  });
});

test("publicOrigin: scheme, host and port, lower case, no trailing slash, the default port dropped", () => {
  assert.equal(publicOrigin("https://links.example.org"), "https://links.example.org");
  assert.equal(publicOrigin("HTTPS://Links.Example.ORG/"), "https://links.example.org");
  assert.equal(publicOrigin("https://links.example.org:443"), "https://links.example.org");
  assert.equal(publicOrigin("http://links.example.org:80/"), "http://links.example.org");
  assert.equal(publicOrigin("http://127.0.0.1:3456"), "http://127.0.0.1:3456");
  assert.equal(publicOrigin("https://links.example.org:8443"), "https://links.example.org:8443");
  for (const bad of [
    "links.example.org",
    "ftp://links.example.org",
    "https://links.example.org/rooms",
    "https://links.example.org/?a=1",
    "https://links.example.org/#x",
    "https://user@links.example.org",
    "",
  ]) {
    assert.throws(() => publicOrigin(bad), /origin/, bad);
  }
});

test("buildServer refuses to start without an origin to take auth for, or with one that is not an origin", async () => {
  await assert.rejects(() => buildServer({ dataDir: ":memory:" }), /PUBLIC_ORIGIN/);
  await assert.rejects(() => buildServer({ dataDir: ":memory:", publicOrigins: [] }), /PUBLIC_ORIGIN/);
  await assert.rejects(
    () => buildServer({ dataDir: ":memory:", publicOrigins: ["https://links.example.org/sub"] }),
    /origin/
  );
});
