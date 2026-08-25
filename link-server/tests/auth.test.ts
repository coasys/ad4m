import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { test } from "node:test";
import {
  createTestAgent,
  getJson,
  postJson,
  signChallenge,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

async function withServer(fn: (server: TestServerHandle) => Promise<void>): Promise<void> {
  const server = await startTestServer();
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

test("first agent to auth becomes room admin and receives a JWT", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();

    const challengeRes = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
    });
    assert.equal(challengeRes.status, 200);
    assert.equal(typeof challengeRes.body.challenge, "string");

    const signature = await signChallenge(agent, challengeRes.body.challenge);
    const verifyRes = await postJson<{ token: string; expiresAt: string }>(
      `${server.url}/rooms/${roomId}/auth`,
      { did: agent.did, challenge: challengeRes.body.challenge, signature }
    );
    assert.equal(verifyRes.status, 200);
    assert.equal(typeof verifyRes.body.token, "string");
    assert.equal(typeof verifyRes.body.expiresAt, "string");

    const acl = await getJson<{ admin: string; members: string[] }>(
      `${server.url}/rooms/${roomId}/acl`,
      verifyRes.body.token
    );
    assert.equal(acl.status, 200);
    assert.equal(acl.body.admin, agent.did);
    assert.deepEqual(acl.body.members, [agent.did]);
  });
});

test("second agent not yet on the ACL gets 403", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const stranger = await createTestAgent();

    // admin creates the room
    const c1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: admin.did,
    });
    const s1 = await signChallenge(admin, c1.body.challenge);
    await postJson(`${server.url}/rooms/${roomId}/auth`, {
      did: admin.did,
      challenge: c1.body.challenge,
      signature: s1,
    });

    // stranger tries to auth against the now-existing room
    const c2 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: stranger.did,
    });
    assert.equal(c2.status, 200);
    const s2 = await signChallenge(stranger, c2.body.challenge);
    const verify2 = await postJson<{ error: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: stranger.did,
      challenge: c2.body.challenge,
      signature: s2,
    });
    assert.equal(verify2.status, 403);
  });
});

test("admin can add a DID to the ACL, after which that agent can authenticate", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    const friend = await createTestAgent();

    const c1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: admin.did,
    });
    const s1 = await signChallenge(admin, c1.body.challenge);
    const adminAuth = await postJson<{ token: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: admin.did,
      challenge: c1.body.challenge,
      signature: s1,
    });
    const adminToken = adminAuth.body.token;

    const aclRes = await postJson<{ members: string[] }>(
      `${server.url}/rooms/${roomId}/acl`,
      { action: "add", did: friend.did },
      adminToken
    );
    assert.equal(aclRes.status, 200);
    assert.ok(aclRes.body.members.includes(friend.did));

    const c2 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: friend.did,
    });
    const s2 = await signChallenge(friend, c2.body.challenge);
    const friendAuth = await postJson<{ token: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: friend.did,
      challenge: c2.body.challenge,
      signature: s2,
    });
    assert.equal(friendAuth.status, 200);
    assert.equal(typeof friendAuth.body.token, "string");
  });
});

test("wrong signature is rejected with 401 and challenge is single-use", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const agent = await createTestAgent();
    const impostor = await createTestAgent();

    const c1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
    });
    // Sign with the WRONG key.
    const badSig = await signChallenge(impostor, c1.body.challenge);
    const badVerify = await postJson<{ error: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
      challenge: c1.body.challenge,
      signature: badSig,
    });
    assert.equal(badVerify.status, 401);

    // Even with the RIGHT signature, the challenge was already consumed by the failed attempt.
    const goodSig = await signChallenge(agent, c1.body.challenge);
    const replay = await postJson<{ error: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: agent.did,
      challenge: c1.body.challenge,
      signature: goodSig,
    });
    assert.equal(replay.status, 401);
  });
});

test("malformed did:key is rejected with 400", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const res = await postJson<{ error: string }>(`${server.url}/rooms/${roomId}/auth`, {
      did: "did:key:notavalidkey",
    });
    assert.equal(res.status, 400);
  });
});

test("protected room endpoints require a bearer token", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/revision`);
    assert.equal(res.status, 401);
  });
});

test("a JWT issued for one room is rejected on a different room", async () => {
  await withServer(async (server) => {
    const roomA = randomUUID();
    const roomB = randomUUID();
    const agent = await createTestAgent();

    const c1 = await postJson<{ challenge: string }>(`${server.url}/rooms/${roomA}/auth`, {
      did: agent.did,
    });
    const s1 = await signChallenge(agent, c1.body.challenge);
    const auth1 = await postJson<{ token: string }>(`${server.url}/rooms/${roomA}/auth`, {
      did: agent.did,
      challenge: c1.body.challenge,
      signature: s1,
    });

    const crossRoom = await getJson<{ error: string }>(
      `${server.url}/rooms/${roomB}/revision`,
      auth1.body.token
    );
    assert.equal(crossRoom.status, 401);
  });
});

test("a garbage bearer token is rejected with 401", async () => {
  await withServer(async (server) => {
    const roomId = randomUUID();
    const res = await getJson<{ error: string }>(`${server.url}/rooms/${roomId}/revision`, "not-a-jwt");
    assert.equal(res.status, 401);
  });
});
