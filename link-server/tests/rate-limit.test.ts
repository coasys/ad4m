import assert from "node:assert/strict";
import { test } from "node:test";
import { SlidingWindowLimiter } from "../src/rate-limit.js";
import {
  authAs,
  authenticateAgent,
  createRoom,
  createTestAgent,
  getJson,
  startTestServer,
  type TestServerHandle,
} from "./helpers.js";

async function withServer(
  overrides: Parameters<typeof startTestServer>[0],
  fn: (server: TestServerHandle) => Promise<void>
): Promise<void> {
  const server = await startTestServer(overrides);
  try {
    await fn(server);
  } finally {
    await server.close();
  }
}

// ---- unit tests for the limiter itself ----

test("SlidingWindowLimiter allows up to the limit, then rejects with a positive retryAfterSec", () => {
  const limiter = new SlidingWindowLimiter(3, 10_000);
  try {
    assert.equal(limiter.check("k").allowed, true);
    assert.equal(limiter.check("k").allowed, true);
    assert.equal(limiter.check("k").allowed, true);
    const fourth = limiter.check("k");
    assert.equal(fourth.allowed, false);
    assert.ok(fourth.retryAfterSec >= 1);
  } finally {
    limiter.close();
  }
});

test("SlidingWindowLimiter tracks keys independently", () => {
  const limiter = new SlidingWindowLimiter(1, 10_000);
  try {
    assert.equal(limiter.check("a").allowed, true);
    assert.equal(limiter.check("a").allowed, false);
    assert.equal(limiter.check("b").allowed, true, "a different key has its own budget");
  } finally {
    limiter.close();
  }
});

test("SlidingWindowLimiter allows again once the window has elapsed", async () => {
  const limiter = new SlidingWindowLimiter(1, 100);
  try {
    assert.equal(limiter.check("k").allowed, true);
    assert.equal(limiter.check("k").allowed, false);
    await new Promise((resolve) => setTimeout(resolve, 130));
    assert.equal(limiter.check("k").allowed, true);
  } finally {
    limiter.close();
  }
});

// ---- wired into the HTTP layer ----

test("auth endpoint enforces the per-IP limiter and returns 429 with Retry-After", async () => {
  await withServer({ rateLimits: { authIp: { limit: 3, windowMs: 60_000 } } }, async (server) => {
    // The genesis POST /membership counts against the same per-IP budget: one slot.
    const roomId = await createRoom(server.url);
    const statuses: number[] = [];
    for (let i = 0; i < 3; i++) statuses.push((await authAs(server.url, roomId, await createTestAgent())).status);
    assert.deepEqual(statuses, [200, 200, 429]);

    const res = await fetch(`${server.url}/rooms/${roomId}/auth`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ did: (await createTestAgent()).did }),
    });
    assert.equal(res.status, 429, "the limiter answers before the body is read");
    const retryAfterHeader = res.headers.get("retry-after");
    assert.ok(retryAfterHeader && Number(retryAfterHeader) >= 1);
  });
});

test("room endpoints enforce the per-JWT limiter, independently per agent", async () => {
  await withServer({ rateLimits: { roomJwt: { limit: 3, windowMs: 60_000 } } }, async (server) => {
    const roomId = await createRoom(server.url);
    const admin = await createTestAgent();
    const member = await createTestAgent();
    const adminToken = await authenticateAgent(server.url, roomId, admin);
    const memberToken = await authenticateAgent(server.url, roomId, member);

    // Drive admin past the limit of 3, then confirm member is unaffected.
    let adminLastStatus = 0;
    for (let i = 0; i < 4; i++) {
      const res = await getJson(`${server.url}/rooms/${roomId}/revision`, adminToken);
      adminLastStatus = res.status;
    }
    assert.equal(adminLastStatus, 429);

    const memberRes = await getJson(`${server.url}/rooms/${roomId}/revision`, memberToken);
    assert.equal(memberRes.status, 200, "a different agent's JWT has its own budget");
  });
});

test("commit has its own stricter per-JWT limiter on top of the general room limiter", async () => {
  await withServer(
    {
      rateLimits: {
        roomJwt: { limit: 1000, windowMs: 60_000 },
        commitJwt: { limit: 2, windowMs: 60_000 },
      },
    },
    async (server) => {
      const roomId = await createRoom(server.url);
      const agent = await createTestAgent();
      const token = await authenticateAgent(server.url, roomId, agent);

      let lastStatus = 0;
      let retryAfterHeader: string | null = null;
      for (let i = 0; i < 3; i++) {
        const res = await fetch(`${server.url}/rooms/${roomId}/commit`, {
          method: "POST",
          headers: { "content-type": "application/json", authorization: `Bearer ${token}` },
          body: JSON.stringify({ additions: [], removals: [] }),
        });
        lastStatus = res.status;
        retryAfterHeader = res.headers.get("retry-after");
      }
      assert.equal(lastStatus, 429);
      assert.ok(retryAfterHeader && Number(retryAfterHeader) >= 1);

      // The general (much higher) room limiter is unaffected -- reads still work.
      const revisionRes = await getJson(`${server.url}/rooms/${roomId}/revision`, token);
      assert.equal(revisionRes.status, 200);
    }
  );
});
