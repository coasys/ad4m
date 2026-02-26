#!/usr/bin/env node
/**
 * AD4M Waker — Node.js integration tests
 *
 * Tests the waker by:
 * 1. Starting a mock GraphQL-over-WebSocket server
 * 2. Starting a mock wake HTTP server
 * 3. Running the waker pointed at both
 * 4. Simulating link events and verifying wake calls
 */

const assert = require("assert");
const http = require("http");
const WebSocket = require("ws");
const { startWaker, matchLink } = require("./ad4m-waker.js");

const TEST_PERSPECTIVE = "test-perspective-uuid-1234";

// ── Helpers ────────────────────────────────────────────────────────

const delay = (ms) => new Promise((r) => setTimeout(r, ms));

// ── Mock GraphQL WS server (graphql-transport-ws protocol) ─────────

function startMockGQLServer(port) {
  const subscribers = new Map(); // ws → subscriptionId
  const wss = new WebSocket.Server({ port });

  wss.on("connection", (ws) => {
    ws.on("message", (raw) => {
      const msg = JSON.parse(String(raw));
      if (msg.type === "connection_init") {
        ws.send(JSON.stringify({ type: "connection_ack" }));
      } else if (msg.type === "subscribe") {
        subscribers.set(ws, msg.id);
      }
    });
    ws.on("close", () => subscribers.delete(ws));
  });

  function sendLink(link) {
    for (const [ws, id] of subscribers) {
      ws.send(JSON.stringify({
        id,
        type: "next",
        payload: { data: { perspectiveLinkAdded: link } },
      }));
    }
  }

  return { wss, sendLink, close: () => wss.close() };
}

// ── Mock wake HTTP server ──────────────────────────────────────────

function startMockWakeServer(port) {
  const calls = [];
  const server = http.createServer((req, res) => {
    let body = "";
    req.on("data", (c) => body += c);
    req.on("end", () => {
      calls.push({
        body: JSON.parse(body),
        token: req.headers.authorization || "",
      });
      res.writeHead(200);
      res.end("ok");
    });
  });
  server.listen(port);
  return { server, calls, close: () => server.close() };
}

// ── Tests ──────────────────────────────────────────────────────────

async function testMatchLink() {
  console.log("  matchLink: basic filtering...");

  const sub = { id: "s1", perspective: "p1", matchPredicate: "ad4m://has_child", matchSource: "literal://string:chan1" };
  const matching = { author: "did:test", timestamp: 0, data: { source: "literal://string:chan1", predicate: "ad4m://has_child", target: "literal://string:msg1" } };
  const wrongPred = { author: "did:test", timestamp: 0, data: { source: "literal://string:chan1", predicate: "flux://body", target: "x" } };
  const wrongSrc = { author: "did:test", timestamp: 0, data: { source: "literal://string:chan2", predicate: "ad4m://has_child", target: "x" } };

  assert.strictEqual(matchLink(sub, matching), true, "should match");
  assert.strictEqual(matchLink(sub, wrongPred), false, "wrong predicate should not match");
  assert.strictEqual(matchLink(sub, wrongSrc), false, "wrong source should not match");

  // No filters = no match
  const emptySub = { id: "s2", perspective: "p1" };
  assert.strictEqual(matchLink(emptySub, matching), false, "empty filter should not match");

  console.log("  ✓ matchLink OK");
}

async function testWakeOnMatch() {
  console.log("  wake on matching link...");
  const GQL_PORT = 19876;
  const WAKE_PORT = 19877;

  const gql = startMockGQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "test-token",
      debounceMs: 100,
      subscriptions: [{
        id: "test-sub-1",
        perspective: TEST_PERSPECTIVE,
        matchPredicate: "ad4m://has_child",
        matchSource: "literal://string:channel1",
      }],
    };

    const waker = await startWaker(config);
    await delay(300);

    gql.sendLink({
      author: "did:test:author",
      timestamp: Date.now(),
      data: {
        source: "literal://string:channel1",
        predicate: "ad4m://has_child",
        target: "literal://string:msg123",
      },
    });

    await delay(500);
    assert.strictEqual(wake.calls.length, 1, `Expected 1 wake call, got ${wake.calls.length}`);
    assert.strictEqual(wake.calls[0].token, "Bearer test-token");
    assert.ok(wake.calls[0].body.text.includes("test-sub-1"), "wake text should include subscription ID");

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
  console.log("  ✓ wake on match OK");
}

async function testNoWakeOnMismatch() {
  console.log("  no wake on non-matching link...");
  const GQL_PORT = 19878;
  const WAKE_PORT = 19879;

  const gql = startMockGQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "tok",
      debounceMs: 100,
      subscriptions: [{
        id: "test-sub-2",
        perspective: TEST_PERSPECTIVE,
        matchPredicate: "ad4m://has_child",
        matchSource: "literal://string:channel1",
      }],
    };

    const waker = await startWaker(config);
    await delay(300);

    // Wrong predicate
    gql.sendLink({ author: "did:test", timestamp: Date.now(), data: { source: "literal://string:channel1", predicate: "flux://body", target: "x" } });
    // Wrong source
    gql.sendLink({ author: "did:test", timestamp: Date.now(), data: { source: "literal://string:channel2", predicate: "ad4m://has_child", target: "x" } });

    await delay(500);
    assert.strictEqual(wake.calls.length, 0, `Expected 0 wake calls, got ${wake.calls.length}`);

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
  console.log("  ✓ no wake on mismatch OK");
}

async function testDebounce() {
  console.log("  debounce collapses rapid links...");
  const GQL_PORT = 19880;
  const WAKE_PORT = 19881;

  const gql = startMockGQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "tok",
      debounceMs: 200,
      subscriptions: [{
        id: "test-sub-3",
        perspective: TEST_PERSPECTIVE,
        matchPredicate: "ad4m://has_child",
      }],
    };

    const waker = await startWaker(config);
    await delay(300);

    for (let i = 0; i < 5; i++) {
      gql.sendLink({ author: "did:test", timestamp: Date.now(), data: { source: "s", predicate: "ad4m://has_child", target: `t${i}` } });
      await delay(20);
    }

    await delay(600);
    assert.strictEqual(wake.calls.length, 1, `Debounce should collapse 5 links into 1 wake, got ${wake.calls.length}`);

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
  console.log("  ✓ debounce OK");
}

// ── Runner ─────────────────────────────────────────────────────────

async function run() {
  console.log("AD4M Waker Tests\n");
  await testMatchLink();
  await testWakeOnMatch();
  await testNoWakeOnMismatch();
  await testDebounce();
  console.log("\n✅ All tests passed!");
}

run().catch((e) => {
  console.error("❌ Test failed:", e);
  process.exit(1);
});
