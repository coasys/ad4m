#!/usr/bin/env node
/**
 * AD4M Waker — integration tests
 *
 * Tests the waker's SurrealDB query subscription flow:
 * 1. Mock executor: handles perspectiveSubscribeSurrealQuery (HTTP) +
 *    perspectiveQuerySubscription (WS subscription) + keepalive
 * 2. Mock wake server: captures wake POST calls
 * 3. Simulates query result changes via subscription pushes
 */

const assert = require("assert");
const http = require("http");
const WebSocket = require("ws");
const { startWaker } = require("./ad4m-waker.js");

const delay = (ms) => new Promise((r) => setTimeout(r, ms));

/**
 * Mock AD4M executor that handles both HTTP (mutations) and WS (subscriptions).
 * Simulates perspectiveSubscribeSurrealQuery + perspectiveQuerySubscription.
 */
function startMockExecutor(port) {
  let surrealSubId = "mock-surreal-sub-" + Math.random().toString(36).slice(2);
  const wsSubscribers = new Map(); // ws → { id, subscriptionId }

  // HTTP server for mutations (subscribe + keepalive)
  const httpServer = http.createServer((req, res) => {
    let body = "";
    req.on("data", (c) => body += c);
    req.on("end", () => {
      const { query, variables } = JSON.parse(body);

      if (query.includes("perspectiveSubscribeSurrealQuery")) {
        // Return subscription ID + initial results (matches real GraphQL shape)
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({
          data: { 
            perspectiveSubscribeSurrealQuery: {
              subscriptionId: surrealSubId,
              result: [{ source: "s", predicate: "p", target: "t" }]
            }
          }
        }));
      } else if (query.includes("perspectiveKeepAliveSurrealQuery")) {
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ data: { perspectiveKeepAliveSurrealQuery: true } }));
      } else {
        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ data: null }));
      }
    });
  });

  // WebSocket server for subscriptions
  const wss = new WebSocket.Server({ server: httpServer });
  wss.on("connection", (ws) => {
    ws.on("message", (raw) => {
      const msg = JSON.parse(String(raw));
      if (msg.type === "connection_init") {
        ws.send(JSON.stringify({ type: "connection_ack" }));
      } else if (msg.type === "subscribe") {
        // Track subscriber
        wsSubscribers.set(ws, { id: msg.id, subscriptionId: surrealSubId });
        // Send init result
        ws.send(JSON.stringify({
          id: msg.id,
          type: "next",
          payload: {
            data: {
              perspectiveQuerySubscription: '#init#[{"source":"s","predicate":"p","target":"t"}]'
            }
          }
        }));
      }
    });
    ws.on("close", () => wsSubscribers.delete(ws));
  });

  httpServer.listen(port);

  function pushUpdate(resultArray) {
    const resultStr = JSON.stringify(resultArray);
    for (const [ws, info] of wsSubscribers) {
      ws.send(JSON.stringify({
        id: info.id,
        type: "next",
        payload: {
          data: { perspectiveQuerySubscription: resultStr }
        }
      }));
    }
  }

  return {
    httpServer, wss, pushUpdate, surrealSubId,
    close: () => { wss.close(); httpServer.close(); }
  };
}

function startMockWakeServer(port) {
  const calls = [];
  const server = http.createServer((req, res) => {
    let body = "";
    req.on("data", (c) => body += c);
    req.on("end", () => {
      calls.push({ body: JSON.parse(body), token: req.headers.authorization || "" });
      res.writeHead(200);
      res.end("ok");
    });
  });
  server.listen(port);
  return { server, calls, close: () => server.close() };
}

// ── Tests ──────────────────────────────────────────────────────────

async function testWakeOnQueryChange() {
  console.log("  wake on SurrealDB query result change...");
  const EXEC_PORT = 19876;
  const WAKE_PORT = 19877;

  const exec = startMockExecutor(EXEC_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    await delay(100); // let servers start

    const config = {
      executorUrl: `ws://localhost:${EXEC_PORT}/graphql`,
      token: "test-token",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "test-wake-token",
      debounceMs: 100,
      subscriptions: [{
        id: "test-sub-1",
        perspective: "test-perspective",
        query: "SELECT * FROM link WHERE predicate = 'ad4m://has_child'"
      }],
    };

    const waker = await startWaker(config);
    await delay(500);

    // Push a changed result
    exec.pushUpdate([
      { source: "s", predicate: "p", target: "t" },
      { source: "s2", predicate: "p2", target: "t2" }
    ]);

    await delay(500);
    assert.strictEqual(wake.calls.length, 1, `Expected 1 wake call, got ${wake.calls.length}`);
    assert.strictEqual(wake.calls[0].token, "Bearer test-wake-token");
    assert.ok(wake.calls[0].body.text.includes("test-sub-1"), "wake text should include subscription ID");

    waker.close();
  } finally {
    exec.close();
    wake.close();
  }
  console.log("  ✓ wake on query change OK");
}

async function testNoWakeOnSameResult() {
  console.log("  no wake when result unchanged...");
  const EXEC_PORT = 19878;
  const WAKE_PORT = 19879;

  const exec = startMockExecutor(EXEC_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    await delay(100);

    const config = {
      executorUrl: `ws://localhost:${EXEC_PORT}/graphql`,
      token: "test-token",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "tok",
      debounceMs: 100,
      subscriptions: [{
        id: "test-sub-2",
        perspective: "test-perspective",
        query: "SELECT * FROM link"
      }],
    };

    const waker = await startWaker(config);
    await delay(500);

    // Push same result as initial
    exec.pushUpdate([{ source: "s", predicate: "p", target: "t" }]);

    await delay(500);
    assert.strictEqual(wake.calls.length, 0, `Expected 0 wake calls, got ${wake.calls.length}`);

    waker.close();
  } finally {
    exec.close();
    wake.close();
  }
  console.log("  ✓ no wake on same result OK");
}

async function testDebounce() {
  console.log("  debounce collapses rapid changes...");
  const EXEC_PORT = 19880;
  const WAKE_PORT = 19881;

  const exec = startMockExecutor(EXEC_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    await delay(100);

    const config = {
      executorUrl: `ws://localhost:${EXEC_PORT}/graphql`,
      token: "test-token",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "tok",
      debounceMs: 200,
      subscriptions: [{
        id: "test-sub-3",
        perspective: "test-perspective",
        query: "SELECT * FROM link"
      }],
    };

    const waker = await startWaker(config);
    await delay(500);

    // Push 5 different results rapidly
    for (let i = 0; i < 5; i++) {
      exec.pushUpdate([{ source: "s", predicate: "p", target: `t${i}` }]);
      await delay(20);
    }

    await delay(600);
    assert.strictEqual(wake.calls.length, 1, `Debounce should collapse to 1 wake, got ${wake.calls.length}`);

    waker.close();
  } finally {
    exec.close();
    wake.close();
  }
  console.log("  ✓ debounce OK");
}

// ── Runner ─────────────────────────────────────────────────────────

async function run() {
  console.log("AD4M Waker Tests (SurrealDB query subscription)\n");
  await testWakeOnQueryChange();
  await testNoWakeOnSameResult();
  await testDebounce();
  console.log("\n✅ All tests passed!");
}

run().catch((e) => {
  console.error("❌ Test failed:", e);
  process.exit(1);
});
