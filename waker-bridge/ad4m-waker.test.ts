/**
 * AD4M Waker Bridge — integration test
 *
 * Tests the waker bridge by:
 * 1. Starting a mock GraphQL-over-WebSocket server
 * 2. Starting a mock wake webhook server
 * 3. Running the waker bridge pointed at both
 * 4. Simulating a link event and verifying the wake call
 */

import { assertEquals } from "https://deno.land/std@0.224.0/assert/mod.ts";
import { startWaker, WakerConfig, LinkExpression } from "./ad4m-waker.ts";

const TEST_PERSPECTIVE = "test-perspective-uuid-1234";

// ── Mock GraphQL WS server (graphql-transport-ws protocol) ─────────

function startMockGraphQLServer(port: number): {
  server: Deno.HttpServer;
  sendLink: (link: LinkExpression) => void;
  close: () => void;
} {
  const subscribers = new Map<WebSocket, string>();

  function sendLink(link: LinkExpression) {
    for (const [ws, id] of subscribers) {
      ws.send(JSON.stringify({
        id,
        type: "next",
        payload: {
          data: { perspectiveLinkAdded: link },
        },
      }));
    }
  }

  const server = Deno.serve({ port, onListen: () => {} }, (req) => {
    if (req.headers.get("upgrade") !== "websocket") {
      return new Response("Not a websocket", { status: 400 });
    }
    const { socket, response } = Deno.upgradeWebSocket(req);

    socket.onmessage = (event) => {
      const msg = JSON.parse(event.data);
      if (msg.type === "connection_init") {
        socket.send(JSON.stringify({ type: "connection_ack" }));
      } else if (msg.type === "subscribe") {
        subscribers.set(socket, msg.id);
      }
    };

    socket.onclose = () => subscribers.delete(socket);
    return response;
  });

  return {
    server,
    sendLink,
    close: () => server.shutdown(),
  };
}

// ── Mock wake webhook server ───────────────────────────────────────

function startMockWakeServer(port: number): {
  server: Deno.HttpServer;
  calls: { body: any; token: string }[];
  close: () => void;
} {
  const calls: { body: any; token: string }[] = [];

  const server = Deno.serve({ port, onListen: () => {} }, async (req) => {
    const body = await req.json();
    const token = req.headers.get("authorization") ?? "";
    calls.push({ body, token });
    return new Response("ok", { status: 200 });
  });

  return { server, calls, close: () => server.shutdown() };
}

// ── Helpers ────────────────────────────────────────────────────────

function delay(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

// ── Tests ──────────────────────────────────────────────────────────

Deno.test("waker bridge posts to wake endpoint on matching link", async () => {
  const GQL_PORT = 19876;
  const WAKE_PORT = 19877;

  const gql = startMockGraphQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config: WakerConfig = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      perspective: TEST_PERSPECTIVE,
      className: "Message",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "test-token",
      wakeMessage: "New message",
      debounceMs: 100, // fast for testing
    };

    const waker = await startWaker(config);
    await delay(200); // let subscription register

    // Send a matching link
    gql.sendLink({
      author: "did:test:author",
      timestamp: Date.now(),
      status: "shared",
      data: {
        source: "flux://channel-general",
        predicate: "rdf://type",
        target: "flux://Message",
      },
    });

    // Wait for debounce + network
    await delay(500);

    assertEquals(wake.calls.length, 1, "Expected exactly 1 wake call");
    assertEquals(wake.calls[0].token, "Bearer test-token");
    assertEquals(wake.calls[0].body.message, "New message");
    assertEquals(wake.calls[0].body.context.class, "Message");

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
});

Deno.test("waker bridge ignores non-matching links", async () => {
  const GQL_PORT = 19878;
  const WAKE_PORT = 19879;

  const gql = startMockGraphQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config: WakerConfig = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      perspective: TEST_PERSPECTIVE,
      className: "Message",
      source: "flux://channel-general",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "test-token",
      wakeMessage: "New message",
      debounceMs: 100,
    };

    const waker = await startWaker(config);
    await delay(200);

    // Send non-matching link (wrong predicate)
    gql.sendLink({
      author: "did:test:author",
      timestamp: Date.now(),
      data: {
        source: "flux://channel-general",
        predicate: "ad4m://some-other",
        target: "flux://Message",
      },
    });

    // Send non-matching link (wrong source)
    gql.sendLink({
      author: "did:test:author",
      timestamp: Date.now(),
      data: {
        source: "flux://channel-random",
        predicate: "rdf://type",
        target: "flux://Message",
      },
    });

    await delay(500);

    assertEquals(wake.calls.length, 0, "Expected no wake calls for non-matching links");

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
});

Deno.test("waker bridge debounces rapid links", async () => {
  const GQL_PORT = 19880;
  const WAKE_PORT = 19881;

  const gql = startMockGraphQLServer(GQL_PORT);
  const wake = startMockWakeServer(WAKE_PORT);

  try {
    const config: WakerConfig = {
      executorUrl: `ws://localhost:${GQL_PORT}`,
      perspective: TEST_PERSPECTIVE,
      className: "Message",
      wakeUrl: `http://localhost:${WAKE_PORT}/hooks/wake`,
      wakeToken: "tok",
      wakeMessage: "msg",
      debounceMs: 200,
    };

    const waker = await startWaker(config);
    await delay(200);

    // Fire 5 matching links rapidly
    for (let i = 0; i < 5; i++) {
      gql.sendLink({
        author: "did:test:author",
        timestamp: Date.now(),
        data: {
          source: "s",
          predicate: "rdf://type",
          target: "flux://Message",
        },
      });
      await delay(20);
    }

    await delay(500);

    assertEquals(wake.calls.length, 1, "Debounce should collapse 5 rapid links into 1 wake call");

    waker.close();
  } finally {
    gql.close();
    wake.close();
  }
});
