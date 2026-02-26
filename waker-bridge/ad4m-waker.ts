#!/usr/bin/env -S deno run --allow-net --allow-env
/**
 * AD4M Waker Bridge
 *
 * Watches an AD4M executor for perspective link changes via GraphQL
 * WebSocket subscriptions and POSTs to an OpenClaw wake endpoint
 * when matching links are detected.
 *
 * Usage:
 *   deno run --allow-net ad4m-waker.ts \
 *     --executor-url ws://localhost:12000/graphql \
 *     --perspective <uuid> \
 *     --class Message \
 *     --wake-url http://localhost:18789/hooks/wake \
 *     --wake-token "my-token" \
 *     --wake-message "New message"
 */

import { parse } from "https://deno.land/std@0.224.0/flags/mod.ts";

// ── Types ──────────────────────────────────────────────────────────

export interface WakerConfig {
  executorUrl: string;          // ws:// or wss:// GraphQL endpoint
  perspective: string;          // perspective UUID
  className?: string;           // subject class to watch (e.g. "Message")
  source?: string;              // optional source filter
  wakeUrl: string;              // OpenClaw wake endpoint
  wakeToken: string;            // Bearer token for wake endpoint
  wakeMessage: string;          // Message to include in wake payload
  token?: string;               // AD4M capability token
  debounceMs?: number;          // debounce interval (default 2000)
  alsoRemoved?: boolean;        // also watch linkRemoved
}

export interface LinkExpression {
  author: string;
  timestamp: number;
  status?: string;
  data: {
    source: string;
    predicate: string;
    target: string;
  };
}

// ── GraphQL-over-WebSocket (graphql-ws protocol) ───────────────────

const GQL_CONNECTION_INIT = "connection_init";
const GQL_CONNECTION_ACK = "connection_ack";
const GQL_SUBSCRIBE = "subscribe";
const GQL_NEXT = "next";
const GQL_ERROR = "error";
const GQL_COMPLETE = "complete";

/**
 * Minimal graphql-ws protocol client for Deno WebSocket.
 */
class GraphQLWSClient {
  private ws!: WebSocket;
  private nextId = 1;
  private handlers = new Map<string, (data: any) => void>();
  private ready: Promise<void>;
  private resolveReady!: () => void;

  constructor(
    private url: string,
    private connectionParams?: Record<string, unknown>,
  ) {
    this.ready = new Promise((r) => (this.resolveReady = r));
  }

  async connect(): Promise<void> {
    this.ready = new Promise((r) => (this.resolveReady = r));
    this.ws = new WebSocket(this.url, "graphql-transport-ws");

    this.ws.onopen = () => {
      this.ws.send(
        JSON.stringify({
          type: GQL_CONNECTION_INIT,
          payload: this.connectionParams ?? {},
        }),
      );
    };

    this.ws.onmessage = (event) => {
      const msg = JSON.parse(String(event.data));
      switch (msg.type) {
        case GQL_CONNECTION_ACK:
          this.resolveReady();
          break;
        case GQL_NEXT: {
          const handler = this.handlers.get(msg.id);
          if (handler) handler(msg.payload);
          break;
        }
        case GQL_ERROR:
          console.error("[waker] subscription error:", msg.payload);
          break;
        case GQL_COMPLETE:
          this.handlers.delete(msg.id);
          break;
      }
    };

    this.ws.onerror = (e) => console.error("[waker] ws error:", e);
    this.ws.onclose = () => console.log("[waker] ws closed");

    await this.ready;
  }

  subscribe(query: string, variables: Record<string, unknown>, handler: (data: any) => void): string {
    const id = String(this.nextId++);
    this.handlers.set(id, handler);
    this.ws.send(
      JSON.stringify({
        id,
        type: GQL_SUBSCRIBE,
        payload: { query, variables },
      }),
    );
    return id;
  }

  close() {
    this.ws.close();
  }
}

// ── Link matching ──────────────────────────────────────────────────

function buildMatcher(config: WakerConfig): (link: LinkExpression) => boolean {
  return (link: LinkExpression) => {
    const { source, predicate, target } = link.data;

    // Class-based matching: look for type links
    if (config.className) {
      // Match rdf://type links pointing to the class
      const isTypeLink =
        predicate === "rdf://type" &&
        target.toLowerCase().includes(config.className.toLowerCase());

      // Match ad4m://has_child collection links
      const isCollectionLink = predicate === "ad4m://has_child";

      if (!isTypeLink && !isCollectionLink) return false;
    }

    // Source filter
    if (config.source && !source.includes(config.source)) return false;

    return true;
  };
}

// ── Wake poster ────────────────────────────────────────────────────

async function postWake(config: WakerConfig, link: LinkExpression): Promise<void> {
  try {
    const body = {
      text: `${config.wakeMessage} | ${link.data.predicate} -> ${link.data.target.substring(0, 100)} (by ${link.author.substring(0, 30)})`,
      mode: "now",
    };

    const resp = await fetch(config.wakeUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${config.wakeToken}`,
      },
      body: JSON.stringify(body),
    });

    const _body = await resp.text(); // consume body
    if (!resp.ok) {
      console.error(`[waker] wake POST failed: ${resp.status} ${_body}`);
    } else {
      console.log(`[waker] wake sent: ${config.wakeMessage}`);
    }
  } catch (err) {
    console.error("[waker] wake POST error:", err);
  }
}

// ── Main bridge logic ──────────────────────────────────────────────

export async function startWaker(config: WakerConfig): Promise<{ close: () => void }> {
  const debounceMs = config.debounceMs ?? 2000;
  const matcher = buildMatcher(config);

  console.log(`[waker] connecting to ${config.executorUrl}`);
  const client = new GraphQLWSClient(
    config.executorUrl,
    config.token ? { headers: { authorization: config.token } } : {},
  );
  await client.connect();
  console.log("[waker] connected");

  // Debounce: avoid flooding wake endpoint
  let debounceTimer: number | undefined;
  let pendingLink: LinkExpression | null = null;

  function onLink(link: LinkExpression) {
    if (!matcher(link)) return;
    console.log(`[waker] matched link: ${link.data.predicate} -> ${link.data.target}`);
    pendingLink = link;
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(() => {
      if (pendingLink) {
        postWake(config, pendingLink);
        pendingLink = null;
      }
    }, debounceMs) as unknown as number;
  }

  const LINK_FIELDS = `author timestamp status data { source predicate target }`;

  const addedQuery = `subscription { perspectiveLinkAdded(uuid: "${config.perspective}") { ${LINK_FIELDS} } }`;
  client.subscribe(addedQuery, {}, (payload) => {
    if (payload?.data?.perspectiveLinkAdded) {
      onLink(payload.data.perspectiveLinkAdded);
    }
  });
  console.log(`[waker] subscribed to linkAdded for ${config.perspective}`);

  if (config.alsoRemoved) {
    const removedQuery = `subscription { perspectiveLinkRemoved(uuid: "${config.perspective}") { ${LINK_FIELDS} } }`;
    client.subscribe(removedQuery, {}, (payload) => {
      if (payload?.data?.perspectiveLinkRemoved) {
        onLink(payload.data.perspectiveLinkRemoved);
      }
    });
    console.log("[waker] subscribed to linkRemoved");
  }

  return {
    close() {
      if (debounceTimer) clearTimeout(debounceTimer);
      client.close();
    },
  };
}

// ── CLI ────────────────────────────────────────────────────────────

if (import.meta.main) {
  const args = parse(Deno.args, {
    string: [
      "executor-url", "perspective", "class", "source",
      "wake-url", "wake-token", "wake-message", "token",
    ],
    boolean: ["also-removed", "help"],
    default: {
      "executor-url": "ws://localhost:12000/graphql",
      "wake-message": "AD4M perspective changed",
      "also-removed": false,
    },
  });

  if (args.help || !args.perspective || !args["wake-url"] || !args["wake-token"]) {
    console.log(`
AD4M Waker Bridge — watch AD4M perspectives, wake OpenClaw

Usage:
  ad4m-waker --executor-url <ws-url> --perspective <uuid> [options]

Required:
  --perspective <uuid>    Perspective UUID to watch
  --wake-url <url>        OpenClaw wake endpoint
  --wake-token <token>    Bearer token for wake endpoint

Options:
  --executor-url <url>    AD4M executor GraphQL WS URL (default: ws://localhost:12000/graphql)
  --class <name>          Subject class to filter (e.g. Message)
  --source <uri>          Filter links by source containing this value
  --wake-message <msg>    Message in wake payload (default: "AD4M perspective changed")
  --token <cap-token>     AD4M capability token for auth
  --also-removed          Also watch for link removals
  --help                  Show this help
`);
    Deno.exit(args.help ? 0 : 1);
  }

  const config: WakerConfig = {
    executorUrl: args["executor-url"],
    perspective: args.perspective,
    className: args["class"],
    source: args.source,
    wakeUrl: args["wake-url"],
    wakeToken: args["wake-token"],
    wakeMessage: args["wake-message"],
    token: args.token,
    alsoRemoved: args["also-removed"],
  };

  const waker = await startWaker(config);

  // Keep alive until SIGINT
  Deno.addSignalListener("SIGINT", () => {
    console.log("\n[waker] shutting down...");
    waker.close();
    Deno.exit(0);
  });

  // Block forever
  await new Promise(() => {});
}
