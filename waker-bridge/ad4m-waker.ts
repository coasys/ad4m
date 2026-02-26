#!/usr/bin/env -S deno run --allow-net --allow-env --allow-read
/**
 * AD4M Waker Bridge
 *
 * Watches AD4M perspectives for link changes via perspectiveLinkAdded
 * GraphQL subscriptions and wakes an OpenClaw agent when matching
 * links are detected.
 *
 * Subscriptions are defined by ID + match criteria (predicate, source, target).
 * When a match fires, the agent is woken with just the subscription ID.
 * The agent looks up context from its memory and uses MCP tools to fetch data.
 *
 * Usage:
 *   deno run --allow-net --allow-read ad4m-waker.ts --config waker-subscriptions.json
 *
 * Or single subscription:
 *   deno run --allow-net ad4m-waker.ts \
 *     --executor-url ws://localhost:12100/graphql \
 *     --token <ad4m-token> \
 *     --wake-url http://localhost:18789/hooks/wake \
 *     --wake-token <hooks-token> \
 *     --perspective <uuid> \
 *     --id <subscription-id> \
 *     --match-predicate "ad4m://has_child" \
 *     --match-source "literal://string:<channel-id>"
 */

import { parse } from "https://deno.land/std@0.224.0/flags/mod.ts";

// ── Types ──────────────────────────────────────────────────────────

export interface Subscription {
  id: string;                   // Subscription ID (from MCP tool)
  perspective: string;          // Perspective UUID
  matchPredicate?: string;      // Filter: link predicate must match
  matchSource?: string;         // Filter: link source must contain this
  matchTarget?: string;         // Filter: link target must contain this
}

export interface WakerConfig {
  executorUrl: string;
  token?: string;               // AD4M capability token
  wakeUrl: string;              // OpenClaw hooks/wake endpoint
  wakeToken: string;            // Bearer token for wake endpoint
  subscriptions: Subscription[];
  debounceMs?: number;          // Per-subscription debounce (default 2000)
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
    this.ws.onclose = () => {
      console.log("[waker] ws closed — reconnecting in 5s...");
      setTimeout(() => this.connect(), 5000);
    };

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

function matchLink(sub: Subscription, link: LinkExpression): boolean {
  const { source, predicate, target } = link.data;

  if (sub.matchPredicate && predicate !== sub.matchPredicate) return false;
  if (sub.matchSource && !source.includes(sub.matchSource)) return false;
  if (sub.matchTarget && !target.includes(sub.matchTarget)) return false;

  // At least one filter must be set
  if (!sub.matchPredicate && !sub.matchSource && !sub.matchTarget) return false;

  return true;
}

// ── Wake poster ────────────────────────────────────────────────────

async function postWake(config: WakerConfig, sub: Subscription, link: LinkExpression): Promise<void> {
  try {
    const body = {
      text: `[AD4M waker] subscription=${sub.id} | ${link.data.predicate} → ${link.data.target.substring(0, 80)} (by ${link.author.substring(0, 30)})`,
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

    const _body = await resp.text();
    if (!resp.ok) {
      console.error(`[waker] wake POST failed: ${resp.status} ${_body}`);
    } else {
      console.log(`[waker] wake sent for subscription ${sub.id}`);
    }
  } catch (err) {
    console.error("[waker] wake POST error:", err);
  }
}

// ── Main ───────────────────────────────────────────────────────────

export async function startWaker(config: WakerConfig): Promise<{ close: () => void }> {
  const debounceMs = config.debounceMs ?? 2000;

  console.log(`[waker] connecting to ${config.executorUrl}`);
  const client = new GraphQLWSClient(
    config.executorUrl,
    config.token ? { headers: { authorization: config.token } } : {},
  );
  await client.connect();
  console.log("[waker] connected");

  // Group subscriptions by perspective
  const byPerspective = new Map<string, Subscription[]>();
  for (const sub of config.subscriptions) {
    const list = byPerspective.get(sub.perspective) ?? [];
    list.push(sub);
    byPerspective.set(sub.perspective, list);
  }

  // Per-subscription debounce state
  const debounceTimers = new Map<string, number>();

  function onLink(link: LinkExpression, subs: Subscription[]) {
    for (const sub of subs) {
      if (!matchLink(sub, link)) continue;

      console.log(`[waker] matched sub=${sub.id}: ${link.data.predicate} → ${link.data.target.substring(0, 60)}`);

      // Debounce per subscription
      const existing = debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);
      debounceTimers.set(sub.id, setTimeout(() => {
        postWake(config, sub, link);
        debounceTimers.delete(sub.id);
      }, debounceMs) as unknown as number);
    }
  }

  // Subscribe to perspectiveLinkAdded for each perspective
  const LINK_FIELDS = `author timestamp status data { source predicate target }`;

  for (const [perspectiveUuid, subs] of byPerspective) {
    const query = `subscription { perspectiveLinkAdded(uuid: "${perspectiveUuid}") { ${LINK_FIELDS} } }`;
    client.subscribe(query, {}, (payload) => {
      if (payload?.data?.perspectiveLinkAdded) {
        onLink(payload.data.perspectiveLinkAdded, subs);
      }
    });
    console.log(`[waker] watching perspective ${perspectiveUuid.substring(0, 8)}... (${subs.length} subscription(s))`);
    for (const sub of subs) {
      console.log(`[waker]   - ${sub.id}: predicate=${sub.matchPredicate || '*'} source=${sub.matchSource?.substring(0, 40) || '*'} target=${sub.matchTarget || '*'}`);
    }
  }

  return {
    close() {
      for (const timer of debounceTimers.values()) clearTimeout(timer);
      client.close();
    },
  };
}

// ── CLI ────────────────────────────────────────────────────────────

if (import.meta.main) {
  const args = parse(Deno.args, {
    string: [
      "executor-url", "token", "wake-url", "wake-token", "config",
      "perspective", "id", "match-predicate", "match-source", "match-target",
    ],
    boolean: ["help"],
    default: {
      "executor-url": "ws://localhost:12100/graphql",
    },
  });

  if (args.help) {
    console.log(`
AD4M Waker Bridge — watch AD4M perspectives, wake OpenClaw agent

Usage with config file:
  ad4m-waker --config subscriptions.json [--executor-url <url>] [--token <token>]
             --wake-url <url> --wake-token <token>

Usage with single subscription:
  ad4m-waker --perspective <uuid> --id <sub-id>
             --match-predicate "ad4m://has_child"
             [--match-source "literal://string:..."]
             [--match-target "..."]
             --wake-url <url> --wake-token <token>

Config file format (subscriptions.json):
  [
    {
      "id": "sub_001",
      "perspective": "<uuid>",
      "matchPredicate": "ad4m://has_child",
      "matchSource": "literal://string:<channel-id>"
    }
  ]
`);
    Deno.exit(0);
  }

  // Build config
  let subscriptions: Subscription[] = [];

  // Load from config file if provided
  if (args.config) {
    try {
      const raw = await Deno.readTextFile(args.config);
      subscriptions = JSON.parse(raw);
      console.log(`[waker] loaded ${subscriptions.length} subscription(s) from ${args.config}`);
    } catch (e) {
      console.error(`[waker] failed to read config: ${e}`);
      Deno.exit(1);
    }
  }

  // Add CLI subscription if provided
  if (args.perspective && args.id) {
    subscriptions.push({
      id: args.id,
      perspective: args.perspective,
      matchPredicate: args["match-predicate"],
      matchSource: args["match-source"],
      matchTarget: args["match-target"],
    });
  }

  if (subscriptions.length === 0 || !args["wake-url"] || !args["wake-token"]) {
    console.error("[waker] Need at least one subscription and --wake-url + --wake-token");
    Deno.exit(1);
  }

  const config: WakerConfig = {
    executorUrl: args["executor-url"],
    token: args.token,
    wakeUrl: args["wake-url"],
    wakeToken: args["wake-token"],
    subscriptions,
  };

  const waker = await startWaker(config);

  Deno.addSignalListener("SIGINT", () => {
    console.log("\n[waker] shutting down...");
    waker.close();
    Deno.exit(0);
  });

  await new Promise(() => {});
}
