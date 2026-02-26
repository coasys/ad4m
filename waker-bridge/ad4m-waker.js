#!/usr/bin/env node
/**
 * AD4M Waker — Node.js
 *
 * Watches AD4M perspectives for link changes via perspectiveLinkAdded
 * GraphQL subscription and wakes an OpenClaw agent when matching
 * links are detected.
 *
 * Uses the graphql-ws protocol over WebSocket — no ad4m client dependency,
 * just raw GraphQL subscriptions (lighter, more portable).
 *
 * Subscriptions are defined by ID + match criteria (predicate, source, target).
 * When a match fires, the agent is woken with just the subscription ID.
 * The agent looks up context from its memory and uses MCP tools to fetch data.
 *
 * Usage:
 *   node ad4m-waker.js --config waker-config.json
 *
 * Or single subscription:
 *   node ad4m-waker.js \
 *     --executor-url ws://localhost:12100/graphql \
 *     --token <ad4m-token> \
 *     --wake-url http://localhost:18789/hooks/wake \
 *     --wake-token <hooks-token> \
 *     --perspective <uuid> \
 *     --id <subscription-id> \
 *     --match-predicate "ad4m://has_child" \
 *     --match-source "literal://string:<channel-id>"
 */

const WebSocket = require("ws");
const http = require("http");
const https = require("https");
const fs = require("fs");
const path = require("path");

// ── Types (JSDoc) ──────────────────────────────────────────────────
/**
 * @typedef {{ id: string, perspective: string, matchPredicate?: string, matchSource?: string, matchTarget?: string }} Subscription
 * @typedef {{ executorUrl: string, token?: string, wakeUrl: string, wakeToken: string, subscriptions: Subscription[], debounceMs?: number }} WakerConfig
 * @typedef {{ author: string, timestamp: number, status?: string, data: { source: string, predicate: string, target: string } }} LinkExpression
 */

// ── GraphQL-over-WebSocket (graphql-transport-ws protocol) ─────────

const GQL_CONNECTION_INIT = "connection_init";
const GQL_CONNECTION_ACK = "connection_ack";
const GQL_SUBSCRIBE = "subscribe";
const GQL_NEXT = "next";
const GQL_ERROR = "error";
const GQL_COMPLETE = "complete";
const GQL_PING = "ping";
const GQL_PONG = "pong";

class GraphQLWSClient {
  constructor(url, connectionParams) {
    this.url = url;
    this.connectionParams = connectionParams || {};
    this.nextId = 1;
    this.handlers = new Map();
    this._subscriptions = []; // for reconnect
    this._closed = false;
  }

  connect() {
    return new Promise((resolve, reject) => {
      this.ws = new WebSocket(this.url, "graphql-transport-ws");

      this.ws.on("open", () => {
        this.ws.send(JSON.stringify({
          type: GQL_CONNECTION_INIT,
          payload: this.connectionParams,
        }));
      });

      this.ws.on("message", (raw) => {
        const msg = JSON.parse(String(raw));
        switch (msg.type) {
          case GQL_CONNECTION_ACK:
            resolve();
            break;
          case GQL_NEXT: {
            const handler = this.handlers.get(msg.id);
            if (handler) handler(msg.payload);
            break;
          }
          case GQL_ERROR:
            console.error("[waker] subscription error:", JSON.stringify(msg.payload));
            break;
          case GQL_COMPLETE:
            this.handlers.delete(msg.id);
            break;
          case GQL_PING:
            this.ws.send(JSON.stringify({ type: GQL_PONG }));
            break;
        }
      });

      this.ws.on("error", (e) => {
        console.error("[waker] ws error:", e.message);
        reject(e);
      });

      this.ws.on("close", () => {
        if (this._closed) return;
        console.log("[waker] ws closed — reconnecting in 5s...");
        setTimeout(() => this._reconnect(), 5000);
      });
    });
  }

  async _reconnect() {
    if (this._closed) return;
    try {
      await this.connect();
      console.log("[waker] reconnected");
      // Re-subscribe
      for (const { query, variables, handler } of this._subscriptions) {
        this._doSubscribe(query, variables, handler);
      }
    } catch (e) {
      console.error("[waker] reconnect failed:", e.message, "— retrying in 10s");
      setTimeout(() => this._reconnect(), 10000);
    }
  }

  _doSubscribe(query, variables, handler) {
    const id = String(this.nextId++);
    this.handlers.set(id, handler);
    this.ws.send(JSON.stringify({
      id,
      type: GQL_SUBSCRIBE,
      payload: { query, variables },
    }));
    return id;
  }

  subscribe(query, variables, handler) {
    this._subscriptions.push({ query, variables, handler });
    return this._doSubscribe(query, variables, handler);
  }

  close() {
    this._closed = true;
    this.ws.close();
  }
}

// ── Link matching ──────────────────────────────────────────────────

function matchLink(sub, link) {
  const { source, predicate, target } = link.data;
  if (sub.matchPredicate && predicate !== sub.matchPredicate) return false;
  if (sub.matchSource && !source.includes(sub.matchSource)) return false;
  if (sub.matchTarget && !target.includes(sub.matchTarget)) return false;
  // At least one filter must be set
  if (!sub.matchPredicate && !sub.matchSource && !sub.matchTarget) return false;
  return true;
}

// ── Wake poster ────────────────────────────────────────────────────

function postWake(config, sub, link) {
  const body = JSON.stringify({
    text: `[AD4M waker] subscription=${sub.id} | ${link.data.predicate} → ${link.data.target.substring(0, 80)} (by ${link.author.substring(0, 30)})`,
    mode: "now",
  });

  const url = new URL(config.wakeUrl);
  const mod = url.protocol === "https:" ? https : http;

  const req = mod.request({
    hostname: url.hostname,
    port: url.port,
    path: url.pathname,
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${config.wakeToken}`,
      "Content-Length": Buffer.byteLength(body),
    },
  }, (res) => {
    let data = "";
    res.on("data", (c) => data += c);
    res.on("end", () => {
      if (res.statusCode >= 400) {
        console.error(`[waker] wake POST failed: ${res.statusCode} ${data}`);
      } else {
        console.log(`[waker] wake sent for subscription ${sub.id}`);
      }
    });
  });
  req.on("error", (e) => console.error("[waker] wake POST error:", e.message));
  req.write(body);
  req.end();
}

// ── Main waker logic ───────────────────────────────────────────────

async function startWaker(config) {
  const debounceMs = config.debounceMs || 2000;

  console.log(`[waker] connecting to ${config.executorUrl}`);
  const client = new GraphQLWSClient(
    config.executorUrl,
    config.token ? { headers: { authorization: config.token } } : {},
  );
  await client.connect();
  console.log("[waker] connected");

  // Group subscriptions by perspective
  const byPerspective = new Map();
  for (const sub of config.subscriptions) {
    const list = byPerspective.get(sub.perspective) || [];
    list.push(sub);
    byPerspective.set(sub.perspective, list);
  }

  // Per-subscription debounce state
  const debounceTimers = new Map();

  function onLink(link, subs) {
    for (const sub of subs) {
      if (!matchLink(sub, link)) continue;

      console.log(`[waker] matched sub=${sub.id}: ${link.data.predicate} → ${link.data.target.substring(0, 60)}`);

      // Debounce per subscription
      const existing = debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);
      debounceTimers.set(sub.id, setTimeout(() => {
        postWake(config, sub, link);
        debounceTimers.delete(sub.id);
      }, debounceMs));
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
      console.log(`[waker]   - ${sub.id}: predicate=${sub.matchPredicate || "*"} source=${sub.matchSource?.substring(0, 40) || "*"} target=${sub.matchTarget || "*"}`);
    }
  }

  return { close: () => { for (const t of debounceTimers.values()) clearTimeout(t); client.close(); } };
}

// ── CLI ────────────────────────────────────────────────────────────

function parseArgs(argv) {
  const args = {};
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg.startsWith("--")) {
      const key = arg.slice(2);
      const next = argv[i + 1];
      if (next && !next.startsWith("--")) {
        args[key] = next;
        i++;
      } else {
        args[key] = true;
      }
    }
  }
  return args;
}

async function main() {
  const args = parseArgs(process.argv.slice(2));

  if (args.help) {
    console.log(`
AD4M Waker — watches perspectives for link changes and wakes OpenClaw agent

Usage:
  node ad4m-waker.js --config <path>
  node ad4m-waker.js --executor-url <ws-url> --wake-url <url> --wake-token <token> \\
    --perspective <uuid> --id <sub-id> --match-predicate <pred> [--match-source <src>]

Config file format (JSON):
  {
    "executorUrl": "ws://localhost:12100/graphql",
    "token": "optional-ad4m-token",
    "wakeUrl": "http://localhost:18789/hooks/wake",
    "wakeToken": "your-wake-token",
    "debounceMs": 2000,
    "subscriptions": [
      {
        "id": "unique-sub-id",
        "perspective": "perspective-uuid",
        "matchPredicate": "ad4m://has_child",
        "matchSource": "literal://string:channel-id"
      }
    ]
  }
`);
    process.exit(0);
  }

  let config;

  if (args.config) {
    const configPath = path.resolve(args.config);
    config = JSON.parse(fs.readFileSync(configPath, "utf-8"));
    console.log(`[waker] loaded config from ${configPath}`);
  } else {
    // Build config from CLI args
    if (!args["executor-url"] || !args["wake-url"] || !args["wake-token"] || !args.perspective || !args.id) {
      console.error("Error: provide --config or all of: --executor-url, --wake-url, --wake-token, --perspective, --id, --match-predicate");
      process.exit(1);
    }
    config = {
      executorUrl: args["executor-url"],
      token: args.token,
      wakeUrl: args["wake-url"],
      wakeToken: args["wake-token"],
      debounceMs: parseInt(args["debounce-ms"] || "2000"),
      subscriptions: [{
        id: args.id,
        perspective: args.perspective,
        matchPredicate: args["match-predicate"],
        matchSource: args["match-source"],
        matchTarget: args["match-target"],
      }],
    };
  }

  if (!config.subscriptions?.length) {
    console.error("Error: no subscriptions configured");
    process.exit(1);
  }

  const waker = await startWaker(config);

  process.on("SIGINT", () => { console.log("\n[waker] shutting down..."); waker.close(); process.exit(0); });
  process.on("SIGTERM", () => { waker.close(); process.exit(0); });
}

// Only run CLI when executed directly (not when required as module)
if (require.main === module) {
  main().catch((err) => {
    console.error("[waker] fatal:", err);
    process.exit(1);
  });
}

module.exports = { startWaker, matchLink };
