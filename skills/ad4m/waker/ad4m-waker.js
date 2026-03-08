#!/usr/bin/env node
/**
 * AD4M Waker — Node.js
 *
 * Watches AD4M perspectives via QuerySubscriptionProxy (SurrealDB-backed)
 * and wakes an OpenClaw agent when query results change.
 *
 * Requires @coasys/ad4m ^0.12.0
 *
 * Usage:
 *   node ad4m-waker.js --config waker-config.json
 *
 * To generate waker subscription configs (e.g. mention-tracking queries),
 * use the AD4M MCP tool `get_mention_waker_config` or `generate_waker_query`.
 *
 * Config format:
 *   {
 *     "executorUrl": "ws://localhost:12100/graphql",
 *     "token": "ad4m-admin-credential",
 *     "wakeUrl": "http://localhost:18789/hooks/wake",
 *     "wakeToken": "openclaw-wake-token",
 *     "debounceMs": 2000,
 *     "subscriptions": [
 *       {
 *         "id": "flux-messages",
 *         "type": "channel-messages",
 *         "perspective": "your-local-perspective-uuid",
 *         "channel": "literal://string:channel-id",
 *         "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
 *       }
 *     ]
 *   }
 *
 * Subscription fields:
 *   - id:            Unique subscription identifier
 *   - type:          "mention" | "channel-messages" — determines the wake message
 *   - perspective:   Local AD4M perspective UUID (from list_perspectives)
 *   - channel:       Channel address (where to read/post messages)
 *   - query:         SurrealQL subscription query
 */

const { Ad4mClient, QuerySubscriptionProxy } = require("@coasys/ad4m");
const { ApolloClient, InMemoryCache } = require("@apollo/client/core");
const { GraphQLWsLink } = require("@apollo/client/link/subscriptions");
const { createClient } = require("graphql-ws");
const WebSocket = require("ws");
const http = require("http");
const https = require("https");
const fs = require("fs");
const path = require("path");

// ── Create Ad4mClient ──────────────────────────────────────────────

function createAd4mClient(url, token) {
  const wsClient = createClient({
    url,
    webSocketImpl: WebSocket,
    connectionParams: token ? { headers: { authorization: token } } : {},
    retryAttempts: Infinity,
    retryWait: async (retries) => {
      const delay = Math.min(1000 * Math.pow(2, retries), 30000);
      console.log(`[waker] reconnecting in ${delay}ms (attempt ${retries + 1})...`);
      await new Promise((r) => setTimeout(r, delay));
    },
  });

  const wsLink = new GraphQLWsLink(wsClient);
  const apolloClient = new ApolloClient({
    link: wsLink,
    cache: new InMemoryCache(),
    defaultOptions: {
      watchQuery: { fetchPolicy: "no-cache" },
      query: { fetchPolicy: "no-cache" },
      mutate: { fetchPolicy: "no-cache" },
    },
  });

  return new Ad4mClient(apolloClient);
}

// ── Build wake message ─────────────────────────────────────────────

function buildWakeMessage(config, sub, detail) {
  const type = sub.type || "unknown";
  const P = sub.perspective || "unknown";
  const CH = sub.channel || "unknown";
  const neighbourhood = sub.neighbourhood || "";
  const MCP = config.mcpEndpoint || "http://localhost:3001/mcp";
  const AUTH = config.adminCredential || config.token;
  const DID = config.agentDid || "unknown";

  const event = type === "mention"
    ? "You were @mentioned in an AD4M neighbourhood."
    : type === "channel-messages"
    ? "New messages in an AD4M neighbourhood."
    : `AD4M waker event (${type}).`;

  return [
    event,
    `Read the AD4M skill for instructions on how to handle this.`,
    ``,
    `MCP endpoint: ${MCP}`,
    `Auth credential: ${AUTH}`,
    `Agent DID: ${DID}`,
    `Perspective: ${P}`,
    `Channel: ${CH}`,
    neighbourhood ? `Neighbourhood: ${neighbourhood}` : null,
    `Subscription: ${sub.id}`,
    `Event type: ${type}`,
  ].filter(Boolean).join("\n");
}

// ── Wake poster ────────────────────────────────────────────────────

function postWake(config, sub, detail) {
  const message = buildWakeMessage(config, sub, detail);

  // Support both /hooks/wake (text+mode) and /hooks/agent (message+name+wakeMode)
  const isAgentHook = config.wakeUrl.includes("/hooks/agent");
  const body = JSON.stringify(
    isAgentHook
      ? { message, name: "AD4M", wakeMode: "now" }
      : { text: message, mode: "now" }
  );

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
    res.on("data", (c) => (data += c));
    res.on("end", () => {
      if (res.statusCode >= 400) {
        console.error(`[waker] wake POST failed: ${res.statusCode} ${data}`);
      } else {
        console.log(`[waker] wake sent for subscription ${sub.id} (type=${sub.type || "unknown"})`);
      }
    });
  });
  req.setTimeout(5000, () => {
    req.destroy(new Error("wake POST timed out"));
  });
  req.on("error", (e) => console.error("[waker] wake POST error:", e.message));
  req.write(body);
  req.end();
}

// ── Main waker logic ───────────────────────────────────────────────

async function startWaker(config) {
  const debounceMs = config.debounceMs || 2000;

  console.log(`[waker] connecting to ${config.executorUrl}`);
  const client = createAd4mClient(config.executorUrl, config.token);

  // Verify connection
  const status = await client.agent.status();
  console.log(`[waker] connected — agent: ${status.did.substring(0, 40)}...`);

  const debounceTimers = new Map();
  const proxies = [];

  for (const sub of config.subscriptions) {
    if (!sub.type) {
      console.warn(`[waker] WARNING: subscription "${sub.id}" is missing the "type" field. Add "type": "mention" or "type": "channel-messages" to your config.`);
    }
    console.log(`[waker] setting up SurrealDB subscription ${sub.id} (type=${sub.type || "unknown"}): ${sub.query.substring(0, 80)}...`);

    // Use QuerySubscriptionProxy directly with SurrealDB query
    const proxy = new QuerySubscriptionProxy(sub.perspective, sub.query, client.perspective, sub.params);
    await proxy.subscribe();
    await proxy.initialized;

    console.log(`[waker] ${sub.id} subscribed, initial result count: ${Array.isArray(proxy.result) ? proxy.result.length : '?'}`);

    // Track last result to skip duplicates
    let lastResultHash = null;

    proxy.onResult((result) => {
      const serialized = JSON.stringify(result);
      if (lastResultHash === serialized) {
        console.log(`[waker] ${sub.id}: duplicate result, skipping`);
        return;
      }
      lastResultHash = serialized;

      const count = Array.isArray(result) ? result.length : "?";
      console.log(`[waker] ${sub.id}: query result changed (${count} items)`);

      const existing = debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);
      debounceTimers.set(
        sub.id,
        setTimeout(() => {
          postWake(config, sub, `query result changed (${count} items)`);
          debounceTimers.delete(sub.id);
        }, debounceMs)
      );
    });

    proxies.push(proxy);
    console.log(`[waker] ${sub.id} active`);
  }

  return {
    close() {
      for (const t of debounceTimers.values()) clearTimeout(t);
      for (const p of proxies) {
        try { p.dispose(); } catch (e) { /* ignore */ }
      }
    },
  };
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
AD4M Waker — watches AD4M perspectives via QuerySubscriptionProxy (SurrealDB)
and wakes an OpenClaw agent when query results change.

Requires @coasys/ad4m ^0.12.0

Usage:
  node ad4m-waker.js --config <path>

Options:
  --config <path>   Path to waker config JSON file (required).
  --help            Show this message.

To generate subscription configs (mention tracking, model changes, etc.),
use the AD4M MCP tools:
  - get_mention_waker_config   — mention-tracking query for a neighbourhood
  - generate_waker_query       — SHACL-based query for subject class changes

Config file format:
  {
    "executorUrl": "ws://localhost:12100/graphql",
    "token": "optional-ad4m-credential",
    "wakeUrl": "http://localhost:18789/hooks/wake",
    "wakeToken": "your-openclaw-wake-token",
    "debounceMs": 2000,
    "subscriptions": [
      {
        "id": "my-subscription",
        "type": "channel-messages",
        "perspective": "your-local-perspective-uuid",
        "channel": "literal://string:channel-id",
        "query": "SELECT * FROM link WHERE ..."
      }
    ]
  }

Subscription fields:
  id             Unique identifier for this subscription
  type           "mention" or "channel-messages" — determines wake message
  perspective    Local AD4M perspective UUID (from list_perspectives)
  channel        Channel address (for reading/posting messages)
  query          SurrealQL subscription query
`);
    process.exit(0);
  }

  if (!args.config) {
    console.error("Error: provide --config <path>");
    process.exit(1);
  }

  const configPath = path.resolve(args.config);
  const config = JSON.parse(fs.readFileSync(configPath, "utf-8"));
  console.log(`[waker] loaded config from ${configPath}`);

  if (!config.subscriptions?.length) {
    console.error("Error: no subscriptions configured");
    process.exit(1);
  }

  const waker = await startWaker(config);

  process.on("SIGINT", () => { console.log("\n[waker] shutting down..."); waker.close(); process.exit(0); });
  process.on("SIGTERM", () => { waker.close(); process.exit(0); });

  console.log("[waker] running — waiting for query changes...");
  setInterval(() => {}, 60000);
}

if (require.main === module) {
  main().catch((err) => {
    console.error("[waker] fatal:", err);
    process.exit(1);
  });
}

module.exports = { startWaker };
