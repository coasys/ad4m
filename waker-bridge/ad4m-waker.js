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
 *         "perspective": "perspective-uuid",
 *         "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
 *       }
 *     ]
 *   }
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

// ── Wake poster ────────────────────────────────────────────────────

function postWake(config, sub, detail) {
  const body = JSON.stringify({
    text: `[AD4M waker] subscription=${sub.id} | ${detail}`,
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
    res.on("data", (c) => (data += c));
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
  const client = createAd4mClient(config.executorUrl, config.token);

  // Verify connection
  const status = await client.agent.status();
  console.log(`[waker] connected — agent: ${status.did.substring(0, 40)}...`);

  const debounceTimers = new Map();
  const proxies = [];

  for (const sub of config.subscriptions) {
    console.log(`[waker] setting up SurrealDB subscription ${sub.id}: ${sub.query.substring(0, 80)}...`);

    // Use QuerySubscriptionProxy directly with SurrealDB query
    const proxy = new QuerySubscriptionProxy(sub.perspective, sub.query, client.perspective);
    await proxy.subscribe();
    await proxy.initialized;

    console.log(`[waker] ${sub.id} subscribed, initial result count: ${Array.isArray(proxy.result) ? proxy.result.length : '?'}`);

    proxy.onResult((result) => {
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

// ── Mention query builder ──────────────────────────────────────────

/**
 * Build SurrealQL queries that fire when messages mention the given agent.
 *
 * Flux message bodies are stored as flux://body links whose target is a
 * URL-encoded JSON literal:
 *   literal://json:{"author":"did:key:...","data":"Hey Marvin, ...","proof":{...}}
 *
 * The base58 DID key suffix and the agent's display name are URL-safe and
 * appear unencoded, so SurrealDB CONTAINS works on them directly.
 *
 * @param {string} did     - Full DID, e.g. "did:key:z6MksZb..."
 * @param {string} name    - Display name, e.g. "Marvin"
 * @returns {{ didQuery: string, nameQuery: string|null }}
 */
function buildMentionQueries(did, name) {
  // Strip "did:key:" prefix — the base58 suffix is URL-safe and unencoded in targets
  const didKey = did.startsWith("did:key:") ? did.slice("did:key:".length) : did;

  const didQuery =
    `SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS '${didKey}'`;

  const nameQuery = name
    ? `SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS '${name}'`
    : null;

  return { didQuery, nameQuery };
}

/**
 * Build waker subscription config entries for mention tracking.
 *
 * @param {string} did           - Agent DID
 * @param {string} name          - Display name (may be empty)
 * @param {string} perspectiveId - Neighbourhood perspective UUID
 * @returns {Array<{id: string, perspective: string, query: string}>}
 */
function buildMentionSubscriptions(did, name, perspectiveId) {
  const { didQuery, nameQuery } = buildMentionQueries(did, name);
  const didKey = did.startsWith("did:key:") ? did.slice("did:key:".length) : did;

  const subs = [
    {
      id: `mention-did-${didKey.slice(0, 12)}`,
      perspective: perspectiveId,
      query: didQuery,
    },
  ];

  if (nameQuery && name) {
    subs.push({
      id: `mention-name-${name.toLowerCase().replace(/\s+/g, "-")}`,
      perspective: perspectiveId,
      query: nameQuery,
    });
  }

  return subs;
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
  node ad4m-waker.js --setup-mentions --perspective <uuid> [--config <path>] [--name <name>] [--executor-url <url>] [--token <tok>]

Modes:
  --config <path>            Run the waker using the given config file (normal mode).
  --setup-mentions           Auto-generate mention subscriptions for a neighbourhood
                             and optionally append them to a config file.

Options for --setup-mentions:
  --perspective <uuid>       Neighbourhood perspective UUID to watch (required).
  --config <path>            If provided, append new subscriptions to this config file.
                             Otherwise, print the subscription JSON to stdout.
  --name <name>              Override the display name used in the query (default: profile name).
  --executor-url <url>       AD4M executor WebSocket URL (default: ws://localhost:12100/graphql).
  --token <tok>              AD4M capability token (default: none).

Config file format (JSON):
  {
    "executorUrl": "ws://localhost:12100/graphql",
    "token": "optional-ad4m-credential",
    "wakeUrl": "http://localhost:18789/hooks/wake",
    "wakeToken": "your-openclaw-wake-token",
    "debounceMs": 2000,
    "subscriptions": [
      {
        "id": "flux-messages",
        "perspective": "perspective-uuid",
        "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
      },
      {
        "id": "mention-did-z6MksZbUemc",
        "perspective": "perspective-uuid",
        "query": "SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS 'z6MksZbUemcXmxjUeez8RSAbg7jkMFwkpSRRe5nLDKwDuATB'"
      },
      {
        "id": "mention-name-marvin",
        "perspective": "perspective-uuid",
        "query": "SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS 'Marvin'"
      }
    ]
  }
`);
    process.exit(0);
  }

  // ── Setup mode: add mention subscriptions for a neighbourhood ────
  if (args["setup-mentions"]) {
    const executorUrl = args["executor-url"] || "ws://localhost:12100/graphql";
    const token = args.token || "";
    const perspectiveId = args.perspective;
    const nameOverride = args.name || null;
    const configPath = args.config ? path.resolve(args.config) : null;

    if (!perspectiveId) {
      console.error("Error: --setup-mentions requires --perspective <uuid>");
      process.exit(1);
    }

    const client = createAd4mClient(executorUrl, token);

    // Fetch agent DID and profile name
    const status = await client.agent.status();
    const did = status.did;

    let name = nameOverride;
    if (!name) {
      try {
        const agentExpr = await client.agent.me();
        const links = agentExpr?.perspective?.links || [];
        for (const link of links) {
          if (link.data.source === "flux://profile") {
            if (link.data.predicate === "sioc://has_username" && !name) {
              name = link.data.target.replace(/^literal:\/\/string:/, "");
            }
            if (link.data.predicate === "sioc://has_given_name" && !name) {
              name = link.data.target.replace(/^literal:\/\/string:/, "");
            }
          }
        }
      } catch (e) {
        console.warn("[setup] Could not fetch profile name:", e.message);
      }
    }

    const newSubs = buildMentionSubscriptions(did, name || "", perspectiveId);

    console.log(`[setup] Agent DID : ${did}`);
    console.log(`[setup] Agent name: ${name || "(none)"}`);
    console.log(`[setup] Perspective: ${perspectiveId}`);
    console.log(`[setup] Generated ${newSubs.length} subscription(s):`);
    for (const s of newSubs) {
      console.log(`  ${s.id}: ${s.query}`);
    }

    if (configPath) {
      let config = { subscriptions: [] };
      if (fs.existsSync(configPath)) {
        config = JSON.parse(fs.readFileSync(configPath, "utf-8"));
        config.subscriptions = config.subscriptions || [];
      }

      // Merge — skip duplicates by id
      const existingIds = new Set(config.subscriptions.map((s) => s.id));
      const toAdd = newSubs.filter((s) => !existingIds.has(s.id));
      config.subscriptions.push(...toAdd);

      fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
      console.log(`[setup] ${toAdd.length} subscription(s) added to ${configPath}`);
      console.log("[setup] Restart the waker to activate.");
    } else {
      console.log("\nAdd these to your waker config subscriptions array:");
      console.log(JSON.stringify(newSubs, null, 2));
    }

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

module.exports = { startWaker, buildMentionQueries, buildMentionSubscriptions };
