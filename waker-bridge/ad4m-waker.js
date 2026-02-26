#!/usr/bin/env node
/**
 * AD4M Waker — Node.js
 *
 * Watches AD4M perspectives via SurrealDB query subscriptions and wakes
 * an OpenClaw agent when results change.
 *
 * Uses the same mechanism as Flux UI:
 * 1. perspectiveSubscribeSurrealQuery(uuid, query) → {subscriptionId, result}
 * 2. perspectiveQuerySubscription(subscriptionId) → live updates via GraphQL sub
 * 3. perspectiveKeepAliveSurrealQuery(uuid, subscriptionId) every 30s
 *
 * Only dependency: ws (WebSocket for Node.js)
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

const WebSocket = require("ws");
const http = require("http");
const https = require("https");
const fs = require("fs");
const path = require("path");

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
    this._closed = false;
    this._pendingReconnectSubs = [];
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
      for (const { query, variables, handler } of this._pendingReconnectSubs) {
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
    this._pendingReconnectSubs.push({ query, variables, handler });
    return this._doSubscribe(query, variables, handler);
  }

  // HTTP mutation/query (for subscribe + keepalive calls)
  async httpQuery(query, variables) {
    const httpUrl = this.url.replace("ws://", "http://").replace("wss://", "https://");
    return new Promise((resolve, reject) => {
      const body = JSON.stringify({ query, variables });
      const url = new URL(httpUrl);
      const mod = url.protocol === "https:" ? https : http;
      const headers = {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
      };
      if (this.connectionParams?.headers?.authorization) {
        headers["Authorization"] = this.connectionParams.headers.authorization;
      }
      const req = mod.request({
        hostname: url.hostname,
        port: url.port,
        path: url.pathname,
        method: "POST",
        headers,
      }, (res) => {
        let data = "";
        res.on("data", (c) => data += c);
        res.on("end", () => {
          try { resolve(JSON.parse(data)); }
          catch (e) { reject(new Error(`Invalid JSON: ${data.substring(0, 200)}`)); }
        });
      });
      req.on("error", reject);
      req.write(body);
      req.end();
    });
  }

  close() {
    this._closed = true;
    this.ws.close();
  }
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

  // Per-subscription debounce state
  const debounceTimers = new Map();
  const keepaliveTimers = [];

  for (const sub of config.subscriptions) {
    console.log(`[waker] setting up subscription ${sub.id}: query=${sub.query.substring(0, 80)}...`);

    // Step 1: Register the SurrealDB query subscription on the executor
    const initResult = await client.httpQuery(
      `mutation perspectiveSubscribeSurrealQuery($uuid: String!, $query: String!) {
        perspectiveSubscribeSurrealQuery(uuid: $uuid, query: $query) {
          subscriptionId
          result
        }
      }`,
      { uuid: sub.perspective, query: sub.query }
    );

    if (initResult.errors) {
      console.error(`[waker] failed to subscribe ${sub.id}:`, initResult.errors);
      continue;
    }

    const subResult = initResult.data.perspectiveSubscribeSurrealQuery;
    const surrealSubId = subResult.subscriptionId;
    let initialResult;
    try { initialResult = JSON.parse(subResult.result); } catch (e) { initialResult = subResult.result; }
    console.log(`[waker] subscription ${sub.id} registered (surreal ID: ${surrealSubId}), initial results: ${Array.isArray(initialResult) ? initialResult.length : '?'}`);

    // Track previous result for change detection
    let lastResultJSON = JSON.stringify(initialResult);

    // Step 2: Subscribe to perspectiveQuerySubscription for live updates
    const GQL_SUB_QUERY = `
      subscription perspectiveQuerySubscription($subscriptionId: String!) {
        perspectiveQuerySubscription(subscriptionId: $subscriptionId)
      }
    `;

    client.subscribe(GQL_SUB_QUERY, { subscriptionId: surrealSubId }, (payload) => {
      if (!payload?.data?.perspectiveQuerySubscription) return;

      let resultStr = payload.data.perspectiveQuerySubscription;

      // Strip #init# prefix (initial result echo)
      if (resultStr.startsWith("#init#")) {
        resultStr = resultStr.substring(6);
        // Skip init messages if we already have a result
        if (lastResultJSON) return;
      }

      // Parse the result
      let result;
      try { result = JSON.parse(resultStr); } catch (e) { result = resultStr; }

      const resultJSON = JSON.stringify(result);
      if (resultJSON === lastResultJSON) {
        // No actual change
        return;
      }
      lastResultJSON = resultJSON;

      const count = Array.isArray(result) ? result.length : "?";
      console.log(`[waker] ${sub.id}: query result changed (${count} items)`);

      // Debounce the wake
      const existing = debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);
      debounceTimers.set(sub.id, setTimeout(() => {
        postWake(config, sub, `query result changed (${count} items)`);
        debounceTimers.delete(sub.id);
      }, debounceMs));
    });

    // Step 3: Keepalive every 30s
    const keepalive = async () => {
      try {
        await client.httpQuery(
          `mutation perspectiveKeepAliveSurrealQuery($uuid: String!, $subscriptionId: String!) {
            perspectiveKeepAliveSurrealQuery(uuid: $uuid, subscriptionId: $subscriptionId)
          }`,
          { uuid: sub.perspective, subscriptionId: surrealSubId }
        );
      } catch (e) {
        console.error(`[waker] keepalive failed for ${sub.id}:`, e.message);
      }
    };

    const timer = setInterval(keepalive, 30000);
    keepaliveTimers.push(timer);

    console.log(`[waker] ${sub.id} fully active (query sub + keepalive)`);
  }

  return {
    close() {
      for (const t of debounceTimers.values()) clearTimeout(t);
      for (const t of keepaliveTimers) clearInterval(t);
      client.close();
    }
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
AD4M Waker — watches perspectives via SurrealDB query subscriptions

Uses the same mechanism as Flux UI:
  1. perspectiveSubscribeSurrealQuery → registers query on executor
  2. perspectiveQuerySubscription → live updates via GraphQL subscription
  3. perspectiveKeepAliveSurrealQuery → 30s keepalive

Usage:
  node ad4m-waker.js --config <path>

Config file format (JSON):
  {
    "executorUrl": "ws://localhost:12100/graphql",
    "token": "optional-ad4m-token",
    "wakeUrl": "http://localhost:18789/hooks/wake",
    "wakeToken": "your-wake-token",
    "debounceMs": 2000,
    "subscriptions": [
      {
        "id": "flux-messages",
        "perspective": "perspective-uuid",
        "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
      }
    ]
  }

The SurrealQL query determines what you're watching. The MCP subscribe_to_model
tool can generate appropriate queries for you.
`);
    process.exit(0);
  }

  let config;

  if (args.config) {
    const configPath = path.resolve(args.config);
    config = JSON.parse(fs.readFileSync(configPath, "utf-8"));
    console.log(`[waker] loaded config from ${configPath}`);
  } else {
    console.error("Error: provide --config <path>");
    process.exit(1);
  }

  if (!config.subscriptions?.length) {
    console.error("Error: no subscriptions configured");
    process.exit(1);
  }

  const waker = await startWaker(config);

  process.on("SIGINT", () => { console.log("\n[waker] shutting down..."); waker.close(); process.exit(0); });
  process.on("SIGTERM", () => { waker.close(); process.exit(0); });
}

// Only run CLI when executed directly
if (require.main === module) {
  main().catch((err) => {
    console.error("[waker] fatal:", err);
    process.exit(1);
  });
}

module.exports = { startWaker };
