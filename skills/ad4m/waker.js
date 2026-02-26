#!/usr/bin/env node
/**
 * AD4M Waker — Model-level change watcher for OpenClaw agents
 * 
 * Connects to an AD4M executor via WebSocket, subscribes to SurrealDB queries
 * on perspectives, and wakes the OpenClaw agent when meaningful changes occur.
 * 
 * Usage:
 *   node waker.js --url ws://localhost:12100/graphql --token <admin-credential>
 *                 --perspective <uuid> --model Message --channel <channel-id>
 *                 --openclaw-wake-url http://localhost:3000/wake
 */

import { createClient } from 'graphql-ws';
import { ApolloClient, InMemoryCache, ApolloLink, Observable } from '@apollo/client/core/index.js';
import { Ad4mClient } from '@coasys/ad4m';
import { print } from 'graphql';
import WebSocket from 'ws';
import { readFileSync, writeFileSync, existsSync } from 'fs';
import { execSync } from 'child_process';

// --- Config ---
const CONFIG_PATH = process.env.AD4M_WAKER_CONFIG || './waker-config.json';

function loadConfig() {
  const defaults = {
    url: 'ws://localhost:12100/graphql',
    token: '',
    perspectiveUuid: '',
    subscriptions: [],  // Array of { model, source, query, label }
    openclawWakeFile: '/tmp/ad4m-waker-events.jsonl',
    gatewayUrl: null,
    gatewayToken: null,
    pollInterval: null, // null = use subscriptions, number = fallback poll ms
  };

  // CLI args override config file
  const args = parseArgs(process.argv.slice(2));
  
  let fileConfig = {};
  if (existsSync(CONFIG_PATH)) {
    try {
      fileConfig = JSON.parse(readFileSync(CONFIG_PATH, 'utf-8'));
    } catch (e) {
      console.error(`[waker] Warning: could not parse ${CONFIG_PATH}:`, e.message);
    }
  }

  return { ...defaults, ...fileConfig, ...args };
}

function parseArgs(argv) {
  const result = {};
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === '--url') result.url = argv[++i];
    else if (arg === '--token') result.token = argv[++i];
    else if (arg === '--perspective') result.perspectiveUuid = argv[++i];
    else if (arg === '--wake-file') result.openclawWakeFile = argv[++i];
    else if (arg === '--config') { /* CONFIG_PATH already handled */ }
  }
  return result;
}

// --- Apollo + graphql-ws link for Node.js ---
function createGraphQLWSLink(wsClient) {
  return new ApolloLink(operation => {
    return new Observable(observer => {
      const dispose = wsClient.subscribe(
        { query: print(operation.query), variables: operation.variables },
        {
          next: observer.next.bind(observer),
          error: observer.error.bind(observer),
          complete: observer.complete.bind(observer),
        }
      );
      return dispose;
    });
  });
}

function createAd4mClient(url, token) {
  const wsClient = createClient({
    url,
    webSocketImpl: WebSocket,
    connectionParams: { headers: { authorization: token } },
    on: {
      connected: () => console.log('[waker] WebSocket connected to', url),
      error: (err) => console.error('[waker] WebSocket error:', err.message || err),
      closed: (event) => {
        console.log('[waker] WebSocket closed, code:', event?.code);
        // graphql-ws handles reconnection automatically
      },
    },
    retryAttempts: Infinity,
    retryWait: async (retries) => {
      const delay = Math.min(1000 * Math.pow(2, retries), 30000);
      console.log(`[waker] Reconnecting in ${delay}ms (attempt ${retries + 1})...`);
      await new Promise(r => setTimeout(r, delay));
    },
  });

  const apolloClient = new ApolloClient({
    link: createGraphQLWSLink(wsClient),
    cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
    defaultOptions: {
      watchQuery: { fetchPolicy: 'no-cache' },
      query: { fetchPolicy: 'no-cache' },
      mutate: { fetchPolicy: 'no-cache' },
    },
  });

  const ad4mClient = new Ad4mClient(apolloClient, true);
  return { ad4mClient, wsClient, apolloClient };
}

// --- Wake mechanism ---
function emitWakeEvent(config, event) {
  const payload = {
    timestamp: new Date().toISOString(),
    ...event,
  };
  
  console.log('[waker] Change detected:', JSON.stringify(payload).substring(0, 200));

  // Write to wake file (OpenClaw cron reads this)
  if (config.openclawWakeFile) {
    try {
      const line = JSON.stringify(payload) + '\n';
      writeFileSync(config.openclawWakeFile, line, { flag: 'a' });
    } catch (e) {
      console.error('[waker] Failed to write wake file:', e.message);
    }
  }

  // Call OpenClaw gateway wake endpoint
  if (config.gatewayUrl && config.gatewayToken) {
    try {
      const wakeText = formatWakeText(event);
      const wakePayload = JSON.stringify({ text: wakeText, mode: "now" });
      execSync(`curl -s -X POST "${config.gatewayUrl}/api/cron/wake" -H "Content-Type: application/json" -H "Authorization: Bearer ${config.gatewayToken}" -d '${wakePayload.replace(/'/g, "'\\''")}'`, { timeout: 5000 });
      console.log('[waker] OpenClaw wake sent successfully');
    } catch (e) {
      console.error('[waker] Failed to call OpenClaw wake:', e.message);
    }
  }
}

// --- Model subscription helpers ---

/**
 * Build a SurrealDB query for a Flux Message model in a specific channel
 */
function buildMessageQuery(channelId) {
  // Simple query: get recent flux://body links ordered by timestamp
  // AD4M's SurrealDB stores links as `link` records with source/target/predicate/timestamp
  // Subqueries may not work reliably, so we query all message bodies and filter client-side
  return `SELECT * FROM link WHERE predicate = 'flux://body' ORDER BY timestamp DESC LIMIT 20`;
}

/**
 * Subscribe to model changes using PerspectiveProxy.subscribeSurrealDB
 */
async function subscribeToModel(ad4mClient, config, sub) {
  const perspective = await ad4mClient.perspective.byUUID(config.perspectiveUuid);
  if (!perspective) {
    console.error(`[waker] Perspective ${config.perspectiveUuid} not found`);
    return null;
  }

  const query = sub.query || buildMessageQuery(sub.source);
  console.log(`[waker] Subscribing to "${sub.label || sub.model}" on perspective ${config.perspectiveUuid.substring(0, 8)}...`);
  console.log(`[waker] Query: ${query.trim().substring(0, 120)}...`);

  const subscription = await perspective.subscribeSurrealDB(query);
  
  console.log(`[waker] Subscription ID: ${subscription.id}`);
  console.log(`[waker] Initial result type: ${typeof subscription.result}, isArray: ${Array.isArray(subscription.result)}, length: ${Array.isArray(subscription.result) ? subscription.result.length : 'N/A'}`);
  
  let lastResultHash = null;
  let callbackCount = 0;

  subscription.onResult((result) => {
    callbackCount++;
    // Hash the result to detect actual changes
    const resultStr = JSON.stringify(result);
    const hash = simpleHash(resultStr);
    
    console.log(`[waker] onResult #${callbackCount}: hash=${hash}, prevHash=${lastResultHash}, changed=${lastResultHash !== null && hash !== lastResultHash}, resultType=${typeof result}`);
    
    if (lastResultHash !== null && hash !== lastResultHash) {
      // Result changed — parse and emit wake event
      emitWakeEvent(config, {
        type: 'model_change',
        model: sub.model || 'unknown',
        label: sub.label || sub.model,
        source: sub.source,
        perspectiveUuid: config.perspectiveUuid,
        resultCount: Array.isArray(result) ? result.length : 0,
        latestItems: Array.isArray(result) ? result.slice(0, 3) : result,
      });
    }
    lastResultHash = hash;
  });

  console.log(`[waker] ✓ Subscribed to "${sub.label || sub.model}"`);
  return subscription;
}

function formatWakeText(event) {
  let text = `[AD4M Flux] New activity in "${event.label || 'channel'}"`;
  if (event.latestItems && Array.isArray(event.latestItems) && event.latestItems.length > 0) {
    const latest = event.latestItems[0];
    // Try to decode the message body
    if (latest.target) {
      try {
        let body = latest.target;
        if (body.startsWith('literal://json:')) {
          const decoded = JSON.parse(decodeURIComponent(body.replace('literal://json:', '')));
          body = decoded.data || decoded;
        } else if (body.startsWith('literal://string:')) {
          body = body.replace('literal://string:', '');
        }
        // Strip HTML tags
        body = body.replace(/<[^>]*>/g, '').trim();
        const author = latest.author?.substring(24, 34) || 'unknown';
        text += ` — "${body.substring(0, 200)}" (from ...${author})`;
      } catch (e) {
        // ignore parse errors
      }
    }
  }
  return text;
}

function simpleHash(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash |= 0;
  }
  return hash;
}

// --- Main ---
async function main() {
  const config = loadConfig();
  
  console.log('[waker] AD4M Waker starting...');
  console.log(`[waker] Executor: ${config.url}`);
  console.log(`[waker] Perspective: ${config.perspectiveUuid || '(none yet)'}`);
  console.log(`[waker] Wake file: ${config.openclawWakeFile}`);

  if (!config.url || !config.token) {
    console.error('[waker] Error: --url and --token are required');
    process.exit(1);
  }

  // Connect
  const { ad4mClient, wsClient } = createAd4mClient(config.url, config.token);
  
  // Verify connection
  try {
    const status = await ad4mClient.agent.status();
    console.log(`[waker] Connected as: ${status.did?.substring(0, 30)}...`);
  } catch (e) {
    console.error('[waker] Failed to connect:', e.message);
    process.exit(1);
  }

  // Subscribe to configured models
  const activeSubscriptions = [];
  
  if (config.subscriptions.length > 0) {
    for (const sub of config.subscriptions) {
      try {
        const subscription = await subscribeToModel(ad4mClient, config, sub);
        if (subscription) activeSubscriptions.push(subscription);
      } catch (e) {
        console.error(`[waker] Failed to subscribe to ${sub.label || sub.model}:`, e.message);
      }
    }
  } else if (config.perspectiveUuid) {
    console.log('[waker] No subscriptions configured. Waiting for config update...');
  }

  console.log(`[waker] Running with ${activeSubscriptions.length} active subscription(s). Press Ctrl+C to stop.`);

  // Graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n[waker] Shutting down...');
    activeSubscriptions.forEach(s => s.dispose());
    wsClient.dispose();
    process.exit(0);
  });

  process.on('SIGTERM', () => {
    console.log('\n[waker] Terminated.');
    activeSubscriptions.forEach(s => s.dispose());
    wsClient.dispose();
    process.exit(0);
  });

  // Keep alive
  setInterval(() => {}, 60000);
}

main().catch(e => {
  console.error('[waker] Fatal error:', e);
  process.exit(1);
});
