#!/usr/bin/env tsx
/**
 * AD4M Performance Profiler
 * 
 * Simulates a Flux-like workload against running executors and measures
 * every layer of the stack: raw queries, GraphQL round-trips, N+1 patterns,
 * batch queries, hydration, and expression resolution.
 * 
 * Usage:
 *   npx tsx src/profiling/index.ts --port 12000 --admin-credential test-admin
 * 
 * Requires a running ad4m-executor with both sparql and baseline features.
 */

import { parseArgs } from 'node:util';

const { values } = parseArgs({
  options: {
    port: { type: 'string', default: '12000' },
    'admin-credential': { type: 'string', default: 'test-admin' },
    iterations: { type: 'string', default: '20' },
    'community-channels': { type: 'string', default: '5' },
    'messages-per-channel': { type: 'string', default: '100' },
    help: { type: 'boolean', short: 'h', default: false },
  },
  strict: true,
});

if (values.help) {
  console.log(`
AD4M Performance Profiler

Options:
  --port                   Executor GQL port (default: 12000)
  --admin-credential       Admin credential (default: test-admin)
  --iterations             Iterations per measurement (default: 20)
  --community-channels     Channels to create (default: 5)
  --messages-per-channel   Messages per channel (default: 100)
  `);
  process.exit(0);
}

const PORT = parseInt(values.port!, 10);
const ADMIN = values['admin-credential']!;
const ITERATIONS = parseInt(values.iterations!, 10);
const NUM_CHANNELS = parseInt(values['community-channels']!, 10);
const MSGS_PER_CHANNEL = parseInt(values['messages-per-channel']!, 10);
const GQL_URL = `http://127.0.0.1:${PORT}/graphql`;

// ─── GraphQL Client ────────────────────────────────────────────────

let jwt = '';

async function gql(query: string, variables: Record<string, unknown> = {}): Promise<any> {
  const headers: Record<string, string> = { 'Content-Type': 'application/json' };
  if (jwt) headers['Authorization'] = jwt;

  const res = await fetch(GQL_URL, {
    method: 'POST',
    headers,
    body: JSON.stringify({ query, variables }),
    signal: AbortSignal.timeout(30_000),
  });
  const json = await res.json();
  if (json.errors) throw new Error(`GraphQL: ${json.errors[0].message}`);
  return json.data;
}

// ─── Timer Utility ─────────────────────────────────────────────────

interface TimingResult {
  name: string;
  iterations: number;
  totalMs: number;
  avgMs: number;
  minMs: number;
  maxMs: number;
  p50Ms: number;
  p95Ms: number;
  p99Ms: number;
}

async function measure(name: string, iterations: number, fn: () => Promise<void>): Promise<TimingResult> {
  const times: number[] = [];
  // Warmup
  for (let i = 0; i < Math.min(3, iterations); i++) await fn();

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    times.push(performance.now() - start);
  }
  times.sort((a, b) => a - b);
  const totalMs = times.reduce((a, b) => a + b, 0);
  return {
    name,
    iterations,
    totalMs,
    avgMs: totalMs / iterations,
    minMs: times[0],
    maxMs: times[times.length - 1],
    p50Ms: times[Math.floor(times.length * 0.5)],
    p95Ms: times[Math.floor(times.length * 0.95)],
    p99Ms: times[Math.floor(times.length * 0.99)],
  };
}

// ─── Setup ─────────────────────────────────────────────────────────

async function authenticate(): Promise<void> {
  // Use admin credential for initial auth requests
  jwt = ADMIN;
  
  // Request capability
  const reqData = await gql(`mutation {
    agentRequestCapability(
      authInfo: {
        appName: "profiler",
        appDesc: "performance profiler",
        appDomain: "localhost",
        capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }]
      }
    )
  }`);
  const requestId = reqData.agentRequestCapability;

  // Permit the capability request (requires AuthInfoExtended JSON)
  const authExtended = JSON.stringify({
    requestId: requestId,
    auth: {
      appName: "profiler",
      appDesc: "performance profiler",
      appDomain: "localhost",
      appUrl: null,
      appIconPath: null,
      capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
    }
  });
  const permitData = await gql(`mutation { agentPermitCapability(auth: ${JSON.stringify(authExtended)}) }`);
  const rand = permitData.agentPermitCapability;

  // Generate JWT
  const jwtData = await gql(`mutation {
    agentGenerateJwt(requestId: "${requestId}", rand: "${rand}")
  }`);
  jwt = jwtData.agentGenerateJwt;
  console.log('✓ Authenticated');
}

async function createPerspective(name: string): Promise<string> {
  const data = await gql(`mutation { perspectiveAdd(name: "${name}") { uuid } }`);
  return data.perspectiveAdd.uuid;
}

// ─── Seed Data ─────────────────────────────────────────────────────

interface SeedResult {
  perspectiveUuid: string;
  channelIds: string[];
  messageIds: string[][];
  totalLinks: number;
  seedTimeMs: number;
}

async function seedFluxLikeData(): Promise<SeedResult> {
  const start = performance.now();
  const uuid = await createPerspective('perf-test-' + Date.now());
  console.log(`  Perspective: ${uuid}`);

  const channelIds: string[] = [];
  const messageIds: string[][] = [];
  let totalLinks = 0;

  // Create community root
  const communityBase = `literal:string:community-${Date.now()}`;
  await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
    source: "${communityBase}",
    predicate: "flux://entry_type",
    target: "flux://has_community"
  }) { author } }`);
  totalLinks++;

  await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
    source: "${communityBase}",
    predicate: "rdf://name",
    target: "literal:string:Test Community"
  }) { author } }`);
  totalLinks++;

  // Create channels
  for (let c = 0; c < NUM_CHANNELS; c++) {
    const channelBase = `literal:string:channel-${c}-${Date.now()}`;
    channelIds.push(channelBase);

    // Flag link
    await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
      source: "${channelBase}",
      predicate: "flux://entry_type",
      target: "flux://has_channel"
    }) { author } }`);
    totalLinks++;

    // Name
    await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
      source: "${channelBase}",
      predicate: "rdf://name",
      target: "literal:string:Channel ${c}"
    }) { author } }`);
    totalLinks++;

    // Community → channel
    await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
      source: "${communityBase}",
      predicate: "flux://has_channel",
      target: "${channelBase}"
    }) { author } }`);
    totalLinks++;

    // Create messages
    const msgs: string[] = [];
    for (let m = 0; m < MSGS_PER_CHANNEL; m++) {
      const msgBase = `literal:string:msg-${c}-${m}-${Date.now()}`;
      msgs.push(msgBase);

      // Flag
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
        source: "${msgBase}",
        predicate: "flux://entry_type",
        target: "flux://has_message"
      }) { author } }`);
      totalLinks++;

      // Content
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
        source: "${msgBase}",
        predicate: "msg://content",
        target: "literal:string:Message ${m} in channel ${c} - Lorem ipsum dolor sit amet"
      }) { author } }`);
      totalLinks++;

      // Channel → message
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
        source: "${channelBase}",
        predicate: "flux://has_message",
        target: "${msgBase}"
      }) { author } }`);
      totalLinks++;

      // Timestamp (as link)
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
        source: "${msgBase}",
        predicate: "msg://timestamp",
        target: "literal:number:${Date.now() - (MSGS_PER_CHANNEL - m) * 1000}"
      }) { author } }`);
      totalLinks++;
    }
    messageIds.push(msgs);
    console.log(`  Channel ${c}: ${msgs.length} messages`);
  }

  const seedTimeMs = performance.now() - start;
  console.log(`  Total links: ${totalLinks}, seeded in ${(seedTimeMs / 1000).toFixed(1)}s`);
  return { perspectiveUuid: uuid, channelIds, messageIds, totalLinks, seedTimeMs };
}

// ─── Phase 1: Raw Query Engine Comparison ──────────────────────────

async function phase1RawQueries(seed: SeedResult): Promise<TimingResult[]> {
  console.log('\n═══ PHASE 1: Raw Query Engine Performance ═══\n');
  const uuid = seed.perspectiveUuid;
  const results: TimingResult[] = [];

  // 1a. Link query (built-in — baseline)
  try {
    results.push(await measure('LinkQuery: all links', ITERATIONS, async () => {
      await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "" }) { data { source predicate target } } }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  LinkQuery: all links: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1b. Link query with predicate filter
  try {
    results.push(await measure('LinkQuery: by predicate (flux://has_message)', ITERATIONS, async () => {
      await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { predicate: "flux://has_message" }) { data { source predicate target } } }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  LinkQuery: by predicate (flux://has_message): SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1c. SPARQL: SELECT all triples
  try {
    results.push(await measure('SPARQL: SELECT * WHERE { ?s ?p ?o }', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?s ?p ?o WHERE { ?s ?p ?o }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL: SELECT * WHERE { ?s ?p ?o }: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1d. SPARQL: Filtered — messages in a specific channel
  const ch0 = seed.channelIds[0].replace(/"/g, '\\"');
  try {
    results.push(await measure('SPARQL: messages in channel (filtered)', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?msg ?content WHERE { <${ch0}> <flux://has_message> ?msg . ?msg <msg://content> ?content }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL: messages in channel (filtered): SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1e. SPARQL: JOIN — messages with content and timestamp, ordered
  try {
    results.push(await measure('SPARQL: JOIN messages + content + timestamp + ORDER BY', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?msg ?content ?ts WHERE { <${ch0}> <flux://has_message> ?msg . ?msg <msg://content> ?content . ?msg <msg://timestamp> ?ts } ORDER BY DESC(?ts) LIMIT 50") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL: JOIN messages + content + timestamp + ORDER BY: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1f. SPARQL: Complex JOIN — all channels with message counts
  try {
    results.push(await measure('SPARQL: channels with message counts (GROUP BY)', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?ch (COUNT(?msg) as ?count) WHERE { ?ch <flux://entry_type> <flux://has_channel> . ?ch <flux://has_message> ?msg } GROUP BY ?ch") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL: channels with message counts (GROUP BY): SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 1g. baseline: equivalent filtered query (if available)
  try {
    results.push(await measure('baseline: messages by predicate filter', ITERATIONS, async () => {
      await gql(`query { perspectiveQueryBaselineDb(uuid: "${uuid}", query: "SELECT * FROM link WHERE predicate = 'flux://has_message' AND source = '${ch0}'") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  baseline: SKIPPED (${e.message.slice(0, 60)})`);
  }

  // 1h. baseline: multi-table join equivalent (has to be done as separate queries)
  try {
    results.push(await measure('baseline: messages + content (2 queries)', ITERATIONS, async () => {
      // First get message IDs
      const msgResult = await gql(`query { perspectiveQueryBaselineDb(uuid: "${uuid}", query: "SELECT target FROM link WHERE predicate = 'flux://has_message' AND source = '${ch0}'") }`);
      // Then get content for each — but baseline can't do JOINs, so we do a WHERE IN
      const parsed = JSON.parse(msgResult.perspectiveQueryBaselineDb);
      const targets = (parsed as any[]).map((r: any) => `'${r.target}'`).join(',');
      if (targets) {
        await gql(`query { perspectiveQueryBaselineDb(uuid: "${uuid}", query: "SELECT * FROM link WHERE predicate = 'msg://content' AND source IN [${targets}]") }`);
      }
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  baseline multi-query: SKIPPED (${e.message.slice(0, 60)})`);
  }

  return results;
}

// ─── Phase 2: N+1 vs Batch Query Patterns ──────────────────────────

async function phase2QueryPatterns(seed: SeedResult): Promise<TimingResult[]> {
  console.log('\n═══ PHASE 2: N+1 vs Batch Query Patterns ═══\n');
  const uuid = seed.perspectiveUuid;
  const results: TimingResult[] = [];

  // 2a. N+1 pattern: load community, then each channel, then each message's content
  try {
    results.push(await measure('N+1: community → channels → messages → content', Math.min(ITERATIONS, 5), async () => {
      // Step 1: Get community
      await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { predicate: "flux://entry_type", target: "flux://has_community" }) { data { source } } }`);
  
      // Step 2: Get channels
      const channelResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { predicate: "flux://has_channel" }) { data { source target } } }`);
      const channels = channelResult.perspectiveQueryLinks;
  
      // Step 3: For each channel, get messages
      for (const ch of channels) {
        const msgResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${ch.data.target}", predicate: "flux://has_message" }) { data { target } } }`);
        const msgs = msgResult.perspectiveQueryLinks;
  
        // Step 4: For each message, get content
        for (const msg of msgs) {
          await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${msg.data.target}", predicate: "msg://content" }) { data { target } } }`);
        }
      }
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  N+1: community → channels → messages → content: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 2b. Batch via SPARQL: single query gets everything
  try {
    results.push(await measure('SPARQL batch: all channels + messages + content in 1 query', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?ch ?chName ?msg ?content ?ts WHERE { ?ch <flux://entry_type> <flux://has_channel> . ?ch <rdf://name> ?chName . ?ch <flux://has_message> ?msg . ?msg <msg://content> ?content . ?msg <msg://timestamp> ?ts } ORDER BY ?ch DESC(?ts)") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL batch: all channels + messages + content in 1 query: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 2c. Intermediate: batch per-channel (1 query per channel, not per message)
  try {
    results.push(await measure('Per-channel SPARQL: 1 query per channel', Math.min(ITERATIONS, 10), async () => {
      for (const ch of seed.channelIds) {
        await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?msg ?content ?ts WHERE { <${ch}> <flux://has_message> ?msg . ?msg <msg://content> ?content . ?msg <msg://timestamp> ?ts } ORDER BY DESC(?ts) LIMIT 50") }`);
      }
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  Per-channel SPARQL: 1 query per channel: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 2d. GraphQL round-trip overhead (empty query)
  try {
    results.push(await measure('GraphQL round-trip: empty link query', ITERATIONS * 5, async () => {
      await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "nonexistent://nothing" }) { data { source } } }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  GraphQL round-trip: empty link query: SKIPPED (${e.message.slice(0, 80)})`);
  }

  return results;
}

// ─── Phase 3: Ad4mModel Simulation ─────────────────────────────────

async function phase3ModelSimulation(seed: SeedResult): Promise<TimingResult[]> {
  console.log('\n═══ PHASE 3: Ad4mModel Hydration Simulation ═══\n');
  const uuid = seed.perspectiveUuid;
  const results: TimingResult[] = [];

  // 3a. Simulate Ad4mModel.findAll() — query links, then hydrate properties
  try {
    results.push(await measure('Model findAll simulation: query + hydrate messages', Math.min(ITERATIONS, 10), async () => {
      // Step 1: Find all message base expressions (like Ad4mModel does)
      const flagResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { predicate: "flux://entry_type", target: "flux://has_message" }) { data { source } } }`);
      const bases = flagResult.perspectiveQueryLinks.map((l: any) => l.data.source);
  
      // Step 2: For each base, get all properties (like hydration does)
      for (const base of bases) {
        await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${base}" }) { data { source predicate target } } }`);
      }
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  Model findAll simulation: query + hydrate messages: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 3b. SPARQL equivalent — single query returns all message data
  try {
    results.push(await measure('SPARQL findAll equivalent: all messages + properties', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?base ?pred ?val WHERE { ?base <flux://entry_type> <flux://has_message> . ?base ?pred ?val }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL findAll equivalent: all messages + properties: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 3c. JS post-processing simulation: fetch all, then sort and limit in JS
  try {
    results.push(await measure('Fetch all + JS sort/limit (current approach)', ITERATIONS, async () => {
      const raw = await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?msg ?ts WHERE { ?msg <flux://entry_type> <flux://has_message> . ?msg <msg://timestamp> ?ts }") }`);
      const parsed = JSON.parse(raw.perspectiveQuerySparql);
      // Simulate JS post-processing
      const sorted = (parsed as any[]).sort((a: any, b: any) => {
        const aTs = parseFloat(a.ts?.value || '0');
        const bTs = parseFloat(b.ts?.value || '0');
        return bTs - aTs;
      });
      const _limited = sorted.slice(0, 50);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  Fetch all + JS sort/limit (current approach): SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 3d. SPARQL with ORDER BY + LIMIT (server-side)
  try {
    results.push(await measure('SPARQL ORDER BY + LIMIT (server-side)', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?msg ?ts WHERE { ?msg <flux://entry_type> <flux://has_message> . ?msg <msg://timestamp> ?ts } ORDER BY DESC(?ts) LIMIT 50") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL ORDER BY + LIMIT (server-side): SKIPPED (${e.message.slice(0, 80)})`);
  }

  return results;
}

// ─── Phase 4: Scalability ──────────────────────────────────────────

async function phase4Scale(seed: SeedResult): Promise<TimingResult[]> {
  console.log('\n═══ PHASE 4: Scalability & Optimisation Opportunities ═══\n');
  const uuid = seed.perspectiveUuid;
  const results: TimingResult[] = [];

  // 4a. CONSTRUCT query — fetch entire message subgraph
  try {
    results.push(await measure('SPARQL CONSTRUCT: entire message subgraph', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "CONSTRUCT { ?msg ?p ?o } WHERE { ?msg <flux://entry_type> <flux://has_message> . ?msg ?p ?o }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL CONSTRUCT: entire message subgraph: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 4b. ASK query — existence check (e.g. "does this channel have messages?")
  const ch0 = seed.channelIds[0];
  try {
    results.push(await measure('SPARQL ASK: channel has messages?', ITERATIONS * 2, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "ASK { <${ch0}> <flux://has_message> ?msg }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  SPARQL ASK: channel has messages?: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 4c. Link add + immediate query (write-then-read latency)
  try {
    results.push(await measure('Write-then-read: addLink → SPARQL query', Math.min(ITERATIONS, 10), async () => {
      const id = `literal:string:perf-${Date.now()}-${Math.random()}`;
      await gql(`mutation { perspectiveAddLink(uuid: "${uuid}", link: {
        source: "${id}",
        predicate: "perf://test",
        target: "literal:string:value"
      }) { author } }`);
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?o WHERE { <${id}> <perf://test> ?o }") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  Write-then-read: addLink → SPARQL query: SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 4d. Full "Flux load" simulation: everything in sequence
  try {
    results.push(await measure('FULL FLUX LOAD SIMULATION (N+1 pattern)', 3, async () => {
      // 1. Get community
      const commResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { predicate: "flux://entry_type", target: "flux://has_community" }) { data { source } } }`);
      const commBase = commResult.perspectiveQueryLinks[0]?.data.source;
      if (!commBase) return;
  
      // 2. Get community name
      await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${commBase}", predicate: "rdf://name" }) { data { target } } }`);
  
      // 3. Get channels
      const chResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${commBase}", predicate: "flux://has_channel" }) { data { target } } }`);
  
      // 4. For each channel, get name + messages
      for (const ch of chResult.perspectiveQueryLinks) {
        await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${ch.data.target}", predicate: "rdf://name" }) { data { target } } }`);
  
        const msgResult = await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${ch.data.target}", predicate: "flux://has_message" }) { data { target } } }`);
  
        // 5. For each message, get all properties
        for (const msg of msgResult.perspectiveQueryLinks) {
          await gql(`query { perspectiveQueryLinks(uuid: "${uuid}", query: { source: "${msg.data.target}" }) { data { predicate target } } }`);
        }
      }
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  FULL FLUX LOAD SIMULATION (N+1 pattern): SKIPPED (${e.message.slice(0, 80)})`);
  }

  // 4e. Optimised "Flux load" — single SPARQL query
  try {
    results.push(await measure('OPTIMISED FLUX LOAD: single SPARQL query', ITERATIONS, async () => {
      await gql(`query { perspectiveQuerySparql(uuid: "${uuid}", query: "SELECT ?ch ?chName ?msg ?content ?ts WHERE { ?comm <flux://entry_type> <flux://has_community> . ?comm <flux://has_channel> ?ch . ?ch <rdf://name> ?chName . ?ch <flux://has_message> ?msg . ?msg <msg://content> ?content . ?msg <msg://timestamp> ?ts } ORDER BY ?ch DESC(?ts)") }`);
    }));
    console.log(`  ${results.at(-1)!.name}: avg ${results.at(-1)!.avgMs.toFixed(2)}ms`);
  } catch (e: any) {
    console.log(`  OPTIMISED FLUX LOAD: single SPARQL query: SKIPPED (${e.message.slice(0, 80)})`);
  }

  return results;
}

// ─── Report ────────────────────────────────────────────────────────

function printReport(allResults: TimingResult[][], seed: SeedResult): void {
  console.log('\n' + '═'.repeat(80));
  console.log('  AD4M PERFORMANCE PROFILE REPORT');
  console.log('═'.repeat(80));
  console.log(`\nDataset: ${NUM_CHANNELS} channels × ${MSGS_PER_CHANNEL} messages = ${NUM_CHANNELS * MSGS_PER_CHANNEL} messages`);
  console.log(`Total links: ${seed.totalLinks}`);
  console.log(`Seed time: ${(seed.seedTimeMs / 1000).toFixed(1)}s`);
  console.log(`GraphQL endpoint: ${GQL_URL}\n`);

  const phases = [
    'Phase 1: Raw Query Engine',
    'Phase 2: N+1 vs Batch',
    'Phase 3: Model Hydration',
    'Phase 4: Scale & Optimisation',
  ];

  for (let i = 0; i < allResults.length; i++) {
    console.log(`\n┌─ ${phases[i]} ${'─'.repeat(60 - phases[i].length)}┐`);
    console.log(`│ ${'Name'.padEnd(55)} │ ${'Avg'.padStart(8)} │ ${'p95'.padStart(8)} │ ${'Min'.padStart(8)} │`);
    console.log(`├${'─'.repeat(57)}┼${'─'.repeat(10)}┼${'─'.repeat(10)}┼${'─'.repeat(10)}┤`);
    for (const r of allResults[i]) {
      const name = r.name.length > 55 ? r.name.slice(0, 52) + '...' : r.name;
      console.log(`│ ${name.padEnd(55)} │ ${(r.avgMs.toFixed(1) + 'ms').padStart(8)} │ ${(r.p95Ms.toFixed(1) + 'ms').padStart(8)} │ ${(r.minMs.toFixed(1) + 'ms').padStart(8)} │`);
    }
    console.log(`└${'─'.repeat(57)}┴${'─'.repeat(10)}┴${'─'.repeat(10)}┴${'─'.repeat(10)}┘`);
  }

  // Key findings
  const n1 = allResults[1]?.find(r => r.name.includes('N+1'));
  const batch = allResults[1]?.find(r => r.name.includes('SPARQL batch'));
  const fullLoad = allResults[3]?.find(r => r.name.includes('FULL FLUX'));
  const optLoad = allResults[3]?.find(r => r.name.includes('OPTIMISED'));
  const roundTrip = allResults[1]?.find(r => r.name.includes('round-trip'));

  console.log('\n┌─ KEY FINDINGS ─────────────────────────────────────────────────┐');
  if (roundTrip) {
    console.log(`│ GraphQL round-trip overhead: ${roundTrip.avgMs.toFixed(1)}ms per call`.padEnd(65) + '│');
  }
  if (n1 && batch) {
    const speedup = n1.avgMs / batch.avgMs;
    console.log(`│ N+1 → SPARQL batch speedup: ${speedup.toFixed(1)}x (${n1.avgMs.toFixed(0)}ms → ${batch.avgMs.toFixed(0)}ms)`.padEnd(65) + '│');
  }
  if (fullLoad && optLoad) {
    const speedup = fullLoad.avgMs / optLoad.avgMs;
    console.log(`│ Full Flux load → Optimised: ${speedup.toFixed(1)}x (${(fullLoad.avgMs / 1000).toFixed(2)}s → ${optLoad.avgMs.toFixed(0)}ms)`.padEnd(65) + '│');
  }
  console.log('└' + '─'.repeat(65) + '┘');

  // Write JSON results
  const jsonPath = `./results/profile-${Date.now()}.json`;
  const fs = require('node:fs');
  fs.mkdirSync('./results', { recursive: true });
  fs.writeFileSync(jsonPath, JSON.stringify({
    timestamp: new Date().toISOString(),
    config: { port: PORT, channels: NUM_CHANNELS, messagesPerChannel: MSGS_PER_CHANNEL, totalLinks: seed.totalLinks },
    phases: phases.map((name, i) => ({ name, results: allResults[i] })),
  }, null, 2));
  console.log(`\nResults written to ${jsonPath}`);
}

// ─── Main ──────────────────────────────────────────────────────────

async function main(): Promise<void> {
  console.log('AD4M Performance Profiler');
  console.log(`Target: ${GQL_URL}`);
  console.log(`Dataset: ${NUM_CHANNELS} channels × ${MSGS_PER_CHANNEL} messages\n`);

  // Authenticate first (needed for all GQL calls)
  try {
    await authenticate();
  } catch (e: any) {
    console.error(`✗ Cannot connect to executor at ${GQL_URL}: ${e.message}`);
    process.exit(1);
  }

  // Check executor is running
  try {
    const status = await gql('query { agentStatus { isInitialized isUnlocked did } }');
    if (!status.agentStatus.isUnlocked) {
      throw new Error('Agent is locked — unlock first');
    }
    console.log(`✓ Executor running, agent: ${status.agentStatus.did}`);
  } catch (e: any) {
    console.error(`✗ Cannot connect to executor at ${GQL_URL}: ${e.message}`);
    process.exit(1);
  }

  console.log('\n─── Seeding Flux-like data ───');
  const seed = await seedFluxLikeData();

  const allResults: TimingResult[][] = [];

  allResults.push(await phase1RawQueries(seed));
  allResults.push(await phase2QueryPatterns(seed));
  allResults.push(await phase3ModelSimulation(seed));
  allResults.push(await phase4Scale(seed));

  printReport(allResults, seed);

  // Cleanup
  await gql(`mutation { perspectiveRemove(uuid: "${seed.perspectiveUuid}") }`);
  console.log('\n✓ Cleaned up test perspective');
}

main().catch(e => {
  console.error('Fatal:', e);
  process.exit(1);
});
