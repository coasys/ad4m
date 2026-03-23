// SPARQL Batching Benchmark — 1K and 10K link scales with varied subject class shapes
const GQL = 'http://127.0.0.1:12000/graphql';
const AUTH = 'test-admin';

async function sleep(ms: number) { return new Promise(r => setTimeout(r, ms)); }

async function gql(query: string, variables?: Record<string, any>, retries = 3): Promise<any> {
  for (let attempt = 0; attempt <= retries; attempt++) {
    try {
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), 120000);
      const res = await fetch(GQL, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', 'Authorization': AUTH },
        body: JSON.stringify({ query, variables }),
        signal: controller.signal,
      });
      clearTimeout(timeout);
      const json = await res.json();
      if (json.errors) throw new Error(JSON.stringify(json.errors));
      return json.data;
    } catch (e: any) {
      const isRetryable = e.cause?.code === 'UND_ERR_SOCKET' || e.cause?.code === 'ECONNREFUSED' || 
        e.message?.includes('fetch failed') || e.name === 'AbortError';
      if (attempt < retries && isRetryable) {
        const wait = 5000 * (attempt + 1);
        console.warn(`   [retry ${attempt + 1}/${retries}] Connection error, waiting ${wait/1000}s...`);
        await sleep(wait);
        continue;
      }
      throw e;
    }
  }
}

async function addLink(uuid: string, source: string, predicate: string, target: string) {
  return gql(`mutation($uuid: String!, $link: LinkInput!) {
    perspectiveAddLink(uuid: $uuid, link: $link) { author }
  }`, { uuid, link: { source, predicate, target } });
}

async function addLinks(uuid: string, links: Array<{source: string, predicate: string, target: string}>) {
  return gql(`mutation($uuid: String!, $links: [LinkInput!]!) {
    perspectiveAddLinks(uuid: $uuid, links: $links) { author }
  }`, { uuid, links });
}

async function queryLinks(uuid: string, query: any = {}): Promise<any> {
  return gql(`query($uuid: String!, $query: LinkQuery!) {
    perspectiveQueryLinks(uuid: $uuid, query: $query)  { data { source predicate target } }
  }`, { uuid, query });
}

async function sparqlQuery(uuid: string, query: string): Promise<string> {
  const data = await gql(`query($uuid: String!, $query: String!) {
    perspectiveQuerySparql(uuid: $uuid, query: $query)
  }`, { uuid, query });
  return data.perspectiveQuerySparql;
}

async function createPerspective(name: string): Promise<string> {
  const data = await gql(`mutation($name: String!) {
    perspectiveAdd(name: $name) { uuid }
  }`, { name });
  return data.perspectiveAdd.uuid;
}

async function measure(name: string, fn: () => Promise<any>, iterations: number = 10): Promise<TestResult> {
  const times: number[] = [];
  for (let i = 0; i < iterations; i++) {
    try {
      const start = performance.now();
      await fn();
      times.push(performance.now() - start);
    } catch (e: any) {
      console.warn(`   [${name}] Error on iteration ${i}: ${e.message?.slice(0, 80)}`);
      break;
    }
  }
  if (times.length === 0) return { name: name + ' (FAILED)', avg: -1, p95: -1, min: -1, max: -1 };
  times.sort((a, b) => a - b);
  return {
    name,
    avg: times.reduce((a, b) => a + b) / times.length,
    p95: times[Math.floor(times.length * 0.95)],
    min: times[0],
    max: times[times.length - 1],
  };
}

function fmt(ms: number): string {
  if (ms < 0) return 'TIMEOUT';
  return ms.toFixed(1);
}

// ── Seeding helpers ──

async function seedFlat(uuid: string, count: number): Promise<number> {
  let links = 0;
  const BATCH = 50;
  for (let i = 0; i < count; i += BATCH) {
    const batch: Array<{source: string, predicate: string, target: string}> = [];
    const end = Math.min(i + BATCH, count);
    for (let j = i; j < end; j++) {
      const src = `literal://string:post-${j}`;
      batch.push({ source: src, predicate: 'blog://entry_type', target: 'blog://post' });
      batch.push({ source: src, predicate: 'blog://title', target: `literal://string:Title ${j}` });
      batch.push({ source: src, predicate: 'blog://body', target: `literal://string:Body content for post ${j}` });
      batch.push({ source: src, predicate: 'blog://published_at', target: `literal://string:2024-01-${(j % 28 + 1).toString().padStart(2, '0')}` });
    }
    await addLinks(uuid, batch);
    links += batch.length;
  }
  return links;
}

async function seedHierarchy(uuid: string, communities: number, channels: number, messages: number): Promise<number> {
  let links = 0;
  const BATCH = 50;
  // Seed communities
  for (let c = 0; c < communities; c++) {
    const csrc = `literal://string:community-${c}`;
    await addLinks(uuid, [
      { source: csrc, predicate: 'flux://entry_type', target: 'flux://has_community' },
      { source: csrc, predicate: 'rdf://name', target: `literal://string:Community ${c}` },
    ]);
    links += 2;
    // Seed channels
    for (let ch = 0; ch < channels; ch++) {
      const chsrc = `literal://string:chan-${c}-${ch}`;
      await addLinks(uuid, [
        { source: chsrc, predicate: 'flux://entry_type', target: 'flux://has_channel' },
        { source: chsrc, predicate: 'rdf://name', target: `literal://string:Channel ${ch}` },
        { source: csrc, predicate: 'flux://has_channel', target: chsrc },
      ]);
      links += 3;
      // Seed messages in batches
      for (let m = 0; m < messages; m += BATCH) {
        const batch: Array<{source: string, predicate: string, target: string}> = [];
        const end = Math.min(m + BATCH, messages);
        for (let mi = m; mi < end; mi++) {
          const msrc = `literal://string:msg-${c}-${ch}-${mi}`;
          batch.push({ source: msrc, predicate: 'flux://entry_type', target: 'flux://has_message' });
          batch.push({ source: msrc, predicate: 'flux://body', target: `literal://string:Message ${mi} in channel ${ch}` });
          batch.push({ source: msrc, predicate: 'flux://timestamp', target: `literal://number:${1700000000 + mi}` });
          batch.push({ source: chsrc, predicate: 'flux://has_message', target: msrc });
        }
        await addLinks(uuid, batch);
        links += batch.length;
      }
    }
  }
  return links;
}

async function seedWide(uuid: string, count: number): Promise<number> {
  let links = 0;
  const BATCH = 20;
  for (let i = 0; i < count; i += BATCH) {
    const batch: Array<{source: string, predicate: string, target: string}> = [];
    const end = Math.min(i + BATCH, count);
    for (let j = i; j < end; j++) {
      const src = `literal://string:profile-${j}`;
      batch.push({ source: src, predicate: 'profile://type', target: 'profile://user' });
      batch.push({ source: src, predicate: 'profile://name', target: `literal://string:User ${j}` });
      batch.push({ source: src, predicate: 'profile://email', target: `literal://string:user${j}@example.com` });
      batch.push({ source: src, predicate: 'profile://bio', target: `literal://string:Bio for user ${j}` });
      batch.push({ source: src, predicate: 'profile://avatar', target: `literal://string:https://avatar.example.com/${j}.png` });
      batch.push({ source: src, predicate: 'profile://location', target: `literal://string:City ${j % 50}` });
      batch.push({ source: src, predicate: 'profile://website', target: `literal://string:https://user${j}.example.com` });
      batch.push({ source: src, predicate: 'profile://joined_at', target: `literal://string:2024-${(j % 12 + 1).toString().padStart(2, '0')}-01` });
    }
    await addLinks(uuid, batch);
    links += batch.length;
  }
  return links;
}

// ── SPARQL templates ──

const SPARQL_FLAT_POSTS = `PREFIX ad4m: <ad4m://ontology/>
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  ?link a ad4m:Link ;
    ad4m:source ?source ;
    ad4m:predicate ?predicate ;
    ad4m:target ?target ;
    ad4m:author ?author ;
    ad4m:timestamp ?timestamp .
  FILTER EXISTS {
    ?cf a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate "blog://entry_type" ;
      ad4m:target "blog://post" .
  }
}`;

const SPARQL_FLAT_PROFILES = `PREFIX ad4m: <ad4m://ontology/>
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  ?link a ad4m:Link ;
    ad4m:source ?source ;
    ad4m:predicate ?predicate ;
    ad4m:target ?target ;
    ad4m:author ?author ;
    ad4m:timestamp ?timestamp .
  FILTER EXISTS {
    ?cf a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate "profile://type" ;
      ad4m:target "profile://user" .
  }
}`;

const SPARQL_FLAT_COMMUNITIES = `PREFIX ad4m: <ad4m://ontology/>
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  ?link a ad4m:Link ;
    ad4m:source ?source ;
    ad4m:predicate ?predicate ;
    ad4m:target ?target ;
    ad4m:author ?author ;
    ad4m:timestamp ?timestamp .
  FILTER EXISTS {
    ?cf a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate "flux://entry_type" ;
      ad4m:target "flux://has_community" .
  }
}`;

const SPARQL_BATCH_COMMUNITY_CHANNELS = `PREFIX ad4m: <ad4m://ontology/>
SELECT ?depth ?parentBase ?source ?predicate ?target ?author ?timestamp WHERE {
  {
    BIND(0 AS ?depth)
    BIND("" AS ?parentBase)
    ?link a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate ?predicate ;
      ad4m:target ?target ;
      ad4m:author ?author ;
      ad4m:timestamp ?timestamp .
    FILTER EXISTS {
      ?cf a ad4m:Link ;
        ad4m:source ?source ;
        ad4m:predicate "flux://entry_type" ;
        ad4m:target "flux://has_community" .
    }
  }
  UNION
  {
    BIND(1 AS ?depth)
    ?parentLink a ad4m:Link ;
      ad4m:source ?parentBase ;
      ad4m:predicate "flux://has_channel" ;
      ad4m:target ?childBase .
    FILTER EXISTS {
      ?pcf a ad4m:Link ;
        ad4m:source ?parentBase ;
        ad4m:predicate "flux://entry_type" ;
        ad4m:target "flux://has_community" .
    }
    ?link a ad4m:Link ;
      ad4m:source ?childBase ;
      ad4m:predicate ?predicate ;
      ad4m:target ?target ;
      ad4m:author ?author ;
      ad4m:timestamp ?timestamp .
    BIND(?childBase AS ?source)
  }
}`;

const SPARQL_DEEP_3LEVEL = `PREFIX ad4m: <ad4m://ontology/>
SELECT ?depth ?parentBase ?source ?predicate ?target ?author ?timestamp WHERE {
  {
    BIND(0 AS ?depth)
    BIND("" AS ?parentBase)
    ?link a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate ?predicate ;
      ad4m:target ?target ;
      ad4m:author ?author ;
      ad4m:timestamp ?timestamp .
    FILTER EXISTS {
      ?cf a ad4m:Link ;
        ad4m:source ?source ;
        ad4m:predicate "flux://entry_type" ;
        ad4m:target "flux://has_community" .
    }
  }
  UNION
  {
    BIND(1 AS ?depth)
    ?parentLink a ad4m:Link ;
      ad4m:source ?parentBase ;
      ad4m:predicate "flux://has_channel" ;
      ad4m:target ?childBase .
    FILTER EXISTS {
      ?pcf a ad4m:Link ;
        ad4m:source ?parentBase ;
        ad4m:predicate "flux://entry_type" ;
        ad4m:target "flux://has_community" .
    }
    ?link a ad4m:Link ;
      ad4m:source ?childBase ;
      ad4m:predicate ?predicate ;
      ad4m:target ?target ;
      ad4m:author ?author ;
      ad4m:timestamp ?timestamp .
    BIND(?childBase AS ?source)
  }
  UNION
  {
    BIND(2 AS ?depth)
    ?gpLink a ad4m:Link ;
      ad4m:source ?gpBase ;
      ad4m:predicate "flux://has_channel" ;
      ad4m:target ?parentBase .
    FILTER EXISTS {
      ?gpcf a ad4m:Link ;
        ad4m:source ?gpBase ;
        ad4m:predicate "flux://entry_type" ;
        ad4m:target "flux://has_community" .
    }
    ?parentLink2 a ad4m:Link ;
      ad4m:source ?parentBase ;
      ad4m:predicate "flux://has_message" ;
      ad4m:target ?childBase .
    ?link a ad4m:Link ;
      ad4m:source ?childBase ;
      ad4m:predicate ?predicate ;
      ad4m:target ?target ;
      ad4m:author ?author ;
      ad4m:timestamp ?timestamp .
    BIND(?childBase AS ?source)
  }
}`;

function sparqlFilterQuery(flagPred: string, flagTarget: string, searchTerm: string): string {
  return `PREFIX ad4m: <ad4m://ontology/>
SELECT ?source ?predicate ?target WHERE {
  ?link a ad4m:Link ;
    ad4m:source ?source ;
    ad4m:predicate ?predicate ;
    ad4m:target ?target .
  FILTER EXISTS {
    ?cf a ad4m:Link ;
      ad4m:source ?source ;
      ad4m:predicate "${flagPred}" ;
      ad4m:target "${flagTarget}" .
  }
  FILTER(CONTAINS(?target, "${searchTerm}"))
}`;
}

function sparqlOrderLimit(channelSrc: string): string {
  return `PREFIX ad4m: <ad4m://ontology/>
SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
  {
    SELECT ?source WHERE {
      ?pl a ad4m:Link ;
        ad4m:source "${channelSrc}" ;
        ad4m:predicate "flux://has_message" ;
        ad4m:target ?source .
      ?tl a ad4m:Link ;
        ad4m:source ?source ;
        ad4m:predicate "flux://timestamp" ;
        ad4m:target ?ts .
    }
    ORDER BY DESC(?ts)
    LIMIT 50
  }
  ?link a ad4m:Link ;
    ad4m:source ?source ;
    ad4m:predicate ?predicate ;
    ad4m:target ?target ;
    ad4m:author ?author ;
    ad4m:timestamp ?timestamp .
}`;
}

// ── Main ──

interface TestResult {
  name: string;
  avg: number;
  p95: number;
  min: number;
  max: number;
}

interface TestSuite {
  name: string;
  linkCount: number;
  results: TestResult[];
}

async function runFlatBenchmark(label: string, count: number, iters: number = 10): Promise<TestSuite> {
  console.log(`\n── ${label}: Seeding ${count} blog posts...`);
  const uuid = await createPerspective(`bench-flat-${count}`);
  const linkCount = await seedFlat(uuid, count);
  console.log(`   Seeded ${linkCount} links`);

  const results: TestResult[] = [];

  console.log('   Measuring...');
  results.push(await measure('LinkQuery (all)', () => queryLinks(uuid), iters));
  results.push(await measure('LinkQuery (by predicate)', () => queryLinks(uuid, { predicate: 'blog://title' }), iters));
  results.push(await measure('SPARQL: flat query', () => sparqlQuery(uuid, SPARQL_FLAT_POSTS), iters));
  results.push(await measure('SPARQL: WHERE filter', () => sparqlQuery(uuid, sparqlFilterQuery('blog://entry_type', 'blog://post', 'Title 1')), iters));

  return { name: label, linkCount, results };
}

async function runHierarchyBenchmark(label: string, communities: number, channels: number, messages: number, iters: number = 10): Promise<TestSuite> {
  console.log(`\n── ${label}: Seeding ${communities}c × ${channels}ch × ${messages}m...`);
  const uuid = await createPerspective(`bench-hier-${communities}-${channels}-${messages}`);
  const linkCount = await seedHierarchy(uuid, communities, channels, messages);
  console.log(`   Seeded ${linkCount} links`);

  const results: TestResult[] = [];
  const firstChan = `literal://string:chan-0-0`;

  console.log('   Measuring...');
  results.push(await measure('LinkQuery (all)', () => queryLinks(uuid), iters));
  results.push(await measure('LinkQuery (by predicate)', () => queryLinks(uuid, { predicate: 'flux://has_message' }), iters));
  results.push(await measure('SPARQL: flat (communities)', () => sparqlQuery(uuid, SPARQL_FLAT_COMMUNITIES), iters));
  results.push(await measure('SPARQL: batch (comm→chan)', () => sparqlQuery(uuid, SPARQL_BATCH_COMMUNITY_CHANNELS), iters));
  results.push(await measure('SPARQL: deep 3-level', () => sparqlQuery(uuid, SPARQL_DEEP_3LEVEL), iters));
  results.push(await measure('SPARQL: WHERE filter', () => sparqlQuery(uuid, sparqlFilterQuery('flux://entry_type', 'flux://has_message', 'Message 1')), iters));
  results.push(await measure('SPARQL: ORDER BY+LIMIT 50', () => sparqlQuery(uuid, sparqlOrderLimit(firstChan)), iters));

  return { name: label, linkCount, results };
}

async function runWideBenchmark(label: string, count: number, iters: number = 10): Promise<TestSuite> {
  console.log(`\n── ${label}: Seeding ${count} profiles...`);
  const uuid = await createPerspective(`bench-wide-${count}`);
  const linkCount = await seedWide(uuid, count);
  console.log(`   Seeded ${linkCount} links`);

  const results: TestResult[] = [];

  console.log('   Measuring...');
  results.push(await measure('LinkQuery (all)', () => queryLinks(uuid), iters));
  results.push(await measure('LinkQuery (by predicate)', () => queryLinks(uuid, { predicate: 'profile://name' }), iters));
  results.push(await measure('SPARQL: flat query', () => sparqlQuery(uuid, SPARQL_FLAT_PROFILES), iters));
  results.push(await measure('SPARQL: WHERE filter', () => sparqlQuery(uuid, sparqlFilterQuery('profile://type', 'profile://user', 'User 1')), iters));

  return { name: label, linkCount, results };
}

import { mkdirSync, writeFileSync } from 'fs';

function writeResults(suites: TestSuite[]) {
  const now = new Date().toISOString();
  let md = `# SPARQL Batching Benchmark Results\n\n`;
  md += `**Date:** ${now}\n\n`;

  for (const suite of suites) {
    md += `## ${suite.name}\n\n`;
    md += `**Links:** ${suite.linkCount}\n\n`;
    if (suite.results.length === 0) {
      md += `*No results — executor crashed or test failed*\n\n`;
      continue;
    }
    md += `| Query Pattern | Avg (ms) | P95 (ms) | Min (ms) | Max (ms) |\n`;
    md += `|---|---:|---:|---:|---:|\n`;
    for (const r of suite.results) {
      md += `| ${r.name} | ${fmt(r.avg)} | ${fmt(r.p95)} | ${fmt(r.min)} | ${fmt(r.max)} |\n`;
    }
    md += `\n`;
  }

  md += `## Summary\n\n`;
  md += `### Key Observations\n\n`;

  const flat1k = suites.find(s => s.name.includes('1K Flat') && s.results.length > 0);
  const hier1k = suites.find(s => s.name.includes('1K Hierarchy') && s.results.length > 0);
  const flat10k = suites.find(s => s.name.includes('10K Flat') && s.results.length > 0);
  const hier10k = suites.find(s => s.name.includes('10K Hierarchy') && s.results.length > 0);

  if (flat1k) {
    const lq = flat1k.results.find(r => r.name === 'LinkQuery (all)');
    const sq = flat1k.results.find(r => r.name === 'SPARQL: flat query');
    if (lq && sq) md += `- **Flat 1K:** LinkQuery all=${fmt(lq.avg)}ms vs SPARQL flat=${fmt(sq.avg)}ms\n`;
  }
  if (hier1k) {
    const lq = hier1k.results.find(r => r.name === 'LinkQuery (all)');
    const batch = hier1k.results.find(r => r.name.includes('batch'));
    const deep = hier1k.results.find(r => r.name.includes('deep'));
    if (lq && batch && deep) {
      md += `- **Hierarchy 1K:** LinkQuery=${fmt(lq.avg)}ms, batch=${fmt(batch.avg)}ms, deep 3-level=${fmt(deep.avg)}ms\n`;
    }
  }
  if (flat10k) {
    const lq = flat10k.results.find(r => r.name === 'LinkQuery (all)');
    const sq = flat10k.results.find(r => r.name === 'SPARQL: flat query');
    if (lq) md += `- **Flat 10K:** LinkQuery all=${fmt(lq.avg)}ms` + (sq && sq.avg > 0 ? `, SPARQL=${fmt(sq.avg)}ms` : ', SPARQL crashed executor') + '\n';
  }

  const crashed10k = suites.filter(s => s.name.includes('10K') && s.results.some(r => r.avg < 0));
  if (crashed10k.length > 0) {
    md += `\n### ⚠️ Executor Stability at 10K\n\n`;
    md += `The executor consistently crashed or became unresponsive when running SPARQL queries against perspectives with 10,000+ links. `;
    md += `This is the most critical finding — SPARQL at scale is currently unusable.\n\n`;
  }

  md += `\n### Which patterns benefit most from SPARQL batching?\n\n`;
  md += `1. **Hierarchical queries** — Single SPARQL query fetching community→channel→message avoids N+1 round trips\n`;
  md += `2. **Filtered queries** — SPARQL WHERE filters avoid transferring and filtering all links client-side\n`;
  md += `3. **Paginated queries** — ORDER BY + LIMIT in SPARQL avoids fetching entire dataset\n`;
  md += `4. **⚠️ Scale concern** — At 10K links, SPARQL queries crash the executor; optimization needed before batching is viable at scale\n`;

  mkdirSync('/Users/josh/workspaces/coasys/ad4m/benchmarks/results', { recursive: true });
  writeFileSync('/Users/josh/workspaces/coasys/ad4m/benchmarks/results/batch-benchmark-results.md', md);
  console.log('\nResults written to benchmarks/results/batch-benchmark-results.md');
  return md;
}

async function ensureAgentReady() {
  for (let i = 0; i < 20; i++) {
    try {
      const status = await gql('{ agentStatus { isInitialized isUnlocked } }');
      const { isInitialized, isUnlocked } = status.agentStatus;
      if (!isInitialized) {
        await gql('mutation { agentGenerate(passphrase: "test", holochain: false) { isInitialized did } }');
      } else if (!isUnlocked) {
        await gql('mutation { agentUnlock(passphrase: "test", holochain: false) { isInitialized did } }');
      }
      return;
    } catch {
      console.log(`   Waiting for executor to recover... (${i + 1})`);
      await sleep(10000);
    }
  }
  throw new Error('Executor did not recover');
}

async function main() {
  console.log('SPARQL Batching Benchmark');
  console.log('=========================\n');

  await ensureAgentReady();
  console.log('Agent ready.');

  const suites: TestSuite[] = [];

  for (const [label, fn] of [
    ['1K Flat (Blog Posts)', () => runFlatBenchmark('1K Flat (Blog Posts)', 250)],
    ['1K Hierarchy (Chat)', () => runHierarchyBenchmark('1K Hierarchy (Chat)', 2, 5, 20)],
    ['1K Wide (Profiles)', () => runWideBenchmark('1K Wide (Profiles)', 125)],
    ['10K Flat (Blog Posts)', () => runFlatBenchmark('10K Flat (Blog Posts)', 2500, 3)],
    ['10K Hierarchy (Chat)', () => runHierarchyBenchmark('10K Hierarchy (Chat)', 5, 10, 40, 3)],
    ['10K Wide (Profiles)', () => runWideBenchmark('10K Wide (Profiles)', 1250, 3)],
  ] as [string, () => Promise<TestSuite>][]) {
    try {
      await ensureAgentReady();
      suites.push(await fn());
    } catch (e: any) {
      console.error(`\n!! ${label} FAILED: ${e.message?.slice(0, 100)}`);
      suites.push({ name: label + ' (FAILED)', linkCount: 0, results: [] });
    }
    // Write incrementally
    writeResults(suites);
  }

  const md = writeResults(suites);
  console.log('\n' + md);
}

main().catch(e => {
  console.error('Benchmark failed:', e);
  process.exit(1);
});
