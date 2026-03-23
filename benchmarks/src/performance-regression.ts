/**
 * Performance Regression Tests for SPARQL Stack
 * 
 * Seeds a standard Flux community dataset and asserts query timing
 * stays within acceptable bounds. Exit code 1 = regression detected.
 */

const ENDPOINT = 'http://127.0.0.1:12000/graphql';
const ADMIN_CRED = 'test-admin';
const PASSPHRASE = 'test';
const WARMUP = 2;
const ITERATIONS = 10;

const P = {
  ENTRY_TYPE: 'flux://entry_type',
  NAME: 'rdf://name',
  DESCRIPTION: 'rdf://description',
  CHANNEL: 'flux://has_channel',
  CHANNEL_NAME: 'flux://has_channel_name',
  CHANNEL_DESC: 'flux://has_channel_description',
  CHANNEL_CONVERSATION: 'flux://has_conversation',
  CONVERSATION_SUBGROUP: 'flux://has_subgroup',
  SUBGROUP_ITEM: 'flux://has_item',
  BODY: 'flux://body',
  CONV_NAME: 'flux://has_name',
  CONV_SUMMARY: 'flux://has_summary',
} as const;

const ET = {
  COMMUNITY: 'flux://has_community',
  CHANNEL: 'flux://has_channel',
  CONVERSATION: 'flux://conversation',
  CONV_SUBGROUP: 'flux://conversation_subgroup',
  MESSAGE: 'flux://has_message',
} as const;

async function gql<T = any>(query: string, variables?: Record<string, unknown>): Promise<T> {
  const resp = await fetch(ENDPOINT, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Authorization: ADMIN_CRED },
    body: JSON.stringify({ query, variables }),
    signal: AbortSignal.timeout(120_000),
  });
  const json = await resp.json() as any;
  if (json.errors?.length) throw new Error(`GQL: ${json.errors.map((e: any) => e.message).join('; ')}`);
  return json.data as T;
}

async function ensureAgent() {
  const { agentStatus: s } = await gql<any>(`query { agentStatus { isInitialized isUnlocked did } }`);
  if (!s.isInitialized) await gql(`mutation { agentGenerate(passphrase: "${PASSPHRASE}") { did } }`);
  else if (!s.isUnlocked) await gql(`mutation { agentUnlock(passphrase: "${PASSPHRASE}") { did } }`);
}

function lit(s: string) { return `literal://string:${s}`; }

function generateLinks() {
  const links: Array<{ source: string; predicate: string; target: string }> = [];
  const communityUri = lit('perf-community');
  const channelUris: string[] = [];

  links.push({ source: communityUri, predicate: P.ENTRY_TYPE, target: ET.COMMUNITY });
  links.push({ source: communityUri, predicate: P.NAME, target: lit('Perf Community') });

  for (let c = 0; c < 10; c++) {
    const chUri = lit(`perf-ch-${c}`);
    channelUris.push(chUri);
    links.push({ source: communityUri, predicate: P.CHANNEL, target: chUri });
    links.push({ source: chUri, predicate: P.ENTRY_TYPE, target: ET.CHANNEL });
    links.push({ source: chUri, predicate: P.CHANNEL_NAME, target: lit(`channel-${c}`) });

    for (let cv = 0; cv < 3; cv++) {
      const convUri = lit(`perf-conv-${c}-${cv}`);
      links.push({ source: chUri, predicate: P.CHANNEL_CONVERSATION, target: convUri });
      links.push({ source: convUri, predicate: P.ENTRY_TYPE, target: ET.CONVERSATION });
      links.push({ source: convUri, predicate: P.CONV_NAME, target: lit(`conv-${cv}`) });

      for (let sg = 0; sg < 2; sg++) {
        const sgUri = lit(`perf-sg-${c}-${cv}-${sg}`);
        links.push({ source: convUri, predicate: P.CONVERSATION_SUBGROUP, target: sgUri });
        links.push({ source: sgUri, predicate: P.ENTRY_TYPE, target: ET.CONV_SUBGROUP });

        for (let m = 0; m < 50; m++) {
          const msgUri = lit(`perf-msg-${c}-${cv}-${sg}-${m}`);
          links.push({ source: sgUri, predicate: P.SUBGROUP_ITEM, target: msgUri });
          links.push({ source: msgUri, predicate: P.ENTRY_TYPE, target: ET.MESSAGE });
          links.push({ source: msgUri, predicate: P.BODY, target: lit(`Message body ${m}`) });
        }
      }
    }
  }

  return { links, communityUri, channelUris };
}

async function querySparql(uuid: string, sparql: string): Promise<any> {
  const d = await gql<any>(
    `query($uuid: String!, $query: String!) { perspectiveQuerySurrealDb(uuid: $uuid, query: $query) }`,
    { uuid, query: sparql }
  );
  return JSON.parse(d.perspectiveQuerySurrealDb);
}

async function queryLinks(uuid: string, q: Record<string, string>): Promise<any[]> {
  const d = await gql<any>(
    `query($uuid: String!, $query: LinkQuery!) {
      perspectiveQueryLinks(uuid: $uuid, query: $query) { data { source predicate target } }
    }`,
    { uuid, query: q }
  );
  return d.perspectiveQueryLinks;
}

async function measure(fn: () => Promise<any>): Promise<number> {
  // warmup
  for (let i = 0; i < WARMUP; i++) await fn();
  const samples: number[] = [];
  for (let i = 0; i < ITERATIONS; i++) {
    const start = performance.now();
    await fn();
    samples.push(performance.now() - start);
  }
  return samples.reduce((a, b) => a + b, 0) / samples.length;
}

interface Check { name: string; fn: () => Promise<any>; maxMs: number }

async function main() {
  console.log('Performance Regression Test');
  console.log('==========================\n');

  await ensureAgent();

  const { links, communityUri, channelUris } = generateLinks();
  console.log(`Generated ${links.length} links`);

  const { perspectiveAdd: { uuid } } = await gql<any>(
    `mutation($n: String!) { perspectiveAdd(name: $n) { uuid } }`,
    { n: `perf-regression-${Date.now()}` }
  );

  // Seed in chunks
  const CHUNK = 500;
  for (let i = 0; i < links.length; i += CHUNK) {
    await gql(
      `mutation($uuid: String!, $links: [LinkInput!]!) {
        perspectiveAddLinks(uuid: $uuid, links: $links) { author }
      }`,
      { uuid, links: links.slice(i, i + CHUNK) }
    );
  }
  console.log('Data seeded\n');

  const firstChannel = channelUris[0];
  const checks: Check[] = [
    {
      name: 'Community Overview (SPARQL)',
      maxMs: 5,
      fn: () => querySparql(uuid, `SELECT ?name ?ch WHERE { ?c <${P.ENTRY_TYPE}> <${ET.COMMUNITY}> . ?c <${P.NAME}> ?name . ?c <${P.CHANNEL}> ?ch . }`)
    },
    {
      name: 'Channel List (SPARQL)',
      maxMs: 5,
      fn: () => querySparql(uuid, `SELECT ?ch ?name WHERE { ?ch <${P.ENTRY_TYPE}> <${ET.CHANNEL}> . ?ch <${P.CHANNEL_NAME}> ?name . }`)
    },
    {
      name: 'Channel Conversations (SPARQL)',
      maxMs: 3,
      fn: () => querySparql(uuid, `SELECT ?conv ?name WHERE { <${firstChannel}> <${P.CHANNEL_CONVERSATION}> ?conv . ?conv <${P.CONV_NAME}> ?name . }`)
    },
    {
      name: 'Conversation Messages (SPARQL)',
      maxMs: 3,
      fn: () => querySparql(uuid, `SELECT ?msg ?body WHERE { <${lit('perf-conv-0-0')}> <${P.CONVERSATION_SUBGROUP}> ?sg . ?sg <${P.SUBGROUP_ITEM}> ?msg . ?msg <${P.BODY}> ?body . }`)
    },
    {
      name: 'Full Community Load (SPARQL)',
      maxMs: 10,
      fn: async () => {
        await querySparql(uuid, `SELECT ?name ?ch WHERE { ?c <${P.ENTRY_TYPE}> <${ET.COMMUNITY}> . ?c <${P.NAME}> ?name . ?c <${P.CHANNEL}> ?ch . }`);
        await querySparql(uuid, `SELECT ?ch ?name WHERE { ?ch <${P.ENTRY_TYPE}> <${ET.CHANNEL}> . ?ch <${P.CHANNEL_NAME}> ?name . }`);
      }
    },
    {
      name: 'LinkQuery by source',
      maxMs: 2,
      fn: () => queryLinks(uuid, { source: firstChannel })
    },
    {
      name: 'LinkQuery by predicate',
      maxMs: 200,
      fn: () => queryLinks(uuid, { predicate: P.ENTRY_TYPE })
    },
    {
      name: 'LinkQuery by source+predicate',
      maxMs: 2,
      fn: () => queryLinks(uuid, { source: firstChannel, predicate: P.CHANNEL_NAME })
    },
  ];

  let failures = 0;
  for (const check of checks) {
    const avg = await measure(check.fn);
    const pass = avg <= check.maxMs;
    const symbol = pass ? '✓' : '✗';
    console.log(`${symbol} ${check.name}: ${avg.toFixed(2)}ms (limit: ${check.maxMs}ms)`);
    if (!pass) failures++;
  }

  // Cleanup
  await gql(`mutation($u: String!) { perspectiveRemove(uuid: $u) }`, { u: uuid });

  console.log(`\n${failures === 0 ? 'ALL PASSED' : `${failures} FAILED`}`);
  process.exit(failures > 0 ? 1 : 0);
}

main().catch(e => { console.error('FATAL:', e); process.exit(1); });
