/**
 * Flux Community Data Shootout
 * Compares N+1 LinkQuery (what Ad4mModel does today) vs Batched SPARQL
 * for loading Flux community data at realistic scales.
 */

const ENDPOINT = 'http://127.0.0.1:12000/graphql';
const ADMIN_CRED = 'test-admin';
const PASSPHRASE = 'test';
const ITERATIONS = 5;

// ── Flux Predicates ──
const P = {
  ENTRY_TYPE: 'flux://entry_type',
  NAME: 'rdf://name',
  DESCRIPTION: 'rdf://description',
  CHANNEL: 'flux://has_channel',
  CHANNEL_NAME: 'flux://has_channel_name',
  CHANNEL_DESC: 'flux://has_channel_description',
  CHANNEL_IS_CONV: 'flux://channel_is_conversation',
  CHANNEL_IS_PINNED: 'flux://channel_is_pinned',
  CHANNEL_CONVERSATION: 'flux://has_conversation',
  CONVERSATION_SUBGROUP: 'flux://has_subgroup',
  SUBGROUP_ITEM: 'flux://has_item',
  BODY: 'flux://body',
  HAS_REPLY: 'flux://has_reply',
  REACTION: 'flux://has_reaction',
  MESSAGE_THREAD: 'flux://has_thread_message',
  PARTICIPANT: 'flux://has_participant',
  CONV_NAME: 'flux://has_name',
  CONV_SUMMARY: 'flux://has_summary',
  CHANNEL_MESSAGE: 'flux://has_message',
} as const;

const ET = {
  COMMUNITY: 'flux://has_community',
  CHANNEL: 'flux://has_channel',
  CONVERSATION: 'flux://conversation',
  CONV_SUBGROUP: 'flux://conversation_subgroup',
  MESSAGE: 'flux://has_message',
} as const;

// ── GraphQL helpers ──
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
  if (!s.isInitialized) {
    await gql(`mutation { agentGenerate(passphrase: "${PASSPHRASE}") { did } }`);
  } else if (!s.isUnlocked) {
    await gql(`mutation { agentUnlock(passphrase: "${PASSPHRASE}") { did } }`);
  }
  console.log('Agent ready:', s.did || '(just initialized)');
}

async function addPerspective(name: string): Promise<string> {
  const d = await gql<any>(`mutation($n: String!) { perspectiveAdd(name: $n) { uuid } }`, { n: name });
  return d.perspectiveAdd.uuid;
}

async function removePerspective(uuid: string) {
  await gql(`mutation($u: String!) { perspectiveRemove(uuid: $u) }`, { u: uuid });
}

async function batchAddLinks(uuid: string, links: Array<{ source: string; predicate: string; target: string }>) {
  // Add in chunks of 500
  const CHUNK = 500;
  for (let i = 0; i < links.length; i += CHUNK) {
    const batch = links.slice(i, i + CHUNK);
    await gql(
      `mutation($uuid: String!, $links: [LinkInput!]!) {
        perspectiveAddLinks(uuid: $uuid, links: $links) { author }
      }`,
      { uuid, links: batch }
    );
  }
}

async function queryLinks(uuid: string, q: Record<string, string>): Promise<any[]> {
  const d = await gql<any>(
    `query($uuid: String!, $query: LinkQuery!) {
      perspectiveQueryLinks(uuid: $uuid, query: $query) {
        author timestamp data { source predicate target }
      }
    }`,
    { uuid, query: q }
  );
  return d.perspectiveQueryLinks;
}

async function querySparql(uuid: string, sparql: string): Promise<any> {
  const d = await gql<any>(
    `query($uuid: String!, $query: String!) {
      perspectiveQuerySurrealDb(uuid: $uuid, query: $query)
    }`,
    { uuid, query: sparql }
  );
  return JSON.parse(d.perspectiveQuerySurrealDb);
}

// ── Data generation ──
const CHANNEL_NAMES = ['general', 'announcements', 'dev', 'design', 'random', 'support', 'off-topic', 'ideas', 'feedback', 'showcase',
  'architecture', 'testing', 'deployment', 'documentation', 'community', 'onboarding', 'research', 'planning', 'standup', 'retrospective',
  'bugs', 'features', 'infrastructure', 'security', 'performance', 'accessibility', 'mobile', 'web', 'api', 'database',
  'frontend', 'backend', 'devops', 'analytics', 'marketing', 'sales', 'hr', 'finance', 'legal', 'operations',
  'product', 'ux', 'data-science', 'ml-ops', 'integrations', 'plugins', 'themes', 'localization', 'ci-cd', 'monitoring'];

const MESSAGE_BODIES = [
  'Hey everyone, just pushed the latest changes to the repo.',
  'Has anyone looked into the performance issues with the new query engine?',
  'I think we should consider a different approach for the caching layer.',
  'The new UI looks great! Nice work on the redesign.',
  'Can someone review my PR? It fixes the race condition in the sync module.',
  'Meeting notes from today: we agreed to prioritize the mobile experience.',
  'I found a bug in the message threading — replies sometimes appear out of order.',
  'Quick question: what\'s the recommended way to handle pagination in our API?',
  'Just deployed v2.3.1 to staging. Please test when you get a chance.',
  'The documentation for the new SDK is now live at docs.example.com.',
  'Reminder: demo day is this Friday at 3pm.',
  'I\'ve been experimenting with WebSockets for real-time updates and the results look promising.',
  'Does anyone have experience with SurrealDB? Thinking about it for our next project.',
  'The CI pipeline is green again after fixing the flaky test.',
  'Proposal: let\'s add end-to-end encryption for DMs.',
  'Working on the notification system — should we use push or pull?',
  'Great discussion in today\'s architecture review.',
  'The memory usage has dropped 40% after the latest optimization.',
  'Anyone available for pair programming this afternoon?',
  'Heads up: maintenance window tonight from 10pm to 2am.',
];

function lit(s: string) { return `literal:string:${s}`; }

interface ScaleConfig {
  label: string;
  numChannels: number;
  convsPerChannel: number;
  subgroupsPerConv: number;
  msgsPerSubgroup: number;
}

function generateLinks(cfg: ScaleConfig) {
  const links: Array<{ source: string; predicate: string; target: string }> = [];
  const communityUri = lit('community-1');
  const channelUris: string[] = [];
  const conversationsByChannel: Map<string, string[]> = new Map();
  const subgroupsByConv: Map<string, string[]> = new Map();
  const messagesBySubgroup: Map<string, string[]> = new Map();

  // Community
  links.push({ source: communityUri, predicate: P.ENTRY_TYPE, target: ET.COMMUNITY });
  links.push({ source: communityUri, predicate: P.NAME, target: lit('Coasys Community') });
  links.push({ source: communityUri, predicate: P.DESCRIPTION, target: lit('The main community for Coasys developers and users') });

  // Channels
  for (let c = 0; c < cfg.numChannels; c++) {
    const chUri = lit(`channel-${c}`);
    channelUris.push(chUri);
    links.push({ source: communityUri, predicate: P.CHANNEL, target: chUri });
    links.push({ source: chUri, predicate: P.ENTRY_TYPE, target: ET.CHANNEL });
    links.push({ source: chUri, predicate: P.CHANNEL_NAME, target: lit(CHANNEL_NAMES[c % CHANNEL_NAMES.length]) });
    links.push({ source: chUri, predicate: P.CHANNEL_DESC, target: lit(`Discussion about ${CHANNEL_NAMES[c % CHANNEL_NAMES.length]}`) });
    links.push({ source: chUri, predicate: P.CHANNEL_IS_CONV, target: lit('false') });
    links.push({ source: chUri, predicate: P.CHANNEL_IS_PINNED, target: lit(c < 3 ? 'true' : 'false') });

    const convUris: string[] = [];
    for (let cv = 0; cv < cfg.convsPerChannel; cv++) {
      const convUri = lit(`conv-${c}-${cv}`);
      convUris.push(convUri);
      links.push({ source: chUri, predicate: P.CHANNEL_CONVERSATION, target: convUri });
      links.push({ source: convUri, predicate: P.ENTRY_TYPE, target: ET.CONVERSATION });
      links.push({ source: convUri, predicate: P.CONV_NAME, target: lit(`Conversation ${cv + 1} in ${CHANNEL_NAMES[c % CHANNEL_NAMES.length]}`) });
      links.push({ source: convUri, predicate: P.CONV_SUMMARY, target: lit(`Summary of discussion topic ${cv + 1}`) });
      links.push({ source: convUri, predicate: P.PARTICIPANT, target: lit('did:key:participant-1') });
      links.push({ source: convUri, predicate: P.PARTICIPANT, target: lit('did:key:participant-2') });

      const sgUris: string[] = [];
      for (let sg = 0; sg < cfg.subgroupsPerConv; sg++) {
        const sgUri = lit(`subgroup-${c}-${cv}-${sg}`);
        sgUris.push(sgUri);
        links.push({ source: convUri, predicate: P.CONVERSATION_SUBGROUP, target: sgUri });
        links.push({ source: sgUri, predicate: P.ENTRY_TYPE, target: ET.CONV_SUBGROUP });
        links.push({ source: sgUri, predicate: P.CONV_NAME, target: lit(`Subgroup ${sg + 1}`) });
        links.push({ source: sgUri, predicate: P.CONV_SUMMARY, target: lit(`Subthread about aspect ${sg + 1}`) });

        const msgUris: string[] = [];
        for (let m = 0; m < cfg.msgsPerSubgroup; m++) {
          const msgUri = lit(`msg-${c}-${cv}-${sg}-${m}`);
          msgUris.push(msgUri);
          links.push({ source: sgUri, predicate: P.SUBGROUP_ITEM, target: msgUri });
          links.push({ source: msgUri, predicate: P.ENTRY_TYPE, target: ET.MESSAGE });
          links.push({ source: msgUri, predicate: P.BODY, target: lit(MESSAGE_BODIES[m % MESSAGE_BODIES.length]) });
        }
        messagesBySubgroup.set(sgUri, msgUris);
      }
      subgroupsByConv.set(convUri, sgUris);
    }
    conversationsByChannel.set(chUri, convUris);
  }

  return { links, communityUri, channelUris, conversationsByChannel, subgroupsByConv, messagesBySubgroup };
}

// ── Benchmark operations ──

// N+1 LinkQuery: simulates what Ad4mModel does
async function nPlus1_communityOverview(uuid: string, communityUri: string) {
  // Find community by entry type
  const communities = await queryLinks(uuid, { predicate: P.ENTRY_TYPE, target: ET.COMMUNITY });
  const uri = communities[0].data.source;
  // Get name
  await queryLinks(uuid, { source: uri, predicate: P.NAME });
  // Get description
  await queryLinks(uuid, { source: uri, predicate: P.DESCRIPTION });
  // Get channels
  const channels = await queryLinks(uuid, { source: uri, predicate: P.CHANNEL });
  return channels.map((l: any) => l.data.target);
}

async function nPlus1_channelList(uuid: string, channelUris: string[]) {
  const results = [];
  for (const chUri of channelUris) {
    const [name, desc, isConv] = await Promise.all([
      queryLinks(uuid, { source: chUri, predicate: P.CHANNEL_NAME }),
      queryLinks(uuid, { source: chUri, predicate: P.CHANNEL_DESC }),
      queryLinks(uuid, { source: chUri, predicate: P.CHANNEL_IS_CONV }),
    ]);
    results.push({ uri: chUri, name: name[0]?.data.target, desc: desc[0]?.data.target });
  }
  return results;
}

async function nPlus1_channelConversations(uuid: string, channelUri: string) {
  const convLinks = await queryLinks(uuid, { source: channelUri, predicate: P.CHANNEL_CONVERSATION });
  const results = [];
  for (const cl of convLinks) {
    const convUri = cl.data.target;
    const [name, summary] = await Promise.all([
      queryLinks(uuid, { source: convUri, predicate: P.CONV_NAME }),
      queryLinks(uuid, { source: convUri, predicate: P.CONV_SUMMARY }),
    ]);
    results.push({ uri: convUri, name: name[0]?.data.target, summary: summary[0]?.data.target });
  }
  return results;
}

async function nPlus1_conversationMessages(uuid: string, convUri: string) {
  const sgLinks = await queryLinks(uuid, { source: convUri, predicate: P.CONVERSATION_SUBGROUP });
  const allMessages = [];
  for (const sgLink of sgLinks) {
    const sgUri = sgLink.data.target;
    const msgLinks = await queryLinks(uuid, { source: sgUri, predicate: P.SUBGROUP_ITEM });
    for (const ml of msgLinks) {
      const msgUri = ml.data.target;
      const body = await queryLinks(uuid, { source: msgUri, predicate: P.BODY });
      allMessages.push({ uri: msgUri, body: body[0]?.data.target });
    }
  }
  return allMessages;
}

async function nPlus1_fullCommunityLoad(uuid: string, communityUri: string) {
  const channelUris = await nPlus1_communityOverview(uuid, communityUri);
  await nPlus1_channelList(uuid, channelUris);
  // Load first channel's conversations with messages
  const convs = await nPlus1_channelConversations(uuid, channelUris[0]);
  if (convs.length > 0) {
    await nPlus1_conversationMessages(uuid, convs[0].uri);
  }
}

// Batched SPARQL approach — uses direct triple model:
// Each AD4M link is a direct triple <source> <predicate> <target> (all IRIs).
// Values are IRIs like <literal:string:foo>, <flux://whatever>, etc.

async function sparql_communityOverview(uuid: string) {
  return await querySparql(uuid, `
    SELECT ?community ?name ?desc ?channel WHERE {
      ?community <${P.ENTRY_TYPE}> <${ET.COMMUNITY}> .
      ?community <${P.NAME}> ?name .
      ?community <${P.DESCRIPTION}> ?desc .
      ?community <${P.CHANNEL}> ?channel .
    }
  `);
}

async function sparql_channelList(uuid: string) {
  return await querySparql(uuid, `
    SELECT ?channel ?name ?desc ?isConv ?isPinned WHERE {
      ?channel <${P.ENTRY_TYPE}> <${ET.CHANNEL}> .
      ?channel <${P.CHANNEL_NAME}> ?name .
      ?channel <${P.CHANNEL_DESC}> ?desc .
      OPTIONAL { ?channel <${P.CHANNEL_IS_CONV}> ?isConv . }
      OPTIONAL { ?channel <${P.CHANNEL_IS_PINNED}> ?isPinned . }
    }
  `);
}

async function sparql_channelConversations(uuid: string, channelUri: string) {
  return await querySparql(uuid, `
    SELECT ?conv ?name ?summary WHERE {
      <${channelUri}> <${P.CHANNEL_CONVERSATION}> ?conv .
      ?conv <${P.ENTRY_TYPE}> <${ET.CONVERSATION}> .
      ?conv <${P.CONV_NAME}> ?name .
      ?conv <${P.CONV_SUMMARY}> ?summary .
    }
  `);
}

async function sparql_conversationMessages(uuid: string, convUri: string) {
  return await querySparql(uuid, `
    SELECT ?msg ?body WHERE {
      <${convUri}> <${P.CONVERSATION_SUBGROUP}> ?sg .
      ?sg <${P.SUBGROUP_ITEM}> ?msg .
      ?msg <${P.BODY}> ?body .
    }
  `);
}

async function sparql_fullCommunityLoad(uuid: string, channelUris: string[], firstConvUri: string) {
  await sparql_communityOverview(uuid);
  await sparql_channelList(uuid);
  await sparql_channelConversations(uuid, channelUris[0]);
  await sparql_conversationMessages(uuid, firstConvUri);
}

// ── Timing ──
interface TimingResult {
  avg: number;
  min: number;
  max: number;
  p95: number;
  samples: number[];
}

async function measure(fn: () => Promise<any>, iterations: number): Promise<TimingResult> {
  const samples: number[] = [];
  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await fn();
    samples.push(performance.now() - start);
  }
  samples.sort((a, b) => a - b);
  return {
    avg: samples.reduce((a, b) => a + b, 0) / samples.length,
    min: samples[0],
    max: samples[samples.length - 1],
    p95: samples[Math.floor(samples.length * 0.95)],
    samples,
  };
}

function fmt(ms: number) { return ms.toFixed(1) + 'ms'; }

// ── Main ──
async function runScale(cfg: ScaleConfig) {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`Scale: ${cfg.label}`);
  console.log(`Channels: ${cfg.numChannels}, Conversations: ${cfg.numChannels * cfg.convsPerChannel}, Subgroups: ${cfg.numChannels * cfg.convsPerChannel * cfg.subgroupsPerConv}, Messages: ${cfg.numChannels * cfg.convsPerChannel * cfg.subgroupsPerConv * cfg.msgsPerSubgroup}`);
  console.log(`${'='.repeat(60)}`);

  const { links, communityUri, channelUris, conversationsByChannel, subgroupsByConv } = generateLinks(cfg);
  console.log(`Generated ${links.length} links`);

  const uuid = await addPerspective(`flux-shootout-${cfg.label}-${Date.now()}`);
  console.log(`Created perspective: ${uuid}`);

  console.log('Seeding data...');
  const seedStart = performance.now();
  await batchAddLinks(uuid, links);
  const seedTime = performance.now() - seedStart;
  console.log(`Seeded in ${fmt(seedTime)}`);

  const firstChannelUri = channelUris[0];
  const firstConvUri = conversationsByChannel.get(firstChannelUri)![0];

  const results: Record<string, { nplus1: TimingResult; sparql: TimingResult }> = {};

  // 1. Community Overview
  console.log('\nBenchmarking: Community Overview...');
  results['Community Overview'] = {
    nplus1: await measure(() => nPlus1_communityOverview(uuid, communityUri), ITERATIONS),
    sparql: await measure(() => sparql_communityOverview(uuid), ITERATIONS),
  };

  // 2. Channel List
  console.log('Benchmarking: Channel List...');
  results['Channel List'] = {
    nplus1: await measure(() => nPlus1_channelList(uuid, channelUris), ITERATIONS),
    sparql: await measure(() => sparql_channelList(uuid), ITERATIONS),
  };

  // 3. Channel Conversations
  console.log('Benchmarking: Channel Conversations...');
  results['Channel Conversations'] = {
    nplus1: await measure(() => nPlus1_channelConversations(uuid, firstChannelUri), ITERATIONS),
    sparql: await measure(() => sparql_channelConversations(uuid, firstChannelUri), ITERATIONS),
  };

  // 4. Conversation Messages
  console.log('Benchmarking: Conversation Messages...');
  results['Conversation Messages'] = {
    nplus1: await measure(() => nPlus1_conversationMessages(uuid, firstConvUri), ITERATIONS),
    sparql: await measure(() => sparql_conversationMessages(uuid, firstConvUri), ITERATIONS),
  };

  // 5. Full Community Load
  console.log('Benchmarking: Full Community Load...');
  results['Full Community Load'] = {
    nplus1: await measure(() => nPlus1_fullCommunityLoad(uuid, communityUri), ITERATIONS),
    sparql: await measure(() => sparql_fullCommunityLoad(uuid, channelUris, firstConvUri), ITERATIONS),
  };

  // Cleanup
  await removePerspective(uuid);
  console.log('Perspective cleaned up.');

  return { cfg, results, seedTime, linkCount: links.length };
}

async function main() {
  console.log('Flux Community Data Shootout');
  console.log('N+1 LinkQuery (current Ad4mModel) vs Batched SPARQL\n');

  await ensureAgent();

  const scales: ScaleConfig[] = [
    { label: 'Small', numChannels: 10, convsPerChannel: 3, subgroupsPerConv: 2, msgsPerSubgroup: 50 },
    { label: 'Large', numChannels: 50, convsPerChannel: 3, subgroupsPerConv: 2, msgsPerSubgroup: 50 },
  ];

  const allResults: Awaited<ReturnType<typeof runScale>>[] = [];
  for (const scale of scales) {
    allResults.push(await runScale(scale));
  }

  // Generate markdown report
  let md = `# Flux Community Data Shootout Results\n\n`;
  md += `**Date:** ${new Date().toISOString()}\n\n`;
  md += `Comparing N+1 LinkQuery (what Ad4mModel does today) vs Batched SPARQL for loading Flux community data.\n\n`;

  for (const { cfg, results, seedTime, linkCount } of allResults) {
    const totalMsgs = cfg.numChannels * cfg.convsPerChannel * cfg.subgroupsPerConv * cfg.msgsPerSubgroup;
    md += `## ${cfg.label} Scale\n\n`;
    md += `- **Channels:** ${cfg.numChannels}\n`;
    md += `- **Conversations:** ${cfg.numChannels * cfg.convsPerChannel}\n`;
    md += `- **Subgroups:** ${cfg.numChannels * cfg.convsPerChannel * cfg.subgroupsPerConv}\n`;
    md += `- **Messages:** ${totalMsgs}\n`;
    md += `- **Total Links:** ${linkCount}\n`;
    md += `- **Seed Time:** ${fmt(seedTime)}\n\n`;

    md += `| Operation | N+1 LinkQuery (avg) | Batched SPARQL (avg) | Speedup | N+1 min/max | SPARQL min/max |\n`;
    md += `|-----------|--------------------:|---------------------:|--------:|------------:|---------------:|\n`;

    for (const [op, { nplus1, sparql }] of Object.entries(results)) {
      const speedup = nplus1.avg / sparql.avg;
      md += `| ${op} | ${fmt(nplus1.avg)} | ${fmt(sparql.avg)} | **${speedup.toFixed(1)}x** | ${fmt(nplus1.min)}/${fmt(nplus1.max)} | ${fmt(sparql.min)}/${fmt(sparql.max)} |\n`;
    }
    md += '\n';
  }

  // Summary
  md += `## Summary\n\n`;
  md += `The N+1 LinkQuery approach issues one GraphQL request per property per entity instance.\n`;
  md += `For a conversation with 100 messages, that's ~100 queries just for message bodies.\n`;
  md += `Batched SPARQL fetches everything in a single query.\n\n`;
  md += `The "Full Community Load" operation represents what a Flux user experiences when opening the app.\n`;

  // Print to console
  console.log('\n\n' + md);

  // Write to file
  const fs = await import('fs');
  fs.mkdirSync('/Users/josh/workspaces/coasys/ad4m/benchmarks/results', { recursive: true });
  fs.writeFileSync('/Users/josh/workspaces/coasys/ad4m/benchmarks/results/flux-shootout-results.md', md);
  console.log('\nResults written to benchmarks/results/flux-shootout-results.md');
}

main().catch(e => { console.error('FATAL:', e); process.exit(1); });
