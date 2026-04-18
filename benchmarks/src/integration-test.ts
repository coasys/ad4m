/**
 * Integration Test — end-to-end SPARQL stack via GraphQL
 * 
 * Tests: create perspective, add/query/remove links, SPARQL queries,
 * RDF-star metadata roundtrip. Exit code 0 = pass, 1 = fail.
 */

const ENDPOINT = 'http://127.0.0.1:12000/graphql';
const ADMIN_CRED = 'test-admin';
const PASSPHRASE = 'test';

async function gql<T = any>(query: string, variables?: Record<string, unknown>): Promise<T> {
  const resp = await fetch(ENDPOINT, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Authorization: ADMIN_CRED },
    body: JSON.stringify({ query, variables }),
    signal: AbortSignal.timeout(30_000),
  });
  const json = await resp.json() as any;
  if (json.errors?.length) throw new Error(`GQL: ${json.errors.map((e: any) => e.message).join('; ')}`);
  return json.data as T;
}

function assert(condition: boolean, msg: string) {
  if (!condition) throw new Error(`ASSERTION FAILED: ${msg}`);
}

let uuid: string;
let failures = 0;

async function test(name: string, fn: () => Promise<void>) {
  try {
    await fn();
    console.log(`  ✓ ${name}`);
  } catch (e: any) {
    console.error(`  ✗ ${name}: ${e.message}`);
    failures++;
  }
}

async function ensureAgent() {
  const { agentStatus: s } = await gql<any>(`query { agentStatus { isInitialized isUnlocked did } }`);
  if (!s.isInitialized) await gql(`mutation { agentGenerate(passphrase: "${PASSPHRASE}") { did } }`);
  else if (!s.isUnlocked) await gql(`mutation { agentUnlock(passphrase: "${PASSPHRASE}") { did } }`);
}

async function main() {
  console.log('Integration Test — SPARQL Stack\n');
  await ensureAgent();

  // 1. Create perspective
  const { perspectiveAdd: { uuid: u } } = await gql<any>(
    `mutation($n: String!) { perspectiveAdd(name: $n) { uuid } }`,
    { n: `integration-test-${Date.now()}` }
  );
  uuid = u;
  console.log(`Created perspective: ${uuid}\n`);

  // 2. Batch add links
  const testLinks = [
    { source: 'ad4m://entity1', predicate: 'test://name', target: 'literal:string:Alice' },
    { source: 'ad4m://entity1', predicate: 'test://age', target: 'literal:string:30' },
    { source: 'ad4m://entity2', predicate: 'test://name', target: 'literal:string:Bob' },
    { source: 'ad4m://entity2', predicate: 'test://age', target: 'literal:string:25' },
    { source: 'ad4m://entity1', predicate: 'test://friend', target: 'ad4m://entity2' },
  ];

  await gql(
    `mutation($uuid: String!, $links: [LinkInput!]!) {
      perspectiveAddLinks(uuid: $uuid, links: $links) { author timestamp data { source predicate target } }
    }`,
    { uuid, links: testLinks }
  );

  // 3. Query via perspectiveQueryLinks
  await test('LinkQuery returns added links by source', async () => {
    const d = await gql<any>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) {
          author timestamp data { source predicate target }
        }
      }`,
      { uuid, query: { source: 'ad4m://entity1' } }
    );
    const links = d.perspectiveQueryLinks;
    assert(links.length === 3, `Expected 3 links for entity1, got ${links.length}`);
  });

  await test('LinkQuery returns links by predicate', async () => {
    const d = await gql<any>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) {
          data { source predicate target }
        }
      }`,
      { uuid, query: { predicate: 'test://name' } }
    );
    assert(d.perspectiveQueryLinks.length === 2, `Expected 2 name links, got ${d.perspectiveQueryLinks.length}`);
  });

  // 4. Query via SPARQL
  await test('SPARQL direct triple query returns correct data', async () => {
    const d = await gql<any>(
      `query($uuid: String!, $query: String!) { perspectiveQuerySparql(uuid: $uuid, query: $query) }`,
      { uuid, query: `SELECT ?name WHERE { <ad4m://entity1> <test://name> ?name . }` }
    );
    const rows = JSON.parse(d.perspectiveQuerySparql);
    assert(rows.length === 1, `Expected 1 row, got ${rows.length}`);
    assert(rows[0].name === 'literal:string:Alice', `Expected Alice, got ${rows[0].name}`);
  });

  await test('SPARQL join query works', async () => {
    const d = await gql<any>(
      `query($uuid: String!, $query: String!) { perspectiveQuerySparql(uuid: $uuid, query: $query) }`,
      {
        uuid,
        query: `SELECT ?person ?name WHERE {
          ?person <test://name> ?name .
          ?person <test://age> ?age .
        }`
      }
    );
    const rows = JSON.parse(d.perspectiveQuerySparql);
    assert(rows.length === 2, `Expected 2 rows, got ${rows.length}`);
  });

  // 5. Add then remove a link
  await test('Remove link removes from both LinkQuery and SPARQL', async () => {
    const addResp = await gql<any>(
      `mutation($uuid: String!, $link: LinkInput!) {
        perspectiveAddLink(uuid: $uuid, link: $link) {
          author timestamp data { source predicate target } proof { valid key signature }
        }
      }`,
      { uuid, link: { source: 'ad4m://temp', predicate: 'test://temp', target: 'literal:string:temp' } }
    );
    const addedLink = addResp.perspectiveAddLink;

    // Verify exists
    let q = await gql<any>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) { data { source } }
      }`,
      { uuid, query: { source: 'ad4m://temp' } }
    );
    assert(q.perspectiveQueryLinks.length === 1, 'Link should exist after add');

    // Remove
    await gql(
      `mutation($uuid: String!, $link: LinkExpressionInput!) {
        perspectiveRemoveLink(uuid: $uuid, link: $link)
      }`,
      { uuid, link: { author: addedLink.author, timestamp: addedLink.timestamp, data: addedLink.data, proof: addedLink.proof } }
    );

    // Verify gone from LinkQuery
    q = await gql<any>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) { data { source } }
      }`,
      { uuid, query: { source: 'ad4m://temp' } }
    );
    assert(q.perspectiveQueryLinks.length === 0, `Link should be gone from LinkQuery, got ${q.perspectiveQueryLinks.length}`);

    // Verify gone from SPARQL
    const s = await gql<any>(
      `query($uuid: String!, $query: String!) { perspectiveQuerySparql(uuid: $uuid, query: $query) }`,
      { uuid, query: `SELECT ?o WHERE { <ad4m://temp> <test://temp> ?o . }` }
    );
    const rows = JSON.parse(s.perspectiveQuerySparql);
    assert(rows.length === 0, `Link should be gone from SPARQL, got ${rows.length}`);
  });

  // 6. RDF-star metadata roundtrip
  await test('RDF-star metadata (author, timestamp) roundtrips via LinkQuery', async () => {
    const d = await gql<any>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) {
          author timestamp data { source predicate target }
        }
      }`,
      { uuid, query: { source: 'ad4m://entity1', predicate: 'test://name' } }
    );
    const link = d.perspectiveQueryLinks[0];
    assert(!!link.author, 'Author should be present');
    assert(link.author.startsWith('did:'), `Author should be a DID, got ${link.author}`);
    assert(!!link.timestamp, 'Timestamp should be present');
    // Timestamp should be ISO format
    assert(!isNaN(Date.parse(link.timestamp)), `Timestamp should be valid ISO date, got ${link.timestamp}`);
  });

  await test('RDF-star annotations visible in raw SPARQL results', async () => {
    // Annotations have quoted triple subjects which appear as null in serialized results
    // but the annotation predicates and values should be present
    const s = await gql<any>(
      `query($uuid: String!, $query: String!) { perspectiveQuerySparql(uuid: $uuid, query: $query) }`,
      {
        uuid,
        query: `SELECT ?p ?v WHERE { ?s ?p ?v . FILTER(!isIRI(?s) && !isBlank(?s)) } LIMIT 10`
      }
    );
    const rows = JSON.parse(s.perspectiveQuerySparql);
    const preds = rows.map((r: any) => r.p);
    assert(preds.includes('ad4m://ontology/author'), 'Should find author annotation');
    assert(preds.includes('ad4m://ontology/timestamp'), 'Should find timestamp annotation');
  });

  // 7. Cleanup
  await gql(`mutation($u: String!) { perspectiveRemove(uuid: $u) }`, { u: uuid });
  console.log(`\nCleaned up perspective ${uuid}`);

  console.log(`\n${failures === 0 ? 'ALL PASSED' : `${failures} FAILED`}`);
  process.exit(failures > 0 ? 1 : 0);
}

main().catch(e => { console.error('FATAL:', e); process.exit(1); });
