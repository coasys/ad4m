// Complex query benchmark suite (SPARQL vs SPARQL/Prolog)

import type { GraphQLClient } from '../client'
import { timeIt, createRng, generateId, computeStats, type Stats } from '../utils'

export interface SparqlQueryResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    baseline: Stats
  }>
}

async function populateStructured(client: GraphQLClient, uuid: string, rng: () => number): Promise<void> {
  // Create community → channels → messages structure
  const communities = 3
  const channelsPerCommunity = 5
  const messagesPerChannel = 20

  for (let c = 0; c < communities; c++) {
    const communityId = `ad4m://community-${c}`
    for (let ch = 0; ch < channelsPerCommunity; ch++) {
      const channelId = `ad4m://channel-${c}-${ch}`
      await client.addLink(uuid, communityId, 'ad4m://has-channel', channelId)

      for (let m = 0; m < messagesPerChannel; m++) {
        const msgId = generateId(rng, 'msg')
        await client.addLink(uuid, channelId, 'ad4m://has-message', msgId)
        await client.addLink(uuid, msgId, 'ad4m://has-author', `did:key:author-${Math.floor(rng() * 10)}`)
        await client.addLink(uuid, msgId, 'ad4m://has-content', `literal:string:message-${m}`)
      }
    }
  }
}

async function benchSparqlQuery(
  client: GraphQLClient,
  uuid: string,
  queryStr: string,
  iterations: number,
  warmup: number,
): Promise<Stats> {
  for (let i = 0; i < warmup; i++) {
    await client.querySparql(uuid, queryStr)
  }
  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.querySparql(uuid, queryStr)
    samples.push(end())
  }
  return computeStats(samples)
}

export async function run(
  sparqlClient: GraphQLClient,
  baselineClient: GraphQLClient,
  iterations: number,
  warmup: number,
): Promise<SparqlQueryResult> {
  const tests: SparqlQueryResult['tests'] = []

  const sparqlUuid = await sparqlClient.addPerspective('bench-sparql-sparql')
  const baselineUuid = await baselineClient.addPerspective('bench-sparql-baseline')

  try {
    process.stdout.write('    Populating structured data... ')
    await populateStructured(sparqlClient, sparqlUuid, createRng(42))
    await populateStructured(baselineClient, baselineUuid, createRng(42))
    console.log('done')

    // Simple SELECT — SPARQL on sparql executor, SPARQL on baseline executor
    process.stdout.write('    Simple SELECT... ')
    const sparqlSimple = await benchSparqlQuery(
      sparqlClient, sparqlUuid,
      'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100',
      iterations, warmup,
    )
    const baselineSimple = await benchSparqlQuery(
      baselineClient, baselineUuid,
      'SELECT * FROM links LIMIT 100',
      iterations, warmup,
    )
    console.log('done')
    tests.push({ name: 'Simple SELECT (100 rows)', sparql: sparqlSimple, baseline: baselineSimple })

    // Filtered by predicate
    process.stdout.write('    Filtered SELECT... ')
    const sparqlFiltered = await benchSparqlQuery(
      sparqlClient, sparqlUuid,
      'SELECT ?s ?o WHERE { ?s <ad4m://has-message> ?o }',
      iterations, warmup,
    )
    const baselineFiltered = await benchSparqlQuery(
      baselineClient, baselineUuid,
      "SELECT * FROM links WHERE predicate = 'ad4m://has-message'",
      iterations, warmup,
    )
    console.log('done')
    tests.push({ name: 'Filtered SELECT (by predicate)', sparql: sparqlFiltered, baseline: baselineFiltered })

    // Multi-join: messages in a specific channel
    process.stdout.write('    Multi-join query... ')
    const sparqlJoin = await benchSparqlQuery(
      sparqlClient, sparqlUuid,
      `SELECT ?msg ?author ?content WHERE {
        <ad4m://channel-0-0> <ad4m://has-message> ?msg .
        ?msg <ad4m://has-author> ?author .
        ?msg <ad4m://has-content> ?content .
      }`,
      iterations, warmup,
    )
    const baselineJoin = await benchSparqlQuery(
      baselineClient, baselineUuid,
      `SELECT msg.target AS msg,
              (SELECT target FROM links WHERE source = msg.target AND predicate = 'ad4m://has-author' LIMIT 1)[0].target AS author,
              (SELECT target FROM links WHERE source = msg.target AND predicate = 'ad4m://has-content' LIMIT 1)[0].target AS content
       FROM links AS msg
       WHERE source = 'ad4m://channel-0-0' AND predicate = 'ad4m://has-message'`,
      iterations, warmup,
    )
    console.log('done')
    tests.push({ name: 'Multi-join (channel→messages→details)', sparql: sparqlJoin, baseline: baselineJoin })

    // Count aggregation
    process.stdout.write('    Aggregation (COUNT)... ')
    const sparqlCount = await benchSparqlQuery(
      sparqlClient, sparqlUuid,
      'SELECT ?p (COUNT(?s) AS ?count) WHERE { ?s ?p ?o } GROUP BY ?p',
      iterations, warmup,
    )
    const baselineCount = await benchSparqlQuery(
      baselineClient, baselineUuid,
      'SELECT predicate, count() AS total FROM links GROUP BY predicate',
      iterations, warmup,
    )
    console.log('done')
    tests.push({ name: 'Aggregation (COUNT GROUP BY)', sparql: sparqlCount, baseline: baselineCount })

  } finally {
    await sparqlClient.removePerspective(sparqlUuid).catch(() => {})
    await baselineClient.removePerspective(baselineUuid).catch(() => {})
  }

  return { name: 'Complex Queries (SPARQL vs SPARQL)', tests }
}
