// Point query benchmark suite

import type { GraphQLClient } from '../client'
import { timeIt, createRng, generateId, computeStats, type Stats } from '../utils'

export interface PointQueryResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    baseline: Stats
  }>
}

const PREDICATES = ['ad4m://has-child', 'ad4m://has-member', 'ad4m://has-message', 'ad4m://has-tag', 'ad4m://has-author']

async function populate(client: GraphQLClient, uuid: string, count: number, rng: () => number): Promise<{ sources: string[]; predicates: string[] }> {
  const sources: string[] = []
  const usedPredicates: string[] = []

  for (let i = 0; i < count; i++) {
    const src = generateId(rng, 'src')
    const pred = PREDICATES[Math.floor(rng() * PREDICATES.length)]
    const tgt = generateId(rng, 'tgt')
    await client.addLink(uuid, src, pred, tgt)
    if (i % 50 === 0) sources.push(src)
    if (!usedPredicates.includes(pred)) usedPredicates.push(pred)
  }
  return { sources, predicates: usedPredicates }
}

async function benchQuery(
  client: GraphQLClient,
  uuid: string,
  query: Record<string, string>,
  iterations: number,
  warmup: number,
): Promise<Stats> {
  for (let i = 0; i < warmup; i++) {
    await client.queryLinks(uuid, query)
  }
  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.queryLinks(uuid, query)
    samples.push(end())
  }
  return computeStats(samples)
}

export async function run(
  sparqlClient: GraphQLClient,
  baselineClient: GraphQLClient,
  iterations: number,
  warmup: number,
): Promise<PointQueryResult> {
  const tests: PointQueryResult['tests'] = []
  const rng = createRng(42)
  const LINK_COUNT = 1000

  const sparqlUuid = await sparqlClient.addPerspective('bench-query-sparql')
  const baselineUuid = await baselineClient.addPerspective('bench-query-baseline')

  try {
    process.stdout.write('    Populating perspectives... ')
    const sparqlData = await populate(sparqlClient, sparqlUuid, LINK_COUNT, createRng(42))
    const baselineData = await populate(baselineClient, baselineUuid, LINK_COUNT, createRng(42))
    console.log('done')

    // Query by source
    process.stdout.write('    Query by source... ')
    const src = sparqlData.sources[0]
    const sparqlBySource = await benchQuery(sparqlClient, sparqlUuid, { source: src }, iterations, warmup)
    const baselineBySource = await benchQuery(baselineClient, baselineUuid, { source: src }, iterations, warmup)
    console.log('done')
    tests.push({ name: 'Query by Source', sparql: sparqlBySource, baseline: baselineBySource })

    // Query by predicate
    process.stdout.write('    Query by predicate... ')
    const pred = sparqlData.predicates[0]
    const sparqlByPred = await benchQuery(sparqlClient, sparqlUuid, { predicate: pred }, iterations, warmup)
    const baselineByPred = await benchQuery(baselineClient, baselineUuid, { predicate: pred }, iterations, warmup)
    console.log('done')
    tests.push({ name: 'Query by Predicate', sparql: sparqlByPred, baseline: baselineByPred })

    // Query by source + predicate
    process.stdout.write('    Query by source+predicate... ')
    const sparqlCombo = await benchQuery(sparqlClient, sparqlUuid, { source: src, predicate: pred }, iterations, warmup)
    const baselineCombo = await benchQuery(baselineClient, baselineUuid, { source: src, predicate: pred }, iterations, warmup)
    console.log('done')
    tests.push({ name: 'Query by Source+Predicate', sparql: sparqlCombo, baseline: baselineCombo })

    // Query all
    process.stdout.write('    Query all links... ')
    const sparqlAll = await benchQuery(sparqlClient, sparqlUuid, {}, iterations, warmup)
    const baselineAll = await benchQuery(baselineClient, baselineUuid, {}, iterations, warmup)
    console.log('done')
    tests.push({ name: 'Query All Links', sparql: sparqlAll, baseline: baselineAll })

  } finally {
    await sparqlClient.removePerspective(sparqlUuid).catch(() => {})
    await baselineClient.removePerspective(baselineUuid).catch(() => {})
  }

  return { name: 'Point Queries', tests }
}
