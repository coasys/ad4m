// Write throughput benchmark suite

import type { GraphQLClient } from '../client'
import { timeIt, createRng, generateId, type Stats, computeStats } from '../utils'

export interface WriteBenchmarkResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    surrealdb: Stats
  }>
}

async function benchSingleWrite(
  client: GraphQLClient,
  uuid: string,
  iterations: number,
  warmup: number,
  rng: () => number,
): Promise<Stats> {
  // Warmup
  for (let i = 0; i < warmup; i++) {
    await client.addLink(uuid, generateId(rng, 'warmup'), 'ad4m://test', generateId(rng, 'warmup'))
  }

  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.addLink(uuid, generateId(rng, 'src'), 'ad4m://test', generateId(rng, 'tgt'))
    samples.push(end())
  }
  return computeStats(samples)
}

async function benchBatchWrite(
  client: GraphQLClient,
  uuid: string,
  batchSize: number,
  iterations: number,
  warmup: number,
  rng: () => number,
): Promise<Stats> {
  // Warmup
  for (let w = 0; w < warmup; w++) {
    for (let i = 0; i < batchSize; i++) {
      await client.addLink(uuid, generateId(rng, 'warmup'), 'ad4m://test', generateId(rng, 'warmup'))
    }
  }

  const samples: number[] = []
  for (let iter = 0; iter < iterations; iter++) {
    const end = timeIt()
    const promises: Promise<unknown>[] = []
    for (let i = 0; i < batchSize; i++) {
      promises.push(client.addLink(uuid, generateId(rng, 'src'), 'ad4m://batch', generateId(rng, 'tgt')))
    }
    await Promise.all(promises)
    samples.push(end())
  }
  const stats = computeStats(samples)
  // Each iteration writes batchSize links, so adjust ops/sec accordingly
  stats.opsPerSec = stats.opsPerSec * batchSize
  return stats
}

export async function run(
  sparqlClient: GraphQLClient,
  surrealClient: GraphQLClient,
  iterations: number,
  warmup: number,
): Promise<WriteBenchmarkResult> {
  const tests: WriteBenchmarkResult['tests'] = []
  const rng = createRng(42)

  const sparqlUuid = await sparqlClient.addPerspective('bench-write-sparql')
  const surrealUuid = await surrealClient.addPerspective('bench-write-surreal')

  try {
    // Single link add
    process.stdout.write('    Single Link Add... ')
    const sparqlSingle = await benchSingleWrite(sparqlClient, sparqlUuid, iterations, warmup, createRng(100))
    const surrealSingle = await benchSingleWrite(surrealClient, surrealUuid, iterations, warmup, createRng(100))
    console.log('done')
    tests.push({ name: 'Single Link Add', sparql: sparqlSingle, surrealdb: surrealSingle })

    // Batch sizes
    for (const batchSize of [10, 100, 1000]) {
      const batchIter = batchSize >= 100 ? Math.max(1, Math.floor(iterations / 10)) : iterations
      process.stdout.write(`    Batch ${batchSize} Links... `)
      const sparqlBatch = await benchBatchWrite(sparqlClient, sparqlUuid, batchSize, batchIter, Math.min(warmup, 2), createRng(200 + batchSize))
      const surrealBatch = await benchBatchWrite(surrealClient, surrealUuid, batchSize, batchIter, Math.min(warmup, 2), createRng(200 + batchSize))
      console.log('done')
      tests.push({ name: `Batch ${batchSize} Links`, sparql: sparqlBatch, surrealdb: surrealBatch })
    }
  } finally {
    await sparqlClient.removePerspective(sparqlUuid).catch(() => {})
    await surrealClient.removePerspective(surrealUuid).catch(() => {})
  }

  return { name: 'Write Throughput', tests }
}
