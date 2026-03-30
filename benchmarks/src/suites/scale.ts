// Scale benchmark suite — test performance at different perspective sizes

import type { GraphQLClient } from '../client'
import { timeIt, createRng, generateId, computeStats, type Stats } from '../utils'

export interface ScaleResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    baseline: Stats
  }>
}

async function populateAndBench(
  client: GraphQLClient,
  targetCount: number,
  queryIterations: number,
  warmup: number,
  rng: () => number,
): Promise<{ writeStats: Stats; queryBySourceStats: Stats; queryAllStats: Stats }> {
  const uuid = await client.addPerspective(`bench-scale-${targetCount}`)
  const sources: string[] = []

  try {
    // Write phase
    const writeSamples: number[] = []
    for (let i = 0; i < targetCount; i++) {
      const src = generateId(rng, 'src')
      const pred = 'ad4m://has-data'
      const tgt = generateId(rng, 'tgt')
      const end = timeIt()
      await client.addLink(uuid, src, pred, tgt)
      writeSamples.push(end())
      if (i % 100 === 0) sources.push(src)
    }
    const writeStats = computeStats(writeSamples)

    // Query phase
    const querySource = sources[Math.floor(sources.length / 2)] || sources[0]

    // Warmup
    for (let i = 0; i < warmup; i++) {
      await client.queryLinks(uuid, { source: querySource })
    }

    const queryBySourceSamples: number[] = []
    for (let i = 0; i < queryIterations; i++) {
      const end = timeIt()
      await client.queryLinks(uuid, { source: querySource })
      queryBySourceSamples.push(end())
    }
    const queryBySourceStats = computeStats(queryBySourceSamples)

    const queryAllSamples: number[] = []
    for (let i = 0; i < Math.min(queryIterations, 10); i++) {
      const end = timeIt()
      await client.queryLinks(uuid, {})
      queryAllSamples.push(end())
    }
    const queryAllStats = computeStats(queryAllSamples)

    return { writeStats, queryBySourceStats, queryAllStats }
  } finally {
    await client.removePerspective(uuid).catch(() => {})
  }
}

export async function run(
  sparqlClient: GraphQLClient,
  baselineClient: GraphQLClient,
  iterations: number,
  warmup: number,
  maxScale: number,
): Promise<ScaleResult> {
  const tests: ScaleResult['tests'] = []
  const scales = [100, 1000, 10000].filter(s => s <= maxScale)

  for (const scale of scales) {
    const queryIter = Math.min(iterations, scale >= 10000 ? 10 : 50)
    process.stdout.write(`    Scale ${scale.toLocaleString()} links... `)

    const sparqlResult = await populateAndBench(sparqlClient, scale, queryIter, warmup, createRng(scale))
    const baselineResult = await populateAndBench(baselineClient, scale, queryIter, warmup, createRng(scale))
    console.log('done')

    tests.push({
      name: `Write ${scale.toLocaleString()} links`,
      sparql: sparqlResult.writeStats,
      baseline: baselineResult.writeStats,
    })
    tests.push({
      name: `Query by source @ ${scale.toLocaleString()}`,
      sparql: sparqlResult.queryBySourceStats,
      baseline: baselineResult.queryBySourceStats,
    })
    tests.push({
      name: `Query all @ ${scale.toLocaleString()}`,
      sparql: sparqlResult.queryAllStats,
      baseline: baselineResult.queryAllStats,
    })
  }

  return { name: 'Scale Tests', tests }
}
