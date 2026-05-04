// Subject operations benchmark suite

import type { HttpBenchClient } from '../client'
import { timeIt, createRng, generateId, computeStats, type Stats } from '../utils'

export interface SubjectOperationsResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    baseline: Stats
  }>
}

// Create SHACL-conforming link patterns for a "Message" subject
async function createMessageInstance(
  client: HttpBenchClient,
  uuid: string,
  instanceId: string,
  author: string,
  content: string,
  timestamp: string,
): Promise<void> {
  await client.addLink(uuid, instanceId, 'rdf://type', 'ad4m://Message')
  await client.addLink(uuid, instanceId, 'ad4m://has-author', author)
  await client.addLink(uuid, instanceId, 'ad4m://has-content', `literal:string:${content}`)
  await client.addLink(uuid, instanceId, 'ad4m://has-timestamp', `literal:number:${timestamp}`)
}

async function benchFindAll(
  client: HttpBenchClient,
  uuid: string,
  iterations: number,
  warmup: number,
): Promise<Stats> {
  const query = { source: 'rdf://type', predicate: 'ad4m://Message' }
  // findAll approximated by querying all instances of a type via link query
  // Actually: query links where predicate = 'rdf://type' and target = 'ad4m://Message'
  for (let i = 0; i < warmup; i++) {
    await client.queryLinks(uuid, { predicate: 'rdf://type' })
  }
  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.queryLinks(uuid, { predicate: 'rdf://type' })
    samples.push(end())
  }
  return computeStats(samples)
}

async function benchGetInstance(
  client: HttpBenchClient,
  uuid: string,
  instanceId: string,
  iterations: number,
  warmup: number,
): Promise<Stats> {
  // Get all properties of an instance (hydration)
  for (let i = 0; i < warmup; i++) {
    await client.queryLinks(uuid, { source: instanceId })
  }
  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.queryLinks(uuid, { source: instanceId })
    samples.push(end())
  }
  return computeStats(samples)
}

async function benchPropertyUpdate(
  client: HttpBenchClient,
  uuid: string,
  instanceId: string,
  iterations: number,
  warmup: number,
  rng: () => number,
): Promise<Stats> {
  let prevLink = await client.addLink(uuid, instanceId, 'ad4m://has-content', `literal:string:pre-warmup`)
  for (let i = 0; i < warmup; i++) {
    await client.removeLink(uuid, prevLink)
    prevLink = await client.addLink(uuid, instanceId, 'ad4m://has-content', `literal:string:updated-warmup-${i}`)
  }
  const samples: number[] = []
  for (let i = 0; i < iterations; i++) {
    const end = timeIt()
    await client.removeLink(uuid, prevLink)
    prevLink = await client.addLink(uuid, instanceId, 'ad4m://has-content', `literal:string:updated-${i}`)
    samples.push(end())
  }
  return computeStats(samples)
}

export async function run(
  sparqlClient: HttpBenchClient,
  baselineClient: HttpBenchClient,
  iterations: number,
  warmup: number,
): Promise<SubjectOperationsResult> {
  const tests: SubjectOperationsResult['tests'] = []
  const INSTANCE_COUNT = 100
  const rng = createRng(42)

  const sparqlUuid = await sparqlClient.addPerspective('bench-subject-sparql')
  const baselineUuid = await baselineClient.addPerspective('bench-subject-baseline')

  const instanceIds: string[] = []

  try {
    process.stdout.write('    Creating subject instances... ')
    for (let i = 0; i < INSTANCE_COUNT; i++) {
      const id = generateId(rng, 'msg')
      instanceIds.push(id)
      await createMessageInstance(sparqlClient, sparqlUuid, id, `did:key:author-${i % 10}`, `Hello ${i}`, String(Date.now() + i))
      await createMessageInstance(baselineClient, baselineUuid, id, `did:key:author-${i % 10}`, `Hello ${i}`, String(Date.now() + i))
    }
    console.log('done')

    // findAll
    process.stdout.write('    findAll (type query)... ')
    const sparqlFindAll = await benchFindAll(sparqlClient, sparqlUuid, iterations, warmup)
    const baselineFindAll = await benchFindAll(baselineClient, baselineUuid, iterations, warmup)
    console.log('done')
    tests.push({ name: 'findAll (100 instances)', sparql: sparqlFindAll, baseline: baselineFindAll })

    // getInstance
    process.stdout.write('    getInstance (property hydration)... ')
    const targetId = instanceIds[0]
    const sparqlGetInstance = await benchGetInstance(sparqlClient, sparqlUuid, targetId, iterations, warmup)
    const baselineGetInstance = await benchGetInstance(baselineClient, baselineUuid, targetId, iterations, warmup)
    console.log('done')
    tests.push({ name: 'getInstance (hydration)', sparql: sparqlGetInstance, baseline: baselineGetInstance })

    // Property update
    process.stdout.write('    Property update... ')
    const sparqlUpdate = await benchPropertyUpdate(sparqlClient, sparqlUuid, targetId, iterations, warmup, createRng(99))
    const baselineUpdate = await benchPropertyUpdate(baselineClient, baselineUuid, targetId, iterations, warmup, createRng(99))
    console.log('done')
    tests.push({ name: 'Property Update', sparql: sparqlUpdate, baseline: baselineUpdate })

  } finally {
    await sparqlClient.removePerspective(sparqlUuid).catch(() => {})
    await baselineClient.removePerspective(baselineUuid).catch(() => {})
  }

  return { name: 'Subject Operations', tests }
}
