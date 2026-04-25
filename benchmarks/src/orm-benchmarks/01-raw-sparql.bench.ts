/**
 * Spec 1: Raw SPARQL benchmarks — headless, no browser.
 */
import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import { setupExecutor, teardownExecutor } from '../orm-setup'
import type { OrmExecutorContext } from '../orm-setup'
import { seedPerspective, DEFAULT_SCALE } from '../seed'
import type { SeedManifest } from '../seed'
import { measure } from '../utils'
import { writeOrmReport, formatOrmMarkdownTable } from '../orm-reporter'
import type { OrmBenchmarkResult } from '../orm-reporter'
import {
  ENTRY_TYPE, BODY, CHANNEL_MESSAGE, CHANNEL_CONVERSATION,
  CONVERSATION_SUBGROUP, SUBGROUP_ITEM, HAS_REPLY, REACTION,
  MESSAGE_THREAD, FLUX_PARTICIPANT, EntryType,
} from '../models/predicates'

const ITERATIONS = 10
const results: OrmBenchmarkResult[] = []

function record(name: string, samples: number[], stats: any) {
  results.push({ name, spec: '01-raw-sparql', transport: 'graphql', samples, stats })
}

let executor: OrmExecutorContext
let manifest: SeedManifest | null = null

async function ensureSeeded() {
  if (manifest) return manifest
  console.log('Seeding perspective with ~100k links...')
  manifest = await seedPerspective(executor.client, executor.perspectiveUuid, DEFAULT_SCALE, 42, (inserted, total) => {
    if (inserted % 10000 === 0 || inserted === total) console.log(`  Seeded ${inserted}/${total} links`)
  })
  console.log(`Seeding complete: ${manifest.totalLinks} links inserted`)
  return manifest
}

beforeAll(async () => { executor = await setupExecutor() })
afterAll(async () => {
  if (results.length > 0) {
    const filepath = await writeOrmReport(results, manifest?.totalLinks ?? 0)
    console.log(`\nResults written to: ${filepath}`)
    console.log('\n' + formatOrmMarkdownTable(results))
  }
  await teardownExecutor()
})

describe('Raw SPARQL @ 100k links', () => {
  test('1. SELECT all messages in a channel', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?msg ?body WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?msg .
      ?msg <${ENTRY_TYPE}> <${EntryType.Message}> .
      ?msg <${BODY}> ?body .
    }`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('SELECT messages in channel', samples, stats)
  })

  test('2. COUNT messages in a channel', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT (COUNT(DISTINCT ?msg) AS ?count) WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?msg .
      ?msg <${ENTRY_TYPE}> <${EntryType.Message}> .
    }`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('COUNT messages in channel', samples, stats)
  })

  test('3. Multi-join: channel → conversation → subgroups', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?conv ?sg ?sgItem WHERE {
      <${channelId}> <${CHANNEL_CONVERSATION}> ?conv .
      ?conv <${CONVERSATION_SUBGROUP}> ?sg .
      ?sg <${SUBGROUP_ITEM}> ?sgItem .
    }`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('Multi-join: ch→conv→sg→items', samples, stats)
  })

  test('4. OPTIONAL joins (messages with optional reactions)', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?msg ?body ?reaction WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?msg .
      ?msg <${BODY}> ?body .
      OPTIONAL { ?msg <${REACTION}> ?reaction . }
    } LIMIT 500`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('OPTIONAL: msgs + reactions', samples, stats)
  })

  test('5. FILTER with IN clause', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?item ?type WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?item .
      ?item <${ENTRY_TYPE}> ?type .
      FILTER(?type IN (<${EntryType.Message}>, <${EntryType.Post}>, <${EntryType.Task}>))
    }`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('FILTER IN (entry types)', samples, stats)
  })

  test('6. Conversation participants (multi-hop)', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT DISTINCT ?conv ?participant WHERE {
      <${channelId}> <${CHANNEL_CONVERSATION}> ?conv .
      ?conv <${FLUX_PARTICIPANT}> ?participant .
    }`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('Conversation participants', samples, stats)
  })

  test('7. Thread depth traversal', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?msg ?threadReply ?body WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?msg .
      ?msg <${MESSAGE_THREAD}> ?threadReply .
      ?threadReply <${BODY}> ?body .
    } LIMIT 200`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('Thread depth traversal', samples, stats)
  })

  test('8. Full channel load (allItems pattern)', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const query = `SELECT ?id ?type ?body WHERE {
      <${channelId}> <${CHANNEL_MESSAGE}> ?id .
      ?id <${ENTRY_TYPE}> ?type .
      FILTER(?type IN (<${EntryType.Message}>, <${EntryType.Post}>, <${EntryType.Task}>))
      OPTIONAL { ?id <${BODY}> ?body . }
    } ORDER BY ?id`
    const { samples, stats } = await measure(async () => {
      const result = await executor.client.querySparql(executor.perspectiveUuid, query)
      expect(result).toBeTruthy()
    }, ITERATIONS)
    record('Full channel load (allItems)', samples, stats)
  })
})
