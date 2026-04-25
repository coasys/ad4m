/**
 * Spec 3: Custom SPARQL getters — exercises real model instance methods.
 */
import { describe, test, expect, beforeAll, afterAll } from 'vitest'
import { setupExecutor, teardownExecutor } from '../orm-setup'
import type { OrmExecutorContext } from '../orm-setup'
import { seedPerspective, DEFAULT_SCALE } from '../seed'
import type { SeedManifest } from '../seed'
import { measure } from '../utils'
import { writeOrmReport, formatOrmMarkdownTable } from '../orm-reporter'
import type { OrmBenchmarkResult } from '../orm-reporter'
import { Channel } from '../models/channel'
import { Message } from '../models/message'
import { Conversation } from '../models/conversation'
import { ConversationSubgroup } from '../models/conversation-subgroup'

const ITERATIONS = 10
const results: OrmBenchmarkResult[] = []

function record(name: string, samples: number[], stats: any) {
  results.push({ name, spec: '03-custom-getters', transport: 'graphql', samples, stats })
}

let executor: OrmExecutorContext
let manifest: SeedManifest | null = null
let modelsRegistered = false

async function ensureSeeded() {
  if (manifest) return manifest
  console.log('Seeding perspective for custom getter benchmarks...')
  manifest = await seedPerspective(executor.client, executor.perspectiveUuid, DEFAULT_SCALE, 42, (inserted, total) => {
    if (inserted % 10000 === 0 || inserted === total) console.log(`  Seeded ${inserted}/${total} links`)
  })
  console.log(`Seeding complete: ${manifest.totalLinks} links`)

  if (!modelsRegistered) {
    console.log('Registering Ad4mModel classes...')
    await Channel.register(executor.perspective)
    await Message.register(executor.perspective)
    await Conversation.register(executor.perspective)
    await ConversationSubgroup.register(executor.perspective)
    modelsRegistered = true
    console.log('  ✓ Models registered')
  }
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

describe('Custom SPARQL getters @ 100k links', () => {
  test('1. Channel.allItems() — instance method via SDK', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const { samples, stats } = await measure(async () => {
      const ch = new Channel(executor.perspective, channelId)
      const items = await ch.allItems()
      expect(items).toBeTruthy()
    }, ITERATIONS)
    record('Channel.allItems()', samples, stats)
  })

  test('2. Channel.totalItemCount() — instance method via SDK', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const { samples, stats } = await measure(async () => {
      const ch = new Channel(executor.perspective, channelId)
      const count = await ch.totalItemCount()
      expect(count).toBeGreaterThan(0)
    }, ITERATIONS)
    record('Channel.totalItemCount()', samples, stats)
  })

  test('3. Channel.unprocessedItems() — set-difference via SDK', async () => {
    const m = await ensureSeeded()
    const channelId = m.channelIds[0]
    const { samples, stats } = await measure(async () => {
      const ch = new Channel(executor.perspective, channelId)
      const items = await ch.unprocessedItems()
      expect(items).toBeDefined()
    }, ITERATIONS)
    record('Channel.unprocessedItems()', samples, stats)
  })

  test('4. ConversationSubgroup.itemsData() — instance method via SDK', async () => {
    const m = await ensureSeeded()
    const sgId = m.subgroupIds[0]
    const { samples, stats } = await measure(async () => {
      const sg = new ConversationSubgroup(executor.perspective, sgId)
      const items = await sg.itemsData()
      expect(items).toBeTruthy()
    }, ITERATIONS)
    record('Subgroup.itemsData()', samples, stats)
  })

  test('5. Message.replyingTo — getter evaluated via ORM .get()', async () => {
    const m = await ensureSeeded()
    const msgId = m.messageIds[0]
    const { samples, stats } = await measure(async () => {
      const msg = new Message(executor.perspective, msgId)
      await msg.get()
    }, ITERATIONS)
    record('Message.get() with replyingTo getter', samples, stats)
  })

  test('6. Conversation.stats() — custom method (SPARQL + get)', async () => {
    const m = await ensureSeeded()
    const convId = m.conversationIds[0]
    const { samples, stats } = await measure(async () => {
      const conv = new Conversation(executor.perspective, convId)
      const s = await conv.stats()
      expect(s.totalSubgroups).toBeGreaterThan(0)
    }, ITERATIONS)
    record('Conversation.stats()', samples, stats)
  })

  test('7. ConversationSubgroup.stats() — custom method', async () => {
    const m = await ensureSeeded()
    const sgId = m.subgroupIds[0]
    const { samples, stats } = await measure(async () => {
      const sg = new ConversationSubgroup(executor.perspective, sgId)
      const s = await sg.stats()
      expect(s.totalItems).toBeGreaterThan(0)
    }, ITERATIONS)
    record('Subgroup.stats()', samples, stats)
  })
})
