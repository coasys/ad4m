/**
 * Seed generator — creates ~100k links in a Flux-shaped hierarchy.
 *
 * Hierarchy:
 *   Channel ──has_message──> Message (with body, reactions, replies, threads)
 *   Channel ──has_conversation──> Conversation ──has_subgroup──> ConversationSubgroup ──has_item──> Message
 *
 * Configurable via ScaleConfig. Default produces ~100k links.
 */
import type { LinkExpression } from './client'
import { createRng, generateId } from './utils'
import {
  ENTRY_TYPE,
  BODY,
  CHANNEL_NAME,
  CHANNEL_DESCRIPTION,
  CHANNEL_IS_CONVERSATION,
  CHANNEL_MESSAGE,
  CHANNEL_CONVERSATION,
  CONVERSATION_SUBGROUP,
  SUBGROUP_ITEM,
  HAS_REPLY,
  REACTION,
  MESSAGE_THREAD,
  FLUX_PARTICIPANT,
  EntryType,
} from './models/predicates'

export interface LinkInput {
  source: string
  predicate: string
  target: string
}

export interface ScaleConfig {
  numChannels: number
  messagesPerChannel: number
  conversationsPerChannel: number
  subgroupsPerConversation: number
  messagesPerSubgroup: number
  reactionsPerMessage: number
  repliesPerChannel: number
  threadsPerChannel: number
  threadDepth: number
  participantsPerConversation: number
}

export const DEFAULT_SCALE: ScaleConfig = {
  numChannels: 5,
  messagesPerChannel: 4000,
  conversationsPerChannel: 15,
  subgroupsPerConversation: 6,
  messagesPerSubgroup: 25,
  reactionsPerMessage: 3,
  repliesPerChannel: 400,
  threadsPerChannel: 80,
  threadDepth: 3,
  participantsPerConversation: 5,
}

export function estimateLinkCount(cfg: ScaleConfig): number {
  const msgs = cfg.numChannels * cfg.messagesPerChannel
  const convs = cfg.numChannels * cfg.conversationsPerChannel
  const subgroups = convs * cfg.subgroupsPerConversation
  const subgroupMsgs = subgroups * cfg.messagesPerSubgroup
  const replies = cfg.numChannels * cfg.repliesPerChannel
  const threads = cfg.numChannels * cfg.threadsPerChannel * cfg.threadDepth
  const reactions = Math.floor(msgs * 0.5) * cfg.reactionsPerMessage
  const participants = convs * cfg.participantsPerConversation

  const channelLinks = cfg.numChannels * 4
  const messageLinks = msgs * 3
  const convLinks = convs * 2
  const subgroupLinks = subgroups * 2
  const subgroupMsgLinks = subgroupMsgs * 3
  const reactionLinks = reactions
  const replyLinks = replies
  const threadLinks = threads
  const participantLinks = participants

  return (
    channelLinks + messageLinks + convLinks + subgroupLinks +
    subgroupMsgLinks + reactionLinks + replyLinks + threadLinks + participantLinks
  )
}

export interface SeedManifest {
  links: LinkInput[]
  channelIds: string[]
  messageIds: string[]
  conversationIds: string[]
  subgroupIds: string[]
  totalLinks: number
}

const EMOJIS = ['emoji://1f44d', 'emoji://2764', 'emoji://1f602', 'emoji://1f525', 'emoji://1f389']
const DIDS = Array.from({ length: 20 }, (_, i) => `did:test:participant${i}`)

export function generateSeedData(cfg: ScaleConfig = DEFAULT_SCALE, seed = 42): SeedManifest {
  const rng = createRng(seed)
  const links: LinkInput[] = []
  const channelIds: string[] = []
  const messageIds: string[] = []
  const conversationIds: string[] = []
  const subgroupIds: string[] = []

  for (let ch = 0; ch < cfg.numChannels; ch++) {
    const channelId = generateId(rng, 'channel')
    channelIds.push(channelId)

    links.push({ source: channelId, predicate: ENTRY_TYPE, target: EntryType.Channel })
    links.push({ source: channelId, predicate: CHANNEL_NAME, target: `literal://string:Channel ${ch}` })
    links.push({ source: channelId, predicate: CHANNEL_DESCRIPTION, target: `literal://string:Benchmark channel ${ch}` })
    links.push({ source: channelId, predicate: CHANNEL_IS_CONVERSATION, target: 'literal://boolean:false' })

    const channelMessageIds: string[] = []
    for (let m = 0; m < cfg.messagesPerChannel; m++) {
      const msgId = generateId(rng, 'msg')
      channelMessageIds.push(msgId)
      messageIds.push(msgId)

      links.push({ source: channelId, predicate: CHANNEL_MESSAGE, target: msgId })
      links.push({ source: msgId, predicate: ENTRY_TYPE, target: EntryType.Message })
      links.push({
        source: msgId,
        predicate: BODY,
        target: `literal://string:Message ${m} in channel ${ch} - ${generateId(rng, 'txt')}`,
      })

      if (rng() < 0.5) {
        for (let r = 0; r < cfg.reactionsPerMessage; r++) {
          const emoji = EMOJIS[Math.floor(rng() * EMOJIS.length)]
          links.push({ source: msgId, predicate: REACTION, target: emoji })
        }
      }
    }

    for (let r = 0; r < cfg.repliesPerChannel && channelMessageIds.length > 1; r++) {
      const fromIdx = Math.floor(rng() * channelMessageIds.length)
      let toIdx = Math.floor(rng() * channelMessageIds.length)
      if (toIdx === fromIdx) toIdx = (toIdx + 1) % channelMessageIds.length
      links.push({
        source: channelMessageIds[fromIdx],
        predicate: HAS_REPLY,
        target: channelMessageIds[toIdx],
      })
    }

    for (let t = 0; t < cfg.threadsPerChannel && channelMessageIds.length > 0; t++) {
      let parentIdx = Math.floor(rng() * channelMessageIds.length)
      for (let d = 0; d < cfg.threadDepth; d++) {
        const threadMsgId = generateId(rng, 'thread')
        messageIds.push(threadMsgId)
        links.push({
          source: channelMessageIds[parentIdx],
          predicate: MESSAGE_THREAD,
          target: threadMsgId,
        })
        links.push({ source: threadMsgId, predicate: ENTRY_TYPE, target: EntryType.Message })
        links.push({
          source: threadMsgId,
          predicate: BODY,
          target: `literal://string:Thread reply depth ${d}`,
        })
        channelMessageIds.push(threadMsgId)
        parentIdx = channelMessageIds.length - 1
      }
    }

    for (let c = 0; c < cfg.conversationsPerChannel; c++) {
      const convId = generateId(rng, 'conv')
      conversationIds.push(convId)

      links.push({ source: channelId, predicate: CHANNEL_CONVERSATION, target: convId })
      links.push({ source: convId, predicate: ENTRY_TYPE, target: EntryType.Conversation })

      for (let p = 0; p < cfg.participantsPerConversation; p++) {
        const did = DIDS[Math.floor(rng() * DIDS.length)]
        links.push({ source: convId, predicate: FLUX_PARTICIPANT, target: did })
      }

      for (let sg = 0; sg < cfg.subgroupsPerConversation; sg++) {
        const sgId = generateId(rng, 'sg')
        subgroupIds.push(sgId)

        links.push({ source: convId, predicate: CONVERSATION_SUBGROUP, target: sgId })
        links.push({ source: sgId, predicate: ENTRY_TYPE, target: EntryType.ConversationSubgroup })

        for (let sm = 0; sm < cfg.messagesPerSubgroup; sm++) {
          let itemId: string
          if (channelMessageIds.length > 0 && rng() < 0.7) {
            itemId = channelMessageIds[Math.floor(rng() * channelMessageIds.length)]
          } else {
            itemId = generateId(rng, 'sgmsg')
            messageIds.push(itemId)
            links.push({ source: itemId, predicate: ENTRY_TYPE, target: EntryType.Message })
            links.push({
              source: itemId,
              predicate: BODY,
              target: `literal://string:Subgroup msg ${sm}`,
            })
          }
          links.push({ source: sgId, predicate: SUBGROUP_ITEM, target: itemId })
        }
      }
    }
  }

  return {
    links,
    channelIds,
    messageIds,
    conversationIds,
    subgroupIds,
    totalLinks: links.length,
  }
}

/**
 * Seed an executor perspective with generated links via the GraphQL client.
 */
export async function seedPerspective(
  client: { addLinks(uuid: string, links: LinkInput[]): Promise<void> },
  perspectiveUuid: string,
  cfg: ScaleConfig = DEFAULT_SCALE,
  seed = 42,
  onProgress?: (inserted: number, total: number) => void,
): Promise<SeedManifest> {
  const manifest = generateSeedData(cfg, seed)
  const CHUNK = 500

  for (let i = 0; i < manifest.links.length; i += CHUNK) {
    const batch = manifest.links.slice(i, i + CHUNK)
    await client.addLinks(perspectiveUuid, batch)
    onProgress?.(Math.min(i + CHUNK, manifest.links.length), manifest.links.length)
  }

  return manifest
}
