import { PubSub } from 'graphql-subscriptions'

export const AGENT_UPDATED = 'agent-updated-topic'
export const AGENT_STATUS_CHANGED = 'agent-status-changed-topic'
export const DIRECT_MESSAGE_RECEIVED = 'direct-message-received-topic'
export const PERSPECTIVE_ADDED_TOPIC = 'perspective-added-topic'
export const PERSPECTIVE_UPDATED_TOPIC = 'perspective-updated-topic'
export const PERSPECTIVE_REMOVED_TOPIC = 'perspective-removed-topic'
export const LINK_ADDED_TOPIC = 'link-added-topic'
export const LINK_REMOVED_TOPIC = 'link-removed-topic'
export const LINK_UPDATED_TOPIC = 'link-updated-topic'
export const SIGNAL = "signal"
export const EXCEPTION_OCCURRED_TOPIC = "exception-occurred-topic"
export const NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC = "neighbourhood-signal-received-topic"
export const PERSPECTIVE_SYNC_STATE_CHANGE = "perspective-sync-state-change"

const pubsub = new PubSub()

export function get() {
    return pubsub
}