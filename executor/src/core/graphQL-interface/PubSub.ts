import { PubSub } from 'graphql-subscriptions'

export const AGENT_UPDATED = 'agent-updated-topic'
export const AGENT_STATUS_CHANGED = 'agent-status-changed-topic'
export const RUNTIME_MESSAGED_RECEIVED_TOPIC = 'runtime-messaged-received-topic'
export const PERSPECTIVE_ADDED_TOPIC = 'perspective-added-topic'
export const PERSPECTIVE_UPDATED_TOPIC = 'perspective-updated-topic'
export const PERSPECTIVE_REMOVED_TOPIC = 'perspective-removed-topic'
export const LINK_ADDED_TOPIC = 'perspective-link-added-topic'
export const LINK_REMOVED_TOPIC = 'perspective-link-removed-topic'
export const LINK_UPDATED_TOPIC = 'perspective-link-updated-topic'
export const SIGNAL = "signal"
export const EXCEPTION_OCCURRED_TOPIC = "exception-occurred-topic"
export const NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC = "neighbourhood-signal-received-topic"
export const PERSPECTIVE_SYNC_STATE_CHANGE = "perspective-sync-state-change"
export const APPS_CHANGED = "apps-changed"

const pubsub = new PubSub()

export function get() {
    return pubsub
}