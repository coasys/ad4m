import { PubSub } from 'apollo-server'
export const AGENT_UPDATED = 'agent-updated-topic'
export const DIRECT_MESSAGE_RECEIVED = 'direct-message-received-topic'
export const PERSPECTIVE_ADDED_TOPIC = 'perspective-added-topic'
export const PERSPECTIVE_UPDATED_TOPIC = 'perspective-updated-topic'
export const PERSPECTIVE_REMOVED_TOPIC = 'perspective-removed-topic'
export const LINK_ADDED_TOPIC = 'link-added-topic'
export const LINK_REMOVED_TOPIC = 'link-removed-topic'
export const SIGNAL = "signal"
export const ERROR_OCCURRED_TOPIC = "error-occurred-topic"

const pubsub = new PubSub()

export function get() {
    return pubsub
}