import { PubSub } from 'apollo-server'
export const AGENT_UPDATED = 'agent-updated-topic'
export const PERSPECTIVE_ADDED_TOPIC = 'perspective-added-topic'
export const PERSPECTIVE_UPDATED_TOPIC = 'perspective-updated-topic'
export const PERSPECTIVE_REMOVED_TOPIC = 'perspective-removed-topic'
export const LINK_ADDED_TOPIC = 'link-added-topic'
export const LINK_REMOVED_TOPIC = 'link-removed-topic'

const pubsub = new PubSub()

export function get() {
    return pubsub
}