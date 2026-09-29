import { ApiClient, WsEvent } from "./apiClient"

/**
 * Subscribe the channel's first handler. `ApiClient` keeps callbacks in a `Set`, so a
 * repeat call adds nothing, and a call after `closeAll()` subscribes again.
 * Returns the unsubscribe function on the first call for a channel, else `undefined`.
 */
export function subscribeChannel(
    apiClient: ApiClient,
    handlers: Map<string, (data: WsEvent) => void>,
    channel: string,
    handler: (data: WsEvent) => void,
): (() => void) | undefined {
    const existing = handlers.get(channel)
    if (existing) {
        apiClient.subscribe(existing)
        return undefined
    }
    handlers.set(channel, handler)
    return apiClient.subscribe(handler)
}
