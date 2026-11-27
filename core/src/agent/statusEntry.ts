import type { ApiClient } from '../apiClient'
import type { Entry } from '../mirror/Entry'
import { AgentStatus } from './AgentStatus'

/**
 * The one `agent.status` entry of a client's mirror, pushed: `agent-status-changed` and
 * the results of `agent.unlock/lock/generate/import` replace it, and a reconnect makes the
 * next read ask the executor again. Concurrent reads share one request. The value is
 * shared: do not change it.
 */
export function agentStatusEntry(apiClient: ApiClient): Entry<AgentStatus> {
    return apiClient.mirror.entry<AgentStatus>({
        method: 'agent.status',
        load: async () => new AgentStatus(await apiClient.call('agent.status', {})),
        on: { 'agent-status-changed': (event, entry) => entry.push(new AgentStatus(event.agent)) },
    })
}

/**
 * The agent's own DID from {@link agentStatusEntry}, or `null` while the executor reports
 * none. A status with no DID is not kept: the next call asks again.
 */
export async function agentDid(apiClient: ApiClient): Promise<string | null> {
    const entry = agentStatusEntry(apiClient)
    const did = (await entry.read()).did || null
    if (!did) entry.invalidate()
    return did
}
