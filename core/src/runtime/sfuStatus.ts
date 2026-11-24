import type { ApiClient } from '../apiClient'
import type { Entry } from '../mirror/Entry'
import type { SfuStatus } from '../generated/api/SfuStatus'

/**
 * The one `sfu.status` entry of a client's mirror. It is fixed for an executor process, so
 * the value is kept and read again after a reconnect; an answer sent before a reconnect
 * is used once and not kept. A failure (the SFU is not running yet) is not kept: the next
 * read asks again.
 */
export function sfuStatusEntry(apiClient: ApiClient): Entry<SfuStatus> {
    return apiClient.mirror.entry<SfuStatus>({
        scope: 'executor',
        method: 'sfu.status',
        params: {},
        load: () => apiClient.call('sfu.status', {}),
    })
}
