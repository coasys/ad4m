import type { ApiClient } from '../apiClient'
import type { Entry } from '../mirror/Entry'
import { PerspectiveHandle } from './PerspectiveHandle'

/**
 * The one `perspective.all` entry of a client's mirror, `pushed`: the executor's
 * `perspective-added`, `perspective-updated` and `perspective-removed` events change
 * the list, and so do the handles an own write returns ({@link keepPerspective},
 * {@link forgetPerspective}), so a reader never asks again. The handles are shared:
 * do not change them.
 */
export function perspectiveList(apiClient: ApiClient): Entry<PerspectiveHandle[]> {
    listen(apiClient)
    return apiClient.mirror.entry<PerspectiveHandle[]>({
        scope: 'agent',
        kind: 'pushed',
        method: 'perspective.all',
        params: {},
        load: async () => (await apiClient.call('perspective.all', {})).map(PerspectiveHandle.fromWire),
    })
}

/** Keeps a handle the executor returned from an own write: added, or replacing the one of its uuid. */
export function keepPerspective(apiClient: ApiClient, handle: PerspectiveHandle): void {
    change(apiClient, (list) => {
        const at = list.findIndex((h) => h.uuid === handle.uuid)
        return at < 0 ? [...list, handle] : list.map((h, i) => (i === at ? handle : h))
    })
}

/** Drops a perspective an own write removed. */
export function forgetPerspective(apiClient: ApiClient, uuid: string): void {
    change(apiClient, (list) => list.filter((h) => h.uuid !== uuid))
}

/**
 * Applies a change to the list held while it is fresh. Otherwise (no list yet, or one a
 * reconnect made stale) the change has no sound base: the entry is invalidated, so a
 * read in flight is not kept and the next read asks.
 */
function change(apiClient: ApiClient, apply: (list: PerspectiveHandle[]) => PerspectiveHandle[]): void {
    const entry = perspectiveList(apiClient)
    const { state, value } = entry.getSnapshot()
    if (state === 'fresh' && value) entry.push(apply(value))
    else entry.invalidate()
}

const listening = new WeakMap<ApiClient, true>()

/** Subscribes the client to the three events, once. */
function listen(apiClient: ApiClient): void {
    if (listening.has(apiClient)) return
    listening.set(apiClient, true)
    apiClient.on('perspective-added', ({ perspective }) => keepPerspective(apiClient, PerspectiveHandle.fromWire(perspective)))
    apiClient.on('perspective-updated', ({ perspective }) => keepPerspective(apiClient, PerspectiveHandle.fromWire(perspective)))
    apiClient.on('perspective-removed', ({ perspectiveUuid }) => forgetPerspective(apiClient, perspectiveUuid))
}
