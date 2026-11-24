import type { ApiClient } from '../apiClient'
import type { Entry } from '../mirror/Entry'
import { PerspectiveHandle } from './PerspectiveHandle'

/**
 * The one `perspective.all` entry of a client's mirror, pushed: the executor's
 * `perspective-added`, `perspective-updated` and `perspective-removed` events change
 * the list, and so do the handles an own write returns ({@link keepPerspective},
 * {@link forgetPerspective}), so a reader never asks again. The handles are shared:
 * do not change them.
 */
export function perspectiveList(apiClient: ApiClient): Entry<PerspectiveHandle[]> {
    return apiClient.mirror.entry<PerspectiveHandle[]>({
        method: 'perspective.all',
        load: async () => (await apiClient.call('perspective.all', {})).map(PerspectiveHandle.fromWire),
        on: {
            'perspective-added': ({ perspective }, entry) => keep(entry, PerspectiveHandle.fromWire(perspective)),
            'perspective-updated': ({ perspective }, entry) => keep(entry, PerspectiveHandle.fromWire(perspective)),
            'perspective-removed': ({ perspectiveUuid }, entry) => forget(entry, perspectiveUuid),
        },
    })
}

/** The handle the list holds for a uuid, if it is fresh: what an own write compares against. */
export function heldPerspective(apiClient: ApiClient, uuid: string): PerspectiveHandle | undefined {
    const { state, value } = perspectiveList(apiClient).getSnapshot()
    return state === 'fresh' ? value?.find((h) => h.uuid === uuid) : undefined
}

/**
 * Keeps the handle an own update returned, unless an event replaced the handle in the list
 * since the call went out (`was` is what {@link heldPerspective} gave then): the event is
 * newer than the reply, which the executor may have computed before another update.
 */
export function keepUpdatedPerspective(apiClient: ApiClient, handle: PerspectiveHandle, was: PerspectiveHandle | undefined): void {
    if (heldPerspective(apiClient, handle.uuid) === was) keepPerspective(apiClient, handle)
}

/** Keeps a handle the executor returned from an own write: added, or replacing the one of its uuid. */
export function keepPerspective(apiClient: ApiClient, handle: PerspectiveHandle): void {
    keep(perspectiveList(apiClient), handle)
}

function keep(entry: Entry<PerspectiveHandle[]>, handle: PerspectiveHandle): void {
    change(entry, (list) => {
        const at = list.findIndex((h) => h.uuid === handle.uuid)
        return at < 0 ? [...list, handle] : list.map((h, i) => (i === at ? handle : h))
    })
}

/** Drops a perspective an own write removed. */
export function forgetPerspective(apiClient: ApiClient, uuid: string): void {
    forget(perspectiveList(apiClient), uuid)
}

function forget(entry: Entry<PerspectiveHandle[]>, uuid: string): void {
    change(entry, (list) => list.filter((h) => h.uuid !== uuid))
}

/**
 * Applies a change to the list held while it is fresh. Otherwise (no list yet, or one a
 * reconnect made stale) the change has no sound base: the entry is invalidated, so a
 * read in flight is not kept and the next read asks.
 */
function change(entry: Entry<PerspectiveHandle[]>, apply: (list: PerspectiveHandle[]) => PerspectiveHandle[]): void {
    const { state, value } = entry.getSnapshot()
    if (state === 'fresh' && value) entry.push(apply(value))
    else entry.invalidate()
}
