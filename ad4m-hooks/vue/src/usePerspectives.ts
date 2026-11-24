import { computed, onScopeDispose, shallowRef, watch } from "vue";
import { Ad4mClient, PerspectiveHandle, PerspectiveProxy } from "@coasys/ad4m";
import { useLive } from "./useLive";

type UUID = string;

/**
 * The agent's perspectives, read from the client's mirror: the executor's
 * `perspective-added`, `-updated` and `-removed` events change the list, a reconnect
 * reloads it. Calls with one client share the list; two clients share nothing. Link
 * callbacks fire for every perspective listed.
 */
export function usePerspectives(client: Ad4mClient) {
  const snapshot = useLive(client.perspective.live.all());
  const perspectives = shallowRef<{ [x: UUID]: PerspectiveProxy }>({});
  const onAddedLinkCbs: Function[] = [];
  const onRemovedLinkCbs: Function[] = [];
  /** The proxy built for each listed handle, with the link listeners it carries. */
  let held = new Map<UUID, { handle: PerspectiveHandle; proxy: PerspectiveProxy }>();

  function addListeners(p: PerspectiveProxy) {
    p.on("link-added", ({ link }) => {
      onAddedLinkCbs.forEach((cb) => {
        cb(p, link);
      });
    });

    p.on("link-removed", ({ link }) => {
      onRemovedLinkCbs.forEach((cb) => {
        cb(p, link);
      });
    });

    // An update replaces a link: the old one leaves, the new one arrives.
    p.on("link-updated", ({ oldLink, newLink }) => {
      onRemovedLinkCbs.forEach((cb) => cb(p, oldLink));
      onAddedLinkCbs.forEach((cb) => cb(p, newLink));
    });
  }

  watch(
    () => snapshot.value.value,
    (handles) => {
      if (!handles) return;
      const next = new Map<UUID, { handle: PerspectiveHandle; proxy: PerspectiveProxy }>();
      for (const handle of handles) {
        const prev = held.get(handle.uuid);
        if (prev?.handle === handle) {
          next.set(handle.uuid, prev);
          continue;
        }
        prev?.proxy.dispose();
        const proxy = new PerspectiveProxy(handle, client.perspective);
        addListeners(proxy);
        next.set(handle.uuid, { handle, proxy });
      }
      for (const [uuid, { proxy }] of held) if (!next.has(uuid)) proxy.dispose();
      held = next;
      perspectives.value = Object.fromEntries([...next].map(([uuid, { proxy }]) => [uuid, proxy]));
    },
    { immediate: true }
  );

  // The listeners go with the calling effect scope.
  onScopeDispose(() => {
    held.forEach(({ proxy }) => proxy.dispose());
    held = new Map();
  });

  const neighbourhoods = computed(() =>
    Object.fromEntries(Object.entries(perspectives.value).filter(([, p]) => p.sharedUrl))
  );

  function onLinkAdded(cb: Function) {
    onAddedLinkCbs.push(cb);
  }

  function onLinkRemoved(cb: Function) {
    onRemovedLinkCbs.push(cb);
  }

  return { perspectives, neighbourhoods, onLinkAdded, onLinkRemoved };
}
