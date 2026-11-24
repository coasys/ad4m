import { onScopeDispose, shallowRef, type ShallowRef } from "vue";
import type { Live, MirrorSnapshot } from "@coasys/ad4m";

/**
 * The snapshot of one of the client's mirror entries (`client.agent.live.me()` and the
 * like) as a ref that changes whenever the entry does. Subscribing makes an entry that is
 * not fresh load; the subscription ends with the calling effect scope.
 */
export function useLive<T>(live: Live<T>): ShallowRef<MirrorSnapshot<T>> {
  const snapshot = shallowRef(live.getSnapshot());
  const unsubscribe = live.subscribe(() => {
    snapshot.value = live.getSnapshot();
  });
  // Changed between getSnapshot() and subscribe().
  snapshot.value = live.getSnapshot();
  onScopeDispose(unsubscribe);
  return snapshot;
}
