import { useCallback, useSyncExternalStore } from "react";
import type { Live, MirrorSnapshot } from "@coasys/ad4m";

/**
 * The snapshot of one of the client's mirror entries (`client.agent.live.me()` and the
 * like), re-rendering whenever it changes. Subscribing makes an entry that is not fresh load.
 * Pass a view that stays the same between renders (memoize `byDID(did)`). `undefined`
 * while there is no client.
 */
export function useLive<T>(live: Live<T> | undefined): MirrorSnapshot<T> | undefined {
  const subscribe = useCallback(
    (onChange: () => void) => (live ? live.subscribe(() => onChange()) : () => {}),
    [live]
  );
  const getSnapshot = useCallback(() => live?.getSnapshot(), [live]);
  return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}
