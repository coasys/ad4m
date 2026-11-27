import { computed } from "vue";
import { Ad4mClient, LinkExpression } from "@coasys/ad4m";
import { useLive } from "./useLive";

/**
 * The agent and its status, read from the client's mirror: `agent-updated` and
 * `agent-status-changed` replace them, a reconnect reloads them. Calls with one client
 * share the entries; two clients share nothing.
 */
export function useMe<T>(client: Ad4mClient, formatter: (links: LinkExpression[]) => T) {
  const statusSnapshot = useLive(client.agent.live.status());
  const meSnapshot = useLive(client.agent.live.me());

  const status = computed(() => statusSnapshot.value.value);
  const me = computed(() => meSnapshot.value.value);
  const profile = computed(() => {
    const perspective = me.value?.perspective;
    return perspective ? formatter(perspective.links) : null;
  });
  const error = computed(() => {
    const failure = meSnapshot.value.error ?? statusSnapshot.value.error;
    return failure === undefined ? undefined : String(failure);
  });

  return { status, me, profile, error };
}
