import { computed, onScopeDispose, ref, shallowRef, watch } from "vue";
import { LinkExpression, AgentClient, type Live, type Agent } from "@coasys/ad4m";

/**
 * The agent with this DID, read from the client's mirror: fresh for 5 minutes, refreshed
 * at once by an `agent-updated` event, read again after a reconnect. Two clients share nothing.
 */
export function useAgent<T>(client: AgentClient, did: string | Function, formatter: (links: LinkExpression[]) => T) {
  const didRef = typeof did === "function" ? (did as any) : ref(did);
  const snapshot = shallowRef<{ value?: Agent | null; error?: unknown }>({});

  let unsubscribe: (() => void) | undefined;
  const follow = (live: Live<Agent | null> | undefined) => {
    unsubscribe?.();
    unsubscribe = undefined;
    if (!live) {
      snapshot.value = {};
      return;
    }
    unsubscribe = live.subscribe(() => {
      snapshot.value = live.getSnapshot();
    });
    snapshot.value = live.getSnapshot();
  };
  watch(
    [() => client, didRef],
    ([c, d]) => follow(d ? (c as AgentClient).live.byDID(d as string) : undefined),
    { immediate: true }
  );
  onScopeDispose(() => unsubscribe?.());

  const agent = computed(() => snapshot.value.value ?? null);
  const profile = computed(() => {
    const perspective = agent.value?.perspective;
    return perspective ? ({ ...formatter(perspective.links), did: didRef.value } as T) : null;
  });
  const error = computed(() => (snapshot.value.error === undefined ? undefined : String(snapshot.value.error)));

  return { agent, profile, error };
}
