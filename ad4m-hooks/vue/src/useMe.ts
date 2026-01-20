import { computed, effect, ref, shallowRef, watch } from "vue";
import { Agent, AgentStatus, LinkExpression, AgentClient } from "@coasys/ad4m";

const status = shallowRef<AgentStatus>({ isInitialized: false, isUnlocked: false });
const agent = shallowRef<Agent | undefined>();
const isListening = shallowRef(false);
const profile = shallowRef<any | null>(null);

export function useMe<T>(client: AgentClient, formatter: (links: LinkExpression[]) => T) {
  effect(async () => {
    if (isListening.value) return;

    status.value = await client.status();
    agent.value = await client.me();

    isListening.value = true;

    client.addAgentStatusChangedListener(async (s: AgentStatus) => {
      status.value = s;
    });

    client.addUpdatedListener(async (a: Agent) => {
      agent.value = a;
    });
  }, {});

  watch(
    () => agent.value,
    (newAgent) => {
      if (agent.value?.perspective) {
        const perspective = newAgent?.perspective;
        if (!perspective) return;
        profile.value = formatter(perspective.links);
      } else {
        profile.value = null;
      }
    },
    { immediate: true }
  )
  

  return { status, me: agent, profile };
}
