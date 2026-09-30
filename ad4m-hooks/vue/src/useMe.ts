import { computed, effect, ref, shallowRef, watch } from "vue";
import { Ad4mClient, Agent, AgentStatus, LinkExpression } from "@coasys/ad4m";
import { agentFromWire } from "@coasys/hooks-helpers";

const status = shallowRef<AgentStatus>({ isInitialized: false, isUnlocked: false });
const agent = shallowRef<Agent | undefined>();
const isListening = shallowRef(false);
const profile = shallowRef<any | null>(null);

export function useMe<T>(client: Ad4mClient, formatter: (links: LinkExpression[]) => T) {
  effect(async () => {
    if (isListening.value) return;

    status.value = await client.agent.status();
    agent.value = await client.agent.me();

    isListening.value = true;

    client.on("agent-status-changed", ({ agent: s }) => {
      status.value = new AgentStatus(s);
    });

    client.on("agent-updated", ({ agent: a }) => {
      agent.value = agentFromWire(a);
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
