import { computed, ref, shallowRef, watch } from "vue";
import { Agent, LinkExpression, AgentClient } from "@coasys/ad4m";



export function useAgent<T>(client: AgentClient, did: string | Function, formatter: (links: LinkExpression[]) => T) {
  const agent = shallowRef<Agent | null>(null);
  const profile = shallowRef<any | null>(null);
  const didRef = typeof did === "function" ? (did as any) : ref(did);

  watch(
    [client, didRef],
    async ([c, d]) => {
      if (d) {
        agent.value = await client.byDID(d);
        if (agent.value?.perspective) {
          const perspective = agent.value.perspective;
    
          const prof = formatter(perspective.links);
    
          profile.value =  { ...prof, did: d} as T;
        } else {
          profile.value =  null;
        }
      }
    },
    { immediate: true }
  );

  return { agent, profile };
}
