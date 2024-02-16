import { computed, ref, shallowRef, watch } from "vue";
import { Agent, LinkExpression } from "@coasys/ad4m";
import { AgentClient } from "@coasys/ad4m";



export function useAgent<T>(client: AgentClient, did: string | Function, formatter: (links: LinkExpression[]) => T) {
  const agent = shallowRef<Agent | null>(null);
  const profile = shallowRef<any | null>(null);
  const didRef = typeof did === "function" ? (did as any) : ref(did);

  watch(
    [client, didRef],
    async ([c, d]) => {
      console.log('meow', c, d)
      if (d) {
        console.log('meow', d)
        agent.value = await client.byDID(d);
        console.log('meow 0', agent)
        if (agent.value?.perspective) {
          console.log('meow 1')
          const perspective = agent.value.perspective;
    
          console.log("perspective", perspective);
        
          const prof = formatter(perspective.links);
    
          console.log("prof", prof, { ...prof, did: d});
    
    
          profile.value =  { ...prof, did: d} as T;
        } else {
          console.log('meow 2')
          profile.value =  null;
        }
      }
    },
    { immediate: true }
  );

  return { agent, profile };
}
