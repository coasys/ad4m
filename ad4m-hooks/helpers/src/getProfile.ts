import { AgentClient } from "@coasys/ad4m";
import { LinkExpression } from "@coasys/ad4m";

export async function getProfile<T>(agent: AgentClient, did: string, formatter?: (links: LinkExpression[]) => T): Promise<T | any> {
  const cleanedDid = did.replace("did://", "");

  const agentPerspective = await agent.byDID(cleanedDid);

  if (agentPerspective) {
    const links = agentPerspective!.perspective!.links;

    if (formatter) {
      return formatter(links);
    }

    return agentPerspective
  }
}
