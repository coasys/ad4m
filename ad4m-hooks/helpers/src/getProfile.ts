import { Ad4mClient } from "@coasys/ad4m";
// @ts-ignore
import { getAd4mClient } from "@coasys/ad4m-connect/utils";
import { LinkExpression } from "@coasys/ad4m";

export interface Payload {
  url: string;
  perspectiveUuid: string;
}

export async function getProfile<T>(did: string, formatter?: (links: LinkExpression[]) => T): Promise<T | any> {
  const cleanedDid = did.replace("did://", "");
  const client: Ad4mClient = await getAd4mClient();

  const agentPerspective = await client.agent.byDID(cleanedDid);

  if (agentPerspective) {
    const links = agentPerspective!.perspective!.links;

    if (formatter) {
      return formatter(links);
    }

    return agentPerspective
  }
}
