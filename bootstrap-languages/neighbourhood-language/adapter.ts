import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from "./index.ts";

class NeighbourhoodPutAdapter implements PublicSharing {
  #agent: AgentService;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
  }

  async createPublic(neighbourhood: object): Promise<Address> {
    // @ts-ignore
    const hash = UTILS.hash(JSON.stringify(neighbourhood));
    const agent = this.#agent;
    const expression = agent.createSignedExpression(neighbourhood);

    //Build the key value object for the neighbourhood object
    const key = hash;
    const neighbourhoodPostData = {
      key: key,
      // Content of the new object.
      value: JSON.stringify(expression),
    };
    //Save the neighbourhood information to the KV store
    const neighbourhoodPostResult = await axiod.post(PROXY_URL, neighbourhoodPostData);
    if (neighbourhoodPostResult.status != 200) {
      console.error("Upload neighbourhood data gets error: ", neighbourhoodPostResult);
    }

    return hash as Address;
  }
}

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;

  constructor(context: LanguageContext) {
    this.putAdapter = new NeighbourhoodPutAdapter(context);
  }

  async get(address: Address): Promise<Expression> {
    const cid = address.toString();

    let presignedUrl;
    try {
      const getPresignedUrl = await axiod.get(PROXY_URL+`?key=${cid}`);
      presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
      console.error("Get neighbourhood failed at getting presigned url", e);
    }

    let neighbourhoodObject;
    try {
      const getneighbourhoodObject = await axiod.get(presignedUrl);
      neighbourhoodObject = getneighbourhoodObject.data;
    } catch (e) {
      console.error("Get meta information failed at getting meta information", e);
    }

    return neighbourhoodObject;
  }
}
