import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, AgentService } from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types"
import axios from "axios";
import https from "https";
import { PROXY_URL } from ".";

class NeighbourhoodPutAdapter implements PublicSharing {
  #agent: AgentService;
  #IPFS: IPFS

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
    this.#IPFS = context.IPFS;
  }

  async createPublic(neighbourhood: object): Promise<Address> {
    const ipfsAddress = await this.#IPFS.add(
      { content: JSON.stringify(neighbourhood)},
      { onlyHash: true},
    );
    // @ts-ignore
    const hash = ipfsAddress.cid.toString();

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
    const httpsAgent = new https.Agent({
      rejectUnauthorized: false
    });
    const neighbourhoodPostResult = await axios.post(PROXY_URL, neighbourhoodPostData, { httpsAgent });
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
      const getPresignedUrl = await axios.get(PROXY_URL+`?key=${cid}`);
      presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
      console.error("Get neighbourhood failed at getting presigned url", e);
    }

    let neighbourhoodObject;
    try {
      const getneighbourhoodObject = await axios.get(presignedUrl);
      neighbourhoodObject = getneighbourhoodObject.data;
    } catch (e) {
      console.error("Get meta information failed at getting meta information", e);
    }

    return neighbourhoodObject;
  }
}
