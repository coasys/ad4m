import type { Address, AgentService, PublicSharing, LanguageContext, LanguageLanguageInput} from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types"
import axios from "axios";
import https from "https";
import { PROXY_URL } from ".";

export class CloudflarePutAdapter implements PublicSharing {
  #agent: AgentService;
  #IPFS: IPFS;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
    this.#IPFS = context.IPFS;
  }

  async createPublic(language: LanguageLanguageInput): Promise<Address> {
    const ipfsAddress = await this.#IPFS.add(
      { content: language.bundle.toString()},
      { onlyHash: true},
    );
    // @ts-ignore
    const hash = ipfsAddress.cid.toString();

    if(hash != language.meta.address)
      throw new Error(`Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${hash}`)

    const agent = this.#agent;
    const expression = agent.createSignedExpression(language.meta);

    //Build the key value object for the meta object
    const key = `meta-${hash}`;
    const metaPostData = {
      key: key,
      // Content of the new object.
      value: JSON.stringify(expression),
    };
    //Save the meta information to the KV store
    const httpsAgent = new https.Agent({
      rejectUnauthorized: false
    });
    const metaPostResult = await axios.post(PROXY_URL, metaPostData, { httpsAgent });
    if (metaPostResult.status != 200) {
      console.error("Upload language meta data gets error: ", metaPostResult);
    }

    //Build the key value object for the language bundle
    const languageBundleBucketParams = {
      key: hash,
      // Content of the new object.
      value: language.bundle.toString(),
    };
    //Save the language bundle to the KV store
    const bundlePostResult = await axios.post(PROXY_URL, languageBundleBucketParams, { httpsAgent });
    if (bundlePostResult.status != 200) {
      console.error("Upload language bundle data gets error: ", metaPostResult);
    }

    return hash as Address;
  }
}
