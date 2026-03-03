import type { Address, AgentService, PublicSharing, LanguageContext, LanguageLanguageInput} from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
//@ts-ignore
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from "./index.ts";

export class CloudflarePutAdapter implements PublicSharing {
  #agent: AgentService;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
  }

  async createPublic(language: LanguageLanguageInput): Promise<Address> {
    // @ts-ignore
    const hash = UTILS.hash(language.bundle.toString());

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
    try {
      //Save the meta information to the KV store
      const metaPostResult = await axiod.post(PROXY_URL, metaPostData);
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
      const bundlePostResult = await axiod.post(PROXY_URL, languageBundleBucketParams);
      if (bundlePostResult.status != 200) {
        console.error("Upload language bundle data gets error: ", metaPostResult);
      }

      return hash as Address;
    } catch (e) {

      if(e.response.status == 400 && e.response.data.includes("Key already exists")) {
        console.log("[Cloudflare-based Language Language]: Tried to replace existing language. Ignoring...")
        return hash as Address;
      }
      console.error("[Cloudflare-based Language Language]: Error storing Language: ", e.response.data);
      throw e
    }
  }
}
