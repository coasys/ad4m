import type { Address, AgentService, PublicSharing, LanguageContext, LanguageLanguageInput} from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
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


export class LanguageStoragePutAdapter implements PublicSharing {
    #agent: AgentService
    #DNA: HolochainLanguageDelegate;
    #IPFS: IPFS;

    constructor(context: LanguageContext) {
        this.#agent = context.agent;
        this.#DNA = context.Holochain as HolochainLanguageDelegate;
        // @ts-ignore
        this.#IPFS = context.IPFS;
    }

    async createPublic(language: LanguageLanguageInput): Promise<Address> {
        const ipfsAddress = await this.#IPFS.add(
            { content: language.bundle.toString()},
            { onlyHash: true},
        );
        // @ts-ignore
        const hash = ipfsAddress.cid.toString();

        if(hash != language.meta.address) {
            throw new Error(`Language Persistence: Can't store language. Address stated in meta differs from actual file\nWanted: ${language.meta.address}\nGot: ${hash}`)
        }

        //console.log("createPublic fileData", language)
        try {
            // Just in case...
            if(typeof language === "string"){
                //@ts-ignore
                fileData = JSON.parse(fileData)
            }
        }catch(e){}

        const storage = new LanguageStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language_storage", fn_name, payload));

        const data_uncompressed = Uint8Array.from(Buffer.from(language.bundle.toString()));
        const data_compressed = pako.deflate(data_uncompressed)
        const blob = new Blob([data_compressed])

        const hashes = await storage.upload(blob);

        const fileMetadata = {
            name: language.meta.name,
            description: language.meta.description,
            address: language.meta.address,
            chunks_hashes: hashes,
            size: data_uncompressed.length,
        } as LanguageMetadata

        //Create the signed expression object
        const expression: LanguageExpression = this.#agent.createSignedExpression(fileMetadata)
        //Remove the data_base64 from the expression, since this is already stored above
        delete expression.data.data_base64;

        //Store the FileMetadataExpression
        await storage.storeLanguageExpression(expression)

        //@ts-ignore
        return hash
    }
}