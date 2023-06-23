import type { Address, AgentService, PublicSharing, LanguageContext, LanguageLanguageInput} from "@perspect3vism/ad4m";
import type { IPFS } from "ipfs-core-types";
import path from "path";
import fs from "fs";

export default function sleep(ms: number): Promise<any> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

export class PutAdapter implements PublicSharing {
  #agent: AgentService;
  #IPFS: IPFS;
  #storagePath: string;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
    this.#IPFS = context.IPFS;
    // @ts-ignore
    if ("storagePath" in context.customSettings) { this.#storagePath = context.customSettings["storagePath"] } else { this.#storagePath = "./src/tst-tmp/languages" };
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
    const metaPath = path.join(this.#storagePath, `meta-${hash}.json`);
    const bundlePath = path.join(this.#storagePath, `bundle-${hash}.js`);
    console.log("Writing meta & bundle path: ", metaPath, bundlePath);
    fs.writeFileSync(metaPath, JSON.stringify(expression));
    fs.writeFileSync(bundlePath, language.bundle.toString());

    return hash as Address;
  }
}
