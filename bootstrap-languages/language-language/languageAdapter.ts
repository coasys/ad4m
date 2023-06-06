import type { Address, LanguageAdapter, PublicSharing, LanguageContext, HolochainLanguageDelegate, LanguageExpression } from "@perspect3vism/ad4m";
import pako from "pako";
import { DNA_NICK } from "./dna";
import { LanguageStorage } from "./languageStorage";
import { LanguageStoragePutAdapter } from "./putAdapter";

export default class LangAdapter implements LanguageAdapter {
  putAdapter: PublicSharing;
  #DNA: HolochainLanguageDelegate;

  constructor(context: LanguageContext) {
    this.putAdapter = new LanguageStoragePutAdapter(context);
    this.#DNA = context.Holochain as HolochainLanguageDelegate;
  }
  async getLanguageSource(address: Address): Promise<string> {
     //Check the first two characters of address are equal to Qm
     if (address.substring(0, 2) != "Qm") {
      console.error("LanguageLanguage.get(): The address is not a valid hash");
      return null;
    }

    const storage = new LanguageStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language_storage", fn_name, payload));

    let addressBuffer = Buffer.from(address, 'hex');
    const expression = (await storage.getLanguageExpression(addressBuffer)) as LanguageExpression

    if (!expression) {
      console.error("LanguageLanguage.get(): Failed to fetch language");
      return null;
    };
    if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
        expression.data.data_base64 = "";
        console.error("LanguageLanguage.get(): Failed to fetch language");
        return null;
    };

    const data_compressed = await storage.download(expression.data.chunks_hashes);
    let data_stream = await data_compressed.arrayBuffer();

    const data_uncompressed = pako.inflate(data_stream);
    const buffer = Buffer.from(data_uncompressed)

    return buffer.toString("base64")
  }
}
