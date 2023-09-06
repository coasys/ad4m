import type { Address, LanguageAdapter, LanguageContext } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
//@ts-ignore
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from "./index.ts";

export default class LangAdapter implements LanguageAdapter {
  constructor(context: LanguageContext) {
    this.putAdapter = new LanguageStoragePutAdapter(context);
    this.#DNA = context.Holochain as HolochainLanguageDelegate;
  }
  async getLanguageSource(address: Address): Promise<string> {
    //Check the first two characters of address are equal to Qm
    if (address.substring(0, 2) != "Qm") {
      console.error("LanguageLanguage.getLanguageSource(): The address is not a valid hash");
      return "";
    }
    const cid = address.toString();

    let presignedUrl;
    try {
      const getPresignedUrl = await axiod.get(PROXY_URL+`?key=${cid}`);
      presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
      console.error("Get language source failed at getting presigned url", address);
      throw (e)
    }

    let languageSource;
    try {
      const getLanguageSource = await axiod.get(presignedUrl);
      languageSource = getLanguageSource.data;
    } catch (e) {
      console.error("Get language source failed at getting language source", address);
      throw (e)
    }

    const expression = (await storage.getLanguageExpression(address)) as LanguageExpression

    if (!expression) {
      console.error("LanguageLanguage.get(): Failed to fetch language");
      return null;
    };
    
    if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
        console.error("LanguageLanguage.get(): Failed to fetch language");
        return null;
    };

    const data_compressed = await storage.download(expression.data.chunks_hashes);
    let data_stream = await data_compressed.arrayBuffer();

    const data_uncompressed = pako.inflate(data_stream);
    const buffer = Buffer.from(data_uncompressed)

    return buffer.toString("utf-8")
  }
}
