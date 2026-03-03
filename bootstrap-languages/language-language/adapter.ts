import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { CloudflarePutAdapter } from "./putAdapter.ts";
//@ts-ignore
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from "./index.ts";

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;

  constructor(context: LanguageContext) {
    this.putAdapter = new CloudflarePutAdapter(context);
  }

  async get(address: Address): Promise<Expression | null> {
    //Check the first two characters of address are equal to Qm
    if (address.substring(0, 2) != "Qm") {
      console.error("LanguageLanguage.get(): The address is not a valid hash");
      return null;
    }
    const metaDataKey = `meta-${address}`;
    
    let presignedUrl;
    try {
      const getPresignedUrl = await axiod.get(PROXY_URL+`?key=${metaDataKey}`);
      presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
      console.error("Get meta information failed at getting presigned url", address);
      return null;
    }

    let metaObject;
    try {
      const getMetaObject = await axiod.get(presignedUrl);
      metaObject = getMetaObject.data;
    } catch (e) {
      console.error("Get meta information failed at getting meta information", presignedUrl);
      return null;
    }

    return metaObject;
  }
}
