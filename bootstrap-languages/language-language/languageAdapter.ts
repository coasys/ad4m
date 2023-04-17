import type { Address, LanguageAdapter, PublicSharing, LanguageContext } from "@perspect3vism/ad4m";
//@ts-ignore
import axiod from "https://deno.land/x/axiod/mod.ts";
import { PROXY_URL } from ".";
import XMLHttpRequest from 'xhr2';

export default class LangAdapter implements LanguageAdapter {
  putAdapter: PublicSharing;

  constructor(context: LanguageContext) {
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

    return languageSource;
  }
}
