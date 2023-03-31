import type { Address, LanguageAdapter, PublicSharing, LanguageContext } from "@perspect3vism/ad4m";
import path from "path";
import fs from "fs";

export default class LangAdapter implements LanguageAdapter {
  putAdapter: PublicSharing;
  #storagePath: string;

  constructor(context: LanguageContext) {
    //@ts-ignore
    if ("storagePath" in context.customSettings) { this.#storagePath = context.customSettings["storagePath"] } else { this.#storagePath = "./src/tst-tmp/languages" };
  }

  async getLanguageSource(address: Address): Promise<string> {
    const bundlePath = path.join(this.#storagePath, `bundle-${address}.js`);
    if (fs.existsSync(bundlePath)) {
      const metaFile = fs.readFileSync(bundlePath).toString();
      return metaFile
    }
  }
}
