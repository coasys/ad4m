import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext } from "@perspect3vism/ad4m";
import { PutAdapter } from "./putAdapter";
import fs from "fs";
import path from "path";

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;
  #storagePath: string;

  constructor(context: LanguageContext) {
    this.putAdapter = new PutAdapter(context);
    //@ts-ignore
    if ("storagePath" in context.customSettings) { this.#storagePath = context.customSettings["storagePath"] } else { this.#storagePath = "./src/tst-tmp/languages" };
  }

  async get(address: Address): Promise<void | Expression> {
    const metaPath = path.join(this.#storagePath, `meta-${address}.json`)
    if (fs.existsSync(metaPath)) {
      const metaFile = JSON.parse(fs.readFileSync(metaPath).toString());
      console.log("Found meta file info", metaFile);
      return metaFile
    } else {
      return null
    } 
  }
}
