import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import type { AppSignal } from '@holochain/conductor-api'

export let defaultLangPath = "";
export let defaultLangs = [];

export async function init(appDataPath: string, resourcePath: string, appDefaultLangPath: string, appDefaultLangs: string[], mocks: boolean): Promise<PerspectivismCore> {
    defaultLangPath = appDefaultLangPath;
    defaultLangs = appDefaultLangs;
    console.log("\x1b[2m", "Starting ad4m core with path:", appDataPath, "Default language path:", defaultLangPath, 
      "And default languages", defaultLangs, "resource path:", resourcePath);

    const core = new create(appDataPath, resourcePath);
    console.log("\x1b[34m", "Init services...");
    await core.initServices();
    console.log("\x1b[31m", "GraphQL server starting...");
    await core.startGraphQLServer(mocks)

    console.log("\x1b[32m", "AD4M init complete");
    return core
}

export default {
  init,
  defaultLangPath,
  defaultLangs,
  PerspectivismCore
}