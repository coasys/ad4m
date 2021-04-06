import create from "./core/PerspectivismCore";

export let defaultLangPath = "";
export let defaultLangs = [];

export async function init(appDataPath: String, resourcePath: string, appDefaultLangPath: string, appDefaultLangs: string[]) {
    defaultLangPath = appDefaultLangPath;
    defaultLangs = appDefaultLangs;
    console.log("Starting ad4m core with path:", appDataPath, "Default language path:", defaultLangPath, 
      "And default languages", defaultLangs, "resource path:", resourcePath);

    const core = new create(appDataPath, resourcePath);
    console.log("Init services...");
    await core.initServices();
    console.log("GraphQL server starting...");
    await core.startGraphQLServer()

    console.log("AD4M init complete");
    return core
}