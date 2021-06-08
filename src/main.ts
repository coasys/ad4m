import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import { BootstrapFixtures, BootstrapLanguages } from "./core/Config"

interface OuterConfig {
  resourcePath: string
  appDataPath: string
  appDefaultLangPath: string
  ad4mBootstrapLanguages: BootstrapLanguages,
  ad4mBootstrapFixtures: BootstrapFixtures | void,
  appBuiltInLangs: string[] | void,
  appLangAliases: object | void,
  mocks: boolean
}


export async function init(config: OuterConfig): Promise<PerspectivismCore> {
    let { resourcePath, appDataPath, appDefaultLangPath, ad4mBootstrapLanguages, ad4mBootstrapFixtures, appBuiltInLangs, appLangAliases, mocks } = config
    let builtInLangPath = appDefaultLangPath;
    let builtInLangs = [
      ad4mBootstrapLanguages.agents, 
      ad4mBootstrapLanguages.languages, 
      ad4mBootstrapLanguages.perspectives
    ]

    let languageAliases = {
      'did': ad4mBootstrapLanguages.agents,
      'lang': ad4mBootstrapLanguages.languages,
      'perspective': ad4mBootstrapLanguages.perspectives
    }

    if(appBuiltInLangs) {
      builtInLangs = Array.from(new Set(builtInLangs.concat(appBuiltInLangs)))
    }

    if(appLangAliases) {
      languageAliases = {
        ...languageAliases,
        ...appLangAliases,
      }
    }
    

    console.log("\x1b[2m", 
      "Starting ad4m core with path:", appDataPath, "\n", 
      "AD4M Bootstrap Languages:", ad4mBootstrapLanguages, "\n", 
      "AD4M Bootstrap Fixtures:", ad4mBootstrapFixtures, "\n", 
      "Built-in languages path:", builtInLangPath, "\n",
      "Built-In languages:", appBuiltInLangs, "\n", 
      "=> All auto-loaded languages:", builtInLangs, "\n",
      "Language aliases:", languageAliases, "\n", 
      "Resource path:", resourcePath, "\n", 
    );

    let bootstrapFixtures = ad4mBootstrapFixtures

    const core = new create({
      appDataPath,
      appResourcePath: resourcePath,
      builtInLangPath,
      builtInLangs,
      languageAliases,
      bootstrapFixtures,
    });
    console.log("\x1b[34m", "Init services...");
    await core.initServices();
    console.log("\x1b[31m", "GraphQL server starting...");
    await core.startGraphQLServer(mocks)

    console.log("\x1b[32m", "AD4M init complete");
    return core
}

export default {
  init,
  PerspectivismCore
}