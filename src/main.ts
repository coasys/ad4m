import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import type { AppSignal } from '@holochain/conductor-api'
import Expression from "@perspect3vism/ad4m/Expression";

export let defaultLangPath = "";
export let defaultLangs = [];
export let languageAliases = {};

interface Config {
  resourcePath: string
  appDataPath: string
  appDefaultLangPath: string
  ad4mBootstrapLanguages: BootstrapLanguages,
  ad4mBootstrapExpressions: BootstrapExpression[] | void,
  appBuiltInLangs: string[] | void,
  appLangAliases: object | void,
  mocks: boolean
}

interface BootstrapLanguages {
  agents: string,
  languages: string,
  perspectives: string,
}

class BootstrapExpression {
  schema: string
  address: string
  expression: Expression
}

export async function init(config: Config): Promise<PerspectivismCore> {
    let { resourcePath, appDataPath, appDefaultLangPath, ad4mBootstrapLanguages, ad4mBootstrapExpressions, appBuiltInLangs, appLangAliases, mocks } = config
    defaultLangPath = appDefaultLangPath;
    defaultLangs = [
      ad4mBootstrapLanguages.agents, 
      ad4mBootstrapLanguages.languages, 
      ad4mBootstrapLanguages.perspectives
    ]

    languageAliases = {
      'did': ad4mBootstrapLanguages.agents,
      'lang': ad4mBootstrapLanguages.languages,
      'perspective': ad4mBootstrapLanguages.perspectives
    }

    if(appBuiltInLangs) {
      defaultLangs = Array.from(new Set(defaultLangs.concat(appBuiltInLangs)))
    }

    if(appLangAliases) {
      languageAliases = {
        ...languageAliases,
        ...appLangAliases,
      }
    }
    

    console.log("\x1b[2m", 
      "Starting ad4m core with path:", appDataPath, "\n", 
      "Default language path:", defaultLangPath, "\n",
      "AD4M Bootstrap Languages:", ad4mBootstrapLanguages, "\n", 
      "AD4M Bootstrap Expressions:", ad4mBootstrapExpressions, "\n", 
      "Built-In languages:", appBuiltInLangs, "\n", 
      "=> All auto-loaded languages:", defaultLangs, "\n",
      "Language aliases:", languageAliases, "\n", 
      "Resource path:", resourcePath, "\n", 
    );

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