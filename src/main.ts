import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import { BootstrapFixtures, BootstrapLanguages, LanguageAlias } from "./core/Config"
// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from './shims/getOwnPropertyDescriptor'
import getPort from 'get-port';
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor
interface OuterConfig {
  resourcePath: string
  appDataPath: string
  appDefaultLangPath: string
  ad4mBootstrapLanguages: BootstrapLanguages,
  ad4mBootstrapFixtures: BootstrapFixtures,
  appBuiltInLangs: string[] | null,
  appLangAliases: object | null,
  mocks: boolean,
  gqlPort?: number,
  hcPortAdmin?: number,
  hcPortApp?: number,
  ipfsSwarmPort?: number,
  ipfsRepoPath?: string
  hcUseLocalProxy?: boolean,
  hcUseMdns?: boolean,
  hcUseProxy?: boolean,
  hcUseBootstrap?: boolean
}


export async function init(config: OuterConfig): Promise<PerspectivismCore> {
    let { 
      resourcePath, appDataPath, appDefaultLangPath, ad4mBootstrapLanguages, ad4mBootstrapFixtures, 
      appBuiltInLangs, appLangAliases, 
      mocks, 
      gqlPort, hcPortAdmin, hcPortApp,
      ipfsSwarmPort,
      ipfsRepoPath,
      hcUseLocalProxy,
      hcUseMdns,
      hcUseProxy,
      hcUseBootstrap
    } = config
    if(!gqlPort) gqlPort = 4000
    // Check to see if PORT 2000 & 1337 are available if not returns a random PORT
    if(!hcPortAdmin) hcPortAdmin = await getPort({ port: 2000 });
    if(!hcPortApp) hcPortApp = await getPort({ port: 1337 });
    if(hcUseMdns === undefined) hcUseMdns = false
    if(hcUseProxy === undefined) hcUseProxy = true
    if(hcUseBootstrap === undefined) hcUseBootstrap = true
    let builtInLangPath = appDefaultLangPath;
    let builtInLangs = [
      ad4mBootstrapLanguages.agents, 
      ad4mBootstrapLanguages.languages, 
      ad4mBootstrapLanguages.neighbourhoods
    ]

    let languageAliases = {
      'did': ad4mBootstrapLanguages.agents,
      'lang': ad4mBootstrapLanguages.languages,
      'neighbourhood': ad4mBootstrapLanguages.neighbourhoods
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
      //"AD4M Bootstrap Fixtures:", ad4mBootstrapFixtures, "\n", 
      "Built-in languages path:", builtInLangPath, "\n",
      "Built-In languages:", appBuiltInLangs, "\n", 
      "=> All auto-loaded languages:", builtInLangs, "\n",
      "Language aliases:", languageAliases, "\n", 
      "Resource path:", resourcePath, "\n", 
      "\x1b[0m"
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
    console.log("\x1b[34m", "Init services...", "\x1b[0m");
    await core.initServices({ hcPortAdmin, hcPortApp, ipfsSwarmPort, ipfsRepoPath, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap });
    console.log("\x1b[31m", "GraphQL server starting...", "\x1b[0m");
    await core.startGraphQLServer(gqlPort, mocks)

    console.log("\x1b[32m", "AD4M init complete", "\x1b[0m");
    return core
}

export default {
  init,
  PerspectivismCore
}