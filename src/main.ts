import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import { LanguageAlias, CoreConfig, BootstrapFixtures, languageLanguageAlias, agentLanguageAlias, neighbourhoodLanguageAlias, perspectiveLanguageAlias } from "./core/Config"
// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from './shims/getOwnPropertyDescriptor'
import getPort from 'get-port';
import fs from "fs";

Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor
interface OuterConfig {
  //Path to resources used by ad4m-executor such as; hc, holochain, prolog
  resourcePath: string
  //Path to be used for storing ad4m data
  appDataPath: string
  //Seed file used to load initial languages & agent configuration 
  networkBootstrapSeed: string
  //Should the ad4m-executor be started with only the languageLanguage, so it can be used for publish other system languages
  languageLanguageOnly?: boolean,
  //Languages & perspectives to be bootstrapped into ad4m-executor without requirment for using language language
  bootstrapFixtures?: BootstrapFixtures,
  //Aliases used by application running ad4m-executor; should be in form {"alias": "language-address"}
  appLangAliases?: object,
  //Should the graphql server be started as mocking service
  mocks: boolean,
  //Port for graphql server
  gqlPort?: number,
  //Port for holochain admin port
  hcPortAdmin?: number,
  //Port for holochain application port
  hcPortApp?: number,
  //Port for IPFS swarm
  ipfsSwarmPort?: number,
  //Port for IPFS repo
  ipfsRepoPath?: string
  //Should holochain use a local proxy
  hcUseLocalProxy?: boolean,
  //Should holochain use Mdns
  hcUseMdns?: boolean,
  //Should holochain use a proxy
  hcUseProxy?: boolean,
  //Should holochain use a bootstrap server
  hcUseBootstrap?: boolean,
  //Should ad4m-executor connect to an existing holochain instance, or spawn its own
  connectHolochain?: boolean,
}

interface SeedFileSchema {
  //DID of agents trusted by user running executor
  trustedAgents: string[],
  //Link language templates that are known
  knownLinkLanguages: string[],
  //Address of language to be used when creating a direct message interface on behalf of agent
  directMessageLanguage: string,
  //Settings to be injected into directMessageLanguage
  directMessageLanguageSettings?: object,
  //Address of language to be used for saving AgentExpression
  agentLanguage: string,
  //Settings to be injected into agentLanguage
  agentLanguageSettings?: object,
  //Address of language to be used for saving perspectives
  perspectiveLanguage: string,
  //Settings to be injected into perspectiveLanguage
  perspectiveLanguageSettings?: object,
  //Address of language to be used for persisting neighbourhoods
  neighbourhoodLanguage: string,
  //Settings to be injected into neighbourhoodLanguage
  neighbourhoodLanguageSettings?: object,
  //Bundle file containg langauge language code 
  languageLanguageBundle: string,
  //Settings to be injected into languageLanguage
  languageLanguageSettings?: object,
}

/// Main function which starts ad4m-executor
export async function init(config: OuterConfig): Promise<PerspectivismCore> {
    let { 
      resourcePath, appDataPath, networkBootstrapSeed, appLangAliases, bootstrapFixtures, languageLanguageOnly,
      mocks, gqlPort, 
      hcPortAdmin, hcPortApp,
      ipfsSwarmPort, ipfsRepoPath,
      hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap, connectHolochain
    } = config
    if(!gqlPort) gqlPort = 4000
    // Check to see if PORT 2000 & 1337 are available if not returns a random PORT
    if(!hcPortAdmin) hcPortAdmin = await getPort({ port: 2000 });
    if(!hcPortApp) hcPortApp = await getPort({ port: 1337 });
    if(hcUseMdns === undefined) hcUseMdns = false
    if(hcUseProxy === undefined) hcUseProxy = true
    if(hcUseBootstrap === undefined) hcUseBootstrap = true
    if(languageLanguageOnly === undefined) languageLanguageOnly = false;

    if(!fs.existsSync(networkBootstrapSeed)) {
      throw new Error(`Could not find networkBootstrapSeed at path ${networkBootstrapSeed}`)
    }

    let networkBootstrapSeedData = JSON.parse(fs.readFileSync(networkBootstrapSeed).toString()) as SeedFileSchema;

    //Core adm4-executor System languages
    let systemLanguages = [
      networkBootstrapSeedData.languageLanguageBundle,
      networkBootstrapSeedData.agentLanguage,  
      networkBootstrapSeedData.neighbourhoodLanguage,
      networkBootstrapSeedData.perspectiveLanguage
    ]
    //Languages to be pre-loaded as supplied in the appLanguageAliases 
    let preloadLanguages: string[] = [];

    let coreLanguageAliases = {} as LanguageAlias;
    //Set this to empty string, since we do not know the address yet and it will be loaded/updated on LanguageController.loadSystemLanguages()
    coreLanguageAliases[languageLanguageAlias] = "";
    coreLanguageAliases[agentLanguageAlias] = networkBootstrapSeedData.agentLanguage;
    coreLanguageAliases[neighbourhoodLanguageAlias] = networkBootstrapSeedData.neighbourhoodLanguage;
    coreLanguageAliases[perspectiveLanguageAlias] = networkBootstrapSeedData.perspectiveLanguage;

    let languageAliases: LanguageAlias = coreLanguageAliases;
    if(appLangAliases) {
      languageAliases = {
        ...coreLanguageAliases,
        ...appLangAliases,
      }

      Object.keys(appLangAliases).forEach(address => {
        preloadLanguages.push(address);
      })
    }
    

    console.log("\x1b[2m", 
      "Starting ad4m core with path:", appDataPath, "\n", 
      "=> AD4M core language addresses:", systemLanguages, "\n",
      "Languages to be preloaded, as supplied by appLangAliases", preloadLanguages, "\n",
      "Language aliases:", languageAliases, "\n", 
      "Bootstrap fixtures:", bootstrapFixtures, "\n",
      "Resource path:", resourcePath, "\n", 
      "\x1b[0m"
    );

    const core = new create({
      appDataPath,
      appResourcePath: resourcePath,
      systemLanguages,
      preloadLanguages,
      directMessageLanguage: networkBootstrapSeedData.directMessageLanguage,
      directMessageLanguageSettings: networkBootstrapSeedData.directMessageLanguageSettings,
      agentLanguageSettings: networkBootstrapSeedData.agentLanguageSettings,
      perspectiveLanguageSettings: networkBootstrapSeedData.perspectiveLanguageSettings,
      neighbourhoodLanguageSettings: networkBootstrapSeedData.neighbourhoodLanguageSettings,
      languageLanguageSettings: networkBootstrapSeedData.languageLanguageSettings,
      knownLinkLanguages: networkBootstrapSeedData.knownLinkLanguages,
      trustedAgents: networkBootstrapSeedData.trustedAgents,
      languageAliases,
      bootstrapFixtures,
      languageLanguageOnly
    } as CoreConfig);

    console.log("\x1b[34m", "Init services...", "\x1b[0m");
    await core.initIPFS({ ipfsSwarmPort, ipfsRepoPath });
    if (connectHolochain) {
      await core.connectHolochain( {hcPortAdmin, hcPortApp} );
    } else {
      await core.initHolochain({ hcPortAdmin, hcPortApp, hcUseLocalProxy, hcUseMdns, hcUseProxy, hcUseBootstrap });
    }
    
    console.log("\x1b[31m", "GraphQL server starting...", "\x1b[0m");
    await core.startGraphQLServer(gqlPort, mocks)

    console.log("\x1b[32m", "AD4M init complete", "\x1b[0m");
    return core
}

export default {
  init,
  PerspectivismCore
}