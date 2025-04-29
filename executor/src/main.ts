import Ad4mCore from "./core/Ad4mCore";
import create from "./core/Ad4mCore";
import { LanguageAlias, CoreConfig, BootstrapFixtures, languageLanguageAlias, agentLanguageAlias, neighbourhoodLanguageAlias, perspectiveLanguageAlias, ad4mExecutorVersion } from "./core/Config"
// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from './shims/getOwnPropertyDescriptor'
import getPort from 'get-port';
import fs from "node:fs";
import { createResolvers } from "./core/graphQL-interface/GraphQL";

Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

export interface OuterConfig {
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
  //Should we start a dApp UI server to allow connecting of crypto wallets
  runDappServer: boolean,
  //Optional port for the dApp UI server
  dAppPort?: number,
  //Port for graphql server
  gqlPort?: number,
  //Port for holochain admin port
  hcPortAdmin?: number,
  //Port for holochain application port
  hcPortApp?: number,
  //Should holochain use a local proxy
  hcUseLocalProxy?: boolean,
  //Should holochain use Mdns
  hcUseMdns?: boolean,
  //Should holochain use a proxy
  hcUseProxy?: boolean,
  //Should holochain use a bootstrap server
  hcUseBootstrap?: boolean,
  hcProxyUrl: string,
  hcBootstrapUrl: string,
  //Should ad4m-executor connect to an existing holochain instance, or spawn its own
  connectHolochain?: boolean,
  //The credential used by admin client to make request
  adminCredential?: string,
  // Log holochain metrics
  logHolochainMetrics?: boolean
}

export interface SeedFileSchema {
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
export async function init(config: OuterConfig): Promise<Ad4mCore> {
    let { 
      appDataPath, networkBootstrapSeed, appLangAliases, bootstrapFixtures, languageLanguageOnly,
      adminCredential, logHolochainMetrics
    } = config

    // Moved the port check to Rust
    // BUT: we have weird problem with our JS runtime 
    // (which will be refactored soon, when we move over the last remaining JS code to Rust)
    // when this function doesn't actually do some async I/O operations, the JS event loop doesn't
    // seem to work for future JS calls.
    // So this here is a hack that works for now.
    // Putting it in a try/catch block to avoid the process from crashing if the port is already in use.
    try {
      await getPort({ port: 50000 })
    } catch (error) {
      //ignore
    }
    
    //await new Promise(resolve => setTimeout(resolve, 1000));
    if(config.hcUseMdns === undefined) config.hcUseMdns = false
    if(config.hcUseProxy === undefined) config.hcUseProxy = true
    if(config.hcUseBootstrap === undefined) config.hcUseBootstrap = true
    if(config.languageLanguageOnly === undefined) config.languageLanguageOnly = false;

    if(!fs.existsSync(networkBootstrapSeed)) {
      throw new Error(`Could not find networkBootstrapSeed at path ${networkBootstrapSeed}`)
    }

    let networkBootstrapSeedData = JSON.parse(fs.readFileSync(networkBootstrapSeed).toString()) as SeedFileSchema;

    if (!languageLanguageOnly) {
      if (!networkBootstrapSeedData.directMessageLanguage) {
        throw new Error('Direct Message Language hash not passed in the seed file');
      }

      if (!networkBootstrapSeedData.languageLanguageBundle) {
        throw new Error('Language Language bundle not passed in the seed file');
      }

      if (!networkBootstrapSeedData.agentLanguage) {
        throw new Error('Agent Language hash not passed in the seed file');
      }
      
      if (!networkBootstrapSeedData.neighbourhoodLanguage) {
        throw new Error('Neighbourhood Language hash not passed in the seed file');
      }

      if (!networkBootstrapSeedData.perspectiveLanguage) {
        throw new Error('Perspective Language hash not passed in the seed file');
      }
    }

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

    console.log("AD4M executor starting with config:");
    console.dir(config);

    const core = new create({
      appDataPath,
      systemLanguages,
      preloadLanguages,
      languageLanguageBundle: networkBootstrapSeedData.languageLanguageBundle,
      languageLanguageSettings: networkBootstrapSeedData.languageLanguageSettings,
      directMessageLanguage: networkBootstrapSeedData.directMessageLanguage,
      directMessageLanguageSettings: networkBootstrapSeedData.directMessageLanguageSettings,
      agentLanguageSettings: networkBootstrapSeedData.agentLanguageSettings,
      perspectiveLanguageSettings: networkBootstrapSeedData.perspectiveLanguageSettings,
      neighbourhoodLanguageSettings: networkBootstrapSeedData.neighbourhoodLanguageSettings,
      knownLinkLanguages: networkBootstrapSeedData.knownLinkLanguages,
      trustedAgents: networkBootstrapSeedData.trustedAgents,
      languageAliases,
      bootstrapFixtures,
      languageLanguageOnly,
      adminCredential,
      logHolochainMetrics
    } as CoreConfig);

    core.resolvers = createResolvers(core, config)

    return core
}

export default {
  init,
  Ad4mCore
}