import PerspectivismCore from "./core/PerspectivismCore";
import create from "./core/PerspectivismCore";
import { LanguageAlias, CoreConfig, BootstrapFixtures, languageLanguageAlias, agentLanguageAlias, neighbourhoodLanguageAlias, perspectiveLanguageAlias, ad4mExecutorVersion } from "./core/Config"
// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from './shims/getOwnPropertyDescriptor'
import getPort from 'get-port';
import fs from "fs";
import { createResolvers } from "./core/graphQL-interface/GraphQL";

Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

export interface OuterConfig {
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
  //The credential used by admin client to make request
  reqCredential?: string,
  //Path to swipl executable
  swiplPath?: string,
  //Path to swipl home directory
  swiplHomePath?: string,
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
export async function init(config: OuterConfig): Promise<PerspectivismCore> {
    let { 
      resourcePath, appDataPath, networkBootstrapSeed, appLangAliases, bootstrapFixtures, languageLanguageOnly,
      mocks, gqlPort, ipfsSwarmPort, ipfsRepoPath, reqCredential, swiplPath, swiplHomePath,runDappServer,
      dAppPort
    } = config
    if(!gqlPort) gqlPort = 4000
    // Check to see if PORT 2000 & 1337 are available if not returns a random PORT
    if(!config.hcPortAdmin) config.hcPortAdmin = await getPort({ port: 2000 });
    if(!config.hcPortApp) config.hcPortApp = await getPort({ port: 1337 });
    if(!dAppPort) dAppPort = await getPort({port: 4200})
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
    

    console.log("\x1b[2m", 
      "AD4M executor starting with version: ", ad4mExecutorVersion, "\n",
      "Starting ad4m core with path:", appDataPath, "\n", 
      "=> AD4M core language addresses: languageLanguage bundle (hidden) + ", systemLanguages.slice(1, systemLanguages.length), "\n",
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
      reqCredential,
      swiplPath,
      swiplHomePath
    } as CoreConfig);

    core.resolvers = createResolvers(core, config)

    //console.log("\x1b[34m", "Init services...", "\x1b[0m");
    //await core.initIPFS({ ipfsSwarmPort, ipfsRepoPath });
    //console.log("\x1b[31m", "GraphQL server starting...", "\x1b[0m");
    //await core.startGraphQLServer(gqlPort, mocks, config);
    if (runDappServer) { core.startDAppServer(dAppPort) };
    //console.log("\x1b[31m", "GraphQL server started, Unlock the agent to start holohchain", "\x1b[0m");

    return core
}

export default {
  init,
  PerspectivismCore
}