import("./core/PerspectivismCore");  // Don't delete this line.
import { PerspectivismCore } from "./core/PerspectivismCore";

//Module types
export interface OuterConfig {
  //Path to resources used by ad4m-executor such as; hc, holochain, prolog
  resourcePath: string
  //Path to be used for storing ad4m data
  appDataPath: string
  //Seed file used to load initial languages & agent configuration 
  networkBootstrapSeed: string
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
  //The credential used by admin client to make request
  reqCredential?: string,
}

export interface Config {
  appDataPath: string
  appResourcePath: string
  languageLanguageBundle: string
  systemLanguages: string[]
  preloadLanguages: string[]
  directMessageLanguage: string
  languageAliases?: LanguageAlias
  bootstrapFixtures?: BootstrapFixtures
}

export function init(config: OuterConfig): Promise<PerspectivismCore>;

export {PerspectivismCore};