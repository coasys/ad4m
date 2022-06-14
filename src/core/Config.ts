import * as path from 'path';
import * as fs from 'fs';
import { Address, Expression } from '@perspect3vism/ad4m';

export let ad4mExecutorVersion = process.env.npm_package_version;
export let rootConfigPath = path.join('', 'ad4m')
export let dataPath = path.join(rootConfigPath, 'data')
export let languagesPath = path.join(rootConfigPath, 'languages')
export let tempLangPath = path.join(languagesPath, "temp")
export let holochainPath = path.join(rootConfigPath, 'h')
export let holochainDataPath = path.join(holochainPath, 'd')
export let holochainConductorPath = path.join(holochainPath, 'c')
export let resourcePath = ''
export let languageLanguageOnly = false;
export let reqCredential = ''

export let knownLinkLanguages: string[] = [];
export let trustedAgents: string[] =  [];
export let systemLanguages: string[] = [];
export let preloadLanguages: string[] = [];
export let languageLanguageBundle: string = '';
export let directMessageLanguage: string = '';
export let languageAliases: LanguageAlias = {};
export let bootstrapFixtures: BootstrapFixtures | null = null;
export let directMessageLanguageSettings: object | null = null;
export let agentLanguageSettings: object | null = null;
export let perspectiveLanguageSettings: object | null = null;
export let neighbourhoodLanguageSettings: object | null = null;
export let languageLanguageSettings: object | null = null;

export let agentLanguageAlias = "did";
export let languageLanguageAlias = "lang";
export let neighbourhoodLanguageAlias = "neighbourhood";
export let perspectiveLanguageAlias = "perspective";

export type LanguageAlias = {
    [key: string]: Address;
}

export interface CoreConfig {
    appDataPath: string
    appResourcePath: string
    languageLanguageBundle: string
    systemLanguages: string[]
    preloadLanguages: string[]
    directMessageLanguage: string
    knownLinkLanguages: string[]
    trustedAgents: string[]
    languageLanguageOnly: boolean
    languageAliases?: LanguageAlias
    bootstrapFixtures?: BootstrapFixtures
    directMessageLanguageSettings?: object
    agentLanguageSettings?: object
    perspectiveLanguageSettings?: object
    neighbourhoodLanguageSettings?: object
    languageLanguageSettings?: object
    reqCredential?: string
}


export function init(c: CoreConfig) {
    //Reinit vars
    resourcePath = c.appResourcePath;
    rootConfigPath = path.join(c.appDataPath, 'ad4m')
    dataPath = path.join(rootConfigPath, 'data')
    languagesPath = path.join(rootConfigPath, 'languages')
    tempLangPath = path.join(languagesPath, "temp")
    holochainPath = path.join(rootConfigPath, 'h')
    holochainDataPath = path.join(holochainPath, 'd')
    holochainConductorPath = path.join(holochainPath, 'c')
    
    if(c.reqCredential) {
        reqCredential = c.reqCredential
    }

    //Create paths if they do not exist
    const dirs = [rootConfigPath, dataPath, languagesPath, tempLangPath, holochainPath, holochainDataPath, holochainConductorPath]
    for(const d of dirs)
    if(!fs.existsSync(d)) {
        fs.mkdirSync(d)
    }

    systemLanguages = c.systemLanguages
    preloadLanguages = c.preloadLanguages
    if(c.languageAliases)
        languageAliases = c.languageAliases

    if (c.bootstrapFixtures) {
        bootstrapFixtures = c.bootstrapFixtures!;
    } else {
        bootstrapFixtures = null
    }
    directMessageLanguage = c.directMessageLanguage
    knownLinkLanguages = c.knownLinkLanguages
    trustedAgents = c.trustedAgents
    languageLanguageOnly = c.languageLanguageOnly;
    languageLanguageBundle = c.languageLanguageBundle;

    if (c.directMessageLanguageSettings) {
        directMessageLanguageSettings = c.directMessageLanguageSettings
    }
    if (c.agentLanguageSettings) {
        agentLanguageSettings = c.agentLanguageSettings
    }
    if (c.perspectiveLanguageSettings) {
        perspectiveLanguageSettings = c.perspectiveLanguageSettings
    }
    if (c.neighbourhoodLanguageSettings) {
        neighbourhoodLanguageSettings = c.neighbourhoodLanguageSettings
    }
    if (c.languageLanguageSettings) {
        languageLanguageSettings = c.languageLanguageSettings
    }
}

export class BootstrapFixtures {
    languages?: BootstrapLanguageFixture[]
    perspectives?: BootstrapPerspectiveFixture[]
}
  
export class BootstrapLanguageFixture {
    address?: string
    meta?: Expression
    bundle?: string
}
  
export class BootstrapPerspectiveFixture {
    address?: string
    expression?: Expression
}