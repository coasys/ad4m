import * as path from 'path';
import * as fs from 'fs';
import { Address, Expression } from '@perspect3vism/ad4m';

export let rootConfigPath = path.join('', 'ad4m')
export let dataPath = path.join(rootConfigPath, 'data')
export let languagesPath = path.join(rootConfigPath, 'languages')
export let tempLangPath = path.join(languagesPath, "temp")
export let holochainPath = path.join(rootConfigPath, 'h')
export let holochainDataPath = path.join(holochainPath, 'd')
export let holochainConductorPath = path.join(holochainPath, 'c')
export let resourcePath = ''

export let systemLanguages: string[] = [];
export let langugeLanguageBundle: string = '';
export let directMessageLanguageAddress: string = '';
export let languageAliases: LanguageAlias = {};
export let bootstrapFixtures: BootstrapFixtures|null = null;

export type LanguageAlias = {
    [key: string]: Address;
}

export interface CoreConfig {
    appDataPath: string
    appResourcePath: string
    languageLanguageBundle: string
    systemLanguages: string[]
    directMessageLanguageAddress: string
    languageAliases?: LanguageAlias
    bootstrapFixtures?: BootstrapFixtures
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

    //Create paths if they do not exist
    const dirs = [rootConfigPath, dataPath, languagesPath, tempLangPath, holochainPath, holochainDataPath, holochainConductorPath]
    for(const d of dirs)
    if(!fs.existsSync(d)) {
        fs.mkdirSync(d)
    }

    systemLanguages = c.systemLanguages
    if(c.languageAliases)
        languageAliases = c.languageAliases

    if (c.bootstrapFixtures) {
        bootstrapFixtures = c.bootstrapFixtures!;
    } else {
        bootstrapFixtures = null
    }
    directMessageLanguageAddress = c.directMessageLanguageAddress
}

export function getLanguageStoragePath(name: string) {
    const languageConfigPath = path.join(languagesPath, name)
    if(!fs.existsSync(languageConfigPath))
        fs.mkdirSync(languageConfigPath)
    const storageDirectory = path.join(languageConfigPath, "storage")
    if(!fs.existsSync(storageDirectory))
        fs.mkdirSync(storageDirectory)
    return storageDirectory
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