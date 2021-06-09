import * as path from 'path';
import * as fs from 'fs';
import Expression from '@perspect3vism/ad4m/Expression';

export let rootConfigPath = path.join('', 'ad4m')
export let dataPath = path.join(rootConfigPath, 'data')
export let languagesPath = path.join(rootConfigPath, 'languages')
export let tempLangPath = path.join(languagesPath, "temp")
export let holochainPath = path.join(rootConfigPath, 'h')
export let holochainDataPath = path.join(holochainPath, 'd')
export let holochainConductorPath = path.join(holochainPath, 'c')
export let resourcePath = ''

export let builtInLangPath = "";
export let builtInLangs = [];
export let languageAliases = {};
export let bootstrapFixtures: BootstrapFixtures|void = null;


export interface CoreConfig {
    appDataPath: string
    appResourcePath: string
    builtInLangPath: string
    builtInLangs: string[]
    languageAliases?: object|void
    bootstrapFixtures?: BootstrapFixtures|void
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

    builtInLangPath = c.builtInLangPath
    builtInLangs = c.builtInLangs
    if(c.languageAliases)
        languageAliases = c.languageAliases
    bootstrapFixtures = c.bootstrapFixtures
}

export function getLanguageStoragePath(name) {
    const languageConfigPath = path.join(languagesPath, name)
    if(!fs.existsSync(languageConfigPath))
        fs.mkdirSync(languageConfigPath)
    const storageDirectory = path.join(languageConfigPath, "storage")
    if(!fs.existsSync(storageDirectory))
        fs.mkdirSync(storageDirectory)
    return storageDirectory
}

export interface BootstrapLanguages {
    agents: string,
    languages: string,
    perspectives: string,
}

export class BootstrapFixtures {
    languages: BootstrapLanguageFixture[]
    perspectives: BootstrapPerspectiveFixture[]
}
  
export class BootstrapLanguageFixture {
    address: string
    meta: Expression
    bundle: string
}
  
export class BootstrapPerspectiveFixture {
    address: string
    expression: Expression
}