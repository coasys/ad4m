import * as path from 'path';
import * as fs from 'fs';

export let rootConfigPath = path.join('', 'ad4m')
export let dataPath = path.join(rootConfigPath, 'data')
export let languagesPath = path.join(rootConfigPath, 'languages')
export let tempLangPath = path.join(languagesPath, "temp")
export let holochainPath = path.join(rootConfigPath, 'h')
export let holochainDataPath = path.join(holochainPath, 'd')
export let holochainConductorPath = path.join(holochainPath, 'c')
export let resourcePath = ''

export function init(appDataPath, appResourcePath) {
    //Reinit vars
    resourcePath = appResourcePath;
    rootConfigPath = path.join(appDataPath, 'ad4m')
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