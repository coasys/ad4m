import * as path from 'path';
import * as fs from 'fs';

export let rootConfigPath = path.join('', 'ad4m')
export let dataPath = path.join(rootConfigPath, 'data')
export let languagesPath = path.join(rootConfigPath, 'languages')
export let holochainPath = path.join(rootConfigPath, 'holochain')
export let holochainConfigPath = path.join(holochainPath, 'config')
export let holochainDataPath = path.join(holochainPath, 'data')
export let resourcePath = ''

export function init(appDataPath, appResourcePath) {
    //Reinit vars
    resourcePath = appResourcePath;
    rootConfigPath = path.join(appDataPath, 'ad4m')
    dataPath = path.join(rootConfigPath, 'data')
    languagesPath = path.join(rootConfigPath, 'languages')
    holochainPath = path.join(rootConfigPath, 'holochain')
    holochainConfigPath = path.join(holochainPath, 'config')
    holochainDataPath = path.join(holochainPath, 'data')

    //Create paths if they do not exist
    const dirs = [rootConfigPath, dataPath, languagesPath, holochainPath, holochainConfigPath, holochainDataPath]
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