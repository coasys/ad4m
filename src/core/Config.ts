import * as path from 'path';
import * as fs from 'fs';

//I think that this needs to be set by the application via an event; filesystems for different OS's might have different paths 
export var rootConfigPath = path.join("", 'acai')
export const dataPath = path.join(rootConfigPath, 'data')
export const languagesPath = path.join(rootConfigPath, 'languages')
export const holochainPath = path.join(rootConfigPath, 'holochain')
export const holochainConfigPath = path.join(holochainPath, 'config')
export const holochainDataPath = path.join(holochainPath, 'data')

export function init() {
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
