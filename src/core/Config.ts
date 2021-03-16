import * as path from 'path';
import * as fs from 'fs';

export var rootConfigPath = path.join('', 'Perspectivism')
export var dataPath = path.join(rootConfigPath, 'data')
export var languagesPath = path.join(rootConfigPath, 'languages')
export var holochainPath = path.join(rootConfigPath, 'holochain')
export var holochainConfigPath = path.join(holochainPath, 'config')
export var holochainDataPath = path.join(holochainPath, 'data')

export function init(appDataPath) {
    rootConfigPath = path.join(appDataPath, 'Perspectivism')
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