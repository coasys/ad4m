import type Expression from '../acai/Expression';
import ExpressionRef from '../acai/ExpressionRef';
import type Language from '../acai/Language'
import type { LinksAdapter } from '../acai/Language'
import type { InteractionCall } from '../acai/Language'
import type LanguageContext from '../acai/LanguageContext';
import type LanguageRef from '../acai/LanguageRef'
import fs from 'fs'
import path from 'path'
import * as Config from './Config'
//import type HolochainService from './storage-services/Holochain/HolochainService';
import type AgentService from './agent/AgentService'
import baseX from 'base-x'
import type Address from '../acai/Address';

const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)

const builtInLanguages = [
    'note-ipfs',
    'url-iframe',
    //'gun-links',
    'ipfs-links',
    'junto-hc-shortform',
    'agent-profiles',
    'languages',
    'shared-perspectives',
    'social-context'
].map(l => `./src/languages/${l}/build/bundle.js`)

const aliases = {
    'http': 'url-iframe',
    'https': 'url-iframe',
    'did': 'agent-profiles',
    'lang': 'languages',
    'perspective': 'shared-perspectives'
}

type LinkObservers = (added: Expression[], removed: Expression[], lang: LanguageRef)=>void;

export default class LanguageController {
    #languages: Map<string, Language>
    #languageConstructors: Map<string, (LanguageContext)=>Language>
    #context: object;
    #linkObservers: LinkObservers[];

    #agentLanguage: Language
    #languageLanguage: Language
    #perspectiveLanguage: Language


    constructor(context: object) {
        this.#context = context
        this.#languages = new Map()
        this.#languageConstructors = new Map()
        this.#linkObservers = []   
    }

    async loadLanguages() {
        await this.loadBuiltInLanguages()
        await this.loadInstalledLanguages()
    }

    async loadBuiltInLanguages() {
        await Promise.all(builtInLanguages.map( async bundle => {
            const { hash, language } = await this.loadLanguage(bundle)
            
            // Do special stuff for ACAI languages:
            Object.keys(aliases).forEach(alias => {
                if(language.name === aliases[alias]) {
                    aliases[alias] = hash
                    if(alias === 'did') {
                        this.#agentLanguage = language;
                        ((this.#context as LanguageContext).agent as AgentService).setAgentLanguage(language)
                    }
                    if(alias === 'lang') {
                        this.#languageLanguage = language
                    }
                    if(alias === 'perspective') {
                        this.#perspectiveLanguage = language
                    }
                }
            })
        }))
    }

    async loadInstalledLanguages() {
        const files = fs.readdirSync(Config.languagesPath)
        await Promise.all(files.map(async file => {
            const bundlePath = path.join(Config.languagesPath, file, 'bundle.js')
            if(fs.existsSync(bundlePath)) {
                try {
                    await this.loadLanguage(bundlePath)
                } catch(e) {
                    console.error("LanguageController.loadInstalledLanguages()=========================")
                    console.error("LanguageController.loadInstalledLanguages(): COULDN'T LOAD LANGUAGE:", bundlePath)
                    console.error(e)
                    console.error("LanguageController.loadInstalledLanguages()=========================")
                }
            }
        }))
    }

    async loadLanguage(sourceFilePath: string): Promise<any> {
        if(!path.isAbsolute(sourceFilePath))
            sourceFilePath = path.join(process.env.PWD, sourceFilePath)

        const bundleBytes = fs.readFileSync(sourceFilePath)
        // @ts-ignore
        const hash = await this.ipfsHash(bundleBytes)
        
        const { default: create, name } = require(sourceFilePath)

        const customSettings = this.getSettings({name, address: hash} as LanguageRef)
        const storageDirectory = Config.getLanguageStoragePath(name)
        const Holochain = "";
        const language = create({...this.#context, customSettings, storageDirectory, Holochain})

        if(language.linksAdapter) {
            language.linksAdapter.addCallback((added, removed) => {
                this.#linkObservers.forEach(o => {
                    o(added, removed, {name, address: hash} as LanguageRef)
                })
            })
        } 
        this.#languages.set(hash, language)
        this.#languageConstructors.set(hash, create)

        return { hash, language }
    }

    async ipfsHash(data: Buffer|string): Promise<string> {
        // @ts-ignore
        const ipfsAddress = await this.#context.IPFS.add({content: data.toString()}, {onlyHash: true})
        return ipfsAddress.cid.toString()
    }

    async installLanguage(address: Address, languageMeta: void|Expression) {
        if(!languageMeta) {
            languageMeta = await this.#languageLanguage.expressionAdapter.get(address)
        }
        // @ts-ignore
        console.log("LanguageController: INSTALLING NEW LANGUAGE:", languageMeta.data)
        const source = await this.#languageLanguage.languageAdapter.getLanguageSource(address)

        const hash = await this.ipfsHash(source)
        if(hash === 'asdf') {
            console.error("LanguageController.installLanguage: COULDN'T VERIFY HASH OF LANGUAGE!")
            console.error("LanguageController.installLanguage: Address:", address)
            console.error("LanguageController.installLanguage: Computed hash:", hash)
            console.error("LanguageController.installLanguage: =================================")
            console.error("LanguageController.installLanguage: LANGUAGE WILL BE IGNORED")
            console.error("LanguageController.installLanguage: =================================")
            console.error("LanguageController.installLanguage:", languageMeta)
            console.error("LanguageController.installLanguage: =================================")
            console.error("LanguageController.installLanguage: =================================")
            console.error("LanguageController.installLanguage: CONTENT:")
            //console.error(source)
            //console.error("LanguageController.installLanguage: =================================")
            //console.error("LanguageController.installLanguage: LANGUAGE WILL BE IGNORED")
            //console.error("LanguageController.installLanguage: =================================")
            return
        }

        const sourcePath = path.join(Config.languagesPath, address, 'bundle.js')
        const metaPath = path.join(Config.languagesPath, address, 'meta.json')
        try {
            fs.mkdirSync(path.join(Config.languagesPath, address))
        } catch {
            console.warn("Already got directory for language at", path.join(Config.languagesPath, address))
        };
        fs.writeFileSync(sourcePath, source)
        fs.writeFileSync(metaPath, JSON.stringify(languageMeta))
        try {
            this.loadLanguage(sourcePath)
        } catch(e) {
            console.error("LanguageController.installLanguage: ERROR LOADING NEWLY INSTALLED LANGUAGE")
            console.error("LanguageController.installLanguage: ======================================")
            console.error(e)
        }
        
    }

    languageForExpression(e: ExpressionRef): Language {
        const address = aliases[e.language.address] ? aliases[e.language.address] : e.language.address
        const language = this.#languages.get(address)
        if(language) {
            return language
        } else {
            throw new Error("Language for expression not found: " + JSON.stringify(e))
        }
    }

    languageByRef(ref: LanguageRef): Language {
        const address = aliases[ref.address] ? aliases[ref.address] : ref.address
        const language = this.#languages.get(address)
        if(language) {
            return language
        } else {
            this.#languageLanguage.expressionAdapter.get(address).then(languageMeta => {
                if(languageMeta) {
                    this.installLanguage(address, languageMeta)
                }
            })
            
            throw new Error("Language not found by reference: " + JSON.stringify(ref))
        }
    }

    filteredLanguageRefs(propertyFilter: void | string): LanguageRef[] {
        const refs: LanguageRef[] = []
        this.#languages.forEach((language, hash) => {
            if(!propertyFilter || Object.keys(language).includes(propertyFilter)) {
                refs.push({
                    address: hash,
                    name: language.name,
                })
            }
        })
        return refs
    }

    getInstalledLanguages(): LanguageRef[] {
        return this.filteredLanguageRefs()
    }

    getLanguagesWithExpressionUI(): LanguageRef[] {
        return this.filteredLanguageRefs("expressionUI")
    }

    getLanguagesWithLinksAdapter(): LanguageRef[] {
        return this.filteredLanguageRefs("linksAdapter")
    }

    getAgentLanguage(): Language {
        if(!this.#agentLanguage) {
            throw new Error("No Agent Language installed!")
        }
        return this.#agentLanguage
    }

    getLanguageLanguage(): Language {
        if(!this.#agentLanguage) {
            throw new Error("No Language Language installed!")
        }
        return this.#languageLanguage
    }

    getPerspectiveLanguage(): Language {
        if(!this.#agentLanguage) {
            throw new Error("No Perspective Language installed!")
        }
        return this.#perspectiveLanguage
    }

    getConstructorIcon(lang: LanguageRef): void | string {
        return this.languageByRef(lang).expressionUI?.constructorIcon()
    }

    getSettingsIcon(lang: LanguageRef): void | string {
        return this.languageByRef(lang).settingsUI?.settingsIcon()
    }

    getIcon(lang: LanguageRef): void | string {
        return  this.languageByRef(lang).expressionUI?.icon()
    }

    getSettings(lang: LanguageRef): object {
        const FILEPATH = path.join(Config.languagesPath, lang.name, 'settings.json')
        if(fs.existsSync(FILEPATH)) {
            return JSON.parse(fs.readFileSync(FILEPATH).toString())
        } else {
            return {}
        }
    }

    putSettings(lang: LanguageRef, settings: object) {
        const directory = path.join(Config.languagesPath, lang.name)
        if(!fs.existsSync(directory))
            fs.mkdirSync(directory)
        const FILEPATH = path.join(directory, 'settings.json')
        fs.writeFileSync(FILEPATH, JSON.stringify(settings))

        this.#languages.set(lang.address, null)
        const create = this.#languageConstructors.get(lang.address)
        const context = this.#context
        const storageDirectory = Config.getLanguageStoragePath(lang.name)
        const newInstance = create({...context, storageDirectory, customSettings: settings})
        this.#languages.set(lang.address, newInstance)
    }

    async createPublicExpression(lang: LanguageRef, content: object): Promise<ExpressionRef> {
        const putAdapter = this.languageByRef(lang).expressionAdapter.putAdapter
        let address = null

        try {
            // Ok, first we assume its a PublicSharing put adapter...
            // @ts-ignore
            address = await putAdapter.createPublic(content)
        } catch(e1) {
            try {
                // ...and if it's not, let's try to treat it like a
                // ReadOnlyLangauge..
                // @ts-ignore
                address = await putAdapter.addressOf(content)
            } catch(e2) {
                // If both don't work, we don't know what to do with this put adapter :/
                throw new Error(`Incompatible putAdapter in Languge ${JSON.stringify(lang)}\nPutAdapter: ${Object.keys(putAdapter)}\nError was: ${e1.toString()}\nand: ${e2.toString()}`)
            }
        }

        // This makes sure that Expression references used in Links (i.e. in Perspectives) use the aliased Language schemas.
        // Especially important for DIDs
        for(const alias of Object.keys(aliases)) {
            const target = aliases[alias]
            if(lang.address === target) {
                lang.address = alias
            }
        }

        return new ExpressionRef(lang, address)
    }

    async getExpression(ref: ExpressionRef): Promise<void | Expression> {
        const expr = await this.languageForExpression(ref).expressionAdapter.get(ref.expression)
        if(expr) {
            try{
                // @ts-ignore
                if(! await this.#context.signatures.verify(expr)) {
                    console.error("BROKEN SIGNATURE FOR EXPRESSION:", expr)
                    expr.proof.invalid = true
                } else {
                    // console.debug("Valid expr:", ref)
                    expr.proof.valid = true
                }
            } catch(e) {
                console.error("Error trying to verify expression signature:", e)
                console.error("For expression:", expr)
            }
        }

        return expr
    }

    interact(expression: ExpressionRef, interaction: InteractionCall) {
        console.log("TODO")
    }

    getLinksAdapter(lang: LanguageRef): void | LinksAdapter {
        try {
            return this.languageByRef(lang).linksAdapter
        } catch(e) {
            return null
        }
        
    }

    addLinkObserver(observer) {
        this.#linkObservers.push(observer)
    }
}

export function init(context: object): LanguageController {
    const languageController = new LanguageController(context)
    return languageController
}
