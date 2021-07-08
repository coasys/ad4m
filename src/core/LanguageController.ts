import type { 
    Address, Expression, Language, LanguageContext, LanguageRef, 
    LinksAdapter, InteractionCall 
} from '@perspect3vism/ad4m';
import { ExpressionRef } from '@perspect3vism/ad4m';
import fs from 'fs'
import path from 'path'
import * as Config from './Config'
import type HolochainService from './storage-services/Holochain/HolochainService';
import type AgentService from './agent/AgentService'
import baseX from 'base-x'
import { builtInLangs, builtInLangPath, languageAliases, bootstrapFixtures } from "./Config";
import * as PubSub from './graphQL-interface/PubSub'

const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)

type LinkObservers = (added: Expression[], removed: Expression[], lang: LanguageRef)=>void;

interface LoadedLanguage {
    language: Language,
    hash: string,
}

export default class LanguageController {
    #languages: Map<string, Language>
    #languageConstructors: Map<string, (context: LanguageContext)=>Language>
    #context: object;
    #linkObservers: LinkObservers[];
    #holochainService: HolochainService
    #builtInLanguages: string[];

    #agentLanguage: Language
    #languageLanguage: Language
    #perspectiveLanguage: Language
    pubSub


    constructor(context: object, holochainService: HolochainService) {
        this.#builtInLanguages = builtInLangs.map(l => `${builtInLangPath}/${l}/build/bundle.js`)

        this.#context = context
        this.#holochainService = holochainService
        this.#languages = new Map()
        this.#languageConstructors = new Map()
        this.#linkObservers = []   
        this.pubSub = PubSub.get()
    }

    async loadLanguages() {
        try {
            await this.loadBuiltInLanguages()
            await this.loadInstalledLanguages()
        } catch (e) {
            throw new Error(`Error loading languages ${e}`);
        }
    }

    loadBuiltInLanguages() {
        console.log("loadBuiltInLanguages: Built in languages:", this.#builtInLanguages);
        return Promise.all(this.#builtInLanguages.map( async bundle => {
            const { hash, language } = await this.loadLanguage(bundle)
            
            // Do special stuff for AD4M languages:
            Object.keys(languageAliases).forEach(alias => {
                if(language.name === languageAliases[alias]) {
                    languageAliases[alias] = hash
                    if(alias === 'did') {
                        this.#agentLanguage = language;
                        ((this.#context as LanguageContext).agent as AgentService).setAgentLanguage(language)
                    }
                    if(alias === 'lang') {
                        this.#languageLanguage = language
                    }
                    if(alias === 'neighbourhood') {
                        this.#perspectiveLanguage = language
                    }
                }
            })
        }))
    }

    async loadInstalledLanguages() {
        const files = fs.readdirSync(Config.languagesPath)
        return Promise.all(files.map(async file => {
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

    async loadLanguage(sourceFilePath: string): Promise<LoadedLanguage> {
        if(!path.isAbsolute(sourceFilePath))
            sourceFilePath = path.join(process.env.PWD!, sourceFilePath)

        const bundleBytes = fs.readFileSync(sourceFilePath)
        // @ts-ignore
        const hash = await this.ipfsHash(bundleBytes)
        
        const { default: create, name } = require(sourceFilePath)

        const customSettings = this.getSettings({name, address: hash} as LanguageRef)
        const storageDirectory = Config.getLanguageStoragePath(name)
        const Holochain = this.#holochainService.getDelegateForLanguage(hash)
        //@ts-ignore
        const ad4mSignal = this.#context.ad4mSignal.bind({language: hash, pubsub: this.pubSub});
        const language = await create({...this.#context, customSettings, storageDirectory, Holochain, ad4mSignal})

        if(language.linksAdapter) {
            language.linksAdapter.addCallback((added: Expression[], removed: Expression[]) => {
                this.#linkObservers.forEach(o => {
                    o(added, removed, {name, address: hash} as LanguageRef)
                })
            })
        }

        this.#languages.set(hash, language)
        this.#languageConstructors.set(hash, create)

        return { hash, language }
    }

    async loadMockLanguage(hash: string, language: Language) {
        this.#languages.set(hash, language)
    }

    async ipfsHash(data: Buffer|string): Promise<string> {
        // @ts-ignore
        const ipfsAddress = await this.#context.IPFS.add({content: data.toString()}, {onlyHash: true})
        return ipfsAddress.cid.toString()
    }

    async installLanguage(address: Address, languageMeta: void|Expression): Promise<Language> {
        const language = this.#languages.get(address)
        
        if (language == undefined) {
            console.log(new Date(), "installLanguage: installing language with address", address);
            if(!languageMeta) {
                languageMeta = await this.getLanguageExpression(address)

                if (languageMeta == null) {
                    //@ts-ignore
                    languageMeta = {data: {}};
                }
            }
            // @ts-ignore
            console.log("LanguageController: INSTALLING NEW LANGUAGE:", languageMeta.data)
            const source = await this.getLanguageSource(address)
            if(!source){
                console.error("LanguageController.installLanguage: COULDN'T GET SOURCE OF LANGUAGE TO INSTALL!")
                console.error("LanguageController.installLanguage: Address:", address)
                console.error("LanguageController.installLanguage:", languageMeta)
                throw Error(`Could not find language source for language with address: ${address}`)
            }
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
    
            const languagePath = path.join(Config.languagesPath, address);
            const sourcePath = path.join(languagePath, 'bundle.js')
            const metaPath = path.join(languagePath, 'meta.json')
            try {
                fs.mkdirSync(languagePath)
            } catch(e) {
                console.error("Error trying to create directory", languagePath)
                console.error("Will proceed with installing language anyway...")
            }
            
            fs.writeFileSync(sourcePath, source)
            fs.writeFileSync(metaPath, JSON.stringify(languageMeta))
            // console.log(new Date(), "LanguageController: installed language");
            try {
                return (await this.loadLanguage(sourcePath)).language
            } catch(e) {
                console.error("LanguageController.installLanguage: ERROR LOADING NEWLY INSTALLED LANGUAGE")
                console.error("LanguageController.installLanguage: ======================================")
                fs.rmdirSync(languagePath, {recursive: true})
                throw Error(e)
            }
        }
    }

    languageForExpression(e: ExpressionRef): Language {
        const address = languageAliases[e.language.address] ? languageAliases[e.language.address] : e.language.address
        const language = this.#languages.get(address)
        if(language) {
            return language
        } else {
            throw new Error("Language for expression not found: " + JSON.stringify(e))
        }
    }

    async languageByRef(ref: LanguageRef): Language {
        const address = languageAliases[ref.address] ? languageAliases[ref.address] : ref.address
        const language = this.#languages.get(address)
        if(language) {
            return language
        } else {
            let languageMeta = await this.getLanguageExpression(address);
            if(languageMeta) {
                await this.installLanguage(address, languageMeta)
                return JSON.parse(languageMeta.data)
            } else {
                throw new Error("Language not found by reference: " + JSON.stringify(ref))
            }
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

    async getLanguageExpression(address: string): Promise<void|Expression> {
        if(bootstrapFixtures) {
            const fixtureLanguage = bootstrapFixtures.languages!.find(f=>f.address===address)
            if(fixtureLanguage) {
                return fixtureLanguage.meta
            }
        }

        return await this.#languageLanguage.expressionAdapter.get(address)
    }

    async getLanguageSource(address: string): Promise<void|string> {
        if(bootstrapFixtures) {
            const fixtureLanguage = bootstrapFixtures.languages!.find(f=>f.address===address)
            if(fixtureLanguage) {
                return fixtureLanguage.bundle
            }
        }
        return await this.#languageLanguage.languageAdapter.getLanguageSource(address)
    }

    async getPerspective(address: string): Promise<void|Expression> {
        if(bootstrapFixtures) {
            const fixturePerspective = bootstrapFixtures.perspectives!.find(f=>f.address===address)
            if(fixturePerspective) {
                return fixturePerspective.expression
            }
        }

        return await this.#perspectiveLanguage.expressionAdapter.get(address)
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
        if(!this.#languageLanguage) {
            throw new Error("No Language Language installed!")
        }
        return this.#languageLanguage
    }

    getPerspectiveLanguage(): Language {
        if(!this.#perspectiveLanguage) {
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

    async putSettings(lang: LanguageRef, settings: object) {
        const directory = path.join(Config.languagesPath, lang.name)
        if(!fs.existsSync(directory))
            fs.mkdirSync(directory)
        const FILEPATH = path.join(directory, 'settings.json')
        fs.writeFileSync(FILEPATH, JSON.stringify(settings))

        this.#languages.set(lang.address, null)
        const create = this.#languageConstructors.get(lang.address)
        const storageDirectory = Config.getLanguageStoragePath(lang.name)
        const Holochain = this.#holochainService.getDelegateForLanguage(lang.address)
        //@ts-ignore
        const ad4mSignal = this.#context.ad4mSignal.bind({language: lang.address, pubsub: this.pubSub});
        const language = await create!({...this.#context, storageDirectory, Holochain, ad4mSignal, customSettings: settings})

        if(language.linksAdapter) {
            language.linksAdapter.addCallback((added: Expression[], removed: Expression[]) => {
                this.#linkObservers.forEach(o => {
                    o(added, removed, {name: lang.name, address: lang.address} as LanguageRef)
                })
            })
        }

        this.#languages.set(lang.address, language)
    }

    async expressionCreate(lang: LanguageRef, content: object): Promise<ExpressionRef> {
        const language = await this.languageByRef(lang)
        const putAdapter = language.expressionAdapter.putAdapter
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
        for(const alias of Object.keys(languageAliases)) {
            const target = languageAliases[alias]
            if(lang.address === target) {
                lang.address = alias
            }
        }

        return new ExpressionRef(lang, address)
    }

    async getExpression(ref: ExpressionRef): Promise<void | Expression> {
        if(bootstrapFixtures && ref.language.address === "perspective") {
            const fixturePerspective = bootstrapFixtures.perspectives!.find(f=>f.address===ref.expression)
            if(fixturePerspective) return fixturePerspective.expression
        }
        if(bootstrapFixtures && ref.language.address === "lang") {
            const fixtureLang = bootstrapFixtures.languages!.find(f=>f.address===ref.expression)
            if(fixtureLang) return fixtureLang.meta
        }
        const lang = this.languageForExpression(ref);
        const expr = await lang.expressionAdapter.get(ref.expression);
        if(expr) {
            try{
                // @ts-ignore
                if(! await this.#context.signatures.verify(expr)) {
                    console.error(new Date().toISOString(), "BROKEN SIGNATURE FOR EXPRESSION:", expr)
                    expr.proof.invalid = true
                    delete expr.proof.valid
                } else {
                    // console.debug("Valid expr:", ref)
                    expr.proof.valid = true
                    delete expr.proof.invalid
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

    addLinkObserver(observer: LinkObservers) {
        this.#linkObservers.push(observer)
    }
}

export function init(context: object, holochainService: HolochainService): LanguageController {
    const languageController = new LanguageController(context, holochainService)
    return languageController
}
