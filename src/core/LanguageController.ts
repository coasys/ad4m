import { 
    Address, Expression, Language, LanguageContext, 
    LinksAdapter, InteractionCall, PublicSharing, ReadOnlyLanguage, LanguageMetaInternal, LanguageMetaInput, PerspectiveExpression, parseExprUrl 
} from '@perspect3vism/ad4m';
import { ExpressionRef, LanguageRef, LanguageExpression, LanguageLanguageInput } from '@perspect3vism/ad4m';
import fs from 'fs'
import path from 'path'
import * as Config from './Config'
import type HolochainService from './storage-services/Holochain/HolochainService';
import type AgentService from './agent/AgentService'
import { builtInLangs, builtInLangPath, languageAliases, bootstrapFixtures } from "./Config";
import * as PubSub from './graphQL-interface/PubSub'
import yaml from "js-yaml";
import { v4 as uuidv4 } from 'uuid'
import RuntimeService from './RuntimeService';
import Signatures from './agent/Signatures';
import { PerspectivismDb } from './db';

type LinkObservers = (added: Expression[], removed: Expression[], lang: LanguageRef)=>void;

interface LoadedLanguage {
    language: Language,
    hash: string,
}

interface Services {
    holochainService: HolochainService, 
    runtimeService: RuntimeService, 
    signatures: Signatures, 
    db: PerspectivismDb
}

export default class LanguageController {
    #languages: Map<string, Language>
    #languageConstructors: Map<string, (context: LanguageContext)=>Language>
    #context: object;
    #linkObservers: LinkObservers[];
    #holochainService: HolochainService
    #builtInLanguages: string[];
    #runtimeService: RuntimeService;
    #signatures: Signatures;
    #db: PerspectivismDb;

    #agentLanguage?: Language
    #languageLanguage?: Language
    #perspectiveLanguage?: Language
    pubSub


    constructor(context: object, services: Services) {
        this.#builtInLanguages = builtInLangs.map(l => `${builtInLangPath}/${l}/build/bundle.js`)

        this.#context = context
        this.#holochainService = services.holochainService
        this.#runtimeService = services.runtimeService
        this.#signatures = services.signatures
        this.#db = services.db
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

        //@ts-ignore
        if(language.directMessageAdapter && language.directMessageAdapter.recipient() == this.#context.agent.did) {
            language.directMessageAdapter.addMessageCallback((message: PerspectiveExpression) => {
                this.pubSub.publish(PubSub.DIRECT_MESSAGE_RECEIVED, message)
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

    async installLanguage(address: Address, languageMeta: null|Expression): Promise<Language | undefined> {
        const language = this.#languages.get(address)
        if (language) return language
    
        console.log(new Date(), "installLanguage: installing language with address", address);
        if(!languageMeta) {
            try {
                languageMeta = await this.getLanguageExpression(address)
            } catch (e) {
                throw Error(`Error getting language meta from language language: ${e}`)
            }

            if (languageMeta == null) {
                //@ts-ignore
                languageMeta = {data: {}};
            }
        }
        console.log("LanguageController: INSTALLING NEW LANGUAGE:", languageMeta.data)
        let source;
        try {
            source = await this.getLanguageSource(address);
        } catch (e) {
            throw Error(`Error getting language source from language language, language adapter: ${e}`)
        }
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
            console.error(e)
            fs.rmdirSync(languagePath, {recursive: true})
            //@ts-ignore
            throw Error(`Error loading language [${sourcePath}]: ${e.toString()}`)
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

    async languageByRef(ref: LanguageRef): Promise<Language> {
        const address = languageAliases[ref.address] ? languageAliases[ref.address] : ref.address
        const language = this.#languages.get(address)
        //If the language is already installed then just return it
        if(language) {
            return language
        } else {
            let languageMeta = await this.getExpression(new ExpressionRef(new LanguageRef('lang'), address));
            if(!languageMeta) {
                throw new Error("Language not found by reference: " + JSON.stringify(ref))
            }
            if (languageMeta.proof.valid != true) {
                throw new Error(`Language to be installed does not have valid proof`);
            }
            const languageMetaData = languageMeta.data as LanguageExpression;
            const languageAuthor = languageMeta.author;
            const trustedAgents: string[] = this.#runtimeService.getTrustedAgents();
            //Check if the author of the language is in the trusted agent list the current agent holds, if so then go ahead and install
            if (trustedAgents.find((agent) => agent === languageAuthor)) {
                //Get the language source so we can generate a hash and check against the hash given in the language meta information
                const languageSource = await this.getLanguageSource(address);
                if (!languageSource) {
                    throw new Error("Could not get languageSource for language")
                }
                const languageHash = await this.ipfsHash(languageSource);
                if (!languageMetaData.address) {
                    throw new Error(`Could not find 'address' value inside languageMetaData object: ${languageMetaData["address"]} - ${languageMetaData.address} - ${JSON.stringify(languageMetaData)}`)
                }
                //Check the hash matches and then install!
                if (languageHash == languageMetaData.address) {
                    //TODO: in here we are getting the source again even though we have already done that before, implement installLocalLanguage()?
                    const lang = await this.installLanguage(address, languageMeta)
                    return lang!
                } else {
                    throw new Error("Calculated languageHash did not match address found in meta information")
                }
            } else {
                //Person who created this language is not trusted so lets try and get a source language template
                if (!languageMetaData.templateAppliedParams) {
                    throw new Error("Language not created by trusted agent and is not templated... aborting language install")
                }
                //Check that there are some template params to even apply
                if (Object.keys(languageMetaData.templateAppliedParams).length == 0) {
                    throw new Error("Language not created by trusted agent and is not templated... aborting language install")
                }
                //Check that there is a sourceLanguage that we can follow to
                if (!languageMetaData.templateSourceLanguageAddress) {
                    throw new Error("Language not created by trusted agent and is not templated... aborting language install");
                }
                //Get the meta information of the source language
                const sourceLanguageHash = languageMetaData.templateSourceLanguageAddress;
                const sourceLanguageMeta = await this.getLanguageExpression(sourceLanguageHash);
                if (!sourceLanguageMeta) {
                    throw new Error("Could not get the meta for the source language");
                }
                const trustedAgents: string[] = this.#runtimeService.getTrustedAgents();
                //Check that the agent who authored the original template language is in the current agents trust list
                if (trustedAgents.find((agent) => agent === sourceLanguageMeta.author)) {
                    //Apply the template information supplied in the language to be installed to the source language and make sure that the resulting
                    //language hash is equal to the one trying to be installed. This ensures that the only thing changed between language versions is the template data
                    const sourceLanguageTemplated = await this.languageApplyTemplateOnSource(sourceLanguageHash, JSON.parse(languageMetaData.templateAppliedParams));
                    const languageSource = await this.getLanguageSource(address);
                    if (!languageSource) {
                        throw new Error("Could not get languageSource for language")
                    }
                    //Hash the language source and ensure its the same as the templated language 
                    const languageHash = await this.ipfsHash(languageSource);
                    if (sourceLanguageTemplated.meta.address === languageHash) {
                        //TODO: in here we are getting the source again even though we have already done that before, implement installLocalLanguage()?
                        const lang = await this.installLanguage(address, languageMeta)
                        return lang!
                    } else {
                        throw new Error(`Templating of original source language did not result in the same language hash of un-trusted language trying to be installed... aborting language install. Expected hash: ${languageHash}. But got: ${sourceLanguageTemplated.meta.address}`)
                    }
                } else {
                    throw new Error("Agent which created source language for language trying to be installed is not a trustedAgent... aborting language install")
                }
            }
        }
    }

    async readAndTemplateHolochainDNA(sourceLanguageLines: string[], templateData: object, sourceLanguageHash?: string): Promise<{dnaCode: string | null}> {
        //Look for var dna in languageSource which would tell us its a holochain language and then apply templating to the holochain language also
        const dnaIndex = sourceLanguageLines.findIndex(element => element.includes(`var dna = `));
        let dnaCodeRes: null | string = null;
        if (dnaIndex != -1) {
            if (!sourceLanguageHash) {
                sourceLanguageHash = uuidv4();
            }
            //Create a directory for all of our DNA templating operations
            const tempTemplatingPath = path.join(Config.tempLangPath, sourceLanguageHash);
            fs.mkdirSync(tempTemplatingPath);
            //The place where we will put the .dna from the b64
            const tempDnaPath = path.join(tempTemplatingPath, `${sourceLanguageHash}.dna`);
            const wasmPath = path.join(tempTemplatingPath, "target/wasm32-unknown-unknown/release/")

            //Write the DNA code to a file
            let dnaCode = sourceLanguageLines[dnaIndex].split("var dna = ")[1]
            //TODO: here was are assuming that the first character is " and the last two "; we should check for this and not assume
            dnaCode = dnaCode.substring(1, dnaCode.length-2);
            fs.writeFileSync(tempDnaPath, Buffer.from(dnaCode, "base64"));
            
            //Unpack the DNA
            //TODO: we need to be able to check for errors in this fn call, currently we just crudly split the result 
            let unpackPath = this.#holochainService.unpackDna(tempDnaPath).replace(/(\r\n|\n|\r)/gm, "");
            //TODO: are all dna's using the same dna.yaml?
            const dnaYamlPath = path.join(unpackPath, "dna.yaml");
            if (!fs.existsSync(dnaYamlPath)) {
                throw new Error("Expected to find DNA of source language at path: " + dnaYamlPath + " after unpacking but could not find at given path");
            }

            //Read for files inside wasm path after unpack since we should now have .wasm file there but we do not know which name it may have
            const wasmName = fs.readdirSync(wasmPath);
            if (wasmName.length == 0 || wasmName.length > 1) {
                throw new Error("Got incorrect number of files inside wasm path when unpacking DNA");
            }

            //Read the yaml file
            let dnaYaml = yaml.load(fs.readFileSync(dnaYamlPath, 'utf8'));
            //@ts-ignore
            if (templateData.uid) {
                //@ts-ignore
                dnaYaml.uid = templateData.uid;
            }
            //@ts-ignore
            for (const [templateKey, templateValue] of Object.entries(templateData)) {
                //@ts-ignore
                dnaYaml.properties[templateKey] = templateValue;
            }

            let dnaYamlDump = yaml.dump(dnaYaml, {
                'styles': {
                  '!!null': 'canonical' // dump null as ~
                }
            });
            fs.writeFileSync(dnaYamlPath, dnaYamlDump);

            //TODO: we need to be able to check for errors in this fn call, currently we just crudly split the result 
            let packPath = this.#holochainService.packDna(unpackPath).replace(/(\r\n|\n|\r)/gm, "");
            const base64 = fs.readFileSync(packPath, "base64").replace(/[\r\n]+/gm, '');

            //Cleanup temp directory
            fs.unlinkSync(tempDnaPath);
            fs.rmdirSync(tempTemplatingPath, {recursive: true});
            dnaCodeRes = base64;
        }
        return {
            dnaCode: dnaCodeRes
        }
    }

    orderObject(templateData: object): object {
        return Object.keys(templateData).sort().reduce(
            (obj, key) => { 
                //@ts-ignore
                obj[key] = templateData[key]; 
                return obj;
            }, 
            {}
        );
    }

    applyTemplateData(sourceLanguageLines: string[], templateData: object) {
        let templateLines = [];
        for (const [templateKey, templateValue] of Object.entries(templateData)) {
            //NOTE: this could be risky and end up removing var ${templateKey} from areas in the code where it is used for normal language operations
            //We need to somehow split the bundle at a given point and only remove template variables above this point
            const patterns = [
                `var ${templateKey} =`, `var ${templateKey}=`, 
                `const ${templateKey} =`, `const ${templateKey}=`,
                `let ${templateKey} =`, `let ${templateKey}=`
            ]
            let p = 0
            let index = -1
            while(index == -1 && p < patterns.length) {
                index = sourceLanguageLines.findIndex(element => element.includes(patterns[p]));
                p++
            }
            
            if (index != -1) {
                sourceLanguageLines.splice(index, 1);
            };
            templateLines.push(`var ${templateKey} = "${templateValue}";`);
        };
        for (const templateValue of templateLines.reverse()) {
            sourceLanguageLines.unshift(templateValue);
        };
    }

    async constructLanguageLanguageInput(
        sourceLanguageLines: string[], 
        metaInput: LanguageMetaInput
    ): Promise<LanguageLanguageInput> {
        const languageData = sourceLanguageLines.join('\n');
        const languageHash = await this.ipfsHash(languageData);

        const internal: LanguageMetaInternal = metaInput as LanguageMetaInternal

        internal.address = languageHash

        let input = new LanguageLanguageInput()
        input.bundle = languageData
        input.meta = internal

        return input
    }

    async languageApplyTemplateOnSource(sourceLanguageHash: string, templateData: object): Promise<LanguageLanguageInput> {
        const sourceLanguageExpr = await this.getLanguageExpression(sourceLanguageHash);
        if(!sourceLanguageExpr) throw new Error(`Language not found: ${sourceLanguageHash}`)
        const sourceLanguage = await this.getLanguageSource(sourceLanguageHash);
        if (!sourceLanguage) {
            throw new Error("LanguageController.languageApplyTemplate: Could not get sourceLanguage with hash: " + sourceLanguageHash)
        }
        if (!sourceLanguageExpr) {
            throw new Error("LanguageController.languageApplyTemplate: Could not get sourceLanguageMeta with hash: " + sourceLanguageHash)
        }
        const sourceLanguageLines = sourceLanguage!.split("\n");
        templateData = this.orderObject(templateData);
        const {dnaCode} = await this.readAndTemplateHolochainDNA(sourceLanguageLines, templateData, sourceLanguageHash);

        //If there was some dna code in the source language then lets also add that to the language bundle (but not to the templateData as we dont want that on the meta)
        if (dnaCode) {
            //@ts-ignore
            templateData["dna"] = dnaCode;
        }
        templateData = this.orderObject(templateData);
        //console.debug("LangugeFactory.languageApplyTemplate: Templating language with template data", templateData);
        this.applyTemplateData(sourceLanguageLines, templateData);

        //@ts-ignore
        delete templateData["dna"];

        const meta = sourceLanguageExpr.data

        //@ts-ignore
        if(templateData["name"])
            //@ts-ignore
            meta.name = templateData["name"]

        //@ts-ignore
        if(templateData["description"])
            //@ts-ignore
            meta.description = templateData["description"]

        meta.templateAppliedParams = JSON.stringify(templateData)
        meta.templateSourceLanguageAddress = sourceLanguageHash

        return await this.constructLanguageLanguageInput(sourceLanguageLines, meta)
    }

    filteredLanguageRefs(propertyFilter?: string): LanguageRef[] {
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

    async getLanguageExpression(address: string): Promise<LanguageExpression | null> {
        if(bootstrapFixtures) {
            const fixtures = bootstrapFixtures.languages;
            if (fixtures) {
                const fixtureLanguage = fixtures.find(f=>f.address===address)
                if(fixtureLanguage && fixtureLanguage.meta) {
                    return fixtureLanguage.meta
                }
            }
        }

        try {
            return await this.#languageLanguage!.expressionAdapter!.get(address)
        } catch (e) {
            throw Error(`Error inside language language expression adapter: ${e}`)
        }
    }

    async getLanguageSource(address: string): Promise<string | null> {
        if(bootstrapFixtures) {
            const fixtures = bootstrapFixtures.languages;
            if (fixtures) {
                const fixtureLanguage = fixtures.find(f=>f.address===address)
                if(fixtureLanguage && fixtureLanguage.bundle) {
                    return fixtureLanguage.bundle
                }
            }
        }
        try {
            return await this.#languageLanguage!.languageAdapter!.getLanguageSource(address)
        } catch (e) {
            throw Error(`Error inside language language getLanguageSource adapter: ${e}`)
        }
    }

    async getPerspective(address: string): Promise<Expression | null> {
        if(bootstrapFixtures) {
            const perspectives = bootstrapFixtures.perspectives;
            if (perspectives) {
                const perspective = perspectives.find(f=>f.address===address)
                if(perspective && perspective.expression) {
                    return perspective.expression
                }
            }
        }

        try {
            return await this.#perspectiveLanguage!.expressionAdapter!.get(address)
        } catch (e) {
            throw Error(`Error inside perspective language expression get adapter: ${e}`)
        }
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

    async getConstructorIcon(lang: LanguageRef): Promise<string | undefined> {
        let grabbedLang = await this.languageByRef(lang);

        return grabbedLang.expressionUI?.constructorIcon()
    }

    async getSettingsIcon(lang: LanguageRef): Promise<string | undefined> {
        let grabbedLang = await this.languageByRef(lang);
        return grabbedLang.settingsUI?.settingsIcon()
    }

    async getIcon(lang: LanguageRef): Promise<string | undefined> {
        let grabbedLang = await this.languageByRef(lang);

        return grabbedLang.expressionUI?.icon()
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

        this.#languages.delete(lang.address)
        const create = this.#languageConstructors.get(lang.address)
        const storageDirectory = Config.getLanguageStoragePath(lang.name)
        const Holochain = this.#holochainService.getDelegateForLanguage(lang.address)
        //@ts-ignore
        const ad4mSignal = this.#context.ad4mSignal.bind({language: lang.address, pubsub: this.pubSub});
        //@ts-ignore
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
        if (!language.expressionAdapter) {
            throw new Error("Language does not have an expressionAdapter")
        }
        if (!language.expressionAdapter.putAdapter) {
            throw new Error("Language does not have an expressionAdapter.putAdapter")
        }
        const putAdapter = language.expressionAdapter.putAdapter;
        let address = null

        let isPublic = function isPublic(adapter: PublicSharing | ReadOnlyLanguage): adapter is PublicSharing {
            return (adapter as PublicSharing).createPublic !== undefined;
        }

        try {
            if (isPublic(putAdapter)) {
                address = await putAdapter.createPublic(content);
            } else {
                address = await putAdapter.addressOf(content);
            }
        } catch (e) {
            throw new Error(`Incompatible putAdapter in Languge ${JSON.stringify(lang)}\nError was: ${e}`)
        }

        // This makes sure that Expression references used in Links (i.e. in Perspectives) use the aliased Language schemas.
        // Especially important for DIDs
        for(const alias of Object.keys(languageAliases)) {
            const target = languageAliases[alias]
            if(lang.address === target) {
                lang.address = alias
            }
        }

        return new ExpressionRef(lang, address!)
    }

    async expressionInteract(url: string, interactionCall: InteractionCall): Promise<string|null> {
        const ref = parseExprUrl(url)
        const lang = await this.languageByRef(ref.language)
        const interaction = lang.interactions(ref.expression).find(i => i.name === interactionCall.name)
        if(!interaction) throw `No interaction named "${interactionCall.name}" found for ${url}`
        return await interaction.execute(interactionCall.parameters)
    }

    async getExpression(ref: ExpressionRef): Promise<Expression | null> {
        if(bootstrapFixtures?.perspectives && ref.language.address === "neighbourhood") {
            const fixturePerspective = bootstrapFixtures.perspectives!.find(f=>f.address===ref.expression)
            if(fixturePerspective && fixturePerspective.expression) return fixturePerspective.expression
        }
        if(bootstrapFixtures?.languages && ref.language.address === "lang") {
            const fixtureLang = bootstrapFixtures.languages!.find(f=>f.address===ref.expression)
            if(fixtureLang && fixtureLang.meta) return fixtureLang.meta
        }
        const lang = this.languageForExpression(ref);
        if (!lang.expressionAdapter) {
            throw Error("Language does not have an expresionAdapter!")
        };
        const langIsImmutable = await this.isImmutableExpression(ref);
        let expr;
        if (langIsImmutable) {
            console.log("Calling cache for expression...");
            const cachedExpression = this.#db.getExpression(ref.expression);
            if (cachedExpression) {
                console.log("Cache hit...");
                expr = JSON.parse(cachedExpression) as Expression
            } else {
                console.log("Cache miss...");
                expr = await lang.expressionAdapter.get(ref.expression);
                if (expr) { this.#db.addExpression(ref.expression, JSON.stringify(expr)) };
            };
        } else {
            expr = await lang.expressionAdapter.get(ref.expression);
        }
        if(expr) {
            try{
                if(! await this.#signatures.verify(expr)) {
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

    async getLinksAdapter(lang: LanguageRef): Promise<LinksAdapter | null> {
        try {
            let gotLang = await this.languageByRef(lang)
            if (gotLang.linksAdapter) {
                return gotLang.linksAdapter
            } else {
                return null
            }
        } catch(e) {
            return null
        }
        
    }

    addLinkObserver(observer: LinkObservers) {
        this.#linkObservers.push(observer)
    }

    async isImmutableExpression(ref: ExpressionRef): Promise<boolean> {
        const language = await this.languageByRef(ref.language);
        if (!language.isImmutableExpression) {
            return false
        } else {
            return language.isImmutableExpression(ref.expression);
        }
    }
}

export function init(context: object, services: Services): LanguageController {
    const languageController = new LanguageController(context, services)
    return languageController
}
