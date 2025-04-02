import {
    Address, Expression, Language, LanguageContext, LinkSyncAdapter, InteractionCall, InteractionMeta,
    PublicSharing, ReadOnlyLanguage, LanguageMetaInternal, LanguageMetaInput, PerspectiveExpression,
    parseExprUrl, Literal, TelepresenceAdapter, PerspectiveState
} from '@coasys/ad4m';
import { ExpressionRef, LanguageRef, LanguageExpression, LanguageLanguageInput, ExceptionType, PerspectiveDiff } from '@coasys/ad4m';
import { ExceptionInfo } from '@coasys/ad4m/lib/src/runtime/RuntimeResolver';
import fs from 'node:fs'
import path from 'node:path'
import * as Config from './Config'
import type HolochainService from './storage-services/Holochain/HolochainService';
import type AgentService from './agent/AgentService'
import * as PubSubDefinitions from './graphQL-interface/SubscriptionDefinitions'
import yaml from "js-yaml";
import { v4 as uuidv4 } from 'uuid';
import { Ad4mDb } from './db';
import stringify from 'json-stable-stringify'
import { getPubSub, tagExpressionSignatureStatus } from './utils';

function cloneWithoutCircularReferences(obj: any, seen: WeakSet<any> = new WeakSet()): any {
    if (typeof obj === 'object' && obj !== null) {
      if (seen.has(obj)) {
        return;
      }
      seen.add(obj);

      const clonedObj: any = Array.isArray(obj) ? [] : {};
      for (const key in obj) {
        if (obj.hasOwnProperty(key)) {
          clonedObj[key] = cloneWithoutCircularReferences(obj[key], seen);
        }
      }

      return clonedObj;
    }

    return obj;
}

type LinkObservers = (diff: PerspectiveDiff, lang: LanguageRef)=>void;
type TelepresenceSignalObserver = (signal: PerspectiveExpression, lang: LanguageRef)=>void;
type SyncStateChangeObserver = (state: PerspectiveState, lang: LanguageRef)=>void;

interface Services {
    holochainService: HolochainService,
    db: Ad4mDb
}

const importModule = async (modulePath: string) => {
    // To deal with ESM on windows requires absolute path and file protocol
    if (process.platform === "win32") {
        const path = `file:\\\\${modulePath}`

        return await import(path)
    }

    return await import(`file://${modulePath}`)
}

const loadModule = async (modulePath: string) => {
    // Check if the file exists
    try {
        //@ts-ignore
        await Deno.stat(modulePath);
    } catch (err) {
        //@ts-ignore
        if (err instanceof Deno.errors.NotFound) {
            throw new Error(`File not found: ${modulePath}`);
        }
        throw err;
    }
    const res  = await UTILS.loadModule(`file://${modulePath}`);

    return await importModule(modulePath)
}

export default class LanguageController {
    #languages: Map<string, Language>
    #languageConstructors: Map<string, (context: LanguageContext)=>Language>
    #context: object;
    #linkObservers: LinkObservers[];
    #telepresenceSignalObservers: TelepresenceSignalObserver[];
    #syncStateChangeObservers: SyncStateChangeObserver[];
    #holochainService: HolochainService
    #db: Ad4mDb;
    #config: Config.MainConfig;
    #pubSub: PubSub;

    #agentLanguage?: Language
    #languageLanguage?: Language
    #neighbourhoodLanguage?: Language
    #perspectiveLanguage?: Language

    constructor(context: object, services: Services) {
        this.#context = context
        this.#holochainService = services.holochainService
        this.#db = services.db
        this.#languages = new Map()
        this.#languages.set("literal", {
            name: "literal",
            interactions() { return [] },
        } as Language)
        this.#languageConstructors = new Map()
        this.#linkObservers = []
        this.#telepresenceSignalObservers = []
        this.#syncStateChangeObservers = []
        this.#config = (context as any).config;
        this.#pubSub = getPubSub();
    }

    async loadLanguages() {
        await this.loadSystemLanguages()
        if (!this.#config.languageLanguageOnly) await this.loadInstalledLanguages()
    }

    async loadSystemLanguages() {
        //Install language language from the bundle file and then update languageAliases to point to language hash
        const { sourcePath, hash: calculatedHash } = await this.saveLanguageBundle(this.#config.languageLanguageBundle);
        if (this.#config.languageLanguageSettings) {
            console.log("LanguageController.loadSystemLanguages: Found settings for languageLanguage, writting settings");
            this.writeSettings(calculatedHash, this.#config.languageLanguageSettings);
        }
        const { hash, language: languageLanguage } = await this.loadLanguage(sourcePath);
        this.#config.languageAliases[Config.languageLanguageAlias] = hash;
        this.#languageLanguage = languageLanguage!;

        if (!this.#config.languageLanguageOnly) {
            //Install the agent language and set
            if (this.#config.agentLanguageSettings) {
                console.log("LanguageController.loadSystemLanguages: Found settings for agentLanguage, writting settings");
                this.writeSettings(this.#config.languageAliases[Config.agentLanguageAlias], this.#config.agentLanguageSettings);
            }
            const agentLanguage = await this.installLanguage(this.#config.languageAliases[Config.agentLanguageAlias], null);
            this.#agentLanguage = agentLanguage!;
            ((this.#context as LanguageContext).agent as unknown as AgentService).setAgentLanguage(agentLanguage!)

            //Install the neighbourhood language and set
            if (this.#config.neighbourhoodLanguageSettings) {
                console.log("LanguageController.loadSystemLanguages: Found settings for neighbourhoodLanguage, writting settings");
                this.writeSettings(this.#config.languageAliases[Config.neighbourhoodLanguageAlias], this.#config.neighbourhoodLanguageSettings);
            }
            const neighbourhoodLanguage = await this.installLanguage(this.#config.languageAliases[Config.neighbourhoodLanguageAlias], null);
            this.#neighbourhoodLanguage = neighbourhoodLanguage!;

            //Install the perspective language and set
            if (this.#config.perspectiveLanguageSettings) {
                console.log("LanguageController.loadSystemLanguages: Found settings for a perspectiveLanguage, writting settings")
                this.writeSettings(this.#config.languageAliases[Config.perspectiveLanguageAlias], this.#config.perspectiveLanguageSettings);
            }
            const perspectiveLanguage = await this.installLanguage(this.#config.languageAliases[Config.perspectiveLanguageAlias], null);
            this.#perspectiveLanguage = perspectiveLanguage!;

            //Install preload languages
            await Promise.all(this.#config.preloadLanguages.map(async hash => {
                await this.installLanguage(hash, null);
            }))

            //Load bootstrap languages
            if (this.#config.bootstrapFixtures) {
                if (this.#config.bootstrapFixtures!.languages) {
                    await Promise.all(this.#config.bootstrapFixtures!.languages!.map(async file => {
                        const { sourcePath } = await this.saveLanguageBundle(this.#config.languageLanguageBundle);
                        await this.loadLanguage(sourcePath);
                    }))
                }
            }
        }
    }

    async loadInstalledLanguages() {
        const files = fs.readdirSync(this.#config.languagesPath)
        return Promise.all(files.map(async file => {
            //Ensure we do not loaded previously loaded system languages again
            if (!this.#config.systemLanguages.find((lang) => lang === file) && !this.#config.preloadLanguages.find((lang) => lang === file)) {
                const bundlePath = path.join(this.#config.languagesPath, file, 'bundle.js')
                if(fs.existsSync(bundlePath)) {
                    try {
                        await this.loadLanguage(bundlePath)
                    } catch(e) {
                        let errMsg = `LanguageController.loadInstalledLanguages(): COULDN'T LOAD LANGUAGE: ${bundlePath}`
                        console.error(errMsg)
                        console.error(e)
                        await this.#pubSub.publish(
                            PubSubDefinitions.EXCEPTION_OCCURRED_TOPIC,
                            {
                                title: "Failed to load installed language",
                                message: errMsg,
                                type: ExceptionType.LanguageIsNotLoaded
                            } as ExceptionInfo
                        );
                    }
                }
            }
        }))
    }

    callLinkObservers(diff: PerspectiveDiff, ref: LanguageRef) {
        LANGUAGE_CONTROLLER.perspectiveDiffReceived(diff, ref.address)
        this.#linkObservers.forEach(o => {
            o(diff, ref)
        })
    }

    callSyncStateChangeObservers(syncState: PerspectiveState, ref: LanguageRef) {
        LANGUAGE_CONTROLLER.syncStateChanged(syncState, ref.address)
        this.#syncStateChangeObservers.forEach(o => {
            o(syncState, ref)
        })
    }

    callTelepresenceSignalObservers(signal: PerspectiveExpression, ref: LanguageRef) {
        LANGUAGE_CONTROLLER.telepresenceSignalReceived(signal, ref.address)
        this.#telepresenceSignalObservers.forEach(o => {
            o(signal, ref)
        })
    }

    async loadLanguage(sourceFilePath: string): Promise<{
        language: Language,
        hash: string,
    }> {
        if(!path.isAbsolute(sourceFilePath))
            // @ts-ignore
            sourceFilePath = path.join(Deno.cwd()!, sourceFilePath)

        const bundleBytes = fs.readFileSync(sourceFilePath)
        if (bundleBytes.length === 0) {
            throw new Error("Language to be loaded does not contain any data")
        }
        // @ts-ignore
        const hash = await this.ipfsHash(bundleBytes)
        console.debug("LanguageController.loadLanguage: loading language at path", sourceFilePath, "with hash", hash);
        let languageSource;
        try {
            languageSource = await loadModule(sourceFilePath);
        } catch (e) {
            const errMsg = `Could not load language ${e}`;
            console.error(errMsg);
            await this.#pubSub.publish(
                PubSubDefinitions.EXCEPTION_OCCURRED_TOPIC,
                {
                    title: "Failed to load installed language",
                    message: errMsg,
                    type: ExceptionType.LanguageIsNotLoaded
                } as ExceptionInfo
            );
            throw new Error(errMsg);
        }
        console.warn("LanguageController.loadLanguage: language loaded!");
        let create;
        if (!languageSource.default) {
            create = languageSource;
        } else {
            if (languageSource.default.default) {
                create = languageSource.default.default;
            } else {
                create = languageSource.default;
            }
        }

        const customSettings = this.getSettings(hash)
        const storageDirectory = this.getLanguageStoragePath(hash)

        const Holochain = this.#holochainService?.getDelegateForLanguage(hash)
        //@ts-ignore
        const ad4mSignal = this.#context.ad4mSignal.bind({language: hash, pubsub: this.#pubSub});
        const language = await create({...this.#context, customSettings, storageDirectory, Holochain, ad4mSignal})

        if(language.linksAdapter) {
            language.linksAdapter.addCallback((diff: PerspectiveDiff) => {
                //console.log("LINKS CALLBACK", diff)
                this.callLinkObservers(diff, {address: hash, name: language.name} as LanguageRef);
            })

            if (language.linksAdapter.addSyncStateChangeCallback) {
                language.linksAdapter.addSyncStateChangeCallback((state: PerspectiveState) => {
                    //console.log("LanguageController.loadLanguage: sync state change", state);
                    this.callSyncStateChangeObservers(state, {address: hash, name: language.name} as LanguageRef);
                })
            }
        }

        if(language.telepresenceAdapter) {
            language.telepresenceAdapter.registerSignalCallback(async (payload: PerspectiveExpression) => {
                await this.tagPerspectiveExpressionSignatureStatus(payload)
                this.callTelepresenceSignalObservers(payload, {address: hash, name: language.name} as LanguageRef);
            })
        }

        //@ts-ignore
        if(language.directMessageAdapter && language.directMessageAdapter.recipient() == this.#context.agent.did) {
            language.directMessageAdapter.addMessageCallback(async (message: PerspectiveExpression) => {
                await this.#pubSub.publish(PubSubDefinitions.RUNTIME_MESSAGED_RECEIVED_TOPIC, message)
            })
        }

        this.#languages.set(hash, language)
        this.#languageConstructors.set(hash, create)

        return { hash, language }
    }

    async reloadLanguage(hash: string): Promise<{
        language: Language,
        hash: string
    }> {
        this.#languages.delete(hash);
        const create = this.#languageConstructors.get(hash)
        if (!create) {
            throw new Error(`Could not find create constructor when trying to reload language: ${hash}`);
        }
        const customSettings = this.getSettings(hash)
        const storageDirectory = this.getLanguageStoragePath(hash)
        const Holochain = this.#holochainService?.getDelegateForLanguage(hash)
        //@ts-ignore
        const ad4mSignal = this.#context.ad4mSignal.bind({language: address, pubsub: this.#pubSub});
        //@ts-ignore
        const language = await create!({...this.#context, storageDirectory, Holochain, ad4mSignal, customSettings})

        if(language.linksAdapter) {
            language.linksAdapter.addCallback((diff: PerspectiveDiff) => {
                this.callLinkObservers(diff, {address: hash, name: language.name});
            })

            if (language.linksAdapter.addSyncStateChangeCallback) {
                language.linksAdapter.addSyncStateChangeCallback((state: PerspectiveState) => {
                    this.callSyncStateChangeObservers(state, {address: hash, name: language.name} as LanguageRef);
                })
            }
        }

        if(language.telepresenceAdapter) {
            language.telepresenceAdapter.registerSignalCallback(async (payload: PerspectiveExpression) => {
                await this.tagPerspectiveExpressionSignatureStatus(payload)
                this.callTelepresenceSignalObservers(payload, {address: hash, name: language.name} as LanguageRef);
            })
        }

        //@ts-ignore
        if(language.directMessageAdapter && language.directMessageAdapter.recipient() == this.#context.agent.did) {
            language.directMessageAdapter.addMessageCallback(async (message: PerspectiveExpression) => {
                await this.#pubSub.publish(PubSubDefinitions.RUNTIME_MESSAGED_RECEIVED_TOPIC, message)
            })
        }

        this.#languages.set(hash, language)
        return {language: language, hash}
    }

    async saveLanguageBundle(bundle: string, languageMeta?: object, hash?: string): Promise<{
        languagePath: string,
        sourcePath: string,
        metaPath: string,
        hash: string
    }> {
        if (!hash) {
            hash = await this.ipfsHash(bundle);
        }
        const languagePath = path.join(this.#config.languagesPath, hash);
        const sourcePath = path.join(languagePath, 'bundle.js')
        const metaPath = path.join(languagePath, 'meta.json')
        console.log("Saving at path", sourcePath);

        if (!fs.existsSync(languagePath)) {
            fs.mkdirSync(languagePath)
        }

        fs.writeFileSync(sourcePath, bundle)
        if (languageMeta) { fs.writeFileSync(metaPath, JSON.stringify(languageMeta)) }

        return {languagePath, sourcePath, metaPath, hash}
    }

    async ipfsHash(data: Buffer|string): Promise<string> {
        if (typeof data != "string") {
            data = data.toString();
        }
        const hash = UTILS.hash(data);
        return hash;
    }

    async installLanguage(address: Address, languageMeta: null|Expression): Promise<Language | undefined> {
        const language = this.#languages.get(address)
        if (language) return language

        if(!languageMeta) {
            //Check that the metafile already exists with language with this address to avoid refetch
            const metaFile = path.join(path.join(this.#config.languagesPath, address), "meta.json");

            if(fs.existsSync(metaFile)) {
                languageMeta = JSON.parse(fs.readFileSync(metaFile).toString());
            } else {
                // We need to get the meta from the language language
                // Retry 10 times with increasing delay to account for Holochain sync
                let retries = 0;
                while (!languageMeta && retries < 10) {
                    try {
                        languageMeta = await this.getLanguageExpression(address)
                    } catch (e) {
                        console.error(`Error getting language meta from language language: ${e}\nRetrying...`)
                    }
                    retries++;
                    await new Promise(r => setTimeout(r, 5000 * retries));
                }
            }
            if (languageMeta == null) {
                //@ts-ignore
                languageMeta = {data: {}};
            }
        }


        console.log("LanguageController.installLanguage: INSTALLING LANGUAGE:", languageMeta.data)
        let bundlePath = path.join(path.join(this.#config.languagesPath, address), "bundle.js");
        let source;
        let hash;
        //Check if the bundle file already exists to avoid refetching
        if (!fs.existsSync(bundlePath)) {
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
            hash = await this.ipfsHash(source)
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
                return
            }
        } else {
            source = fs.readFileSync(bundlePath).toString();
            hash = await this.ipfsHash(source);
        }

        //TODO: potential for unnecassary write to happen here if meta / source is already present at languages paths
        const {languagePath, sourcePath} = await this.saveLanguageBundle(source, languageMeta, hash);
        console.log(new Date(), "LanguageController.installLanguage: installed language");
        try {
            const {language} = await this.loadLanguage(sourcePath);

            let newLang = {
                ...language,
                linksAdapter: cloneWithoutCircularReferences(language).linksAdapter,
                telepresenceAdapter: cloneWithoutCircularReferences(language).telepresenceAdapter
            };

            return newLang
        } catch(e) {
            console.error("LanguageController.installLanguage: ERROR LOADING NEWLY INSTALLED LANGUAGE")
            console.error("LanguageController.installLanguage: ======================================")
            console.error(e)
            //fs.rmdirSync(languagePath, {recursive: true})
            //@ts-ignore
            // throw Error(`Error loading language [${sourcePath}]: ${e.toString()}`)
        }
    }

    async languageRemove(hash: String): Promise<void> {
        //Teardown any intervals the language has running
        const language = this.#languages.get(hash as string);
        if (language?.teardown) {
            language.teardown();
        }

        //Remove language from memory
        this.#languages.delete(hash as string);
        this.#languageConstructors.delete(hash as string);
        try {
            await this.#holochainService?.removeDnaForLang(hash as string);
        } catch(e) {
            console.log("No DNA found for language installed");
        }
        //Remove language files
        const languagePath = path.join(this.#config.languagesPath, hash as string);
        fs.rmdirSync(languagePath, {recursive: true});
    }

    languageForExpression(e: ExpressionRef): Language {
        const address = this.#config.languageAliases[e.language.address] ? this.#config.languageAliases[e.language.address] : e.language.address
        const language = this.#languages.get(address)
        if(language) {
            return language
        } else {
            throw new Error("Language for expression not found: " + JSON.stringify(e))
        }
    }

    async languageByRef(ref: LanguageRef): Promise<Language> {
        const address = this.#config.languageAliases[ref.address] ? this.#config.languageAliases[ref.address] : ref.address
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
            const trustedAgents: string[] = await RUNTIME_SERVICE.getTrustedAgents();
            const agentService = (this.#context as LanguageContext).agent as AgentService;
            //Check if the author of the language is in the trusted agent list the current agent holds, if so then go ahead and install
            if (trustedAgents.find((agent) => agent === languageAuthor) || AGENT.did() === languageAuthor) {
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
                    // @ts-ignore
                    return lang
                } else {
                    throw new Error("Calculated languageHash did not match address found in meta information")
                }
            } else {
                //Person who created this language is not trusted so lets try and get a source language template
                //Check that there are some template params to even apply
                //Check that there is a sourceLanguage that we can follow to
                if (!languageMetaData.templateAppliedParams ||
                    Object.keys(languageMetaData.templateAppliedParams).length == 0 ||
                    !languageMetaData.templateSourceLanguageAddress
                ) {
                    let errMsg = `Language not created by trusted agent: ${languageAuthor} and is not templated... aborting language install. Language metadata: ${stringify(languageMetaData)}`
                    console.error(errMsg)
                    await this.#pubSub.publish(
                        PubSubDefinitions.EXCEPTION_OCCURRED_TOPIC,
                        {
                            title: "Failed to install language",
                            message: errMsg,
                            type: ExceptionType.AgentIsUntrusted,
                            addon: languageAuthor,
                        } as ExceptionInfo
                    );
                    throw new Error(errMsg);
                }

                //Get the meta information of the source language
                const sourceLanguageHash = languageMetaData.templateSourceLanguageAddress;
                const sourceLanguageMeta = await this.getLanguageExpression(sourceLanguageHash);
                if (!sourceLanguageMeta) {
                    throw new Error("Could not get the meta for the source language");
                }

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
                          // @ts-ignore
                        return lang!
                    } else {
                        throw new Error(`Templating of original source language did not result in the same language hash of un-trusted language trying to be installed... aborting language install. Expected hash: ${languageHash}. But got: ${sourceLanguageTemplated.meta.address}`)
                    }
                } else {
                    let errMsg = "Agent which created source language for language trying to be installed is not a trustedAgent... aborting language install";
                    await this.#pubSub.publish(
                        PubSubDefinitions.EXCEPTION_OCCURRED_TOPIC,
                        {
                            title: "Failed to install language",
                            message: errMsg,
                            type: ExceptionType.AgentIsUntrusted,
                            addon: sourceLanguageMeta.author,
                        } as ExceptionInfo
                    );
                    throw new Error(errMsg)
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
            const tempTemplatingPath = path.join(this.#config.tempLangPath, sourceLanguageHash);
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
            console.log("LanguageController.readAndTemplateHolochainDNA: unpacking DNA");
            let unpackPath = (await this.#holochainService?.unpackDna(tempDnaPath)).replace(/(\r\n|\n|\r)/gm, "");
            fs.unlinkSync(tempDnaPath);
            //TODO: are all dna's using the same dna.yaml?
            const dnaYamlPath = path.join(unpackPath, "dna.yaml");
            if (!fs.existsSync(dnaYamlPath)) {
                throw new Error("Expected to find DNA of source language at path: " + dnaYamlPath + " after unpacking but could not find at given path");
            }

            //Read for files inside wasm path after unpack since we should now have .wasm file there but we do not know which name it may have
            const wasmName = fs.readdirSync(wasmPath);
            if (wasmName.length == 0) {
                throw new Error("Got incorrect number of files inside wasm path when unpacking DNA");
            }

            //Read the yaml file
            let dnaYaml = yaml.load(fs.readFileSync(dnaYamlPath, 'utf8'));
            //@ts-ignore
            if (templateData.uid) {
                //@ts-ignore
                dnaYaml.integrity.network_seed = templateData.uid;
            }
            //@ts-ignore
            for (const [templateKey, templateValue] of Object.entries(templateData)) {
                //@ts-ignore
                dnaYaml.integrity.properties[templateKey] = templateValue;
            }

            let dnaYamlDump = yaml.dump(dnaYaml, {
                'styles': {
                  '!!null': 'canonical' // dump null as ~
                }
            });
            fs.writeFileSync(dnaYamlPath, dnaYamlDump);

            //TODO: we need to be able to check for errors in this fn call, currently we just crudly split the result
            console.log("LanguageController.readAndTemplateHolochainDNA: packing DNA");
            let packPath = (await this.#holochainService?.packDna(unpackPath)).replace(/(\r\n|\n|\r)/gm, "");
            const base64 = fs.readFileSync(packPath, "base64").replace(/[\r\n]+/gm, '');

            //Cleanup temp directory
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
        //Get lines in sourceLanguageLines which have ad4m-template-variable declared
        const ad4mTemplatePattern = "//!@ad4m-template-variable";
        var indexes = [];
        for(let i = 0; i < sourceLanguageLines.length; i++) {
            if (sourceLanguageLines[i].includes(ad4mTemplatePattern)) {
                indexes.push(i);
            }
        }

        //Possible variable patterns for template variables
        const patterns = [
            new RegExp(/var ([a-zA-Z0-9_-]{1,})/g),
            new RegExp(/const ([a-zA-Z0-9_-]{1,})/g),
            new RegExp(/let ([a-zA-Z0-9_-]{1,})/g)
        ];

        //Look over the template variable indexes
        for (let i = 0; i < indexes.length; i++) {
            //Get the index of the variable which should be next line after comment denote
            let variableIndex = indexes[i] + 1;
            let variable = sourceLanguageLines[variableIndex];

            for (const pattern of patterns) {
                let matches = variable.match(pattern);
                if (matches) {
                    //Get variable type and name
                    let variableType = matches[0].split(" ")[0];
                    let variableName = matches[0].split(" ")[1];
                    for (const [key, value] of Object.entries(templateData)) {
                        //if matching variable name from template data
                        if (key === variableName) {
                            //Ensure we use the correct type of variable
                            if (variableType === "const") {
                                //Ensure we use quotes where required if variable is a string
                                if (typeof value === "string") {
                                    sourceLanguageLines[variableIndex] = `const ${key} = "${value}"`;
                                } else {
                                    sourceLanguageLines[variableIndex] = `const ${key} = ${value}`
                                }
                            } else if (variableType === "let") {
                                if (typeof value === "string") {
                                    sourceLanguageLines[variableIndex] = `let ${key} = "${value}"`;
                                } else {
                                    sourceLanguageLines[variableIndex] = `let ${key} = ${value}`
                                }
                            } else if (variableType === "var") {
                                if (typeof value === "string") {
                                    sourceLanguageLines[variableIndex] = `var ${key} = "${value}"`;
                                } else {
                                    sourceLanguageLines[variableIndex] = `var ${key} = ${value}`
                                }
                            }
                        }
                    }
                }
            }
        }

        //Add custom case for inserting templated dna data
        const dnaPattern = new RegExp(/var (dna{1,})/g);
        //@ts-ignore
        if (templateData["dna"]) {
            let dnaIndex = 0;
            for(let i = 0; i < sourceLanguageLines.length; i++) {
                if (sourceLanguageLines[i].match(dnaPattern)) {
                    dnaIndex = i;
                }
            }
            //@ts-ignore
            sourceLanguageLines[dnaIndex] = `var dna = "${templateData["dna"]}"`;
        }
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
        const { dnaCode } = await this.readAndTemplateHolochainDNA(sourceLanguageLines, templateData, sourceLanguageHash);

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
        if(this.#config.bootstrapFixtures) {
            const fixtures = this.#config.bootstrapFixtures.languages;
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
        if(this.#config.bootstrapFixtures) {
            const fixtures = this.#config.bootstrapFixtures.languages;
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
        if(this.#config.bootstrapFixtures) {
            const perspectives = this.#config.bootstrapFixtures.perspectives;
            if (perspectives) {
                const perspective = perspectives.find(f=>f.address===address)
                if(perspective && perspective.expression) {
                    return perspective.expression
                }
            }
        }

        try {
            return await this.#neighbourhoodLanguage!.expressionAdapter!.get(address)
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

    getNeighbourhoodLanguage(): Language {
        if(!this.#neighbourhoodLanguage) {
            throw new Error("No Neighbourhood Language installed!")
        }
        return this.#neighbourhoodLanguage
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

    getSettings(hash: string): object {
        const FILEPATH = path.join(this.#config.languagesPath, hash, 'settings.json')
        if(fs.existsSync(FILEPATH)) {
            return JSON.parse(fs.readFileSync(FILEPATH).toString())
        } else {
            return {}
        }
    }

    writeSettings(hash: string, settings: object) {
        const directory = path.join(this.#config.languagesPath, hash)
        if(!fs.existsSync(directory))
            fs.mkdirSync(directory)
        const FILEPATH = path.join(directory, 'settings.json')
        fs.writeFileSync(FILEPATH, JSON.stringify(settings))
    }

    async putSettings(hash: string, settings: object) {
        this.writeSettings(hash, settings);
        this.reloadLanguage(hash);
    }

    getLanguageStoragePath(hash: string) {
        const languageConfigPath = path.join(this.#config.languagesPath, hash)
        if(!fs.existsSync(languageConfigPath))
            fs.mkdirSync(languageConfigPath)
        const storageDirectory = path.join(languageConfigPath, "storage")
        if(!fs.existsSync(storageDirectory))
            fs.mkdirSync(storageDirectory)
        return storageDirectory
    }

    async expressionCreate(lang: LanguageRef, content: object): Promise<ExpressionRef> {
        if(lang.address == "literal") {
            const expr = (this.#context as LanguageContext).agent.createSignedExpression(content)
            return parseExprUrl(Literal.from(expr).toUrl())
        }
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
            throw new Error(`Incompatible putAdapter in Languge ${JSON.stringify(lang)}\nError was: ${JSON.stringify(e)}`)
        }

        // This makes sure that Expression references used in Links (i.e. in Perspectives) use the aliased Language schemas.
        // Especially important for DIDs
        for(const alias of Object.keys(this.#config.languageAliases)) {
            const target = this.#config.languageAliases[alias]
            if(lang.address === target) {
                lang.address = alias
            }
        }

        return new ExpressionRef(lang, address!)
    }

    async expressionInteractions(url: string): Promise<InteractionMeta[]> {
        const ref = parseExprUrl(url)
        const lang = await this.languageByRef(ref.language)
        return lang.interactions(ref.expression).map(ic => {
            return { label: ic.label, name: ic.name, parameters: ic.parameters}
        })
    }

    async expressionInteract(url: string, interactionCall: InteractionCall): Promise<string|null> {
        const ref = parseExprUrl(url)
        const lang = await this.languageByRef(ref.language)
        const interaction = lang.interactions(ref.expression).find(i => i.name === interactionCall.name)
        if(!interaction) throw `No interaction named "${interactionCall.name}" found for ${url}`
        return await interaction.execute(interactionCall.parameters)
    }

    async getExpression(ref: ExpressionRef): Promise<Expression | null> {
        if(this.#config.bootstrapFixtures?.perspectives && ref.language.address === "neighbourhood") {
            const fixturePerspective = this.#config.bootstrapFixtures.perspectives!.find(f=>f.address===ref.expression)
            if(fixturePerspective && fixturePerspective.expression) return fixturePerspective.expression
        }
        if(this.#config.bootstrapFixtures?.languages && ref.language.address === "lang") {
            const fixtureLang = this.#config.bootstrapFixtures.languages!.find(f=>f.address===ref.expression)
            if(fixtureLang && fixtureLang.meta) return fixtureLang.meta
        }
        let expr;

        try {
            if(ref.language.address == "literal" || ref.language.name == 'literal') {
                expr = Literal.fromUrl(`literal://${ref.expression}`).get()
                if(! (typeof expr === 'object')) {
                    expr = {
                        author: '<unknown>',
                        timestamp: '<unknown>',
                        data: expr,
                        proof: {}
                    }
                }
            } else {
                const lang = this.languageForExpression(ref);
                if (!lang.expressionAdapter) {
                    throw Error("Language does not have an expresionAdapter!")
                };

                const langIsImmutable = await this.isImmutableExpression(ref);
                if (langIsImmutable) {
                    console.log("Calling cache for expression...");
                    const cachedExpression = await this.#db.getExpression(ref.expression);
                    if (cachedExpression) {
                        console.log("Cache hit...");
                        expr = cachedExpression;
                    } else {
                        console.log("Cache miss...");
                        expr = await lang.expressionAdapter.get(ref.expression);
                        if (expr) { await this.#db.addExpression(ref.expression, expr) };
                    };
                } else {
                    expr = await lang.expressionAdapter.get(ref.expression);
                }
            }
        } catch (e) {
            console.error("LanguageController.getExpression(): Error getting the expression: ", e);
            return null
        }

        if(expr) {
            await this.tagExpressionSignatureStatus(expr);
        }

        return expr
    }

    async tagExpressionSignatureStatus(expression: Expression) {
        if(expression) {
            tagExpressionSignatureStatus(expression)
        }
    }

    async tagPerspectiveExpressionSignatureStatus(perspective: PerspectiveExpression) {
        await this.tagExpressionSignatureStatus(perspective);
        if (perspective.data.links) {
            for (const link of perspective.data.links) {
                await this.tagExpressionSignatureStatus(link);
            }
        }
    }

    async getLinksAdapter(lang: LanguageRef): Promise<LinkSyncAdapter | null> {
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

    async getTelepresenceAdapter(lang: LanguageRef): Promise<TelepresenceAdapter | null> {
        try {
            let gotLang = await this.languageByRef(lang)
            if (gotLang.telepresenceAdapter) {
                return gotLang.telepresenceAdapter
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

    addTelepresenceSignalObserver(observer: TelepresenceSignalObserver) {
        this.#telepresenceSignalObservers.push(observer)
    }

    addSyncStateChangeObserver(listener: SyncStateChangeObserver) {
        this.#syncStateChangeObservers.push(listener)
    }

    async isImmutableExpression(ref: ExpressionRef): Promise<boolean> {
        if(ref.language.address == "literal") return true
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
