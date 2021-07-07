import type { Address, LanguageRef, SharingType, PublicSharing, PerspectiveHandle, Language, Perspective  } from '@perspect3vism/ad4m'
import { parseExprURL, Neighbourhood as SharedPerspective } from '@perspect3vism/ad4m'

import * as Config from './Config'
import * as Db from './db'
import type { PerspectivismDb } from './db'
import HolochainService from './storage-services/Holochain/HolochainService';
import * as IPFS from './storage-services/IPFS'
import AgentService from './agent/AgentService'
import PerspectivesController from './PerspectivesController'
import LanguageController from './LanguageController'
import * as GraphQL from './graphQL-interface/GraphQL'
import * as DIDs from './agent/DIDs'
import type { DIDResolver } from './agent/DIDs'
import Signatures from './agent/Signatures'
import LanguageFactory from './LanguageFactory'
import * as PubSub from './graphQL-interface/PubSub'

export default class PerspectivismCore {
    #holochain: HolochainService
    #IPFS: any

    #agentService: AgentService
    #db: PerspectivismDb
    #didResolver: DIDResolver
    #signatures: Signatures

    #perspectivesController: PerspectivesController
    #languageController: LanguageController

    #languageFactory: LanguageFactory

    constructor(config: Config.CoreConfig) {
        Config.init(config)

        this.#agentService = new AgentService(Config.rootConfigPath)
        this.#agentService.load()
        this.#db = Db.init(Config.dataPath)
        this.#didResolver = DIDs.init(Config.dataPath)
        this.#signatures = new Signatures(this.#didResolver)
    }

    get agentService(): AgentService {
        return this.#agentService
    }

    get perspectivesController(): PerspectivesController {
        return this.#perspectivesController
    }

    get languageController(): LanguageController {
        return this.#languageController
    }

    async exit() {
        this.#IPFS.stop();
        await this.#holochain.stop();
    }

    async startGraphQLServer(mocks: boolean) {
        const { url, subscriptionsUrl } = await GraphQL.startServer(this, mocks)
        console.log(`🚀  GraphQL Server ready at ${url}`)
        console.log(`🚀  GraphQL subscriptions ready at ${subscriptionsUrl}`)
    }

    async initServices() {
        console.log("Init HolochainService with data path: ", Config.holochainDataPath, ". Conductor path: ", Config.holochainConductorPath, ". Resource path: ", Config.resourcePath)
        this.#holochain = new HolochainService(Config.holochainConductorPath, Config.holochainDataPath, Config.resourcePath)
        let [ipfs, _] = await Promise.all([IPFS.init(), this.#holochain.run()]);
        this.#IPFS = ipfs;
    }

    async waitForAgent(): Promise<void> {
        return this.#agentService.ready
    }
    
    languageSignal(signal: any) {
        //@ts-ignore
        console.log(new Date().toISOString(), "PerspectivismCore.languageSignal: Got signal");
        //NOTE (optimization): worth considering if its worth keeping around pubsub in this or if we should just get a new pubsub here
        //@ts-ignore
        this.pubsub.publish(PubSub.SIGNAL, { signal: JSON.stringify(signal), language: this.language });
    }

    initControllers() {
        this.#languageController = new LanguageController({
            agent: this.#agentService,
            IPFS: this.#IPFS,
            signatures: this.#signatures,
            ad4mSignal: this.languageSignal
        }, this.#holochain)

        this.#perspectivesController = new PerspectivesController(Config.rootConfigPath, {
            db: this.#db,
            agentService: this.agentService,
            languageController: this.#languageController
        })
    }

    async initLanguages(omitLanguageFactory: boolean|void) {
        await this.#languageController.loadLanguages()
        if(!omitLanguageFactory) {
            this.#languageFactory = new LanguageFactory(this.#agentService, this.#languageController.getLanguageLanguage(), this.#holochain)
        }
    }

    initMockLanguages(hashes: string[], languages: Language[]) {
        languages.forEach((lang, index) => {
            this.#languageController.loadMockLanguage(hashes[index], lang);
        });
    }

    async publishPerspective(uuid: string, name: string, description: string, sharingType: SharingType, 
        passphrase: string, requiredExpressionLanguages: Address[], allowedExpressionLanguages: Address[]): Promise<SharedPerspective> {
        // We only work on the PerspectiveID object.
        // On PerspectiveController.update() below, the instance will get updated as well, but we don't need the
        // instance object here
        const perspectiveID = this.#perspectivesController.perspective(uuid).plain()

        const sharedPerspective = new SharedPerspective(name, description, sharingType)
        // Create LinkLanguage
        const linkLanguageRef = await this.#languageFactory.createLinkLanguageForSharedPerspective(sharedPerspective, passphrase)
        sharedPerspective.linkLanguages = [linkLanguageRef]
        sharedPerspective.allowedExpressionLanguages = allowedExpressionLanguages
        sharedPerspective.requiredExpressionLanguages = requiredExpressionLanguages

        await this.#languageController.installLanguage(linkLanguageRef.address, null)
        let installs = allowedExpressionLanguages.concat(requiredExpressionLanguages);
        installs = Array.from(new Set(installs));
        console.log("\x1b[32m", "PerspectivismCore.publishPerspective: Attempting to install expression languages", installs);
        for (const language of installs) {
            await this.#languageController.installLanguage(language, null);
        }

        // Create SharedPerspective
        const perspectiveAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter.putAdapter as PublicSharing).createPublic(sharedPerspective)
        const perspectiveUrl = `perspective://${perspectiveAddress}`

        //Add shared perspective to original perpspective and then update controller
        perspectiveID.sharedURL = perspectiveUrl
        this.#perspectivesController.replace(perspectiveID)
        return sharedPerspective
    }

    async neighbourhoodPublishFromPerspective(uuid: string, linkLanguage: string, meta: Perspective): Promise<SharedPerspective> {
        // We only work on the PerspectiveID object.
        // On PerspectiveController.update() below, the instance will get updated as well, but we don't need the
        // instance object here
        const perspectiveID = this.#perspectivesController.perspective(uuid).plain()

        const sharedPerspective = new SharedPerspective(linkLanguage, meta);
        let language = await this.#languageController.installLanguage(linkLanguage, null)
        if (!language.linkLanguage) {
            throw Error("Language used is not a link language");
        }

        // Create SharedPerspective
        const perspectiveAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter.putAdapter as PublicSharing).createPublic(sharedPerspective)
        const perspectiveUrl = `perspective://${perspectiveAddress}`

        //Add shared perspective to original perpspective and then update controller
        perspectiveID.sharedURL = perspectiveUrl
        this.#perspectivesController.replace(perspectiveID)
        return sharedPerspective
    }

    async installSharedPerspective(url: Address): Promise<PerspectiveHandle> {
        let sharedPerspectiveExp = await this.languageController.getPerspective(parseExprURL(url).expression);
        if (sharedPerspectiveExp == null) {
            throw Error(`Could not find sharedPerspective with URL ${url}`);
        };
        // console.log("Core.installSharedPerspective: Got shared perspective", sharedPerspectiveExp);
        //@ts-ignore
        let sharedPerspective: SharedPerspective = sharedPerspectiveExp.data!;
        const languages = {}
        sharedPerspective.requiredExpressionLanguages.forEach(l => languages[l] = l)
        sharedPerspective.linkLanguages.forEach(l => languages[l.address] = l.address)
        const installs: string[] = Object.values(languages)
        console.log(new Date(), "Core.installSharedPerspective: Attempting to install languages", installs);
        for (const language of installs) {
            await this.#languageController.installLanguage(language, null);
        }
        
        let localPerspective = {
            name: sharedPerspective.name, 
            author: this.agentService.agent, 
            timestamp: new Date().toISOString(), 
            sharedPerspective: sharedPerspective, 
            sharedURL: url
        };
        let perspective = this.#perspectivesController.add(localPerspective);
        return perspective;        
    }

    async languageCloneHolochainTemplate(languagePath: string, dnaNick: string, uid: string): Promise<LanguageRef> {
        if (!this.#languageFactory) {
            throw Error("Language factory was not started when calling core.initLanguages()")
        };
        return await this.#languageFactory.languageCloneHolochainTemplate(languagePath, dnaNick, uid)
    }

    async pubKeyForLanguage(lang: string): Promise<Buffer> {
        return await this.#holochain.pubKeyForLanguage(lang)
    }
}

export function create(config: Config.CoreConfig): PerspectivismCore {
    return new PerspectivismCore(config)
}