import * as Config from './Config'
import * as Db from './db'
import type { PerspectivismDb } from './db'
import HolochainService from '@perspect3vism/ad4m-language-context/lib/Holochain/HolochainService'
import * as IPFS from './storage-services/IPFS'
import AgentService from './agent/AgentService'
import PerspectivesController from './PerspectivesController'
import LanguageController from './LanguageController'
import type LanguageRef from '@perspect3vism/ad4m/LanguageRef'
import * as GraphQL from './graphQL-interface/GraphQL'
import * as DIDs from './agent/DIDs'
import type { DIDResolver } from './agent/DIDs'
import Signatures from './agent/Signatures'
import SharedPerspective from '@perspect3vism/ad4m/SharedPerspective'
import type { SharingType } from '@perspect3vism/ad4m/SharedPerspective'
import LanguageFactory from './LanguageFactory'
import type { PublicSharing } from '@perspect3vism/ad4m/Language'
import type Address from "@perspect3vism/ad4m/Address"
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveID from './PerspectiveID'
import { parseExprURL } from '@perspect3vism/ad4m/ExpressionRef'

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

    constructor(appDataPath, resourcePath) {
        Config.init(appDataPath, resourcePath)

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

    exit() {
        this.#IPFS.stop();
        this.#holochain.stop();
    }

    async startGraphQLServer(mocks: boolean) {
        const { url, subscriptionsUrl } = await GraphQL.startServer(this, mocks)
        console.log(`🚀  GraphQL Server ready at ${url}`)
        console.log(`🚀  GraphQL subscriptions ready at ${subscriptionsUrl}`)
    }

    async initServices() {
        console.log("Init HolochainService with sandbox path:", Config.holochainDataPath, "config path:", Config.holochainConfigPath, "resource path:", Config.resourcePath)
        this.#holochain = new HolochainService(Config.holochainDataPath, Config.holochainConfigPath, Config.resourcePath)
        this.#IPFS = await IPFS.init()
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

    async initLanguages() {
        await this.#languageController.loadLanguages()
        this.#languageFactory = new LanguageFactory(this.#agentService, this.#languageController.getLanguageLanguage(), this.#holochain)
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

        await this.#languageController.installLanguage(linkLanguageRef.address)
        let installs = allowedExpressionLanguages.concat(requiredExpressionLanguages);
        installs = Array.from(new Set(installs));
        console.log("\x1b[32m", "PerspectivismCore.publishPerspective: Attempting to install expression languages", installs);
        await Promise.all(installs.map(async (install) => {
            await this.#languageController.installLanguage(install, null)
        }))

        // Create SharedPerspective
        const perspectiveAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter.putAdapter as PublicSharing).createPublic(sharedPerspective)
        const perspectiveUrl = `perspective://${perspectiveAddress}`

        //Add shared perspective to original perpspective and then update controller
        perspectiveID.sharedPerspective = sharedPerspective
        perspectiveID.sharedURL = perspectiveUrl
        this.#perspectivesController.update(perspectiveID)
        return sharedPerspective
    }

    async installSharedPerspective(url: Address): Promise<PerspectiveID> {
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
        await Promise.all(installs.map(async (install) => {
            await this.#languageController.installLanguage(install, null)
        }))
        
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

    async createUniqueHolochainExpressionLanguageFromTemplate(languagePath: string, dnaNick: string, uid: string): Promise<LanguageRef> {
        return await this.#languageFactory.createUniqueHolochainExpressionLanguageFromTemplate(languagePath, dnaNick, uid)
    }

    async pubKeyForLanguage(lang: string): Promise<Buffer> {
        return await this.#holochain.pubKeyForLanguage(lang)
    }
}

export function create(appDataPath, resourcePath): PerspectivismCore {
    return new PerspectivismCore(appDataPath, resourcePath)
}