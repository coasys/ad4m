import * as Config from './Config'
import * as Db from './db'
import type { PerspectivismDb } from './db'
import HolochainService from 'language-context/lib/Holochain/HolochainService'
import * as IPFS from './storage-services/IPFS'
import AgentService from './agent/AgentService'
import PerspectivesController from './PerspectivesController'
import LanguageController from './LanguageController'
import type LanguageRef from 'ad4m/LanguageRef'
import * as GraphQL from './graphQL-interface/GraphQL'
import * as DIDs from './agent/DIDs'
import type { DIDResolver } from './agent/DIDs'
import Signatures from './agent/Signatures'
import type Perspective from './Perspective'
import SharedPerspective from 'ad4m/SharedPerspective'
import type { SharingType } from 'ad4m/SharedPerspective'
import LanguageFactory from './LanguageFactory'
import type { PublicSharing } from 'ad4m/Language'
import type PerspectiveID from './PerspectiveID'
import type Address from "ad4m/Address"

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

    async startGraphQLServer() {
        const { url, subscriptionsUrl } = await GraphQL.startServer(this)
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

    async initControllers() {
        this.#languageController = new LanguageController({
            agent: this.#agentService,
            IPFS: this.#IPFS,
            signatures: this.#signatures
        }, this.#holochain)

        await this.#languageController.loadLanguages()

        this.#perspectivesController = new PerspectivesController(Config.rootConfigPath, {
            db: this.#db,
            agentService: this.agentService,
            languageController: this.#languageController
        })

        this.#languageFactory = new LanguageFactory(this.#agentService, this.#languageController.getLanguageLanguage(), this.#languageController.getEncrypedLanguageLanguage(), this.#holochain)
    }

    async publishPerspective(uuid: string, name: string, description: string, sharingType: SharingType, 
        encrypt: Boolean, passphrase: string, requiredExpressionLanguages: Address[], allowedExpressionLanguages: Address[]): Promise<SharedPerspective> {
        // We only work on the PerspectiveID object.
        // On PerspectiveController.update() below, the instance will get updated as well, but we don't need the
        // instance object here
        const perspectiveID = this.#perspectivesController.perspective(uuid).plain()

        const sharedPerspective = new SharedPerspective(name, description, sharingType)

        // Create LinkLanguage
        const linkLanguageRef = await this.#languageFactory.createLinkLanguageForSharedPerspective(sharedPerspective, encrypt, passphrase)
        sharedPerspective.linkLanguages = [linkLanguageRef]
        sharedPerspective.allowedExpressionLanguages = allowedExpressionLanguages
        sharedPerspective.requiredExpressionLanguages = requiredExpressionLanguages
        if (encrypt) {
            await this.#languageController.installEncryptedLanguage(linkLanguageRef.address, null, passphrase)
            allowedExpressionLanguages.forEach(language => this.#languageController.installEncryptedLanguage(language, null, passphrase))
            requiredExpressionLanguages.forEach(language => this.#languageController.installEncryptedLanguage(language, null, passphrase))
        } else {
            await this.#languageController.installLanguage(linkLanguageRef.address)
            allowedExpressionLanguages.forEach(language => this.#languageController.installLanguage(language, null))
            requiredExpressionLanguages.forEach(language => this.#languageController.installLanguage(language, null))
        }

        // Create SharedPerspective
        const perspectiveAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter.putAdapter as PublicSharing).createPublic(perspectiveID.sharedPerspective)
        const perspectiveUrl = `perspective://${perspectiveAddress}`

        // Put it back together and safe new Perspective state
        perspectiveID.sharedURL = perspectiveUrl
        perspectiveID.sharedPerspective = sharedPerspective
        this.#perspectivesController.update(perspectiveID)

        return sharedPerspective
    }

    createUniqueHolochainExpressionLanguageFromTemplate(languagePath: string, dnaNick: string, encrypt: Boolean, passphrase: string): Promise<LanguageRef> {
        return this.#languageFactory.createUniqueHolochainExpressionLanguageFromTemplate(languagePath, dnaNick, encrypt, passphrase)
    }
}

export function create(appDataPath, resourcePath): PerspectivismCore {
    return new PerspectivismCore(appDataPath, resourcePath)
}