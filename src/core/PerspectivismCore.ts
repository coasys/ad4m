import * as Config from './Config'
import * as Db from './db'
import type { PerspectivismDb } from './db'
import HolochainService from './storage-services/Holochain/HolochainService'
import * as IPFS from './storage-services/IPFS'
import AgentService from './agent/AgentService'
import PerspectivesController from './PerspectivesController'
import LanguageController from './LanguageController'
import * as GraphQL from './graphQL-interface/GraphQL'
import * as DIDs from './agent/DIDs'
import type { DIDResolver } from './agent/DIDs'
import Signatures from './agent/Signatures'
import type Perspective from './Perspective'
import SharedPerspective from '../ad4m/SharedPerspective'
import type { SharingType } from '../ad4m/SharedPerspective'
import LinkLanguageFactory from './LinkLanguageFactory'
import type { PublicSharing } from '../ad4m/Language'
import type PerspectiveID from './PerspectiveID'


export default class PerspectivismCore {
    #holochain: HolochainService
    #IPFS: any

    #agentService: AgentService
    #db: PerspectivismDb
    #didResolver: DIDResolver
    #signatures: Signatures

    #perspectivesController: PerspectivesController
    #languageController: LanguageController

    #linkLanguageFactory: LinkLanguageFactory

    constructor() {
        Config.init()

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

    async startGraphQLServer() {
        const { url, subscriptionsUrl } = await GraphQL.startServer(this)
        console.log(`🚀  GraphQL Server ready at ${url}`)
        console.log(`🚀  GraphQL subscriptions ready at ${subscriptionsUrl}`)
    }

    async initServices() {
        this.#holochain = new HolochainService(Config.holochainConfigPath, Config.holochainDataPath)
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

        this.#linkLanguageFactory = new LinkLanguageFactory(this.#agentService, this.#languageController.getLanguageLanguage())
    }

    async publishPerspective(uuid: string, name: string, description: string, sharingType: SharingType): Promise<PerspectiveID> {
        // We only work on the PerspectiveID object.
        // On PerspectiveController.update() below, the instance will get updated as well, but we don't need the
        // instance object here
        const perspectiveID = this.#perspectivesController.perspective(uuid).plain()

        const sharedPerspective = new SharedPerspective(name, description, sharingType)

        // Create LinkLanguage
        const linkLanguageRef = await this.#linkLanguageFactory.createLinkLanguageForSharedPerspective(sharedPerspective)
        sharedPerspective.linkLanguages = [linkLanguageRef]
        await this.#languageController.installLanguage(linkLanguageRef.address)

        // Create SharedPerspective
        const perspectiveAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter.putAdapter as PublicSharing).createPublic(sharedPerspective)
        const perspectiveUrl = `perspective://${perspectiveAddress}`

        // Put it back together and safe new Perspective state
        perspectiveID.sharedURL = perspectiveUrl
        perspectiveID.sharedPerspective = sharedPerspective
        this.#perspectivesController.update(perspectiveID)

        return perspectiveID
    }
}

export function create(): PerspectivismCore {
    return new PerspectivismCore()
}