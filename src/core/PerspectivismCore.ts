import type { Address, LanguageRef, PublicSharing, PerspectiveHandle, Language, Perspective  } from '@perspect3vism/ad4m'
import { parseExprUrl, Neighbourhood } from '@perspect3vism/ad4m'

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
    #holochain?: HolochainService
    #IPFS: any

    #agentService: AgentService
    #db: PerspectivismDb
    #didResolver: DIDResolver
    #signatures: Signatures

    #perspectivesController?: PerspectivesController
    #languageController?: LanguageController

    #languageFactory?: LanguageFactory

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
        if (!this.#perspectivesController) {
            throw Error("No perspectiveController")
        }
        return this.#perspectivesController
    }

    get languageController(): LanguageController {
        if (!this.#languageController) {
            throw Error("No languageController")
        }
        return this.#languageController!
    }

    async exit() {
        this.#IPFS.stop();
        await this.#holochain?.stop();
    }

    async startGraphQLServer(port: number, mocks: boolean) {
        const { url, subscriptionsUrl } = await GraphQL.startServer({
            core: this, 
            mocks,
            port
        })
        console.log(`🚀  GraphQL Server ready at ${url}`)
        console.log(`🚀  GraphQL subscriptions ready at ${subscriptionsUrl}`)
    }

    async initServices(portHCAdmin?: number, portHCApp?: number) {
        console.log("Init HolochainService with data path: ", Config.holochainDataPath, ". Conductor path: ", Config.holochainConductorPath, ". Resource path: ", Config.resourcePath)
        this.#holochain = new HolochainService(Config.holochainConductorPath, Config.holochainDataPath, Config.resourcePath, portHCAdmin, portHCApp)
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
        }, this.#holochain!)

        this.#perspectivesController = new PerspectivesController(Config.rootConfigPath, {
            db: this.#db,
            agentService: this.agentService,
            languageController: this.#languageController
        })
    }

    async initLanguages(omitLanguageFactory: boolean|void) {
        await this.#languageController!.loadLanguages()
        if(!omitLanguageFactory) {
            this.#languageFactory = new LanguageFactory(this.#agentService, this.#languageController!.getLanguageLanguage(), this.#holochain!)
        }
    }

    initMockLanguages(hashes: string[], languages: Language[]) {
        languages.forEach((lang, index) => {
            this.#languageController!.loadMockLanguage(hashes[index], lang);
        });
    }

    async neighbourhoodPublishFromPerspective(uuid: string, linkLanguage: string, meta: Perspective): Promise<string> {
        // We only work on the PerspectiveID object.
        // On PerspectiveController.update() below, the instance will get updated as well, but we don't need the
        // instance object here
        const perspectiveID = this.#perspectivesController!.perspective(uuid).plain()

        const neighbourhood = new Neighbourhood(linkLanguage, meta);
        let language = await this.#languageController!.installLanguage(linkLanguage, null)
        if (!language!.linksAdapter) {
            throw Error("Language used is not a link language");
        }

        // Create neighbourhood
        const neighbourhoodAddress = await (this.languageController.getPerspectiveLanguage().expressionAdapter!.putAdapter as PublicSharing).createPublic(neighbourhood)
        const neighbourhoodUrl = `neighbourhood://${neighbourhoodAddress}`

        //Add shared perspective to original perpspective and then update controller
        perspectiveID.sharedUrl = neighbourhoodUrl
        this.#perspectivesController!.replace(perspectiveID)
        return neighbourhoodUrl
    }

    async installNeighbourhood(url: Address): Promise<PerspectiveHandle> {
        let neighbourHoodExp = await this.languageController.getPerspective(parseExprUrl(url).expression);
        if (neighbourHoodExp == null) {
            throw Error(`Could not find neighbourhood with URL ${url}`);
        };
        // console.log("Core.installNeighbourhood: Got neighbourhood", neighbourHoodExp);
        let neighbourhood: Neighbourhood = neighbourHoodExp.data!;
        
        return this.#perspectivesController!.add("", url, neighbourhood);        
    }

    async languageCloneHolochainTemplate(languagePath: string, dnaNick: string, uid: string): Promise<LanguageRef> {
        if (!this.#languageFactory) {
            throw Error("Language factory was not started when calling core.initLanguages()")
        };
        return await this.#languageFactory.languageCloneHolochainTemplate(languagePath, dnaNick, uid)
    }

    async pubKeyForLanguage(lang: string): Promise<Buffer> {
        return await this.#holochain!.pubKeyForLanguage(lang)
    }
}

export function create(config: Config.CoreConfig): PerspectivismCore {
    return new PerspectivismCore(config)
}