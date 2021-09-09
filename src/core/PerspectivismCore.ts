import type { Address, LanguageRef, PublicSharing, PerspectiveHandle, Language, Perspective, LanguageMetaInternal, LanguageLanguageInput, LanguageExpression  } from '@perspect3vism/ad4m'
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
import * as PubSub from './graphQL-interface/PubSub'
import { IPFS as IPFSType } from 'ipfs'
import path from 'path'
import fs from 'fs'
import { RequestAgentInfoResponse } from '@holochain/conductor-api'
import RuntimeService from './RuntimeService'

export interface InitServicesParams {
    hcPortAdmin?: number, 
    hcPortApp?: number,
    ipfsSwarmPort?: number,
    ipfsRepoPath?: string
    hcUseBootstrap?: boolean,
    hcUseProxy?: boolean,
    hcUseLocalProxy?: boolean,
    hcUseMdns?: boolean
}
export default class PerspectivismCore {
    #holochain?: HolochainService
    #IPFS?: IPFSType

    #agentService: AgentService
    #runtimeService: RuntimeService

    #db: PerspectivismDb
    #didResolver: DIDResolver
    #signatures: Signatures

    #perspectivesController?: PerspectivesController
    #languageController?: LanguageController

    constructor(config: Config.CoreConfig) {
        Config.init(config)

        this.#agentService = new AgentService(Config.rootConfigPath)
        this.#runtimeService = new RuntimeService(Config.rootConfigPath)
        this.#agentService.ready.then(() => {
            this.#runtimeService.did = this.#agentService!.did!
        })
        this.#agentService.load()
        this.#db = Db.init(Config.dataPath)
        this.#didResolver = DIDs.init(Config.dataPath)
        this.#signatures = new Signatures(this.#didResolver)
    }

    get agentService(): AgentService {
        return this.#agentService
    }

    get runtimeService(): RuntimeService {
        return this.#runtimeService
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
        await this.#IPFS?.stop();
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

    async initServices(params: InitServicesParams) {
        console.log("Init HolochainService with data path: ", Config.holochainDataPath, ". Conductor path: ", Config.holochainConductorPath, ". Resource path: ", Config.resourcePath)
        console.log(`Holochain ports: admin=${params.hcPortAdmin} app=${params.hcPortApp}`)
        this.#holochain = new HolochainService({
            conductorPath: Config.holochainConductorPath, 
            dataPath: Config.holochainDataPath, 
            resourcePath: Config.resourcePath, 
            adminPort: params.hcPortAdmin, 
            appPort: params.hcPortApp,
            useBootstrap: params.hcUseBootstrap,
            useProxy: params.hcUseProxy,
            useLocalProxy: params.hcUseLocalProxy,
            useMdns: params.hcUseMdns,
        })
        let [ipfs, _] = await Promise.all([IPFS.init(
            params.ipfsSwarmPort, 
            params.ipfsRepoPath
        ), this.#holochain.run()]);
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
            runtime: this.#runtimeService,
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

    async initLanguages() {
        await this.#languageController!.loadLanguages()
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
        perspectiveID.neighbourhood = neighbourhood;
        this.#perspectivesController!.replace(perspectiveID, neighbourhood)
        return neighbourhoodUrl
    }

    async installNeighbourhood(url: Address): Promise<PerspectiveHandle> {
        let neighbourHoodExp = await this.languageController.getPerspective(parseExprUrl(url).expression);
        if (neighbourHoodExp == null) {
            throw Error(`Could not find neighbourhood with URL ${url}`);
        };
        console.log("Core.installNeighbourhood(): Got neighbourhood", neighbourHoodExp);
        let neighbourhood: Neighbourhood = neighbourHoodExp.data;
        this.languageController.installLanguage(neighbourhood.linkLanguage, null);
        
        return this.#perspectivesController!.add("", url, neighbourhood);        
    }

    async languageApplyTemplateAndPublish(sourceLanguageHash: string, templateData: object): Promise<LanguageRef> {
        if (!this.#languageController) {
            throw Error("LanguageController not been init'd. Please init before calling language altering functions.")
        };
        let languageLanguageInput = await this.#languageController.languageApplyTemplateOnSource(sourceLanguageHash, templateData);
        return this.publish(languageLanguageInput)
    }

    async languagePublish(languagePath: string, languageMeta: LanguageMetaInternal): Promise<LanguageExpression> {
        if (!fs.existsSync(languagePath)) {
            throw new Error("Language at path: " + languagePath + " does not exist");
        };
        if (!this.#languageController) {
            throw Error("LanguageController not been init'd. Please init before calling language altering functions.")
        };
        
        const sourceLanguage = fs.readFileSync(languagePath).toString();
        const sourceLanguageLines = sourceLanguage!.split("\n");

        const languageLanguageInput = await this.#languageController.constructLanguageLanguageInput(sourceLanguageLines, languageMeta)
        const languageRef = await this.publish(languageLanguageInput)
        return (await this.#languageController.getLanguageExpression(languageRef.address))!
    }

    async publish(languageLanguageInput: LanguageLanguageInput): Promise<LanguageRef> {
        try {
            const address = await (this.#languageController!.getLanguageLanguage().expressionAdapter!.putAdapter as PublicSharing).createPublic(languageLanguageInput)
            return {
                address,
                //@ts-ignore
                name: languageLanguageInput.meta.name
            } as LanguageRef
        } catch(e) {
            console.error("Core.installAndPublish(): ERROR creating new language:", e)
            throw e
        } 
    }

    async pubKeyForLanguage(lang: string): Promise<Buffer> {
        return await this.#holochain!.pubKeyForLanguage(lang)
    }

    async holochainRequestAgentInfos(): Promise<RequestAgentInfoResponse> {
        return await this.#holochain!.requestAgentInfos()
    }

    async holochainAddAgentInfos(agent_infos: RequestAgentInfoResponse) {
        await this.#holochain!.addAgentInfos(agent_infos)
    }
}

export function create(config: Config.CoreConfig): PerspectivismCore {
    return new PerspectivismCore(config)
}