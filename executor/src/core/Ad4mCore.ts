import type { Address, PublicSharing, PerspectiveHandle, Perspective, LanguageLanguageInput, LanguageExpression, LanguageMetaInput, AgentExpression, Language, NeighbourhoodExpression  } from '@coasys/ad4m'
import { parseExprUrl, LanguageRef, Neighbourhood, PerspectiveState } from '@coasys/ad4m'

import * as Config from './Config'
import * as Db from './db'
import type { Ad4mDb } from './db'
import HolochainService, { HolochainConfiguration } from './storage-services/Holochain/HolochainService';
import AgentService from './agent/AgentService'
import LanguageController from './LanguageController'
import * as DIDs from './agent/DIDs'
import type { DIDResolver } from './agent/DIDs'
import * as PubSubDefinitions from './graphQL-interface/SubscriptionDefinitions'
import fs from 'node:fs'
import { AgentInfoResponse } from '@holochain/client'
import { v4 as uuidv4 } from 'uuid';
import { MainConfig } from './Config'
import { getPubSub, sleep } from "./utils";

export interface InitServicesParams {
    agentService: AgentService,
}
export interface InitIPFSParams {
    ipfsSwarmPort?: number,
    ipfsRepoPath?: string,
}
export interface InitHolochainParams {
    hcPortAdmin?: number,
    hcPortApp?: number,
    hcUseBootstrap?: boolean,
    hcUseProxy?: boolean,
    hcUseLocalProxy?: boolean,
    hcUseMdns?: boolean,
    passphrase?: string
    hcProxyUrl: string,
    hcBootstrapUrl: string,
    hcRelayUrl?: string,
    logHolochainMetrics?: boolean
}

export interface HolochainUnlockConfiguration extends HolochainConfiguration {
    passphrase: string;
}

export interface ConnectHolochainParams {
    hcPortAdmin: number,
    hcPortApp: number,
}

export default class Ad4mCore {
    #config: MainConfig;
    #holochain?: HolochainService
    //#IPFS?: IPFSType

    #agentService: AgentService
    #db: Ad4mDb
    #didResolver: DIDResolver

    #languageController?: LanguageController

    #languagesReady: Promise<void>
    #resolveLanguagesReady: (value: void) => void

    resolvers: any

    constructor(config: Config.CoreConfig) {
        this.#config = Config.init(config);

        this.#agentService = new AgentService(this.#config.rootConfigPath, this.#config.adminCredential)
        const agent = AGENT.load();
        this.#db = Db.init(this.#config.dataPath)
        this.#didResolver = DIDs.init(this.#config.dataPath)
        const that = this
        this.#resolveLanguagesReady = () => {}
        this.#languagesReady = new Promise(resolve => {
            that.#resolveLanguagesReady = resolve
        })
    }

    async callResolver (type: string, fnName: string, args: any, context: any) {
        //console.log("Calling resolvers with data", type, fnName, args, context);
      if(!this.resolvers[type]) throw new Error(`Could not find resolver for type ${type}`)
      if(!this.resolvers[type][fnName]) throw new Error(`Could not find resolver function ${fnName} for type ${type}`)
      try {
        let result;
        if (args && context) {
            result = await this.resolvers[type][fnName](args, context);
        }
        if (!args) {
            result = await this.resolvers[type][fnName](context);
        }
        if (!context) {
            result = await this.resolvers[type][fnName](args);
        }
        if (!args && !context) {
            result = await this.resolvers[type][fnName]();
        }
        return {"Ok": result}
      } catch (error) {
        //@ts-ignore
        if (typeof error.message === "object") {
            //@ts-ignore
            return {"Error": JSON.stringify(error.message)};
        } else {
            //@ts-ignore
            return {"Error": error.message}
        }
      }
    }

    get holochainService(): HolochainService | undefined {
        return this.#holochain
    }

    get agentService(): AgentService {
        return this.#agentService
    }

    get languageController(): LanguageController {
        if (!this.#languageController) {
            throw Error("No languageController")
        }
        return this.#languageController!
    }

    async exit() {
        console.log("Exiting gracefully...")
        console.log("Stopping Holochain conductor")
        await this.#holochain?.stop();
        console.log("Done.")
    }

    async initIPFS(params: InitIPFSParams) {
        console.log("Init IPFS service with port ", params.ipfsSwarmPort, " at path: ", params.ipfsRepoPath);

        //let ipfs = await IPFS.init(params.ipfsSwarmPort, params.ipfsRepoPath);
        //this.#IPFS = ipfs;
    }

    async initHolochain(params: InitHolochainParams) {
        console.log("Init HolochainService with data path: ", this.#config.holochainDataPath, ". Conductor path: ", this.#config.holochainConductorPath, ". Resource path: ", this.#config.resourcePath)
        console.log(`Holochain ports: admin=${params.hcPortAdmin} app=${params.hcPortApp}`);

        const holochainConfig: HolochainConfiguration = {
            conductorPath: this.#config.holochainConductorPath,
            dataPath: this.#config.holochainDataPath,
            resourcePath: this.#config.resourcePath,
            adminPort: params.hcPortAdmin,
            appPort: params.hcPortApp,
            useBootstrap: params.hcUseBootstrap,
            useProxy: params.hcUseProxy,
            useLocalProxy: params.hcUseLocalProxy,
            useMdns: params.hcUseMdns,
            hcProxyUrl: params.hcProxyUrl,
            hcBootstrapUrl: params.hcBootstrapUrl,
            hcRelayUrl: params.hcRelayUrl,
            logHolochainMetrics: this.#config.logHolochainMetrics
        }

        this.#holochain = new HolochainService(holochainConfig)
        await this.#holochain.run({
            ...holochainConfig,
            passphrase: params.passphrase!
        });
    }

    async waitForLanguages(): Promise<void> {
        return this.#languagesReady
    }

    async languageSignal(signal: any) {
        // //@ts-ignore
        // console.log(new Date().toISOString(), "Ad4mCore.languageSignal: Got signal");
        let pubSub = getPubSub();
        //@ts-ignore
        await pubSub.publish(PubSubDefinitions.SIGNAL, { signal: JSON.stringify(signal), language: this.language });
    }

    initControllers() {
        this.#languageController = new LanguageController({
            agent: this.#agentService,
            //IPFS: this.#IPFS,
            ad4mSignal: this.languageSignal,
            config: this.#config,
        }, { holochainService: this.#holochain!, db: this.#db } )
    }

    async initLanguages() {
        await this.#languageController!.loadLanguages()
        this.#resolveLanguagesReady()
    }

    async languageApplyTemplateAndPublish(sourceLanguageHash: string, templateData: object): Promise<LanguageRef> {
        if (!this.#languageController) {
            throw Error("LanguageController not been init'd. Please init before calling language altering functions.")
        };
        let languageLanguageInput = await this.#languageController.languageApplyTemplateOnSource(sourceLanguageHash, templateData);
        return this.publish(languageLanguageInput)
    }

    async languagePublish(languagePath: string, languageMeta: LanguageMetaInput): Promise<LanguageExpression> {
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

    async holochainRequestAgentInfos(): Promise<AgentInfoResponse> {
        return await this.#holochain!.requestAgentInfos()
    }

    async holochainAddAgentInfos(agent_infos: AgentInfoResponse) {
        await this.#holochain!.addAgentInfos(agent_infos)
    }

    async connectToAd4mHolochainNode() {
        // //@ts-ignore
        // await this.holochainAddAgentInfos(PERSPECT3VIMS_AGENT_INFO())
        console.debug("Added AD4M Holochain agent infos.")
    }

    async initializeAgentsDirectMessageLanguage() {
        console.log("wait for languages");
        await this.waitForLanguages()
        console.log("finished wait");
        const agent = AGENT.agent();
        if(agent.directMessageLanguage) return
        console.log("Agent doesn't have direct message language set yet. Creating from template...")

        console.log("Cloning direct message language from template...");
        const templateParams = {
            uid: uuidv4(),
            recipient_did: agent?.did,
            recipient_hc_agent_pubkey: Buffer.from(await HOLOCHAIN_SERVICE.getAgentKey()).toString('hex')
        }
        console.debug("Now creating clone with parameters:", templateParams)
        const createdDmLang = await this.languageApplyTemplateAndPublish(this.#config.directMessageLanguage, templateParams)
        console.debug("DM Language cloned...")
        // Install language by calling languageByRef
        // TODO: extract language installing code into its own function
        await this.#languageController?.languageByRef(createdDmLang)
        console.debug("DM Language installed...")
        agent.directMessageLanguage = createdDmLang.address
        await this.#agentService.updateAgent(agent)
        console.debug("DM Language published...")
        console.log("Agent's direct message language successfully cloned, installed and published!")
    }

    async friendsDirectMessageLanguage(did: string): Promise<Language | null> {
        const expression = await this.#languageController!.getAgentLanguage().expressionAdapter?.get(did)! as AgentExpression
        //console.log("AGENT EXPRESSION:", expression)
        if(!expression) return null
        const dmLang = expression.data.directMessageLanguage
        //console.log("DM LANG", dmLang)
        if(dmLang)
            return await this.#languageController!.languageByRef(new LanguageRef(dmLang))
        else
            return null
    }

    async myDirectMessageLanguage(): Promise<Language> {
        const agent = AGENT.agent();
        const dmLang = agent!.directMessageLanguage!
        return await this.#languageController!.languageByRef(new LanguageRef(dmLang))
    }
}

export function create(config: Config.CoreConfig): Ad4mCore {
    return new Ad4mCore(config)
}