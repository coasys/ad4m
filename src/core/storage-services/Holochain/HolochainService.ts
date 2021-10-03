import { AdminWebsocket, AgentPubKey, AppSignalCb, AppWebsocket, CapSecret, AppSignal, AppStatusFilter } from '@holochain/conductor-api'
import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import fs from 'fs'
import HolochainLanguageDelegate from "./HolochainLanguageDelegate"
import {stopProcesses, unpackDna, packDna, writeDefaultConductor, runHolochain, ConductorConfiguration} from "./HcExecution"
import type { Dna } from '@perspect3vism/ad4m'
import type { ChildProcess } from 'child_process'
import { RequestAgentInfoResponse } from '@holochain/conductor-api'
import EntanglementProofController from '../../EntanglementProof'
import AgentService from '../../agent/AgentService'
import { CellId } from '@holochain/conductor-api'

export const fakeCapSecret = (): CapSecret => Buffer.from(Array(64).fill('aa').join(''), 'hex')

const bootstrapUrl = "https://bootstrap-staging.holo.host"
const kitsuneProxy = "kitsune-proxy://SYVd4CF3BdJ4DS7KwLLgeU3_DbHoZ34Y-qroZ79DOs8/kitsune-quic/h/165.22.32.11/p/5779/--"

export interface HolochainConfiguration {
    conductorPath: string, 
    dataPath: string, 
    resourcePath: string
    adminPort?: number;
    appPort?: number;
    useBootstrap?: boolean,
    useProxy?: boolean,
    useLocalProxy?: boolean;
    useMdns?: boolean;
}

export default class HolochainService {
    #db: any
    #adminPort: number
    #appPort: number
    #adminWebsocket?: AdminWebsocket
    #appWebsocket?: AppWebsocket
    #dataPath: string
    #ready?: Promise<void>
    #hcProcess?: ChildProcess
    #lairProcess?: ChildProcess
    #resourcePath: string
    #conductorPath: string
    #didResolveError: boolean
    #conductorConfigPath: string
    signalCallbacks: Map<string, AppSignalCb[]>;
    #agentService: AgentService
    #entanglementProofController?: EntanglementProofController
    #signingService?: CellId

    constructor(config: HolochainConfiguration, agentService: AgentService, entanglementProofController?: EntanglementProofController) {
        let {
            conductorPath, 
            dataPath, 
            resourcePath,
            adminPort,
            appPort,
            useBootstrap,
            useProxy,
            useLocalProxy,
            useMdns,
        } = config;

        this.#didResolveError = false;
        this.#agentService = agentService;
        this.#entanglementProofController = entanglementProofController;

        console.log("HolochainService: Creating low-db instance for holochain-serivce");
        this.#dataPath = dataPath
        this.#db = low(new FileSync(path.join(dataPath, 'holochain-service.json')))
        this.#db.defaults({pubKeys: []}).write()
        this.signalCallbacks = new Map();

        const holochainAppPort = appPort ? appPort : 1337;
        const holochainAdminPort = adminPort ? adminPort : 2000;
        if(useMdns === undefined) useMdns = true
        if(useBootstrap === undefined) useBootstrap = true
        if(useProxy === undefined) useProxy = true
        if(useLocalProxy === undefined) useLocalProxy = false
        this.#adminPort = holochainAdminPort;
        this.#appPort = holochainAppPort;
        this.#resourcePath = resourcePath;
        this.#conductorPath = conductorPath;
    
        let conductorConfigPath = path.join(conductorPath, "conductor-config.yaml");
        this.#conductorConfigPath = conductorConfigPath;
        if (!fs.existsSync(conductorConfigPath)) {
            writeDefaultConductor({
                proxyUrl: kitsuneProxy,
                environmentPath: conductorPath,
                adminPort: holochainAdminPort,
                appPort: holochainAppPort,
                useBootstrap,
                bootstrapService: bootstrapUrl,
                conductorConfigPath: conductorConfigPath,
                useProxy,
                useLocalProxy,
                useMdns
            } as ConductorConfiguration);
        };
    }

    handleCallback(signal: AppSignal) {
        // console.log(new Date().toISOString(), "GOT CALLBACK FROM HC, checking against language callbacks");
        if (this.signalCallbacks.size != 0) {
            let callbacks = this.signalCallbacks.get(signal.data.cellId[0].toString("hex"))
            if (callbacks && callbacks! != undefined) {
                //TODO: test that these multiple callbacks work correctly
                for (const callback of callbacks) {
                    callback(signal)
                }
            };
        };
    }

    async run() {
        let resolveReady: ((value: void | PromiseLike<void>) => void) | undefined;
        this.#ready = new Promise(resolve => resolveReady = resolve)
        let hcProcesses = await runHolochain(this.#resourcePath, this.#conductorConfigPath, this.#conductorPath);
        console.log("HolochainService: Holochain running... Attempting connection\n\n\n");
        [this.#hcProcess, this.#lairProcess] = hcProcesses;
        try {
            if (this.#adminWebsocket == undefined) {
                this.#adminWebsocket = await AdminWebsocket.connect(`ws://localhost:${this.#adminPort}`)

                try {
                    await this.#adminWebsocket.attachAppInterface({ port: this.#appPort })
                } catch {
                    console.warn("HolochainService: Could not attach app interface on port", this.#appPort, ", assuming already attached...")
                }
                console.debug("HolochainService: Holochain admin interface connected on port", this.#adminPort);
            };
            if (this.#appWebsocket == undefined) {
                this.#appWebsocket = await AppWebsocket.connect(`ws://localhost:${this.#appPort}`, 100000, this.handleCallback.bind(this))
                console.debug("HolochainService: Holochain app interface connected on port", this.#appPort)
            };

            //Install signing service DNA
            const activeApps = await this.#adminWebsocket!.listActiveApps();
            if (!activeApps.includes("signing_service")) {
                const pubKey = await this.pubKeyForLanguage("main");

                const hash = await this.#adminWebsocket!.registerDna({
                    //Pretty sure this is not gonna work in production
                    path: path.join(__dirname, "../../../../public/signing.dna")
                })

                const installedApp = await this.#adminWebsocket!.installApp({
                    installed_app_id: "signing_service", agent_key: pubKey, dnas: [{hash: hash, nick: "crypto"}]
                })
                this.#signingService = installedApp.cell_data[0].cell_id;

                try {
                    await this.#adminWebsocket!.activateApp({installed_app_id: "signing_service"})
                } catch(e) {
                    console.error("HolochainService: ERROR activating app signing_service", " - ", e)
                }
            } else {
                const { cell_data } = await this.#appWebsocket!.appInfo({installed_app_id: "signing_service"})
                const cell = cell_data.find(c => c.cell_nick === "crypto")
                if(!cell) {
                    const e = new Error(`No DNA with nick signing_service found for language signing service DNA`)
                    throw e
                }
                this.#signingService = cell.cell_id;
            }

            resolveReady!()
            this.#didResolveError = false;
        } catch(e) {
            console.error("HolochainService: Error intializing Holochain conductor:", e)
            this.#didResolveError = true;
            resolveReady!()
        }
    }

    async callSigningService(data: string): Promise<string> {
        if (!this.#signingService) {
            throw new Error("Signing service DNA is not init'd yet!")
        }
        const pubKey = await this.pubKeyForLanguage("main");
        const result = await this.#appWebsocket!.callZome({
            cap: null,
            cell_id: this.#signingService!,
            zome_name: "crypto",
            fn_name: "sign",
            payload: data,
            provenance: pubKey
        })
        return result.toString("hex")
    }

    async stop() {
        await this.#ready
        console.log("HolochainService.stop(): Stopping holochain and lair processes");
        if (this.#didResolveError) {
            console.error("HolochainService.stop: Warning attempting to close holochain processes when they did not start error free...")
        }
        if (this.#hcProcess && this.#lairProcess) {
            stopProcesses(this.#hcProcess, this.#lairProcess)
        }
    }

    unpackDna(dnaPath: string): string {
        let result = unpackDna(`${this.#resourcePath}/hc`, dnaPath);
        let splitResult = result.split("Unpacked to directory ");
        if (splitResult.length == 2) {
            return splitResult[1]
        } else {
            return result
        }
    }

    packDna(workdirPath: string): string {
        let result = packDna(`${this.#resourcePath}/hc`, workdirPath);
        let splitResult = result.split("Wrote bundle ");
        if (splitResult.length == 2) {
            return splitResult[1]
        } else {
            return result
        }
    }

    async pubKeyForLanguage(lang: string): Promise<AgentPubKey> {
        const alreadyExisting = this.#db.get('pubKeys').find({lang}).value()
        if(alreadyExisting) {
            const pubKey = Buffer.from(alreadyExisting.pubKey)
            console.debug("Found existing pubKey", pubKey.toString("base64"), "for language:", lang)
            return pubKey
        } else {
            const pubKey = await this.#adminWebsocket!.generateAgentPubKey()
            this.#db.get('pubKeys').push({lang, pubKey}).write()
            console.debug("Created new pubKey", pubKey.toString("base64"), "for language", lang)
            return pubKey
        }
    }

    async ensureInstallDNAforLanguage(lang: string, dnas: Dna[], callback: AppSignalCb | undefined) {
        await this.#ready
        if (this.#didResolveError) {
            console.error("HolochainService.ensureInstallDNAforLanguage: Warning attempting to install holochain DNA when conductor did not start error free...")
        }
        const pubKey = await this.pubKeyForLanguage("main");
        const activeApps = await this.#adminWebsocket!.listApps({status_filter: AppStatusFilter.Enabled});
        // console.log("HolochainService: Found running apps:", activeApps);
        if(!activeApps.map(value => value.installed_app_id).includes(lang)) {

            let installed
            // 1. install app
            try {
                console.debug("HolochainService: Installing DNAs for language", lang);
                // console.debug(dnaFile)
                // let installedCellIds = await this.#adminWebsocket.listCellIds()
                // console.debug("HolochainService: Installed cells before:", installedCellIds)
                // const cellId = HolochainService.dnaID(lang, nick)

                for (let dna of dnas) {
                    //console.log("HolochainService: Installing DNA:", dna, "at data path:", this.#dataPath, "\n");
                    const p = path.join(this.#dataPath, `${lang}-${dna.nick}.dna`);
                    fs.writeFileSync(p, dna.file);
                    const hash = await this.#adminWebsocket!.registerDna({
                        path: p
                    })
                    if (callback != undefined) {
                        console.log("HolochainService: setting holochains signal callback for language", lang);
                        const hashHex = hash.toString("hex");
                        let callbacks = this.signalCallbacks.get(hashHex);
                        let newCallbacks = [];
                        if (callbacks) {
                            callbacks.push(callback);
                            newCallbacks = callbacks;
                        } else {
                            newCallbacks = [callback] as AppSignalCb[]
                        }
                        this.signalCallbacks.set(hashHex, newCallbacks);
                    };
                    const did = this.#agentService.did;
                    //Did should only ever be undefined when the system DNA's get init'd before agent create occurs
                    //These system DNA's do not currently need EP proof's
                    let membraneProof;
                    if(did) {
                        const signedDid = await this.callSigningService(did);
                        const didHolochainEntanglement = await this.#entanglementProofController!.generateHolochainProof(pubKey.toString("hex"), signedDid);
                        membraneProof = Buffer.from(JSON.stringify({"ad4mDidEntanglement": didHolochainEntanglement}));
                    }
                    //The membrane proof passing here is untested and thus most likely broken
                    await this.#adminWebsocket!.installApp({
                        installed_app_id: lang, agent_key: pubKey, dnas: [{hash: hash, nick: dna.nick, membrane_proof: membraneProof}]
                    })
                }
                installed = true
            } catch(e) {
                // if(!e.data?.data?.indexOf('AppAlreadyInstalled')) {
                //     console.error("Error during install of DNA:", e)
                //     installed = false
                // } else {
                console.error(e);
                installed = false
            }

            if(!installed)
                return

            // 2. activate app
            try {
                await this.#adminWebsocket!.activateApp({installed_app_id: lang})
            } catch(e) {
                console.error("HolochainService: ERROR activating app", lang, " - ", e)
            }
        } else {
            for (let dna of dnas) {
                if (callback != undefined) {
                    console.log("HolochainService: setting holochains signal callback for language", lang);
                    let infoResult = await this.#appWebsocket!.appInfo({installed_app_id: lang})
                    const { cell_data } = infoResult
                    const cell = cell_data.find(c => c.cell_nick === dna.nick)
                    if(!cell) {
                        const e = new Error(`No DNA with nick '${dna.nick}' found for language ${lang}`)
                        console.error(e)
                        return e
                    }
                    const hash = cell.cell_id[0];
                    
                    const hashHex = hash.toString("hex");
                    let callbacks = this.signalCallbacks.get(hashHex);
                    let newCallbacks = [];
                    if (callbacks) {
                        callbacks.push(callback);
                        newCallbacks = callbacks;
                    } else {
                        newCallbacks = [callback] as AppSignalCb[]
                    }
                    this.signalCallbacks.set(hashHex, newCallbacks);
                };
            }
        }
    }

    getDelegateForLanguage(languageHash: string) {
        return new HolochainLanguageDelegate(languageHash, this)
    }

    static dnaID(languageHash: string, dnaNick: string) {
        return `${languageHash}-${dnaNick}`
    }

    async callZomeFunction(lang: string, dnaNick: string, zomeName: string, fnName: string, payload: object|string): Promise<any> {
        await this.#ready
        if (this.#didResolveError) {
            console.error("HolochainService.callZomeFunction: Warning attempting to call zome function when conductor did not start error free...")
        }
        const installed_app_id = lang
        //console.debug("HolochainService.callZomefunction: getting info for app:", installed_app_id)
        let infoResult = await this.#appWebsocket!.appInfo({installed_app_id})
        let tries = 1
        while(!infoResult && tries < 10) {
            await sleep(500)
            infoResult = await this.#appWebsocket!.appInfo({installed_app_id})
            tries++
        }

        if(!infoResult) {
            console.error("HolochainService: no installed hApp found during callZomeFunction() for Language:", lang)
            console.error("Did the Language forget to register a DNA?")
            throw new Error("No DNA installed")
        }

        //console.debug("HolochainService.callZomefunction: get info result:", infoResult)
        const { cell_data } = infoResult
        if(cell_data.length === 0) {
            console.error("HolochainService: tried to call zome function without any installed cell!")
            return null
        }

        const cell = cell_data.find(c => c.cell_nick === dnaNick)
        if(!cell) {
            const e = new Error(`No DNA with nick '${dnaNick}' found for language ${installed_app_id}`)
            console.error(e)
            return e
        }

        //console.debug("HolochainService: found cell", cell);
        const cell_id = cell.cell_id
        const [_dnaHash, provenance] = cell_id

        try {
            console.debug("\x1b[31m", new Date().toISOString(), "HolochainService calling zome function:", dnaNick, zomeName, fnName, payload, "\nFor language with address", lang, "\x1b[0m")
            const result = await this.#appWebsocket!.callZome({
                cap: null,
                cell_id,
                zome_name: zomeName,
                fn_name: fnName,
                provenance,
                payload
            })
            console.debug("\x1b[32m", new Date().toISOString(),"HolochainService zome function result:", result, "\x1b[0m")
            return result
        } catch(e) {
            console.error("\x1b[31m", "HolochainService: ERROR calling zome function:", e, "\x1b[0m")
            return e
        }
    }

    async requestAgentInfos(): Promise<RequestAgentInfoResponse> {
        return await this.#adminWebsocket!.requestAgentInfo({cell_id: null})
    }

    async addAgentInfos(agent_infos: RequestAgentInfoResponse) {
        await this.#adminWebsocket!.addAgentInfo({ agent_infos })
    }
}

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(() => resolve(), ms));