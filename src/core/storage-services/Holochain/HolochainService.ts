import { AdminWebsocket, AgentPubKey, AppSignalCb, AppWebsocket, CapSecret, AppSignal, CellId, RequestAgentInfoResponse, AppStatusFilter } from '@holochain/client'
import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import fs from 'fs'
import HolochainLanguageDelegate from "./HolochainLanguageDelegate"
import {stopProcesses, unpackDna, packDna, writeDefaultConductor, runHolochain, ConductorConfiguration} from "./HcExecution"
import type { Dna } from '@perspect3vism/ad4m'
import type { ChildProcess } from 'child_process'

import EntanglementProofController from '../../EntanglementProof'
import AgentService from '../../agent/AgentService'
import fetch from "node-fetch";
import yaml from 'js-yaml';


export const fakeCapSecret = (): CapSecret => Buffer.from(Array(64).fill('aa').join(''), 'hex')

const bootstrapUrl = "https://bootstrap-staging.holo.host"
const kitsuneProxy = "kitsune-proxy://SYVd4CF3BdJ4DS7KwLLgeU3_DbHoZ34Y-qroZ79DOs8/kitsune-quic/h/165.22.32.11/p/5779/--"
const signingServiceVersion = "0.0.2";

export interface HolochainConfiguration {
    conductorPath?: string, 
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
    #conductorPath?: string
    #didResolveError: boolean
    #conductorConfigPath?: string
    #signalCallbacks: [CellId, AppSignalCb, string][]
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
        this.#signalCallbacks = [];

        const holochainAppPort = appPort ? appPort : 1337;
        const holochainAdminPort = adminPort ? adminPort : 2000;
        if(useMdns === undefined) useMdns = true
        if(useBootstrap === undefined) useBootstrap = true
        if(useProxy === undefined) useProxy = true
        if(useLocalProxy === undefined) useLocalProxy = false
        this.#adminPort = holochainAdminPort;
        this.#appPort = holochainAppPort;
        this.#resourcePath = resourcePath;

        if (conductorPath) {
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
            } else {
                const config = yaml.load(fs.readFileSync(conductorConfigPath, 'utf-8')) as any;
                const adminPort = config.admin_interfaces[0].driver.port as number;
    
                if (adminPort !== this.#adminPort) {
                    console.debug(`HC PORT: ${this.#adminPort} supplied is different than the PORT: ${adminPort} set in config, using the config port`);
                    this.#adminPort = adminPort;
                }
            }
        }
    }

    handleCallback(signal: AppSignal) {
        console.debug(new Date().toISOString(), "GOT CALLBACK FROM HC, checking against language callbacks", signal);
        //console.debug("registered callbacks:", this.#signalCallbacks)
        if (this.#signalCallbacks.length != 0) {
            const signalDna = Buffer.from(signal.data.cellId[0]).toString('hex')
            const signalPubkey = Buffer.from(signal.data.cellId[1]).toString('hex')
            //console.debug("Looking for:", signalDna, signalPubkey)
            let callbacks = this.#signalCallbacks.filter(e => {
                const dna = Buffer.from(e[0][0]).toString('hex')
                const pubkey = Buffer.from(e[0][1]).toString('hex')
                //console.debug("Checking:", dna, pubkey)
                return ( dna === signalDna ) && (pubkey === signalPubkey)
            })
            callbacks.forEach(cb => {
                if (cb && cb![1] != undefined) {
                    cb![1](signal);
                };
            })
        };
    }

    async connect() {
        let resolveReady: ((value: void | PromiseLike<void>) => void) | undefined;
        this.#ready = new Promise(resolve => resolveReady = resolve)
        
        console.log("Connecting to holochain process.");

        try {
            if (this.#adminWebsocket == undefined) {
                this.#adminWebsocket = await AdminWebsocket.connect(`ws://localhost:${this.#adminPort}`)
                console.debug("HolochainService: Holochain admin interface connected on port", this.#adminPort);
                try {
                    await this.#adminWebsocket!.attachAppInterface({ port: this.#appPort });
                } catch {
                    console.warn("HolochainService: Could not attach app interface on port", this.#appPort, ", assuming already attached...");
                }
            };
            if (this.#appWebsocket == undefined) {
                this.#appWebsocket = await AppWebsocket.connect(`ws://localhost:${this.#appPort}`, 100000, this.handleCallback.bind(this))
                console.debug("HolochainService: Holochain app interface connected on port", this.#appPort)
            };

            //Install signing service DNA
            const activeApps = await this.#adminWebsocket!.listApps({status_filter: AppStatusFilter.Enabled});
            if (!activeApps.map(value => value.installed_app_id).includes("signing_service")) {
                const pubKey = await this.pubKeyForLanguage("main");

                const dest = path.join(this.#dataPath, "signing.dna");
                const res = await fetch(`https://github.com/perspect3vism/signing-service/releases/download/${signingServiceVersion}/signing.dna`);
                const fileStream = fs.createWriteStream(dest);
                await new Promise((resolve, reject) => {
                    res.body.pipe(fileStream);
                    res.body.on("error", reject);
                    fileStream.on("finish", resolve);
                });

                const hash = await this.#adminWebsocket!.registerDna({
                    path: dest
                })

                const installedApp = await this.#adminWebsocket!.installApp({
                    installed_app_id: "signing_service", agent_key: pubKey, dnas: [{hash: hash, role_id: "crypto"}]
                })
                this.#signingService = installedApp.cell_data[0].cell_id;

                try {
                    await this.#adminWebsocket!.activateApp({installed_app_id: "signing_service"})
                } catch(e) {
                    console.error("HolochainService: ERROR activating app signing_service", " - ", e)
                }
            } else {
                const { cell_data } = await this.#appWebsocket!.appInfo({installed_app_id: "signing_service"})
                const cell = cell_data.find(c => c.role_id === "crypto")
                if(!cell) {
                    const e = new Error(`No DNA with nick signing_service found for language signing service DNA`)
                    throw e
                }
                this.#signingService = cell.cell_id;
            }

            resolveReady!()
            this.#didResolveError = false;
        } catch(e) {
            console.error("HolochainService: connect Holochain process with error:", e)
            this.#didResolveError = true;
            resolveReady!()
        }
    }

    async run() {
        let resolveReady: ((value: void | PromiseLike<void>) => void) | undefined;
        this.#ready = new Promise(resolve => resolveReady = resolve)
        if (this.#conductorPath == undefined || this.#conductorConfigPath == undefined) {
            console.error("HolochainService: Error intializing Holochain conductor, conductor path is invalid")
            this.#didResolveError = true;
            resolveReady!()
            return
        }
        let hcProcesses = await runHolochain(this.#resourcePath, this.#conductorConfigPath, this.#conductorPath);
        [this.#hcProcess, this.#lairProcess] = hcProcesses;
        console.log("HolochainService: Holochain running... Attempting connection\n\n\n");

        await this.connect();
        
        resolveReady!()
        this.#didResolveError = false;
    }

    async callSigningService(data: string): Promise<string> {
        if (!this.#signingService) {
            throw new Error("Signing service DNA is not init'd yet!")
        }
        const pubKey = await this.pubKeyForLanguage("main");
        const result = await this.#appWebsocket!.callZome({
            cap_secret: null,
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

    async pubKeyForAllLanguages(): Promise<AgentPubKey> {
        const alreadyExisting = this.#db.get('pubKeys').find({lang: "global"}).value()
        if(alreadyExisting) {
            const pubKey = Buffer.from(alreadyExisting.pubKey)
            console.debug("Found existing pubKey", pubKey.toString("base64"), "for all languages")
            return pubKey
        } else {
            const pubKey = await this.#adminWebsocket!.generateAgentPubKey()
            this.#db.get('pubKeys').push({lang: "global", pubKey}).write()
            console.debug("Created new pubKey", Buffer.from(pubKey).toString("base64"), "for all languages")
            return pubKey
        }
    }

    async pubKeyForLanguage(lang: string): Promise<AgentPubKey> {
        return this.pubKeyForAllLanguages()

        // TODO using the same key for all DNAs should only be a temporary thing.
        const alreadyExisting = this.#db.get('pubKeys').find({lang}).value()
        if(alreadyExisting) {
            const pubKey = Buffer.from(alreadyExisting.pubKey)
            console.debug("Found existing pubKey", pubKey.toString("base64"), "for language:", lang)
            return pubKey
        } else {
            const pubKey = await this.#adminWebsocket!.generateAgentPubKey()
            this.#db.get('pubKeys').push({lang, pubKey}).write()
            console.debug("Created new pubKey", Buffer.from(pubKey).toString("base64"), "for language", lang)
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
                    const did = this.#agentService.did;
                    //Did should only ever be undefined when the system DNA's get init'd before agent create occurs
                    //These system DNA's do not currently need EP proof's
                    let membraneProof;
                    if(did) {
                        const signedDid = await this.callSigningService(did);
                        const didHolochainEntanglement = await this.#entanglementProofController!.generateHolochainProof(Buffer.from(pubKey).toString("base64"), signedDid);
                        membraneProof = Buffer.from(JSON.stringify({"ad4mDidEntanglement": didHolochainEntanglement}));
                    }
                    //The membrane proof passing here is untested and thus most likely broken
                    await this.#adminWebsocket!.installApp({
                        installed_app_id: lang, agent_key: pubKey, dnas: [{hash: hash, role_id: dna.nick, membrane_proof: membraneProof}]
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
        }

        if (callback != undefined) {
            console.log("HolochainService: setting holochains signal callback for language", lang);
            const { cell_data } = await this.#appWebsocket!.appInfo({installed_app_id: lang})
            this.#signalCallbacks.push([cell_data[0].cell_id, callback, lang]);
        };
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

        const cell = cell_data.find(c => c.role_id === dnaNick)
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
                cap_secret: fakeCapSecret(),
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