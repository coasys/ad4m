import { AdminWebsocket, AgentPubKey, AppSignalCb, AppWebsocket, CapSecret, AppSignal } from '@holochain/conductor-api'
import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import fs from 'fs'
import HolochainLanguageDelegate from "./HolochainLanguageDelegate"
import {stopProcesses, unpackDna, packDna, writeDefaultConductor, runHolochain, ConductorConfiguration} from "./HcExecution"
import type { Dna } from '@perspect3vism/ad4m'
import type { ChildProcess } from 'child_process'

export const fakeCapSecret = (): CapSecret => Buffer.from(Array(64).fill('aa').join(''), 'hex')

const bootstrapUrl = "https://bootstrap-staging.holo.host"
const kitsuneProxy = "kitsune-proxy://SYVd4CF3BdJ4DS7KwLLgeU3_DbHoZ34Y-qroZ79DOs8/kitsune-quic/h/165.22.32.11/p/5779/--"

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
    signalCallbacks: Map<string, [AppSignalCb, string]>;

    constructor(conductorPath: string, dataPath: string, resourcePath: string, adminPort?: number, appPort?: number) {
        this.#didResolveError = false;

        console.log("HolochainService: Creating low-db instance for holochain-serivce");
        this.#dataPath = dataPath
        this.#db = low(new FileSync(path.join(dataPath, 'holochain-service.json')))
        this.#db.defaults({pubKeys: []}).write()
        this.signalCallbacks = new Map();

        const holochainAppPort = appPort ? appPort : 1337;
        const holochainAdminPort = adminPort ? adminPort : 2000;
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
                bootstrapService: bootstrapUrl,
                conductorConfigPath: conductorConfigPath,
            } as ConductorConfiguration);
        };
    }

    handleCallback(signal: AppSignal) {
        // console.log(new Date().toISOString(), "GOT CALLBACK FROM HC, checking against language callbacks");
        if (this.signalCallbacks.size != 0) {
            let callbacks = this.signalCallbacks.get(signal.data.cellId[1].toString("base64"))
            if (callbacks![0] != undefined) {
                callbacks![0](signal);
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
                try {
                    this.#adminWebsocket = await AdminWebsocket.connect(`ws://localhost:${this.#adminPort}`)
                } catch (e) {
                    throw new Error(e)
                }
                try {
                    await this.#adminWebsocket.attachAppInterface({ port: this.#appPort })
                } catch {
                    console.warn("HolochainService: Could not attach app interface, assuming already attached...")
                }
                console.debug("HolochainService: Holochain admin interface connected on port", this.#adminPort);
            };
            if (this.#appWebsocket == undefined) {
                try {
                    this.#appWebsocket = await AppWebsocket.connect(`ws://localhost:${this.#appPort}`, 100000, this.handleCallback.bind(this))
                    console.debug("HolochainService: Holochain app interface connected on port", this.#appPort)
                } catch (e) {
                    throw new Error(e)
                }
            };
            resolveReady!()
            this.#didResolveError = false;
        } catch(e) {
            console.error("HolochainService: Error intializing Holochain conductor:", e)
            this.#didResolveError = true;
            resolveReady!()
        }
    }

    async stop() {
        await this.#ready
        if (this.#didResolveError) {
            console.error("HolochainService.stop: Warning attempting to close holochain processes when they did not start error free...")
        }
        if (this.#hcProcess && this.#lairProcess) {
            stopProcesses(this.#hcProcess, this.#lairProcess)
        }
    }

    unpackDna(dnaPath: string): string {
        return unpackDna(`${this.#resourcePath}/hc`, dnaPath)
    }

    packDna(workdirPath: string): string {
        return packDna(`${this.#resourcePath}/hc`, workdirPath)
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
        const pubKey = await this.pubKeyForLanguage(lang);
        if (callback != undefined) {
            console.log("HolochainService: setting holochains signal callback for language", lang);
            this.signalCallbacks.set(pubKey.toString("base64"), [callback, lang]);
        };
        const activeApps = await this.#adminWebsocket!.listActiveApps();
        //console.log("HolochainService: Found running apps:", activeApps);
        if(!activeApps.includes(lang)) {

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
                    await this.#adminWebsocket!.installApp({
                        installed_app_id: lang, agent_key: pubKey, dnas: [{hash: hash, nick: dna.nick}]
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
            console.debug("\x1b[31m", new Date().toISOString(), "HolochainService calling zome function:", dnaNick, zomeName, fnName, payload)
            const result = await this.#appWebsocket!.callZome({
                cap: fakeCapSecret(),
                cell_id,
                zome_name: zomeName,
                fn_name: fnName,
                provenance,
                payload
            })
            console.debug("\x1b[32m", new Date().toISOString(),"HolochainService zome function result:", result)
            return result
        } catch(e) {
            console.error("\x1b[31m", "HolochainService: ERROR calling zome function:", e)
            return e
        }
    }
}

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(() => resolve(), ms));