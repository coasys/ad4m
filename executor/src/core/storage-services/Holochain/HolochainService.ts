import { AdminWebsocket, AgentPubKey, AppSignalCb, AppWebsocket, CapSecret, AppSignal, CellId, AppStatusFilter, authorizeNewSigningKeyPair, AgentInfoResponse } from '@holochain/client'
import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import fs from 'fs'
import HolochainLanguageDelegate from "./HolochainLanguageDelegate"
import {stopProcesses, unpackDna, packDna, writeDefaultConductor, runHolochain, ConductorConfiguration} from "./HcExecution"
import type { Dna } from '@perspect3vism/ad4m'
import type { ChildProcess } from 'child_process'
import yaml from 'js-yaml';
import { AsyncQueue } from './Queue'
import { HolochainUnlockConfiguration } from '../../PerspectivismCore'

export const fakeCapSecret = (): CapSecret => Buffer.from(Array(64).fill('aa').join(''), 'hex')

export const bootstrapUrl = "https://bootstrap.holo.host"
export const kitsuneProxy = "kitsune-proxy://CIW6PxKxsPPlcuvUCbMcKwUpaMSmB7kLD8xyyj4mqcw/kitsune-quic/h/proxy.holochain.org/p/5778/--"

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
    #resourcePath: string
    #conductorPath?: string
    #didResolveError: boolean
    #conductorConfigPath?: string
    #signalCallbacks: [CellId, AppSignalCb, string][];
    #queue: AsyncQueue

    constructor(config: HolochainConfiguration) {
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

        console.log("HolochainService: Creating low-db instance for holochain-serivce");
        this.#dataPath = dataPath
        this.#db = low(new FileSync(path.join(dataPath, 'holochain-service.json')))
        this.#db.defaults({pubKeys: []}).write()
        this.#signalCallbacks = [];

        const holochainAppPort = appPort ? appPort : 1337;
        const holochainAdminPort = adminPort ? adminPort : 2000;
        if(useMdns === undefined) useMdns = false
        if(useBootstrap === undefined) useBootstrap = true
        if(useProxy === undefined) useProxy = true
        if(useLocalProxy === undefined) useLocalProxy = false
        this.#adminPort = holochainAdminPort;
        this.#appPort = holochainAppPort;
        this.#resourcePath = resourcePath;
        this.#queue = new AsyncQueue();

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
                    useMdns,
                    lairConnectionUrl: ''
                } as ConductorConfiguration);
            } else {
                const config = yaml.load(fs.readFileSync(conductorConfigPath, 'utf-8')) as any;
                const adminPort = config.admin_interfaces[0].driver.port as number;
    
                if (adminPort !== this.#adminPort) {
                    console.debug(`HC PORT: ${this.#adminPort} supplied is different than the PORT: ${adminPort} set in config, updating conductor config`);
                    config.admin_interfaces[0].driver.port = this.#adminPort;
                    fs.writeFileSync(conductorConfigPath, yaml.dump(config, {
                        'styles': {
                          '!!null': 'canonical' // dump null as ~
                        }
                    }));
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
                this.#adminWebsocket = await AdminWebsocket.connect(`ws://127.0.0.1:${this.#adminPort}`)
                console.debug("HolochainService: Holochain admin interface connected on port", this.#adminPort);
                try {
                    await this.#adminWebsocket!.attachAppInterface({ port: this.#appPort });
                } catch (e) {
                    console.warn("HolochainService: Could not attach app interface on port", this.#appPort, ", assuming already attached...", e);
                }
            };
            if (this.#appWebsocket == undefined) {
                this.#appWebsocket = await AppWebsocket.connect(`ws://127.0.0.1:${this.#appPort}`, 100000);
                this.#appWebsocket.on('signal', this.handleCallback.bind(this))
                console.debug("HolochainService: Holochain app interface connected on port", this.#appPort)
            };
            resolveReady!()
            this.#didResolveError = false;
        } catch(e) {
            console.error("HolochainService: connect Holochain process with error:", e)
            this.#didResolveError = true;
            resolveReady!()
        }
    }

    async run(config: HolochainUnlockConfiguration) {
        let resolveReady: ((value: void | PromiseLike<void>) => void) | undefined;
        this.#ready = new Promise(resolve => resolveReady = resolve)
        if (this.#conductorPath == undefined || this.#conductorConfigPath == undefined) {
            console.error("HolochainService: Error intializing Holochain conductor, conductor path is invalid")
            this.#didResolveError = true;
            resolveReady!()
            return
        }
        let hcProcesses = await runHolochain(this.#resourcePath, this.#conductorConfigPath, this.#conductorPath, config);
        this.#hcProcess = hcProcesses;
        console.log("HolochainService: Holochain running... Attempting connection\n\n\n");

        await this.connect();
        
        resolveReady!()
        this.#didResolveError = false;
    }

    async stop() {
        await this.#ready
        console.log("HolochainService.stop(): Stopping holochain process");
        if (this.#didResolveError) {
            console.error("HolochainService.stop: Warning attempting to close holochain processes when they did not start error free...")
        }
        if (this.#hcProcess) {
            stopProcesses(this.#hcProcess)
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
        const pubKey = await this.pubKeyForLanguage(lang);
        
        const activeApps = await (await this.#adminWebsocket!.listApps({status_filter: AppStatusFilter.Enabled})).map((app) => app.installed_app_id);
        console.warn("HolochainService: Found running apps:", activeApps);
       
        if(!activeApps.includes(lang)) {
            const cellIDsAndZomeCalls = [] as {cellId: CellId, zomeCalls: [string, string][]}[];
            // 1. install app
            try {
                console.debug("HolochainService: Installing DNAs for language", lang);
                for (let dna of dnas) {
                    //console.log("HolochainService: Installing DNA:", dna, "at data path:", this.#dataPath, "\n");
                    //Write the dna file to a temporary installation path
                    const p = path.join(this.#dataPath, `${lang}-${dna.nick}.dna`);
                    fs.writeFileSync(p, dna.file);
                    //Install the app; with on the fly generated app bundle
                    const installAppResult = await this.#adminWebsocket!.installApp({
                        installed_app_id: lang, agent_key: pubKey, membrane_proofs: {}, bundle: {
                            manifest: {
                                manifest_version: "1",
                                name: dna.nick,
                                roles: [{
                                    name: "main",
                                    dna: {
                                        //@ts-ignore
                                        path: p
                                    }
                                }]
                            },
                            resources: {}
                        }
                    })
                    //Get the cell info for the installed app
                    const cell_info = installAppResult.cell_info["main"][0];
                    const cellId = ("Provisioned" in cell_info) ? cell_info.Provisioned.cell_id : (("Cloned" in cell_info) ? cell_info.Cloned.cell_id : undefined);
                    if (cellId === undefined) {
                        console.error("HolochainService: Failed to install DNA, did not get cellId back from installAppResult:", installAppResult);
                        return;
                    }
                    //Keep track of the cellId's and their associated zome calls
                    cellIDsAndZomeCalls.push({cellId, zomeCalls: dna.zomeCalls});
                    console.warn("HolochainService: Installed DNA:", dna.nick, " with result:", installAppResult);
                }
            } catch(e) {
                console.error("HolochainService: InstallApp, got error: ", e);
                return;
            }

            // 2. activate app
            try {
                const activateResult = await this.#adminWebsocket!.enableApp({installed_app_id: lang})
                //console.warn("HolochainService: Activated app:", lang, "with result:", activateResult);
            } catch(e) {
                console.error("HolochainService: ERROR activating app", lang, " - ", e)
            }

            // 3. For each cell of the app create a new signing key
            for (const data of cellIDsAndZomeCalls) {
                const {cellId, zomeCalls} = data;
                //Create new signing keys for cell
                try {
                    await authorizeNewSigningKeyPair(this.#adminWebsocket!, cellId, zomeCalls);
                } catch (e) {
                    console.error("Got error when trying to generate key pair", e);
                    return;
                }
            }
        }

        //Register the callback to the cell internally
        if (callback != undefined) {
            //Check for apps matching this language address and register the signal callbacks
            console.log("HolochainService: setting holochains signal callback for language", lang);
            const app_infos = await this.#appWebsocket!.appInfo({installed_app_id: lang})
            if (app_infos) {
                const { cell_info } = app_infos;
                Object.values(cell_info).forEach(async value => {
                    const cellId = ("Provisioned" in value[0]) ? value[0].Provisioned.cell_id : (("Cloned" in value [0]) ? value[0].Cloned.cell_id : undefined);
                    if (cellId === undefined) {
                        console.error("HolochainService: ERROR: Could not get cellId from cell_info, got StemCell where not expected", value);
                        return;
                    }
                    this.#signalCallbacks.push([cellId, callback, lang]);
                })
            }
        };
    }

    async removeDnaForLang(lang: string) {
        const activeApps = await (await this.#adminWebsocket!.listApps({})).map((app) => app.installed_app_id);
        const apps = activeApps.filter(app => app === lang);
        for (const app of apps) {
            await this.#adminWebsocket!.uninstallApp({installed_app_id: app});
        }
    }

    getDelegateForLanguage(languageHash: string) {
        return new HolochainLanguageDelegate(languageHash, this, this.#queue)
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

        //1. Check for apps with installed_app_id matching this language address
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

        //2. Check that this app has valid cells that we can call
        // console.debug("HolochainService.callZomefunction: get info result:", infoResult)
        const { cell_info } = infoResult
        if(Object.keys(cell_info).length === 0) {
            console.error("HolochainService: tried to call zome function without any installed cell for given app!")
            return null
        }

        //3. Get the cellId of the cell with the role "main"
        let cellInfos = cell_info['main'];
        if(!cellInfos) {
            const e = new Error(`No cell role main found for installed app: ${installed_app_id}`)
            console.error(e)
            return e
        }
        const cell = ("Provisioned" in cellInfos[0]) ? cellInfos[0].Provisioned : (("Cloned" in cellInfos [0]) ? cellInfos[0].Cloned : undefined)
        const cell_id = cell!.cell_id;
        const [_dnaHash, provenance] = cell_id

        //4. Call the zome function
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

    async requestAgentInfos(): Promise<AgentInfoResponse> {
        return await this.#adminWebsocket!.agentInfo({cell_id: null})
    }

    async addAgentInfos(agent_infos: AgentInfoResponse) {
        await this.#adminWebsocket!.addAgentInfo({ agent_infos })
    }
}

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(() => resolve(), ms));