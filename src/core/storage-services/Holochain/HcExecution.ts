import child_process, { execSync } from "child_process";
import fs from "fs";
import path from "path";
import { HolochainUnlockConfiguration } from "../../PerspectivismCore";
import { bootstrapUrl, kitsuneProxy } from "./HolochainService";

function escapeShellArg (arg: string) {
    return arg.replace(" ", "\ ");
}

export function stopProcesses(hcProcess: child_process.ChildProcess, lairProcess: child_process.ChildProcess) {
    hcProcess.kill("SIGTERM");
    lairProcess.kill("SIGTERM");
}

export function unpackDna(hcPath: string, dnaPath: string): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "unpack", `${escapeShellArg(dnaPath)}`]).toString();
}

export function packDna(hcPath: string, workdirPath: string): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "pack", `${escapeShellArg(workdirPath)}`]).toString();
}

export interface ConductorConfiguration {
    proxyUrl: string;
    environmentPath: string;
    adminPort: number;
    appPort: number;
    useBootstrap: boolean,
    bootstrapService: string;
    conductorConfigPath: string;
    useProxy: boolean,
    useLocalProxy: boolean;
    useMdns: boolean;
    lairConnectionUrl: string;
}

export function writeDefaultConductor(conductorConfig: ConductorConfiguration) {
    console.log("HolochainService: Writing fresh conductor file...")
    let proxyType;
    if(conductorConfig.useLocalProxy) {
        proxyType = "local_proxy_server"
    } else {
        proxyType = "remote_proxy_client"
    }
    // TODO: should use lair-connection url
    let conductorStringConfig = `
---
environment_path: ${escapeShellArg(conductorConfig.environmentPath)}
use_dangerous_test_keystore: false
signing_service_uri: ~
encryption_service_uri: ~
decryption_service_uri: ~
dpki: ~
keystore:
  type: lair_server
  connection_url: ${conductorConfig.lairConnectionUrl}
admin_interfaces:
  - driver:
      type: websocket
      port: ${conductorConfig.adminPort}
network:
  network_type: ${conductorConfig.useMdns? 'quic_mdns' : 'quic_bootstrap'}
  ${conductorConfig.useBootstrap ? 'bootstrap_service: '+conductorConfig.bootstrapService : ''}
  transport_pool:
    - type: ${conductorConfig.useProxy ? 'proxy' : 'quic'}
      sub_transport:
        type: quic
      proxy_config:
        type: ${proxyType}
        proxy_url: "${conductorConfig.proxyUrl}"
  tuning_params:
    gossip_strategy: sharded-gossip
    gossip_loop_iteration_delay_ms: "1000"
    gossip_outbound_target_mbps: "0.5"
    gossip_inbound_target_mbps: "0.5"
    gossip_historic_outbound_target_mbps: "0.1"
    gossip_historic_inbound_target_mbps: "0.1"
    gossip_peer_on_success_next_gossip_delay_ms: "60000"
    gossip_peer_on_error_next_gossip_delay_ms: "300000"
    gossip_local_sync_delay_ms: "60000"
    default_rpc_single_timeout_ms: "30000"
    default_rpc_multi_remote_agent_count: "3"
    default_rpc_multi_remote_request_grace_ms: "3000"
    agent_info_expires_after_ms: "1200000"
    tls_in_mem_session_storage: "512"
    proxy_keepalive_ms: "120000"
    proxy_to_expire_ms: "300000"
`
    fs.writeFileSync(conductorConfig.conductorConfigPath, conductorStringConfig);
}

async function initializeLairKeystore(lairPath: string, hcDataPath: string, config: HolochainUnlockConfiguration) {
    return new Promise(async (resolve, reject) => {
        const echo = child_process.spawn('echo', [config.passphrase])
        const keyStoreFolderExists = fs.existsSync(`${escapeShellArg(hcDataPath)}/keystore`);
        if (!keyStoreFolderExists) {
            fs.mkdirSync(`${escapeShellArg(hcDataPath)}/keystore`)
        }

        let lairProcess = child_process.spawn(`${escapeShellArg(lairPath)}`, ["init", "-p"], {
            env: { ...process.env, LAIR_DIR: `${escapeShellArg(hcDataPath)}/keystore` },
            cwd: `${escapeShellArg(hcDataPath)}/keystore`
        });

        echo.stdout.on('data', (data) => {
            lairProcess.stdin.write(data);
        })

        echo.on('close', (code) => {
            if (code !== 0) {
              console.log(`echo process exited with code ${code}`);
            }
            lairProcess.stdin.end();
        });

        //Log lair process stdout to out
        lairProcess.stdout?.on('data', (data) => {
            console.log(`1 ${data}`);
        });

        //Log lair process stderr to out
        lairProcess.stderr?.on('data', (data) => {
            console.log(`${data}`);
            reject();
        });

        lairProcess.on('exit', () => {
            let {
                conductorPath, 
                adminPort,
                appPort,
                useBootstrap,
                useProxy,
                useLocalProxy,
                useMdns,
            } = config;

            const conductorConfigPath = path.join(conductorPath!, "conductor-config.yaml");
            const holochainAppPort = appPort ? appPort : 1337;
            const holochainAdminPort = adminPort ? adminPort : 2000;
            if(useMdns === undefined) useMdns = true
            if(useBootstrap === undefined) useBootstrap = true
            if(useProxy === undefined) useProxy = true
            if(useLocalProxy === undefined) useLocalProxy = false;

            const lairConnectionUrl = execSync(`${escapeShellArg(lairPath)} url`, {
                cwd: `${escapeShellArg(hcDataPath)}/keystore`
            }).toString();

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
                lairConnectionUrl,
            } as ConductorConfiguration);
            resolve(true);
        });
        lairProcess.on('close', () => {
            resolve(true);
        });
    });
}

export async function startLair(resourcePath: string, hcDataPath: string, config: HolochainUnlockConfiguration): Promise<child_process.ChildProcess> {
    const lairPath = path.join(resourcePath, "lair-keystore");
    const islairConfigExist = fs.existsSync(path.join(`${escapeShellArg(hcDataPath)}/keystore`, "lair-keystore-config.yaml"));

    if (!islairConfigExist) {
        await initializeLairKeystore(lairPath, hcDataPath, config);
    }

    const echo = child_process.spawn('echo', [config.passphrase!])
    
    let lairProcess = child_process.spawn(`${escapeShellArg(lairPath)}`, ["server", "-p"], {
        env: { ...process.env, LAIR_DIR: `${escapeShellArg(hcDataPath)}/keystore` },
        cwd: `${escapeShellArg(hcDataPath)}/keystore`
    });

    echo.stdout.on('data', (data) => {
        lairProcess.stdin.write(data);
    })

    echo.on('close', (code) => {
        if (code !== 0) {
          console.log(`echo process exited with code ${code}`);
        }
        lairProcess.stdin.end();
    });


    //Log lair process stdout to out
    lairProcess.stdout?.on('data', (data) => {
        console.log(`${data}`);
    });

    //Log lair process stderr to out
    lairProcess.stderr?.on('data', (data) => {
        console.log(`${data}`);
    });

    let isReady = new Promise((resolve, reject) => {
        lairProcess.stdout?.on('data', (data) => {
            if (data.includes("# lair-keystore running #")) {
                resolve(null);
            };
        });
    });
    await isReady;
    return lairProcess
}

export async function runHolochain(resourcePath: string, conductorConfigPath: string, hcDataPath: string, config: HolochainUnlockConfiguration): Promise<[child_process.ChildProcess, child_process.ChildProcess]> {
    let lairProcess = await startLair(resourcePath, hcDataPath, config)
    const echo = child_process.spawn('echo', [config.passphrase!])
    
    let hcProcess = child_process.spawn(`${escapeShellArg(path.join(resourcePath, "holochain"))}`, ["-c", escapeShellArg(conductorConfigPath), "-p"],
        {
            env: {
                ...process.env,
                RUST_LOG: process.env.RUST_LOG ? process.env.RUST_LOG : "wasmer_compiler_cranelift=error,holochain::conductor::manager=warn,info,holochain::core::workflow::publish_dht_ops_workflow::publish_query=warn",
            },
        }
    );

    echo.stdout.on('data', (data) => {
        hcProcess.stdin.write(data);
    })

    echo.on('close', (code) => {
        if (code !== 0) {
          console.log(`echo process exited with code ${code}`);
        }
        hcProcess.stdin.end();
    });

    process.on("SIGINT", function () {
        // fs.unlinkSync(`${escapeShellArg(holochainDataPath)}/keystore/pid`)
        hcProcess.kill("SIGINT");
        lairProcess.kill("SIGINT");
        process.exit();
    });

    //Log holochain process stdout to out
    hcProcess.stdout.on('data', (data) => {
        console.log(`${data}`);
    });
    //Log holochain process stderr to out
    hcProcess.stderr.on('data', (data) => {
        console.log(`${data}`);
    });

    let isReady = new Promise((resolve, reject) => {
        hcProcess.stdout.on('data', (data) => {
            if (data.includes("Conductor ready")) {
                resolve(null);
            };
        });
    });
    await isReady;
    return [hcProcess, lairProcess];
}