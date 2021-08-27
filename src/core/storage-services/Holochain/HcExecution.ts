import child_process from "child_process";
import fs from "fs";
import path from "path";

function escapeShellArg (arg: string) {
    return arg.replace(" ", "\ ");
}

export function stopProcesses(hcProcess: child_process.ChildProcess, lairProcess: child_process.ChildProcess) {
    // fs.unlinkSync(`${escapeShellArg(hcDataPath)}/keystore/pid`)
    hcProcess.kill("SIGINT");
    lairProcess.kill("SIGINT");
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
    bootstrapService: string;
    conductorConfigPath: string;
    useLocalProxy: boolean;
}

export function writeDefaultConductor(conductorConfig: ConductorConfiguration) {
    console.log("HolochainService: Writing fresh conductor file...")
    let proxyType;
    if(conductorConfig.useLocalProxy) {
        proxyType = "local_proxy_server"
    } else {
        proxyType = "remote_proxy_client"
    }
    let conductorStringConfig = `
---
environment_path: ${escapeShellArg(conductorConfig.environmentPath)}
use_dangerous_test_keystore: false
signing_service_uri: ~
encryption_service_uri: ~
decryption_service_uri: ~
dpki: ~
keystore_path: ${escapeShellArg(conductorConfig.environmentPath)}/keystore
passphrase_service: 
    type: danger_insecure_from_config
    passphrase: "foobar"
admin_interfaces:
  - driver:
      type: websocket
      port: ${conductorConfig.adminPort}
network:
  network_type: quic_bootstrap
  bootstrap_service: https://bootstrap-staging.holo.host
  transport_pool:
    - type: proxy
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

export async function startLair(lairPath: string, hcDataPath: string): Promise<child_process.ChildProcess> {
    let lairProcess = child_process.spawn(`${escapeShellArg(lairPath)}`, [], {
        env: { ...process.env, LAIR_DIR: `${escapeShellArg(hcDataPath)}/keystore` },
    });
    
    //Log lair process stdout to out
    lairProcess.stdout.on('data', (data) => {
        console.log(`${data}`);
    });
    //Log lair process stderr to out
    lairProcess.stderr.on('data', (data) => {
        console.log(`${data}`);
    });

    let isReady = new Promise((resolve, reject) => {
        lairProcess.stdout.on('data', (data) => {
            if (data.includes("#lair-keystore-ready#")) {
                resolve(null);
            };
        });
    });
    await isReady;
    return lairProcess
}

export async function runHolochain(resourcePath: string, conductorConfigPath: string, hcDataPath: string): Promise<[child_process.ChildProcess, child_process.ChildProcess]> {
    let lairProcess = await startLair(path.join(resourcePath, "lair-keystore"), hcDataPath)
    let hcProcess = child_process.spawn(`${escapeShellArg(path.join(resourcePath, "holochain"))}`, ["-c", escapeShellArg(conductorConfigPath)],
        {
            env: {
                ...process.env,
                RUST_LOG: process.env.RUST_LOG ? process.env.RUST_LOG : "wasmer_compiler_cranelift=error,holochain::conductor::manager=warn,info,holochain::core::workflow::publish_dht_ops_workflow::publish_query=warn",
            },
        }
    );
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