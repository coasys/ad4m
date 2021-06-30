import child_process from "child_process";
import fs from "fs";
import path from "path";
import { holochainDataPath } from "../../Config";

function escapeShellArg (arg) {
    return arg.replace(" ", "\ ");
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

export function stopProcesses(hcDataPath, hcProcess, lairProcess) {
    // fs.unlinkSync(`${escapeShellArg(hcDataPath)}/keystore/pid`)
    hcProcess.kill("SIGINT");
    lairProcess.kill("SIGINT");
}

export function unpackDna(hcPath, dnaPath): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "unpack", `${escapeShellArg(dnaPath)}`]).toString();
}

export function packDna(hcPath, workdirPath): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "pack", `${escapeShellArg(workdirPath)}`]).toString();
}

export interface ConductorConfiguration {
    proxyUrl: string;
    environmentPath: string;
    adminPort: number;
    appPort: number;
    bootstrapService: string;
    conductorConfigPath: string;
}

export function writeDefaultConductor(conductorConfig: ConductorConfiguration) {
    console.log("HolochainService: Writing fresh conductor file...")
    let conductorStringConfig = `
---
environment_path: ${escapeShellArg(conductorConfig.environmentPath)}
use_dangerous_test_keystore: false
signing_service_uri: ~
encryption_service_uri: ~
decryption_service_uri: ~
dpki: ~
keystore_path: ${escapeShellArg(conductorConfig.environmentPath)}/keystore
passphrase_service: ~
admin_interfaces:
  - driver:
      type: websocket
      port: ${conductorConfig.adminPort}
network:
  transport_pool:
    - type: proxy
      sub_transport:
        type: quic
        bind_to: ~
        override_host: ~
        override_port: ~
      proxy_config:
        type: remote_proxy_client
        proxy_url: "${conductorConfig.proxyUrl}"
  bootstrap_service: "${conductorConfig.bootstrapService}"
  tuning_params:
    gossip_loop_iteration_delay_ms: "10"
    default_notify_remote_agent_count: "5"
    default_notify_timeout_ms: "30000"
    default_rpc_single_timeout_ms: "30000"
    default_rpc_multi_remote_agent_count: "2"
    default_rpc_multi_timeout_ms: "30000"
    agent_info_expires_after_ms: "1200000"
    tls_in_mem_session_storage: "512"
    proxy_keepalive_ms: "120000"
    proxy_to_expire_ms: "300000"
    concurrent_limit_per_thread: "32"
    tx2_quic_max_idle_timeout_ms: "30000"
    tx2_pool_max_connection_count: "4096"
    tx2_channel_count_per_connection: "3"
    tx2_implicit_timeout_ms: "30000"
    tx2_initial_connect_retry_delay_ms: "200"
  network_type: quic_bootstrap
`
    fs.writeFileSync(conductorConfig.conductorConfigPath, conductorStringConfig);
}

export async function startLair(lairPath, hcDataPath): Promise<child_process.ChildProcess> {
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

export async function runHolochain(resourcePath, conductorConfigPath, hcDataPath): Promise<[child_process.ChildProcess, child_process.ChildProcess]> {
    let lairProcess = await startLair(path.join(resourcePath, "lair-keystore"), hcDataPath)

    let hcProcess = child_process.spawn(`${escapeShellArg(path.join(resourcePath, "holochain"))}`, ["-c", escapeShellArg(conductorConfigPath)],
        {
            env: {
                ...process.env,
                RUST_LOG: process.env.RUST_LOG ? process.env.RUST_LOG : "info",
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