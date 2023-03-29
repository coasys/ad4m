import child_process, { execSync } from "child_process";
import fs from "fs";
import path from "path";
import { HolochainUnlockConfiguration } from "../../PerspectivismCore";
import { kitsuneProxy } from "./HolochainService";

function escapeShellArg (arg: string) {
    return arg.replace(" ", "\ ");
}

export function stopProcesses(hcProcess: child_process.ChildProcess) {
    hcProcess.kill("SIGTERM");
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
    let conductorStringConfig = `
---
environment_path: ${escapeShellArg(conductorConfig.environmentPath)}
keystore:
  type: lair_server_in_proc
admin_interfaces:
  - driver:
      type: websocket
      port: ${conductorConfig.adminPort}
network:
  network_type: ${conductorConfig.useMdns? 'quic_mdns' : 'quic_bootstrap'}
  ${conductorConfig.useBootstrap ? 'bootstrap_service: '+ conductorConfig.bootstrapService : ''}
  transport_pool:
    - type: ${conductorConfig.useProxy ? 'proxy' : 'quic'}
      sub_transport:
        type: quic
      proxy_config:
        type: remote_proxy_client
        proxy_url: ${kitsuneProxy}
  tuning_params:
    gossip_strategy: sharded-gossip
    gossip_loop_iteration_delay_ms: '1000'
    gossip_outbound_target_mbps: '100'
    gossip_inbound_target_mbps: '100'
    gossip_historic_outbound_target_mbps: '100'
    gossip_historic_inbound_target_mbps: '100'
    gossip_burst_ratio: '100'
    gossip_peer_on_success_next_gossip_delay_ms: '30000'
    gossip_peer_on_error_next_gossip_delay_ms: '60000'
    gossip_agent_info_update_interval_ms: '600000'
    gossip_local_sync_delay_ms: '30000'
    gossip_redundancy_target: '100'
    gossip_max_batch_size: '1000000'
    gossip_dynamic_arcs: 'true'
    gossip_single_storage_arc_per_space: 'false'
    default_rpc_single_timeout_ms: '30000'
    default_rpc_multi_remote_agent_count: '3'
    default_rpc_multi_remote_request_grace_ms: '3000'
    agent_info_expires_after_ms: '1200000'
    tls_in_mem_session_storage: '512'
    proxy_keepalive_ms: '120000'
    proxy_to_expire_ms: '600000'
    concurrent_limit_per_thread: '4096'
    tx2_quic_max_idle_timeout_ms: '30000'
    tx2_pool_max_connection_count: '4096'
    tx2_channel_count_per_connection: '2'
    tx2_implicit_timeout_ms: '30000'
    tx2_initial_connect_retry_delay_ms: '200'
    danger_tls_keylog: no_keylog
    danger_gossip_recent_threshold_secs: '900'
    disable_publish: 'false'
    disable_recent_gossip: 'false'
    disable_historical_gossip: 'false'
chc_namespace: null
db_sync_strategy: Fast
`
    fs.writeFileSync(conductorConfig.conductorConfigPath, conductorStringConfig);
}

export async function runHolochain(resourcePath: string, conductorConfigPath: string, hcDataPath: string, config: HolochainUnlockConfiguration): Promise<child_process.ChildProcess> {
    console.log("Starting holochain version: ", child_process.execFileSync(`${escapeShellArg(path.join(resourcePath, "holochain"))}`, ["--version"]).toString());
    
    let hcProcess = child_process.spawn(`${escapeShellArg(path.join(resourcePath, "holochain"))}`, ["-c", escapeShellArg(conductorConfigPath), "-p"],
        {
            env: {
                ...process.env,
                RUST_LOG: process.env.RUST_LOG ? process.env.RUST_LOG : "wasmer_compiler_cranelift=error,holochain::conductor::manager=warn,holochain::core::workflow::publish_dht_ops_workflow::publish_query=warn,publish_dht_ops_workflow=error,kitsune_p2p_types::metrics=error,kitsune_p2p::gossip::sharded_gossip=error,wasm_trace=debug,app_validation_workflow=error",
                WASM_LOG: process.env.WASM_LOG ? process.env.WASM_LOG: "debug,wasmer_compiler_cranelift=error,holochain::conductor::manager=warn,holochain::core::workflow::publish_dht_ops_workflow::publish_query=warn,publish_dht_ops_workflow=error,kitsune_p2p_types::metrics=error,kitsune_p2p::gossip::sharded_gossip=error,wasm_trace=debug,app_validation_workflow=error"
            },
        }
    );

    //Log holochain process stdout to out
    hcProcess.stdout.on('data', (data) => {
        console.log(`${data}`);
    });
    // hcProcess.stdout.on('error', (err) => {
    //     if (err.code == "EPIPE") {
    //         process.exit(0);
    //     }
    // });
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

    const echo = child_process.exec(`echo ${config.passphrase}`, (error, stdout, stderr) => {
        if (error) {
            console.log(`runHolochain.echo.exec: error: ${error.message}`);
            return;
        }
        if (stderr) {
            console.log(`runHolochain.echo.exec: stderr: ${stderr}`);
            return;
        }
        if (stdout) {
            hcProcess.stdin.write(stdout);
            return;
        }
    });

    echo.on('close', (code) => {
        if (code !== 0) {
          console.log(`echo process exited with code ${code}`);
        }
        hcProcess.stdin.end();
    });

    process.on("SIGINT", function () {
        hcProcess.kill("SIGINT");
        process.exit();
    });

    await isReady;
    return hcProcess;
}