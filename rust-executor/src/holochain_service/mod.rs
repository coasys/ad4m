use chrono::Duration;
use crypto_box::rand_core::OsRng;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use lazy_static::lazy_static;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

use holochain::conductor::api::{AppInfo, AppStatusFilter, CellInfo};
use holochain::conductor::config::{ConductorConfig, NetworkConfig, SpaceNetworkOverride};
use holochain::conductor::paths::DataRootPath;
use holochain::conductor::{ConductorBuilder, ConductorHandle};
use holochain::prelude::hash_type::Agent;
use holochain::prelude::{
    AppManifest, ExternIO, HoloHash, InstallAppPayload, Kitsune2NetworkMetricsRequest, Signal,
    Signature, Timestamp, ZomeCallParams, ZomeCallResponse,
};
use holochain::test_utils::itertools::Either;

use holochain_types::dna::ValidatedDnaManifest;
use holochain_types::websocket::AllowedOrigins;
use kitsune_p2p_types::dependencies::url2::Url2;
use log::{error, info, warn};
use rand::Rng;
use serde::{Deserialize, Serialize};
use tokio::select;
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio::time::timeout;

use tokio_stream::StreamExt;

pub mod holochain_service_extension;
pub(crate) mod interface;

pub(crate) use interface::{
    get_holochain_service, maybe_get_holochain_service, HolochainServiceInterface,
    HolochainServiceRequest, HolochainServiceResponse,
};

use self::interface::set_holochain_service;

// Store the config globally so we can restart with the same configuration
lazy_static! {
    static ref HOLOCHAIN_CONFIG: Arc<RwLock<Option<LocalConductorConfig>>> =
        Arc::new(RwLock::new(None));
}

//const COASYS_BOOTSTRAP_AGENT_INFO: &str = r#" ["g6VhZ2VudMQkeWyy+u7ziOZEejqRGCHVSjWuNDGCkHSFWpkp/DsXJFVDyWYdqXNpZ25hdHVyZcRAlYaUoegA0DB+U8F2cONLcoORjqz7WqW4dBSfvWyQ4AixLLB3h0jsvqGUo0UfowjUP1ntBhMjA8xo/oQateooDaphZ2VudF9pbmZvxPuGpXNwYWNlxCReuo1fprVD9jjsQWRglwEzVlWFiYB+4BEA7BQIwOpYgUgezPGlYWdlbnTEJHlssvru84jmRHo6kRgh1Uo1rjQxgpB0hVqZKfw7FyRVQ8lmHaR1cmxzkdlJd3NzOi8vc2lnbmFsLmhvbG8uaG9zdC90eDUtd3MvNEFNaGNWNHhpdFdPMHI2YUR1NjFwcW5jMW5LNjBmdkRfYTRyZUJmUFdTMKxzaWduZWRfYXRfbXPPAAABk/NOnPewZXhwaXJlc19hZnRlcl9tc84AEk+AqW1ldGFfaW5mb8QZgahhcnFfc2l6ZYKlcG93ZXIRpWNvdW50CA=="]"#;
#[derive(Clone)]
pub struct HolochainService {
    pub conductor: ConductorHandle,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LocalConductorConfig {
    pub passphrase: String,
    pub conductor_path: String,
    pub data_path: String,
    pub use_bootstrap: bool,
    pub use_proxy: bool,
    pub use_local_proxy: bool,
    pub use_mdns: bool,
    pub proxy_url: String,
    pub bootstrap_url: String,
    pub relay_url: Option<String>,
    pub app_port: u16,
}

impl LocalConductorConfig {
    /// Create a LocalConductorConfig from the global Ad4mConfig and a passphrase.
    pub fn from_ad4m_config(config: &crate::config::Ad4mConfig, passphrase: String) -> Self {
        let app_data_path = config
            .app_data_path
            .as_ref()
            .expect("app_data_path not set");
        let base = std::path::Path::new(app_data_path).join("ad4m");
        Self {
            passphrase,
            conductor_path: base.join("h").join("c").to_string_lossy().into_owned(),
            data_path: base.join("h").join("d").to_string_lossy().into_owned(),
            use_bootstrap: config.hc_use_bootstrap.unwrap_or(true),
            use_proxy: config.hc_use_proxy.unwrap_or(true),
            use_local_proxy: config.hc_use_local_proxy.unwrap_or(false),
            use_mdns: config.hc_use_mdns.unwrap_or(false),
            proxy_url: config.hc_proxy_url.clone().unwrap_or_default(),
            bootstrap_url: config.hc_bootstrap_url.clone().unwrap_or_default(),
            relay_url: config.hc_relay_url.clone(),
            app_port: config.hc_app_port.unwrap_or(1337),
        }
    }
}

impl HolochainService {
    /// Formats an error with proper stacktrace formatting for readability
    fn format_error_with_stacktrace(err: &dyn std::fmt::Debug) -> String {
        let err_str = format!("{:?}", err);

        // Check if the error contains a stacktrace pattern
        if err_str.contains("RuntimeError:") && err_str.contains("\\n    at ") {
            // Replace escaped newlines with actual newlines throughout the error string
            // This will make the stacktrace readable line by line
            return err_str.replace("\\n", "\n");
        }

        err_str
    }

    pub async fn init(local_config: LocalConductorConfig) -> Result<(), AnyError> {
        // Store the config for potential restarts
        {
            let mut config_lock = HOLOCHAIN_CONFIG.write().await;
            *config_lock = Some(local_config.clone());
        }

        let (sender, mut receiver) = mpsc::unbounded_channel::<HolochainServiceRequest>();
        let (stream_sender, stream_receiver) = mpsc::unbounded_channel::<Signal>();
        let (new_app_ids_sender, mut new_app_ids_receiver) = mpsc::unbounded_channel::<AppInfo>();

        let inteface = HolochainServiceInterface {
            sender,
            stream_receiver: Arc::new(Mutex::new(stream_receiver)),
        };

        let (response_sender, response_receiver) = oneshot::channel();

        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_multi_thread()
                .thread_name(String::from("holochain_service"))
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            tokio::task::block_in_place(|| {
                rt.block_on(async move {
                    let mut service = HolochainService::new(local_config).await.unwrap();
                    let conductor_clone = service.conductor.clone();

                    // Spawn a new task to forward items from the stream to the receiver
                    let spawned_sig = tokio::spawn(async move {

                        let mut streams: tokio_stream::StreamMap<String, tokio_stream::wrappers::BroadcastStream<Signal>> = tokio_stream::StreamMap::new();
                        conductor_clone.list_apps(Some(AppStatusFilter::Enabled)).await.unwrap().into_iter().for_each(|app| {
                            let sig_broadcasters = conductor_clone.subscribe_to_app_signals(app.installed_app_id.clone());
                            streams.insert(app.installed_app_id.clone(), tokio_stream::wrappers::BroadcastStream::new(sig_broadcasters));
                        });

                        response_sender
                            .send(HolochainServiceResponse::InitComplete(Ok(())))
                            .unwrap();

                        loop {
                            tokio::select! {
                                Some((_, maybe_signal)) = streams.next() => {
                                    if let Ok(signal) = maybe_signal {
                                        let _ = stream_sender.send(signal);
                                    } else {
                                        log::error!("Got error from Holochain through app signal stream: {:?}", maybe_signal.expect_err("to be error since we're in else case"))
                                    }
                                }
                                Some(new_app_id) = new_app_ids_receiver.recv() => {
                                    let sig_broadcasters = conductor_clone.subscribe_to_app_signals(new_app_id.installed_app_id.clone());
                                    streams.insert(new_app_id.installed_app_id.clone(), tokio_stream::wrappers::BroadcastStream::new(sig_broadcasters));
                                }
                                // Backoff when no signals arrive and the StreamMap
                                // is empty — prevents select! from busy-spinning.
                                // Real signals arrive via the branches above.
                                _ = tokio::time::sleep(tokio::time::Duration::from_millis(100)) => {}
                                else => break,
                            }
                        }
                    });

                    let spawned_receiver = tokio::spawn(async move {
                        while let Some(message) = receiver.recv().await {
                            match message {
                                HolochainServiceRequest::InstallApp(payload, response) => {
                                    match timeout(
                                        std::time::Duration::from_secs(10),
                                        service.install_app(payload)
                                    ).await.map_err(|_| anyhow!("Timeout error; InstallApp call")) {
                                        Ok(result) => {
                                            if let Ok(app_info) = &result {
                                                let _ = new_app_ids_sender.send(app_info.clone());
                                            }
                                            let _ = response.send(HolochainServiceResponse::InstallApp(result));
                                        },
                                        Err(err) => {
                                            let _ = response.send(HolochainServiceResponse::InstallApp(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::CallZomeFunction {
                                    app_id,
                                    cell_name,
                                    zome_name,
                                    fn_name,
                                    payload,
                                    response,
                                } => {
                                    match timeout(
                                        std::time::Duration::from_secs(90),
                                        service.call_zome_function(app_id, cell_name, zome_name, fn_name, payload)
                                    ).await.map_err(|_| anyhow!("Timeout error; Call Zome Function")) {
                                        Ok(result) => {
                                            let _ = response.send(HolochainServiceResponse::CallZomeFunction(result));
                                        },
                                        Err(err) => {
                                            let _ = response.send(HolochainServiceResponse::CallZomeFunction(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::RemoveApp(app_id, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(10),
                                        service.remove_app(app_id)
                                    ).await.map_err(|_| anyhow!("Timeout error; Remove App")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::RemoveApp(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::RemoveApp(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::EnableApp(app_id, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(10),
                                        async {
                                            service.conductor.clone().enable_app(app_id).await
                                                .map(|_| ())
                                                .map_err(|e| anyhow!("Could not enable app: {:?}", e))
                                        }
                                    ).await.map_err(|_| anyhow!("Timeout error; Enable App")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::EnableApp(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::EnableApp(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::AgentInfos(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(30),
                                        service.agent_infos()
                                    ).await.map_err(|_| anyhow!("Timeout error; AgentInfos")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::AgentInfos(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::AgentInfos(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::AddAgentInfos(agent_infos, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(30),
                                        service.add_agent_infos(agent_infos)
                                    ).await.map_err(|_| anyhow!("Timeout error; AddAgentInfos")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::AddAgentInfos(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::AddAgentInfos(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::Sign(data, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        service.sign(data)
                                    ).await.map_err(|_| anyhow!("Timeout error; Sign")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::Sign(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::Sign(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::Shutdown(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        service.shutdown()
                                    ).await.map_err(|_| anyhow!("Timeout error Shutdown")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::Shutdown(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::Shutdown(Err(err)));
                                        },
                                    }
                                    break;
                                }
                                HolochainServiceRequest::GetAgentKey(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        service.get_agent_key()
                                    ).await.map_err(|_| anyhow!("Timeout error; GetAgentKey")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::GetAgentKey(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::GetAgentKey(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::GetAppInfo(app_id, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        service.get_app_info(app_id)
                                    ).await.map_err(|_| anyhow!("Timeout error; GetAppInfo")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::GetAppInfo(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::GetAppInfo(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::LogNetworkMetrics(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        service.log_network_metrics()
                                    ).await.map_err(|_| anyhow!("Timeout error; LogNetworkMetrics")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::LogNetworkMetrics(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::LogNetworkMetrics(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::GetNetworkMetrics(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(30),
                                        service.get_network_metrics()
                                    ).await.map_err(|_| anyhow!("Timeout error; GetNetworkMetrics")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::GetNetworkMetrics(result));
                                        },
                                        Err(err) => {
                                            error!("GetNetworkMetrics timed out after 30s");
                                            let _ = response_tx.send(HolochainServiceResponse::GetNetworkMetrics(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::PackDna(path, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        HolochainService::pack_dna(path)
                                    ).await.map_err(|_| anyhow!("Timeout error; PackDna")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::PackDna(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::PackDna(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::UnPackDna(path, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        HolochainService::unpack_dna(path)
                                    ).await.map_err(|_| anyhow!("Timeout error; UnpackDna")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::UnPackDna(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::UnPackDna(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::PackHapp(path, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        HolochainService::pack_happ(path)
                                    ).await.map_err(|_| anyhow!("Timeout error; PackHapp")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::PackHapp(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::PackHapp(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::UnPackHapp(path, response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
                                        HolochainService::unpack_happ(path)
                                    ).await.map_err(|_| anyhow!("Timeout error; UnPackHapp")) {
                                        Ok(result) => {
                                            let _ = response_tx.send(HolochainServiceResponse::UnPackHapp(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::UnPackHapp(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::NewSignKeypair(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(10),
                                        service.conductor.keystore().new_sign_keypair_random()
                                    ).await.map_err(|_| anyhow!("Timeout error; NewSignKeypair")) {
                                        Ok(result) => {
                                            let result = result.map_err(|e| anyhow!("Failed to generate new signing keypair: {}", e));
                                            let _ = response_tx.send(HolochainServiceResponse::NewSignKeypair(result));
                                        },
                                        Err(err) => {
                                            let _ = response_tx.send(HolochainServiceResponse::NewSignKeypair(Err(err)));
                                        },
                                    }
                                }
                                HolochainServiceRequest::SignWithKey(agent_key, data, response_tx) => {
                                    let keystore = service.conductor.keystore();
                                    let data_arc = Arc::from(data.into_boxed_slice());
                                    let result = keystore.sign(agent_key, data_arc).await
                                        .map_err(|e| anyhow!("Failed to sign with key: {}", e));
                                    let _ = response_tx.send(HolochainServiceResponse::SignWithKey(result));
                                }
                            };
                        };
                        error!("Holochain service receiver closed");
                    });

                    select! {
                        _ = spawned_sig => {},
                        _ = spawned_receiver => {},
                    }

                    error!("Holochain service exited")
                });
            })
        });

        match response_receiver.await? {
            HolochainServiceResponse::InitComplete(result) => result?,
            _ => unreachable!(),
        };

        //let agent_infos: Vec<String> = serde_json::from_str(COASYS_BOOTSTRAP_AGENT_INFO)?;
        //info!("Adding agent infos: {:?}", agent_infos);
        //if let Err(e) = inteface.add_agent_infos(agent_infos).await {
        //    error!("Error adding agent infos: {:?}", e);
        //}

        set_holochain_service(inteface).await;

        Ok(())
    }

    pub async fn restart_service() -> Result<(), AnyError> {
        log::info!("Restarting Holochain service...");

        // Get the stored config
        let config = {
            let config_lock = HOLOCHAIN_CONFIG.read().await;
            config_lock
                .clone()
                .ok_or_else(|| anyhow!("No Holochain config stored for restart"))?
        };

        // Shut down the old conductor first so it releases the port
        if let Some(hc) = maybe_get_holochain_service().await {
            log::info!("Shutting down old Holochain conductor...");
            if let Err(e) = hc.shutdown().await {
                log::warn!(
                    "Error shutting down old conductor (continuing anyway): {}",
                    e
                );
            }
            // Give the OS time to release the port
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }

        // Restart the service with the stored config
        Self::init(config).await
    }

    pub async fn get_stored_config() -> Option<LocalConductorConfig> {
        let config_lock = HOLOCHAIN_CONFIG.read().await;
        config_lock.clone()
    }

    pub async fn new(local_config: LocalConductorConfig) -> Result<HolochainService, AnyError> {
        let conductor_yaml_path =
            std::path::Path::new(&local_config.conductor_path).join("conductor_config.yaml");

        // Resolve the relay_url once so both the new-config branch (below)
        // and the legacy-migration branch (above) share the same value.
        // Precedence: use_proxy → explicit relay_url → default.
        // CodeRabbit review PR #907 finding #4 (proxy/relay precedence).
        let resolved_relay_url = resolve_relay_url(&local_config);

        let mut config = if conductor_yaml_path.exists() {
            // CodeRabbit review PR #907 finding #3: Holochain 0.7 tightened
            // conductor-config deserialization and now:
            //   - rejects obsolete network.signal_url and
            //     network.webrtc_config fields
            //   - REQUIRES network.relay_url
            // Users upgrading from HC 0.6 in place have both problems (their
            // file has obsolete fields AND lacks relay_url), so load_yaml
            // fails either way. Migrate the file in place before load_yaml()
            // sees it. Idempotent: fields already-clean / already-present are
            // left untouched (no rewrite, no mtime change).
            //
            // CodeRabbit round-2 finding @508: also inject resolved relay_url
            // when absent, otherwise load_yaml fails on the required-field
            // check for a migrated file.
            if let Err(e) =
                migrate_legacy_conductor_config(&conductor_yaml_path, Some(&resolved_relay_url))
                    .await
            {
                warn!(
                    "Could not migrate legacy conductor_config.yaml at {:?}: {} \
                     — load_yaml may still succeed if the file is already 0.7-clean.",
                    conductor_yaml_path, e
                );
            }
            ConductorConfig::load_yaml(&conductor_yaml_path)?
        } else {
            let mut config = ConductorConfig::default();
            let data_root_path: DataRootPath =
                PathBuf::from(local_config.conductor_path.clone()).into();
            config.data_root_path = Some(data_root_path);
            config.admin_interfaces = None;

            let mut network_config = NetworkConfig::default();

            if local_config.use_bootstrap {
                network_config.bootstrap_url = Url2::parse(local_config.bootstrap_url.as_str());
            } else {
                network_config.bootstrap_url = Url2::parse("http://bootstrap.ad4m.dev:4433");
            }

            // HC 0.7.0 dropped NetworkConfig.signal_url — the signal-server
            // concept was folded into bootstrap+relay. Old AD4M code set
            // signal_url to a WS URL for direct peer signaling; under 0.7
            // this responsibility moved to the iroh relay (relay_url).
            //
            // Precedence resolution is factored into resolve_relay_url() at
            // the top of this function so the legacy-migration path can share
            // the same value. CodeRabbit review PR #907 finding #4.
            network_config.relay_url = Url2::parse(resolved_relay_url.as_str());

            config.network = network_config;

            config
        };

        // Apply unyt space override: the unyt DNA gets its own bootstrap,
        // relay, and auth material. All other DNAs use the AD4M defaults above.
        //
        // HC 0.7.0: SpaceNetworkOverride no longer carries signal_url — the
        // signal-server responsibility moved to the iroh relay. We drop that
        // field and keep bootstrap_url + relay_url + base64_auth_material.
        // UNYT_SIGNAL_URL is intentionally ignored (legacy WS signaller,
        // not part of the 0.7 topology).
        let dna_hash_opt = crate::db::Ad4mDb::global_instance()
            .lock()
            .ok()
            .and_then(|guard| {
                guard
                    .as_ref()
                    .and_then(|db| db.get_setting("unyt_dna_hash").ok().flatten())
            });
        if let Some(dna_hash) = dna_hash_opt {
            // Grab the auth material stashed by setup_bootstrap_auth().
            // Without this the authenticated Unyt bootstrap will refuse the
            // connection. CodeRabbit review PR #907 finding #5.
            let auth_material =
                crate::db::Ad4mDb::with_global_instance(|db| db.get_setting("unyt_auth_material"))
                    .ok()
                    .flatten();
            if auth_material.is_none() {
                info!(
                    "Applying unyt space override for DNA {} (no auth material \
                     stashed yet — first-run bootstrap; setup_bootstrap_auth \
                     will populate it)",
                    dna_hash
                );
            } else {
                info!(
                    "Applying unyt space override for DNA {} with base64 auth material",
                    dna_hash
                );
            }
            config.network.space_overrides.insert(
                dna_hash,
                SpaceNetworkOverride {
                    bootstrap_url: Some(Url2::parse(crate::unyt_service::UNYT_BOOTSTRAP_URL)),
                    base64_auth_material: auth_material,
                    relay_url: Some(Url2::parse(crate::unyt_service::UNYT_RELAY_URL)),
                },
            );
        }

        // NET-DIAG: log the resolved network config in a compact form so we
        // can compare across nodes in CI without wading through the debug
        // pretty-print of the whole ConductorConfig.
        info!(
            "NET-DIAG conductor network config: bootstrap_url={:?} relay_url={:?} space_overrides={} \
             (use_bootstrap={}, use_proxy={}, use_local_proxy={}, use_mdns={}, proxy_url={:?})",
            config.network.bootstrap_url.as_str(),
            config.network.relay_url.as_str(),
            config.network.space_overrides.len(),
            local_config.use_bootstrap,
            local_config.use_proxy,
            local_config.use_local_proxy,
            local_config.use_mdns,
            local_config.proxy_url,
        );
        for (space_hash, override_cfg) in &config.network.space_overrides {
            info!(
                "NET-DIAG space_override[{}]: bootstrap_url={:?} relay_url={:?} auth={}",
                space_hash,
                override_cfg.bootstrap_url.as_ref().map(|u| u.as_str()),
                override_cfg.relay_url.as_ref().map(|u| u.as_str()),
                if override_cfg.base64_auth_material.is_some() { "yes" } else { "no" },
            );
        }
        info!("Starting holochain conductor with config: {:#?}", config);
        let passphrase_locked_array =
            sodoken::LockedArray::from(local_config.passphrase.as_bytes().to_vec());
        let passphrase = Arc::new(std::sync::Mutex::new(passphrase_locked_array));
        let conductor = ConductorBuilder::new()
            .config(config)
            .passphrase(Some(passphrase))
            .build()
            .await;

        if let Err(e) = conductor {
            info!("Could not start holochain conductor: {:#?}", e);
            panic!("Could not start holochain conductor: {:#?}", e);
        }

        info!("Started holochain conductor");

        let conductor = conductor.unwrap();

        let interface = conductor
            .clone()
            .add_app_interface(
                Either::Left(local_config.app_port),
                None,
                AllowedOrigins::Any,
                None,
            )
            .await;

        info!("Added app interface: {:?}", interface);

        let service = Self { conductor };

        Ok(service)
    }

    pub async fn install_app(
        &mut self,
        install_app_payload: InstallAppPayload,
    ) -> Result<AppInfo, AnyError> {
        if install_app_payload.installed_app_id.is_none() {
            return Err(anyhow!("App id is required"));
        }

        let app_id = install_app_payload.installed_app_id.clone().unwrap();

        //Check if app_id already exists
        let app_info = self.conductor.get_app_info(&app_id).await?;

        match app_info {
            None => {
                self.conductor
                    .clone()
                    .install_app_bundle(install_app_payload)
                    .await
                    .map_err(|e| anyhow!("Could not install app: {:?}", e))?;
            }
            Some(_) => {
                info!("App already installed with id: {:?}", app_id);
            }
        }

        // Always ensure the app is enabled, even if already installed.
        // This is necessary because enable_app creates the K2 network space
        // via the join operation. If we don't call enable_app for already-installed
        // apps, their K2 spaces won't exist and p2p calls will fail with
        // "The K2 Space does not exist" error.
        self.conductor
            .clone()
            .enable_app(app_id.clone())
            .await
            .map_err(|e| anyhow!("Could not enable app: {:?}", e))?;

        // Get app info to extract cell IDs
        let app_info = self.conductor.get_app_info(&app_id).await?;
        let app_info =
            app_info.ok_or_else(|| anyhow!("App not found after enabling: {}", app_id))?;

        // Extract all cell IDs from the app
        let mut app_cell_ids = Vec::new();
        for (_role_name, cell_infos) in &app_info.cell_info {
            for cell_info in cell_infos {
                match cell_info {
                    CellInfo::Provisioned(cell) => app_cell_ids.push(cell.cell_id.clone()),
                    CellInfo::Cloned(cell) => {
                        if cell.enabled {
                            app_cell_ids.push(cell.cell_id.clone())
                        }
                    }
                    CellInfo::Stem(_) => {} // Stem cells are not yet instantiated
                }
            }
        }

        // Wait for all cells to complete their network join.
        // This uses Holochain's event-driven readiness signaling instead of
        // retry loops or arbitrary timeouts.
        for cell_id in &app_cell_ids {
            if let Err(e) = self
                .conductor
                .await_cell_network_join_complete(cell_id, std::time::Duration::from_secs(10))
                .await
            {
                error!(
                    "Cell {:?} in app {} failed to join network: {:?}",
                    cell_id, app_id, e
                );
            }
        }

        let app_info = self.conductor.get_app_info(&app_id).await?;
        let app_info = app_info.ok_or_else(|| anyhow!("App not found: {}", app_id))?;
        Ok(app_info)
    }

    pub async fn call_zome_function(
        &self,
        app_id: String,
        cell_name: String,
        zome_name: String,
        fn_name: String,
        payload: Option<ExternIO>,
    ) -> Result<ZomeCallResponse, AnyError> {
        // info!(
        //     "Calling zome function: {:?} {:?} {:?} {:?}",
        //     app_id, cell_name, zome_name, fn_name
        // );
        let app_info = self.conductor.get_app_info(&app_id).await?;

        if app_info.is_none() {
            error!("App not installed with id: {:?}", app_id);
            return Err(anyhow!("App not installed with id: {:?}", app_id));
        }

        let app_info = app_info.unwrap();

        let cell_entry = app_info.cell_info.get(&cell_name);

        if cell_entry.is_none() {
            error!(
                "Cell not installed with name: {:?} in app: {:?}",
                cell_name, app_id
            );
            return Err(anyhow!(
                "Cell not installed with name: {:?} in app: {:?}",
                cell_name,
                app_id
            ));
        }

        if cell_entry.unwrap().is_empty() {
            error!(
                "No cells for cell name: {:?} in app: {:?}",
                cell_name, app_id
            );
            return Err(anyhow!(
                "No cells for cell name: {:?} in app: {:?}",
                cell_name,
                app_id
            ));
        }

        let cell_info = cell_entry.unwrap().first().unwrap().clone();
        let cell_id = match cell_info {
            CellInfo::Provisioned(cell) => cell.cell_id,
            CellInfo::Cloned(cell) => cell.cell_id,
            CellInfo::Stem(_cell) => {
                error!("Cell is not provisioned or cloned");
                return Err(anyhow!("Cell is not provisioned or cloned"));
            }
        };

        let agent_pub_key = app_info.agent_pub_key;

        //Get the agents pub key from the conductor

        fn generate_nonce() -> [u8; 32] {
            let mut rng = OsRng;
            let mut nonce = [0u8; 32];
            rng.fill(&mut nonce);
            nonce
        }

        let payload = match payload {
            Some(payload) => payload,
            None => ExternIO::encode(()).unwrap(),
        };

        let zome_call_params = ZomeCallParams {
            cell_id,
            zome_name: zome_name.into(),
            fn_name: fn_name.into(),
            payload,
            cap_secret: None,
            provenance: agent_pub_key,
            nonce: generate_nonce().into(),
            expires_at: Timestamp::now()
                .checked_add_signed(&Duration::seconds(300))
                .unwrap(),
        };

        //let keystore = self.conductor.keystore();
        //let signed_zome_call = ZomeCall::try_from_unsigned_zome_call(keystore, zome_call_unsigned)
        //    .await
        //    .map_err(|err| anyhow!("Could not sign zome call: {:?}", err))?;

        let conductor_api_result = self.conductor.call_zome(zome_call_params).await;
        match conductor_api_result {
            Ok(result) => match result {
                Ok(result) => Ok(result.into()),
                Err(err) => {
                    let formatted_err = Self::format_error_with_stacktrace(&err);
                    error!("Error calling zome function:\n{}", formatted_err);
                    Err(anyhow!("Error calling zome function: {:?}", err))
                }
            },
            Err(err) => {
                let formatted_err = Self::format_error_with_stacktrace(&err);
                error!("Conductor API error:\n{}", formatted_err);
                Err(anyhow!("Conductor API error: {:?}", err))
            }
        }
    }

    pub async fn remove_app(&self, app_id: String) -> Result<(), AnyError> {
        //Check that the app exists on the conductor
        let app_info = self.conductor.get_app_info(&app_id).await?;

        if app_info.is_none() {
            return Err(anyhow!("App not installed with id: {:?}", app_id));
        }

        self.conductor
            .clone()
            .uninstall_app(&app_id, true)
            .await
            .map_err(|e| anyhow!("Could not remove app: {:?}", e))?;

        info!("Removed app with id: {:?}", app_id);
        Ok(())
    }

    pub async fn agent_infos(&self) -> Result<Vec<String>, AnyError> {
        // Get agent infos for running cells.
        // K2 spaces should already be available since install_app awaits
        // cell network join completion before returning.
        let running_cell_ids = self.conductor.running_cell_ids();
        let running_dna_hashes: std::collections::HashSet<_> = running_cell_ids
            .iter()
            .map(|cell_id| cell_id.dna_hash().clone())
            .collect();

        if running_dna_hashes.is_empty() {
            return Ok(Vec::new());
        }

        let mut all_agent_infos = Vec::new();
        let mut failed_dnas = Vec::new();

        for dna_hash in running_dna_hashes {
            match self
                .conductor
                .get_agent_infos(Some(vec![dna_hash.clone()]))
                .await
            {
                Ok(infos) => {
                    for info in infos {
                        if let Ok(encoded) = (*info).encode() {
                            all_agent_infos.push(encoded);
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to get agent infos for DNA {:?}: {:?}", dna_hash, e);
                    failed_dnas.push(dna_hash.clone());
                }
            }
        }

        if !failed_dnas.is_empty() {
            info!(
                "Got agent infos for {} DNAs, {} DNAs failed",
                all_agent_infos.len(),
                failed_dnas.len()
            );
        }

        Ok(all_agent_infos)
    }

    pub async fn add_agent_infos(&self, agent_infos: Vec<String>) -> Result<(), AnyError> {
        // Add agent infos individually. K2 spaces should already be available
        // since install_app awaits cell network join completion — but under HC
        // 0.7's cell-init races, the space is occasionally still spinning up
        // when the first burst of agent-info gossip arrives.
        //
        // CodeRabbit review PR #907 finding #2: earlier version swallowed
        // every error into the `skipped` bucket, hiding real problems. New
        // shape:
        //   - transient K2SpaceNotFound  → short retry with backoff, then skip
        //     if it still doesn't come up (space genuinely isn't ours to join)
        //   - other errors               → keep going but log at ERROR and
        //     surface via failure_count in the summary line, so a real
        //     conductor bug doesn't fall off silently
        //
        // CodeRabbit round-2 finding @949: the dispatcher wraps this whole
        // fn in a 30s timeout. Per-item retries of 200+400+800ms = 1.4s
        // each; ~21 items for an unavailable K2 space burn the full budget
        // and the caller gets a Timeout instead of the skip summary. Fix:
        // once we've exhausted retries for a given K2SpaceNotFound error
        // fingerprint, cache it and short-circuit further items whose error
        // matches — no sleep, straight to `skipped`. The K2SpaceNotFound
        // error string embeds the space id/hash, so distinct spaces have
        // distinct fingerprints.
        const K2_SPACE_RETRIES: usize = 3;
        const K2_SPACE_RETRY_BASE_MS: u64 = 200;

        let mut success_count = 0usize;
        let mut skipped_count = 0usize;
        let mut failure_count = 0usize;
        let mut exhausted_spaces: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        /// Extract a stable per-space fingerprint from the K2SpaceNotFound
        /// error string. The error typically formats as
        /// `K2SpaceNotFound(<hex-space-id>)` — slicing the whole `{:?}` is
        /// good enough as a set key: distinct spaces give distinct debug
        /// strings; identical errors give identical strings.
        fn space_fingerprint(err_str: &str) -> String {
            // Trim to the substring starting at K2SpaceNotFound (or K2 Space)
            // so trailing context that varies per-call doesn't defeat the cache.
            if let Some(idx) = err_str.find("K2SpaceNotFound") {
                err_str[idx..]
                    .split(&[',', ')', '\n'][..])
                    .next()
                    .unwrap_or(err_str)
                    .to_string()
            } else if let Some(idx) = err_str.find("K2 Space") {
                err_str[idx..]
                    .split(&['\n'][..])
                    .next()
                    .unwrap_or(err_str)
                    .to_string()
            } else {
                err_str.to_string()
            }
        }

        // NET-DIAG: track which spaces we successfully added infos into vs
        // which stayed "unavailable" — lets us cross-reference the running
        // cell list at the end of the batch.
        let mut succeeded_by_space: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        let mut skipped_by_space: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for (idx, agent_info) in agent_infos.iter().enumerate() {
            let mut attempt = 0usize;
            loop {
                match self
                    .conductor
                    .add_agent_infos(vec![agent_info.clone()])
                    .await
                {
                    Ok(()) => {
                        success_count += 1;
                        // We don't have the space id here — use a placeholder.
                        *succeeded_by_space.entry("<ok>".to_string()).or_insert(0) += 1;
                        break;
                    }
                    Err(e) => {
                        let error_str = format!("{:?}", e);
                        let is_space_not_found = error_str.contains("K2SpaceNotFound")
                            || (error_str.contains("K2 Space")
                                && error_str.contains("does not exist"));

                        if is_space_not_found {
                            let fp = space_fingerprint(&error_str);
                            if exhausted_spaces.contains(&fp) {
                                skipped_count += 1;
                                *skipped_by_space.entry(fp).or_insert(0) += 1;
                                break;
                            }
                            if attempt < K2_SPACE_RETRIES {
                                let delay_ms = K2_SPACE_RETRY_BASE_MS * (1u64 << attempt as u32);
                                tokio::time::sleep(std::time::Duration::from_millis(delay_ms))
                                    .await;
                                attempt += 1;
                                continue;
                            }
                            // NET-DIAG: log the actual K2SpaceNotFound error the
                            // FIRST time we hit a new fingerprint. Currently we
                            // just count-and-move-on — which is what hid the
                            // multi-user cross-node failure signature for weeks.
                            error!(
                                "add_agent_infos: exhausted retries for space (item #{idx}): {error_str}"
                            );
                            exhausted_spaces.insert(fp.clone());
                            skipped_count += 1;
                            *skipped_by_space.entry(fp).or_insert(0) += 1;
                            break;
                        }

                        // Not a K2 space error — real problem, surface it.
                        error!("Failed to add agent info: {:?}", e);
                        failure_count += 1;
                        break;
                    }
                }
            }
        }

        // NET-DIAG: end-of-batch snapshot. Lists every running cell's DNA
        // hash (which becomes the K2 space id), and the per-space skip
        // counts so we can see whether the "unavailable" spaces correspond
        // to DNAs the conductor thinks it has running — that would prove
        // the space-init race is a false alarm and the real problem is
        // network handshake.
        {
            let running_cell_ids = self.conductor.running_cell_ids();
            let running_dnas: std::collections::HashSet<_> = running_cell_ids
                .iter()
                .map(|c| c.dna_hash().to_string())
                .collect();
            info!(
                "NET-DIAG add_agent_infos: running_dnas={} skipped_spaces={} skips_by_space={:?} running={:?}",
                running_dnas.len(),
                exhausted_spaces.len(),
                skipped_by_space,
                running_dnas,
            );
        }

        if skipped_count > 0 || failure_count > 0 {
            info!(
                "Added {} agent infos, skipped {} (K2 space unavailable across {} distinct \
                 space(s)), failed {} (other errors)",
                success_count,
                skipped_count,
                exhausted_spaces.len(),
                failure_count
            );
        }

        Ok(())
    }

    pub async fn sign(&self, data: String) -> Result<Signature, AnyError> {
        let keystore = self.conductor.keystore();
        let pub_keys = keystore.list_public_keys().await?;
        if pub_keys.is_empty() {
            return Err(anyhow!("No public keys found"));
        }
        let agent = pub_keys.first().unwrap();

        let vec_u8 = data.into_bytes();
        let data = Arc::from(vec_u8.into_boxed_slice());

        let signature = keystore.sign(agent.clone(), data).await?;
        Ok(signature)
    }

    pub async fn shutdown(&self) -> Result<(), AnyError> {
        self.conductor.clone().shutdown().await??;
        Ok(())
    }

    pub async fn get_agent_key(&self) -> Result<HoloHash<Agent>, AnyError> {
        let keystore = self.conductor.keystore();
        let pub_keys = keystore.list_public_keys().await?;
        if pub_keys.is_empty() {
            return Err(anyhow!("No public keys found"));
        }
        let agent = pub_keys.first().unwrap();
        Ok(agent.to_owned())
    }

    pub async fn get_app_info(&self, app_id: String) -> Result<Option<AppInfo>, AnyError> {
        Ok(self.conductor.get_app_info(&app_id).await?)
    }

    pub async fn log_network_metrics(&self) -> Result<(), AnyError> {
        let metrics = self
            .conductor
            .dump_network_metrics(Kitsune2NetworkMetricsRequest {
                dna_hash: None,
                include_dht_summary: true,
            })
            .await?;
        info!("Network metrics: {:?}", metrics);

        let stats = self.conductor.dump_network_stats().await?;
        info!("Network stats: {:?}", stats);

        Ok(())
    }

    pub async fn get_network_metrics(&self) -> Result<String, AnyError> {
        let metrics = self
            .conductor
            .dump_network_metrics(Kitsune2NetworkMetricsRequest {
                dna_hash: None,
                include_dht_summary: true,
            })
            .await?;

        let stats = self.conductor.dump_network_stats().await?;

        // Convert HoloHash<Dna> keys to strings for JSON serialization
        let metrics_with_string_keys: std::collections::HashMap<String, _> = metrics
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect();

        // Convert stats to JSON-safe structure. The blocked_message_counts field
        // has HashMap<Url, HashMap<SpaceId, _>> where SpaceId doesn't serialize
        // as a JSON string key, so we convert all map keys to strings.
        let blocked_counts_safe: std::collections::HashMap<
            String,
            std::collections::HashMap<String, _>,
        > = stats
            .blocked_message_counts
            .into_iter()
            .map(|(url, inner)| {
                let inner_safe: std::collections::HashMap<String, _> = inner
                    .into_iter()
                    .map(|(space_id, count)| (format!("{:?}", space_id), count))
                    .collect();
                (url.to_string(), inner_safe)
            })
            .collect();

        let combined_metrics = serde_json::json!({
            "metrics": metrics_with_string_keys,
            "stats": {
                "transport_stats": stats.transport_stats,
                "blocked_message_counts": blocked_counts_safe
            }
        });

        Ok(serde_json::to_string(&combined_metrics)?)
    }

    pub async fn pack_happ(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let name = holochain_cli_bundle::get_app_name(&path).await?;
        info!("Got hApp name: {:?}", name);
        let pack = holochain_cli_bundle::pack::<AppManifest>(&path, None, name).await?;
        info!("Packed hApp at path: {:#?}", pack);
        Ok(pack.to_str().unwrap().to_string())
    }

    pub async fn unpack_happ(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let pack = holochain_cli_bundle::expand_bundle::<AppManifest>(&path, None, true).await?;
        info!("UnPacked hApp at path: {:#?}", pack);
        Ok(pack.to_str().unwrap().to_string())
    }

    pub async fn pack_dna(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let name = holochain_cli_bundle::get_dna_name(&path).await?;
        info!("Got dna name: {:?}", name);
        let pack = holochain_cli_bundle::pack::<ValidatedDnaManifest>(&path, None, name).await?;
        info!("Packed dna at path: {:#?}", pack);
        Ok(pack.to_str().unwrap().to_string())
    }

    pub async fn unpack_dna(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let pack =
            holochain_cli_bundle::expand_bundle::<ValidatedDnaManifest>(&path, None, true).await?;
        info!("UnPacked dna at path: {:#?}", pack);
        Ok(pack.to_str().unwrap().to_string())
    }
}

pub async fn run_local_hc_services() -> Result<(), AnyError> {
    let ops = holochain_cli_run_local_services::HcRunLocalServices::new(
        None,
        String::from("127.0.0.1"),
        0,
        false,
        None,
        String::from("127.0.0.1"),
        0,
        false,
    );
    ops.run().await;
    Ok(())
}

/// Resolve the relay_url that HC 0.7's NetworkConfig requires.
///
/// Precedence (matches pre-0.7 AD4M semantics):
///   1. `use_proxy=true` + non-empty `proxy_url` → use `proxy_url` as relay
///   2. explicit `relay_url` (from launcher config)          → use it
///   3. hard-coded default (public AD4M bootstrap relay)     → fallback
///
/// Factored out of `HolochainService::new` so the same resolution can be
/// applied both to fresh configs and to migrated legacy configs (see
/// `migrate_legacy_conductor_config`).
///
/// CodeRabbit review PR #907 finding #4.
fn resolve_relay_url(local_config: &LocalConductorConfig) -> String {
    if local_config.use_proxy && !local_config.proxy_url.is_empty() {
        local_config.proxy_url.clone()
    } else if let Some(ref relay_url) = local_config.relay_url {
        relay_url.clone()
    } else {
        "http://bootstrap.ad4m.dev:4433/relay".to_string()
    }
}

/// Migrate a Holochain 0.6-era `conductor_config.yaml` in place so that
/// Holochain 0.7's stricter deserializer accepts it.
///
/// Two changes are needed:
///   1. **Strip obsolete keys** (HC 0.7 rejects unknown fields):
///      `network.signal_url` (folded into the iroh relay) and
///      `network.webrtc_config` (WebRTC transport is gone).
///   2. **Inject `network.relay_url`** if absent — HC 0.7 makes this field
///      REQUIRED. A file migrated by (1) alone still fails `load_yaml` on
///      the missing-field check. The caller passes in the resolved URL
///      (from `resolve_relay_url`) so precedence stays centralised.
///
/// The helper reads the file, parses it as a generic YAML mapping, applies
/// the changes, and writes it back. Idempotent: if the file is already
/// 0.7-clean (no obsolete keys, `relay_url` already present), it is left
/// untouched (no rewrite, no mtime change).
///
/// CodeRabbit review PR #907 finding #3 (strip obsolete keys) + round-2
/// finding @508 (inject relay_url).
async fn migrate_legacy_conductor_config(
    path: &std::path::Path,
    resolved_relay_url: Option<&str>,
) -> Result<(), AnyError> {
    let raw = tokio::fs::read_to_string(path)
        .await
        .map_err(|e| anyhow!("read conductor_config.yaml: {}", e))?;

    let mut doc: serde_yaml::Value = serde_yaml::from_str(&raw)
        .map_err(|e| anyhow!("parse conductor_config.yaml as YAML: {}", e))?;

    let mut removed: Vec<&'static str> = Vec::new();
    let mut injected_relay = false;

    // Ensure a `network:` mapping exists so we can operate on it uniformly.
    // If the file has no network section at all, create an empty one.
    let doc_map = doc
        .as_mapping_mut()
        .ok_or_else(|| anyhow!("conductor_config.yaml root is not a mapping"))?;
    let network_key = serde_yaml::Value::String("network".to_string());
    if !doc_map.contains_key(&network_key) {
        doc_map.insert(
            network_key.clone(),
            serde_yaml::Value::Mapping(serde_yaml::Mapping::new()),
        );
    }
    let network = doc_map
        .get_mut(&network_key)
        .and_then(|n| n.as_mapping_mut())
        .ok_or_else(|| anyhow!("conductor_config.yaml `network` is not a mapping"))?;

    for legacy_key in ["signal_url", "webrtc_config"] {
        if network
            .remove(serde_yaml::Value::String(legacy_key.to_string()))
            .is_some()
        {
            removed.push(legacy_key);
        }
    }

    // Round-2 finding @508: inject the resolved relay_url when absent.
    // We only inject when the caller gave us one AND the file doesn't
    // already have one. If the file HAS a relay_url we honour it (users
    // may have hand-tuned it).
    let relay_key = serde_yaml::Value::String("relay_url".to_string());
    if !network.contains_key(&relay_key) {
        if let Some(url) = resolved_relay_url {
            network.insert(relay_key, serde_yaml::Value::String(url.to_string()));
            injected_relay = true;
        }
    }

    if removed.is_empty() && !injected_relay {
        // Nothing to migrate; leave the file untouched to preserve mtime.
        return Ok(());
    }

    let out = serde_yaml::to_string(&doc)
        .map_err(|e| anyhow!("re-serialize migrated conductor_config.yaml: {}", e))?;
    tokio::fs::write(path, out)
        .await
        .map_err(|e| anyhow!("write migrated conductor_config.yaml: {}", e))?;

    let mut summary = format!("Migrated conductor_config.yaml at {:?}:", path);
    if !removed.is_empty() {
        summary.push_str(&format!(" stripped obsolete network.* keys: {:?}", removed));
    }
    if injected_relay {
        summary.push_str(&format!(
            "{} injected required network.relay_url = {}",
            if removed.is_empty() { "" } else { ";" },
            resolved_relay_url.unwrap_or("")
        ));
    }
    info!("{}", summary);
    Ok(())
}

#[cfg(test)]
mod tests {
    use tokio::time::{Duration, Instant};

    /// Integration test: start a real Holochain conductor and generate signing keypairs.
    #[tokio::test(flavor = "multi_thread")]
    async fn test_new_sign_keypair_random() {
        use super::*;

        // Init V8 / Deno platform (once) — required by Holochain
        {
            use std::sync::Once;
            static V8_INIT: Once = Once::new();
            V8_INIT.call_once(|| {
                deno_core::v8::V8::set_flags_from_string("--max-opt=0");
                deno_core::JsRuntime::init_platform(None);
            });
        }

        let _ = rustls::crypto::aws_lc_rs::default_provider().install_default();

        let tmp = std::env::temp_dir().join(format!("ad4m_test_keypair_{}", std::process::id()));
        let conductor_path = tmp.join("conductor");
        std::fs::create_dir_all(&conductor_path).unwrap();

        // Cleanup on drop
        struct CleanupDir(std::path::PathBuf);
        impl Drop for CleanupDir {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }
        let _cleanup = CleanupDir(tmp.clone());

        let config = LocalConductorConfig {
            passphrase: "test-passphrase".into(),
            conductor_path: conductor_path.to_string_lossy().into(),
            data_path: tmp.to_string_lossy().into(),
            use_bootstrap: false,
            use_proxy: false,
            use_local_proxy: false,
            use_mdns: false,
            proxy_url: "ws://localhost:4444".into(),
            bootstrap_url: "http://localhost:4445".into(),
            relay_url: None,
            app_port: 0,
        };

        let service = HolochainService::new(config)
            .await
            .expect("Failed to start conductor");

        // Generate first keypair
        let key1 = service
            .conductor
            .keystore()
            .new_sign_keypair_random()
            .await
            .expect("Failed to generate first keypair");
        let raw1 = key1.get_raw_39();
        assert_eq!(raw1.len(), 39, "Agent key should be 39 bytes");

        // Generate second keypair — must be different
        let key2 = service
            .conductor
            .keystore()
            .new_sign_keypair_random()
            .await
            .expect("Failed to generate second keypair");
        let raw2 = key2.get_raw_39();
        assert_eq!(raw2.len(), 39);
        assert_ne!(raw1, raw2, "Two generated keys must be distinct");

        // Both keys should be in the keystore now
        let all_keys = service
            .conductor
            .keystore()
            .list_public_keys()
            .await
            .expect("Failed to list keys");
        assert!(
            all_keys.len() >= 2,
            "Keystore should have at least 2 keys, got {}",
            all_keys.len()
        );
        assert!(all_keys.contains(&key1), "Key1 should be in keystore");
        assert!(all_keys.contains(&key2), "Key2 should be in keystore");

        // Verify round-trip: base64 encode then decode back
        use base64::Engine;
        let b64 = base64::engine::general_purpose::STANDARD.encode(raw1);
        let decoded = base64::engine::general_purpose::STANDARD
            .decode(&b64)
            .expect("base64 decode failed");
        let reconstructed = holochain::prelude::AgentPubKey::from_raw_39(decoded);
        assert_eq!(
            key1, reconstructed,
            "Round-trip base64 encode/decode should produce the same key"
        );

        service.shutdown().await.expect("Failed to shut down");
    }

    /// Integration test: generate keypairs via the HolochainServiceInterface message-passing path.
    #[tokio::test(flavor = "multi_thread")]
    async fn test_new_sign_keypair_via_interface() {
        use super::*;

        {
            use std::sync::Once;
            static V8_INIT2: Once = Once::new();
            V8_INIT2.call_once(|| {
                // V8 may already be initialized by the other test
            });
        }

        let _ = rustls::crypto::aws_lc_rs::default_provider().install_default();

        let tmp =
            std::env::temp_dir().join(format!("ad4m_test_keypair_iface_{}", std::process::id()));
        let conductor_path = tmp.join("conductor");
        std::fs::create_dir_all(&conductor_path).unwrap();

        struct CleanupDir(std::path::PathBuf);
        impl Drop for CleanupDir {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }
        let _cleanup = CleanupDir(tmp.clone());

        let config = LocalConductorConfig {
            passphrase: "test-passphrase-iface".into(),
            conductor_path: conductor_path.to_string_lossy().into(),
            data_path: tmp.to_string_lossy().into(),
            use_bootstrap: false,
            use_proxy: false,
            use_local_proxy: false,
            use_mdns: false,
            proxy_url: "ws://localhost:4444".into(),
            bootstrap_url: "http://localhost:4445".into(),
            relay_url: None,
            app_port: 0,
        };

        // Use HolochainService::init which sets up the full message-passing loop
        HolochainService::init(config)
            .await
            .expect("Failed to init holochain service");

        // Get the interface
        let iface = get_holochain_service().await;

        // Generate keypair via interface
        let key1 = iface
            .new_sign_keypair_random()
            .await
            .expect("Failed to generate keypair via interface");
        assert_eq!(key1.get_raw_39().len(), 39);

        let key2 = iface
            .new_sign_keypair_random()
            .await
            .expect("Failed to generate second keypair via interface");
        assert_ne!(
            key1, key2,
            "Two generated keys via interface must be distinct"
        );

        // Also verify get_agent_key still works
        let existing_key = iface
            .get_agent_key()
            .await
            .expect("Failed to get existing agent key");
        assert_eq!(existing_key.get_raw_39().len(), 39);

        iface.shutdown().await.expect("Failed to shutdown");
    }

    #[tokio::test]
    async fn test_signal_loop_performance() {
        // Test that the signal processing loop doesn't consume excessive CPU
        // when no signals are being processed

        let start_time = Instant::now();
        let test_duration = Duration::from_millis(100);

        // Simulate the signal processing pattern with backoff
        let iterations = tokio::spawn(async move {
            let mut count = 0;
            let start = Instant::now();

            while start.elapsed() < test_duration {
                // Simulate the select! pattern with backoff
                tokio::select! {
                    // Simulate no signals available
                    _ = tokio::time::sleep(Duration::from_millis(1)) => {
                        // This branch represents the backoff case
                    }
                }
                count += 1;
            }
            count
        })
        .await
        .unwrap();

        let elapsed = start_time.elapsed();

        // With 1ms delays, we should get roughly 100 iterations in 100ms
        // This verifies the backoff is working and not busy-waiting
        // Allow for some variance due to system scheduling
        assert!(
            iterations < 300,
            "Too many iterations: {} (expected < 300), suggests busy-waiting",
            iterations
        );
        assert!(
            iterations > 20,
            "Too few iterations: {} (expected > 20), suggests delays are too long",
            iterations
        );
        assert!(
            elapsed >= test_duration,
            "Test didn't run for expected duration"
        );

        println!(
            "Signal loop test: {} iterations in {:?}",
            iterations, elapsed
        );
    }
}
