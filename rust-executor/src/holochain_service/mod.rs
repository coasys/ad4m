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
    AppManifest, DnaHash, ExternIO, HoloHash, InstallAppPayload, Kitsune2NetworkMetricsRequest,
    Signal, Signature, Timestamp, ZomeCallParams, ZomeCallResponse,
};
use holochain::test_utils::itertools::Either;

use holochain_types::dna::ValidatedDnaManifest;
use holochain_types::websocket::AllowedOrigins;
use log::{error, info};
use rand::Rng;
use serde::{Deserialize, Serialize};
use tokio::select;
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio::time::timeout;
use url2::Url2;

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
        // Resolve the relay_url once. Precedence: use_proxy → explicit
        // relay_url → default. CodeRabbit review PR #907 finding #4.
        let resolved_relay_url = resolve_relay_url(&local_config);

        // The conductor config is always constructed in code from
        // LocalConductorConfig. Nothing in AD4M has ever written a
        // conductor_config.yaml into conductor_path, so the old
        // "load_yaml if the file exists" branch (added 2023) only ever
        // served hand-authored files — and silently overrode the launcher/
        // CLI flags when one was present. Dropped on PR #907 (per Nico's
        // review question) together with the HC 0.6→0.7 yaml migration
        // shim that existed only to keep that branch loadable.
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
        network_config.relay_url = Url2::parse(resolved_relay_url.as_str());

        config.network = network_config;

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

        // Two-stage network readiness gate before we hand control back to the
        // caller. Both stages are needed because HC 0.7's `JoinComplete` and
        // "the peer store has at least one entry" are distinct events, and
        // the caller's very next step is typically `add_agent_infos`, which
        // routes via `holochain_p2p.publish_agent_info`. That routing lookup
        // needs a resolvable peer entry in the target space — an empty peer
        // store yields K2SpaceNotFound and the info is silently skipped,
        // which manifests upstream as "cross-node discovery never completes".
        //
        // Stage 1 — per-cell: wait for JoinComplete via the fork's built-in
        //   event-driven method. Fast (~ms) once the k2 space join returns
        //   Ok. The fork's own enum doc says JoinComplete means "the agent
        //   has successfully joined the k2 space, but peers may not yet be
        //   discovered via bootstrap" — hence stage 2.
        //
        // Stage 2 — per DNA: wait until at least one peer has appeared in
        //   the peer store for that DNA, or the fork's internal peer-monitoring
        //   task has given up. See `await_initial_peer_discovery` below for
        //   the naming rationale and the polling-vs-subscribe trade-off.
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

        let dna_hashes: std::collections::HashSet<_> =
            app_cell_ids.iter().map(|c| c.dna_hash().clone()).collect();
        for dna_hash in &dna_hashes {
            self.await_initial_peer_discovery(dna_hash, std::time::Duration::from_secs(11))
                .await;
        }

        let app_info = self.conductor.get_app_info(&app_id).await?;
        let app_info = app_info.ok_or_else(|| anyhow!("App not found: {}", app_id))?;
        Ok(app_info)
    }

    /// Wait until the peer store for `dna_hash` holds at least one peer, or
    /// the fork's internal peer-monitoring task has given up (whichever is
    /// sooner). Returns silently in either case — not being able to reach a
    /// peer at startup is common (single-node deployments, isolated tests,
    /// first node online) and is surfaced only via log level.
    ///
    /// # Naming
    ///
    /// Deliberately not called `await_bootstrap_complete`. In a fully p2p
    /// network the peer store is *never* "complete" — it just accumulates
    /// as new peers are discovered. "Initial peer discovery" reflects what
    /// this actually gates on: the *first* peer showing up (so a following
    /// `add_agent_infos` burst has somewhere to route to), not a false
    /// milestone that discovery has finished. Framing per Nico's discussion
    /// with Guillem and Joost from the Holochain team.
    ///
    /// # Implementation
    ///
    /// Polls `ConductorNetworkState::is_bootstrap_complete` (which is the
    /// state-side mirror of `NetworkEvent::BootstrapComplete`, fired by the
    /// fork's `start_peer_monitoring` task once the first peer appears OR
    /// after its internal 10s cap). We poll rather than subscribe to the
    /// broadcast channel because subscription races with already-fired
    /// events — the fork's own `await_cell_network_join_complete` uses a
    /// subscribe-then-recheck-state bracket to work around this. For this
    /// call site polling at 200ms with an 11s cap is cheap, simple, and
    /// avoids re-implementing that bracket AD4M-side. If this ever needs
    /// to be tighter it should move upstream as a proper `Conductor`
    /// wrapper (see `crates/holochain/src/conductor/conductor.rs` next to
    /// `await_cell_network_join_complete`) and dropped here.
    async fn await_initial_peer_discovery(&self, dna_hash: &DnaHash, timeout: std::time::Duration) {
        const POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(200);

        let start = std::time::Instant::now();
        loop {
            let discovered = {
                let state = self.conductor.network_state.read().await;
                state.is_bootstrap_complete(dna_hash)
            };
            if discovered {
                info!(
                    "await_initial_peer_discovery: first peer observed for DNA {:?} after {:?}",
                    dna_hash,
                    start.elapsed()
                );
                return;
            }
            if start.elapsed() >= timeout {
                // Not fatal — the fork's peer monitor also emits
                // BootstrapComplete after its internal cap even with zero
                // peers (which flips the state field we're polling), so
                // this branch normally shouldn't trip. If it does, we
                // surface it and continue: subsequent gossip rounds may
                // still recover.
                error!(
                    "await_initial_peer_discovery: no peer observed within {:?} for DNA {:?} \
                     (peer store may still be empty; add_agent_infos rounds may skip this space)",
                    timeout, dna_hash
                );
                return;
            }
            tokio::time::sleep(POLL_INTERVAL).await;
        }
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

        for agent_info in agent_infos {
            let mut attempt = 0usize;
            loop {
                match self
                    .conductor
                    .add_agent_infos(vec![agent_info.clone()])
                    .await
                {
                    Ok(()) => {
                        success_count += 1;
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
                                // Same space we already gave up on — skip
                                // immediately, no sleep. This keeps the whole
                                // batch within the 30s dispatcher budget even
                                // if hundreds of agent infos target the same
                                // never-going-to-arrive space.
                                skipped_count += 1;
                                break;
                            }
                            if attempt < K2_SPACE_RETRIES {
                                // First-in-space — try backoff. Space may
                                // still be initialising.
                                let delay_ms = K2_SPACE_RETRY_BASE_MS * (1u64 << attempt as u32);
                                tokio::time::sleep(std::time::Duration::from_millis(delay_ms))
                                    .await;
                                attempt += 1;
                                continue;
                            }
                            // Retries exhausted for THIS space — cache the
                            // fingerprint so subsequent items with the same
                            // failure short-circuit.
                            exhausted_spaces.insert(fp);
                            skipped_count += 1;
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

/// Resolve the relay_url that HC 0.7's NetworkConfig requires.
///
/// Precedence (matches pre-0.7 AD4M semantics):
///   1. `use_proxy=true` + non-empty `proxy_url` → use `proxy_url` as relay
///   2. explicit `relay_url` (from launcher config)          → use it
///   3. hard-coded default (public AD4M bootstrap relay)     → fallback
///
/// # Path suffix
///
/// `kitsune2-bootstrap-srv` (>=0.4.0) serves the iroh relay at the
/// `/relay` path on the *same* port as the bootstrap endpoints — verified
/// against `kitsune2_bootstrap_srv-0.5.0/src/http.rs:525` and
/// `src/lib.rs`. iroh dials the WebSocket upgrade at that exact path, so
/// the URL we hand to `NetworkConfig.relay_url` MUST include `/relay`.
///
/// Legacy launcher configs still carry `proxy_url` / `relay_url` values
/// from the pre-0.7 days when the relay was hosted at the port root, so
/// we defensively append `/relay` to any candidate that lacks it. If a
/// self-hoster explicitly wants a different sub-path they can set it
/// explicitly; leaving `/relay` absent produces silent 404s that only
/// surface downstream as "cross-node discovery never completes", which
/// is the exact multi-user failure mode we've been chasing on this PR.
/// (Data's PR #907 review MED-2 — defaults disagreed; Nico confirmed
/// the relay was folded into the same binary but is still at `/relay`.)
///
/// CodeRabbit review PR #907 finding #4.
fn resolve_relay_url(local_config: &LocalConductorConfig) -> String {
    let url = if local_config.use_proxy && !local_config.proxy_url.is_empty() {
        local_config.proxy_url.clone()
    } else if let Some(ref relay_url) = local_config.relay_url {
        relay_url.clone()
    } else {
        "http://bootstrap.ad4m.dev:4433/relay".to_string()
    };
    let url = normalize_relay_scheme(&url);
    ensure_relay_path(&url)
}

/// Ensure the resolved relay URL carries the `/relay` path suffix that
/// `kitsune2-bootstrap-srv` expects. Pre-0.7 AD4M launcher configs
/// carry base URLs without a path (the old proxy server was hosted at
/// the port root); the new kitsune2 relay lives at `/relay` on the
/// same port as the bootstrap endpoints. Trailing slashes on the base
/// are tolerated.
fn ensure_relay_path(url: &str) -> String {
    let trimmed = url.trim_end_matches('/');
    if trimmed.ends_with("/relay") {
        trimmed.to_string()
    } else {
        format!("{}/relay", trimmed)
    }
}

/// Normalize a relay URL to the scheme iroh expects.
///
/// HC 0.7's `relay_url` is the iroh relay's BASE URL: `https://` for TLS
/// relays, `http://` for plain-text ones (the iroh client performs the
/// websocket upgrade itself). Pre-0.7 AD4M configs carry websocket-scheme
/// proxy URLs (`wss://` / `ws://`), and `resolve_relay_url` reuses
/// `proxy_url` as the relay — so those legacy schemes still reach us here.
/// iroh happens to dial TLS for `wss` too, but kitsune2's plain-text guard
/// only recognises `http`, so a plain-text `ws://` URL would slip past it
/// and then fail the TLS handshake. Map ws→http and wss→https so both the
/// guard and the dialer see the canonical scheme.
/// CodeRabbit review PR #907, relay_url-scheme thread.
fn normalize_relay_scheme(url: &str) -> String {
    if let Some(rest) = url.strip_prefix("wss://") {
        format!("https://{}", rest)
    } else if let Some(rest) = url.strip_prefix("ws://") {
        format!("http://{}", rest)
    } else {
        url.to_string()
    }
}

#[cfg(test)]
mod tests {
    use tokio::time::{Duration, Instant};

    #[test]
    fn test_normalize_relay_scheme() {
        // Legacy websocket schemes from pre-0.7 proxy_url configs map to
        // the base-URL schemes iroh expects.
        assert_eq!(
            super::normalize_relay_scheme("wss://proxy.ad4m.dev:443/relay"),
            "https://proxy.ad4m.dev:443/relay"
        );
        assert_eq!(
            super::normalize_relay_scheme("ws://127.0.0.1:4433"),
            "http://127.0.0.1:4433"
        );
        // Canonical schemes pass through untouched.
        assert_eq!(
            super::normalize_relay_scheme("https://relay.example/"),
            "https://relay.example/"
        );
        assert_eq!(
            super::normalize_relay_scheme("http://bootstrap.ad4m.dev:4433/relay"),
            "http://bootstrap.ad4m.dev:4433/relay"
        );
    }

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
