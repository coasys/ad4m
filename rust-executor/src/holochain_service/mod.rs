use chrono::Duration;
use crypto_box::rand_core::OsRng;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::path::PathBuf;
use std::sync::Arc;

use holochain::conductor::api::{AppInfo, AppStatusFilter, CellInfo};
use holochain::conductor::config::{ConductorConfig, NetworkConfig};
use holochain::conductor::paths::DataRootPath;
use holochain::conductor::{ConductorBuilder, ConductorHandle};
use holochain::prelude::hash_type::Agent;
use holochain::prelude::{
    ExternIO, HoloHash, InstallAppPayload, Kitsune2NetworkMetricsRequest, Signal, Signature, Timestamp, ZomeCallParams, ZomeCallResponse
};
use holochain::test_utils::itertools::Either;

use holochain_types::dna::ValidatedDnaManifest;
use holochain_types::websocket::AllowedOrigins;
use kitsune_p2p_types::dependencies::url2::Url2;
use log::{error, info};
use rand::Rng;
use serde::{Deserialize, Serialize};
use tokio::select;
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio::task::yield_now;
use tokio::time::timeout;

use tokio_stream::StreamExt;

pub mod holochain_service_extension;
pub(crate) mod interface;

pub(crate) use interface::{
    get_holochain_service, maybe_get_holochain_service, HolochainServiceInterface,
    HolochainServiceRequest, HolochainServiceResponse,
};

use self::interface::set_holochain_service;

const COASYS_BOOTSTRAP_AGENT_INFO: &str = r#" ["g6VhZ2VudMQkeWyy+u7ziOZEejqRGCHVSjWuNDGCkHSFWpkp/DsXJFVDyWYdqXNpZ25hdHVyZcRAlYaUoegA0DB+U8F2cONLcoORjqz7WqW4dBSfvWyQ4AixLLB3h0jsvqGUo0UfowjUP1ntBhMjA8xo/oQateooDaphZ2VudF9pbmZvxPuGpXNwYWNlxCReuo1fprVD9jjsQWRglwEzVlWFiYB+4BEA7BQIwOpYgUgezPGlYWdlbnTEJHlssvru84jmRHo6kRgh1Uo1rjQxgpB0hVqZKfw7FyRVQ8lmHaR1cmxzkdlJd3NzOi8vc2lnbmFsLmhvbG8uaG9zdC90eDUtd3MvNEFNaGNWNHhpdFdPMHI2YUR1NjFwcW5jMW5LNjBmdkRfYTRyZUJmUFdTMKxzaWduZWRfYXRfbXPPAAABk/NOnPewZXhwaXJlc19hZnRlcl9tc84AEk+AqW1ldGFfaW5mb8QZgahhcnFfc2l6ZYKlcG93ZXIRpWNvdW50CA=="]"#;
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
    pub app_port: u16,
}

impl HolochainService {
    pub async fn init(local_config: LocalConductorConfig) -> Result<(), AnyError> {
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
                        conductor_clone.list_apps(Some(AppStatusFilter::Running)).await.unwrap().into_iter().for_each(|app| {
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
                                else => break,
                            }
                            yield_now().await;
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
                                        std::time::Duration::from_secs(5),
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
                                HolochainServiceRequest::AgentInfos(response_tx) => {
                                    match timeout(
                                        std::time::Duration::from_secs(3),
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
                                        std::time::Duration::from_secs(3),
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

        let agent_infos: Vec<String> = serde_json::from_str(COASYS_BOOTSTRAP_AGENT_INFO)?;
        inteface
            .add_agent_infos(agent_infos)
            .await?;

        set_holochain_service(inteface).await;

        Ok(())
    }

    pub async fn new(local_config: LocalConductorConfig) -> Result<HolochainService, AnyError> {
        let conductor_yaml_path =
            std::path::Path::new(&local_config.conductor_path).join("conductor_config.yaml");
        let config = if conductor_yaml_path.exists() {
            ConductorConfig::load_yaml(&conductor_yaml_path)?
        } else {
            let mut config = ConductorConfig::default();
            let data_root_path: DataRootPath =
                PathBuf::from(local_config.conductor_path.clone()).into();
            config.data_root_path = Some(data_root_path);
            config.admin_interfaces = None;

            let mut network_config = NetworkConfig::default();

            
            // prod - https://bootstrap.holo.host
            // staging - https://bootstrap-staging.holo.host
            // dev - https://bootstrap-dev.holohost.workers.dev
            // own - http://207.148.16.17:38245
            network_config.bootstrap_url = Url2::parse(local_config.bootstrap_url);

            // prod - wss://signal.holo.host
            // dev - wss://signal.holotest.net
            // our - ws://207.148.16.17:42697
            network_config.signal_url = Url2::parse(local_config.proxy_url);

            config.network = network_config;

            config
        };

        info!("Starting holochain conductor with config: {:#?}", config);
        let passphrase_locked_array = sodoken::LockedArray::from(local_config.passphrase.as_bytes().to_vec());
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

                self.conductor
                    .clone()
                    .enable_app(app_id.clone())
                    .await
                    .map_err(|e| anyhow!("Could not activate app: {:?}", e))?;

                let app_info = self.conductor.get_app_info(&app_id).await?;
                Ok(app_info.unwrap())
            }
            Some(app_info) => {
                info!("App already installed with id: {:?}", app_id);
                Ok(app_info)
            }
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
            return Err(anyhow!("App not installed with id: {:?}", app_id));
        }

        let app_info = app_info.unwrap();

        let cell_entry = app_info.cell_info.get(&format!("{}-{}", app_id, cell_name));

        if cell_entry.is_none() {
            return Err(anyhow!(
                "Cell not installed with name: {:?} in app: {:?}",
                cell_name,
                app_id
            ));
        }

        if cell_entry.unwrap().is_empty() {
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
            CellInfo::Stem(_cell) => return Err(anyhow!("Cell is not provisioned or cloned",)),
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

        let result = self.conductor.call_zome(zome_call_params).await??;

        Ok(result)
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
        Ok(self.conductor.get_agent_infos(None).await?.into_iter().map(|arc| (*arc).encode()).collect::<Result<Vec<_>, _>>()?)
    }

    pub async fn add_agent_infos(&self, agent_infos: Vec<String>) -> Result<(), AnyError> {
        Ok(self.conductor.add_agent_infos(agent_infos).await?)
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
        self.conductor.shutdown().await??;
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
        let metrics = self.conductor.dump_network_metrics(Kitsune2NetworkMetricsRequest{
            dna_hash: None,
            include_dht_summary: true,
        }).await?;
        info!("Network metrics: {:?}", metrics);

        let stats = self.conductor.dump_network_stats().await?;
        info!("Network stats: {:?}", stats);

        Ok(())
    }

    pub async fn pack_dna(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let name = holochain_cli_bundle::get_dna_name(&path).await?;
        info!("Got dna name: {:?}", name);
        let pack =
            holochain_cli_bundle::pack::<ValidatedDnaManifest>(&path, None, name, false).await?;
        info!("Packed dna at path: {:#?}", pack.0);
        Ok(pack.0.to_str().unwrap().to_string())
    }

    pub async fn unpack_dna(path: String) -> Result<String, AnyError> {
        let path = PathBuf::from(path);
        let pack =
            holochain_cli_bundle::unpack::<ValidatedDnaManifest>("dna", &path, None, true).await?;
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
