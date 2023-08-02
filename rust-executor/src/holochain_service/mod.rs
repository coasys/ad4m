use std::path::PathBuf;
use std::sync::Arc;

use chrono::Duration;
use crypto_box::rand_core::OsRng;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use holochain::conductor::api::{AppInfo, CellInfo, ZomeCall};
use holochain::conductor::config::ConductorConfig;
use holochain::conductor::{ConductorBuilder, ConductorHandle};
use holochain::prelude::agent_store::AgentInfoSigned;
use holochain::prelude::hash_type::Agent;
use holochain::prelude::kitsune_p2p::dependencies::url2::Url2;
use holochain::prelude::{
    ExternIO, HoloHash, InstallAppPayload, KitsuneP2pConfig, NetworkType, ProxyConfig, Signal,
    Signature, Timestamp, TransportConfig, ZomeCallResponse, ZomeCallUnsigned,
};
use holochain::test_utils::itertools::Either;
use log::info;
use rand::Rng;
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, oneshot, Mutex};
use tokio_stream::StreamExt;

pub(crate) mod holochain_service_extension;
pub(crate) mod interface;

pub(crate) use interface::{
    get_holochain_service, maybe_get_holochain_service, HolochainServiceInterface,
    HolochainServiceRequest, HolochainServiceResponse,
};

use self::interface::set_holochain_service;

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
    pub admin_port: u16,
}

impl HolochainService {
    pub async fn init(local_config: LocalConductorConfig) -> Result<(), AnyError> {
        let (sender, mut receiver) = mpsc::channel::<HolochainServiceRequest>(32);
        let (stream_sender, stream_receiver) = mpsc::unbounded_channel::<Signal>();

        let inteface = HolochainServiceInterface {
            sender,
            stream_receiver: Arc::new(Mutex::new(stream_receiver)),
        };

        let (response_sender, response_receiver) = oneshot::channel();

        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            rt.block_on(async move {
                let mut service = HolochainService::new(local_config).await.unwrap();

                set_holochain_service(inteface).await;

                let conductor_clone = service.conductor.clone();
                // Spawn a new task to forward items from the stream to the receiver
                tokio::spawn(async move {
                    let sig_broadcasters = conductor_clone.signal_broadcaster();

                    let mut streams = tokio_stream::StreamMap::new();
                    for (i, rx) in sig_broadcasters
                        .subscribe_separately()
                        .into_iter()
                        .enumerate()
                    {
                        streams.insert(i, tokio_stream::wrappers::BroadcastStream::new(rx));
                    }
                    let mut stream =
                        streams.map(|(_, signal)| signal.expect("Couldn't receive a signal"));

                    response_sender
                        .send(HolochainServiceResponse::InitComplete(Ok(())))
                        .unwrap();

                    loop {
                        while let Some(item) = stream.next().await {
                            let _ = stream_sender.send(item);
                        }
                    }
                });

                while let Some(message) = receiver.recv().await {
                    match message {
                        HolochainServiceRequest::InstallApp(payload, response) => {
                            let result = service.install_app(payload).await;
                            let _ = response.send(HolochainServiceResponse::InstallApp(result));
                        }
                        HolochainServiceRequest::CallZomeFunction {
                            app_id,
                            cell_name,
                            zome_name,
                            fn_name,
                            payload,
                            response,
                        } => {
                            let result = service
                                .call_zome_function(app_id, cell_name, zome_name, fn_name, payload)
                                .await;
                            let _ =
                                response.send(HolochainServiceResponse::CallZomeFunction(result));
                        }
                        HolochainServiceRequest::RemoveApp(app_id, response_tx) => {
                            let result = service.remove_app(app_id).await;
                            let _ = response_tx.send(HolochainServiceResponse::RemoveApp(result));
                        }
                        HolochainServiceRequest::AgentInfos(response_tx) => {
                            let result = service.agent_infos().await;
                            let _ = response_tx.send(HolochainServiceResponse::AgentInfos(result));
                        }
                        HolochainServiceRequest::AddAgentInfos(agent_infos, response_tx) => {
                            let result = service.add_agent_infos(agent_infos).await;
                            let _ =
                                response_tx.send(HolochainServiceResponse::AddAgentInfos(result));
                        }
                        HolochainServiceRequest::Sign(data, response_tx) => {
                            let result = service.sign(data).await;
                            let _ = response_tx.send(HolochainServiceResponse::Sign(result));
                        }
                        HolochainServiceRequest::Shutdown(response_tx) => {
                            let result = service.shutdown().await;
                            let _ = response_tx.send(HolochainServiceResponse::Shutdown(result));
                        }
                        HolochainServiceRequest::GetAgentKey(response_tx) => {
                            let result = service.get_agent_key().await;
                            let _ = response_tx.send(HolochainServiceResponse::GetAgentKey(result));
                        }
                        HolochainServiceRequest::GetAppInfo(app_id, response_tx) => {
                            let result = service.get_app_info(app_id).await;
                            let _ = response_tx.send(HolochainServiceResponse::GetAppInfo(result));
                        }
                        HolochainServiceRequest::GetNetworkMetrics(response_tx) => {
                            let result = service.get_network_metrics().await;
                            let _ = response_tx
                                .send(HolochainServiceResponse::GetNetworkMetrics(result));
                        }
                    }
                }
            });
        });

        match response_receiver.await? {
            HolochainServiceResponse::InitComplete(result) => result?,
            _ => unreachable!(),
        };

        Ok(())
    }

    pub async fn new(local_config: LocalConductorConfig) -> Result<HolochainService, AnyError> {
        let conductor_yaml_path =
            std::path::Path::new(&local_config.conductor_path).join("conductor_config.yaml");
        let config = if conductor_yaml_path.exists() {
            let config = ConductorConfig::load_yaml(&conductor_yaml_path)?;
            config
        } else {
            let mut config = ConductorConfig::default();
            config.environment_path = PathBuf::from(local_config.conductor_path.clone()).into();
            config.admin_interfaces = None;

            let mut kitsune_config = KitsuneP2pConfig::default();

            if local_config.use_bootstrap {
                kitsune_config.bootstrap_service = Some(Url2::parse(local_config.bootstrap_url));
            } else {
                kitsune_config.bootstrap_service = None;
            }
            if local_config.use_mdns {
                kitsune_config.network_type = NetworkType::QuicMdns;
            } else {
                kitsune_config.network_type = NetworkType::QuicBootstrap;
            }
            if local_config.use_proxy {
                kitsune_config.transport_pool = vec![TransportConfig::Proxy {
                    sub_transport: Box::new(TransportConfig::Quic {
                        bind_to: None,
                        override_host: None,
                        override_port: None,
                    }),
                    proxy_config: ProxyConfig::RemoteProxyClient {
                        proxy_url: Url2::parse(local_config.proxy_url),
                    },
                }];
            } else {
                kitsune_config.transport_pool = vec![
                    TransportConfig::Quic {
                        bind_to: None,
                        override_host: None,
                        override_port: None,
                    },
                    TransportConfig::Mem {},
                    TransportConfig::Proxy {
                        sub_transport: Box::new(TransportConfig::Quic {
                            bind_to: None,
                            override_host: None,
                            override_port: None,
                        }),
                        proxy_config: ProxyConfig::RemoteProxyClient {
                            proxy_url: Url2::parse(local_config.proxy_url),
                        },
                    },
                ];
            }
            config.network = Some(kitsune_config);

            config
        };

        info!("Starting holochain conductor with config: {:?}", config);
        let conductor = ConductorBuilder::new()
            .config(config)
            .passphrase(Some(local_config.passphrase.as_bytes().into()))
            .build()
            .await;

        if conductor.is_err() {
            panic!("Could not start holochain conductor");
        }

        info!("Started holochain conductor");

        let conductor = conductor.unwrap();

        let interface = conductor
            .clone()
            .add_app_interface(Either::Left(local_config.admin_port))
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

                let activate = self
                    .conductor
                    .clone()
                    .enable_app(app_id.clone())
                    .await
                    .map_err(|e| anyhow!("Could not activate app: {:?}", e))?;
                info!("Installed app with result: {:?}", activate);

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
        info!(
            "Calling zome function: {:?} {:?} {:?} {:?}",
            app_id, cell_name, zome_name, fn_name
        );
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

        if cell_entry.unwrap().len() == 0 {
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

        let zome_call_unsigned = ZomeCallUnsigned {
            cell_id: cell_id,
            zome_name: zome_name.into(),
            fn_name: fn_name.into(),
            payload: payload,
            cap_secret: None,
            provenance: agent_pub_key,
            nonce: generate_nonce().into(),
            expires_at: Timestamp::now()
                .checked_add_signed(&Duration::seconds(300))
                .unwrap(),
        };

        let keystore = self.conductor.keystore();
        let signed_zome_call = ZomeCall::try_from_unsigned_zome_call(keystore, zome_call_unsigned)
            .await
            .map_err(|err| anyhow!("Could not sign zome call: {:?}", err))?;

        let result = self.conductor.call_zome(signed_zome_call).await??;

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
            .uninstall_app(&app_id)
            .await
            .map_err(|e| anyhow!("Could not remove app: {:?}", e))?;

        info!("Removed app with id: {:?}", app_id);
        Ok(())
    }

    pub async fn agent_infos(&self) -> Result<Vec<AgentInfoSigned>, AnyError> {
        Ok(self.conductor.get_agent_infos(None).await?)
    }

    pub async fn add_agent_infos(&self, agent_infos: Vec<AgentInfoSigned>) -> Result<(), AnyError> {
        Ok(self.conductor.add_agent_infos(agent_infos).await?)
    }

    pub async fn sign(&self, data: String) -> Result<Signature, AnyError> {
        let keystore = self.conductor.keystore();
        let pub_keys = keystore.list_public_keys().await?;
        if pub_keys.len() == 0 {
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
        if pub_keys.len() == 0 {
            return Err(anyhow!("No public keys found"));
        }
        let agent = pub_keys.first().unwrap();
        info!("Agent key: {:?}", agent);
        Ok(agent.to_owned())
    }

    pub async fn get_app_info(&self, app_id: String) -> Result<Option<AppInfo>, AnyError> {
        Ok(self.conductor.get_app_info(&app_id).await?)
    }

    pub async fn get_network_metrics(&self) -> Result<String, AnyError> {
        Ok(self.conductor.dump_network_metrics(None).await?)
    }
}
