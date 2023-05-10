use std::path::PathBuf;
use std::sync::Arc;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use holochain::conductor::config::{AdminInterfaceConfig, ConductorConfig};
use holochain::conductor::interface::InterfaceDriver;
use holochain::conductor::{ConductorBuilder, ConductorHandle};
use holochain::prelude::InstallAppPayload;
use log::info;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};

pub(crate) mod holochain_service_extension;

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
}

impl HolochainService {
    pub async fn new(local_config: LocalConductorConfig) -> Result<Self, AnyError> {
        let mut config = ConductorConfig::default();
        config.environment_path = PathBuf::from(local_config.conductor_path.clone()).into();
        config.admin_interfaces = None;

        //TODO; handle using proxy/bootstrap/mdns

        info!("Starting holochain conductor with config: {:?}", config);
        let conductor = ConductorBuilder::new()
            .config(config)
            .passphrase(Some(local_config.passphrase.as_bytes().into()))
            .build()
            .await
            .map_err(|err| anyhow!("Could not build conductor: {:?}", err))?;
        let service = Self { conductor };

        let set_res = HOLOCHAIN_CONDUCTOR.set(Arc::new(service.clone()));
        if set_res.is_err() {
            panic!("Could not set global conductor");
        }

        info!("Started holochain conductor and set reference in rust executor");

        Ok(service)
    }

    pub async fn install_app(
        &self,
        install_app_payload: InstallAppPayload,
    ) -> Result<(), AnyError> {
        if install_app_payload.installed_app_id.is_none() {
            return Err(anyhow!("App id is required"));
        }

        let app_id = install_app_payload.installed_app_id.clone().unwrap();

        self.conductor
            .clone()
            .install_app_bundle(install_app_payload)
            .await
            .map_err(|e| anyhow!("Could not install app: {:?}", e))?;
        let activate = self
            .conductor
            .clone()
            .enable_app(app_id)
            .await
            .map_err(|e| anyhow!("Could not activate app: {:?}", e))?;

        info!("Installed app with result: {:?}", activate.0);
        Ok(())
    }
}

static HOLOCHAIN_CONDUCTOR: OnceCell<Arc<HolochainService>> = OnceCell::new();

pub async fn get_global_conductor() -> Arc<HolochainService> {
    HOLOCHAIN_CONDUCTOR
        .get()
        .expect("Conductor not initialized")
        .clone()
}
