use std::path::PathBuf;
use std::sync::Arc;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use holochain::conductor::api::{CellInfo, ZomeCall};
use holochain::conductor::config::ConductorConfig;
use holochain::conductor::{ConductorBuilder, ConductorHandle};
use holochain::prelude::agent_store::AgentInfoSigned;
use holochain::prelude::{
    ExternIO, InstallAppPayload, Signature, Timestamp, ZomeCallResponse, ZomeCallUnsigned,
};
use log::info;
use once_cell::sync::OnceCell;
use rand::Rng;
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

    pub async fn call_zome_function(
        &self,
        app_id: String,
        cell_name: String,
        zome_name: String,
        fn_name: String,
        payload: serde_json::Value,
    ) -> Result<ZomeCallResponse, AnyError> {
        let app_info = self.conductor.get_app_info(&app_id).await?;

        if app_info.is_none() {
            return Err(anyhow!("App not installed with id: {:?}", app_id));
        }

        let app_info = app_info.unwrap();

        let cell_entry = app_info.cell_info.get(&cell_name);

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

        let mut rng = rand::thread_rng();
        let random_bytes: [u8; 32] = rng.gen();

        let zome_call_unsigned = ZomeCallUnsigned {
            cell_id: cell_id,
            zome_name: zome_name.into(),
            fn_name: fn_name.into(),
            payload: ExternIO::from(serde_json::to_vec(&payload).unwrap()),
            cap_secret: None,
            provenance: agent_pub_key,
            nonce: random_bytes.into(),
            expires_at: Timestamp::from_micros(300000000),
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
}

static HOLOCHAIN_CONDUCTOR: OnceCell<Arc<HolochainService>> = OnceCell::new();

pub async fn get_global_conductor() -> Arc<HolochainService> {
    HOLOCHAIN_CONDUCTOR
        .get()
        .expect("Conductor not initialized")
        .clone()
}
