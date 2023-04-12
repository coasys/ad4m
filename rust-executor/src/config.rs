use crate::utils;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Ad4mConfig {
    pub app_data_path: Option<String>,
    pub resource_path: Option<String>,
    pub network_bootstrap_seed: Option<String>,
    pub language_language_only: Option<bool>,
    pub run_dapp_server: Option<bool>,
    pub gql_port: Option<u16>,
    pub hc_admin_port: Option<u16>,
    pub hc_app_port: Option<u16>,
    pub ipfs_swarm_port: Option<u16>,
    pub connect_holochain: Option<bool>,
    pub admin_credential: Option<String>,
    pub swipl_path: Option<String>,
    pub swipl_home_path: Option<String>,
}

impl Ad4mConfig {
    pub fn prepare(&mut self) {
        if self.app_data_path.is_none() {
            self.app_data_path = Some(
                utils::ad4m_data_directory()
                    .into_os_string()
                    .into_string()
                    .expect("Could not convert data path to string"),
            );
        }
        if self.resource_path.is_none() {
            let mut data_path = utils::ad4m_data_directory();
            data_path.push("binary");
            self.resource_path = Some(
                data_path
                    .into_os_string()
                    .into_string()
                    .expect("Could not convert binary path to string"),
            );
        }
        if self.network_bootstrap_seed.is_none() {
            let mut data_path = utils::ad4m_data_directory();
            data_path.push("mainnet_seed.seed");
            self.network_bootstrap_seed = Some(
                data_path
                    .into_os_string()
                    .into_string()
                    .expect("Could not convert seed path to string"),
            );
        }
        if self.language_language_only.is_none() {
            self.language_language_only = Some(false);
        }
        if self.run_dapp_server.is_none() {
            self.run_dapp_server = Some(true);
        }
        if self.gql_port.is_none() {
            self.gql_port = Some(14000);
        }
        if self.connect_holochain.is_none() {
            self.connect_holochain = Some(false);
        }
        if self.swipl_path.is_none() {
            let mut data_path = utils::ad4m_data_directory();
            //If OS is windows push swipl/
            if cfg!(windows) {
                data_path.push("swipl");
            }
            //If os is other push swipl/lib/swipl/
            else {
                data_path.push("swipl/lib/swipl");
            }
            self.swipl_path = Some(
                data_path
                    .into_os_string()
                    .into_string()
                    .expect("Could not convert swipl path to string"),
            );
        }
        if self.swipl_home_path.is_none() {
            let mut data_path = utils::ad4m_data_directory();
            data_path.push("swipl/bin/swipl");
            self.swipl_home_path = Some(
                data_path
                    .into_os_string()
                    .into_string()
                    .expect("Could not convert swipl home path to string"),
            );
        }
    }

    pub fn get_json(&self) -> String {
        serde_json::to_string(self).expect("Could not convert config to json")
    }
}

impl Default for Ad4mConfig {
    fn default() -> Self {
        let mut config = Ad4mConfig {
            app_data_path: None,
            resource_path: None,
            network_bootstrap_seed: None,
            language_language_only: None,
            run_dapp_server: None,
            gql_port: None,
            hc_admin_port: None,
            hc_app_port: None,
            ipfs_swarm_port: None,
            connect_holochain: None,
            admin_credential: None,
            swipl_path: None,
            swipl_home_path: None,
        };
        config.prepare();
        config
    }
}
