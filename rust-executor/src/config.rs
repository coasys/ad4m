use crate::utils;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

lazy_static::lazy_static! {
    /// Global SMTP configuration for sending emails
    pub static ref SMTP_CONFIG: Arc<Mutex<Option<SmtpConfig>>> = Arc::new(Mutex::new(None));
}

/// Set the global SMTP config (called during server initialization)
pub fn set_smtp_config(config: Option<SmtpConfig>) -> Result<(), AnyError> {
    let mut smtp_config = SMTP_CONFIG.lock().map_err(|e| {
        AnyError::from(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to acquire SMTP config mutex lock: {}", e),
        ))
    })?;
    *smtp_config = config;
    Ok(())
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TlsConfig {
    pub cert_file_path: String,
    pub key_file_path: String,
    pub tls_port: u16, // Port for the HTTPS/WSS server
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SmtpConfig {
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String,
    pub from_address: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Ad4mConfig {
    pub app_data_path: Option<String>,
    pub network_bootstrap_seed: Option<String>,
    pub language_language_only: Option<bool>,
    pub run_dapp_server: Option<bool>,
    pub gql_port: Option<u16>,
    #[serde(rename = "hcPortAdmin")]
    pub hc_admin_port: Option<u16>,
    #[serde(rename = "hcPortApp")]
    pub hc_app_port: Option<u16>,
    pub hc_use_local_proxy: Option<bool>,
    pub hc_use_mdns: Option<bool>,
    pub hc_use_proxy: Option<bool>,
    pub hc_use_bootstrap: Option<bool>,
    pub hc_proxy_url: Option<String>,
    pub hc_bootstrap_url: Option<String>,
    pub connect_holochain: Option<bool>,
    pub admin_credential: Option<String>,
    pub localhost: Option<bool>,
    pub auto_permit_cap_requests: Option<bool>,
    pub tls: Option<TlsConfig>,
    pub log_holochain_metrics: Option<bool>,
    pub enable_multi_user: Option<bool>,
    pub smtp_config: Option<SmtpConfig>,
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
        if self.network_bootstrap_seed.is_none() {
            let mut data_path = PathBuf::from(self.app_data_path.clone().unwrap());
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
            self.gql_port = Some(12000);
        }
        if self.connect_holochain.is_none() {
            self.connect_holochain = Some(false);
        }
        if self.hc_proxy_url.is_none() {
            self.hc_proxy_url = Some("ws://relay.ad4m.dev:4433".to_string());
        }
        if self.hc_bootstrap_url.is_none() {
            self.hc_bootstrap_url = Some("http://relay.ad4m.dev:4433".to_string());
        }
        if self.hc_use_bootstrap.is_none() {
            self.hc_use_bootstrap = Some(true);
        }
        if self.hc_use_mdns.is_none() {
            self.hc_use_mdns = Some(false);
        }
        if self.hc_use_proxy.is_none() {
            self.hc_use_proxy = Some(true)
        }
        if self.localhost.is_none() {
            self.localhost = Some(true);
        }
        if self.log_holochain_metrics.is_none() {
            self.log_holochain_metrics = Some(true);
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
            network_bootstrap_seed: None,
            language_language_only: None,
            run_dapp_server: None,
            gql_port: None,
            hc_admin_port: None,
            hc_app_port: None,
            hc_use_local_proxy: None,
            hc_use_mdns: None,
            hc_use_proxy: None,
            hc_use_bootstrap: None,
            hc_proxy_url: None,
            hc_bootstrap_url: None,
            connect_holochain: None,
            admin_credential: None,
            localhost: None,
            auto_permit_cap_requests: None,
            tls: None,
            log_holochain_metrics: None,
            enable_multi_user: None,
            smtp_config: None,
        };
        config.prepare();
        config
    }
}
