use crate::utils;
use crate::wallet::{KEY_NAME_MAIN, KEY_NAME_PLATFORM};
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

lazy_static::lazy_static! {
    /// Global SMTP configuration for sending emails
    pub static ref SMTP_CONFIG: Arc<Mutex<Option<SmtpConfig>>> = Arc::new(Mutex::new(None));

    /// Global Ad4mConfig instance, set once during startup
    pub static ref GLOBAL_AD4M_CONFIG: Arc<Mutex<Option<Ad4mConfig>>> = Arc::new(Mutex::new(None));
}

/// Store the Ad4mConfig globally so services can access it without passing it through every call.
/// Recovers from a poisoned mutex so that a panic in one test does not cascade
/// into every subsequent test that touches the global config.
pub fn set_global_config(config: Ad4mConfig) {
    let mut global_config = GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner());
    *global_config = Some(config);
}

/// Get a clone of the global Ad4mConfig.
/// Recovers from a poisoned mutex (see `set_global_config` for rationale).
pub fn get_global_config() -> Ad4mConfig {
    let global_config = GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner());
    global_config
        .clone()
        .expect("GLOBAL_AD4M_CONFIG not initialized")
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
    pub port: Option<u16>,
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
    pub hc_relay_url: Option<String>,
    pub connect_holochain: Option<bool>,
    pub admin_credential: Option<String>,
    pub localhost: Option<bool>,
    pub auto_permit_cap_requests: Option<bool>,
    pub tls: Option<TlsConfig>,
    pub log_holochain_metrics: Option<bool>,
    pub enable_multi_user: Option<bool>,
    pub smtp_config: Option<SmtpConfig>,
    /// Enable MCP (Model Context Protocol) server for AI agent integration
    pub enable_mcp: Option<bool>,
    /// Port for MCP HTTP server (default: 3001)
    pub mcp_port: Option<u16>,
    /// Path to write PID file (for test harness cleanup)
    pub pid_file: Option<String>,
    /// Wallet backend type: "local" (default) or "shared".
    /// "local" keeps keys in-process (self-hosted default).
    /// "shared" delegates to an external HTTP wallet service.
    pub wallet_backend: Option<String>,
    /// Base URL for the shared wallet service (required when wallet_backend = "shared").
    pub wallet_backend_url: Option<String>,
    /// Name of the key used for JWT signing. Defaults to "main" (local) or "platform" (shared).
    pub wallet_signing_key_name: Option<String>,

    /// Database backend type: "local" (default) or "shared".
    /// "local" uses the in-process SQLite database (Ad4mDb).
    /// "shared" delegates to the platform Worker's internal DB API.
    pub db_backend: Option<String>,
    /// Base URL for the shared DB service (required when db_backend = "shared").
    pub db_backend_url: Option<String>,

    /// Interval in seconds between perspective snapshots (default 300 = 5 min).
    /// Set to 0 to disable periodic snapshots. Only applies in shared mode.
    pub snapshot_interval_secs: Option<u64>,

    /// Bearer token for internal API authentication (outbound: executor → platform Worker).
    /// MUST differ from `admin_credential` (inbound: client → executor) to maintain
    /// trust boundary separation. See the assertion in lib.rs::run().
    pub internal_api_token: Option<String>,
}

impl Ad4mConfig {
    /// Resolve the wallet signing key name from config, falling back to
    /// "main" for local mode or "platform" for shared mode.
    pub fn signing_key_name(&self) -> String {
        if let Some(name) = &self.wallet_signing_key_name {
            return name.clone();
        }
        match self.wallet_backend.as_deref() {
            Some("shared") => KEY_NAME_PLATFORM.to_string(),
            _ => KEY_NAME_MAIN.to_string(),
        }
    }

    pub fn prepare(&mut self) {
        // Read shared-backend config from environment variables when not set
        // programmatically. This allows Docker containers to configure the
        // executor via standard `environment:` directives without CLI flags.
        if self.wallet_backend.is_none() {
            self.wallet_backend = std::env::var("WALLET_BACKEND").ok();
        }
        if self.wallet_backend_url.is_none() {
            self.wallet_backend_url = std::env::var("WALLET_BACKEND_URL").ok();
        }
        if self.wallet_signing_key_name.is_none() {
            self.wallet_signing_key_name = std::env::var("WALLET_SIGNING_KEY_NAME").ok();
        }
        if self.db_backend.is_none() {
            self.db_backend = std::env::var("DB_BACKEND").ok();
        }
        if self.db_backend_url.is_none() {
            self.db_backend_url = std::env::var("DB_BACKEND_URL").ok();
        }
        if self.snapshot_interval_secs.is_none() {
            self.snapshot_interval_secs = std::env::var("SNAPSHOT_INTERVAL_SECS")
                .ok()
                .and_then(|v| v.parse().ok());
        }
        if self.internal_api_token.is_none() {
            self.internal_api_token = std::env::var("INTERNAL_API_TOKEN").ok();
        }

        // Validate shared-backend URLs use HTTPS (or approved local addresses)
        if self.wallet_backend.as_deref() == Some("shared") {
            if let Some(ref url) = self.wallet_backend_url {
                if let Err(msg) = validate_shared_backend_url(url, "WALLET_BACKEND_URL") {
                    log::warn!("{}", msg);
                }
            }
        }
        if self.db_backend.as_deref() == Some("shared") {
            if let Some(ref url) = self.db_backend_url {
                if let Err(msg) = validate_shared_backend_url(url, "DB_BACKEND_URL") {
                    log::warn!("{}", msg);
                }
            }
        }

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
        if self.port.is_none() {
            self.port = Some(12000);
        }
        if self.connect_holochain.is_none() {
            self.connect_holochain = Some(false);
        }
        if self.hc_proxy_url.is_none() {
            self.hc_proxy_url = Some("ws://bootstrap.ad4m.dev:4433".to_string());
        }
        if self.hc_bootstrap_url.is_none() {
            self.hc_bootstrap_url = Some("http://bootstrap.ad4m.dev:4433".to_string());
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

/// Validate that a shared-backend URL uses HTTPS for production security.
/// Local development addresses (localhost, 127.0.0.1, [::1], host.docker.internal)
/// are exempt — they run inside Docker networks or on loopback.
///
/// Returns Ok(()) for valid URLs, Err(message) for invalid ones.
pub fn validate_shared_backend_url(url: &str, label: &str) -> Result<(), String> {
    // Parse the URL to extract scheme and host
    let lower = url.to_lowercase();

    // HTTPS always OK
    if lower.starts_with("https://") {
        return Ok(());
    }

    // HTTP only allowed for local/Docker addresses
    if lower.starts_with("http://") {
        let host_part = &lower["http://".len()..];
        // Strip path, query, fragment to get host:port
        let host_and_port = host_part.split('/').next().unwrap_or(host_part);
        // IPv6 addresses use [addr]:port — extract the bracketed address intact
        let host = if host_and_port.starts_with('[') {
            // Take everything up to and including ']'
            host_and_port
                .split(']')
                .next()
                .map(|s| &host_and_port[..s.len() + 1])
                .unwrap_or(host_and_port)
        } else {
            host_and_port.split(':').next().unwrap_or(host_and_port)
        };

        let allowed_hosts = [
            "localhost",
            "127.0.0.1",
            "[::1]",
            "::1",
            "host.docker.internal",
        ];

        if allowed_hosts.contains(&host) {
            return Ok(());
        }

        // Also allow any *.internal or *.local hostname (Docker service names)
        if host.ends_with(".internal") || host.ends_with(".local") {
            return Ok(());
        }

        return Err(format!(
            "{} URL must use HTTPS for non-local hosts (got: {}). \
             HTTP is only allowed for localhost, 127.0.0.1, [::1], \
             host.docker.internal, and *.internal/*.local hostnames.",
            label, url
        ));
    }

    Err(format!(
        "{} URL must start with http:// or https:// (got: {})",
        label, url
    ))
}

impl Default for Ad4mConfig {
    fn default() -> Self {
        let mut config = Ad4mConfig {
            app_data_path: None,
            network_bootstrap_seed: None,
            language_language_only: None,
            run_dapp_server: None,
            port: None,
            hc_admin_port: None,
            hc_app_port: None,
            hc_use_local_proxy: None,
            hc_use_mdns: None,
            hc_use_proxy: None,
            hc_use_bootstrap: None,
            hc_proxy_url: None,
            hc_bootstrap_url: None,
            hc_relay_url: None,
            connect_holochain: None,
            admin_credential: None,
            localhost: None,
            auto_permit_cap_requests: None,
            tls: None,
            log_holochain_metrics: None,
            enable_multi_user: None,
            smtp_config: None,
            enable_mcp: None,
            mcp_port: None,
            pid_file: None,
            wallet_backend: None,
            wallet_backend_url: None,
            wallet_signing_key_name: None,
            db_backend: None,
            db_backend_url: None,
            snapshot_interval_secs: None,
            internal_api_token: None,
        };
        config.prepare();
        config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_https_url() {
        assert!(
            validate_shared_backend_url("https://api.coasys.org/internal/wallet", "TEST").is_ok()
        );
    }

    #[test]
    fn test_validate_http_localhost() {
        assert!(
            validate_shared_backend_url("http://localhost:8787/internal/wallet", "TEST").is_ok()
        );
        assert!(validate_shared_backend_url("http://127.0.0.1:8787/internal/db", "TEST").is_ok());
        assert!(validate_shared_backend_url("http://[::1]:8787/internal/db", "TEST").is_ok());
    }

    #[test]
    fn test_validate_http_docker_internal() {
        assert!(validate_shared_backend_url(
            "http://host.docker.internal:8787/internal/wallet",
            "TEST"
        )
        .is_ok());
    }

    #[test]
    fn test_validate_http_internal_suffix() {
        assert!(validate_shared_backend_url("http://worker.internal:8787/api", "TEST").is_ok());
    }

    #[test]
    fn test_validate_http_local_suffix() {
        assert!(validate_shared_backend_url("http://executor.local:12000/api", "TEST").is_ok());
    }

    #[test]
    fn test_reject_http_remote() {
        let result = validate_shared_backend_url("http://api.coasys.org/internal/wallet", "TEST");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("HTTPS"));
    }

    #[test]
    fn test_reject_no_scheme() {
        let result = validate_shared_backend_url("api.coasys.org/internal/wallet", "TEST");
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_case_insensitive() {
        assert!(validate_shared_backend_url("HTTP://LOCALHOST:8787/path", "TEST").is_ok());
        assert!(validate_shared_backend_url("HTTPS://API.COASYS.ORG/path", "TEST").is_ok());
    }
}
