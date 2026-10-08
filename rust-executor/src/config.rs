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

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SmtpConfig {
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String,
    pub from_address: String,
}

/// `Debug` prints [`Ad4mConfig::redacted_json`], so a logged config never
/// carries a secret.
#[derive(Clone, Serialize, Deserialize)]
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
    /// When false, skip Holochain conductor startup entirely.
    /// Bootstrap languages must not depend on Holochain (use local bootstrap languages).
    pub run_holochain: Option<bool>,
    /// Grants every capability to whoever presents it. Required: `run`
    /// refuses to start without one (an empty string counts as none) unless
    /// `insecure_no_admin_credential` is set. See [`Ad4mConfig::check_admin_credential`].
    pub admin_credential: Option<String>,
    /// Tests and local development only: start without an admin credential.
    /// The empty token is then the operator (every capability), so anyone who
    /// can reach the listener controls the executor.
    pub insecure_no_admin_credential: Option<bool>,
    pub localhost: Option<bool>,
    pub auto_permit_cap_requests: Option<bool>,
    pub tls: Option<TlsConfig>,
    pub log_holochain_metrics: Option<bool>,
    pub enable_multi_user: Option<bool>,
    pub smtp_config: Option<SmtpConfig>,
    /// Log level per crate over [`crate::logging::get_default_log_config`];
    /// `RUST_LOG` overrides it.
    pub log_config: Option<std::collections::HashMap<String, String>>,
    /// Enable MCP (Model Context Protocol) server for AI agent integration
    pub enable_mcp: Option<bool>,
    /// Port for MCP HTTP server (default: 3001)
    pub mcp_port: Option<u16>,
    /// Expose the dynamic per-class MCP tools (`{class}_create`,
    /// `{class}_set_{property}`, `{class}_add_{collection}`, …) over the MCP
    /// transport, alongside the static `instance_*` tools. Default `false`:
    /// external MCP clients see only the fixed static tool surface, so the tool
    /// list stays constant no matter which social DNA is loaded. The in-process
    /// interpretation/flow harness is unaffected by this flag either way.
    pub dynamic_class_tools: Option<bool>,
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
        // An empty credential is no credential. Normalised here, once, so
        // every reader after prepare() (the startup check, REST/WS
        // capabilities, the MCP bind host and auth) agrees on it.
        self.admin_credential = non_empty_credential(self.admin_credential.take());

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
            self.port = Some(DEFAULT_PORT);
        }
        if self.connect_holochain.is_none() {
            self.connect_holochain = Some(false);
        }
        if self.run_holochain.is_none() {
            self.run_holochain = Some(true);
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

    /// Secure by default: an executor without an admin credential serves every
    /// caller as the operator, so it only starts that way when the testing
    /// flag says so. Called by `run()` after `prepare()`, which has already
    /// turned `Some("")` into `None`, and before any service starts, so every
    /// entry point (CLI, launcher, library) goes through it.
    pub fn check_admin_credential(&self) -> Result<(), String> {
        if self.admin_credential.is_some() || self.insecure_no_admin_credential == Some(true) {
            return Ok(());
        }
        Err(NO_ADMIN_CREDENTIAL_ERROR.to_string())
    }

    pub fn get_json(&self) -> String {
        serde_json::to_string(self).expect("Could not convert config to json")
    }

    /// The config as JSON with every secret value replaced by
    /// [`crate::config_file::REDACTED`]; an unset secret stays `null`.
    pub fn redacted_json(&self) -> serde_json::Value {
        let mut json = serde_json::to_value(self).expect("Ad4mConfig serializes");
        let redact = |value: &mut serde_json::Value| {
            if !value.is_null() {
                *value = serde_json::Value::from(crate::config_file::REDACTED);
            }
        };
        for pointer in [
            "/adminCredential",
            "/internalApiToken",
            "/smtpConfig/password",
        ] {
            if let Some(value) = json.pointer_mut(pointer) {
                redact(value);
            }
        }
        json
    }
}

impl std::fmt::Debug for Ad4mConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ad4mConfig {}", self.redacted_json())
    }
}

impl std::fmt::Debug for SmtpConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SmtpConfig")
            .field("enabled", &self.enabled)
            .field("host", &self.host)
            .field("port", &self.port)
            .field("username", &self.username)
            .field("password", &crate::config_file::REDACTED)
            .field("from_address", &self.from_address)
            .finish()
    }
}

/// `Some("")` is no credential: an empty environment variable (a compose
/// `${VAR}` with `VAR` unset) must not become a credential the empty token
/// matches.
pub fn non_empty_credential(credential: Option<String>) -> Option<String> {
    credential.filter(|credential| !credential.is_empty())
}

/// Why `run` refused to start; names both ways out.
pub const NO_ADMIN_CREDENTIAL_ERROR: &str =
    "Refusing to start: no admin credential is set, and without one every caller \
     on a loopback listener gets full admin access. Set AD4M_ADMIN_CREDENTIAL (or --admin-credential, \
     `adminCredential` in the config) to a secret, or, for tests and local \
     development only, pass --insecure-no-admin-credential \
     (AD4M_INSECURE_NO_ADMIN_CREDENTIAL=true).";

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

/// The RPC port when none is configured.
pub const DEFAULT_PORT: u16 = 12000;

impl Default for Ad4mConfig {
    fn default() -> Self {
        let mut config = Ad4mConfig::unprepared();
        config.prepare();
        config
    }
}

impl Ad4mConfig {
    /// Every field unset. [`Ad4mConfig::prepare`] (which `run` calls) fills
    /// the defaults, including those derived from other fields, such as the
    /// bootstrap seed path inside `app_data_path`.
    pub fn unprepared() -> Self {
        Ad4mConfig {
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
            run_holochain: None,
            admin_credential: None,
            insecure_no_admin_credential: None,
            localhost: None,
            auto_permit_cap_requests: None,
            tls: None,
            log_holochain_metrics: None,
            enable_multi_user: None,
            smtp_config: None,
            log_config: None,
            enable_mcp: None,
            mcp_port: None,
            dynamic_class_tools: None,
            pid_file: None,
            wallet_backend: None,
            wallet_backend_url: None,
            wallet_signing_key_name: None,
            db_backend: None,
            db_backend_url: None,
            snapshot_interval_secs: None,
            internal_api_token: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn with_credential(
        admin_credential: Option<&str>,
        insecure_no_admin_credential: Option<bool>,
    ) -> Ad4mConfig {
        Ad4mConfig {
            admin_credential: admin_credential.map(str::to_string),
            insecure_no_admin_credential,
            ..Default::default()
        }
    }

    #[test]
    fn no_admin_credential_refuses_without_the_testing_flag() {
        for flag in [None, Some(false)] {
            let err = with_credential(None, flag)
                .check_admin_credential()
                .expect_err("no credential, no flag");
            assert_eq!(err, NO_ADMIN_CREDENTIAL_ERROR);
        }
    }

    #[test]
    fn empty_admin_credential_counts_as_none() {
        let prepared = |flag| {
            let mut config = with_credential(Some(""), flag);
            config.prepare();
            config
        };
        assert!(prepared(None).check_admin_credential().is_err());
        assert!(prepared(Some(true)).check_admin_credential().is_ok());
    }

    /// `prepare()` turns an empty credential into `None`, so every reader after
    /// it (capabilities, the MCP bind host and auth) sees the same "no
    /// credential" that `check_admin_credential` refused or let through.
    #[test]
    fn prepare_turns_an_empty_admin_credential_into_none() {
        for flag in [None, Some(false), Some(true)] {
            let mut config = with_credential(Some(""), flag);
            config.prepare();
            assert_eq!(config.admin_credential, None, "flag {flag:?}");
        }
        let mut config = with_credential(Some("secret"), Some(true));
        config.prepare();
        assert_eq!(config.admin_credential.as_deref(), Some("secret"));
    }

    /// The interpretation pass reads AD4M_ADMIN_CREDENTIAL itself, through
    /// this helper, so an empty variable is no credential there too.
    #[test]
    fn non_empty_credential_drops_only_the_empty_string() {
        assert_eq!(non_empty_credential(Some(String::new())), None);
        assert_eq!(non_empty_credential(None), None);
        assert_eq!(
            non_empty_credential(Some(" ".to_string())).as_deref(),
            Some(" ")
        );
    }

    #[test]
    fn admin_credential_or_testing_flag_starts() {
        assert!(with_credential(Some("secret"), None)
            .check_admin_credential()
            .is_ok());
        assert!(with_credential(Some("secret"), Some(false))
            .check_admin_credential()
            .is_ok());
        assert!(with_credential(None, Some(true))
            .check_admin_credential()
            .is_ok());
    }

    #[test]
    fn the_refusal_names_both_options() {
        for option in [
            "AD4M_ADMIN_CREDENTIAL",
            "--admin-credential",
            "--insecure-no-admin-credential",
            "AD4M_INSECURE_NO_ADMIN_CREDENTIAL",
        ] {
            assert!(NO_ADMIN_CREDENTIAL_ERROR.contains(option), "{option}");
        }
    }

    #[test]
    fn debug_and_redacted_json_hide_secrets() {
        let config = Ad4mConfig {
            admin_credential: Some("admin-secret".into()),
            internal_api_token: Some("internal-secret".into()),
            smtp_config: Some(SmtpConfig {
                enabled: true,
                host: "smtp.example".into(),
                port: 465,
                username: "u".into(),
                password: "smtp-secret".into(),
                from_address: "f".into(),
            }),
            ..Default::default()
        };
        let debug = format!("{config:?}");
        for secret in ["admin-secret", "internal-secret", "smtp-secret"] {
            assert!(!debug.contains(secret), "{debug}");
        }
        assert!(debug.contains("smtp.example"), "{debug}");
        let json = config.redacted_json();
        assert_eq!(json["adminCredential"], "<redacted>");
        assert_eq!(json["smtpConfig"]["password"], "<redacted>");
        assert!(Ad4mConfig::default().redacted_json()["adminCredential"].is_null());
    }

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
