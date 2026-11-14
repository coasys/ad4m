use crate::secret::Secret;
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

/// Whether the executor permits capability requests without the operator (development
/// only). `false` without a global config. Never panics.
pub fn auto_permit_cap_requests() -> bool {
    GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .and_then(|config| config.auto_permit_cap_requests)
        .unwrap_or(false)
}

/// Whether this executor serves the custody channel (`--custody-channel`). `false` without a
/// global config, so a check on it fails closed. Never panics.
pub fn custody_channel_on() -> bool {
    GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .and_then(|config| config.custody_channel)
        .unwrap_or(false)
}

/// Tests: the global config with `--custody-channel` on, as a desktop app's executor runs, until
/// dropped; the config before it comes back then.
#[cfg(test)]
pub(crate) struct CustodyChannelForTest(Option<Ad4mConfig>);

#[cfg(test)]
impl CustodyChannelForTest {
    pub(crate) fn on() -> Self {
        let mut global = GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner());
        let saved = global.clone();
        let mut config = saved.clone().unwrap_or_default();
        config.custody_channel = Some(true);
        *global = Some(config);
        CustodyChannelForTest(saved)
    }
}

#[cfg(test)]
impl Drop for CustodyChannelForTest {
    fn drop(&mut self) {
        *GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner()) = self.0.take();
    }
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
    /// Serialises as `"<redacted>"`: no serialised config carries it.
    #[serde(serialize_with = "crate::secret::serialize_redacted")]
    pub admin_credential: Option<Secret>,
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
    /// `{class}_set_{property}`, `{class}_add_{collection}`, …) alongside the
    /// static `instance_*` tools: over the MCP transport and in the tool list
    /// an AI session's model is offered. Default `false`: agents see only the
    /// fixed static tool surface, so the tool list stays constant no matter
    /// which social DNA is loaded. An interpretation pass sets its own flag
    /// and is unaffected by this one either way.
    pub dynamic_class_tools: Option<bool>,
    /// Path to write PID file (for test harness cleanup)
    pub pid_file: Option<String>,
    /// Stop gracefully (exit 0) when stdin reaches its end: a supervisor holds the pipe open,
    /// so the executor never outlives it (`--watch-stdin`).
    pub watch_stdin: Option<bool>,
    /// This executor serves the custody channel: the desktop app's custody service takes a
    /// master key that arrived sealed to this computer from it, with the custody credential on
    /// a loopback listener (`--custody-channel`). Never together with multi-user mode
    /// ([`refuse_custody_channel_with_multi_user`]), never without a custody credential
    /// ([`refuse_custody_channel_without_its_credential`]).
    pub custody_channel: Option<bool>,
    /// The custody channel's own credential: the first line of stdin at start
    /// ([`read_custody_credential`]), never the environment or a flag, which other processes
    /// can read. Not a config file key. The operator on this machine, like the admin credential,
    /// and the only caller the custody channel admits: the admin credential, which an app's
    /// window may hold, never reaches it. Must differ from the admin credential. Serialises as
    /// `"<redacted>"`.
    #[serde(serialize_with = "crate::secret::serialize_redacted")]
    pub custody_credential: Option<Secret>,
    /// The folder of the executor's own rotated log files (`--log-dir`). Unset: stdout.
    pub log_dir: Option<String>,
    /// The config file the executor started with (`--config`): the file `runtime.setConfig`
    /// writes. Unset: no file, and `runtime.setConfig` answers 409.
    pub config_file: Option<PathBuf>,
    /// The config file's keys that a flag or an environment variable overrides at this start:
    /// a write to the file would not take effect while the override stays.
    #[serde(default)]
    pub pinned_config_keys: Vec<String>,
    /// What the flags and environment variables of this start lay over the config file: the
    /// value of each pinned key. `runtime.setConfig` checks a written file with them, as the
    /// next start reads it. The file's shape, so no secret.
    #[serde(default)]
    pub config_overrides: serde_json::Map<String, serde_json::Value>,
    /// Shut down once the process that started the executor is gone (unix). For launchers and
    /// test harnesses that may die without stopping it. Default `false`: a daemonised executor
    /// (`nohup`, `&` from a shell that then exits) keeps running.
    pub exit_with_parent: Option<bool>,
    /// Local DID for this SFU node in a cascade cluster.  Required if
    /// any cascade options are set; otherwise the SFU runs single-node.
    pub sfu_local_did: Option<String>,
    /// Capacity hint published by this SFU node in every cascade
    /// announce.  Defaults to `8` if cascade is configured without
    /// an explicit value.
    pub sfu_max_participants_per_node: Option<u32>,
    /// TCP address where this node binds its cascade gossip listener,
    /// formatted as `host:port`.  When unset, cascade is disabled.
    pub sfu_cascade_listen: Option<String>,
    /// Peer SFU nodes in the cascade cluster, each `did=host:port`.
    /// Empty list = standalone SFU.
    pub sfu_cascade_peers: Option<Vec<String>>,
    /// IP address the SFU media server binds to (e.g. `203.0.113.5`).
    /// str0m rejects `0.0.0.0` as a candidate, so this must be a real
    /// interface IP.  When unset, the SFU follows `localhost`: loopback
    /// by default, the machine's outbound IP with `localhost: false`.
    pub sfu_bind_addr: Option<String>,
    /// STUN server (`host:port`) the SFU asks once at startup, from its own
    /// socket, for its public address. Unset: no probe, and reachability is
    /// "unknown" unless the bind address is private. A loopback bind is
    /// never probed.
    pub sfu_stun_server: Option<String>,
    /// UDP port the SFU binds. Unset (or 0): a free port, chosen at each
    /// start. Fixed, a router can forward it; startup fails when it is taken.
    pub sfu_udp_port: Option<u16>,
    /// Public IP the SFU offers callers as a server-reflexive candidate,
    /// with the bound port. Unset: the STUN probe's reflexive IP, when the
    /// bind address is private.
    pub sfu_public_addr: Option<String>,
    /// DID of the cascade node that takes redirected joins first. Unset:
    /// the least-loaded node.
    pub sfu_preferred_node: Option<String>,
    /// `identity.storeUrl`: the identity store's base URL, given to the agent language as its
    /// `storeUrl` setting. Unset: the network seed's `identityStore`, else no store.
    pub identity_store_url: Option<String>,
    /// `identity.storeKeys`: the store keys (Ed25519 multikeys) a head from the store must be
    /// signed by. A list, so the store's key can rotate. Unset: the seed's store keys when
    /// `identity_store_url` is unset too, else none (`IdentitySettings::from_config`).
    pub identity_store_keys: Option<Vec<String>>,
    /// `identity.allowPrivateSrc`: let the `src` of a DID URL that `identity.resolve` reads
    /// reach loopback and private addresses. Off by default; for tests and local development.
    pub identity_allow_private_src: Option<bool>,
    /// `public_origins`: the origins clients reach this executor at. A key statement's
    /// `audience` must be one of them; unset, one of this executor's own loopback origins —
    /// and on a listener the network reaches (`listens_on_network`: TLS, or `localhost`
    /// off) none at all, so key login is refused.
    pub public_origins: Option<Vec<String>>,
    /// The config file's `ai_endpoints`, `ai_models` and `ai_defaults`, applied at
    /// startup (`ai_service::endpoints::config::apply`).
    pub ai_settings: Option<crate::ai_service::endpoints::config::AiSettings>,
    /// The per-user store of local AI runtimes and models. Unset: `AD4M_LOCAL_AI_DIR`, else
    /// `<data local dir>/ad4m/local-ai` (`~/.local/share/ad4m/local-ai` on Linux).
    pub local_ai_dir: Option<String>,
    /// Start the installed local AI runtimes with the executor. Unset: yes. It never
    /// installs or downloads anything: only `ai.localInstall`, `ad4m-executor local-ai
    /// install` and the Docker image's `INCLUDE_MODELS` do.
    pub local_ai_autostart: Option<bool>,
    /// The device local AI plans for. Unset: the best one detected.
    pub local_ai_device: Option<crate::ai_service::local::hardware::Backend>,
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
    /// Serialises as `"<redacted>"`, like `admin_credential`.
    #[serde(serialize_with = "crate::secret::serialize_redacted")]
    pub internal_api_token: Option<Secret>,

    /// Seconds one request-driven SPARQL evaluation may run before it is
    /// stopped (default 30). Covers `perspective.querySparql`,
    /// `perspective.modelQuery`, live-query opening and their re-checks.
    pub query_deadline_secs: Option<u64>,
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

    /// Whether the API answers on a listener the network reaches: the TLS listener
    /// binds 0.0.0.0, and the cleartext one does too when `localhost` is off without
    /// TLS (`api::bind_api_listeners`). With no `public_origins` such a listener
    /// accepts no key statement (`key_login::Audiences::None`).
    pub fn listens_on_network(&self) -> bool {
        self.tls.is_some() || self.localhost == Some(false)
    }

    /// The data folder: `app_data_path`, else the default `prepare` fills in.
    pub fn data_folder(&self) -> PathBuf {
        self.app_data_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(utils::ad4m_data_directory)
    }

    pub fn prepare(&mut self) {
        // An empty credential is no credential: an empty environment variable
        // (a compose `${VAR}` with `VAR` unset) must not become a credential
        // the empty token matches. Normalised here, once, so every reader
        // after prepare() (the startup check, REST/WS capabilities, the MCP
        // bind host and auth) agrees on it.
        self.admin_credential = self.admin_credential.take().filter(|c| !c.is_empty());
        self.custody_credential = self.custody_credential.take().filter(|c| !c.is_empty());

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
            self.internal_api_token = Secret::non_empty(std::env::var("INTERNAL_API_TOKEN").ok());
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
                self.data_folder()
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
    ///
    /// Multi-user mode never starts without a credential, testing flag or not: a
    /// caller with no token on a loopback listener would be the operator, so any
    /// user on the host (or a user who drops their token) could act as every user.
    pub fn check_admin_credential(&self) -> Result<(), String> {
        if self.admin_credential.is_some() {
            return Ok(());
        }
        if self.enable_multi_user == Some(true) {
            return Err(MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR.to_string());
        }
        if self.insecure_no_admin_credential == Some(true) {
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
            "/custodyCredential",
            "/internalApiToken",
            "/smtpConfig/password",
        ] {
            if let Some(value) = json.pointer_mut(pointer) {
                redact(value);
            }
        }
        // An endpoint URL's user name and password, for a config built in code (a file
        // with them is refused at parse).
        if let Some(endpoints) = json
            .pointer_mut("/aiSettings/endpoints")
            .and_then(|e| e.as_array_mut())
        {
            for url in endpoints.iter_mut().filter_map(|e| e.get_mut("base_url")) {
                let Some(mut parsed) = url.as_str().and_then(|u| url::Url::parse(u).ok()) else {
                    continue;
                };
                if crate::ai_service::endpoints::has_credentials(&parsed) {
                    let _ = parsed.set_username("");
                    let _ = parsed.set_password(None);
                    let marker = format!("://{}@", crate::config_file::REDACTED);
                    *url = parsed.as_str().replacen("://", &marker, 1).into();
                }
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

/// The second startup gate, after [`Ad4mConfig::check_admin_credential`]: that one refuses
/// `--enable-multi-user` without a credential, but the mode may also be on in the
/// executor's database (`user.setMultiUserEnabled` on an earlier run). `run()` calls this
/// once the database is open, with `multi_user_on` read from it.
pub fn refuse_multi_user_from_db(
    admin_credential: &Option<Secret>,
    multi_user_on: bool,
) -> Result<(), String> {
    if admin_credential.is_none() && multi_user_on {
        return Err(format!(
            "Refusing to start: multi-user mode is on in this executor's database. {}",
            MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR
        ));
    }
    Ok(())
}

/// The custody channel and multi-user mode never run together: the channel hands a master key
/// to whoever holds the admin credential, and on a multi-user executor that is a host, not the
/// person whose key it is. The one place of this rule: the CLI (exit 78), `run` (with the mode
/// read from the database) and `user.setMultiUserEnabled` all call it.
pub fn refuse_custody_channel_with_multi_user(
    custody_channel: bool,
    multi_user_on: bool,
) -> Result<(), String> {
    if custody_channel && multi_user_on {
        return Err(CUSTODY_CHANNEL_WITH_MULTI_USER_ERROR.to_string());
    }
    Ok(())
}

/// Who a managed agent of this executor is for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UserKind {
    /// A person with an account (`user.create`, `user.signup`).
    Person,
    /// A bot that acts for a person under the person's grant (`bot.createAgent`).
    Bot,
}

/// What [`may_add_managed_user`] answers for a person without multi-user mode.
pub const PERSON_WITHOUT_MULTI_USER_ERROR: &str = "Multi-user mode is not enabled";

/// Whether this executor may add a managed agent of `kind`. A person's account needs
/// multi-user mode, which the custody channel refuses (a host would hold the person's master
/// key). A bot is no account: it acts for its owner, whose master key never reaches it, under
/// a grant its owner sets and the owner's key log bounds. So a desktop with the custody channel
/// runs bots, and an executor without multi-user mode does too.
pub fn may_add_managed_user(
    kind: UserKind,
    custody_channel: bool,
    multi_user_on: bool,
) -> Result<(), String> {
    match kind {
        UserKind::Bot => Ok(()),
        UserKind::Person if custody_channel => {
            Err(CUSTODY_CHANNEL_WITH_MULTI_USER_ERROR.to_string())
        }
        UserKind::Person if !multi_user_on => Err(PERSON_WITHOUT_MULTI_USER_ERROR.to_string()),
        UserKind::Person => Ok(()),
    }
}

/// The custody channel opens only with its own credential, and that credential is not the
/// admin credential: whoever holds the admin credential (an app's window among them) must not
/// take a master key. The CLI (exit 78) and `run` call it.
pub fn refuse_custody_channel_without_its_credential(
    custody_channel: bool,
    custody_credential: &Option<Secret>,
    admin_credential: &Option<Secret>,
) -> Result<(), String> {
    if !custody_channel {
        return Ok(());
    }
    let Some(custody) = custody_credential.as_ref().filter(|c| !c.is_empty()) else {
        return Err(CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR.to_string());
    };
    if admin_credential
        .as_ref()
        .is_some_and(|admin| admin.expose_for_compare(custody.expose()))
    {
        return Err(CUSTODY_CREDENTIAL_IS_ADMIN_CREDENTIAL_ERROR.to_string());
    }
    Ok(())
}

/// Reads the custody credential of `--custody-channel` from `input`, the executor's stdin: its
/// first line, without the line ending (`\n` or `\r\n`). Only that line is consumed; a
/// buffered `input` keeps what follows for the next reader (`--watch-stdin`'s watcher).
///
/// Stdin, because every other way in is readable by someone else: a process's environment and
/// command line are files (`/proc/<pid>/environ`, `/proc/<pid>/cmdline`) other processes of the
/// user can read, and the admin credential, which an app's window holds, reads host files. The
/// pipe's contents are gone once read.
///
/// Refused (the CLI exits 78): `input` ends before a line ends, an empty or whitespace-only
/// line, a line that is not UTF-8, and the admin credential's value. The value is never in the
/// error. The read buffer is wiped.
pub fn read_custody_credential(
    input: &mut impl std::io::BufRead,
    admin_credential: &Option<Secret>,
) -> Result<Secret, String> {
    let mut line = zeroize::Zeroizing::new(Vec::new());
    input.read_until(b'\n', &mut line).map_err(|e| {
        format!(
            "{CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR} (stdin: {})",
            e.kind()
        )
    })?;
    let Some(value) = line.strip_suffix(b"\n") else {
        return Err(CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR.to_string());
    };
    let value = value.strip_suffix(b"\r").unwrap_or(value);
    let Ok(value) = std::str::from_utf8(value) else {
        return Err(CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR.to_string());
    };
    if value.trim().is_empty() {
        return Err(CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR.to_string());
    }
    if admin_credential
        .as_ref()
        .is_some_and(|admin| admin.expose_for_compare(value))
    {
        return Err(CUSTODY_CREDENTIAL_IS_ADMIN_CREDENTIAL_ERROR.to_string());
    }
    Ok(Secret::new(value))
}

/// Why an executor refuses the custody channel without a custody credential.
pub const CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR: &str =
    "Refusing the custody channel without its credential: --custody-channel needs the custody \
     credential, a secret only the custody service holds, as the first line of stdin (the \
     value, then a newline). Write it there, or start without --custody-channel.";

/// Why an executor refuses a custody credential equal to the admin credential.
pub const CUSTODY_CREDENTIAL_IS_ADMIN_CREDENTIAL_ERROR: &str =
    "Refusing the custody channel: the custody credential on stdin is the admin credential. \
     The custody channel needs a credential of its own, which only the custody service holds.";

/// Why an executor refuses to start with a custody credential in its environment.
pub fn custody_credential_in_environment_error(variable: &str) -> String {
    format!(
        "Refusing to start: {variable} is set. The custody credential is never read from the \
         environment, which other processes can read (/proc/<pid>/environ); the custody \
         service writes it as the first line of stdin with --custody-channel. Unset {variable}."
    )
}

/// The environment variables that once carried the custody credential: set, they stop a start.
pub const CUSTODY_CREDENTIAL_VARIABLES: [&str; 2] =
    ["AD4M_CUSTODY_CREDENTIAL", "AD4M_CUSTODY_CREDENTIAL_FILE"];

/// Whether `token` is the custody credential of an executor that serves the custody channel.
/// `false` without a global config, without the channel or without the credential, so a check
/// on it fails closed. Never panics.
pub fn is_custody_credential(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .filter(|config| config.custody_channel == Some(true))
        .and_then(|config| config.custody_credential.as_ref())
        .is_some_and(|custody| !custody.is_empty() && custody.expose_for_compare(token))
}

/// Why an executor refuses the custody channel with multi-user mode.
pub const CUSTODY_CHANNEL_WITH_MULTI_USER_ERROR: &str =
    "Refusing the custody channel with multi-user mode: the channel gives a master key to the \
     holder of the admin credential, which on a multi-user executor is not the key's owner. \
     Start without --custody-channel, or turn multi-user mode off.";

/// Why `run` refused to start; names both ways out.
pub const NO_ADMIN_CREDENTIAL_ERROR: &str =
    "Refusing to start: no admin credential is set, and without one every caller \
     on a loopback listener gets full admin access. Set AD4M_ADMIN_CREDENTIAL (or --admin-credential, \
     `adminCredential` in the config) to a secret, or, for tests and local \
     development only, pass --insecure-no-admin-credential \
     (AD4M_INSECURE_NO_ADMIN_CREDENTIAL=true).";

/// Why an executor without an admin credential refuses multi-user mode, at startup
/// (`--enable-multi-user`, or the mode left on in its database) and from
/// `user.setMultiUserEnabled`.
pub const MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR: &str =
    "Multi-user mode needs an admin credential: without one, a caller with no token on a \
     loopback listener is the operator and could act as every user. Set \
     AD4M_ADMIN_CREDENTIAL (or --admin-credential, `adminCredential` in the config); \
     --insecure-no-admin-credential does not allow multi-user mode.";

/// Whether the running executor has an admin credential. `false` without a global config
/// (it is set before any service starts), so a check on it fails closed.
pub fn has_admin_credential() -> bool {
    GLOBAL_AD4M_CONFIG
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .as_ref()
        .is_some_and(|config| config.admin_credential.is_some())
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
            query_deadline_secs: None,
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
            watch_stdin: None,
            custody_channel: None,
            custody_credential: None,
            log_dir: None,
            config_file: None,
            pinned_config_keys: Vec::new(),
            config_overrides: serde_json::Map::new(),
            exit_with_parent: None,
            sfu_local_did: None,
            sfu_max_participants_per_node: None,
            sfu_cascade_listen: None,
            sfu_cascade_peers: None,
            sfu_bind_addr: None,
            sfu_stun_server: None,
            sfu_udp_port: None,
            sfu_public_addr: None,
            sfu_preferred_node: None,
            identity_store_url: None,
            identity_store_keys: None,
            identity_allow_private_src: None,
            public_origins: None,
            ai_settings: None,
            local_ai_dir: None,
            local_ai_autostart: None,
            local_ai_device: None,
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
            admin_credential: admin_credential.map(Secret::from),
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
            assert!(config.admin_credential.is_none(), "flag {flag:?}");
        }
        let mut config = with_credential(Some("secret"), Some(true));
        config.prepare();
        assert!(config
            .admin_credential
            .unwrap()
            .expose_for_compare("secret"));
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

    /// Multi-user mode without an admin credential would make a tokenless loopback caller
    /// the operator of every user: refused, with the testing flag too.
    #[test]
    fn multi_user_without_an_admin_credential_refuses_to_start() {
        for flag in [None, Some(false), Some(true)] {
            let config = Ad4mConfig {
                enable_multi_user: Some(true),
                ..with_credential(None, flag)
            };
            assert_eq!(
                config.check_admin_credential().unwrap_err(),
                MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR,
                "flag {flag:?}"
            );
        }
        let credentialed = Ad4mConfig {
            enable_multi_user: Some(true),
            ..with_credential(Some("secret"), None)
        };
        assert!(credentialed.check_admin_credential().is_ok());
        let single_user = Ad4mConfig {
            enable_multi_user: Some(false),
            ..with_credential(None, Some(true))
        };
        assert!(single_user.check_admin_credential().is_ok());
        assert!(MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR.contains("--admin-credential"));
    }

    /// The custody channel refuses multi-user mode, whichever came first; each alone passes.
    #[test]
    fn the_custody_channel_refuses_multi_user_mode() {
        assert_eq!(
            refuse_custody_channel_with_multi_user(true, true).unwrap_err(),
            CUSTODY_CHANNEL_WITH_MULTI_USER_ERROR
        );
        assert!(refuse_custody_channel_with_multi_user(true, false).is_ok());
        assert!(refuse_custody_channel_with_multi_user(false, true).is_ok());
        assert!(refuse_custody_channel_with_multi_user(false, false).is_ok());
    }

    /// A bot agent is added beside the custody channel and without multi-user mode; a
    /// person's account needs multi-user mode and never runs beside the channel.
    #[test]
    fn a_bot_is_a_managed_agent_beside_the_custody_channel_and_a_person_is_not() {
        for custody in [true, false] {
            for multi_user in [true, false] {
                assert!(may_add_managed_user(UserKind::Bot, custody, multi_user).is_ok());
            }
        }
        assert_eq!(
            may_add_managed_user(UserKind::Person, true, false).unwrap_err(),
            CUSTODY_CHANNEL_WITH_MULTI_USER_ERROR
        );
        assert_eq!(
            may_add_managed_user(UserKind::Person, false, false).unwrap_err(),
            PERSON_WITHOUT_MULTI_USER_ERROR
        );
        assert!(may_add_managed_user(UserKind::Person, false, true).is_ok());
    }

    /// The custody channel needs its own credential, not the admin credential; without the
    /// channel neither matters.
    #[test]
    fn the_custody_channel_needs_its_own_credential() {
        let secret = |s: &str| Some(Secret::from(s));
        assert_eq!(
            refuse_custody_channel_without_its_credential(true, &None, &secret("admin"))
                .unwrap_err(),
            CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR
        );
        assert_eq!(
            refuse_custody_channel_without_its_credential(true, &secret(""), &secret("admin"))
                .unwrap_err(),
            CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR
        );
        assert_eq!(
            refuse_custody_channel_without_its_credential(true, &secret("admin"), &secret("admin"))
                .unwrap_err(),
            CUSTODY_CREDENTIAL_IS_ADMIN_CREDENTIAL_ERROR
        );
        assert!(refuse_custody_channel_without_its_credential(
            true,
            &secret("custody"),
            &secret("admin")
        )
        .is_ok());
        assert!(
            refuse_custody_channel_without_its_credential(true, &secret("custody"), &None).is_ok()
        );
        assert!(refuse_custody_channel_without_its_credential(false, &None, &None).is_ok());
        assert!(CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR.contains("first line of stdin"));
    }

    /// The custody credential is stdin's first line, without its line ending; the rest stays in
    /// the buffer for the next reader (`--watch-stdin`). Every other first line is refused, and
    /// the error never carries the value.
    #[test]
    fn the_custody_credential_is_the_first_line_of_stdin() {
        use std::io::Read;
        let admin = Some(Secret::from("admin"));
        let mut input = std::io::Cursor::new(b"custody\nthe watcher's\n".to_vec());
        let custody = read_custody_credential(&mut input, &admin).unwrap();
        assert!(custody.expose_for_compare("custody"));
        let mut rest = String::new();
        input.read_to_string(&mut rest).unwrap();
        assert_eq!(rest, "the watcher's\n");

        let mut crlf = std::io::Cursor::new(b"custody\r\n".to_vec());
        assert!(read_custody_credential(&mut crlf, &admin)
            .unwrap()
            .expose_for_compare("custody"));

        for refused in [
            &b""[..],
            b"custody",
            b"\n",
            b"\r\n",
            b"  \t\n",
            b"\xff\xfe\n",
        ] {
            let error = read_custody_credential(&mut std::io::Cursor::new(refused), &admin)
                .err()
                .expect("refused");
            assert_eq!(
                error, CUSTODY_CHANNEL_WITHOUT_CREDENTIAL_ERROR,
                "{refused:?}"
            );
        }
        let error = read_custody_credential(&mut std::io::Cursor::new(b"admin\n"), &admin)
            .err()
            .expect("refused");
        assert_eq!(error, CUSTODY_CREDENTIAL_IS_ADMIN_CREDENTIAL_ERROR);
        assert!(!error.contains("admin\n"));
    }

    /// A pipe, as a supervisor gives stdin: the line is read once it is written, before the
    /// pipe closes, and a buffered reader leaves what follows for the watcher.
    #[test]
    fn the_custody_line_and_the_watcher_share_one_buffered_pipe() {
        let (reader, mut writer) = std::io::pipe().expect("a pipe");
        std::io::Write::write_all(&mut writer, b"custody\nmore").unwrap();
        let mut reader = std::io::BufReader::new(reader);
        let custody = read_custody_credential(&mut reader, &None).unwrap();
        assert!(custody.expose_for_compare("custody"));
        let (closed_tx, closed_rx) = std::sync::mpsc::channel();
        let watcher = crate::exit::stop_when_closed(reader, move || closed_tx.send(()).unwrap());
        assert!(closed_rx
            .recv_timeout(std::time::Duration::from_millis(200))
            .is_err());
        drop(writer);
        closed_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the watcher sees the pipe close after the line");
        watcher.join().unwrap();
    }

    /// `runtime.config`'s echo, `config print` and `Debug` never carry the custody credential.
    #[test]
    fn the_custody_credential_is_redacted_everywhere_the_config_shows() {
        let config = Ad4mConfig {
            custody_channel: Some(true),
            custody_credential: Some(Secret::from("custody-secret-value")),
            ..Default::default()
        };
        let json = config.redacted_json();
        assert_eq!(json["custodyCredential"], crate::config_file::REDACTED);
        assert!(!json.to_string().contains("custody-secret-value"));
        assert!(!format!("{config:?}").contains("custody-secret-value"));
    }

    /// Both startup gates for multi-user mode, as `run()` applies them: the start flag
    /// (`check_admin_credential`) and the mode left on in the database
    /// (`refuse_multi_user_from_db`). Without a credential either refuses; with one, both pass.
    #[test]
    fn multi_user_gates_refuse_without_an_admin_credential() {
        // (credential, --enable-multi-user, on in the database, starts)
        let table = [
            (None, Some(true), false, false),
            (None, Some(false), true, false),
            (None, None, true, false),
            (Some("secret"), Some(true), false, true),
            (Some("secret"), None, true, true),
            (Some("secret"), Some(true), true, true),
            (None, Some(false), false, true),
        ];
        for (credential, flag, db_on, starts) in table {
            let config = Ad4mConfig {
                enable_multi_user: flag,
                ..with_credential(credential, Some(true))
            };
            let gates = config
                .check_admin_credential()
                .and_then(|()| refuse_multi_user_from_db(&config.admin_credential, db_on));
            assert_eq!(
                gates.is_ok(),
                starts,
                "credential {credential:?}, flag {flag:?}, db {db_on}: {gates:?}"
            );
            if let Err(reason) = gates {
                assert!(
                    reason.contains(MULTI_USER_WITHOUT_ADMIN_CREDENTIAL_ERROR),
                    "{reason}"
                );
            }
        }
        let from_db = refuse_multi_user_from_db(&None, true).unwrap_err();
        assert!(
            from_db.contains("on in this executor's database"),
            "{from_db}"
        );
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

    /// `config print` (`redacted_json`) hides the user name and password of an AI
    /// endpoint URL, and prints a URL without them as it is.
    #[test]
    fn redacted_json_hides_ai_endpoint_url_credentials() {
        let settings: crate::ai_service::endpoints::config::AiSettings =
            serde_json::from_value(serde_json::json!({
                "endpoints": [
                    { "name": "plain", "kind": "OLLAMA", "base_url": "http://127.0.0.1:11434/" },
                    { "name": "other", "kind": "OPEN_AI", "base_url": "https://api.example/v1" },
                ],
                "models": [],
                "defaults": {},
            }))
            .unwrap();
        let mut config = Ad4mConfig {
            ai_settings: Some(settings),
            ..Default::default()
        };
        let endpoints = &mut config.ai_settings.as_mut().unwrap().endpoints;
        endpoints[1].base_url = url::Url::parse("https://alice:hunter2@api.example/v1").unwrap();

        let printed = serde_json::to_string_pretty(&config.redacted_json()).unwrap();
        assert!(!printed.contains("hunter2"), "{printed}");
        assert!(!printed.contains("alice"), "{printed}");
        let endpoints = &config.redacted_json()["aiSettings"]["endpoints"];
        assert_eq!(endpoints[0]["base_url"], "http://127.0.0.1:11434/");
        assert_eq!(
            endpoints[1]["base_url"],
            "https://<redacted>@api.example/v1"
        );
        assert!(!format!("{config:?}").contains("hunter2"));
    }

    /// Plain serialisation (`get_json`) never writes the admin credential
    /// either: the field serialises as the redaction marker. Reading a config
    /// that carries the credential still works.
    #[test]
    fn the_admin_credential_serialises_redacted_and_deserialises() {
        let config = with_credential(Some("admin-secret"), None);
        let json = config.get_json();
        assert!(!json.contains("admin-secret"), "{json}");
        assert!(json.contains(r#""adminCredential":"<redacted>""#), "{json}");

        let mut value: serde_json::Value = serde_json::from_str(&json).unwrap();
        value["adminCredential"] = "from-json".into();
        let read: Ad4mConfig = serde_json::from_value(value).unwrap();
        assert!(read
            .admin_credential
            .unwrap()
            .expose_for_compare("from-json"));
        assert!(Ad4mConfig::default()
            .get_json()
            .contains(r#""adminCredential":null"#));
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
