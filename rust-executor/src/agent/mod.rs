use std::path;
use std::sync::{Arc, Mutex};

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

use crate::db::Ad4mDb;
use crate::types::domain::Perspective;
use crate::types::{Agent, AgentStatus};
use crate::types::{Expression, ExpressionProof};
use crate::wallet::wallet_backend;

pub mod capabilities;
pub mod grants;
pub mod session;
pub mod signatures;

/// Validate that a user email is safe to use as a filesystem path segment.
/// Rejects path separators, "..", null bytes, and other unsafe characters.
fn validate_user_email_for_path(email: &str) -> Result<(), AnyError> {
    if email.is_empty() {
        return Err(anyhow!("User email cannot be empty"));
    }
    if email.contains('/') || email.contains('\\') || email.contains('\0') || email.contains("..") {
        return Err(anyhow!(
            "Invalid user email: contains unsafe path characters"
        ));
    }
    Ok(())
}

/// One of the executor's agents: the main agent (agent #0, owned by the operator), or the
/// managed agent of a user. An agent is its DID: a managed agent is named by nothing else.
/// An email is only the login name of the user account that owns a managed agent; it
/// resolves to the DID where a caller names an address (login, signup, an RPC parameter).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AgentContext {
    /// The main agent (agent #0). Its DID is read from `AgentService` once it exists.
    Main,
    /// A managed agent, by its DID.
    Managed { did: String },
}

impl AgentContext {
    /// Whether this is the main agent.
    pub fn is_main_agent(&self) -> bool {
        matches!(self, AgentContext::Main)
    }

    /// The DID of a managed agent; `None` for the main agent.
    pub fn managed_did(&self) -> Option<&str> {
        match self {
            AgentContext::Main => None,
            AgentContext::Managed { did } => Some(did),
        }
    }

    /// The main agent.
    pub fn main_agent() -> Self {
        AgentContext::Main
    }

    /// The managed agent whose DID is `did`. Checks nothing: callers that start from a name a
    /// caller supplied resolve it through `AgentService::managed_agent_for_email` or
    /// `AgentService::managed_agent_for_subject`, which check the agent exists.
    pub fn managed(did: String) -> Self {
        AgentContext::Managed { did }
    }

    /// The name of this agent's key in the wallet: `KEY_NAME_MAIN` for the main agent, the
    /// DID for a managed agent (see [`AgentService::wallet_key_for_did`]).
    pub fn wallet_key_name(&self) -> Result<String, AnyError> {
        match self {
            AgentContext::Main => Ok(crate::wallet::KEY_NAME_MAIN.to_string()),
            AgentContext::Managed { did } => AgentService::wallet_key_for_did(did),
        }
    }
}

/// Where each managed agent's key sits in the wallet, by DID. Filled by
/// [`AgentService::wallet_key_for_did`]; cleared when the wallet's key names change.
static MANAGED_KEY_NAMES: once_cell::sync::Lazy<
    std::sync::RwLock<std::collections::HashMap<String, String>>,
> = once_cell::sync::Lazy::new(|| std::sync::RwLock::new(std::collections::HashMap::new()));

fn forget_managed_key_names() {
    if let Ok(mut names) = MANAGED_KEY_NAMES.write() {
        names.clear();
    }
}

/// Whether `name` names one of the executor's own wallet keys: the main agent's, the shared
/// platform key or the configured signing key. Such a key never belongs to a managed agent,
/// whatever a `users` row says: a user signed up under that name before agents were keyed by
/// DID was handed the executor's own key.
pub(crate) fn is_executor_key_name(name: &str) -> bool {
    name == crate::wallet::KEY_NAME_MAIN
        || name == crate::wallet::KEY_NAME_PLATFORM
        || crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .as_ref()
            .is_some_and(|config| config.signing_key_name() == name)
}

/// The DID of the key named `name` in `wallet`, if it holds one.
fn derived_did(wallet: &dyn crate::wallet::WalletBackend, name: &str) -> Option<String> {
    wallet.get_did_document(name).map(|doc| doc.id)
}

/// The outcome of [`migrate_managed_key_names`] for one user.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyNameMigration {
    /// The key was already named by the DID, and no key carries the old name.
    AlreadyNamedByDid,
    /// The key was copied under the DID, and the old name removed.
    Renamed,
    /// Both names held the same key: the old name was removed.
    RemovedDuplicate,
    /// The key is named by the DID, and the old name holds another key (deriving `derived`).
    /// That key is left alone: it is not this agent's, and removing it could lose it.
    KeptOtherKey { derived: String },
    /// The user has no key under either name.
    NoKey,
    /// The key under the old name derives another DID than the user's: it stays under its
    /// old name, and the user's sessions fail with `NoIdentity` until an operator fixes it.
    Mismatch { derived: String },
    /// The wallet refused the copy or the removal.
    Failed(String),
    /// The user's name is the name of one of the executor's own keys (see
    /// [`is_executor_key_name`]): that key is never copied or removed, and the user's sessions
    /// fail with `NoIdentity`.
    ExecutorKey,
}

/// Names every managed agent's key by its DID (D16): for each `(username, did)` from the
/// `users` table, a key named `username` is copied under `did`, the copy is checked to derive
/// `did`, and the old name is removed. `persist` writes the wallet to disk; it runs after
/// every copy and before any old name goes, so a crash at any point leaves the key reachable
/// under the old name, the new one, or both, and the next run finishes the job. With both
/// names present, the old one goes only if both hold the same key.
///
/// Idempotent: a second run on a migrated wallet changes nothing and persists nothing.
pub fn migrate_managed_key_names(
    wallet: &dyn crate::wallet::WalletBackend,
    users: &[(String, String)],
    mut persist: impl FnMut() -> Result<(), AnyError>,
) -> Vec<(String, KeyNameMigration)> {
    let mut outcomes = Vec::new();
    let mut to_remove = Vec::new();
    for (username, did) in users {
        if did.is_empty() || username == did {
            continue;
        }
        if is_executor_key_name(username) || is_executor_key_name(did) {
            outcomes.push((username.clone(), KeyNameMigration::ExecutorKey));
            continue;
        }
        let old_key = derived_did(wallet, username);
        let new_key = derived_did(wallet, did);
        let outcome = match (old_key, new_key) {
            (_, Some(new_did)) if &new_did != did => {
                KeyNameMigration::Mismatch { derived: new_did }
            }
            (None, Some(_)) => KeyNameMigration::AlreadyNamedByDid,
            (Some(old_did), Some(_)) if &old_did == did => {
                to_remove.push(username.clone());
                KeyNameMigration::RemovedDuplicate
            }
            (Some(old_did), Some(_)) => KeyNameMigration::KeptOtherKey { derived: old_did },
            (None, None) => KeyNameMigration::NoKey,
            (Some(derived), None) if &derived != did => KeyNameMigration::Mismatch { derived },
            (Some(_), None) => match wallet.copy_key(username, did) {
                Ok(()) if derived_did(wallet, did).as_deref() == Some(did.as_str()) => {
                    to_remove.push(username.clone());
                    KeyNameMigration::Renamed
                }
                Ok(()) => {
                    // A copy that derives another DID must not stay under the user's DID.
                    let _ = wallet.remove_key(did);
                    KeyNameMigration::Failed("the copy derives another DID".to_string())
                }
                Err(e) => KeyNameMigration::Failed(e.to_string()),
            },
        };
        outcomes.push((username.clone(), outcome));
    }

    if to_remove.is_empty() {
        return outcomes;
    }
    // The copies reach the disk before any old name goes.
    if let Err(e) = persist() {
        log::error!(
            "🔑 Could not save the wallet after naming keys by DID; the old names stay: {}",
            e
        );
        return outcomes;
    }
    for username in &to_remove {
        if let Err(e) = wallet.remove_key(username) {
            log::error!("🔑 Could not remove the key named {}: {}", username, e);
        }
    }
    if let Err(e) = persist() {
        log::warn!(
            "🔑 Could not save the wallet after removing the old key names; the next unlock \
             removes them again: {}",
            e
        );
    }
    forget_managed_key_names();
    outcomes
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AgentStore {
    did: String,
    #[serde(rename = "didDocument")]
    did_document: String,
    #[serde(rename = "signingKeyId")]
    signing_key_id: String,
    keystore: String,
    agent: Option<Agent>,
    /// Fingerprint of the keys in `keystore`, written while `agent.json.legacy` exists; see
    /// `AgentService::unlock_from_legacy_backup`.
    #[serde(
        rename = "keystoreFingerprint",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    keystore_fingerprint: Option<String>,
}

/// The stored DID document with every `privateKey…` member removed, or `None`
/// if it carried none (or is not JSON, which `dump()` cannot serve either).
///
/// `agent.json` written before #1229 holds the main agent's document as the
/// wallet used to build it — with `privateKeyBase58` for both keys, in
/// plaintext next to the encrypted keystore. `load()` serves that string, so it
/// has to be cleaned there. The executor never reads a secret from a document;
/// it signs through the wallet.
fn did_document_without_secrets(did_document: &str) -> Option<String> {
    fn strip(value: &mut serde_json::Value) -> bool {
        match value {
            serde_json::Value::Object(map) => {
                let before = map.len();
                map.retain(|key, _| !key.starts_with("privateKey"));
                let removed = map.len() != before;
                map.values_mut().fold(removed, |acc, v| strip(v) || acc)
            }
            serde_json::Value::Array(items) => {
                items.iter_mut().fold(false, |acc, v| strip(v) || acc)
            }
            _ => false,
        }
    }

    let mut value: serde_json::Value = serde_json::from_str(did_document).ok()?;
    if strip(&mut value) {
        serde_json::to_string(&value).ok()
    } else {
        None
    }
}

/// Replace `path` with `value` serialised as JSON, so that `path` holds either
/// the old or the new contents after a crash or power loss, never a truncated
/// file:
///
/// - The temp file sits next to `path` (a rename is atomic only within one
///   filesystem) and has a name unique to this process and call, so two
///   executors on one data dir never write the same temp file.
/// - It is created 0600, then takes the mode of the file it replaces, so an
///   operator's `chmod 600` survives the rename instead of falling back to
///   the umask.
/// - Its data is synced before the rename, and the directory after it:
///   without the first, some filesystems can commit the rename before the
///   data and leave an empty file; without the second, the rename itself can
///   be lost.
/// - On any error the temp file is removed and `path` is untouched.
///
/// The only writer of the agent file and its legacy backup: `try_save` and the
/// load-time private-key strip both go through it. It started as #1163's
/// `try_save` write (0600, original mode kept, `sync_all`, rename) and adds the
/// unique temp name, the cleanup on error and the directory sync.
fn write_json_atomically<T: Serialize>(path: &path::Path, value: &T) -> Result<(), AnyError> {
    use std::io::Write;
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT_TMP: AtomicU64 = AtomicU64::new(0);

    let dir = match path.parent() {
        Some(dir) if !dir.as_os_str().is_empty() => dir,
        _ => path::Path::new("."),
    };
    let file_name = path
        .file_name()
        .ok_or_else(|| anyhow!("{} has no file name", path.display()))?
        .to_string_lossy();
    let tmp = dir.join(format!(
        ".{}.{}.{}.tmp",
        file_name,
        std::process::id(),
        NEXT_TMP.fetch_add(1, Ordering::Relaxed)
    ));

    let write_tmp = || -> Result<(), AnyError> {
        let mut options = std::fs::OpenOptions::new();
        options.write(true).create_new(true);
        #[cfg(unix)]
        std::os::unix::fs::OpenOptionsExt::mode(&mut options, 0o600);
        let file = options.open(&tmp)?;
        if let Ok(meta) = std::fs::metadata(path) {
            file.set_permissions(meta.permissions())?;
        }
        let mut writer = std::io::BufWriter::new(file);
        serde_json::to_writer(&mut writer, value)?;
        writer.flush()?;
        writer.get_ref().sync_all()?;
        Ok(())
    };
    if let Err(e) = write_tmp().and_then(|()| Ok(std::fs::rename(&tmp, path)?)) {
        let _ = std::fs::remove_file(&tmp);
        return Err(e);
    }

    // The rename is done; a failed directory sync only weakens durability.
    #[cfg(unix)]
    if let Err(e) = std::fs::File::open(dir).and_then(|d| d.sync_all()) {
        log::warn!(
            "Could not sync {} after writing {}: {}",
            dir.display(),
            path.display(),
            e
        );
    }
    Ok(())
}

pub fn did_document_for_context(context: &AgentContext) -> Result<did_key::Document, AnyError> {
    let key_name = context.wallet_key_name()?;
    wallet_backend()
        .get_did_document(&key_name)
        .ok_or(anyhow!("{} key not found", key_name))
}

pub fn signing_key_id_for_context(context: &AgentContext) -> Result<String, AnyError> {
    let did_doc = did_document_for_context(context)?;
    Ok(did_doc.verification_method[0].id.clone())
}

pub fn did_for_context(context: &AgentContext) -> Result<String, AnyError> {
    match context {
        AgentContext::Main => AgentService::with_global_instance(|a| {
            a.did
                .clone()
                .ok_or(anyhow!("DID requested but not yet set in AgentService"))
        }),
        AgentContext::Managed { did } => Ok(did.clone()),
    }
}

pub fn sign_for_context(payload: &[u8], context: &AgentContext) -> Result<Vec<u8>, AnyError> {
    let backend = wallet_backend();
    let key_name = context.wallet_key_name()?;
    let signature = backend
        .sign(&key_name, payload)
        .ok_or(anyhow!("{} key not found", key_name))?;
    Ok(signature)
}

pub fn did_document() -> did_key::Document {
    did_document_for_context(&AgentContext::main_agent())
        .expect("Failed to get did_document for main agent")
}

pub fn signing_key_id() -> String {
    signing_key_id_for_context(&AgentContext::main_agent())
        .expect("Failed to get signing_key_id for main agent")
}

pub fn did() -> String {
    did_for_context(&AgentContext::main_agent()).expect("Failed to get did for main agent")
}

pub fn sign(payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    sign_for_context(payload, &AgentContext::main_agent())
}

pub fn check_keys_and_create(did: String) -> did_key::Document {
    let backend = wallet_backend();
    let name = crate::wallet::KEY_NAME_MAIN;
    if backend.get_did_document(name).is_none() {
        // In shared mode initialize_keys returns None (keys live server-side,
        // not importable from a DID string). Fall back to fetching the existing
        // DID document — the platform Worker already holds the keypair.
        match backend.initialize_keys(name, &did) {
            Some(doc) => doc,
            None => backend.get_did_document(name).unwrap_or_else(|| {
                panic!(
                    "Failed to initialise or retrieve DID document for key '{}'. \
                         In shared mode, ensure the platform Worker has generated the keypair.",
                    name
                )
            }),
        }
    } else {
        did_document()
    }
}

pub fn create_signed_expression<T: Serialize>(
    data: T,
    context: &AgentContext,
) -> Result<Expression<T>, AnyError> {
    let timestamp = chrono::Utc::now();
    let signature = hex::encode(sign_for_context(
        &signatures::hash_data_and_timestamp(&data, &timestamp),
        context,
    )?);

    Ok(Expression {
        author: did_for_context(context)?,
        timestamp: timestamp.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
        data,
        proof: ExpressionProof {
            key: signing_key_id_for_context(context)?,
            signature,
        },
    })
}

pub fn sign_string_hex_for_context(
    data: String,
    context: &AgentContext,
) -> Result<String, AnyError> {
    let payload_bytes = signatures::hash_message(&data);
    let signature = sign_for_context(&payload_bytes, context)?;
    let sig_hex = hex::encode(signature);
    Ok(sig_hex)
}

pub fn sign_string_hex(data: String) -> Result<String, AnyError> {
    sign_string_hex_for_context(data, &AgentContext::main_agent())
}

/// Convert an Agent's decorated perspective to plain LinkExpressions for publishing.
/// Returns a JSON value with non-decorated types (no proof.valid/invalid, no status).
fn agent_to_publish_json(agent: &Agent) -> Result<serde_json::Value, AnyError> {
    use crate::types::{LinkExpression, Perspective as PlainPerspective};

    let plain_perspective = agent.perspective.as_ref().map(|p| PlainPerspective {
        links: p
            .links
            .iter()
            .cloned()
            .map(|d| {
                let mut le = LinkExpression::from(d);
                le.status = None;
                le
            })
            .collect(),
    });

    Ok(serde_json::json!({
        "did": agent.did,
        "directMessageLanguage": agent.direct_message_language,
        "perspective": plain_perspective,
    }))
}

pub struct AgentSignature {
    pub signature: String,
    pub public_key: String,
}

impl AgentSignature {
    pub fn from_message(message: String) -> Result<AgentSignature, AnyError> {
        Self::from_message_for_context(message, &AgentContext::main_agent())
    }

    /// Signs `message` with the key of `context`: the main agent, or a user's own key.
    pub fn from_message_for_context(
        message: String,
        context: &AgentContext,
    ) -> Result<AgentSignature, AnyError> {
        Ok(AgentSignature {
            signature: sign_string_hex_for_context(message, context)?,
            public_key: signing_key_id_for_context(context)?,
        })
    }
}

impl From<AgentSignature> for crate::types::AgentSignature {
    fn from(val: AgentSignature) -> Self {
        crate::types::AgentSignature {
            signature: val.signature,
            public_key: val.public_key,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AgentService {
    pub did: Option<String>,
    pub did_document: Option<String>,
    pub signing_key_id: Option<String>,
    file: String,
    file_profile: String,
    users_dir: String,
    pub agent: Option<Agent>,
    #[serde(skip)]
    pub passphrase: Option<String>,
    /// The loaded agent file holds a keystore in the legacy format; the next unlock
    /// rewrites it in the current one.
    #[serde(skip)]
    legacy_keystore: bool,
}

lazy_static! {
    static ref AGENT_SERVICE: Arc<Mutex<Option<AgentService>>> = Arc::new(Mutex::new(None));
}

/// Serializes agent publishes. Callers may publish in the background, so two
/// publishes can overlap; holding this across the read and the write means the
/// last one to finish always carries the latest stored profile, never an older
/// snapshot that happened to be slower on the network.
///
/// Process-wide rather than per-DID on purpose: every agent publishes through
/// the one agent-language runtime, which runs requests serially, so a stuck
/// publish already blocks other users there. A per-DID lock would add
/// bookkeeping without decoupling them.
static AGENT_PUBLISH_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

/// SHA-256 over the names and public keys in a wallet, in name order.
fn key_set_fingerprint(wallet: &dyn crate::wallet::WalletBackend) -> String {
    use sha2::{Digest, Sha256};
    let mut names = wallet.list_key_names();
    names.sort();
    let mut hasher = Sha256::new();
    for name in names {
        let public_key = wallet.get_public_key(&name).unwrap_or_default();
        hasher.update((name.len() as u64).to_le_bytes());
        hasher.update(name.as_bytes());
        hasher.update((public_key.len() as u64).to_le_bytes());
        hasher.update(&public_key);
    }
    hex::encode(hasher.finalize())
}

impl AgentService {
    pub fn init_global_instance(app_path: String) {
        let mut agent_instance = AGENT_SERVICE.lock().unwrap();
        *agent_instance = Some(AgentService::new(app_path));
    }

    pub fn init_global_test_instance() {
        // Ensure a wallet backend exists before create_new_keys() tries to
        // call wallet_backend(). Tests run in arbitrary order so the backend
        // may already be initialised by a prior test — try_init is idempotent.
        let local = Arc::new(crate::wallet::LocalWallet::new());
        let _ =
            crate::wallet::try_init_wallet_backend(local as Arc<dyn crate::wallet::WalletBackend>);

        // Ensure a global config exists (create_new_keys reads signing_key_name).
        {
            let cfg = crate::config::GLOBAL_AD4M_CONFIG
                .lock()
                .unwrap_or_else(|e| e.into_inner());
            if cfg.is_none() {
                drop(cfg);
                crate::config::set_global_config(crate::config::Ad4mConfig {
                    app_data_path: Some("test".to_string()),
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
                });
            }
        }

        let mut agent_instance = AGENT_SERVICE.lock().unwrap();

        *agent_instance = Some(AgentService {
            did: None,
            did_document: None,
            file: "test".to_string(),
            file_profile: "test".to_string(),
            users_dir: "test".to_string(),
            agent: None,
            signing_key_id: None,
            passphrase: None,
            legacy_keystore: false,
        });

        (*agent_instance).as_mut().unwrap().create_new_keys();
    }

    pub fn new(app_path: String) -> AgentService {
        let agent_path = format!("{}/ad4m/agent.json", app_path);
        let agent_profile_path = format!("{}/ad4m/agentProfile.json", app_path);
        let users_dir = format!("{}/ad4m/users", app_path);

        AgentService {
            did: None,
            did_document: None,
            file: agent_path,
            file_profile: agent_profile_path,
            users_dir: users_dir,
            agent: None,
            signing_key_id: None,
            passphrase: None,
            legacy_keystore: false,
        }
    }

    pub fn global_instance() -> Arc<Mutex<Option<AgentService>>> {
        AGENT_SERVICE.clone()
    }

    pub fn with_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&AgentService) -> R,
    {
        let global_instance_arc = AgentService::global_instance();
        let lock_result = global_instance_arc.lock();
        let agent_service_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let agent_service_ref = agent_service_lock
            .as_ref()
            .expect("AgentService not initialized");
        func(agent_service_ref)
    }

    pub fn with_mutable_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&mut AgentService) -> R,
    {
        let global_instance_arc = AgentService::global_instance();
        let lock_result = global_instance_arc.lock();
        let mut agent_service_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let agent_service_mut = agent_service_lock
            .as_mut()
            .expect("AgentService not initialized");
        func(agent_service_mut)
    }

    pub fn is_initialized(&self) -> bool {
        let is_initialized = path::Path::new(self.file.as_str()).exists();
        is_initialized
    }

    pub fn is_unlocked(&self) -> bool {
        let backend = wallet_backend();
        backend.is_unlocked()
    }

    fn signing_checks(&self) -> Result<(), AnyError> {
        if !self.is_initialized() {
            return Err(anyhow!("Agent not initialized"));
        }
        if !self.is_unlocked() {
            return Err(anyhow!("Agent not unlocked"));
        }
        if self.signing_key_id.is_none() {
            return Err(anyhow!("Agent signing key not found"));
        }
        Ok(())
    }

    pub fn create_signed_expression<T: Serialize>(
        &self,
        data: T,
        context: &AgentContext,
    ) -> Result<Expression<T>, AnyError> {
        self.signing_checks()?;

        create_signed_expression(data, context)
    }

    pub fn sign_string_hex(&self, data: String) -> Result<String, AnyError> {
        self.signing_checks()?;

        sign_string_hex(data)
    }

    // User management functions

    /// Creates the key of a new managed agent and returns its DID. A wallet that can name keys
    /// freely names it by the DID. A shared wallet's service creates a key under the name it
    /// is given and cannot rename it, so there it is named by the user's email, where
    /// [`AgentService::wallet_key_for_did`] finds it.
    pub fn create_managed_agent_key(user_email: &str) -> Result<String, AnyError> {
        // A shared wallet's `get_or_create_keypair` would hand out the executor's own key.
        if is_executor_key_name(user_email) {
            return Err(anyhow!(
                "{} is the name of one of the executor's own keys",
                user_email
            ));
        }
        let backend = wallet_backend();
        if backend.can_rename_keys() {
            return backend.generate_keypair_named_by_did();
        }
        backend.get_or_create_keypair(user_email)?;
        derived_did(backend.as_ref(), user_email)
            .ok_or_else(|| anyhow!("The wallet did not create a key for {}", user_email))
    }

    /// The DID of the managed agent of the user `user_email`, from the `users` table. On an
    /// executor with a shared database, a user created on another executor is imported first.
    pub fn did_for_email(user_email: &str) -> Result<String, AnyError> {
        let local = Ad4mDb::with_global_instance(|db| db.get_user(user_email).ok());
        let did = match local {
            Some(user) => user.did,
            None => crate::user_management::import_user_from_shared_db(user_email)?
                .ok_or_else(|| anyhow!("No user {}", user_email))?,
        };
        if did.is_empty() {
            return Err(anyhow!("The user {} has no agent", user_email));
        }
        Ok(did)
    }

    /// The DID of the managed agent of the user `user_email` in this executor's `users` table,
    /// without importing from a shared database: for publishers, which must not create users.
    pub fn local_did_for_email(user_email: &str) -> Option<String> {
        Ad4mDb::with_global_instance(|db| db.get_user(user_email).ok())
            .map(|user| user.did)
            .filter(|did| !did.is_empty())
    }

    /// The DID of the managed agent of the user `user_email`, writing nothing: the `users`
    /// table, else, on an executor with a shared database, the shared record read without
    /// importing the user. For lookups that anonymous callers trigger (D15), where
    /// [`Self::did_for_email`] would import, and so write, a user.
    pub fn did_for_email_read_only(user_email: &str) -> Option<String> {
        match Ad4mDb::with_global_instance(|db| db.get_user(user_email).ok()) {
            Some(user) => Some(user.did).filter(|did| !did.is_empty()),
            None => crate::user_management::shared_did_for_email(user_email),
        }
    }

    /// The email of the user who owns the managed agent `did`: the address to write to, and
    /// the name of the user's profile directory. Not an identity: everything else names the
    /// agent by its DID.
    pub fn email_for_did(did: &str) -> Result<String, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_username_by_did(did))?
            .ok_or_else(|| anyhow!("No user owns the agent {}", did))
    }

    /// The DIDs of the executor's managed agents: every user with an agent.
    pub fn managed_agent_dids() -> Result<Vec<String>, AnyError> {
        Ok(Ad4mDb::with_global_instance(|db| db.list_users())?
            .into_iter()
            .map(|user| user.did)
            .filter(|did| !did.is_empty())
            .collect())
    }

    /// The managed agent of the user `user_email`: where a caller names a user by email
    /// (login, an RPC parameter), this resolves it to the agent once. Fails if the user does
    /// not exist or the wallet holds no key that derives the agent's DID.
    pub fn managed_agent_for_email(user_email: &str) -> Result<AgentContext, AnyError> {
        let did = Self::did_for_email(user_email)?;
        Self::wallet_key_for_did(&did)?;
        Ok(AgentContext::managed(did))
    }

    /// The managed agent a user token's `sub` names. Tokens minted since agents are keyed by
    /// DID carry the DID; tokens minted before carry the user's email. `email_hint` is the
    /// token's `userEmail`, which imports a user created on another executor of a shared
    /// database. Fails if no user owns the agent or the wallet holds no key for it.
    pub fn managed_agent_for_subject(
        sub: &str,
        email_hint: Option<&str>,
    ) -> Result<AgentContext, AnyError> {
        if !sub.starts_with("did:") {
            return Self::managed_agent_for_email(sub);
        }
        if Self::email_for_did(sub).is_err() {
            let imported = match email_hint {
                Some(email) => crate::user_management::import_user_from_shared_db(email)?,
                None => None,
            };
            if imported.as_deref() != Some(sub) {
                return Err(anyhow!("No user owns the agent {}", sub));
            }
        }
        Self::wallet_key_for_did(sub)?;
        Ok(AgentContext::managed(sub.to_string()))
    }

    /// The name of the wallet key of the managed agent `did`. Keys are named by the DID (D16).
    /// A key under the user's email (a shared wallet, which cannot rename keys, or a wallet
    /// whose migration kept the old name) is used only if it derives `did`. Fails if neither
    /// name holds a key that derives `did`: the agent has no identity on this executor.
    pub fn wallet_key_for_did(did: &str) -> Result<String, AnyError> {
        if let Some(name) = MANAGED_KEY_NAMES
            .read()
            .ok()
            .and_then(|names| names.get(did).cloned())
        {
            return Ok(name);
        }
        let backend = wallet_backend();
        let name = if derived_did(backend.as_ref(), did).as_deref() == Some(did) {
            did.to_string()
        } else {
            let email = Self::email_for_did(did)?;
            if is_executor_key_name(&email) {
                return Err(anyhow!(
                    "The user {} is named like one of the executor's own keys",
                    email
                ));
            }
            if derived_did(backend.as_ref(), &email).as_deref() != Some(did) {
                return Err(anyhow!("No key in the wallet derives the agent {}", did));
            }
            email
        };
        if let Ok(mut names) = MANAGED_KEY_NAMES.write() {
            names.insert(did.to_string(), name.clone());
        }
        Ok(name)
    }

    /// Names every managed agent's key by its DID; see [`migrate_managed_key_names`]. Runs
    /// on unlock, the only time the keys are in memory. Does nothing on a wallet that cannot
    /// rename keys.
    fn migrate_managed_key_names_on_unlock(&self, password: &str) {
        // The unlocked keystore replaces the keys in memory: resolve names afresh.
        forget_managed_key_names();
        let backend = wallet_backend();
        if !backend.can_rename_keys() {
            return;
        }
        // While the legacy backup exists, `unlock_from_legacy_backup` needs the agent file to
        // hold exactly the backup's keys, names included. The unlock that retires the backup
        // runs this again.
        if path::Path::new(&self.legacy_backup_file()).exists() {
            return;
        }
        let users = match Ad4mDb::global_instance().lock() {
            Ok(db) => match db.as_ref().map(|db| db.list_users()) {
                Some(Ok(users)) => users,
                Some(Err(e)) => {
                    log::error!("🔑 Could not list users to name their keys by DID: {}", e);
                    return;
                }
                // No database yet (some tests): no users.
                None => return,
            },
            Err(_) => return,
        };
        let pairs: Vec<(String, String)> = users.into_iter().map(|u| (u.username, u.did)).collect();
        let outcomes =
            migrate_managed_key_names(backend.as_ref(), &pairs, || self.try_save(password));
        for (username, outcome) in outcomes {
            match outcome {
                KeyNameMigration::AlreadyNamedByDid => {}
                KeyNameMigration::Renamed | KeyNameMigration::RemovedDuplicate => {
                    log::info!("🔑 The key of {} is now named by its DID", username)
                }
                KeyNameMigration::KeptOtherKey { derived } => log::warn!(
                    "🔑 The key of {} is named by its DID; the key under the old name derives \
                     {} and was left in place",
                    username,
                    derived
                ),
                KeyNameMigration::NoKey => {
                    log::warn!("🔑 The wallet holds no key for the user {}", username)
                }
                KeyNameMigration::Mismatch { derived } => log::error!(
                    "🔑 The key of the user {} derives {}, not the user's DID; it keeps its old \
                     name and the user's sessions fail until an operator fixes it",
                    username,
                    derived
                ),
                KeyNameMigration::Failed(e) => log::error!(
                    "🔑 Could not name the key of {} by its DID: {}",
                    username,
                    e
                ),
                KeyNameMigration::ExecutorKey => log::error!(
                    "🔑 The user {} is named like one of the executor's own keys; that key stays \
                     where it is and the user's sessions fail until an operator fixes the account",
                    username
                ),
            }
        }
    }

    /// The file that holds a user's agent profile. Profiles sit in a directory named by the
    /// user's email: a DID carries `:`, which some filesystems refuse in a name.
    pub fn user_profile_path(&self, user_email: &str) -> Result<path::PathBuf, AnyError> {
        validate_user_email_for_path(user_email)?;
        Ok(path::Path::new(&self.users_dir)
            .join(user_email)
            .join("profile.json"))
    }

    /// The stored profile of the managed agent `did`, if it has one.
    pub fn load_managed_agent_profile(did: &str) -> Result<Option<Agent>, AnyError> {
        // Resolved before the service lock: the lookup takes the database lock.
        let email = Self::email_for_did(did)?;
        Self::with_global_instance(|svc| svc.load_user_agent_profile(&email))
    }

    /// Stores the profile of the managed agent `did`.
    pub fn store_managed_agent_profile(did: &str, agent: &Agent) -> Result<(), AnyError> {
        let email = Self::email_for_did(did)?;
        Self::with_global_instance(|svc| svc.store_user_agent_profile(&email, agent))
    }

    /// Store agent profile for a specific user
    pub fn store_user_agent_profile(
        &self,
        user_email: &str,
        agent: &Agent,
    ) -> Result<(), AnyError> {
        let profile_path = self.user_profile_path(user_email)?;
        if let Some(user_profile_dir) = profile_path.parent() {
            std::fs::create_dir_all(user_profile_dir)?;
        }
        std::fs::write(profile_path, serde_json::to_string(agent)?)?;
        Ok(())
    }

    /// Unified method to publish an agent profile to the agent language.
    /// Works for both the main agent and managed users.
    /// Strips link decorations before publishing.
    pub async fn publish_agent_to_language(context: &AgentContext) -> Result<(), AnyError> {
        let controller = crate::languages::LanguageController::global_instance();
        let agent_lang = controller
            .get_agent_language()
            .await
            .map_err(|e| anyhow!("Agent language not available: {}", e))?;

        // Must be taken before the read below, not after: reading the profile
        // outside the lock would let a stale snapshot be published last.
        let _publish_guard = AGENT_PUBLISH_LOCK.lock().await;
        let agent = Self::get_agent_for_context(context)?;
        let context_did = did_for_context(context)?;
        if agent.did != context_did {
            return Err(anyhow!(
                "DID mismatch: stored profile has DID {} but signing context resolves to {}",
                agent.did,
                context_did
            ));
        }
        let agent_json = agent_to_publish_json(&agent)?;
        controller
            .expression_create(agent_lang.address(), agent_json, context)
            .await
            .map_err(|e| anyhow!("Failed to publish agent to language: {}", e))?;

        log::info!("Published agent {} to agent language", agent.did);
        Ok(())
    }

    /// Get the Agent data for a given context (main agent or managed user).
    pub fn get_agent_for_context(context: &AgentContext) -> Result<Agent, AnyError> {
        match context {
            AgentContext::Managed { did } => Self::load_managed_agent_profile(did)?
                .ok_or_else(|| anyhow!("User profile not found for {}", did)),
            AgentContext::Main => AgentService::with_mutable_global_instance(|svc| {
                svc.ensure_main_agent_loaded();
                svc.agent
                    .clone()
                    .ok_or_else(|| anyhow!("Agent not initialized"))
            }),
        }
    }

    pub fn ensure_main_agent_loaded(&mut self) {
        if self.agent.is_none() && self.is_initialized() {
            self.load();
        }
    }

    /// Load agent profile for a specific user
    fn load_user_agent_profile(&self, user_email: &str) -> Result<Option<Agent>, AnyError> {
        let profile_path = self.user_profile_path(user_email)?;

        if !profile_path.exists() {
            return Ok(None);
        }

        let profile_json = std::fs::read_to_string(profile_path)?;
        let agent: Agent = serde_json::from_str(&profile_json)?;
        Ok(Some(agent))
    }

    pub fn store_agent_profile(&self) {
        let agent = self.agent.as_ref().expect("Agent profile not found");
        std::fs::write(
            self.file_profile.as_str(),
            serde_json::to_string(&agent).unwrap(),
        )
        .expect("Failed to write agent profile file");

        // Note: callers who need the agent published to the agent language
        // should call publish_agent_to_language() separately (this method
        // is sync and cannot await).
    }

    pub fn save_agent_profile(&mut self, agent: Agent) {
        self.agent = Some(agent);
        self.store_agent_profile();
    }

    pub fn create_new_keys(&mut self) {
        let backend = wallet_backend();
        backend
            .generate_keypair(crate::wallet::KEY_NAME_MAIN)
            .expect("failed to generate main keypair");

        // In shared mode the JWT signing key name differs from "main"
        // (defaults to "platform"). Create it once if it does not exist yet,
        // so all executors sharing the wallet can sign tokens.
        //
        // Concurrency: `key_exists` + `generate_keypair` is a TOCTOU
        // window — two executors starting at the same time both see the
        // key missing and both call `generate_keypair`. This is safe
        // *because the platform Worker's key-creation endpoint is
        // idempotent*: it returns the existing key if the name is already
        // taken, so racing calls converge on the same keypair rather than
        // producing two competing signing identities. If the Worker's
        // key-creation semantics ever change from idempotent-by-name to
        // create-or-fail, this branch has to become a
        // check-then-atomically-create RPC instead. See
        // `SharedWallet::generate_keypair`.
        let signing_name = crate::config::get_global_config().signing_key_name();
        if signing_name != crate::wallet::KEY_NAME_MAIN && !backend.key_exists(&signing_name) {
            backend
                .generate_keypair(&signing_name)
                .expect("failed to generate signing keypair");
            log::info!("Created shared signing key '{}'", signing_name);
        }

        let did = backend
            .get_did_document(crate::wallet::KEY_NAME_MAIN)
            .expect("couldn't get DID document for keys that were just generated above")
            .id;

        self.did_document = Some(serde_json::to_string(&did_document()).unwrap());
        self.did = Some(did.clone());
        self.agent = Some(Agent {
            did,
            perspective: Some(Perspective { links: vec![] }),
            direct_message_language: None,
        });
        self.signing_key_id = Some(signing_key_id());
    }

    /// Copy of the agent file as it was before its legacy keystore was rewritten.
    ///
    /// The legacy key only saw the first 24 bytes of the space-padded passphrase, so
    /// `pw` and `pw ` (or two passphrases that differ after byte 24) open the same legacy
    /// file. The rewrite binds the file to whichever of them was typed. Until an unlock
    /// opens the rewritten file directly, a failed unlock on a locked executor falls back to
    /// this copy (`unlock_from_legacy_backup`).
    fn legacy_backup_file(&self) -> String {
        format!("{}.legacy", self.file)
    }

    /// Tries `password` on the legacy backup. On success the keys are unlocked and the agent
    /// file is rewritten under `password`. Refuses, and changes nothing, unless the agent
    /// file's fingerprint shows that it holds exactly the backup's keys: the rewrite would
    /// lose any other key (a new user, a regenerated main key).
    fn unlock_from_legacy_backup(&mut self, password: &str) -> Result<(), AnyError> {
        let read_store = |path: &str| -> Result<AgentStore, AnyError> {
            Ok(serde_json::from_str(&std::fs::read_to_string(path)?)?)
        };
        let legacy = read_store(&self.legacy_backup_file())?.keystore;
        if !crate::wallet::is_legacy_keystore(&legacy) {
            return Err(anyhow!("the keystore backup is not in the legacy format"));
        }
        let current = read_store(&self.file)?;
        let scratch = crate::wallet::LocalWallet::new();
        scratch.load(&legacy);
        scratch.unlock(password)?;
        if current.keystore_fingerprint != Some(key_set_fingerprint(&scratch)) {
            // Logged, not returned: `unlock` reports its own error, so a caller that guesses
            // passphrases does not learn that this one opens the backup.
            log::warn!(
                "🔑 The passphrase opens the legacy keystore backup {}, but the agent file holds \
                 keys the backup lacks. Only the passphrase that rewrote the agent file opens it; \
                 both files stay as they are.",
                self.legacy_backup_file()
            );
            return Err(anyhow!(
                "the agent file holds keys that its legacy backup lacks"
            ));
        }

        let backend = wallet_backend();
        backend.load(&legacy);
        if let Err(e) = backend.unlock(password) {
            backend.load(&current.keystore);
            return Err(e);
        }
        match self.try_save(password) {
            Ok(()) => log::info!("🔑 Rewrote the keystore from its legacy backup."),
            Err(e) => log::warn!(
                "🔑 Could not rewrite the keystore from its legacy backup: {}",
                e
            ),
        }
        Ok(())
    }

    pub fn unlock(&mut self, password: String) -> Result<(), AnyError> {
        let backend = wallet_backend();
        // An unlocked executor never falls back: the backup's keys would replace keys in
        // memory that the executor may not have saved yet.
        let was_unlocked = backend.is_unlocked();
        let mut result = backend.unlock(&password);
        let backup_exists = std::path::Path::new(&self.legacy_backup_file()).exists();
        if result.is_ok() && !self.legacy_keystore && backup_exists {
            // The keys can open from memory that `lock` re-encrypted under another
            // passphrase, so bind the agent file to this one before the backup goes.
            match self.try_save(&password).and_then(|_| {
                std::fs::remove_file(self.legacy_backup_file()).map_err(AnyError::from)
            }) {
                Ok(()) => log::info!("🔑 Removed the legacy keystore backup."),
                Err(e) => log::warn!("🔑 Kept the legacy keystore backup: {}", e),
            }
        }
        if result.is_err()
            && !was_unlocked
            && !self.legacy_keystore
            && backup_exists
            && self.unlock_from_legacy_backup(&password).is_ok()
        {
            result = Ok(());
        }
        if result.is_ok() {
            // Best effort: an executor on a read-only or full disk still unlocks, and its keystore
            // stays in the legacy format until a later save succeeds. No rewrite without a
            // backup of the legacy file, see `legacy_backup_file`.
            if self.legacy_keystore {
                // Synced before the rewrite: after a power loss the backup must hold the
                // legacy file whenever the agent file holds the new one.
                match std::fs::copy(&self.file, self.legacy_backup_file())
                    .and_then(|_| std::fs::File::open(self.legacy_backup_file())?.sync_all())
                    .map_err(AnyError::from)
                    .and_then(|_| self.try_save(&password))
                {
                    Ok(()) => {
                        self.legacy_keystore = false;
                        log::info!("🔑 Rewrote the keystore in the current format.");
                    }
                    Err(e) => log::warn!(
                        "🔑 Could not rewrite the keystore in the current format; it stays in the \
                         legacy format until the next save: {}",
                        e
                    ),
                }
            }
            self.migrate_managed_key_names_on_unlock(&password);
            self.passphrase = Some(password);
            let key_count = backend.list_key_names().len();
            log::debug!("🔑 Wallet unlocked. {} key(s) present.", key_count);

            // Ensure the shared signing key exists (may have been created by
            // another executor; create only if missing).
            //
            // Same TOCTOU note as `create_new_keys` above: safe because the
            // Worker's key-creation endpoint is idempotent by name and
            // racing executors converge on the same key.
            let signing_name = crate::config::get_global_config().signing_key_name();
            if signing_name != crate::wallet::KEY_NAME_MAIN && !backend.key_exists(&signing_name) {
                backend.generate_keypair(&signing_name).map_err(|e| {
                    anyhow!(
                        "Failed to create signing key '{}' during unlock: {}",
                        signing_name,
                        e
                    )
                })?;
                log::info!("Created shared signing key '{}'", signing_name);
            }
        }
        result
    }

    pub fn lock(&mut self, password: String) {
        // Save wallet before locking to persist any changes
        if self.passphrase.is_some() {
            self.save(self.passphrase.clone().unwrap());
        }

        let backend = wallet_backend();
        backend.lock(&password);

        // Clear the stored passphrase after locking
        self.passphrase = None;
    }

    pub fn save(&self, password: String) {
        self.try_save(&password)
            .expect("Failed to write agent file");
    }

    /// Writes the agent file through `write_json_atomically`, so a crash mid-write never
    /// leaves a truncated keystore behind and an operator's `chmod` survives.
    pub fn try_save(&self, password: &str) -> Result<(), AnyError> {
        let backend = wallet_backend();
        let keystore = backend.export(password);
        // Taken after the export: a key that arrives in between only stops the fallback
        // until the next save.
        let keystore_fingerprint = std::path::Path::new(&self.legacy_backup_file())
            .exists()
            .then(|| key_set_fingerprint(backend.as_ref()));

        let store = AgentStore {
            did: self.did.clone().ok_or(anyhow!("no DID to save"))?,
            did_document: self
                .did_document
                .clone()
                .ok_or(anyhow!("no DID document to save"))?,
            signing_key_id: self
                .signing_key_id
                .clone()
                .ok_or(anyhow!("no signing key id to save"))?,
            keystore,
            agent: self.agent.clone(),
            keystore_fingerprint,
        };

        write_json_atomically(path::Path::new(&self.file), &store)
    }

    /// Removes the private keys from `store`'s DID document and, if it had any, rewrites
    /// `file` with the result. Failure is logged, not returned: the caller still serves
    /// the cleaned `store`, so nothing leaks over the API.
    ///
    /// Done at load rather than at the next save(): save() runs on lock and profile
    /// changes, so an executor that is only ever unlocked would keep its private keys on disk
    /// in plaintext indefinitely. The file is re-serialised from `AgentStore`, as save()
    /// does: the keystore string and its fingerprint are kept byte for byte, but a
    /// top-level key that `AgentStore` does not model is dropped.
    fn strip_private_keys_from_stored_document(file: &str, store: &mut AgentStore) {
        let Some(public_document) = did_document_without_secrets(&store.did_document) else {
            return;
        };
        store.did_document = public_document;
        if let Err(e) = write_json_atomically(path::Path::new(file), &*store) {
            log::error!(
                "Could not remove private keys from the DID document in {}: {}",
                file,
                e
            );
        }
    }

    pub fn load(&mut self) {
        if !self.is_initialized() {
            return;
        }

        let file = std::fs::read_to_string(self.file.as_str()).expect("Failed to read agent file");
        let mut dump: AgentStore = serde_json::from_str(&file).unwrap();
        Self::strip_private_keys_from_stored_document(&self.file, &mut dump);

        // An executor that ran #1163 before this fix may have copied its leaking agent file to
        // the legacy backup. Only the backup's keystore is ever read back
        // (`unlock_from_legacy_backup`), so its document is cleaned the same way.
        let backup = self.legacy_backup_file();
        if path::Path::new(&backup).exists() {
            match std::fs::read_to_string(&backup)
                .map_err(AnyError::from)
                .and_then(|s| Ok(serde_json::from_str::<AgentStore>(&s)?))
            {
                Ok(mut backup_store) => {
                    Self::strip_private_keys_from_stored_document(&backup, &mut backup_store)
                }
                Err(e) => log::warn!("Could not read the keystore backup {}: {}", backup, e),
            }
        }

        self.did = Some(dump.did.clone());
        self.did_document = Some(dump.did_document);
        self.signing_key_id = Some(dump.signing_key_id);

        self.legacy_keystore = crate::wallet::is_legacy_keystore(&dump.keystore);
        {
            let backend = wallet_backend();
            backend.load(&dump.keystore);
        }

        if std::path::Path::new(self.file_profile.as_str()).exists() {
            let file_profile = std::fs::read_to_string(self.file_profile.as_str())
                .expect("Failed to read agent profile file");
            self.agent =
                Some(serde_json::from_str(&file_profile).expect("Failed to parse agent profile"));
        } else if let Some(agent) = dump.agent {
            // Restore agent profile from agent.json (save() embeds it there).
            // agentProfile.json may not exist if the profile was only saved via save().
            self.agent = Some(agent);
        } else {
            // No profile anywhere - create a minimal placeholder with just the DID.
            // DO NOT call check_keys_and_create() here because that would initialize
            // wallet keys without a passphrase, making is_unlocked() return true
            // before the user has actually entered their password.
            self.agent = Some(Agent {
                did: dump.did.clone(),
                perspective: Some(Perspective { links: vec![] }),
                direct_message_language: None,
            });
        }
    }

    pub fn dump(&self) -> AgentStatus {
        let did_document_value = self
            .did_document
            .as_ref()
            .and_then(|doc| serde_json::from_str(doc).ok());

        AgentStatus {
            did: self.did.clone(),
            did_document: did_document_value,
            is_initialized: self.is_initialized(),
            is_unlocked: self.is_unlocked(),
            error: None,
        }
    }
}

#[cfg(test)]
mod tests {

    use serde_json::json;

    use super::*;
    use crate::agent::signatures::verify_string_signed_by_did;
    use crate::test_utils::{setup_agent, setup_wallet};

    use once_cell::sync::OnceCell;

    static SETUP: OnceCell<()> = OnceCell::new();

    fn ensure_setup() {
        SETUP.get_or_init(|| {
            setup_wallet();
            setup_agent();
        });
    }

    /// Serialises one map entry, then fails: a write that breaks partway.
    struct FailsPartway;
    impl Serialize for FailsPartway {
        fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            use serde::ser::{Error, SerializeMap};
            let mut map = serializer.serialize_map(None)?;
            map.serialize_entry("keystore", "half")?;
            Err(S::Error::custom("disk full"))
        }
    }

    fn dir_entries(dir: &std::path::Path) -> Vec<String> {
        let mut names: Vec<String> = std::fs::read_dir(dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }

    #[test]
    fn write_json_atomically_replaces_the_file_and_leaves_no_temp_file() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("agent.json");
        std::fs::write(&file, "old").unwrap();

        write_json_atomically(&file, &json!({"keystore": "new"})).unwrap();

        assert_eq!(
            std::fs::read_to_string(&file).unwrap(),
            r#"{"keystore":"new"}"#
        );
        assert_eq!(dir_entries(dir.path()), vec!["agent.json"]);
    }

    #[test]
    fn write_json_atomically_keeps_the_original_and_removes_the_temp_file_when_the_write_fails() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("agent.json");
        std::fs::write(&file, "the only keystore").unwrap();

        let result = write_json_atomically(&file, &FailsPartway);

        assert!(result.is_err(), "a failed write must be reported");
        assert_eq!(
            std::fs::read_to_string(&file).unwrap(),
            "the only keystore",
            "a failed write must leave the original untouched"
        );
        assert_eq!(
            dir_entries(dir.path()),
            vec!["agent.json"],
            "a failed write must not leave its temp file behind"
        );
    }

    #[cfg(unix)]
    #[test]
    fn write_json_atomically_keeps_the_mode_of_the_file_it_replaces() {
        use std::os::unix::fs::PermissionsExt;
        let mode = |p: &std::path::Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
        let dir = tempfile::tempdir().unwrap();

        // 0640 differs from both the 0600 the temp file starts with and the
        // umask default, so only copying the mode gets it right.
        let file = dir.path().join("agent.json");
        std::fs::write(&file, "old").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o640)).unwrap();
        write_json_atomically(&file, &json!({})).unwrap();
        assert_eq!(mode(&file), 0o640);

        // A new file is created private, not with the umask default.
        let new_file = dir.path().join("new.json");
        write_json_atomically(&new_file, &json!({})).unwrap();
        assert_eq!(mode(&new_file), 0o600);
    }

    #[test]
    fn test_sign_and_verify_string_hex_roundtrip() {
        ensure_setup();
        let test_message = "Hello, World!".to_string();
        let signature = sign_string_hex(test_message.clone()).expect("Failed to sign message");
        let did = did();

        assert!(
            verify_string_signed_by_did(&did, &test_message, &signature)
                .expect("Verification failed"),
            "Signature verification for sign_string_hex failed"
        );
    }

    #[test]
    fn test_create_signed_expression() {
        ensure_setup();
        let signed_expression =
            create_signed_expression(json!({"test": "data"}), &AgentContext::main_agent())
                .expect("Failed to create signed expression");
        assert!(
            signatures::verify(&signed_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression failed"
        );

        let mut broken = signed_expression.clone();
        broken.proof.signature = "broken".to_string();

        assert!(
            signatures::verify(&broken).is_err(),
            "Broken signature verification should fail"
        );

        let mut changed = signed_expression.clone();
        changed.data = json!({"changed": "data"});

        assert!(
            !signatures::verify(&changed).expect("Verification failed"),
            "Signature invalidation for create_signed_expression failed"
        );
    }

    #[test]
    fn test_agent_signature_roundtrip() {
        ensure_setup();
        let test_message = "Agent signature test".to_string();
        let agent_signature = AgentSignature::from_message(test_message.clone())
            .expect("Failed to create agent signature");
        let did = did();

        assert!(
            verify_string_signed_by_did(&did, &test_message, &agent_signature.signature)
                .expect("Verification failed"),
            "Signature verification for AgentSignature failed"
        );
    }

    #[test]
    fn test_create_signed_expression_with_data_string() {
        ensure_setup();
        let json_value =
            serde_json::Value::String(r#"{"key2": "value1", "key1": "value2"}"#.to_string());
        let signed_expression = create_signed_expression(json_value, &AgentContext::main_agent())
            .expect("Failed to create signed expression");
        // Verify the expression with changed sorting
        assert!(
            signatures::verify(&signed_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression with string data should succeed"
        );
    }

    // Context-aware function tests

    #[test]
    fn test_agent_context_creation() {
        ensure_setup();
        let main_context = AgentContext::main_agent();
        assert!(main_context.is_main_agent());
        assert_eq!(main_context.managed_did(), None);
        assert_eq!(main_context.wallet_key_name().unwrap(), "main");

        let user_did = crate::test_utils::create_test_user("test@example.com");
        let user_context = AgentContext::managed(user_did.clone());
        assert!(!user_context.is_main_agent());
        assert_eq!(user_context.managed_did(), Some(user_did.as_str()));
        assert_eq!(
            user_context.wallet_key_name().unwrap(),
            user_did,
            "a managed agent's key is named by its DID"
        );
    }

    #[test]
    fn test_context_aware_functions_main_agent() {
        ensure_setup();
        let context = AgentContext::main_agent();

        // Test did_for_context
        let context_did = did_for_context(&context).expect("Failed to get DID for context");
        let static_did = did();
        assert_eq!(
            context_did, static_did,
            "Context-aware DID should match static DID"
        );

        // Test signing_key_id_for_context
        let context_key_id =
            signing_key_id_for_context(&context).expect("Failed to get signing key ID for context");
        let static_key_id = signing_key_id();
        assert_eq!(
            context_key_id, static_key_id,
            "Context-aware signing key ID should match static signing key ID"
        );

        // Test did_document_for_context
        let context_doc =
            did_document_for_context(&context).expect("Failed to get DID document for context");
        let static_doc = did_document();
        assert_eq!(
            context_doc.id, static_doc.id,
            "Context-aware DID document should match static DID document"
        );
    }

    #[test]
    fn test_context_aware_sign_and_verify() {
        ensure_setup();
        let context = AgentContext::main_agent();
        let test_payload = b"test message for context signing";

        // Test sign_for_context
        let context_signature =
            sign_for_context(test_payload, &context).expect("Failed to sign with context");
        let static_signature = sign(test_payload).expect("Failed to sign with static function");

        // Both should produce valid signatures (though they may be different due to randomness in some signature schemes)
        assert!(
            !context_signature.is_empty(),
            "Context signature should not be empty"
        );
        assert!(
            !static_signature.is_empty(),
            "Static signature should not be empty"
        );
    }

    #[test]
    fn test_context_aware_create_signed_expression() {
        ensure_setup();
        let context = AgentContext::main_agent();
        let test_data = json!({"test": "context_data"});

        // Test create_signed_expression_for_context
        let context_expr = create_signed_expression(test_data.clone(), &context)
            .expect("Failed to create signed expression with context");
        let static_expr = create_signed_expression(test_data, &AgentContext::main_agent())
            .expect("Failed to create signed expression with static function");

        // Both expressions should be valid
        assert!(
            signatures::verify(&context_expr).expect("Context expression verification failed"),
            "Context-aware expression should be valid"
        );
        assert!(
            signatures::verify(&static_expr).expect("Static expression verification failed"),
            "Static expression should be valid"
        );

        // Both should have the same author (main agent DID)
        assert_eq!(
            context_expr.author, static_expr.author,
            "Both expressions should have the same author"
        );
    }

    #[test]
    fn a_managed_agent_signs_only_with_a_key_that_derives_its_did() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        // A DID no key in the wallet derives, and no user owns.
        let unknown = AgentContext::managed(
            "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK".to_string(),
        );
        assert!(did_document_for_context(&unknown).is_err());
        assert!(signing_key_id_for_context(&unknown).is_err());
        assert!(sign_for_context(b"test", &unknown).is_err());
        assert!(create_signed_expression(json!({"test": "data"}), &unknown).is_err());

        let user_did = crate::test_utils::create_test_user("test.auto@example.com");
        let user_context = AgentContext::managed(user_did.clone());
        assert_eq!(
            did_document_for_context(&user_context).unwrap().id,
            user_did
        );
        assert!(signing_key_id_for_context(&user_context).is_ok());
        assert_eq!(did_for_context(&user_context).unwrap(), user_did);
        assert!(sign_for_context(b"test", &user_context).is_ok());
        let expression = create_signed_expression(json!({"test": "data"}), &user_context)
            .expect("Creating signed expression should succeed once key exists");
        assert_eq!(expression.author, user_did);
        assert!(signatures::verify(&expression).unwrap());
    }

    // User key management tests

    #[test]
    fn a_new_managed_agent_key_is_named_by_its_did() {
        ensure_setup();
        let email = "test.keygeneration@example.com";

        let did = AgentService::create_managed_agent_key(email).expect("key created");
        let backend = wallet_backend();
        assert_eq!(
            backend.get_did_document(&did).map(|doc| doc.id),
            Some(did.clone()),
            "the key is under its own DID"
        );
        assert!(
            backend.get_did_document(email).is_none(),
            "no key is named by the email"
        );

        // Each call makes a new agent: `create_user` refuses a taken email before it.
        let other = AgentService::create_managed_agent_key(email).expect("second key");
        assert_ne!(other, did);
    }

    #[test]
    fn an_email_resolves_to_its_users_agent_and_back() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let email = "test.agentdata@example.com";
        let did = crate::test_utils::create_test_user(email);

        assert!(did.starts_with("did:key:"));
        assert_eq!(AgentService::did_for_email(email).unwrap(), did);
        assert_eq!(AgentService::email_for_did(&did).unwrap(), email);
        assert_eq!(
            AgentService::managed_agent_for_email(email).unwrap(),
            AgentContext::managed(did.clone())
        );
        assert_eq!(AgentService::wallet_key_for_did(&did).unwrap(), did);

        assert!(AgentService::did_for_email("nobody@example.com").is_err());
        assert!(AgentService::email_for_did("did:key:nobody").is_err());
        assert!(AgentService::managed_agent_for_email("nobody@example.com").is_err());
    }

    #[test]
    fn managed_agents_are_the_users_dids() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let initial = AgentService::managed_agent_dids().unwrap();

        let did1 = crate::test_utils::create_test_user("user1.list@example.com");
        let did2 = crate::test_utils::create_test_user("user2.list@example.com");
        // A user without an agent is no agent.
        Ad4mDb::with_global_instance(|db| db.add_user("no.agent@example.com", "", "pw")).unwrap();

        let managed = AgentService::managed_agent_dids().unwrap();
        assert_eq!(managed.len(), initial.len() + 2, "{managed:?}");
        assert!(managed.contains(&did1));
        assert!(managed.contains(&did2));
        assert!(
            !managed.contains(&did()),
            "the main agent is not a managed agent"
        );
    }

    #[test]
    fn test_did_based_agent_context() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();

        let alice_email = "alice@example.com";
        let bob_email = "bob@example.com";
        let alice_did = crate::test_utils::create_test_user(alice_email);
        let bob_did = crate::test_utils::create_test_user(bob_email);
        assert_ne!(alice_did, bob_did);

        let alice_context = AgentService::managed_agent_for_email(alice_email).unwrap();
        let bob_context = AgentService::managed_agent_for_email(bob_email).unwrap();
        assert!(!alice_context.is_main_agent());
        assert!(!bob_context.is_main_agent());
        assert_eq!(alice_context, AgentContext::managed(alice_did.clone()));
        assert_eq!(bob_context, AgentContext::managed(bob_did.clone()));

        // Wallet key names are DIDs
        assert_eq!(alice_context.wallet_key_name().unwrap(), alice_did);
        assert_eq!(bob_context.wallet_key_name().unwrap(), bob_did);
        assert_eq!(did_for_context(&alice_context).unwrap(), alice_did);
        assert_eq!(did_for_context(&bob_context).unwrap(), bob_did);

        let test_data = json!({"message": "DID-based context test"});
        let alice_expr = create_signed_expression(test_data.clone(), &alice_context)
            .expect("Alice expression creation failed");
        let bob_expr = create_signed_expression(test_data.clone(), &bob_context)
            .expect("Bob expression creation failed");

        assert!(signatures::verify(&alice_expr).expect("Alice expression verification failed"));
        assert!(signatures::verify(&bob_expr).expect("Bob expression verification failed"));
        assert_eq!(alice_expr.author, alice_did);
        assert_eq!(bob_expr.author, bob_did);
        assert_ne!(alice_expr.proof.signature, bob_expr.proof.signature);

        let managed = AgentService::managed_agent_dids().unwrap();
        assert!(managed.contains(&alice_did));
        assert!(managed.contains(&bob_did));
    }

    #[test]
    fn test_multi_user_agent_system_integration() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();

        let main_context = AgentContext::main_agent();
        let alice_did = crate::test_utils::create_test_user("alice.integration@example.com");
        let bob_did = crate::test_utils::create_test_user("bob.integration@example.com");
        let alice_context = AgentContext::managed(alice_did.clone());
        let bob_context = AgentContext::managed(bob_did.clone());

        let main_did = did_for_context(&main_context).expect("Main agent DID failed");
        assert_ne!(main_did, alice_did);
        assert_ne!(main_did, bob_did);
        assert_ne!(alice_did, bob_did);

        let test_data =
            json!({"message": "Hello from multi-user system", "timestamp": "2024-01-01"});

        let main_expr = create_signed_expression(test_data.clone(), &main_context)
            .expect("Main agent expression creation failed");
        let alice_expr = create_signed_expression(test_data.clone(), &alice_context)
            .expect("Alice expression creation failed");
        let bob_expr = create_signed_expression(test_data.clone(), &bob_context)
            .expect("Bob expression creation failed");

        assert!(signatures::verify(&main_expr).expect("Main expression verification failed"));
        assert!(signatures::verify(&alice_expr).expect("Alice expression verification failed"));
        assert!(signatures::verify(&bob_expr).expect("Bob expression verification failed"));

        assert_eq!(main_expr.author, main_did);
        assert_eq!(alice_expr.author, alice_did);
        assert_eq!(bob_expr.author, bob_did);

        assert_ne!(main_expr.proof.signature, alice_expr.proof.signature);
        assert_ne!(main_expr.proof.signature, bob_expr.proof.signature);
        assert_ne!(alice_expr.proof.signature, bob_expr.proof.signature);

        let managed = AgentService::managed_agent_dids().unwrap();
        assert!(managed.contains(&alice_did));
        assert!(managed.contains(&bob_did));
        assert!(!managed.contains(&main_did));
    }

    #[test]
    fn test_user_key_isolation() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let user1_did = crate::test_utils::create_test_user("user1.isolation@example.com");
        let user2_did = crate::test_utils::create_test_user("user2.isolation@example.com");
        let user1_context = AgentContext::managed(user1_did.clone());
        let user2_context = AgentContext::managed(user2_did.clone());

        assert_ne!(user1_did, user2_did);

        let test_payload = b"isolation test message";
        let user1_signature =
            sign_for_context(test_payload, &user1_context).expect("User 1 signing failed");
        let user2_signature =
            sign_for_context(test_payload, &user2_context).expect("User 2 signing failed");

        assert_ne!(user1_signature, user2_signature);
        let user1_key = signing_key_id_for_context(&user1_context).unwrap();
        assert!(
            signatures::verify_string_signed_by_did(
                &user1_did,
                "isolation",
                &sign_string_hex_for_context("isolation".to_string(), &user1_context).unwrap()
            )
            .unwrap(),
            "{user1_key} signs as {user1_did}"
        );
    }

    #[test]
    fn test_user_did_persistence_across_save_load() {
        ensure_setup();
        let test_user_email = "persistence.test@example.com";
        let test_passphrase = "test_passphrase_123";

        // Create test directory for agent files
        let test_dir = "test_data/ad4m";
        std::fs::create_dir_all(test_dir).expect("Failed to create test directory");

        // Create the user and its agent
        let did_before_save = crate::test_utils::create_test_user(test_user_email);

        // Simulate unlock by storing passphrase
        AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.passphrase = Some(test_passphrase.to_string());
        });

        // Save the agent service (this should persist the wallet with the user key)
        AgentService::with_global_instance(|agent_service| {
            agent_service.save(test_passphrase.to_string());
        });

        // Simulate a restart by loading the agent service
        AgentService::with_mutable_global_instance(|agent_service| {
            agent_service.load();
            agent_service
                .unlock(test_passphrase.to_string())
                .expect("Failed to unlock after load");
        });

        // The key reloads under the DID and still derives it
        let did_after_reload = AgentService::managed_agent_for_email(test_user_email)
            .and_then(|agent| did_document_for_context(&agent))
            .expect("Failed to get DID after reload")
            .id;

        // Verify DIDs are the same
        assert_eq!(
            did_before_save, did_after_reload,
            "User DID should persist across save/load cycles. Before: {}, After: {}",
            did_before_save, did_after_reload
        );
    }

    /// Regression test: generate + save to disk, then clear in-memory state and
    /// verify that `ensure_main_agent_loaded` recovers from disk.
    #[test]
    fn get_agent_recovers_from_disk_after_memory_cleared() {
        ensure_setup();
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");

        let expected_did = {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();

            svc.create_new_keys();
            let did = svc
                .agent
                .as_ref()
                .expect("agent must exist after create_new_keys")
                .did
                .clone();

            svc.save("test-passphrase".to_string());

            assert!(
                std::path::Path::new(&format!("{}/ad4m/agent.json", app_path)).exists(),
                "agent.json must exist on disk after save()"
            );

            svc.agent = None;
            did
        };

        AgentService::with_global_instance(|svc| {
            assert!(svc.agent.is_none(), "in-memory agent should be None");
            assert!(svc.is_initialized(), "agent.json must still be on disk");
        });

        AgentService::with_mutable_global_instance(|svc| {
            svc.ensure_main_agent_loaded();
        });

        let recovered = AgentService::with_global_instance(|svc| svc.agent.clone());
        assert!(
            recovered.is_some(),
            "agent must be recovered from disk instead of remaining None"
        );

        let agent = recovered.unwrap();
        assert_eq!(
            agent.did, expected_did,
            "recovered agent must have the same DID as the generated one"
        );
        assert!(
            agent.perspective.is_some(),
            "recovered agent must have a perspective"
        );

        // Restore the global AgentService and re-sync with the wallet's
        // current "main" key. The test above replaced the "main" key via
        // create_new_keys(); re-initialising the agent re-generates the key
        // and syncs the DID, preventing mismatches for subsequent tests.
        setup_agent();
    }

    // ---- D16: managed agents' keys are named by their DID ----------------

    /// A wallet of its own, with a key named `name`; returns the key's DID.
    fn wallet_with_key(wallet: &crate::wallet::LocalWallet, name: &str) -> String {
        use crate::wallet::WalletBackend;
        wallet.generate_keypair(name).unwrap();
        wallet.get_did_document(name).unwrap().id
    }

    fn key_names(wallet: &dyn crate::wallet::WalletBackend) -> Vec<String> {
        let mut names = wallet.list_key_names();
        names.sort();
        names
    }

    /// Before D16 a user could sign up as `main` (or, on a shared wallet, `platform`) and was
    /// handed the executor's own key under that name, so the `users` row names the executor's
    /// DID. The migration never moves an executor key: it keeps its name.
    #[test]
    fn the_key_name_migration_never_moves_an_executor_key() {
        let wallet = crate::wallet::LocalWallet::new();
        let main = wallet_with_key(&wallet, crate::wallet::KEY_NAME_MAIN);
        let platform = wallet_with_key(&wallet, crate::wallet::KEY_NAME_PLATFORM);
        let users = vec![
            (crate::wallet::KEY_NAME_MAIN.to_string(), main),
            (crate::wallet::KEY_NAME_PLATFORM.to_string(), platform),
        ];

        let outcomes = migrate_managed_key_names(&wallet, &users, || Ok(()));

        assert_eq!(
            outcomes,
            vec![
                (
                    crate::wallet::KEY_NAME_MAIN.to_string(),
                    KeyNameMigration::ExecutorKey
                ),
                (
                    crate::wallet::KEY_NAME_PLATFORM.to_string(),
                    KeyNameMigration::ExecutorKey
                ),
            ]
        );
        assert_eq!(
            key_names(&wallet),
            vec![
                crate::wallet::KEY_NAME_MAIN.to_string(),
                crate::wallet::KEY_NAME_PLATFORM.to_string()
            ]
        );
    }

    #[test]
    fn the_key_name_migration_copies_persists_then_removes() {
        let wallet = crate::wallet::LocalWallet::new();
        let alice = wallet_with_key(&wallet, "alice@example.org");
        let users = vec![("alice@example.org".to_string(), alice.clone())];

        let mut seen_at_persist = Vec::new();
        let outcomes = migrate_managed_key_names(&wallet, &users, || {
            seen_at_persist.push(key_names(&wallet));
            Ok(())
        });

        assert_eq!(
            outcomes,
            vec![("alice@example.org".to_string(), KeyNameMigration::Renamed)]
        );
        assert_eq!(
            seen_at_persist.first(),
            Some(&vec!["alice@example.org".to_string(), alice.clone()]),
            "the copy reaches the disk while the old name still holds the key"
        );
        assert_eq!(key_names(&wallet), vec![alice.clone()], "the old name goes");
        assert_eq!(
            seen_at_persist.last(),
            Some(&vec![alice.clone()]),
            "the removal is persisted too"
        );
        assert_eq!(wallet_did(&wallet, &alice), Some(alice.clone()));

        // Idempotent: nothing to do, nothing written.
        let mut persisted_again = 0;
        let again = migrate_managed_key_names(&wallet, &users, || {
            persisted_again += 1;
            Ok(())
        });
        assert_eq!(
            again,
            vec![(
                "alice@example.org".to_string(),
                KeyNameMigration::AlreadyNamedByDid
            )]
        );
        assert_eq!(persisted_again, 0);
        assert_eq!(key_names(&wallet), vec![alice]);
    }

    fn wallet_did(wallet: &dyn crate::wallet::WalletBackend, name: &str) -> Option<String> {
        wallet.get_did_document(name).map(|doc| doc.id)
    }

    #[test]
    fn the_key_name_migration_finishes_after_a_crash_and_keeps_keys_it_cannot_vouch_for() {
        use crate::wallet::WalletBackend;
        let wallet = crate::wallet::LocalWallet::new();
        // Crashed after the copy was persisted: both names hold the same key.
        let bob = wallet_with_key(&wallet, "bob@example.org");
        wallet.copy_key("bob@example.org", &bob).unwrap();
        // A key under the old name that is not the user's: it derives another DID.
        let carol_key = wallet_with_key(&wallet, "carol@example.org");
        let carol = wallet_with_key(&wallet, "carol-agent");
        wallet.copy_key("carol-agent", &carol).unwrap();
        wallet.remove_key("carol-agent").unwrap();
        // The user's row names a DID the key under its email does not derive.
        let dave_key = wallet_with_key(&wallet, "dave@example.org");
        let dave = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK".to_string();
        let users = vec![
            ("bob@example.org".to_string(), bob.clone()),
            ("carol@example.org".to_string(), carol.clone()),
            ("dave@example.org".to_string(), dave.clone()),
            ("erin@example.org".to_string(), "did:key:erin".to_string()),
            ("frank@example.org".to_string(), String::new()),
        ];

        let outcomes = migrate_managed_key_names(&wallet, &users, || Ok(()));

        assert_eq!(
            outcomes,
            vec![
                (
                    "bob@example.org".to_string(),
                    KeyNameMigration::RemovedDuplicate
                ),
                (
                    "carol@example.org".to_string(),
                    KeyNameMigration::KeptOtherKey {
                        derived: carol_key.clone()
                    }
                ),
                (
                    "dave@example.org".to_string(),
                    KeyNameMigration::Mismatch {
                        derived: dave_key.clone()
                    }
                ),
                ("erin@example.org".to_string(), KeyNameMigration::NoKey),
            ]
        );
        assert_eq!(wallet_did(&wallet, &bob), Some(bob.clone()));
        assert!(!wallet.key_exists("bob@example.org"), "the duplicate goes");
        assert_eq!(
            wallet_did(&wallet, "carol@example.org"),
            Some(carol_key),
            "a key that is not the user's is never removed"
        );
        assert_eq!(
            wallet_did(&wallet, "dave@example.org"),
            Some(dave_key),
            "a mismatched key keeps its old name"
        );
        assert!(
            !wallet.key_exists(&dave),
            "nothing is named by a DID no key derives"
        );
    }

    #[test]
    fn a_failed_save_keeps_the_old_key_names() {
        let wallet = crate::wallet::LocalWallet::new();
        let alice = wallet_with_key(&wallet, "alice@example.org");
        let users = vec![("alice@example.org".to_string(), alice.clone())];

        migrate_managed_key_names(&wallet, &users, || Err(anyhow!("disk full")));

        assert_eq!(
            wallet_did(&wallet, "alice@example.org"),
            Some(alice.clone()),
            "without a saved copy the old name stays"
        );
    }

    /// An executor whose agent file still names a managed agent's key by the user's email.
    /// Returns the agent file's path. Leaves the global `AgentService` on `app_path`.
    fn executor_with_a_key_named_by_email(
        app_path: &str,
        passphrase: &str,
        email: &str,
        users_did: Option<&str>,
    ) -> (String, String) {
        let backend = wallet_backend();
        backend.generate_keypair(email).unwrap();
        let key_did = backend.get_did_document(email).unwrap().id;
        let did = users_did.map(str::to_string).unwrap_or(key_did);
        Ad4mDb::with_global_instance(|db| db.add_user(email, &did, "pw")).unwrap();
        let global = AgentService::global_instance();
        let mut lock = global.lock().unwrap();
        *lock = Some(AgentService::new(app_path.to_string()));
        let svc = lock.as_mut().unwrap();
        svc.create_new_keys();
        svc.save(passphrase.to_string());
        (format!("{}/ad4m/agent.json", app_path), did)
    }

    /// Unlocks the agent file on disk: the keys it holds replace the ones in memory.
    fn unlock_from_disk(passphrase: &str) {
        AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.unlock(passphrase.to_string()).expect("unlock");
        });
    }

    /// D16: restart on pre-migration data (a managed agent's key named by the user's email).
    /// The unlock names the key by the DID, removes the old name, saves both steps, and the
    /// user still signs as the same DID. A second unlock changes nothing.
    #[test]
    fn unlock_names_a_managed_agents_key_by_its_did() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let tmp = tempfile::tempdir().unwrap();
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).unwrap();
        let passphrase = "d16 migration";
        let email = format!("migrate.{}@example.org", uuid::Uuid::new_v4());
        let (agent_file, did) =
            executor_with_a_key_named_by_email(&app_path, passphrase, &email, None);
        let backend = wallet_backend();
        assert!(backend.key_exists(&email) && !backend.key_exists(&did));

        unlock_from_disk(passphrase);

        assert_eq!(wallet_did(backend.as_ref(), &did), Some(did.clone()));
        assert!(!backend.key_exists(&email), "the old name is gone");
        // What the unlock saved, read back from the agent file.
        let saved = std::fs::read_to_string(&agent_file).unwrap();
        unlock_from_disk(passphrase);
        assert_eq!(wallet_did(backend.as_ref(), &did), Some(did.clone()));
        assert!(
            !backend.key_exists(&email),
            "the agent file holds the new name only"
        );
        assert_eq!(
            std::fs::read_to_string(&agent_file).unwrap(),
            saved,
            "a second unlock writes nothing: every export draws a fresh salt"
        );

        let agent = AgentService::managed_agent_for_email(&email).unwrap();
        assert_eq!(agent, AgentContext::managed(did.clone()));
        let signed = create_signed_expression(json!({ "after": "migration" }), &agent).unwrap();
        assert_eq!(signed.author, did, "the user signs as the same DID");
        assert!(signatures::verify(&signed).unwrap());

        setup_agent();
    }

    /// D16: a key whose DID is not the user's keeps its old name, and the user's sessions
    /// fail with `NoIdentity` until an operator fixes it.
    #[test]
    fn unlock_keeps_a_mismatched_key_and_its_users_sessions_fail() {
        use crate::agent::capabilities::ListenerReach;
        use crate::agent::session::{AuthError, Session};
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let tmp = tempfile::tempdir().unwrap();
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).unwrap();
        let passphrase = "d16 mismatch";
        let email = format!("mismatch.{}@example.org", uuid::Uuid::new_v4());
        let recorded = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK";
        let (_, did) =
            executor_with_a_key_named_by_email(&app_path, passphrase, &email, Some(recorded));
        let backend = wallet_backend();
        let key_did = wallet_did(backend.as_ref(), &email).unwrap();
        assert_ne!(key_did, did);

        unlock_from_disk(passphrase);

        assert_eq!(
            wallet_did(backend.as_ref(), &email),
            Some(key_did),
            "the key keeps its old name"
        );
        assert!(!backend.key_exists(&did));
        let token = crate::user_management::generate_user_jwt(&email, "test").unwrap();
        let old_token =
            crate::test_utils::resigned_token(&token, |claims| claims["sub"] = json!(email));
        for token in [token, old_token] {
            let err = Session::open(&token, &None, ListenerReach::Network).unwrap_err();
            assert!(matches!(err, AuthError::NoIdentity(_)), "{err:?}");
        }

        setup_agent();
    }

    /// A user signed up as `main` before D16 has a `users` row naming the main agent's DID,
    /// whose key sits under the user's name. That user never acts as the main agent: the
    /// email fallback refuses an executor key, so the user's sessions fail with `NoIdentity`.
    /// No new user gets an executor key's name.
    #[test]
    fn a_user_named_like_an_executor_key_never_gets_that_key() {
        use crate::agent::capabilities::ListenerReach;
        use crate::agent::session::{AuthError, Session};
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let main_did = did();
        Ad4mDb::with_global_instance(|db| {
            db.add_user(crate::wallet::KEY_NAME_MAIN, &main_did, "pw")
        })
        .unwrap();
        forget_managed_key_names();

        assert!(AgentService::wallet_key_for_did(&main_did).is_err());
        assert!(AgentService::managed_agent_for_email(crate::wallet::KEY_NAME_MAIN).is_err());
        let token = crate::user_management::generate_user_jwt(crate::wallet::KEY_NAME_MAIN, "test")
            .unwrap();
        let err = Session::open(&token, &None, ListenerReach::Network).unwrap_err();
        assert!(matches!(err, AuthError::NoIdentity(_)), "{err:?}");

        for name in [
            crate::wallet::KEY_NAME_MAIN,
            crate::wallet::KEY_NAME_PLATFORM,
        ] {
            assert!(
                AgentService::create_managed_agent_key(name).is_err(),
                "{name}"
            );
        }
    }

    /// An agent file that an older executor wrote holds a legacy keystore. The first
    /// unlock rewrites it in the current format, and it still unlocks afterwards.
    #[test]
    fn unlock_rewrites_a_legacy_keystore_in_the_current_format() {
        ensure_setup();
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");
        let agent_file = format!("{}/ad4m/agent.json", app_path);
        let keystore_on_disk = || -> String {
            let store: serde_json::Value =
                serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
            store["keystore"].as_str().unwrap().to_string()
        };

        {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();
            svc.create_new_keys();
            svc.save("migration passphrase".to_string());
        }

        let mut store: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        store["keystore"] = serde_json::Value::String(crate::wallet::reencrypt_as_legacy(
            &keystore_on_disk(),
            "migration passphrase",
        ));
        std::fs::write(&agent_file, store.to_string()).unwrap();
        assert!(crate::wallet::is_legacy_keystore(&keystore_on_disk()));
        let legacy_file = std::fs::read(&agent_file).unwrap();

        AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            assert!(svc.unlock("wrong passphrase".to_string()).is_err());
            assert!(crate::wallet::is_legacy_keystore(&keystore_on_disk()));
            svc.unlock("migration passphrase".to_string())
                .expect("the legacy keystore unlocks");
        });
        assert!(!crate::wallet::is_legacy_keystore(&keystore_on_disk()));
        assert_eq!(
            std::fs::read(format!("{}.legacy", agent_file)).unwrap(),
            legacy_file,
            "the backup holds the legacy file byte for byte"
        );

        AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.unlock("migration passphrase".to_string())
                .expect("the rewritten keystore unlocks");
        });

        setup_agent();
    }

    /// When the rewrite cannot write (read-only or full disk), unlock still succeeds, the
    /// file keeps its legacy keystore, and the next unlock tries again.
    #[test]
    fn a_failed_keystore_rewrite_does_not_break_unlock() {
        ensure_setup();
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");
        let agent_file = format!("{}/ad4m/agent.json", app_path);

        {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();
            svc.create_new_keys();
            svc.save("rewrite passphrase".to_string());
        }
        let mut store: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        let current = store["keystore"].as_str().unwrap().to_string();
        store["keystore"] = serde_json::Value::String(crate::wallet::reencrypt_as_legacy(
            &current,
            "rewrite passphrase",
        ));
        std::fs::write(&agent_file, store.to_string()).unwrap();

        // A path whose parent is a regular file: no write can succeed there, even as root.
        let blocker = format!("{}/not-a-directory", app_path);
        std::fs::write(&blocker, "").unwrap();

        AgentService::with_mutable_global_instance(|svc| {
            svc.load();
            svc.file = format!("{}/agent.json", blocker);
            svc.unlock("rewrite passphrase".to_string())
                .expect("unlock succeeds although the rewrite fails");
            assert!(svc.legacy_keystore, "the next unlock should try again");
            svc.file = agent_file.clone();
        });
        let on_disk: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        assert!(crate::wallet::is_legacy_keystore(
            on_disk["keystore"].as_str().unwrap()
        ));

        setup_agent();
    }

    /// The legacy key ignored a trailing space, so `owner ` opens a legacy file written
    /// under `owner`. If that variant is the one that migrates the file, `owner` must still
    /// open it afterwards and yield the same keys.
    #[test]
    fn a_migration_with_a_trailing_space_does_not_lock_out_the_real_passphrase() {
        ensure_setup();
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");
        let agent_file = format!("{}/ad4m/agent.json", app_path);
        let backup_file = format!("{}.legacy", agent_file);
        let owner = "owner passphrase";
        let variant = "owner passphrase ";
        let keystore_on_disk = || -> String {
            let store: serde_json::Value =
                serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
            store["keystore"].as_str().unwrap().to_string()
        };

        {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();
            svc.create_new_keys();
            svc.save(owner.to_string());
        }
        let main_key = wallet_backend()
            .get_public_key(crate::wallet::KEY_NAME_MAIN)
            .expect("the main key exists");
        let mut store: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        store["keystore"] = serde_json::Value::String(crate::wallet::reencrypt_as_legacy(
            &keystore_on_disk(),
            owner,
        ));
        std::fs::write(&agent_file, store.to_string()).unwrap();

        // Each unlock starts from a locked backend, as after a restart.
        let restart_and_unlock = |password: &str| -> Result<(), AnyError> {
            wallet_backend().lock("scratch");
            AgentService::with_mutable_global_instance(|svc| {
                svc.load();
                svc.unlock(password.to_string())
            })
        };

        restart_and_unlock(variant).expect("the legacy key ignores a trailing space");
        assert!(!crate::wallet::is_legacy_keystore(&keystore_on_disk()));

        restart_and_unlock(owner).expect("the real passphrase still opens the migrated file");
        assert_eq!(
            wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN),
            Some(main_key.clone())
        );

        // The file is now bound to the real passphrase; opening it retires the backup.
        restart_and_unlock(owner).expect("the rewritten file opens under the real passphrase");
        assert!(!std::path::Path::new(&backup_file).exists());
        assert_eq!(
            wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN),
            Some(main_key)
        );
        assert!(restart_and_unlock(variant).is_err());

        setup_agent();
    }

    /// An agent file, under a fresh app path, whose keystore an older executor wrote under
    /// `passphrase`. Keep the temp dir alive while the test runs.
    fn legacy_agent_file(passphrase: &str) -> (tempfile::TempDir, String) {
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");
        let agent_file = format!("{}/ad4m/agent.json", app_path);
        {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();
            svc.create_new_keys();
            svc.save(passphrase.to_string());
        }
        let mut store: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        let current = store["keystore"].as_str().unwrap().to_string();
        store["keystore"] =
            serde_json::Value::String(crate::wallet::reencrypt_as_legacy(&current, passphrase));
        std::fs::write(&agent_file, store.to_string()).unwrap();
        (tmp, agent_file)
    }

    /// Unlocks from a locked backend and no stored passphrase, as after a restart.
    fn restart_and_unlock(password: &str) -> Result<(), AnyError> {
        wallet_backend().lock("scratch");
        AgentService::with_mutable_global_instance(|svc| {
            svc.passphrase = None;
            svc.load();
            svc.unlock(password.to_string())
        })
    }

    const OWNER: &str = "owner passphrase";
    /// Opens a legacy file written under `OWNER`: the legacy key ignored a trailing space.
    const VARIANT: &str = "owner passphrase ";

    /// Keys added after a migration under a variant passphrase exist only in the rewritten
    /// file. The real passphrase opens the legacy backup, which lacks them, so the fallback
    /// must refuse and change nothing.
    #[test]
    fn the_legacy_fallback_never_drops_keys_added_after_the_migration() {
        ensure_setup();
        let (_tmp, agent_file) = legacy_agent_file(OWNER);
        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        // A new user's key, saved the way `create_user` saves it.
        wallet_backend().generate_keypair("added").unwrap();
        AgentService::with_global_instance(|svc| svc.save(VARIANT.to_string()));
        let migrated = std::fs::read_to_string(&agent_file).unwrap();

        assert!(restart_and_unlock(OWNER).is_err());
        assert_eq!(std::fs::read_to_string(&agent_file).unwrap(), migrated);
        assert!(std::path::Path::new(&format!("{}.legacy", agent_file)).exists());
        assert!(!wallet_backend().is_unlocked());
        assert!(AgentService::with_global_instance(|svc| svc
            .passphrase
            .is_none()));

        restart_and_unlock(VARIANT).expect("the rewritten file opens under its passphrase");
        assert!(wallet_backend().get_public_key("added").is_some());

        setup_agent();
    }

    /// `agent.generate` on a migrated executor replaces the main key under the same name. The
    /// fingerprint covers public keys, so the fallback does not bring the old identity back.
    #[test]
    fn the_legacy_fallback_never_restores_a_replaced_key() {
        ensure_setup();
        let (_tmp, _agent_file) = legacy_agent_file(OWNER);
        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        wallet_backend()
            .generate_keypair(crate::wallet::KEY_NAME_MAIN)
            .unwrap();
        let new_main = wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN);
        AgentService::with_global_instance(|svc| svc.save(VARIANT.to_string()));

        assert!(restart_and_unlock(OWNER).is_err());
        restart_and_unlock(VARIANT).expect("the rewritten file opens under its passphrase");
        assert_eq!(
            wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN),
            new_main
        );

        setup_agent();
    }

    /// A save that adds no key, such as a profile update, keeps the fallback open.
    #[test]
    fn a_save_without_new_keys_keeps_the_legacy_fallback_open() {
        ensure_setup();
        let (_tmp, _agent_file) = legacy_agent_file(OWNER);
        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        let main_key = wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN);
        AgentService::with_global_instance(|svc| svc.save(VARIANT.to_string()));

        restart_and_unlock(OWNER).expect("the real passphrase still opens the executor");
        assert_eq!(
            wallet_backend().get_public_key(crate::wallet::KEY_NAME_MAIN),
            main_key
        );

        setup_agent();
    }

    /// D16 on a legacy keystore that names a managed agent's key by the user's email. The
    /// rewrite unlock keeps the key names, so the agent file still holds exactly the backup's
    /// keys and the fallback stays open. The unlock that retires the backup names the key by
    /// its DID.
    #[test]
    fn the_legacy_fallback_stays_open_until_the_backup_goes_then_keys_are_named_by_did() {
        ensure_setup();
        let _db = crate::test_utils::MultiUserMode::on();
        let email = format!("legacy.{}@example.org", uuid::Uuid::new_v4());
        let backend = wallet_backend();
        backend.generate_keypair(&email).unwrap();
        let user_did = wallet_did(backend.as_ref(), &email).unwrap();
        Ad4mDb::with_global_instance(|db| db.add_user(&email, &user_did, "pw")).unwrap();
        let (_tmp, agent_file) = legacy_agent_file(OWNER);
        let backup_file = format!("{}.legacy", agent_file);

        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        assert!(std::path::Path::new(&backup_file).exists());
        assert!(
            backend.key_exists(&email),
            "no rename while the backup exists"
        );

        restart_and_unlock(OWNER).expect("the real passphrase still opens the executor");
        assert!(std::path::Path::new(&backup_file).exists());
        assert!(backend.key_exists(&email));

        restart_and_unlock(OWNER).expect("the rewritten file opens under the real passphrase");
        assert!(!std::path::Path::new(&backup_file).exists());
        assert_eq!(
            wallet_did(backend.as_ref(), &user_did),
            Some(user_did.clone())
        );
        assert!(
            !backend.key_exists(&email),
            "named by its DID once the backup is gone"
        );

        setup_agent();
    }

    /// `agent.lock` saves under the passphrase that unlocked the executor but re-encrypts memory
    /// under the one it gets, so the next unlock opens memory, not the file. The backup goes
    /// only once the agent file opens with that passphrase too.
    #[test]
    fn the_backup_goes_only_once_the_agent_file_opens_with_the_passphrase() {
        ensure_setup();
        let (_tmp, agent_file) = legacy_agent_file(OWNER);
        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        AgentService::with_mutable_global_instance(|svc| {
            svc.lock(OWNER.to_string());
            svc.unlock(OWNER.to_string())
                .expect("memory opens under the lock passphrase");
        });
        assert!(!std::path::Path::new(&format!("{}.legacy", agent_file)).exists());

        restart_and_unlock(OWNER).expect("the agent file opens with the passphrase in use");

        setup_agent();
    }

    /// On an unlocked executor, a failed unlock never falls back: the backup's keys would
    /// replace a key in memory that the executor has not saved yet.
    #[test]
    fn an_unlocked_node_never_falls_back_to_the_legacy_backup() {
        ensure_setup();
        let (_tmp, _agent_file) = legacy_agent_file(OWNER);
        restart_and_unlock(VARIANT).expect("the variant opens the legacy file");
        wallet_backend().generate_keypair("not-saved-yet").unwrap();

        let result =
            AgentService::with_mutable_global_instance(|svc| svc.unlock(OWNER.to_string()));
        assert!(result.is_err());
        assert!(wallet_backend().get_public_key("not-saved-yet").is_some());

        setup_agent();
    }

    /// A save replaces the agent file through a temp file; the replacement keeps the mode
    /// an operator set on the original.
    #[cfg(unix)]
    #[test]
    fn save_keeps_the_agent_file_mode() {
        use std::os::unix::fs::PermissionsExt;
        ensure_setup();
        let tmp = tempfile::tempdir().expect("create temp dir");
        let app_path = tmp.path().to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");
        let agent_file = format!("{}/ad4m/agent.json", app_path);

        {
            let global = AgentService::global_instance();
            let mut lock = global.lock().unwrap();
            *lock = Some(AgentService::new(app_path.clone()));
            let svc = lock.as_mut().unwrap();
            svc.create_new_keys();
            svc.save("mode passphrase".to_string());
            std::fs::set_permissions(&agent_file, std::fs::Permissions::from_mode(0o640)).unwrap();
            svc.save("mode passphrase".to_string());
        }
        let mode = std::fs::metadata(&agent_file).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o640);

        setup_agent();
    }

    /// A legacy agent file under `OWNER` whose DID document carries the main key's private
    /// keys, as every agent file written before #1229 does. Returns the temp dir, the
    /// file, the private-key values and the legacy keystore string.
    fn leaking_legacy_agent_file() -> (tempfile::TempDir, String, Vec<String>, String) {
        let (tmp, agent_file) = legacy_agent_file(OWNER);
        let backend = wallet_backend();
        let public = backend
            .get_public_key(crate::wallet::KEY_NAME_MAIN)
            .unwrap();
        let secret = backend
            .get_secret_key(crate::wallet::KEY_NAME_MAIN)
            .unwrap();
        let secrets = crate::test_utils::private_key_values(&public, &secret);
        let leaking_document = {
            use did_key::DIDCore;
            let keypair =
                did_key::from_existing_key::<did_key::Ed25519KeyPair>(&public, Some(&secret));
            serde_json::to_string(&keypair.get_did_document(did_key::CONFIG_LD_PRIVATE)).unwrap()
        };
        let mut store: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&agent_file).unwrap()).unwrap();
        store["didDocument"] = json!(leaking_document);
        std::fs::write(&agent_file, store.to_string()).unwrap();
        let legacy_keystore = store["keystore"].as_str().unwrap().to_string();
        (tmp, agent_file, secrets, legacy_keystore)
    }

    fn stored(file: &str) -> (String, serde_json::Value) {
        let raw = std::fs::read_to_string(file).unwrap();
        let value = serde_json::from_str(&raw).unwrap();
        (raw, value)
    }

    /// #1229 and #1163 in one start: a file with both a leaking document and a legacy
    /// keystore. `load()` strips the document first, keeping the legacy keystore verbatim;
    /// the unlock then backs up the already-clean file and migrates the keystore. Neither
    /// file ends up with a private key, and the migrated file still unlocks.
    #[test]
    fn a_leaking_legacy_file_is_stripped_on_load_and_then_migrated_on_unlock() {
        ensure_setup();
        let (_tmp, agent_file, secrets, legacy_keystore) = leaking_legacy_agent_file();
        let backup_file = format!("{}.legacy", agent_file);

        wallet_backend().lock("scratch");
        let after_load = AgentService::with_mutable_global_instance(|svc| {
            svc.passphrase = None;
            svc.load();
            stored(&agent_file)
        });
        restart_and_unlock(OWNER).expect("the stripped legacy file unlocks");
        let migrated = stored(&agent_file);
        let backup = stored(&backup_file);
        let reopened = restart_and_unlock(OWNER);
        setup_agent();

        crate::test_utils::assert_no_private_keys(&after_load.0, &secrets, "agent.json after load");
        assert_eq!(
            after_load.1["keystore"],
            json!(legacy_keystore),
            "the strip must keep the legacy keystore verbatim"
        );
        crate::test_utils::assert_no_private_keys(
            &migrated.0,
            &secrets,
            "agent.json after migration",
        );
        assert!(!crate::wallet::is_legacy_keystore(
            migrated.1["keystore"].as_str().unwrap()
        ));
        crate::test_utils::assert_no_private_keys(&backup.0, &secrets, "the legacy backup");
        assert_eq!(
            backup.0, after_load.0,
            "the backup holds the stripped legacy file byte for byte"
        );
        reopened.expect("the migrated file unlocks");
    }

    /// An executor that ran #1163 before #1229 copied its leaking agent file to the legacy
    /// backup. `load()` cleans the backup too, keeping its keystore verbatim.
    #[test]
    fn load_strips_private_keys_from_an_existing_legacy_backup() {
        ensure_setup();
        let (_tmp, agent_file, secrets, legacy_keystore) = leaking_legacy_agent_file();
        let backup_file = format!("{}.legacy", agent_file);
        std::fs::copy(&agent_file, &backup_file).unwrap();

        wallet_backend().lock("scratch");
        AgentService::with_mutable_global_instance(|svc| {
            svc.passphrase = None;
            svc.load();
        });
        let backup = stored(&backup_file);
        let unlocked = restart_and_unlock(OWNER);
        setup_agent();

        crate::test_utils::assert_no_private_keys(
            &backup.0,
            &secrets,
            "the legacy backup after load",
        );
        assert_eq!(
            backup.1["keystore"],
            json!(legacy_keystore),
            "the strip must keep the backup's keystore verbatim"
        );
        unlocked.expect("the executor still unlocks");
    }
}

#[cfg(test)]
mod agent_context_tests {
    use super::{capabilities, AgentContext};
    use crate::agent::session::{AuthError, Session};
    use crate::test_utils::{expired_user_token, setup_agent, setup_wallet, MultiUserMode};

    const ADMIN: &str = "the-admin-credential";

    fn setup() {
        setup_wallet();
        setup_agent();
    }

    /// The agent a caller presenting `token` acts as, on an executor with an admin credential.
    fn acts_as(token: &str) -> Result<AgentContext, AuthError> {
        let admin = Some(crate::secret::Secret::from(ADMIN));
        Session::open(token, &admin, capabilities::ListenerReach::Network).map(|s| s.acts_as())
    }

    /// A login of `email`, whose agent gets a key and a `users` row first: a login's grant
    /// names its DID.
    fn login(email: &str) -> String {
        crate::test_utils::create_test_user(email);
        crate::user_management::generate_user_jwt(email, "test").unwrap()
    }

    // A token that no longer decoded used to act as the main agent.
    #[test]
    fn an_expired_user_token_acts_as_nobody() {
        setup();
        let _multi_user = MultiUserMode::on();
        let token = expired_user_token("expired@example.org");
        assert!(acts_as(&token).is_err());
    }

    #[test]
    fn an_unreadable_token_acts_as_nobody() {
        setup();
        let _multi_user = MultiUserMode::on();
        assert!(acts_as("not-a-jwt").is_err());
    }

    #[test]
    fn a_user_token_acts_as_its_user() {
        setup();
        let _multi_user = MultiUserMode::on();
        let did = crate::test_utils::create_test_user("alice@example.org");
        let context = acts_as(&login("alice@example.org")).unwrap();
        assert!(!context.is_main_agent());
        assert_eq!(context, AgentContext::managed(did));
    }

    #[test]
    fn the_admin_credential_and_the_empty_token_act_as_the_main_agent() {
        setup();
        let _multi_user = MultiUserMode::on();
        assert!(acts_as(ADMIN).unwrap().is_main_agent());
        assert!(acts_as("").unwrap().is_main_agent());
    }

    // A user token used to act as the main agent once multi-user mode read as off
    // (switched off, or a failed read of the setting).
    #[test]
    fn a_user_token_acts_as_its_user_when_multi_user_mode_is_off() {
        setup();
        let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        let did = crate::test_utils::create_test_user("alice@example.org");
        let context = acts_as(&login("alice@example.org")).unwrap();
        assert!(!context.is_main_agent());
        assert_eq!(context, AgentContext::managed(did));
    }

    #[test]
    fn a_revoked_token_acts_as_nobody() {
        setup();
        let token = login("revoked@example.org");
        assert!(acts_as(&token).is_ok());
        let grants = super::grants::Grants::global();
        grants.revoke(&grants.find(&token)).unwrap();
        assert_eq!(acts_as(&token).unwrap_err(), AuthError::Revoked);
    }

    #[test]
    fn only_the_agent_without_a_user_is_the_main_agent() {
        let managed = AgentContext::managed("did:key:z6MkManaged".to_string());
        assert!(!managed.is_main_agent());
        assert_eq!(managed.managed_did(), Some("did:key:z6MkManaged"));
        let main = AgentContext::main_agent();
        assert!(main.is_main_agent());
        assert_eq!(main.managed_did(), None);
        assert_eq!(
            main.wallet_key_name().unwrap(),
            crate::wallet::KEY_NAME_MAIN
        );
    }
}
