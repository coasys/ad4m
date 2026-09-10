use std::path;
use std::sync::{Arc, Mutex};

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

use crate::types::domain::Perspective;
use crate::types::{Agent, AgentStatus};
use crate::types::{Expression, ExpressionProof};
use crate::wallet::wallet_backend;

pub mod capabilities;
pub mod enrolment;
pub mod kel;
pub mod resolver;
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

/// Context for determining which agent to use for operations
#[derive(Debug, Clone, PartialEq)]
pub struct AgentContext {
    pub user_email: Option<String>, // The user's email for wallet key lookup
    pub is_main_agent: bool,
}

impl AgentContext {
    /// Create AgentContext from auth token string
    pub fn from_auth_token(auth_token: String) -> Self {
        let user_email = capabilities::user_email_from_token(auth_token);
        Self {
            is_main_agent: user_email.is_none(),
            user_email,
        }
    }

    /// Create AgentContext for main agent
    pub fn main_agent() -> Self {
        Self {
            user_email: None,
            is_main_agent: true,
        }
    }

    /// Create AgentContext for specific user
    pub fn for_user_email(user_email: String) -> Self {
        Self {
            user_email: Some(user_email),
            is_main_agent: false,
        }
    }

    /// Get the wallet key name for this context
    /// For users, this uses their email directly
    pub fn wallet_key_name(&self) -> String {
        match &self.user_email {
            Some(email) => email.clone(),
            None => crate::wallet::KEY_NAME_MAIN.to_string(),
        }
    }
}

/// Data for a specific agent (main or user)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentData {
    pub did: String,
    pub did_document: String,
    pub signing_key_id: String,
    pub wallet_key_name: String,
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
}

pub fn did_document_for_context(context: &AgentContext) -> Result<did_key::Document, AnyError> {
    if context.is_main_agent {
        let backend = wallet_backend();
        let key_name = context.wallet_key_name();
        backend
            .get_did_document(&key_name)
            .ok_or(anyhow!("{} key not found", key_name))
    } else if let Some(user_email) = &context.user_email {
        let agent_data = AgentService::get_user_agent_data(user_email)?;
        let did_doc: did_key::Document = serde_json::from_str(&agent_data.did_document)?;
        Ok(did_doc)
    } else {
        Err(anyhow!("Invalid user context"))
    }
}

pub fn signing_key_id_for_context(context: &AgentContext) -> Result<String, AnyError> {
    let did_doc = did_document_for_context(context)?;
    Ok(did_doc.verification_method[0].id.clone())
}

pub fn did_for_context(context: &AgentContext) -> Result<String, AnyError> {
    if context.is_main_agent {
        // For main agent, get from AgentService
        let did_result = AgentService::with_global_instance(|a| {
            a.did
                .clone()
                .ok_or(anyhow!("DID requested but not yet set in AgentService"))
        });
        did_result
    } else {
        if let Some(user_email) = &context.user_email {
            AgentService::get_user_did_by_email(user_email)
        } else {
            Err(anyhow!("Invalid user context"))
        }
    }
}

pub fn sign_for_context(payload: &[u8], context: &AgentContext) -> Result<Vec<u8>, AnyError> {
    let backend = wallet_backend();
    let key_name = context.wallet_key_name();
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
            ..Default::default()
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
        let signature = sign_string_hex(message)?;
        Ok(AgentSignature {
            signature,
            public_key: signing_key_id(),
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
}

lazy_static! {
    static ref AGENT_SERVICE: Arc<Mutex<Option<AgentService>>> = Arc::new(Mutex::new(None));
}

impl AgentService {
    pub fn init_global_instance(app_path: String) {
        // Install the identity resolver (KEL-backed did:scid verification).
        let kel_db_path = format!("{}/ad4m/kel.db", app_path);
        match kel::adapter::SqliteAdapter::open(&kel_db_path) {
            Ok(adapter) => {
                let resolver = resolver::AgentLanguageResolver::new(
                    Arc::new(adapter),
                    Arc::new(kel::adapter::MonotonicityCache::new()),
                    Arc::new(resolver::ReverseIndex::new()),
                );
                signatures::set_key_state_resolver(Arc::new(resolver));
                log::info!("Identity resolver installed (KEL at {})", kel_db_path);
            }
            Err(e) => {
                log::error!(
                    "Failed to open KEL database at {}: {} — did:scid verification disabled",
                    kel_db_path,
                    e
                );
            }
        }

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

    /// Ensure a user key exists in the wallet, generating it if necessary.
    /// Uses email as the wallet key name.
    pub fn ensure_user_key_exists(user_email: &str) -> Result<(), AnyError> {
        let backend = wallet_backend();

        let available_keys = backend.list_key_names();
        log::debug!(
            "🔧 ensure_user_key_exists() called for user: '{}'",
            user_email
        );
        log::debug!("🔧 Keys in wallet: {} total", available_keys.len());

        if backend.get_did_document(user_email).is_some() {
            log::debug!("✅ Key already exists for user: '{}'", user_email);
            return Ok(());
        }

        log::warn!(
            "⚠️  Key NOT found for user '{}', generating new key",
            user_email
        );
        log::warn!(
            "⚠️  This will create a NEW DID! {} keys in wallet",
            available_keys.len()
        );
        backend.get_or_create_keypair(user_email)?;

        Ok(())
    }

    /// Get user agent data for a specific user email. Fails if the user does not exist.
    pub fn get_user_agent_data(user_email: &str) -> Result<AgentData, AnyError> {
        let backend = wallet_backend();

        let available_keys = backend.list_key_names();
        log::trace!("🔍 get_user_agent_data() called for user: '{}'", user_email);
        log::trace!("🔍 Keys in wallet: {} total", available_keys.len());

        let did_document = backend.get_did_document(user_email).ok_or_else(|| {
            log::error!(
                "❌ No key found for user '{}'. {} keys in wallet",
                user_email,
                available_keys.len()
            );
            anyhow!("No key found for user {}", user_email)
        })?;

        let signing_key_id = did_document.verification_method[0].id.clone();
        let did = did_document.id.clone();

        log::trace!("✅ Found user key for '{}' with DID: {}", user_email, did);

        Ok(AgentData {
            did,
            did_document: serde_json::to_string(&did_document)?,
            signing_key_id,
            wallet_key_name: user_email.to_string(),
        })
    }

    /// Check whether a user key exists in the wallet.
    pub fn user_exists(user_email: &str) -> bool {
        let backend = wallet_backend();
        backend.key_exists(user_email)
    }

    /// List all user emails that have keys in the wallet (excluding the
    /// main agent key and platform signing key).
    pub fn list_user_emails() -> Result<Vec<String>, AnyError> {
        let backend = wallet_backend();
        let signing_name = crate::config::get_global_config().signing_key_name();
        let all_keys = backend.list_key_names();
        let user_emails: Vec<String> = all_keys
            .into_iter()
            .filter(|key_name| {
                key_name != crate::wallet::KEY_NAME_MAIN && key_name != &signing_name
            })
            .collect();
        Ok(user_emails)
    }

    /// Get DID for a user by email. Fails if the user does not exist.
    pub fn get_user_did_by_email(user_email: &str) -> Result<String, AnyError> {
        let agent_data = Self::get_user_agent_data(user_email)?;
        Ok(agent_data.did)
    }

    /// Store agent profile for a specific user
    pub fn store_user_agent_profile(
        &self,
        user_email: &str,
        agent: &Agent,
    ) -> Result<(), AnyError> {
        validate_user_email_for_path(user_email)?;
        // Create user-specific profile directory
        let user_profile_dir = format!("{}/{}", self.users_dir, user_email);
        std::fs::create_dir_all(&user_profile_dir)?;

        // Store profile in user-specific file
        let profile_path = format!("{}/profile.json", user_profile_dir);
        let profile_json = serde_json::to_string(agent)?;
        std::fs::write(profile_path, profile_json)?;

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
        match &context.user_email {
            Some(email) => {
                let agent =
                    AgentService::with_global_instance(|svc| svc.load_user_agent_profile(email))?;
                agent.ok_or_else(|| anyhow!("User profile not found for {}", email))
            }
            None => AgentService::with_mutable_global_instance(|svc| {
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
    pub fn load_user_agent_profile(&self, user_email: &str) -> Result<Option<Agent>, AnyError> {
        validate_user_email_for_path(user_email)?;
        let profile_path = format!("{}/{}/profile.json", self.users_dir, user_email);

        if !std::path::Path::new(&profile_path).exists() {
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

    pub fn unlock(&mut self, password: String) -> Result<(), AnyError> {
        let backend = wallet_backend();
        let result = backend.unlock(&password);
        if result.is_ok() {
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
        let backend = wallet_backend();
        let keystore = backend.export(&password);

        let store = AgentStore {
            did: self.did.clone().unwrap().clone(),
            did_document: self.did_document.clone().unwrap(),
            signing_key_id: self.signing_key_id.clone().unwrap(),
            keystore,
            agent: self.agent.clone(),
        };

        std::fs::write(self.file.as_str(), serde_json::to_string(&store).unwrap())
            .expect("Failed to write agent file");
    }

    pub fn load(&mut self) {
        if !self.is_initialized() {
            return;
        }

        let file = std::fs::read_to_string(self.file.as_str()).expect("Failed to read agent file");
        let dump: AgentStore = serde_json::from_str(&file).unwrap();

        self.did = Some(dump.did.clone());
        self.did_document = Some(dump.did_document);
        self.signing_key_id = Some(dump.signing_key_id);

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
        let main_context = AgentContext::main_agent();
        assert!(main_context.is_main_agent);
        assert_eq!(main_context.user_email, None);
        assert_eq!(main_context.wallet_key_name(), "main");

        let user_email = "test@example.com".to_string();
        let user_context = AgentContext::for_user_email(user_email.clone());
        assert!(!user_context.is_main_agent);
        assert_eq!(user_context.user_email, Some(user_email.clone()));
        assert_eq!(user_context.wallet_key_name(), user_email);
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
    fn test_user_context_auto_key_generation() {
        ensure_setup();
        let test_user_email = "test.auto@example.com";
        let user_context = AgentContext::for_user_email(test_user_email.to_string());

        // Before the key exists, operations should fail
        assert!(did_document_for_context(&user_context).is_err());
        assert!(signing_key_id_for_context(&user_context).is_err());
        assert!(did_for_context(&user_context).is_err());
        assert!(sign_for_context(b"test", &user_context).is_err());

        AgentService::ensure_user_key_exists(test_user_email).expect("Failed to create user key");

        assert!(did_document_for_context(&user_context).is_ok());
        assert!(signing_key_id_for_context(&user_context).is_ok());
        assert!(did_for_context(&user_context).is_ok());
        assert!(sign_for_context(b"test", &user_context).is_ok());
        assert!(
            create_signed_expression(json!({"test": "data"}), &user_context).is_ok(),
            "Creating signed expression should succeed once key exists"
        );
    }

    #[test]
    fn test_agent_context_from_auth_token() {
        // Test with empty token (main agent)
        let empty_token = String::new();
        let context = AgentContext::from_auth_token(empty_token);
        assert!(
            context.is_main_agent,
            "Empty token should result in main agent context"
        );
        assert!(
            context.user_email.is_none(),
            "Empty token should have no user email"
        );

        // Note: Full JWT token testing will be added in integration tests
        // since it requires more complex setup with JWT tokens and user creation
    }

    // User key management tests

    #[test]
    fn test_ensure_user_key_exists() {
        ensure_setup();
        let test_user_email = "test.keygeneration@example.com";

        // First call should generate the key
        let result = AgentService::ensure_user_key_exists(test_user_email);
        assert!(
            result.is_ok(),
            "First call to ensure_user_key_exists should succeed"
        );

        // Second call should be idempotent (key already exists)
        let result2 = AgentService::ensure_user_key_exists(test_user_email);
        assert!(
            result2.is_ok(),
            "Second call to ensure_user_key_exists should also succeed"
        );

        // Verify the key was actually created
        let backend = wallet_backend();
        let did_doc = backend.get_did_document(test_user_email);
        assert!(
            did_doc.is_some(),
            "User key should exist in wallet after generation"
        );
    }

    #[test]
    fn test_get_or_create_user_agent_data() {
        ensure_setup();
        let test_user_email = "test.agentdata@example.com";

        AgentService::ensure_user_key_exists(test_user_email).expect("Failed to create user key");

        let data = AgentService::get_user_agent_data(test_user_email)
            .expect("Failed to get user agent data");
        assert!(
            data.did.starts_with("did:key:"),
            "Agent data should have a valid DID"
        );
        assert!(
            !data.did_document.is_empty(),
            "Agent data should have a DID document"
        );
        assert!(
            !data.signing_key_id.is_empty(),
            "Agent data should have a signing key ID"
        );
        assert_eq!(
            data.wallet_key_name, test_user_email,
            "Wallet key name should match user email"
        );
    }

    #[test]
    fn test_list_user_emails() {
        ensure_setup();
        let test_user_email1 = "user1.list@example.com";
        let test_user_email2 = "user2.list@example.com";

        // Initially should only have main key
        let initial_users = AgentService::list_user_emails().unwrap_or_default();
        let initial_count = initial_users.len();

        // Add two user keys
        AgentService::ensure_user_key_exists(test_user_email1)
            .expect("Failed to create user 1 key");
        AgentService::ensure_user_key_exists(test_user_email2)
            .expect("Failed to create user 2 key");

        // Should now have at least 2 more users (other parallel tests may
        // have added keys to the shared wallet backend concurrently).
        let final_users = AgentService::list_user_emails().expect("Failed to list user emails");
        assert!(
            final_users.len() >= initial_count + 2,
            "Should have at least 2 more users after key generation, got {} (initial {})",
            final_users.len(),
            initial_count
        );

        assert!(
            final_users.contains(&test_user_email1.to_string()),
            "User list should contain test user 1"
        );
        assert!(
            final_users.contains(&test_user_email2.to_string()),
            "User list should contain test user 2"
        );

        // Should not contain "main"
        assert!(
            !final_users.contains(&"main".to_string()),
            "User list should not contain main agent"
        );
    }

    #[test]
    fn test_email_based_agent_context() {
        ensure_setup();

        // Test the new email-based approach
        let alice_email = "alice@example.com";
        let bob_email = "bob@example.com";

        // Keys do not exist initially
        let alice_context = AgentContext::for_user_email(alice_email.to_string());
        let bob_context = AgentContext::for_user_email(bob_email.to_string());

        assert!(did_for_context(&alice_context).is_err());
        assert!(did_for_context(&bob_context).is_err());

        AgentService::ensure_user_key_exists(alice_email).expect("Failed to create alice key");
        AgentService::ensure_user_key_exists(bob_email).expect("Failed to create bob key");

        let alice_context = AgentContext::for_user_email(alice_email.to_string());
        let bob_context = AgentContext::for_user_email(bob_email.to_string());

        assert!(
            !alice_context.is_main_agent,
            "Alice context should not be main agent"
        );
        assert!(
            !bob_context.is_main_agent,
            "Bob context should not be main agent"
        );
        assert_eq!(alice_context.user_email, Some(alice_email.to_string()));
        assert_eq!(bob_context.user_email, Some(bob_email.to_string()));

        // Verify wallet key names are emails
        assert_eq!(alice_context.wallet_key_name(), alice_email);
        assert_eq!(bob_context.wallet_key_name(), bob_email);

        // Generate DIDs
        let alice_did = did_for_context(&alice_context).expect("Alice DID generation failed");
        let bob_did = did_for_context(&bob_context).expect("Bob DID generation failed");

        assert_ne!(alice_did, bob_did);

        let test_data = json!({"message": "Email-based context test"});
        let alice_expr = create_signed_expression(test_data.clone(), &alice_context)
            .expect("Alice expression creation failed");
        let bob_expr = create_signed_expression(test_data.clone(), &bob_context)
            .expect("Bob expression creation failed");

        assert!(signatures::verify(&alice_expr).expect("Alice expression verification failed"));
        assert!(signatures::verify(&bob_expr).expect("Bob expression verification failed"));
        assert_eq!(alice_expr.author, alice_did);
        assert_eq!(bob_expr.author, bob_did);
        assert_ne!(alice_expr.proof.signature, bob_expr.proof.signature);

        let alice_data =
            AgentService::get_user_agent_data(alice_email).expect("Alice agent data failed");
        let bob_data = AgentService::get_user_agent_data(bob_email).expect("Bob agent data failed");

        assert_eq!(alice_data.wallet_key_name, alice_email);
        assert_eq!(bob_data.wallet_key_name, bob_email);

        let user_emails = AgentService::list_user_emails().expect("Failed to list user emails");
        assert!(user_emails.contains(&alice_email.to_string()));
        assert!(user_emails.contains(&bob_email.to_string()));
    }

    #[test]
    fn test_multi_user_agent_system_integration() {
        ensure_setup();

        let alice_email = "alice.integration@example.com";
        let bob_email = "bob.integration@example.com";
        let main_context = AgentContext::main_agent();

        AgentService::ensure_user_key_exists(alice_email).expect("Failed to create alice key");
        AgentService::ensure_user_key_exists(bob_email).expect("Failed to create bob key");

        let alice_context = AgentContext::for_user_email(alice_email.to_string());
        let bob_context = AgentContext::for_user_email(bob_email.to_string());

        let main_did = did_for_context(&main_context).expect("Main agent DID failed");
        let alice_did_result = did_for_context(&alice_context).expect("Alice DID failed");
        let bob_did_result = did_for_context(&bob_context).expect("Bob DID failed");

        assert_ne!(main_did, alice_did_result);
        assert_ne!(main_did, bob_did_result);
        assert_ne!(alice_did_result, bob_did_result);

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
        assert_eq!(alice_expr.author, alice_did_result);
        assert_eq!(bob_expr.author, bob_did_result);

        assert_ne!(main_expr.proof.signature, alice_expr.proof.signature);
        assert_ne!(main_expr.proof.signature, bob_expr.proof.signature);
        assert_ne!(alice_expr.proof.signature, bob_expr.proof.signature);

        let user_list = AgentService::list_user_emails().expect("Failed to list user emails");
        assert!(user_list.contains(&alice_email.to_string()));
        assert!(user_list.contains(&bob_email.to_string()));
        assert!(!user_list.contains(&"main".to_string()));

        let alice_data =
            AgentService::get_user_agent_data(alice_email).expect("Failed to get Alice agent data");
        let bob_data =
            AgentService::get_user_agent_data(bob_email).expect("Failed to get Bob agent data");

        assert_eq!(alice_data.did, alice_did_result);
        assert_eq!(bob_data.did, bob_did_result);
    }

    #[test]
    fn test_user_key_isolation() {
        ensure_setup();
        let user1_email = "user1.isolation@example.com";
        let user2_email = "user2.isolation@example.com";

        let user1_context = AgentContext::for_user_email(user1_email.to_string());
        let user2_context = AgentContext::for_user_email(user2_email.to_string());

        assert!(did_for_context(&user1_context).is_err());
        assert!(did_for_context(&user2_context).is_err());

        AgentService::ensure_user_key_exists(user1_email).expect("Failed to create user1 key");
        AgentService::ensure_user_key_exists(user2_email).expect("Failed to create user2 key");

        let user1_did_result =
            did_for_context(&user1_context).expect("User 1 DID generation failed");
        let user2_did_result =
            did_for_context(&user2_context).expect("User 2 DID generation failed");

        assert_ne!(user1_did_result, user2_did_result);

        let test_payload = b"isolation test message";
        let user1_signature =
            sign_for_context(test_payload, &user1_context).expect("User 1 signing failed");
        let user2_signature =
            sign_for_context(test_payload, &user2_context).expect("User 2 signing failed");

        assert_ne!(user1_signature, user2_signature);
    }

    #[test]
    fn test_user_did_persistence_across_save_load() {
        ensure_setup();
        let test_user_email = "persistence.test@example.com";
        let test_passphrase = "test_passphrase_123";

        // Create test directory for agent files
        let test_dir = "test_data/ad4m";
        std::fs::create_dir_all(test_dir).expect("Failed to create test directory");

        // Create user key
        AgentService::ensure_user_key_exists(test_user_email).expect("Failed to create user key");

        // Get the DID before save
        let did_before_save = AgentService::get_user_did_by_email(test_user_email)
            .expect("Failed to get DID before save");

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

        // Get the DID after reload
        let did_after_reload = AgentService::get_user_did_by_email(test_user_email)
            .expect("Failed to get DID after reload");

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
}
