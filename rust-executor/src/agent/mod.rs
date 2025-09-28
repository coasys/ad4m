use std::path;
use std::sync::{Arc, Mutex};

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

use crate::graphql::graphql_types::{Agent, AgentStatus, Perspective};
use crate::types::{Expression, ExpressionProof};
use crate::wallet::Wallet;

pub mod capabilities;
pub mod signatures;

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
            None => "main".to_string(),
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
    // For user contexts, ensure the key exists
    if context.is_main_agent {
        let wallet_instance = Wallet::instance();
        let wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_ref().expect("wallet instance");
        let key_name = context.wallet_key_name();

        wallet_ref
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
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let key_name = context.wallet_key_name();

    let signature = wallet_ref
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
    let wallet_instance = Wallet::instance();
    let mut wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_mut().expect("wallet instance");
    let name = "main".to_string();
    if wallet_ref.get_did_document(&name).is_none() {
        wallet_ref.initialize_keys(name, did).unwrap()
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

pub fn sign_string_hex(data: String) -> Result<String, AnyError> {
    let payload_bytes = signatures::hash_message(&data);
    let signature = sign_for_context(&payload_bytes, &AgentContext::main_agent())?;
    let sig_hex = hex::encode(signature);
    Ok(sig_hex)
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

impl From<AgentSignature> for crate::graphql::graphql_types::AgentSignature {
    fn from(val: AgentSignature) -> Self {
        crate::graphql::graphql_types::AgentSignature {
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
    pub agent: Option<Agent>,
}

lazy_static! {
    static ref AGENT_SERVICE: Arc<Mutex<Option<AgentService>>> = Arc::new(Mutex::new(None));
}

impl AgentService {
    pub fn init_global_instance(app_path: String) {
        let mut agent_instance = AGENT_SERVICE.lock().unwrap();
        *agent_instance = Some(AgentService::new(app_path));
    }

    pub fn init_global_test_instance() {
        let mut agent_instance = AGENT_SERVICE.lock().unwrap();
        *agent_instance = Some(AgentService {
            did: Some("did:key:z6Mkq9o619876543210".to_string()),
            did_document: None,
            file: "test".to_string(),
            file_profile: "test".to_string(),
            agent: None,
            signing_key_id: Some("did:key:z6Mkq9o619876543210".to_string()),
        });
    }

    pub fn new(app_path: String) -> AgentService {
        let agent_path = format!("{}/ad4m/agent.json", app_path);
        let agent_profile_path = format!("{}/ad4m/agentProfile.json", app_path);

        AgentService {
            did: None,
            did_document: None,
            file: agent_path,
            file_profile: agent_profile_path,
            agent: None,
            signing_key_id: None,
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
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref: &mut Wallet = wallet.as_mut().expect("wallet instance");
        wallet_ref.is_unlocked()
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
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_mut().expect("wallet instance");

        if wallet_ref
            .get_did_document(&user_email.to_string())
            .is_some()
        {
            return Ok(());
        }

        log::info!("Generating new key for user: {}", user_email);
        wallet_ref.generate_keypair(user_email.to_string());

        Ok(())
    }

    /// Get user agent data for a specific user email. Fails if the user does not exist.
    pub fn get_user_agent_data(user_email: &str) -> Result<AgentData, AnyError> {
        let wallet_instance = Wallet::instance();
        let wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_ref().expect("wallet instance");

        let did_document = wallet_ref
            .get_did_document(&user_email.to_string())
            .ok_or(anyhow!("No key found for user {}", user_email))?;

        let signing_key_id = did_document.verification_method[0].id.clone();

        Ok(AgentData {
            did: did_document.id.clone(),
            did_document: serde_json::to_string(&did_document)?,
            signing_key_id,
            wallet_key_name: user_email.to_string(),
        })
    }

    /// Check whether a user key exists in the wallet.
    pub fn user_exists(user_email: &str) -> bool {
        let wallet_instance = Wallet::instance();
        if let Ok(wallet) = wallet_instance.lock() {
            if let Some(wallet_ref) = wallet.as_ref() {
                return wallet_ref
                    .get_did_document(&user_email.to_string())
                    .is_some();
            }
        }
        false
    }

    /// List all user emails that have keys in the wallet (excluding "main")
    pub fn list_user_emails() -> Result<Vec<String>, AnyError> {
        let wallet_instance = Wallet::instance();
        let wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_ref().expect("wallet instance");

        let all_keys = wallet_ref.list_key_names();
        let user_emails: Vec<String> = all_keys
            .into_iter()
            .filter(|key_name| key_name != "main")
            .collect();

        Ok(user_emails)
    }

    /// Get DID for a user by email. Fails if the user does not exist.
    pub fn get_user_did_by_email(user_email: &str) -> Result<String, AnyError> {
        let agent_data = Self::get_user_agent_data(user_email)?;
        Ok(agent_data.did)
    }

    /// Store agent profile for a specific user
    pub fn store_user_agent_profile(user_email: &str, agent: &Agent) -> Result<(), AnyError> {
        // Create user-specific profile directory
        let app_data_path = std::env::var("APP_DATA_PATH").unwrap_or_else(|_| {
            std::env::current_dir()
                .unwrap()
                .to_string_lossy()
                .to_string()
        });
        let user_profile_dir = format!("{}/ad4m/users/{}", app_data_path, user_email);
        std::fs::create_dir_all(&user_profile_dir)?;

        // Store profile in user-specific file
        let profile_path = format!("{}/profile.json", user_profile_dir);
        let profile_json = serde_json::to_string(agent)?;
        std::fs::write(profile_path, profile_json)?;

        Ok(())
    }

    /// Publish agent to the agent language (decentralized storage)
    pub async fn publish_user_agent_to_language(user_email: &str, agent: &Agent, js_handle: &mut crate::js_core::JsCoreHandle) -> Result<(), AnyError> {
        // Create a context-aware agent service that can sign with the user's key
        // We need to inject this into the language context temporarily
        let agent_json = serde_json::to_string(agent)?;
        
        let script = format!(
            r#"
            (async () => {{
                // Get the agent language
                const agentLanguage = core.languageController.getAgentLanguage();
                if (!agentLanguage || !agentLanguage.expressionAdapter) {{
                    throw new Error("Agent language not available");
                }}
                
                const userAgent = {};
                
                // Set the agent service context to this user temporarily
                // The agent language uses the same agent service instance from core
                const originalContext = core.agentService.getUserContext();
                core.agentService.setUserContext("{}");
                
                try {{
                    // Get the putAdapter like in storeAgentProfile()
                    const putAdapter = agentLanguage.expressionAdapter.putAdapter;
                    if (!putAdapter) {{
                        throw new Error("No putAdapter found in agent language");
                    }}
                    
                    // Now call createPublic - the agent service will return the user's DID and sign with user's key
                    await putAdapter.createPublic(userAgent);
                    
                    return "success";
                }} finally {{
                    // Restore the original context
                    core.agentService.setUserContext(originalContext);
                }}
            }})()
            "#,
            agent_json, user_email
        );
        
        let result = js_handle.execute(script).await;
        
        match result {
            Ok(_) => {
                log::info!("Successfully published agent {} to agent language", agent.did);
                Ok(())
            }
            Err(e) => {
                log::error!("Failed to publish agent {} to agent language: {}", agent.did, e);
                Err(anyhow!("Failed to publish agent to language: {}", e))
            }
        }
    }

    /// Load agent profile for a specific user
    pub fn load_user_agent_profile(user_email: &str) -> Result<Option<Agent>, AnyError> {
        let app_data_path = std::env::var("APP_DATA_PATH").unwrap_or_else(|_| {
            std::env::current_dir()
                .unwrap()
                .to_string_lossy()
                .to_string()
        });
        let profile_path = format!("{}/ad4m/users/{}/profile.json", app_data_path, user_email);

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

        // TODO: once language controller is moved add updating agent profile here
    }

    pub fn save_agent_profile(&mut self, agent: Agent) {
        self.agent = Some(agent);
        self.store_agent_profile();
    }

    pub fn create_new_keys(&mut self) {
        let wallet_instance = Wallet::instance();
        let did = {
            let mut wallet = wallet_instance.lock().expect("wallet lock");
            let wallet_ref: &mut Wallet = wallet.as_mut().expect("wallet instance");
            wallet_ref.generate_keypair("main".to_string());
            wallet_ref
                .get_did_document(&"main".to_string())
                .expect("couldn't get DID document for keys that were just generated above")
                .id
        };

        self.did_document = Some(serde_json::to_string(&did_document()).unwrap());
        self.did = Some(did.clone());
        self.agent = Some(Agent {
            did,
            perspective: Some(Perspective { links: vec![] }),
            direct_message_language: None,
        });
        self.signing_key_id = Some(signing_key_id());
    }

    pub fn unlock(&self, password: String) -> Result<(), AnyError> {
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref: &mut Wallet = wallet.as_mut().expect("wallet instance");
        wallet_ref.unlock(password)
    }

    pub fn lock(&self, password: String) {
        let wallet_instance = Wallet::instance();
        {
            let mut wallet = wallet_instance.lock().expect("wallet lock");
            let wallet_ref: &mut Wallet = wallet.as_mut().expect("wallet instance");
            wallet_ref.lock(password);
        }
    }

    pub fn save(&self, password: String) {
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_mut().expect("wallet instance");

        let keystore = wallet_ref.export(password);

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
            let wallet_instance = Wallet::instance();
            let mut wallet = match wallet_instance.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            let wallet_ref = wallet.as_mut().expect("wallet instance");
            wallet_ref.load(dump.keystore);
        }

        if std::path::Path::new(self.file_profile.as_str()).exists() {
            let file_profile = std::fs::read_to_string(self.file_profile.as_str())
                .expect("Failed to read agent profile file");
            self.agent =
                Some(serde_json::from_str(&file_profile).expect("Failed to parse agent profile"));
        } else {
            let did_clone = dump.did.clone();
            let did = check_keys_and_create(did_clone).id.clone();

            self.agent = Some(Agent {
                did,
                perspective: Some(Perspective { links: vec![] }),
                direct_message_language: None,
            });
        }
    }

    pub fn dump(&self) -> AgentStatus {
        let did_document_str = self.did_document.as_ref().map(|doc| {
            serde_json::to_string(doc).unwrap_or_default()
        });

        AgentStatus {
            did: self.did.clone(),
            did_document: did_document_str,
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
        let wallet_instance = Wallet::instance();
        let wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_ref().expect("wallet instance");

        let did_doc = wallet_ref.get_did_document(&test_user_email.to_string());
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

        // Should now have 2 more users
        let final_users = AgentService::list_user_emails().expect("Failed to list user emails");
        assert_eq!(
            final_users.len(),
            initial_count + 2,
            "Should have 2 more users after key generation"
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
}
