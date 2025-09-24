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
    pub user_did: Option<String>,
    pub is_main_agent: bool,
}

impl AgentContext {
    /// Create AgentContext from auth token string
    pub fn from_auth_token(auth_token: String) -> Self {
        let user_did = capabilities::user_did_from_token(auth_token);
        Self {
            is_main_agent: user_did.is_none(),
            user_did,
        }
    }

    /// Create AgentContext for main agent
    pub fn main_agent() -> Self {
        Self {
            user_did: None,
            is_main_agent: true,
        }
    }

    /// Create AgentContext for specific user
    pub fn for_user(user_did: String) -> Self {
        Self {
            user_did: Some(user_did),
            is_main_agent: false,
        }
    }

    /// Get the wallet key name for this context
    pub fn wallet_key_name(&self) -> String {
        match &self.user_did {
            Some(did) => did.clone(),
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

pub fn did_document() -> did_key::Document {
    did_document_for_context(&AgentContext::main_agent())
        .expect("Failed to get did_document for main agent")
}

pub fn signing_key_id() -> String {
    signing_key_id_for_context(&AgentContext::main_agent())
        .expect("Failed to get signing_key_id for main agent")
}

pub fn did() -> String {
    did_for_context(&AgentContext::main_agent())
        .expect("Failed to get did for main agent")
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

pub fn create_signed_expression<T: Serialize>(data: T) -> Result<Expression<T>, AnyError> {
    create_signed_expression_for_context(data, &AgentContext::main_agent())
}

pub fn sign(payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    sign_for_context(payload, &AgentContext::main_agent())
}

// Context-aware agent functions

pub fn did_document_for_context(context: &AgentContext) -> Result<did_key::Document, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let key_name = context.wallet_key_name();
    
    wallet_ref
        .get_did_document(&key_name)
        .ok_or(anyhow!("{} key not found. call createMainKey() first", key_name))
}

pub fn signing_key_id_for_context(context: &AgentContext) -> Result<String, AnyError> {
    let did_doc = did_document_for_context(context)?;
    Ok(did_doc.verification_method[0].id.clone())
}

pub fn did_for_context(context: &AgentContext) -> Result<String, AnyError> {
    if context.is_main_agent {
        // For main agent, get from AgentService
        let did_result = AgentService::with_global_instance(|a| {
            a.did.clone().ok_or(anyhow!("DID requested but not yet set in AgentService"))
        });
        did_result
    } else {
        // For user agents, get from did document
        let did_doc = did_document_for_context(context)?;
        Ok(did_doc.id)
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

pub fn create_signed_expression_for_context<T: Serialize>(
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
    let signature = sign(&payload_bytes)?;
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
    ) -> Result<Expression<T>, AnyError> {
        self.signing_checks()?;

        create_signed_expression(data)
    }

    pub fn sign_string_hex(&self, data: String) -> Result<String, AnyError> {
        self.signing_checks()?;

        sign_string_hex(data)
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
        AgentStatus {
            did: self.did.clone(),
            did_document: self.did_document.clone(),
            is_initialized: self.is_initialized(),
            is_unlocked: self.is_unlocked(),
            error: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use serde_json::json;

    use super::*;
    use crate::agent::signatures::verify_string_signed_by_did;
    use crate::test_utils::{setup_agent, setup_wallet};
    use itertools::Itertools;

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
        let signed_expression = create_signed_expression(json!({"test": "data"}))
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

    //#[test]
    fn _test_create_signed_expression_and_verify_with_changed_sorting() {
        ensure_setup();
        let json_value = json!({"key2": "value1", "key1": "value2"});
        let signed_expression =
            create_signed_expression(json_value).expect("Failed to create signed expression");

        // Simulate changing the sorting of the JSON in the signed expression
        let mut data_map = BTreeMap::new();
        let sorted_keys = signed_expression.data.as_object().unwrap().keys().sorted();
        for key in sorted_keys {
            data_map.insert(key.clone(), signed_expression.data[key].clone());
        }
        let sorted_json = json!(data_map);
        let mut sorted_expression = signed_expression.clone();
        sorted_expression.data = sorted_json;

        // Verify the expression with changed sorting
        assert!(
            signatures::verify(&sorted_expression).expect("Verification failed"),
            "Signature verification for create_signed_expression with changed sorting should succeed"
        );
    }

    #[test]
    fn test_create_signed_expression_with_data_string() {
        ensure_setup();
        let json_value =
            serde_json::Value::String(r#"{"key2": "value1", "key1": "value2"}"#.to_string());
        let signed_expression =
            create_signed_expression(json_value).expect("Failed to create signed expression");
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
        assert_eq!(main_context.user_did, None);
        assert_eq!(main_context.wallet_key_name(), "main");

        let user_did = "did:key:z6MkhaXgBZDvotDkL5257faiztiGiC2QtKLGpbnnEGta2doK".to_string();
        let user_context = AgentContext::for_user(user_did.clone());
        assert!(!user_context.is_main_agent);
        assert_eq!(user_context.user_did, Some(user_did.clone()));
        assert_eq!(user_context.wallet_key_name(), user_did);
    }

    #[test]
    fn test_context_aware_functions_main_agent() {
        ensure_setup();
        let context = AgentContext::main_agent();

        // Test did_for_context
        let context_did = did_for_context(&context).expect("Failed to get DID for context");
        let static_did = did();
        assert_eq!(context_did, static_did, "Context-aware DID should match static DID");

        // Test signing_key_id_for_context
        let context_key_id = signing_key_id_for_context(&context).expect("Failed to get signing key ID for context");
        let static_key_id = signing_key_id();
        assert_eq!(context_key_id, static_key_id, "Context-aware signing key ID should match static signing key ID");

        // Test did_document_for_context
        let context_doc = did_document_for_context(&context).expect("Failed to get DID document for context");
        let static_doc = did_document();
        assert_eq!(context_doc.id, static_doc.id, "Context-aware DID document should match static DID document");
    }

    #[test]
    fn test_context_aware_sign_and_verify() {
        ensure_setup();
        let context = AgentContext::main_agent();
        let test_payload = b"test message for context signing";

        // Test sign_for_context
        let context_signature = sign_for_context(test_payload, &context).expect("Failed to sign with context");
        let static_signature = sign(test_payload).expect("Failed to sign with static function");
        
        // Both should produce valid signatures (though they may be different due to randomness in some signature schemes)
        assert!(!context_signature.is_empty(), "Context signature should not be empty");
        assert!(!static_signature.is_empty(), "Static signature should not be empty");
    }

    #[test]
    fn test_context_aware_create_signed_expression() {
        ensure_setup();
        let context = AgentContext::main_agent();
        let test_data = json!({"test": "context_data"});

        // Test create_signed_expression_for_context
        let context_expr = create_signed_expression_for_context(test_data.clone(), &context)
            .expect("Failed to create signed expression with context");
        let static_expr = create_signed_expression(test_data)
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
        assert_eq!(context_expr.author, static_expr.author, "Both expressions should have the same author");
    }

    #[test]
    fn test_user_context_with_nonexistent_key() {
        ensure_setup();
        let fake_user_did = "did:key:z6MkfakeUserDidThatDoesNotExist".to_string();
        let user_context = AgentContext::for_user(fake_user_did);

        // These should fail because the user key doesn't exist in the wallet
        assert!(
            did_document_for_context(&user_context).is_err(),
            "Getting DID document for nonexistent user should fail"
        );
        assert!(
            signing_key_id_for_context(&user_context).is_err(),
            "Getting signing key ID for nonexistent user should fail"
        );
        assert!(
            did_for_context(&user_context).is_err(),
            "Getting DID for nonexistent user should fail"
        );
        assert!(
            sign_for_context(b"test", &user_context).is_err(),
            "Signing for nonexistent user should fail"
        );
        assert!(
            create_signed_expression_for_context(json!({"test": "data"}), &user_context).is_err(),
            "Creating signed expression for nonexistent user should fail"
        );
    }

    #[test]
    fn test_agent_context_from_auth_token() {
        // Test with empty token (main agent)
        let empty_token = String::new();
        let context = AgentContext::from_auth_token(empty_token);
        assert!(context.is_main_agent, "Empty token should result in main agent context");
        assert!(context.user_did.is_none(), "Empty token should have no user DID");
        
        // Note: Full JWT token testing will be added in integration tests
        // since it requires more complex setup with JWT tokens and user creation
    }
}
