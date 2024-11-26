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
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    wallet_ref
        .get_did_document(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))
        .unwrap()
}

pub fn signing_key_id() -> String {
    did_document().verification_method[0].id.clone()
}

pub fn did() -> String {
    AgentService::with_global_instance(|a| {
        a.did.clone()
    }).expect("DID requested but not yet set in AgentService")
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
    let timestamp = chrono::Utc::now();
    let signature = hex::encode(sign(&signatures::hash_data_and_timestamp(
        &data, &timestamp,
    ))?);

    Ok(Expression {
        author: did(),
        timestamp: timestamp.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
        data,
        proof: ExpressionProof {
            signature,
            key: signing_key_id(),
        },
    })
}

pub fn sign(payload: &[u8]) -> Result<Vec<u8>, AnyError> {
    let wallet_instance = Wallet::instance();
    let wallet = wallet_instance.lock().expect("wallet lock");
    let wallet_ref = wallet.as_ref().expect("wallet instance");
    let name = "main".to_string();
    let signature = wallet_ref
        .sign(&name, payload)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;
    Ok(signature)
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
        let agent_service_ref = agent_service_lock.as_ref().expect("Ad4mDb not initialized");
        func(agent_service_ref)
    }

    pub fn with_mutable_global_instance<F, R>(func: F) -> R
    where
        F: FnOnce(&mut AgentService) -> R,
    {
        let global_instance_arc = AgentService::global_instance();
        let lock_result = global_instance_arc.lock();
        let mut agent_service_lock = lock_result.expect("Couldn't get lock on Ad4mDb");
        let agent_service_mut = agent_service_lock.as_mut().expect("Ad4mDb not initialized");
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
        {
            let mut wallet = wallet_instance.lock().expect("wallet lock");
            let wallet_ref: &mut Wallet = wallet.as_mut().expect("wallet instance");
            wallet_ref.generate_keypair("main".to_string());
        }

        self.did_document = Some(serde_json::to_string(&did_document()).unwrap());
        self.did = Some(did());
        self.agent = Some(Agent {
            did: did(),
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
    use crate::test_utils::setup_wallet;
    use itertools::Itertools;

    #[test]
    fn test_sign_and_verify_string_hex_roundtrip() {
        setup_wallet();
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
        setup_wallet();
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
        setup_wallet();
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
        setup_wallet();
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
        setup_wallet();
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
}
