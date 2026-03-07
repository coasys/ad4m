use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use std::sync::Mutex;
use once_cell::sync::Lazy;

// Global in-memory storage for agent expressions (stored as JSON strings)
static AGENT_STORE: Lazy<Mutex<HashMap<String, String>>> = Lazy::new(|| {
    Mutex::new(HashMap::new())
});

// Context storage (set during init)
static CONTEXT: Lazy<Mutex<Option<LanguageContext>>> = Lazy::new(|| {
    Mutex::new(None)
});

/// Language metadata - exported for AD4M runtime
#[wasm_bindgen]
pub fn name() -> String {
    "agent-expression-store-wasm".to_string()
}

/// Version of the language
#[wasm_bindgen]
pub fn version() -> String {
    "0.1.0".to_string()
}

/// Capabilities this language provides
#[wasm_bindgen]
pub fn capabilities() -> JsValue {
    let caps = vec!["expression-storage"];
    serde_wasm_bindgen::to_value(&caps).unwrap()
}

/// Initialize the language with context
#[wasm_bindgen]
pub fn init(context: JsValue) -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    
    let ctx: LanguageContext = serde_wasm_bindgen::from_value(context)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse context: {:?}", e)))?;
    
    let mut global_ctx = CONTEXT.lock().unwrap();
    *global_ctx = Some(ctx);
    
    web_sys::console::log_1(&"Agent language WASM initialized".into());
    Ok(())
}

/// Get an agent expression by DID (address)
#[wasm_bindgen]
pub async fn expression_get(address: String) -> Result<JsValue, JsValue> {
    let store = AGENT_STORE.lock().unwrap();
    
    match store.get(&address) {
        Some(expr_json) => {
            // Parse the JSON string back to JsValue
            let expr: serde_json::Value = serde_json::from_str(expr_json)
                .map_err(|e| JsValue::from_str(&format!("Failed to parse expression: {:?}", e)))?;
            Ok(serde_wasm_bindgen::to_value(&expr).unwrap())
        },
        None => Err(JsValue::from_str(&format!("Agent expression not found: {}", address)))
    }
}

/// Create a new agent expression
#[wasm_bindgen]
pub async fn expression_create_public(content: JsValue) -> Result<String, JsValue> {
    let ctx = CONTEXT.lock().unwrap();
    let context = ctx.as_ref()
        .ok_or_else(|| JsValue::from_str("Language not initialized"))?;
    
    // Parse the content as an Agent object
    let agent: Agent = serde_wasm_bindgen::from_value(content)
        .map_err(|e| JsValue::from_str(&format!("Invalid agent content: {:?}", e)))?;
    
    // Validate the agent
    if agent.did.is_empty() {
        return Err(JsValue::from_str("Agent must have a DID"));
    }
    
    // Check that we're creating for the local agent
    if agent.did != context.agent.did {
        return Err(JsValue::from_str(
            "Can't set Agent Expression for foreign DID - only for self"
        ));
    }
    
    // Create signed expression
    let expression = create_signed_expression(&agent, context)?;
    
    // Store it as JSON string
    let address = agent.did.clone();
    let expr_json = serde_json::to_string(&expression)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize expression: {:?}", e)))?;
    
    {
        let mut store = AGENT_STORE.lock().unwrap();
        store.insert(address.clone(), expr_json);
    }
    
    web_sys::console::log_1(&format!("Created agent expression for DID: {}", address).into());
    
    Ok(address)
}

/// Calculate address from content (for read-only languages)
#[wasm_bindgen]
pub fn expression_address_of(content: JsValue) -> Result<String, JsValue> {
    let agent: Agent = serde_wasm_bindgen::from_value(content)
        .map_err(|e| JsValue::from_str(&format!("Invalid agent content: {:?}", e)))?;
    
    Ok(agent.did)
}

/// Teardown/cleanup
#[wasm_bindgen]
pub fn teardown() {
    let mut store = AGENT_STORE.lock().unwrap();
    store.clear();
    
    let mut ctx = CONTEXT.lock().unwrap();
    *ctx = None;
    
    web_sys::console::log_1(&"Agent language WASM torn down".into());
}

/// Returns empty array (no interactions for this language)
#[wasm_bindgen]
pub fn interactions(_expression: String) -> JsValue {
    let empty: Vec<String> = vec![];
    serde_wasm_bindgen::to_value(&empty).unwrap()
}

// ============================================
// Internal types and functions
// ============================================

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LanguageContext {
    agent: AgentContext,
    #[serde(rename = "storageDirectory")]
    storage_directory: String,
    #[serde(rename = "customSettings")]
    custom_settings: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AgentContext {
    did: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Agent {
    did: String,
    perspective: Option<Perspective>,
    #[serde(rename = "directMessageLanguage")]
    direct_message_language: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Perspective {
    links: Vec<Link>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Link {
    source: String,
    predicate: String,
    target: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Expression {
    author: String,
    timestamp: String,
    data: Agent,
    proof: ExpressionProof,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ExpressionProof {
    signature: String,
    #[serde(rename = "keyHash")]
    key_hash: String,
    #[serde(rename = "valid")]
    valid: Option<bool>,
    #[serde(rename = "invalid")]
    invalid: Option<bool>,
}

fn create_signed_expression(agent: &Agent, context: &LanguageContext) -> Result<Expression, JsValue> {
    // In a real implementation, this would use the agent's keys to sign
    // For this WASM proof-of-concept, we create a mock signature
    
    let timestamp = js_sys::Date::now() as i64;
    let timestamp_str = format!("{}", timestamp);
    
    // Create a mock signature (in production, use actual crypto)
    let signature_data = format!("{}:{}", agent.did, timestamp);
    let signature = format!("mock-sig-{}", hash_string(&signature_data));
    
    Ok(Expression {
        author: context.agent.did.clone(),
        timestamp: timestamp_str,
        data: agent.clone(),
        proof: ExpressionProof {
            signature,
            key_hash: format!("key-hash-{}", &context.agent.did[..8]),
            valid: Some(true),
            invalid: None,
        },
    })
}

fn hash_string(s: &str) -> String {
    // Simple hash for mock signatures
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    format!("{:x}", hasher.finish())
}
