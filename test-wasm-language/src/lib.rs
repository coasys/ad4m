//! Test WASM Language for AD4M
//!
//! This is a minimal test language that demonstrates the AD4M LDK usage.
//! It implements the Language trait and can be compiled to WASM for testing
//! the flat import interface.

use ad4m_ldk::{
    Language, LanguageContext, Link, LinkExpression, Perspective, PerspectiveDiff,
    ExpressionProof, Provenance, links_trigger_callback, agent, lang,
};
use wasm_bindgen::prelude::*;

// Note: wee_alloc is already defined in ad4m-ldk, so we don't define it here

/// Test language state
static mut LANGUAGE_STATE: Option<TestLanguage> = None;

/// A simple test language implementation
pub struct TestLanguage {
    context: LanguageContext,
    links: Vec<LinkExpression>,
}

impl TestLanguage {
    /// Creates a new test language with some sample data
    fn new(context: LanguageContext) -> Self {
        // Create some sample links
        let sample_links = vec![
            LinkExpression {
                author: context.agent_did.clone(),
                timestamp: "1234567890".to_string(),
                data: Link {
                    source: "test://source1".to_string(),
                    predicate: Some("test://predicate1".to_string()),
                    target: "test://target1".to_string(),
                },
                proof: ExpressionProof {
                    key: "test-key".to_string(),
                    signature: "test-signature".to_string(),
                },
                status: None,
                },
            LinkExpression {
                author: context.agent_did.clone(),
                timestamp: "1234567891".to_string(),
                data: Link {
                    source: "test://source2".to_string(),
                    predicate: Some("test://predicate2".to_string()),
                    target: "test://target2".to_string(),
                },
                proof: ExpressionProof {
                    key: "test-key".to_string(),
                    signature: "test-signature".to_string(),
                },
                status: None,
            },
        ];

        Self {
            context,
            links: sample_links,
        }
    }

    /// Returns a hardcoded perspective for testing
    fn get_test_perspective(&self) -> Perspective {
        Perspective {
            links: self.links.clone(),
        }
    }
}

impl Language for TestLanguage {
    const NAME: &'static str = "test-wasm-language";
    const VERSION: &'static str = "0.1.0";

    fn init(context: LanguageContext) -> Result<(), String> {
        let lang = TestLanguage::new(context);
        unsafe {
            LANGUAGE_STATE = Some(lang);
        }
        Ok(())
    }

    fn get_state(&self) -> Result<Option<Perspective>, String> {
        Ok(Some(self.get_test_perspective()))
    }

    fn receive(&self, data: Vec<u8>) -> Result<(), String> {
        // Log received data (in a real implementation, this would use proper logging)
        web_sys::console::log_1(&format!("TestLanguage received: {:?}", data).into());
        Ok(())
    }
}

/// Initialize the language with the given context JSON
#[wasm_bindgen]
pub fn init(context_json: String) -> Result<(), JsValue> {
    let context: LanguageContext = serde_json::from_str(&context_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse context: {}", e)))?;

    TestLanguage::init(context)
        .map_err(|e| JsValue::from_str(&e))
}

/// Get the current state as a JSON string
#[wasm_bindgen]
pub fn get_state() -> Result<String, JsValue> {
    unsafe {
        match &LANGUAGE_STATE {
            Some(lang) => {
                let state = lang.get_state()
                    .map_err(|e| JsValue::from_str(&e))?;
                serde_json::to_string(&state)
                    .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))
            }
            None => Err(JsValue::from_str("Language not initialized")),
        }
    }
}

/// Receive data from another agent
#[wasm_bindgen]
pub fn receive(data: Vec<u8>) -> Result<(), JsValue> {
    unsafe {
        match &LANGUAGE_STATE {
            Some(lang) => {
                lang.receive(data)
                    .map_err(|e| JsValue::from_str(&e))
            }
            None => Err(JsValue::from_str("Language not initialized")),
        }
    }
}

/// Get the language name
#[wasm_bindgen]
pub fn name() -> String {
    TestLanguage::NAME.to_string()
}

/// Get the language version
#[wasm_bindgen]
pub fn version() -> String {
    TestLanguage::VERSION.to_string()
}

/// Test the agent imports
#[wasm_bindgen]
pub fn test_agent_imports() -> Result<String, JsValue> {
    let did = agent::get_did();
    let key_id = agent::signing_key_id();
    Ok(format!("Agent DID: {}, Key ID: {}", did, key_id))
}

/// Test the language imports
#[wasm_bindgen]
pub fn test_language_imports() -> Result<String, JsValue> {
    let addr = lang::language_address();
    let hash = lang::language_hash();
    Ok(format!("Language Address: {}, Hash: {}", addr, hash))
}

/// Trigger a links callback (for testing)
#[wasm_bindgen]
pub fn trigger_links_callback() -> Result<(), JsValue> {
    let provenance = Provenance::new("did:test:abc123", 1234567890);
    let diff = PerspectiveDiff::empty();
    links_trigger_callback(provenance, diff);
    Ok(())
}

/// Get the language info as a JSON object
#[wasm_bindgen]
pub fn get_info() -> Result<String, JsValue> {
    let info = serde_json::json!({
        "name": TestLanguage::NAME,
        "version": TestLanguage::VERSION,
        "description": "A test WASM language for AD4M",
    });
    Ok(info.to_string())
}
