//! Flat import wrappers for AD4M host functions
//!
//! These functions wrap the flat WASM import functions exposed by the AD4M executor.
//! They provide a Rust-friendly interface to the underlying host capabilities.

use crate::types::{Entry, EntryHash};
use wasm_bindgen::prelude::*;

// ============================================================================
// Raw WASM imports from the host
// ============================================================================

#[wasm_bindgen]
extern "C" {
    // Agent imports
    #[wasm_bindgen(js_namespace = __agent)]
    fn did() -> String;

    #[wasm_bindgen(js_namespace = __agent)]
    fn sign(payload: &[u8]) -> Vec<u8>;

    #[wasm_bindgen(js_namespace = __agent)]
    fn sign_string_hex(payload: &str) -> String;

    #[wasm_bindgen(js_namespace = __agent)]
    fn create_signed_expression(data: &JsValue) -> JsValue;

    #[wasm_bindgen(js_namespace = __agent)]
    fn signing_key_id() -> String;

    #[wasm_bindgen(js_namespace = __agent)]
    fn get_all_local_user_dids() -> Vec<String>;

    #[wasm_bindgen(js_namespace = __agent)]
    fn create_signed_expression_for_user(email: &str, data: &JsValue) -> JsValue;

    #[wasm_bindgen(js_namespace = __agent)]
    fn did_for_user(email: &str) -> String;

    // Holochain imports
    #[wasm_bindgen(js_namespace = __holochain)]
    fn register_dnas(dnas: &JsValue) -> JsValue;

    #[wasm_bindgen(js_namespace = __holochain)]
    fn call(dna: &str, zome: &str, fn_name: &str, params: &JsValue) -> JsValue;

    #[wasm_bindgen(js_namespace = __holochain)]
    fn call_async(dna: &str, zome: &str, fn_name: &str, params: &JsValue) -> JsValue;

    // Signal imports
    #[wasm_bindgen(js_namespace = __signal)]
    fn emit(data: &JsValue);

    // Language imports
    #[wasm_bindgen(js_namespace = __language)]
    fn address() -> String;

    #[wasm_bindgen(js_namespace = __language)]
    fn hash() -> String;
}

// ============================================================================
// Agent module
// ============================================================================

/// Agent-related functions for identity and signing
pub mod agent {
    use super::*;

    /// Returns the current agent's DID
    pub fn get_did() -> String {
        did()
    }

    /// Signs the given payload and returns the signature
    pub fn sign_payload(payload: &[u8]) -> Vec<u8> {
        sign(payload)
    }

    /// Signs a hex string payload
    pub fn sign_hex(payload: &str) -> String {
        sign_string_hex(payload)
    }

    /// Creates a signed expression with the given data
    pub fn create_signed_expression_data(data: &JsValue) -> JsValue {
        create_signed_expression(data)
    }

    /// Returns the agent's signing key ID
    pub fn signing_key_id() -> String {
        super::signing_key_id()
    }

    /// Returns all local user DIDs
    pub fn get_all_local_user_dids() -> Vec<String> {
        super::get_all_local_user_dids()
    }

    /// Creates a signed expression for a specific user
    pub fn create_signed_expression_for_user_data(email: &str, data: &JsValue) -> JsValue {
        create_signed_expression_for_user(email, data)
    }

    /// Returns the DID for a specific user
    pub fn did_for_user(email: &str) -> String {
        super::did_for_user(email)
    }
}

// ============================================================================
// Holochain module
// ============================================================================

/// Holochain-related functions for DNA registration and calls
pub mod holochain {
    use super::*;
    use serde::{Deserialize, Serialize};

    /// Specification for registering a DNA
    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct DnaSpec {
        pub nick: String,
        pub source: DnaSource,
    }

    /// Source of a DNA bundle
    #[derive(Serialize, Deserialize, Debug, Clone)]
    #[serde(tag = "type")]
    pub enum DnaSource {
        #[serde(rename = "path")]
        Path { value: String },
        #[serde(rename = "bundle")]
        Bundle { value: Vec<u8> },
        #[serde(rename = "bytes")]
        Bytes { value: Vec<u8> },
    }

    /// Result of DNA registration
    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct AppInfo {
        pub app_id: String,
        pub dna_hash: String,
        pub cell_id: String,
    }

    /// Registers DNAs with the Holochain conductor
    pub fn register_dnas_spec(dnas: Vec<DnaSpec>) -> Result<Vec<AppInfo>, String> {
        let dnas_js = serde_wasm_bindgen::to_value(&dnas)
            .map_err(|e| format!("Failed to serialize DNAs: {}", e))?;
        let result = register_dnas(&dnas_js);
        serde_wasm_bindgen::from_value(result)
            .map_err(|e| format!("Failed to deserialize AppInfo: {}", e))
    }

    /// Calls a zome function on a DNA
    pub fn call_zome(
        dna: &str,
        zome: &str,
        fn_name: &str,
        params: impl Serialize,
    ) -> Result<JsValue, String> {
        let params_js = serde_wasm_bindgen::to_value(&params)
            .map_err(|e| format!("Failed to serialize params: {}", e))?;
        Ok(call(dna, zome, fn_name, &params_js))
    }

    /// Calls a zome function asynchronously
    pub fn call_zome_async(
        dna: &str,
        zome: &str,
        fn_name: &str,
        params: impl Serialize,
    ) -> Result<JsValue, String> {
        let params_js = serde_wasm_bindgen::to_value(&params)
            .map_err(|e| format!("Failed to serialize params: {}", e))?;
        Ok(call_async(dna, zome, fn_name, &params_js))
    }

    /// Commits an entry to Holochain
    pub fn commit_entry(entry: Entry) -> Result<EntryHash, String> {
        // This would be implemented via a specific Holochain call
        // For now, we use the generic call mechanism
        let params = serde_json::json!({
            "content": entry.content,
        });
        let result = call_zome("app", "integrity", "commit", params)?;
        let hash_bytes: Vec<u8> = serde_wasm_bindgen::from_value(result)
            .map_err(|e| format!("Failed to deserialize hash: {}", e))?;
        Ok(EntryHash::new(hash_bytes))
    }

    /// Gets an entry from Holochain by its hash
    pub fn get_entry(entry_hash: EntryHash) -> Result<Option<Entry>, String> {
        let params = serde_json::json!({
            "hash": entry_hash.0,
        });
        let result = call_zome("app", "integrity", "get", params)?;
        let maybe_content: Option<Vec<u8>> = serde_wasm_bindgen::from_value(result)
            .map_err(|e| format!("Failed to deserialize entry: {}", e))?;
        Ok(maybe_content.map(|content| Entry { content }))
    }

    /// Calls a Holochain function with a string payload
    pub fn call_function(fn_name: String, payload: String) -> Result<String, String> {
        let params = serde_json::json!({
            "fn_name": fn_name,
            "payload": payload,
        });
        let result = call_zome("app", "bridge", "call", params)?;
        serde_wasm_bindgen::from_value(result)
            .map_err(|e| format!("Failed to deserialize result: {}", e))
    }
}

// ============================================================================
// Language module
// ============================================================================

/// Language-related functions for accessing language metadata
pub mod language {
    use super::*;

    /// Returns this language's address
    pub fn language_address() -> String {
        address()
    }

    /// Returns this language's hash
    pub fn language_hash() -> String {
        hash()
    }
}

// ============================================================================
// Signal emit module
// ============================================================================

/// Signal emission functions
pub mod signal_emit {
    use super::*;
    use serde::Serialize;

    /// Emits a signal to the AD4M signal bus
    pub fn emit_signal(detail: impl Serialize) -> Result<(), String> {
        let detail_js = serde_wasm_bindgen::to_value(&detail)
            .map_err(|e| format!("Failed to serialize signal detail: {}", e))?;
        emit(&detail_js);
        Ok(())
    }

    /// Emits a raw string signal
    pub fn emit_string(detail: String) -> Result<(), String> {
        let detail_js = JsValue::from_str(&detail);
        emit(&detail_js);
        Ok(())
    }
}

// serde_wasm_bindgen helper
mod serde_wasm_bindgen {
    use wasm_bindgen::prelude::*;

    pub fn to_value<T: serde::Serialize>(value: &T) -> Result<JsValue, serde_json::Error> {
        let json = serde_json::to_string(value)?;
        Ok(JsValue::from_str(&json))
    }

    pub fn from_value<T: for<'de> serde::Deserialize<'de>>(value: JsValue) -> Result<T, String> {
        let json = value.as_string().ok_or("Expected string value")?;
        serde_json::from_str(&json).map_err(|e| e.to_string())
    }
}
