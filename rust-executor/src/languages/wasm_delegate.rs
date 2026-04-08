//! # WASM Language Delegate Interface
//!
//! Defines the traits that WASM-compiled languages use to interact with the AD4M host.
//! These mirror the JS flat-export delegate interface (agentProxy, holochainDelegate, ad4mSignal)
//! but expressed as Rust traits suitable for wasm-bindgen.
//!
//! ## Usage
//!
//! Languages compiled to WASM (via wasm-bindgen) import these as extern "C" functions:
//!
//! ```ignore
//! #[wasm_bindgen]
//! extern "C" {
//!     fn __agent_create_signed_expression(data: JsValue) -> JsValue;
//!     fn __agent_sign(payload: &[u8]) -> Vec<u8>;
//!     fn __agent_did() -> String;
//!     fn __holochain_call(dna: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue;
//!     fn __signal_emit(data: JsValue);
//! }
//! ```
//!
//! The flat export init() receives a JSON context with serializable data.
//! Non-serializable delegates (agent, holochain, signal) are available via the import functions above.

use serde::{Deserialize, Serialize};

/// Serializable context passed to flat-export languages via init(contextJson: string).
/// This is the only data that needs to cross the WASM boundary as JSON.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageInitContext {
    /// Directory path for language-specific storage
    pub storage_directory: String,
    /// Language-specific custom settings from the agent
    #[serde(rename = "customSettings")]
    pub custom_settings: serde_json::Value,
    /// This language's address in the HC network
    #[serde(rename = "languageAddress")]
    pub language_address: String,
}

/// Agent delegate — handles identity and signing for expressions.
/// Mirrors the JS agentProxy: { did, signingKeyId, createSignedExpression, sign, ... }
pub trait AgentDelegate: Send + Sync {
    /// Get the DID of the current agent
    fn did(&self) -> String;
    /// Get the signing key ID for the current agent
    fn signing_key_id(&self) -> String;
    /// Create a signed expression with the given data
    fn create_signed_expression(&self, data: serde_json::Value) -> serde_json::Value;
    /// Sign arbitrary payload bytes, return signature
    fn sign(&self, payload: &[u8]) -> Vec<u8>;
    /// Sign a hex string, return hex signature
    fn sign_string_hex(&self, payload: &str) -> String;
    /// Get all local user DIDs (for multi-user contexts)
    fn get_all_local_user_dids(&self) -> Vec<String>;
    /// Create signed expression for a specific user (by email)
    fn create_signed_expression_for_user(
        &self,
        user_email: &str,
        data: serde_json::Value,
    ) -> serde_json::Value;
    /// Get DID for a specific user (by email)
    fn did_for_user(&self, user_email: &str) -> String;
}

/// Holochain delegate — provides access to DNA calls and DNA registration.
/// Mirrors the JS holochainDelegate: { registerDNAs, call, callAsync }
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DnaSpec {
    pub nick: String,
    pub source: DnaSource,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DnaSource {
    #[serde(rename = "path")]
    Path { value: String },
    #[serde(rename = "bundle")]
    Bundle { value: Vec<u8> },
    #[serde(rename = "bytes")]
    Bytes { value: Vec<u8> },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppInfo {
    pub app_id: String,
    pub dna_hash: String,
    pub cell_id: String,
}

pub trait HolochainDelegate: Send + Sync {
    /// Register one or more DNAs for this language.
    /// The signal_callback is called when the DNA emits a signal.
    fn register_dnas(&self, dnas: Vec<DnaSpec>) -> Vec<AppInfo>;
    /// Synchronous call to a zome function
    fn call(
        &self,
        dna_nick: &str,
        zome: &str,
        fn_name: &str,
        params: serde_json::Value,
    ) -> serde_json::Value;
    /// Async call to a zome function
    fn call_async(
        &self,
        dna_nick: &str,
        zome: &str,
        fn_name: &str,
        params: serde_json::Value,
    ) -> serde_json::Value;
}

/// Signal delegate — emits signals to the AD4M signal bus.
/// Mirrors the JS ad4mSignal: (signal) => LANGUAGE_CONTROLLER.ad4mSignalEmitted(signal, languageAddress)
pub trait SignalDelegate: Send + Sync {
    fn emit(&self, signal: serde_json::Value);
}

/// Flat export language interface version marker
pub const FLAT_EXPORT_VERSION: &str = "1.0";

/// Minimal set of exports a flat-export WASM language must provide
pub trait FlatExportLanguage {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn init(&mut self, context: LanguageInitContext);
    fn teardown(&mut self);
}
