//! # WASM Language Delegate Interface (Flat Import Pattern)
//!
//! Defines plain function signatures that WASM-compiled languages use to interact
//! with the AD4M host. No trait objects — just `extern "C"` declarations that
//! a WASM module links against via wasm-bindgen.
//!
//! ## Usage
//!
//! Languages compiled to WASM (via wasm-bindgen) declare these as imports:
//!
//! ```ignore
//! // In the WASM language's Rust source (with wasm-bindgen):
//! #[wasm_bindgen]
//! extern "C" {
//!     fn __agent_did() -> String;
//!     fn __agent_sign(payload: &[u8]) -> Vec<u8>;
//!     fn __agent_create_signed_expression(data: JsValue) -> JsValue;
//!     fn __holochain_call(dna: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue;
//!     fn __signal_emit(data: JsValue);
//! }
//! ```
//!
//! Both the Rust host and the JS/Deno host implement the same function signatures.
//! The language doesn't care which one — it just calls the imports.
//!
//! ## Context
//!
//! `init()` receives serializable context as a JSON string. Non-serializable
//! delegates (agent identity, Holochain, signals) are only available via
//! the import functions above.

use serde::{Deserialize, Serialize};

// ============================================================================
// Context (serializable — crosses WASM boundary as JSON)
// ============================================================================

/// Serializable context passed to flat-export languages via init(contextJson: string).
/// This is the only data that crosses the WASM boundary as JSON.
/// All other delegates are available via the flat import functions.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageInitContext {
    /// Directory path for language-specific file storage
    pub storage_directory: String,
    /// Language-specific settings configured by the agent
    #[serde(rename = "customSettings")]
    pub custom_settings: serde_json::Value,
    /// This language's address in the Holochain network
    #[serde(rename = "languageAddress")]
    pub language_address: String,
}

// ============================================================================
// Supporting types (for import function signatures)
// ============================================================================

/// Specification for registering a DNA with the Holochain delegate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DnaSpec {
    pub nick: String,
    pub source: DnaSource,
}

/// Source of a DNA bundle
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

/// Result of DNA registration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppInfo {
    pub app_id: String,
    pub dna_hash: String,
    pub cell_id: String,
}

// ============================================================================
// Flat import function signatures
//
// These are the function signatures that the WASM host MUST implement.
// The Rust executor and JS/Deno bootstrap both expose the same functions.
// WASM languages call them as `extern "C"` imports.
// ============================================================================
//
// ## Agent imports
//
// `fn __agent_did() -> String` — returns the current agent's DID
// `fn __agent_sign(payload: &[u8]) -> Vec<u8>` — signs bytes, returns signature
// `fn __agent_sign_string_hex(payload: &str) -> String` — signs hex string
// `fn __agent_create_signed_expression(data: JsValue) -> JsValue` — creates signed expression
// `fn __agent_signing_key_id() -> String` — returns signing key ID
// `fn __agent_get_all_local_user_dids() -> Vec<String>` — all local user DIDs
// `fn __agent_create_signed_expression_for_user(email: &str, data: JsValue) -> JsValue`
// `fn __agent_did_for_user(email: &str) -> String`
//
// ## Holochain imports
//
// `fn __holochain_register_dnas(dnas: JsValue) -> JsValue` — register DNAs, returns Vec<AppInfo>
// `fn __holochain_call(dna: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue` — sync call
// `fn __holochain_call_async(...) -> JsValue` — async call
//
// ## Signal imports
//
// `fn __signal_emit(data: JsValue)` — emits a signal to the AD4M signal bus

/// Flat export language interface version marker
pub const FLAT_EXPORT_VERSION: &str = "1.0";
