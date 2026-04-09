//! Test WASM Language — demonstrates the new flat import pattern.
//!
//! This language uses `extern "C"` imports from the AD4M host.
//! No adapter wrappers, no JSON parsing — just direct function calls.
//!
//! ## Interface
//!
//! - `init()` — NO arguments. Context is accessed via imports:
//!   - `language_storage_directory()` — returns storage directory path
//!   - `language_address()` — returns this language's address
//!   - `language_settings()` — returns settings JSON string
//! - `name`, `version` — metadata
//! - `expressionCreate`, `expressionGet` — simple expression storage
//! - `teardown()` — cleanup

use wasm_bindgen::prelude::*;

// ============================================================================
// Flat imports from the AD4M host
// These MUST be provided by the runtime (both Rust and JS/Deno).
// ============================================================================

extern "C" {
    // Language context — NEW interface (no JSON to init())
    fn __language_storage_directory() -> String;
    fn __language_address() -> String;
    fn __language_settings() -> String;

    // Agent imports
    fn __agent_did() -> String;
    fn __agent_signing_key_id() -> String;
    fn __agent_create_signed_expression(data: JsValue) -> JsValue;

    // Signal import
    fn __signal_emit(data: JsValue);
}

// Helper wrappers for nicer API
fn language_storage_directory() -> String {
    unsafe { __language_storage_directory() }
}

fn language_address() -> String {
    unsafe { __language_address() }
}

fn language_settings() -> String {
    unsafe { __language_settings() }
}

fn agent_did() -> String {
    unsafe { __agent_did() }
}

fn agent_signing_key_id() -> String {
    unsafe { __agent_signing_key_id() }
}

fn agent_create_signed_expression(data: JsValue) -> JsValue {
    unsafe { __agent_create_signed_expression(data) }
}

fn signal_emit(data: JsValue) {
    unsafe { __signal_emit(data) }
}

// ============================================================================
// Module-level state
// ============================================================================

static mut STORAGE_DIR: String = String::new();
static mut LANGUAGE_ADDR: String = String::new();

// ============================================================================
// Required metadata
// ============================================================================

#[wasm_bindgen]
pub fn name() -> String {
    "test-wasm-language".to_string()
}

#[wasm_bindgen]
pub fn version() -> String {
    "0.1.0".to_string()
}

// ============================================================================
// init — NEW: takes NO arguments
// ============================================================================

#[wasm_bindgen]
pub fn init() {
    // Get language context via flat imports
    let storage_dir = language_storage_directory();
    let lang_addr = language_address();
    let settings_json = language_settings();

    // Store in module state
    unsafe {
        STORAGE_DIR = storage_dir.clone();
        LANGUAGE_ADDR = lang_addr.clone();
    }

    // Log for debugging
    signal_emit(JsValue::from_str(&format!(
        "[test-wasm-language] init: storage={}, address={}, settings={}",
        storage_dir, lang_addr, settings_json
    )));
}

// ============================================================================
// teardown
// ============================================================================

#[wasm_bindgen]
pub fn teardown() {
    unsafe {
        STORAGE_DIR.clear();
        LANGUAGE_ADDR.clear();
    }
}

// ============================================================================
// Expression capability — simple key-value store
// ============================================================================

#[wasm_bindgen]
pub fn expression_create(data: JsValue) -> String {
    // Create signed expression
    let signed = agent_create_signed_expression(data);

    // Hash the expression to get address (simplified)
    // In a real language, this would be a proper content-address
    let address = format!("test-wasm-{}", js_sys::Math::random());

    // Emit signal to show it worked
    signal_emit(JsValue::from_str(&format!(
        "[test-wasm-language] created: {}",
        address
    )));

    address
}

#[wasm_bindgen]
pub fn expression_get(address: String) -> JsValue {
    // Simplified: just return the address back as "data"
    // In a real language, would read from storage
    JsValue::from_str(&format!("data for {}", address))
}

#[wasm_bindgen]
pub fn interactions(_address: String) -> JsValue {
    JsValue::NULL
}