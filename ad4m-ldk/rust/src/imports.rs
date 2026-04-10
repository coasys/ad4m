//! Typed wrappers around the runtime imports the AD4M executor exposes
//! to a WASM Language module. Spec §7.

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // ----- Agent (spec §7.1) -----
    #[wasm_bindgen(js_name = "agentDid")]
    pub fn agent_did() -> String;
    #[wasm_bindgen(js_name = "agentSigningKeyId")]
    pub fn agent_signing_key_id() -> String;
    #[wasm_bindgen(js_name = "agentSign")]
    pub fn agent_sign(payload: &[u8]) -> Vec<u8>;
    #[wasm_bindgen(js_name = "agentSignStringHex")]
    pub fn agent_sign_string_hex(payload: &str) -> String;
    #[wasm_bindgen(js_name = "agentCreateSignedExpression")]
    pub fn agent_create_signed_expression(data: JsValue) -> JsValue;

    // ----- Holochain (spec §7.2) -----
    #[wasm_bindgen(js_name = "holochainRegisterDnas")]
    pub fn holochain_register_dnas(dnas: JsValue) -> JsValue;
    #[wasm_bindgen(js_name = "holochainCall")]
    pub fn holochain_call(dna_nick: &str, zome: &str, fn_name: &str, params: JsValue) -> JsValue;

    // ----- Language context (spec §7.3) -----
    #[wasm_bindgen(js_name = "languageAddress")]
    pub fn language_address() -> String;
    #[wasm_bindgen(js_name = "languageSettings")]
    pub fn language_settings() -> String;
    #[wasm_bindgen(js_name = "languageStorageDirectory")]
    pub fn language_storage_directory() -> String;

    // ----- Storage KV (spec §7.4) -----
    #[wasm_bindgen(js_name = "storageGet")]
    pub fn storage_get(key: &str) -> JsValue;
    #[wasm_bindgen(js_name = "storagePut")]
    pub fn storage_put(key: &str, value: &str);
    #[wasm_bindgen(js_name = "storageDelete")]
    pub fn storage_delete(key: &str);
    #[wasm_bindgen(js_name = "storageListKeys")]
    pub fn storage_list_keys(prefix: Option<String>) -> Vec<JsValue>;

    // ----- Event emission (spec §7.5) -----
    #[wasm_bindgen(js_name = "emitPerspectiveDiff")]
    pub fn emit_perspective_diff(diff: JsValue);
    #[wasm_bindgen(js_name = "emitSyncStateChange")]
    pub fn emit_sync_state_change(state: &str);
    #[wasm_bindgen(js_name = "emitTelepresenceSignal")]
    pub fn emit_telepresence_signal(payload: JsValue, recipient_did: Option<String>);
    #[wasm_bindgen(js_name = "emitSignal")]
    pub fn emit_signal(data: JsValue);
}

// ---------------------------------------------------------------------------
// Safe typed wrappers around the JsValue-shaped emit imports.
//
// `serde_wasm_bindgen::to_value` defaults to serializing Rust maps and
// `serde_json::Value::Object` as JS `Map` objects. The runtime dispatcher
// then runs `JSON.stringify(...)` on the result and gets `"{}"` because
// `Map` is not enumerable as own properties — every structured payload is
// silently lost.
//
// All cross-boundary serialization in this crate goes through `__serde::to_js`
// which sets `serialize_maps_as_objects(true)`. Expose typed wrappers for
// the emit imports so language authors don't have to know this detail and
// can't accidentally pass a `serde_wasm_bindgen::to_value` JsValue.
// ---------------------------------------------------------------------------

use serde::Serialize;

/// Emit a perspective diff. Type-safe wrapper around `emit_perspective_diff`.
/// Serializes the diff via the maps-as-objects serializer so the runtime
/// dispatcher actually receives the diff data instead of an empty object.
pub fn emit_perspective_diff_typed<T: Serialize + ?Sized>(diff: &T) {
    if let Ok(v) = crate::__serde::to_js(diff) {
        emit_perspective_diff(v);
    }
}

/// Emit a telepresence signal. Type-safe wrapper around
/// `emit_telepresence_signal` — see `emit_perspective_diff_typed` for the
/// reason this exists.
pub fn emit_telepresence_signal_typed<T: Serialize + ?Sized>(
    payload: &T,
    recipient_did: Option<String>,
) {
    if let Ok(v) = crate::__serde::to_js(payload) {
        emit_telepresence_signal(v, recipient_did);
    }
}

/// Emit an arbitrary signal. Type-safe wrapper around `emit_signal`.
pub fn emit_signal_typed<T: Serialize + ?Sized>(data: &T) {
    if let Ok(v) = crate::__serde::to_js(data) {
        emit_signal(v);
    }
}
