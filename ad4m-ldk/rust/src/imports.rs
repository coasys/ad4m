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

    // ----- Storage KV (spec §7) -----
    #[wasm_bindgen(js_name = "storageGet")]
    pub fn storage_get(key: &str) -> JsValue;
    #[wasm_bindgen(js_name = "storagePut")]
    pub fn storage_put(key: &str, value: &str);
    #[wasm_bindgen(js_name = "storageDelete")]
    pub fn storage_delete(key: &str);
    #[wasm_bindgen(js_name = "storageListKeys")]
    pub fn storage_list_keys(prefix: Option<String>) -> Vec<JsValue>;

    // ----- Event emission (spec §7.4) -----
    #[wasm_bindgen(js_name = "emitPerspectiveDiff")]
    pub fn emit_perspective_diff(diff: JsValue);
    #[wasm_bindgen(js_name = "emitSyncStateChange")]
    pub fn emit_sync_state_change(state: &str);
    #[wasm_bindgen(js_name = "emitTelepresenceSignal")]
    pub fn emit_telepresence_signal(payload: JsValue, recipient_did: Option<String>);
    #[wasm_bindgen(js_name = "emitSignal")]
    pub fn emit_signal(data: JsValue);
}
