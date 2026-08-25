//! Typed wrappers around the runtime imports the AD4M executor exposes
//! to a WASM Language module. Spec §7.
//!
//! The extern block uses `module = "ad4m:host"` so wasm-bindgen emits
//! `import { agentDid, holochainCall, ... } from "ad4m:host"` at the
//! top of the generated language bundle. The executor registers a
//! synthetic module at the `ad4m:host` specifier (see
//! `rust-executor/src/js_core/options.rs` and `host.js`) that bridges
//! the runtime-provided globals to the canonical camelCase API surface.

use wasm_bindgen::prelude::*;

#[wasm_bindgen(module = "ad4m:host")]
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
    // These JS-side imports return Promises — `registerDNAs` installs an
    // app via the HOLOCHAIN_SERVICE ops which are async, and `call`
    // forwards to `callZomeFunction` which is likewise async. Declaring
    // them as plain sync `-> JsValue` returns handed the raw Promise
    // object through to Rust callers, who then could not await it
    // without manual JsFuture::from(promise) gymnastics — and most
    // language authors would silently receive a `Promise { <pending> }`
    // payload instead of the actual result. Use `async fn` so
    // wasm-bindgen emits the JsFuture conversion automatically and the
    // Rust caller just writes `.await`.
    #[wasm_bindgen(js_name = "holochainRegisterDnas", catch)]
    pub async fn holochain_register_dnas(dnas: JsValue) -> Result<JsValue, JsValue>;
    #[wasm_bindgen(js_name = "holochainCall", catch)]
    pub async fn holochain_call(
        dna_nick: &str,
        zome: &str,
        fn_name: &str,
        params: JsValue,
    ) -> Result<JsValue, JsValue>;

    // ----- Runtime utilities (spec §7.7) -----
    // Canonical AD4M content-address hash: SHA-256 -> CIDv1 -> base58btc,
    // prefixed with "Qm". The deterministic address function used by
    // every content-addressed Language.
    #[wasm_bindgen(js_name = "hash")]
    pub fn hash(data: &str) -> String;

    // ----- HTTP fetch (spec §7.2b) -----
    // Thin wrapper around Deno's native fetch(). `headers_json` is a JSON
    // string of `{ "Header": "Value", ... }` (empty string for no
    // headers), `body` is a raw request body string (empty for GET/HEAD).
    // Returns the response body as a string and rejects with a non-2xx
    // message on error. Callers JSON-encode/decode on either side as
    // needed.
    #[wasm_bindgen(js_name = "httpFetch", catch)]
    pub async fn http_fetch(
        url: &str,
        method: &str,
        headers_json: &str,
        body: &str,
    ) -> Result<JsValue, JsValue>;

    // ----- Language context (spec §7.3) -----
    #[wasm_bindgen(js_name = "languageAddress")]
    pub fn language_address() -> String;
    #[wasm_bindgen(js_name = "languageSettings")]
    pub fn language_settings() -> String;
    #[wasm_bindgen(js_name = "languageStorageDirectory")]
    pub fn language_storage_directory() -> String;

    // ----- Storage KV -- CORE (spec §7.4) -----
    #[wasm_bindgen(js_name = "storageGet")]
    pub fn storage_get(key: &str) -> JsValue;
    #[wasm_bindgen(js_name = "storagePut")]
    pub fn storage_put(key: &str, value: &str);
    #[wasm_bindgen(js_name = "storageDelete")]
    pub fn storage_delete(key: &str);
    #[wasm_bindgen(js_name = "storageListKeys")]
    pub fn storage_list_keys(prefix: Option<String>) -> Vec<JsValue>;

    // ----- Storage File I/O -- OPTIONAL EXTENSION (spec §7.6) -----
    // Raw read/write access to a filesystem-like storage layer.
    // `catch` because the JS wrapper throws on runtimes that don't
    // install the extension -- Rust callers get Err(JsValue) instead
    // of a panic. Prefer the KV API above unless you specifically
    // need filesystem-like semantics.
    #[wasm_bindgen(js_name = "readStorageFile", catch)]
    pub fn read_storage_file(path: &str) -> Result<String, JsValue>;
    #[wasm_bindgen(js_name = "writeStorageFile", catch)]
    pub fn write_storage_file(path: &str, content: &str) -> Result<(), JsValue>;

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

// ---------------------------------------------------------------------------
// Typed wrappers for the remaining JsValue-shaped imports.
//
// Same reason as above: language authors must route every cross-boundary
// Serialize through `__serde::to_js` so maps serialize as objects. These
// wrappers cover the rest of the §7 surface that still takes raw JsValue.
// ---------------------------------------------------------------------------

/// Register DNAs with the Holochain delegate. Type-safe wrapper around
/// `holochain_register_dnas`. Returns the resulting AppInfo list as a
/// JsValue (callers typically deserialize it via `__serde::from_js`).
/// Async because the underlying JS import is a Promise.
pub async fn holochain_register_dnas_typed<T: Serialize + ?Sized>(
    dnas: &T,
) -> Result<JsValue, JsValue> {
    let v = crate::__serde::to_js(dnas).map_err(|e| JsValue::from_str(&e.to_string()))?;
    holochain_register_dnas(v).await
}

/// Call a zome function. Type-safe wrapper around `holochain_call` — only
/// the params payload needs the maps-as-objects serializer; the three
/// identifier strings go through unchanged. Async because the underlying
/// JS import is a Promise.
pub async fn holochain_call_typed<T: Serialize + ?Sized>(
    dna_nick: &str,
    zome: &str,
    fn_name: &str,
    params: &T,
) -> Result<JsValue, JsValue> {
    let v = crate::__serde::to_js(params).map_err(|e| JsValue::from_str(&e.to_string()))?;
    holochain_call(dna_nick, zome, fn_name, v).await
}

/// Create a signed expression. Type-safe wrapper around
/// `agent_create_signed_expression` — ensures the wrapped data crosses
/// the boundary as a plain object, not a Map.
pub fn agent_create_signed_expression_typed<T: Serialize + ?Sized>(data: &T) -> JsValue {
    match crate::__serde::to_js(data) {
        Ok(v) => agent_create_signed_expression(v),
        Err(_) => JsValue::NULL,
    }
}

/// POST a JSON-serializable body to `url` and return the response body as
/// a String. Sets `Content-Type: application/json` automatically.
pub async fn http_post_json<T: Serialize + ?Sized>(
    url: &str,
    body: &T,
) -> Result<String, JsValue> {
    let body_s = serde_json::to_string(body)
        .map_err(|e| JsValue::from_str(&format!("http_post_json serialize: {}", e)))?;
    let headers = "{\"Content-Type\":\"application/json\"}";
    let v = http_fetch(url, "POST", headers, &body_s).await?;
    Ok(v.as_string().unwrap_or_default())
}

/// GET `url` with an optional query string (appended as `?k=v&...` if
/// non-empty) and return the response body as a String.
pub async fn http_get(url: &str) -> Result<String, JsValue> {
    let v = http_fetch(url, "GET", "", "").await?;
    Ok(v.as_string().unwrap_or_default())
}
