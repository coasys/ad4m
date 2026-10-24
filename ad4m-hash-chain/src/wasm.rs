//! wasm-bindgen exports of the `HashChain` TypeScript surface (build plan 5.18.4):
//! `entryHash(entry: object): string` and `compare(ours, theirs, from, chain): Compare`.
//! Errors are thrown as a `ChainErrorCode` object (`{ code: "malformed", reason }`).
//! A chain type's `fold` and `import` are exported by the crate that holds its policy.

use serde_json::Value;
use wasm_bindgen::prelude::*;

use crate::{compare, entry_hash_json, Compare};

fn malformed(reason: &str) -> JsValue {
    let code = serde_json::json!({"code": "malformed", "reason": reason});
    js_sys::JSON::parse(&code.to_string()).unwrap_or_else(|_| JsValue::from_str(reason))
}

fn to_js(value: &impl serde::Serialize) -> Result<JsValue, JsValue> {
    let text = serde_json::to_string(value).map_err(|e| malformed(&e.to_string()))?;
    js_sys::JSON::parse(&text)
}

/// The entry hash of a JSON object, as the Rust `entry_hash` computes it.
#[wasm_bindgen(js_name = entryHash)]
pub fn entry_hash_js(entry: JsValue) -> Result<String, JsValue> {
    let text = js_sys::JSON::stringify(&entry)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| malformed("entry is not JSON"))?;
    let value: Value = serde_json::from_str(&text).map_err(|e| malformed(&e.to_string()))?;
    entry_hash_json(&value).map_err(|e| malformed(&e.to_string()))
}

/// `compare` on hashes from seq `from` on. `from` must be a safe non-negative integer.
#[wasm_bindgen(js_name = compare)]
pub fn compare_js(
    ours: Vec<String>,
    theirs: Vec<String>,
    from: f64,
    chain: String,
) -> Result<JsValue, JsValue> {
    if !(from >= 0.0 && from.fract() == 0.0 && from <= crate::MAX_SAFE_INTEGER as f64) {
        return Err(malformed("from must be a safe non-negative integer"));
    }
    let result: Compare = compare(&ours, &theirs, from as u64, &chain);
    to_js(&result)
}
