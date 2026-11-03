//! wasm-bindgen exports of the `HashChain` TypeScript surface:
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

/// `compare` on hashes from seq `from` on. `ours` and `theirs` must be arrays of strings.
/// `from` must be a safe non-negative integer, and so must the seq of every hash listed
/// (`from + length - 1`): every seq the answer names is one of those or one past the shorter
/// list, so it reaches JavaScript exactly.
#[wasm_bindgen(js_name = compare)]
pub fn compare_js(
    #[wasm_bindgen(unchecked_param_type = "string[]")] ours: JsValue,
    #[wasm_bindgen(unchecked_param_type = "string[]")] theirs: JsValue,
    from: f64,
    chain: String,
) -> Result<JsValue, JsValue> {
    let (ours, theirs) = (hashes_of(&ours, "ours")?, hashes_of(&theirs, "theirs")?);
    if !(from >= 0.0 && from.fract() == 0.0 && from <= crate::MAX_SAFE_INTEGER as f64) {
        return Err(malformed("from must be a safe non-negative integer"));
    }
    let from = from as u64;
    let longest = u64::try_from(ours.len().max(theirs.len())).unwrap_or(u64::MAX);
    if from.saturating_add(longest) > crate::MAX_SAFE_INTEGER + 1 {
        return Err(malformed(
            "the seq of every hash (from + index) must be a safe integer",
        ));
    }
    let result: Compare = compare(&ours, &theirs, from, &chain)
        .ok_or_else(|| malformed("a seq of the answer does not fit in a u64"))?;
    to_js(&result)
}

/// The strings of a JavaScript array; `malformed` for anything else, so a caller gets the
/// crate's `ChainErrorCode` and not wasm-bindgen's conversion error.
fn hashes_of(list: &JsValue, name: &str) -> Result<Vec<String>, JsValue> {
    if !js_sys::Array::is_array(list) {
        return Err(malformed(&format!("{name} must be an array of strings")));
    }
    js_sys::Array::from(list)
        .iter()
        .map(|item| {
            item.as_string()
                .ok_or_else(|| malformed(&format!("{name} must be an array of strings")))
        })
        .collect()
}
