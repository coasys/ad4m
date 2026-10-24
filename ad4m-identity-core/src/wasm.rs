//! wasm-bindgen exports of the `KelChain` TypeScript surface:
//!
//! ```ts
//! class KelChain { static fold(events: object[]): KelHandle;                  // throws ChainErrorCode
//!                  static import(handle: KelHandle, events: object[]): ImportOutcome; }
//! class KelHandle { id(): string; head(): Head; hashes(): string[]; hashAt(seq: number): string | undefined;
//!                   state(): KeyState; }
//! ```
//!
//! Errors are thrown as a `ChainErrorCode` object. A KEL refusal is
//! `{ code: "policy", reason: <FoldError name>, detail: <message> }`.

use serde_json::{json, Value};
use wasm_bindgen::prelude::*;

use crate::{error_code, KelChain as Chain};

fn to_js(value: &Value) -> Result<JsValue, JsValue> {
    js_sys::JSON::parse(&value.to_string())
}

fn thrown(code: Value) -> JsValue {
    to_js(&code).unwrap_or_else(|_| JsValue::from_str(&code.to_string()))
}

fn malformed(reason: &str) -> JsValue {
    thrown(json!({"code": "malformed", "reason": reason}))
}

fn events_of(events: &JsValue) -> Result<Vec<Value>, JsValue> {
    let text = js_sys::JSON::stringify(events)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| malformed("events are not JSON"))?;
    match serde_json::from_str(&text).map_err(|e| malformed(&e.to_string()))? {
        Value::Array(items) => Ok(items),
        _ => Err(malformed("events must be an array")),
    }
}

fn json_of(value: &impl serde::Serialize) -> Result<JsValue, JsValue> {
    let value = serde_json::to_value(value).map_err(|e| malformed(&e.to_string()))?;
    to_js(&value)
}

/// One folded log, held in WASM memory.
#[wasm_bindgen]
pub struct KelHandle {
    chain: Chain,
}

#[wasm_bindgen]
impl KelHandle {
    /// The SCID.
    pub fn id(&self) -> String {
        self.chain.id().clone()
    }

    /// `{ seq, hash }` of the last event.
    pub fn head(&self) -> Result<JsValue, JsValue> {
        json_of(&self.chain.head())
    }

    /// The hash at every seq from 0 to the head.
    pub fn hashes(&self) -> Vec<String> {
        self.chain.hashes().to_vec()
    }

    /// The hash at `seq`, if the log holds it.
    #[wasm_bindgen(js_name = hashAt)]
    pub fn hash_at(&self, seq: f64) -> Option<String> {
        if !(seq >= 0.0 && seq.fract() == 0.0 && seq <= ad4m_hash_chain::MAX_SAFE_INTEGER as f64) {
            return None;
        }
        self.chain.hash_at(seq as u64).cloned()
    }

    /// The replayed `KeyState` as JSON.
    pub fn state(&self) -> Result<JsValue, JsValue> {
        json_of(self.chain.state())
    }
}

/// The `KelChain` namespace of the TypeScript surface.
#[wasm_bindgen(js_name = KelChain)]
pub struct KelChainJs {}

#[wasm_bindgen(js_class = KelChain)]
impl KelChainJs {
    /// Replays `events` (JSON objects) into a log.
    pub fn fold(events: JsValue) -> Result<KelHandle, JsValue> {
        let events = events_of(&events)?;
        crate::fold_json(&events)
            .map(|chain| KelHandle { chain })
            .map_err(|e| thrown(error_code(&e)))
    }

    /// Imports `events` into `handle`: `{ kind: "appended", head, count }`, `already-have`,
    /// `gap` or `fork`. The handle changes only when the outcome is `appended`.
    pub fn import(handle: &mut KelHandle, events: JsValue) -> Result<JsValue, JsValue> {
        let events = events_of(&events)?;
        let outcome =
            crate::import_json(&mut handle.chain, &events).map_err(|e| thrown(error_code(&e)))?;
        json_of(&outcome)
    }
}
