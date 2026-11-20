//! wasm-bindgen exports of the `KelChain` TypeScript surface:
//!
//! ```ts
//! class KelChain { static fold(events: object[]): KelHandle;                  // throws ChainErrorCode
//!                  static import(handle: KelHandle, events: object[]): ImportOutcome; }
//! class KelHandle { id(): string; head(): Head; hashes(): string[]; hashAt(seq: number): string | undefined;
//!                   state(): KeyState;
//!                   didDocument(version?: number, alsoKnownAs?: string[]):
//!                     { didDocument: DidDocument, didDocumentMetadata: DocumentMetadata }; }
//! class Pow { static solve(scid: string, challenge: string, bits: number):
//!               { nonce: string, bits: number, challenge: string };
//!             static solveRange(scid: string, challenge: string, bits: number, start: number,
//!               tries: number): { nonce: string, bits: number, challenge: string } | undefined;
//!             static check(scid: string, pow: object, bits: number): boolean; }
//! ```
//!
//! Errors are thrown as a `ChainErrorCode` object. A KEL refusal is
//! `{ code: "policy", reason: <FoldError name>, detail: <message> }`. `didDocument` throws
//! `{ code: "document", reason: "VersionPastHead", detail }` for a version past the head and
//! `{ code: "malformed", reason }` for a version that is not a seq.

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

/// A JS number as a seq: a non-negative safe integer.
fn seq_of(n: f64) -> Option<u64> {
    (n >= 0.0 && n.fract() == 0.0 && n <= ad4m_hash_chain::MAX_SAFE_INTEGER as f64)
        .then_some(n as u64)
}

/// What `didDocument` returns.
#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct Resolved<'a> {
    did_document: &'a crate::DidDocument,
    did_document_metadata: &'a crate::DocumentMetadata,
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
        self.chain.hash_at(seq_of(seq)?).cloned()
    }

    /// The replayed `KeyState` as JSON.
    pub fn state(&self) -> Result<JsValue, JsValue> {
        json_of(self.chain.state())
    }

    /// The DID document at `version` (the head when absent) with its metadata:
    /// `{ didDocument, didDocumentMetadata }`.
    #[wasm_bindgen(js_name = didDocument)]
    pub fn did_document(
        &self,
        version: Option<f64>,
        also_known_as: Option<Vec<String>>,
    ) -> Result<JsValue, JsValue> {
        let version = match version {
            None => None,
            Some(v) => Some(seq_of(v).ok_or_else(|| malformed("version must be a seq"))?),
        };
        let (document, metadata) =
            crate::did_document(&self.chain, version, &also_known_as.unwrap_or_default()).map_err(
                |e| {
                    thrown(json!({"code": "document", "reason": e.name(), "detail": e.to_string()}))
                },
            )?;
        // Through text, not `Value`, so the object keeps the document's member order.
        let text = serde_json::to_string(&Resolved {
            did_document: &document,
            did_document_metadata: &metadata,
        })
        .map_err(|e| malformed(&e.to_string()))?;
        js_sys::JSON::parse(&text)
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

/// The `Pow` namespace of the TypeScript surface: the brake's proof of work for event #0
/// (`crate::pow`). The party that builds #0 solves it over the host's `challenge`; the store
/// checks it.
#[wasm_bindgen(js_name = Pow)]
pub struct PowJs {}

#[wasm_bindgen(js_class = Pow)]
impl PowJs {
    /// The proof `{ nonce, bits, challenge }` for `scid` over `challenge` at `bits`.
    /// Deterministic. It does not return until it finds a proof (about `2^bits` hashes), so
    /// call it in a web worker, and refuse first a `bits` from a store that the surface
    /// cannot afford.
    pub fn solve(scid: &str, challenge: &str, bits: u8) -> Result<JsValue, JsValue> {
        json_of(&crate::pow::solve(scid, challenge, bits))
    }

    /// Tries the nonces `start`, `start + 1`, … for at most `tries` of them: the first proof
    /// `{ nonce, bits, challenge }` that meets `bits`, else `undefined`. A surface that shows
    /// progress calls it in rounds (`start` grows by `tries` each round). `start` and `tries`
    /// are safe integers; another number throws `{ code: "malformed" }`.
    #[wasm_bindgen(js_name = solveRange)]
    pub fn solve_range(
        scid: &str,
        challenge: &str,
        bits: u8,
        start: f64,
        tries: f64,
    ) -> Result<JsValue, JsValue> {
        let start = seq_of(start).ok_or_else(|| malformed("start must be a safe integer"))?;
        let tries = seq_of(tries).ok_or_else(|| malformed("tries must be a safe integer"))?;
        match crate::pow::solve_range(scid, challenge, bits, u128::from(start), tries) {
            Some(pow) => json_of(&pow),
            None => Ok(JsValue::UNDEFINED),
        }
    }

    /// True when `pow` (`{ nonce, bits, challenge }`) meets `bits` for `scid`. `pow.bits` is
    /// not read; a `pow` that is not that shape is false.
    pub fn check(scid: &str, pow: JsValue, bits: u8) -> bool {
        let Some(text) = js_sys::JSON::stringify(&pow)
            .ok()
            .and_then(|s| s.as_string())
        else {
            return false;
        };
        match serde_json::from_str::<crate::pow::Pow>(&text) {
            Ok(pow) => crate::pow::check(scid, &pow, bits),
            Err(_) => false,
        }
    }
}
