//! wasm-bindgen exports of the `KelChain` and `MembershipReplay` TypeScript surface:
//!
//! ```ts
//! class KelChain { static fold(events: object[]): KelHandle;                  // throws ChainErrorCode
//!                  static import(handle: KelHandle, events: object[]): ImportOutcome; }
//! class KelHandle { id(): string; head(): Head; hashes(): string[]; hashAt(seq: number): string | undefined;
//!                   state(): KeyState;
//!                   didDocument(version?: number, alsoKnownAs?: string[]):
//!                     { didDocument: DidDocument, didDocumentMetadata: DocumentMetadata }; }
//! class MembershipReplay { constructor(creator: string);
//!                          apply(judgedLinkJson: string): string;   // MembershipChange[] as JSON
//!                          members(): string;                       // Member[] as JSON
//!                          isMember(person: string): boolean;
//!                          admits(person: string | undefined, byBot: boolean,
//!                                 invite: string | undefined, now: number): void; }
//! ```
//!
//! Errors are thrown as a `ChainErrorCode` object. A KEL refusal is
//! `{ code: "policy", reason: <FoldError name>, detail: <message> }`. `didDocument` throws
//! `{ code: "document", reason: "VersionPastHead", detail }` for a version past the head and
//! `{ code: "malformed", reason }` for a version that is not a seq. `MembershipReplay.apply` and
//! `admits` throw `{ code: "refused", reason: <Refusal> }` for a link or a join that counts for
//! nothing, and `{ code: "malformed", reason }` for a judged link that is not one or a `now`
//! that is not a safe integer.

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

fn refused(refusal: crate::membership::Refusal) -> JsValue {
    thrown(json!({"code": "refused", "reason": refusal}))
}

fn text_of(value: &impl serde::Serialize) -> Result<String, JsValue> {
    serde_json::to_string(value).map_err(|e| malformed(&e.to_string()))
}

/// The membership replay of one space, held in WASM memory.
#[wasm_bindgen]
pub struct MembershipReplay {
    graph: crate::membership::MembershipGraph,
}

#[wasm_bindgen]
impl MembershipReplay {
    /// The empty graph of a space whose creator is the person `creator`.
    #[wasm_bindgen(constructor)]
    pub fn new(creator: String) -> MembershipReplay {
        MembershipReplay {
            graph: crate::membership::MembershipGraph::new(&creator),
        }
    }

    /// Applies one judged link (JSON, as `JudgedLink` serializes): the changes as JSON.
    pub fn apply(&mut self, judged_link_json: &str) -> Result<String, JsValue> {
        let link: crate::membership::JudgedLink =
            serde_json::from_str(judged_link_json).map_err(|e| malformed(&e.to_string()))?;
        text_of(&self.graph.apply(&link).map_err(refused)?)
    }

    /// Every membership, current or ended, as JSON.
    pub fn members(&self) -> Result<String, JsValue> {
        text_of(&self.graph.members())
    }

    /// Whether `person` is a current member.
    #[wasm_bindgen(js_name = isMember)]
    pub fn is_member(&self, person: &str) -> bool {
        self.graph.is_member(person)
    }

    /// What a self-join by `person` would meet at `now` (ms): returns when it would count.
    pub fn admits(
        &self,
        person: Option<String>,
        by_bot: bool,
        invite: Option<String>,
        now: f64,
    ) -> Result<(), JsValue> {
        let now = seq_of(now).ok_or_else(|| malformed("now must be a safe integer"))?;
        self.graph
            .admits(person.as_deref(), by_bot, invite.as_deref(), now)
            .map_err(refused)
    }
}
