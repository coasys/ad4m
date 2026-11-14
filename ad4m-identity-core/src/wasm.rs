//! wasm-bindgen exports of the `KelChain` and `MembershipReplay` TypeScript surface:
//!
//! ```ts
//! class KelChain { static fold(events: object[]): KelHandle;                  // throws ChainErrorCode
//!                  static import(handle: KelHandle, events: object[]): ImportOutcome; }
//! class KelHandle { id(): string; head(): Head; hashes(): string[]; hashAt(seq: number): string | undefined;
//!                   state(): KeyState;
//!                   trustedSeq(head: SignedHead, storeKeys: string[]): number | undefined;
//!                   judgeLink(wireLinkJson: string, trusted: number, now: number): string; // JudgedLink JSON
//!                   authorOf(key: string, trusted: number, now: number): { person: string, byBot: boolean } | null;
//!                   didDocument(version?: number, alsoKnownAs?: string[]):
//!                     { didDocument: DidDocument, didDocumentMetadata: DocumentMetadata }; }
//! class MembershipReplay { constructor(creator: string);
//!                          apply(judgedLinkJson: string): string;   // MembershipChange[] as JSON
//!                          tryApply(judgedLinkJson: string): string; // as apply; a refusal keeps nothing
//!                          members(): string;                       // Member[] as JSON
//!                          isMember(person: string): boolean;
//!                          admits(person: string | undefined, byBot: boolean,
//!                                 invite: string | undefined, now: number): void; }
//! function judgeLinkWithoutLog(wireLinkJson: string, now: number): string;   // JudgedLink JSON
//! function wireLinkHash(wireLinkJson: string): string;                      // hex
//! function wireSignatureHolds(wireLinkJson: string): boolean;
//! function roomKeySigningBytes(did: string, x25519PublicKey: string): Uint8Array;
//! function verifyRoomKey(did: string, x25519PublicKey: string, signature: string): boolean;
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

    /// The seq up to which this log is trusted by the store-signed `head` (a `SignedHead`
    /// object): the head's seq when one of `storeKeys` (multikeys) signed it and the log holds
    /// its hash at its seq, else `undefined`.
    #[wasm_bindgen(js_name = trustedSeq)]
    pub fn trusted_seq(
        &self,
        head: JsValue,
        store_keys: Vec<String>,
    ) -> Result<Option<f64>, JsValue> {
        let head: crate::wire::SignedHead = value_of(&head)?;
        Ok(crate::wire::trusted_seq(&self.chain, &head, &store_keys).map(|s| s as f64))
    }

    /// Judges a signed link (JSON text) arriving now against this log, trusted up to `trusted`:
    /// the `JudgedLink` as JSON, ready for `MembershipReplay.apply`.
    #[wasm_bindgen(js_name = judgeLink)]
    pub fn judge_link(&self, link_json: &str, trusted: f64, now: f64) -> Result<String, JsValue> {
        let trusted = seq_of(trusted).ok_or_else(|| malformed("trusted must be a seq"))?;
        judged(
            link_json,
            crate::judgement::SignerLog::Listed {
                state: self.chain.state(),
                trusted,
            },
            now,
        )
    }

    /// Whom the signing multikey `key` speaks for at the trusted seq `trusted` and `now` (ms):
    /// `{ person, byBot }`, or `null` for a key that is no standing device key of this log.
    #[wasm_bindgen(js_name = authorOf)]
    pub fn author_of(&self, key: &str, trusted: f64, now: f64) -> Result<JsValue, JsValue> {
        let trusted = seq_of(trusted).ok_or_else(|| malformed("trusted must be a seq"))?;
        let now = seq_of(now).ok_or_else(|| malformed("now must be a safe integer"))?;
        json_of(&crate::wire::author_of(
            self.chain.state(),
            key,
            trusted,
            now,
        ))
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

/// `value` (a JS value) as `T`, through its JSON text.
fn value_of<T: serde::de::DeserializeOwned>(value: &JsValue) -> Result<T, JsValue> {
    let text = js_sys::JSON::stringify(value)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| malformed("not JSON"))?;
    serde_json::from_str(&text).map_err(|e| malformed(&e.to_string()))
}

fn wire_link_of(link_json: &str) -> Result<crate::wire::WireLink, JsValue> {
    serde_json::from_str(link_json).map_err(|e| malformed(&e.to_string()))
}

fn judged(link_json: &str, log: crate::judgement::SignerLog, now: f64) -> Result<String, JsValue> {
    let now = seq_of(now).ok_or_else(|| malformed("now must be a safe integer"))?;
    let link = wire_link_of(link_json)?;
    let judged =
        crate::wire::judge_wire_link(&link, log, now).map_err(|e| malformed(&e.to_string()))?;
    text_of(&judged)
}

/// Judges a signed link (JSON text) whose signer no log lists (a device-only key): the
/// `JudgedLink` as JSON. It counts for no person.
#[wasm_bindgen(js_name = judgeLinkWithoutLog)]
pub fn judge_link_without_log(link_json: &str, now: f64) -> Result<String, JsValue> {
    judged(link_json, crate::judgement::SignerLog::NoLog, now)
}

/// The hash of a signed link (JSON text), as hex: `sha256(author, source, predicate, target,
/// timestamp)`, concatenated.
#[wasm_bindgen(js_name = wireLinkHash)]
pub fn wire_link_hash_js(link_json: &str) -> Result<String, JsValue> {
    Ok(hex::encode(crate::wire::wire_link_hash(&wire_link_of(
        link_json,
    )?)))
}

/// Whether a signed link's (JSON text) signature holds under its author's `did:key`, without
/// asking any log: a cheap first check, so a forged link costs its reader no store lookup.
#[wasm_bindgen(js_name = wireSignatureHolds)]
pub fn wire_signature_holds_js(link_json: &str) -> Result<bool, JsValue> {
    Ok(crate::wire::wire_signature_holds(&wire_link_of(link_json)?))
}

/// What the device key `did` signs for its X25519 key (`x25519PublicKey`, 64 lowercase hex):
/// `coasys:room-key` over `{x25519PublicKey, did}`.
#[wasm_bindgen(js_name = roomKeySigningBytes)]
pub fn room_key_signing_bytes_js(did: &str, x25519_public_key: &str) -> Result<Vec<u8>, JsValue> {
    crate::wire::room_key_signing_bytes(did, x25519_public_key)
        .map_err(|e| malformed(&e.to_string()))
}

/// Whether `signature` (hex) is `did`'s signature over its X25519 key.
#[wasm_bindgen(js_name = verifyRoomKey)]
pub fn verify_room_key_js(did: &str, x25519_public_key: &str, signature: &str) -> bool {
    crate::wire::verify_room_key(did, x25519_public_key, signature)
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

    /// Applies one judged link like [`apply`](Self::apply), but only when it counts: a refused
    /// link leaves the replay exactly as it was (`apply` keeps some refused links, to count them
    /// should an earlier link arrive later). A reader that stores only counted links keeps its
    /// replay equal to the replay of what it stored, with no rebuild after a refusal.
    #[wasm_bindgen(js_name = tryApply)]
    pub fn try_apply(&mut self, judged_link_json: &str) -> Result<String, JsValue> {
        let link: crate::membership::JudgedLink =
            serde_json::from_str(judged_link_json).map_err(|e| malformed(&e.to_string()))?;
        let mut trial = self.graph.clone();
        let changes = trial.apply(&link).map_err(refused)?;
        self.graph = trial;
        text_of(&changes)
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
