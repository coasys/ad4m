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
//! The new device's side (`crate::device`; none of it signs, the signature comes back in):
//!
//! ```ts
//! class EncKey { static generate(): EncKey; static fromSecret(secret: Uint8Array): EncKey;
//!                secret(): Uint8Array; multikey(): string; }
//! KelHandle.deviceKeyId(signMultikey: string): string | undefined;  // listed and valid at the head
//! ed25519Multikey(raw: Uint8Array): string; x25519Multikey(raw: Uint8Array): string;
//! proofValue(signature: Uint8Array): string;
//! newR(): string;
//! deviceString(sign, enc, r, exp, hint: string | undefined, store): string;
//! preInboxAddress(sign, r): string;
//! preInboxBytes(auth): Uint8Array;  preInboxHeader(auth, sig): string;
//! openMessage(enc: EncKey, address, sealed): InboxPayload;
//! ticketScid(payload): string | undefined;
//! checkMessage(payload, me: string, chain: KelHandle, now: number): Accept;
//! claimBytes(ticket, deviceKey, encKey, canKeepMaster): Uint8Array;
//! claim(ticket, deviceKey, encKey, canKeepMaster, hint: string | undefined, coSig): Claim;
//! matchCode(scid, offerId, deviceKey): string;
//! bodyHash(body: Uint8Array): string;
//! requestBytes(req): Uint8Array;  requestHeader(req, sig): string;
//! openRecord(sealed, receipt, chain: KelHandle, stores: string[], keyId, enc: EncKey): PrivateRecord;
//! pickerDevices(record): { keyId, name, kind, address }[];
//! statementBytes(statement): Uint8Array;  signedStatement(statement, sig): { statement, sig };
//! ```
//!
//! Errors are thrown as a `ChainErrorCode` object. A KEL refusal is
//! `{ code: "policy", reason: <FoldError name>, detail: <message> }`. `didDocument` throws
//! `{ code: "document", reason: "VersionPastHead", detail }` for a version past the head and
//! `{ code: "malformed", reason }` for a version that is not a seq.

use serde_json::{json, Value};
use wasm_bindgen::prelude::*;

use crate::device::{self, EncKey as Enc, Failure};
use crate::inbox::Accept;
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

    /// The key id of the device key `sign` (a multikey) when the log lists it and has not
    /// revoked it by the head; else `undefined`. A browser trusts its own key only through this.
    #[wasm_bindgen(js_name = deviceKeyId)]
    pub fn device_key_id(&self, sign: &str) -> Option<String> {
        let state = self.chain.state();
        let id = state.device_key_id_of(&sign.to_string())?;
        state
            .key_valid_at(id, self.chain.head().seq)
            .then(|| id.clone())
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

fn failed(f: Failure) -> JsValue {
    thrown(json!({"code": f.code, "reason": f.reason, "detail": f.detail}))
}

fn value_of(v: &JsValue) -> Result<Value, JsValue> {
    let text = js_sys::JSON::stringify(v)
        .ok()
        .and_then(|s| s.as_string())
        .ok_or_else(|| malformed("not JSON"))?;
    serde_json::from_str(&text).map_err(|e| malformed(&e.to_string()))
}

fn typed<T: serde::de::DeserializeOwned>(v: &JsValue) -> Result<T, JsValue> {
    serde_json::from_value(value_of(v)?).map_err(|e| malformed(&e.to_string()))
}

fn secret_of(bytes: &[u8]) -> Result<[u8; 32], JsValue> {
    bytes.try_into().map_err(|_| malformed("a key is 32 bytes"))
}

/// The device's X25519 key, held in WASM memory and zeroed when freed.
#[wasm_bindgen(js_name = EncKey)]
pub struct EncKeyJs {
    key: Enc,
}

#[wasm_bindgen(js_class = EncKey)]
impl EncKeyJs {
    /// A new key from the platform's randomness.
    pub fn generate() -> EncKeyJs {
        EncKeyJs {
            key: Enc::generate(&mut rand_core::OsRng),
        }
    }

    /// The key with this 32-byte secret.
    #[wasm_bindgen(js_name = fromSecret)]
    pub fn from_secret(secret: &[u8]) -> Result<EncKeyJs, JsValue> {
        Ok(EncKeyJs {
            key: Enc::from_secret(secret_of(secret)?),
        })
    }

    /// The 32-byte secret. Only a surface that keeps the key beyond one link asks for it.
    pub fn secret(&self) -> Vec<u8> {
        self.key.secret().to_vec()
    }

    /// The public key as a multikey.
    pub fn multikey(&self) -> String {
        self.key.multikey()
    }
}

/// `z` + base58btc of the Ed25519 public key `raw` (32 bytes) with its multicodec prefix.
#[wasm_bindgen(js_name = ed25519Multikey)]
pub fn ed25519_multikey(raw: &[u8]) -> Result<String, JsValue> {
    Ok(ad4m_hash_chain::encode_ed25519_multikey(&secret_of(raw)?))
}

/// The same for an X25519 public key.
#[wasm_bindgen(js_name = x25519Multikey)]
pub fn x25519_multikey(raw: &[u8]) -> Result<String, JsValue> {
    Ok(crate::encode_x25519_multikey(&secret_of(raw)?))
}

/// A 64-byte Ed25519 signature as the protocol's text (`z` + base58btc).
#[wasm_bindgen(js_name = proofValue)]
pub fn proof_value(signature: &[u8]) -> Result<String, JsValue> {
    device::proof_value(signature).map_err(failed)
}

/// A fresh `r` for a device string.
#[wasm_bindgen(js_name = newR)]
pub fn new_r() -> String {
    device::new_r(&mut rand_core::OsRng)
}

/// The device string to show as a link or a QR code.
#[wasm_bindgen(js_name = deviceString)]
pub fn device_string(
    sign: &str,
    enc: &str,
    r: &str,
    exp: f64,
    hint: Option<String>,
    store: &str,
) -> Result<String, JsValue> {
    let exp = seq_of(exp).ok_or_else(|| malformed("exp must be a safe integer"))?;
    device::device_string(sign, enc, r, exp, hint.as_deref(), store).map_err(failed)
}

/// The address of the device's pre-join inbox.
#[wasm_bindgen(js_name = preInboxAddress)]
pub fn pre_inbox_address(sign: &str, r: &str) -> Result<String, JsValue> {
    device::pre_address(sign, r).map_err(failed)
}

/// The bytes to sign for a pre-join read or ack (`auth` is the `PreInboxAuth` without `sig`).
#[wasm_bindgen(js_name = preInboxBytes)]
pub fn pre_inbox_bytes(auth: JsValue) -> Result<Vec<u8>, JsValue> {
    device::pre_auth_bytes(&value_of(&auth)?).map_err(failed)
}

/// The `X-Coasys-Inbox` header value; throws for a signature that does not verify.
#[wasm_bindgen(js_name = preInboxHeader)]
pub fn pre_inbox_header(auth: JsValue, sig: &str) -> Result<String, JsValue> {
    device::pre_auth_header(&value_of(&auth)?, sig).map_err(failed)
}

/// Opens an inbox message sealed to `enc` at the pre-join `address`: the `InboxPayload`. It is
/// not checked: `checkMessage` does that against the log.
#[wasm_bindgen(js_name = openMessage)]
pub fn open_message(enc: &EncKeyJs, address: &str, sealed: &str) -> Result<JsValue, JsValue> {
    json_of(&device::open_message(&enc.key, address, sealed).map_err(failed)?)
}

/// The identity a message's offer ticket is for (the log to fetch before `checkMessage`).
#[wasm_bindgen(js_name = ticketScid)]
pub fn ticket_scid(payload: JsValue) -> Result<Option<String>, JsValue> {
    let payload: crate::inbox::InboxPayload = typed(&payload)?;
    Ok(device::ticket_scid(&payload).map(str::to_string))
}

/// Checks a message against the log: `{ kind: "claim-offer", ticket }`, `{ kind: "keep-master",
/// index }` or `{ kind: "stale" }`; throws when the sender, the signature or the ticket fails.
#[wasm_bindgen(js_name = checkMessage)]
pub fn check_message(
    payload: JsValue,
    me: &str,
    chain: &KelHandle,
    now: f64,
) -> Result<JsValue, JsValue> {
    let now = seq_of(now).ok_or_else(|| malformed("now must be a safe integer"))?;
    let accept = device::check_message(&typed(&payload)?, me, &chain.chain, now).map_err(failed)?;
    json_of(&match accept {
        Accept::ClaimOffer(ticket) => json!({"kind": "claim-offer", "ticket": ticket}),
        Accept::KeepMaster { index } => json!({"kind": "keep-master", "index": index}),
        Accept::Stale => json!({"kind": "stale"}),
    })
}

/// The bytes the device key signs to claim `ticket`'s offer.
#[wasm_bindgen(js_name = claimBytes)]
pub fn claim_bytes(
    ticket: JsValue,
    device_key: &str,
    enc_key: &str,
    can_keep_master: bool,
) -> Result<Vec<u8>, JsValue> {
    device::claim_bytes(&typed(&ticket)?, device_key, enc_key, can_keep_master).map_err(failed)
}

/// The claim to `POST` to the store, with the device key's signature `co_sig` over
/// `claimBytes`; throws for a signature that does not verify.
#[wasm_bindgen]
pub fn claim(
    ticket: JsValue,
    device_key: &str,
    enc_key: &str,
    can_keep_master: bool,
    hint: Option<String>,
    co_sig: &str,
) -> Result<JsValue, JsValue> {
    json_of(
        &device::claim(
            &typed(&ticket)?,
            device_key,
            enc_key,
            can_keep_master,
            hint.as_deref(),
            co_sig,
        )
        .map_err(failed)?,
    )
}

/// The match code both screens show.
#[wasm_bindgen(js_name = matchCode)]
pub fn match_code(scid: &str, offer_id: &str, device_key: &str) -> String {
    device::match_code(scid, offer_id, device_key)
}

/// The `bodyHash` of a store request for `body`.
#[wasm_bindgen(js_name = bodyHash)]
pub fn body_hash(body: &[u8]) -> String {
    device::body_hash(body)
}

/// The bytes the device key signs for a store request (`req` is the `StoreRequest`).
#[wasm_bindgen(js_name = requestBytes)]
pub fn request_bytes(req: JsValue) -> Result<Vec<u8>, JsValue> {
    device::request_bytes(&value_of(&req)?).map_err(failed)
}

/// The `X-Coasys-Request` header value.
#[wasm_bindgen(js_name = requestHeader)]
pub fn request_header(req: JsValue, sig: &str) -> Result<String, JsValue> {
    device::request_header(&value_of(&req)?, sig).map_err(failed)
}

/// The private record, opened with `enc` through the entry `key_id` after the receipt, the
/// writer's signature and the log have been checked. `stores` are the store keys trusted.
#[wasm_bindgen(js_name = openRecord)]
pub fn open_record(
    sealed: JsValue,
    receipt: JsValue,
    chain: &KelHandle,
    stores: Vec<String>,
    key_id: &str,
    enc: &EncKeyJs,
) -> Result<JsValue, JsValue> {
    json_of(
        &device::open_record(
            &typed(&sealed)?,
            &typed(&receipt)?,
            &chain.chain,
            &stores,
            key_id,
            &enc.key,
        )
        .map_err(failed)?,
    )
}

/// The devices of an opened record that run an executor: `{ keyId, name, kind, address }[]`.
#[wasm_bindgen(js_name = pickerDevices)]
pub fn picker_devices(record: JsValue) -> Result<JsValue, JsValue> {
    json_of(&device::picker_devices(&typed(&record)?))
}

/// The bytes the device key signs for a key statement (login or renew).
#[wasm_bindgen(js_name = statementBytes)]
pub fn statement_bytes(statement: JsValue) -> Result<Vec<u8>, JsValue> {
    device::statement_bytes(&value_of(&statement)?).map_err(failed)
}

/// `{ statement, sig }`, the `SignedKeyStatement` an executor takes.
#[wasm_bindgen(js_name = signedStatement)]
pub fn signed_statement(statement: JsValue, sig: &str) -> Result<JsValue, JsValue> {
    to_js(&device::signed_statement(&value_of(&statement)?, sig).map_err(failed)?)
}
