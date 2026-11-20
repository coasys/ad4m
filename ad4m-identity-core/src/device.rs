//! The new device's side of `did:scid:coasys:1`, for a surface that holds only a signing key it
//! cannot export (a browser's WebCrypto Ed25519 key): every format it reads or writes, as pure
//! functions on text, JSON and bytes. The `wasm` exports call these and nothing else, so a native
//! test can run the same steps.
//!
//! The device never signs here. Each `*_bytes` function gives the bytes to sign and the matching
//! function that takes the signature back checks the shape and builds the value to send:
//!
//! | step | bytes to sign | value to send |
//! |---|---|---|
//! | pre-join inbox read or ack | [`pre_auth_bytes`] | [`pre_auth_header`] |
//! | claim an offer | [`claim_bytes`] | [`claim`] |
//! | signed store request | [`request_bytes`] | [`request_header`] |
//! | login or renew at an executor | [`statement_bytes`] | [`signed_statement`] |
//!
//! The X25519 key is the one secret the device keeps in this code ([`EncKey`]); it opens the
//! inbox ([`open_message`]) and the record key ([`open_record`]).

use std::fmt;

use ad4m_hash_chain::{decode_proof_value, encode_proof_value, Hash, Multikey};
use rand_core::{CryptoRng, RngCore};
use serde::de::DeserializeOwned;
use serde_json::{json, Value};

use crate::domain::{signing_bytes, REQUEST_DOMAIN, SESSION_DOMAIN};
use crate::inbox::{
    self, Accept, InboxAddress, InboxError, InboxPayload, OfferTicket, PreInboxAuth,
};
use crate::keys::{x25519_multikey_of, X25519Secret};
use crate::link::{self, Claim, DeviceString, LinkError, CLAIM_DOMAIN};
use crate::record::{self, PrivateRecord, RecordError, RecordReceipt, SealedRecord};
use crate::requests::{SignedStoreRequest, StoreRequest};
use crate::KelChain;

/// Why a step failed: `code` names the part (`malformed`, `inbox`, `link`, `record`), `reason`
/// the error's stable name, `detail` its message.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Failure {
    pub code: &'static str,
    pub reason: String,
    pub detail: String,
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.reason, self.detail)
    }
}

fn malformed(detail: impl fmt::Display) -> Failure {
    Failure {
        code: "malformed",
        reason: "Malformed".into(),
        detail: detail.to_string(),
    }
}

impl From<InboxError> for Failure {
    fn from(e: InboxError) -> Self {
        Failure {
            code: "inbox",
            reason: e.name().into(),
            detail: e.to_string(),
        }
    }
}

impl From<LinkError> for Failure {
    fn from(e: LinkError) -> Self {
        Failure {
            code: "link",
            reason: e.name().into(),
            detail: e.to_string(),
        }
    }
}

impl From<RecordError> for Failure {
    fn from(e: RecordError) -> Self {
        Failure {
            code: "record",
            reason: e.name().into(),
            detail: e.to_string(),
        }
    }
}

fn parse<T: DeserializeOwned>(value: &Value, what: &str) -> Result<T, Failure> {
    serde_json::from_value(value.clone()).map_err(|e| malformed(format!("{what}: {e}")))
}

/// `value` with `sig` set to `sig` (a signature is the last member the header adds).
fn with_sig(value: &Value, sig: &str) -> Result<Value, Failure> {
    let mut value = value.clone();
    value
        .as_object_mut()
        .ok_or_else(|| malformed("expected an object"))?
        .insert("sig".into(), json!(sig));
    Ok(value)
}

/// The device's X25519 key: it receives what is sealed to the device. Zeroed when dropped.
pub struct EncKey(X25519Secret);

impl EncKey {
    /// A new key from `rng`.
    pub fn generate<R: CryptoRng + RngCore>(rng: &mut R) -> EncKey {
        EncKey(X25519Secret::random_from_rng(rng))
    }

    /// The key whose secret is `secret`.
    pub fn from_secret(secret: [u8; 32]) -> EncKey {
        EncKey(X25519Secret::from(secret))
    }

    /// The secret, for a surface that keeps the key beyond the link.
    pub fn secret(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// The public key as a multikey (`z6LS…`).
    pub fn multikey(&self) -> Multikey {
        x25519_multikey_of(&self.0)
    }
}

/// A fresh `r` for a device string: 16 random bytes, base58btc.
pub fn new_r<R: CryptoRng + RngCore>(rng: &mut R) -> String {
    let mut r = [0u8; link::DEVICE_R_LEN];
    rng.fill_bytes(&mut r);
    bs58::encode(r).into_string()
}

/// The device string the holder scans: `coasys:device:1:…`. `hint` is cut to 64 characters.
pub fn device_string(
    sign: &str,
    enc: &str,
    r: &str,
    exp: u64,
    hint: Option<&str>,
    store: &str,
) -> Result<String, Failure> {
    let string = DeviceString {
        sign: sign.to_string(),
        enc: enc.to_string(),
        r: r.to_string(),
        exp,
        hint: hint.map(|h| h.chars().take(link::HINT_MAX_CHARS).collect()),
        store: store.to_string(),
    };
    // Parsing the encoded text checks every field's shape.
    DeviceString::parse(&string.encode())?;
    Ok(string.encode())
}

/// The pre-join inbox address of `sign` and `r`.
pub fn pre_address(sign: &str, r: &str) -> Result<String, Failure> {
    Ok(inbox::pre_address(sign, r)?)
}

/// The bytes the device key signs for a pre-join read or ack: `auth` is the `PreInboxAuth`
/// without `sig`.
pub fn pre_auth_bytes(auth: &Value) -> Result<Vec<u8>, Failure> {
    Ok(inbox::pre_auth_signing_bytes(&parse(
        &with_sig(auth, "")?,
        "auth",
    )?)?)
}

/// The `X-Coasys-Inbox` header of `auth` signed with `sig`; refuses a signature that does not
/// verify or an address that is not the device's.
pub fn pre_auth_header(auth: &Value, sig: &str) -> Result<String, Failure> {
    let auth: PreInboxAuth = parse(&with_sig(auth, sig)?, "auth")?;
    inbox::verify_pre_auth(&auth)?;
    header_value(&auth)
}

/// `base64url(JCS(value))`: the text of an `X-Coasys-*` header.
fn header_value<T: serde::Serialize>(value: &T) -> Result<String, Failure> {
    use base64::Engine;
    let value = serde_json::to_value(value).map_err(malformed)?;
    let text = ad4m_hash_chain::canonical_json(&value).map_err(malformed)?;
    Ok(base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(text))
}

/// The hash a store request's `bodyHash` holds for `body`.
pub fn body_hash(body: &[u8]) -> Hash {
    ad4m_hash_chain::hash_bytes(body)
}

/// The bytes the device key signs for a store request: `req` is the `StoreRequest`.
pub fn request_bytes(req: &Value) -> Result<Vec<u8>, Failure> {
    let req: StoreRequest = parse(req, "req")?;
    signing_bytes(REQUEST_DOMAIN, &req).map_err(malformed)
}

/// The `X-Coasys-Request` header of `req` signed with `sig`.
pub fn request_header(req: &Value, sig: &str) -> Result<String, Failure> {
    header_value(&SignedStoreRequest {
        req: parse(req, "req")?,
        sig: sig.to_string(),
    })
}

/// Opens an inbox message sealed to `enc` at the pre-join `address`.
pub fn open_message(enc: &EncKey, address: &str, sealed: &str) -> Result<InboxPayload, Failure> {
    Ok(inbox::open(
        &enc.0,
        &InboxAddress::Pre(address.to_string()),
        sealed,
    )?)
}

/// What `payload` asks of the device `me` (its signing multikey), checked against the log `chain`
/// of the identity the payload names, at `now`: the sender is a master key of the log and its
/// signature verifies, and a ticket's signed offer holds and is bound to `me`.
pub fn check_message(
    payload: &InboxPayload,
    me: &str,
    chain: &KelChain,
    now: u64,
) -> Result<Accept, Failure> {
    Ok(inbox::check_payload(payload, me, chain, now)?)
}

/// The identity a payload's ticket is for: the log to fetch and fold before [`check_message`].
pub fn ticket_scid(payload: &InboxPayload) -> Option<&str> {
    match &payload.body {
        inbox::InboxBody::OfferTicket(ticket) => Some(&ticket.scid),
        _ => None,
    }
}

/// The bytes the device key signs to claim `ticket`'s offer.
pub fn claim_bytes(
    ticket: &OfferTicket,
    device_key: &str,
    enc_key: &str,
    can_keep_master: bool,
) -> Result<Vec<u8>, Failure> {
    let claim = unsigned_claim(ticket, device_key, enc_key, can_keep_master, None);
    signing_bytes(CLAIM_DOMAIN, &link::claim_body(&ticket.scid, &claim)).map_err(malformed)
}

fn unsigned_claim(
    ticket: &OfferTicket,
    device_key: &str,
    enc_key: &str,
    can_keep_master: bool,
    hint: Option<String>,
) -> Claim {
    Claim {
        offer_id: ticket.offer_id.clone(),
        s: ticket.s.clone(),
        device_key: device_key.to_string(),
        enc_key: enc_key.to_string(),
        can_keep_master,
        hint,
        co_sig: String::new(),
    }
}

/// The claim to post to the store, with the device key's `co_sig` over [`claim_bytes`]; refuses
/// a co-signature that does not verify.
pub fn claim(
    ticket: &OfferTicket,
    device_key: &str,
    enc_key: &str,
    can_keep_master: bool,
    hint: Option<&str>,
    co_sig: &str,
) -> Result<Claim, Failure> {
    let mut claim = unsigned_claim(
        ticket,
        device_key,
        enc_key,
        can_keep_master,
        hint.map(|h| h.chars().take(link::HINT_MAX_CHARS).collect()),
    );
    claim.co_sig = co_sig.to_string();
    link::verify_claim(&ticket.scid, &claim)?;
    Ok(claim)
}

/// The match code both screens show.
pub fn match_code(scid: &str, offer_id: &str, device_key: &str) -> String {
    link::match_code(scid, offer_id, device_key)
}

/// The bytes the device key signs for a key statement (`coasys:session`): a login or a renewal
/// at an executor. `statement` is the executor's `KeyStatement`, absent members left out.
pub fn statement_bytes(statement: &Value) -> Result<Vec<u8>, Failure> {
    if !statement.is_object() {
        return Err(malformed("a statement is an object"));
    }
    signing_bytes(SESSION_DOMAIN, statement).map_err(malformed)
}

/// `{ statement, sig }`: the `SignedKeyStatement` the executor takes.
pub fn signed_statement(statement: &Value, sig: &str) -> Result<Value, Failure> {
    if decode_proof_value(sig).is_none() {
        return Err(malformed("sig is a base58btc proof value"));
    }
    Ok(json!({ "statement": statement, "sig": sig }))
}

/// A 64-byte Ed25519 signature as the text the protocol uses (`z` + base58btc).
pub fn proof_value(signature: &[u8]) -> Result<String, Failure> {
    let signature: &[u8; 64] = signature
        .try_into()
        .map_err(|_| malformed("an Ed25519 signature is 64 bytes"))?;
    Ok(encode_proof_value(signature))
}

/// The record `sealed` of the identity whose log is `chain`, opened with `enc` through the
/// entry `key_id`, after the store's `receipt` (signed by one of `stores`, `did:key:…` as a store
/// names itself) and the writer's signature have been checked.
pub fn open_record(
    sealed: &SealedRecord,
    receipt: &RecordReceipt,
    chain: &KelChain,
    stores: &[String],
    key_id: &str,
    enc: &EncKey,
) -> Result<PrivateRecord, Failure> {
    let keys: Vec<Multikey> = stores
        .iter()
        .filter_map(|s| s.strip_prefix("did:key:").map(str::to_string))
        .collect();
    Ok(record::open_record(
        sealed, receipt, chain, &keys, key_id, &enc.0,
    )?)
}

/// One of the person's devices that can answer a sign-in: it runs an executor at an address.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct PickerDevice {
    pub key_id: String,
    pub name: String,
    pub kind: Value,
    pub address: String,
}

/// The devices of `record` that run an executor and have an address, in key id order.
pub fn picker_devices(record: &PrivateRecord) -> Vec<PickerDevice> {
    record
        .devices
        .iter()
        .filter(|(_, d)| d.runs_executor)
        .filter_map(|(id, d)| {
            Some(PickerDevice {
                key_id: id.clone(),
                name: d.name.clone(),
                kind: serde_json::to_value(d.kind).ok()?,
                address: d.address.clone()?,
            })
        })
        .collect()
}
