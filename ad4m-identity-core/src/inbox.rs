//! The device inbox: every device-to-device message of device linking, before and after the
//! device joins (spec section 17).
//!
//! A message is an [`InboxPayload`] signed by a master key of the identity under
//! `coasys:inbox` and sealed with HPKE ([`seal`]) to the receiving device's X25519 key, so the
//! store keeps only ciphertext and can neither read nor strip the signature. The store places
//! a message at an [`InboxAddress`]: a pre-join address ([`pre_address`]) that a device's
//! string gives before its key is in the log, or the device's key id after it joined. The
//! device opens it ([`open`]) and checks it against its replayed copy of the log
//! ([`check_payload`]) before it acts.
//!
//! A pre-join inbox is read with a [`PreInboxAuth`]: the device's key is in no log yet, so a
//! store request cannot carry it.
//!
//! Pure functions: no I/O, no clock (callers pass `now`), and the caller's RNG for a seal.

use std::fmt;

use ad4m_hash_chain::{canonical_json, decode_proof_value, sha256, Multikey};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use zeroize::Zeroizing;

use crate::domain::{signing_bytes, verify_signed, DomainError};
use crate::keys::{is_ed25519_multikey, X25519Secret};
use crate::link::{
    decode_secret, offer_commit, verify_offer, LinkError, OfferString, SignedBy, SignedOffer,
};
use crate::seal;
use crate::{KelChain, KeyId};

/// The signing domain of inbox payloads and of a pre-join read; also the `info` label of the
/// seal (`"coasys:inbox" ‖ 0x00 ‖ address`).
pub const INBOX_DOMAIN: &str = "coasys:inbox";
/// A joined message waits up to 30 days.
pub const JOINED_TTL_S: u64 = 30 * 86_400;
/// The longest sealed text (base64url) the store keeps.
pub const MAX_SEALED_BYTES: usize = 8 * 1024;
/// Messages per pre-join address.
pub const MAX_PRE_MESSAGES: usize = 2;
/// Messages per joined inbox.
pub const MAX_JOINED_MESSAGES: usize = 16;
/// How far ahead a pre-join read's `expires` may lie (as a store request's).
pub const PRE_AUTH_MAX_AHEAD_S: u64 = 300;
/// The prefix of a pre-join address's text.
pub const PRE_PREFIX: &str = "pre:";

/// Where the store keeps a message.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InboxAddress {
    /// Before the device joins: [`pre_address`] of its device string.
    Pre(String),
    /// After the device joins: the identity and the device's key id in its log.
    Joined { scid: String, key_id: KeyId },
}

impl InboxAddress {
    /// The address as text: `pre:<a>` or `<scid>/<keyId>`. It enters the seal's `info`, so a
    /// message sealed for one address does not open at another.
    pub fn text(&self) -> String {
        match self {
            InboxAddress::Pre(a) => format!("{PRE_PREFIX}{a}"),
            InboxAddress::Joined { scid, key_id } => format!("{scid}/{key_id}"),
        }
    }
}

/// Why an inbox message, its seal or a pre-join read is refused.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum InboxError {
    #[error("malformed: {0}")]
    Malformed(String),
    #[error("the message does not open with this key at this address")]
    Open,
    #[error("the message is for another device")]
    Recipient,
    #[error("the signer is not a master key of the identity at a position the log holds")]
    Signer,
    #[error("the signature does not verify")]
    Signature,
    #[error("the offer ticket does not hold: {0}")]
    Ticket(LinkError),
    #[error("the secret is not master key {0} of the log")]
    Master(u32),
    #[error("{0}")]
    Domain(#[from] DomainError),
}

impl InboxError {
    /// A short name for error bodies.
    pub fn name(&self) -> &'static str {
        match self {
            InboxError::Malformed(_) => "malformed",
            InboxError::Open => "open",
            InboxError::Recipient => "recipient",
            InboxError::Signer => "signer",
            InboxError::Signature => "signature",
            InboxError::Ticket(_) => "ticket",
            InboxError::Master(_) => "master",
            InboxError::Domain(_) => "domain",
        }
    }
}

fn malformed(what: impl Into<String>) -> InboxError {
    InboxError::Malformed(what.into())
}

/// True when `r` is a device string's `r`: 16 bytes of base58btc.
fn is_device_r(r: &str) -> bool {
    r.len() <= 22
        && bs58::decode(r)
            .into_vec()
            .is_ok_and(|b| b.len() == crate::link::DEVICE_R_LEN)
}

/// The pre-join address of a device string's `sign` and `r`:
/// `base58btc(sha256(signing_bytes("coasys:inbox", {"sign": sign, "r": r})))`.
pub fn pre_address(sign: &str, r: &str) -> Result<String, InboxError> {
    if !is_ed25519_multikey(sign) {
        return Err(malformed("sign is an Ed25519 multikey"));
    }
    if !is_device_r(r) {
        return Err(malformed("r is 16 bytes of base58btc"));
    }
    let bytes = signing_bytes(INBOX_DOMAIN, &json!({ "sign": sign, "r": r }))?;
    Ok(bs58::encode(sha256(&bytes)).into_string())
}

/// What a holder sends a device it scanned: the offer to claim, the offer string's fields and
/// the signed offer, which the device checks against the log before it claims.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct OfferTicket {
    /// The store's base URL.
    pub store: String,
    pub scid: String,
    pub offer_id: String,
    /// The offer secret, 32 bytes, base58btc.
    pub s: String,
    pub signed_offer: SignedOffer,
}

impl OfferTicket {
    /// The offer string the ticket stands for.
    pub fn offer_string(&self) -> OfferString {
        OfferString {
            store: self.store.clone(),
            scid: self.scid.clone(),
            offer_id: self.offer_id.clone(),
            s: self.s.clone(),
        }
    }
}

/// What a message says. `Debug` never prints a master key's secret.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case", deny_unknown_fields)]
pub enum InboxBody {
    /// Linking with the holder scanning: the bound offer the device claims.
    OfferTicket(Box<OfferTicket>),
    /// A master key for a device that keeps it, sealed to the device's own `enc` key.
    #[serde(rename_all = "camelCase")]
    Master {
        /// The identity, bare.
        did: String,
        /// The master key's number: `masters[index]` of the log.
        index: u32,
        /// The Ed25519 secret of that master key, 32 bytes, base58btc.
        secret: String,
    },
}

impl fmt::Debug for InboxBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InboxBody::OfferTicket(ticket) => f.debug_tuple("OfferTicket").field(ticket).finish(),
            InboxBody::Master { did, index, .. } => f
                .debug_struct("Master")
                .field("did", did)
                .field("index", index)
                .finish_non_exhaustive(),
        }
    }
}

/// The plaintext inside a sealed message.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct InboxPayload {
    /// The receiving device's signing key; the device refuses any other.
    pub to: Multikey,
    pub body: InboxBody,
    /// Unix seconds, the sender's clock.
    pub sent_at: u64,
    /// A master key of the identity at a log position.
    pub by: SignedBy,
    /// That master key over [`payload_signing_bytes`].
    pub sig: String,
}

/// The bytes a master key signs for a payload:
/// `signing_bytes("coasys:inbox", {to, body, sentAt, by})`.
pub fn payload_signing_bytes(
    to: &str,
    body: &InboxBody,
    sent_at: u64,
    by: &SignedBy,
) -> Result<Vec<u8>, InboxError> {
    Ok(signing_bytes(
        INBOX_DOMAIN,
        &json!({ "to": to, "body": body, "sentAt": sent_at, "by": by }),
    )?)
}

/// A payload to `to`, signed by `master` at the position `by`.
#[cfg(feature = "build")]
pub fn sign_payload(
    to: Multikey,
    body: InboxBody,
    sent_at: u64,
    by: SignedBy,
    master: &dyn crate::build::Signer,
) -> Result<InboxPayload, InboxError> {
    // The bytes hold the body, a master key's secret too: wiped once signed.
    let bytes = Zeroizing::new(payload_signing_bytes(&to, &body, sent_at, &by)?);
    let sig = ad4m_hash_chain::encode_proof_value(&master.sign(&bytes));
    Ok(InboxPayload {
        to,
        body,
        sent_at,
        by,
        sig,
    })
}

/// The seal's `info`: `"coasys:inbox" ‖ 0x00 ‖ address`.
pub fn seal_info(address: &InboxAddress) -> Vec<u8> {
    seal::info(INBOX_DOMAIN, &address.text())
}

/// Seals `payload` (its canonical JSON) to the X25519 key `to_enc` for `address`.
pub fn seal<R: CryptoRng + RngCore>(
    to_enc: &str,
    address: &InboxAddress,
    payload: &InboxPayload,
    rng: &mut R,
) -> Result<String, InboxError> {
    let value = serde_json::to_value(payload).map_err(|e| malformed(e.to_string()))?;
    let text = Zeroizing::new(canonical_json(&value).map_err(|e| malformed(e.to_string()))?);
    let sealed = seal::seal(to_enc, &seal_info(address), text.as_bytes(), rng)
        .map_err(|_| malformed("the recipient is an X25519 multikey"))?;
    if sealed.len() > MAX_SEALED_BYTES {
        return Err(malformed(format!(
            "a sealed message is at most {MAX_SEALED_BYTES} bytes"
        )));
    }
    Ok(sealed)
}

/// Opens a message sealed for `address` with the device's X25519 `secret`. It does not check
/// the payload: [`check_payload`] does, against the log.
pub fn open(
    secret: &X25519Secret,
    address: &InboxAddress,
    sealed: &str,
) -> Result<InboxPayload, InboxError> {
    if sealed.len() > MAX_SEALED_BYTES {
        return Err(InboxError::Open);
    }
    let bytes = seal::open(secret, &seal_info(address), sealed).map_err(|_| InboxError::Open)?;
    serde_json::from_slice(&bytes).map_err(|e| malformed(e.to_string()))
}

/// What a device does with a payload that passed [`check_payload`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Accept {
    /// Claim the ticket's offer (after showing the identity and the match code).
    ClaimOffer(Box<OfferTicket>),
    /// Keep master key `index`, the current one.
    KeepMaster { index: u32 },
    /// A master key that no longer counts: acknowledge the message and drop it.
    Stale,
}

/// The device's checks after opening, against its replayed copy of the log, at `now`:
/// `to` is `me`; `by` names a master key of the identity, valid at a position the log holds
/// with that hash; the signature verifies. An offer ticket's signed offer verifies
/// (`link::verify_offer`), names the ticket's offer, is signed by the same master key, is
/// bound to `me`, and its commit is the ticket's `s`. A `Master` secret must be
/// `masters[index]` of the log; `index` must be the current master, or the answer is `Stale`.
///
/// A payload carries no single-use mark: a store can deliver an exact copy of a message again,
/// and while its master is still current the copy passes as the first did. A consumer acts
/// idempotently: keeping a master key it already keeps changes nothing, and a ticket whose
/// offer it already claimed fails at the store's claim.
pub fn check_payload(
    p: &InboxPayload,
    me: &str,
    chain: &KelChain,
    now: u64,
) -> Result<Accept, InboxError> {
    if p.to != me {
        return Err(InboxError::Recipient);
    }
    let state = chain.state();
    let master = state
        .masters
        .iter()
        .find(|m| m.id == p.by.key_id)
        .ok_or(InboxError::Signer)?;
    if chain.hash_at(p.by.kel_seq) != Some(&p.by.kel_hash)
        || !state.key_valid_at(&master.id, p.by.kel_seq)
    {
        return Err(InboxError::Signer);
    }
    let sig = decode_proof_value(&p.sig).ok_or(InboxError::Signature)?;
    let signed = json!({ "to": p.to, "body": p.body, "sentAt": p.sent_at, "by": p.by });
    if !verify_signed(INBOX_DOMAIN, &signed, &master.sign, &sig) {
        return Err(InboxError::Signature);
    }
    match &p.body {
        InboxBody::OfferTicket(ticket) => {
            check_ticket(ticket, &p.by, me, chain, now)?;
            Ok(Accept::ClaimOffer(ticket.clone()))
        }
        InboxBody::Master { did, index, secret } => {
            if did != &state.did {
                return Err(InboxError::Master(*index));
            }
            let listed = state
                .masters
                .get(*index as usize)
                .ok_or(InboxError::Master(*index))?;
            let secret = Zeroizing::new(decode_secret(secret).ok_or(InboxError::Master(*index))?);
            let key = ed25519_dalek::SigningKey::from_bytes(&secret);
            let multikey = ad4m_hash_chain::encode_ed25519_multikey(key.verifying_key().as_bytes());
            if multikey != listed.sign {
                return Err(InboxError::Master(*index));
            }
            let current = state.current_master().map(|m| &m.id) == Some(&listed.id);
            Ok(if current {
                Accept::KeepMaster { index: *index }
            } else {
                Accept::Stale
            })
        }
    }
}

fn check_ticket(
    ticket: &OfferTicket,
    by: &SignedBy,
    me: &str,
    chain: &KelChain,
    now: u64,
) -> Result<(), InboxError> {
    let signed = &ticket.signed_offer;
    verify_offer(signed, chain, now).map_err(InboxError::Ticket)?;
    let offer = &signed.offer;
    if offer.scid != ticket.scid || offer.offer_id != ticket.offer_id {
        return Err(InboxError::Ticket(LinkError::Malformed(
            "the signed offer is not the ticket's offer".into(),
        )));
    }
    if signed.by.key_id != by.key_id {
        return Err(InboxError::Ticket(LinkError::Signer));
    }
    if offer.device_key.as_deref() != Some(me) {
        return Err(InboxError::Ticket(LinkError::Bound));
    }
    let s = decode_secret(&ticket.s).ok_or(InboxError::Ticket(LinkError::Secret))?;
    if offer_commit(&s) != offer.commit {
        return Err(InboxError::Ticket(LinkError::Secret));
    }
    Ok(())
}

/// A pre-join read or acknowledgement: header `X-Coasys-Inbox` = base64url(JCS(PreInboxAuth)).
/// `sig` is the device key `sign` over [`pre_auth_signing_bytes`]; `pre_address(sign, r)` must
/// be `address`. Like a store request it names the store it is for (`store`, the store key's
/// `did:key`, so a read cannot be replayed to another store), its method and path (without the
/// query), and its nonce is single use.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct PreInboxAuth {
    pub address: String,
    pub sign: Multikey,
    pub r: String,
    /// did:key of the store the read is for: a store refuses a read for another
    /// (`403 store`), as it refuses a signed request.
    pub store: String,
    pub method: String,
    pub path: String,
    /// 16 bytes, base58btc.
    pub nonce: String,
    pub expires: u64,
    pub sig: String,
}

fn pre_auth_value(auth: &PreInboxAuth) -> Value {
    json!({
        "address": auth.address,
        "sign": auth.sign,
        "r": auth.r,
        "store": auth.store,
        "method": auth.method,
        "path": auth.path,
        "nonce": auth.nonce,
        "expires": auth.expires,
    })
}

/// The bytes the device key signs: `signing_bytes("coasys:inbox", auth without sig)`.
pub fn pre_auth_signing_bytes(auth: &PreInboxAuth) -> Result<Vec<u8>, InboxError> {
    Ok(signing_bytes(INBOX_DOMAIN, &pre_auth_value(auth))?)
}

/// A pre-join read of the inbox of `sign`'s device string with `r`, at the store whose key is
/// the `did:key` `store`, signed by `device`.
#[cfg(feature = "build")]
pub fn sign_pre_auth(
    r: &str,
    store: &str,
    method: &str,
    path: &str,
    nonce: &str,
    expires: u64,
    device: &dyn crate::build::Signer,
) -> Result<PreInboxAuth, InboxError> {
    let sign = device.key();
    let mut auth = PreInboxAuth {
        address: pre_address(&sign, r)?,
        sign,
        r: r.to_string(),
        store: store.to_string(),
        method: method.to_string(),
        path: path.to_string(),
        nonce: nonce.to_string(),
        expires,
        sig: String::new(),
    };
    auth.sig = ad4m_hash_chain::encode_proof_value(&device.sign(&pre_auth_signing_bytes(&auth)?));
    Ok(auth)
}

/// Checks a pre-join read's address and signature. The store checks `store`, `method`, `path`,
/// `expires` and the nonce against the request it serves.
pub fn verify_pre_auth(auth: &PreInboxAuth) -> Result<(), InboxError> {
    if pre_address(&auth.sign, &auth.r)? != auth.address {
        return Err(malformed("address is not pre_address(sign, r)"));
    }
    let sig = decode_proof_value(&auth.sig).ok_or(InboxError::Signature)?;
    if !verify_signed(INBOX_DOMAIN, &pre_auth_value(auth), &auth.sign, &sig) {
        return Err(InboxError::Signature);
    }
    Ok(())
}

/// Checks a sealed text's shape as the store keeps it: base64url, at most
/// [`MAX_SEALED_BYTES`]. The store cannot open it.
pub fn check_sealed(sealed: &str) -> Result<(), InboxError> {
    if sealed.len() > MAX_SEALED_BYTES {
        return Err(malformed(format!(
            "a sealed message is at most {MAX_SEALED_BYTES} bytes"
        )));
    }
    if !seal::is_base64url(sealed) {
        return Err(malformed("sealed is base64url without padding"));
    }
    Ok(())
}
