//! Offers and claims (AR-02 4.3–4.5): how a holder of the current master key lets a new device
//! put its key into the log.
//!
//! The holder opens an [`Offer`] (a commit of a 32-byte secret `s`, an expiry, an optional bound
//! key or bot scope) and signs it with the current master key ([`sign_offer`], domain
//! `coasys:offer`). It hands the device an offer string that carries `s` ([`OfferString`]). The
//! device makes a [`Claim`]: its signing key, its X25519 key, `s`, and a co-signature over
//! [`ClaimBody`] (domain `coasys:claim`). Both sides show the [`match_code`]; the holder then
//! appends the `add` that spends the claim (`build::add_claimed`), and the store checks it with
//! [`check_add`] (rule A7).
//!
//! Pure functions: no I/O, no clock (callers pass `now`), no randomness (callers draw `s` and
//! the offer id).

use ad4m_hash_chain::{
    canonical_json, decode_hash, decode_proof_value, encode_hash, sha256, Hash, Multikey,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use crate::domain::{signing_bytes, verify_signed, DomainError};
use crate::event::{BotScope, Event, KeyLife, Op, BOT_ACTIONS};
use crate::keys::{is_ed25519_multikey, is_x25519_multikey};
use crate::{KelChain, KeyId, KeyState};

/// An offer takes claims for at most 5 minutes after it is issued.
pub const OFFER_TTL_S: u64 = 300;
/// A claimed offer accepts its `add` until `expires` plus 300 s.
pub const CONFIRM_GRACE_S: u64 = 300;
/// Offers in `open` or `claimed` per identity.
pub const MAX_OPEN_OFFERS: usize = 5;
/// Wrong secrets that lock an offer.
pub const MAX_WRONG_SECRETS: u32 = 5;
/// The signing domain of offers.
pub const OFFER_DOMAIN: &str = "coasys:offer";
/// The signing domain of claim co-signatures.
pub const CLAIM_DOMAIN: &str = "coasys:claim";
/// The prefix of the offer commit `sha256("coasys:offer" ‖ s)` (a hash input, not a signature).
pub const OFFER_COMMIT_PREFIX: &str = "coasys:offer";
/// The match code's hash label (AR-01 4.2, second table).
pub const MATCH_LABEL: &str = "coasys:match";
/// The prefix of an offer string.
pub const OFFER_STRING_PREFIX: &str = "coasys:offer:1:";
/// The prefix of a device string.
pub const DEVICE_STRING_PREFIX: &str = "coasys:device:1:";
/// The page that opens an offer link; the offer string sits only in the fragment.
pub const OFFER_LINK_PAGE: &str = "https://coasys.id/o";
/// Bytes of the offer secret `s`.
pub const SECRET_LEN: usize = 32;
/// Bytes of an offer id.
pub const OFFER_ID_LEN: usize = 16;
/// Bytes of a device string's `r`.
pub const DEVICE_R_LEN: usize = 16;
/// Characters of a claim's or device string's hint.
pub const HINT_MAX_CHARS: usize = 64;

/// What the offer adds: a device (an app), a node (an executor) or a bot.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OfferKind {
    Device,
    Node,
    Bot,
}

/// An offer, as the holder signs it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Offer {
    pub scid: String,
    /// 16 random bytes, base58btc.
    pub offer_id: String,
    pub kind: OfferKind,
    /// `multihash_b58(sha256("coasys:offer" ‖ s))`.
    pub commit: Hash,
    /// Unix seconds, at most issue time + [`OFFER_TTL_S`].
    pub expires: u64,
    /// A bound offer: only this signing key may claim.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub device_key: Option<Multikey>,
    /// A bot offer names the actions and expiry the holder will add.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bot: Option<BotScope>,
}

/// The log position of the master key that signed an offer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SignedBy {
    /// A master key id, `m<n>`.
    pub key_id: KeyId,
    pub kel_seq: u64,
    pub kel_hash: Hash,
}

/// `coasys:offer` over `{offer, by}`, by the current master key.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SignedOffer {
    pub offer: Offer,
    pub by: SignedBy,
    /// `z` + base58btc of the 64-byte signature.
    pub sig: String,
}

/// A new device's claim on an offer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Claim {
    pub offer_id: String,
    /// 32 bytes, base58btc. The store checks it against `commit` and never stores it.
    pub s: String,
    /// Ed25519, made on the device.
    pub device_key: Multikey,
    /// X25519 of the device key.
    pub enc_key: Multikey,
    /// True only for an app that may keep the master key.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub can_keep_master: bool,
    /// "Pixel 8", at most 64 characters; private, never enters the log.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
    /// The device key over `signing_bytes("coasys:claim", ClaimBody)`.
    pub co_sig: String,
}

/// What the claim's co-signature covers. `enc_key` and `can_keep_master` are in it, so a store
/// cannot swap the key a master key is later sealed to, nor make a server's claim look like an app's.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ClaimBody<'a> {
    pub scid: &'a str,
    pub offer_id: &'a str,
    pub device_key: &'a str,
    pub enc_key: &'a str,
    pub can_keep_master: bool,
}

/// What the holder sees of a claim; `s` is gone.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ClaimView {
    pub device_key: Multikey,
    pub enc_key: Multikey,
    pub can_keep_master: bool,
    #[serde(default)]
    pub hint: Option<String>,
    pub co_sig: String,
    pub claimed_at: u64,
}

/// The state of an offer (AR-02 4.3.3). Only `Open` and `Claimed` are live.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "kebab-case")]
pub enum OfferState {
    Open,
    Claimed { claim: ClaimView },
    Spent { seq: u64 },
    Expired,
    Cancelled,
    Contested,
    Locked,
    Superseded,
}

impl OfferState {
    /// The wire name (`"open"`, `"claimed"`, …).
    pub fn name(&self) -> &'static str {
        match self {
            OfferState::Open => "open",
            OfferState::Claimed { .. } => "claimed",
            OfferState::Spent { .. } => "spent",
            OfferState::Expired => "expired",
            OfferState::Cancelled => "cancelled",
            OfferState::Contested => "contested",
            OfferState::Locked => "locked",
            OfferState::Superseded => "superseded",
        }
    }

    /// True for `Open` and `Claimed`.
    pub fn is_live(&self) -> bool {
        matches!(self, OfferState::Open | OfferState::Claimed { .. })
    }
}

/// Why an offer, a claim, an offer string or an `add` that spends a claim is refused.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum LinkError {
    #[error("malformed: {0}")]
    Malformed(String),
    #[error("the offer names another identity")]
    WrongScid,
    #[error("the signer is not the current master key")]
    Signer,
    #[error("the log moved past the position the offer names")]
    Moved,
    #[error("the signature does not verify")]
    Signature,
    #[error("the offer has expired")]
    Expired,
    #[error("the offer expires more than {OFFER_TTL_S} s ahead")]
    TooLong,
    #[error("the secret does not match the offer's commit")]
    Secret,
    #[error("the offer is bound to another key")]
    Bound,
    #[error("the claim's co-signature does not verify")]
    CoSig,
    #[error("the add does not spend the claim: {0}")]
    Claim(&'static str),
    #[error("{0}")]
    Domain(#[from] DomainError),
}

impl LinkError {
    /// A short name for error bodies.
    pub fn name(&self) -> &'static str {
        match self {
            LinkError::Malformed(_) => "malformed",
            LinkError::WrongScid => "wrong-chain",
            LinkError::Signer => "signer",
            LinkError::Moved => "moved",
            LinkError::Signature => "signature",
            LinkError::Expired => "expired",
            LinkError::TooLong => "expires",
            LinkError::Secret => "secret",
            LinkError::Bound => "bound",
            LinkError::CoSig => "cosig",
            LinkError::Claim(_) => "claim",
            LinkError::Domain(_) => "domain",
        }
    }
}

fn malformed(what: impl Into<String>) -> LinkError {
    LinkError::Malformed(what.into())
}

fn b58_exact(text: &str, len: usize) -> Option<Vec<u8>> {
    bs58::decode(text)
        .into_vec()
        .ok()
        .filter(|bytes| bytes.len() == len)
}

/// The secret `s` from its base58btc text, if it is 32 bytes.
pub fn decode_secret(s: &str) -> Option<[u8; SECRET_LEN]> {
    b58_exact(s, SECRET_LEN).and_then(|b| b.try_into().ok())
}

/// The offer commit: `multihash_b58(sha256("coasys:offer" ‖ s))`.
pub fn offer_commit(s: &[u8; SECRET_LEN]) -> Hash {
    let mut input = OFFER_COMMIT_PREFIX.as_bytes().to_vec();
    input.extend_from_slice(s);
    encode_hash(&sha256(&input))
}

/// The bytes the master key signs for an offer: `signing_bytes("coasys:offer", {offer, by})`.
pub fn offer_signing_bytes(offer: &Offer, by: &SignedBy) -> Result<Vec<u8>, LinkError> {
    Ok(signing_bytes(
        OFFER_DOMAIN,
        &json!({ "offer": offer, "by": by }),
    )?)
}

/// The claim body of `claim` for the identity `scid`.
pub fn claim_body<'a>(scid: &'a str, claim: &'a Claim) -> ClaimBody<'a> {
    ClaimBody {
        scid,
        offer_id: &claim.offer_id,
        device_key: &claim.device_key,
        enc_key: &claim.enc_key,
        can_keep_master: claim.can_keep_master,
    }
}

/// What the holder sees of `claim`, stored at `claimed_at`.
pub fn claim_view(claim: &Claim, claimed_at: u64) -> ClaimView {
    ClaimView {
        device_key: claim.device_key.clone(),
        enc_key: claim.enc_key.clone(),
        can_keep_master: claim.can_keep_master,
        hint: claim.hint.clone(),
        co_sig: claim.co_sig.clone(),
        claimed_at,
    }
}

fn check_bot_scope(scope: &BotScope) -> Result<(), LinkError> {
    if scope.can.is_empty() || scope.can.iter().any(|a| !BOT_ACTIONS.contains(&a.as_str())) {
        return Err(malformed("a bot scope names read, write or join"));
    }
    if scope.expires > ad4m_hash_chain::MAX_SAFE_INTEGER {
        return Err(malformed("bot expires is not a safe integer"));
    }
    Ok(())
}

/// The shape of an offer: ids and keys well formed, `bot` set exactly for a bot offer.
pub fn check_offer_shape(offer: &Offer) -> Result<(), LinkError> {
    if b58_exact(&offer.offer_id, OFFER_ID_LEN).is_none() {
        return Err(malformed("offerId is 16 bytes of base58btc"));
    }
    if decode_hash(&offer.commit).is_err() {
        return Err(malformed("commit is a sha2-256 multihash"));
    }
    if offer.expires > ad4m_hash_chain::MAX_SAFE_INTEGER {
        return Err(malformed("expires is not a safe integer"));
    }
    if let Some(key) = &offer.device_key {
        if !is_ed25519_multikey(key) {
            return Err(malformed("deviceKey is an Ed25519 multikey"));
        }
    }
    match (offer.kind, &offer.bot) {
        (OfferKind::Bot, Some(scope)) => check_bot_scope(scope),
        (OfferKind::Bot, None) => Err(malformed("a bot offer carries its bot scope")),
        (_, Some(_)) => Err(malformed("only a bot offer carries a bot scope")),
        (_, None) => Ok(()),
    }
}

/// Signs `offer` at the position `by` with the current master key.
#[cfg(feature = "build")]
pub fn sign_offer(
    offer: Offer,
    by: SignedBy,
    master: &dyn crate::build::Signer,
) -> Result<SignedOffer, LinkError> {
    let bytes = offer_signing_bytes(&offer, &by)?;
    let sig = ad4m_hash_chain::encode_proof_value(&master.sign(&bytes));
    Ok(SignedOffer { offer, by, sig })
}

/// Checks a signed offer against the log: same identity, signed by the current master key at the
/// head, signature valid, `now < expires <= now + OFFER_TTL_S`, and a well-formed shape.
/// A position the log holds below the head gives [`LinkError::Moved`] (the store's `409`).
pub fn verify_offer(o: &SignedOffer, chain: &KelChain, now: u64) -> Result<(), LinkError> {
    check_offer_shape(&o.offer)?;
    if &o.offer.scid != chain.id() {
        return Err(LinkError::WrongScid);
    }
    let master = chain.state().current_master().ok_or(LinkError::Signer)?;
    if o.by.key_id != master.id {
        return Err(LinkError::Signer);
    }
    let head = chain.head();
    if o.by.kel_seq != head.seq || o.by.kel_hash != head.hash {
        let known = chain.hash_at(o.by.kel_seq) == Some(&o.by.kel_hash);
        return Err(if known && o.by.kel_seq >= master.added {
            LinkError::Moved
        } else {
            LinkError::Signer
        });
    }
    let sig = decode_proof_value(&o.sig).ok_or(LinkError::Signature)?;
    let value = json!({ "offer": o.offer, "by": o.by });
    if !verify_signed(OFFER_DOMAIN, &value, &master.sign, &sig) {
        return Err(LinkError::Signature);
    }
    if o.offer.expires <= now {
        return Err(LinkError::Expired);
    }
    if o.offer.expires > now.saturating_add(OFFER_TTL_S) {
        return Err(LinkError::TooLong);
    }
    Ok(())
}

/// The claim a device makes with its signing key `device` and its X25519 key `enc`.
#[cfg(feature = "build")]
pub fn make_claim(
    scid: &str,
    offer_id: &str,
    s: [u8; SECRET_LEN],
    device: &dyn crate::build::Signer,
    enc: Multikey,
    can_keep_master: bool,
    hint: Option<String>,
) -> Result<Claim, LinkError> {
    let mut claim = Claim {
        offer_id: offer_id.to_string(),
        s: bs58::encode(s).into_string(),
        device_key: device.key(),
        enc_key: enc,
        can_keep_master,
        hint,
        co_sig: String::new(),
    };
    let bytes = signing_bytes(CLAIM_DOMAIN, &claim_body(scid, &claim))?;
    claim.co_sig = ad4m_hash_chain::encode_proof_value(&device.sign(&bytes));
    Ok(claim)
}

/// Checks a claim's shape and its co-signature. It does not check `s` against an offer
/// ([`check_secret`]) nor a bound key.
pub fn verify_claim(scid: &str, c: &Claim) -> Result<(), LinkError> {
    if decode_secret(&c.s).is_none() {
        return Err(malformed("s is 32 bytes of base58btc"));
    }
    if b58_exact(&c.offer_id, OFFER_ID_LEN).is_none() {
        return Err(malformed("offerId is 16 bytes of base58btc"));
    }
    if !is_ed25519_multikey(&c.device_key) {
        return Err(malformed("deviceKey is an Ed25519 multikey"));
    }
    if !is_x25519_multikey(&c.enc_key) {
        return Err(malformed("encKey is an X25519 multikey"));
    }
    if c.hint
        .as_ref()
        .is_some_and(|h| h.chars().count() > HINT_MAX_CHARS)
    {
        return Err(malformed("hint is at most 64 characters"));
    }
    let sig = decode_proof_value(&c.co_sig).ok_or(LinkError::CoSig)?;
    if !verify_signed(CLAIM_DOMAIN, &claim_body(scid, c), &c.device_key, &sig) {
        return Err(LinkError::CoSig);
    }
    Ok(())
}

/// True when the claim's `s` hashes to the offer's commit.
pub fn check_secret(offer: &Offer, c: &Claim) -> bool {
    decode_secret(&c.s).is_some_and(|s| offer_commit(&s) == offer.commit)
}

/// The bytes the match code hashes: `JCS({scid, offerId, deviceKey, domain: "coasys:match"})`.
pub fn match_input(scid: &str, offer_id: &str, device_key: &str) -> Vec<u8> {
    let value = json!({
        "scid": scid,
        "offerId": offer_id,
        "deviceKey": device_key,
        "domain": MATCH_LABEL,
    });
    canonical_json(&value)
        .expect("an object of strings has a canonical form")
        .into_bytes()
}

/// The match code both devices show: the first 6 characters of base32 (RFC 4648, no padding) of
/// `sha256(match_input)`, as two groups of three (`"R7K Q2M"`, 30 bits).
pub fn match_code(scid: &str, offer_id: &str, device_key: &str) -> String {
    let digest = sha256(&match_input(scid, offer_id, device_key));
    let code = base32(&digest[..4]);
    format!("{} {}", &code[0..3], &code[3..6])
}

const BASE32: &[u8; 32] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";

/// RFC 4648 base32 without padding.
fn base32(bytes: &[u8]) -> String {
    let mut out = String::new();
    let mut buffer: u32 = 0;
    let mut bits = 0;
    for byte in bytes {
        buffer = (buffer << 8) | u32::from(*byte);
        bits += 8;
        while bits >= 5 {
            bits -= 5;
            out.push(BASE32[((buffer >> bits) & 31) as usize] as char);
        }
    }
    if bits > 0 {
        out.push(BASE32[((buffer << (5 - bits)) & 31) as usize] as char);
    }
    out
}

/// Rule A7's conditions 2–5 (AR-02 4.3.2) for `event`, an `add` appended on `before` that names
/// `offer`, whose stored claim is `claim`. The store checks condition 1 (a `Claimed` offer of the
/// same SCID) when it reads the offer, and condition 6 (spend the claim, close the offer) when it
/// commits.
pub fn check_add(
    offer: &SignedOffer,
    claim: &ClaimView,
    before: &KeyState,
    event: &Event,
    now: u64,
) -> Result<(), LinkError> {
    let Op::Add {
        add,
        offer: named,
        co_sig,
    } = &event.op
    else {
        return Err(LinkError::Claim("not an add"));
    };
    if event.scid != offer.offer.scid || named != &offer.offer.offer_id {
        return Err(LinkError::Claim("the add names another offer"));
    }
    let master = before
        .current_master()
        .ok_or(LinkError::Claim("the log has ended"))?;
    if offer.by.key_id != master.id {
        return Err(LinkError::Claim(
            "the offer's signer is no longer the master key",
        ));
    }
    if now >= offer.offer.expires.saturating_add(CONFIRM_GRACE_S) {
        return Err(LinkError::Claim("the offer's confirm time has passed"));
    }
    let [key] = add.as_slice() else {
        return Err(LinkError::Claim("an add lists one key"));
    };
    if key.sign != claim.device_key || key.enc != claim.enc_key || co_sig != &claim.co_sig {
        return Err(LinkError::Claim("the key is not the claimed key"));
    }
    match (offer.offer.kind, &key.life) {
        (OfferKind::Bot, KeyLife::Bot(scope)) if Some(scope) == offer.offer.bot.as_ref() => Ok(()),
        (OfferKind::Bot, _) => Err(LinkError::Claim(
            "a bot offer adds a bot key with the offer's scope",
        )),
        (_, KeyLife::Bot(_)) => Err(LinkError::Claim("only a bot offer adds a bot key")),
        _ => Ok(()),
    }
}

fn encode_string(prefix: &str, value: &Value) -> String {
    let text = canonical_json(value).expect("strings and safe integers have a canonical form");
    format!("{prefix}{}", bs58::encode(text.as_bytes()).into_string())
}

fn decode_string(prefix: &str, text: &str) -> Result<Value, LinkError> {
    let body = text
        .strip_prefix(prefix)
        .ok_or_else(|| malformed(format!("the string does not start with {prefix}")))?;
    let bytes = bs58::decode(body)
        .into_vec()
        .map_err(|_| malformed("the string is not base58btc"))?;
    let value: Value =
        serde_json::from_slice(&bytes).map_err(|_| malformed("the string holds no JSON"))?;
    let canonical = canonical_json(&value).map_err(|e| malformed(e.to_string()))?;
    if canonical.as_bytes() != bytes.as_slice() {
        return Err(malformed("the string's JSON is not canonical"));
    }
    Ok(value)
}

/// What an offer string carries: the store, the identity, the offer and its secret. It carries
/// no `kind` and no `expires`, so the link stays short; the claim's answer gives both.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct OfferString {
    /// The store's base URL.
    pub store: String,
    pub scid: String,
    pub offer_id: String,
    /// 32 bytes, base58btc.
    pub s: String,
}

impl OfferString {
    /// `coasys:offer:1:<base58btc(JCS({store, scid, offerId, s}))>`.
    pub fn encode(&self) -> String {
        encode_string(
            OFFER_STRING_PREFIX,
            &serde_json::to_value(self).expect("plain strings serialise"),
        )
    }

    /// The offer link: the offer string only in the fragment of `https://coasys.id/o`.
    pub fn link(&self) -> String {
        format!("{OFFER_LINK_PAGE}#{}", self.encode())
    }

    /// Parses an offer string. Refuses any other prefix, any extra field, a short code, a
    /// secret that is not 32 bytes and an offer id that is not 16 bytes.
    pub fn parse(text: &str) -> Result<OfferString, LinkError> {
        let value = decode_string(OFFER_STRING_PREFIX, text)?;
        let parsed: OfferString =
            serde_json::from_value(value).map_err(|e| malformed(e.to_string()))?;
        if decode_secret(&parsed.s).is_none() {
            return Err(malformed("s is 32 bytes of base58btc"));
        }
        if b58_exact(&parsed.offer_id, OFFER_ID_LEN).is_none() {
            return Err(malformed("offerId is 16 bytes of base58btc"));
        }
        if decode_hash(&parsed.scid).is_err() {
            return Err(malformed("scid is a sha2-256 multihash"));
        }
        if parsed.store.is_empty() {
            return Err(malformed("store is the store's base URL"));
        }
        Ok(parsed)
    }

    /// Parses what a person pastes: an offer string, or an offer link that holds the string in
    /// its fragment. A link with a query, or with the string anywhere but the fragment, is refused.
    pub fn parse_link(text: &str) -> Result<OfferString, LinkError> {
        let text = text.trim();
        if text.starts_with(OFFER_STRING_PREFIX) {
            return OfferString::parse(text);
        }
        let (page, fragment) = text
            .split_once('#')
            .ok_or_else(|| malformed("an offer link holds the offer string in its fragment"))?;
        if page.contains('?') {
            return Err(malformed("an offer link has no query"));
        }
        if page.contains(OFFER_STRING_PREFIX) {
            return Err(malformed("the offer string sits only in the fragment"));
        }
        OfferString::parse(fragment)
    }
}

/// What a device string carries: the device's public keys, a fresh `r`, its expiry, a hint and
/// the store. No secret.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DeviceString {
    pub sign: Multikey,
    pub enc: Multikey,
    /// 16 bytes, base58btc.
    pub r: String,
    /// Unix seconds; 5 minutes after the string shows.
    pub exp: u64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
    pub store: String,
}

impl DeviceString {
    /// `coasys:device:1:<base58btc(JCS({sign, enc, r, exp, hint, store}))>`.
    pub fn encode(&self) -> String {
        encode_string(
            DEVICE_STRING_PREFIX,
            &serde_json::to_value(self).expect("strings and an integer serialise"),
        )
    }

    /// Parses a device string. Refuses any other prefix, any extra field and malformed keys.
    pub fn parse(text: &str) -> Result<DeviceString, LinkError> {
        let value = decode_string(DEVICE_STRING_PREFIX, text.trim())?;
        let parsed: DeviceString =
            serde_json::from_value(value).map_err(|e| malformed(e.to_string()))?;
        if !is_ed25519_multikey(&parsed.sign) {
            return Err(malformed("sign is an Ed25519 multikey"));
        }
        if !is_x25519_multikey(&parsed.enc) {
            return Err(malformed("enc is an X25519 multikey"));
        }
        if b58_exact(&parsed.r, DEVICE_R_LEN).is_none() {
            return Err(malformed("r is 16 bytes of base58btc"));
        }
        if parsed
            .hint
            .as_ref()
            .is_some_and(|h| h.chars().count() > HINT_MAX_CHARS)
        {
            return Err(malformed("hint is at most 64 characters"));
        }
        Ok(parsed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base32_follows_rfc4648() {
        // RFC 4648 section 10 test vectors, without padding.
        assert_eq!(base32(b""), "");
        assert_eq!(base32(b"f"), "MY");
        assert_eq!(base32(b"fo"), "MZXQ");
        assert_eq!(base32(b"foo"), "MZXW6");
        assert_eq!(base32(b"foob"), "MZXW6YQ");
        assert_eq!(base32(b"fooba"), "MZXW6YTB");
        assert_eq!(base32(b"foobar"), "MZXW6YTBOI");
    }

    #[test]
    fn match_code_is_two_groups_of_three() {
        let code = match_code("QmScid", "offer", "z6MkKey");
        assert_eq!(code.len(), 7);
        assert_eq!(&code[3..4], " ");
        assert!(code
            .chars()
            .filter(|c| *c != ' ')
            .all(|c| BASE32.contains(&(c as u8))));
        assert_ne!(code, match_code("QmScid", "offer", "z6MkOther"));
    }
}
