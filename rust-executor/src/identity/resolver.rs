//! The one place the executor decides what a `did:scid:coasys:1` says: which key speaks for an
//! identity (for expressions and links) and the DID document of a DID URL (section 8 of
//! `docs-src/identity/did-scid-coasys-1.md`).
//!
//! - [`key_valid_at_anchor`] and [`verify_expression`]: a device key's signature for its
//!   person, by the cached log up to its accepted, store-signed head (refreshed once from the
//!   store when the cache does not admit the key). Events past that head, or from a foreign
//!   `src`, never make a key speak.
//! - [`Resolver::resolve`]: a DID URL (`did:scid:coasys:1:<scid>[?src=…&versionId=…][#dN]`) or a
//!   bare device `did:key` to a DID Core resolution result, through the [`KelCache`] and the
//!   [`IdentityBackend`]. The cache only extends: a log from any source is imported, never put
//!   in place of a held one; a fork is recorded and reported.
//!
//! A `src` is a request to an address someone else chose. The configured store is read through
//! the agent language; a foreign `src` is read by the executor itself ([`SrcFetch`]), HTTPS
//! only, connecting only to public addresses ([`is_public`]) and following no redirect, unless
//! `identity.allowPrivateSrc` is set (see `src_fetch.rs` and `AGENTS.md`).

use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::{Duration, Instant};

use ad4m_identity_core::{
    did_document, did_of, scid_of, BotScope, Event, ForkEvidence, ImportOutcome, KelChain,
};
use serde_json::Value;

use super::backend::{IdentityBackend, IdentityError};
use super::cache::{
    fold_log, scid_from, CacheError, Ingested, KelCache, KeyLookup, KeyMap, TrustedLog, MAX_PAGES,
};
use super::src_fetch::{refuse_private_host, SrcFetch};
use super::types::{
    convert, DidDocumentMetadata, DidResolutionMetadata, DidResolutionResult, ForkRecord,
    SkippedSource,
};
use super::verdict::{judge, LinkFacts, LinkVerdict, SignerLog};

/// The representation the resolver writes.
pub const DID_LD_JSON: &str = "application/did+ld+json";
/// How long a failed fetch from a foreign host keeps the resolver from asking it again for the
/// same identity.
pub const MISS_TTL: Duration = Duration::from_secs(300);
/// The most bytes one resolution reads from a foreign `src`, over all its pages. A person's key
/// log is an incept and a few adds, revokes and rotations, about a kilobyte each (an event the
/// store takes is at most 64 KiB), so 1 MiB holds hundreds of ordinary events. The page cap
/// alone (`MAX_PAGES` × `MAX_LOG_PAGE_BYTES`, about 1 GiB) would let any `agent:READ` session
/// make the executor hold a host's choice of bytes in memory for one answer.
const MAX_SRC_LOG_BYTES: usize = 1 << 20;
/// The most events a page holds (`identity-store`'s page size).
const PAGE_EVENTS: usize = 256;
/// How long (seconds) a log refreshed from the configured store may admit a key before the
/// next admission refreshes it again ([`verify_expression`]).
pub const ADMIT_FRESHNESS: u64 = 60;
/// Bounds of the in-memory maps (failed fetches, confirmed keys).
const MAP_BOUND: usize = 4096;
/// How long the resolver stops asking the key index after it failed to answer, so an outage
/// fails fast into `Unavailable`.
pub const BREAKER: Duration = Duration::from_secs(30);

// ---------------------------------------------------------------------------------------------
// Keys that speak for an identity
// ---------------------------------------------------------------------------------------------

/// The id (`d<n>`) of the device key whose signing multikey is `key`; `None` for a master key
/// or a key the log does not list. A master signs the log's events, never for the person.
/// The id of `key` among the log's device keys, never a master (`KeyState::device_key_id_of`).
pub(super) fn device_key_id(state: &ad4m_identity_core::KeyState, key: &str) -> Option<String> {
    state.device_key_id_of(&key.to_string()).cloned()
}

/// The cached log of `did` and its trusted seq ([`KelCache::trusted`]); `None` without one or
/// when the cache cannot read it.
async fn trusted_log(cache: &KelCache, did: &str) -> Option<(KelChain, u64)> {
    match cache.trusted(scid_from(did)).await {
        Ok(found) => found,
        Err(e) => {
            log::warn!("identity: the cached log of {did} is unreadable: {e}");
            None
        }
    }
}

/// Verifies `expr`, authored by a `did:scid`, whose `proof.key` must be the bare `did:key` of a
/// key the author's log in `cache` lists, and whose own signature must hold under that key.
///
/// `record` is the expression's `proof.record`, which the agent language adds to a `did:scid`
/// profile: `{version, anchor, sig}` of the agent record it came in. The key counts at the
/// record's anchor: the record signature must hold ([`record_signature_holds`]) and the key be
/// valid at the anchor ([`key_valid_at_anchor`]). A profile signed before a revocation stays
/// valid after it. A `record` that does not hold makes the expression invalid. The
/// expression's own timestamp does not count: the signer states it.
///
/// The log counts only up to its trusted seq: an `add` carries only the master's signature, so
/// a log a foreign `src` served may list anyone's key. The log is refreshed once from the
/// configured store through `backend` (which advances the accepted head) and checked again when
/// the cached log does not admit the key (no log or no store-signed head yet, a trusted seq
/// below the anchor, or a device added since), and when it does admit it but last refreshed
/// more than [`ADMIT_FRESHNESS`] seconds ago in this run (a restart makes every log stale).
/// Offline: a refresh that fails (store unreachable) leaves the cached trusted head to answer.
pub async fn verify_expression<T: serde::Serialize>(
    cache: &KelCache,
    backend: Option<&dyn IdentityBackend>,
    expr: &crate::types::Expression<T>,
    record: &Value,
) -> Result<bool, deno_core::error::AnyError> {
    let now = super::service::now();
    verify_expression_at(cache, backend, expr, record, now).await
}

/// [`verify_expression`] by the clock `now` (unix seconds): the freshness window and key
/// expiry are judged at `now`.
pub(crate) async fn verify_expression_at<T: serde::Serialize>(
    cache: &KelCache,
    backend: Option<&dyn IdentityBackend>,
    expr: &crate::types::Expression<T>,
    record: &Value,
    now: u64,
) -> Result<bool, deno_core::error::AnyError> {
    let Some(key) = expr
        .proof
        .key
        .strip_prefix("did:key:")
        .filter(|key| ad4m_hash_chain::decode_ed25519_multikey(key).is_some())
    else {
        return Ok(false);
    };
    let Some(record) = RecordProof::read(record) else {
        return Ok(false);
    };
    // The anchor counts only through the record signature: check it before the log.
    if !record_signature_holds(expr, key, &record)? {
        return Ok(false);
    }
    let admits = || key_valid_at_anchor(cache, &expr.author, key, record.anchor, now);
    let mut admitted = admits().await;
    if let Some(backend) = backend {
        let stale =
            admitted && !cache.refreshed_within(scid_from(&expr.author), now, ADMIT_FRESHNESS);
        if !admitted || stale {
            // A failed refresh changes nothing in the cache: the cached trusted head answers.
            // It counts as an attempt, so an unreachable store is tried once per window, not on
            // every read.
            if let Err(e) = cache.refresh(backend, &expr.author).await {
                log::info!("identity: refresh of {} failed ({e})", expr.author);
                cache.note_refreshed(scid_from(&expr.author), now);
            }
            admitted = admits().await;
        }
    }
    if !admitted {
        return Ok(false);
    }
    crate::agent::signatures::verify_by(expr, &expr.proof.key)
}

/// `proof.record` on a `did:scid` profile expression: the agent record's `version`, `anchor`
/// and `sig` (`ad4m_ldk::identity::AgentRecordProof`). A member it does not name makes it
/// malformed: nothing unsigned rides along.
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RecordProof {
    pub version: u64,
    pub anchor: u64,
    pub sig: String,
}

impl RecordProof {
    fn read(value: &Value) -> Option<RecordProof> {
        serde_json::from_value(value.clone()).ok()
    }
}

/// Whether `record.sig` is `key`'s `coasys:link` signature over the agent record
/// `{profile, version, anchor}`, `profile` being `expr` as the record stores it: `author`,
/// `timestamp`, `data` and `proof: {key, signature}`, without `proof.record` or the
/// `proof.valid`/`proof.invalid` flags (the typed expression carries none of them).
fn record_signature_holds<T: serde::Serialize>(
    expr: &crate::types::Expression<T>,
    key: &str,
    record: &RecordProof,
) -> Result<bool, deno_core::error::AnyError> {
    let agent_record = ad4m_ldk::identity::AgentRecord {
        did: expr.author.clone(),
        profile: serde_json::to_value(expr)?,
        version: record.version,
        signed_by: expr.proof.key.clone(),
        anchor: Some(record.anchor),
        sig: record.sig.clone(),
    };
    Ok(
        ad4m_hash_chain::decode_proof_value(&agent_record.sig).is_some_and(|sig| {
            ad4m_identity_core::verify_signed(
                ad4m_identity_core::LINK_DOMAIN,
                &agent_record.signed_value(),
                key,
                &sig,
            )
        }),
    )
}

/// Whether the signing multikey `key` spoke for `did` at seq `anchor` of the cached log: the
/// anchor is at or below the log's trusted seq ([`KelCache::trusted`]: the accepted,
/// store-signed head), and the log lists `key` as a device key, valid at the anchor by `now` and
/// without the bot mark there (the rule the store and the agent language apply to an agent
/// record). A key revoked at seq N signs nothing valid anchored at or after N.
pub async fn key_valid_at_anchor(
    cache: &KelCache,
    did: &str,
    key: &str,
    anchor: u64,
    now: u64,
) -> bool {
    let Some((chain, trusted)) = trusted_log(cache, did).await else {
        return false;
    };
    if anchor > trusted {
        return false;
    }
    let state = chain.state();
    let Some(id) = device_key_id(state, key) else {
        return false;
    };
    state.bot_scope_at(&id, anchor).is_none() && state.key_valid_at_time(&id, anchor, now)
}

// ---------------------------------------------------------------------------------------------
// Links: the verdict and the signer's person
// ---------------------------------------------------------------------------------------------

/// Who a signing key speaks for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Author {
    /// A device key of a person's log. `revoked`: the seq that revoked it, at or below the
    /// trusted seq.
    Person {
        did: String,
        key_id: String,
        revoked: Option<u64>,
    },
    /// A bot key: it sits in its owner's log with the bot mark (the scope at the trusted seq).
    Bot {
        owner: String,
        key_id: String,
        scope: BotScope,
    },
    /// No log lists the key (a device-only identity), or the key does not parse.
    NoLog,
    /// The lookup failed. Callers that count or filter treat it as no person.
    Unavailable,
}

/// The signing multikey of a `did:key` DID; `None` for another DID or a malformed key.
pub fn multikey_of(did: &str) -> Option<&str> {
    did.strip_prefix("did:key:")
        .filter(|key| ad4m_hash_chain::decode_ed25519_multikey(key).is_some())
}

/// [`Author`] of `key` in the trusted log of `scid`. A master key speaks for no person.
fn author_in(scid: &str, log: &TrustedLog, key: &str) -> Author {
    let state = &log.state;
    let Some(id) = device_key_id(state, key) else {
        return Author::NoLog;
    };
    let revoked = state
        .keys
        .get(&id)
        .and_then(|r| r.revoked)
        .filter(|r| *r <= log.trusted);
    match state.bot_scope_at(&id, log.trusted) {
        Some(scope) => Author::Bot {
            owner: did_of(scid),
            key_id: id,
            scope: scope.clone(),
        },
        None => Author::Person {
            did: did_of(scid),
            key_id: id,
            revoked,
        },
    }
}

/// The verdict of a signed expression and the person it counts for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CheckedLink {
    pub verdict: LinkVerdict,
    /// The `did:scid` of the signer's person (a bot's owner), only for a `Valid` link whose
    /// signer a log lists.
    pub person: Option<String>,
}

/// Checks `expr` by what `map` holds, with no I/O: the signature (the bytes its proof names),
/// then the signer's log ([`judge`]). `entered_after_revoke`: where the expression entered the
/// space's ordered history against its key's `revoke` (`None`: no order).
pub fn check_signed_with<T: serde::Serialize>(
    expr: &crate::types::Expression<T>,
    map: &KeyMap,
    entered_after_revoke: Option<bool>,
    now: u64,
) -> CheckedLink {
    let signature_holds = crate::agent::signatures::signature_holds(expr);
    let signer = multikey_of(&expr.author);
    let lookup = signer.map_or(KeyLookup::NoLog, |key| map.lookup(key));
    let log = match &lookup {
        KeyLookup::Listed { log, .. } => SignerLog::Listed {
            state: &log.state,
            trusted: log.trusted,
        },
        KeyLookup::NoLog => SignerLog::NoLog,
        KeyLookup::Unknown => SignerLog::Unknown,
    };
    let verdict = judge(&LinkFacts {
        signature_holds,
        signer: signer.unwrap_or_default(),
        anchor: expr.proof.anchor,
        log,
        entered_after_revoke,
        now,
    });
    let person = match (&verdict, &lookup, signer) {
        (LinkVerdict::Valid, KeyLookup::Listed { scid, log }, Some(key)) => {
            match author_in(scid, log, key) {
                Author::Person { did, .. } => Some(did),
                Author::Bot { owner, .. } => Some(owner),
                Author::NoLog | Author::Unavailable => None,
            }
        }
        _ => None,
    };
    CheckedLink { verdict, person }
}

// ---------------------------------------------------------------------------------------------
// DID URLs
// ---------------------------------------------------------------------------------------------

/// A DID Core resolution error.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum ResolveError {
    #[error("invalidDid: {0}")]
    InvalidDid(String),
    #[error("invalidDidUrl: {0}")]
    InvalidDidUrl(String),
    #[error("methodNotSupported: {0}")]
    MethodNotSupported(String),
    #[error("notFound: {0}")]
    NotFound(String),
    #[error("representationNotSupported: {0}")]
    RepresentationNotSupported(String),
    #[error("internalError: {0}")]
    Internal(String),
}

impl ResolveError {
    /// The DID Core error code.
    pub fn code(&self) -> &'static str {
        match self {
            ResolveError::InvalidDid(_) => "invalidDid",
            ResolveError::InvalidDidUrl(_) => "invalidDidUrl",
            ResolveError::MethodNotSupported(_) => "methodNotSupported",
            ResolveError::NotFound(_) => "notFound",
            ResolveError::RepresentationNotSupported(_) => "representationNotSupported",
            ResolveError::Internal(_) => "internalError",
        }
    }

    fn message(&self) -> &str {
        match self {
            ResolveError::InvalidDid(m)
            | ResolveError::InvalidDidUrl(m)
            | ResolveError::MethodNotSupported(m)
            | ResolveError::NotFound(m)
            | ResolveError::RepresentationNotSupported(m)
            | ResolveError::Internal(m) => m,
        }
    }
}

/// A parsed `did:scid:coasys:1` DID URL. Parameter values and the fragment are percent-decoded.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScidUrl {
    /// The DID: the part before `?` and `#`.
    pub did: String,
    pub scid: String,
    pub src: Option<String>,
    pub version_id: Option<u64>,
    /// The key id of the fragment (`d1`, `d1-enc`).
    pub fragment: Option<String>,
}

/// What a DID URL names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Target {
    Scid(ScidUrl),
    /// A bare device `did:key`: its signing multikey.
    Key(String),
}

/// Percent-decodes `s`; a `%` not followed by two hex digits, or bytes that are not UTF-8,
/// make the DID URL invalid.
fn percent_decode(s: &str, what: &str) -> Result<String, ResolveError> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            let ok = bytes.len() > i + 2
                && bytes[i + 1].is_ascii_hexdigit()
                && bytes[i + 2].is_ascii_hexdigit();
            if !ok {
                return Err(ResolveError::InvalidDidUrl(format!(
                    "{what}: a % without two hex digits"
                )));
            }
            i += 3;
        } else {
            i += 1;
        }
    }
    percent_encoding::percent_decode_str(s)
        .decode_utf8()
        .map(|c| c.into_owned())
        .map_err(|_| ResolveError::InvalidDidUrl(format!("{what}: not UTF-8 once decoded")))
}

/// `did:<method>:<id>` by DID Core's syntax (method: lower-case letters and digits).
fn is_some_did(s: &str) -> bool {
    let Some(rest) = s.strip_prefix("did:") else {
        return false;
    };
    match rest.split_once(':') {
        Some((method, id)) => {
            !method.is_empty()
                && method
                    .bytes()
                    .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit())
                && !id.is_empty()
        }
        None => false,
    }
}

/// Parses a DID URL (section 8): `did:scid:coasys:1:<scid>[?<params>][#<keyId>]`, or a bare
/// `did:key:<Ed25519 multikey>`. Reads `src` and `versionId`; `versionTime` is refused (the log
/// holds no time); other parameters are ignored. A parameter given twice is refused.
pub fn parse_did_url(input: &str) -> Result<Target, ResolveError> {
    if input.is_empty() {
        return Err(ResolveError::InvalidDid("empty".into()));
    }
    if input
        .chars()
        .any(|c| c.is_whitespace() || c.is_control() || !c.is_ascii())
    {
        return Err(ResolveError::InvalidDidUrl(
            "a DID URL is ASCII without spaces (percent-encode the rest)".into(),
        ));
    }
    let (rest, fragment) = match input.split_once('#') {
        Some((rest, fragment)) => (rest, Some(fragment)),
        None => (input, None),
    };
    let (did_part, query) = match rest.split_once('?') {
        Some((did, query)) => (did, Some(query)),
        None => (rest, None),
    };

    if let Some(key) = did_part.strip_prefix("did:key:") {
        if query.is_some() || fragment.is_some() || key.contains('/') {
            return Err(ResolveError::InvalidDidUrl(
                "a did:key resolves bare: no path, parameters or fragment".into(),
            ));
        }
        if ad4m_hash_chain::decode_ed25519_multikey(key).is_none() {
            return Err(ResolveError::InvalidDid(format!(
                "{did_part} is not an Ed25519 did:key"
            )));
        }
        return Ok(Target::Key(key.to_string()));
    }

    let (did, path) = match did_part.find('/') {
        Some(at) => (&did_part[..at], Some(&did_part[at..])),
        None => (did_part, None),
    };
    let Some(scid) = scid_of(did) else {
        if did.starts_with(ad4m_identity_core::DID_PREFIX) || !is_some_did(did) {
            return Err(ResolveError::InvalidDid(format!(
                "{did} is not a did:scid:coasys:1"
            )));
        }
        return Err(ResolveError::MethodNotSupported(format!(
            "this resolver reads did:scid:coasys:1 and device did:key, not {did}"
        )));
    };
    if path.is_some() {
        return Err(ResolveError::InvalidDidUrl(
            "a did:scid:coasys:1 URL has no path".into(),
        ));
    }

    let mut src = None;
    let mut version_id = None;
    for param in query.unwrap_or("").split('&').filter(|p| !p.is_empty()) {
        let (key, value) = param.split_once('=').ok_or_else(|| {
            ResolveError::InvalidDidUrl(format!("the parameter {param} has no value"))
        })?;
        let key = percent_decode(key, "a parameter name")?;
        match key.as_str() {
            "src" => {
                let value = percent_decode(value, "src")?;
                if value.is_empty() {
                    return Err(ResolveError::InvalidDidUrl("src is empty".into()));
                }
                if src.replace(value).is_some() {
                    return Err(ResolveError::InvalidDidUrl("src given twice".into()));
                }
            }
            "versionId" => {
                let value = percent_decode(value, "versionId")?;
                let seq = (!value.is_empty() && value.bytes().all(|b| b.is_ascii_digit()))
                    .then(|| value.parse::<u64>().ok())
                    .flatten()
                    .ok_or_else(|| {
                        ResolveError::InvalidDidUrl(format!(
                            "versionId {value} is not a decimal seq"
                        ))
                    })?;
                if version_id.replace(seq).is_some() {
                    return Err(ResolveError::InvalidDidUrl("versionId given twice".into()));
                }
            }
            "versionTime" => {
                return Err(ResolveError::InvalidDidUrl(
                    "versionTime is not supported: the log holds no time; use versionId".into(),
                ))
            }
            _ => {}
        }
    }

    let fragment = match fragment {
        None => None,
        Some(f) => {
            let f = percent_decode(f, "the fragment")?;
            let ok = !f.is_empty()
                && f.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_' || b == b'.');
            if !ok {
                return Err(ResolveError::InvalidDidUrl(format!(
                    "the fragment #{f} is not a key id"
                )));
            }
            Some(f)
        }
    };

    Ok(Target::Scid(ScidUrl {
        did: did.to_string(),
        scid: scid.to_string(),
        src,
        version_id,
        fragment,
    }))
}

/// An HTTPS base a `src` names: `https://<host>[:port][/path]`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HttpsBase {
    pub url: url::Url,
    /// `https://<host>[:port][/path]`, without a trailing `/`.
    pub base: String,
    /// `<host>[:port][/path]`: the `src` form, and the `coasys:source` of an answer.
    pub label: String,
}

impl HttpsBase {
    fn from_url(url: url::Url) -> Result<Self, String> {
        if !url.username().is_empty() || url.password().is_some() {
            return Err("a src carries no user info".into());
        }
        if url.query().is_some() || url.fragment().is_some() {
            return Err("a src carries no query or fragment".into());
        }
        let host = url.host_str().ok_or("a src names a host")?.to_string();
        let port = url.port().map(|p| format!(":{p}")).unwrap_or_default();
        let path = url.path().trim_end_matches('/').to_string();
        let label = format!("{host}{port}{path}");
        Ok(HttpsBase {
            base: format!("https://{label}"),
            url,
            label,
        })
    }

    /// The same endpoint as `other`, by host, port (the scheme's default when absent) and path.
    fn same_endpoint(&self, other: &url::Url) -> bool {
        self.url.host_str() == other.host_str()
            && self.url.port_or_known_default() == other.port_or_known_default()
            && self.url.path().trim_end_matches('/') == other.path().trim_end_matches('/')
    }
}

/// What a `src` names (section 8, step 1).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SrcKind {
    Https(HttpsBase),
    /// A scheme this resolver has no adapter for: the source is skipped.
    Unsupported(String),
}

/// Reads a `src` (already percent-decoded). It has a scheme when it starts with an RFC 3986
/// scheme and a `:` not followed by a port (digits, then `/` or the end); without one it is an
/// HTTPS base with `https://` left out.
pub fn parse_src(src: &str) -> Result<SrcKind, String> {
    let scheme_end = src.find(':').filter(|&at| {
        let scheme = &src[..at];
        let mut chars = scheme.chars();
        let valid = chars.next().is_some_and(|c| c.is_ascii_alphabetic())
            && chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, '+' | '-' | '.'));
        let after = &src[at + 1..];
        let digits = after.bytes().take_while(u8::is_ascii_digit).count();
        let is_port = digits > 0 && matches!(after.as_bytes().get(digits), None | Some(b'/'));
        valid && !is_port
    });
    let rest = match scheme_end {
        Some(at) if src[..at].eq_ignore_ascii_case("https") => {
            match src[at + 1..].strip_prefix("//") {
                Some(rest) => rest,
                None => return Err("https: needs //".into()),
            }
        }
        Some(at) => return Ok(SrcKind::Unsupported(src[..at].to_ascii_lowercase())),
        None => src,
    };
    if rest.contains('\\') {
        return Err("a src has no backslash".into());
    }
    let url = url::Url::parse(&format!("https://{rest}")).map_err(|e| format!("{e}"))?;
    HttpsBase::from_url(url).map(SrcKind::Https)
}

/// Whether `ip` is a public unicast address: not loopback, private (RFC 1918, fc00::/7),
/// link-local, CGNAT (100.64/10), benchmarking (198.18/15), multicast, unspecified, "this
/// network" (0/8), reserved (240/4, broadcast) or IETF protocol (192.0.0/24). An IPv6 address
/// that carries an IPv4 one (mapped `::ffff:a.b.c.d`, compatible `::a.b.c.d`, NAT64
/// `64:ff9b::/96` and the local-use `64:ff9b:1::/48`, 6to4 `2002::/16`) is judged by that IPv4
/// address (for NAT64, its last 32 bits).
pub fn is_public(ip: IpAddr) -> bool {
    fn v4_public(ip: std::net::Ipv4Addr) -> bool {
        let o = ip.octets();
        !(o[0] == 0
            || o[0] == 10
            || o[0] == 127
            || (o[0] == 100 && (o[1] & 0xc0) == 64)
            || (o[0] == 169 && o[1] == 254)
            || (o[0] == 172 && (o[1] & 0xf0) == 16)
            || (o[0] == 192 && o[1] == 168)
            || (o[0] == 198 && (o[1] & 0xfe) == 18)
            || (o[0] == 192 && o[1] == 0 && o[2] == 0)
            || o[0] >= 224)
    }
    match ip {
        IpAddr::V4(v4) => v4_public(v4),
        IpAddr::V6(v6) => {
            if v6.is_loopback() || v6.is_unspecified() {
                return false;
            }
            let s = v6.segments();
            let o = v6.octets();
            let embedded = |a: usize| std::net::Ipv4Addr::new(o[a], o[a + 1], o[a + 2], o[a + 3]);
            if s[..5] == [0, 0, 0, 0, 0] && (s[5] == 0xffff || s[5] == 0) {
                return v4_public(embedded(12));
            }
            // NAT64: the well-known prefix and the local-use one (64:ff9b:1::/48, RFC 8215).
            if s[..6] == [0x64, 0xff9b, 0, 0, 0, 0] || s[..3] == [0x64, 0xff9b, 1] {
                return v4_public(embedded(12));
            }
            if s[0] == 0x2002 {
                return v4_public(embedded(2));
            }
            !((s[0] & 0xfe00) == 0xfc00
                || (s[0] & 0xffc0) == 0xfe80
                || (s[0] & 0xffc0) == 0xfec0
                || (s[0] & 0xff00) == 0xff00)
        }
    }
}

// ---------------------------------------------------------------------------------------------
// The resolver
// ---------------------------------------------------------------------------------------------

/// Where the resolver reads a log from.
enum Source {
    /// A `src` of the DID URL, checked.
    Foreign(HttpsBase),
    /// The configured store (`identity.storeUrl`, or the agent language's default), through
    /// `KelCache::refresh` so that its signed head counts.
    Store,
    /// A source that was not used, with why.
    Skip(SkippedSource),
}

/// What [`Resolver::load`] found besides the log.
#[derive(Default)]
struct Loaded {
    /// The host that answered; `None`: the cache.
    source: Option<String>,
    skipped: Vec<SkippedSource>,
    /// A foreign `src` whose log conflicts with the one the cache holds: reported in the
    /// answer like a fork, never recorded (the foreign log was never cached).
    conflict: Option<ForkEvidence>,
}

/// Why a foreign `src` gave no log for the answer.
enum ForeignMiss {
    /// Its log and the cache's differ at a seq both hold.
    Conflict(ForkEvidence),
    /// The fetch failed, or what it served does not fold: why.
    Failed(String),
}

/// DID resolution through the key-log cache and the agent language.
pub struct Resolver {
    cache: Arc<KelCache>,
    backend: Arc<dyn IdentityBackend>,
    /// `identity.storeUrl`, parsed.
    store: Option<url::Url>,
    /// `identity.allowPrivateSrc`: skip the public-address check (tests, local development).
    allow_private_src: bool,
    /// The reader of foreign `src` hosts, under the same `allowPrivateSrc`.
    src_fetch: SrcFetch,
    /// Failed fetches from foreign hosts: `(host label, scid)` → when.
    misses: StdMutex<HashMap<(String, String), Instant>>,
    /// Device keys confirmed in a log: signing multikey → SCID.
    keys: StdMutex<HashMap<String, String>>,
    /// Until when the key index is not asked, after it failed to answer ([`BREAKER`]).
    index_down_until: StdMutex<Option<Instant>>,
}

impl Resolver {
    pub fn new(
        cache: Arc<KelCache>,
        backend: Arc<dyn IdentityBackend>,
        store_url: Option<&str>,
        allow_private_src: bool,
    ) -> Self {
        let store = store_url.and_then(|u| match url::Url::parse(u) {
            Ok(url) => Some(url),
            Err(e) => {
                log::warn!("identity: identity.storeUrl {u} does not parse ({e})");
                None
            }
        });
        Resolver {
            cache,
            backend,
            store,
            allow_private_src,
            src_fetch: SrcFetch::new(allow_private_src),
            misses: StdMutex::new(HashMap::new()),
            keys: StdMutex::new(HashMap::new()),
            index_down_until: StdMutex::new(None),
        }
    }

    /// Who the `did:key` `did_key` speaks for: the key map first (every trusted log the cache
    /// holds), then the key index, confirmed against the log at its store-signed head. A found
    /// person stays for good; "no log" holds for [`super::cache::NO_LOG_TTL`]; `Unavailable` is
    /// never kept.
    pub async fn author_of(&self, did_key: &str) -> Author {
        let Some(key) = multikey_of(did_key) else {
            return Author::NoLog;
        };
        match self.lookup_signer(key).await {
            KeyLookup::Listed { scid, log } => author_in(&scid, &log, key),
            KeyLookup::NoLog => Author::NoLog,
            KeyLookup::Unknown => Author::Unavailable,
        }
    }

    /// What the logs say about the signing multikey `key`, filling the key map: the map, a key
    /// [`Resolver::resolve`] confirmed, then the key index (a hint) and the log it names, read
    /// from the store once when the cached log does not list the key up to its trusted seq.
    /// `NoLog` when the index does not know the key or the named log does not list it (a
    /// warning: the index and the log disagree); `Unknown` when the store does not answer or the
    /// log lists the key only past its trusted seq.
    pub async fn lookup_signer(&self, key: &str) -> KeyLookup {
        let map = self.cache.key_map();
        match map.lookup(key) {
            KeyLookup::Unknown => {}
            known => return known,
        }
        let confirmed = self.keys.lock().ok().and_then(|k| k.get(key).cloned());
        if let Some(scid) = confirmed {
            if let found @ KeyLookup::Listed { .. } = self.trusted_listing(&scid, key).await {
                return found;
            }
        }
        if !self.cache.trusts_a_store() {
            // No head can be accepted, so no log speaks for anyone.
            map.note_no_log(key);
            return KeyLookup::NoLog;
        }
        if self.index_down() {
            return KeyLookup::Unknown;
        }
        let scid = match self.backend.key_owner(key, 0).await {
            Ok(Some(entry)) => match scid_of(&entry.did) {
                Some(scid) => scid.to_string(),
                None => {
                    log::warn!(
                        "identity: the key index names {} for did:key:{key}, not a did:scid:coasys:1",
                        entry.did
                    );
                    map.note_no_log(key);
                    return KeyLookup::NoLog;
                }
            },
            Ok(None) | Err(IdentityError::NotFound) => {
                map.note_no_log(key);
                return KeyLookup::NoLog;
            }
            Err(e) => {
                log::info!("identity: the key index did not answer for did:key:{key} ({e})");
                self.index_failed();
                return KeyLookup::Unknown;
            }
        };
        if let found @ KeyLookup::Listed { .. } = self.trusted_listing(&scid, key).await {
            return found;
        }
        // The cached log may predate the key: read it from the store once.
        match self
            .cache
            .refresh(self.backend.as_ref(), &did_of(&scid))
            .await
        {
            Ok(_) => {}
            Err(CacheError::Unavailable(e)) => {
                log::info!("identity: refresh of {} failed ({e})", did_of(&scid));
                self.index_failed();
                return KeyLookup::Unknown;
            }
            Err(e) => log::info!("identity: refresh of {} failed ({e})", did_of(&scid)),
        }
        if let found @ KeyLookup::Listed { .. } = self.trusted_listing(&scid, key).await {
            return found;
        }
        let listed_at_all = match self.cache.chain(&scid).await {
            Ok(Some(chain)) => chain.state().record(key).is_some(),
            _ => false,
        };
        if listed_at_all {
            // Listed past its store-signed head: it may count once the head reaches it.
            return KeyLookup::Unknown;
        }
        log::warn!(
            "identity: the key index names {} for did:key:{key}, but its log does not list the key",
            did_of(&scid)
        );
        map.note_no_log(key);
        KeyLookup::NoLog
    }

    /// The key map's answer for `key` once the trusted log of `scid` is in it (reading it puts
    /// it there); `Unknown` unless it lists the key.
    async fn trusted_listing(&self, scid: &str, key: &str) -> KeyLookup {
        if let Err(e) = self.cache.trusted(scid).await {
            log::warn!("identity: the cached log of {scid} is unreadable: {e}");
        }
        match self.cache.key_map().lookup(key) {
            found @ KeyLookup::Listed { .. } => found,
            _ => KeyLookup::Unknown,
        }
    }

    /// Fills the key map for the signers of links about to be checked: each `(did:key,
    /// anchor)` once. A link anchored past the trusted seq of its signer's log refreshes that
    /// log from the store, at most once per [`ADMIT_FRESHNESS`].
    pub async fn prepare(&self, signers: &[(String, Option<u64>)]) {
        let now = super::service::now();
        let mut seen = std::collections::HashSet::new();
        for (did, anchor) in signers {
            if !seen.insert((did.as_str(), *anchor)) {
                continue;
            }
            let Some(key) = multikey_of(did) else {
                continue;
            };
            let KeyLookup::Listed { scid, log } = self.lookup_signer(key).await else {
                continue;
            };
            if anchor.is_some_and(|a| a > log.trusted)
                && !self.cache.refreshed_within(&scid, now, ADMIT_FRESHNESS)
            {
                if let Err(e) = self
                    .cache
                    .refresh(self.backend.as_ref(), &did_of(&scid))
                    .await
                {
                    log::info!("identity: refresh of {} failed ({e})", did_of(&scid));
                    self.cache.note_refreshed(&scid, now);
                }
                if let Err(e) = self.cache.trusted(&scid).await {
                    log::warn!("identity: the cached log of {scid} is unreadable: {e}");
                }
            }
        }
    }

    fn index_down(&self) -> bool {
        self.index_down_until
            .lock()
            .ok()
            .and_then(|until| *until)
            .is_some_and(|until| Instant::now() < until)
    }

    fn index_failed(&self) {
        if let Ok(mut until) = self.index_down_until.lock() {
            *until = Some(Instant::now() + BREAKER);
        }
    }

    /// This resolver with `src_fetch` as its reader of foreign hosts (tests: plain HTTP to local
    /// hosts).
    #[cfg(test)]
    pub(crate) fn with_src_fetch(mut self, src_fetch: SrcFetch) -> Self {
        self.src_fetch = src_fetch;
        self
    }

    /// Forgets what this resolver remembers of `scid`: its confirmed device keys and its failed
    /// fetches (`identity.resetLog`).
    pub fn forget(&self, scid: &str) {
        if let Ok(mut keys) = self.keys.lock() {
            keys.retain(|_, held| held != scid);
        }
        if let Ok(mut misses) = self.misses.lock() {
            misses.retain(|(_, held), _| held != scid);
        }
    }

    /// The configured store as a `src` (`host[:port][/path]`).
    fn store_src(&self) -> Option<String> {
        let url = self.store.as_ref()?;
        let host = url.host_str()?;
        let port = url.port().map(|p| format!(":{p}")).unwrap_or_default();
        Some(format!("{host}{port}{}", url.path().trim_end_matches('/')))
    }

    fn store_label(&self) -> String {
        self.store_src().unwrap_or_else(|| "store".into())
    }

    /// Resolves `did_url` (a `did:scid:coasys:1` DID URL or a bare device `did:key`). The cache
    /// answers unless `fresh`, or it lacks the log or the asked `versionId`.
    pub async fn resolve(&self, did_url: &str, fresh: bool) -> DidResolutionResult {
        self.resolve_as(did_url, fresh, None).await
    }

    /// [`Resolver::resolve`] for the representation `accept` (`None`: the default).
    pub async fn resolve_as(
        &self,
        did_url: &str,
        fresh: bool,
        accept: Option<&str>,
    ) -> DidResolutionResult {
        if let Some(accept) = accept.filter(|a| !a.is_empty()) {
            let supported = accept
                .split(',')
                .map(|t| t.split(';').next().unwrap_or("").trim())
                .any(|t| t == DID_LD_JSON || t == "*/*" || t == "application/*");
            if !supported {
                return failure(
                    ResolveError::RepresentationNotSupported(format!(
                        "{accept}: this resolver writes {DID_LD_JSON}"
                    )),
                    DidResolutionMetadata::default(),
                );
            }
        }
        match parse_did_url(did_url) {
            Err(e) => failure(e, DidResolutionMetadata::default()),
            Ok(Target::Scid(url)) => self.resolve_scid(&url, fresh).await,
            Ok(Target::Key(key)) => self.resolve_key(&key, fresh).await,
        }
    }

    async fn resolve_scid(&self, url: &ScidUrl, fresh: bool) -> DidResolutionResult {
        let mut meta = DidResolutionMetadata::default();
        match self
            .load(&url.scid, url.src.as_deref(), url.version_id, fresh)
            .await
        {
            Ok((chain, loaded)) => {
                meta.source = Some(loaded.source.unwrap_or_else(|| "cache".into()));
                meta.skipped = loaded.skipped;
                meta.fork = loaded.conflict.as_ref().map(fork_record);
                self.answer(&chain, url.version_id, url.fragment.as_deref(), meta)
                    .await
            }
            Err((e, loaded)) => {
                meta.skipped = loaded.skipped;
                failure(e, meta)
            }
        }
    }

    /// A device `did:key`: the key index names an identity (a hint), the identity's log must
    /// list the key at or below its trusted seq (the accepted, store-signed head), and the
    /// answer is the person's document at the head, `coasys:keyValid` at the trusted seq.
    async fn resolve_key(&self, key: &str, fresh: bool) -> DidResolutionResult {
        let mut meta = DidResolutionMetadata::default();
        let known = match self.cache.key_map().lookup(key) {
            KeyLookup::Listed { scid, .. } => Some(scid),
            _ => self.keys.lock().ok().and_then(|k| k.get(key).cloned()),
        };
        let scid = match known {
            Some(scid) => scid,
            None => match self.backend.key_owner(key, 0).await {
                Ok(Some(entry)) => match scid_of(&entry.did) {
                    Some(scid) => scid.to_string(),
                    None => {
                        log::warn!(
                            "identity: the key index names {} for did:key:{key}, not a did:scid:coasys:1",
                            entry.did
                        );
                        meta.key_index_mismatch = Some(true);
                        return failure(
                            ResolveError::NotFound(format!(
                                "the key index names {}, not a did:scid:coasys:1",
                                entry.did
                            )),
                            meta,
                        );
                    }
                },
                Ok(None) | Err(IdentityError::NotFound) => {
                    return failure(
                        ResolveError::NotFound("the key index does not know this key".into()),
                        meta,
                    )
                }
                Err(e) => {
                    return failure(
                        ResolveError::NotFound(format!("the key index could not answer: {e}")),
                        meta,
                    )
                }
            },
        };

        // The key's id and the seq that added it.
        let device_key = |chain: &KelChain| {
            let state = chain.state();
            device_key_id(state, key).and_then(|id| state.keys.get(&id).map(|r| (id, r.added)))
        };
        // Listed up to the log's trusted seq: the store admitted the key for this person.
        let confirmed = |chain: &KelChain, trusted: Option<u64>| {
            device_key(chain)
                .zip(trusted)
                .is_some_and(|((_, added), t)| added <= t)
        };
        let (mut chain, mut loaded) = match self.load(&scid, None, None, fresh).await {
            Ok(found) => found,
            Err((e, loaded)) => {
                meta.skipped = loaded.skipped;
                return failure(e, meta);
            }
        };
        let mut trusted = self.trusted_seq(&scid).await;
        if !confirmed(&chain, trusted) && loaded.source.is_none() && !fresh {
            // The cached log may predate the key, or hold it only past its store-signed head:
            // read it from the store once.
            if let Ok(found) = self.load(&scid, None, None, true).await {
                (chain, loaded) = found;
                trusted = self.trusted_seq(&scid).await;
            }
        }
        meta.source = Some(loaded.source.unwrap_or_else(|| "cache".into()));
        meta.skipped = loaded.skipped;
        let Some((key_id, added)) = device_key(&chain) else {
            log::warn!(
                "identity: the key index names {} for did:key:{key}, but its log does not list the key",
                did_of(&scid)
            );
            meta.key_index_mismatch = Some(true);
            return failure(
                ResolveError::NotFound(format!(
                    "the log of {} does not list this key",
                    did_of(&scid)
                )),
                meta,
            );
        };
        // A log lists any key its master adds (spec section 11): only a listing up to the
        // store-signed head maps the key to this person.
        let Some(trusted) = trusted.filter(|t| added <= *t) else {
            log::info!(
                "identity: the log of {} lists did:key:{key} only past its store-signed head",
                did_of(&scid)
            );
            return failure(
                ResolveError::NotFound(format!(
                    "no store-signed head of the log of {} lists this key",
                    did_of(&scid)
                )),
                meta,
            );
        };
        if let Ok(mut keys) = self.keys.lock() {
            if keys.len() >= MAP_BOUND {
                keys.clear();
            }
            keys.insert(key.to_string(), scid.clone());
        }
        meta.key_valid = Some(chain.state().key_valid_at(&key_id, trusted));
        meta.key_id = Some(key_id);
        self.answer(&chain, None, None, meta).await
    }

    /// The trusted seq of `scid`'s cached log ([`KelCache::trusted`]), `None` without one.
    async fn trusted_seq(&self, scid: &str) -> Option<u64> {
        match self.cache.trusted(scid).await {
            Ok(found) => found.map(|(_, seq)| seq),
            Err(e) => {
                log::warn!("identity: the cached log of {scid} is unreadable: {e}");
                None
            }
        }
    }

    /// The sources for `src`, in order: the `src`, then the configured store. A `src` that
    /// names the configured store is the configured store.
    fn sources(&self, src: Option<&str>) -> Vec<Source> {
        let mut sources = Vec::new();
        if let Some(src) = src {
            match parse_src(src) {
                Ok(SrcKind::Https(base))
                    if self.store.as_ref().is_some_and(|s| base.same_endpoint(s)) => {}
                Ok(SrcKind::Https(base)) => sources.push(Source::Foreign(base)),
                Ok(SrcKind::Unsupported(scheme)) => sources.push(Source::Skip(SkippedSource {
                    src: src.to_string(),
                    reason: format!("no adapter for {scheme}:"),
                })),
                Err(why) => sources.push(Source::Skip(SkippedSource {
                    src: src.to_string(),
                    reason: format!("not an HTTPS base: {why}"),
                })),
            }
        }
        sources.push(Source::Store);
        sources
    }

    /// The log of `scid`: from the cache when it holds it (and `version`) and not `fresh`, else
    /// from the sources in order. A foreign `src`'s log is folded in memory on what the cache
    /// held and serves this answer only: none of it is cached. The configured store is read
    /// after it in any case (into the cache, which only extends), so a `src` serving a prefix
    /// cannot hide an event the store holds: the answer is the `src`'s log extended by the
    /// cache's, or, when the two differ at a seq both hold, the cache's with the conflict.
    async fn load(
        &self,
        scid: &str,
        src: Option<&str>,
        version: Option<u64>,
        fresh: bool,
    ) -> Result<(KelChain, Loaded), (ResolveError, Loaded)> {
        let mut loaded = Loaded::default();
        let internal = |e: CacheError| ResolveError::Internal(format!("the key-log cache: {e}"));
        let cached = match self.cache.chain(scid).await {
            Ok(cached) => cached,
            Err(e) => return Err((internal(e), loaded)),
        };
        let covers = |chain: &KelChain| version.is_none_or(|v| v <= chain.head().seq);
        if let Some(chain) = cached.as_ref().filter(|c| !fresh && covers(c)) {
            return Ok((chain.clone(), loaded));
        }

        let did = did_of(scid);
        // The foreign host's label and the events it served past what the cache held.
        let mut foreign: Option<(String, KelChain, Vec<Event>)> = None;
        for source in self.sources(src) {
            match source {
                Source::Skip(skipped) => loaded.skipped.push(skipped),
                Source::Foreign(base) => {
                    if self.recent_miss(&base.label, scid) {
                        loaded.skipped.push(SkippedSource {
                            src: base.label,
                            reason: format!(
                                "a fetch failed within the last {} s",
                                MISS_TTL.as_secs()
                            ),
                        });
                        continue;
                    }
                    // The transport refuses private addresses itself; an IP literal or a
                    // loopback name is refused here, without a connection.
                    if !self.allow_private_src {
                        if let Err(why) = refuse_private_host(&base.url) {
                            log::info!("identity: refused src {}: {why}", base.label);
                            loaded.skipped.push(SkippedSource {
                                src: base.label,
                                reason: format!("refused: {why}"),
                            });
                            continue;
                        }
                    }
                    match self.fetch_foreign(scid, &base, cached.clone()).await {
                        Ok((chain, events)) => foreign = Some((base.label, chain, events)),
                        Err(ForeignMiss::Conflict(fork)) => loaded.conflict = Some(fork),
                        Err(ForeignMiss::Failed(why)) => {
                            self.note_miss(&base.label, scid);
                            loaded.skipped.push(SkippedSource {
                                src: base.label,
                                reason: why,
                            });
                        }
                    }
                }
                Source::Store => {
                    let fetched = match self.cache.refresh(self.backend.as_ref(), &did).await {
                        Ok(_) => Ok(()),
                        // A head no key of identity.storeKeys signed: the events still count,
                        // the head does not.
                        Err(CacheError::BadHeadSignature) => self.fetch_store_unsigned(scid).await,
                        Err(e) => Err(e),
                    };
                    match fetched {
                        Ok(()) => loaded.source = Some(self.store_label()),
                        // The cache keeps the log it held first; the fork is in the metadata.
                        Err(CacheError::Fork(_)) => {}
                        Err(e) => loaded.skipped.push(SkippedSource {
                            src: self.store_label(),
                            reason: e.to_string(),
                        }),
                    }
                }
            }
        }

        let held = match self.cache.chain(scid).await {
            Ok(held) => held,
            Err(e) => return Err((internal(e), loaded)),
        };
        match (foreign, held) {
            (Some((label, chain, _)), None) => {
                loaded.source = Some(label);
                Ok((chain, loaded))
            }
            // The src's events onto the cache's log: what the store holds past the src's
            // head stays (a revocation the src left out), and what the src holds past the
            // store's shows, untrusted.
            (Some((label, _, events)), Some(held)) => {
                let mut merged = held.clone();
                match ad4m_identity_core::import(&mut merged, &events) {
                    Ok(ImportOutcome::Appended { .. }) | Ok(ImportOutcome::AlreadyHave) => {
                        loaded.source = Some(label);
                        Ok((merged, loaded))
                    }
                    Ok(ImportOutcome::Fork(fork)) => {
                        loaded.conflict = Some(fork);
                        Ok((held, loaded))
                    }
                    Ok(ImportOutcome::Gap { have, first }) => {
                        loaded.skipped.push(SkippedSource {
                            src: label,
                            reason: format!(
                                "its events start at seq {first}, the log ends at {have}"
                            ),
                        });
                        Ok((held, loaded))
                    }
                    Err(e) => {
                        loaded.skipped.push(SkippedSource {
                            src: label,
                            reason: format!(
                                "its events do not continue the log: {}",
                                ad4m_identity_core::error_code(&e)
                            ),
                        });
                        Ok((held, loaded))
                    }
                }
            }
            (None, Some(held)) => Ok((held, loaded)),
            (None, None) => Err((
                ResolveError::NotFound(format!("no source holds a log of {did}")),
                loaded,
            )),
        }
    }

    /// Reads `scid`'s log from the foreign host `base` over the did:scid host profile
    /// ([`SrcFetch`]) and folds it in memory on `held` (the cache's log, if any), never into the
    /// cache. The host's head is a hint of how far to page, never trusted. At most `MAX_PAGES`
    /// pages and [`MAX_SRC_LOG_BYTES`] in all. The log, and the events the host served (from
    /// `held`'s head on).
    async fn fetch_foreign(
        &self,
        scid: &str,
        base: &HttpsBase,
        held: Option<KelChain>,
    ) -> Result<(KelChain, Vec<Event>), ForeignMiss> {
        let failed = |e: CacheError| ForeignMiss::Failed(e.to_string());
        let mut chain = held;
        let mut served: Vec<Event> = Vec::new();
        let mut budget = MAX_SRC_LOG_BYTES;
        for _ in 0..MAX_PAGES {
            let from = chain.as_ref().map_or(0, |c| c.head().seq);
            let (page, bytes) = match self.src_fetch.page(base, scid, from, budget).await {
                Ok(Some(found)) => found,
                Ok(None) => return Err(failed(CacheError::NotFound)),
                Err(why) => return Err(ForeignMiss::Failed(why)),
            };
            budget = budget.saturating_sub(bytes);
            let events = page
                .events
                .iter()
                .map(Event::from_json)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| failed(CacheError::Fold(ad4m_identity_core::ChainError::Policy(e))))?;
            let count = events.len();
            let head = match chain.as_mut() {
                _ if events.is_empty() => break,
                None => {
                    let (started, _) = fold_log(scid, &events).map_err(failed)?;
                    let head = started.head();
                    chain = Some(started);
                    head
                }
                Some(chain) => match ad4m_identity_core::import(chain, &events)
                    .map_err(|e| failed(CacheError::Fold(e)))?
                {
                    ImportOutcome::AlreadyHave => break,
                    ImportOutcome::Appended { head, .. } => head,
                    ImportOutcome::Gap { have, first } => {
                        return Err(failed(CacheError::Gap {
                            have: Some(have),
                            first,
                        }))
                    }
                    ImportOutcome::Fork(fork) => return Err(ForeignMiss::Conflict(fork)),
                },
            };
            served.extend(events);
            if head.seq >= page.head.seq || count < PAGE_EVENTS {
                break;
            }
        }
        match chain {
            Some(chain) => Ok((chain, served)),
            None => Err(failed(CacheError::NotFound)),
        }
    }

    /// Reads `scid`'s log from the configured store page by page from the cached head and
    /// imports the events with no head: for a store whose head no key of `identity.storeKeys`
    /// signed (the events count, the head does not).
    async fn fetch_store_unsigned(&self, scid: &str) -> Result<(), CacheError> {
        let did = did_of(scid);
        for _ in 0..MAX_PAGES {
            let cached = self.cache.chain(scid).await?;
            let from = cached.as_ref().map_or(0, |c| c.head().seq);
            let Some(page) = self.backend.log(&did, from).await? else {
                return Err(CacheError::NotFound);
            };
            let events = page
                .events
                .iter()
                .map(Event::from_json)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| CacheError::Fold(ad4m_identity_core::ChainError::Policy(e)))?;
            if events.is_empty() {
                return match cached {
                    Some(_) => Ok(()),
                    None => Err(CacheError::NotFound),
                };
            }
            match self.cache.ingest(scid, &events).await? {
                Ingested::AlreadyHave => return Ok(()),
                Ingested::Appended(head) => {
                    if head.seq >= page.head.seq || events.len() < PAGE_EVENTS {
                        return Ok(());
                    }
                }
            }
        }
        Ok(())
    }

    fn recent_miss(&self, host: &str, scid: &str) -> bool {
        let Ok(misses) = self.misses.lock() else {
            return false;
        };
        misses
            .get(&(host.to_string(), scid.to_string()))
            .is_some_and(|at| at.elapsed() < MISS_TTL)
    }

    fn note_miss(&self, host: &str, scid: &str) {
        if let Ok(mut misses) = self.misses.lock() {
            misses.retain(|_, at| at.elapsed() < MISS_TTL);
            if misses.len() >= MAP_BOUND {
                return;
            }
            misses.insert((host.to_string(), scid.to_string()), Instant::now());
        }
    }

    /// Whether `chain`'s head is the log's trusted head: [`KelCache::trusted`] (the accepted
    /// head, while it verifies under the current `identity.storeKeys` and the cache holds its
    /// event) is at `chain`'s head, with its hash.
    async fn head_trusted(&self, chain: &KelChain) -> bool {
        let head = chain.head();
        if !matches!(self.cache.trusted(chain.id()).await, Ok(Some((_, seq))) if seq == head.seq) {
            return false;
        }
        matches!(
            self.cache.accepted_head(chain.id()),
            Ok(Some(accepted)) if accepted.seq == head.seq && accepted.hash == head.hash
        )
    }

    /// The document of `chain` at `version`, or the method `#<fragment>` of it.
    async fn answer(
        &self,
        chain: &KelChain,
        version: Option<u64>,
        fragment: Option<&str>,
        mut meta: DidResolutionMetadata,
    ) -> DidResolutionResult {
        let did = did_of(chain.id());
        let also_known_as: Vec<String> = self
            .store_src()
            .map(|src| format!("{did}?src={src}"))
            .into_iter()
            .collect();
        meta.head_trusted = Some(self.head_trusted(chain).await);
        // The first recorded fork; else a conflict of this answer's `src` the caller set.
        let recorded = match self.cache.forks(chain.id()).await {
            Ok(forks) => forks.first().map(fork_record),
            Err(e) => {
                log::warn!("identity: could not read the forks of {did}: {e}");
                None
            }
        };
        meta.fork = recorded.or(meta.fork.take());
        let (document, metadata) = match did_document(chain, version, &also_known_as) {
            Ok(found) => found,
            Err(e) => return failure(ResolveError::NotFound(e.to_string()), meta),
        };
        let written = (|| {
            let metadata: DidDocumentMetadata = convert(&metadata)?;
            let method = match fragment {
                None => None,
                Some(f) => {
                    let id = format!("#{f}");
                    match document.verification_method.iter().find(|m| m.id == id) {
                        Some(m) => {
                            let mut value = serde_json::to_value(m)?;
                            value["id"] = Value::String(format!("{did}{id}"));
                            Some(Some(value))
                        }
                        None => Some(None),
                    }
                }
            };
            Ok::<_, serde_json::Error>((metadata, method, serde_json::to_value(&document)?))
        })();
        let (metadata, method, document) = match written {
            Ok(written) => written,
            Err(e) => return failure(ResolveError::Internal(e.to_string()), meta),
        };
        meta.content_type = Some(DID_LD_JSON.into());
        match method {
            None => DidResolutionResult {
                did_document: Some(document),
                did_document_metadata: metadata,
                did_resolution_metadata: meta,
                verification_method: None,
            },
            Some(Some(method)) => DidResolutionResult {
                did_document: None,
                did_document_metadata: metadata,
                did_resolution_metadata: meta,
                verification_method: Some(method),
            },
            Some(None) => {
                meta.content_type = None;
                failure(
                    ResolveError::NotFound(format!(
                        "{did} has no verification method #{} at version {}",
                        fragment.unwrap_or_default(),
                        metadata.version_id.unwrap_or_default()
                    )),
                    meta,
                )
            }
        }
    }
}

fn fork_record(fork: &ForkEvidence) -> ForkRecord {
    ForkRecord {
        seq: fork.seq,
        ours: fork.ours.clone(),
        theirs: fork.theirs.clone(),
    }
}

/// A result that carries `error` and no document.
fn failure(e: ResolveError, mut meta: DidResolutionMetadata) -> DidResolutionResult {
    meta.error = Some(e.code().into());
    meta.message = Some(e.message().to_string());
    DidResolutionResult {
        did_document: None,
        did_document_metadata: DidDocumentMetadata::default(),
        did_resolution_metadata: meta,
        verification_method: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::cache::tests::{accept_signed, in_memory, trusting, FakeStore, TestLog};
    use crate::identity::src_fetch::test_host::{from_of, response, TestHost};
    use crate::identity::types::DidDocument;
    use ad4m_identity_core::build::Signer;
    use std::sync::atomic::Ordering;
    use tokio::sync::Notify;

    fn scid_url(input: &str) -> ScidUrl {
        match parse_did_url(input).unwrap() {
            Target::Scid(url) => url,
            other => panic!("expected a did:scid URL, got {other:?}"),
        }
    }

    fn code(input: &str) -> &'static str {
        parse_did_url(input).unwrap_err().code()
    }

    #[test]
    fn a_did_url_parses_into_did_src_version_and_fragment() {
        let log = TestLog::new(1);
        let did = did_of(&log.scid());
        let url = scid_url(&did);
        assert_eq!((url.did.as_str(), url.scid), (did.as_str(), log.scid()));
        assert_eq!((url.src, url.version_id, url.fragment), (None, None, None));

        let url = scid_url(&format!("{did}?src=store.example:8443&versionId=4#d3"));
        assert_eq!(url.did, did, "the DID is the part before ? and #");
        assert_eq!(url.src.as_deref(), Some("store.example:8443"));
        assert_eq!(url.version_id, Some(4));
        assert_eq!(url.fragment.as_deref(), Some("d3"));

        // Values are percent-decoded; unknown parameters are ignored.
        let url = scid_url(&format!(
            "{did}?hl=x&src=https%3A%2F%2Fstore.example%2Fkel&versionId=%31%30#d1%2Denc"
        ));
        assert_eq!(url.src.as_deref(), Some("https://store.example/kel"));
        assert_eq!(url.version_id, Some(10));
        assert_eq!(url.fragment.as_deref(), Some("d1-enc"));
        assert_eq!(scid_url(&format!("{did}?")).src, None);
    }

    #[test]
    fn bad_did_urls_carry_their_did_core_error() {
        let did = did_of(&TestLog::new(1).scid());
        for (input, want) in [
            ("".to_string(), "invalidDid"),
            ("not a did".to_string(), "invalidDidUrl"),
            ("did:scid:coasys:1:QmNotAHash".to_string(), "invalidDid"),
            ("did:scid:coasys:1:".to_string(), "invalidDid"),
            ("scid:coasys:1:Qm".to_string(), "invalidDid"),
            ("did:web:example.org".to_string(), "methodNotSupported"),
            ("did:scid:coasys:2:Qm".to_string(), "methodNotSupported"),
            (format!("{did}/path"), "invalidDidUrl"),
            (
                format!("{did}?versionTime=2026-01-01T00:00:00Z"),
                "invalidDidUrl",
            ),
            (format!("{did}?versionId=x"), "invalidDidUrl"),
            (format!("{did}?versionId=-1"), "invalidDidUrl"),
            (format!("{did}?versionId="), "invalidDidUrl"),
            (
                format!("{did}?versionId=99999999999999999999999"),
                "invalidDidUrl",
            ),
            (format!("{did}?versionId=1&versionId=2"), "invalidDidUrl"),
            (
                format!("{did}?src=a.example&src=b.example"),
                "invalidDidUrl",
            ),
            (format!("{did}?src="), "invalidDidUrl"),
            (format!("{did}?src"), "invalidDidUrl"),
            (format!("{did}?src=%zz"), "invalidDidUrl"),
            (format!("{did}?src=%FF"), "invalidDidUrl"),
            (format!("{did}#"), "invalidDidUrl"),
            (format!("{did}#d1/x"), "invalidDidUrl"),
            (format!("{did} "), "invalidDidUrl"),
        ] {
            assert_eq!(code(&input), want, "{input}");
        }
    }

    #[test]
    fn a_device_did_key_parses_only_bare() {
        let log = TestLog::new(1);
        let key = log.device.key();
        assert_eq!(
            parse_did_url(&format!("did:key:{key}")).unwrap(),
            Target::Key(key.clone())
        );
        assert_eq!(code(&format!("did:key:{key}#d1")), "invalidDidUrl");
        assert_eq!(code(&format!("did:key:{key}?src=x")), "invalidDidUrl");
        assert_eq!(code("did:key:z6Mkbad"), "invalidDid");
        let enc = ad4m_identity_core::encode_x25519_multikey(&[1; 32]);
        assert_eq!(code(&format!("did:key:{enc}")), "invalidDid");
    }

    #[test]
    fn a_src_is_an_https_base_with_or_without_its_scheme() {
        let base = |src: &str| match parse_src(src).unwrap() {
            SrcKind::Https(b) => (b.base, b.label),
            other => panic!("{src}: {other:?}"),
        };
        assert_eq!(
            base("store.example"),
            ("https://store.example".into(), "store.example".into())
        );
        assert_eq!(
            base("store.example:8443/logs/"),
            (
                "https://store.example:8443/logs".into(),
                "store.example:8443/logs".into()
            )
        );
        assert_eq!(base("HTTPS://Store.Example").0, "https://store.example");
        assert_eq!(base("https://store.example:443").0, "https://store.example");
        assert_eq!(base("[2001:db8::1]:8443").0, "https://[2001:db8::1]:8443");
        // Another scheme names an adapter this resolver does not have.
        for (src, scheme) in [
            ("http://store.example", "http"),
            ("did:web:store.example", "did"),
            ("ipfs:Qm", "ipfs"),
        ] {
            assert_eq!(parse_src(src).unwrap(), SrcKind::Unsupported(scheme.into()));
        }
        for src in [
            "https:store.example",
            "https://u:p@store.example",
            "store.example?x=1",
            "store.example:99999",
            "a\\b",
        ] {
            assert!(parse_src(src).is_err(), "{src}");
        }
    }

    #[test]
    fn only_public_addresses_pass() {
        for ip in [
            "127.0.0.1",
            "127.255.0.9",
            "10.1.2.3",
            "172.16.0.1",
            "172.31.255.255",
            "192.168.1.1",
            "169.254.169.254",
            "100.64.0.1",
            "100.127.255.255",
            "0.0.0.0",
            "224.0.0.1",
            "255.255.255.255",
            "::",
            "::1",
            "fc00::1",
            "fd12:3456::1",
            "fe80::1",
            "ff02::1",
            "::ffff:127.0.0.1",
            "::ffff:10.0.0.1",
            "::127.0.0.1",
            "64:ff9b::a00:1",
            "2002:c0a8:101::1",
            "198.18.0.1",
            "198.19.255.255",
            "64:ff9b:1::a00:1",
            "64:ff9b:1:ffff::7f00:1",
            "::ffff:198.18.0.1",
        ] {
            assert!(!is_public(ip.parse().unwrap()), "{ip} must be refused");
        }
        for ip in [
            "203.0.113.7",
            "8.8.8.8",
            "172.32.0.1",
            "100.128.0.1",
            "2001:db8::1",
            "2606:4700::1111",
            "::ffff:8.8.8.8",
            "198.17.255.255",
            "198.20.0.1",
            "64:ff9b:1::808:808",
            "64:ff9b::808:808",
        ] {
            assert!(is_public(ip.parse().unwrap()), "{ip} is public");
        }
    }

    fn resolver(
        cache: KelCache,
        store: Arc<FakeStore>,
        store_url: Option<&str>,
    ) -> (Resolver, Arc<FakeStore>) {
        let backend: Arc<dyn IdentityBackend> = store.clone();
        (
            Resolver::new(Arc::new(cache), backend, store_url, false),
            store,
        )
    }

    fn methods(result: &DidResolutionResult) -> Vec<String> {
        let doc: DidDocument =
            serde_json::from_value(result.did_document.clone().expect("a document")).unwrap();
        doc.verification_method.into_iter().map(|m| m.id).collect()
    }

    /// A host on 127.0.0.1 serving `host_store`'s log over the did:scid host profile.
    async fn log_host(host_store: Arc<FakeStore>) -> TestHost {
        TestHost::start(move |target| match host_store.page(from_of(target)) {
            Some(page) => response("200 OK", &[], &serde_json::to_vec(&page).unwrap()),
            None => response("404 Not Found", &[], b""),
        })
        .await
    }

    /// A resolver with private srcs allowed that reads foreign hosts over plain HTTP, every
    /// name answering 127.0.0.1 (the hosts of [`log_host`]).
    fn local_resolver(cache: KelCache, store: Arc<FakeStore>) -> Resolver {
        let backend: Arc<dyn IdentityBackend> = store;
        let lookup: crate::identity::src_fetch::Lookup =
            Arc::new(|_| Box::pin(async { Ok(vec!["127.0.0.1".parse().unwrap()]) }));
        Resolver::new(Arc::new(cache), backend, None, true)
            .with_src_fetch(SrcFetch::plain_http(true, lookup))
    }

    #[tokio::test]
    async fn the_cache_answers_without_a_fetch() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let cache = in_memory(vec![]);
        cache.ingest(&log.scid(), &log.events).await.unwrap();
        let (resolver, store) = resolver(
            cache,
            Arc::new(FakeStore::new(vec![])),
            Some("https://store.example"),
        );
        let did = did_of(&log.scid());

        let head = resolver.resolve(&did, false).await;
        assert_eq!(head.did_resolution_metadata.error, None, "{head:?}");
        assert_eq!(
            head.did_resolution_metadata.source.as_deref(),
            Some("cache")
        );
        assert_eq!(head.did_resolution_metadata.head_trusted, Some(false));
        assert!(methods(&head).is_empty(), "d1 is revoked at the head");
        let doc = head.did_document.as_ref().unwrap();
        assert_eq!(doc["id"], did.as_str());
        assert_eq!(doc["alsoKnownAs"][0], format!("{did}?src=store.example"));
        assert_eq!(head.did_document_metadata.version_id.as_deref(), Some("1"));
        assert_eq!(head.did_document_metadata.next_version_id, None);
        assert_eq!(
            head.did_resolution_metadata.content_type.as_deref(),
            Some(DID_LD_JSON)
        );

        // The document is the core's own JSON, and the TypeScript contract reads it.
        let (core_doc, _) = ad4m_identity_core::did_document(
            &log.chain(),
            None,
            &[format!("{did}?src=store.example")],
        )
        .unwrap();
        assert_eq!(doc, &serde_json::to_value(&core_doc).unwrap());

        let earlier = resolver.resolve(&format!("{did}?versionId=0"), false).await;
        assert_eq!(methods(&earlier), vec!["#d1", "#d1-enc"]);
        assert_eq!(
            earlier.did_document_metadata.next_version_id.as_deref(),
            Some("1")
        );

        // A fragment dereferences one method, with an absolute id.
        let method = resolver
            .resolve(&format!("{did}?versionId=0#d1"), false)
            .await;
        assert!(method.did_document.is_none());
        let method = method.verification_method.unwrap();
        assert_eq!(method["id"], format!("{did}#d1"));
        assert_eq!(method["publicKeyMultibase"], log.device.key());
        let gone = resolver.resolve(&format!("{did}#d1"), false).await;
        assert_eq!(
            gone.did_resolution_metadata.error.as_deref(),
            Some("notFound")
        );
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 0);
    }

    #[tokio::test]
    async fn a_src_is_read_over_the_host_profile_and_its_head_is_not_trusted() {
        let log = TestLog::new(1);
        // The configured store holds no log of this person, so only the src answers.
        let store = Arc::new(FakeStore::new(Vec::new()));
        // The foreign host signs its head with the key the cache trusts: still, a foreign
        // src's head never counts.
        let host = log_host(Arc::new(FakeStore::new(log.events.clone()))).await;
        let resolver = local_resolver(in_memory(vec![store.key.multikey()]), store.clone());
        let did = did_of(&log.scid());
        let src = format!("log.example:{}", host.port);
        let result = resolver.resolve(&format!("{did}?src={src}"), false).await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(host.connections(), 1, "the executor read the src itself");
        assert_eq!(
            store.log_calls.load(Ordering::SeqCst),
            1,
            "after the src, the configured store is read on"
        );
        assert!(
            result.did_resolution_metadata.fork.is_none(),
            "a store with no log of it is no fork: {result:?}"
        );
        assert_eq!(
            result.did_resolution_metadata.source.as_deref(),
            Some(src.as_str())
        );
        assert_eq!(result.did_resolution_metadata.head_trusted, Some(false));
        assert_eq!(methods(&result), vec!["#d1", "#d1-enc"]);
        assert!(
            resolver.cache.chain(&log.scid()).await.unwrap().is_none(),
            "the src's log answered this resolution only"
        );
        assert!(resolver
            .cache
            .stored_events(&log.scid(), 0)
            .await
            .unwrap()
            .is_empty());
        assert!(
            result.did_document.unwrap().get("alsoKnownAs").is_none(),
            "no store configured: no alsoKnownAs"
        );
    }

    #[tokio::test]
    async fn the_trusted_store_gives_a_trusted_head() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, store) = resolver(
            in_memory(vec![store.key.multikey()]),
            store,
            Some("http://127.0.0.1:9"),
        );
        let did = did_of(&log.scid());
        // A src that names the configured store reads from the configured store.
        let result = resolver
            .resolve(&format!("{did}?src=127.0.0.1:9"), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(
            result.did_resolution_metadata.source.as_deref(),
            Some("127.0.0.1:9")
        );
        assert_eq!(result.did_resolution_metadata.head_trusted, Some(true));
        assert!(
            store.log_calls.load(Ordering::SeqCst) > 0,
            "through the agent language"
        );
        assert_eq!(
            result.did_document_metadata.version_id.as_deref(),
            Some("1")
        );
    }

    #[tokio::test]
    async fn a_version_past_the_head_is_not_found() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, store) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let did = did_of(&log.scid());
        let result = resolver.resolve(&format!("{did}?versionId=5"), false).await;
        assert_eq!(
            result.did_resolution_metadata.error.as_deref(),
            Some("notFound")
        );
        assert!(result.did_document.is_none());
        // The cache lacked the seq, so the store was asked.
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn a_fork_is_reported_and_the_held_log_answers() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, store) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let did = did_of(&log.scid());
        resolver.resolve(&did, false).await;
        // The store now withholds the revoke.
        store.events.lock().unwrap().truncate(1);
        let result = resolver.resolve(&did, true).await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        let fork = result
            .did_resolution_metadata
            .fork
            .clone()
            .expect("the fork");
        assert_eq!(fork.seq, 1);
        assert_eq!(fork.ours, log.chain().head().hash);
        assert_eq!(
            result.did_document_metadata.version_id.as_deref(),
            Some("1")
        );
        assert!(methods(&result).is_empty(), "the held log, with the revoke");
    }

    #[tokio::test]
    async fn private_src_hosts_are_refused_without_a_fetch() {
        let log = TestLog::new(1);
        let did = did_of(&log.scid());
        let (resolver, store) = resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])), None);
        for src in [
            "127.0.0.1",
            "10.0.0.8",
            "192.168.1.1:8443",
            "169.254.169.254",
            "100.64.1.1",
            "[::1]",
            "[fc00::1]",
            "[::ffff:127.0.0.1]",
            "localhost:8443",
            "https://0x7f.1",
        ] {
            let result = resolver.resolve(&format!("{did}?src={src}"), true).await;
            assert_eq!(
                result.did_resolution_metadata.error.as_deref(),
                Some("notFound"),
                "{src}"
            );
            let skipped = &result.did_resolution_metadata.skipped;
            assert!(
                skipped
                    .first()
                    .is_some_and(|s| s.reason.starts_with("refused")),
                "{src}: {skipped:?}"
            );
        }
        // The configured store was still asked, as the next source.
        assert!(store.log_calls.load(Ordering::SeqCst) > 0);
    }

    #[tokio::test]
    async fn allow_private_src_lets_a_loopback_src_through() {
        let log = TestLog::new(1);
        let host = log_host(Arc::new(FakeStore::new(log.events.clone()))).await;
        let resolver = local_resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])));
        let did = did_of(&log.scid());
        let result = resolver
            .resolve(&format!("{did}?src=127.0.0.1:{}", host.port), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(host.connections(), 1);
    }

    #[tokio::test]
    async fn a_failed_foreign_fetch_is_not_repeated_for_a_while() {
        let log = TestLog::new(1);
        // The host holds no log: 404.
        let host = log_host(Arc::new(FakeStore::new(vec![]))).await;
        let resolver = local_resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])));
        let url = format!("{}?src=log.example:{}", did_of(&log.scid()), host.port);
        for _ in 0..3 {
            let result = resolver.resolve(&url, true).await;
            assert_eq!(
                result.did_resolution_metadata.error.as_deref(),
                Some("notFound")
            );
        }
        assert_eq!(host.connections(), 1, "one fetch, then the miss");
        let result = resolver.resolve(&url, true).await;
        assert!(result.did_resolution_metadata.skipped[0]
            .reason
            .contains("failed within"));
    }

    #[tokio::test]
    async fn a_foreign_host_failure_is_skipped_with_its_reason() {
        let log = TestLog::new(1);
        let host = TestHost::start(|_| response("500 Internal Server Error", &[], b"")).await;
        let resolver = local_resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])));
        let url = format!("{}?src=log.example:{}", did_of(&log.scid()), host.port);
        let result = resolver.resolve(&url, true).await;
        let skipped = &result.did_resolution_metadata.skipped;
        assert_eq!(skipped[0].src, format!("log.example:{}", host.port));
        assert_eq!(
            skipped[0].reason,
            "the host answered 500 Internal Server Error"
        );
    }

    #[tokio::test]
    async fn a_name_that_answers_a_private_address_at_fetch_time_is_never_connected() {
        // The up-front check passes a name; the address it answers when the client connects
        // is what counts (DNS rebinding).
        let log = TestLog::new(1);
        let host = log_host(Arc::new(FakeStore::new(log.events.clone()))).await;
        let lookup: crate::identity::src_fetch::Lookup =
            Arc::new(|_| Box::pin(async { Ok(vec!["127.0.0.1".parse().unwrap()]) }));
        let backend: Arc<dyn IdentityBackend> = Arc::new(FakeStore::new(vec![]));
        let resolver = Resolver::new(Arc::new(in_memory(vec![])), backend, None, false)
            .with_src_fetch(SrcFetch::plain_http(false, lookup));
        let url = format!("{}?src=rebind.example:{}", did_of(&log.scid()), host.port);
        let result = resolver.resolve(&url, true).await;
        assert_eq!(
            result.did_resolution_metadata.error.as_deref(),
            Some("notFound")
        );
        let reason = &result.did_resolution_metadata.skipped[0].reason;
        assert!(reason.contains("not a public address"), "{reason}");
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert_eq!(host.connections(), 0);
    }

    #[tokio::test]
    async fn an_unsupported_representation_is_refused() {
        let log = TestLog::new(1);
        let (resolver, _) = resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])), None);
        let did = did_of(&log.scid());
        let result = resolver
            .resolve_as(&did, false, Some("application/did+cbor"))
            .await;
        assert_eq!(
            result.did_resolution_metadata.error.as_deref(),
            Some("representationNotSupported")
        );
    }

    #[tokio::test]
    async fn a_device_did_key_resolves_to_its_person() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, store) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let result = resolver
            .resolve(&format!("did:key:{}", log.device.key()), false)
            .await;
        let meta = &result.did_resolution_metadata;
        assert_eq!(meta.error, None, "{result:?}");
        assert_eq!(meta.key_id.as_deref(), Some("d1"));
        assert_eq!(meta.key_valid, Some(true));
        assert_eq!(meta.key_index_mismatch, None);
        assert_eq!(
            result.did_document.as_ref().unwrap()["id"],
            did_of(&log.scid())
        );
        // The confirmed key is remembered: no second index call is needed.
        store.events.lock().unwrap().clear();
        let again = resolver
            .resolve(&format!("did:key:{}", log.device.key()), false)
            .await;
        assert_eq!(again.did_resolution_metadata.key_id.as_deref(), Some("d1"));
    }

    #[tokio::test]
    async fn a_revoked_device_key_still_names_its_person() {
        let mut log = TestLog::new(1);
        log.revoke_d1();
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, _) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let result = resolver
            .resolve(&format!("did:key:{}", log.device.key()), false)
            .await;
        let meta = &result.did_resolution_metadata;
        assert_eq!(meta.error, None, "{result:?}");
        assert_eq!(meta.key_id.as_deref(), Some("d1"));
        assert_eq!(meta.key_valid, Some(false));
        assert!(methods(&result).is_empty());
    }

    #[tokio::test]
    async fn an_unknown_device_key_is_not_found() {
        let log = TestLog::new(1);
        let stranger = TestLog::new(2);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (resolver, _) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let result = resolver
            .resolve(&format!("did:key:{}", stranger.device.key()), false)
            .await;
        assert_eq!(
            result.did_resolution_metadata.error.as_deref(),
            Some("notFound")
        );
        assert_eq!(result.did_resolution_metadata.key_index_mismatch, None);
    }

    #[tokio::test]
    async fn an_index_answer_whose_log_lacks_the_key_is_a_mismatch() {
        let log = TestLog::new(1);
        let other = TestLog::new(2);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        // The index names `other`, whose log (cached here) does not list the key.
        *store.key_owner_override.lock().unwrap() = Some(did_of(&other.scid()));
        let cache = in_memory(vec![store.key.multikey()]);
        cache.ingest(&other.scid(), &other.events).await.unwrap();
        let (resolver, _) = resolver(cache, store, None);
        let result = resolver
            .resolve(&format!("did:key:{}", log.device.key()), false)
            .await;
        let meta = &result.did_resolution_metadata;
        assert_eq!(meta.error.as_deref(), Some("notFound"), "{result:?}");
        assert_eq!(meta.key_index_mismatch, Some(true));
        assert!(result.did_document.is_none());
    }

    // --- did:scid expressions: proof.record anchors the key ----------------------------------

    use super::test_profiles::{signed_profile, with_record};

    /// `verify_expression` on the JSON expression `answered`, its `proof.record` read as
    /// `verify_expression_proof_with` reads it.
    async fn verifies(cache: &KelCache, answered: &Value) -> bool {
        let expr: crate::types::Expression<Value> =
            serde_json::from_value(answered.clone()).unwrap();
        verify_expression(cache, None, &expr, &answered["proof"]["record"])
            .await
            .unwrap()
    }

    async fn cache_holding(log: &TestLog) -> KelCache {
        let cache = trusting();
        accept_signed(&cache, &log.events).await;
        cache
    }

    #[tokio::test]
    async fn a_profile_anchored_before_a_revocation_stays_valid_after_it() {
        let mut log = TestLog::new(41);
        let did = did_of(&log.scid());
        let profile = signed_profile(&did, &log.device);
        let anchored = with_record(&profile, &log.device, 1, 0);
        let cache = cache_holding(&log).await;
        assert!(verifies(&cache, &anchored).await);

        log.revoke_d1();
        accept_signed(&cache, &log.events).await;
        assert!(
            verifies(&cache, &anchored).await,
            "signed at seq 0, before the revoke at seq 1"
        );
        // Anchored at the head, the revoke counts: d1 is revoked there.
        assert!(!verifies(&cache, &with_record(&profile, &log.device, 2, 1)).await);
    }

    #[tokio::test]
    async fn a_profile_anchored_at_or_after_the_revocation_is_invalid() {
        let mut log = TestLog::new(42);
        let did = did_of(&log.scid());
        log.revoke_d1();
        let cache = cache_holding(&log).await;
        let profile = signed_profile(&did, &log.device);
        assert!(!verifies(&cache, &with_record(&profile, &log.device, 1, 1)).await);
    }

    #[tokio::test]
    async fn an_anchor_past_the_head_is_invalid() {
        let log = TestLog::new(43);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        let profile = signed_profile(&did, &log.device);
        assert!(!verifies(&cache, &with_record(&profile, &log.device, 1, 1)).await);
        assert!(!verifies(&cache, &with_record(&profile, &log.device, 1, 7)).await);
    }

    #[tokio::test]
    async fn a_tampered_record_is_invalid() {
        let mut log = TestLog::new(44);
        let did = did_of(&log.scid());
        let profile = signed_profile(&did, &log.device);
        // Signed anchored at seq 1, so the key is not valid there once revoked at seq 1...
        log.revoke_d1();
        let cache = cache_holding(&log).await;
        let at_one = with_record(&profile, &log.device, 3, 1);
        assert!(!verifies(&cache, &at_one).await);

        // ...and moving the anchor back, unsigned, does not help.
        let mut earlier = at_one.clone();
        earlier["proof"]["record"]["anchor"] = Value::from(0);
        assert!(!verifies(&cache, &earlier).await, "the anchor is signed");
        let mut version = with_record(&profile, &log.device, 3, 0);
        assert!(verifies(&cache, &version).await);
        version["proof"]["record"]["version"] = Value::from(4);
        assert!(!verifies(&cache, &version).await, "the version is signed");

        // A changed profile breaks the record signature (and the expression's).
        let mut changed = with_record(&profile, &log.device, 3, 0);
        changed["data"]["directMessageLanguage"] = Value::from("lang://other");
        assert!(!verifies(&cache, &changed).await);

        // Malformed records: a missing or an unsigned extra member, a bad signature.
        let good = with_record(&profile, &log.device, 3, 0);
        let mut missing = good.clone();
        missing["proof"]["record"]
            .as_object_mut()
            .unwrap()
            .remove("sig");
        let mut extra = good.clone();
        extra["proof"]["record"]["head"] = Value::from(1);
        let mut garbage = good.clone();
        garbage["proof"]["record"]["sig"] = Value::from("zNotASignature");
        let mut not_an_object = good.clone();
        not_an_object["proof"]["record"] = Value::from("0");
        for bad in [missing, extra, garbage, not_an_object] {
            assert!(!verifies(&cache, &bad).await, "{}", bad["proof"]["record"]);
        }

        // Another log's valid key signing the record for this person.
        let other = TestLog::new(45);
        accept_signed(&cache, &other.events).await;
        let foreign = with_record(&signed_profile(&did, &other.device), &other.device, 1, 0);
        assert!(!verifies(&cache, &foreign).await);
    }

    #[tokio::test]
    async fn a_record_that_holds_needs_the_expression_signature_too() {
        let log = TestLog::new(46);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        let mut profile = signed_profile(&did, &log.device);
        // The expression's own signature is broken; the record signs the broken profile.
        profile["proof"]["signature"] = Value::from(hex::encode([7u8; 64]));
        assert!(!verifies(&cache, &with_record(&profile, &log.device, 1, 0)).await);
    }

    /// Adds a bot key (`read`, never expiring) to `log` at the next seq; its secret.
    fn add_bot(log: &mut TestLog, seed: u8) -> ed25519_dalek::SigningKey {
        use ad4m_identity_core::build::{add_unsigned, cosign};
        use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
        let bot = ed25519_dalek::SigningKey::from_bytes(&[seed; 32]);
        let chain = log.chain();
        let device = DeviceKey {
            id: chain.state().next_device_id(),
            sign: bot.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[seed; 32]),
            life: KeyLife::Bot(BotScope {
                can: vec!["read".into()],
                expires: 4_000_000_000,
            }),
        };
        let mut event = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
        bot
    }

    #[tokio::test]
    async fn a_bot_key_signs_no_anchored_profile() {
        let mut log = TestLog::new(47);
        let bot = add_bot(&mut log, 78);
        let cache = cache_holding(&log).await;
        let did = did_of(&log.scid());
        let by_bot = signed_profile(&did, &bot);
        assert!(!verifies(&cache, &with_record(&by_bot, &bot, 1, 1)).await);
        // The standing device key at the same anchor does verify.
        let by_device = signed_profile(&did, &log.device);
        assert!(verifies(&cache, &with_record(&by_device, &log.device, 1, 1)).await);
    }

    #[tokio::test]
    async fn an_anchored_profile_refreshes_a_log_shorter_than_its_anchor_once() {
        let mut log = TestLog::new(48);
        let did = did_of(&log.scid());
        let store = FakeStore::new(vec![]);
        let cache = in_memory(vec![store.key.multikey()]);
        cache.ingest(&log.scid(), &log.events).await.unwrap();
        // The store holds seq 1 (a bot added); the cache only seq 0.
        add_bot(&mut log, 79);
        *store.events.lock().unwrap() = log.events.clone();
        let anchored = with_record(&signed_profile(&did, &log.device), &log.device, 1, 1);
        assert!(!verifies(&cache, &anchored).await, "past the cached head");
        let expr: crate::types::Expression<Value> =
            serde_json::from_value(anchored.clone()).unwrap();
        let record = &anchored["proof"]["record"];
        assert!(verify_expression(&cache, Some(&store), &expr, record)
            .await
            .unwrap());
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 1);
        // Now cached: no second read.
        assert!(verify_expression(&cache, Some(&store), &expr, record)
            .await
            .unwrap());
        assert_eq!(store.log_calls.load(Ordering::SeqCst), 1);
    }

    // --- authorship reads a log only up to its store-signed head -----------------------------

    /// Appends an `add` of the standing device key `key` to `log`, signed by its master only:
    /// what any controller can write, the key's holder agreeing or not (spec section 11).
    fn add_key(log: &mut TestLog, key: &ed25519_dalek::SigningKey, seed: u8) {
        use ad4m_identity_core::build::{add_unsigned, cosign};
        use ad4m_identity_core::{DeviceKey, KeyLife};
        let chain = log.chain();
        let device = DeviceKey {
            id: chain.state().next_device_id(),
            sign: key.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[seed; 32]),
            life: KeyLife::Standing,
        };
        let mut event = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
    }

    /// A log only ingested (as a foreign `src` serves it) that lists the victim's key: the key
    /// does not speak for the log's person, so an expression by it authored as that person is
    /// invalid, with or without a record.
    #[tokio::test]
    async fn a_log_without_a_store_signed_head_makes_no_key_speak() {
        let victim = TestLog::new(50);
        let mut attacker = TestLog::new(51);
        add_key(&mut attacker, &victim.device, 52);
        let x = did_of(&attacker.scid());
        let cache = trusting();
        accept_signed(&cache, &victim.events).await;
        cache
            .ingest(&attacker.scid(), &attacker.events)
            .await
            .unwrap();
        let now = super::super::service::now();
        assert!(cache.chain(&attacker.scid()).await.unwrap().is_some());
        // The victim's key is d2 at seq 1 of X's log, the attacker's own d1 at seq 0.
        assert!(!key_valid_at_anchor(&cache, &x, &victim.device.key(), 1, now).await);
        assert!(!key_valid_at_anchor(&cache, &x, &attacker.device.key(), 0, now).await);

        let rewritten = signed_profile(&x, &victim.device);
        assert!(!verifies(&cache, &with_record(&rewritten, &victim.device, 1, 1)).await);
        // The victim's key still speaks for the victim, whose log is the store's.
        let own = signed_profile(&did_of(&victim.scid()), &victim.device);
        assert!(verifies(&cache, &with_record(&own, &victim.device, 1, 0)).await);
    }

    /// A key added past the accepted head does not speak until a store-signed head covers it.
    #[tokio::test]
    async fn a_key_added_past_the_accepted_head_speaks_only_once_the_head_is_accepted() {
        let mut log = TestLog::new(53);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        let second = ed25519_dalek::SigningKey::from_bytes(&[54; 32]);
        add_key(&mut log, &second, 54);
        cache.ingest(&log.scid(), &log.events[1..]).await.unwrap();
        let now = super::super::service::now();
        // The second key is d2, added at seq 1; the trusted seq is 0.
        let profile = with_record(&signed_profile(&did, &second), &second, 1, 1);
        assert!(!key_valid_at_anchor(&cache, &did, &second.key(), 1, now).await);
        assert!(!verifies(&cache, &profile).await);
        // The key listed at the trusted seq still speaks.
        assert!(key_valid_at_anchor(&cache, &did, &log.device.key(), 0, now).await);

        accept_signed(&cache, &log.events).await;
        assert!(key_valid_at_anchor(&cache, &did, &second.key(), 1, now).await);
        assert!(verifies(&cache, &profile).await);
    }

    /// A revocation past the accepted head counts only once a store-signed head covers it: a
    /// profile anchored at the revoke's seq is past the trusted seq until then, and the key is
    /// revoked there after.
    #[tokio::test]
    async fn a_revocation_past_the_accepted_head_counts_once_the_head_is_accepted() {
        let mut log = TestLog::new(55);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        log.revoke_d1();
        cache.ingest(&log.scid(), &log.events[1..]).await.unwrap();
        let profile = signed_profile(&did, &log.device);
        let before = with_record(&profile, &log.device, 1, 0);
        let at_revoke = with_record(&profile, &log.device, 2, 1);
        assert!(verifies(&cache, &before).await, "at the trusted seq 0");
        assert!(
            !verifies(&cache, &at_revoke).await,
            "past the trusted seq 0"
        );

        accept_signed(&cache, &log.events).await;
        assert!(verifies(&cache, &before).await, "signed before the revoke");
        assert!(
            !verifies(&cache, &at_revoke).await,
            "revoked at the trusted seq 1"
        );
    }

    /// An anchored profile whose anchor is past the trusted seq is invalid, even though the
    /// cached log reaches it; valid once the head is accepted.
    #[tokio::test]
    async fn an_anchor_past_the_trusted_seq_is_invalid_until_the_head_is_accepted() {
        let mut log = TestLog::new(56);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        let second = ed25519_dalek::SigningKey::from_bytes(&[57; 32]);
        add_key(&mut log, &second, 57);
        cache.ingest(&log.scid(), &log.events[1..]).await.unwrap();
        let anchored = with_record(&signed_profile(&did, &log.device), &log.device, 1, 1);
        assert_eq!(
            cache.chain(&log.scid()).await.unwrap().unwrap().head().seq,
            1
        );
        assert!(
            !verifies(&cache, &anchored).await,
            "anchor 1 past trusted 0"
        );
        accept_signed(&cache, &log.events).await;
        assert!(verifies(&cache, &anchored).await);
    }

    /// Through the resolver: resolving `did:scid:X?src=<foreign host>` caches X's log, which
    /// lists the victim's key K; an expression by K with `author` rewritten to X still does not
    /// verify, nor does `did:key:K` resolve to X. The foreign host signs its head with a key
    /// the cache trusts: a foreign `src`'s head never counts.
    #[tokio::test]
    async fn a_foreign_log_listing_a_victims_key_does_not_make_it_speak() {
        let victim = TestLog::new(58);
        let mut attacker = TestLog::new(59);
        add_key(&mut attacker, &victim.device, 60);
        let x = did_of(&attacker.scid());
        let host = log_host(Arc::new(FakeStore::new(attacker.events.clone()))).await;
        // The configured store holds only the victim's log; its index names X for every key.
        let store = Arc::new(FakeStore::new(victim.events.clone()));
        *store.key_owner_override.lock().unwrap() = Some(x.clone());
        let resolver = local_resolver(trusting(), store.clone());

        let resolved = resolver
            .resolve(&format!("{x}?src=log.example:{}", host.port), false)
            .await;
        assert_eq!(resolved.did_resolution_metadata.error, None, "{resolved:?}");
        assert_eq!(resolved.did_resolution_metadata.head_trusted, Some(false));
        assert_eq!(
            methods(&resolved),
            vec!["#d1", "#d1-enc", "#d2", "#d2-enc"],
            "resolution shows the foreign log's keys"
        );
        assert!(
            resolver
                .cache
                .chain(&attacker.scid())
                .await
                .unwrap()
                .is_none(),
            "a foreign log is never cached"
        );

        let rewritten = signed_profile(&x, &victim.device);
        let backend: &dyn IdentityBackend = store.as_ref();
        // The foreign log adds K at seq 1: K speaks for X at neither anchor.
        for anchor in [0, 1] {
            let anchored = with_record(&rewritten, &victim.device, 1, anchor);
            let expr: crate::types::Expression<Value> =
                serde_json::from_value(anchored.clone()).unwrap();
            assert!(!verify_expression(
                &resolver.cache,
                Some(backend),
                &expr,
                &anchored["proof"]["record"]
            )
            .await
            .unwrap());
        }

        let by_key = resolver
            .resolve(&format!("did:key:{}", victim.device.key()), false)
            .await;
        assert_eq!(
            by_key.did_resolution_metadata.error.as_deref(),
            Some("notFound"),
            "{by_key:?}"
        );
        assert_eq!(by_key.did_resolution_metadata.key_id, None);
        assert!(by_key.did_document.is_none());
    }

    #[tokio::test]
    async fn a_src_that_serves_a_prefix_does_not_hide_a_revocation_the_store_holds() {
        // d1 is revoked at seq 1; the foreign host serves only seq 0.
        let mut log = TestLog::new(61);
        let prefix = log.events.clone();
        log.revoke_d1();
        let host = log_host(Arc::new(FakeStore::new(prefix))).await;
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let resolver = local_resolver(trusting(), store);
        let did = did_of(&log.scid());

        let result = resolver
            .resolve(&format!("{did}?src=log.example:{}", host.port), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert!(
            methods(&result).is_empty(),
            "d1 is revoked at the store's head: {result:?}"
        );
        assert_eq!(trusted_seq(&resolver.cache, &log.scid()).await, Some(1));
        assert!(!key_valid_at_anchor(&resolver.cache, &did, &log.device.key(), 1, 1).await);
    }

    /// The trusted seq of the cached log of `scid` ([`KelCache::trusted`]).
    async fn trusted_seq(cache: &KelCache, scid: &str) -> Option<u64> {
        cache
            .trusted(scid)
            .await
            .unwrap()
            .map(|(_, trusted)| trusted)
    }

    #[tokio::test]
    async fn a_src_prefix_never_speaks_for_its_person_while_the_store_answer_is_pending() {
        // d1 is revoked at seq 1; the foreign host serves only seq 0, and the store's answer is
        // held back until a concurrent verifier has looked at the cache.
        let mut log = TestLog::new(62);
        let prefix = log.events.clone();
        log.revoke_d1();
        let host = log_host(Arc::new(FakeStore::new(prefix))).await;
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let resolver = local_resolver(trusting(), store);
        let did = did_of(&log.scid());
        let d1 = log.device.key();
        let url = format!("{did}?src=log.example:{}", host.port);
        let (result, ()) = tokio::join!(resolver.resolve(&url, false), async {
            entered.notified().await;
            // The src has answered (this is the window) and the store's answer is pending.
            // The prefix serves this resolution only: the shared cache holds none of it, and
            // only a store-signed head makes a key speak.
            let held = resolver.cache.chain(&log.scid()).await.unwrap();
            assert!(held.is_none(), "the src's prefix is not cached");
            assert!(
                !key_valid_at_anchor(&resolver.cache, &did, &d1, 0, 1).await,
                "the src's prefix speaks for the person before the store answered"
            );
            release.notify_one();
        });
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert!(
            methods(&result).is_empty(),
            "d1 is revoked at the store's head"
        );
        assert_eq!(trusted_seq(&resolver.cache, &log.scid()).await, Some(1));
        assert!(!key_valid_at_anchor(&resolver.cache, &did, &d1, 1, 1).await);
    }

    #[tokio::test]
    async fn a_src_log_never_speaks_when_the_store_cannot_answer() {
        let log = TestLog::new(63);
        let host = log_host(Arc::new(FakeStore::new(log.events.clone()))).await;
        let store = Arc::new(FakeStore::new(log.events.clone()));
        store.store_down.store(true, Ordering::SeqCst);
        let resolver = local_resolver(trusting(), store);
        let did = did_of(&log.scid());
        let result = resolver
            .resolve(&format!("{did}?src=log.example:{}", host.port), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(
            methods(&result),
            vec!["#d1", "#d1-enc"],
            "shown in resolution"
        );
        assert!(
            resolver.cache.chain(&log.scid()).await.unwrap().is_none(),
            "a foreign log is never cached"
        );
        assert!(resolver
            .cache
            .stored_events(&log.scid(), 0)
            .await
            .unwrap()
            .is_empty());
        assert!(resolver.cache.accepted_head(&log.scid()).unwrap().is_none());
        assert!(!key_valid_at_anchor(&resolver.cache, &did, &log.device.key(), 0, 1).await);
    }

    /// A foreign log that differs from the cache's at a seq both hold: the answer is the
    /// cache's log with the conflict as `coasys:fork`, and nothing is recorded (the foreign
    /// log was never cached).
    #[tokio::test]
    async fn a_src_that_conflicts_with_the_cache_is_reported_not_recorded() {
        let mut held = TestLog::new(80);
        held.revoke_d1();
        let mut other = TestLog::new(80);
        add_key(
            &mut other,
            &ed25519_dalek::SigningKey::from_bytes(&[81; 32]),
            81,
        );
        assert_eq!(held.scid(), other.scid());
        let host = log_host(Arc::new(FakeStore::new(other.events.clone()))).await;
        let resolver = local_resolver(trusting(), Arc::new(FakeStore::new(held.events.clone())));
        let did = did_of(&held.scid());
        resolver.resolve(&did, false).await;

        let result = resolver
            .resolve(&format!("{did}?src=log.example:{}", host.port), true)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        let fork = result
            .did_resolution_metadata
            .fork
            .clone()
            .expect("the conflict");
        assert_eq!(fork.seq, 1);
        assert_eq!(fork.ours, held.chain().head().hash);
        assert_eq!(fork.theirs, other.chain().head().hash);
        assert!(
            methods(&result).is_empty(),
            "the cache's log, with the revoke"
        );
        assert_eq!(result.did_resolution_metadata.head_trusted, Some(true));
        assert!(
            resolver.cache.forks(&held.scid()).await.unwrap().is_empty(),
            "no fork recorded"
        );
        assert_eq!(
            resolver
                .cache
                .chain(&held.scid())
                .await
                .unwrap()
                .unwrap()
                .head()
                .hash,
            held.chain().head().hash
        );
    }

    /// One resolution reads at most `MAX_SRC_LOG_BYTES` from a foreign host, whatever the page
    /// cap allows; past it the src is skipped and nothing of it is kept.
    #[tokio::test]
    async fn a_src_log_past_the_byte_cap_is_skipped() {
        let log = TestLog::new(82);
        let mut page =
            serde_json::to_value(FakeStore::new(log.events.clone()).page(0).unwrap()).unwrap();
        page["pad"] = Value::from("x".repeat(MAX_SRC_LOG_BYTES));
        let body = serde_json::to_vec(&page).unwrap();
        assert!(
            body.len() < ad4m_ldk::identity::MAX_LOG_PAGE_BYTES,
            "under the page cap"
        );
        let host = TestHost::start(move |_| response("200 OK", &[], &body)).await;
        let resolver = local_resolver(trusting(), Arc::new(FakeStore::new(vec![])));
        let did = did_of(&log.scid());
        let src = format!("log.example:{}", host.port);
        let result = resolver.resolve(&format!("{did}?src={src}"), false).await;
        assert_eq!(
            result.did_resolution_metadata.error.as_deref(),
            Some("notFound"),
            "{result:?}"
        );
        let skipped = &result.did_resolution_metadata.skipped;
        assert!(
            skipped
                .iter()
                .any(|s| s.src == src && s.reason.contains("bytes left to read")),
            "{skipped:?}"
        );
        assert!(resolver.cache.chain(&log.scid()).await.unwrap().is_none());
    }

    /// The master key signs the log, never for its person: an expression it signed with the
    /// person as author is invalid, with or without a record.
    #[tokio::test]
    async fn the_master_key_never_signs_for_its_person() {
        let log = TestLog::new(83);
        let did = did_of(&log.scid());
        let cache = cache_holding(&log).await;
        let now = super::super::service::now();
        assert!(!key_valid_at_anchor(&cache, &did, &log.master.key(), 0, now).await);
        let by_master = signed_profile(&did, &log.master);
        assert!(!verifies(&cache, &with_record(&by_master, &log.master, 1, 0)).await);
        let by_device = signed_profile(&did, &log.device);
        assert!(verifies(&cache, &with_record(&by_device, &log.device, 1, 0)).await);
    }

    /// A log that admitted a key is read again from the store once per `ADMIT_FRESHNESS`, and
    /// the read advances its trusted seq. With the store unreachable the cached trusted head
    /// answers, and the failed attempt counts as the window's read. The anchor fixes the
    /// verdict: a revocation the refresh brings in does not touch a profile signed before it.
    #[tokio::test]
    async fn an_admitting_log_is_read_again_once_per_freshness_window() {
        let mut log = TestLog::new(84);
        let did = did_of(&log.scid());
        let store = FakeStore::new(log.events.clone());
        let cache = trusting();
        let anchored = with_record(&signed_profile(&did, &log.device), &log.device, 1, 0);
        let expr: crate::types::Expression<Value> =
            serde_json::from_value(anchored.clone()).unwrap();
        let record = &anchored["proof"]["record"];
        let backend: &dyn IdentityBackend = &store;
        let verify = |now: u64| verify_expression_at(&cache, Some(backend), &expr, record, now);
        let now = super::super::service::now();
        assert!(verify(now).await.unwrap(), "read from the store, d1 valid");

        log.revoke_d1();
        *store.events.lock().unwrap() = log.events.clone();
        let reads = store.log_calls.load(Ordering::SeqCst);
        assert!(verify(now + ADMIT_FRESHNESS - 5).await.unwrap());
        assert_eq!(store.log_calls.load(Ordering::SeqCst), reads, "no read");
        assert_eq!(trusted_seq(&cache, &log.scid()).await, Some(0));

        store.store_down.store(true, Ordering::SeqCst);
        assert!(
            verify(now + ADMIT_FRESHNESS + 5).await.unwrap(),
            "offline: the cached trusted head answers"
        );
        assert_eq!(store.log_calls.load(Ordering::SeqCst), reads + 1);

        store.store_down.store(false, Ordering::SeqCst);
        assert!(verify(now + ADMIT_FRESHNESS + 10).await.unwrap());
        assert_eq!(
            store.log_calls.load(Ordering::SeqCst),
            reads + 1,
            "a failed attempt counts: no new read within the window after it"
        );
        assert_eq!(trusted_seq(&cache, &log.scid()).await, Some(0));

        assert!(
            verify(now + 2 * ADMIT_FRESHNESS + 10).await.unwrap(),
            "signed at seq 0, before the revoke"
        );
        assert_eq!(store.log_calls.load(Ordering::SeqCst), reads + 2);
        assert_eq!(
            trusted_seq(&cache, &log.scid()).await,
            Some(1),
            "past the next window the store's revocation is read"
        );
    }

    /// `coasys:headTrusted` follows `KelCache::trusted`: once the key that signed the accepted
    /// head leaves `identity.storeKeys`, the cached log answers with an untrusted head.
    #[tokio::test]
    async fn head_trusted_follows_the_configured_store_keys_across_a_reopen() {
        use crate::identity::head::test_store_key::TestStoreKey;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let log = TestLog::new(85);
        let did = did_of(&log.scid());
        let store = Arc::new(FakeStore::new(log.events.clone()));
        {
            let cache = KelCache::open(&path, vec![store.key.multikey()]).unwrap();
            let (resolver, _) = resolver(cache, store.clone(), None);
            let result = resolver.resolve(&did, false).await;
            assert_eq!(result.did_resolution_metadata.head_trusted, Some(true));
        }
        let cache = KelCache::open(&path, vec![TestStoreKey::new(3).multikey()]).unwrap();
        let (resolver, store) = resolver(cache, store, None);
        let calls = store.log_calls.load(Ordering::SeqCst);
        let result = resolver.resolve(&did, false).await;
        assert_eq!(
            store.log_calls.load(Ordering::SeqCst),
            calls,
            "the cache answered"
        );
        assert_eq!(
            result.did_resolution_metadata.source.as_deref(),
            Some("cache")
        );
        assert_eq!(result.did_resolution_metadata.head_trusted, Some(false));
    }

    #[tokio::test]
    async fn a_tampered_record_is_invalid_even_when_the_key_is_valid_at_the_head() {
        // d1 is valid at every seq, so only the record itself refuses these.
        let log = TestLog::new(64);
        let did = did_of(&log.scid());
        let profile = signed_profile(&did, &log.device);
        let cache = cache_holding(&log).await;
        let good = with_record(&profile, &log.device, 3, 0);
        assert!(verifies(&cache, &good).await);
        let mut garbage = good.clone();
        garbage["proof"]["record"]["sig"] = Value::from("zNotASignature");
        let mut version = good.clone();
        version["proof"]["record"]["version"] = Value::from(4);
        let mut not_an_object = good.clone();
        not_an_object["proof"]["record"] = Value::from("0");
        for bad in [garbage, version, not_an_object] {
            assert!(!verifies(&cache, &bad).await, "{}", bad["proof"]["record"]);
        }
    }
}

/// `did:scid` profile expressions as the agent language answers them, for tests here and in
/// `languages`.
#[cfg(test)]
pub(crate) mod test_profiles {
    use ad4m_identity_core::build::Signer;
    use ed25519_dalek::SigningKey;
    use serde_json::{json, Value};

    /// An Agent profile authored by `author`, signed by the device key `signer` the way
    /// `create_signed_expression` signs, `proof.key` the signer's `did:key`: what an agent
    /// record stores as its `profile`.
    pub fn signed_profile(author: &str, signer: &SigningKey) -> Value {
        let data = crate::js_core::utils::sort_json_value(&json!({
            "did": author,
            "perspective": { "links": [] },
            "directMessageLanguage": "lang://dm",
        }));
        let timestamp = chrono::Utc::now();
        let signature = ed25519_dalek::Signer::sign(
            signer,
            &crate::agent::signatures::hash_data_and_timestamp(&data, &timestamp),
        );
        json!({
            "author": author,
            "timestamp": timestamp.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            "data": data,
            "proof": {
                "key": format!("did:key:{}", signer.key()),
                "signature": hex::encode(signature.to_bytes()),
            },
        })
    }

    /// `profile` with `proof.record`: `signer`'s `coasys:link` signature over the agent record
    /// `{profile, version, anchor}`, as the agent language answers a `did:scid` profile.
    pub fn with_record(profile: &Value, signer: &SigningKey, version: u64, anchor: u64) -> Value {
        let record = ad4m_ldk::identity::AgentRecord {
            did: profile["author"].as_str().unwrap().to_string(),
            profile: profile.clone(),
            version,
            signed_by: profile["proof"]["key"].as_str().unwrap().to_string(),
            anchor: Some(anchor),
            sig: String::new(),
        };
        let bytes = ad4m_identity_core::signing_bytes(
            ad4m_identity_core::LINK_DOMAIN,
            &record.signed_value(),
        )
        .unwrap();
        let sig = ad4m_hash_chain::encode_proof_value(&signer.sign(&bytes));
        let mut answered = profile.clone();
        answered["proof"]["record"] = json!({ "version": version, "anchor": anchor, "sig": sig });
        answered
    }
}

#[cfg(test)]
mod link_check_tests {
    use super::*;
    use crate::agent::signatures::{
        anchored_signing_bytes, hash_data_and_timestamp, signature_holds, verify_by, TestSigner,
    };
    use crate::identity::cache::tests::{accept_signed, trusting, FakeStore, TestLog};
    use crate::identity::verdict::{InvalidReason, KeyVerdict, VerdictChange};
    use crate::types::{Expression, ExpressionProof, Link};
    use ad4m_identity_core::build::Signer;
    use std::sync::atomic::Ordering;

    const NOW: u64 = 1_000;

    fn link() -> Link {
        Link {
            source: "ad4m://s".into(),
            predicate: Some("ad4m://p".into()),
            target: "ad4m://t".into(),
        }
    }

    /// A link signed by `signer` (a key of a [`TestLog`]): anchored at `anchor`, or over the
    /// old signed bytes without one.
    fn signed_with(signer: &dyn Signer, anchor: Option<u64>) -> Expression<Link> {
        let key = signer.key();
        let now = chrono::Utc::now();
        let timestamp = now.to_rfc3339_opts(chrono::SecondsFormat::Millis, true);
        let data = link();
        let bytes = match anchor {
            Some(anchor) => anchored_signing_bytes(&data, &timestamp, anchor).unwrap(),
            None => hash_data_and_timestamp(&data, &now),
        };
        Expression {
            author: format!("did:key:{key}"),
            timestamp,
            data,
            proof: ExpressionProof {
                key: format!("did:key:{key}#{key}"),
                signature: hex::encode(signer.sign(&bytes)),
                anchor,
            },
        }
    }

    async fn cache_with(log: &TestLog) -> KelCache {
        let cache = trusting();
        accept_signed(&cache, &log.events).await;
        cache
    }

    fn check(cache: &KelCache, expr: &Expression<Link>) -> CheckedLink {
        check_signed_with(expr, cache.key_map(), None, NOW)
    }

    fn valid_for(did: Option<String>) -> CheckedLink {
        CheckedLink {
            verdict: LinkVerdict::Valid,
            person: did,
        }
    }

    #[tokio::test]
    async fn an_anchored_link_by_a_device_key_counts_for_its_person() {
        let log = TestLog::new(61);
        let cache = cache_with(&log).await;
        let expr = signed_with(&log.device, Some(0));
        assert!(signature_holds(&expr));
        assert_eq!(check(&cache, &expr), valid_for(Some(did_of(&log.scid()))));
    }

    #[tokio::test]
    async fn the_signature_covers_the_anchor() {
        let log = TestLog::new(62);
        let cache = cache_with(&log).await;
        let mut moved = signed_with(&log.device, Some(0));
        moved.proof.anchor = Some(1);
        assert_eq!(
            check(&cache, &moved).verdict,
            LinkVerdict::Invalid(InvalidReason::BadSignature)
        );
        let mut dropped = signed_with(&log.device, Some(0));
        dropped.proof.anchor = None;
        assert_eq!(
            check(&cache, &dropped).verdict,
            LinkVerdict::Invalid(InvalidReason::BadSignature)
        );
    }

    #[test]
    fn a_did_key_link_without_an_anchor_verifies_as_before() {
        let signer = TestSigner::generate();
        let expr = signer.sign(link());
        assert!(
            verify_by(&expr, &expr.author).unwrap(),
            "the old signed bytes"
        );
        let map = KeyMap::new();
        assert_eq!(check_signed_with(&expr, &map, None, NOW), valid_for(None));
        map.note_no_log(&signer.multikey());
        assert_eq!(check_signed_with(&expr, &map, None, NOW), valid_for(None));
    }

    #[test]
    fn an_anchored_link_by_a_key_no_log_lists_does_not_count() {
        let signer = TestSigner::generate();
        let expr = signer.sign_anchored(link(), 0);
        assert!(signature_holds(&expr));
        let map = KeyMap::new();
        assert_eq!(
            check_signed_with(&expr, &map, None, NOW).verdict,
            LinkVerdict::CantCheck,
            "nothing known yet"
        );
        map.note_no_log(&signer.multikey());
        assert_eq!(
            check_signed_with(&expr, &map, None, NOW).verdict,
            LinkVerdict::Invalid(InvalidReason::NotListed)
        );
    }

    #[tokio::test]
    async fn a_master_key_never_signs_a_link() {
        let log = TestLog::new(63);
        let cache = cache_with(&log).await;
        for anchor in [Some(0), None] {
            let expr = signed_with(&log.master, anchor);
            assert_eq!(
                check(&cache, &expr).verdict,
                LinkVerdict::Invalid(InvalidReason::MasterKey)
            );
        }
    }

    #[tokio::test]
    async fn an_anchor_past_the_trusted_seq_cant_be_checked() {
        let log = TestLog::new(64);
        let cache = cache_with(&log).await;
        let expr = signed_with(&log.device, Some(1));
        assert_eq!(check(&cache, &expr).verdict, LinkVerdict::CantCheck);
    }

    #[tokio::test]
    async fn a_key_revoked_at_or_before_the_anchor_is_invalid() {
        let mut log = TestLog::new(65);
        log.revoke_d1();
        let cache = cache_with(&log).await;
        assert_eq!(
            check(&cache, &signed_with(&log.device, Some(1))).verdict,
            LinkVerdict::Invalid(InvalidReason::Revoked)
        );
        assert_eq!(
            check(&cache, &signed_with(&log.device, Some(0))),
            valid_for(Some(did_of(&log.scid()))),
            "signed before the revocation"
        );
    }

    #[tokio::test]
    async fn a_revocation_reaches_the_link_that_could_not_be_checked() {
        let mut log = TestLog::new(66);
        let cache = cache_with(&log).await;
        let mut changes = cache.subscribe();
        let expr = signed_with(&log.device, Some(1));
        assert_eq!(check(&cache, &expr).verdict, LinkVerdict::CantCheck);

        log.revoke_d1();
        accept_signed(&cache, &log.events).await;
        assert_eq!(
            changes.try_recv().expect("a verdict change"),
            VerdictChange {
                key: log.device.key(),
                scid: log.scid(),
                verdict: KeyVerdict::Revoked,
            }
        );
        assert_eq!(
            check(&cache, &expr).verdict,
            LinkVerdict::Invalid(InvalidReason::Revoked)
        );
    }

    /// A key that signed before any log listed it (a guest key the claim lists as `d1`).
    #[tokio::test]
    async fn an_unanchored_link_by_a_key_a_log_adopts_is_judged_at_its_listing() {
        let mut log = TestLog::new(67);
        let expr = signed_with(&log.device, None);
        let cache = trusting();
        cache.key_map().note_no_log(&log.device.key());
        assert_eq!(check(&cache, &expr), valid_for(None), "no person yet");

        let mut changes = cache.subscribe();
        accept_signed(&cache, &log.events).await;
        assert_eq!(
            changes.try_recv().expect("the key's first listing").verdict,
            KeyVerdict::Valid
        );
        let person = Some(did_of(&log.scid()));
        assert_eq!(check(&cache, &expr), valid_for(person.clone()));

        // Revoked later: judged at the seq that listed the key, so it stays the person's
        // unless the space saw it after the revocation.
        log.revoke_d1();
        accept_signed(&cache, &log.events).await;
        assert_eq!(check(&cache, &expr), valid_for(person));
        assert_eq!(
            check_signed_with(&expr, cache.key_map(), Some(true), NOW).verdict,
            LinkVerdict::Flagged
        );
    }

    fn resolver_on(store: Arc<FakeStore>) -> Resolver {
        let backend: Arc<dyn IdentityBackend> = store;
        Resolver::new(Arc::new(trusting()), backend, None, false)
    }

    #[tokio::test]
    async fn author_of_finds_the_person_once_and_keeps_it() {
        let log = TestLog::new(68);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let resolver = resolver_on(store.clone());
        let device = format!("did:key:{}", log.device.key());
        let person = Author::Person {
            did: did_of(&log.scid()),
            key_id: "d1".into(),
            revoked: None,
        };
        assert_eq!(resolver.author_of(&device).await, person);
        assert_eq!(resolver.author_of(&device).await, person);
        assert_eq!(store.key_owner_calls.load(Ordering::SeqCst), 1);
        assert_eq!(
            resolver
                .author_of(&format!("did:key:{}", log.master.key()))
                .await,
            Author::NoLog,
            "a master key speaks for no person"
        );
        assert_eq!(
            resolver.author_of("did:web:example.com").await,
            Author::NoLog
        );
    }

    #[tokio::test]
    async fn author_of_keeps_no_log_and_never_keeps_unavailable() {
        let store = Arc::new(FakeStore::new(TestLog::new(69).events));
        let resolver = resolver_on(store.clone());
        let guest = TestSigner::generate();
        assert_eq!(resolver.author_of(&guest.did).await, Author::NoLog);
        assert_eq!(resolver.author_of(&guest.did).await, Author::NoLog);
        assert_eq!(store.key_owner_calls.load(Ordering::SeqCst), 1);

        let other = TestSigner::generate();
        store.key_index_down.store(true, Ordering::SeqCst);
        assert_eq!(resolver.author_of(&other.did).await, Author::Unavailable);
        assert!(matches!(
            resolver.cache.key_map().lookup(&other.multikey()),
            KeyLookup::Unknown
        ));
        // The breaker: the index is not asked again at once.
        let calls = store.key_owner_calls.load(Ordering::SeqCst);
        assert_eq!(resolver.author_of(&other.did).await, Author::Unavailable);
        assert_eq!(store.key_owner_calls.load(Ordering::SeqCst), calls);
    }

    #[tokio::test]
    async fn prepare_fills_the_key_map_for_a_check_without_io() {
        let log = TestLog::new(70);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let resolver = resolver_on(store);
        let expr = signed_with(&log.device, Some(0));
        assert_eq!(
            check_signed_with(&expr, resolver.cache.key_map(), None, NOW).verdict,
            LinkVerdict::CantCheck
        );
        resolver
            .prepare(&[(expr.author.clone(), expr.proof.anchor)])
            .await;
        assert_eq!(
            check_signed_with(&expr, resolver.cache.key_map(), None, NOW),
            valid_for(Some(did_of(&log.scid())))
        );
    }
}
