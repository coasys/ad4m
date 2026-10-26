//! The one place the executor decides what a `did:scid:coasys:1` says: which key speaks for an
//! identity (for expressions and links) and the DID document of a DID URL (section 8 of
//! `docs-src/identity/did-scid-coasys-1.md`).
//!
//! - [`key_speaks_for`] and [`verify_expression`]: a device key's signature for its person, by
//!   the cached log (refreshed once when the cache does not admit the key).
//! - [`Resolver::resolve`]: a DID URL (`did:scid:coasys:1:<scid>[?src=…&versionId=…][#dN]`) or a
//!   bare device `did:key` to a DID Core resolution result, through the [`KelCache`] and the
//!   [`IdentityBackend`]. The cache only extends: a log from any source is imported, never put
//!   in place of a held one; a fork is recorded and reported.
//!
//! A `src` is a request to an address someone else chose: before one reaches the agent
//! language the resolver checks it is HTTPS and that its host resolves to public addresses only
//! ([`is_public`]), unless `identity.allowPrivateSrc` is set. The language resolves the name
//! again when it fetches, so a name that changes its answer between the two lookups (DNS
//! rebinding) or a redirect the fetch follows is not covered here (see `AGENTS.md`).

use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::{Duration, Instant};

use ad4m_identity_core::{did_document, did_of, scid_of, Event, KelChain};
use serde_json::Value;

use super::backend::{IdentityBackend, IdentityError};
use super::cache::{scid_from, CacheError, Ingested, KelCache};
use super::types::{
    convert, DidDocumentMetadata, DidResolutionMetadata, DidResolutionResult, ForkRecord,
    SkippedSource,
};

/// The representation the resolver writes.
pub const DID_LD_JSON: &str = "application/did+ld+json";
/// How long a failed fetch from a foreign host keeps the resolver from asking it again for the
/// same identity.
pub const MISS_TTL: Duration = Duration::from_secs(300);
/// The most pages the resolver reads from one source for one log.
const MAX_PAGES: usize = 64;
/// The most events a page holds (`identity-store`'s page size).
const PAGE_EVENTS: usize = 256;
/// How long the lookup of a `src` host may take.
const DNS_TIMEOUT: Duration = Duration::from_secs(5);
/// Bounds of the in-memory maps (failed fetches, confirmed keys).
const MAP_BOUND: usize = 4096;

// ---------------------------------------------------------------------------------------------
// Keys that speak for an identity
// ---------------------------------------------------------------------------------------------

/// Whether the signing multikey `key` speaks for `did` by the cached log alone: the log lists
/// it, valid at the cached head by `now` (unix seconds) and without the bot mark. `false` when
/// the cache holds no log of `did` or cannot read it.
pub async fn key_speaks_for(cache: &KelCache, did: &str, key: &str, now: u64) -> bool {
    let chain = match cache.chain(scid_from(did)).await {
        Ok(Some(chain)) => chain,
        Ok(None) => return false,
        Err(e) => {
            log::warn!("identity: the cached log of {did} is unreadable: {e}");
            return false;
        }
    };
    let state = chain.state();
    let head = chain.head().seq;
    state.key_id_of(&key.to_string()).is_some()
        && state.bot_scope_at(key, head).is_none()
        && state.key_valid_at_time(key, head, now)
}

/// Verifies `expr`, authored by a `did:scid`: its `proof.key` must be the bare `did:key` of a
/// key that the author's log in `cache` lists, valid at the cached head by the clock now and
/// without the bot mark (as a device key's signature counts everywhere in this module), and
/// the expression's signature must hold under that key. The expression's own timestamp does
/// not count: the signer states it. When the cached log does not admit the key (no log yet, or
/// a device added since), the log is refreshed once through `backend` and checked again.
pub async fn verify_expression<T: serde::Serialize>(
    cache: &KelCache,
    backend: Option<&dyn IdentityBackend>,
    expr: &crate::types::Expression<T>,
) -> Result<bool, deno_core::error::AnyError> {
    let Some(key) = expr
        .proof
        .key
        .strip_prefix("did:key:")
        .filter(|key| ad4m_hash_chain::decode_ed25519_multikey(key).is_some())
    else {
        return Ok(false);
    };
    let mut admitted = key_speaks_for(cache, &expr.author, key, super::service::now()).await;
    if let (false, Some(backend)) = (admitted, backend) {
        if let Err(e) = cache.refresh(backend, &expr.author).await {
            log::info!("identity: refresh of {} failed ({e})", expr.author);
        }
        admitted = key_speaks_for(cache, &expr.author, key, super::service::now()).await;
    }
    if !admitted {
        return Ok(false);
    }
    crate::agent::signatures::verify_by(expr, &expr.proof.key)
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
    /// `https://<host>[:port][/path]`, without a trailing `/`: what the agent language gets.
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
/// link-local, CGNAT (100.64/10), multicast, unspecified, "this network" (0/8), reserved
/// (240/4, broadcast) or IETF protocol (192.0.0/24). An IPv6 address that carries an IPv4 one
/// (mapped `::ffff:a.b.c.d`, compatible `::a.b.c.d`, NAT64 `64:ff9b::/96`, 6to4 `2002::/16`) is
/// judged by that IPv4 address.
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
            if s[..6] == [0x64, 0xff9b, 0, 0, 0, 0] {
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

/// Refuses a `src` host that is, or resolves to, any address that is not [`is_public`]. Every
/// address of a name must be public: the fetch may use any of them.
pub async fn check_public_host(url: &url::Url) -> Result<(), String> {
    let refuse = |ip: IpAddr| Err(format!("{ip} is not a public address"));
    match url.host() {
        Some(url::Host::Ipv4(ip)) if !is_public(ip.into()) => refuse(ip.into()),
        Some(url::Host::Ipv6(ip)) if !is_public(ip.into()) => refuse(ip.into()),
        Some(url::Host::Ipv4(_)) | Some(url::Host::Ipv6(_)) => Ok(()),
        Some(url::Host::Domain(name)) => {
            let name = name.trim_end_matches('.').to_ascii_lowercase();
            if name == "localhost" || name.ends_with(".localhost") {
                return Err(format!("{name} is a loopback name"));
            }
            let port = url.port_or_known_default().unwrap_or(443);
            let lookup = tokio::net::lookup_host((name.as_str(), port));
            let addrs: Vec<IpAddr> = match tokio::time::timeout(DNS_TIMEOUT, lookup).await {
                Ok(Ok(addrs)) => addrs.map(|a| a.ip()).collect(),
                Ok(Err(e)) => return Err(format!("{name} does not resolve: {e}")),
                Err(_) => return Err(format!("the lookup of {name} timed out")),
            };
            if addrs.is_empty() {
                return Err(format!("{name} has no address"));
            }
            match addrs.into_iter().find(|ip| !is_public(*ip)) {
                Some(ip) => Err(format!("{name} resolves to {ip}, not a public address")),
                None => Ok(()),
            }
        }
        None => Err("no host".into()),
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
}

/// DID resolution through the key-log cache and the agent language.
pub struct Resolver {
    cache: Arc<KelCache>,
    backend: Arc<dyn IdentityBackend>,
    /// `identity.storeUrl`, parsed.
    store: Option<url::Url>,
    /// `identity.allowPrivateSrc`: skip the public-address check (tests, local development).
    allow_private_src: bool,
    /// Failed fetches from foreign hosts: `(host label, scid)` → when.
    misses: StdMutex<HashMap<(String, String), Instant>>,
    /// Device keys confirmed in a log: signing multikey → SCID.
    keys: StdMutex<HashMap<String, String>>,
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
            misses: StdMutex::new(HashMap::new()),
            keys: StdMutex::new(HashMap::new()),
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
    /// list the key, and the answer is the person's document at the head.
    async fn resolve_key(&self, key: &str, fresh: bool) -> DidResolutionResult {
        let mut meta = DidResolutionMetadata::default();
        let known = self.keys.lock().ok().and_then(|k| k.get(key).cloned());
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

        let device_key = |chain: &KelChain| {
            chain
                .state()
                .keys
                .values()
                .find(|r| r.sign == key)
                .map(|r| r.id.clone())
        };
        let (mut chain, mut loaded) = match self.load(&scid, None, None, fresh).await {
            Ok(found) => found,
            Err((e, loaded)) => {
                meta.skipped = loaded.skipped;
                return failure(e, meta);
            }
        };
        if device_key(&chain).is_none() && loaded.source.is_none() && !fresh {
            // The cached log may predate the key: read it from the store once.
            if let Ok(found) = self.load(&scid, None, None, true).await {
                (chain, loaded) = found;
            }
        }
        meta.source = Some(loaded.source.unwrap_or_else(|| "cache".into()));
        meta.skipped = loaded.skipped;
        let Some(key_id) = device_key(&chain) else {
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
        if let Ok(mut keys) = self.keys.lock() {
            if keys.len() >= MAP_BOUND {
                keys.clear();
            }
            keys.insert(key.to_string(), scid.clone());
        }
        meta.key_valid = Some(chain.state().key_valid_at(key, chain.head().seq));
        meta.key_id = Some(key_id);
        self.answer(&chain, None, None, meta).await
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
    /// from the sources in order (after a `src` that answered, the configured store's events
    /// too), imported into the cache (which only extends), then from the cache.
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
        if let Some(chain) = cached.filter(|c| !fresh && covers(c)) {
            return Ok((chain, loaded));
        }

        let did = did_of(scid);
        for source in self.sources(src) {
            let (label, fetched) = match source {
                Source::Skip(skipped) => {
                    loaded.skipped.push(skipped);
                    continue;
                }
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
                    if !self.allow_private_src {
                        if let Err(why) = check_public_host(&base.url).await {
                            log::info!("identity: refused src {}: {why}", base.label);
                            loaded.skipped.push(SkippedSource {
                                src: base.label,
                                reason: format!("refused: {why}"),
                            });
                            continue;
                        }
                    }
                    let fetched = self.fetch_untrusted(scid, Some(&base.base)).await;
                    if matches!(&fetched, Err(e) if !matches!(e, CacheError::Fork(_))) {
                        self.note_miss(&base.label, scid);
                    }
                    if fetched.is_ok() {
                        // A foreign host may serve a prefix of the log, one that stops before a
                        // revocation, and the cache keeps what it imports: read on from the
                        // configured store, so a src cannot hide an event the store holds. A
                        // store that holds no log of it changes nothing.
                        match self.fetch_untrusted(scid, None).await {
                            Ok(()) | Err(CacheError::NotFound) => {}
                            Err(e) => loaded.skipped.push(SkippedSource {
                                src: self.store_label(),
                                reason: e.to_string(),
                            }),
                        }
                    }
                    (base.label, fetched)
                }
                Source::Store => {
                    let fetched = match self.cache.refresh(self.backend.as_ref(), &did).await {
                        Ok(_) => Ok(()),
                        // A head no key of identity.storeKeys signed: the events still count,
                        // the head does not.
                        Err(CacheError::BadHeadSignature) => self.fetch_untrusted(scid, None).await,
                        Err(e) => Err(e),
                    };
                    (self.store_label(), fetched)
                }
            };
            match fetched {
                Ok(()) => {
                    loaded.source = Some(label);
                    break;
                }
                // The cache keeps the log it held first; the fork is in the metadata.
                Err(CacheError::Fork(_)) => break,
                Err(e) => loaded.skipped.push(SkippedSource {
                    src: label,
                    reason: e.to_string(),
                }),
            }
        }

        match self.cache.chain(scid).await {
            Ok(Some(chain)) => Ok((chain, loaded)),
            Ok(None) => Err((
                ResolveError::NotFound(format!("no source holds a log of {did}")),
                loaded,
            )),
            Err(e) => Err((internal(e), loaded)),
        }
    }

    /// Reads `scid`'s log from `src` (the configured store when `None`) page by page from the
    /// cached head and imports the events with no head: a host's head is a hint of how far to
    /// page, never trusted.
    async fn fetch_untrusted(&self, scid: &str, src: Option<&str>) -> Result<(), CacheError> {
        let did = did_of(scid);
        for _ in 0..MAX_PAGES {
            let cached = self.cache.chain(scid).await?;
            let from = cached.as_ref().map_or(0, |c| c.head().seq);
            let Some(page) = self.backend.log(&did, from, src).await? else {
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

    /// Whether the cache's accepted (store-signed) head is the log's head.
    fn head_trusted(&self, chain: &KelChain) -> bool {
        let head = chain.head();
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
        meta.head_trusted = Some(self.head_trusted(chain));
        meta.fork = match self.cache.forks(chain.id()).await {
            Ok(forks) => forks.first().map(|f| ForkRecord {
                seq: f.seq,
                ours: f.ours.clone(),
                theirs: f.theirs.clone(),
            }),
            Err(e) => {
                log::warn!("identity: could not read the forks of {did}: {e}");
                None
            }
        };
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
    use crate::identity::cache::tests::{in_memory, FakeStore, TestLog};
    use crate::identity::types::DidDocument;
    use ad4m_identity_core::build::Signer;
    use std::sync::atomic::Ordering;

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

    fn foreign_srcs(store: &FakeStore) -> Vec<String> {
        store
            .log_srcs
            .lock()
            .unwrap()
            .iter()
            .flatten()
            .cloned()
            .collect()
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
    async fn a_src_is_read_once_checked_and_its_head_is_not_trusted() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        // The cache trusts the store's key: still, a foreign src's head never counts.
        let (resolver, store) = resolver(in_memory(vec![store.key.multikey()]), store, None);
        let did = did_of(&log.scid());
        let result = resolver
            .resolve(&format!("{did}?src=203.0.113.7:8443"), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(foreign_srcs(&store), vec!["https://203.0.113.7:8443"]);
        assert_eq!(
            result.did_resolution_metadata.source.as_deref(),
            Some("203.0.113.7:8443")
        );
        assert_eq!(result.did_resolution_metadata.head_trusted, Some(false));
        assert_eq!(methods(&result), vec!["#d1", "#d1-enc"]);
        assert!(
            result.did_document.unwrap().get("alsoKnownAs").is_none(),
            "no store configured: no alsoKnownAs"
        );
    }

    #[tokio::test]
    async fn a_src_that_serves_a_prefix_does_not_hide_a_revocation_the_store_holds() {
        // d1 is revoked at seq 1; the foreign host serves only seq 0.
        let mut log = TestLog::new(1);
        let prefix = log.events.clone();
        log.revoke_d1();
        let store = Arc::new(FakeStore::new(log.events.clone()));
        *store.foreign_events.lock().unwrap() = Some(prefix);
        let (resolver, store) = resolver(
            in_memory(vec![store.key.multikey()]),
            store,
            Some("https://store.example"),
        );
        let did = did_of(&log.scid());
        let result = resolver
            .resolve(&format!("{did}?src=203.0.113.7"), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(foreign_srcs(&store), vec!["https://203.0.113.7"]);
        assert!(
            methods(&result).is_empty(),
            "d1 is revoked at the store's head"
        );
        // Nor does the revoked key speak for the person on the expression read path.
        assert!(!key_speaks_for(&resolver.cache, &did, &log.device.key(), 1).await);
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
        assert!(foreign_srcs(&store).is_empty());
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
        assert!(
            foreign_srcs(&store).is_empty(),
            "no fetch reached a private host"
        );
        // The configured store was still asked, as the next source.
        assert!(store.log_calls.load(Ordering::SeqCst) > 0);
    }

    #[tokio::test]
    async fn allow_private_src_lets_a_loopback_src_through() {
        let log = TestLog::new(1);
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let backend: Arc<dyn IdentityBackend> = store.clone();
        let resolver = Resolver::new(Arc::new(in_memory(vec![])), backend, None, true);
        let did = did_of(&log.scid());
        let result = resolver
            .resolve(&format!("{did}?src=127.0.0.1:8443"), false)
            .await;
        assert_eq!(result.did_resolution_metadata.error, None, "{result:?}");
        assert_eq!(foreign_srcs(&store), vec!["https://127.0.0.1:8443"]);
    }

    #[tokio::test]
    async fn a_failed_foreign_fetch_is_not_repeated_for_a_while() {
        let log = TestLog::new(1);
        let (resolver, store) = resolver(in_memory(vec![]), Arc::new(FakeStore::new(vec![])), None);
        let url = format!("{}?src=203.0.113.9", did_of(&log.scid()));
        for _ in 0..3 {
            let result = resolver.resolve(&url, true).await;
            assert_eq!(
                result.did_resolution_metadata.error.as_deref(),
                Some("notFound")
            );
        }
        assert_eq!(foreign_srcs(&store).len(), 1, "one fetch, then the miss");
        let result = resolver.resolve(&url, true).await;
        assert!(result.did_resolution_metadata.skipped[0]
            .reason
            .contains("failed within"));
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
}
