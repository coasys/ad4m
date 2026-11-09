//! Login with a device key, and the end of the sessions of a key that stops being valid.
//!
//! A native app asks `user.sessionChallenge` for a one-use challenge, signs a [`KeyStatement`]
//! with one of its person's device keys under `coasys:session`, and sends it to
//! `user.loginWithKey`. The executor checks, in order: the challenge (consumed whatever
//! follows), the audience (its own DID, an `audience` origin it answers at ([`Audiences`]),
//! `issuedAt` within [`CLOCK_SKEW`] of its clock), the signature, the key (a device key of the
//! named log, valid at the log's trusted seq, without the bot mark, read at most [`LOGIN_MAX_AGE`] ago), then the role:
//!
//! - the identity is the operator record's: the operator role ([`KeyGrant::Operator`]);
//! - a local agent speaks for the identity (`identity_agents`, its own device key valid in the
//!   log at the trusted seq): a grant on that agent, the main agent first;
//! - otherwise `not-known`.
//!
//! A refusal answers one stable reason ([`KeyLoginError::code`]) and leaves no state but the
//! consumed challenge. A login mints a grant bound to the key for [`KEY_SESSION_TTL`];
//! `user.renewSession` moves its expiry on a fresh verdict, keeping its id. A key that stops
//! being valid ends its grants: at once on the cache's [`VerdictChange`], and on the re-check
//! every [`KEY_RECHECK_INTERVAL`], which also ends grants whose key could not be read for
//! [`STORE_OUTAGE_GRACE`] (fail closed).

use std::collections::{BTreeMap, HashMap};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use ad4m_identity_core::{did_of, signing_bytes, SESSION_DOMAIN};
use base64::Engine;
use rand::RngCore;

use super::cache::scid_from;
use super::resolver::{multikey_of, SessionKey, SessionKeyError};
use super::service::IdentityService;
use super::types::{
    EndedSessions, KeyPurpose, KeySessionResult, KeyStatement, LoginWithKeyParams, RenewedSession,
    SessionChallenge, SessionClient, SessionInfo, SessionRole, SignedKeyStatement,
    UnsignedKeyStatement,
};
use super::verdict::{KeyVerdict, VerdictChange};
use crate::agent::capabilities::{
    get_user_default_capabilities, grant_caps_for, AuthInfo, AGENT_AUTH_CAPABILITY,
    AGENT_UPDATE_CAPABILITY,
};
use crate::agent::grants::{self, Grants, KeyBinding, KeyGrant, LiveGrant};
use crate::agent::session::{Principal, Session};
use crate::agent::{did_for_context, AgentContext};
use crate::api::ws_handler::WsRpcError;

/// How long a key session's grant lives after its login or its last renewal.
pub const KEY_SESSION_TTL: u64 = 60 * 60;
/// How long a challenge stays open.
pub const CHALLENGE_TTL: u64 = 60;
/// The most challenges open at once for callers other than key sessions renewing (each of
/// those has its own slot).
pub const MAX_OPEN_CHALLENGES: usize = 10_000;
/// The most challenges one address holds open at once.
pub const CHALLENGES_PER_SOURCE: usize = 100;
/// How far a statement's `issuedAt` may be from the executor's clock.
pub const CLOCK_SKEW: u64 = 120;
/// The oldest read of a key's log a login takes; an older one goes to the store. A renewal
/// always reads the store.
pub const LOGIN_MAX_AGE: u64 = 5 * 60;
/// How often the keys of live key-bound grants are read again from the store: the longest a
/// revocation the store holds leaves a session open when the cache's change missed it. It
/// is also one re-check pass's deadline.
pub const KEY_RECHECK_INTERVAL: Duration = Duration::from_secs(5 * 60);
/// The most one store read of a re-check may take: longer counts as the outage it is, so a
/// stuck read cannot hold the interval's revocation window open. A pass fits
/// [`KEY_RECHECK_INTERVAL`] / [`KEY_READ_TIMEOUT`] sequential reads per slot.
pub const KEY_READ_TIMEOUT: Duration = Duration::from_secs(60);
/// The least store reads of one re-check that run at once; a pass widens it to
/// ⌈keys × [`KEY_READ_TIMEOUT`] / [`KEY_RECHECK_INTERVAL`]⌉ when more keys are live.
/// With every read stalled to its timeout, every key's read then still starts within the
/// pass's one deadline ([`KEY_RECHECK_INTERVAL`]), so a readable revoked key waits at most
/// an interval, not a pass per batch.
pub const KEY_RECHECK_READS_AT_ONCE: usize = 8;
/// How long a key-bound grant lasts on its last valid verdict while the store cannot be read:
/// a renewal proceeds, and the re-check keeps it, until then.
pub const STORE_OUTAGE_GRACE: u64 = 60 * 60;

/// Why a key login, a renewal or a statement signature was refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeyLoginError {
    /// The challenge is unknown, used or expired.
    Challenge,
    /// The statement names another executor (its DID, or an `audience` origin that does not
    /// name this executor), or its `issuedAt` is off the executor's clock.
    Audience,
    /// The statement's `audience` names an origin this executor does not accept key
    /// statements for: none of the configured `public_origins`, and without any set none of
    /// its own loopback origins. It restricts what the statement is signed for, not where
    /// the caller is.
    NoPublicOrigin,
    /// This executor accepts no key statement at all: it listens on the network (TLS, or
    /// `localhost` off) and `public_origins` is unset, so a remote caller could sign a
    /// loopback audience just as easily as any other and no origin is an accepted audience.
    /// The refusal names the setting that opens it.
    NoAudience,
    /// The signature does not hold under the statement's key.
    Signature,
    /// The key is not a valid device key of the named log at its trusted seq, or a bot's.
    Key,
    /// The identity has no role on this executor.
    NotKnown,
    /// The store did not answer, and no recent enough read of the log is cached.
    StoreUnavailable,
    /// A renewal names another grant or another key than the session's.
    Grant,
    /// Too many challenges are open.
    TooManyChallenges,
    /// The session may not do this: the message says what it lacks.
    Forbidden(String),
    /// The call does not fit the session (no key-bound grant, no identity to sign for) or the
    /// statement's shape: the message says which.
    BadRequest(String),
    Internal(String),
}

impl KeyLoginError {
    /// The stable reason a refusal answers.
    pub fn code(&self) -> &'static str {
        match self {
            KeyLoginError::Challenge => "challenge",
            KeyLoginError::Audience => "audience",
            KeyLoginError::NoPublicOrigin => "no-public-origin",
            KeyLoginError::NoAudience => "no-audience",
            KeyLoginError::Signature => "signature",
            KeyLoginError::Key => "key",
            KeyLoginError::NotKnown => "not-known",
            KeyLoginError::StoreUnavailable => "store-unavailable",
            KeyLoginError::Grant => "grant",
            KeyLoginError::TooManyChallenges => "too-many-challenges",
            KeyLoginError::Forbidden(_) => "forbidden",
            KeyLoginError::BadRequest(_) => "bad-request",
            KeyLoginError::Internal(_) => "internal",
        }
    }
}

impl From<KeyLoginError> for WsRpcError {
    fn from(e: KeyLoginError) -> Self {
        let code = e.code();
        match e {
            KeyLoginError::Challenge
            | KeyLoginError::Audience
            | KeyLoginError::Signature
            | KeyLoginError::Key
            | KeyLoginError::NotKnown
            | KeyLoginError::Grant => WsRpcError::forbidden(code),
            KeyLoginError::NoPublicOrigin => WsRpcError::forbidden(format!(
                "{code}: the statement's audience is no origin this executor takes key \
                 statements for (without public_origins, only its own loopback origins). \
                 Set public_origins (--public-origin) to the origin clients reach it at"
            )),
            KeyLoginError::NoAudience => WsRpcError::forbidden(format!(
                "{code}: key login on a network listener needs public_origins \
                 (--public-origin): the origins clients reach this executor at"
            )),
            KeyLoginError::StoreUnavailable => WsRpcError {
                code: 503,
                message: code.into(),
            },
            KeyLoginError::TooManyChallenges => WsRpcError::too_many_requests(code),
            KeyLoginError::Forbidden(m) => WsRpcError::forbidden(m),
            KeyLoginError::BadRequest(m) => WsRpcError::bad_request(m),
            KeyLoginError::Internal(m) => WsRpcError::internal(m),
        }
    }
}

/// Who a challenge is for, which decides the budget it draws from.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ChallengeSource {
    /// A key session renewing: its grant's one slot, outside the shared pool. A new challenge
    /// for the grant replaces the one still open.
    Grant(String),
    /// Any other caller, by the address it connects from ([`ChallengeSource::peer`]): at most
    /// [`CHALLENGES_PER_SOURCE`] open, all of them together at most [`MAX_OPEN_CHALLENGES`].
    /// `None` when the address is unknown (a server without connect info, in-process calls).
    Peer(Option<IpAddr>),
}

impl ChallengeSource {
    /// The source of a `user.sessionChallenge` call: the grant of a key session, else the
    /// address the caller connects from.
    pub fn of(session: &Session, peer: Option<IpAddr>) -> ChallengeSource {
        match session.grant() {
            Some(grant) if grant.key.is_some() => ChallengeSource::Grant(grant.id.0.clone()),
            _ => ChallengeSource::peer(peer),
        }
    }

    /// The budget of the address `peer`: every loopback address is one source (127.0.0.1), an
    /// IPv4-mapped IPv6 address is its IPv4 address, and an IPv6 address counts by its /64,
    /// the block one host gets.
    pub fn peer(peer: Option<IpAddr>) -> ChallengeSource {
        ChallengeSource::Peer(peer.map(|ip| {
            let ip = match ip {
                IpAddr::V6(v6) => v6.to_ipv4_mapped().map_or(IpAddr::V6(v6), IpAddr::V4),
                v4 => v4,
            };
            match ip {
                ip if ip.is_loopback() => IpAddr::V4(Ipv4Addr::LOCALHOST),
                IpAddr::V6(v6) => {
                    let s = v6.segments();
                    IpAddr::V6(Ipv6Addr::new(s[0], s[1], s[2], s[3], 0, 0, 0, 0))
                }
                v4 => v4,
            }
        }))
    }
}

struct OpenChallenge {
    expires: u64,
    source: ChallengeSource,
}

#[derive(Default)]
struct Open {
    challenges: HashMap<String, OpenChallenge>,
    /// How many challenges each address holds; their sum is the shared pool's use.
    per_peer: HashMap<Option<IpAddr>, usize>,
    /// The open challenge of each grant.
    slots: HashMap<String, String>,
}

impl Open {
    fn pool(&self) -> usize {
        self.challenges.len() - self.slots.len()
    }

    fn take(&mut self, challenge: &str) -> Option<u64> {
        let open = self.challenges.remove(challenge)?;
        match &open.source {
            ChallengeSource::Grant(grant) => {
                self.slots.remove(grant);
            }
            ChallengeSource::Peer(peer) => {
                if let Some(held) = self.per_peer.get_mut(peer) {
                    *held -= 1;
                    if *held == 0 {
                        self.per_peer.remove(peer);
                    }
                }
            }
        }
        Some(open.expires)
    }
}

/// The open challenges, each with its expiry (Unix seconds) and source. In memory only: a
/// restart drops them, and a challenge never outlives [`CHALLENGE_TTL`] anyway.
#[derive(Default)]
pub struct Challenges {
    open: Mutex<Open>,
}

impl Challenges {
    /// A new challenge for `source`, open until the expiry it answers with. Expired challenges
    /// go first. An address at its [`CHALLENGES_PER_SOURCE`], or a full pool, gets
    /// `TooManyChallenges`; a grant always gets one, and its previous one closes.
    pub fn issue(
        &self,
        source: &ChallengeSource,
        now: u64,
    ) -> Result<(String, u64), KeyLoginError> {
        let mut open = self
            .open
            .lock()
            .map_err(|_| KeyLoginError::Internal("the challenge store is poisoned".into()))?;
        let expired: Vec<String> = open
            .challenges
            .iter()
            .filter(|(_, c)| c.expires < now)
            .map(|(challenge, _)| challenge.clone())
            .collect();
        for challenge in expired {
            open.take(&challenge);
        }
        match source {
            ChallengeSource::Grant(grant) => {
                if let Some(previous) = open.slots.get(grant).cloned() {
                    open.take(&previous);
                }
            }
            ChallengeSource::Peer(peer) => {
                if open.pool() >= MAX_OPEN_CHALLENGES
                    || open.per_peer.get(peer).copied().unwrap_or(0) >= CHALLENGES_PER_SOURCE
                {
                    return Err(KeyLoginError::TooManyChallenges);
                }
            }
        }
        let mut bytes = [0u8; 32];
        rand::rngs::OsRng.fill_bytes(&mut bytes);
        let challenge = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes);
        let expires = now + CHALLENGE_TTL;
        match source {
            ChallengeSource::Grant(grant) => {
                open.slots.insert(grant.clone(), challenge.clone());
            }
            ChallengeSource::Peer(peer) => *open.per_peer.entry(*peer).or_default() += 1,
        }
        open.challenges.insert(
            challenge.clone(),
            OpenChallenge {
                expires,
                source: source.clone(),
            },
        );
        Ok((challenge, expires))
    }

    /// Uses `challenge` up: true when it was open and has not expired. It is gone afterwards,
    /// whatever the answer.
    pub fn consume(&self, challenge: &str, now: u64) -> bool {
        self.open
            .lock()
            .ok()
            .and_then(|mut open| open.take(challenge))
            .is_some_and(|expires| now <= expires)
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.open.lock().unwrap().challenges.len()
    }
}

/// The executor's DID: the audience of every statement it accepts.
pub fn executor_did() -> Result<String, KeyLoginError> {
    crate::agent::capabilities::token::issuer_did()
        .map_err(|e| KeyLoginError::Internal(e.to_string()))
}

/// Whether `session` may ask for a challenge: a key session may, whatever its capabilities
/// (its grant's key binding is the proof, and its challenge is its grant's own slot); every
/// other caller needs AGENT_AUTH, which the anonymous set holds too.
pub fn may_ask_challenge(session: &Session) -> bool {
    session.key_binding().is_some() || session.may(&AGENT_AUTH_CAPABILITY)
}

/// `user.sessionChallenge`, for `source` ([`ChallengeSource::of`]). Refused when no
/// statement's audience can pass ([`Audiences::None`]): a challenge could only be spent.
pub fn challenge(
    service: &IdentityService,
    source: &ChallengeSource,
    now: u64,
) -> Result<SessionChallenge, KeyLoginError> {
    service.audiences.admit()?;
    let executor = executor_did()?;
    let (challenge, expires_at) = service.challenges.issue(source, now)?;
    Ok(SessionChallenge {
        challenge,
        executor,
        expires_at,
    })
}

/// The shape every statement this executor takes has: version 1, a native client without PKCE,
/// a `did:scid:coasys:1` identity and an Ed25519 device key; a `grant` exactly on a renewal.
fn check_shape(
    v: u8,
    purpose: KeyPurpose,
    client: &SessionClient,
    code_challenge: Option<&String>,
    grant: Option<&String>,
    expected: KeyPurpose,
) -> Result<(), KeyLoginError> {
    let bad = |m: &str| Err(KeyLoginError::BadRequest(m.to_string()));
    if v != 1 {
        return bad("a key statement has v 1");
    }
    if purpose != expected {
        return match expected {
            KeyPurpose::Login => bad("user.loginWithKey takes a statement with purpose login"),
            KeyPurpose::Renew => bad("user.renewSession takes a statement with purpose renew"),
        };
    }
    match client {
        SessionClient::Native if code_challenge.is_some() => {
            return bad("a native statement carries no codeChallenge")
        }
        SessionClient::Native => {}
        SessionClient::Web { .. } => {
            return bad("a web login answers a one-time code, which this executor does not issue")
        }
    }
    match (expected, grant) {
        (KeyPurpose::Login, Some(_)) => bad("a login statement names no grant"),
        (KeyPurpose::Renew, None) => bad("a renew statement names the grant it renews"),
        _ => Ok(()),
    }
}

fn check_identity(statement: &KeyStatement) -> Result<(), KeyLoginError> {
    if ad4m_identity_core::scid_of(&statement.scid).is_none() {
        return Err(KeyLoginError::BadRequest(format!(
            "{} is not a did:scid:coasys:1 identity",
            statement.scid
        )));
    }
    if !ad4m_identity_core::is_ed25519_multikey(&statement.key) {
        return Err(KeyLoginError::BadRequest(format!(
            "{} is not an Ed25519 multikey",
            statement.key
        )));
    }
    Ok(())
}

fn check_audience(
    statement: &KeyStatement,
    executor: &str,
    audiences: &Audiences,
    now: u64,
) -> Result<(), KeyLoginError> {
    if statement.executor != executor || now.abs_diff(statement.issued_at) > CLOCK_SKEW {
        return Err(KeyLoginError::Audience);
    }
    audiences.check(&statement.audience)
}

/// `origin` as `scheme://host[:port]`: `ws`/`wss` read as `http`/`https`, the host lowercase
/// (IDNA), a default port left out, a trailing `/` dropped. Anything else (another scheme, a
/// path, a query, credentials) is not an origin.
pub fn normalise_origin(origin: &str) -> Result<String, String> {
    let not = |why: &str| format!("{origin} is not an http(s) origin: {why}");
    let mut url = url::Url::parse(origin.trim()).map_err(|e| not(&e.to_string()))?;
    let scheme = match url.scheme() {
        "http" | "ws" => "http",
        "https" | "wss" => "https",
        _ => return Err(not("the scheme is not http, https, ws or wss")),
    };
    if url.host().is_none() {
        return Err(not("no host"));
    }
    if !url.username().is_empty() || url.password().is_some() {
        return Err(not("it carries credentials"));
    }
    if url.path() != "/" || url.query().is_some() || url.fragment().is_some() {
        return Err(not("it has a path, a query or a fragment"));
    }
    // A default port of the old scheme is already gone; `ws` and `http` share theirs.
    url.set_scheme(scheme)
        .map_err(|_| not("the scheme cannot change"))?;
    Ok(url.origin().ascii_serialization())
}

/// Whether the normalised origin `origin` names this machine: `localhost` (or a name under
/// it), `127.0.0.0/8` or `::1`.
fn is_loopback_origin(origin: &str) -> bool {
    match url::Url::parse(origin)
        .ok()
        .and_then(|u| u.host().map(|h| h.to_owned()))
    {
        Some(url::Host::Domain(name)) => name == "localhost" || name.ends_with(".localhost"),
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        None => false,
    }
}

/// The origins a key statement's `audience` may name on this executor.
///
/// Only configuration decides them, never the request: the `Host` a connection sends is the
/// caller's to choose, and a server that relays a person's statement sends whichever one the
/// statement names. The app signs the origin it dialed, so a statement it signed for another
/// server's origin is refused here. On a loopback-only listener without `public_origins` the
/// executor's own loopback origins are the audiences; on a listener the network reaches,
/// none — accepting a loopback audience there would take a statement from anywhere, so key
/// login is refused until `public_origins` names origins.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Audiences {
    /// `public_origins`, normalised: these and no other.
    Public(Vec<String>),
    /// No `public_origins`: only this executor's own loopback origins on its API port
    /// ([`Audiences::loopback`]). That restricts the origin the statement is signed
    /// for, not where the caller is: a caller anywhere signs for a loopback origin
    /// just the same.
    Loopback(Vec<String>),
    /// No `public_origins` on a listener the network reaches (TLS, or `localhost`
    /// off): no origin at all. A remote caller signs a statement for a loopback
    /// origin just as easily as for any other, so accepting it accepts statements
    /// from anywhere. Key login is refused until `public_origins` names the origins
    /// clients reach this executor at.
    None,
}

impl Default for Audiences {
    /// No origin at all: an executor whose API port is unknown accepts no key statement.
    fn default() -> Self {
        Audiences::Loopback(vec![])
    }
}

impl Audiences {
    /// The configured `public_origins` when there are any, else the loopback origins of
    /// `api_port` — and none at all ([`Audiences::None`]) on a listener the network
    /// reaches (`network_listener`: TLS, or `localhost` off) without `public_origins`,
    /// since a remote caller signs a loopback audience just as easily. Fails on a
    /// configured value that is not an origin ([`normalise_origin`]).
    pub fn new(
        public_origins: &[String],
        api_port: Option<u16>,
        network_listener: bool,
    ) -> Result<Self, String> {
        if public_origins.is_empty() {
            return Ok(if network_listener {
                Audiences::None
            } else {
                api_port.map(Audiences::loopback).unwrap_or_default()
            });
        }
        public_origins
            .iter()
            .map(|origin| normalise_origin(origin).map_err(|e| format!("public_origins: {e}")))
            .collect::<Result<_, _>>()
            .map(Audiences::Public)
    }

    /// `http://127.0.0.1:<port>`, `http://localhost:<port>` and `http://[::1]:<port>`.
    pub fn loopback(port: u16) -> Self {
        Audiences::Loopback(
            ["127.0.0.1", "localhost", "[::1]"]
                .iter()
                .filter_map(|host| normalise_origin(&format!("http://{host}:{port}")).ok())
                .collect(),
        )
    }

    /// `Err` when the executor accepts no origin at all ([`Audiences::None`]), for
    /// `user.sessionChallenge` too: a challenge there could only be spent on a refusal.
    pub fn admit(&self) -> Result<(), KeyLoginError> {
        match self {
            Audiences::None => Err(KeyLoginError::NoAudience),
            _ => Ok(()),
        }
    }

    /// `Ok` when `audience` is one of these origins. Otherwise `Audience`; or, with no
    /// `public_origins` and an audience beyond this machine, `NoPublicOrigin`, which names the
    /// setting; or `NoAudience` when there is no accepted origin at all.
    pub fn check(&self, audience: &str) -> Result<(), KeyLoginError> {
        self.admit()?;
        let Ok(audience) = normalise_origin(audience) else {
            return Err(KeyLoginError::Audience);
        };
        match self {
            Audiences::Public(origins) | Audiences::Loopback(origins)
                if origins.contains(&audience) =>
            {
                Ok(())
            }
            Audiences::Loopback(_) if !is_loopback_origin(&audience) => {
                Err(KeyLoginError::NoPublicOrigin)
            }
            _ => Err(KeyLoginError::Audience),
        }
    }
}

/// Whether `proof.sig` is the statement key's signature over
/// `signing_bytes("coasys:session", statement)`.
pub fn signature_holds(proof: &SignedKeyStatement) -> bool {
    let Some(sig) = ad4m_hash_chain::decode_proof_value(&proof.sig) else {
        return false;
    };
    ad4m_identity_core::verify_signed(SESSION_DOMAIN, &proof.statement, &proof.statement.key, &sig)
}

/// The verdict of the statement's key in the statement's log, at most [`LOGIN_MAX_AGE`] old.
async fn admitted_key(
    service: &IdentityService,
    statement: &KeyStatement,
) -> Result<SessionKey, KeyLoginError> {
    let found = service
        .resolver
        .session_key(&statement.scid, &statement.key, LOGIN_MAX_AGE)
        .await
        .map_err(|e| match e {
            SessionKeyError::NotListed => KeyLoginError::Key,
            SessionKeyError::StoreUnavailable => KeyLoginError::StoreUnavailable,
        })?;
    if !found.admits() {
        return Err(KeyLoginError::Key);
    }
    Ok(found)
}

fn db<R>(
    f: impl FnOnce(&dyn crate::db_backend::DbBackend) -> crate::db::Ad4mDbResult<R>,
) -> Result<R, KeyLoginError> {
    f(crate::db_backend::db_backend()).map_err(|e| KeyLoginError::Internal(e.to_string()))
}

/// Who a key login of the identity `did` (whose key `key` the log admits) acts as here: the
/// operator role for the operator record's identity, else the local agent that speaks for it.
async fn role_for(
    service: &IdentityService,
    did: &str,
    key: &SessionKey,
) -> Result<Role, KeyLoginError> {
    if db(|db| db.operator_record())?.is_some_and(|record| record.scid == did) {
        return Ok(Role::Operator);
    }
    let mut agents = db(|db| db.agents_of_identity(&key.scid))?;
    let main = did_for_context(&AgentContext::main_agent()).ok();
    // The main agent first, then the managed agents in their order.
    agents.sort_by_key(|agent| Some(agent) != main.as_ref());
    for agent_did in agents {
        // The agent speaks for the identity only while its own device key is valid there.
        let Some(agent_key) = multikey_of(&agent_did) else {
            continue;
        };
        let speaks = service
            .resolver
            .cached_session_key(&key.scid, agent_key)
            .await
            .is_ok_and(|k| k.admits());
        if !speaks {
            continue;
        }
        return Ok(if Some(&agent_did) == main.as_ref() {
            Role::Agent(AgentContext::main_agent())
        } else {
            Role::Agent(AgentContext::managed(agent_did))
        });
    }
    Err(KeyLoginError::NotKnown)
}

enum Role {
    Operator,
    Agent(AgentContext),
}

/// `user.loginWithKey`: checks the statement (see the module doc) and mints a key-bound grant.
pub async fn login(
    service: &IdentityService,
    params: LoginWithKeyParams,
    now: u64,
) -> Result<KeySessionResult, KeyLoginError> {
    let LoginWithKeyParams { proof, app } = params;
    let statement = &proof.statement;
    if !service.challenges.consume(&statement.challenge, now) {
        return Err(KeyLoginError::Challenge);
    }
    check_shape(
        statement.v,
        statement.purpose,
        &statement.client,
        statement.code_challenge.as_ref(),
        statement.grant.as_ref(),
        KeyPurpose::Login,
    )?;
    check_identity(statement)?;
    let auth = AuthInfo::try_from(app).map_err(KeyLoginError::BadRequest)?;
    check_audience(statement, &executor_did()?, &service.audiences, now)?;
    if !signature_holds(&proof) {
        return Err(KeyLoginError::Signature);
    }
    let key = admitted_key(service, statement).await?;
    let role = role_for(service, &statement.scid, &key).await?;

    let (what, session_role) = match role {
        Role::Operator => (KeyGrant::Operator, SessionRole::Operator),
        Role::Agent(agent) => {
            let requested = auth
                .capabilities
                .clone()
                .unwrap_or_else(get_user_default_capabilities);
            let caps = grant_caps_for(&agent, &requested);
            let role = if agent.is_main_agent() {
                SessionRole::App
            } else {
                SessionRole::Tenant
            };
            (KeyGrant::Agent { agent, caps }, role)
        }
    };
    let binding = KeyBinding::new(
        statement.scid.clone(),
        statement.key.clone(),
        key.key_id.clone(),
        key.head_seq,
        now,
    );
    let (token, grant) = grants::mint_key_grant(what, auth, binding, KEY_SESSION_TTL)
        .map_err(KeyLoginError::Internal)?;
    // A login dropped after the mint — the caller cancelled, or its socket went
    // away — leaves no live grant without an answer.
    let grant = EndUnlessAnswered(Some(grant));
    // The grant is in the map now, so a revocation that lands after here ends it
    // (`Grants::revoke_key` on the change, else the re-check). One that landed between the
    // key's verdict and the insert is caught by one fresh read of the store; a store that
    // does not answer leaves the trusted verdict the cache already holds to decide — a
    // revocation it processed while the grant went in still ends the grant, and a cache
    // that cannot judge at all fails closed. The read is bounded: a stalled store counts
    // as the outage it is, not a login that hangs.
    let read = match tokio::time::timeout(
        KEY_READ_TIMEOUT,
        service
            .resolver
            .session_key(&statement.scid, &statement.key, 0),
    )
    .await
    {
        Ok(read) => read,
        Err(_) => Err(SessionKeyError::StoreUnavailable),
    };
    let admitted = !grant.is_ended()
        && match read {
            Ok(key) => key.admits(),
            Err(SessionKeyError::StoreUnavailable) => match service
                .resolver
                .cached_session_key(&statement.scid, &statement.key)
                .await
            {
                Ok(key) => key.admits(),
                Err(_) => {
                    return Err(KeyLoginError::StoreUnavailable);
                }
            },
            Err(SessionKeyError::NotListed) => false,
        };
    if !admitted {
        return Err(KeyLoginError::Key);
    }
    // The fresh read can also age the selected agent's own key out: it is read again
    // as `role_for` read it, or the grant would act as an agent that no longer speaks
    // for the identity. An operator-role grant names no agent.
    if !grant.is_operator() {
        let agent_did =
            did_for_context(&grant.agent).map_err(|e| KeyLoginError::Internal(e.to_string()))?;
        let speaks = match multikey_of(&agent_did) {
            Some(agent_key) => service
                .resolver
                .cached_session_key(&statement.scid, &agent_key)
                .await
                .is_ok_and(|k| k.admits()),
            None => false,
        };
        if !speaks {
            return Err(KeyLoginError::NotKnown);
        }
    }
    let grant = grant.keep();
    let did = did_for_context(&grant.agent).map_err(|e| KeyLoginError::Internal(e.to_string()))?;
    Ok(KeySessionResult {
        token,
        expires_at: grant.expires_at(),
        grant_id: grant.id.0.clone(),
        role: session_role,
        did,
        scid: statement.scid.clone(),
    })
}

/// `user.renewSession`: one more [`KEY_SESSION_TTL`] for the caller's key-bound grant, on a
/// `renew` statement of the grant's own key naming the grant. The key is read again: a key no
/// longer valid ends the grant. While the store cannot be read, a grant whose key was found
/// valid less than [`STORE_OUTAGE_GRACE`] ago renews on the cached log.
pub async fn renew(
    service: &IdentityService,
    session: &Session,
    proof: SignedKeyStatement,
    now: u64,
) -> Result<RenewedSession, KeyLoginError> {
    let grant = key_grant(session)?;
    let binding = grant.key.as_ref().ok_or_else(not_a_key_session)?;
    let statement = &proof.statement;
    if !service.challenges.consume(&statement.challenge, now) {
        return Err(KeyLoginError::Challenge);
    }
    check_shape(
        statement.v,
        statement.purpose,
        &statement.client,
        statement.code_challenge.as_ref(),
        statement.grant.as_ref(),
        KeyPurpose::Renew,
    )?;
    if statement.grant.as_deref() != Some(grant.id.0.as_str())
        || statement.key != binding.key
        || statement.scid != binding.scid
    {
        return Err(KeyLoginError::Grant);
    }
    check_audience(statement, &executor_did()?, &service.audiences, now)?;
    if !signature_holds(&proof) {
        return Err(KeyLoginError::Signature);
    }
    // The store is read whatever the cache's age: a failed refresh also marks the log as read
    // (backoff), and a renewal on that cached log would count as a check. On the cached log, the
    // key was not checked: the grace keeps running from the last real check, so a device
    // revoked on the store during the outage cannot renew past it.
    let (read, checked_at) = match service
        .resolver
        .session_key(&binding.scid, &binding.key, 0)
        .await
    {
        Err(SessionKeyError::StoreUnavailable)
            if now.saturating_sub(binding.checked_at()) < STORE_OUTAGE_GRACE =>
        {
            (
                service
                    .resolver
                    .cached_session_key(&binding.scid, &binding.key)
                    .await,
                binding.checked_at(),
            )
        }
        other => (other, now),
    };
    let key = match read {
        Ok(key) if key.admits() => key,
        Ok(_) | Err(SessionKeyError::NotListed) => {
            end(&grant);
            return Err(KeyLoginError::Key);
        }
        Err(SessionKeyError::StoreUnavailable) => return Err(KeyLoginError::StoreUnavailable),
    };
    let until = now + KEY_SESSION_TTL;
    let grant = Grants::global()
        .renew(&grant.id, until, key.head_seq, checked_at)
        .map_err(|e| match e {
            grants::GrantError::NotFound => KeyLoginError::Grant,
            grants::GrantError::Store(e) => KeyLoginError::Internal(e),
        })?;
    // The expiry is read once: a concurrent renewal may move it between the token's
    // signing and the answer, and the two must agree.
    let expires_at = grant.expires_at();
    let token = grants::renewed_token(&grant, expires_at).map_err(KeyLoginError::Internal)?;
    #[cfg(test)]
    renew_during(&grant);
    Ok(RenewedSession { token, expires_at })
}

/// Test seam for `renew`: a concurrent renewal that lands between the token's signing and
/// the answer, moving the grant's expiry on ([`RENEWAL_LATER`]).
#[cfg(test)]
fn renew_during(grant: &Arc<LiveGrant>) {
    use std::sync::atomic::Ordering;
    let later = RENEWAL_LATER.swap(0, Ordering::SeqCst);
    if later > 0 {
        let key = grant.key.as_ref().unwrap();
        Grants::global()
            .renew(&grant.id, later, key.head_seq(), key.checked_at())
            .unwrap();
    }
}

/// The expiry a concurrent renewal moves the grant to in [`renew_during`].
#[cfg(test)]
static RENEWAL_LATER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn not_a_key_session() -> KeyLoginError {
    KeyLoginError::BadRequest(
        "user.renewSession renews a session that a key login opened; this session is not one"
            .into(),
    )
}

/// `Ok` when a key login opened `session`: the one kind of session `user.renewSession` renews.
pub fn require_key_session(session: &Session) -> Result<(), KeyLoginError> {
    key_grant(session).map(|_| ())
}

fn key_grant(session: &Session) -> Result<Arc<LiveGrant>, KeyLoginError> {
    session
        .grant()
        .filter(|g| g.key.is_some())
        .cloned()
        .ok_or_else(not_a_key_session)
}

/// Ends `grant` on drop unless [`EndUnlessAnswered::keep`] takes it out: a login
/// abandoned after the mint — the caller cancelled, or its socket went away —
/// leaves no live grant without an answer.
struct EndUnlessAnswered(Option<Arc<LiveGrant>>);

impl EndUnlessAnswered {
    fn keep(mut self) -> Arc<LiveGrant> {
        self.0.take().expect("kept once")
    }
}

impl std::ops::Deref for EndUnlessAnswered {
    type Target = Arc<LiveGrant>;
    fn deref(&self) -> &Arc<LiveGrant> {
        self.0.as_ref().expect("the grant outlives the guard")
    }
}

impl Drop for EndUnlessAnswered {
    fn drop(&mut self) {
        if let Some(grant) = self.0.take() {
            end(&grant);
        }
    }
}

/// Revokes `grant`; a failed write is logged (the grant has ended in memory).
fn end(grant: &Arc<LiveGrant>) {
    if let Err(e) = Grants::global().revoke(std::slice::from_ref(grant)) {
        log::error!("key session {}: {e}", grant.id.0);
    }
}

/// `user.currentSession`: the caller's own session.
pub fn session_info(session: &Session) -> SessionInfo {
    let role = match &session.principal {
        Principal::Operator => SessionRole::Operator,
        Principal::Custody => SessionRole::Custody,
        Principal::Agent(AgentContext::Managed { .. }) => SessionRole::Tenant,
        Principal::Agent(AgentContext::Main) => SessionRole::App,
        Principal::Anonymous => SessionRole::Anonymous,
    };
    let binding = session.key_binding();
    SessionInfo {
        role,
        did: session.acts_as_did(),
        scid: binding.map(|b| b.scid.clone()),
        key_id: binding.map(|b| b.key_id.clone()),
        grant_id: session.grant_id().map(|id| id.0.clone()),
        expires_at: session.grant().map(|g| g.expires_at()),
        operator_source: session.operator_source().map(|s| s.as_str().to_string()),
    }
}

/// `user.endAllSessions`: ends every grant bound to a key of the caller's identity and, for a
/// session on a managed agent, every grant on that agent (password logins and approved apps
/// included). `keep_current` keeps the caller's own grant; `before` ends only grants made
/// before it.
pub fn end_all_sessions(
    session: &Session,
    keep_current: bool,
    before: Option<u64>,
) -> Result<EndedSessions, KeyLoginError> {
    let binding = session.key_binding();
    let managed = session
        .grant()
        .and_then(|_| session.managed_did())
        .map(|did| AgentContext::managed(did.to_string()));
    if binding.is_none() && managed.is_none() {
        return Err(KeyLoginError::BadRequest(
            "user.endAllSessions needs a session that a key login opened, or a session on a managed agent"
                .into(),
        ));
    }
    // Ending the agent's other grants (its logins, its approved apps) takes AGENT_UPDATE, as
    // `user.revokeAllSessions` does: an app the user approved with read capabilities only must
    // not log the user out everywhere. A key-bound session ends its identity's grants whatever
    // its capabilities: the device key is the proof.
    let managed = match managed {
        Some(_) if !session.may(&AGENT_UPDATE_CAPABILITY) && binding.is_none() => {
            return Err(KeyLoginError::Forbidden(
                "user.endAllSessions on a managed agent needs the AGENT UPDATE capability".into(),
            ));
        }
        Some(_) if !session.may(&AGENT_UPDATE_CAPABILITY) => None,
        other => other,
    };
    let keep = keep_current.then(|| session.grant_id()).flatten();
    let store = |e: grants::GrantError| KeyLoginError::Internal(e.to_string());
    let mut ended = 0;
    if let Some(binding) = binding {
        ended += Grants::global()
            .revoke_identity(&binding.scid, keep, before)
            .map_err(store)?;
    }
    if let Some(agent) = managed {
        ended += Grants::global()
            .revoke_agent_except(&agent, keep, before)
            .map_err(store)?;
    }
    Ok(EndedSessions {
        ended: u32::try_from(ended).unwrap_or(u32::MAX),
    })
}

/// `agent.signSessionStatement`: signs `unsigned` with the main agent's device key, as the main
/// agent's identity, under `coasys:session`; only a native statement without PKCE. Who may call
/// it is the method's: `.local_only()`, the operator on this machine (the shell), never an
/// operator from a key login (`Session::is_operator_on_this_machine`).
pub fn sign_statement(unsigned: UnsignedKeyStatement) -> Result<SignedKeyStatement, KeyLoginError> {
    check_shape(
        unsigned.v,
        unsigned.purpose,
        &unsigned.client,
        unsigned.code_challenge.as_ref(),
        unsigned.grant.as_ref(),
        unsigned.purpose,
    )?;
    normalise_origin(&unsigned.audience).map_err(KeyLoginError::BadRequest)?;
    let main = AgentContext::main_agent();
    let main_did = did_for_context(&main).map_err(|e| KeyLoginError::Internal(e.to_string()))?;
    let scid = db(|db| db.agent_identity(&main_did))?.ok_or_else(|| {
        KeyLoginError::BadRequest(
            "no-identity: this executor's device key speaks for no identity yet".into(),
        )
    })?;
    let key = multikey_of(&main_did)
        .ok_or_else(|| KeyLoginError::Internal(format!("{main_did} is not an Ed25519 did:key")))?
        .to_string();
    let statement = KeyStatement {
        v: unsigned.v,
        purpose: unsigned.purpose,
        scid: did_of(scid_from(&scid)),
        key,
        executor: unsigned.executor,
        audience: unsigned.audience,
        challenge: unsigned.challenge,
        client: unsigned.client,
        code_challenge: None,
        grant: unsigned.grant,
        issued_at: unsigned.issued_at,
    };
    let bytes = signing_bytes(SESSION_DOMAIN, &statement)
        .map_err(|e| KeyLoginError::Internal(e.to_string()))?;
    let sig = crate::agent::sign_for_context(&bytes, &main)
        .map_err(|e| KeyLoginError::Internal(e.to_string()))?;
    let sig: [u8; 64] = sig.as_slice().try_into().map_err(|_| {
        KeyLoginError::Internal(format!("the wallet gave a {}-byte signature", sig.len()))
    })?;
    Ok(SignedKeyStatement {
        statement,
        sig: ad4m_hash_chain::encode_proof_value(&sig),
    })
}

/// A verdict change for a key the cache learnt: a key no longer valid ends its grants at once.
/// Returns how many active grants ended.
pub fn on_verdict_change(change: &VerdictChange) -> usize {
    if change.verdict == KeyVerdict::Valid {
        return 0;
    }
    match Grants::global().revoke_key(&change.key) {
        Ok(ended) => {
            if ended > 0 {
                log::info!(
                    "identity: ended {ended} key session(s) of did:key:{}: {:?} in {}",
                    change.key,
                    change.verdict,
                    did_of(&change.scid)
                );
            }
            ended
        }
        Err(e) => {
            log::error!(
                "identity: ending the sessions of did:key:{}: {e}",
                change.key
            );
            0
        }
    }
}

/// When each key's store read in a re-check last began, by `(scid, key)`: each pass reads
/// the least-recently-attempted first — a read that stalls until its timeout moves its
/// key to the back — and sizes its reads-at-once to the key count
/// ([`KEY_RECHECK_READS_AT_ONCE`]), so every live key's read starts within the pass's one
/// deadline. Entries go when their keys do ([`RecheckAttempts::retain`]). In memory only —
/// it is the watcher's, and a restart asks every key again in its own order.
#[derive(Default)]
pub struct RecheckAttempts(Mutex<HashMap<(String, String), tokio::time::Instant>>);

impl RecheckAttempts {
    /// `key`'s last attempt; `None` — first in the pass's order — when no pass began a
    /// read for it yet.
    fn last(&self, scid: &str, key: &str) -> Option<tokio::time::Instant> {
        self.0
            .lock()
            .ok()
            .and_then(|last| last.get(&(scid.to_string(), key.to_string())).copied())
    }

    /// Marks `key`'s read as begun: an attempt, whatever it answers or however it stalls.
    fn mark(&self, scid: &str, key: &str) {
        if let Ok(mut last) = self.0.lock() {
            last.insert(
                (scid.to_string(), key.to_string()),
                tokio::time::Instant::now(),
            );
        }
    }

    /// Drops the attempts of keys `live` does not hold: a key whose grants are gone leaves
    /// the map, which is then bounded by the keys now live, not by every key a pass ever
    /// marked.
    fn retain(&self, live: &BTreeMap<(String, String), Vec<Arc<LiveGrant>>>) {
        if let Ok(mut last) = self.0.lock() {
            last.retain(|key, _| live.contains_key(key));
        }
    }
}

/// Reads the key of every live key-bound grant again from the store, whatever the cache's age
/// (a cached read up to an interval old would let a revocation the cache missed stand for two
/// intervals): a key no longer valid, or a bot's, ends its grants; a valid one records the
/// check. While the store cannot be read, grants whose last valid check is
/// [`STORE_OUTAGE_GRACE`] old end.
/// Returns how many active grants ended.
pub async fn recheck_key_grants(
    service: &IdentityService,
    now: u64,
    attempts: &RecheckAttempts,
) -> usize {
    let mut by_key: BTreeMap<(String, String), Vec<Arc<LiveGrant>>> = BTreeMap::new();
    for grant in Grants::global().key_bound() {
        if let Some(binding) = &grant.key {
            by_key
                .entry((binding.scid.clone(), binding.key.clone()))
                .or_default()
                .push(grant.clone());
        }
    }
    // The reads run together under one deadline for the pass — the interval —
    // least-recently-attempted first: a read that stalls until its timeout moves its key
    // to the back, holds its slot at most [`KEY_READ_TIMEOUT`], and frees it for the
    // next key then, so a slot starts [`KEY_RECHECK_INTERVAL`] / [`KEY_READ_TIMEOUT`]
    // reads in a pass. `reads_at_once` is sized to the key count so their product covers
    // every key — with every read stalled, every live key's read still starts within one
    // interval. A key the pass does not reach counts as unread, as a timed-out read does.
    use futures::StreamExt;
    attempts.retain(&by_key);
    let mut keys: Vec<(String, String)> = by_key.keys().cloned().collect();
    keys.sort_by_key(|(scid, key)| attempts.last(scid, key));
    let reads_at_once = KEY_RECHECK_READS_AT_ONCE.max(
        (keys.len() * KEY_READ_TIMEOUT.as_secs() as usize)
            .div_ceil(KEY_RECHECK_INTERVAL.as_secs() as usize),
    );
    let reads = futures::stream::iter(
        keys.into_iter()
            .map(|(scid, key)| {
                let resolver = service.resolver.clone();
                async move {
                    attempts.mark(&scid, &key);
                    let read = match tokio::time::timeout(
                        KEY_READ_TIMEOUT,
                        resolver.session_key(&scid, &key, 0),
                    )
                    .await
                    {
                        Ok(read) => read,
                        Err(_) => Err(SessionKeyError::StoreUnavailable),
                    };
                    ((scid, key), read)
                }
            })
            .collect::<Vec<_>>(),
    )
    .buffer_unordered(reads_at_once);
    let mut reads = Box::pin(reads);
    let mut results: HashMap<(String, String), Result<SessionKey, SessionKeyError>> =
        HashMap::new();
    let _ = tokio::time::timeout(KEY_RECHECK_INTERVAL, async {
        while let Some((key, read)) = reads.next().await {
            results.insert(key, read);
        }
    })
    .await;
    let mut ended = 0;
    let db = crate::db_backend::db_backend();
    for ((scid, key), grants) in by_key {
        // A read that did not answer in [`KEY_READ_TIMEOUT`], or that the pass did not
        // reach within it, counts as the outage it is: the grant's own last check
        // decides, bounded by [`STORE_OUTAGE_GRACE`].
        let read = results
            .remove(&(scid.clone(), key.clone()))
            .unwrap_or(Err(SessionKeyError::StoreUnavailable));
        let to_end: Vec<Arc<LiveGrant>> = match read {
            Ok(found) if found.admits() => {
                for grant in &grants {
                    if let Some(binding) = &grant.key {
                        binding.checked(found.head_seq, now);
                        // The check is kept in the row too, so a restart reads when the key
                        // was last seen valid, not when the grant started. The expiry does
                        // not move.
                        if let Err(e) = db.note_key_check(&grant.id.0, found.head_seq, now) {
                            log::warn!("identity: noting the check of {} failed: {e}", grant.id.0);
                        }
                    }
                }
                continue;
            }
            Ok(_) | Err(SessionKeyError::NotListed) => grants,
            Err(SessionKeyError::StoreUnavailable) => {
                match service.resolver.cached_session_key(&scid, &key).await {
                    // The cached verdict first: a revocation it already holds ends the
                    // grant now. A valid one is no fresh check — the outage grace still
                    // runs from the grant's own last check.
                    Ok(cached) if !cached.admits() => grants,
                    _ => grants
                        .into_iter()
                        .filter(|g| {
                            g.key.as_ref().is_some_and(|b| {
                                now.saturating_sub(b.checked_at()) >= STORE_OUTAGE_GRACE
                            })
                        })
                        .collect(),
                }
            }
        };
        if to_end.is_empty() {
            continue;
        }
        let active = to_end.iter().filter(|g| !g.is_ended()).count();
        match Grants::global().revoke(&to_end) {
            Ok(()) => {
                log::info!(
                    "identity: ended {active} key session(s) of did:key:{key} on the re-check"
                );
                ended += active;
            }
            Err(e) => log::error!("identity: ending the sessions of did:key:{key}: {e}"),
        }
    }
    ended
}

/// The one pass of live key-bound grants against the log the cache last trusted, run at
/// start after the grants load and before the API serves: `cached_session_key` reads no
/// store — it may be down. A key the cached log shows revoked, or does not list at its
/// trusted seq, ends its grants here — the revocation the cache persisted ends its
/// sessions even when the write that ended them never landed (a failed pending note and
/// a failed revocation), and before the watcher's first pass could run.
/// Returns how many active grants ended.
pub async fn end_key_grants_revoked_in_cache(service: &IdentityService, grants: &Grants) -> usize {
    let mut checked: HashMap<(String, String), bool> = HashMap::new();
    let mut to_end: Vec<Arc<LiveGrant>> = Vec::new();
    for grant in grants.key_bound() {
        let Some(binding) = &grant.key else {
            continue;
        };
        let admits = match checked.entry((binding.scid.clone(), binding.key.clone())) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => *entry.insert(
                service
                    .resolver
                    .cached_session_key(&binding.scid, &binding.key)
                    .await
                    .is_ok_and(|found| found.admits()),
            ),
        };
        if !admits {
            to_end.push(grant);
        }
    }
    if to_end.is_empty() {
        return 0;
    }
    let active = to_end.iter().filter(|g| !g.is_ended()).count();
    match grants.revoke(&to_end) {
        Ok(()) => {
            log::info!("identity: ended {active} key session(s) on the cached verdicts at start");
            active
        }
        Err(e) => {
            log::error!("identity: ending key sessions on the cached verdicts at start: {e}");
            0
        }
    }
}

/// Ends key sessions whose key stops being valid: on each verdict change the cache publishes,
/// and every [`KEY_RECHECK_INTERVAL`] for what a change cannot carry (a store that was down, a
/// key that gained the bot mark). Started by `identity::init`, which makes `changes` before it
/// spawns this: a change fired while the task waited to run — a replay of the identities at
/// start — is buffered on it, not lost.
pub async fn run_key_session_watch(
    service: Arc<IdentityService>,
    mut changes: tokio::sync::broadcast::Receiver<VerdictChange>,
) {
    use tokio::sync::broadcast::error::RecvError;
    let mut tick = tokio::time::interval(KEY_RECHECK_INTERVAL);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    // A re-check runs in its own task: however long its store reads take, a verdict change
    // is handled as it arrives. The first `due` is the interval's first tick, at once:
    // grants loaded at start are checked then.
    let mut rechecking: Option<tokio::task::JoinHandle<()>> = None;
    let mut due = true;
    let attempts = Arc::new(RecheckAttempts::default());
    loop {
        tokio::select! {
            change = changes.recv() => match change {
                Ok(change) => {
                    on_verdict_change(&change);
                }
                // Changes were missed: read every key again.
                Err(RecvError::Lagged(_)) => due = true,
                Err(RecvError::Closed) => return,
            },
            _ = tick.tick() => due = true,
        }
        if due && rechecking.as_ref().is_none_or(|r| r.is_finished()) {
            due = false;
            let (service, attempts) = (service.clone(), attempts.clone());
            rechecking = Some(tokio::spawn(async move {
                recheck_key_grants(&service, super::service::now(), &attempts).await;
            }));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::{AuthError, OperatorSource};
    use crate::identity::backend::IdentityBackend;
    use crate::identity::cache::tests::{
        accept_signed, on_file, store_key, trusting, FakeStore, TestLog,
    };
    use crate::identity::device::{enc_key_of, multikey_of as device_multikey};
    use crate::identity::resolver::Resolver;
    use crate::types::{AuthInfoInput, CapabilityInput, ResourceInput};
    use ad4m_identity_core::build::{add_unsigned, cosign, revoke, sign_domain, Signer};
    use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
    use ed25519_dalek::SigningKey;
    use std::sync::atomic::Ordering;

    /// The test-only rows (the operator record, cleared identities) are `Ad4mDb` methods
    /// outside `DbBackend`.
    fn test_db<R>(
        f: impl FnOnce(&crate::db::Ad4mDb) -> crate::db::Ad4mDbResult<R>,
    ) -> crate::db::Ad4mDbResult<R> {
        crate::db::Ad4mDb::with_global_instance(f)
    }

    /// A person's log on a store: master m0, d1 (`log.device`), a second device d2, and a bot
    /// key d3; the cache holds it under a head the store signed at its last event.
    struct World {
        service: IdentityService,
        store: Arc<FakeStore>,
        log: TestLog,
        phone: SigningKey,
        bot: SigningKey,
        did: String,
    }

    fn add_key(log: &mut TestLog, sign: String, enc: String, life: KeyLife) {
        let chain = log.chain();
        let device = DeviceKey {
            id: chain.state().next_device_id(),
            sign,
            enc,
            life,
        };
        let mut event = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
    }

    fn signing_key(seed: u8) -> SigningKey {
        SigningKey::from_bytes(&[seed; 32])
    }

    async fn world(seed: u8) -> World {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let mut log = TestLog::new(seed);
        let phone = signing_key(seed.wrapping_add(1));
        let bot = signing_key(seed.wrapping_add(2));
        let enc = |s: u8| ad4m_identity_core::encode_x25519_multikey(&[s; 32]);
        add_key(
            &mut log,
            phone.key(),
            enc(seed.wrapping_add(1)),
            KeyLife::Standing,
        );
        add_key(
            &mut log,
            bot.key(),
            enc(seed.wrapping_add(2)),
            KeyLife::Bot(BotScope {
                can: vec!["read".into()],
                expires: 4_000_000_000,
            }),
        );
        let did = did_of(&log.scid());
        let world = World {
            service: service(log.events.clone()),
            store: Arc::new(FakeStore::new(vec![])),
            log,
            phone,
            bot,
            did,
        };
        accept_signed(&world.service.cache, &world.log.events).await;
        world.with_store()
    }

    fn service(events: Vec<ad4m_identity_core::Event>) -> IdentityService {
        let store = Arc::new(FakeStore::new(events));
        let backend: Arc<dyn IdentityBackend> = store;
        let cache = Arc::new(trusting());
        IdentityService {
            resolver: Arc::new(Resolver::new(cache.clone(), backend.clone(), None, false)),
            backend,
            cache,
            store_url: None,
            challenges: Default::default(),
            inbox: Default::default(),
            audiences: Audiences::loopback(LOCAL_PORT),
            store_keys_source: crate::identity::StoreSource::Config,
        }
    }

    impl World {
        /// The world whose `store` is the service's backend, holding the world's log.
        fn with_store(mut self) -> World {
            let store = Arc::new(FakeStore::new(self.log.events.clone()));
            let backend: Arc<dyn IdentityBackend> = store.clone();
            self.service = IdentityService {
                resolver: Arc::new(Resolver::new(
                    self.service.cache.clone(),
                    backend.clone(),
                    None,
                    false,
                )),
                backend,
                cache: self.service.cache.clone(),
                store_url: None,
                challenges: Default::default(),
                inbox: Default::default(),
                audiences: Audiences::loopback(LOCAL_PORT),
                store_keys_source: crate::identity::StoreSource::Config,
            };
            self.store = store;
            self
        }

        fn challenge(&self) -> SessionChallenge {
            challenge(&self.service, &ChallengeSource::peer(None), now()).unwrap()
        }

        /// A login statement of `key` for this executor on a fresh challenge.
        fn statement(&self, key: &SigningKey) -> KeyStatement {
            let challenge = self.challenge();
            KeyStatement {
                v: 1,
                purpose: KeyPurpose::Login,
                scid: self.did.clone(),
                key: key.key(),
                executor: challenge.executor,
                audience: LOCAL_ORIGIN.into(),
                challenge: challenge.challenge,
                client: SessionClient::Native,
                code_challenge: None,
                grant: None,
                issued_at: now(),
            }
        }

        async fn login_as(&self, key: &SigningKey) -> Result<KeySessionResult, KeyLoginError> {
            let statement = self.statement(key);
            login(&self.service, params(signed(statement, key)), now()).await
        }

        /// Records the operator record as this world's identity.
        fn operate(&self) {
            test_db(|db| db.set_operator_record_for_test(Some(&self.did))).unwrap();
        }
    }

    /// The world whose key-log cache is the file at `path`: a restart re-opens the same
    /// logs and heads, as the executor's `ad4m_db.sqlite` does.
    async fn world_on_file(seed: u8, path: &std::path::Path) -> World {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let mut log = TestLog::new(seed);
        let phone = signing_key(seed.wrapping_add(1));
        let bot = signing_key(seed.wrapping_add(2));
        let enc = |s: u8| ad4m_identity_core::encode_x25519_multikey(&[s; 32]);
        add_key(
            &mut log,
            phone.key(),
            enc(seed.wrapping_add(1)),
            KeyLife::Standing,
        );
        add_key(
            &mut log,
            bot.key(),
            enc(seed.wrapping_add(2)),
            KeyLife::Bot(BotScope {
                can: vec!["read".into()],
                expires: 4_000_000_000,
            }),
        );
        let did = did_of(&log.scid());
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let backend: Arc<dyn IdentityBackend> = store.clone();
        let cache = Arc::new(on_file(path, vec![store_key().multikey()]));
        let service = IdentityService {
            resolver: Arc::new(Resolver::new(cache.clone(), backend.clone(), None, false)),
            backend,
            cache,
            store_url: None,
            challenges: Default::default(),
            audiences: Audiences::loopback(LOCAL_PORT),
            store_keys_source: crate::identity::StoreSource::Config,
            inbox: Default::default(),
        };
        let world = World {
            service,
            store,
            log,
            phone,
            bot,
            did,
        };
        accept_signed(&world.service.cache, &world.log.events).await;
        world
    }

    fn now() -> u64 {
        super::super::service::now()
    }

    fn signed(statement: KeyStatement, key: &SigningKey) -> SignedKeyStatement {
        let sig = sign_domain(SESSION_DOMAIN, &statement, key).unwrap();
        SignedKeyStatement {
            statement,
            sig: ad4m_hash_chain::encode_proof_value(&sig),
        }
    }

    /// The loopback origin the tests' local client dials, and how its connection arrives.
    const LOCAL_ORIGIN: &str = "http://127.0.0.1:12000";
    const LOCAL_PORT: u16 = 12000;

    fn app() -> AuthInfoInput {
        AuthInfoInput {
            app_name: "phone".into(),
            app_desc: "the phone app".into(),
            app_domain: "coasys.org".into(),
            capabilities: Some(vec![CapabilityInput {
                with: ResourceInput {
                    domain: "*".into(),
                    pointers: vec!["*".into()],
                },
                can: vec!["*".into()],
            }]),
            ..AuthInfoInput::default()
        }
    }

    fn params(proof: SignedKeyStatement) -> LoginWithKeyParams {
        LoginWithKeyParams { proof, app: app() }
    }

    fn open(token: &str) -> Result<Session, AuthError> {
        Session::open(
            token,
            &Some("the-admin-credential".into()),
            crate::agent::capabilities::ListenerReach::Network,
        )
    }

    fn no_operator() {
        test_db(|db| db.set_operator_record_for_test(None)).unwrap();
    }

    /// Binds the main agent to `scid` in the shared database until dropped.
    struct MainBound(String);

    impl MainBound {
        fn to(scid: &str) -> MainBound {
            let main = did_for_context(&AgentContext::main_agent()).unwrap();
            test_db(|db| db.clear_agent_identity_for_test(&main)).unwrap();
            db(|db| db.set_agent_identity(&main, scid, 1)).unwrap();
            MainBound(main)
        }
    }

    impl Drop for MainBound {
        fn drop(&mut self) {
            let _ = test_db(|db| db.clear_agent_identity_for_test(&self.0));
        }
    }

    /// The number of grants the executor holds bound to `key`.
    fn grants_of(key: &SigningKey) -> usize {
        let key = key.key();
        crate::db::Ad4mDb::with_global_instance(|db| db.grants())
            .unwrap()
            .into_iter()
            .filter(|row| row.key.as_ref().is_some_and(|k| k.signing_key == key))
            .count()
    }

    #[test]
    fn a_challenge_works_once_and_not_after_a_minute() {
        let challenges = Challenges::default();
        let here = ChallengeSource::peer(None);
        let (c, expires) = challenges.issue(&here, 1_000).unwrap();
        assert_eq!(expires, 1_000 + CHALLENGE_TTL);
        assert!(challenges.consume(&c, 1_010));
        assert!(!challenges.consume(&c, 1_010), "a second use");

        let (late, _) = challenges.issue(&here, 1_000).unwrap();
        assert!(
            !challenges.consume(&late, 1_000 + CHALLENGE_TTL + 1),
            "after 60 s"
        );
        assert!(!challenges.consume(&late, 1_000), "gone after the late use");
        assert!(!challenges.consume("never-issued", 1_000));

        let (a, _) = challenges.issue(&here, 1_000).unwrap();
        let (b, _) = challenges.issue(&here, 1_000).unwrap();
        assert_ne!(a, b);
        assert_eq!(
            base64::engine::general_purpose::URL_SAFE_NO_PAD
                .decode(&a)
                .unwrap()
                .len(),
            32
        );
    }

    #[test]
    fn at_most_ten_thousand_challenges_are_open_and_expired_ones_make_room() {
        let challenges = Challenges::default();
        for i in 0..MAX_OPEN_CHALLENGES {
            challenges
                .issue(&address(i / CHALLENGES_PER_SOURCE), 1_000)
                .unwrap();
        }
        assert_eq!(
            challenges
                .issue(&address(MAX_OPEN_CHALLENGES), 1_000)
                .unwrap_err(),
            KeyLoginError::TooManyChallenges
        );
        // A grant's slot is outside the pool.
        assert!(challenges
            .issue(&ChallengeSource::Grant("g".into()), 1_000)
            .is_ok());
        assert!(challenges
            .issue(&address(0), 1_000 + CHALLENGE_TTL + 1)
            .is_ok());
        assert_eq!(challenges.len(), 1);
    }

    /// A distinct public IPv4 address for each `n`.
    fn address(n: usize) -> ChallengeSource {
        ChallengeSource::peer(Some(IpAddr::V4(Ipv4Addr::from(0xc633_6400 + n as u32))))
    }

    // Expired challenges go on every issue, not only at the cap: an address's budget frees as
    // its challenges expire.
    #[test]
    fn expired_challenges_go_on_every_issue() {
        let challenges = Challenges::default();
        challenges.issue(&address(1), 1_000).unwrap();
        challenges.issue(&address(2), 1_000).unwrap();
        challenges
            .issue(&ChallengeSource::Grant("g".into()), 1_000)
            .unwrap();
        challenges
            .issue(&address(3), 1_000 + CHALLENGE_TTL + 1)
            .unwrap();
        assert_eq!(challenges.len(), 1);

        for _ in 0..CHALLENGES_PER_SOURCE {
            challenges.issue(&address(4), 2_000).unwrap();
        }
        assert_eq!(
            challenges.issue(&address(4), 2_000).unwrap_err(),
            KeyLoginError::TooManyChallenges
        );
        assert!(challenges
            .issue(&address(4), 2_000 + CHALLENGE_TTL + 1)
            .is_ok());
    }

    // Every loopback address is one source, an IPv4-mapped address is its IPv4 address, and an
    // IPv6 address counts by its /64. A key session's challenge comes from its grant's slot,
    // and a new one for the grant closes the one before.
    #[test]
    fn challenges_are_budgeted_by_address_and_by_grant() {
        let peer = |s: &str| ChallengeSource::peer(Some(s.parse().unwrap()));
        for local in ["127.0.0.2", "::1", "::ffff:127.0.0.1"] {
            assert_eq!(peer(local), peer("127.0.0.1"), "{local}");
        }
        assert_eq!(peer("::ffff:198.51.100.7"), peer("198.51.100.7"));
        assert_eq!(peer("2001:db8:1:2::1"), peer("2001:db8:1:2:ffff::9"));
        assert_ne!(peer("2001:db8:1:2::1"), peer("2001:db8:1:3::1"));
        assert_ne!(peer("198.51.100.7"), peer("198.51.100.8"));
        assert_ne!(peer("127.0.0.1"), ChallengeSource::peer(None));

        let challenges = Challenges::default();
        for _ in 0..CHALLENGES_PER_SOURCE {
            challenges.issue(&peer("127.0.0.1"), 1_000).unwrap();
        }
        assert_eq!(
            challenges.issue(&peer("::1"), 1_000).unwrap_err(),
            KeyLoginError::TooManyChallenges,
            "loopback is one source"
        );

        let grant = ChallengeSource::Grant("g".into());
        let (first, _) = challenges.issue(&grant, 1_000).unwrap();
        let (second, _) = challenges.issue(&grant, 1_000).unwrap();
        assert!(!challenges.consume(&first, 1_000), "replaced on reissue");
        assert!(challenges.consume(&second, 1_000));
        assert_eq!(challenges.len(), CHALLENGES_PER_SOURCE);

        let anonymous = Session::for_test(Principal::Anonymous);
        assert_eq!(
            ChallengeSource::of(&anonymous, Some("::1".parse().unwrap())),
            peer("127.0.0.1")
        );
    }

    // A key session's challenge is dispatched to its grant's own slot — the binding, not
    // its capabilities, decides — while every other session counts by its address.
    #[test]
    fn a_key_sessions_challenge_is_dispatched_to_its_grant() {
        let grant = crate::agent::grants::LiveGrant::for_test(
            "key-session",
            crate::agent::AgentContext::main_agent(),
            u64::MAX,
        )
        .bound_to(crate::agent::grants::KeyBinding::new(
            "did:scid:coasys:1:QmS".into(),
            "z6Mk".into(),
            "z6Mk#1".into(),
            1,
            0,
        ));
        let session = Session::for_test(Principal::Agent(AgentContext::main_agent()))
            .with_grant(std::sync::Arc::new(grant));
        assert!(may_ask_challenge(&session), "the binding is the proof");
        assert_eq!(
            ChallengeSource::of(&session, Some("127.0.0.1".parse().unwrap())),
            ChallengeSource::Grant("key-session".into())
        );
        let app = Session::for_test(Principal::Agent(AgentContext::main_agent()));
        assert_eq!(
            ChallengeSource::of(&app, Some("127.0.0.1".parse().unwrap())),
            ChallengeSource::peer(Some("127.0.0.1".parse().unwrap())),
            "a session with no key binding keeps the address's pool"
        );
    }

    // One address that asks for challenges and never uses them runs out of its own budget
    // only: a caller from another address still gets a challenge and logs in.
    #[tokio::test]
    async fn one_address_exhausting_its_budget_does_not_block_another_login() {
        let world = world(205).await;
        world.operate();
        let greedy = ChallengeSource::peer(Some("203.0.113.9".parse().unwrap()));
        for _ in 0..CHALLENGES_PER_SOURCE {
            challenge(&world.service, &greedy, now()).unwrap();
        }
        assert_eq!(
            challenge(&world.service, &greedy, now()).unwrap_err(),
            KeyLoginError::TooManyChallenges
        );

        let other = ChallengeSource::peer(Some("198.51.100.20".parse().unwrap()));
        let mut statement = world.statement(&world.phone);
        statement.challenge = challenge(&world.service, &other, now()).unwrap().challenge;
        let result = login(
            &world.service,
            params(signed(statement, &world.phone)),
            now(),
        )
        .await;
        assert!(result.is_ok(), "{result:?}");
        no_operator();
    }

    // A key session's renewal challenge comes from its grant's slot: a pool that anonymous
    // callers filled does not stop the renewal.
    #[tokio::test]
    async fn a_full_pool_does_not_stop_a_renewal() {
        let world = world(215).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // The statement's own challenge (from the pool) is replaced below.
        let mut statement = world.statement(&world.phone);
        statement.purpose = KeyPurpose::Renew;
        statement.grant = Some(result.grant_id.clone());
        for i in 0..MAX_OPEN_CHALLENGES - 1 {
            challenge(&world.service, &address(i / CHALLENGES_PER_SOURCE), now()).unwrap();
        }
        let from = Some("198.51.100.30".parse().unwrap());
        assert_eq!(
            challenge(&world.service, &ChallengeSource::peer(from), now()).unwrap_err(),
            KeyLoginError::TooManyChallenges,
            "the pool is full"
        );

        let source = ChallengeSource::of(&session, from);
        assert_eq!(source, ChallengeSource::Grant(result.grant_id.clone()));
        statement.challenge = challenge(&world.service, &source, now()).unwrap().challenge;
        let renewed = renew(
            &world.service,
            &session,
            signed(statement, &world.phone),
            now(),
        )
        .await;
        assert!(renewed.is_ok(), "{renewed:?}");
        no_operator();
    }

    // A device key of the operator record's identity gets the operator role: the token opens an
    // operator session within a grant bound to the key.
    #[tokio::test]
    async fn a_device_key_of_the_operators_identity_gets_the_operator_role() {
        let world = world(120).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        assert_eq!(result.role, SessionRole::Operator);
        assert_eq!(result.scid, world.did);
        assert_eq!(
            result.did,
            did_for_context(&AgentContext::main_agent()).unwrap()
        );
        assert!(result.expires_at.abs_diff(now() + KEY_SESSION_TTL) <= 2);

        let session = open(&result.token).unwrap();
        assert!(session.is_operator());
        assert_eq!(session.operator_source(), Some(OperatorSource::Key));
        assert!(session.may(&crate::agent::capabilities::ALL_CAPABILITY));
        let binding = session.key_binding().expect("a key-bound grant");
        assert_eq!(
            (
                binding.scid.as_str(),
                binding.key.as_str(),
                binding.key_id.as_str()
            ),
            (world.did.as_str(), world.phone.key().as_str(), "d2")
        );
        assert_eq!(binding.head_seq(), 2);
        assert_eq!(session.grant_id().unwrap().0, result.grant_id);

        let info = session_info(&session);
        assert_eq!(info.role, SessionRole::Operator);
        assert_eq!(info.operator_source.as_deref(), Some("key"));
        assert_eq!(info.key_id.as_deref(), Some("d2"));

        // The grant reads back with its role and key after a restart.
        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        let grant = reloaded
            .get(&crate::agent::session::TokenId(result.grant_id))
            .unwrap();
        assert!(grant.is_operator());
        assert_eq!(grant.key.as_ref().unwrap().key, world.phone.key());
        no_operator();
    }

    // A device key of an identity a managed agent speaks for gets that agent: a session on it
    // with a user's capabilities, never more.
    #[tokio::test]
    async fn a_device_key_of_a_bound_identity_gets_that_agent() {
        let mut world = world(130).await;
        no_operator();
        let user = crate::test_utils::create_test_user(&format!(
            "key-login-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let key = device_multikey(&user).unwrap();
        add_key(
            &mut world.log,
            key.clone(),
            enc_key_of(&key).unwrap(),
            KeyLife::Standing,
        );
        let world = world.with_store();
        accept_signed(&world.service.cache, &world.log.events).await;
        db(|db| db.set_agent_identity(&user, &world.log.scid(), 1)).unwrap();

        let result = world.login_as(&world.phone).await.unwrap();
        assert_eq!(result.role, SessionRole::Tenant);
        assert_eq!(result.did, user);
        let session = open(&result.token).unwrap();
        assert_eq!(
            session.principal,
            Principal::Agent(AgentContext::managed(user.clone()))
        );
        assert!(!session.is_operator());
        assert!(!session.may(&crate::agent::capabilities::ALL_CAPABILITY));
        assert!(session.may(&crate::agent::capabilities::AGENT_UPDATE_CAPABILITY));
        assert_eq!(session_info(&session).role, SessionRole::Tenant);
    }

    // The main agent speaking for the identity, without an operator record: a grant on the
    // main agent with the capabilities the app asked for.
    #[tokio::test]
    async fn a_device_key_of_the_main_agents_identity_gets_the_main_agent() {
        let mut world = world(135).await;
        no_operator();
        let main = did_for_context(&AgentContext::main_agent()).unwrap();
        let key = device_multikey(&main).unwrap();
        add_key(
            &mut world.log,
            key.clone(),
            enc_key_of(&key).unwrap(),
            KeyLife::Standing,
        );
        let world = world.with_store();
        accept_signed(&world.service.cache, &world.log.events).await;
        let _bound = MainBound::to(&world.log.scid());
        let result = world.login_as(&world.phone).await.unwrap();
        assert_eq!(result.role, SessionRole::App);
        let session = open(&result.token).unwrap();
        assert_eq!(
            session.principal,
            Principal::Agent(AgentContext::main_agent())
        );
        assert!(!session.is_operator());
    }

    // Each refusal has its stable reason and leaves no grant behind.
    #[tokio::test]
    async fn every_refused_login_answers_its_reason_and_leaves_no_grant() {
        let mut world = world(140).await;
        world.operate();
        let stranger = signing_key(141 + 100);

        // A key of an identity whose log does not list it.
        assert_eq!(
            world.login_as(&stranger).await.unwrap_err(),
            KeyLoginError::Key
        );
        // The master key: it signs the log, never for the person.
        let master = world.log.master.clone();
        assert_eq!(
            world.login_as(&master).await.unwrap_err(),
            KeyLoginError::Key
        );
        // A bot key.
        let bot = world.bot.clone();
        assert_eq!(world.login_as(&bot).await.unwrap_err(), KeyLoginError::Key);

        // A bad signature: signed by another key.
        let statement = world.statement(&world.phone);
        let forged = signed(statement, &stranger);
        assert_eq!(
            login(&world.service, params(forged), now())
                .await
                .unwrap_err(),
            KeyLoginError::Signature
        );
        // A statement for another executor, and one two minutes off the clock.
        let mut other = world.statement(&world.phone);
        other.executor = "did:key:z6MkpTHR8VNsBxYAAWHut2Geadd9jSwuBV8xRoAnwWsdvktH".into();
        assert_eq!(
            login(&world.service, params(signed(other, &world.phone)), now())
                .await
                .unwrap_err(),
            KeyLoginError::Audience
        );
        let mut late = world.statement(&world.phone);
        late.issued_at = now() - CLOCK_SKEW - 1;
        assert_eq!(
            login(&world.service, params(signed(late, &world.phone)), now())
                .await
                .unwrap_err(),
            KeyLoginError::Audience
        );
        // A replayed challenge, also after a failed first use.
        let statement = world.statement(&world.phone);
        let proof = signed(statement, &world.phone);
        let first = login(&world.service, params(proof.clone()), now()).await;
        assert!(first.is_ok(), "{first:?}");
        assert_eq!(
            login(&world.service, params(proof), now())
                .await
                .unwrap_err(),
            KeyLoginError::Challenge
        );
        let mut failed = world.statement(&world.phone);
        failed.executor = "did:key:another".into();
        let failed = signed(failed, &world.phone);
        let _ = login(&world.service, params(failed.clone()), now()).await;
        assert_eq!(
            login(&world.service, params(failed), now())
                .await
                .unwrap_err(),
            KeyLoginError::Challenge
        );
        // A web statement, and a native one with PKCE.
        let mut web = world.statement(&world.phone);
        web.client = SessionClient::Web {
            origin: "https://app.example".into(),
        };
        assert!(matches!(
            login(&world.service, params(signed(web, &world.phone)), now())
                .await
                .unwrap_err(),
            KeyLoginError::BadRequest(_)
        ));
        let mut pkce = world.statement(&world.phone);
        pkce.code_challenge = Some("abc".into());
        assert!(matches!(
            login(&world.service, params(signed(pkce, &world.phone)), now())
                .await
                .unwrap_err(),
            KeyLoginError::BadRequest(_)
        ));

        // A revoked key.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        let world = world.with_store();
        accept_signed(&world.service.cache, &world.log.events).await;
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::Key
        );

        // Only the one login above made a grant.
        assert_eq!(grants_of(&world.phone), 1);
        for (name, key) in [("stranger", &stranger), ("master", &master), ("bot", &bot)] {
            assert_eq!(grants_of(key), 0, "{name}");
        }
        no_operator();
    }

    // A key the log lists only past its store-signed head does not speak yet.
    #[tokio::test]
    async fn a_key_listed_only_past_the_trusted_seq_is_refused() {
        let mut world = world(150).await;
        world.operate();
        let laptop = signing_key(153);
        add_key(
            &mut world.log,
            laptop.key(),
            ad4m_identity_core::encode_x25519_multikey(&[153; 32]),
            KeyLife::Standing,
        );
        let scid = world.log.scid();
        world
            .service
            .cache
            .ingest(&scid, &world.log.events[3..])
            .await
            .unwrap();
        // The store answered the read just now: no refresh brings the store's newer head.
        world.service.cache.note_read(&scid, now());
        assert_eq!(
            world.login_as(&laptop).await.unwrap_err(),
            KeyLoginError::Key
        );
        assert_eq!(grants_of(&laptop), 0);
        no_operator();
    }

    // An identity with no role here, and a store that does not answer.
    #[tokio::test]
    async fn an_identity_without_a_role_is_not_known_and_a_store_outage_fails_closed() {
        let world = world(160).await;
        no_operator();
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::NotKnown
        );

        world.operate();
        world.store.store_down.store(true, Ordering::SeqCst);
        // The log was read when the world was made, but a login takes no read older than
        // five minutes: an older one goes to the store, which is down.
        world
            .service
            .cache
            .note_read(&world.log.scid(), now() - LOGIN_MAX_AGE - 1);
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::StoreUnavailable
        );
        let error: WsRpcError = KeyLoginError::StoreUnavailable.into();
        assert_eq!(
            (error.code, error.message.as_str()),
            (503, "store-unavailable")
        );
        assert_eq!(grants_of(&world.phone), 0);
        no_operator();
    }

    // A revocation the cache learns ends the key's sessions at once: the grant ends, the
    // sockets' token fires, `Session::over` completes, and the token opens no session again,
    // after a restart too.
    #[tokio::test]
    async fn revoking_the_key_ends_its_session() {
        let mut world = world(170).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let socket = session.ended();
        let over = session.over();

        let mut changes = world.service.cache.subscribe();
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&world.service.cache, &world.log.events).await;
        let mut ended = 0;
        while let Ok(change) = changes.try_recv() {
            ended += on_verdict_change(&change);
        }
        assert_eq!(ended, 1);
        assert!(socket.is_cancelled(), "the session's sockets close");
        tokio::time::timeout(Duration::from_secs(1), over)
            .await
            .expect("Session::over completes");
        assert_eq!(session.check(), Err(AuthError::Revoked));
        assert_eq!(open(&result.token).unwrap_err(), AuthError::Revoked);

        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        assert!(reloaded
            .get(&crate::agent::session::TokenId(result.grant_id))
            .is_none());
        no_operator();
    }

    // A revocation the persisted key log holds ends the grant at start even when no
    // grant-table write ever landed: the pending note and the revocation write both
    // failed, so the row reads active again — the cached verdict refuses the token
    // before the API serves it (the watcher's first pass may still be waiting, and the
    // store may be down).
    #[tokio::test]
    async fn a_revoked_keys_grant_is_refused_at_start_when_its_end_never_wrote() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m_db.sqlite");
        let mut world = world_on_file(133, &path).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        assert!(open(&result.token).is_ok());
        let grant_id = crate::agent::session::TokenId(result.grant_id.clone());

        // The key's revocation is accepted into the cache: persisted on the file a
        // restart re-opens.
        let mut changes = world.service.cache.subscribe();
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&world.service.cache, &world.log.events).await;
        let mut change = None;
        while let Ok(c) = changes.try_recv() {
            if c.key == world.phone.key() {
                change = Some(c);
            }
        }
        let change = change.expect("the revocation's verdict change");

        // Both writes of the end fail: no pending note lands and the row stays active,
        // so a restart forgets the in-memory end too.
        Grants::global().retry_pending();
        crate::db::FAIL_NEXT_GRANT_NOTE.store(true, Ordering::SeqCst);
        crate::db::FAIL_NEXT_GRANT_WRITE.store(true, Ordering::SeqCst);
        on_verdict_change(&change);
        assert_eq!(open(&result.token).unwrap_err(), AuthError::Revoked);

        // The restart: grants load from the table (the row still active), the cache
        // re-opens on the same file, the store is down and the watcher has not run.
        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        let restarted = Arc::new(on_file(&path, vec![store_key().multikey()]));
        let down = Arc::new(FakeStore::new(vec![]));
        down.store_down.store(true, Ordering::SeqCst);
        let backend: Arc<dyn IdentityBackend> = down;
        let service = IdentityService {
            resolver: Arc::new(Resolver::new(
                restarted.clone(),
                backend.clone(),
                None,
                false,
            )),
            backend,
            cache: restarted,
            store_url: None,
            challenges: Default::default(),
            audiences: Audiences::loopback(LOCAL_PORT),
            store_keys_source: crate::identity::StoreSource::Config,
            inbox: Default::default(),
        };
        assert!(
            reloaded.get(&grant_id).is_some(),
            "the row still reads active: no write ever landed"
        );
        end_key_grants_revoked_in_cache(&service, &reloaded).await;
        assert!(
            reloaded.get(&grant_id).is_none(),
            "the cached verdict ends it before the API serves"
        );
        no_operator();
    }

    // The re-check reads each key again from the store: a revocation the subscription missed
    // ends the session, however fresh the cache; a valid key records the check; a store outage ends only grants whose last
    // valid check is an hour old.
    #[tokio::test]
    async fn the_recheck_ends_the_sessions_of_keys_no_longer_valid() {
        let mut world = world(180).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let binding = session.key_binding().unwrap();

        // Valid: the check is recorded.
        let t = now();
        let attempts = RecheckAttempts::default();
        recheck_key_grants(&world.service, t + 10, &attempts).await;
        assert!(session.check().is_ok());
        assert_eq!(binding.checked_at(), t + 10);

        // The store is down: kept within the hour, ended after it.
        world.store.store_down.store(true, Ordering::SeqCst);
        recheck_key_grants(&world.service, t + 20, &attempts).await;
        assert!(session.check().is_ok(), "kept on the last valid verdict");
        recheck_key_grants(&world.service, t + 10 + STORE_OUTAGE_GRACE, &attempts).await;
        assert_eq!(session.check(), Err(AuthError::Revoked), "fail closed");

        // A login takes a read up to LOGIN_MAX_AGE old: with an older one, the outage fails it.
        world.service.cache.note_read(&world.log.scid(), 0);
        let again = world.login_as(&world.phone).await;
        assert_eq!(again.unwrap_err(), KeyLoginError::StoreUnavailable);
        world.store.store_down.store(false, Ordering::SeqCst);
        // The failed reads back off a few seconds per identity; the store is up again.
        world.service.resolver.forget_admission_failures();
        let fresh = world.login_as(&world.phone).await.unwrap();
        let fresh_session = open(&fresh.token).unwrap();
        // A revocation on the store, never published to this executor's subscribers, right
        // after that login read the log: the re-check reads the store all the same.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();
        assert_eq!(
            recheck_key_grants(&world.service, now(), &attempts).await,
            1
        );
        assert_eq!(fresh_session.check(), Err(AuthError::Revoked));
        no_operator();
    }

    // A failed admission read backs off per identity for a few seconds: the next login of
    // the same identity answers StoreUnavailable without asking the store again.
    #[tokio::test]
    async fn a_failed_admission_read_backs_off_per_identity() {
        let mut world = world(90).await;
        world.operate();
        world.service.cache.note_read(&world.log.scid(), 0);
        world.store.store_down.store(true, Ordering::SeqCst);
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::StoreUnavailable
        );
        let reads = world.store.log_calls.load(Ordering::SeqCst);
        assert!(reads > 0);
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::StoreUnavailable
        );
        assert_eq!(
            world.store.log_calls.load(Ordering::SeqCst),
            reads,
            "the backoff answers without another store read"
        );
        world.store.store_down.store(false, Ordering::SeqCst);
        world.service.resolver.forget_admission_failures();
        assert!(world.login_as(&world.phone).await.is_ok());
        no_operator();
    }

    // A renewal keeps the grant's id and moves its expiry, so a socket opened before it stays
    // open past the old expiry; a statement for another grant or by another key is refused; a
    // key no longer valid ends the grant instead.
    #[tokio::test]
    async fn a_renewal_keeps_the_grant_and_moves_its_expiry() {
        let mut world = world(190).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let renew_statement = |world: &World, key: &SigningKey, grant: &str| {
            let mut statement = world.statement(key);
            statement.purpose = KeyPurpose::Renew;
            statement.grant = Some(grant.to_string());
            statement
        };

        // Another grant, another key: refused.
        let other = renew_statement(&world, &world.phone, "another-grant");
        assert_eq!(
            renew(&world.service, &session, signed(other, &world.phone), now())
                .await
                .unwrap_err(),
            KeyLoginError::Grant
        );
        let by_d1 = renew_statement(&world, &world.log.device, &result.grant_id);
        assert_eq!(
            renew(
                &world.service,
                &session,
                signed(by_d1, &world.log.device),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::Grant
        );
        // A login statement is no renewal.
        let login_statement = world.statement(&world.phone);
        assert!(matches!(
            renew(
                &world.service,
                &session,
                signed(login_statement, &world.phone),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::BadRequest(_)
        ));

        // Renewed half an hour on: the expiry moves by the session's life from then.
        let later = now() + 30 * 60;
        let mut statement = renew_statement(&world, &world.phone, &result.grant_id);
        statement.challenge = world
            .service
            .challenges
            .issue(&ChallengeSource::Grant(result.grant_id.clone()), later)
            .unwrap()
            .0;
        statement.issued_at = later;
        let renewed = renew(
            &world.service,
            &session,
            signed(statement, &world.phone),
            later,
        )
        .await
        .unwrap();
        assert_eq!(renewed.expires_at, later + KEY_SESSION_TTL);
        let grant = session.grant().unwrap();
        assert_eq!(grant.expires_at(), later + KEY_SESSION_TTL);
        let claims = crate::agent::capabilities::decode_jwt(renewed.token.clone()).unwrap();
        assert_eq!(claims.nonce(), result.grant_id, "the same grant");
        assert!(open(&renewed.token).is_ok());
        // The socket opened before the renewal is still open past the old expiry.
        assert!(!grant.is_expired_at(result.expires_at + 600));
        let over = session.over();
        assert!(
            tokio::time::timeout(Duration::from_millis(100), over)
                .await
                .is_err(),
            "Session::over waits for the new expiry"
        );

        // The key is revoked: a renewal ends the grant.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        let statement = renew_statement(&world, &world.phone, &result.grant_id);
        assert_eq!(
            renew(
                &world.service,
                &session,
                signed(statement, &world.phone),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::Key
        );
        assert_eq!(session.check(), Err(AuthError::Revoked));
        no_operator();
    }

    // A session that no key login opened cannot renew.
    #[tokio::test]
    async fn only_a_key_session_renews() {
        let world = world(200).await;
        let statement = world.statement(&world.phone);
        let operator = Session::for_test(Principal::Operator);
        assert!(matches!(
            renew(
                &world.service,
                &operator,
                signed(statement, &world.phone),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::BadRequest(_)
        ));
    }

    // Log out everywhere ends every grant of the caller's identity; `keepCurrent` keeps the
    // caller's own, and `before` keeps grants made since.
    #[tokio::test]
    async fn ending_all_sessions_ends_the_identitys_grants() {
        let world = world(210).await;
        world.operate();
        let first = world.login_as(&world.phone).await.unwrap();
        let second = world.login_as(&world.log.device).await.unwrap();
        let caller = open(&first.token).unwrap();

        // Grants made at or after `before` stay.
        let created = caller.grant().unwrap().created_at;
        let kept = end_all_sessions(&caller, true, Some(created)).unwrap();
        assert_eq!(kept.ended, 0);
        assert!(open(&second.token).is_ok());

        let ended = end_all_sessions(&caller, true, None).unwrap();
        assert_eq!(ended.ended, 1);
        assert_eq!(open(&second.token).unwrap_err(), AuthError::Revoked);
        assert!(
            caller.check().is_ok(),
            "keepCurrent keeps the caller's grant"
        );
        assert_eq!(end_all_sessions(&caller, false, None).unwrap().ended, 1);
        assert_eq!(caller.check(), Err(AuthError::Revoked));

        // The admin credential has no identity to end.
        assert!(matches!(
            end_all_sessions(&Session::for_test(Principal::Operator), false, None),
            Err(KeyLoginError::BadRequest(_))
        ));
        no_operator();
    }

    // The executor's own device key signs a native statement for its identity under
    // `coasys:session`, for the operator on this machine only.
    #[tokio::test]
    async fn the_executor_signs_a_native_statement_for_the_operator_on_this_machine_only() {
        let world = world(220).await;
        let main = did_for_context(&AgentContext::main_agent()).unwrap();
        let unsigned = UnsignedKeyStatement {
            v: 1,
            purpose: KeyPurpose::Login,
            executor: "did:key:z6MkTarget".into(),
            audience: "WSS://Target.Example:443".into(),
            challenge: "c".into(),
            client: SessionClient::Native,
            code_challenge: None,
            grant: None,
            issued_at: 5,
        };
        test_db(|db| db.clear_agent_identity_for_test(&main)).unwrap();
        assert!(matches!(
            sign_statement(unsigned.clone()),
            Err(KeyLoginError::BadRequest(m)) if m.starts_with("no-identity")
        ));
        let _bound = MainBound::to(&world.log.scid());
        let signed = sign_statement(unsigned.clone()).unwrap();
        assert_eq!(signed.statement.scid, world.did);
        assert_eq!(signed.statement.key, multikey_of(&main).unwrap());
        assert!(signature_holds(&signed));
        assert_eq!(
            signed.statement.audience, "WSS://Target.Example:443",
            "signed as given"
        );
        let no_origin = UnsignedKeyStatement {
            audience: "target.example/api".into(),
            ..unsigned.clone()
        };
        assert!(matches!(
            sign_statement(no_origin),
            Err(KeyLoginError::BadRequest(_))
        ));

        let web = UnsignedKeyStatement {
            client: SessionClient::Web {
                origin: "https://app.example".into(),
            },
            ..unsigned.clone()
        };
        assert!(matches!(
            sign_statement(web),
            Err(KeyLoginError::BadRequest(_))
        ));
        let pkce = UnsignedKeyStatement {
            code_challenge: Some("x".into()),
            ..unsigned.clone()
        };
        assert!(matches!(
            sign_statement(pkce),
            Err(KeyLoginError::BadRequest(_))
        ));

        // The method is `.local_only()`: the operator on this machine signs; an operator from a
        // key login (even on a loopback listener, as through a forwarded port) and the admin
        // credential on a network listener get 403 local-only, an app the operator refusal.
        let mut map = crate::api::ws_handler::HandlerMap::new();
        crate::api::key_login_ws::register_ws_handlers(&mut map);
        let call = |session: Session| {
            map.dispatch(
                "agent.signSessionStatement",
                serde_json::json!({ "statement": unsigned.clone() }),
                Arc::new(crate::types::RequestContext::for_session(session)),
            )
        };
        let reply = call(Session::for_test(Principal::Operator)).await.unwrap();
        let signed: SignedKeyStatement = serde_json::from_value(reply).unwrap();
        assert!(signature_holds(&signed));

        world.operate();
        let token = world.login_as(&world.phone).await.unwrap().token;
        let key_operator = Session::open(
            &token,
            &Some("the-admin-credential".into()),
            crate::agent::capabilities::ListenerReach::Loopback,
        )
        .unwrap();
        assert_eq!(key_operator.operator_source(), Some(OperatorSource::Key));
        for refused in [
            key_operator,
            Session::for_test(Principal::Operator).on_network(),
        ] {
            let refused = call(refused).await.unwrap_err();
            assert_eq!(
                (refused.code, refused.message.as_str()),
                (403, crate::api::guards::LOCAL_ONLY)
            );
        }
        let app = Session::for_test(Principal::Agent(AgentContext::main_agent()));
        assert_eq!(call(app).await.unwrap_err().code, 403);
        no_operator();
    }

    #[test]
    fn the_anonymous_caller_has_the_anonymous_role() {
        let info = session_info(&Session::for_test(Principal::Anonymous));
        assert_eq!(info.role, SessionRole::Anonymous);
        assert_eq!(info.did, None);
        let operator = session_info(&Session::for_test(Principal::Operator));
        assert_eq!(operator.operator_source.as_deref(), Some("credential"));
        assert_eq!(operator.grant_id, None);
    }

    #[test]
    fn origins_are_normalised_and_anything_else_is_refused() {
        for (given, origin) in [
            ("https://ad4m.example.org", "https://ad4m.example.org"),
            ("https://AD4M.Example.org/", "https://ad4m.example.org"),
            ("wss://ad4m.example.org:443", "https://ad4m.example.org"),
            ("ws://127.0.0.1:12000", "http://127.0.0.1:12000"),
            ("http://localhost:80", "http://localhost"),
            (
                "https://ad4m.example.org:8443",
                "https://ad4m.example.org:8443",
            ),
            ("http://[::1]:12000", "http://[::1]:12000"),
        ] {
            assert_eq!(normalise_origin(given).as_deref(), Ok(origin), "{given}");
        }
        for bad in [
            "ad4m.example.org",
            "ftp://ad4m.example.org",
            "https://ad4m.example.org/api/v1/ws",
            "https://ad4m.example.org?x=1",
            "https://user:pw@ad4m.example.org",
            "",
        ] {
            assert!(normalise_origin(bad).is_err(), "{bad}");
        }
    }

    // Only configuration decides which audience names this executor: the configured
    // `public_origins`, and without them its own loopback origins on its API port —
    // on a loopback-only listener. On a listener the network reaches (TLS, or
    // `localhost` off) with no `public_origins` there is no accepted audience: a
    // remote caller signs a loopback audience just as easily, so key login is
    // refused outright until `public_origins` names origins.
    #[test]
    fn an_audience_is_a_configured_origin_or_else_an_own_loopback_origin() {
        let public =
            Audiences::new(&["https://A.example/".into()], Some(LOCAL_PORT), false).unwrap();
        assert_eq!(public, Audiences::Public(vec!["https://a.example".into()]));
        assert_eq!(public.check("https://a.example"), Ok(()));
        assert_eq!(public.check("wss://a.example:443"), Ok(()));
        for refused in [
            "https://e.example",
            "https://a.example:8443",
            "http://a.example",
            LOCAL_ORIGIN,
            "not an origin",
        ] {
            assert_eq!(
                public.check(refused),
                Err(KeyLoginError::Audience),
                "{refused}"
            );
        }
        assert!(Audiences::new(&["a.example".into()], None, false)
            .unwrap_err()
            .starts_with("public_origins: "));

        let loopback = Audiences::new(&[], Some(LOCAL_PORT), false).unwrap();
        assert_eq!(loopback, Audiences::loopback(LOCAL_PORT));
        for own in [
            LOCAL_ORIGIN,
            "ws://127.0.0.1:12000",
            "http://localhost:12000",
            "http://[::1]:12000",
        ] {
            assert_eq!(loopback.check(own), Ok(()), "{own}");
        }
        for (refused, why) in [
            ("https://e.example", KeyLoginError::NoPublicOrigin),
            ("http://192.168.1.2:12000", KeyLoginError::NoPublicOrigin),
            ("http://127.0.0.1:12001", KeyLoginError::Audience),
            ("https://127.0.0.1:12000", KeyLoginError::Audience),
            ("http://localhost:12001", KeyLoginError::Audience),
        ] {
            assert_eq!(loopback.check(refused), Err(why), "{refused}");
        }
        assert_eq!(
            Audiences::default().check(LOCAL_ORIGIN),
            Err(KeyLoginError::Audience),
            "no API port, no origin"
        );
        let message = WsRpcError::from(KeyLoginError::NoPublicOrigin).message;
        assert!(message.starts_with("no-public-origin: "), "{message}");
        assert!(
            message.contains("public_origins (--public-origin)"),
            "{message}"
        );

        // On a network listener (TLS, or `localhost` off) without `public_origins` no
        // origin is accepted — a loopback one least of all, since a remote caller
        // signs it just as easily. `no-audience` names the setting that opens it.
        let none = Audiences::new(&[], Some(LOCAL_PORT), true).unwrap();
        assert_eq!(none, Audiences::None);
        for refused in [LOCAL_ORIGIN, "https://a.example", "not an origin"] {
            assert_eq!(
                none.check(refused),
                Err(KeyLoginError::NoAudience),
                "{refused}"
            );
        }
        let message = WsRpcError::from(KeyLoginError::NoAudience).message;
        assert!(message.starts_with("no-audience: "), "{message}");
        assert!(message.contains("public_origins"), "{message}");

        // With `public_origins` they alone are the audiences, on a network listener too.
        let public = Audiences::new(&["https://a.example".into()], Some(LOCAL_PORT), true).unwrap();
        assert_eq!(public.check("https://a.example"), Ok(()));
        assert_eq!(public.check(LOCAL_ORIGIN), Err(KeyLoginError::Audience));
    }

    // The relay: a server E the person added takes a fresh challenge from the person's own
    // executor A and hands A's challenge and A's DID to the person's app, which signs. The app
    // names the origin it dialed, E's, so A refuses the statement and the challenge is used up.
    // A statement for A's own origin logs in.
    #[tokio::test]
    async fn a_statement_for_another_origin_is_refused_so_a_relay_gets_no_session() {
        let mut world = world(225).await;
        world.operate();
        world.service.audiences =
            Audiences::new(&["https://a.example".into()], None, true).unwrap();
        let for_origin = |world: &World, audience: &str| {
            let mut statement = world.statement(&world.phone);
            statement.audience = audience.into();
            statement
        };

        for audience in ["https://e.example", "wss://e.example", LOCAL_ORIGIN] {
            let statement = for_origin(&world, audience);
            let proof = signed(statement.clone(), &world.phone);
            assert!(signature_holds(&proof), "the person's key signed it");
            assert_eq!(
                login(&world.service, params(proof), now())
                    .await
                    .unwrap_err(),
                KeyLoginError::Audience,
                "{audience}"
            );
            let mut retry = statement;
            retry.audience = "https://a.example".into();
            assert_eq!(
                login(&world.service, params(signed(retry, &world.phone)), now())
                    .await
                    .unwrap_err(),
                KeyLoginError::Challenge,
                "the challenge is used up"
            );
        }
        assert_eq!(grants_of(&world.phone), 0, "no session from a relay");

        let session = login(
            &world.service,
            params(signed(
                for_origin(&world, "https://a.example"),
                &world.phone,
            )),
            now(),
        )
        .await
        .unwrap();
        assert_eq!(session.role, SessionRole::Operator);

        // Without public_origins, a loopback-only A takes key logins only on its own
        // loopback origins.
        world.service.audiences = Audiences::new(&[], Some(LOCAL_PORT), false).unwrap();
        assert_eq!(
            login(
                &world.service,
                params(signed(
                    for_origin(&world, "https://e.example"),
                    &world.phone
                )),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::NoPublicOrigin
        );
        assert!(world.login_as(&world.phone).await.is_ok());
        no_operator();
    }

    // The same for a renewal: E relays a renew statement the app signed for E's origin.
    #[tokio::test]
    async fn a_renewal_for_another_origin_is_refused() {
        let mut world = world(245).await;
        world.operate();
        world.service.audiences =
            Audiences::new(&["https://a.example".into()], None, true).unwrap();
        let mut first = world.statement(&world.phone);
        first.audience = "https://a.example".into();
        let result = login(&world.service, params(signed(first, &world.phone)), now())
            .await
            .unwrap();
        let session = open(&result.token).unwrap();
        let renewal = |audience: &str| {
            let mut statement = world.statement(&world.phone);
            statement.purpose = KeyPurpose::Renew;
            statement.grant = Some(result.grant_id.clone());
            statement.audience = audience.into();
            statement
        };

        let relayed = renewal("https://e.example");
        assert_eq!(
            renew(
                &world.service,
                &session,
                signed(relayed.clone(), &world.phone),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::Audience
        );
        let mut retry = relayed;
        retry.audience = "https://a.example".into();
        assert_eq!(
            renew(&world.service, &session, signed(retry, &world.phone), now())
                .await
                .unwrap_err(),
            KeyLoginError::Challenge,
            "the challenge is used up"
        );
        assert!(
            session.check().is_ok(),
            "a refused renewal leaves the grant"
        );

        let renewed = renew(
            &world.service,
            &session,
            signed(renewal("https://a.example"), &world.phone),
            now(),
        )
        .await
        .unwrap();
        assert!(open(&renewed.token).is_ok());
        no_operator();
    }

    // On a listener the network reaches without `public_origins` there is no accepted
    // audience at all: `user.sessionChallenge` refuses already, so no challenge is
    // spent on a refusal, and `user.loginWithKey` and `user.renewSession` answer the
    // same `no-audience`, which names the setting that opens key login there.
    #[tokio::test]
    async fn on_a_network_listener_without_public_origins_key_login_is_refused() {
        let mut world = world(85).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let login_statement = world.statement(&world.phone);
        let mut renewal = world.statement(&world.phone);
        renewal.purpose = KeyPurpose::Renew;
        renewal.grant = Some(result.grant_id.clone());

        // The executor answers on the network (TLS, or `localhost` off) with no
        // `public_origins`: `Audiences::new` gives none.
        world.service.audiences = Audiences::new(&[], Some(LOCAL_PORT), true).unwrap();

        for source in [
            ChallengeSource::peer(None),
            ChallengeSource::Grant(result.grant_id.clone()),
        ] {
            assert_eq!(
                challenge(&world.service, &source, now()).unwrap_err(),
                KeyLoginError::NoAudience,
                "{source:?}"
            );
        }
        assert_eq!(
            login(
                &world.service,
                params(signed(login_statement, &world.phone)),
                now(),
            )
            .await
            .unwrap_err(),
            KeyLoginError::NoAudience
        );
        assert_eq!(
            renew(
                &world.service,
                &session,
                signed(renewal, &world.phone),
                now()
            )
            .await
            .unwrap_err(),
            KeyLoginError::NoAudience
        );
        assert!(session.check().is_ok(), "the grant stays until its hour");
        no_operator();
    }

    // While the store is down, a renewal proceeds on the cached log only for STORE_OUTAGE_GRACE
    // after the last REAL check. The fallback renewal is no check: it must not move
    // `checked_at`, or a device revoked on the store during the outage renews for ever.
    #[tokio::test]
    async fn an_outage_renewal_does_not_extend_the_outage_grace() {
        let mut world = world(230).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let binding = session.key_binding().unwrap();
        let checked = binding.checked_at();
        let renewal = |world: &World, at: u64| {
            let mut statement = world.statement(&world.phone);
            statement.purpose = KeyPurpose::Renew;
            statement.grant = Some(result.grant_id.clone());
            statement.challenge = world
                .service
                .challenges
                .issue(&ChallengeSource::Grant(result.grant_id.clone()), at)
                .unwrap()
                .0;
            statement.issued_at = at;
            signed(statement, &world.phone)
        };

        // The store goes down right after the login; the cached log is what there is.
        world.store.store_down.store(true, Ordering::SeqCst);

        // Fifty minutes on: renewed on the cached log, within the grace of the login's check.
        let first = checked + 50 * 60;
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        renew(&world.service, &session, renewal(&world, first), first)
            .await
            .unwrap();
        assert_eq!(
            binding.checked_at(),
            checked,
            "a renewal on the cached log is no check of the key"
        );

        // A hundred minutes on: the last real check is past the grace, so a renewal fails closed.
        let second = checked + 100 * 60;
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        assert_eq!(
            renew(&world.service, &session, renewal(&world, second), second)
                .await
                .unwrap_err(),
            KeyLoginError::StoreUnavailable,
            "the outage grace is measured from the last real check, not the last renewal"
        );
        assert_eq!(binding.checked_at(), checked);
        world.store.store_down.store(false, Ordering::SeqCst);
        no_operator();
    }

    // A failed refresh marks the log as read (backoff), so a renewal that took a recent read
    // from the cache would renew on the stale log as if the store had answered, and move
    // `checked_at` on through the outage. A renewal reads the store whatever the cache's age.
    #[tokio::test]
    async fn a_renewal_after_a_failed_refresh_is_no_check_of_the_key() {
        let mut world = world(250).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let binding = session.key_binding().unwrap();
        let checked = binding.checked_at();
        let renewal = |world: &World, at: u64| {
            let mut statement = world.statement(&world.phone);
            statement.purpose = KeyPurpose::Renew;
            statement.grant = Some(result.grant_id.clone());
            statement.challenge = world
                .service
                .challenges
                .issue(&ChallengeSource::Grant(result.grant_id.clone()), at)
                .unwrap()
                .0;
            statement.issued_at = at;
            signed(statement, &world.phone)
        };

        world.store.store_down.store(true, Ordering::SeqCst);
        for minutes in [20, 40, 59] {
            let at = checked + minutes * 60;
            // A read that just failed (a link check, say) noted the log as refreshed.
            world.service.cache.note_refreshed(&world.log.scid(), now());
            renew(&world.service, &session, renewal(&world, at), at)
                .await
                .unwrap();
            assert_eq!(
                binding.checked_at(),
                checked,
                "a renewal while the store is down is no check, after {minutes} minutes"
            );
        }
        let past = checked + STORE_OUTAGE_GRACE + 60;
        world.service.cache.note_refreshed(&world.log.scid(), now());
        assert_eq!(
            renew(&world.service, &session, renewal(&world, past), past)
                .await
                .unwrap_err(),
            KeyLoginError::StoreUnavailable
        );
        world.store.store_down.store(false, Ordering::SeqCst);
        no_operator();
    }

    // A session on a managed agent ends the agent's other grants only with AGENT_UPDATE, as
    // `user.revokeAllSessions` requires: an app the user approved with read capabilities only
    // must not log the user out everywhere.
    #[tokio::test]
    async fn a_read_only_app_cannot_end_its_owners_sessions() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let user = AgentContext::managed("did:key:z6MkReadOnlyOwner".into());
        let auth = |name: &str, caps: Vec<crate::agent::capabilities::Capability>| AuthInfo {
            app_name: name.into(),
            capabilities: Some(caps),
            ..AuthInfo::default()
        };
        // The sessions as `Session::open` builds them from a managed agent's grant: its agent,
        // its grant and its capabilities (the wallet holds no key for this test agent).
        let session_on = |grant: Arc<LiveGrant>| {
            Session::for_test(Principal::Agent(user.clone()))
                .with_caps(grant.caps.clone())
                .with_grant(grant)
        };
        let (_, login_grant) = grants::mint(
            user.clone(),
            auth("login", get_user_default_capabilities()),
            None,
            3600,
        )
        .unwrap();
        let (_, app_grant) = grants::mint(
            user.clone(),
            auth(
                "reader",
                vec![crate::agent::capabilities::AGENT_READ_CAPABILITY.clone()],
            ),
            None,
            3600,
        )
        .unwrap();
        let login_session = session_on(login_grant);
        let app = session_on(app_grant);
        assert!(!app.may(&crate::agent::capabilities::AGENT_UPDATE_CAPABILITY));

        // The read-only app: refused, and the owner's login stands.
        assert!(
            matches!(
                end_all_sessions(&app, true, None),
                Err(KeyLoginError::Forbidden(_))
            ),
            "a read-only app must not end the owner's sessions"
        );
        assert!(login_session.check().is_ok(), "the owner's login stands");

        // The owner's login (AGENT_UPDATE) ends the app's grant.
        assert_eq!(
            end_all_sessions(&login_session, true, None).unwrap().ended,
            1
        );
        assert_eq!(app.check(), Err(AuthError::Revoked));
        assert!(login_session.check().is_ok());
    }

    // A revocation the store learns between the login's verdict and the grant's insert must
    // not leave a live grant: the insert checks the key against the store again, and it ends
    // there.
    #[tokio::test]
    async fn a_revocation_between_the_verdict_and_the_grant_ends_it() {
        let mut world = world(255).await;
        world.operate();
        // A fresh read admits the key: the login's `session_key` serves it from the cache.
        let key = world
            .service
            .resolver
            .session_key(&world.log.scid(), &world.phone.key(), LOGIN_MAX_AGE)
            .await
            .unwrap();
        assert!(key.admits());
        // The store learns the revocation; the cache does not (the change is on its way).
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::Key
        );
        // No live grant bound to the revoked key escaped the revocation.
        assert!(Grants::global()
            .key_bound()
            .iter()
            .all(|g| g.key.as_ref().is_none_or(|b| b.key != world.phone.key())));
        no_operator();
    }

    // A revocation the cache learns between the login's verdict and the grant's insert ends
    // the grant even when the post-mint store read cannot answer: the trusted cached verdict
    // decides then, not the login's stale one.
    #[tokio::test]
    async fn a_revocation_the_cache_knows_ends_the_grant_when_the_store_read_fails() {
        use tokio::sync::Notify;
        let mut world = world(45).await;
        world.operate();
        // A fresh read admits the key: the login's `session_key` serves it from the cache.
        let key = world
            .service
            .resolver
            .session_key(&world.log.scid(), &world.phone.key(), LOGIN_MAX_AGE)
            .await
            .unwrap();
        assert!(key.admits());
        let statement = world.statement(&world.phone);
        // The post-mint read pauses inside the store: between the verdict and the read the
        // cache learns the revocation, and the store stops answering.
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *world.store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let service = Arc::new(world.service);
        let attempt = {
            let service = service.clone();
            let proof = signed(statement, &world.phone);
            tokio::spawn(async move { login(&service, params(proof), now()).await })
        };
        entered.notified().await;
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&service.cache, &world.log.events).await;
        world.store.store_down.store(true, Ordering::SeqCst);
        release.notify_one();

        assert_eq!(attempt.await.unwrap().unwrap_err(), KeyLoginError::Key);
        assert!(Grants::global()
            .key_bound()
            .iter()
            .all(|g| g.key.as_ref().is_none_or(|b| b.key != world.phone.key())));
        no_operator();
    }

    // A failed refresh marks the log as read (backoff): a link check's attempt. A session
    // admission counts only reads the store answered, or that attempt would admit a revoked
    // device from a stale head.
    #[tokio::test]
    async fn a_failed_refresh_attempt_is_no_successful_read_for_a_login() {
        let world = world(235).await;
        world.operate();
        world.store.store_down.store(true, Ordering::SeqCst);
        let _ = world
            .service
            .cache
            .refresh(world.service.backend.as_ref(), &world.did)
            .await;
        world.service.cache.note_refreshed(&world.log.scid(), now());
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::StoreUnavailable
        );
        world.store.store_down.store(false, Ordering::SeqCst);
        no_operator();
    }

    // A re-check's store read is bounded: a `log` call that never answers counts as the
    // outage it is, so the re-check ends and the grant is kept on its fresh check.
    #[tokio::test(start_paused = true)]
    async fn a_stuck_store_read_is_counted_as_the_outage_it_is() {
        use tokio::sync::Notify;
        let world = world(238).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *world.store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let service = Arc::new(world.service);
        let recheck = tokio::spawn(async move {
            recheck_key_grants(&service, now(), &RecheckAttempts::default()).await;
        });
        entered.notified().await;
        tokio::time::advance(KEY_READ_TIMEOUT + Duration::from_secs(1)).await;
        recheck.await.unwrap();
        assert!(session.check().is_ok(), "kept on its fresh check");
        release.notify_one();
        no_operator();
    }

    // A store answer behind the cached head is no fresh read of it: the head stays
    // unconfirmed, and no session is admitted on it.
    #[tokio::test]
    async fn a_store_behind_the_cached_head_is_no_fresh_read_of_it() {
        let world = world(95).await;
        world.operate();
        let (scid, key) = (world.log.scid(), world.phone.key());
        let key_state = world
            .service
            .resolver
            .session_key(&scid, &key, LOGIN_MAX_AGE)
            .await
            .unwrap();
        assert!(key_state.admits());
        // The store falls behind the cached head: it serves the log's older prefix now.
        let shorter = world.log.events.len() - 1;
        *world.store.events.lock().unwrap() = world.log.events[..shorter].to_vec();
        world.service.cache.note_read(&scid, 0);
        assert_eq!(
            world
                .service
                .resolver
                .session_key(&scid, &key, 0)
                .await
                .unwrap_err(),
            SessionKeyError::StoreUnavailable
        );
        // And a behind answer is no fresh read of the head: a later admission read does
        // not take the stale head as confirmed by the store.
        assert_eq!(
            world
                .service
                .resolver
                .session_key(&scid, &key, LOGIN_MAX_AGE)
                .await
                .unwrap_err(),
            SessionKeyError::StoreUnavailable,
            "a behind answer is no fresh read: the head stays unconfirmed"
        );
        no_operator();
    }

    // A re-check whose store read fails consults the cached verdict first: a revocation the
    // cache already holds ends the grant now — the outage grace does not hold it.
    #[tokio::test]
    async fn a_recheck_ends_the_grant_on_a_cached_revocation() {
        let mut world = world(105).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // The cache's trusted log learns the revocation; the store cannot be read.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&world.service.cache, &world.log.events).await;
        world.store.store_down.store(true, Ordering::SeqCst);
        world.service.cache.note_read(&world.log.scid(), 0);
        recheck_key_grants(&world.service, now(), &RecheckAttempts::default()).await;
        assert_eq!(
            session.check(),
            Err(AuthError::Revoked),
            "the cached verdict ends it now"
        );
        no_operator();
    }

    // The fresh read the post-mint check takes can age the selected agent's own key out too:
    // the login is refused, and no grant acts as an agent that no longer speaks for the
    // identity.
    #[tokio::test]
    async fn an_agents_revocation_seen_at_the_post_mint_read_refuses_the_login() {
        let mut world = world(115).await;
        no_operator();
        let main = did_for_context(&AgentContext::main_agent()).unwrap();
        let main_key = device_multikey(&main).unwrap();
        let agent_key_id = world.log.chain().state().next_device_id();
        add_key(
            &mut world.log,
            main_key,
            enc_key_of(&device_multikey(&main).unwrap()).unwrap(),
            KeyLife::Standing,
        );
        let mut world = world.with_store();
        accept_signed(&world.service.cache, &world.log.events).await;
        let _bound = MainBound::to(&world.log.scid());
        // A fresh read admits the phone's key and the agent's: the login's `session_key`
        // serves them from the cache.
        assert!(world
            .service
            .resolver
            .session_key(&world.log.scid(), &world.phone.key(), LOGIN_MAX_AGE)
            .await
            .unwrap()
            .admits());
        // The store learns the agent's revocation; the cache does not.
        let revoke_agent = revoke(&world.log.chain(), &world.log.master, &[agent_key_id]).unwrap();
        world.log.events.push(revoke_agent);
        *world.store.events.lock().unwrap() = world.log.events.clone();
        assert_eq!(
            world.login_as(&world.phone).await.unwrap_err(),
            KeyLoginError::NotKnown
        );
        assert_eq!(grants_of(&world.phone), 1, "only the ended mint's row");
        assert!(Grants::global()
            .key_bound()
            .iter()
            .all(|g| g.key.as_ref().is_none_or(|b| b.key != world.phone.key())));
        no_operator();
    }

    // A login whose caller goes away while its post-mint read stalls leaves no live grant:
    // the dropped future ends the minted grant.
    #[tokio::test(start_paused = true)]
    async fn a_login_dropped_after_the_mint_leaves_no_live_grant() {
        use tokio::sync::Notify;
        let world = world(125).await;
        world.operate();
        // The admission read is fresh, so the login reaches the mint and stalls at the
        // post-mint read.
        assert!(world
            .service
            .resolver
            .session_key(&world.log.scid(), &world.phone.key(), LOGIN_MAX_AGE)
            .await
            .unwrap()
            .admits());
        let statement = world.statement(&world.phone);
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *world.store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let service = Arc::new(world.service);
        let attempt = {
            let service = service.clone();
            let proof = signed(statement, &world.phone);
            tokio::spawn(async move { login(&service, params(proof), now()).await })
        };
        entered.notified().await;
        attempt.abort();
        assert!(attempt.await.is_err(), "the caller went away");
        assert!(Grants::global()
            .key_bound()
            .iter()
            .all(|g| g.key.as_ref().is_none_or(|b| b.key != world.phone.key())));
        release.notify_one();
        no_operator();
    }

    // A change the cache fired before the watcher's task first ran still reaches it: the
    // watcher is handed a subscription made before the replay it watches for (`init`
    // subscribes, `load_local_identities` replays, then the task runs).
    #[tokio::test]
    async fn a_change_fired_before_the_watch_started_still_ends_the_session() {
        let mut world = world(75).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // Subscribed before the change fired, as `init` subscribes before it spawns.
        let changes = world.service.cache.subscribe();
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&world.service.cache, &world.log.events).await;
        // The store cannot confirm it: only the change the watcher buffered ends the
        // session now (the re-check keeps the grant on its fresh check).
        world.store.store_down.store(true, Ordering::SeqCst);
        let service = Arc::new(world.service);
        let watch = tokio::spawn(run_key_session_watch(service.clone(), changes));
        for _ in 0..10 {
            tokio::task::yield_now().await;
            if session.check().is_err() {
                break;
            }
        }
        assert_eq!(session.check(), Err(AuthError::Revoked));
        watch.abort();
        no_operator();
    }

    // A re-check parked on a store read does not hold back a verdict change: the watcher
    // ends the session while the read still waits.
    #[tokio::test(start_paused = true)]
    async fn a_verdict_change_during_a_stuck_recheck_still_ends_the_session() {
        use tokio::sync::Notify;
        let mut world = world(110).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *world.store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let service = Arc::new(world.service);
        let watch = tokio::spawn(run_key_session_watch(
            service.clone(),
            service.cache.subscribe(),
        ));
        entered.notified().await;
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        accept_signed(&service.cache, &world.log.events).await;
        for _ in 0..10 {
            tokio::task::yield_now().await;
            if session.check().is_err() {
                break;
            }
        }
        assert_eq!(session.check(), Err(AuthError::Revoked));
        release.notify_one();
        watch.abort();
        no_operator();
    }

    // Stalled reads ahead of a key in the re-check's order must not hold it back: the keys
    // are read together under one bound, so a key whose revocation notice was missed is
    // still ended within the read timeout, not after every stalled read's own.
    #[tokio::test(start_paused = true)]
    async fn stalled_reads_do_not_hold_back_another_keys_recheck() {
        use tokio::sync::Notify;
        let mut world = world(65).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // Two key-bound grants whose store reads stall, sorted ahead of the session's key.
        let entered: Vec<Arc<Notify>> = ["0-stall-a", "0-stall-b"]
            .iter()
            .map(|scid| {
                let notify = Arc::new(Notify::new());
                world
                    .store
                    .stall_for
                    .lock()
                    .unwrap()
                    .insert(format!("did:scid:coasys:1:{scid}"), notify.clone());
                Grants::global()
                    .insert(
                        LiveGrant::for_test(
                            &format!("stalled-{scid}"),
                            AgentContext::main_agent(),
                            now() + 3600,
                        )
                        .bound_to(KeyBinding::new(
                            scid.to_string(),
                            format!("z6Mk{scid}"),
                            "d1".into(),
                            0,
                            now(),
                        )),
                    )
                    .unwrap();
                notify
            })
            .collect();
        // The session's key is revoked on the store; no change ever reached this executor.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();

        let service = Arc::new(world.service);
        let started = tokio::time::Instant::now();
        let recheck = tokio::spawn(async move {
            recheck_key_grants(&service, now(), &RecheckAttempts::default()).await
        });
        for notify in &entered {
            notify.notified().await;
        }
        tokio::time::advance(KEY_READ_TIMEOUT + Duration::from_secs(1)).await;
        let ended = recheck.await.unwrap();
        assert!(
            started.elapsed() <= KEY_READ_TIMEOUT + Duration::from_secs(5),
            "the pass took {:?}",
            started.elapsed()
        );
        assert_eq!(ended, 1);
        assert_eq!(session.check(), Err(AuthError::Revoked));
        // The stalled grants kept their fresh check.
        for scid in ["0-stall-a", "0-stall-b"] {
            assert!(
                Grants::global()
                    .get(&crate::agent::session::TokenId(format!("stalled-{scid}")))
                    .is_some(),
                "{scid} is kept within the outage grace"
            );
        }
        world.store.stall_for.lock().unwrap().clear();
        no_operator();
    }

    // The reads that stall until their own timeout free their slots for the rest of the
    // pass: under the pass's one deadline — the interval — a slot starts a read per
    // KEY_READ_TIMEOUT, and the reads-at-once is sized to the key count, so a key starved
    // behind them is read in the same pass, not a later one.
    #[tokio::test(start_paused = true)]
    async fn stalled_reads_hold_the_starved_key_back_only_a_wave() {
        use tokio::sync::Notify;
        let mut world = world(64).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // Two full batches of key-bound grants whose store reads stall, all sorted ahead
        // of the session's key: the first batch holds the slots until the read timeout,
        // the second fills them for the next wave — then the session's key is read.
        for i in 0..2 * KEY_RECHECK_READS_AT_ONCE {
            let scid = format!("0-stall-{i}");
            world
                .store
                .stall_for
                .lock()
                .unwrap()
                .insert(format!("did:scid:coasys:1:{scid}"), Arc::new(Notify::new()));
            Grants::global()
                .insert(
                    LiveGrant::for_test(
                        &format!("starved-stall-{i}"),
                        AgentContext::main_agent(),
                        now() + 3600,
                    )
                    .bound_to(KeyBinding::new(
                        scid.clone(),
                        format!("z6Mk{scid}"),
                        "d1".into(),
                        0,
                        now(),
                    )),
                )
                .unwrap();
        }
        // The session's key is revoked on the store; no verdict change reached this
        // executor.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();

        let service = Arc::new(world.service);
        let attempts = Arc::new(RecheckAttempts::default());
        let recheck = tokio::spawn({
            let (service, attempts) = (service.clone(), attempts.clone());
            async move { recheck_key_grants(&service, now(), &attempts).await }
        });
        // Two stalled waves, then the starved key's read: the pass is done well inside
        // the interval.
        tokio::time::advance(3 * KEY_READ_TIMEOUT + Duration::from_secs(5)).await;
        recheck.await.unwrap();
        assert_eq!(
            session.check(),
            Err(AuthError::Revoked),
            "reached in the same pass"
        );
        for i in 0..2 * KEY_RECHECK_READS_AT_ONCE {
            assert!(
                attempts
                    .last(&format!("0-stall-{i}"), &format!("z6Mk0-stall-{i}"))
                    .is_some(),
                "key 0-stall-{i} was attempted"
            );
        }
        world.store.stall_for.lock().unwrap().clear();
        no_operator();
    }

    // So many stalled reads ahead of a revoked key that a pass reading only
    // [`KEY_RECHECK_READS_AT_ONCE`] at a time would need passes — whole intervals — to
    // reach it. The pass sizes its reads-at-once to the key count instead, so the revoked
    // key's read starts within one interval and its session ends within it and one read
    // timeout of the watch starting.
    #[tokio::test(start_paused = true)]
    async fn a_revoked_key_behind_many_stalled_reads_is_ended_within_the_interval() {
        use tokio::sync::Notify;
        let mut world = world(66).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        // Enough key-bound grants that their stalled reads outnumber four full batches:
        // the pass marks the batch it reads and the batch the deadline frees the slots
        // for, so the revoked key — sorted last ('0' sorts before 'did:scid:…') — waited
        // passes, one whole interval each.
        let entered: Vec<Arc<Notify>> = (0..4 * KEY_RECHECK_READS_AT_ONCE + 1)
            .map(|i| {
                let scid = format!("0-crowd-{i}");
                let notify = Arc::new(Notify::new());
                world
                    .store
                    .stall_for
                    .lock()
                    .unwrap()
                    .insert(format!("did:scid:coasys:1:{scid}"), notify.clone());
                Grants::global()
                    .insert(
                        LiveGrant::for_test(
                            &format!("crowd-stall-{i}"),
                            AgentContext::main_agent(),
                            now() + 3600,
                        )
                        .bound_to(KeyBinding::new(
                            scid.clone(),
                            format!("z6Mk{scid}"),
                            "d1".into(),
                            0,
                            now(),
                        )),
                    )
                    .unwrap();
                notify
            })
            .collect();
        // The session's key is revoked on the store; no verdict change reached this
        // executor.
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();

        let service = Arc::new(world.service);
        let started = tokio::time::Instant::now();
        let watch = tokio::spawn(run_key_session_watch(
            service.clone(),
            service.cache.subscribe(),
        ));
        entered[0].notified().await;
        while session.check().is_ok()
            && started.elapsed() <= KEY_RECHECK_INTERVAL + KEY_READ_TIMEOUT
        {
            tokio::time::advance(Duration::from_secs(10)).await;
            tokio::task::yield_now().await;
        }
        assert!(
            started.elapsed() <= KEY_RECHECK_INTERVAL + KEY_READ_TIMEOUT,
            "the revoked key's read started within the interval; the session ended after {:?}",
            started.elapsed()
        );
        assert_eq!(session.check(), Err(AuthError::Revoked));
        watch.abort();
        world.store.stall_for.lock().unwrap().clear();
        no_operator();
    }

    // A key whose grants are gone leaves the re-check's attempts: the next pass retains
    // only the keys of the live snapshot, so the map is bounded by the live keys, not by
    // every key a pass ever marked.
    #[tokio::test]
    async fn a_key_whose_grants_are_gone_leaves_the_rechecks_attempts() {
        let world = world(67).await;
        let attempts = RecheckAttempts::default();
        let grant = Grants::global()
            .insert(
                LiveGrant::for_test("gone-key-grant", AgentContext::main_agent(), now() + 3600)
                    .bound_to(KeyBinding::new(
                        "0-0gone".into(),
                        "z6Mk0-0gone".into(),
                        "d1".into(),
                        0,
                        now(),
                    )),
            )
            .unwrap();
        recheck_key_grants(&world.service, now(), &attempts).await;
        assert!(
            attempts.last("0-0gone", "z6Mk0-0gone").is_some(),
            "the pass marked the key's read"
        );
        Grants::global().revoke(&[grant]).unwrap();
        recheck_key_grants(&world.service, now(), &attempts).await;
        assert_eq!(attempts.last("0-0gone", "z6Mk0-0gone"), None);
    }

    // A successful periodic check is persisted: the row's `checked_at` moves to the check's
    // time, and the grant's expiry does not.
    #[tokio::test]
    async fn a_recheck_persists_the_check_time() {
        let world = world(111).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let t = now() + 60;
        recheck_key_grants(&world.service, t, &RecheckAttempts::default()).await;
        let row = crate::db::Ad4mDb::with_global_instance(|db| db.grants())
            .unwrap()
            .into_iter()
            .find(|r| r.id == result.grant_id)
            .unwrap();
        let key = row.key.unwrap();
        assert_eq!(key.checked_at, t);
        assert_eq!(
            row.expires_at, result.expires_at,
            "the check moves no expiry"
        );
        no_operator();
    }

    // A renewed token carries the same expiry the answer does: the grant's own.
    #[tokio::test]
    async fn a_renewed_token_expires_at_the_answered_expiry() {
        let world = world(112).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let at = now() + 30;
        let mut statement = world.statement(&world.phone);
        statement.purpose = KeyPurpose::Renew;
        statement.grant = Some(result.grant_id.clone());
        statement.challenge = world
            .service
            .challenges
            .issue(&ChallengeSource::Grant(result.grant_id.clone()), at)
            .unwrap()
            .0;
        statement.issued_at = at;
        let renewed = renew(
            &world.service,
            &session,
            signed(statement, &world.phone),
            at,
        )
        .await
        .unwrap();
        let claims = crate::agent::capabilities::decode_jwt(renewed.token).unwrap();
        assert_eq!(claims.expires_at(), renewed.expires_at);
        no_operator();
    }

    // A concurrent renewal can move the grant's expiry between the token's signing and the
    // answer: what is answered must be what the token carries.
    #[tokio::test]
    async fn a_renewal_between_the_signing_and_the_answer_keeps_them_agreed() {
        let world = world(55).await;
        world.operate();
        let result = world.login_as(&world.phone).await.unwrap();
        let session = open(&result.token).unwrap();
        let at = now() + 30;
        let mut statement = world.statement(&world.phone);
        statement.purpose = KeyPurpose::Renew;
        statement.grant = Some(result.grant_id.clone());
        statement.challenge = world
            .service
            .challenges
            .issue(&ChallengeSource::Grant(result.grant_id.clone()), at)
            .unwrap()
            .0;
        statement.issued_at = at;
        // The concurrent renewal lands between the token's signing and the answer.
        RENEWAL_LATER.store(at + 2 * KEY_SESSION_TTL, Ordering::SeqCst);
        let renewed = renew(
            &world.service,
            &session,
            signed(statement, &world.phone),
            at,
        )
        .await
        .unwrap();
        let claims = crate::agent::capabilities::decode_jwt(renewed.token).unwrap();
        assert_eq!(claims.expires_at(), renewed.expires_at);
        no_operator();
    }

    // A key-bound session on a managed agent ends its identity's grants whatever its
    // capabilities: the device key is the proof. Ending the agent's own other grants (a
    // password login, an approved app) still takes AGENT_UPDATE.
    #[tokio::test]
    async fn a_key_session_ends_its_identitys_grants_whatever_its_capabilities() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let user = AgentContext::managed("did:key:z6MkKeyLogout".into());
        let scid = "did:scid:coasys:1:QmKeyLogout";
        let insert_key =
            |key: &str, id: &str| {
                Grants::global()
                    .insert(
                        LiveGrant::for_test(id, user.clone(), now() + 3600).bound_to(
                            KeyBinding::new(scid.into(), key.into(), "d2".into(), 1, now()),
                        ),
                    )
                    .unwrap()
            };
        let key_grant = insert_key("z6MkPhone", "key-session-grant");
        let other = insert_key("z6MkLaptop", "other-device-grant");
        let (_, login_grant) = grants::mint(
            user.clone(),
            AuthInfo {
                app_name: "login".into(),
                capabilities: Some(vec![]),
                ..AuthInfo::default()
            },
            None,
            3600,
        )
        .unwrap();
        let session = Session::for_test(Principal::Agent(user.clone()))
            .with_caps(vec![
                crate::agent::capabilities::AGENT_READ_CAPABILITY.clone()
            ])
            .with_grant(key_grant);
        assert!(!session.may(&AGENT_UPDATE_CAPABILITY));

        let ended = end_all_sessions(&session, true, None).unwrap();
        assert_eq!(ended.ended, 1, "the identity's other key grant only");
        assert!(other.is_ended());
        assert!(login_grant.holds().is_ok(), "the agent's login stands");
    }

    // A challenge is a key session's own, whatever its capabilities (the binding proves the
    // key); every other caller needs AGENT_AUTH, which the anonymous set holds too.
    #[test]
    fn a_challenge_is_a_key_sessions_or_an_agent_auth_callers() {
        let key_session = Session::for_test(Principal::Agent(AgentContext::managed(
            "did:key:z6MkKeyChallenge".into(),
        )))
        .with_caps(vec![])
        .with_grant(Arc::new(
            LiveGrant::for_test(
                "challenge-grant",
                AgentContext::managed("did:key:z6MkKeyChallenge".into()),
                u64::MAX,
            )
            .bound_to(KeyBinding::new(
                "scid".into(),
                "z6Mk".into(),
                "d2".into(),
                1,
                1,
            )),
        ));
        let no_auth = Session::for_test(Principal::Agent(AgentContext::managed(
            "did:key:z6MkNoAuth".into(),
        )))
        .with_caps(vec![]);
        let anonymous = Session::for_test(Principal::Anonymous).with_caps(
            crate::agent::capabilities::anonymous_capabilities_when(false),
        );
        assert!(may_ask_challenge(&key_session));
        assert!(!may_ask_challenge(&no_auth));
        assert!(may_ask_challenge(&anonymous));
    }
}
