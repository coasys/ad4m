//! Login with a device key, and the end of the sessions of a key that stops being valid.
//!
//! A native app asks `user.sessionChallenge` for a one-use challenge, signs a [`KeyStatement`]
//! with one of its person's device keys under `coasys:session`, and sends it to
//! `user.loginWithKey`. The executor checks, in order: the challenge (consumed whatever
//! follows), the audience (its own DID, `issuedAt` within [`CLOCK_SKEW`] of its clock), the
//! signature, the key (a device key of the named log, valid at the log's trusted seq, without
//! the bot mark, read at most [`LOGIN_MAX_AGE`] ago), then the role:
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
use crate::agent::capabilities::{get_user_default_capabilities, grant_caps_for, AuthInfo};
use crate::agent::grants::{self, Grants, KeyBinding, KeyGrant, LiveGrant};
use crate::agent::session::{OperatorSource, Principal, Session};
use crate::agent::{did_for_context, AgentContext};
use crate::api::ws_handler::WsRpcError;

/// How long a key session's grant lives after its login or its last renewal.
pub const KEY_SESSION_TTL: u64 = 60 * 60;
/// How long a challenge stays open.
pub const CHALLENGE_TTL: u64 = 60;
/// The most challenges open at once.
pub const MAX_OPEN_CHALLENGES: usize = 10_000;
/// How far a statement's `issuedAt` may be from the executor's clock.
pub const CLOCK_SKEW: u64 = 120;
/// The oldest read of a key's log a login or a renewal takes; an older one goes to the store.
pub const LOGIN_MAX_AGE: u64 = 5 * 60;
/// How often the keys of live key-bound grants are read again.
pub const KEY_RECHECK_INTERVAL: Duration = Duration::from_secs(5 * 60);
/// How long a key-bound grant lasts on its last valid verdict while the store cannot be read:
/// a renewal proceeds, and the re-check keeps it, until then.
pub const STORE_OUTAGE_GRACE: u64 = 60 * 60;

/// Why a key login, a renewal or a statement signature was refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeyLoginError {
    /// The challenge is unknown, used or expired.
    Challenge,
    /// The statement names another executor, or its `issuedAt` is off the executor's clock.
    Audience,
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
    /// Only the admin credential or the loopback caller signs as this device.
    LocalOnly,
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
            KeyLoginError::Signature => "signature",
            KeyLoginError::Key => "key",
            KeyLoginError::NotKnown => "not-known",
            KeyLoginError::StoreUnavailable => "store-unavailable",
            KeyLoginError::Grant => "grant",
            KeyLoginError::TooManyChallenges => "too-many-challenges",
            KeyLoginError::LocalOnly => "local-only",
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
            | KeyLoginError::Grant
            | KeyLoginError::LocalOnly => WsRpcError::forbidden(code),
            KeyLoginError::StoreUnavailable => WsRpcError {
                code: 503,
                message: code.into(),
            },
            KeyLoginError::TooManyChallenges => WsRpcError::too_many_requests(code),
            KeyLoginError::BadRequest(m) => WsRpcError::bad_request(m),
            KeyLoginError::Internal(m) => WsRpcError::internal(m),
        }
    }
}

/// The open challenges, each with its expiry (Unix seconds). In memory only: a restart drops
/// them, and a challenge never outlives [`CHALLENGE_TTL`] anyway.
#[derive(Default)]
pub struct Challenges {
    open: Mutex<HashMap<String, u64>>,
}

impl Challenges {
    /// A new challenge, open until the expiry it answers with.
    pub fn issue(&self, now: u64) -> Result<(String, u64), KeyLoginError> {
        let mut open = self
            .open
            .lock()
            .map_err(|_| KeyLoginError::Internal("the challenge store is poisoned".into()))?;
        if open.len() >= MAX_OPEN_CHALLENGES {
            open.retain(|_, expires| *expires >= now);
        }
        if open.len() >= MAX_OPEN_CHALLENGES {
            return Err(KeyLoginError::TooManyChallenges);
        }
        let mut bytes = [0u8; 32];
        rand::rngs::OsRng.fill_bytes(&mut bytes);
        let challenge = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes);
        let expires = now + CHALLENGE_TTL;
        open.insert(challenge.clone(), expires);
        Ok((challenge, expires))
    }

    /// Uses `challenge` up: true when it was open and has not expired. It is gone afterwards,
    /// whatever the answer.
    pub fn consume(&self, challenge: &str, now: u64) -> bool {
        self.open
            .lock()
            .ok()
            .and_then(|mut open| open.remove(challenge))
            .is_some_and(|expires| now <= expires)
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.open.lock().unwrap().len()
    }
}

/// The executor's DID: the audience of every statement it accepts.
pub fn executor_did() -> Result<String, KeyLoginError> {
    crate::agent::capabilities::token::issuer_did()
        .map_err(|e| KeyLoginError::Internal(e.to_string()))
}

/// `user.sessionChallenge`.
pub fn challenge(service: &IdentityService, now: u64) -> Result<SessionChallenge, KeyLoginError> {
    let executor = executor_did()?;
    let (challenge, expires_at) = service.challenges.issue(now)?;
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

fn check_audience(statement: &KeyStatement, executor: &str, now: u64) -> Result<(), KeyLoginError> {
    if statement.executor != executor || now.abs_diff(statement.issued_at) > CLOCK_SKEW {
        return Err(KeyLoginError::Audience);
    }
    Ok(())
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
    f: impl FnOnce(&crate::db::Ad4mDb) -> crate::db::Ad4mDbResult<R>,
) -> Result<R, KeyLoginError> {
    crate::db::Ad4mDb::with_global_instance(f).map_err(|e| KeyLoginError::Internal(e.to_string()))
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
    check_audience(statement, &executor_did()?, now)?;
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
    check_audience(statement, &executor_did()?, now)?;
    if !signature_holds(&proof) {
        return Err(KeyLoginError::Signature);
    }
    let read = match service
        .resolver
        .session_key(&binding.scid, &binding.key, LOGIN_MAX_AGE)
        .await
    {
        Err(SessionKeyError::StoreUnavailable)
            if now.saturating_sub(binding.checked_at()) < STORE_OUTAGE_GRACE =>
        {
            service
                .resolver
                .cached_session_key(&binding.scid, &binding.key)
                .await
        }
        other => other,
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
        .renew(&grant.id, until, key.head_seq, now)
        .map_err(|e| match e {
            grants::GrantError::NotFound => KeyLoginError::Grant,
            grants::GrantError::Store(e) => KeyLoginError::Internal(e),
        })?;
    let token = grants::renewed_token(&grant, KEY_SESSION_TTL).map_err(KeyLoginError::Internal)?;
    Ok(RenewedSession {
        token,
        expires_at: grant.expires_at(),
    })
}

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
        Principal::Agent(AgentContext::Managed { .. }) => SessionRole::Tenant,
        Principal::Agent(AgentContext::Main) => SessionRole::App,
        Principal::Anonymous => SessionRole::Anonymous,
    };
    let binding = session.key_binding();
    SessionInfo {
        role,
        did: session.acts_as_did().map(str::to_string),
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
/// agent's identity, under `coasys:session`. Only for the admin credential or the loopback
/// caller (the shell on this machine), never for an operator from a key login; only a native
/// statement without PKCE.
pub fn sign_statement(
    session: &Session,
    unsigned: UnsignedKeyStatement,
) -> Result<SignedKeyStatement, KeyLoginError> {
    if !matches!(
        session.operator_source(),
        Some(OperatorSource::Credential | OperatorSource::Loopback)
    ) {
        return Err(KeyLoginError::LocalOnly);
    }
    check_shape(
        unsigned.v,
        unsigned.purpose,
        &unsigned.client,
        unsigned.code_challenge.as_ref(),
        unsigned.grant.as_ref(),
        unsigned.purpose,
    )?;
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

/// Reads the key of every live key-bound grant again (at most [`KEY_RECHECK_INTERVAL`] old):
/// a key no longer valid, or a bot's, ends its grants; a valid one records the check. While the
/// store cannot be read, grants whose last valid check is [`STORE_OUTAGE_GRACE`] old end.
/// Returns how many active grants ended.
pub async fn recheck_key_grants(service: &IdentityService, now: u64) -> usize {
    let mut by_key: BTreeMap<(String, String), Vec<Arc<LiveGrant>>> = BTreeMap::new();
    for grant in Grants::global().key_bound() {
        if let Some(binding) = &grant.key {
            by_key
                .entry((binding.scid.clone(), binding.key.clone()))
                .or_default()
                .push(grant.clone());
        }
    }
    let mut ended = 0;
    for ((scid, key), grants) in by_key {
        let read = service
            .resolver
            .session_key(&scid, &key, KEY_RECHECK_INTERVAL.as_secs())
            .await;
        let to_end: Vec<Arc<LiveGrant>> = match read {
            Ok(found) if found.admits() => {
                for grant in &grants {
                    if let Some(binding) = &grant.key {
                        binding.checked(found.head_seq, now);
                    }
                }
                continue;
            }
            Ok(_) | Err(SessionKeyError::NotListed) => grants,
            Err(SessionKeyError::StoreUnavailable) => grants
                .into_iter()
                .filter(|g| {
                    g.key
                        .as_ref()
                        .is_some_and(|b| now.saturating_sub(b.checked_at()) >= STORE_OUTAGE_GRACE)
                })
                .collect(),
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

/// Ends key sessions whose key stops being valid: on each verdict change the cache publishes,
/// and every [`KEY_RECHECK_INTERVAL`] for what a change cannot carry (a store that was down, a
/// key that gained the bot mark). Started by `identity::init`.
pub async fn run_key_session_watch(service: Arc<IdentityService>) {
    use tokio::sync::broadcast::error::RecvError;
    let mut changes = service.cache.subscribe();
    let mut tick = tokio::time::interval(KEY_RECHECK_INTERVAL);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    // The first tick is at once: grants loaded at start are checked then.
    loop {
        tokio::select! {
            change = changes.recv() => match change {
                Ok(change) => {
                    on_verdict_change(&change);
                }
                // Changes were missed: read every key again.
                Err(RecvError::Lagged(_)) => {
                    recheck_key_grants(&service, super::service::now()).await;
                }
                Err(RecvError::Closed) => return,
            },
            _ = tick.tick() => {
                recheck_key_grants(&service, super::service::now()).await;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::AuthError;
    use crate::identity::backend::IdentityBackend;
    use crate::identity::cache::tests::{accept_signed, trusting, FakeStore, TestLog};
    use crate::identity::device::{enc_key_of, multikey_of as device_multikey};
    use crate::identity::resolver::Resolver;
    use crate::types::{AuthInfoInput, CapabilityInput, ResourceInput};
    use ad4m_identity_core::build::{add_unsigned, cosign, revoke, sign_domain, Signer};
    use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
    use ed25519_dalek::SigningKey;
    use std::sync::atomic::Ordering;

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
            };
            self.store = store;
            self
        }

        fn challenge(&self) -> SessionChallenge {
            challenge(&self.service, now()).unwrap()
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
            db(|db| db.set_operator_record_for_test(Some(&self.did))).unwrap();
        }
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
        db(|db| db.set_operator_record_for_test(None)).unwrap();
    }

    /// Binds the main agent to `scid` in the shared database until dropped.
    struct MainBound(String);

    impl MainBound {
        fn to(scid: &str) -> MainBound {
            let main = did_for_context(&AgentContext::main_agent()).unwrap();
            db(|db| db.clear_agent_identity_for_test(&main)).unwrap();
            db(|db| db.set_agent_identity(&main, scid, 1)).unwrap();
            MainBound(main)
        }
    }

    impl Drop for MainBound {
        fn drop(&mut self) {
            let _ = db(|db| db.clear_agent_identity_for_test(&self.0));
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
        let (c, expires) = challenges.issue(1_000).unwrap();
        assert_eq!(expires, 1_000 + CHALLENGE_TTL);
        assert!(challenges.consume(&c, 1_010));
        assert!(!challenges.consume(&c, 1_010), "a second use");

        let (late, _) = challenges.issue(1_000).unwrap();
        assert!(
            !challenges.consume(&late, 1_000 + CHALLENGE_TTL + 1),
            "after 60 s"
        );
        assert!(!challenges.consume(&late, 1_000), "gone after the late use");
        assert!(!challenges.consume("never-issued", 1_000));

        let (a, _) = challenges.issue(1_000).unwrap();
        let (b, _) = challenges.issue(1_000).unwrap();
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
        for _ in 0..MAX_OPEN_CHALLENGES {
            challenges.issue(1_000).unwrap();
        }
        assert_eq!(
            challenges.issue(1_000).unwrap_err(),
            KeyLoginError::TooManyChallenges
        );
        assert!(challenges.issue(1_000 + CHALLENGE_TTL + 1).is_ok());
        assert_eq!(challenges.len(), 1);
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
        crate::db::Ad4mDb::with_global_instance(|db| reloaded.load(db)).unwrap();
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
        for key in [&stranger, &master, &bot] {
            assert_eq!(grants_of(key), 0);
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
        // The cache read the store just now: no refresh brings the store's newer head.
        world.service.cache.note_refreshed(&scid, now());
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
            .note_refreshed(&world.log.scid(), now() - LOGIN_MAX_AGE - 1);
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
        crate::db::Ad4mDb::with_global_instance(|db| reloaded.load(db)).unwrap();
        assert!(reloaded
            .get(&crate::agent::session::TokenId(result.grant_id))
            .is_none());
        no_operator();
    }

    // The re-check reads each key again: a revocation the subscription missed ends the
    // session; a valid key records the check; a store outage ends only grants whose last
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
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        recheck_key_grants(&world.service, t + 10).await;
        assert!(session.check().is_ok());
        assert_eq!(binding.checked_at(), t + 10);

        // The store is down: kept within the hour, ended after it.
        world.store.store_down.store(true, Ordering::SeqCst);
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        recheck_key_grants(&world.service, t + 20).await;
        assert!(session.check().is_ok(), "kept on the last valid verdict");
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        recheck_key_grants(&world.service, t + 10 + STORE_OUTAGE_GRACE).await;
        assert_eq!(session.check(), Err(AuthError::Revoked), "fail closed");

        // A revocation on the store, never published to this executor's subscribers.
        let again = world.login_as(&world.phone).await;
        assert_eq!(again.unwrap_err(), KeyLoginError::StoreUnavailable);
        world.store.store_down.store(false, Ordering::SeqCst);
        let fresh = world.login_as(&world.phone).await.unwrap();
        let fresh_session = open(&fresh.token).unwrap();
        let revoke_phone =
            revoke(&world.log.chain(), &world.log.master, &["d2".to_string()]).unwrap();
        world.log.events.push(revoke_phone);
        *world.store.events.lock().unwrap() = world.log.events.clone();
        world.service.cache.note_refreshed(&world.log.scid(), 0);
        assert_eq!(recheck_key_grants(&world.service, now()).await, 1);
        assert_eq!(fresh_session.check(), Err(AuthError::Revoked));
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
                now()
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
                now()
            )
            .await
            .unwrap_err(),
            KeyLoginError::BadRequest(_)
        ));

        // Renewed half an hour on: the expiry moves by the session's life from then.
        let later = now() + 30 * 60;
        let mut statement = renew_statement(&world, &world.phone, &result.grant_id);
        statement.challenge = world.service.challenges.issue(later).unwrap().0;
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
                now()
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
                now()
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
    // `coasys:session`, for the admin credential only.
    #[tokio::test]
    async fn the_executor_signs_a_native_statement_for_the_admin_credential_only() {
        let world = world(220).await;
        let main = did_for_context(&AgentContext::main_agent()).unwrap();
        let operator = Session::for_test(Principal::Operator);
        let unsigned = UnsignedKeyStatement {
            v: 1,
            purpose: KeyPurpose::Login,
            executor: "did:key:z6MkTarget".into(),
            challenge: "c".into(),
            client: SessionClient::Native,
            code_challenge: None,
            grant: None,
            issued_at: 5,
        };
        db(|db| db.clear_agent_identity_for_test(&main)).unwrap();
        assert!(matches!(
            sign_statement(&operator, unsigned.clone()),
            Err(KeyLoginError::BadRequest(m)) if m.starts_with("no-identity")
        ));
        let _bound = MainBound::to(&world.log.scid());
        let signed = sign_statement(&operator, unsigned.clone()).unwrap();
        assert_eq!(signed.statement.scid, world.did);
        assert_eq!(signed.statement.key, multikey_of(&main).unwrap());
        assert!(signature_holds(&signed));

        let web = UnsignedKeyStatement {
            client: SessionClient::Web {
                origin: "https://app.example".into(),
            },
            ..unsigned.clone()
        };
        assert!(matches!(
            sign_statement(&operator, web),
            Err(KeyLoginError::BadRequest(_))
        ));
        let pkce = UnsignedKeyStatement {
            code_challenge: Some("x".into()),
            ..unsigned.clone()
        };
        assert!(matches!(
            sign_statement(&operator, pkce),
            Err(KeyLoginError::BadRequest(_))
        ));

        // An operator from a key login, and an app, may not sign as this device.
        world.operate();
        let key_operator = open(&world.login_as(&world.phone).await.unwrap().token).unwrap();
        assert_eq!(
            sign_statement(&key_operator, unsigned.clone()).unwrap_err(),
            KeyLoginError::LocalOnly
        );
        let app = Session::for_test(Principal::Agent(AgentContext::main_agent()));
        assert_eq!(
            sign_statement(&app, unsigned).unwrap_err(),
            KeyLoginError::LocalOnly
        );
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
}
