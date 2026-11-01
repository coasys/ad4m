//! Grants: the one allowlist of tokens.
//!
//! Every token the executor issues has a grant: the agent it acts as, its capabilities and the
//! app that holds it. A token opens a session only while its grant exists, is active and has
//! not expired ([`Grants::get`]); its capabilities are the grant's, not the token's. Revoking a
//! grant (`agent.revokeToken`, `user.logout`, `user.revokeAllSessions`) or removing it
//! (`agent.removeApp`) ends the token: a missing grant refuses as a revoked one does, so a
//! removed app's token stays refused (#396).
//!
//! The table `grants` keeps the grants across restarts; [`load_at_start`] reads it back into
//! memory and drops the rows of expired tokens. Each live grant carries a
//! [`CancellationToken`] that fires when the grant ends: every socket of a session watches a
//! child of it, so a revocation closes the grant's sockets at once.
//!
//! Three paths mint tokens, and all insert a grant through [`mint`] or [`mint_key_grant`]: the
//! capability flow of an app (a grant on the main agent), a user's login (an auto-approved
//! grant on the user's managed agent) and a login with a device key (`identity::key_login`): a
//! grant bound to that key ([`KeyBinding`]), with the operator role ([`GrantRole::Operator`])
//! or on the agent that speaks for the key's identity. The admin credential is not a token and
//! has no grant.
//!
//! A key-bound grant lives one hour and is renewed ([`Grants::renew`]): its expiry is atomic,
//! so sockets opened before a renewal stay open. It ends when its key stops being valid in the
//! identity's log ([`Grants::revoke_key`]) and on log out everywhere ([`Grants::revoke_identity`]).

use super::capabilities::{
    read_stored_claims, AuthInfo, Capability, AGENT_AUTH_CAPABILITY, TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::session::{AuthError, TokenId};
use super::AgentContext;
use crate::db::{Ad4mDb, GrantKeyRow, GrantRow};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, RwLock};
use tokio_util::sync::CancellationToken;

lazy_static! {
    static ref GRANTS: Grants = Grants::default();
}

/// A grant in memory: what a session acting through a token may do, for how long.
#[derive(Debug)]
pub struct LiveGrant {
    /// The token's id (its `nonce` claim).
    pub id: TokenId,
    /// The capability request the grant answers. A login grant uses its own id.
    pub request_id: String,
    /// The agent the token acts as.
    pub agent: AgentContext,
    /// That agent's DID when the grant was made.
    pub did: String,
    /// What the token may do.
    pub caps: Vec<Capability>,
    /// The app that holds the token: its metadata and requested capabilities.
    pub auth: AuthInfo,
    /// The token's expiry (Unix seconds). A renewal of a key-bound grant moves it.
    expires_at: AtomicU64,
    pub created_at: u64,
    /// Fires when the grant is revoked or removed.
    pub ended: CancellationToken,
    /// What the grant carries: one agent, or the operator role.
    pub role: GrantRole,
    /// The device key a key login made the grant for. `None` for app approvals and password
    /// logins.
    pub key: Option<KeyBinding>,
}

/// What a grant carries.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GrantRole {
    /// A grant on one agent: the main agent, or a managed agent.
    Agent,
    /// The operator role, carried by a grant: `Session::open` maps it to `Principal::Operator`.
    /// Only a key login of the operator record's identity mints one.
    Operator,
}

/// The device key behind a grant that a key login made.
#[derive(Debug)]
pub struct KeyBinding {
    /// The identity: `did:scid:coasys:1:…`.
    pub scid: String,
    /// The device key that signed the login: its multikey (`z6Mk…`).
    pub key: String,
    /// Its id in the key log (`d2`).
    pub key_id: String,
    /// The trusted seq of the log at the last valid verdict.
    head_seq: AtomicU64,
    /// When the key was last found valid (Unix seconds).
    checked_at: AtomicU64,
}

impl KeyBinding {
    pub fn new(scid: String, key: String, key_id: String, head_seq: u64, checked_at: u64) -> Self {
        KeyBinding {
            scid,
            key,
            key_id,
            head_seq: AtomicU64::new(head_seq),
            checked_at: AtomicU64::new(checked_at),
        }
    }

    pub fn head_seq(&self) -> u64 {
        self.head_seq.load(Ordering::SeqCst)
    }

    pub fn checked_at(&self) -> u64 {
        self.checked_at.load(Ordering::SeqCst)
    }

    /// Records a valid verdict at `head_seq`, read at `at`.
    pub fn checked(&self, head_seq: u64, at: u64) {
        self.head_seq.store(head_seq, Ordering::SeqCst);
        self.checked_at.fetch_max(at, Ordering::SeqCst);
    }

    fn row(&self) -> GrantKeyRow {
        GrantKeyRow {
            scid: self.scid.clone(),
            signing_key: self.key.clone(),
            key_id: self.key_id.clone(),
            head_seq: self.head_seq(),
            checked_at: self.checked_at(),
        }
    }

    fn from_row(row: GrantKeyRow) -> Self {
        KeyBinding::new(
            row.scid,
            row.signing_key,
            row.key_id,
            row.head_seq,
            row.checked_at,
        )
    }
}

impl LiveGrant {
    /// A grant on `agent` (DID `did`) for the app `auth`, with the capabilities that `auth`
    /// requests: a token whose app requested none may only request one.
    pub fn new(
        id: TokenId,
        request_id: String,
        agent: AgentContext,
        did: String,
        auth: AuthInfo,
        expires_at: u64,
        created_at: u64,
    ) -> LiveGrant {
        let caps = auth
            .capabilities
            .clone()
            .unwrap_or_else(|| vec![AGENT_AUTH_CAPABILITY.clone()]);
        LiveGrant {
            id,
            request_id,
            agent,
            did,
            caps,
            auth,
            expires_at: AtomicU64::new(expires_at),
            created_at,
            ended: CancellationToken::new(),
            role: GrantRole::Agent,
            key: None,
        }
    }

    /// This grant with the operator role, on the main agent, with every capability.
    pub fn operator_role(self) -> LiveGrant {
        LiveGrant {
            role: GrantRole::Operator,
            agent: AgentContext::main_agent(),
            did: String::new(),
            caps: vec![super::capabilities::ALL_CAPABILITY.clone()],
            ..self
        }
    }

    /// This grant with `caps` as its capabilities.
    pub fn with_caps(self, caps: Vec<Capability>) -> LiveGrant {
        LiveGrant { caps, ..self }
    }

    /// This grant bound to the device key `key`.
    pub fn bound_to(self, key: KeyBinding) -> LiveGrant {
        LiveGrant {
            key: Some(key),
            ..self
        }
    }

    /// The token's expiry (Unix seconds).
    pub fn expires_at(&self) -> u64 {
        self.expires_at.load(Ordering::SeqCst)
    }

    /// Whether the grant carries the operator role.
    pub fn is_operator(&self) -> bool {
        self.role == GrantRole::Operator
    }

    /// The name of the app that holds the token.
    pub fn app_name(&self) -> &str {
        &self.auth.app_name
    }

    /// Whether the grant was revoked or removed.
    pub fn is_ended(&self) -> bool {
        self.ended.is_cancelled()
    }

    /// Whether the token is past its expiry at `now`, beyond the leeway that JWT validation
    /// allows.
    pub fn is_expired_at(&self, now: u64) -> bool {
        now > self
            .expires_at()
            .saturating_add(TOKEN_EXPIRY_LEEWAY_SECONDS)
    }

    /// `Ok` while the grant lets its token act: not ended and not expired.
    pub fn holds(&self) -> Result<(), AuthError> {
        if self.is_ended() {
            return Err(AuthError::Revoked);
        }
        if self.is_expired_at(now()) {
            return Err(AuthError::Expired);
        }
        Ok(())
    }

    /// The grant as the app list shows it. The token goes only to the app that redeemed its
    /// code (`agent.generateJwt`): anyone who read it could act as that app.
    pub fn as_app(&self) -> crate::types::Apps {
        crate::types::Apps {
            auth: self.auth.clone(),
            request_id: self.request_id.clone(),
            revoked: Some(self.is_ended()),
            token: String::new(),
        }
    }

    fn row(&self) -> Result<GrantRow, String> {
        Ok(GrantRow {
            id: self.id.0.clone(),
            request_id: self.request_id.clone(),
            agent_did: self.agent.managed_did().map(str::to_string),
            auth_info: serde_json::to_string(&self.auth).map_err(|e| e.to_string())?,
            revoked: self.is_ended(),
            expires_at: self.expires_at(),
            created_at: self.created_at,
            operator: self.is_operator(),
            key: self.key.as_ref().map(KeyBinding::row),
        })
    }

    fn from_row(row: GrantRow) -> Result<LiveGrant, String> {
        let auth: AuthInfo = serde_json::from_str(&row.auth_info)
            .map_err(|e| format!("grant {}: unreadable auth info: {e}", row.id))?;
        // The table names only a managed agent's DID: a main-agent grant read back leaves
        // its DID to the session, which reads the main agent's.
        let (agent, did) = match row.agent_did {
            Some(did) => (AgentContext::managed(did.clone()), did),
            None => (AgentContext::main_agent(), String::new()),
        };
        let mut grant = LiveGrant::new(
            TokenId(row.id),
            row.request_id,
            agent,
            did,
            auth,
            row.expires_at,
            row.created_at,
        );
        if row.operator {
            grant = grant.operator_role();
        }
        if let Some(key) = row.key {
            grant = grant.bound_to(KeyBinding::from_row(key));
        }
        if row.revoked {
            grant.ended.cancel();
        }
        Ok(grant)
    }
}

#[cfg(test)]
impl LiveGrant {
    /// A grant on `agent` that no store holds, for sessions built in tests.
    pub(crate) fn for_test(id: &str, agent: AgentContext, expires_at: u64) -> LiveGrant {
        LiveGrant::new(
            TokenId(id.to_string()),
            id.to_string(),
            agent,
            String::new(),
            AuthInfo::default(),
            expires_at,
            0,
        )
    }

    /// A copy with another expiry that ends with this grant.
    pub(crate) fn expiring_at(&self, expires_at: u64) -> LiveGrant {
        LiveGrant {
            id: self.id.clone(),
            request_id: self.request_id.clone(),
            agent: self.agent.clone(),
            did: self.did.clone(),
            caps: self.caps.clone(),
            auth: self.auth.clone(),
            expires_at: AtomicU64::new(expires_at),
            created_at: self.created_at,
            ended: self.ended.clone(),
            role: self.role,
            key: self.key.as_ref().map(|k| {
                KeyBinding::new(
                    k.scid.clone(),
                    k.key.clone(),
                    k.key_id.clone(),
                    k.head_seq(),
                    k.checked_at(),
                )
            }),
        }
    }
}

/// Why a grant operation failed.
#[derive(Debug, PartialEq, Eq)]
pub enum GrantError {
    /// No grant matches.
    NotFound,
    /// The change holds in memory, but the database did not take it, so a restart would
    /// undo it.
    Store(String),
}

impl std::fmt::Display for GrantError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GrantError::NotFound => write!(f, "No app matches this token or request id"),
            GrantError::Store(e) => write!(f, "Revoked until restart, but not saved: {e}"),
        }
    }
}

/// The executor's grants in memory, by token id. Ended grants stay until a restart (or their
/// expiry), so the app list shows them as revoked.
#[derive(Default)]
pub struct Grants {
    live: RwLock<HashMap<TokenId, Arc<LiveGrant>>>,
}

impl Grants {
    /// The executor's grants: every mint inserts here, and every session reads them.
    pub fn global() -> &'static Grants {
        &GRANTS
    }

    /// Adds `grant`, in the database first: a grant the database did not take would be gone
    /// after a restart, so its token is never handed out.
    pub fn insert(&self, grant: LiveGrant) -> Result<Arc<LiveGrant>, String> {
        let row = grant.row()?;
        with_db(|db| db.add_grant(&row).map_err(|e| e.to_string()))?;
        let grant = Arc::new(grant);
        let now = now();
        let mut live = self.live.write().map_err(|e| e.to_string())?;
        live.retain(|_, g| !g.is_expired_at(now));
        live.insert(grant.id.clone(), grant.clone());
        Ok(grant)
    }

    /// The grant of the token `id` while it lets the token act: active and not expired.
    /// `None` otherwise, and on a poisoned lock: fail closed.
    pub fn get(&self, id: &TokenId) -> Option<Arc<LiveGrant>> {
        let live = self.live.read().ok()?;
        live.get(id).filter(|g| g.holds().is_ok()).cloned()
    }

    /// The unexpired grants that `token_or_request_id` names: the grants answering that
    /// request id, or else the grant of that token (read without the wallet: the operator
    /// may revoke while the wallet is locked).
    pub fn find(&self, token_or_request_id: &str) -> Vec<Arc<LiveGrant>> {
        let now = now();
        let live = read(&self.live);
        let by_request: Vec<Arc<LiveGrant>> = live
            .values()
            .filter(|g| g.request_id == token_or_request_id && !g.is_expired_at(now))
            .cloned()
            .collect();
        if !by_request.is_empty() {
            return by_request;
        }
        read_stored_claims(token_or_request_id)
            .and_then(|claims| live.get(&TokenId(claims.nonce().to_string())).cloned())
            .filter(|g| !g.is_expired_at(now))
            .into_iter()
            .collect()
    }

    /// Revokes the grants `grants`: their tokens stop working and their sockets close at
    /// once, even when the database write fails (then only until a restart).
    pub fn revoke(&self, grants: &[Arc<LiveGrant>]) -> Result<(), GrantError> {
        if grants.is_empty() {
            return Err(GrantError::NotFound);
        }
        for grant in grants {
            grant.ended.cancel();
        }
        let ids: Vec<String> = grants.iter().map(|g| g.id.0.clone()).collect();
        with_db(|db| db.revoke_grants(&ids).map_err(|e| e.to_string())).map_err(GrantError::Store)
    }

    /// Revokes the grant of the token `id`.
    pub fn revoke_id(&self, id: &TokenId) -> Result<(), GrantError> {
        let grant = read(&self.live).get(id).cloned();
        self.revoke(&grant.into_iter().collect::<Vec<_>>())
    }

    /// Revokes every grant on `agent`. Returns how many were active. It writes the ended
    /// ones too: `revoke` ends a grant before its write, so after a failed write the
    /// grant is ended here but active in the database, which a restart loads.
    pub fn revoke_agent(&self, agent: &AgentContext) -> Result<usize, GrantError> {
        let grants: Vec<Arc<LiveGrant>> = read(&self.live)
            .values()
            .filter(|g| g.agent == *agent)
            .cloned()
            .collect();
        if grants.is_empty() {
            return Ok(0);
        }
        let active = grants.iter().filter(|g| !g.is_ended()).count();
        self.revoke(&grants).map(|_| active)
    }

    /// Deletes the grants `grants`: their tokens stop working as a revoked token does.
    /// They stay here, ended, until the database delete succeeds, so a retry after a
    /// failed delete still finds them.
    pub fn remove(&self, grants: &[Arc<LiveGrant>]) -> Result<(), GrantError> {
        if grants.is_empty() {
            return Err(GrantError::NotFound);
        }
        for grant in grants {
            grant.ended.cancel();
        }
        let ids: Vec<String> = grants.iter().map(|g| g.id.0.clone()).collect();
        with_db(|db| db.delete_grants(&ids).map_err(|e| e.to_string()))
            .map_err(GrantError::Store)?;
        let mut live = write(&self.live);
        for grant in grants {
            live.remove(&grant.id);
        }
        Ok(())
    }

    /// The unexpired grants on `agent`, active and revoked, oldest first.
    pub fn list(&self, agent: &AgentContext) -> Vec<Arc<LiveGrant>> {
        let now = now();
        let mut grants: Vec<Arc<LiveGrant>> = read(&self.live)
            .values()
            .filter(|g| g.agent == *agent && !g.is_expired_at(now))
            .cloned()
            .collect();
        grants.sort_by(|a, b| {
            (a.created_at, &a.request_id, &a.id.0).cmp(&(b.created_at, &b.request_id, &b.id.0))
        });
        grants
    }

    /// Revokes every grant bound to the device key `key` (a multikey): its key stopped being
    /// valid in its identity's log. Returns how many were active.
    pub fn revoke_key(&self, key: &str) -> Result<usize, GrantError> {
        self.revoke_where(|g| g.key.as_ref().is_some_and(|k| k.key == key))
    }

    /// Revokes every grant bound to a key of the identity `scid` (`did:scid:…`), except the
    /// grant `keep` and grants made at or after `before` (Unix seconds). Returns how many were
    /// active.
    pub fn revoke_identity(
        &self,
        scid: &str,
        keep: Option<&TokenId>,
        before: Option<u64>,
    ) -> Result<usize, GrantError> {
        self.revoke_where(|g| {
            g.key.as_ref().is_some_and(|k| k.scid == scid)
                && Some(&g.id) != keep
                && before.is_none_or(|before| g.created_at < before)
        })
    }

    /// Revokes every grant on `agent`, except the grant `keep` and grants made at or after
    /// `before`. Returns how many were active.
    pub fn revoke_agent_except(
        &self,
        agent: &AgentContext,
        keep: Option<&TokenId>,
        before: Option<u64>,
    ) -> Result<usize, GrantError> {
        self.revoke_where(|g| {
            g.agent == *agent
                && g.role == GrantRole::Agent
                && Some(&g.id) != keep
                && before.is_none_or(|before| g.created_at < before)
        })
    }

    /// Revokes the unexpired grants that `matches`, ended ones included (a failed write left
    /// them active in the database). Returns how many were active.
    fn revoke_where(&self, matches: impl Fn(&LiveGrant) -> bool) -> Result<usize, GrantError> {
        let now = now();
        let grants: Vec<Arc<LiveGrant>> = read(&self.live)
            .values()
            .filter(|g| !g.is_expired_at(now) && matches(g))
            .cloned()
            .collect();
        if grants.is_empty() {
            return Ok(0);
        }
        let active = grants.iter().filter(|g| !g.is_ended()).count();
        self.revoke(&grants).map(|_| active)
    }

    /// The active, unexpired grants bound to a device key: what the re-check of keys reads.
    pub fn key_bound(&self) -> Vec<Arc<LiveGrant>> {
        read(&self.live)
            .values()
            .filter(|g| g.key.is_some() && g.holds().is_ok())
            .cloned()
            .collect()
    }

    /// Renews the key-bound grant `id` until `until` (Unix seconds) on a valid verdict of its
    /// key at `head_seq`, read at `checked_at`. The grant keeps its id, so its open sockets
    /// stay open. `NotFound` for a grant that does not hold or has no key.
    pub fn renew(
        &self,
        id: &TokenId,
        until: u64,
        head_seq: u64,
        checked_at: u64,
    ) -> Result<Arc<LiveGrant>, GrantError> {
        let grant = self.get(id).ok_or(GrantError::NotFound)?;
        let key = grant.key.as_ref().ok_or(GrantError::NotFound)?;
        with_db(|db| {
            db.renew_grant(&id.0, until, head_seq, checked_at)
                .map_err(|e| e.to_string())
        })
        .map_err(GrantError::Store)?;
        key.checked(head_seq, checked_at);
        grant.expires_at.fetch_max(until, Ordering::SeqCst);
        Ok(grant)
    }

    /// Adds the grants that `db` holds, after it deletes the rows of expired tokens.
    pub fn load(&self, db: &Ad4mDb) -> Result<(), String> {
        db.delete_grants_expired_before(prune_cutoff(now()))
            .map_err(|e| e.to_string())?;
        let mut live = write(&self.live);
        for row in db.grants().map_err(|e| e.to_string())? {
            match LiveGrant::from_row(row) {
                Ok(grant) => {
                    live.insert(grant.id.clone(), Arc::new(grant));
                }
                // An unreadable row grants nothing: its token is refused.
                Err(e) => log::error!("Skipping a grant: {e}"),
            }
        }
        Ok(())
    }
}

/// Reads the map; a poisoned lock still serves reads that end grants or list them.
fn read<T>(lock: &RwLock<T>) -> std::sync::RwLockReadGuard<'_, T> {
    lock.read().unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn write<T>(lock: &RwLock<T>) -> std::sync::RwLockWriteGuard<'_, T> {
    lock.write()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn with_db<R>(f: impl FnOnce(&Ad4mDb) -> Result<R, String>) -> Result<R, String> {
    let db = Ad4mDb::global_instance();
    let db = db.lock().map_err(|e| e.to_string())?;
    let db = db.as_ref().ok_or("the database is not open")?;
    f(db)
}

/// A row whose token expired before this, leeway included, can go.
fn prune_cutoff(now: u64) -> u64 {
    now.saturating_sub(TOKEN_EXPIRY_LEEWAY_SECONDS)
}

/// Seconds since the Unix epoch.
pub(crate) fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(u64::MAX)
}

/// Signs a token that acts as `agent` for the app `auth`, valid for `valid_for` seconds, and
/// inserts its grant. `request_id` names the capability request the grant answers; `None`
/// (a login) uses the token's id. Fails, handing out no token, when the grant is not stored.
pub fn mint(
    agent: AgentContext,
    auth: AuthInfo,
    request_id: Option<String>,
    valid_for: u64,
) -> Result<(String, Arc<LiveGrant>), String> {
    let (token, claims) = super::capabilities::token::sign_token(
        auth.app_name.clone(),
        valid_for,
        agent.managed_did().map(str::to_string),
    )
    .map_err(|e| e.to_string())?;
    // A main-agent grant names no DID, as one read back from the table does: the session
    // reads the main agent's. The issuer is the signing key, which on a shared wallet is
    // `platform`, not the main agent.
    let did = agent.managed_did().unwrap_or_default().to_string();
    let id = claims.nonce().to_string();
    let grant = LiveGrant::new(
        TokenId(id.clone()),
        request_id.unwrap_or(id),
        agent,
        did,
        auth,
        claims.expires_at(),
        claims.issued_at(),
    );
    let grant = Grants::global().insert(grant)?;
    Ok((token, grant))
}

/// What a key login grants: the operator role, or one agent with these capabilities.
#[derive(Clone, Debug)]
pub enum KeyGrant {
    Operator,
    Agent {
        agent: AgentContext,
        caps: Vec<Capability>,
    },
}

/// Signs a token for the app `auth`, valid for `valid_for` seconds, whose grant is bound to
/// `key` and carries `what`, and inserts the grant. Fails, handing out no token, when the grant
/// is not stored. The grant's capabilities are kept in its `auth`, so a restart reads them back.
pub fn mint_key_grant(
    what: KeyGrant,
    mut auth: AuthInfo,
    key: KeyBinding,
    valid_for: u64,
) -> Result<(String, Arc<LiveGrant>), String> {
    let (agent, caps) = match &what {
        KeyGrant::Operator => (
            AgentContext::main_agent(),
            vec![super::capabilities::ALL_CAPABILITY.clone()],
        ),
        KeyGrant::Agent { agent, caps } => (agent.clone(), caps.clone()),
    };
    auth.capabilities = Some(caps.clone());
    let (token, claims) = super::capabilities::token::sign_token(
        auth.app_name.clone(),
        valid_for,
        agent.managed_did().map(str::to_string),
    )
    .map_err(|e| e.to_string())?;
    let did = agent.managed_did().unwrap_or_default().to_string();
    let id = claims.nonce().to_string();
    let mut grant = LiveGrant::new(
        TokenId(id.clone()),
        id,
        agent,
        did,
        auth,
        claims.expires_at(),
        claims.issued_at(),
    )
    .with_caps(caps)
    .bound_to(key);
    if matches!(what, KeyGrant::Operator) {
        grant = grant.operator_role();
    }
    let grant = Grants::global().insert(grant)?;
    Ok((token, grant))
}

/// A token for the key-bound grant `grant` after its renewal: the same id (`nonce`), valid for
/// `valid_for` seconds.
pub fn renewed_token(grant: &LiveGrant, valid_for: u64) -> Result<String, String> {
    super::capabilities::token::sign_token_with_id(
        grant.auth.app_name.clone(),
        valid_for,
        grant.agent.managed_did().map(str::to_string),
        Some(grant.id.0.clone()),
    )
    .map(|(token, _)| token)
    .map_err(|e| e.to_string())
}

/// Fills the executor's grants at start, after the database opens and before the API serves:
/// deletes expired rows and loads the rest. The app registry of executors before grants
/// (`apps_data.json`) is not read: data from before grants is not supported.
pub fn load_at_start() -> Result<(), String> {
    with_db(|db| Grants::global().load(db))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::{AGENT_READ_CAPABILITY, ALL_CAPABILITY};

    fn auth(name: &str, caps: Vec<Capability>) -> AuthInfo {
        AuthInfo {
            app_name: name.to_string(),
            capabilities: Some(caps),
            ..AuthInfo::default()
        }
    }

    fn setup() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
    }

    fn user(name: &str) -> AgentContext {
        AgentContext::managed(format!(
            "did:key:z6Mk{name}{}",
            uuid::Uuid::new_v4().simple()
        ))
    }

    // A grant on the main agent names no DID, as minted and as read back after a restart: a
    // session reads the main agent's. The token's issuer is the signing key, which is not the
    // main agent on a shared wallet (`platform`), so a session must never take it as its DID.
    #[test]
    fn a_main_agent_grant_names_the_same_did_minted_and_reloaded() {
        setup();
        let (_, grant) = mint(AgentContext::main_agent(), auth("app", vec![]), None, 3600).unwrap();
        let reloaded = LiveGrant::from_row(grant.row().unwrap()).unwrap();
        assert_eq!(grant.did, reloaded.did);
        assert_eq!(
            grant.did, "",
            "a main-agent grant leaves its DID to the session"
        );
    }

    #[test]
    fn a_minted_token_has_an_active_grant_with_the_apps_capabilities() {
        setup();
        let (token, grant) = mint(
            AgentContext::main_agent(),
            auth("grants-mint", vec![AGENT_READ_CAPABILITY.clone()]),
            Some("req-mint".into()),
            3600,
        )
        .unwrap();
        let claims = read_stored_claims(&token).unwrap();
        assert_eq!(grant.id.0, claims.nonce());
        assert_eq!(claims.sub, None);
        let got = Grants::global().get(&grant.id).expect("an active grant");
        assert_eq!(
            format!("{:?}", got.caps),
            format!("{:?}", vec![AGENT_READ_CAPABILITY.clone()])
        );
        assert_eq!(got.request_id, "req-mint");
        assert_eq!(got.app_name(), "grants-mint");
    }

    #[test]
    fn an_unknown_token_id_has_no_grant() {
        setup();
        assert!(Grants::global()
            .get(&TokenId("never-issued".into()))
            .is_none());
    }

    #[test]
    fn a_revoked_grant_is_gone_from_get_and_fires_its_token() {
        setup();
        let (_, grant) = mint(
            AgentContext::main_agent(),
            auth("grants-revoke", vec![ALL_CAPABILITY.clone()]),
            None,
            3600,
        )
        .unwrap();
        let socket = grant.ended.child_token();
        Grants::global().revoke_id(&grant.id).unwrap();
        assert!(Grants::global().get(&grant.id).is_none());
        assert!(socket.is_cancelled(), "the grant's sockets end");
        let listed = Grants::global().list(&AgentContext::main_agent());
        let app = listed
            .iter()
            .find(|g| g.id == grant.id)
            .expect("still listed");
        assert_eq!(app.as_app().revoked, Some(true));
    }

    #[test]
    fn a_removed_grant_refuses_its_token_and_leaves_the_list() {
        setup();
        let (_, grant) = mint(
            AgentContext::main_agent(),
            auth("grants-remove", vec![ALL_CAPABILITY.clone()]),
            Some(format!("req-{}", uuid::Uuid::new_v4())),
            3600,
        )
        .unwrap();
        let found = Grants::global().find(&grant.request_id);
        Grants::global().remove(&found).unwrap();
        assert!(Grants::global().get(&grant.id).is_none());
        assert!(grant.is_ended());
        assert!(Grants::global().find(&grant.request_id).is_empty());
    }

    #[test]
    fn find_takes_a_request_id_or_the_token_itself() {
        setup();
        let request_id = format!("req-{}", uuid::Uuid::new_v4());
        let (token, grant) = mint(
            AgentContext::main_agent(),
            auth("grants-find", vec![]),
            Some(request_id.clone()),
            3600,
        )
        .unwrap();
        assert_eq!(Grants::global().find(&request_id)[0].id, grant.id);
        assert_eq!(Grants::global().find(&token)[0].id, grant.id);
        assert!(Grants::global().find("nothing").is_empty());
        assert_eq!(
            Grants::global().revoke(&Grants::global().find("nothing")),
            Err(GrantError::NotFound)
        );
    }

    #[test]
    fn revoke_agent_ends_every_grant_of_that_agent_only() {
        setup();
        let (alice, bob) = (user("alice"), user("bob"));
        let insert = |agent: &AgentContext, name: &str| {
            let id = uuid::Uuid::new_v4().to_string();
            Grants::global()
                .insert(LiveGrant::new(
                    TokenId(id.clone()),
                    id,
                    agent.clone(),
                    "did:key:test".into(),
                    auth(name, vec![]),
                    now() + 3600,
                    now(),
                ))
                .unwrap()
        };
        let (a1, a2, b1) = (
            insert(&alice, "a1"),
            insert(&alice, "a2"),
            insert(&bob, "b1"),
        );
        assert_eq!(Grants::global().revoke_agent(&alice).unwrap(), 2);
        assert!(a1.is_ended() && a2.is_ended());
        assert!(!b1.is_ended());
        assert_eq!(Grants::global().revoke_agent(&alice).unwrap(), 0);
        assert_eq!(Grants::global().list(&alice).len(), 2);
        assert_eq!(Grants::global().list(&bob).len(), 1);
    }

    #[test]
    fn grants_and_their_state_survive_a_reload() {
        setup();
        let (_, kept) = mint(
            AgentContext::main_agent(),
            auth("grants-kept", vec![AGENT_READ_CAPABILITY.clone()]),
            None,
            3600,
        )
        .unwrap();
        let (_, revoked) = mint(
            AgentContext::main_agent(),
            auth("grants-revoked", vec![]),
            None,
            3600,
        )
        .unwrap();
        Grants::global().revoke_id(&revoked.id).unwrap();

        let reloaded = Grants::default();
        Ad4mDb::with_global_instance(|db| reloaded.load(db)).unwrap();
        let back = reloaded.get(&kept.id).expect("the active grant is back");
        assert_eq!(back.app_name(), "grants-kept");
        assert_eq!(format!("{:?}", back.caps), format!("{:?}", kept.caps));
        assert!(
            reloaded.get(&revoked.id).is_none(),
            "the revoked grant stays revoked"
        );
    }

    /// `revoke` ends grants in memory before it writes them. When that write fails, a
    /// retried revoke-all must still write them: the database, not `ended`, is what a
    /// restart loads.
    #[test]
    fn revoking_an_agent_again_writes_grants_already_ended_in_memory() {
        setup();
        let alice = user("alice");
        let insert = |name: &str| {
            let id = uuid::Uuid::new_v4().to_string();
            Grants::global()
                .insert(LiveGrant::new(
                    TokenId(id.clone()),
                    id,
                    alice.clone(),
                    "did:key:test".into(),
                    auth(name, vec![]),
                    now() + 3600,
                    now(),
                ))
                .unwrap()
        };
        let (failed, active) = (insert("write-failed"), insert("active"));
        // What a revoke whose database write failed leaves: ended here, active in the db.
        failed.ended.cancel();

        assert_eq!(Grants::global().revoke_agent(&alice).unwrap(), 1);

        let reloaded = Grants::default();
        Ad4mDb::with_global_instance(|db| reloaded.load(db)).unwrap();
        assert!(
            reloaded.get(&failed.id).is_none(),
            "the retry wrote the ended grant"
        );
        assert!(reloaded.get(&active.id).is_none());
    }

    #[test]
    fn loading_drops_expired_rows() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let row = |id: &str, expires_at: u64| GrantRow {
            id: id.into(),
            request_id: id.into(),
            agent_did: None,
            auth_info: serde_json::to_string(&AuthInfo::default()).unwrap(),
            revoked: false,
            expires_at,
            created_at: 0,
            ..GrantRow::default()
        };
        db.add_grant(&row("expired", now() - TOKEN_EXPIRY_LEEWAY_SECONDS - 10))
            .unwrap();
        db.add_grant(&row("live", now() + 3600)).unwrap();
        let grants = Grants::default();
        grants.load(&db).unwrap();
        assert!(grants.get(&TokenId("live".into())).is_some());
        assert!(grants.get(&TokenId("expired".into())).is_none());
        let ids: Vec<String> = db.grants().unwrap().into_iter().map(|r| r.id).collect();
        assert_eq!(ids, ["live"], "the expired row is deleted");
    }

    #[test]
    fn a_poisoned_lock_refuses_every_token() {
        let grants = Grants::default();
        let id = TokenId("poisoned".into());
        grants.live.write().unwrap().insert(
            id.clone(),
            Arc::new(LiveGrant::for_test(
                "poisoned",
                AgentContext::main_agent(),
                u64::MAX,
            )),
        );
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _guard = grants.live.write().unwrap();
            panic!("poison the lock");
        }));
        assert!(grants.get(&id).is_none());
    }
}
