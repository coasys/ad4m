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
//! A revoke or a remove ends the grant in memory — its `ended` token fires, so its
//! sessions and sockets close — before any persistence attempt or retry (i). The end
//! is one pending operation per grant id — a `revoke`, or a `delete` — kept in the
//! `pending` map and noted durably in `pending_grant_writes`; a delete outranks a
//! revoke in both places, so a revoke never downgrades a pending delete (ii). Each
//! pass over the pending ops writes the note (recording the grant's strongest pending
//! op), then the grant's own change, then clears the note — and clears it only when
//! that op, or a stronger one, has landed (iii). A pass retries the note's insert as
//! well as the grant's write (iv), so a note that failed once is owed again rather
//! than left for a restart to miss. [`load`] ends every grant that has a note and
//! drives the ops again (v). What cannot survive is a crash between the in-memory
//! end and the first landed note — a restart then finds the grant still active in
//! the table.
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
use crate::db::{GrantKeyRow, GrantRow};
use crate::db_backend::DbBackend;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, RwLock};
use tokio_util::sync::CancellationToken;

use std::time::Duration;

lazy_static! {
    static ref GRANTS: Grants = Grants::default();
}

/// How often a revocation whose write failed is tried again, while a runtime runs it.
const REVOKE_RETRY_EVERY: Duration = Duration::from_secs(30);

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

/// A change to the `grants` table that is still owed: the grant stays ended in memory,
/// and the write is tried again until the table takes it. A `Delete` outranks a
/// `Revoke`: a revoke never downgrades a pending delete.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PendingWrite {
    Revoke,
    Delete,
}

impl PendingWrite {
    /// The `op` of `pending_grant_writes`.
    fn name(self) -> &'static str {
        match self {
            PendingWrite::Revoke => PENDING_REVOKE,
            PendingWrite::Delete => PENDING_DELETE,
        }
    }
}

/// The `op` values of the `pending_grant_writes` table: the write a revoke or a delete
/// left unlanded. It is noted before the change itself, so a restart between the
/// in-memory end and the table's write still finds the grant ended.
const PENDING_REVOKE: &str = "revoke";
const PENDING_DELETE: &str = "delete";

/// The executor's grants in memory, by token id. Ended grants stay until a restart (or their
/// expiry), so the app list shows them as revoked.
#[derive(Default)]
pub struct Grants {
    live: RwLock<HashMap<TokenId, Arc<LiveGrant>>>,
    /// Token ids whose end reached memory but not the table ([`Grants::retry_pending`]).
    pending: Mutex<HashMap<String, PendingWrite>>,
    /// One retry loop at a time (`kick_retry`).
    retry_scheduled: AtomicBool,
}

impl Grants {
    /// The executor's grants: every mint inserts here, and every session reads them.
    pub fn global() -> &'static Grants {
        &GRANTS
    }

    /// Adds `grant`, in the database first: a grant the database did not take would be gone
    /// after a restart, so its token is never handed out.
    pub fn insert(&self, grant: LiveGrant) -> Result<Arc<LiveGrant>, String> {
        self.retry_pending();
        let row = grant.row()?;
        crate::db_backend::db_backend()
            .add_grant(&row)
            .map_err(|e| e.to_string())?;
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
    /// once, even when the database write fails (then until the retry's write lands, or
    /// a restart).
    pub fn revoke(&self, grants: &[Arc<LiveGrant>]) -> Result<(), GrantError> {
        if grants.is_empty() {
            return Err(GrantError::NotFound);
        }
        self.end_pending(grants, PendingWrite::Revoke)
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
        self.revoke_where(|g| g.agent == *agent)
    }

    /// Revokes every grant of every agent, as [`Grants::revoke_agent`] does for one: the key
    /// that signed their tokens is gone (`identity.rekey` on the local wallet).
    pub fn revoke_all(&self) -> Result<usize, GrantError> {
        self.revoke_where(|_| true)
    }

    fn revoke_where(&self, matches: impl Fn(&LiveGrant) -> bool) -> Result<usize, GrantError> {
        let grants: Vec<Arc<LiveGrant>> = read(&self.live)
            .values()
            .filter(|g| matches(g))
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
        self.end_pending(grants, PendingWrite::Delete)
    }

    /// Ends `grants` in memory, then drives their `op` (`Revoke`, or the stronger
    /// `Delete`) to the table: the pending-op journal of the module doc. Answers
    /// `Store` when the grant's own write did not land; the op is then owed, noted
    /// and retried.
    fn end_pending(&self, grants: &[Arc<LiveGrant>], op: PendingWrite) -> Result<(), GrantError> {
        // (i) the grants end in memory before any persistence attempt or retry:
        // their tokens stop and their sockets close at once, whatever the store does.
        for grant in grants {
            grant.ended.cancel();
        }
        let ids: Vec<String> = grants.iter().map(|g| g.id.0.clone()).collect();
        // (ii) the strongest op still unlanded is what the map and the note keep.
        self.mark_pending(ids.iter().map(|id| (id.clone(), op)));
        let landed = self.drive_pending();
        self.kick_if_pending();
        match ids
            .iter()
            .filter_map(|id| landed.get(id))
            .find_map(|result| result.as_ref().err().cloned())
        {
            Some(e) => Err(GrantError::Store(e)),
            None => Ok(()),
        }
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

    /// Revokes every grant bound to the device key `key` (a multikey) of the identity `scid`
    /// (`did:scid:…`): its key stopped being valid in that identity's log. Returns how many
    /// were active.
    pub fn revoke_key(&self, scid: &str, key: &str) -> Result<usize, GrantError> {
        self.revoke_unexpired_where(|g| {
            g.key
                .as_ref()
                .is_some_and(|k| k.scid == scid && k.key == key)
        })
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
        self.revoke_unexpired_where(|g| {
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
        self.revoke_unexpired_where(|g| {
            g.agent == *agent
                && g.role == GrantRole::Agent
                && Some(&g.id) != keep
                && before.is_none_or(|before| g.created_at < before)
        })
    }

    /// Revokes the unexpired grants that `matches`, ended ones included (a failed write left
    /// them active in the database). Returns how many were active.
    fn revoke_unexpired_where(
        &self,
        matches: impl Fn(&LiveGrant) -> bool,
    ) -> Result<usize, GrantError> {
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
        self.retry_pending();
        let changed = crate::db_backend::db_backend()
            .renew_grant(&id.0, until, head_seq, checked_at)
            .map_err(|e| GrantError::Store(e.to_string()))?;
        // A grant that ended between the lookup and the write takes no rows: its token is
        // gone, so answering it renewed would hand out one that cannot work.
        if changed == 0 || grant.is_ended() {
            return Err(GrantError::NotFound);
        }
        key.checked(head_seq, checked_at);
        grant.expires_at.fetch_max(until, Ordering::SeqCst);
        Ok(grant)
    }

    /// Adds the grants that `db` holds, after it deletes the rows of expired tokens. A
    /// write a failed call left pending (`pending_grant_writes`) ends its grant here
    /// too — the row's state is not what decides — and the write is tried again.
    pub fn load(&self, db: &dyn DbBackend) -> Result<(), String> {
        db.delete_grants_expired_before(prune_cutoff(now()))
            .map_err(|e| e.to_string())?;
        self.retry_pending();
        let pending: Vec<(String, PendingWrite)> = db
            .pending_grant_writes()
            .map_err(|e| e.to_string())?
            .into_iter()
            .map(|(id, op)| {
                (
                    id,
                    match op.as_str() {
                        PENDING_DELETE => PendingWrite::Delete,
                        _ => PendingWrite::Revoke,
                    },
                )
            })
            .collect();
        {
            let mut live = write(&self.live);
            for row in db.grants().map_err(|e| e.to_string())? {
                match LiveGrant::from_row(row) {
                    Ok(grant) => {
                        if pending.iter().any(|(id, _)| *id == grant.id.0) {
                            grant.ended.cancel();
                        }
                        live.insert(grant.id.clone(), Arc::new(grant));
                    }
                    // An unreadable row grants nothing: its token is refused.
                    Err(e) => log::error!("Skipping a grant: {e}"),
                }
            }
        }
        // (v) every grant with a note ended above; the notes' ops are owed again now.
        self.mark_pending(pending.into_iter());
        self.drive_pending();
        self.kick_if_pending();
        Ok(())
    }

    /// Merges the writes `ops` (`id`, what to write) into the pending map: the
    /// strongest op still unlanded per grant, so a revoke never downgrades a
    /// pending delete. Nothing here reactivates a grant: it stays ended in memory
    /// while the write is retried.
    fn mark_pending(&self, ops: impl Iterator<Item = (String, PendingWrite)>) {
        let Ok(mut pending) = self.pending.lock() else {
            return;
        };
        for (id, op) in ops {
            if !matches!(pending.get(&id), Some(PendingWrite::Delete)) {
                pending.insert(id, op);
            }
        }
    }

    /// Drops `id`'s pending op once it landed; an op marked stronger meanwhile stays.
    fn unpend(&self, id: &str, op: PendingWrite) {
        if let Ok(mut pending) = self.pending.lock() {
            if pending.get(id) == Some(&op) {
                pending.remove(id);
            }
        }
    }

    /// Whether an earlier write is still waiting for its retry.
    fn pending_is_empty(&self) -> bool {
        self.pending.lock().map(|p| p.is_empty()).unwrap_or(true)
    }

    /// Starts the retry loop for the shared grants when an op is still owed.
    fn kick_if_pending(&self) {
        if std::ptr::eq(self, Grants::global()) && !self.pending_is_empty() {
            self.kick_retry();
        }
    }

    /// One pass over the pending ops: for each, the durable note of its strongest
    /// op, the grant's own write, then the note's clear — and it stays queued while
    /// any of the three failed, so the next pass retries the note's insert too.
    /// Returns each attempted id's write outcome for the caller's answer.
    fn drive_pending(&self) -> HashMap<String, Result<(), String>> {
        let todo: Vec<(String, PendingWrite)> = match self.pending.lock() {
            Ok(pending) => pending.iter().map(|(id, op)| (id.clone(), *op)).collect(),
            Err(_) => return HashMap::new(),
        };
        let db = crate::db_backend::db_backend();
        let mut landed = HashMap::new();
        for (id, op) in todo {
            if let Err(e) = db.note_pending_grant_writes(std::slice::from_ref(&id), op.name()) {
                log::error!("grants: noting the pending write of {id} failed: {e}");
            }
            let result = match op {
                PendingWrite::Revoke => db.revoke_grants(std::slice::from_ref(&id)),
                PendingWrite::Delete => db.delete_grants(std::slice::from_ref(&id)),
            };
            let Err(e) = result else {
                // (iii) the note goes only when this op landed; a failed clear keeps
                // the op queued so the next pass tries it all again.
                match db.clear_pending_grant_writes(std::slice::from_ref(&id), op.name()) {
                    Ok(()) => {
                        self.unpend(&id, op);
                        // A delete that lands takes the grant out of memory too, as
                        // `remove` does: the app list stops showing it.
                        if matches!(op, PendingWrite::Delete) {
                            write(&self.live).remove(&TokenId(id.clone()));
                        }
                    }
                    Err(e) => log::error!("grants: clearing the pending write of {id} failed: {e}"),
                }
                landed.insert(id, Ok(()));
                continue;
            };
            landed.insert(id, Err(e.to_string()));
        }
        landed
    }

    /// Writes every pending op once: the retry task calls it on [`REVOKE_RETRY_EVERY`];
    /// the next grants call calls it too.
    pub(crate) fn retry_pending(&self) {
        self.drive_pending();
    }

    /// Starts the retry loop of [`REVOKE_RETRY_EVERY`] if none runs.
    fn kick_retry(&self) {
        if self.retry_scheduled.swap(true, Ordering::SeqCst) {
            return;
        }
        let Ok(handle) = tokio::runtime::Handle::try_current() else {
            self.retry_scheduled.store(false, Ordering::SeqCst);
            return;
        };
        handle.spawn(async move {
            let grants = Grants::global();
            loop {
                tokio::time::sleep(REVOKE_RETRY_EVERY).await;
                grants.retry_pending();
                if grants.pending_is_empty() {
                    // The flag goes down before the last check, so a failure after it
                    // starts the next loop.
                    grants.retry_scheduled.store(false, Ordering::SeqCst);
                    if grants.pending_is_empty() {
                        break;
                    }
                    grants.kick_retry();
                    break;
                }
            }
        });
    }

    /// What `revoke` leaves when the table's write fails: the id, pending a `Revoke` write.
    #[cfg(test)]
    pub(crate) fn pend_revoke_for_test(&self, id: &str) {
        self.pending
            .lock()
            .unwrap()
            .insert(id.to_string(), PendingWrite::Revoke);
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

/// A token for the key-bound grant `grant` after its renewal: the same id (`nonce`), expiring
/// at `expires_at` — the expiry the grant carries, so the token and the answer agree.
pub fn renewed_token(grant: &LiveGrant, expires_at: u64) -> Result<String, String> {
    super::capabilities::token::sign_token_expiring_at(
        grant.auth.app_name.clone(),
        expires_at,
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
    Grants::global().load(crate::db_backend::db_backend())
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

    /// Opens the shared test database, held behind the one test lock for the whole
    /// test: the `grants`/`pending_grant_writes` tables and the `FAIL_NEXT_GRANT_*`
    /// seams are process-global, so these tests never run beside one another — nor
    /// beside `identity` tests minting or ending grants on the same tables.
    fn setup() -> tokio::sync::MutexGuard<'static, ()> {
        let seam = crate::test_utils::OPERATOR_RECORD.blocking_lock();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        seam
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
        let _seam = setup();
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
        let _seam = setup();
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
        let _seam = setup();
        assert!(Grants::global()
            .get(&TokenId("never-issued".into()))
            .is_none());
    }

    #[test]
    fn a_revoked_grant_is_gone_from_get_and_fires_its_token() {
        let _seam = setup();
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
        let _seam = setup();
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
        let _seam = setup();
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
        let _seam = setup();
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
        let _seam = setup();
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
        reloaded.load(crate::db_backend::db_backend()).unwrap();
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
        let _seam = setup();
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
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        assert!(
            reloaded.get(&failed.id).is_none(),
            "the retry wrote the ended grant"
        );
        assert!(reloaded.get(&active.id).is_none());
    }

    #[test]
    fn loading_drops_expired_rows() {
        crate::test_utils::ensure_test_db();
        let db = crate::db_backend::db_backend();
        let (expired, live) = (
            format!("expired-{}", uuid::Uuid::new_v4()),
            format!("live-{}", uuid::Uuid::new_v4()),
        );
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
        db.add_grant(&row(&expired, now() - TOKEN_EXPIRY_LEEWAY_SECONDS - 10))
            .unwrap();
        db.add_grant(&row(&live, now() + 3600)).unwrap();
        let grants = Grants::default();
        grants.load(db).unwrap();
        assert!(grants.get(&TokenId(live.clone())).is_some());
        assert!(grants.get(&TokenId(expired.clone())).is_none());
        let ids: Vec<String> = db.grants().unwrap().into_iter().map(|r| r.id).collect();
        assert!(ids.contains(&live));
        assert!(!ids.contains(&expired), "the expired row is deleted");
    }

    // A grant that ended between the renewal's lookup and the update takes no rows: the
    // renewal is refused rather than answering a token that cannot work.
    #[test]
    fn a_renewal_of_a_grant_that_just_ended_is_refused() {
        let _seam = setup();
        let (_, grant) = mint_key_grant(
            KeyGrant::Agent {
                agent: AgentContext::main_agent(),
                caps: vec![],
            },
            auth("renew-ended", vec![]),
            KeyBinding::new(
                "did:scid:coasys:1:Qm".into(),
                "z6Mk".into(),
                "d2".into(),
                1,
                now(),
            ),
            3600,
        )
        .unwrap();
        // The row is revoked while the grant still reads active: the update takes no rows.
        crate::db_backend::db_backend()
            .revoke_grants(std::slice::from_ref(&grant.id.0))
            .unwrap();
        assert_eq!(
            Grants::global()
                .renew(&grant.id, now() + 7200, 1, now())
                .unwrap_err(),
            GrantError::NotFound
        );
    }

    // A grant ended in memory whose revocation never reached the table stays ended here and
    // is written by the retry: the next grants call, or the retry task, persists it.
    #[tokio::test]
    async fn a_revocation_whose_write_failed_is_written_by_the_retry() {
        let _seam = crate::test_utils::OPERATOR_RECORD.lock().await;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
        let (_, grant) = mint(
            AgentContext::main_agent(),
            auth("revocation-retry", vec![]),
            None,
            3600,
        )
        .unwrap();
        // What a failed write leaves: ended in memory, active in the table, pending a retry.
        grant.ended.cancel();
        Grants::global().pend_revoke_for_test(&grant.id.0);
        let row = || {
            crate::db_backend::db_backend()
                .grants()
                .unwrap()
                .into_iter()
                .find(|r| r.id == grant.id.0)
                .unwrap()
        };
        assert!(!row().revoked, "the table still holds it active");
        Grants::global().retry_pending();
        assert!(row().revoked, "the retry wrote it");
        assert!(grant.is_ended(), "still ended in memory");
    }

    // A revocation whose write failed before a restart keeps the token refused: what it
    // left pending is durable, so the load ends the grant again and the retry finishes
    // the write.
    #[test]
    fn a_revocation_pending_at_a_restart_keeps_the_token_refused() {
        let _seam = setup();
        let (_, grant) = mint(
            AgentContext::main_agent(),
            auth("pending-restart", vec![]),
            None,
            3600,
        )
        .unwrap();
        // The table's write fails: the grant is ended in memory but the row stays active.
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            Grants::global().revoke(std::slice::from_ref(&grant)),
            Err(GrantError::Store(_))
        ));
        let row = || {
            crate::db_backend::db_backend()
                .grants()
                .unwrap()
                .into_iter()
                .find(|r| r.id == grant.id.0)
                .unwrap()
        };
        assert!(!row().revoked, "the table still holds it active");

        // The restart before the retry lands.
        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        assert!(
            reloaded.get(&grant.id).is_none(),
            "the pending revocation refuses the token"
        );
        assert!(row().revoked, "the retry finished the write");
    }

    // A delete whose write failed leaves the grant ended but listed; when the retry's write
    // lands the grant goes from memory too, as `remove` itself takes it out — the app list
    // stops showing it.
    #[test]
    fn a_delete_that_lands_on_retry_removes_the_grant() {
        let _seam = setup();
        let grants = Grants::default();
        let grant = grants
            .insert(LiveGrant::new(
                TokenId(uuid::Uuid::new_v4().to_string()),
                "req".into(),
                AgentContext::main_agent(),
                String::new(),
                auth("gone", vec![]),
                now() + 3600,
                now(),
            ))
            .unwrap();
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            grants.remove(std::slice::from_ref(&grant)),
            Err(GrantError::Store(_))
        ));
        assert_eq!(
            grants.list(&AgentContext::main_agent()).len(),
            1,
            "ended, still listed until the delete lands"
        );
        grants.retry_pending();
        assert!(
            grants.list(&AgentContext::main_agent()).is_empty(),
            "the landed delete leaves the list"
        );
        assert!(grants.pending_is_empty());
    }

    /// A grant ends in memory before any persistence attempt or retry: while the
    /// owed delete's retry is still blocked on the database, the new grant the
    /// revoke names is already ended — before its own or any pending write lands.
    #[test]
    fn a_grant_ends_before_pending_writes_are_driven() {
        let _seam = setup();
        let grants = Grants::default();
        let insert = |name: &str| {
            grants
                .insert(LiveGrant::new(
                    TokenId(uuid::Uuid::new_v4().to_string()),
                    name.into(),
                    AgentContext::main_agent(),
                    String::new(),
                    auth(name, vec![]),
                    now() + 3600,
                    now(),
                ))
                .unwrap()
        };
        let (failed, grant) = (insert("failed-remove"), insert("revoked"));
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            grants.remove(std::slice::from_ref(&failed)),
            Err(GrantError::Store(_))
        ));
        // The database's lock stands between the revoke and every pending write:
        // hold it, and the grant must read as ended while no write can land — a
        // revoke that drove the old writes first would leave it live here.
        let db_instance = crate::db::Ad4mDb::global_instance();
        let held_db = db_instance
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        std::thread::scope(|scope| {
            let revoke = scope.spawn(|| grants.revoke(std::slice::from_ref(&grant)));
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while !grant.is_ended() {
                assert!(
                    std::time::Instant::now() < deadline,
                    "the grant ends while its write is still blocked"
                );
                std::thread::yield_now();
            }
            // The grant is ended before any pending write lands: release the writes.
            drop(held_db);
            assert!(revoke.join().unwrap().is_ok());
        });
        assert!(failed.is_ended());
        assert!(grants.get(&grant.id).is_none());
        assert!(grants.get(&failed.id).is_none());
        // The one drive landed both: the owed delete and the revoke. The revoked
        // grant stays listed, ended; the deleted one is gone.
        assert!(grants.pending_is_empty());
        let listed = grants.list(&AgentContext::main_agent());
        assert_eq!(listed.len(), 1);
        assert!(listed[0].is_ended());
    }

    /// A revoke that lands must not clear a pending delete's durable note: the note names
    /// the strongest write still unlanded.
    #[test]
    fn a_landed_revoke_keeps_a_pending_delete_noted() {
        let _seam = setup();
        let db = crate::db_backend::db_backend();
        // Two in-memory grant lists over the one table, as a load makes: `a` owes a
        // delete, `b` revokes the same grant.
        let (a, b) = (Grants::default(), Grants::default());
        let id = TokenId(uuid::Uuid::new_v4().to_string());
        let grant = |grants: &Grants| {
            grants
                .insert(LiveGrant::new(
                    id.clone(),
                    "req".into(),
                    AgentContext::main_agent(),
                    String::new(),
                    auth("gone", vec![]),
                    now() + 3600,
                    now(),
                ))
                .unwrap()
        };
        let (ga, gb) = (grant(&a), grant(&b));
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            a.remove(std::slice::from_ref(&ga)),
            Err(GrantError::Store(_))
        ));
        assert!(b.revoke(std::slice::from_ref(&gb)).is_ok());
        assert_eq!(
            db.pending_grant_writes().unwrap(),
            vec![(ga.id.0.clone(), PENDING_DELETE.to_string())],
            "the delete is still unlanded: its note stays"
        );
        // The owed delete still lands on the retry.
        a.retry_pending();
        assert!(db.pending_grant_writes().unwrap().is_empty());
        assert!(db.grants().unwrap().iter().all(|r| r.id != ga.id.0));
    }

    /// A failed clear of the durable note keeps the op queued: the grant stays listed,
    /// ended, until the retry converges note, write and clear.
    #[test]
    fn a_failed_note_clear_keeps_the_op_queued() {
        let _seam = setup();
        let db = crate::db_backend::db_backend();
        let grants = Grants::default();
        let grant = grants
            .insert(LiveGrant::new(
                TokenId(uuid::Uuid::new_v4().to_string()),
                "req".into(),
                AgentContext::main_agent(),
                String::new(),
                auth("gone", vec![]),
                now() + 3600,
                now(),
            ))
            .unwrap();
        crate::db::grants::FAIL_NEXT_GRANT_CLEAR.store(true, std::sync::atomic::Ordering::SeqCst);
        grants.remove(std::slice::from_ref(&grant)).unwrap();
        assert!(
            !grants.pending_is_empty(),
            "the op stays queued while its note is not cleared"
        );
        assert_eq!(grants.list(&AgentContext::main_agent()).len(), 1);
        grants.retry_pending();
        assert!(grants.pending_is_empty());
        assert!(grants.list(&AgentContext::main_agent()).is_empty());
        assert!(db.pending_grant_writes().unwrap().is_empty());
    }

    /// The retry writes the durable note again too: when the note insert and the grant's
    /// write both failed once, a restart after the next pass still finds the grant ended.
    #[test]
    fn a_failed_note_is_retried_so_a_restart_finds_the_end() {
        let _seam = setup();
        let grants = Grants::default();
        let grant = grants
            .insert(LiveGrant::new(
                TokenId(uuid::Uuid::new_v4().to_string()),
                "req".into(),
                AgentContext::main_agent(),
                String::new(),
                auth("gone", vec![]),
                now() + 3600,
                now(),
            ))
            .unwrap();
        // The note insert and the grant's write both fail: nothing durable of the end.
        crate::db::grants::FAIL_NEXT_GRANT_NOTE.store(true, std::sync::atomic::Ordering::SeqCst);
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            grants.revoke(std::slice::from_ref(&grant)),
            Err(GrantError::Store(_))
        ));
        assert!(crate::db_backend::db_backend()
            .pending_grant_writes()
            .unwrap()
            .is_empty());
        // The retry writes the note again; the grant's write still fails, and the note
        // is what a restart reads.
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        grants.retry_pending();
        assert_eq!(
            crate::db_backend::db_backend()
                .pending_grant_writes()
                .unwrap(),
            vec![(grant.id.0.clone(), PENDING_REVOKE.to_string())]
        );
        // The restart: the note ends the grant on load, and the retry lands the write.
        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        assert!(reloaded.get(&grant.id).is_none());
        assert!(
            crate::db_backend::db_backend()
                .grants()
                .unwrap()
                .iter()
                .find(|r| r.id == grant.id.0)
                .unwrap()
                .revoked
        );
    }

    /// A revoke never downgrades a pending delete, in the queue or in the notes: driving
    /// the pending op lands the delete, not the revoke.
    #[test]
    fn a_revoke_never_downgrades_a_pending_delete() {
        let _seam = setup();
        let db = crate::db_backend::db_backend();
        let grants = Grants::default();
        let grant = grants
            .insert(LiveGrant::new(
                TokenId(uuid::Uuid::new_v4().to_string()),
                "req".into(),
                AgentContext::main_agent(),
                String::new(),
                auth("gone", vec![]),
                now() + 3600,
                now(),
            ))
            .unwrap();
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            grants.remove(std::slice::from_ref(&grant)),
            Err(GrantError::Store(_))
        ));
        assert!(grants.revoke(std::slice::from_ref(&grant)).is_ok());
        assert!(grants.list(&AgentContext::main_agent()).is_empty());
        assert!(db.grants().unwrap().iter().all(|r| r.id != grant.id.0));
        assert!(db.pending_grant_writes().unwrap().is_empty());
    }

    /// A delete pending at a restart removes the grant: the durable note ends it on load
    /// and the retry's delete takes the row out.
    #[test]
    fn a_pending_delete_at_a_restart_removes_the_grant() {
        let _seam = setup();
        let grants = Grants::default();
        let grant = grants
            .insert(LiveGrant::new(
                TokenId(uuid::Uuid::new_v4().to_string()),
                "req".into(),
                AgentContext::main_agent(),
                String::new(),
                auth("gone", vec![]),
                now() + 3600,
                now(),
            ))
            .unwrap();
        crate::db::grants::FAIL_NEXT_GRANT_WRITE.store(true, std::sync::atomic::Ordering::SeqCst);
        assert!(matches!(
            grants.remove(std::slice::from_ref(&grant)),
            Err(GrantError::Store(_))
        ));
        let reloaded = Grants::default();
        reloaded.load(crate::db_backend::db_backend()).unwrap();
        assert!(reloaded.get(&grant.id).is_none());
        assert!(reloaded
            .list(&AgentContext::main_agent())
            .iter()
            .all(|g| g.id != grant.id));
        assert!(crate::db_backend::db_backend()
            .grants()
            .unwrap()
            .iter()
            .all(|r| r.id != grant.id.0));
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
