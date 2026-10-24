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
//! Two paths mint tokens, and both insert a grant through [`mint`]: the capability flow of an
//! app (a grant on the main agent) and a user's login (an auto-approved grant on the user's
//! managed agent). The admin credential is not a token and has no grant.

use super::capabilities::{
    read_stored_claims, AuthInfo, Capability, AGENT_AUTH_CAPABILITY, TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::session::{AuthError, TokenId};
use super::AgentContext;
use crate::db::{Ad4mDb, GrantRow};
use std::collections::HashMap;
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
    /// The token's expiry (Unix seconds).
    pub expires_at: u64,
    pub created_at: u64,
    /// Fires when the grant is revoked or removed.
    pub ended: CancellationToken,
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
            expires_at,
            created_at,
            ended: CancellationToken::new(),
        }
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
        now > self.expires_at.saturating_add(TOKEN_EXPIRY_LEEWAY_SECONDS)
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
            expires_at: self.expires_at,
            created_at: self.created_at,
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
        let grant = LiveGrant::new(
            TokenId(row.id),
            row.request_id,
            agent,
            did,
            auth,
            row.expires_at,
            row.created_at,
        );
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
            expires_at,
            created_at: self.created_at,
            ended: self.ended.clone(),
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
