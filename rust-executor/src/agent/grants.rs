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
    read_stored_claims, AuthInfo, AuthInfoExtended, Capability, AGENT_AUTH_CAPABILITY,
    TOKEN_EXPIRY_LEEWAY_SECONDS,
};
use super::session::{AuthError, TokenId};
use super::AgentContext;
use crate::db::{Ad4mDb, GrantRow};
use std::collections::HashMap;
use std::path::Path;
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

    /// Revokes every active grant on `agent`. Returns how many there were.
    pub fn revoke_agent(&self, agent: &AgentContext) -> Result<usize, GrantError> {
        let grants: Vec<Arc<LiveGrant>> = read(&self.live)
            .values()
            .filter(|g| g.agent == *agent && !g.is_ended())
            .cloned()
            .collect();
        if grants.is_empty() {
            return Ok(0);
        }
        self.revoke(&grants).map(|_| grants.len())
    }

    /// Deletes the grants `grants`: their tokens stop working as a revoked token does.
    pub fn remove(&self, grants: &[Arc<LiveGrant>]) -> Result<(), GrantError> {
        if grants.is_empty() {
            return Err(GrantError::NotFound);
        }
        {
            let mut live = write(&self.live);
            for grant in grants {
                grant.ended.cancel();
                live.remove(&grant.id);
            }
        }
        let ids: Vec<String> = grants.iter().map(|g| g.id.0.clone()).collect();
        with_db(|db| db.delete_grants(&ids).map_err(|e| e.to_string())).map_err(GrantError::Store)
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
    let did = match agent.managed_did() {
        Some(did) => did.to_string(),
        None => claims.issuer().to_string(),
    };
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

/// The app registry file of executors before grants: apps by request id, each with its
/// token and a revoked flag.
#[derive(serde::Deserialize)]
struct RegisteredApp {
    auth_info_extended: AuthInfoExtended,
    revoked: bool,
    token: String,
}

/// Moves the apps of the registry file `apps_file` into grant rows on the main agent, so their
/// tokens keep working (and revoked ones stay refused), then renames the file
/// `<name>.migrated`. Reads each token's id and expiry without the wallet, which is locked at
/// start. A token that does not parse grants nothing. Running it again changes nothing: a row
/// with the same id stays as it is.
pub fn migrate_apps_file(db: &Ad4mDb, apps_file: &Path) -> Result<usize, String> {
    if !apps_file.exists() {
        return Ok(0);
    }
    let text = std::fs::read_to_string(apps_file).map_err(|e| e.to_string())?;
    let apps: HashMap<String, RegisteredApp> = serde_json::from_str(&text)
        .map_err(|e| format!("{}: not an app registry: {e}", apps_file.display()))?;
    let mut migrated = 0;
    for (request_id, app) in apps {
        let Some(claims) = read_stored_claims(&app.token) else {
            log::warn!("Not migrating app {request_id}: its token does not parse");
            continue;
        };
        let auth = AuthInfo {
            user_email: None,
            ..app.auth_info_extended.auth
        };
        let row = GrantRow {
            id: claims.nonce().to_string(),
            request_id,
            agent_did: None,
            auth_info: serde_json::to_string(&auth).map_err(|e| e.to_string())?,
            revoked: app.revoked,
            expires_at: claims.expires_at(),
            created_at: claims.issued_at(),
        };
        db.add_grant(&row).map_err(|e| e.to_string())?;
        migrated += 1;
    }
    let mut renamed = apps_file.as_os_str().to_owned();
    renamed.push(".migrated");
    std::fs::rename(apps_file, &renamed).map_err(|e| e.to_string())?;
    Ok(migrated)
}

/// Fills the executor's grants at start, after the database opens and before the API serves:
/// migrates the old app registry `apps_file`, deletes expired rows, and loads the rest.
pub fn load_at_start(apps_file: &Path) -> Result<(), String> {
    with_db(|db| {
        match migrate_apps_file(db, apps_file) {
            Ok(0) => {}
            Ok(n) => log::info!("Moved {n} apps from {} into grants", apps_file.display()),
            // The file stays for a retry; its apps' tokens are refused until then.
            Err(e) => log::error!("Could not migrate the app registry: {e}"),
        }
        Grants::global().load(db)
    })
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

    /// Writes an app registry as executors before grants did: tokens that carry the app's
    /// `AuthInfo` as `capabilities`.
    fn old_registry(dir: &Path) -> (std::path::PathBuf, String, String) {
        use jsonwebtoken::{encode, EncodingKey, Header};
        let now = now();
        let token = |name: &str| {
            let claims = serde_json::json!({
                "iss": "did:key:the-main-agent",
                "sub": null,
                "aud": name,
                "exp": now + 3600,
                "iat": now,
                "nonce": uuid::Uuid::new_v4().to_string(),
                "capabilities": auth(name, vec![AGENT_READ_CAPABILITY.clone()]),
            });
            encode(&Header::default(), &claims, &EncodingKey::from_secret(b"k")).unwrap()
        };
        let (live, revoked) = (token("live-app"), token("revoked-app"));
        let entry = |name: &str, token: &str, revoked: bool| {
            serde_json::json!({
                "auth_info_extended": {
                    "requestId": format!("req-{name}"),
                    "auth": auth(name, vec![AGENT_READ_CAPABILITY.clone()]),
                },
                "revoked": revoked,
                "token": token,
            })
        };
        let registry = serde_json::json!({
            "req-live-app": entry("live-app", &live, false),
            "req-revoked-app": entry("revoked-app", &revoked, true),
            "req-garbage": entry("garbage", "not-a-jwt", false),
        });
        let path = dir.join("apps_data.json");
        std::fs::write(&path, registry.to_string()).unwrap();
        (path, live, revoked)
    }

    #[test]
    fn the_old_app_registry_becomes_grants_on_the_main_agent() {
        let dir = tempfile::tempdir().unwrap();
        let (path, live, revoked) = old_registry(dir.path());
        let db = Ad4mDb::new(":memory:").unwrap();

        assert_eq!(
            migrate_apps_file(&db, &path).unwrap(),
            2,
            "the garbage token is skipped"
        );
        assert!(!path.exists());
        assert!(dir.path().join("apps_data.json.migrated").exists());

        let grants = Grants::default();
        grants.load(&db).unwrap();
        let id = |token: &str| TokenId(read_stored_claims(token).unwrap().nonce().to_string());
        let app = grants
            .get(&id(&live))
            .expect("the app's token keeps working");
        assert_eq!(app.agent, AgentContext::main_agent());
        assert_eq!(app.request_id, "req-live-app");
        // The table names the main agent by `NULL`; a session reads the main agent's DID.
        assert_eq!(app.did, "");
        assert_eq!(
            format!("{:?}", app.caps),
            format!("{:?}", vec![AGENT_READ_CAPABILITY.clone()])
        );
        assert!(
            grants.get(&id(&revoked)).is_none(),
            "a revoked app stays revoked"
        );
        assert_eq!(grants.list(&AgentContext::main_agent()).len(), 2);

        // A second start finds no file and changes nothing.
        assert_eq!(migrate_apps_file(&db, &path).unwrap(), 0);
        assert_eq!(db.grants().unwrap().len(), 2);
    }

    #[test]
    fn an_old_token_with_capabilities_in_its_claims_decodes() {
        let dir = tempfile::tempdir().unwrap();
        let (_, live, _) = old_registry(dir.path());
        let claims = read_stored_claims(&live).expect("old claims decode");
        assert_eq!(claims.expires_at(), claims.issued_at() + 3600);
    }

    /// D16: grants stored before agents were keyed by DID name a managed agent by its user's
    /// email (`agent_email`) and carry `agent_did NOT NULL`. A start moves them to `agent_did`
    /// alone (`NULL` for the main agent), and their tokens still open sessions as the same
    /// agents: a login whose `sub` is the email, and an app on the main agent.
    #[test]
    fn grants_keyed_by_email_move_to_the_did_and_their_tokens_act_as_the_same_agents() {
        use crate::agent::capabilities::ListenerReach;
        use crate::agent::session::{Principal, Session};
        setup();
        let email = format!(
            "grants-by-email-{}@example.org",
            uuid::Uuid::new_v4().simple()
        );
        let did = crate::test_utils::create_test_user(&email);
        // Tokens as the executor minted them then: a login names the user by email.
        let sign = |sub: Option<String>| {
            super::super::capabilities::token::sign_token("old".to_string(), 3600, sub).unwrap()
        };
        let (login, login_claims) = sign(Some(email.clone()));
        let (login_without_did, second_claims) = sign(Some(email.clone()));
        let (app, app_claims) = sign(None);
        let (_, orphan_claims) = sign(Some("gone@example.org".to_string()));

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("ad4m.db");
        let path = path.to_str().unwrap();
        Ad4mDb::new(path)
            .unwrap()
            .add_user_prehashed(&email, &did, "hash")
            .unwrap();
        {
            let conn = rusqlite::Connection::open(path).unwrap();
            conn.execute_batch(
                "DROP TABLE grants;
                 CREATE TABLE grants (
                    id TEXT PRIMARY KEY,
                    request_id TEXT NOT NULL,
                    agent_email TEXT,
                    agent_did TEXT NOT NULL,
                    auth_info TEXT NOT NULL,
                    state TEXT NOT NULL CHECK (state IN ('active', 'revoked')),
                    expires_at INTEGER NOT NULL,
                    created_at INTEGER NOT NULL
                 );
                 CREATE INDEX idx_grants_agent_email ON grants(agent_email);",
            )
            .unwrap();
            let auth =
                serde_json::to_string(&auth("old", vec![AGENT_READ_CAPABILITY.clone()])).unwrap();
            for (claims, agent_email, agent_did) in [
                (&login_claims, Some(email.as_str()), did.as_str()),
                (&second_claims, Some(email.as_str()), ""),
                (&app_claims, None, app_claims.issuer()),
                (&orphan_claims, Some("gone@example.org"), ""),
            ] {
                conn.execute(
                    "INSERT INTO grants VALUES (?1, ?1, ?2, ?3, ?4, 'active', ?5, ?6)",
                    rusqlite::params![
                        claims.nonce(),
                        agent_email,
                        agent_did,
                        auth,
                        claims.expires_at() as i64,
                        claims.issued_at() as i64
                    ],
                )
                .unwrap();
            }
        }

        let db = Ad4mDb::new(path).unwrap();
        let columns: Vec<String> = rusqlite::Connection::open(path)
            .unwrap()
            .prepare("SELECT name FROM pragma_table_info('grants')")
            .unwrap()
            .query_map([], |row| row.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert!(!columns.contains(&"agent_email".to_string()), "{columns:?}");
        let by_id = |claims: &super::super::capabilities::Claims| {
            db.grants()
                .unwrap()
                .into_iter()
                .find(|row| row.id == claims.nonce())
                .map(|row| row.agent_did)
        };
        assert_eq!(by_id(&login_claims), Some(Some(did.clone())));
        assert_eq!(
            by_id(&second_claims),
            Some(Some(did.clone())),
            "an empty DID is filled from the user"
        );
        assert_eq!(by_id(&app_claims), Some(None), "the main agent is NULL");
        assert_eq!(
            by_id(&orphan_claims),
            None,
            "a grant no user owns is dropped"
        );

        // A second start changes nothing.
        drop(db);
        let db = Ad4mDb::new(path).unwrap();
        assert_eq!(db.grants().unwrap().len(), 3);

        Grants::global().load(&db).unwrap();
        for token in [&login, &login_without_did] {
            let session = Session::open(token, &None, ListenerReach::Network).unwrap();
            assert_eq!(
                session.principal,
                Principal::Agent(AgentContext::managed(did.clone()))
            );
            assert_eq!(session.acts_as_did(), Some(did.as_str()));
        }
        let session = Session::open(&app, &None, ListenerReach::Network).unwrap();
        assert_eq!(
            session.principal,
            Principal::Agent(AgentContext::main_agent())
        );
        assert!(session.may(&AGENT_READ_CAPABILITY));
        assert_eq!(
            Grants::global()
                .list(&AgentContext::managed(did.clone()))
                .len(),
            2
        );
    }
}
