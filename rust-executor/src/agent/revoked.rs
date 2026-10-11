//! Revoked tokens, by token id, and per user by issue time ([`SessionCutoffs`]).
//!
//! A revoked token stays revoked whatever happens to its app record afterwards: `removeApp`
//! deletes the record, not the revocation (#396). Each id keeps its token's expiry, so
//! [`RevokedSet::prune`] can drop an id once its token would fail the expiry check anyway.
//!
//! The table `revoked_tokens` keeps the set across restarts. [`revoke`] writes through to it,
//! and [`load_at_start`] reads it back. Both drop the ids of expired tokens, in memory and in
//! the table.

use super::capabilities::TOKEN_EXPIRY_LEEWAY_SECONDS;
use super::session::TokenId;
use super::session_registry::SessionRegistry;
use crate::db::{Ad4mDb, Ad4mDbResult};
use std::collections::HashMap;
use std::sync::RwLock;

lazy_static! {
    static ref REVOKED: RevokedSet = RevokedSet::default();
    static ref CUTOFFS: SessionCutoffs = SessionCutoffs::default();
}

/// Per user, the time (Unix seconds) at or before which the user's tokens are revoked:
/// `user.revokeAllSessions`. The `users` table keeps it across restarts.
#[derive(Default)]
pub struct SessionCutoffs(RwLock<HashMap<String, u64>>);

impl SessionCutoffs {
    /// The executor's cutoffs: every user session check reads them.
    pub fn global() -> &'static SessionCutoffs {
        &CUTOFFS
    }

    /// The user's cutoff, if the user revoked their sessions.
    pub fn get(&self, email: &str) -> Option<u64> {
        match self.0.read() {
            Ok(cutoffs) => cutoffs.get(email).copied(),
            Err(poisoned) => poisoned.into_inner().get(email).copied(),
        }
    }

    /// Whether a token of `email` issued at `issued_at` is revoked. A poisoned lock reads as
    /// revoked: fail closed.
    pub fn refuses(&self, email: &str, issued_at: u64) -> bool {
        self.0
            .read()
            .map(|cutoffs| {
                cutoffs
                    .get(email)
                    .is_some_and(|cutoff| issued_at <= *cutoff)
            })
            .unwrap_or(true)
    }

    /// Sets the user's cutoff. A cutoff never moves back: tokens it revoked stay revoked.
    pub fn set(&self, email: &str, cutoff: u64) {
        let mut cutoffs = match self.0.write() {
            Ok(cutoffs) => cutoffs,
            Err(poisoned) => poisoned.into_inner(),
        };
        let entry = cutoffs.entry(email.to_string()).or_insert(cutoff);
        *entry = (*entry).max(cutoff);
    }

    /// The cutoffs that `db` holds.
    pub fn load(db: &Ad4mDb) -> Ad4mDbResult<SessionCutoffs> {
        let cutoffs = SessionCutoffs::default();
        cutoffs.add_from(db)?;
        Ok(cutoffs)
    }

    fn add_from(&self, db: &Ad4mDb) -> Ad4mDbResult<()> {
        for (email, cutoff) in db.user_session_cutoffs()? {
            self.set(&email, cutoff);
        }
        Ok(())
    }
}

/// Why `revoke_all_sessions` failed.
#[derive(Debug, PartialEq, Eq)]
pub enum RevokeAllError {
    /// No user has this email.
    NoSuchUser,
    /// The cutoff holds in memory, but the database did not take it, so a restart would
    /// drop it.
    Store(String),
}

impl std::fmt::Display for RevokeAllError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevokeAllError::NoSuchUser => write!(f, "No user has this email"),
            RevokeAllError::Store(e) => write!(f, "Revoked until restart, but not saved: {e}"),
        }
    }
}

/// Revokes every token of the user `email` issued up to now, and closes their open
/// connections. Tokens issued later work: `generate_user_jwt` issues them after the cutoff.
pub fn revoke_all_sessions(email: &str) -> Result<(), RevokeAllError> {
    // Past any earlier cutoff: a login between two calls in the same second is issued at
    // the first cutoff + 1, and the second call must still reach it.
    let previous = SessionCutoffs::global().get(email);
    let cutoff = now().max(previous.map_or(0, |c| c + 1));
    let saved = Ad4mDb::global_instance()
        .lock()
        .map_err(|e| e.to_string())
        .and_then(|db| {
            let db = db.as_ref().ok_or("the database is not open")?;
            db.set_user_sessions_valid_after(email, cutoff)
                .map_err(|e| e.to_string())
        });
    if saved == Ok(false) {
        return Err(RevokeAllError::NoSuchUser);
    }
    // The cutoff holds in memory even when the database write failed: fail closed.
    SessionCutoffs::global().set(email, cutoff);
    // After the cutoff is set: a connection that registers later fails its own check.
    SessionRegistry::global().end_user(email);
    saved.map(|_| ()).map_err(RevokeAllError::Store)
}

/// Token ids the operator revoked, each with its token's expiry (Unix seconds).
#[derive(Default)]
pub struct RevokedSet(RwLock<HashMap<TokenId, u64>>);

impl RevokedSet {
    /// The executor's set: every revocation goes here, and every session check reads it.
    pub fn global() -> &'static RevokedSet {
        &REVOKED
    }

    /// Revokes `id`. `expires_at` is the token's expiry; the id stays until then.
    pub fn revoke(&self, id: TokenId, expires_at: u64) {
        match self.0.write() {
            Ok(mut ids) => ids.insert(id, expires_at),
            Err(poisoned) => poisoned.into_inner().insert(id, expires_at),
        };
    }

    /// Whether `id` is revoked. A poisoned lock reads as revoked: fail closed.
    pub fn contains(&self, id: &TokenId) -> bool {
        self.0
            .read()
            .map(|ids| ids.contains_key(id))
            .unwrap_or(true)
    }

    /// Drops every id whose token expired before `now`, past the leeway that the expiry check
    /// allows: such a token fails that check, revoked or not.
    pub fn prune(&self, now: u64) {
        let mut ids = match self.0.write() {
            Ok(ids) => ids,
            Err(poisoned) => poisoned.into_inner(),
        };
        ids.retain(|_, expires_at| *expires_at >= prune_cutoff(now));
    }

    /// The set that `db` holds at `now`. Deletes the rows of expired tokens first.
    pub fn load(db: &Ad4mDb, now: u64) -> Ad4mDbResult<RevokedSet> {
        let set = RevokedSet::default();
        set.add_from(db, now)?;
        Ok(set)
    }

    /// Adds the ids that `db` holds at `now`, after it deletes the rows of expired tokens.
    fn add_from(&self, db: &Ad4mDb, now: u64) -> Ad4mDbResult<()> {
        db.delete_revoked_tokens_expired_before(prune_cutoff(now))?;
        for (id, expires_at) in db.revoked_tokens()? {
            self.revoke(TokenId(id), expires_at);
        }
        Ok(())
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.0.read().unwrap().len()
    }
}

/// An id whose token expired before this, leeway included, can go.
fn prune_cutoff(now: u64) -> u64 {
    now.saturating_sub(TOKEN_EXPIRY_LEEWAY_SECONDS)
}

/// Fills the executor's set at start-up, after the database opens and before the API serves:
/// the table's rows, the users' session cutoffs, and the apps the registry file marks
/// revoked. Apps revoked before the table existed are in the file only; this writes them to
/// the table, so they stay revoked after `removeApp`. Call it after the registry's file path
/// is set.
pub fn load_at_start() -> Result<(), String> {
    Ad4mDb::with_global_instance(|db| {
        RevokedSet::global().add_from(db, now())?;
        SessionCutoffs::global().add_from(db)
    })
    .map_err(|e| e.to_string())?;
    for (id, expires_at) in super::capabilities::apps_map::revoked_token_ids() {
        revoke(id, expires_at)?;
    }
    Ok(())
}

/// Revokes a token in the executor's set and in the database, drops the ids whose tokens
/// have expired from both, and closes the token's open connections. The set takes the id
/// even when the database write fails, so the token stays revoked until a restart.
pub fn revoke(id: TokenId, expires_at: u64) -> Result<(), String> {
    let now = now();
    let set = RevokedSet::global();
    set.prune(now);
    set.revoke(id.clone(), expires_at);
    // After the set takes the id: a connection that registers later fails its own check.
    SessionRegistry::global().end(&id);

    let db = Ad4mDb::global_instance();
    let db = db.lock().map_err(|e| e.to_string())?;
    let db = db.as_ref().ok_or("the database is not open")?;
    db.delete_revoked_tokens_expired_before(prune_cutoff(now))
        .and_then(|_| db.add_revoked_token(&id.0, expires_at))
        .map_err(|e| e.to_string())
}

/// Seconds since the Unix epoch.
pub(crate) fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(u64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(s: &str) -> TokenId {
        TokenId(s.to_string())
    }

    #[test]
    fn a_revoked_id_is_contained_and_others_are_not() {
        let set = RevokedSet::default();
        set.revoke(id("a"), 2_000);
        assert!(set.contains(&id("a")));
        assert!(!set.contains(&id("b")));
    }

    #[test]
    fn prune_drops_only_ids_whose_tokens_expired_past_the_leeway() {
        let set = RevokedSet::default();
        let now = 10_000;
        set.revoke(id("long-expired"), now - TOKEN_EXPIRY_LEEWAY_SECONDS - 1);
        set.revoke(id("inside-leeway"), now - TOKEN_EXPIRY_LEEWAY_SECONDS);
        set.revoke(id("live"), now + 3600);

        set.prune(now);
        assert!(!set.contains(&id("long-expired")));
        assert!(set.contains(&id("inside-leeway")), "the token still passes");
        assert!(set.contains(&id("live")));
        assert_eq!(set.len(), 2);
    }

    // A restart builds a fresh set from the database: the revocation must be in it.
    #[test]
    fn a_revocation_survives_a_reload_from_the_database() {
        crate::test_utils::use_test_apps_file();
        let revoked = id(&uuid::Uuid::new_v4().to_string());
        revoke(revoked.clone(), now() + 3600).unwrap();

        let reloaded = Ad4mDb::with_global_instance(|db| RevokedSet::load(db, now())).unwrap();
        assert!(reloaded.contains(&revoked));
        assert!(!reloaded.contains(&id("never-revoked")));
    }

    #[test]
    fn a_reopened_database_file_keeps_its_revocations() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("revoked.sqlite");
        let path = path.to_str().unwrap();
        Ad4mDb::new(path)
            .unwrap()
            .add_revoked_token("kept", now() + 3600)
            .unwrap();

        let reopened = Ad4mDb::new(path).unwrap();
        assert!(RevokedSet::load(&reopened, now())
            .unwrap()
            .contains(&id("kept")));
    }

    #[test]
    fn loading_drops_the_rows_of_expired_tokens() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let now = 10_000;
        db.add_revoked_token("expired", now - TOKEN_EXPIRY_LEEWAY_SECONDS - 1)
            .unwrap();
        db.add_revoked_token("inside-leeway", now - TOKEN_EXPIRY_LEEWAY_SECONDS)
            .unwrap();
        db.add_revoked_token("live", now + 3600).unwrap();

        let set = RevokedSet::load(&db, now).unwrap();
        assert!(!set.contains(&id("expired")));
        assert!(set.contains(&id("inside-leeway")));
        assert!(set.contains(&id("live")));

        let mut rows: Vec<String> = db
            .revoked_tokens()
            .unwrap()
            .into_iter()
            .map(|(id, _)| id)
            .collect();
        rows.sort();
        assert_eq!(
            rows,
            ["inside-leeway", "live"],
            "the expired row is deleted"
        );
    }

    // Apps revoked before the table existed are in the registry file only; start-up reads
    // them and writes them to the table, so they stay revoked after `removeApp`.
    #[test]
    fn start_up_moves_the_revoked_apps_of_the_registry_into_the_table() {
        use crate::agent::capabilities::{
            apps_map, generate_jwt, read_stored_token_id, AuthInfo, AuthInfoExtended,
        };
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let app = |name: &str| {
            let token = generate_jwt(name.to_string(), 3600, AuthInfo::default()).unwrap();
            let request_id = format!("{name}-{}", uuid::Uuid::new_v4());
            let info = AuthInfoExtended {
                request_id: request_id.clone(),
                auth: AuthInfo::default(),
            };
            apps_map::insert_app(request_id.clone(), info, token.clone()).unwrap();
            let id = read_stored_token_id(&token).unwrap().0;
            (request_id, id)
        };
        let (revoked_app, revoked_id) = app("revoked");
        let (live_app, live_id) = app("live");
        apps_map::revoke_app(&revoked_app).unwrap();

        let ids: Vec<TokenId> = apps_map::revoked_token_ids()
            .into_iter()
            .map(|(id, _)| id)
            .collect();
        assert!(ids.contains(&revoked_id));
        assert!(!ids.contains(&live_id));

        // An executor from before the table: the revocation is in the registry file only.
        Ad4mDb::with_global_instance(|db| db.delete_revoked_tokens_expired_before(u64::MAX))
            .unwrap();
        load_at_start().unwrap();
        let table = Ad4mDb::with_global_instance(|db| RevokedSet::load(db, now())).unwrap();
        assert!(table.contains(&revoked_id));
        assert!(!table.contains(&live_id));
        apps_map::remove_app(&revoked_app).unwrap();
        apps_map::remove_app(&live_app).unwrap();
    }
}
