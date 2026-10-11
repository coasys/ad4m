//! Revoked tokens, by token id.
//!
//! A revoked token stays revoked whatever happens to its app record afterwards: `removeApp`
//! deletes the record, not the revocation (#396). Each id keeps its token's expiry, so
//! [`RevokedSet::prune`] can drop an id once its token would fail the expiry check anyway.

use super::capabilities::TOKEN_EXPIRY_LEEWAY_SECONDS;
use super::session::TokenId;
use std::collections::HashMap;
use std::sync::RwLock;

lazy_static! {
    static ref REVOKED: RevokedSet = RevokedSet::default();
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
        ids.retain(|_, expires_at| expires_at.saturating_add(TOKEN_EXPIRY_LEEWAY_SECONDS) >= now);
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.0.read().unwrap().len()
    }
}

/// Fills the executor's set at start-up, before the API serves: the apps the registry file
/// marks revoked. Call it after the registry's file path is set.
pub fn load_at_start() {
    let set = RevokedSet::global();
    for (id, expires_at) in super::capabilities::apps_map::revoked_token_ids() {
        set.revoke(id, expires_at);
    }
    set.prune(now());
}

/// Revokes a token in the executor's set, and drops the ids whose tokens have expired.
pub fn revoke(id: TokenId, expires_at: u64) {
    let set = RevokedSet::global();
    set.prune(now());
    set.revoke(id, expires_at);
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

    // Apps revoked before the set existed are in the registry file only; start-up reads them.
    #[test]
    fn start_up_reads_the_revoked_apps_of_the_registry() {
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
        apps_map::remove_app(&revoked_app).unwrap();
        apps_map::remove_app(&live_app).unwrap();
    }
}
