pub mod apps_map;
pub mod defs;
pub mod pending;
pub mod token;
pub mod types;

pub use defs::*;
use pending::PendingApprovals;
pub use pending::MAX_CODE_ATTEMPTS;
pub use token::*;
#[allow(ambiguous_glob_reexports)]
pub use types::*;

use crate::agent::revoked::now;
use crate::events::{Audience, Event, Topic};
use crate::pubsub::get_global_pubsub;
use crate::secret::Secret;
use crate::types::*;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub const DEFAULT_TOKEN_VALID_PERIOD: u64 = 180 * 24 * 60 * 60; // 180 days in seconds

// Cache for last_seen timestamps to avoid repeated database lookups
// Maps user_email -> (last_checked_timestamp, last_seen_value)
#[derive(Clone)]
struct LastSeenCacheEntry {
    last_checked: i64,    // When we last checked the database
    last_seen_value: i64, // The last_seen value we got from DB
}

lazy_static! {
    static ref LAST_SEEN_CACHE: Arc<RwLock<HashMap<String, LastSeenCacheEntry>>> =
        Arc::new(RwLock::new(HashMap::new()));
}

const CACHE_TTL_SECONDS: i64 = 300; // 5 minutes cache TTL

/// Minimum interval (seconds) between two `users.last_seen` writes for the same
/// user, so an active user's `last_seen` can be this stale. The auto-processor
/// supervisor's online window depends on it
/// ([`crate::perspectives::auto_processor::watcher::MANAGED_USER_ONLINE_WINDOW_S`], #1070).
pub const LAST_SEEN_WRITE_THROTTLE_S: i64 = 300;

/// Who can connect to the listener a request arrived on.
///
/// Without an admin credential the executor runs the single-user local trust model: a caller
/// with no token is the operator. That premise holds on a loopback listener and nowhere else,
/// so it is checked per listener instead of assumed (#1059). The API server attaches each
/// listener's reach where it binds it (`api::listener_router`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListenerReach {
    /// Bound to a loopback address: only processes on this machine connect, including any
    /// proxy or tunnel running here. The API reads a request that carries a `Forwarded`,
    /// `X-Forwarded-For` or `X-Real-IP` header as `Network` (`api::auth::listener_reach`). A
    /// proxy that sets none of them (a raw TCP forward, `ssh -R`) still reads as `Loopback`,
    /// so its remote clients are the operator: a node behind a proxy needs an admin credential.
    Loopback,
    /// Bound to any other address: whoever can route to it connects.
    Network,
}

impl ListenerReach {
    pub fn of(addr: &std::net::SocketAddr) -> Self {
        if addr.ip().is_loopback() {
            ListenerReach::Loopback
        } else {
            ListenerReach::Network
        }
    }
}

/// Returns true if the given token is the admin_credential that grants launcher-level access.
/// When admin_credential is Some, the token must match it exactly (constant-time).
/// When admin_credential is None (legacy single-user mode), an empty token is treated as admin,
/// but only on a loopback listener. On a network listener it is an anonymous caller (#1059).
pub fn is_admin_credential_token_on(
    token: &str,
    admin_credential: &Option<Secret>,
    reach: ListenerReach,
) -> bool {
    match admin_credential {
        Some(cred) => cred.expose_for_compare(token),
        None => token.is_empty() && reach == ListenerReach::Loopback,
    }
}

pub fn check_capability(
    capabilities: &Result<Vec<Capability>, String>,
    expected: &Capability,
) -> Result<(), String> {
    let capabilities = capabilities.as_ref().map_err(|e| e.clone())?;
    if !capability_matches(capabilities, expected) {
        return Err(format!(
            "Capability is not matched, you have capabilities: {:?}, expected: {:?}",
            capabilities, expected
        ));
    }
    Ok(())
}

/// Whether any of `capabilities` covers `expected`: the same domain, every pointer and every
/// action, each of which `*` matches.
pub fn capability_matches(capabilities: &[Capability], expected: &Capability) -> bool {
    capabilities.iter().any(|cap| {
        (cap.with.domain == WILD_CARD || cap.with.domain == expected.with.domain)
            && (cap.with.pointers.contains(&WILD_CARD.to_string())
                || expected
                    .with
                    .pointers
                    .iter()
                    .all(|p| cap.with.pointers.contains(p)))
            && (cap.can.contains(&WILD_CARD.to_string())
                || expected.can.iter().all(|c| cap.can.contains(c)))
    })
}

/// Seconds of clock skew that JWT validation allows (jsonwebtoken's default leeway).
pub(crate) const TOKEN_EXPIRY_LEEWAY_SECONDS: u64 = 60;

/// Update the last_seen timestamp of the user the caller's session names.
/// Throttled to one write per [`LAST_SEEN_WRITE_THROTTLE_S`] to reduce database writes
/// Uses an in-memory cache to avoid blocking the async runtime with repeated DB lookups
pub async fn track_last_seen(user_email: &str) {
    use crate::db::Ad4mDb;

    let user_email = user_email.to_string();
    let now = chrono::Utc::now().timestamp();

    // Check cache first (non-blocking read)
    {
        let cache = LAST_SEEN_CACHE.read().await;
        if let Some(entry) = cache.get(&user_email) {
            let cache_age = now - entry.last_checked;
            if cache_age < CACHE_TTL_SECONDS {
                // Cache is fresh, check if update is needed based on cached value
                let time_since_last_seen = now - entry.last_seen_value;
                if time_since_last_seen < LAST_SEEN_WRITE_THROTTLE_S {
                    // Still inside the throttle period, no need to update
                    log::trace!(
                        "last_seen tracking for {}: cache hit, no update needed (last_seen={}, age={}s)",
                        user_email, entry.last_seen_value, time_since_last_seen
                    );
                    return;
                }
            }
        }
    }

    // Cache miss or stale - need to check database
    // Use spawn_blocking to avoid blocking the async runtime
    let user_email_clone = user_email.clone();
    let should_update = tokio::task::spawn_blocking(move || {
        Ad4mDb::with_global_instance(|db| {
            if let Ok(user) = db.get_user(&user_email_clone) {
                if let Some(last_seen) = user.last_seen {
                    let throttle_cutoff = now.saturating_sub(LAST_SEEN_WRITE_THROTTLE_S);

                    // Handle unrealistic future timestamps by treating them as stale
                    // (allow some clock skew tolerance of 1 minute)
                    let should_update = if last_seen > now + 60 {
                        log::warn!(
                            "last_seen tracking for {}: unrealistic future timestamp {}, treating as stale",
                            user_email_clone, last_seen
                        );
                        true
                    } else {
                        last_seen < throttle_cutoff
                    };

                    log::trace!("last_seen tracking for {}: last_seen={}, throttle_cutoff={}, should_update={}",
                        user_email_clone, last_seen, throttle_cutoff, should_update);
                    (should_update, Some(last_seen))
                } else {
                    log::debug!(
                        "last_seen tracking for {}: never seen before, updating now",
                        user_email_clone
                    );
                    (true, None) // Never updated, do it now
                }
            } else {
                log::warn!(
                    "last_seen tracking: user {} not found in database",
                    user_email_clone
                );
                (false, None) // User not found
            }
        })
    })
    .await;

    let (should_update, last_seen_value) = match should_update {
        Ok((update, value)) => (update, value),
        Err(e) => {
            log::error!(
                "Failed to check last_seen status (spawn_blocking join error): {:?}",
                e
            );
            return;
        }
    };

    // Update cache with the value we got from DB
    if let Some(last_seen_val) = last_seen_value {
        let mut cache = LAST_SEEN_CACHE.write().await;
        cache.insert(
            user_email.clone(),
            LastSeenCacheEntry {
                last_checked: now,
                last_seen_value: last_seen_val,
            },
        );
    }

    if should_update {
        log::debug!("Updating last_seen for user: {}", user_email);

        // Perform the update in spawn_blocking
        let user_email_for_update = user_email.clone();
        let update_result = tokio::task::spawn_blocking(move || {
            Ad4mDb::with_global_instance(|db| db.update_user_last_seen(&user_email_for_update))
        })
        .await;

        match update_result {
            Ok(Ok(())) => {
                // Update succeeded, refresh cache with new timestamp
                let mut cache = LAST_SEEN_CACHE.write().await;
                cache.insert(
                    user_email,
                    LastSeenCacheEntry {
                        last_checked: now,
                        last_seen_value: now,
                    },
                );
            }
            Ok(Err(e)) => {
                log::error!(
                    "Failed to update last_seen for user {}: {:?}",
                    user_email,
                    e
                );
            }
            Err(e) => {
                log::error!(
                    "Failed to update last_seen for user {} (spawn_blocking join error): {:?}",
                    user_email,
                    e
                );
            }
        }
    }
}

/// What a caller without a token may do: request a capability and read whether multi-user
/// mode is on, plus sign up, log in and verify an email when it is. Reading users is left out,
/// so anonymous callers cannot list them.
pub fn anonymous_capabilities() -> Vec<Capability> {
    use crate::db::Ad4mDb;
    anonymous_capabilities_when(Ad4mDb::with_global_instance(|db| {
        db.get_multi_user_enabled().unwrap_or(false)
    }))
}

/// [`anonymous_capabilities`] with multi-user mode on or off.
pub fn anonymous_capabilities_when(multi_user_enabled: bool) -> Vec<Capability> {
    if multi_user_enabled {
        vec![
            AGENT_AUTH_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_VERIFY_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
        ]
    } else {
        vec![
            AGENT_AUTH_CAPABILITY.clone(),
            RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY.clone(),
        ]
    }
}

pub async fn request_capability(auth_info: AuthInfo) -> String {
    let request_id = uuid::Uuid::new_v4().to_string();
    let app_name = auth_info.app_name.clone();

    let auth_extended = AuthInfoExtended {
        request_id: request_id.clone(),
        auth: auth_info,
    };

    let exception_info = ExceptionInfo {
        title: "Request to authenticate application".to_string(),
        message: format!(
            "{} is waiting for authentication, open the ADAM Launcher for more information.",
            app_name
        ),
        r#type: ExceptionType::CapabilityRequested,
        addon: Some(serde_json::to_string(&auth_extended).unwrap()),
    };

    PendingApprovals::global().request(request_id.clone(), auth_extended.auth, now());
    get_global_pubsub()
        .await
        .publish_event(Event::exception(&exception_info));

    request_id
}

/// The operator approves a pending request: returns the code the app redeems, for the
/// operator only.
pub fn permit_capability(auth_info_extended: AuthInfoExtended) -> Result<String, String> {
    let code = gen_random_digits();
    PendingApprovals::global().approve(
        &auth_info_extended.request_id,
        auth_info_extended.auth,
        code.clone(),
        now(),
    )?;
    Ok(code)
}

/// Redeems a permitted request's code for the app's token. The token goes only to the
/// caller: the app registry keeps its id.
pub async fn generate_capability_token(request_id: String, rand: String) -> Result<Secret, String> {
    let pending = PendingApprovals::global();
    let approval = pending
        .redeem(&request_id, &rand, now())
        .map_err(|e| e.to_string())?;
    let auth = approval.auth().clone();

    let issued = match token::issue_jwt(
        auth.app_name.clone(),
        DEFAULT_TOKEN_VALID_PERIOD,
        auth.clone(),
    ) {
        Ok(issued) => issued,
        Err(e) => {
            pending.restore(request_id, approval);
            return Err(e.to_string());
        }
    };

    let auth_for_publish = auth.clone();

    // A registry that does not take the app leaves no record of it; the token is dropped
    // undelivered, and the app retries with the same code.
    if let Err(e) = apps_map::insert_app(
        request_id.clone(),
        AuthInfoExtended {
            request_id: request_id.clone(),
            auth,
        },
        issued.id,
        issued.expires_at,
    ) {
        pending.restore(request_id, approval);
        return Err(e);
    }

    // The event reaches every session that can approve apps; the token goes only to the caller.
    let apps_changed = Apps {
        auth: auth_for_publish,
        request_id: request_id.clone(),
        revoked: Some(false),
        token: String::new(),
    };
    get_global_pubsub().await.publish_event(Event::new(
        Topic::AppsChanged,
        Audience::Approvers,
        Some(apps_changed),
    ));

    Ok(issued.jwt)
}

pub fn gen_random_digits() -> String {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    rng.gen_range(100000..1000000).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_capability_is_expected() {
        let all_capability = &ALL_CAPABILITY;
        assert_eq!(all_capability.with.domain, "*");
        assert_eq!(all_capability.with.pointers, vec!["*"]);
        assert_eq!(all_capability.can, vec!["*"]);
    }

    #[test]
    fn agent_auth_capability_is_expected() {
        let agent_auth_capability = &AGENT_AUTH_CAPABILITY;
        assert_eq!(agent_auth_capability.with.domain, "agent");
        assert_eq!(agent_auth_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_auth_capability.can, vec!["AUTHENTICATE"]);
    }

    #[test]
    fn agent_read_capability_is_expected() {
        let agent_read_capability = &AGENT_READ_CAPABILITY;
        assert_eq!(agent_read_capability.with.domain, "agent");
        assert_eq!(agent_read_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_read_capability.can, vec!["READ"]);
    }

    #[test]
    fn runtime_user_management_read_enabled_capability_is_expected() {
        let capability = &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY;
        assert_eq!(capability.with.domain, "runtime.user_management");
        assert_eq!(capability.with.pointers, vec!["enabled"]);
        assert_eq!(capability.can, vec!["READ"]);
    }

    #[test]
    fn runtime_user_management_login_capability_is_expected() {
        let capability = &RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY;
        assert_eq!(capability.with.domain, "runtime.user_management");
        assert_eq!(capability.with.pointers, vec!["*"]);
        assert_eq!(capability.can, vec!["LOGIN"]);
    }

    #[test]
    fn agent_create_capability_is_expected() {
        let agent_create_capability = &AGENT_CREATE_CAPABILITY;
        assert_eq!(agent_create_capability.with.domain, "agent");
        assert_eq!(agent_create_capability.with.pointers, vec!["*"]);
        assert_eq!(agent_create_capability.can, vec!["CREATE"]);
    }

    #[test]
    fn query_capability_is_expected() {
        let capability = perspective_query_capability(vec!["123".to_string(), "456".to_string()]);
        assert_eq!(capability.with.domain, "perspective");
        assert_eq!(capability.with.pointers, vec!["123", "456"]);
        assert_eq!(capability.can, vec!["READ"]);
    }

    #[test]
    fn agent_with_all_capability_can_permit_an_auth_request() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_PERMIT_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_all_capability_can_request_agent_status() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_READ_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_all_capability_can_mutate_the_agent() {
        assert!(
            check_capability(&Ok(vec![ALL_CAPABILITY.clone()]), &AGENT_CREATE_CAPABILITY).is_ok()
        );
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_request_the_agent_status() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_READ_CAPABILITY
        )
        .is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_cannot_mutate_the_agent() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_CREATE_CAPABILITY
        )
        .is_err());
    }

    #[test]
    fn agent_with_agent_auth_capability_can_request_an_auth() {
        assert!(check_capability(
            &Ok(vec![AGENT_AUTH_CAPABILITY.clone()]),
            &AGENT_AUTH_CAPABILITY
        )
        .is_ok());
    }

    #[test]
    fn agent_with_agent_read_capability_can_request_the_agent_status() {
        assert!(check_capability(
            &Ok(vec![AGENT_READ_CAPABILITY.clone()]),
            &AGENT_READ_CAPABILITY
        )
        .is_ok());
    }

    #[test]
    fn agent_with_perspective_query_capability_can_query_a_perspective() {
        let query_capability = perspective_query_capability(vec!["*".to_string()]);
        let expected_capability = perspective_query_capability(vec!["123".to_string()]);
        assert!(check_capability(&Ok(vec![query_capability]), &expected_capability).is_ok());
    }

    #[test]
    fn gen_random_digits_returns_a_6_digit_string() {
        let rand = gen_random_digits();
        assert!(rand.len() == 6 && rand.chars().all(|c| c.is_ascii_digit()));
    }

    #[test]
    fn a_listener_is_loopback_only_on_a_loopback_address() {
        let reach = |addr: &str| ListenerReach::of(&addr.parse().unwrap());
        assert_eq!(reach("127.0.0.1:12000"), ListenerReach::Loopback);
        assert_eq!(reach("[::1]:12000"), ListenerReach::Loopback);
        assert_eq!(reach("0.0.0.0:12000"), ListenerReach::Network);
        assert_eq!(reach("[::]:12000"), ListenerReach::Network);
        assert_eq!(reach("192.168.1.5:12000"), ListenerReach::Network);
    }

    // #1059: without an admin credential, an empty token is the operator only on loopback.
    // On the network it gets what an anonymous caller gets on a node with a credential:
    // enough to request a capability, nothing to read with.
    #[test]
    fn without_a_credential_an_empty_token_is_the_operator_only_on_loopback() {
        let empty = String::new();
        assert!(is_admin_credential_token_on(
            &empty,
            &None,
            ListenerReach::Loopback
        ));
        assert!(!is_admin_credential_token_on(
            &empty,
            &None,
            ListenerReach::Network
        ));

        let open = |reach| crate::agent::session::Session::open(&empty, &None, reach).unwrap();
        assert!(open(ListenerReach::Loopback).may(&AGENT_READ_CAPABILITY));

        // The anonymous set reads the multi-user setting.
        let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        let remote = open(ListenerReach::Network);
        assert!(!remote.may(&AGENT_READ_CAPABILITY));
        assert!(remote.may(&AGENT_AUTH_CAPABILITY));
    }

    // The admin credential works on any listener: it is what makes a network listener safe.
    #[test]
    fn the_admin_credential_is_the_operator_on_the_network() {
        let credential = Some(Secret::from("the-admin-credential"));
        assert!(is_admin_credential_token_on(
            "the-admin-credential",
            &credential,
            ListenerReach::Network
        ));
        assert!(!is_admin_credential_token_on(
            "the-admin-credential-",
            &credential,
            ListenerReach::Network
        ));
        let session = crate::agent::session::Session::open(
            "the-admin-credential",
            &credential,
            ListenerReach::Network,
        )
        .unwrap();
        assert!(session.may(&AGENT_READ_CAPABILITY));
    }

    // A caller without a token may always check the multi-user setting and request a
    // capability, and may log in, sign up and verify an email only in multi-user mode.
    #[test]
    fn anonymous_callers_may_check_the_mode_and_log_in_only_in_multi_user_mode() {
        for multi_user in [false, true] {
            let caps = anonymous_capabilities_when(multi_user);
            let may = |cap: &Capability| check_capability(&Ok(caps.clone()), cap).is_ok();
            assert!(may(&RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY));
            assert!(may(&AGENT_AUTH_CAPABILITY));
            assert_eq!(may(&RUNTIME_USER_MANAGEMENT_LOGIN_CAPABILITY), multi_user);
            assert_eq!(may(&RUNTIME_USER_MANAGEMENT_CREATE_CAPABILITY), multi_user);
            assert!(!may(&AGENT_READ_CAPABILITY));
        }
    }
}

#[cfg(test)]
mod app_token_tests {
    use super::*;
    use std::time::Duration;

    fn test_auth_info() -> AuthInfo {
        AuthInfo {
            app_name: "Test app".to_string(),
            app_desc: "An app for tests".to_string(),
            app_domain: Some("test.example".to_string()),
            app_url: Some("https://test.example".to_string()),
            app_icon_path: None,
            capabilities: Some(vec![ALL_CAPABILITY.clone()]),
            user_email: None,
        }
    }

    async fn permitted_request() -> (String, String) {
        let request_id = request_capability(test_auth_info()).await;
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: test_auth_info(),
        })
        .unwrap();
        (request_id, code)
    }

    // The code has six digits. Without a limit, anyone who learned a request id could try
    // every code and receive the app's token.
    #[tokio::test]
    async fn a_request_stops_accepting_codes_after_too_many_wrong_ones() {
        let (request_id, code) = permitted_request().await;
        let wrong = if code == "100000" { "100001" } else { "100000" };
        let mut last_err = String::new();
        for _ in 0..MAX_CODE_ATTEMPTS {
            last_err = generate_capability_token(request_id.clone(), wrong.to_string())
                .await
                .unwrap_err();
        }
        assert!(last_err.contains("Too many wrong codes"), "{}", last_err);
        assert!(generate_capability_token(request_id.clone(), code)
            .await
            .is_err());
    }

    // `agent.generateJwt` needs no token, so anyone can send made-up request ids. They must
    // not leave entries behind, or repeated calls fill memory.
    #[tokio::test]
    async fn made_up_request_ids_leave_no_entries() {
        let ids: Vec<String> = (0..10_000).map(|i| format!("made-up-{i}")).collect();
        for id in &ids {
            let err = generate_capability_token(id.clone(), "100000".to_string())
                .await
                .unwrap_err();
            assert_eq!(err, "Can't find permitted request");
        }
        assert!(!ids.iter().any(|id| PendingApprovals::global().contains(id)));
    }

    // The request goes once it runs out of code attempts.
    #[tokio::test]
    async fn a_locked_out_request_is_deleted() {
        let (request_id, code) = permitted_request().await;
        assert!(PendingApprovals::global().contains(&request_id));
        let wrong = if code == "100000" { "100001" } else { "100000" };
        for _ in 0..MAX_CODE_ATTEMPTS {
            let _ = generate_capability_token(request_id.clone(), wrong.to_string()).await;
        }
        assert!(!PendingApprovals::global().contains(&request_id));
    }

    // Request ids are UUIDs, which contain dashes; requests used to be stored under
    // `{id}-{code}`. Wrong codes sent for only the start of someone else's id must not lock
    // that request out.
    #[tokio::test]
    async fn wrong_codes_for_a_prefix_of_an_id_leave_that_request_alone() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let (request_id, code) = permitted_request().await;
        let prefix = request_id.split('-').next().unwrap().to_string();
        assert_ne!(prefix, request_id);
        for _ in 0..MAX_CODE_ATTEMPTS {
            let _ = generate_capability_token(prefix.clone(), "100000".to_string()).await;
        }
        assert!(generate_capability_token(request_id, code).await.is_ok());
    }

    #[tokio::test]
    async fn a_correct_code_after_a_few_wrong_ones_still_works() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let (request_id, code) = permitted_request().await;
        let wrong = if code == "100000" { "100001" } else { "100000" };
        for _ in 0..MAX_CODE_ATTEMPTS - 1 {
            assert!(
                generate_capability_token(request_id.clone(), wrong.to_string())
                    .await
                    .is_err()
            );
        }
        assert!(generate_capability_token(request_id, code).await.is_ok());
    }

    // A code redeems once. A token that cannot be issued (a locked wallet) uses up nothing:
    // the app retries with the same code.
    #[tokio::test]
    async fn a_code_redeems_once_and_a_locked_wallet_keeps_the_request() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let (request_id, code) = permitted_request().await;

        let backend = crate::wallet::wallet_backend();
        backend.lock("pending-locked-test");
        let locked = generate_capability_token(request_id.clone(), code.clone()).await;
        backend
            .unlock("pending-locked-test")
            .expect("restore the shared wallet");
        assert!(locked.unwrap_err().contains("Executor is locked"));
        assert!(PendingApprovals::global().contains(&request_id));

        assert!(generate_capability_token(request_id.clone(), code.clone())
            .await
            .is_ok());
        assert!(!PendingApprovals::global().contains(&request_id));
        assert_eq!(
            generate_capability_token(request_id.clone(), code)
                .await
                .unwrap_err(),
            "Can't find permitted request"
        );
        apps_map::remove_app(&request_id).unwrap();
    }

    // A registry file that does not take the app: the token is never delivered, so no record
    // may name it, and the app retries with the same code.
    #[tokio::test]
    async fn a_registry_that_cannot_be_saved_keeps_the_request_and_no_app() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let (request_id, code) = permitted_request().await;

        apps_map::set_data_file_path("/nonexistent-dir-for-insert-test/apps.json".to_string());
        let unsaved = generate_capability_token(request_id.clone(), code.clone()).await;
        crate::test_utils::use_test_apps_file();

        assert!(unsaved.is_err());
        assert!(apps_map::get_app(&request_id).unwrap().is_none());
        assert!(PendingApprovals::global().has_code(&request_id, &code));

        let token = generate_capability_token(request_id.clone(), code)
            .await
            .expect("the same code redeems once the registry saves")
            .expose_once();
        assert!(apps_map::get_app(&request_id).unwrap().is_some());
        assert!(capabilities_from_token(token, &None).is_ok());
        apps_map::remove_app(&request_id).unwrap();
    }

    // The `apps-changed` event and the app list reach sessions other than the app, so
    // neither may carry the token. The executor does not store it either: the registry
    // keeps the token's id, which is enough to revoke it.
    #[tokio::test]
    async fn app_events_the_app_list_and_the_registry_carry_no_token() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let mut events = get_global_pubsub().await.subscribe_events();

        let (request_id, code) = permitted_request().await;
        let token = generate_capability_token(request_id.clone(), code)
            .await
            .unwrap()
            .expose_once();
        assert!(!token.is_empty());

        let event = loop {
            let event = tokio::time::timeout(Duration::from_secs(5), events.recv())
                .await
                .expect("an apps-changed event arrives")
                .unwrap();
            if event.topic != Topic::AppsChanged {
                continue;
            }
            assert_eq!(event.audience, Audience::Approvers);
            if let Ok(Some(app)) = serde_json::from_value::<Option<Apps>>(event.payload.clone()) {
                if app.request_id == request_id {
                    break app;
                }
            }
        };
        assert!(event.token.is_empty(), "the event carried the app token");

        let apps = apps_map::get_apps();
        let listed = apps
            .iter()
            .find(|app| app.request_id == request_id)
            .expect("the app appears in the list");
        assert!(
            listed.token.is_empty(),
            "the app list carried the app token"
        );
        assert!(!serde_json::to_string(&apps).unwrap().contains(&token));
        let record = apps_map::get_app(&request_id).unwrap().unwrap();
        assert!(!format!("{record:?}").contains(&token));
        let file = std::fs::read_to_string(apps_map::get_data_file_path()).unwrap();
        assert!(file.contains(&request_id) && !file.contains(&token));

        let open = |token: &str| {
            crate::agent::session::Session::open(token, &None, ListenerReach::Loopback)
        };
        assert!(open(&token).is_ok());
        apps_map::revoke_app(&request_id).unwrap();
        assert!(open(&token).is_err());
    }
}
