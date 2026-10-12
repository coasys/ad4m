pub mod defs;
pub mod pending;
pub mod token;
pub mod types;

pub use defs::*;
use pending::PendingRequests;
pub use pending::RequestTarget;
pub use pending::MAX_CODE_ATTEMPTS;
pub use token::*;
#[allow(ambiguous_glob_reexports)]
pub use types::*;

use crate::agent::grants::now;
use crate::agent::session::Session;
use crate::agent::AgentContext;
use crate::events::Event;
use crate::pubsub::get_global_pubsub;
use crate::secret::Secret;
use crate::types::*;

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub const DEFAULT_TOKEN_VALID_PERIOD: u64 = 180 * 24 * 60 * 60; // 180 days in seconds

// Cache for last_seen timestamps to avoid repeated database lookups
// Maps agent DID -> (last_checked_timestamp, last_seen_value)
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
    /// so its remote clients are the operator: an executor behind a proxy needs an admin credential.
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

/// [`is_admin_credential_token_on`] for a caller on a loopback listener: for the MCP server,
/// where [`capabilities_from_token`] explains why loopback holds.
pub fn is_admin_credential_token(token: &str, admin_credential: &Option<Secret>) -> bool {
    is_admin_credential_token_on(token, admin_credential, ListenerReach::Loopback)
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
    require_capability(capabilities.as_ref().map_err(|e| e.clone())?, expected)
}

/// `Ok` if any of `capabilities` covers `expected`; otherwise the error a refused call reports.
pub fn require_capability(
    capabilities: &[Capability],
    expected: &Capability,
) -> Result<(), String> {
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

/// The grant of a decoded token while it lets the token act; `Err` when the token has no
/// grant, its grant was revoked, removed or expired, its `sub` names another agent, or its
/// agent has no identity. For MCP, which resolves its tokens without a `Session`; it applies
/// the checks `Session::open` applies ([`crate::agent::session::grant_for`]).
pub fn granted(claims: &Claims) -> Result<std::sync::Arc<crate::agent::grants::LiveGrant>, String> {
    crate::agent::session::grant_for(claims).map_err(|e| e.to_string())
}

/// Seconds of clock skew that JWT validation allows (jsonwebtoken's default leeway).
pub(crate) const TOKEN_EXPIRY_LEEWAY_SECONDS: u64 = 60;

/// Update the last_seen timestamp of the account that owns the managed agent `agent_did`, the
/// agent the caller's session acts as.
/// Throttled to one write per [`LAST_SEEN_WRITE_THROTTLE_S`] to reduce database writes
/// Uses an in-memory cache to avoid blocking the async runtime with repeated DB lookups
pub async fn track_last_seen(agent_did: &str) {
    use crate::db::Ad4mDb;

    let agent_did = agent_did.to_string();
    let now = chrono::Utc::now().timestamp();

    // Check cache first (non-blocking read)
    {
        let cache = LAST_SEEN_CACHE.read().await;
        if let Some(entry) = cache.get(&agent_did) {
            let cache_age = now - entry.last_checked;
            if cache_age < CACHE_TTL_SECONDS {
                // Cache is fresh, check if update is needed based on cached value
                let time_since_last_seen = now - entry.last_seen_value;
                if time_since_last_seen < LAST_SEEN_WRITE_THROTTLE_S {
                    // Still inside the throttle period, no need to update
                    log::trace!(
                        "last_seen tracking for {}: cache hit, no update needed (last_seen={}, age={}s)",
                        agent_did, entry.last_seen_value, time_since_last_seen
                    );
                    return;
                }
            }
        }
    }

    // Cache miss or stale - need to check database
    // Use spawn_blocking to avoid blocking the async runtime
    let agent_did_clone = agent_did.clone();
    let should_update = tokio::task::spawn_blocking(move || {
        Ad4mDb::with_global_instance(|db| {
            if let Ok(Some(user)) = db.get_user_by_did(&agent_did_clone) {
                if let Some(last_seen) = user.last_seen {
                    let throttle_cutoff = now.saturating_sub(LAST_SEEN_WRITE_THROTTLE_S);

                    // Handle unrealistic future timestamps by treating them as stale
                    // (allow some clock skew tolerance of 1 minute)
                    let should_update = if last_seen > now + 60 {
                        log::warn!(
                            "last_seen tracking for {}: unrealistic future timestamp {}, treating as stale",
                            agent_did_clone, last_seen
                        );
                        true
                    } else {
                        last_seen < throttle_cutoff
                    };

                    log::trace!("last_seen tracking for {}: last_seen={}, throttle_cutoff={}, should_update={}",
                        agent_did_clone, last_seen, throttle_cutoff, should_update);
                    (should_update, Some(last_seen))
                } else {
                    log::debug!(
                        "last_seen tracking for {}: never seen before, updating now",
                        agent_did_clone
                    );
                    (true, None) // Never updated, do it now
                }
            } else {
                log::warn!(
                    "last_seen tracking: no user owns the agent {}",
                    agent_did_clone
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
            agent_did.clone(),
            LastSeenCacheEntry {
                last_checked: now,
                last_seen_value: last_seen_val,
            },
        );
    }

    if should_update {
        log::debug!("Updating last_seen for agent: {}", agent_did);

        // Perform the update in spawn_blocking
        let agent_did_for_update = agent_did.clone();
        let update_result = tokio::task::spawn_blocking(move || {
            Ad4mDb::with_global_instance(|db| db.update_agent_last_seen(&agent_did_for_update))
        })
        .await;

        match update_result {
            Ok(Ok(())) => {
                // Update succeeded, refresh cache with new timestamp
                let mut cache = LAST_SEEN_CACHE.write().await;
                cache.insert(
                    agent_did,
                    LastSeenCacheEntry {
                        last_checked: now,
                        last_seen_value: now,
                    },
                );
            }
            Ok(Err(e)) => {
                log::error!(
                    "Failed to update last_seen for agent {}: {:?}",
                    agent_did,
                    e
                );
            }
            Err(e) => {
                log::error!(
                    "Failed to update last_seen for agent {} (spawn_blocking join error): {:?}",
                    agent_did,
                    e
                );
            }
        }
    }
}

/// [`capabilities_on`] for a caller on a loopback listener.
///
/// For the MCP server. Loopback holds there because `mcp::server::resolve_host`
/// binds an MCP server with no admin credential to loopback, and its HTTPS listener does not
/// start without one. The exception is an explicit `MCP_HOST`, which it warns about.
pub fn capabilities_from_token(
    token: String,
    admin_credential: &Option<Secret>,
) -> Result<Vec<Capability>, String> {
    capabilities_on(token, admin_credential, ListenerReach::Loopback)
}

/// What a caller without a token may do: request a capability and read whether multi-user
/// mode is on, plus sign up, log in and verify an email when it is. Reading users is left out,
/// so anonymous callers cannot list them.
pub fn anonymous_capabilities() -> Vec<Capability> {
    use crate::db::Ad4mDb;
    let multi_user_enabled =
        Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled().unwrap_or(false));
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

/// The capabilities of a caller that presents `token` on a listener with this reach.
pub fn capabilities_on(
    token: String,
    admin_credential: &Option<Secret>,
    reach: ListenerReach,
) -> Result<Vec<Capability>, String> {
    // The same test that decides `is_admin_credential`, so the two cannot disagree.
    if is_admin_credential_token_on(&token, admin_credential, reach) {
        return Ok(vec![ALL_CAPABILITY.clone()]);
    }

    if token.is_empty() {
        // An anonymous caller: the executor has an admin credential, or the listener is on the
        // network.
        return Ok(anonymous_capabilities());
    }

    let claims = decode_jwt(token).map_err(|e| e.to_string())?;
    Ok(granted(&claims)?.caps.clone())
}

/// The agent that a capability request names (#1399, D11, D16): `agent` is a managed user's
/// email or an agent's DID; absent or empty, the main agent. An email resolves to its user's
/// DID here, at the edge; an email no user has is taken all the same, as
/// [`RequestTarget::NoAgent`]: anyone may request, and an answer that differed would tell
/// them which emails have accounts. Such a request reaches no one and expires. A DID (public
/// anyway) must be the main agent's or a user's.
pub fn requested_agent(agent: Option<&str>) -> Result<RequestTarget, String> {
    let Some(agent) = agent.map(str::trim).filter(|agent| !agent.is_empty()) else {
        return Ok(AgentContext::main_agent().into());
    };
    if !agent.starts_with("did:") {
        return Ok(
            match crate::agent::AgentService::did_for_email(&agent.to_lowercase()) {
                Ok(did) => AgentContext::managed(did).into(),
                Err(_) => RequestTarget::NoAgent,
            },
        );
    }
    if crate::agent::did_for_context(&AgentContext::main_agent()).is_ok_and(|did| did == agent) {
        return Ok(AgentContext::main_agent().into());
    }
    match crate::agent::AgentService::email_for_did(agent) {
        Ok(_) => Ok(AgentContext::managed(agent.to_string()).into()),
        Err(_) => Err(format!("No agent on this executor has the DID {agent}")),
    }
}

/// The email of the user who owns `agent`, as a display hint in the request and the grant's
/// auth info (`AuthInfo.user_email`): the approver sees whose agent it is. Never a key: the
/// agent is its DID (D16).
fn display_email(agent: &AgentContext) -> Option<String> {
    agent
        .managed_did()
        .and_then(|did| crate::agent::AgentService::email_for_did(did).ok())
}

/// Records an app's request for capabilities on the agent `target` and returns its id. The
/// request waits in memory ([`pending`]) for the owner of `target` to approve it: the approval
/// request event goes to `target`'s sessions (by DID) that hold AGENT_PERMIT. A request for
/// [`RequestTarget::NoAgent`] publishes nothing. No request is ever approved here: the app
/// name and domain are whatever the (possibly anonymous) caller declares, so they never
/// grant anything.
pub async fn request_capability(auth_info: AuthInfo, target: impl Into<RequestTarget>) -> String {
    let target = target.into();
    let request_id = uuid::Uuid::new_v4().to_string();
    // The request names its agent's user in the auth info, so the approver sees whose it is.
    let auth = AuthInfo {
        user_email: target.agent().and_then(display_email),
        ..auth_info
    };
    let pending = PendingRequests::global();
    pending.request(request_id.clone(), auth.clone(), target.clone(), now());

    let Some(agent) = target.agent() else {
        return request_id;
    };
    let app_name = auth.app_name.clone();
    let auth_extended = AuthInfoExtended {
        request_id: request_id.clone(),
        auth,
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
    get_global_pubsub()
        .await
        .publish_event(Event::approval_request(&exception_info, agent));

    request_id
}

/// The agent that the pending request `request_id` is for, while it waits.
pub fn request_target(request_id: &str) -> Option<RequestTarget> {
    PendingRequests::global().target(request_id, now())
}

/// Whether `session` may approve a capability request for `target` (D12): the operator, or a
/// session that acts as `target` (the same DID) and holds AGENT_PERMIT (a user on their own
/// agent; on the main agent, an app the operator gave AGENT_PERMIT). Nobody approves for
/// another agent; only the operator approves a request for [`RequestTarget::NoAgent`], and
/// its code redeems nothing.
pub fn may_approve(session: &Session, target: &RequestTarget) -> bool {
    session.is_operator()
        || target.agent().is_some_and(|agent| {
            session.acts_as() == *agent && session.may(&AGENT_PERMIT_CAPABILITY)
        })
}

/// Approves a pending request with `auth_info_extended.auth`, which may grant less than the
/// app asked for, and returns the code the app redeems: for the approver only. The request's
/// agent stays the one it named. The caller has checked [`may_approve`].
pub fn permit_capability(auth_info_extended: AuthInfoExtended) -> Result<String, String> {
    let code = gen_random_digits();
    PendingRequests::global().approve(
        &auth_info_extended.request_id,
        auth_info_extended.auth,
        Secret::new(code.clone()),
        now(),
    )?;
    Ok(code)
}

/// What a grant on `agent` may hold of `approved` (D13): all of it on the main agent, which
/// holds every capability; on a managed agent, only what that agent holds itself (its user's
/// capabilities). A capability that one of the agent's covers stays; a wider one (`*`)
/// narrows to the agent's capabilities it covers.
pub fn grant_caps_for(agent: &AgentContext, approved: &[Capability]) -> Vec<Capability> {
    if agent.is_main_agent() {
        return approved.to_vec();
    }
    let own = get_user_default_capabilities();
    let mut caps: Vec<Capability> = approved
        .iter()
        .filter(|cap| capability_matches(&own, cap))
        .cloned()
        .collect();
    for cap in &own {
        if capability_matches(approved, cap) && !capability_matches(&caps, cap) {
            caps.push(cap.clone());
        }
    }
    caps
}

/// Redeems an approved request's code for the app's token, in one step: the grant is minted
/// on the request's agent with the approved capabilities, capped by [`grant_caps_for`], and
/// only then does the request go. A mint that fails (a locked wallet, a database that does
/// not take the grant) leaves the request approved, and the same code redeems later. The
/// token goes only to the caller; the grant keeps its id.
pub async fn generate_capability_token(request_id: String, code: String) -> Result<Secret, String> {
    let (token, grant) = PendingRequests::global()
        .redeem(&request_id, &code, now(), |target, auth| {
            let capabilities = match &auth.capabilities {
                Some(approved) => {
                    let caps = grant_caps_for(target, approved);
                    if caps.is_empty() {
                        return Err("None of the approved capabilities is within the agent's \
                                    own; the request must be approved again"
                            .to_string());
                    }
                    Some(caps)
                }
                None => None,
            };
            let auth = AuthInfo {
                capabilities,
                user_email: display_email(target),
                ..auth.clone()
            };
            crate::agent::grants::mint(
                target.clone(),
                auth,
                Some(request_id.clone()),
                DEFAULT_TOKEN_VALID_PERIOD,
            )
        })
        .map_err(|e| e.to_string())?;

    // The event reaches the sessions that may approve apps for the agent the app acts as;
    // the token goes only to the caller.
    get_global_pubsub()
        .await
        .publish_event(Event::apps_changed(&grant.as_app(), &grant.agent));

    Ok(Secret::new(token))
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
    // On the network it gets what an anonymous caller gets on an executor with a credential:
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

        let local = Ok(capabilities_on(empty.clone(), &None, ListenerReach::Loopback).unwrap());
        assert!(check_capability(&local, &AGENT_READ_CAPABILITY).is_ok());

        // The anonymous set reads the multi-user setting.
        let _ = crate::db::Ad4mDb::init_global_instance(":memory:");
        let remote = Ok(capabilities_on(empty, &None, ListenerReach::Network).unwrap());
        assert!(check_capability(&remote, &AGENT_READ_CAPABILITY).is_err());
        assert!(check_capability(&remote, &AGENT_AUTH_CAPABILITY).is_ok());
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
        let capabilities = capabilities_on(
            "the-admin-credential".to_string(),
            &credential,
            ListenerReach::Network,
        );
        assert!(check_capability(&capabilities, &AGENT_READ_CAPABILITY).is_ok());
    }

    #[test]
    fn unauthenticated_users_can_check_multi_user_enabled() {
        // Empty token (unauthenticated) should have capability to check if multi-user is enabled
        let capabilities = capabilities_from_token(String::new(), &None).unwrap();
        assert!(
            check_capability(
                &Ok(capabilities),
                &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY
            )
            .is_ok(),
            "Unauthenticated users should be able to check if multi-user mode is enabled"
        );
    }

    #[test]
    fn unauthenticated_users_get_login_capability_in_multi_user_mode() {
        // Empty token (unauthenticated) should have the right capabilities
        let capabilities = capabilities_from_token(String::new(), &None).unwrap();

        // Should have read enabled capability (to check if multi-user is on)
        assert!(
            check_capability(
                &Ok(capabilities.clone()),
                &RUNTIME_USER_MANAGEMENT_READ_ENABLED_CAPABILITY
            )
            .is_ok(),
            "Unauthenticated users should be able to check if multi-user mode is enabled"
        );

        // Should have auth capability (for standard capability request flow)
        assert!(
            check_capability(&Ok(capabilities.clone()), &AGENT_AUTH_CAPABILITY).is_ok(),
            "Unauthenticated users should have auth capability"
        );

        // Note: We can't easily test the multi-user mode grant of LOGIN and CREATE capabilities
        // without setting up the database, but the logic is tested by integration tests
    }
}

#[cfg(test)]
mod app_token_tests {
    use super::*;
    use crate::agent::grants::Grants;
    use crate::events::{Audience, Topic};
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

    fn setup() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::ensure_test_db();
    }

    async fn permitted_request() -> (String, String) {
        let request_id = request_capability(test_auth_info(), AgentContext::main_agent()).await;
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: test_auth_info(),
        })
        .unwrap();
        (request_id, code)
    }

    fn wrong(code: &str) -> String {
        if code == "100000" { "100001" } else { "100000" }.to_string()
    }

    fn grant_of(token: &str) -> std::sync::Arc<crate::agent::grants::LiveGrant> {
        granted(&decode_jwt(token.to_string()).unwrap()).unwrap()
    }

    fn caps_text(caps: &[Capability]) -> Vec<String> {
        let mut text: Vec<String> = caps.iter().map(|c| format!("{c:?}")).collect();
        text.sort();
        text
    }

    /// The approval request of `request_id` among the events `events` receives, if one
    /// arrives within `wait`.
    async fn approval_request(
        events: &mut tokio::sync::broadcast::Receiver<std::sync::Arc<Event>>,
        request_id: &str,
        wait: Duration,
    ) -> Option<std::sync::Arc<Event>> {
        let deadline = tokio::time::Instant::now() + wait;
        loop {
            let event = tokio::time::timeout_at(deadline, events.recv())
                .await
                .ok()?
                .ok()?;
            if event.topic == Topic::ExceptionOccurred
                && event.payload.to_string().contains(request_id)
            {
                return Some(event);
            }
        }
    }

    // The code has six digits. Without a limit, anyone who learned a request id could try
    // every code and receive the app's token.
    #[tokio::test]
    async fn a_request_stops_accepting_codes_after_too_many_wrong_ones() {
        let (request_id, code) = permitted_request().await;
        let mut last_err = String::new();
        for _ in 0..MAX_CODE_ATTEMPTS {
            last_err = generate_capability_token(request_id.clone(), wrong(&code))
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
        assert!(!ids.iter().any(|id| PendingRequests::global().contains(id)));
    }

    // The request goes once it runs out of code attempts.
    #[tokio::test]
    async fn a_locked_out_request_is_deleted() {
        let (request_id, code) = permitted_request().await;
        assert!(PendingRequests::global().contains(&request_id));
        for _ in 0..MAX_CODE_ATTEMPTS {
            let _ = generate_capability_token(request_id.clone(), wrong(&code)).await;
        }
        assert!(!PendingRequests::global().contains(&request_id));
    }

    // Request ids are UUIDs, which contain dashes; requests used to be stored under
    // `{id}-{code}`. Wrong codes sent for only the start of someone else's id must not lock
    // that request out.
    #[tokio::test]
    async fn wrong_codes_for_a_prefix_of_an_id_leave_that_request_alone() {
        setup();
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
        setup();
        let (request_id, code) = permitted_request().await;
        for _ in 0..MAX_CODE_ATTEMPTS - 1 {
            assert!(generate_capability_token(request_id.clone(), wrong(&code))
                .await
                .is_err());
        }
        assert!(generate_capability_token(request_id, code).await.is_ok());
    }

    // A code redeems once. A token that cannot be issued (a locked wallet) uses up nothing:
    // the app retries with the same code.
    #[tokio::test]
    async fn a_code_redeems_once_and_a_locked_wallet_keeps_the_request() {
        setup();
        let (request_id, code) = permitted_request().await;

        let backend = crate::wallet::wallet_backend();
        backend.lock("pending-locked-test");
        let locked = generate_capability_token(request_id.clone(), code.clone()).await;
        backend
            .unlock("pending-locked-test")
            .expect("restore the shared wallet");
        assert!(locked.unwrap_err().contains("Executor is locked"));
        assert!(PendingRequests::global().has_code(&request_id, &code));

        assert!(generate_capability_token(request_id.clone(), code.clone())
            .await
            .is_ok());
        assert!(!PendingRequests::global().contains(&request_id));
        assert_eq!(
            generate_capability_token(request_id.clone(), code)
                .await
                .unwrap_err(),
            "Can't find permitted request"
        );
    }

    // A database that does not take the grant: the token is never delivered, so no grant may
    // name it, and the app retries with the same code.
    #[tokio::test]
    async fn a_database_that_does_not_take_the_grant_keeps_the_request_and_no_grant() {
        setup();
        let (request_id, code) = permitted_request().await;

        let db = crate::db::Ad4mDb::global_instance();
        let taken = db.lock().unwrap().take();
        let unsaved = generate_capability_token(request_id.clone(), code.clone()).await;
        *db.lock().unwrap() = taken;

        assert!(unsaved.is_err());
        assert!(Grants::global().find(&request_id).is_empty());
        assert!(PendingRequests::global().has_code(&request_id, &code));

        let token = generate_capability_token(request_id.clone(), code)
            .await
            .expect("the same code redeems once the database takes the grant")
            .expose_once();
        assert_eq!(Grants::global().find(&request_id).len(), 1);
        assert!(capabilities_from_token(token, &None).is_ok());
    }

    // The `apps-changed` event and the app list reach sessions other than the app, so
    // neither may carry the token. The executor does not store it either: the grant keeps
    // the token's id, which is enough to revoke it.
    #[tokio::test]
    async fn app_events_the_app_list_and_the_grants_carry_no_token() {
        setup();
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
            if let Ok(Some(app)) = serde_json::from_value::<Option<Apps>>(event.payload.clone()) {
                if app.request_id == request_id {
                    let main = crate::agent::did();
                    assert_eq!(event.audience, Audience::Dids(vec![main]));
                    assert_eq!(event.capability, *AGENT_PERMIT_CAPABILITY);
                    break app;
                }
            }
        };
        assert!(event.token.is_empty(), "the event carried the app token");

        let apps: Vec<Apps> = Grants::global()
            .list(&AgentContext::main_agent())
            .iter()
            .map(|grant| grant.as_app())
            .collect();
        let listed = apps
            .iter()
            .find(|app| app.request_id == request_id)
            .expect("the app appears in the list");
        assert!(
            listed.token.is_empty(),
            "the app list carried the app token"
        );
        assert!(!serde_json::to_string(&apps).unwrap().contains(&token));
        let rows = crate::db::Ad4mDb::with_global_instance(|db| db.grants()).unwrap();
        assert!(rows.iter().any(|row| row.request_id == request_id));
        assert!(!format!("{rows:?}").contains(&token));

        assert!(capabilities_from_token(token.clone(), &None).is_ok());
        let grants = Grants::global();
        grants.revoke(&grants.find(&request_id)).unwrap();
        assert!(capabilities_from_token(token, &None).is_err());
    }

    // MCP resolves tokens without a `Session`, by the same grants: a removed app's token
    // has no grant and is refused (#396), and so is a token that never had one.
    #[test]
    fn mcp_refuses_a_token_without_an_active_grant() {
        setup();
        let token = crate::test_utils::granted_token(test_auth_info());
        assert!(capabilities_from_token(token.clone(), &None).is_ok());
        assert!(crate::agent::AgentContext::from_auth_token(token.clone(), false).is_ok());

        let grants = Grants::global();
        let found = grants.find(&token);
        grants.revoke(&found).unwrap();
        grants.remove(&found).unwrap();
        assert!(capabilities_from_token(token.clone(), &None).is_err());
        assert!(crate::agent::AgentContext::from_auth_token(token, false).is_err());

        let (ungranted, _) = token::sign_token("no grant".to_string(), 3600, None).unwrap();
        assert!(capabilities_from_token(ungranted.clone(), &None).is_err());
        assert!(crate::agent::AgentContext::from_auth_token(ungranted, false).is_err());
    }

    // #1399 (D11, D13, D14): a request names a managed agent. Its approval request goes to
    // that agent, the approver narrows it, and the grant acts as that agent with only the
    // approved capabilities.
    #[tokio::test]
    async fn a_request_for_a_managed_agent_mints_a_grant_on_it_with_the_approved_subset() {
        setup();
        let alice_email = crate::test_utils::managed_user("req-alice");
        let alice = crate::agent::AgentService::managed_agent_for_email(&alice_email).unwrap();
        let alice_did = crate::agent::did_for_context(&alice).unwrap();
        let mut events = get_global_pubsub().await.subscribe_events();

        let request_id = request_capability(test_auth_info(), alice.clone()).await;
        assert_eq!(request_target(&request_id), Some(alice.clone().into()));
        let event = approval_request(&mut events, &request_id, Duration::from_secs(5))
            .await
            .expect("the approval request is published");
        assert_eq!(event.audience, Audience::Dids(vec![alice_did.clone()]));
        assert_eq!(event.capability, *AGENT_PERMIT_CAPABILITY);
        assert!(
            event.payload.to_string().contains(&alice_email),
            "the approver sees whose agent the request is for: {}",
            event.payload
        );

        let subset = vec![AGENT_READ_CAPABILITY.clone()];
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: AuthInfo {
                capabilities: Some(subset.clone()),
                ..test_auth_info()
            },
        })
        .unwrap();
        let token = generate_capability_token(request_id.clone(), code)
            .await
            .unwrap()
            .expose_once();

        let grant = grant_of(&token);
        assert_eq!(grant.agent, alice);
        assert_eq!(grant.did, alice_did);
        assert_eq!(caps_text(&grant.caps), caps_text(&subset));
        assert_eq!(
            decode_jwt(token).unwrap().sub.as_deref(),
            Some(alice_did.as_str()),
            "the token names the agent it acts as"
        );
    }

    // D13: whatever the approver grants, a grant on a managed agent holds no capability its
    // user lacks. `*` narrows to the user's set; a capability outside it is dropped.
    #[test]
    fn a_grant_on_a_managed_agent_is_capped_at_its_users_capabilities() {
        let alice = AgentContext::managed("did:key:z6MkAlice".into());
        let user = get_user_default_capabilities();
        assert_eq!(
            caps_text(&grant_caps_for(&alice, &[ALL_CAPABILITY.clone()])),
            caps_text(&user)
        );
        assert!(!capability_matches(&user, &AGENT_LOCK_CAPABILITY));
        assert_eq!(
            caps_text(&grant_caps_for(
                &alice,
                &[AGENT_LOCK_CAPABILITY.clone(), AGENT_READ_CAPABILITY.clone()]
            )),
            caps_text(&[AGENT_READ_CAPABILITY.clone()])
        );
        assert!(grant_caps_for(&alice, &[AGENT_LOCK_CAPABILITY.clone()]).is_empty());

        // The main agent holds every capability.
        assert_eq!(
            caps_text(&grant_caps_for(
                &AgentContext::main_agent(),
                &[ALL_CAPABILITY.clone()]
            )),
            caps_text(&[ALL_CAPABILITY.clone()])
        );
    }

    #[tokio::test]
    async fn redeeming_caps_a_wider_approval_at_the_managed_agents_capabilities() {
        setup();
        let alice = crate::agent::AgentService::managed_agent_for_email(
            &crate::test_utils::managed_user("cap-alice"),
        )
        .unwrap();
        let request_id = request_capability(test_auth_info(), alice.clone()).await;
        let code = permit_capability(AuthInfoExtended {
            request_id: request_id.clone(),
            auth: test_auth_info(),
        })
        .unwrap();
        let token = generate_capability_token(request_id, code)
            .await
            .unwrap()
            .expose_once();
        let grant = grant_of(&token);
        assert_eq!(grant.agent, alice);
        assert_eq!(
            caps_text(&grant.caps),
            caps_text(&get_user_default_capabilities()),
            "ALL narrows to the user's capabilities"
        );
    }

    // An app's name and domain are whatever the requester declares, so they grant nothing:
    // whatever app a request claims to be, for the main agent or a managed one, the owner is
    // asked, and neither the empty code nor a guess redeems it. Only the owner's code does.
    #[tokio::test]
    async fn no_app_name_skips_the_owners_approval() {
        setup();
        let alice = crate::agent::AgentService::managed_agent_for_email(
            &crate::test_utils::managed_user("claim-alice"),
        )
        .unwrap();
        let mut events = get_global_pubsub().await.subscribe_events();

        for target in [AgentContext::main_agent(), alice] {
            for (name, app_domain) in [
                ("ADAM Launcher", Some("localhost".to_string())),
                ("Flux", Some("app.flux.social".to_string())),
                ("Any app", None),
            ] {
                let auth = AuthInfo {
                    app_name: name.into(),
                    app_domain,
                    ..test_auth_info()
                };
                let request_id = request_capability(auth.clone(), target.clone()).await;
                assert!(
                    approval_request(&mut events, &request_id, Duration::from_secs(5))
                        .await
                        .is_some(),
                    "{name}: the owner is asked"
                );
                assert!(
                    !PendingRequests::global().is_approved(&request_id),
                    "{name}: a request is never approved at the request"
                );
                for code in ["", "000000"] {
                    assert_eq!(
                        generate_capability_token(request_id.clone(), code.to_string())
                            .await
                            .unwrap_err(),
                        "Can't find permitted request",
                        "{name}: {code:?} redeems nothing"
                    );
                }

                let code = permit_capability(AuthInfoExtended {
                    request_id: request_id.clone(),
                    auth,
                })
                .unwrap();
                assert_eq!(
                    generate_capability_token(request_id.clone(), String::new())
                        .await
                        .unwrap_err(),
                    "Can't find permitted request",
                    "{name}: the empty code does not redeem an approved request"
                );
                let token = generate_capability_token(request_id, code)
                    .await
                    .expect("the owner's code redeems")
                    .expose_once();
                assert_eq!(grant_of(&token).agent, target);
            }
        }
    }

    #[test]
    fn a_request_names_its_agent_by_email_or_did() {
        setup();
        let main: RequestTarget = AgentContext::main_agent().into();
        assert_eq!(requested_agent(None).unwrap(), main);
        assert_eq!(requested_agent(Some(" ")).unwrap(), main);
        assert_eq!(requested_agent(Some(&crate::agent::did())).unwrap(), main);
        let email = crate::test_utils::managed_user("named");
        let alice = crate::agent::AgentService::managed_agent_for_email(&email).unwrap();
        assert_eq!(
            requested_agent(Some(&format!(" {} ", email.to_uppercase()))).unwrap(),
            alice.clone().into()
        );
        let did = crate::agent::did_for_context(&alice).unwrap();
        assert_eq!(requested_agent(Some(&did)).unwrap(), alice.into());
        assert!(requested_agent(Some("did:key:z6MkNoSuchAgent")).is_err());
        // An email without a user is taken, as one for nobody: the answer does not say
        // whether the email has an account.
        assert_eq!(
            requested_agent(Some("nobody-here@example.org")).unwrap(),
            RequestTarget::NoAgent
        );
    }
}
