use axum::{extract::FromRequestParts, http::request::Parts};

use super::errors::ApiError;
use crate::agent::capabilities::{
    capabilities_on, is_admin_credential_token_on, user_email_from_token, Capability, ListenerReach,
};
use crate::agent::AgentService;
use crate::types::RequestContext;

/// Auth context extracted from the Authorization header.
/// Constructs the RequestContext from auth headers.
#[derive(Clone)]
pub struct AuthContext {
    pub capabilities: Result<Vec<Capability>, String>,
    pub auto_permit_cap_requests: bool,
    pub auth_token: String,
    pub is_admin_credential: bool,
}

/// Resolves the user behind a token, and keeps the session from acting as anyone else.
///
/// Returns the capabilities to use, the user's email and the user's DID. A token that names
/// a user whose DID cannot load (a wallet or shared-backend error) gets no capabilities:
/// perspective checks treat a missing DID as the node's main agent, so the session must not
/// run with one.
pub(crate) fn resolve_user_session(
    auth_token: &str,
    capabilities: Result<Vec<Capability>, String>,
) -> (
    Result<Vec<Capability>, String>,
    Option<String>,
    Option<String>,
) {
    let user_email = user_email_from_token(auth_token.to_string());
    match &user_email {
        None => (capabilities, None, None),
        Some(email) => match AgentService::get_user_did_by_email(email) {
            Ok(did) => (capabilities, user_email, Some(did)),
            Err(e) => (
                Err(format!(
                    "Could not load the identity of this user session; reconnect: {}",
                    e
                )),
                user_email,
                None,
            ),
        },
    }
}

impl AuthContext {
    /// Convert to the existing RequestContext used by internal functions.
    pub fn to_request_context(&self) -> RequestContext {
        let (capabilities, user_email, user_did) =
            resolve_user_session(&self.auth_token, self.capabilities.clone());
        RequestContext {
            capabilities,
            auto_permit_cap_requests: self.auto_permit_cap_requests,
            auth_token: self.auth_token.clone(),
            is_admin_credential: self.is_admin_credential,
            user_email,
            user_did,
            cancel_token: None,
        }
    }
}

/// AppState shared across all API handlers.
#[derive(Clone)]
pub struct AppState {
    pub admin_credential: Option<String>,
    pub auto_permit_cap_requests: bool,
}

impl<S: Send + Sync> FromRequestParts<S> for AuthContext
where
    AppState: FromRef<S>,
{
    type Rejection = ApiError;

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let app_state = AppState::from_ref(state);

        let auth_header = parts
            .headers
            .get("Authorization")
            .and_then(|v| v.to_str().ok())
            .map(|s| s.strip_prefix("Bearer ").unwrap_or(s))
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                parts
                    .uri
                    .query()
                    .and_then(|q| {
                        url::form_urlencoded::parse(q.as_bytes())
                            .find(|(k, _)| k == "token")
                            .map(|(_, v)| v.to_string())
                    })
                    .unwrap_or_default()
            });

        // Track last_seen for multi-user mode
        crate::agent::capabilities::track_last_seen_from_token(auth_header.clone()).await;

        let reach = listener_reach(parts);
        let capabilities = capabilities_on(
            auth_header.clone(),
            app_state.admin_credential.clone(),
            reach,
        );
        let is_admin_credential =
            is_admin_credential_token_on(&auth_header, &app_state.admin_credential, reach);

        Ok(AuthContext {
            capabilities,
            auto_permit_cap_requests: app_state.auto_permit_cap_requests,
            auth_token: auth_header,
            is_admin_credential,
        })
    }
}

/// Headers a reverse proxy sets to name the client it forwards for.
const FORWARDING_HEADERS: [&str; 3] = ["forwarded", "x-forwarded-for", "x-real-ip"];

/// The reach of the caller behind a request: the listener's, as `api::listener_router` marked
/// it, unless the request says a proxy forwarded it.
///
/// An unmarked router reads as `Network`: nothing has shown that its callers are on this
/// machine, so an anonymous caller there is not the operator.
///
/// A proxy on this machine (nginx, Caddy, cloudflared) connects to a loopback listener, so
/// its clients would read as `Loopback`. A request to a loopback listener that carries a
/// forwarding header is `Network`. Local tools do not send these headers, and a caller that
/// forges one can only make itself anonymous. A proxy that sets none of them (a raw TCP
/// forward, `ssh -R`) still reads as `Loopback`: only an admin credential covers that.
pub fn listener_reach(parts: &Parts) -> ListenerReach {
    let marked = parts
        .extensions
        .get::<ListenerReach>()
        .copied()
        .unwrap_or(ListenerReach::Network);
    let forwarded = FORWARDING_HEADERS
        .iter()
        .any(|name| parts.headers.contains_key(*name));
    if forwarded {
        ListenerReach::Network
    } else {
        marked
    }
}

impl<S: Send + Sync> FromRequestParts<S> for ListenerReach {
    type Rejection = std::convert::Infallible;

    async fn from_request_parts(parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        Ok(listener_reach(parts))
    }
}

/// Needed for axum's FromRef to extract AppState from the router state.
pub trait FromRef<T> {
    fn from_ref(input: &T) -> Self;
}

impl FromRef<AppState> for AppState {
    fn from_ref(input: &AppState) -> Self {
        input.clone()
    }
}

#[cfg(test)]
mod session_tests {
    use super::*;
    use crate::agent::capabilities::{get_user_default_capabilities, ALL_CAPABILITY};
    use crate::test_utils::MultiUserMode;

    // A user token whose DID could not load used to run with that user's capabilities and
    // no DID, and perspective checks treat a missing DID as the node's main agent.
    #[test]
    fn a_user_session_without_a_loadable_identity_gets_no_capabilities() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let _multi_user = MultiUserMode::on();
        let token = crate::user_management::generate_user_jwt("no.key@example.org", "test")
            .expect("the wallet is unlocked in tests");

        let (capabilities, user_email, user_did) =
            resolve_user_session(&token, Ok(get_user_default_capabilities()));
        assert!(capabilities.is_err(), "the session must not act at all");
        assert_eq!(user_email.as_deref(), Some("no.key@example.org"));
        assert!(user_did.is_none());
    }

    #[test]
    fn a_user_session_with_an_identity_keeps_its_capabilities() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let _multi_user = MultiUserMode::on();
        let email = "has.key@example.org";
        AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();

        let (capabilities, user_email, user_did) =
            resolve_user_session(&token, Ok(get_user_default_capabilities()));
        assert!(capabilities.is_ok());
        assert_eq!(user_email.as_deref(), Some(email));
        assert_eq!(
            user_did,
            Some(AgentService::get_user_did_by_email(email).unwrap())
        );
    }

    #[test]
    fn an_operator_session_is_unchanged() {
        let (capabilities, user_email, user_did) =
            resolve_user_session("", Ok(vec![ALL_CAPABILITY.clone()]));
        assert!(capabilities.is_ok());
        assert!(user_email.is_none());
        assert!(user_did.is_none());
    }
}
