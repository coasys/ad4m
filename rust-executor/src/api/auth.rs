use std::sync::Arc;

use axum::{extract::FromRequestParts, http::request::Parts};

use super::errors::ApiError;
use crate::agent::capabilities::{
    capabilities_on, user_email_from_token, Capability, ListenerReach, TokenCheck,
};
use crate::agent::session::Session;
use crate::agent::AgentService;
use crate::types::RequestContext;

/// Who is calling, resolved once from the token: by the HTTP extractor (REST and the
/// OpenAI-compatible API) per request, and by the RPC socket per connection.
#[derive(Clone)]
pub struct AuthContext {
    pub capabilities: Result<Vec<Capability>, String>,
    pub auto_permit_cap_requests: bool,
    pub auth_token: String,
    pub is_admin_credential: bool,
    /// The caller's session, when the token opens one.
    pub session: Option<Arc<Session>>,
    /// The user the token names, and that user's DID.
    pub user_email: Option<String>,
    pub user_did: Option<String>,
    /// `capabilities`, except that a user whose DID does not load gets none
    /// (see [`resolve_user_session`]). The request context runs with these.
    context_capabilities: Result<Vec<Capability>, String>,
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
    /// The identity of a caller that presents `token` on a listener with this reach.
    ///
    /// A token that opens a [`Session`] takes its identity from it. A token that opens none
    /// (invalid, expired, revoked, or a user whose DID does not load) keeps the identity it
    /// had before sessions existed: its capabilities fail, so every capability check refuses
    /// it, except for a user whose DID does not load, whose request context alone fails.
    pub fn open(
        token: &str,
        admin_credential: &Option<String>,
        auto_permit_cap_requests: bool,
        reach: ListenerReach,
    ) -> Self {
        let (
            capabilities,
            context_capabilities,
            is_admin_credential,
            user_email,
            user_did,
            session,
        ) = match Session::open(token, admin_credential, reach) {
            Ok(session) => {
                let user = session.user();
                (
                    Ok(session.capabilities()),
                    Ok(session.capabilities()),
                    session.is_operator(),
                    user.map(|(email, _)| email.to_string()),
                    user.map(|(_, did)| did.to_string()),
                    Some(Arc::new(session)),
                )
            }
            // The admin credential always opens a session, so this caller is not the
            // operator.
            Err(_) => {
                let capabilities =
                    capabilities_on(token.to_string(), admin_credential.clone(), reach);
                let (context_capabilities, user_email, user_did) =
                    resolve_user_session(token, capabilities.clone());
                (
                    capabilities,
                    context_capabilities,
                    false,
                    user_email,
                    user_did,
                    None,
                )
            }
        };
        AuthContext {
            capabilities,
            auto_permit_cap_requests,
            auth_token: token.to_string(),
            is_admin_credential,
            session,
            user_email,
            user_did,
            context_capabilities,
        }
    }

    /// Convert to the existing RequestContext used by internal functions.
    pub fn to_request_context(&self) -> RequestContext {
        RequestContext {
            capabilities: self.context_capabilities.clone(),
            auto_permit_cap_requests: self.auto_permit_cap_requests,
            auth_token: self.auth_token.clone(),
            is_admin_credential: self.is_admin_credential,
            user_email: self.user_email.clone(),
            user_did: self.user_did.clone(),
            cancel_token: None,
            session: self.session.clone(),
        }
    }

    /// The check a connection that stays open runs again on each message: the session's, or
    /// for a token that opened none, the token's revocation and expiry.
    pub fn token_check(&self) -> TokenCheck {
        match &self.session {
            Some(session) => TokenCheck::for_session(session.clone()),
            None => TokenCheck::new(&self.auth_token, false),
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

        let auth = AuthContext::open(
            &auth_header,
            &app_state.admin_credential,
            app_state.auto_permit_cap_requests,
            listener_reach(parts),
        );

        // Track last_seen for multi-user mode
        if let Some(email) = &auth.user_email {
            crate::agent::capabilities::track_last_seen(email).await;
        }

        Ok(auth)
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

    // #1175: REST and the OpenAI-compatible API used to decode the token again for every
    // user lookup. The extractor now opens a session and the request context carries it.
    #[tokio::test]
    async fn a_rest_request_with_a_user_token_carries_the_users_session() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let email = "rest.session@example.org";
        AgentService::ensure_user_key_exists(email).unwrap();
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let did = AgentService::get_user_did_by_email(email).unwrap();

        let request = axum::http::Request::builder()
            .uri("/v1/models")
            .header("Authorization", format!("Bearer {token}"))
            .extension(ListenerReach::Network)
            .body(())
            .unwrap();
        let (mut parts, _) = request.into_parts();
        let state = AppState {
            admin_credential: Some("the-admin-credential".to_string()),
            auto_permit_cap_requests: false,
        };
        let auth = AuthContext::from_request_parts(&mut parts, &state)
            .await
            .unwrap_or_else(|_| panic!("the extractor accepts a user token"));

        let session = auth.session.as_ref().expect("a user token opens a session");
        assert_eq!(session.user(), Some((email, did.as_str())));
        assert_eq!(auth.user_email.as_deref(), Some(email));
        assert!(!auth.is_admin_credential);

        let ctx = auth.to_request_context();
        assert!(ctx.session.is_some());
        assert_eq!(ctx.user_email.as_deref(), Some(email));
        assert_eq!(ctx.user_did.as_deref(), Some(did.as_str()));
        assert_eq!(
            ctx.acts_as(),
            crate::agent::AgentContext::for_user_email(email.to_string())
        );
    }

    // A token that opens no session keeps the identity it had before sessions existed.
    #[test]
    fn a_user_without_a_loadable_identity_keeps_the_rest_fallback() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let email = "rest.nokey@example.org";
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let auth = AuthContext::open(
            &token,
            &Some("the-admin-credential".to_string()),
            false,
            ListenerReach::Network,
        );
        assert!(auth.session.is_none());
        assert!(auth.capabilities.is_ok(), "as `capabilities_on` gave it");
        assert_eq!(auth.user_email.as_deref(), Some(email));
        let ctx = auth.to_request_context();
        assert!(
            ctx.capabilities.is_err(),
            "the request context must not act"
        );
        assert!(ctx.user_did.is_none());
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
