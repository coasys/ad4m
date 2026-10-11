use std::sync::Arc;

use axum::{extract::FromRequestParts, http::request::Parts};

use super::errors::ApiError;
use crate::agent::capabilities::{Capability, ListenerReach};
use crate::agent::session::{AuthError, Session};
use crate::types::RequestContext;

/// Who is calling, resolved once from the token: by the HTTP extractor (REST and the
/// OpenAI-compatible API) per request, and by the RPC socket per connection.
#[derive(Clone)]
pub struct AuthContext {
    /// The session's capabilities. Always `Ok`: a token that opens no session gets no
    /// `AuthContext`.
    pub capabilities: Result<Vec<Capability>, String>,
    pub auto_permit_cap_requests: bool,
    pub auth_token: String,
    /// The caller's session.
    pub session: Arc<Session>,
}

impl AuthContext {
    /// The identity of a caller that presents `token` on a listener with this reach.
    ///
    /// Fails for a token that opens no [`Session`]: invalid, expired, revoked, or a user
    /// whose DID does not load. The caller refuses such a token where it arrives. An empty
    /// token opens a session (the operator or an anonymous caller).
    pub fn open(
        token: &str,
        admin_credential: &Option<String>,
        auto_permit_cap_requests: bool,
        reach: ListenerReach,
    ) -> Result<Self, AuthError> {
        let session = Session::open(token, admin_credential, reach)?;
        Ok(AuthContext {
            capabilities: Ok(session.capabilities()),
            auto_permit_cap_requests,
            auth_token: token.to_string(),
            session: Arc::new(session),
        })
    }

    /// The email of the user the session names, if it names one.
    pub fn user_email(&self) -> Option<&str> {
        self.session.user().map(|(email, _)| email)
    }

    /// Whether the caller is the node's operator. See [`RequestContext::is_operator`].
    pub fn is_operator(&self) -> bool {
        self.session.is_operator()
    }

    /// Convert to the existing RequestContext used by internal functions.
    pub fn to_request_context(&self) -> RequestContext {
        RequestContext {
            capabilities: self.capabilities.clone(),
            auto_permit_cap_requests: self.auto_permit_cap_requests,
            auth_token: self.auth_token.clone(),
            cancel_token: None,
            session: self.session.clone(),
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

        // A token that opens no session gets 401: the client logs in again. A locked wallet
        // gets 503: the token may be good, and the client retries after the unlock.
        let auth = AuthContext::open(
            &auth_header,
            &app_state.admin_credential,
            app_state.auto_permit_cap_requests,
            listener_reach(parts),
        )
        .map_err(|e| match e {
            AuthError::Locked => ApiError::Unavailable(e.to_string()),
            e => ApiError::Unauthorized(e.to_string()),
        })?;

        // Track last_seen for multi-user mode
        if let Some(email) = auth.user_email() {
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
    use crate::agent::AgentService;

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

        assert_eq!(auth.session.user(), Some((email, did.as_str())));
        assert_eq!(auth.user_email(), Some(email));
        assert!(!auth.is_operator());

        let ctx = auth.to_request_context();
        assert_eq!(ctx.user_email(), Some(email));
        assert_eq!(ctx.user_did(), Some(did.as_str()));
        assert_eq!(
            ctx.acts_as(),
            crate::agent::AgentContext::for_user_email(email.to_string())
        );
    }

    /// What the HTTP extractor makes of a request that carries `token`.
    async fn extract(token: &str) -> Result<AuthContext, ApiError> {
        let mut request = axum::http::Request::builder()
            .uri("/v1/models")
            .extension(ListenerReach::Network);
        if !token.is_empty() {
            request = request.header("Authorization", format!("Bearer {token}"));
        }
        let (mut parts, _) = request.body(()).unwrap().into_parts();
        let state = AppState {
            admin_credential: Some("the-admin-credential".to_string()),
            auto_permit_cap_requests: false,
        };
        AuthContext::from_request_parts(&mut parts, &state).await
    }

    // A user whose DID does not load used to get a request context with `Err` capabilities.
    #[test]
    fn a_user_without_a_loadable_identity_opens_no_auth_context() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let token =
            crate::user_management::generate_user_jwt("rest.nokey@example.org", "test").unwrap();
        let refused = AuthContext::open(
            &token,
            &Some("the-admin-credential".to_string()),
            false,
            ListenerReach::Network,
        );
        assert!(
            matches!(refused, Err(AuthError::NoIdentity(_))),
            "{:?}",
            refused.err()
        );
    }

    // A dead token used to reach the handler with `Err` capabilities, and failed there with
    // whatever that route answered. Now the client gets 401 and logs in again.
    #[tokio::test]
    async fn the_http_extractor_answers_401_for_a_token_that_opens_no_session() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let revoked = crate::agent::capabilities::token::generate_jwt(
            "a revoked app".to_string(),
            3600,
            crate::agent::capabilities::AuthInfo::default(),
        )
        .unwrap();
        let request_id = format!("extractor-{}", uuid::Uuid::new_v4());
        let app = crate::agent::capabilities::AuthInfoExtended {
            request_id: request_id.clone(),
            auth: crate::agent::capabilities::AuthInfo::default(),
        };
        crate::agent::capabilities::apps_map::insert_app(request_id.clone(), app, revoked.clone())
            .unwrap();
        crate::agent::capabilities::apps_map::revoke_app(&request_id).unwrap();

        let expired = crate::test_utils::expired_user_token("rest.expired@example.org");
        for token in [revoked.as_str(), expired.as_str(), "not-a-jwt"] {
            match extract(token).await {
                Err(ApiError::Unauthorized(reason)) => assert!(!reason.is_empty()),
                Err(other) => panic!("token {token:.20}…: expected 401, got {other:?}"),
                Ok(_) => panic!("token {token:.20}…: the extractor accepted a dead token"),
            }
        }
        let response =
            axum::response::IntoResponse::into_response(extract("not-a-jwt").await.err().unwrap());
        assert_eq!(response.status(), axum::http::StatusCode::UNAUTHORIZED);
    }

    #[tokio::test]
    async fn the_http_extractor_accepts_an_empty_token() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        // The anonymous set reads the multi-user setting. Initialise the database only when
        // no test has, so this does not wipe one another test is using.
        if crate::db::Ad4mDb::global_instance()
            .lock()
            .unwrap()
            .is_none()
        {
            crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        }
        let auth = extract("")
            .await
            .unwrap_or_else(|e| panic!("refused: {e:?}"));
        assert!(!auth.is_operator());
        assert!(matches!(
            auth.session.principal,
            crate::agent::session::Principal::Anonymous { .. }
        ));
    }
}
