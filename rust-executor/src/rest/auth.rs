use axum::{extract::FromRequestParts, http::request::Parts};

use super::errors::ApiError;
use crate::agent::capabilities::{capabilities_from_token, is_admin_credential_token, Capability};
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

impl AuthContext {
    /// Convert to the existing RequestContext used by internal functions.
    pub fn to_request_context(&self) -> RequestContext {
        RequestContext {
            capabilities: self.capabilities.clone(),
            auto_permit_cap_requests: self.auto_permit_cap_requests,
            auth_token: self.auth_token.clone(),
            is_admin_credential: self.is_admin_credential,
        }
    }
}

/// AppState shared across all REST handlers.
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

        // Try Authorization header first, then fall back to ?token= query param
        // (needed for SSE/EventSource which doesn't support custom headers)
        let auth_header = parts
            .headers
            .get("authorization")
            .and_then(|v| v.to_str().ok())
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

        let capabilities =
            capabilities_from_token(auth_header.clone(), app_state.admin_credential.clone());
        let is_admin_credential =
            is_admin_credential_token(&auth_header, &app_state.admin_credential);

        Ok(AuthContext {
            capabilities,
            auto_permit_cap_requests: app_state.auto_permit_cap_requests,
            auth_token: auth_header,
            is_admin_credential,
        })
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
