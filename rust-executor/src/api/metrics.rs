//! `GET /metrics`: the executor's resource counters in the Prometheus text format
//! (`crate::metrics`).
//!
//! The output names users by DID and perspectives by id, so it is personal data: only the
//! operator reads it, by the same rule as `Guard::Operator`. That is the admin credential
//! (`Authorization: Bearer <credential>`), or no token on a loopback listener of an
//! executor without one. Every other caller gets 403; a token that opens no session gets
//! 401 from the extractor.

use axum::{
    http::{header, StatusCode},
    response::{IntoResponse, Response},
};

use super::errors::ApiError;
use super::guards::OPERATOR_ONLY;
use crate::types::RequestContext;

/// The content type of the Prometheus text exposition format.
pub const CONTENT_TYPE: &str = "text/plain; version=0.0.4; charset=utf-8";

/// `GET /metrics`, for the operator only.
pub async fn metrics(ctx: RequestContext) -> Result<Response, ApiError> {
    if !ctx.is_operator() {
        return Err(ApiError::Forbidden(OPERATOR_ONLY.to_string()));
    }
    let body = crate::metrics::render(&crate::metrics::snapshot().await);
    Ok((StatusCode::OK, [(header::CONTENT_TYPE, CONTENT_TYPE)], body).into_response())
}

#[cfg(test)]
mod tests {
    use crate::agent::capabilities::ListenerReach;
    use crate::api::auth::AppState;
    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use tower::ServiceExt;

    const ADMIN: &str = "metrics-admin-credential";

    async fn get(
        token: Option<&str>,
        reach: ListenerReach,
        admin: Option<&str>,
    ) -> (StatusCode, String) {
        let router = crate::api::api_router(AppState {
            admin_credential: admin.map(Into::into),
        })
        .layer(axum::Extension(reach));
        let mut request = Request::builder().uri("/metrics");
        if let Some(token) = token {
            request = request.header("Authorization", format!("Bearer {token}"));
        }
        let response = router
            .oneshot(request.body(Body::empty()).unwrap())
            .await
            .unwrap();
        let status = response.status();
        let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        (status, String::from_utf8_lossy(&bytes).to_string())
    }

    #[tokio::test]
    async fn the_admin_credential_reads_the_metrics() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let (status, body) = get(Some(ADMIN), ListenerReach::Network, Some(ADMIN)).await;
        assert_eq!(status, StatusCode::OK, "{body}");
        assert!(
            body.contains("# TYPE ad4m_perspectives_open gauge"),
            "{body}"
        );
        assert!(body.contains("ad4m_queries_in_flight "), "{body}");
    }

    #[tokio::test]
    async fn a_caller_without_the_admin_credential_is_refused() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        // No token on a network listener: anonymous.
        let (status, body) = get(None, ListenerReach::Network, Some(ADMIN)).await;
        assert_eq!(status, StatusCode::FORBIDDEN, "{body}");
        assert!(!body.contains("ad4m_"), "{body}");
        // No token on loopback when an admin credential is set: still anonymous.
        let (status, _) = get(None, ListenerReach::Loopback, Some(ADMIN)).await;
        assert_eq!(status, StatusCode::FORBIDDEN);
        // A wrong credential opens no session.
        let (status, _) = get(
            Some("not-the-credential"),
            ListenerReach::Network,
            Some(ADMIN),
        )
        .await;
        assert_eq!(status, StatusCode::UNAUTHORIZED);
    }

    #[tokio::test]
    async fn a_users_token_is_refused() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let email = "metrics.user@example.org";
        crate::test_utils::create_test_user(email);
        let token = crate::user_management::generate_user_jwt(email, "test").unwrap();
        let (status, body) = get(Some(&token), ListenerReach::Network, Some(ADMIN)).await;
        assert_eq!(status, StatusCode::FORBIDDEN, "{body}");
    }

    #[tokio::test]
    async fn no_token_on_loopback_without_an_admin_credential_is_the_operator() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let (status, _) = get(None, ListenerReach::Loopback, None).await;
        assert_eq!(status, StatusCode::OK);
        let (status, _) = get(None, ListenerReach::Network, None).await;
        assert_eq!(status, StatusCode::FORBIDDEN);
    }
}
