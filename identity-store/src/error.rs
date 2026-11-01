//! Refusals and failures as HTTP answers. Every body is JSON with an `error` name; the agent
//! language maps the status and the body onto `IdentityErrorCode`.

use ad4m_ldk::identity::SignedHead;
use axum::http::{header, HeaderValue, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::Json;
use serde_json::{json, Value};

/// An answer other than success.
#[derive(Debug, Clone, PartialEq)]
pub struct ApiError {
    pub status: StatusCode,
    pub body: Value,
}

impl ApiError {
    fn new(status: StatusCode, body: Value) -> Self {
        ApiError { status, body }
    }

    /// `400 {error}`: malformed, or a `ChainError` / `FoldError` name.
    pub fn invalid(error: &str, detail: impl Into<String>) -> Self {
        Self::new(
            StatusCode::BAD_REQUEST,
            json!({ "error": error, "detail": detail.into() }),
        )
    }

    /// `403 {error}` with a reason of `IdentityErrorCode::forbidden`.
    pub fn forbidden(reason: &str, detail: impl Into<String>) -> Self {
        Self::new(
            StatusCode::FORBIDDEN,
            json!({ "error": reason, "detail": detail.into() }),
        )
    }

    /// `404`.
    pub fn not_found(detail: impl Into<String>) -> Self {
        Self::new(
            StatusCode::NOT_FOUND,
            json!({ "error": "not-found", "detail": detail.into() }),
        )
    }

    /// `409 {head}`: the log moved; the body carries the current signed head.
    pub fn conflict(head: &SignedHead) -> Self {
        Self::new(
            StatusCode::CONFLICT,
            json!({ "error": "conflict", "head": head }),
        )
    }

    /// `409 {error:"key-taken", key}` (the one-key check).
    pub fn key_taken(key: &str) -> Self {
        Self::new(
            StatusCode::CONFLICT,
            json!({ "error": "key-taken", "key": key }),
        )
    }

    /// `409 {error, …}` for a conflict that carries no head.
    pub fn conflict_with(error: &str, extra: Value) -> Self {
        let mut body = json!({ "error": error });
        if let (Some(body), Some(extra)) = (body.as_object_mut(), extra.as_object()) {
            for (k, v) in extra {
                body.insert(k.clone(), v.clone());
            }
        }
        Self::new(StatusCode::CONFLICT, body)
    }

    /// `410 {state}`: an offer that ended.
    pub fn gone(state: &str) -> Self {
        Self::new(StatusCode::GONE, json!({ "error": "gone", "state": state }))
    }

    /// `413`.
    pub fn too_large() -> Self {
        Self::new(
            StatusCode::PAYLOAD_TOO_LARGE,
            json!({ "error": "too-large" }),
        )
    }

    /// `429 {scope, retryAfter}` with `Retry-After`.
    pub fn rate_limited(scope: &str, retry_after: u64) -> Self {
        Self::new(
            StatusCode::TOO_MANY_REQUESTS,
            json!({ "error": "rate-limited", "scope": scope, "retryAfter": retry_after }),
        )
    }

    /// `401`: no or a wrong mirror token.
    pub fn unauthorized() -> Self {
        Self::new(StatusCode::UNAUTHORIZED, json!({ "error": "unauthorized" }))
    }

    /// `503 {error}`: a service the store depends on did not answer.
    pub fn unavailable(error: &str) -> Self {
        Self::new(StatusCode::SERVICE_UNAVAILABLE, json!({ "error": error }))
    }

    /// `500`: the store failed (its database); nothing was committed.
    pub fn internal(detail: impl Into<String>) -> Self {
        Self::new(
            StatusCode::INTERNAL_SERVER_ERROR,
            json!({ "error": "internal", "detail": detail.into() }),
        )
    }
}

impl From<rusqlite::Error> for ApiError {
    fn from(e: rusqlite::Error) -> Self {
        ApiError::internal(e.to_string())
    }
}

impl From<ad4m_hash_chain::store::StoreError> for ApiError {
    fn from(e: ad4m_hash_chain::store::StoreError) -> Self {
        ApiError::internal(e.to_string())
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let retry = self.body.get("retryAfter").and_then(Value::as_u64);
        let mut response = (self.status, Json(self.body)).into_response();
        if let Some(retry) = retry {
            if let Ok(value) = HeaderValue::from_str(&retry.to_string()) {
                response.headers_mut().insert(header::RETRY_AFTER, value);
            }
        }
        response
    }
}
