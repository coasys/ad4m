//! OpenAI-shaped error envelope.
//!
//! Every fallible handler in this module returns [`OpenAIError`] so the
//! response always matches the spec:
//!
//! ```json
//! { "error": { "message": "...", "type": "...", "param": null, "code": "..." } }
//! ```
//!
//! [`OpenAIJson`] is a drop-in replacement for [`axum::Json`] that
//! converts axum's default plain-text/422 rejection body into the
//! OpenAI-envelope 400 that clients actually expect. All handlers
//! accepting JSON bodies use this so `Missing required field` etc.
//! parse-time errors reach the client through the correct wire shape.

use axum::{
    extract::{rejection::JsonRejection, FromRequest, Request},
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde::{de::DeserializeOwned, Serialize};

#[derive(Debug, Serialize)]
pub struct OpenAIError {
    #[serde(skip)]
    pub status: StatusCode,
    pub error: OpenAIErrorBody,
    /// Sent as `Retry-After`: an upstream's, passed on with its 429. Boxed: every handler
    /// returns this error, so it stays small.
    #[serde(skip)]
    pub retry_after: Option<Box<str>>,
}

#[derive(Debug, Serialize)]
pub struct OpenAIErrorBody {
    pub message: String,
    #[serde(rename = "type")]
    pub error_type: String,
    pub param: Option<String>,
    pub code: Option<String>,
}

impl OpenAIError {
    pub fn new(
        status: StatusCode,
        message: impl Into<String>,
        error_type: impl Into<String>,
        code: Option<&str>,
    ) -> Self {
        Self {
            status,
            error: OpenAIErrorBody {
                message: message.into(),
                error_type: error_type.into(),
                param: None,
                code: code.map(|s| s.to_string()),
            },
            retry_after: None,
        }
    }

    pub fn invalid_request(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::BAD_REQUEST,
            message,
            "invalid_request_error",
            Some("invalid_request"),
        )
    }

    pub fn unauthorized(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::UNAUTHORIZED,
            message,
            "invalid_request_error",
            Some("invalid_api_key"),
        )
    }

    pub fn forbidden(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::FORBIDDEN,
            message,
            "invalid_request_error",
            Some("insufficient_permissions"),
        )
    }

    pub fn insufficient_quota(message: impl Into<String>) -> Self {
        // OpenAI returns 429 for quota exhaustion; we follow suit so
        // libraries that retry on 429 do the right thing.
        Self::new(
            StatusCode::TOO_MANY_REQUESTS,
            message,
            "insufficient_quota",
            Some("insufficient_quota"),
        )
    }

    pub fn not_found(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::NOT_FOUND,
            message,
            "invalid_request_error",
            Some("not_found"),
        )
    }

    pub fn internal(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::INTERNAL_SERVER_ERROR,
            message,
            "server_error",
            None,
        )
    }

    pub fn not_implemented(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::NOT_IMPLEMENTED,
            message,
            "server_error",
            Some("not_implemented"),
        )
    }

    /// The request names a model whose endpoint cannot serve this route.
    pub fn model_not_supported(message: impl Into<String>) -> Self {
        Self::new(
            StatusCode::BAD_REQUEST,
            message,
            "invalid_request_error",
            Some("model_not_supported"),
        )
    }

    fn upstream_failure(status: StatusCode, message: &str, kind: &str) -> Self {
        Self::new(status, message, kind, Some(kind))
    }

    /// The client's view of a failure of the model's endpoint (plan §5.4). The messages
    /// name no URL (a user does not read endpoints), and an upstream 401 or 403 is never
    /// passed on: the client's own token was fine.
    pub fn from_upstream(error: &anyhow::Error) -> Self {
        use crate::ai_service::providers::http::{UpstreamStatus, UpstreamUnreachable};
        if let Some(unreachable) = error.downcast_ref::<UpstreamUnreachable>() {
            return if unreachable.timed_out {
                Self::upstream_failure(
                    StatusCode::GATEWAY_TIMEOUT,
                    "The model's endpoint did not answer in time",
                    "upstream_timeout",
                )
            } else {
                Self::upstream_failure(
                    StatusCode::SERVICE_UNAVAILABLE,
                    "The model's endpoint could not be reached",
                    "upstream_unavailable",
                )
            };
        }
        let Some(answered) = error.downcast_ref::<UpstreamStatus>() else {
            return Self::internal(error.to_string());
        };
        let reason = upstream_reason(&answered.body);
        match answered.status.as_u16() {
            401 | 403 => Self::upstream_failure(
                StatusCode::BAD_GATEWAY,
                "The model's endpoint refused this executor's credentials",
                "upstream_auth",
            ),
            404 => Self::new(
                StatusCode::NOT_FOUND,
                reason,
                "invalid_request_error",
                Some("model_not_found"),
            ),
            400 | 422 => Self::new(
                StatusCode::from_u16(answered.status.as_u16()).unwrap_or(StatusCode::BAD_REQUEST),
                reason,
                "invalid_request_error",
                Some("invalid_request"),
            ),
            429 => Self {
                retry_after: answered.retry_after.as_deref().map(Box::from),
                ..Self::upstream_failure(
                    StatusCode::TOO_MANY_REQUESTS,
                    &reason,
                    "rate_limit_exceeded",
                )
            },
            status => Self::upstream_failure(
                StatusCode::BAD_GATEWAY,
                &format!("The model's endpoint answered {status}"),
                "upstream_error",
            ),
        }
    }
}

/// The reason in an upstream's error body (already scrubbed of its key): OpenAI's
/// `error.message`, a bare `error` or FastAPI's `detail` string, else the body.
fn upstream_reason(body: &str) -> String {
    let parsed: Option<serde_json::Value> = serde_json::from_str(body).ok();
    parsed
        .as_ref()
        .and_then(|json| {
            json.pointer("/error/message")
                .or_else(|| json.get("error"))
                .or_else(|| json.get("detail"))
                .and_then(|reason| reason.as_str())
        })
        .map(str::to_string)
        .unwrap_or_else(|| body.to_string())
}

impl IntoResponse for OpenAIError {
    fn into_response(self) -> Response {
        let mut response = (
            self.status,
            Json(serde_json::json!({ "error": self.error })),
        )
            .into_response();
        if let Some(value) = self
            .retry_after
            .and_then(|value| axum::http::HeaderValue::from_str(&value).ok())
        {
            response
                .headers_mut()
                .insert(axum::http::header::RETRY_AFTER, value);
        }
        response
    }
}

impl From<crate::billing::BillingError> for OpenAIError {
    fn from(e: crate::billing::BillingError) -> Self {
        match e {
            crate::billing::BillingError::InsufficientCredits => {
                OpenAIError::insufficient_quota("Insufficient compute credits")
            }
            other => {
                log::error!("Billing operation failed: {other}");
                OpenAIError::internal("Billing operation failed")
            }
        }
    }
}

impl From<JsonRejection> for OpenAIError {
    /// Map axum's Json extractor rejection to the OpenAI 400 envelope.
    /// Without this, malformed / missing-field JSON returns axum's
    /// default plain-text 422, which OpenAI SDK clients can't parse.
    fn from(rej: JsonRejection) -> Self {
        OpenAIError::invalid_request(rej.body_text())
    }
}

/// Drop-in replacement for [`axum::Json`] that converts extraction
/// errors into the OpenAI error envelope with the correct status code.
///
/// Use `OpenAIJson<T>` in every /v1 handler that would otherwise write
/// `Json<T>`, so parse errors reach the client as:
///
/// ```json
/// { "error": { "message": "...", "type": "invalid_request_error", "code": "invalid_request" } }
/// ```
///
/// with HTTP 400 (matching OpenAI's own behaviour) instead of axum's
/// default plain-text `422 Unprocessable Entity`.
pub struct OpenAIJson<T>(pub T);

impl<T, S> FromRequest<S> for OpenAIJson<T>
where
    T: DeserializeOwned,
    S: Send + Sync,
{
    type Rejection = OpenAIError;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        let Json(value) = Json::<T>::from_request(req, state).await?;
        Ok(OpenAIJson(value))
    }
}

/// Result alias used by all handler functions in this module.
pub type OpenAIResult<T> = Result<T, OpenAIError>;
