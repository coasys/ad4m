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
}

impl IntoResponse for OpenAIError {
    fn into_response(self) -> Response {
        (
            self.status,
            Json(serde_json::json!({ "error": self.error })),
        )
            .into_response()
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
