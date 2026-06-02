//! Typed language errors. Spec §10.

use serde::{Deserialize, Serialize};
use wasm_bindgen::JsValue;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ErrorCode {
    NotFound,
    InvalidInput,
    PermissionDenied,
    Transient,
    Internal,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageError {
    pub code: ErrorCode,
    pub message: String,
}

impl LanguageError {
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self { code: ErrorCode::NotFound, message: msg.into() }
    }
    pub fn invalid_input(msg: impl Into<String>) -> Self {
        Self { code: ErrorCode::InvalidInput, message: msg.into() }
    }
    pub fn permission_denied(msg: impl Into<String>) -> Self {
        Self { code: ErrorCode::PermissionDenied, message: msg.into() }
    }
    pub fn transient(msg: impl Into<String>) -> Self {
        Self { code: ErrorCode::Transient, message: msg.into() }
    }
    pub fn internal(msg: impl Into<String>) -> Self {
        Self { code: ErrorCode::Internal, message: msg.into() }
    }
}

impl std::fmt::Display for LanguageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.code, self.message)
    }
}

impl std::error::Error for LanguageError {}

impl From<LanguageError> for JsValue {
    fn from(e: LanguageError) -> Self {
        serde_wasm_bindgen::to_value(&e).unwrap_or_else(|_| JsValue::from_str(&e.to_string()))
    }
}

impl From<serde_wasm_bindgen::Error> for LanguageError {
    fn from(e: serde_wasm_bindgen::Error) -> Self {
        Self::internal(format!("serde-wasm-bindgen: {e}"))
    }
}

impl From<serde_json::Error> for LanguageError {
    fn from(e: serde_json::Error) -> Self {
        Self::internal(format!("serde_json: {e}"))
    }
}

pub type LanguageResult<T> = Result<T, LanguageError>;
