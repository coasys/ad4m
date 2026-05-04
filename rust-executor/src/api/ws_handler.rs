//! WebSocket RPC handler infrastructure.
//!
//! Provides the types and registration mechanism for WS-native handlers.
//! Each domain module (agent, perspectives, runtime, etc.) registers its
//! handlers via `register_ws_handlers(&mut HandlerMap)` — no central route map.

use serde_json::Value;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use crate::types::RequestContext;

// ── Error type ──────────────────────────────────────────────────────────────

/// Error returned by WS RPC handlers.
/// Maps directly to the `{ "error": { "code": N, "message": "..." } }` wire format.
#[derive(Debug)]
pub struct WsRpcError {
    pub code: u16,
    pub message: String,
}

impl WsRpcError {
    pub fn bad_request(msg: impl Into<String>) -> Self {
        Self {
            code: 400,
            message: msg.into(),
        }
    }
    pub fn unauthorized(msg: impl Into<String>) -> Self {
        Self {
            code: 401,
            message: msg.into(),
        }
    }
    pub fn forbidden(msg: impl Into<String>) -> Self {
        Self {
            code: 403,
            message: msg.into(),
        }
    }
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self {
            code: 404,
            message: msg.into(),
        }
    }
    pub fn internal(msg: impl Into<String>) -> Self {
        Self {
            code: 500,
            message: msg.into(),
        }
    }
    pub fn not_implemented(msg: impl Into<String>) -> Self {
        Self {
            code: 501,
            message: msg.into(),
        }
    }
}

impl std::fmt::Display for WsRpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WsRpcError({}): {}", self.code, self.message)
    }
}

impl std::error::Error for WsRpcError {}

impl From<serde_json::Error> for WsRpcError {
    fn from(e: serde_json::Error) -> Self {
        Self::bad_request(format!("JSON error: {}", e))
    }
}

// ── Handler type ────────────────────────────────────────────────────────────

/// A boxed async handler function.
///
/// Takes `(params: Value, ctx: Arc<RequestContext>)` and returns `Result<Value, WsRpcError>`.
pub type WsHandler = Box<
    dyn Fn(
            Value,
            Arc<RequestContext>,
        ) -> Pin<Box<dyn Future<Output = Result<Value, WsRpcError>> + Send>>
        + Send
        + Sync,
>;

// ── Handler map ─────────────────────────────────────────────────────────────

/// Registry of message-type → handler mappings.
///
/// Built at startup by calling each module's `register_ws_handlers()`.
/// Used by the WS dispatcher to route incoming RPC messages.
pub struct HandlerMap {
    handlers: HashMap<String, WsHandler>,
}

impl HandlerMap {
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register a handler for a message type (e.g. `"agent.get"`).
    ///
    /// Panics if the type is already registered (catches duplicate registrations at startup).
    pub fn register<F, Fut>(&mut self, msg_type: &str, handler: F)
    where
        F: Fn(Value, Arc<RequestContext>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Value, WsRpcError>> + Send + 'static,
    {
        let msg_type = msg_type.to_string();
        if self.handlers.contains_key(&msg_type) {
            panic!("Duplicate WS handler registration for '{}'", msg_type);
        }
        self.handlers.insert(
            msg_type,
            Box::new(move |params, ctx| Box::pin(handler(params, ctx))),
        );
    }

    /// Dispatch an RPC message to the appropriate handler.
    pub async fn dispatch(
        &self,
        msg_type: &str,
        params: Value,
        ctx: Arc<RequestContext>,
    ) -> Result<Value, WsRpcError> {
        let handler = self
            .handlers
            .get(msg_type)
            .ok_or_else(|| WsRpcError::not_found(format!("Unknown type: {}", msg_type)))?;
        handler(params, ctx).await
    }

    /// Number of registered handlers (useful for logging at startup).
    pub fn len(&self) -> usize {
        self.handlers.len()
    }
}

// ── Build the handler map ───────────────────────────────────────────────────

/// Build the complete handler map by calling each module's registration function.
///
/// This is the ONLY place that lists modules — each module owns its own handlers.
pub fn build_handler_map() -> HandlerMap {
    let mut map = HandlerMap::new();
    super::agent_ws::register_ws_handlers(&mut map);
    super::perspectives_ws::register_ws_handlers(&mut map);
    super::runtime_ws::register_ws_handlers(&mut map);
    super::languages_ws::register_ws_handlers(&mut map);
    super::expressions_ws::register_ws_handlers(&mut map);
    super::ai_ws::register_ws_handlers(&mut map);
    super::neighbourhoods_ws::register_ws_handlers(&mut map);
    super::users_ws::register_ws_handlers(&mut map);
    super::hosting_ws::register_ws_handlers(&mut map);
    log::info!("WS RPC: registered {} handlers", map.len());
    map
}

// ── Param extraction helpers ────────────────────────────────────────────────

/// Helper trait for extracting typed values from JSON params.
pub trait ParamExt {
    /// Get a required string parameter.
    fn require_str(&self, key: &str) -> Result<String, WsRpcError>;
    /// Get an optional string parameter.
    fn opt_str(&self, key: &str) -> Option<String>;
    /// Get a required nested object/value parameter.
    fn require(&self, key: &str) -> Result<Value, WsRpcError>;
}

impl ParamExt for Value {
    fn require_str(&self, key: &str) -> Result<String, WsRpcError> {
        self.get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .ok_or_else(|| {
                WsRpcError::bad_request(format!("Missing required parameter: '{}'", key))
            })
    }

    fn opt_str(&self, key: &str) -> Option<String> {
        self.get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    }

    fn require(&self, key: &str) -> Result<Value, WsRpcError> {
        self.get(key).cloned().ok_or_else(|| {
            WsRpcError::bad_request(format!("Missing required parameter: '{}'", key))
        })
    }
}
