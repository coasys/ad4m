//! `crate::api` on wasm32: api/mod.rs without the HTTP server. The ring-2 handler modules
//! are real; the other handler modules register nothing, so their methods answer 404
//! (method not found) in the web executor.
//!
//! Not here: `auth` and `ws_rpc` (the axum socket; its connection loop is `rpc_session`, which
//! the Worker feeds from `postMessage`), `internal` and `openai_compat` (HTTP only).

pub mod errors;
pub mod event_interest;
pub mod events_ws;
pub mod guards;
pub mod rpc_session;
pub mod types;

// ── WS-native handler modules ──
pub mod agent_ws;
pub mod perspective_instances_ws;
pub mod perspectives_ws;
pub mod ws_handler;

/// Handler modules outside ring 2: each registers no method on wasm32.
macro_rules! no_handlers {
    ($($module:ident),* $(,)?) => {
        $(
            pub mod $module {
                pub fn register_ws_handlers(_map: &mut super::ws_handler::HandlerMap) {}
            }
        )*
    };
}

no_handlers!(
    agent_session_ws,
    ai_sessions_ws,
    ai_ws,
    config_ws,
    expressions_ws,
    hosting_ws,
    languages_ws,
    neighbourhoods_ws,
    runtime_ws,
    sfu_ws,
    users_ws,
);

/// `identity.*` is outside ring 2: no method is registered, so the frame check that
/// `rpc_session` runs before dispatch has nothing to refuse.
pub mod identity_ws {
    pub fn register_ws_handlers(_map: &mut super::ws_handler::HandlerMap) {}

    /// Native: an `identity.publish` / `identity.cosignIncept` frame that names a member
    /// twice. Those methods answer 404 here, so there is never a refusal.
    pub(crate) fn duplicate_member_refusal(
        _method: &str,
        _frame: &str,
    ) -> Option<super::ws_handler::WsRpcError> {
        None
    }
}
