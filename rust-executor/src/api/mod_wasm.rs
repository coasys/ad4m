//! `crate::api` on wasm32: api/mod.rs without the HTTP server. The ring-2 handler modules
//! are real; the other handler modules register nothing, so their methods answer 404
//! (method not found) in the web executor.
//!
//! Not here: `auth` and `ws_rpc` (the axum socket; S10 splits its frame loop out),
//! `internal` and `openai_compat` (HTTP only).

pub mod errors;
pub mod event_interest;
pub mod events_ws;
pub mod guards;
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
    identity_ws,
    languages_ws,
    neighbourhoods_ws,
    runtime_ws,
    sfu_ws,
    users_ws,
);
