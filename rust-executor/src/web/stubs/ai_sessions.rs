//! `crate::ai_sessions` on wasm32: no AI chat sessions run in the web executor.
//!
//! Real: `types` (the native file by path; the database and the event list name them).
//! Stubbed: `ai_session_store()`, which only looks a session up in the database, as the
//! native store does. No `ai.session*` method is registered, so the table stays empty.

#[path = "../../ai_sessions/types.rs"]
pub mod types;

use crate::db_backend::db_backend;
use anyhow::Error as AnyError;
use std::sync::Arc;
use types::AiSession;

/// The part of the native `AiSessionStore` that ring 2 calls.
pub struct AiSessionStore;

impl AiSessionStore {
    /// The session `id` whoever owns it.
    pub fn session_any(&self, id: &str) -> Result<Option<AiSession>, AnyError> {
        db_backend().get_ai_session_any(id).map_err(Into::into)
    }
}

pub fn ai_session_store() -> Arc<AiSessionStore> {
    Arc::new(AiSessionStore)
}
