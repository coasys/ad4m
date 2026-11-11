//! Where a session's durable state is written: [`AgentSessionStore`].
//!
//! One seam between the run code and the tables: the hosted platform may later move these
//! rows to a central database, so nothing reads or writes `agent_sessions` /
//! `agent_messages` without going through the trait. The shipped implementation runs on
//! the executor's `DbBackend` — `LocalDb` and `SharedDb` both serve domain methods from the
//! executor's SQLite — and nothing about a run lives only in memory here: a turn's status is
//! the row's `status`, the running-task registry in `service` only holds the tokio handle.
//!
//! Every method takes the owner's DID (`None`: the main agent) and scopes to it, so a
//! caller can never see or touch another agent's session, whichever store answers.

use std::sync::{Arc, RwLock};

use deno_core::error::AnyError;
use serde_json::Value;

use super::types::{
    AgentSession, AgentSessionMessage, AgentSessionMessageRole, AgentSessionUpdate, AgentTurnStatus,
};
use crate::db_backend::db_backend;

/// The `agent_sessions` and `agent_messages` tables, behind one trait.
pub trait AgentSessionStore: Send + Sync {
    /// `session`, written.
    fn create_session(&self, session: &AgentSession) -> Result<(), AnyError>;
    /// The session `id` of `owner_did` (`None`: the main agent), `None` when it is missing
    /// or another agent's.
    fn session(&self, owner_did: Option<&str>, id: &str) -> Result<Option<AgentSession>, AnyError>;
    /// The session `id` whoever owns it: for the executor's own lookups, never for a
    /// caller's word.
    fn session_any(&self, id: &str) -> Result<Option<AgentSession>, AnyError>;
    /// Every session of `owner_did` (`None`: the main agent), newest first.
    fn sessions(&self, owner_did: Option<&str>) -> Result<Vec<AgentSession>, AnyError>;
    /// The fields `update` names changed. `false`: no such session of this owner.
    fn update_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
        update: &AgentSessionUpdate,
    ) -> Result<bool, AnyError>;
    /// The session `id` of `owner_did` and its transcript, gone. `false`: no such session
    /// of this owner.
    fn delete_session(&self, owner_did: Option<&str>, id: &str) -> Result<bool, AnyError>;

    /// The transcript entry `id`, `role`, `content`, `status` appended to the session
    /// `session_id` of `owner_did` with its next `seq`. `None`: no such session of this
    /// owner.
    fn append_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        session_id: &str,
        role: AgentSessionMessageRole,
        content: &Value,
        status: Option<AgentTurnStatus>,
    ) -> Result<Option<AgentSessionMessage>, AnyError>;
    /// The message `id` of a session `owner_did` owns.
    fn message(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AgentSessionMessage>, AnyError>;
    /// The turn's end written on `id`: `status`, `error`, `content_patch` merged into
    /// `content`. `false`: no such message of this owner.
    fn finish_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        status: AgentTurnStatus,
        error: Option<&str>,
        content_patch: Option<&Value>,
    ) -> Result<bool, AnyError>;
    /// The newest `limit` transcript entries of the session, in `seq` order; with `before`,
    /// only those older than that `seq`. Empty for a missing or foreign session.
    fn messages(
        &self,
        owner_did: Option<&str>,
        session_id: &str,
        limit: u32,
        before: Option<i64>,
    ) -> Result<Vec<AgentSessionMessage>, AnyError>;
    /// Every `running` row — of the session `session_id`, or of the whole table when none is
    /// named — marked `interrupted`. How many it caught.
    fn interrupt_running(&self, session_id: Option<&str>) -> Result<u64, AnyError>;
}

/// [`AgentSessionStore`] on the executor's `DbBackend`: the executor's SQLite tables on
/// `LocalDb` and on `SharedDb` (domain methods stay local there; only the shared key-value
/// records go to the Worker).
pub struct DbAgentSessionStore;

impl AgentSessionStore for DbAgentSessionStore {
    fn create_session(&self, session: &AgentSession) -> Result<(), AnyError> {
        db_backend()
            .create_agent_session(session)
            .map_err(Into::into)
    }

    fn session(&self, owner_did: Option<&str>, id: &str) -> Result<Option<AgentSession>, AnyError> {
        db_backend()
            .get_agent_session(owner_did, id)
            .map_err(Into::into)
    }

    fn session_any(&self, id: &str) -> Result<Option<AgentSession>, AnyError> {
        db_backend().get_agent_session_any(id).map_err(Into::into)
    }

    fn sessions(&self, owner_did: Option<&str>) -> Result<Vec<AgentSession>, AnyError> {
        db_backend()
            .list_agent_sessions(owner_did)
            .map_err(Into::into)
    }

    fn update_session(
        &self,
        owner_did: Option<&str>,
        id: &str,
        update: &AgentSessionUpdate,
    ) -> Result<bool, AnyError> {
        db_backend()
            .update_agent_session(owner_did, id, update)
            .map_err(Into::into)
    }

    fn delete_session(&self, owner_did: Option<&str>, id: &str) -> Result<bool, AnyError> {
        db_backend()
            .delete_agent_session(owner_did, id)
            .map_err(Into::into)
    }

    fn append_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        session_id: &str,
        role: AgentSessionMessageRole,
        content: &Value,
        status: Option<AgentTurnStatus>,
    ) -> Result<Option<AgentSessionMessage>, AnyError> {
        db_backend()
            .append_agent_message(
                owner_did,
                id,
                session_id,
                role.as_str(),
                &content.to_string(),
                status.map(AgentTurnStatus::as_str),
            )
            .map_err(Into::into)
    }

    fn message(
        &self,
        owner_did: Option<&str>,
        id: &str,
    ) -> Result<Option<AgentSessionMessage>, AnyError> {
        db_backend()
            .get_agent_message(owner_did, id)
            .map_err(Into::into)
    }

    fn finish_message(
        &self,
        owner_did: Option<&str>,
        id: &str,
        status: AgentTurnStatus,
        error: Option<&str>,
        content_patch: Option<&Value>,
    ) -> Result<bool, AnyError> {
        db_backend()
            .finish_agent_message(owner_did, id, status.as_str(), error, content_patch)
            .map_err(Into::into)
    }

    fn messages(
        &self,
        owner_did: Option<&str>,
        session_id: &str,
        limit: u32,
        before: Option<i64>,
    ) -> Result<Vec<AgentSessionMessage>, AnyError> {
        db_backend()
            .list_agent_messages(owner_did, session_id, limit, before)
            .map_err(Into::into)
    }

    fn interrupt_running(&self, session_id: Option<&str>) -> Result<u64, AnyError> {
        db_backend()
            .interrupt_running_agent_messages(session_id)
            .map_err(Into::into)
    }
}

static STORE: RwLock<Option<Arc<dyn AgentSessionStore>>> = RwLock::new(None);

/// The store the run code uses. [`DbAgentSessionStore`] until a test or a platform build
/// sets another.
pub fn agent_session_store() -> Arc<dyn AgentSessionStore> {
    match STORE.read().unwrap().as_ref() {
        Some(store) => store.clone(),
        None => Arc::new(DbAgentSessionStore),
    }
}

/// The store the run code uses from now on, or `None` for the default again. A test that
/// sets one restores the default when it ends so the next test sees a clean table through
/// its own backend.
#[cfg(test)]
pub fn set_agent_session_store(store: Option<Arc<dyn AgentSessionStore>>) {
    *STORE.write().unwrap() = store;
}
