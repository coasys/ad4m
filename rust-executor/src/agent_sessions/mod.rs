//! Agent sessions: a conversation with the executor's agent, kept in the executor's own
//! database.
//!
//! An AD4M agent is a harness like OpenClaw: its sessions live in `agent_sessions` /
//! `agent_messages` ([`db::agent_sessions`]), not in a perspective — a session *works* in
//! the caller's perspectives through the same MCP tools an app uses, within the
//! intersection of the caller's and the session's own capabilities. Chat in a
//! neighbourhood is a different feature (the interpretation engine); nothing here writes
//! there.
//!
//! - [`types`] — the row and wire types: session, message, turn status, the
//!   `agent-session-changed` event payload.
//! - [`store`] — [`store::AgentSessionStore`], the seam the run code writes through, so a
//!   platform build can one day serve the rows from a central database.
//! - [`service`] — `agent.session*` calls and the detached-turn registry: one running
//!   turn per session, at most eight per caller, 15-minute timeout, cancel, startup
//!   sweep.
//! - [`run`] — one turn through the harness: transcript rebuild (tool calls included),
//!   system prompt, session-scoped MCP provider, credit gate.
//! - [`observer`] — the `ToolObserver` writing each tool call as a live `tool` transcript
//!   row.

mod observer;
pub mod run;
pub mod service;
pub mod store;
#[cfg(test)]
mod tests;
pub mod types;

pub use service::SessionError;
pub use store::{agent_session_store, AgentSessionStore};
pub use types::{
    AgentSession, AgentSessionChanged, AgentSessionMessage, AgentSessionMessageRole,
    AgentTurnStatus,
};
