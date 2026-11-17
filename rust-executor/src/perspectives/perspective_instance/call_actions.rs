//! `call` actions on a shape (design §3): a constructor, setter or
//! destructor entry `{ "action": "call", "source": <url|"this">,
//! "predicate": <interaction name>, "target": <JSON object text> }` calls
//! into a language instead of writing links. `create_subject`,
//! `update_subject` and `remove_subject` make the calls first — before any
//! link run and outside any batch, since a language call cannot be rolled
//! back — and hand each call's `MountDelta` to `apply_call_delta`, where
//! A4 turns it into links.
//!
//! The action's own JSON `target` is parsed as a JSON object and the
//! caller's values merged over it; a call never goes through
//! `resolve_property_value` or `execute_commands`' parameter substitution
//! (§3.4). An action list that mixes calls and link actions is an
//! authoring error.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;

use super::{Command, PerspectiveInstance};
use crate::agent::AgentContext;

impl PerspectiveInstance {
    /// `create_subject` for a class whose constructor calls into a
    /// language. Not implemented yet — the test commit is red.
    pub(super) async fn create_through_call(
        &mut self,
        _class_name: &str,
        _commands: Vec<Command>,
        _initial_values: Option<Value>,
        _batch_id: Option<String>,
        _context: &AgentContext,
    ) -> Result<Option<String>, AnyError> {
        Err(anyhow!("call actions are not implemented"))
    }
}
