//! `crate::perspectives::agent_session` on wasm32: no agent sessions (`perspective.runAgent`)
//! in the web executor. Only `tool_writes`, which the real `ai_sessions::types` names.

/// Every tool counts as a write: no tool list exists on wasm32 to say otherwise, and no
/// session runs a tool here.
pub fn tool_writes(_name: &str) -> bool {
    true
}
