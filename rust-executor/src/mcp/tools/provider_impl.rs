//! `Ad4mToolProvider` — the harness-facing `ToolProvider` implementation
//! over `Ad4mMcpHandler`.
//!
//! Thin adapter: `tools()` and `call()` both delegate to the corresponding
//! `pub(crate)` methods on `Ad4mMcpHandler` (`list_tool_schemas` /
//! `call_tool_by_name`) which live in `harness_bridge.rs`. The provider
//! itself has no policy — filtering (e.g. read-only) is caller-composed via
//! `FilteredProvider`, not baked in here.
//!
//! Design: [[planning/llm-harness-design-2026-08-21-v3.md]] §6.

use super::Ad4mMcpHandler;
use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
use anyhow::Result;
use serde_json::Value;
use std::sync::Arc;

/// Adapts the AD4M MCP handler to the harness's `ToolProvider` trait so the
/// interpretation-pass LLM sees the same tool surface the MCP transport
/// exposes to external clients.
///
/// Held by `Arc<Ad4mMcpHandler>` because the handler is `Clone`-cheap but
/// carries no shared-state guarantees — an Arc lets the harness share one
/// instance across the loop iterations (each iteration re-reads `tools()`)
/// and across concurrent passes if a future refactor moves that way.
pub struct Ad4mToolProvider {
    handler: Arc<Ad4mMcpHandler>,
}

impl Ad4mToolProvider {
    pub fn new(handler: Arc<Ad4mMcpHandler>) -> Self {
        Self { handler }
    }
}

#[async_trait::async_trait]
impl ToolProvider for Ad4mToolProvider {
    async fn tools(&self) -> Vec<ToolSchema> {
        // Re-read on every call — dynamic per-class tools reflect the
        // currently-registered SHACL shape, so a class registration mid-pass
        // is visible on the next loop iteration. `handler.list_tool_schemas`
        // does the merge (static router + dynamic per-class).
        self.handler.list_tool_schemas().await
    }

    async fn call(&self, name: &str, args: Value) -> Result<String> {
        // Case-insensitive name resolution BEFORE dispatch, not as a retry
        // on error. Rationale (Lal's PR #911 review, provider_impl.rs:55):
        // if the tool exists under the LLM's spelling but errors partway
        // through a side-effectful path (permission denied after inserting a
        // link, network timeout mid-write), an on-error retry would
        // re-dispatch with the same args — potentially double-writing. The
        // earlier match-on-Err retry gated only on "canonical name differs
        // from what was called" which still permits double-dispatch when the
        // model happened to lowercase a name.
        //
        // Resolve first: look up the canonical spelling in the current tool
        // list. Dispatch exactly once with the canonical name (or the
        // caller's name if no case-insensitive candidate exists — in which
        // case the underlying handler's own "unknown tool" error is what
        // surfaces).
        let tools = self.handler.list_tool_schemas().await;
        let canonical = tools
            .iter()
            .find(|t| t.name.eq_ignore_ascii_case(name))
            .map(|t| t.name.as_str())
            .unwrap_or(name);
        self.handler.call_tool_by_name(canonical, args).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // Note: exercising the ToolProvider impl in isolation requires an
    // `Ad4mMcpHandler` (which pulls in the full MCP + PerspectiveInstance
    // stack); regression coverage for the "no retry on error" contract lives
    // in the e2e scenarios (which run the full stack against real perspectives
    // + a real LLM). The behavioural change is trivially reviewable in the
    // diff — the earlier `match Err(_) => { ... retry ... }` branch is gone.
    #[test]
    fn provider_impl_module_compiles() {
        // Marker test — the harness module's own scripted-LLM tests hit the
        // dispatch path via a fake provider (see provider.rs tests). This
        // file's impl over the real handler is covered end-to-end.
        fn _assert_send_sync<T: Send + Sync>() {}
        _assert_send_sync::<Ad4mToolProvider>();
    }
}
