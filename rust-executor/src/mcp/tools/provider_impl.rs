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
        // Case-insensitive fallback: some models (e.g. Gemma-3 12B) lowercase
        // tool names even when the surface spells them `Task_propose_create`.
        // Try the exact name first (fast path, avoids listing tools); on a
        // miss, scan the current tool list for a case-insensitive hit and
        // dispatch with the canonical spelling.
        match self.handler.call_tool_by_name(name, args.clone()).await {
            Ok(text) => Ok(text),
            Err(e) => {
                let tools = self.handler.list_tool_schemas().await;
                if let Some(canonical) = tools
                    .iter()
                    .find(|t| t.name.eq_ignore_ascii_case(name))
                    .map(|t| t.name.clone())
                {
                    if canonical != name {
                        self.handler.call_tool_by_name(&canonical, args).await
                    } else {
                        Err(e)
                    }
                } else {
                    Err(e)
                }
            }
        }
    }
}
