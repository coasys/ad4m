//! `Ad4mMcpHandler` → harness bridge.
//!
//! Two `pub(crate)` methods on `Ad4mMcpHandler` that let the LLM harness use
//! the MCP tool surface without going through the `rmcp` transport:
//!
//! * `list_tool_schemas()` — returns every currently-exposed tool (both the
//!   static `#[tool]` methods registered on `tool_router` AND the dynamic
//!   per-class SHACL tools generated from the perspective's shape) as
//!   `Vec<ToolSchema>`, the harness-facing type.
//! * `call_tool_by_name(name, args)` — dispatches a tool call by name,
//!   returning the tool's textual result. Bypasses `rmcp`'s `ToolRouter::call`
//!   because that requires a `RequestContext<RoleServer>` whose `Peer` has a
//!   `pub(crate)` constructor — impossible to fabricate from outside rmcp.
//!
//! ## Why dispatch is duplicated (design v3 §6 caveat)
//!
//! Design v3 §6 says "reuse MCP `#[tool]` macros as the single source of
//! truth." Tool metadata IS reused verbatim via `tool_router.list_all()` —
//! descriptions, JSON schemas, names all flow from the same `#[tool(...)]`
//! attributes the MCP transport reads. Only the DISPATCH is duplicated,
//! because rmcp's router entrypoint requires a `Peer` we cannot construct.
//!
//! The dispatch match below covers the interpretation-pass read subset first
//! (see [[planning/llm-harness-design-2026-08-21-v3.md]] §1). Additional
//! tools are one match arm each; extend as harness use cases surface them.
//! Any tool not in the match delegates to `handle_dynamic_tool`, which
//! covers ALL per-class SHACL tools (`Task_query`, `Channel_create`, etc.)
//! — that path already exists and works without any context.

use super::Ad4mMcpHandler;
use crate::ai_service::harness::provider::ToolSchema;
use anyhow::{anyhow, Result};
use rmcp::handler::server::wrapper::Parameters;
use serde_json::Value;

impl Ad4mMcpHandler {
    /// Enumerate every tool this handler exposes, in the shape the harness
    /// consumes. Merges the router-registered static tools with the dynamic
    /// per-class SHACL tools. Ordering is stable within a router but not
    /// across (map iteration order); tests that need determinism should
    /// sort.
    pub(crate) async fn list_tool_schemas(&self) -> Vec<ToolSchema> {
        let mut out = Vec::new();

        // Static tools — every #[tool]-annotated method registered on the
        // router. `attr` is the rmcp `Tool` with name / description /
        // input_schema — convert directly.
        for tool in self.tool_router.list_all() {
            out.push(rmcp_tool_to_schema(&tool));
        }

        // Dynamic tools — per-class SHACL-generated (create/query/list/get/
        // delete + property setters + collection ops). Already returns
        // `Vec<Tool>`; convert the same way.
        for tool in self.generate_dynamic_tools().await {
            out.push(rmcp_tool_to_schema(&tool));
        }

        out
    }

    /// Dispatch a tool call by name. Returns the tool's raw text result
    /// (what the LLM sees as the next `role: "tool"` message content).
    ///
    /// Dispatch strategy:
    /// 1. If `name` matches one of the interpretation-relevant static tools,
    ///    deserialize `args` into the tool's `Parameters<T>` and call the
    ///    method directly. The method returns `String` already.
    /// 2. Otherwise, delegate to `handle_dynamic_tool` — that covers every
    ///    per-class SHACL tool (`Task_query`, `Channel_create`, etc.) and
    ///    returns a `CallToolResult` we flatten to text.
    /// 3. If neither matches, error — the harness turns errors into
    ///    `role: "tool"` content prefixed `error:` so the LLM can recover.
    ///
    /// The static-tool arm covers the read subset needed by the initial
    /// harness wiring. Additional static tools land here one arm at a time;
    /// the pattern is mechanical.
    pub(crate) async fn call_tool_by_name(&self, name: &str, args: Value) -> Result<String> {
        // The seven static-tool arms below all deserialize into a distinct
        // `Parameters<T>` where T is the argument struct on the
        // `#[tool]`-annotated method. That's why they can't collapse into
        // a `match` value + a shared closure — each T's turbofish is
        // per-call. The `dispatch_static_tool!` macro paves the pattern:
        // it inlines the deserialize + Parameters wrap + await, with a
        // uniform error message that names the tool the LLM asked for.
        //
        // Adding a static tool: add one line here naming the method. If
        // the method isn't in scope yet, add a `use` at the top of the
        // module or reach it via `self`. Lal's PR #911 review
        // (harness_bridge.rs:78) flagged the mechanical repetition —
        // this macro is the DRY that keeps future additions honest
        // without adding runtime cost or dyn-dispatch.
        macro_rules! dispatch_static_tool {
            ($method:ident) => {{
                let params = serde_json::from_value(args).map_err(|e| {
                    anyhow!(
                        "failed to deserialize `{}` arguments: {e}",
                        stringify!($method)
                    )
                })?;
                return Ok(self.$method(Parameters(params)).await);
            }};
        }

        match name {
            // ── perspectives.rs ─────────────────────────────────────────
            "list_perspectives" => dispatch_static_tool!(list_perspectives),
            "get_models" => dispatch_static_tool!(get_models),
            "query_links" => dispatch_static_tool!(query_links),
            "infer" => dispatch_static_tool!(infer),
            // ── subjects.rs ─────────────────────────────────────────────
            "query_subjects" => dispatch_static_tool!(query_subjects),
            "get_subject_data" => dispatch_static_tool!(get_subject_data),
            "get_subject_children" => dispatch_static_tool!(get_subject_children),
            // ── fallback: per-class dynamic tools ───────────────────────
            _ => {
                // `handle_dynamic_tool` expects the args as a
                // `Option<Map<String, Value>>`. Coerce our JSON Value into
                // that shape — non-object arguments become an empty map so
                // the callee can produce its own "wrong shape" error message.
                let arg_map = match args {
                    Value::Object(m) => Some(m),
                    Value::Null => None,
                    _ => None,
                };
                let result = self
                    .handle_dynamic_tool(name, arg_map)
                    .await
                    .map_err(|e| anyhow!("dynamic tool `{name}` failed: {e}"))?;
                Ok(call_tool_result_to_text(&result))
            }
        }
    }
}

/// Convert an rmcp `Tool` into the harness-facing `ToolSchema`.
///
/// rmcp stores the input schema as `Arc<serde_json::Map<String, Value>>`
/// while the harness wants a plain `Value` (so it can be passed straight
/// into an OpenAI `tools[]` entry via `to_openai_tool_entry`). One clone;
/// tool metadata is small (10s to low-100s of bytes).
fn rmcp_tool_to_schema(tool: &rmcp::model::Tool) -> ToolSchema {
    let parameters = Value::Object((*tool.input_schema).clone());
    ToolSchema {
        name: tool.name.to_string(),
        description: tool
            .description
            .as_ref()
            .map(|d| d.to_string())
            .unwrap_or_default(),
        parameters,
    }
}

/// Flatten a `CallToolResult` into the plain-text string the harness passes
/// as tool_result content. `handle_dynamic_tool` returns either a success
/// with `Content::text` children or an error with the same shape — join the
/// text children with newlines and let the LLM read whatever's there.
fn call_tool_result_to_text(result: &rmcp::model::CallToolResult) -> String {
    result
        .content
        .iter()
        .filter_map(|c| c.as_text().map(|t| t.text.clone()))
        .collect::<Vec<_>>()
        .join("\n")
}

// Unit tests deferred to the same follow-up commit that adds the first e2e
// test on Marvin's Ollama tunnel: the two pure fns below (`rmcp_tool_to_schema`
// + `call_tool_result_to_text`) get exercised transitively when the harness
// loop calls into a real `Ad4mMcpHandler`, which is a stronger check than
// constructing rmcp `Tool` / `Content` values by hand (their pub-field surface
// shifts across patch bumps).
