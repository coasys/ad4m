//! `ToolProvider` — the shared seam between MCP and the interpretation-pass
//! LLM harness. Design: [[planning/llm-harness-design-2026-08-21-v3.md]] §6
//! + v2 §Q4.
//!
//! Both the `rmcp` transport and the harness loop consume the same tool
//! metadata + dispatch through this trait. No duplication: if Josh adds a
//! `#[tool]` on `Ad4mMcpHandler` tomorrow, both surfaces get it.
//!
//! Not tied to MCP transport concerns (JWT injection, streaming ping frames,
//! rmcp's `RequestContext`). Those live in the MCP layer.

use anyhow::Result;
use serde_json::{json, Value};
use std::sync::Arc;

/// LLM-facing description of one tool. The three fields map 1-to-1 onto the
/// OpenAI `tools[]` request array (function.name / function.description /
/// function.parameters). No wrapper: the harness passes this straight into
/// `chat_gpt_lib_rs` / kalosm and the LLM sees exactly what's here.
///
/// `parameters` is a JSON Schema fragment describing the argument object.
/// Zero-arg tools use `{"type":"object","properties":{},"required":[]}` —
/// don't drop the object wrapper (OpenAI + kalosm both reject bare types).
#[derive(Debug, Clone, PartialEq)]
pub struct ToolSchema {
    pub name: String,
    pub description: String,
    pub parameters: Value,
}

impl ToolSchema {
    /// Cheap constructor for zero-arg tools — the empty-object schema is
    /// verbose enough that inlining it everywhere hurts readability.
    pub fn zero_arg(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            parameters: json!({ "type": "object", "properties": {}, "required": [] }),
        }
    }

    /// Render as an OpenAI `tools[]` entry (the `{type:"function", function:{...}}`
    /// wrapper the /v1/chat/completions endpoint expects). Kept here rather
    /// than in the harness loop so both consumers agree on the wire shape.
    pub fn to_openai_tool_entry(&self) -> Value {
        json!({
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": self.parameters,
            }
        })
    }
}

/// One shared source of truth for the tool surface — both the `rmcp` transport
/// and the interpretation harness dispatch through this.
///
/// Implementations MUST make `tools()` cheap: the harness re-reads it every
/// iteration and the MCP transport re-reads it on every `list_tools` request.
/// Dynamic per-class tools (generated from currently-registered SHACL
/// classes) are recomputed on each `tools()` call by design — that keeps the
/// tool surface in sync with the perspective's current shape without any
/// invalidation bookkeeping (design v2 §3: "not doing tool caching").
///
/// `call()` returns a plain `String` result. The harness slots it in as the
/// content of the next `role: "tool"` message; the MCP transport wraps it
/// into `CallToolResult` with a single text `Content`. Errors bubble up as
/// `anyhow::Error` — the harness converts to an error `role: "tool"` payload
/// so the LLM can recover, the MCP transport surfaces the error text via
/// rmcp's `is_error` flag.
#[async_trait::async_trait]
pub trait ToolProvider: Send + Sync {
    /// Enumerate every tool this provider currently exposes. Ordering must
    /// be stable within a process (unit tests assert this).
    async fn tools(&self) -> Vec<ToolSchema>;

    /// Dispatch a tool call by name. `args` is the JSON object the LLM
    /// emitted as `function.arguments`. Returns the tool's result text.
    async fn call(&self, name: &str, args: Value) -> Result<String>;
}

/// Filter that narrows one provider's surface to a caller-defined subset.
/// Enables the per-flow-step allowlist (v3 §1) + the interpretation-pass
/// read-only cut (v2 §Q5) without touching the underlying provider.
///
/// Kept generic (predicate on `&ToolSchema`) rather than a hard-coded
/// allowlist so callers can filter by name prefix, by description content,
/// or by structural properties of the schema — whatever the use case wants.
pub struct FilteredProvider<P: ToolProvider> {
    inner: Arc<P>,
    predicate: Arc<dyn Fn(&ToolSchema) -> bool + Send + Sync>,
}

impl<P: ToolProvider> FilteredProvider<P> {
    pub fn new(
        inner: Arc<P>,
        predicate: impl Fn(&ToolSchema) -> bool + Send + Sync + 'static,
    ) -> Self {
        Self {
            inner,
            predicate: Arc::new(predicate),
        }
    }
}

#[async_trait::async_trait]
impl<P: ToolProvider + 'static> ToolProvider for FilteredProvider<P> {
    async fn tools(&self) -> Vec<ToolSchema> {
        self.inner
            .tools()
            .await
            .into_iter()
            .filter(|t| (self.predicate)(t))
            .collect()
    }

    async fn call(&self, name: &str, args: Value) -> Result<String> {
        // The predicate gates *advertisement*, not dispatch. A well-behaved
        // LLM only calls what it saw in `tools()`, but a misbehaving one
        // could try a filtered-out name — re-check on dispatch so a caller
        // that filters out `_delete` tools can't be tricked into running one
        // via a hallucinated call. The alternative (allow dispatch of any
        // upstream tool) would defeat the point of the filter.
        let allowed = self
            .inner
            .tools()
            .await
            .into_iter()
            .any(|t| t.name == name && (self.predicate)(&t));
        if !allowed {
            anyhow::bail!("tool `{name}` is not in the current provider's filtered surface");
        }
        self.inner.call(name, args).await
    }
}

/// Convenience predicate: drop any tool whose name starts with a write-adjacent
/// verb. This is the interpretation-pass read-only cut (v2 §Q5) — the harness
/// wraps `Ad4mToolProvider` in `FilteredProvider::new(_, is_read_only)` before
/// handing it to a pass. Kept as a free function so it can be composed.
///
/// The list is deliberately conservative — a tool whose *effect* is a write
/// but whose name doesn't match one of these prefixes will slip through. Add
/// to it as the MCP surface grows write-adjacent verbs; test
/// `readonly_filter_excludes_add_delete_set` pins the current expected
/// behaviour.
pub fn is_read_only(t: &ToolSchema) -> bool {
    const WRITE_PREFIXES: &[&str] = &["add_", "remove_", "delete_", "create_", "update_", "set_"];
    // Per-class dynamic write verbs (see mcp::tools::dynamic): `<class>_create`,
    // `<class>_delete`, `<class>_set_<prop>`, `<class>_add_to_<coll>`,
    // `<class>_remove_from_<coll>`.
    const WRITE_SUFFIXES: &[&str] = &["_create", "_delete"];
    const WRITE_INFIXES: &[&str] = &["_set_", "_add_to_", "_remove_from_"];

    if WRITE_PREFIXES.iter().any(|p| t.name.starts_with(p)) {
        return false;
    }
    if WRITE_SUFFIXES.iter().any(|s| t.name.ends_with(s)) {
        return false;
    }
    if WRITE_INFIXES.iter().any(|i| t.name.contains(i)) {
        return false;
    }
    true
}

// ── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// Minimal in-memory ToolProvider used by unit tests. Records every
    /// `call()` so tests can assert on dispatch behaviour without spinning
    /// up a full MCP handler.
    pub struct MockProvider {
        tools: Vec<ToolSchema>,
        calls: Mutex<Vec<(String, Value)>>,
    }

    impl MockProvider {
        pub fn new(tools: Vec<ToolSchema>) -> Self {
            Self {
                tools,
                calls: Mutex::new(Vec::new()),
            }
        }

        pub fn recorded_calls(&self) -> Vec<(String, Value)> {
            self.calls.lock().unwrap().clone()
        }
    }

    #[async_trait::async_trait]
    impl ToolProvider for MockProvider {
        async fn tools(&self) -> Vec<ToolSchema> {
            self.tools.clone()
        }
        async fn call(&self, name: &str, args: Value) -> Result<String> {
            self.calls
                .lock()
                .unwrap()
                .push((name.to_string(), args.clone()));
            Ok(format!("ok:{name}"))
        }
    }

    fn sample_tools() -> Vec<ToolSchema> {
        vec![
            ToolSchema::zero_arg("list_perspectives", "List all perspectives"),
            ToolSchema {
                name: "query_links".into(),
                description: "Query links".into(),
                parameters: json!({
                    "type": "object",
                    "properties": {"source": {"type": "string"}},
                    "required": ["source"],
                }),
            },
            ToolSchema::zero_arg("add_link", "Add a link to a perspective"),
            ToolSchema::zero_arg("Task_create", "Create a Task instance"),
            ToolSchema::zero_arg("Task_delete", "Delete a Task instance"),
            ToolSchema::zero_arg("Task_set_title", "Set the title of a Task"),
            ToolSchema::zero_arg("Task_add_to_tags", "Add tag to Task's tags"),
            ToolSchema::zero_arg("Channel_children_via_messages", "Read"),
        ]
    }

    #[tokio::test]
    async fn openai_tool_entry_has_function_wrapper_and_parameters_object() {
        let t = ToolSchema {
            name: "hello".into(),
            description: "say hi".into(),
            parameters: json!({"type":"object","properties":{"x":{"type":"string"}},"required":["x"]}),
        };
        let e = t.to_openai_tool_entry();
        assert_eq!(e["type"], "function");
        assert_eq!(e["function"]["name"], "hello");
        assert_eq!(e["function"]["description"], "say hi");
        assert_eq!(e["function"]["parameters"]["type"], "object");
        assert_eq!(e["function"]["parameters"]["required"][0], "x");
    }

    #[tokio::test]
    async fn zero_arg_schema_is_openai_valid_empty_object() {
        let t = ToolSchema::zero_arg("noop", "does nothing");
        // Empty-object schemas STILL need the object wrapper — OpenAI
        // + kalosm both reject a bare type field with no properties.
        assert_eq!(t.parameters["type"], "object");
        assert!(t.parameters["properties"].is_object());
        assert!(t.parameters["required"].is_array());
    }

    #[tokio::test]
    async fn tool_provider_enumerates_all_tools_in_stable_order() {
        let p = MockProvider::new(sample_tools());
        let names: Vec<_> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert_eq!(
            names,
            vec![
                "list_perspectives",
                "query_links",
                "add_link",
                "Task_create",
                "Task_delete",
                "Task_set_title",
                "Task_add_to_tags",
                "Channel_children_via_messages",
            ],
            "tools() must preserve declaration order for reproducible tests"
        );
    }

    #[tokio::test]
    async fn tool_provider_call_records_name_and_args() {
        let p = MockProvider::new(sample_tools());
        let out = p
            .call("query_links", json!({"source": "ns://a"}))
            .await
            .unwrap();
        assert_eq!(out, "ok:query_links");
        let calls = p.recorded_calls();
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].0, "query_links");
        assert_eq!(calls[0].1, json!({"source": "ns://a"}));
    }

    #[tokio::test]
    async fn readonly_filter_excludes_add_delete_set_verbs() {
        let all = sample_tools();
        let kept: Vec<_> = all
            .iter()
            .filter(|t| is_read_only(t))
            .map(|t| t.name.clone())
            .collect();
        // Reads survive.
        assert!(kept.contains(&"list_perspectives".to_string()));
        assert!(kept.contains(&"query_links".to_string()));
        assert!(kept.contains(&"Channel_children_via_messages".to_string()));
        // Writes drop.
        assert!(!kept.contains(&"add_link".to_string()), "add_ prefix");
        // AD4M dynamic `<class>_create` — the CI regression that motivated this
        // test case. When `_create` was neither a suffix nor an infix in the
        // filter, `extintention_create` slipped through, defeating the
        // propose-writes decorator and bouncing gemma3:12b on `perspective_uuid`.
        assert!(!kept.contains(&"Task_create".to_string()), "_create suffix");
        assert!(!kept.contains(&"Task_delete".to_string()), "_delete suffix");
        assert!(!kept.contains(&"Task_set_title".to_string()), "_set_ infix");
        assert!(
            !kept.contains(&"Task_add_to_tags".to_string()),
            "_add_to_ infix"
        );
    }

    #[tokio::test]
    async fn filtered_provider_hides_and_blocks_calls_to_filtered_tools() {
        let inner = Arc::new(MockProvider::new(sample_tools()));
        let f = FilteredProvider::new(inner.clone(), is_read_only);

        // tools() is narrowed.
        let visible: Vec<_> = f.tools().await.into_iter().map(|t| t.name).collect();
        assert!(!visible.contains(&"add_link".to_string()));
        assert!(!visible.contains(&"Task_create".to_string()));
        assert!(!visible.contains(&"Task_delete".to_string()));

        // call() on an allowed tool passes through.
        assert!(f.call("query_links", json!({"source": "x"})).await.is_ok());

        // call() on a filtered-out tool is refused — the LLM can't smuggle a
        // write past the read-only filter by hallucinating a name it didn't
        // see in tools(). This is a real safety check, not a nicety.
        let err = f.call("add_link", json!({})).await.unwrap_err();
        assert!(err
            .to_string()
            .contains("not in the current provider's filtered surface"));
    }
}
