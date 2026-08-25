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
pub struct FilteredProvider<P: ToolProvider + ?Sized> {
    inner: Arc<P>,
    predicate: Arc<dyn Fn(&ToolSchema) -> bool + Send + Sync>,
}

impl<P: ToolProvider + ?Sized> FilteredProvider<P> {
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
impl<P: ToolProvider + ?Sized + 'static> ToolProvider for FilteredProvider<P> {
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

/// Auto-inject a fixed set of argument values on every dispatch, and hide
/// those parameters from the schemas the LLM sees.
///
/// Purpose: when the caller has context (e.g. "the perspective this pass is
/// running on") that a tool wants as an argument, but the LLM has no way to
/// know it, this decorator makes the tool call succeed without the LLM
/// guessing. CI job 22287 on `dcaeba21b` failed 8/8 attempts because the
/// dynamic `extbelief_query` / `extintention_propose_create` schemas listed
/// `perspective_id` as required — gemma3:12b hallucinated the string
/// `"ad4m"`, hit "Perspective not found", and gave up in plain text.
///
/// Behaviour:
/// - `tools()` — for each inner schema, strip the bound parameter names from
///   `properties` and `required`. Non-object schemas pass through unchanged.
/// - `call()` — merge the bound values into `args` (bound values do NOT
///   override an argument the LLM already supplied; the strip step keeps the
///   LLM from seeing them, so this is defense-in-depth) and delegate.
pub struct BoundArgsProvider<P: ToolProvider + ?Sized> {
    inner: Arc<P>,
    bindings: std::collections::BTreeMap<String, Value>,
}

impl<P: ToolProvider + ?Sized> BoundArgsProvider<P> {
    pub fn new(inner: Arc<P>, bindings: std::collections::BTreeMap<String, Value>) -> Self {
        Self { inner, bindings }
    }
}

#[async_trait::async_trait]
impl<P> ToolProvider for BoundArgsProvider<P>
where
    P: ToolProvider + ?Sized + Send + Sync + 'static,
{
    async fn tools(&self) -> Vec<ToolSchema> {
        self.inner
            .tools()
            .await
            .into_iter()
            .map(|mut t| {
                strip_bound_from_schema(&mut t.parameters, &self.bindings);
                t
            })
            .collect()
    }

    async fn call(&self, name: &str, mut args: Value) -> Result<String> {
        merge_bound_into_args(&mut args, &self.bindings);
        self.inner.call(name, args).await
    }
}

fn strip_bound_from_schema(
    schema: &mut Value,
    bindings: &std::collections::BTreeMap<String, Value>,
) {
    let Value::Object(map) = schema else {
        return;
    };
    if let Some(Value::Object(props)) = map.get_mut("properties") {
        for k in bindings.keys() {
            props.remove(k);
        }
    }
    if let Some(Value::Array(req)) = map.get_mut("required") {
        req.retain(|v| match v {
            Value::String(s) => !bindings.contains_key(s),
            _ => true,
        });
    }
}

fn merge_bound_into_args(args: &mut Value, bindings: &std::collections::BTreeMap<String, Value>) {
    // Small local models occasionally send `null` or an unwrapped scalar when
    // the (post-strip) schema is empty; coerce to an object so injection can
    // land regardless of the caller's shape.
    if !args.is_object() {
        *args = Value::Object(serde_json::Map::new());
    }
    if let Value::Object(map) = args {
        for (k, v) in bindings {
            map.entry(k.clone()).or_insert_with(|| v.clone());
        }
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
    // Write-adjacent verb tokens: if ANY `_`-separated token in the tool name
    // matches one of these, the tool is treated as a writer. Token-based
    // (not substring/prefix) so namespaced tools like
    // `neighbourhood_publish_from_perspective` are caught even though
    // `publish` isn't at position 0. Audited against every `async fn` in
    // rust-executor/src/mcp/tools/*.rs on 2026-08-24 (Lal's PR #911 review):
    //   add / remove / delete / create / update / set — CRUD verbs, both
    //     static (add_link, delete_subject) and dynamic per-class
    //     (`<class>_add_<coll>`, `<class>_set_<prop>`, ...).
    //   publish / join / leave — neighbourhood mutations.
    //   send / signal — inter-agent side-effects.
    //   revoke / grant — capability rotation.
    //   install / uninstall / clone — language lifecycle.
    //   signup / login / logout — auth writes.
    //   generate / mint — mint new artifacts.
    //   store / save — persist to state.
    //   request — creates a pending request record.
    //   start / run — flow lifecycle mutations (`flow_start_*`, `flow_run_action`).
    //
    // The earlier prefix/suffix/infix approach (`_add_to_`, `_remove_from_`)
    // was a copy-paste mismatch against dynamic.rs's real emissions
    // (`_add_<coll>`, `_remove_<coll>`) so every collection mutator silently
    // leaked through the read-only cut (Lal's 2026-08-24 review,
    // provider.rs:240). Token-based fixes it structurally.
    const WRITE_VERBS: &[&str] = &[
        // CRUD
        "add",
        "remove",
        "delete",
        "create",
        "update",
        "set",
        // neighbourhood + p2p mutations
        "publish",
        "join",
        "leave",
        "send",
        "signal",
        // auth + capabilities
        "revoke",
        "grant",
        "signup",
        "login",
        "logout",
        // language lifecycle
        "install",
        "uninstall",
        "clone",
        // mint / persist
        "generate",
        "mint",
        "store",
        "save",
        // side-effect creators
        "request",
        // flow lifecycle
        "start",
        "run",
    ];

    // Some read tools legitimately contain a write verb as part of their
    // subject (`request_type`, `run_id`, ...) but at the CORE the tool is
    // a read. Guard: if the leading token itself starts a well-known read
    // verb, trust it. This is the tie-breaker; token match still wins if
    // it's on a subsequent segment (`neighbourhood_publish_*`).
    const READ_LEADERS: &[&str] = &["list", "get", "query", "read", "find", "search"];

    let tokens: Vec<&str> = t.name.split('_').collect();
    if tokens.is_empty() {
        return true;
    }
    let leader = tokens[0].to_lowercase();
    let leader_is_read = READ_LEADERS.iter().any(|r| leader == *r);

    let mut writer_hit = None;
    for (i, tok) in tokens.iter().enumerate() {
        let low = tok.to_lowercase();
        if WRITE_VERBS.iter().any(|v| low == *v) {
            writer_hit = Some(i);
            break;
        }
    }

    match (writer_hit, leader_is_read) {
        // No write token found → read.
        (None, _) => true,
        // Write token IS the leader → definitely a writer.
        (Some(0), _) => false,
        // Write token is downstream AND leader is a read verb (`get`, `list`,
        // ...) → tool is a read (e.g. `get_publish_config` if such existed).
        // Downstream writers with a non-read leader are writers
        // (`neighbourhood_publish_from_perspective`).
        (Some(_), true) => true,
        (Some(_), false) => false,
    }
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
        // Real dynamic emissions (see mcp/tools/dynamic.rs make_collection_add_
        // /make_collection_remove_tool): `<class>_add_<coll>` and
        // `<class>_remove_<coll>` — NOT the `_add_to_`/`_remove_from_` variants
        // the earlier filter had. Regression against Lal's 2026-08-24 review.
        assert!(
            !kept.contains(&"Task_add_tags".to_string()),
            "_add_ infix (real dynamic collection-adder shape)"
        );
        assert!(
            !kept.contains(&"Task_remove_tags".to_string()),
            "_remove_ infix (real dynamic collection-remover shape)"
        );
        // Additional write-adjacent static verbs the MCP surface exposes
        // (audited by grep'ing async fn under rust-executor/src/mcp/tools/*.rs
        // 2026-08-24). If any of these leak into a read-only interpretation
        // pass, the LLM could publish neighbourhoods / send signals / rotate
        // capability tokens as a side-effect of a "read" turn.
        for write_tool in [
            "neighbourhood_publish_from_perspective",
            "neighbourhood_join_from_url",
            "clone_link_language",
            "signup",
            "login_email",
            "generate_jwt",
            "revoke_capability", // hypothetical; guard against future adds
            "flow_start_task",
            "flow_run_action",
        ] {
            let t = ToolSchema::zero_arg(write_tool, "");
            assert!(
                !is_read_only(&t),
                "write-adjacent static tool `{write_tool}` must not pass is_read_only"
            );
        }
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

    fn perspective_id_bound() -> std::collections::BTreeMap<String, Value> {
        std::collections::BTreeMap::from([(
            "perspective_id".to_string(),
            Value::String("uuid-under-test".to_string()),
        )])
    }

    fn schema_with_perspective_id() -> ToolSchema {
        ToolSchema {
            name: "extbelief_query".into(),
            description: "Query beliefs".into(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "perspective_id": {"type": "string", "description": "Perspective UUID"},
                    "limit": {"type": "integer"}
                },
                "required": ["perspective_id"],
            }),
        }
    }

    #[tokio::test]
    async fn bound_args_strips_param_from_schema_properties_and_required() {
        let inner = Arc::new(MockProvider::new(vec![schema_with_perspective_id()]));
        let bound = BoundArgsProvider::new(inner, perspective_id_bound());
        let out = bound.tools().await;
        assert_eq!(out.len(), 1);
        let props = &out[0].parameters["properties"];
        assert!(props.get("perspective_id").is_none());
        assert!(props.get("limit").is_some());
        let required: Vec<&str> = out[0].parameters["required"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();
        assert!(!required.contains(&"perspective_id"));
    }

    #[tokio::test]
    async fn bound_args_injects_value_on_call_when_llm_omits_it() {
        let inner = Arc::new(MockProvider::new(vec![schema_with_perspective_id()]));
        let recorder = inner.clone();
        let bound = BoundArgsProvider::new(inner, perspective_id_bound());
        // LLM sees the post-strip schema, so it only sends the visible arg.
        let out = bound.call("extbelief_query", json!({"limit": 5})).await;
        assert!(out.is_ok(), "call should succeed: {out:?}");
        let calls = recorder.recorded_calls();
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].1["perspective_id"], "uuid-under-test");
        assert_eq!(calls[0].1["limit"], 5);
    }

    #[tokio::test]
    async fn bound_args_promotes_non_object_args_to_object_before_inject() {
        let inner = Arc::new(MockProvider::new(vec![schema_with_perspective_id()]));
        let recorder = inner.clone();
        let bound = BoundArgsProvider::new(inner, perspective_id_bound());
        // gemma3:12b sometimes sends `null` for a zero-arg tool call; the
        // decorator must still land the injected binding.
        let out = bound.call("extbelief_query", Value::Null).await;
        assert!(out.is_ok(), "call should succeed on null args: {out:?}");
        let calls = recorder.recorded_calls();
        assert_eq!(calls[0].1["perspective_id"], "uuid-under-test");
    }

    #[tokio::test]
    async fn bound_args_does_not_override_llm_supplied_value() {
        // Defense-in-depth: the schema hides the param, but if an out-of-band
        // arg smuggles it in, respect the LLM's value rather than clobbering.
        let inner = Arc::new(MockProvider::new(vec![schema_with_perspective_id()]));
        let recorder = inner.clone();
        let bound = BoundArgsProvider::new(inner, perspective_id_bound());
        let out = bound
            .call(
                "extbelief_query",
                json!({"perspective_id": "override", "limit": 1}),
            )
            .await;
        assert!(out.is_ok());
        let calls = recorder.recorded_calls();
        assert_eq!(calls[0].1["perspective_id"], "override");
    }

    #[tokio::test]
    async fn bound_args_leaves_unrelated_schemas_untouched() {
        let inner = Arc::new(MockProvider::new(vec![
            schema_with_perspective_id(),
            ToolSchema::zero_arg("noop", "does nothing"),
        ]));
        let bound = BoundArgsProvider::new(inner, perspective_id_bound());
        let out = bound.tools().await;
        assert_eq!(out.len(), 2);
        // Zero-arg schema retains its shape (no perspective_id to remove).
        assert_eq!(out[1].parameters["type"], "object");
        assert!(out[1].parameters["properties"]
            .as_object()
            .unwrap()
            .is_empty());
    }

    #[tokio::test]
    async fn bound_args_composes_under_filtered_provider() {
        // Compose exactly the way `run_interpretation_with_harness_and_model`
        // does: BoundArgsProvider(Ad4mToolProvider) → FilteredProvider. The
        // strip must apply BEFORE the filter, so downstream layers already
        // see the perspective-clean schema.
        let inner = Arc::new(MockProvider::new(vec![
            schema_with_perspective_id(),
            ToolSchema::zero_arg("add_link", "write verb"),
        ]));
        let bound: Arc<dyn ToolProvider> = Arc::new(BoundArgsProvider::new(
            inner.clone(),
            perspective_id_bound(),
        ));
        let filtered = FilteredProvider::new(bound, is_read_only);
        let visible: Vec<_> = filtered.tools().await;
        // Read tool survives, write tool drops.
        assert_eq!(visible.len(), 1);
        assert_eq!(visible[0].name, "extbelief_query");
        // Perspective id is gone from what the LLM sees.
        assert!(visible[0].parameters["properties"]
            .get("perspective_id")
            .is_none());
        // Dispatch through the filter still injects.
        filtered
            .call("extbelief_query", json!({"limit": 3}))
            .await
            .expect("call succeeds");
        let calls = inner.recorded_calls();
        assert_eq!(calls[0].1["perspective_id"], "uuid-under-test");
    }
}
