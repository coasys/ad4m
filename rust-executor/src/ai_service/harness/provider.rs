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

/// Declared side-effect of a tool. Rendered onto every [`ToolSchema`] at
/// construction time — the interpretation-pass read-only cut checks this
/// field rather than inferring from the tool name.
///
/// Rationale (James Weir's PR #911 review, 2026-08-25 issue-comment):
///
/// > The security boundary of the whole design inferred from a verb match
/// > on tool names… The structural fix is the version where a newly added
/// > tool can't silently land on the wrong side. What's new is that the
/// > discussion so far has only covered false negatives — writers slipping
/// > through the cut. False positives are equally silent and already
/// > present: a class named `Signal` produces `signal_query` / `signal_get`
/// > / `signal_list`, all of which classify as writes, because `signal` is
/// > the leading token and it's in `WRITE_VERBS`.
///
/// The old `is_read_only(&t)` shipped a verb-token classifier
/// (`WRITE_VERBS`) that resolved collisions between the verb vocabulary
/// and consumer class names by accident in both directions. Declaring
/// side-effect at the point a tool is *emitted* — either the static
/// `#[tool]` method's entry in the side-effect table or the dynamic
/// generator that mints CRUD tools per class — makes new tools born with
/// the correct classification instead of retroactively pattern-matched.
///
/// Default is [`SideEffect::Read`]: safer default (reads never mutate).
/// The compile-time parity assertion in
/// `mcp::tools::harness_bridge::side_effects` ensures every static tool
/// name has an explicit entry, so a new `#[tool]` method added without a
/// matching table row fails the test — the default never masks silent
/// drift on the write side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideEffect {
    /// Read-only tool — safe for a class-scoped interpretation pass to
    /// call at will. `query_*`, `get_*`, `list_*`, `find_*`, `search_*`,
    /// class introspection, model reads.
    Read,
    /// Write / mutation / side-effect creator. Blocked from the harness
    /// read-only cut; only the propose-* wrappers can enqueue mutations,
    /// which then drain through `apply_with_overlay`.
    Write,
}

impl Default for SideEffect {
    fn default() -> Self {
        Self::Read
    }
}

/// LLM-facing description of one tool. The three schema fields map 1-to-1
/// onto the OpenAI `tools[]` request array (function.name /
/// function.description / function.parameters). No wrapper: the harness
/// passes this straight into `chat_gpt_lib_rs` / kalosm and the LLM sees
/// exactly what's here.
///
/// `parameters` is a JSON Schema fragment describing the argument object.
/// Zero-arg tools use `{"type":"object","properties":{},"required":[]}` —
/// don't drop the object wrapper (OpenAI + kalosm both reject bare types).
///
/// `side_effect` is NOT sent to the LLM. It's an internal classification
/// consumed by [`is_read_only`] (which drives the harness's read-only cut)
/// and any future capability-gating layer. Default is
/// [`SideEffect::Read`]; every construction site should be explicit — the
/// harness_bridge side-effect table enforces this for static tools.
#[derive(Debug, Clone, PartialEq)]
pub struct ToolSchema {
    pub name: String,
    pub description: String,
    pub parameters: Value,
    pub side_effect: SideEffect,
}

impl ToolSchema {
    /// Cheap constructor for zero-arg READ tools — the empty-object schema
    /// is verbose enough that inlining it everywhere hurts readability.
    /// Read is the safer default and the shape most zero-arg tools take
    /// (`list_perspectives`, `get_models`, ...). Use
    /// [`ToolSchema::zero_arg_write`] for mutators.
    pub fn zero_arg(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            parameters: json!({ "type": "object", "properties": {}, "required": [] }),
            side_effect: SideEffect::Read,
        }
    }

    /// Zero-arg WRITE tool — companion to [`ToolSchema::zero_arg`]. Kept as
    /// a distinct constructor so a test author can't accidentally emit a
    /// write with the default classification.
    #[allow(dead_code)]
    pub fn zero_arg_write(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            parameters: json!({ "type": "object", "properties": {}, "required": [] }),
            side_effect: SideEffect::Write,
        }
    }

    /// Render as an OpenAI `tools[]` entry (the `{type:"function", function:{...}}`
    /// wrapper the /v1/chat/completions endpoint expects). Kept here rather
    /// than in the harness loop so both consumers agree on the wire shape.
    ///
    /// `side_effect` is deliberately omitted from the OpenAI payload — it's
    /// an internal capability marker, not something the LLM needs to see.
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

/// The interpretation-pass read-only cut (v2 §Q5) — the harness wraps
/// `Ad4mToolProvider` in `FilteredProvider::new(_, is_read_only)` before
/// handing it to a pass.
///
/// Reads [`ToolSchema::side_effect`] directly. Prior implementations
/// inferred read-vs-write from a verb-token scan of `name`, which was
/// symmetric-fragile: writers slipped through when the verb list missed
/// something (`_add_<coll>` vs `_add_to_<coll>`, plugged in `436477457`),
/// and reads got misclassified when a user class name collided with the
/// verb vocabulary (`Signal` class → `signal_query` classified as write,
/// James Weir 2026-08-25 review). Structural declaration at emission
/// site fixes both directions.
pub fn is_read_only(t: &ToolSchema) -> bool {
    t.side_effect == SideEffect::Read
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
                side_effect: SideEffect::Read,
            },
            ToolSchema::zero_arg_write("add_link", "Add a link to a perspective"),
            ToolSchema::zero_arg_write("Task_create", "Create a Task instance"),
            ToolSchema::zero_arg_write("Task_delete", "Delete a Task instance"),
            ToolSchema::zero_arg_write("Task_set_title", "Set the title of a Task"),
            ToolSchema::zero_arg_write("Task_add_to_tags", "Add tag to Task's tags"),
            ToolSchema::zero_arg("Channel_children_via_messages", "Read"),
        ]
    }

    #[tokio::test]
    async fn openai_tool_entry_has_function_wrapper_and_parameters_object() {
        let t = ToolSchema {
            name: "hello".into(),
            description: "say hi".into(),
            parameters: json!({"type":"object","properties":{"x":{"type":"string"}},"required":["x"]}),
            side_effect: SideEffect::Read,
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
    async fn is_read_only_reads_declared_side_effect_field() {
        // Reads survive; writes drop. Uses the sample_tools fixture where
        // side_effect is declared explicitly on each entry — the classifier
        // no longer infers from the name.
        let all = sample_tools();
        let kept: Vec<_> = all
            .iter()
            .filter(|t| is_read_only(t))
            .map(|t| t.name.clone())
            .collect();
        assert!(kept.contains(&"list_perspectives".to_string()));
        assert!(kept.contains(&"query_links".to_string()));
        assert!(kept.contains(&"Channel_children_via_messages".to_string()));
        assert!(!kept.contains(&"add_link".to_string()));
        assert!(!kept.contains(&"Task_create".to_string()));
        assert!(!kept.contains(&"Task_delete".to_string()));
        assert!(!kept.contains(&"Task_set_title".to_string()));
        assert!(!kept.contains(&"Task_add_to_tags".to_string()));
    }

    #[tokio::test]
    async fn is_read_only_ignores_verb_collisions_in_class_names() {
        // James Weir's 2026-08-25 PR #911 review — the false-positive case
        // the pre-structural verb-token classifier silently mangled.
        //
        // A user perspective declaring a `Signal` subject class produces
        // dynamic tools `signal_query` / `signal_get` / `signal_list`. Under
        // the pre-#911 shape, `signal` was in `WRITE_VERBS` (it captures
        // `send_signal` / `signal_broadcast` mutators on the MCP side), so
        // the FIRST token match classified `signal_query` as a write. The
        // harness's read-only cut then hid every Signal-class read from
        // the LLM. Silent — the pass just couldn't see that class.
        //
        // Structural declaration fixes it: dynamic per-class generators
        // emit `signal_query` with `side_effect: Read`, and the collision
        // between the user class name and the verb vocabulary stops mattering.
        let signal_query = ToolSchema {
            name: "signal_query".into(),
            description: "Query Signal instances (user-defined class)".into(),
            parameters: json!({"type":"object","properties":{},"required":[]}),
            side_effect: SideEffect::Read,
        };
        assert!(
            is_read_only(&signal_query),
            "signal_query on a user Signal class must survive the read-only cut"
        );

        // Symmetric case: a user class named `Update` or `Add` shouldn't
        // fail the read cut for its query tools either.
        for user_class in ["update", "add", "remove", "delete", "create", "set"] {
            let read_tool = ToolSchema {
                name: format!("{user_class}_query"),
                description: "".into(),
                parameters: json!({"type":"object","properties":{},"required":[]}),
                side_effect: SideEffect::Read,
            };
            assert!(
                is_read_only(&read_tool),
                "read tool named `{user_class}_query` must survive the read-only cut"
            );
        }
    }

    #[tokio::test]
    async fn is_read_only_does_not_infer_from_name() {
        // Fabricate a maximally suspicious name that would fail every
        // verb-inference heuristic — declaring it Read must still pass.
        // Symmetric: a benign-looking `get_stuff` tool declared Write is
        // still a write.
        let unusual_read = ToolSchema {
            name: "add_create_delete_publish_signal_grant".into(),
            description: "".into(),
            parameters: json!({"type":"object","properties":{},"required":[]}),
            side_effect: SideEffect::Read,
        };
        assert!(
            is_read_only(&unusual_read),
            "declaration wins over any name-shape inference"
        );

        let sneaky_write = ToolSchema {
            name: "get_stuff".into(),
            description: "".into(),
            parameters: json!({"type":"object","properties":{},"required":[]}),
            side_effect: SideEffect::Write,
        };
        assert!(
            !is_read_only(&sneaky_write),
            "a Write-declared tool must never pass the read-only cut, regardless of name"
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
            side_effect: SideEffect::Read,
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
            ToolSchema::zero_arg_write("add_link", "write verb"),
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
