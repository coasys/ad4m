//! Propose-write tools for the interpretation-pass harness.
//!
//! During a harness pass the LLM sees a decorated tool surface: every real
//! read tool from the inner provider (query / list / get / children_via /
//! …) PLUS two synthetic per-class writers — `<class>_propose_create` and
//! `<class>_propose_link_child` — that accumulate `InterpretationOp`
//! entries into a shared [`ProposalBuffer`] rather than mutating the
//! perspective directly.
//!
//! The buffered ops are handed to `apply_with_overlay` by the engine after
//! the pass terminates (v3 §6 — writes only cross the overlay gate at pass
//! boundary, not mid-loop). Design: [[planning/llm-harness-design-2026-08-21-v3.md]] §6.
//!
//! ## Why decorator, not new dynamic MCP tools
//!
//! The propose-write tools only make sense inside a harness pass — they
//! carry a per-pass buffer and their side-effect is "queue an op", not
//! "mutate the graph." Putting them in `mcp/tools/dynamic.rs` would leak
//! them onto the external MCP transport where they'd have no buffer to
//! write to and no engine draining it. Keeping them in this decorator
//! keeps the MCP surface unchanged and makes the harness-only nature
//! obvious from the type.

use super::provider::{ToolProvider, ToolSchema};
use crate::mcp::shacl::ShaclClass;
use crate::perspectives::interpretation::InterpretationOp;
use crate::types::Link;
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use serde_json::{json, Value};
use std::sync::{Arc, Mutex};
use uuid::Uuid;

// ── buffer ────────────────────────────────────────────────────────────────

/// Per-pass accumulator for InterpretationOps emitted by `_propose_*` tool
/// calls. Cloneable Arc so the ToolProvider (which the harness owns for
/// the duration of the loop) and the engine (which drains at pass end)
/// hold independent references.
///
/// The mutex is only held during a single push/drain — tool calls are
/// serialised through the harness loop anyway, so contention is nil.
#[derive(Debug, Clone, Default)]
pub struct ProposalBuffer {
    inner: Arc<Mutex<Vec<InterpretationOp>>>,
}

impl ProposalBuffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&self, op: InterpretationOp) {
        self.inner
            .lock()
            .expect("ProposalBuffer mutex poisoned — see anyhow trace on the panicking thread")
            .push(op);
    }

    pub fn drain(&self) -> Vec<InterpretationOp> {
        std::mem::take(
            &mut *self
                .inner
                .lock()
                .expect("ProposalBuffer mutex poisoned — see anyhow trace on the panicking thread"),
        )
    }

    pub fn len(&self) -> usize {
        self.inner
            .lock()
            .expect("ProposalBuffer mutex poisoned — see anyhow trace on the panicking thread")
            .len()
    }
}

// ── per-class shape ───────────────────────────────────────────────────────

/// Enough of a SHACL class to describe its propose-write tool surface —
/// name + scalar properties + which are required. Constructed by the
/// engine via [`class_propose_shape_from_shacl`] and handed in at
/// provider construction so this module doesn't reach into perspective
/// state at all.
#[derive(Debug, Clone)]
pub struct ClassProposeShape {
    /// Original class name (e.g. `Task`); used verbatim in tool descriptions.
    pub class_name: String,
    /// Scalar (non-collection) property name + short type description
    /// (from `ShaclProperty::type_description`). These become the tool's
    /// JSON-schema properties.
    pub scalar_props: Vec<(String, String)>,
    /// Names of the scalar properties SHACL marks required. Copied into
    /// the tool schema's `required[]` so the LLM can't propose a fresh
    /// instance missing a mandatory field.
    pub required: Vec<String>,
}

/// Extract a [`ClassProposeShape`] from a loaded SHACL class. Filters out
/// collection properties — those are set via `_propose_link_child` after
/// creation, not inline on create.
pub fn class_propose_shape_from_shacl(class: &ShaclClass) -> ClassProposeShape {
    let scalar_props: Vec<(String, String)> = class
        .scalar_properties()
        .into_iter()
        .map(|p| (p.name.clone(), p.type_description()))
        .collect();
    let required: Vec<String> = class
        .required_properties()
        .into_iter()
        .filter(|p| !p.is_collection)
        .map(|p| p.name.clone())
        .collect();
    ClassProposeShape {
        class_name: class.name.clone(),
        scalar_props,
        required,
    }
}

// ── decorator ─────────────────────────────────────────────────────────────

/// ToolProvider decorator that adds `_propose_create` / `_propose_link_child`
/// per class on top of an inner (read-tool) provider.
///
/// - `tools()` = inner.tools() + 2 synthetic tools per registered class.
/// - `call()` — if the name matches one of the synthetics, mutate `buffer`
///   and return a short human-readable ack; otherwise delegate to inner.
///
/// The class list is fixed at construction — new perspective classes
/// registered mid-pass will NOT appear until the next pass. This matches
/// the design v3 §6 guarantee that the tool surface a pass advertises is
/// stable for the duration of that pass.
pub struct ProposeWritesProvider<P: ToolProvider + ?Sized> {
    inner: Arc<P>,
    classes: Vec<ClassProposeShape>,
    buffer: ProposalBuffer,
    /// URI prefix for auto-minted bases when the LLM doesn't supply one.
    /// The pass owner (engine) passes its `base_prefix` here — matches the
    /// `plan_interpretation_ops_resolved` argument name.
    base_prefix: String,
}

impl<P: ToolProvider + ?Sized> ProposeWritesProvider<P> {
    pub fn new(
        inner: Arc<P>,
        classes: Vec<ClassProposeShape>,
        buffer: ProposalBuffer,
        base_prefix: String,
    ) -> Self {
        Self {
            inner,
            classes,
            buffer,
            base_prefix,
        }
    }
}

#[async_trait]
impl<P> ToolProvider for ProposeWritesProvider<P>
where
    P: ToolProvider + ?Sized + Send + Sync,
{
    async fn tools(&self) -> Vec<ToolSchema> {
        let mut out = self.inner.tools().await;
        for c in &self.classes {
            out.push(propose_create_schema(c));
            out.push(propose_link_child_schema(c));
        }
        out
    }

    async fn call(&self, name: &str, args: Value) -> Result<String> {
        // Order matters: `_propose_link_child` is a strict suffix of a
        // hypothetical `_propose_child_create` in some future rename, so we
        // check the more-specific longer suffix first. Belt-and-braces —
        // today there's no conflict.
        if let Some(class_name) = strip_class_suffix(name, "_propose_link_child") {
            return self.handle_propose_link_child(&class_name, args);
        }
        if let Some(class_name) = strip_class_suffix(name, "_propose_create") {
            return self.handle_propose_create(&class_name, args);
        }
        // Small local models (gemma3:12b in CI) reliably hallucinate the
        // dynamic CRUD verbs from AD4M's MCP surface — `<class>_create`,
        // `add_<class>`, `<class>_add` — because that's what a CRUD API
        // "should" look like. Even when we filter those tools out and
        // publish the explicit `<class>_propose_create` name, gemma3
        // reaches for the shorter familiar form and, when it hits an
        // error, gives up in plain text instead of retrying with the
        // longer name (observed 8/8 attempts in CI job 22266 on
        // `45a363b94` — a helpful redirect message didn't rescue it).
        //
        // Fix: alias the hallucinated forms transparently. If the LLM
        // calls a class-prefixed create-shaped verb that is NOT in the
        // inner (filtered) surface, dispatch to handle_propose_create
        // for that class. Overlay semantics are preserved — the buffer
        // still queues an InterpretationOp gated by the human-divergence
        // check on apply. Small models get their "obvious" verb; the
        // extraction pass proceeds without stalling.
        let name_lc = name.to_lowercase();
        for c in &self.classes {
            let lower = c.class_name.to_lowercase();
            let matches_alias = name_lc == format!("{lower}_create")
                || name_lc == format!("create_{lower}")
                || name_lc == format!("{lower}_new")
                || name_lc == format!("new_{lower}")
                || name_lc == format!("add_{lower}")
                || name_lc == format!("{lower}_add");
            if !matches_alias {
                continue;
            }
            let inner_has_name = self
                .inner
                .tools()
                .await
                .into_iter()
                .any(|t| t.name.eq_ignore_ascii_case(name));
            if inner_has_name {
                break;
            }
            log::warn!("harness: aliasing hallucinated tool `{name}` → `{lower}_propose_create`");
            return self.handle_propose_create(&lower, args);
        }
        // Second alias family: `<class>_add_<relation>` (e.g.
        // `extintention_add_basedon`). CI job 22282 attempts 1-4 showed
        // gemma3:12b reaching for this shape when it wants to attach a
        // relation, because that's how a CRUD API would name it. The
        // real tool is `<class>_propose_link_child` — but a filter-not-
        // found error left the LLM confused and it bailed in plain text.
        // Return an actionable redirect naming the correct tool + the
        // predicate hint the LLM likely intended (derived from the tail).
        for c in &self.classes {
            let lower = c.class_name.to_lowercase();
            let add_prefix = format!("{lower}_add_");
            let Some(relation_tail) = name_lc.strip_prefix(&add_prefix) else {
                continue;
            };
            if relation_tail.is_empty() {
                continue;
            }
            let inner_has_name = self
                .inner
                .tools()
                .await
                .into_iter()
                .any(|t| t.name.eq_ignore_ascii_case(name));
            if inner_has_name {
                break;
            }
            return Err(anyhow!(
                "tool `{name}` is not available. To link a {class} to a related \
                 instance, call `{lower}_propose_link_child` with `parent` = the \
                 {class} URI, `predicate` = the relation IRI (e.g. `soa://{relation_tail}` \
                 if that matches the class definition), and `child` = the target URI. \
                 Discover real URIs with the target class's `_query` tool first — \
                 never invent placeholder URIs.",
                class = c.class_name
            ));
        }
        self.inner.call(name, args).await
    }
}

impl<P: ToolProvider + ?Sized> ProposeWritesProvider<P> {
    fn find_class(&self, lower_class: &str) -> Option<&ClassProposeShape> {
        self.classes
            .iter()
            .find(|c| c.class_name.to_lowercase() == lower_class)
    }

    fn handle_propose_create(&self, lower_class: &str, args: Value) -> Result<String> {
        let class = self
            .find_class(lower_class)
            .ok_or_else(|| anyhow!("no registered class `{lower_class}` for propose_create"))?;

        // Extract known scalar props into an ops-shaped Map. Unknown keys are
        // dropped silently — the LLM sometimes hallucinates fields; letting
        // the pass proceed with what it got is better than aborting.
        let mut values = serde_json::Map::new();
        if let Value::Object(obj) = &args {
            for (prop_name, _type_desc) in &class.scalar_props {
                if let Some(v) = obj.get(prop_name) {
                    values.insert(prop_name.clone(), v.clone());
                }
            }
        }

        // Required-field check. The tool schema already enforces this on the
        // LLM side (`required[]`), but the wire is untrusted — an argument
        // dropped by the grammar path or hallucinated as null must still
        // bounce back as a tool-call error the LLM can recover from.
        let missing: Vec<&String> = class
            .required
            .iter()
            .filter(|k| !values.contains_key(*k) || values.get(*k).map_or(true, Value::is_null))
            .collect();
        if !missing.is_empty() {
            return Err(anyhow!(
                "{}_propose_create missing required fields: {}",
                lower_class,
                missing
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        // Base URI: honour caller-supplied `base` if present (lets the LLM
        // reference the instance from a later propose_link_child in the same
        // turn without a round-trip), else mint one under base_prefix.
        let base = args
            .get("base")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                format!(
                    "{}{}{}/{}",
                    self.base_prefix,
                    if self.base_prefix.ends_with('/') {
                        ""
                    } else {
                        "/"
                    },
                    lower_class,
                    Uuid::new_v4()
                )
            });

        self.buffer.push(InterpretationOp::Create {
            base: base.clone(),
            class: class.class_name.clone(),
            values,
        });

        Ok(format!("proposed create: {base}"))
    }

    fn handle_propose_link_child(&self, lower_class: &str, args: Value) -> Result<String> {
        // The class is only used to validate the caller understands what
        // they're linking under — we accept ANY class the provider knows
        // about (link semantics don't depend on which class registered
        // the tool). Reject unknown classes to catch typos early.
        if self.find_class(lower_class).is_none() {
            return Err(anyhow!(
                "no registered class `{lower_class}` for propose_link_child"
            ));
        }

        let parent = args
            .get("parent")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow!("propose_link_child: missing `parent` (URI of parent)"))?
            .to_string();
        let predicate = args
            .get("predicate")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow!("propose_link_child: missing `predicate` (link predicate IRI)"))?
            .to_string();
        let child = args
            .get("child")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow!("propose_link_child: missing `child` (URI of child)"))?
            .to_string();

        // Bounce placeholder URIs (`ad4m://obj/unknown`, `.../placeholder`,
        // `.../example`, `.../...`) — small models tend to invent these when
        // they skip the query step. Observed on CI job 22282 attempts 5/7/8
        // where the LLM called `_propose_link_child` with `ad4m://obj/unknown`
        // instead of first calling `<class>_query` to get real URIs. Failing
        // fast with a specific redirect gives the LLM an actionable next step.
        if let Some(bad) = placeholder_uri(&parent) {
            return Err(anyhow!(
                "propose_link_child: `parent` looks like a placeholder ({bad}). \
                 Do NOT invent URIs. First call the class-specific `_query` tool \
                 (e.g. `{lower_class}_query`) to discover real URIs, then pass the \
                 URI returned by the query verbatim as `parent`."
            ));
        }
        if let Some(bad) = placeholder_uri(&child) {
            return Err(anyhow!(
                "propose_link_child: `child` looks like a placeholder ({bad}). \
                 Do NOT invent URIs. First call the class-specific `_query` tool \
                 for the child's class to discover real URIs, then pass the URI \
                 returned by the query verbatim as `child`."
            ));
        }

        // AddLinks carries the source separately from each Link.source so the
        // overlay's downstream apply path can batch links per-source; keep
        // both fields in sync here so a future single-source refactor is a
        // no-op.
        self.buffer.push(InterpretationOp::AddLinks {
            source: parent.clone(),
            links: vec![Link {
                source: parent.clone(),
                predicate: Some(predicate.clone()),
                target: child.clone(),
            }],
        });

        Ok(format!("proposed link: {parent} --{predicate}--> {child}"))
    }
}

/// Case-insensitive class-name extract: `Task_propose_create` → `task`.
/// Returns None if the string doesn't end with the suffix or nothing is
/// left before it. Matches the lowercase form because dynamic tool names
/// are always emitted lowercase (see `dynamic.rs::make_create_tool`).
fn strip_class_suffix(name: &str, suffix: &str) -> Option<String> {
    name.strip_suffix(suffix)
        .filter(|prefix| !prefix.is_empty())
        .map(|s| s.to_lowercase())
}

/// If `uri` matches an obvious placeholder pattern the LLM might invent
/// when it skips a query step, return the offending token. Returns None
/// for real-looking URIs (which is not a proof of existence — just that
/// the URI is worth attempting).
///
/// Recognised: empty string, trailing `unknown` / `placeholder` /
/// `example` / `...` / `xxx` / `todo` (case-insensitive) — the tail after
/// the last `/` or `:` is checked so `ad4m://obj/unknown` and plain
/// `unknown` both bounce.
fn placeholder_uri(uri: &str) -> Option<&'static str> {
    let trimmed = uri.trim();
    if trimmed.is_empty() {
        return Some("empty");
    }
    let tail = trimmed
        .rsplit(|c: char| c == '/' || c == ':')
        .next()
        .unwrap_or(trimmed)
        .to_ascii_lowercase();
    for bad in &["unknown", "placeholder", "example", "...", "xxx", "todo"] {
        if tail == *bad {
            return Some(match *bad {
                "unknown" => "unknown",
                "placeholder" => "placeholder",
                "example" => "example",
                "..." => "...",
                "xxx" => "xxx",
                "todo" => "todo",
                _ => "placeholder",
            });
        }
    }
    None
}

// ── tool schemas ──────────────────────────────────────────────────────────

fn propose_create_schema(c: &ClassProposeShape) -> ToolSchema {
    let mut props = serde_json::Map::new();
    props.insert(
        "base".to_string(),
        json!({
            "type": "string",
            "description": "Optional URI for the new instance. Auto-minted from base_prefix if omitted; supply one when you need to reference this instance from a follow-up propose_link_child in the same turn.",
        }),
    );
    for (prop_name, type_desc) in &c.scalar_props {
        let required_marker = if c.required.contains(prop_name) {
            "* (required) "
        } else {
            ""
        };
        props.insert(
            prop_name.clone(),
            json!({
                "type": "string",
                "description": format!("{required_marker}{type_desc}"),
            }),
        );
    }

    let parameters = json!({
        "type": "object",
        "properties": Value::Object(props),
        "required": c.required.clone(),
    });

    ToolSchema {
        name: format!("{}_propose_create", c.class_name.to_lowercase()),
        description: format!(
            "Propose creating a new {class} instance. The proposal is buffered — \
             it becomes visible in the perspective (subject to the overlay's \
             human-divergence gate) after the interpretation pass completes.\n\
             \n\
             The response is `proposed create: <uri>` — save that URI verbatim \
             if you need to pass it as `parent` to a follow-up \
             `{lower}_propose_link_child` call in the same pass.",
            class = c.class_name,
            lower = c.class_name.to_lowercase()
        ),
        parameters,
    }
}

fn propose_link_child_schema(c: &ClassProposeShape) -> ToolSchema {
    let parameters = json!({
        "type": "object",
        "properties": {
            "parent": {
                "type": "string",
                "description": "URI of the parent instance. MUST be a real URI: either one returned by a `_query` tool in this same pass, or one just returned by `_propose_create` (its `proposed create: <uri>` result). NEVER invent a URI like `ad4m://obj/unknown` — placeholders are rejected.",
            },
            "predicate": {
                "type": "string",
                "description": "Predicate IRI for the link (e.g. `soa://basedOn`, `rdfs:member`). Use the exact predicate name from the class relationship definition; it appears in the class description as `<predicate>` → <Target>.",
            },
            "child": {
                "type": "string",
                "description": "URI of the child instance. Same rules as `parent`: use a URI returned by `_query` or a URI you just created via `_propose_create`. NEVER invent placeholder URIs.",
            },
        },
        "required": ["parent", "predicate", "child"],
    });

    ToolSchema {
        name: format!("{}_propose_link_child", c.class_name.to_lowercase()),
        description: format!(
            "Propose a parent → child link under a {class} instance. Buffered until the pass completes.\n\
             \n\
             **Workflow — always in this order:**\n\
             1. Call `<class>_query` for each side to discover real URIs (or use a URI from a prior `_propose_create` in this same pass).\n\
             2. Call this tool with those real URIs. Placeholders like `ad4m://obj/unknown` are rejected — the pass makes no progress if you invent URIs.",
            class = c.class_name
        ),
        parameters,
    }
}

// ── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Inner provider that reports zero tools and answers any call with an
    /// echo — lets tests verify delegation without a real perspective.
    struct EmptyInner;

    #[async_trait]
    impl ToolProvider for EmptyInner {
        async fn tools(&self) -> Vec<ToolSchema> {
            Vec::new()
        }
        async fn call(&self, name: &str, _args: Value) -> Result<String> {
            Ok(format!("inner:{name}"))
        }
    }

    fn task_shape() -> ClassProposeShape {
        ClassProposeShape {
            class_name: "Task".into(),
            scalar_props: vec![
                ("title".into(), "string, required, max 1".into()),
                ("status".into(), "string, optional".into()),
            ],
            required: vec!["title".into()],
        }
    }

    fn provider(
        classes: Vec<ClassProposeShape>,
    ) -> (Arc<ProposeWritesProvider<EmptyInner>>, ProposalBuffer) {
        let buffer = ProposalBuffer::new();
        let p = Arc::new(ProposeWritesProvider::new(
            Arc::new(EmptyInner),
            classes,
            buffer.clone(),
            "ns://test".into(),
        ));
        (p, buffer)
    }

    #[tokio::test]
    async fn tools_lists_two_synthetics_per_class_plus_inner() {
        let (p, _) = provider(vec![task_shape()]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert_eq!(
            names,
            vec!["task_propose_create", "task_propose_link_child"]
        );
    }

    #[tokio::test]
    async fn propose_create_appends_to_buffer_and_mints_base_when_missing() {
        let (p, buf) = provider(vec![task_shape()]);
        let out = p
            .call(
                "task_propose_create",
                json!({"title": "Write PR body", "status": "todo"}),
            )
            .await
            .unwrap();
        assert!(out.starts_with("proposed create: ns://test/task/"), "{out}");
        assert_eq!(buf.len(), 1);
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                assert!(base.starts_with("ns://test/task/"));
                assert_eq!(class, "Task");
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write PR body")
                );
                assert_eq!(values.get("status").and_then(|v| v.as_str()), Some("todo"));
            }
            other => panic!("expected Create, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn propose_create_honours_caller_supplied_base() {
        let (p, buf) = provider(vec![task_shape()]);
        p.call(
            "task_propose_create",
            json!({"base": "ns://caller/t1", "title": "T"}),
        )
        .await
        .unwrap();
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::Create { base, .. } => assert_eq!(base, "ns://caller/t1"),
            other => panic!("expected Create, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn propose_create_errors_on_missing_required_field() {
        let (p, buf) = provider(vec![task_shape()]);
        let err = p
            .call("task_propose_create", json!({"status": "todo"}))
            .await
            .unwrap_err();
        assert!(
            err.to_string().contains("missing required fields: title"),
            "{err}"
        );
        assert_eq!(buf.len(), 0, "buffer must not accumulate on error");
    }

    #[tokio::test]
    async fn propose_create_drops_unknown_fields_silently() {
        // LLM hallucinates a `deadline` field the class doesn't have. The
        // pass should proceed with what it got — dropping unknowns beats
        // aborting on every model imperfection.
        let (p, buf) = provider(vec![task_shape()]);
        p.call(
            "task_propose_create",
            json!({"title": "T", "deadline": "2026-01-01"}),
        )
        .await
        .unwrap();
        match &buf.drain()[0] {
            InterpretationOp::Create { values, .. } => {
                assert!(values.get("deadline").is_none());
                assert!(values.get("title").is_some());
            }
            _ => panic!(),
        }
    }

    #[tokio::test]
    async fn propose_link_child_appends_add_links_op() {
        let (p, buf) = provider(vec![task_shape()]);
        p.call(
            "task_propose_link_child",
            json!({
                "parent": "ns://project/p1",
                "predicate": "rdfs:member",
                "child": "ns://task/t1",
            }),
        )
        .await
        .unwrap();
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::AddLinks { source, links } => {
                assert_eq!(source, "ns://project/p1");
                assert_eq!(links.len(), 1);
                assert_eq!(links[0].source, "ns://project/p1");
                assert_eq!(links[0].predicate.as_deref(), Some("rdfs:member"));
                assert_eq!(links[0].target, "ns://task/t1");
            }
            other => panic!("expected AddLinks, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn unknown_tool_delegates_to_inner() {
        let (p, buf) = provider(vec![task_shape()]);
        let out = p.call("some_other_tool", json!({})).await.unwrap();
        assert_eq!(out, "inner:some_other_tool");
        assert_eq!(buf.len(), 0, "delegation must not touch the buffer");
    }

    #[tokio::test]
    async fn unknown_class_on_propose_errors() {
        let (p, _) = provider(vec![task_shape()]);
        let err = p
            .call("nothing_propose_create", json!({"title": "x"}))
            .await
            .unwrap_err();
        assert!(err.to_string().contains("no registered class"), "{err}");
    }

    #[tokio::test]
    async fn hallucinated_create_verb_is_aliased_to_propose_create() {
        // gemma3:12b reaches for `task_create` (not `task_propose_create`)
        // — the harness should transparently dispatch to propose_create so
        // the pass makes progress instead of stalling on tool-not-found.
        for verb in [
            "task_create",
            "create_task",
            "task_new",
            "new_task",
            "add_task",
            "task_add",
        ] {
            let (p, buf) = provider(vec![task_shape()]);
            let out = p
                .call(verb, json!({"title": "x"}))
                .await
                .unwrap_or_else(|e| panic!("verb `{verb}` should have aliased through: {e}"));
            assert!(out.contains("proposed create"), "verb `{verb}`: {out}");
            assert_eq!(buf.len(), 1, "verb `{verb}` should have queued 1 op");
        }
    }

    #[tokio::test]
    async fn hallucinated_create_verb_is_case_insensitive() {
        let (p, buf) = provider(vec![task_shape()]);
        let out = p.call("Task_Create", json!({"title": "x"})).await.unwrap();
        assert!(out.contains("proposed create"), "{out}");
        assert_eq!(buf.len(), 1);
    }

    #[tokio::test]
    async fn hallucinated_verb_for_unknown_class_falls_through() {
        // If the create-shaped verb doesn't match any offered class, it
        // must delegate to the inner provider — otherwise we'd silently
        // swallow real tool calls.
        let (p, buf) = provider(vec![task_shape()]);
        let out = p.call("stranger_create", json!({})).await.unwrap();
        assert_eq!(
            out, "inner:stranger_create",
            "unknown-class verb should fall through to inner"
        );
        assert_eq!(buf.len(), 0);
    }

    #[tokio::test]
    async fn propose_link_child_bounces_placeholder_parent_and_child() {
        // Small models (gemma3:12b) invent placeholder URIs when they
        // skip the query step — observed on CI job 22282 attempts 5/7/8.
        // Rejecting these with a specific redirect (naming the class-
        // specific `_query` tool) gives the LLM an actionable next step.
        for bad in [
            "ad4m://obj/unknown",
            "unknown",
            "ad4m://obj/PLACEHOLDER",
            "ad4m://obj/example",
            "ad4m://obj/...",
            "",
        ] {
            let (p, buf) = provider(vec![task_shape()]);
            let err = p
                .call(
                    "task_propose_link_child",
                    json!({
                        "parent": bad,
                        "predicate": "rdfs:member",
                        "child": "ns://real/child",
                    }),
                )
                .await
                .unwrap_err();
            let s = err.to_string();
            assert!(s.contains("placeholder"), "parent={bad}: {s}");
            assert!(s.contains("_query"), "parent={bad}: {s}");
            assert_eq!(buf.len(), 0, "buffer must not accumulate on placeholder");
        }

        for bad in ["ad4m://obj/unknown", "unknown", ""] {
            let (p, buf) = provider(vec![task_shape()]);
            let err = p
                .call(
                    "task_propose_link_child",
                    json!({
                        "parent": "ns://real/parent",
                        "predicate": "rdfs:member",
                        "child": bad,
                    }),
                )
                .await
                .unwrap_err();
            let s = err.to_string();
            assert!(s.contains("placeholder"), "child={bad}: {s}");
            assert_eq!(buf.len(), 0, "buffer must not accumulate on placeholder");
        }
    }

    #[tokio::test]
    async fn hallucinated_add_relation_verb_returns_actionable_redirect() {
        // gemma3:12b reaches for `<class>_add_<relation>` (e.g.
        // `extintention_add_basedon`) when it wants to attach a relation.
        // CI job 22282 attempts 1-4 stalled on this shape because the
        // filtered-inner error was too generic. Redirect must name the
        // real tool + a plausible predicate hint.
        let (p, buf) = provider(vec![task_shape()]);
        let err = p
            .call("task_add_owner", json!({}))
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("task_propose_link_child"),
            "redirect must name the real tool: {err}"
        );
        assert!(err.contains("soa://owner"), "predicate hint missing: {err}");
        assert!(err.contains("_query"), "must guide toward query: {err}");
        assert_eq!(buf.len(), 0, "redirect must not touch the buffer");
    }

    #[tokio::test]
    async fn hallucinated_add_relation_verb_for_unknown_class_falls_through() {
        // If the add-shape verb doesn't match any offered class, delegate
        // to inner — otherwise we'd shadow real tool calls.
        let (p, buf) = provider(vec![task_shape()]);
        let out = p.call("stranger_add_thing", json!({})).await.unwrap();
        assert_eq!(out, "inner:stranger_add_thing");
        assert_eq!(buf.len(), 0);
    }
}
