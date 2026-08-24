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
    /// Relations declared on the class — the enum of predicates the LLM can
    /// legally pick for `_propose_link_child` on instances of this class.
    /// Also drives dispatch-time normalization (LLM sends `basedOn` /
    /// `soa:basedOn` → canonical `ns://basedOn`).
    pub relations: Vec<RelationInfo>,
}

/// One relation on a `ClassProposeShape` — the harness's per-relation view
/// of a SHACL property that points to another class. The `hint` field
/// (from the property's `ad4m://interpretation_hint` link) is what makes
/// `basedOn` mean "prior beliefs this intention derives from" instead of
/// just "some link" in the tool schema description.
#[derive(Debug, Clone)]
pub struct RelationInfo {
    /// Property name as declared in SHACL, e.g. `basedOn`.
    pub name: String,
    /// Canonical predicate URI, e.g. `ns://basedOn`. This is what lands in
    /// the actual `Link.predicate` and what `LinkQuery { predicate: … }`
    /// matches on.
    pub predicate: String,
    /// Natural-language meaning of the relation, from the SDNA's
    /// `interpretationHint`. Rendered into the `_propose_link_child`
    /// `predicate` field description so the LLM knows which relation
    /// applies to which situation.
    pub hint: Option<String>,
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
    // Relations = properties with a typed target class (sh:class) and a
    // predicate URI. Covers hasMany (basedOn → Belief) and hasOne alike.
    // Carry `interpretation_hint` alongside — the harness renders it into
    // the propose_link_child schema so the LLM knows *what* each relation
    // means, not just that it exists.
    let relations: Vec<RelationInfo> = class
        .properties
        .iter()
        .filter(|p| p.class.is_some())
        .filter_map(|p| {
            p.predicate.clone().map(|pred| RelationInfo {
                name: p.name.clone(),
                predicate: pred,
                hint: p.interpretation_hint.clone(),
            })
        })
        .collect();
    ClassProposeShape {
        class_name: class.name.clone(),
        scalar_props,
        required,
        relations,
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
            // Only advertise `_propose_link_child` for classes that DECLARE at
            // least one relation. Without this gate the tool degenerates into
            // a wildcard link-writer (any predicate, any parent/child) — and
            // gemma3:12b reliably calls the WRONG side, e.g. writes
            // `belief_propose_link_child(parent=belief, predicate=basedOn,
            // child=intention)`. The link then lands with source=belief,
            // target=intention, and `LinkQuery { source: intention,
            // predicate: basedOn }` returns zero. Observed 2026-08-24
            // CI job 22735 (Scenario E) — 8/8 attempts failed with links
            // in the wrong direction because Belief has no `basedOn`
            // relation, only Intention does.
            if !c.relations.is_empty() {
                out.push(propose_link_child_schema(c));
            }
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
            // Same gate as the `tools()` advertisement: classes with no
            // declared relations don't get `_propose_link_child`, so
            // redirecting them to a nonexistent tool would just deadlock
            // the LLM. Skip → falls through to the inner provider's error.
            if c.relations.is_empty() {
                continue;
            }
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

        // Base URI is always auto-minted, never LLM-supplied. Earlier the
        // schema exposed a `base` field the LLM could override to reference
        // a fresh instance from a follow-up propose_link_child in the same
        // pass, but small local models used the field to invent trivially
        // short URIs (`soa://ext/intention/12345`, observed 2026-08-24
        // scenario E attempt 6 on Marvin's gemma3:12b) which passed the
        // scheme whitelist, got written with a partial link set, and then
        // failed model-query conformance on read-back — a real footgun for
        // no real user benefit, since the LLM sees the minted URI in the
        // ack (`"proposed create: soa://ext/intention/<uuid>"`) and can
        // use that verbatim as `parent` in a follow-up propose_link_child.
        let base = format!(
            "{}{}{}/{}",
            self.base_prefix,
            if self.base_prefix.ends_with('/') {
                ""
            } else {
                "/"
            },
            lower_class,
            Uuid::new_v4()
        );

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
        let class_shape = self
            .find_class(lower_class)
            .ok_or_else(|| anyhow!("no registered class `{lower_class}` for propose_link_child"))?;

        // Defensive: `_propose_link_child` isn't advertised for classes
        // without declared relations (see `tools()` above), so a call landing
        // here means the LLM guessed a tool name that isn't in the surface.
        // Return an actionable error naming a class that DOES declare
        // relations, if any — so the LLM has somewhere to redirect to.
        if class_shape.relations.is_empty() {
            let with_relations: Vec<String> = self
                .classes
                .iter()
                .filter(|c| !c.relations.is_empty())
                .map(|c| format!("`{}_propose_link_child`", c.class_name.to_lowercase()))
                .collect();
            let hint = if with_relations.is_empty() {
                "No class in this pass declares a relation — this tool is unavailable".to_string()
            } else {
                format!(
                    "Only these classes declare relations you can link through: {}",
                    with_relations.join(", ")
                )
            };
            return Err(anyhow!(
                "{lower_class}_propose_link_child is not a valid tool — class `{}` \
                 declares no relations. {hint}. Remember: the class that OWNS the \
                 relation is the `parent`; the referenced instance is the `child`.",
                class_shape.class_name
            ));
        }

        // Class-directional field name: `{lower_class}_uri` (e.g.
        // `intention_uri`). Structural rename from the older generic
        // `parent`/`child` — small models (gemma3:12b, observed 2026-08-24
        // Root tests on `e62474c75` for `harness_intention_links_to_seeded_
        // beliefs`) read "intention rests on beliefs" as "intention is the
        // CHILD of beliefs" and swap `parent`/`child` 8/8 attempts, so the
        // link lands with `source=belief, target=intention` and the
        // `LinkQuery { source: intention, predicate: basedOn }` in the test
        // returns zero. `<class>_uri` makes it lexically impossible to place
        // the wrong instance on the source side. Accept the legacy
        // `parent` name too for callers that still use it.
        let source_field = format!("{lower_class}_uri");
        let parent = args
            .get(&source_field)
            .or_else(|| args.get("parent"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                anyhow!(
                    "propose_link_child: missing `{source_field}` — the URI of the \
                     `{class}` instance that owns the relation (the SOURCE side of \
                     the link).",
                    class = class_shape.class_name
                )
            })?
            .to_string();
        let raw_predicate = args
            .get("predicate")
            .and_then(|v| v.as_str())
            .ok_or_else(|| anyhow!("propose_link_child: missing `predicate` (link predicate IRI)"))?
            .to_string();
        // Normalize predicate against the class's declared relations, when the
        // class has any. Accepted forms → canonical SHACL URI:
        //   - Exact URI match (`ns://basedOn` → `ns://basedOn`)
        //   - Local-name match after `/` or `:` (`basedOn`, `soa:basedOn`,
        //     `rdfs:basedOn`, `ns://basedOn`)
        // Anything else is rejected with an actionable error listing the
        // valid predicates for this class. Rationale: gemma3:12b writes
        // `soa:basedOn` when the SDNA declares `ns://basedOn`, so the write
        // lands under the wrong predicate and downstream queries miss it
        // (observed 2026-08-24 harness_intention_links_to_seeded_beliefs
        // attempt 8 — all three seeded beliefs linked but under `soa:basedOn`).
        let predicate = if class_shape.relations.is_empty() {
            raw_predicate.clone()
        } else {
            match resolve_relation_predicate(&class_shape.relations, &raw_predicate) {
                Some(canonical) => canonical,
                None => {
                    let valid = class_shape
                        .relations
                        .iter()
                        .map(|r| format!("`{}` (relation `{}`)", r.predicate, r.name))
                        .collect::<Vec<_>>()
                        .join(", ");
                    return Err(anyhow!(
                        "{lower_class}_propose_link_child: `predicate` `{raw_predicate}` \
                         is not a declared relation on class `{}`. Valid predicates: {valid}. \
                         Use the exact URI shown — do not abbreviate or re-scheme.",
                        class_shape.class_name
                    ));
                }
            }
        };
        // Target-side field: `linked_uri` (generic, since a relation-typed
        // class may hasMany multiple target classes). Accept the legacy
        // `child` name too.
        let child = args
            .get("linked_uri")
            .or_else(|| args.get("child"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                anyhow!(
                    "propose_link_child: missing `linked_uri` — the URI of the \
                     instance being linked TO via `{predicate}` (the TARGET side)."
                )
            })?
            .to_string();

        // Bounce placeholder URIs (`ad4m://obj/unknown`, `.../placeholder`,
        // `.../example`, `.../...`) — small models tend to invent these when
        // they skip the query step. Observed on CI job 22282 attempts 5/7/8
        // where the LLM called `_propose_link_child` with `ad4m://obj/unknown`
        // instead of first calling `<class>_query` to get real URIs. Failing
        // fast with a specific redirect gives the LLM an actionable next step.
        if let Some(bad) = placeholder_uri(&parent) {
            return Err(anyhow!(
                "propose_link_child: `{source_field}` looks like a placeholder ({bad}). \
                 Do NOT invent URIs. First call the class-specific `_query` tool \
                 (e.g. `{lower_class}_query`) to discover real URIs, then pass the \
                 URI returned by the query verbatim as `{source_field}`."
            ));
        }
        if let Some(bad) = placeholder_uri(&child) {
            return Err(anyhow!(
                "propose_link_child: `linked_uri` looks like a placeholder ({bad}). \
                 Do NOT invent URIs. First call the class-specific `_query` tool \
                 for the target class to discover real URIs, then pass the URI \
                 returned by the query verbatim as `linked_uri`."
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
/// Recognised:
/// - empty string
/// - angle-bracket-wrapped template like `<uri>`, `<URI>`, `<target>` —
///   small models (gemma3:12b) copy schema-example placeholders verbatim
///   (observed 2026-08-24 harness_intention_links_to_seeded_beliefs
///   attempts 3 & 8 where `base: "<uri>"` became the base URI and the
///   subsequent read-back panicked)
/// - trailing `unknown` / `placeholder` / `example` / `...` / `xxx` /
///   `todo` (case-insensitive) — the tail after the last `/` or `:` is
///   checked so `ad4m://obj/unknown` and plain `unknown` both bounce
/// - unknown-scheme "URIs" like `belief_query:...` (a tool-name + query
///   pasted as URI, observed same test attempt 7) — only ad4m/soa/
///   literal/https/http/urn schemes are accepted for link URIs
/// Resolve an LLM-supplied `predicate` string against a class's declared
/// relations. Accepts exact URI matches and local-name matches (the tail
/// after the last `/` or `:` — so `basedOn`, `soa:basedOn`, `ns://basedOn`
/// all resolve to `ns://basedOn` when SHACL declared that URI). Returns the
/// canonical SHACL URI on success, `None` when the predicate can't be
/// matched to any declared relation — the caller turns that into an
/// actionable error naming the valid predicates.
fn resolve_relation_predicate(relations: &[RelationInfo], raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    for r in relations {
        if r.predicate == trimmed {
            return Some(r.predicate.clone());
        }
    }
    let local_of = |s: &str| -> String {
        s.rsplit(|c: char| c == '/' || c == ':')
            .next()
            .unwrap_or(s)
            .to_string()
    };
    let raw_local = local_of(trimmed).to_ascii_lowercase();
    if raw_local.is_empty() {
        return None;
    }
    for r in relations {
        if r.name.eq_ignore_ascii_case(&raw_local) {
            return Some(r.predicate.clone());
        }
        if local_of(&r.predicate).eq_ignore_ascii_case(&raw_local) {
            return Some(r.predicate.clone());
        }
    }
    None
}

fn placeholder_uri(uri: &str) -> Option<&'static str> {
    let trimmed = uri.trim();
    if trimmed.is_empty() {
        return Some("empty");
    }
    if trimmed.len() >= 3 && trimmed.starts_with('<') && trimmed.ends_with('>') {
        return Some("template-placeholder (angle brackets)");
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
    // Scheme whitelist for link URIs. `belief_query:Local-first...` gets
    // through the tail check because `local-first...` isn't in the bad
    // list, but the scheme `belief_query` is nonsense — the LLM likely
    // pasted a tool name + query as the URI. If the string has an `:`
    // (any scheme-like separator), the part before must be a known scheme.
    // Strings without any `:` (bare tokens) are not rejected here — the
    // tail check above catches the common bare placeholders.
    if let Some(colon_at) = trimmed.find(':') {
        let scheme = &trimmed[..colon_at].to_ascii_lowercase();
        let is_known = matches!(
            scheme.as_str(),
            "ad4m" | "soa" | "literal" | "https" | "http" | "urn" | "did"
        );
        if !is_known {
            return Some("unknown-scheme (not ad4m/soa/literal/https/http/urn/did)");
        }
    }
    None
}

// ── tool schemas ──────────────────────────────────────────────────────────

fn propose_create_schema(c: &ClassProposeShape) -> ToolSchema {
    let mut props = serde_json::Map::new();
    // No `base` field: the URI is always auto-minted. Historically we let
    // the LLM override it, but small local models invented trivially short
    // URIs (`.../intention/12345`) that passed scheme validation but failed
    // model-query conformance on read-back — the LLM sees the minted URI in
    // the ack anyway, so exposing the field only creates footguns.
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
             The response is a string starting with `proposed create: ` followed \
             by the newly-minted URI (for example `proposed create: soa://ext/{lower}/9c616970-…`). \
             Extract the URI part after the prefix and pass it verbatim as \
             `parent` (or `child`) in follow-up `{lower}_propose_link_child` \
             calls in the same pass — that URI is how you link this new \
             instance to related instances.",
            class = c.class_name,
            lower = c.class_name.to_lowercase()
        ),
        parameters,
    }
}

fn propose_link_child_schema(c: &ClassProposeShape) -> ToolSchema {
    // Predicate schema: if the class declares relations, restrict `predicate`
    // to the exact URIs SHACL knows about (grammar-constrained decoding then
    // makes it impossible for the LLM to invent a variant like `soa:basedOn`
    // when the canonical form is `ns://basedOn`). Without relations declared
    // we fall back to the old free-form string so callers of legacy classes
    // still work.
    let predicate_schema = if c.relations.is_empty() {
        json!({
            "type": "string",
            "description": "Predicate IRI for the link. Use the exact predicate URI from the class's relation definition — invented variants like `soa:basedOn` when the class declares `ns://basedOn` will be rejected."
        })
    } else {
        let allowed: Vec<&str> = c.relations.iter().map(|r| r.predicate.as_str()).collect();
        // Per-predicate description: `URI (relation "name" — interpretation hint)`.
        // The hint (from RelationOptions.interpretationHint / SHACL
        // ad4m://interpretation_hint) is what tells the LLM which relation
        // applies semantically. Without it the LLM has to guess from the
        // predicate name alone (e.g. `basedOn` is ambiguous — "based on
        // what"?). When hint is absent, fall back to the bare "name → target
        // class" phrasing.
        let pretty = c
            .relations
            .iter()
            .map(|r| match &r.hint {
                Some(hint) => format!("`{}` (relation `{}` — {})", r.predicate, r.name, hint),
                None => format!("`{}` (relation `{}` → target class)", r.predicate, r.name),
            })
            .collect::<Vec<_>>()
            .join(", ");
        json!({
            "type": "string",
            "enum": allowed,
            "description": format!(
                "Exact predicate URI declared on this class. One of: {pretty}. \
                 Do NOT abbreviate (`basedOn`) or re-scheme (`soa:basedOn`) — the \
                 SHACL-declared URI is the only valid value; anything else is \
                 rejected."
            )
        })
    };

    // Structural rename to defeat the parent/child direction inversion:
    // gemma3:12b reads "an intention rests on beliefs" as "intention is
    // the CHILD of beliefs" and swapped `parent`/`child` 8/8 attempts in
    // CI Root tests on `e62474c75` (2026-08-24 Scenario D). Naming the
    // source field after the tool's namesake class (`intention_uri`)
    // makes it lexically impossible to place the wrong instance on the
    // source side. Legacy `parent`/`child` still accepted at dispatch.
    let class_lower = c.class_name.to_lowercase();
    let source_field = format!("{class_lower}_uri");
    let parameters = json!({
        "type": "object",
        "properties": {
            source_field.clone(): {
                "type": "string",
                "description": format!(
                    "URI of the `{class}` instance that OWNS the relation (SOURCE side of the link). \
                     MUST be a real URI you already have on hand: either the URI returned by a \
                     `{class_lower}_query` in this same pass (extract from its JSON `id`/`uri` field), \
                     or the URI part of a `proposed create: <URI>` response from `{class_lower}_propose_create`. \
                     Do NOT copy schema-example placeholders like `<uri>` or `ad4m://obj/unknown` — \
                     those are rejected. Do NOT paste tool names or query text as URIs.",
                    class = c.class_name,
                    class_lower = class_lower,
                ),
            },
            "predicate": predicate_schema,
            "linked_uri": {
                "type": "string",
                "description": "URI of the instance being linked TO via `predicate` (TARGET side of the link). Same URI-sourcing rules as the source-side field: use a URI returned by the target class's `_query` (JSON `id`/`uri`) or a URI from a prior `_propose_create` in this same pass. NEVER invent or copy schema placeholders.",
            },
        },
        "required": [source_field.clone(), "predicate", "linked_uri"],
    });

    // Class description enumerates the declared relations inline so the LLM
    // sees "relation name + canonical predicate + target class direction"
    // before it drafts a tool call — the two most common failure modes on
    // gemma3:12b were (a) inventing a predicate variant, (b) linking with a
    // relation that isn't declared on the class.
    let rel_hint = if c.relations.is_empty() {
        String::new()
    } else {
        let list = c
            .relations
            .iter()
            .map(|r| match &r.hint {
                Some(hint) => {
                    format!("  - `{}` → predicate `{}` — {hint}", r.name, r.predicate)
                }
                None => format!("  - `{}` → predicate `{}`", r.name, r.predicate),
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("\n\n**Declared relations on `{class}` (pick `predicate` from these URIs exactly):**\n{list}", class = c.class_name)
    };

    ToolSchema {
        name: format!("{}_propose_link_child", class_lower),
        description: format!(
            "Attach an existing {class} to a related instance via one of its declared relations. Buffered until the pass completes.\n\
             \n\
             **DIRECTION — READ CAREFULLY:** `{source_field}` MUST be the `{class}` (source of the link, the side that OWNS the relation). `linked_uri` MUST be the related instance (target of the link). Do NOT invert. Example: if `{class}` has relation `basedOn → Belief`, then `{source_field}` = the {class}'s URI, `linked_uri` = the Belief's URI.\n\
             \n\
             **Workflow — always in this order:**\n\
             1. Call `_query` for each side to discover real URIs (or reuse a URI from a prior `_propose_create` in this same pass).\n\
             2. Call this tool with those real URIs. Placeholders like `ad4m://obj/unknown` are rejected — the pass makes no progress if you invent URIs.{rel_hint}",
            class = c.class_name,
            source_field = source_field,
            rel_hint = rel_hint,
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
            relations: Vec::new(),
        }
    }

    /// Shape with a declared relation — mirrors the
    /// harness_intention_links_to_seeded_beliefs Scenario D fixture so the
    /// predicate-normalization tests exercise the real shape.
    fn intention_shape() -> ClassProposeShape {
        ClassProposeShape {
            class_name: "Intention".into(),
            scalar_props: vec![
                ("title".into(), "string, required, max 1".into()),
                ("owner".into(), "string, optional".into()),
            ],
            required: vec!["title".into()],
            relations: vec![RelationInfo {
                name: "basedOn".into(),
                predicate: "ns://basedOn".into(),
                hint: Some("The prior beliefs this intention derives from.".into()),
            }],
        }
    }

    fn intention_shape_without_hint() -> ClassProposeShape {
        ClassProposeShape {
            class_name: "Intention".into(),
            scalar_props: vec![("title".into(), "string, required, max 1".into())],
            required: vec!["title".into()],
            relations: vec![RelationInfo {
                name: "basedOn".into(),
                predicate: "ns://basedOn".into(),
                hint: None,
            }],
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
    async fn tools_lists_propose_create_for_relationless_class() {
        // Regression against CI job 22735 (Scenario E, 2026-08-24) — the
        // `_propose_link_child` tool must NOT be advertised for classes
        // without declared relations. gemma3:12b would call it with the
        // parent/child directions swapped, poisoning downstream link queries.
        let (p, _) = provider(vec![task_shape()]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert_eq!(names, vec!["task_propose_create"]);
    }

    #[tokio::test]
    async fn tools_lists_both_synthetics_for_class_with_relations() {
        let (p, _) = provider(vec![intention_shape()]);
        let names: Vec<String> = p.tools().await.into_iter().map(|t| t.name).collect();
        assert_eq!(
            names,
            vec!["intention_propose_create", "intention_propose_link_child"]
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
    async fn propose_create_ignores_caller_supplied_base_and_auto_mints() {
        // Regression against the earlier "honour caller-supplied base" behaviour
        // that let small local models poison a pass with trivially short URIs
        // that failed model-query conformance on read-back. Auto-mint always.
        let (p, buf) = provider(vec![task_shape()]);
        p.call(
            "task_propose_create",
            json!({"base": "soa://caller/12345", "title": "T"}),
        )
        .await
        .unwrap();
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::Create { base, .. } => {
                assert!(
                    base.starts_with("ns://test/task/"),
                    "caller-supplied `base` must be ignored; got {base}"
                );
            }
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
        // Uses intention_shape (has `basedOn` relation) because Task has none
        // and no longer advertises `_propose_link_child` after the Scenario E
        // fix — the relationless-class test above is the deliberate flip side.
        let (p, buf) = provider(vec![intention_shape()]);
        p.call(
            "intention_propose_link_child",
            json!({
                "parent": "soa://intention/i1",
                "predicate": "ns://basedOn",
                "child": "soa://belief/b1",
            }),
        )
        .await
        .unwrap();
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::AddLinks { source, links } => {
                assert_eq!(source, "soa://intention/i1");
                assert_eq!(links.len(), 1);
                assert_eq!(links[0].source, "soa://intention/i1");
                assert_eq!(links[0].predicate.as_deref(), Some("ns://basedOn"));
                assert_eq!(links[0].target, "soa://belief/b1");
            }
            other => panic!("expected AddLinks, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn propose_link_child_accepts_class_directional_field_names() {
        // Regression against 2026-08-24 Root-tests failure on `e62474c75`:
        // gemma3:12b read "intention rests on beliefs" as "intention is the
        // CHILD of beliefs" and inverted `parent`/`child` 8/8 attempts. The
        // schema now names the source field `<class>_uri` (e.g.
        // `intention_uri`) so it's lexically impossible to place the wrong
        // instance on the source side. Verifies both new names round-trip
        // to the same AddLinks op as the legacy `parent`/`child` above.
        let (p, buf) = provider(vec![intention_shape()]);
        p.call(
            "intention_propose_link_child",
            json!({
                "intention_uri": "soa://intention/i1",
                "predicate": "ns://basedOn",
                "linked_uri": "soa://belief/b1",
            }),
        )
        .await
        .unwrap();
        let drained = buf.drain();
        match &drained[0] {
            InterpretationOp::AddLinks { source, links } => {
                assert_eq!(source, "soa://intention/i1");
                assert_eq!(links[0].source, "soa://intention/i1");
                assert_eq!(links[0].target, "soa://belief/b1");
            }
            other => panic!("expected AddLinks, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn propose_link_child_schema_names_source_field_after_class() {
        // The schema for `<class>_propose_link_child` must expose the source
        // slot as `<class>_uri` and the target slot as `linked_uri`, both
        // marked `required[]`. The class-directional name is the entire
        // defense against small-model direction inversion.
        let schema = propose_link_child_schema(&intention_shape());
        let params = &schema.parameters;
        let props = &params["properties"];
        assert!(
            props.get("intention_uri").is_some(),
            "expected source field `intention_uri`, got props: {props:?}"
        );
        assert!(
            props.get("linked_uri").is_some(),
            "expected target field `linked_uri`, got props: {props:?}"
        );
        let required: Vec<&str> = params["required"]
            .as_array()
            .expect("required[] must be an array")
            .iter()
            .filter_map(|v| v.as_str())
            .collect();
        assert!(required.contains(&"intention_uri"));
        assert!(required.contains(&"linked_uri"));
        // Description must call out direction explicitly — the psychological
        // failure mode is that the LLM reads "intention rests on beliefs"
        // and swaps source/target. The tool prose is the belt to the
        // rename's braces.
        let desc = schema.description.to_lowercase();
        assert!(
            desc.contains("direction") && desc.contains("intention_uri"),
            "description must include a direction cue naming the source field: {}",
            schema.description
        );
    }

    #[tokio::test]
    async fn propose_link_child_errors_when_class_has_no_relations() {
        // Defensive backstop: `_propose_link_child` isn't advertised for
        // relationless classes, but if the LLM guesses the name anyway the
        // dispatch handler must return a redirect naming a class that DOES
        // declare relations rather than silently accepting any predicate.
        let (p, buf) = provider(vec![task_shape(), intention_shape()]);
        let err = p
            .call(
                "task_propose_link_child",
                json!({
                    "parent": "soa://project/p1",
                    "predicate": "rdfs:member",
                    "child": "soa://task/t1",
                }),
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("declares no relations"),
            "expected 'declares no relations' hint, got: {err}"
        );
        assert!(
            err.contains("intention_propose_link_child"),
            "must redirect to a class that DOES declare relations, got: {err}"
        );
        assert_eq!(buf.len(), 0, "buffer must not accumulate on rejected link");
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
        // skip the query step — observed on CI job 22282 attempts 5/7/8
        // and 2026-08-24 harness_intention_links_to_seeded_beliefs
        // attempts 3, 7, 8. Rejecting these with a specific redirect
        // (naming the class-specific `_query` tool) gives the LLM an
        // actionable next step.
        for bad in [
            "ad4m://obj/unknown",
            "unknown",
            "ad4m://obj/PLACEHOLDER",
            "ad4m://obj/example",
            "ad4m://obj/...",
            "",
            "<uri>",
            "<URI>",
            "<target>",
            "belief_query:Local-first beats cloud-first",
            "belief_query:Small models",
        ] {
            let (p, buf) = provider(vec![intention_shape()]);
            let err = p
                .call(
                    "intention_propose_link_child",
                    json!({
                        "parent": bad,
                        "predicate": "ns://basedOn",
                        "child": "soa://real/child",
                    }),
                )
                .await
                .unwrap_err();
            let s = err.to_string();
            assert!(s.contains("placeholder"), "parent={bad}: {s}");
            assert!(s.contains("_query"), "parent={bad}: {s}");
            assert_eq!(buf.len(), 0, "buffer must not accumulate on placeholder");
        }

        for bad in [
            "ad4m://obj/unknown",
            "unknown",
            "",
            "<uri>",
            "belief_query:foo",
        ] {
            let (p, buf) = provider(vec![intention_shape()]);
            let err = p
                .call(
                    "intention_propose_link_child",
                    json!({
                        "parent": "soa://real/parent",
                        "predicate": "ns://basedOn",
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
    async fn propose_create_always_auto_mints_ignoring_any_llm_base() {
        // Regression: earlier passes accepted `base` from the LLM and only
        // bounced obvious placeholders (`<uri>`, `unknown`, empty). Small
        // models slipped by with URIs that looked real but weren't (e.g.
        // `.../intention/12345`) and later crashed model-query read-back.
        // Now `base` is ignored regardless of value — always UUID under
        // base_prefix — and the crash surface is closed structurally.
        for llm_supplied in [
            "<uri>",
            "unknown",
            "",
            "soa://ext/task/12345",
            "soa://ext/task/my-explicit-id",
            "belief_query:foo",
        ] {
            let (p, buf) = provider(vec![task_shape()]);
            let out = p
                .call(
                    "task_propose_create",
                    json!({"title": "x", "base": llm_supplied}),
                )
                .await
                .unwrap();
            assert!(
                out.starts_with("proposed create: ns://test/task/"),
                "base={llm_supplied}: expected auto-mint, got {out}"
            );
            assert_eq!(buf.len(), 1);
            match &buf.drain()[0] {
                InterpretationOp::Create { base, .. } => {
                    assert!(
                        base.starts_with("ns://test/task/")
                            && base.len() > "ns://test/task/".len() + 30,
                        "base={llm_supplied}: expected auto-minted uuid path, got {base}"
                    );
                    assert_ne!(base, llm_supplied, "must NOT honour LLM-supplied base");
                }
                _ => panic!(),
            }
        }
    }

    #[tokio::test]
    async fn hallucinated_add_relation_verb_returns_actionable_redirect() {
        // gemma3:12b reaches for `<class>_add_<relation>` (e.g.
        // `extintention_add_basedon`) when it wants to attach a relation.
        // CI job 22282 attempts 1-4 stalled on this shape because the
        // filtered-inner error was too generic. Redirect must name the
        // real tool + a plausible predicate hint. Only fires for classes
        // that DO declare relations — redirecting relationless classes
        // to a nonexistent `_propose_link_child` would deadlock the LLM.
        let (p, buf) = provider(vec![intention_shape()]);
        let err = p
            .call("intention_add_basedon", json!({}))
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("intention_propose_link_child"),
            "redirect must name the real tool: {err}"
        );
        assert!(
            err.contains("soa://basedon"),
            "predicate hint missing: {err}"
        );
        assert!(err.contains("_query"), "must guide toward query: {err}");
        assert_eq!(buf.len(), 0, "redirect must not touch the buffer");
    }

    #[tokio::test]
    async fn hallucinated_add_relation_verb_for_relationless_class_falls_through() {
        // Regression against CI job 22735 (Scenario E, 2026-08-24) — the
        // add-relation redirect must SKIP classes without declared relations
        // because they no longer advertise `_propose_link_child`. Falling
        // through to the inner provider surfaces the real "tool not found"
        // error, which is at least honest.
        let (p, buf) = provider(vec![task_shape()]);
        let out = p.call("task_add_owner", json!({})).await.unwrap();
        assert_eq!(out, "inner:task_add_owner");
        assert_eq!(buf.len(), 0);
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

    // ── predicate normalization (relations enum) ──────────────────────────

    #[tokio::test]
    async fn link_child_schema_enums_predicate_when_class_has_relations() {
        // Class with declared relations MUST advertise `predicate` as an
        // enum of the canonical URIs — grammar-constrained decoding then
        // makes it impossible for the LLM to write `soa:basedOn` when the
        // SDNA declared `ns://basedOn`.
        let schema = propose_link_child_schema(&intention_shape());
        let params = &schema.parameters;
        let pred = &params["properties"]["predicate"];
        assert_eq!(pred["type"], "string");
        let enum_vals: Vec<&str> = pred["enum"]
            .as_array()
            .expect("predicate enum should be an array when relations are declared")
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();
        assert_eq!(enum_vals, vec!["ns://basedOn"]);
        let desc = schema.description.to_lowercase();
        assert!(
            desc.contains("basedon") && desc.contains("ns://basedon"),
            "description should enumerate declared relations: {}",
            schema.description
        );
    }

    #[tokio::test]
    async fn link_child_schema_predicate_stays_freeform_without_relations() {
        // Legacy shapes without declared relations keep the free-form
        // predicate string — no enum surprise for existing callers.
        let schema = propose_link_child_schema(&task_shape());
        let pred = &schema.parameters["properties"]["predicate"];
        assert_eq!(pred["type"], "string");
        assert!(
            pred.get("enum").is_none(),
            "predicate must not carry an enum when the class has no declared relations"
        );
    }

    #[tokio::test]
    async fn link_child_normalizes_local_name_to_canonical_predicate() {
        // Bare local name `basedOn` → declared URI `ns://basedOn`. Exercises
        // the exact failure mode observed 2026-08-24 attempt 8 (LLM wrote
        // `soa:basedOn` when the SDNA declared `ns://basedOn`).
        let (p, buf) = provider(vec![intention_shape()]);
        let variants = ["basedOn", "soa:basedOn", "rdfs:basedOn", "ns://basedOn"];
        for variant in variants {
            buf.drain();
            let _ = p
                .call(
                    "intention_propose_link_child",
                    json!({
                        "parent": "soa://ext/intention/parent",
                        "predicate": variant,
                        "child": "soa://existing/belief/1",
                    }),
                )
                .await
                .unwrap_or_else(|e| panic!("variant `{variant}` should normalize, got err: {e:#}"));
            let ops = buf.drain();
            assert_eq!(ops.len(), 1, "variant `{variant}`");
            match &ops[0] {
                InterpretationOp::AddLinks { links, .. } => {
                    assert_eq!(
                        links[0].predicate.as_deref(),
                        Some("ns://basedOn"),
                        "variant `{variant}` should normalize to canonical URI"
                    );
                }
                other => panic!("expected AddLinks, got {other:?}"),
            }
        }
    }

    #[tokio::test]
    async fn link_child_rejects_undeclared_predicate_with_valid_list() {
        // Any predicate the class doesn't declare is rejected with an
        // actionable error naming the valid predicates — the LLM can
        // recover in the next tool call rather than bailing in plain text.
        let (p, _buf) = provider(vec![intention_shape()]);
        let err = p
            .call(
                "intention_propose_link_child",
                json!({
                    "parent": "soa://ext/intention/parent",
                    "predicate": "ns://randomlyInvented",
                    "child": "soa://existing/belief/1",
                }),
            )
            .await
            .expect_err("undeclared predicate must be rejected");
        let msg = err.to_string();
        assert!(
            msg.contains("ns://basedOn"),
            "err should list valid predicates: {msg}"
        );
        assert!(
            msg.contains("randomlyInvented"),
            "err should echo the bad predicate for context: {msg}"
        );
    }

    #[tokio::test]
    async fn propose_link_child_schema_carries_relation_interpretation_hint() {
        // When the SDNA declared a per-relation interpretation hint, it
        // should appear in the predicate-field description alongside the URI
        // and relation name — this is what tells the LLM what the relation
        // means semantically, not just that it exists. Without it the model
        // has to guess from the predicate name (`basedOn` is ambiguous —
        // "based on what?"). Pulled into #911 from the deferred
        // feature/relation-interpretation-hints branch per Nico's 2026-08-24
        // ask ("do that in this PR already").
        let (p, _buf) = provider(vec![intention_shape()]); // hint = "The prior beliefs..."
        let tools = p.tools().await;
        let link = tools
            .into_iter()
            .find(|t| t.name == "intention_propose_link_child")
            .expect("intention_propose_link_child should be advertised");
        let predicate_desc = link.parameters["properties"]["predicate"]["description"]
            .as_str()
            .expect("predicate.description must be a string");
        assert!(
            predicate_desc.contains("The prior beliefs this intention derives from."),
            "predicate description must render the relation's interpretation hint; got: {predicate_desc}"
        );
        // Class-level description also lists the relation with its hint (the
        // "Declared relations" section) so the LLM sees the hint in both
        // places — right before it drafts the call.
        assert!(
            link.description
                .contains("The prior beliefs this intention derives from."),
            "tool description must also render the hint; got: {}",
            link.description
        );
    }

    #[tokio::test]
    async fn propose_link_child_schema_omits_hint_gracefully_when_absent() {
        // SDNA with a relation but no declared interpretation hint — the
        // schema falls back to the bare "name → target class" phrasing
        // instead of dangling an empty `— ` marker.
        let (p, _buf) = provider(vec![intention_shape_without_hint()]);
        let tools = p.tools().await;
        let link = tools
            .into_iter()
            .find(|t| t.name == "intention_propose_link_child")
            .unwrap();
        let predicate_desc = link.parameters["properties"]["predicate"]["description"]
            .as_str()
            .unwrap();
        assert!(
            predicate_desc.contains("`basedOn`"),
            "still names the relation: {predicate_desc}"
        );
        assert!(
            !predicate_desc.contains("— )"),
            "no empty hint marker leaks through: {predicate_desc}"
        );
    }

    #[test]
    fn resolve_relation_predicate_covers_documented_variants() {
        // Pure-function coverage of the resolver — cheaper regression net
        // than the tokio-driven decorator tests above, and readable as a
        // spec of what forms we accept.
        let rels = vec![RelationInfo {
            name: "basedOn".into(),
            predicate: "ns://basedOn".into(),
            hint: None,
        }];
        for accepted in [
            "ns://basedOn",
            "basedOn",
            "BasedOn",
            "soa:basedOn",
            "rdfs:basedOn",
            "ns://basedOn ",
        ] {
            assert_eq!(
                resolve_relation_predicate(&rels, accepted).as_deref(),
                Some("ns://basedOn"),
                "should accept `{accepted}`"
            );
        }
        for rejected in ["", "   ", "invented", "ns://otherRelation"] {
            assert!(
                resolve_relation_predicate(&rels, rejected).is_none(),
                "should reject `{rejected}`"
            );
        }
    }
}
