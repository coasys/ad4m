use super::ProposedInstance;
use crate::perspectives::model_query::types::{ModelShape, ShapeProperty};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::Link;
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// The property a class declares as its dedup identity (its title-like
/// interpretation key) — the first property with `identity == true`. `None`
/// when the SDNA declared no identity, in which case the class is never
/// deduplicated (still interpreted and created, just not deduped).
pub(crate) fn identity_property(shape: &ModelShape) -> Option<&ShapeProperty> {
    shape.properties.iter().find(|p| p.identity)
}

/// Canonicalize an identity value for equality: trim, collapse internal
/// whitespace to single spaces, and lowercase. So "Ship  the MVP " and
/// "ship the mvp" compare equal. Semantic/embedding dedup is a later
/// follow-up; this is deliberately a cheap normalized string match.
pub(crate) fn normalize_identity(s: &str) -> String {
    s.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_lowercase()
}

/// Names of the shape's relation (link-typed) properties. `load_shape` lists
/// every relation both in `properties` (so the query pipeline sees its
/// predicate) *and* in `include_relations`; we key off the latter to recognise
/// them. Relations never travel through the scalar write path: their targets are
/// instance URIs, not literals, so `create_subject`/`update_subject` must not see
/// them in `initial_values` (a setter would literal-encode an instance ref into a
/// bogus `literal:` URI). They are resolved separately into
/// [`InterpretationOp::AddLinks`].
pub(crate) fn relation_names(shape: &ModelShape) -> HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.name.as_str())
        .collect()
}

/// Predicates of the shape's relation properties — the `properties`-side view of
/// [`relation_names`], used where a shape property must be recognised as a
/// relation by predicate (prompt field rendering, hint join).
pub(crate) fn relation_predicates(shape: &ModelShape) -> HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.predicate.as_str())
        .collect()
}

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, identity-value) already exists in the graph, compared under
/// [`normalize_identity`]. This is the hard guarantee behind the soft
/// `existing` hint in [`build_interpretation_input`] — even if the model
/// re-proposes a known item, it never becomes a new instance.
///
/// `existing` maps a class's local name to the already-present identity
/// values. `identity_props` maps a class's local name to the NAME of its
/// declared identity property. An instance whose class has no identity property
/// is always kept (no dedup); likewise one missing that property's value.
///
/// Filters **in place**: the surviving instances keep the LLM's emission order,
/// which is what the `new:<Class>:<n>` relation ordinals in
/// [`plan_interpretation_ops_with_context`] resolve against.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
    identity_props: &HashMap<String, String>,
) -> Vec<ProposedInstance> {
    let known: HashMap<&String, HashSet<String>> = existing
        .iter()
        .map(|(class, values)| {
            (
                class,
                values.iter().map(|v| normalize_identity(v)).collect(),
            )
        })
        .collect();
    instances
        .into_iter()
        .filter(|inst| {
            // An instance carrying an `id` is an explicit upsert target — it
            // names a specific existing node, so its identity value *should*
            // match one already present. Never dedup it away.
            if inst.id.is_some() {
                return true;
            }
            // No declared identity for this class ⇒ no dedup.
            let Some(idp_name) = identity_props.get(&inst.class) else {
                return true;
            };
            let Some(value) = inst.props.get(idp_name).and_then(|v| v.as_str()) else {
                return true; // no identity value to compare on — keep it
            };
            let normalized = normalize_identity(value);
            let already = known
                .get(&inst.class)
                .map(|set| set.contains(&normalized))
                .unwrap_or(false);
            if already {
                log::debug!(
                    "interpretation: dropping already-present {} '{}'",
                    inst.class,
                    value
                );
            }
            !already
        })
        .collect()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

/// The scalar (non-relation) field values a proposed instance wants written —
/// the payload handed to `create_subject` / `update_subject` as `initial_values`.
/// Relation fields are stripped (their values are instance *refs*, resolved by
/// [`resolve_relation_links`] instead), and `null`s are dropped so a missing
/// optional field never becomes a literal-encoded `null`.
///
/// Unknown/extra keys survive: the class's SDNA is the authority on what gets
/// written, and `create_subject` only acts on properties that declare an
/// `ad4m://setter`, so a hallucinated field can never become a link.
pub(crate) fn scalar_values(
    shape: &ModelShape,
    inst: &ProposedInstance,
) -> serde_json::Map<String, serde_json::Value> {
    let rel_names = relation_names(shape);
    inst.props
        .iter()
        .filter(|(k, v)| !rel_names.contains(k.as_str()) && !v.is_null())
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

/// A single write the interpreter wants to make.
///
/// Post-#884 the scalar write path is `create_subject` / `update_subject`, which
/// own literal encoding (each property's `ad4m://setter` + `resolveLanguage`).
/// So `Create` and `Update` carry the *values* to write, not pre-encoded links —
/// they differ only in whether the class constructor runs (minting the type
/// flag). `AddLinks` is the one op that still carries raw links: relation targets
/// are instance URIs, so there is nothing to encode.
#[derive(Debug, Clone, PartialEq)]
pub enum InterpretationOp {
    /// Mint a new instance at `base`: constructor (type flag) + setters.
    Create {
        base: String,
        class: String,
        values: serde_json::Map<String, serde_json::Value>,
    },
    /// Patch the scalar fields of an existing instance, leaving its type flag in
    /// place — this is how the interpreter grows/refines a tree node (Flux
    /// "grouping": continue an existing subgroup vs. start a new one). Same
    /// per-predicate replace semantics as `Create`, minus the constructor.
    Update {
        base: String,
        class: String,
        values: serde_json::Map<String, serde_json::Value>,
    },
    /// Append relation links onto an instance. Purely additive — a relation to a
    /// freshly-minted node grows the graph and must not clear sibling relations
    /// (unlike scalar `Update`, which replaces-per-predicate). Removing a
    /// relation is out of scope (Phase 3 semantic diff).
    AddLinks { source: String, links: Vec<Link> },
}

/// Turn proposed instances into create/update ops with no relation context.
/// Thin wrapper over [`plan_interpretation_ops_with_context`] with an empty
/// existing-id set. Any proposed `id` is therefore treated as hallucinated and
/// routed to `Create` — callers that need to exercise the id-becomes-`Update`
/// path (or resolve relations against the graph's existing instances) must
/// call the `_with_context` form directly with a real
/// `known_existing_ids` set.
pub fn plan_interpretation_ops(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<InterpretationOp> {
    plan_interpretation_ops_with_context(shapes, proposed, base_prefix, &HashSet::new())
}

/// Turn proposed instances into create/update/add-links ops (Phase 2:
/// relation-aware). A proposal with an `id` present in `known_existing_ids`
/// becomes an `Update` on that existing base (scalar fields); a proposal with
/// no `id` — OR with an `id` the graph doesn't recognise (the LLM
/// hallucinated it) — becomes a `Create` under `base_prefix` with a fresh
/// base. Hallucinated-id Updates against non-existent bases produce a silent
/// no-op write, so routing them to Create yields a visible instance instead of
/// data lost between the model and the graph. On top of that, forward relation
/// fields are resolved into real `Link`s and emitted as an additive `AddLinks`
/// op on the proposal's base. Unknown-class proposals are dropped.
///
/// Two passes, because a relation ref can point *forward* to a sibling minted
/// later in the same response:
///   1. Place every proposal — mint or reuse its base — and index bases per
///      class **in the LLM's output order**, so `new:<Class>:<n>` ordinals
///      resolve deterministically. `known_existing_ids` seeds the set of valid
///      existing-id relation targets (what the model was shown in `existing`).
///   2. Resolve each proposal's relation refs against the full index and emit
///      links. Unresolvable refs (typo'd id, out-of-range ordinal) are dropped
///      with a `log::warn!` — the node still lands, just without that edge.
///
/// `proposed` **must be in the LLM's emission order** for the ordinals to line
/// up; `run_interpretation` guarantees this by dedup-filtering in place rather
/// than re-partitioning.
pub fn plan_interpretation_ops_with_context(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
    known_existing_ids: &HashSet<String>,
) -> Vec<InterpretationOp> {
    struct Placed<'a> {
        shape: &'a ModelShape,
        inst: &'a ProposedInstance,
        base: String,
        is_update: bool,
    }

    // Pass 1: place proposals + build the per-class ordinal index.
    let mut per_class: HashMap<String, Vec<String>> = HashMap::new();
    let mut existing_ids: HashSet<String> = known_existing_ids.clone();
    let mut placed: Vec<Placed> = Vec::with_capacity(proposed.len());
    for inst in proposed {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "interpretation: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let (base, is_update) = match &inst.id {
            Some(existing) if known_existing_ids.contains(existing) => (existing.clone(), true),
            _ => {
                if let Some(hallucinated) = &inst.id {
                    log::debug!(
                        "interpretation: proposed id {hallucinated:?} not in known_existing_ids for class {}; routing to Create",
                        inst.class
                    );
                }
                (
                    format!(
                        "{base_prefix}{}/{}",
                        inst.class.to_lowercase(),
                        Uuid::new_v4()
                    ),
                    false,
                )
            }
        };
        // Index under the class name the LLM uses (matches the relation's
        // `targetClass`, i.e. the bare local name), in output order.
        per_class
            .entry(inst.class.clone())
            .or_default()
            .push(base.clone());
        if is_update {
            existing_ids.insert(base.clone());
        }
        placed.push(Placed {
            shape,
            inst,
            base,
            is_update,
        });
    }

    // Pass 2: build ops, resolving relations against the full index.
    let mut out = Vec::with_capacity(placed.len());
    for p in &placed {
        let values = scalar_values(p.shape, p.inst);
        if p.is_update {
            if !values.is_empty() {
                out.push(InterpretationOp::Update {
                    base: p.base.clone(),
                    class: p.inst.class.clone(),
                    values,
                });
            }
        } else {
            out.push(InterpretationOp::Create {
                base: p.base.clone(),
                class: p.inst.class.clone(),
                values,
            });
        }
        let rel_links = resolve_relation_links(p.shape, p.inst, &p.base, &per_class, &existing_ids);
        if !rel_links.is_empty() {
            out.push(InterpretationOp::AddLinks {
                source: p.base.clone(),
                links: rel_links,
            });
        }
    }
    out
}

/// Resolve a proposed instance's forward relation fields into perspective
/// links. Each relation field's value is a ref (or array of refs), each of the
/// two forms taught in the system prompt: an existing instance's `id`, or
/// `new:<Class>:<n>` (1-based ordinal into that class's output-order bases).
/// Reverse-direction relations are skipped (they need the inverse predicate on
/// the target class — Phase 3). Single-cardinality relations keep only the
/// first resolved ref. Unresolved/malformed refs are dropped with `log::warn!`.
fn resolve_relation_links(
    shape: &ModelShape,
    inst: &ProposedInstance,
    source_base: &str,
    per_class: &HashMap<String, Vec<String>>,
    existing_ids: &HashSet<String>,
) -> Vec<Link> {
    let mut links = Vec::new();
    for rel in &shape.include_relations {
        if rel.direction != "forward" {
            // Reverse relations (belongsTo*) store the edge on the other class;
            // writing one requires the inverse predicate. Deferred to Phase 3.
            continue;
        }
        let Some(value) = inst.props.get(&rel.name) else {
            continue;
        };
        let Some(raw_refs) = normalize_refs(value) else {
            log::warn!(
                "interpretation: relation '{}' on '{}' had a non-string ref value; skipping",
                rel.name,
                inst.class
            );
            continue;
        };
        let single =
            matches!(rel.kind.as_str(), "hasOne" | "belongsToOne") || rel.max_count == Some(1);
        let mut emitted = 0usize;
        for raw in raw_refs {
            match resolve_ref(&raw, per_class, existing_ids) {
                Some(target) => {
                    if single && emitted >= 1 {
                        log::warn!(
                            "interpretation: single-cardinality relation '{}' on '{}' got extra ref '{}'; ignoring",
                            rel.name,
                            inst.class,
                            raw
                        );
                        continue;
                    }
                    links.push(Link {
                        source: source_base.to_string(),
                        predicate: Some(rel.predicate.clone()),
                        target,
                    });
                    emitted += 1;
                }
                None => {
                    log::warn!(
                        "interpretation: dropping unresolved relation ref '{}' on '{}.{}'",
                        raw,
                        inst.class,
                        rel.name
                    );
                }
            }
        }
    }
    links
}

/// Normalise a relation field value into a list of raw ref strings. A single
/// string becomes a 1-element vec; an array of strings passes through; anything
/// else (number, object, array with a non-string element) yields `None` so the
/// caller can warn and skip.
fn normalize_refs(value: &serde_json::Value) -> Option<Vec<String>> {
    match value {
        serde_json::Value::String(s) => Some(vec![s.clone()]),
        serde_json::Value::Array(arr) => arr
            .iter()
            .map(|v| v.as_str().map(|s| s.to_string()))
            .collect(),
        _ => None,
    }
}

/// Resolve one raw relation ref to a target base URI. `new:<Class>:<n>` looks up
/// the (n-1)th base of `<Class>` in `per_class`; anything else is treated as an
/// existing-id ref, accepted only if present in `existing_ids` (what the LLM was
/// shown). Returns `None` for out-of-range ordinals, unknown classes, unparsable
/// `<n>`, or invented ids.
fn resolve_ref(
    raw: &str,
    per_class: &HashMap<String, Vec<String>>,
    existing_ids: &HashSet<String>,
) -> Option<String> {
    if let Some(rest) = raw.strip_prefix("new:") {
        // `<Class>:<n>` — split on the LAST colon so class names are free to
        // contain none (they never contain one in practice, but be safe).
        let (class, n_str) = rest.rsplit_once(':')?;
        let n: usize = n_str.trim().parse().ok()?;
        if n == 0 {
            return None;
        }
        per_class.get(class)?.get(n - 1).cloned()
    } else if existing_ids.contains(raw) {
        Some(raw.to_string())
    } else {
        None
    }
}

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_interpretation`] and skip this helper.
pub async fn gather_transcript(
    perspective: &PerspectiveInstance,
    source: &str,
    message_predicate: &str,
) -> anyhow::Result<Vec<(String, String)>> {
    use crate::types::LinkQuery;
    let query = LinkQuery {
        source: Some(source.to_string()),
        predicate: Some(message_predicate.to_string()),
        ..Default::default()
    };
    let links = perspective
        .get_links(&query)
        .await
        .map_err(|e| anyhow::anyhow!("gather_transcript: get_links failed: {e:#}"))?;
    let mut out = Vec::with_capacity(links.len());
    for l in links {
        if let Some(body) = decode_literal_string(&l.data.target) {
            out.push((l.author, body));
        }
    }
    Ok(out)
}

/// Decode a `literal:` URI to a plain `String`, but only when it holds a
/// string value. Reuses `ad4m-client`'s `Literal` (the canonical
/// parse/serialize type); non-string literals (number/bool/json) and
/// non-literal URIs yield `None`.
pub(crate) fn decode_literal_string(uri: &str) -> Option<String> {
    use ad4m_client::literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        _ => None,
    }
}

/// One existing instance the interpreter should know about — the LLM sees these
/// so it can decide whether an interpreted item is a genuinely new node (no `id`
/// on the output) or the continuation/refinement of an existing one (emit this
/// entry's `id` to trigger the upsert path in
/// [`plan_interpretation_ops_with_context`]).
///
/// `class` is redundant with the enclosing map key, but kept on each row so the
/// JSON entry rendered into the prompt is self-contained and unambiguous when
/// the LLM scans a mixed-class list.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceContext {
    /// Base URI of the existing instance — what the LLM emits as `id` to update.
    pub id: String,
    /// The class's declared `identity` value (usually `title`), decoded. Raw,
    /// not normalized: the prompt shows it to the LLM verbatim, and
    /// [`filter_already_present`] normalizes both sides when comparing.
    pub title: String,
    /// Local class name (e.g. "Task"), matching the map key of the returned map.
    pub class: String,
}

/// read the instances already present in the perspective for each target class,
/// keyed by the class's local name. Each row carries the instance's `id` (base
/// URI) alongside its declared identity value.
///
/// Serves both halves of the interpretation contract:
///   * the prompt ([`build_interpretation_input`]) shows `id` + identity so the
///     model can steer away from re-proposing known items *and* emit an `id` to
///     upsert one, or reference it as a relation target;
///   * the identity values ([`identities_from_context`]) feed the deterministic
///     dedup safety net ([`filter_already_present`]), and the ids
///     ([`ids_from_context`]) bound which existing-id relation refs are accepted.
///
/// The dedup/display key is whichever property the class declares as its
/// `identity` (via [`identity_property`]), not a hard-coded `title`. Classes
/// with no identity property are skipped entirely — no identity ⇒ no dedup, and
/// nothing meaningful to show the model.
///
/// Instances are read through the model-query API (`PerspectiveInstance::
/// model_query`) — the symmetric counterpart to writing them via
/// `create_subject` — so class conformance and field decoding go through the
/// class's own shape/getters rather than hand-matched type-flag links. A
/// per-class query failure (e.g. the class isn't registered in this
/// perspective) is treated as "no existing instances" — dedup is a soft hint,
/// guaranteed deterministically downstream by [`filter_already_present`].
pub async fn existing_instance_context(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<InstanceContext>>> {
    let mut out: HashMap<String, Vec<InstanceContext>> = HashMap::new();
    for shape in shapes {
        // No declared identity property ⇒ no dedup key ⇒ skip.
        let Some(idp) = identity_property(shape) else {
            continue;
        };
        let idp_name = idp.name.clone();
        let class = class_local_name(&shape.target_class);

        let query = format!(r#"{{"properties":["{idp_name}"]}}"#);
        let result_json = match perspective.model_query(class, &query).await {
            Ok(json) => json,
            Err(e) => {
                log::warn!("existing_instance_context: model_query({class}) failed, treating as no existing instances: {e:#}");
                continue;
            }
        };
        let result: serde_json::Value = serde_json::from_str(&result_json).map_err(|e| {
            anyhow::anyhow!("existing_instance_context: bad model_query result for {class}: {e:#}")
        })?;

        let rows: Vec<InstanceContext> = result
            .get("instances")
            .and_then(|v| v.as_array())
            .map(|rows| {
                rows.iter()
                    .filter_map(|inst| {
                        // `id` is the instance's base URI, injected by the
                        // model-query hydration for every row.
                        let id = inst.get("id").and_then(|v| v.as_str())?;
                        let title = inst.get(&idp_name).and_then(|v| v.as_str())?;
                        Some(InstanceContext {
                            id: id.to_string(),
                            title: title.to_string(),
                            class: class.to_string(),
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        if !rows.is_empty() {
            out.insert(class.to_string(), rows);
        }
    }
    Ok(out)
}

/// The identity-value-only view of an [`existing_instance_context`] snapshot,
/// for [`filter_already_present`] (which compares identity values, not ids).
pub fn identities_from_context(
    ctx: &HashMap<String, Vec<InstanceContext>>,
) -> HashMap<String, Vec<String>> {
    ctx.iter()
        .map(|(class, rows)| {
            (
                class.clone(),
                rows.iter().map(|r| r.title.clone()).collect(),
            )
        })
        .collect()
}

/// The set of all existing instance `id`s across a context snapshot — the valid
/// targets for an existing-id relation ref (exactly what the LLM was shown in
/// each class's `existing` list). Feeds
/// [`plan_interpretation_ops_with_context`].
pub fn ids_from_context(ctx: &HashMap<String, Vec<InstanceContext>>) -> HashSet<String> {
    ctx.values()
        .flat_map(|rows| rows.iter().map(|r| r.id.clone()))
        .collect()
}
