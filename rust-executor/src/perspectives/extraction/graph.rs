use super::{parse_extraction_response, ProposedInstance};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::Link;
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

/// Deterministic dedup safety-net (pure): drop proposed instances whose
/// (class, title) already exists in the graph, case-insensitively. This is the
/// hard guarantee behind the soft `existing` hint in [`build_extraction_input`]
/// — even if the model re-proposes a known item, it never becomes a link.
///
/// `existing` maps a class's local name to the titles already present. Only the
/// `title` field is compared (the human-facing identity of an SoA node);
/// instances without a `title` are always kept.
pub fn filter_already_present(
    instances: Vec<ProposedInstance>,
    existing: &HashMap<String, Vec<String>>,
) -> Vec<ProposedInstance> {
    let known: HashMap<&String, std::collections::HashSet<String>> = existing
        .iter()
        .map(|(class, titles)| (class, titles.iter().map(|t| t.to_lowercase()).collect()))
        .collect();
    instances
        .into_iter()
        .filter(|inst| {
            // An instance carrying an `id` is an explicit upsert target — it
            // names a specific existing node, so its title *should* match one
            // already present. Never title-dedup it, and (crucially) keep it in
            // place: callers rely on this to preserve the LLM's output order,
            // which is what `new:<Class>:<n>` relation ordinals resolve against.
            if inst.id.is_some() {
                return true;
            }
            let Some(title) = inst.props.get("title").and_then(|v| v.as_str()) else {
                return true; // no title to compare on — keep it
            };
            let already = known
                .get(&inst.class)
                .map(|set| set.contains(&title.to_lowercase()))
                .unwrap_or(false);
            if already {
                log::debug!(
                    "extraction: dropping already-present {} '{}'",
                    inst.class,
                    title
                );
            }
            !already
        })
        .collect()
}

/// Predicates of the shape's relation (link-typed) properties. `load_shape`
/// lists every relation both in `properties` (so the query pipeline sees its
/// predicate) *and* in `include_relations`; we key off the latter to recognise
/// them. Relations are excluded from generic scalar extraction: their targets
/// are instance URIs, not literals, so we neither offer them to the LLM nor
/// write LLM-proposed values through `value_to_literal_uri` (which would encode
/// e.g. an array as a bogus `literal:json:` URI). Relation extraction is a
/// later PR.
pub(crate) fn relation_predicates(shape: &ModelShape) -> std::collections::HashSet<&str> {
    shape
        .include_relations
        .iter()
        .map(|r| r.predicate.as_str())
        .collect()
}

/// Local class name from a class URI: `ns://Intention` -> `Intention`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

/// turn a `ProposedInstance` (parsed LLM output) into perspective links
/// anchored at `base`. Pure — no store, no LLM. Emits, in shape order:
///   1. one link per type-flag property (predicate = flag path, target = the
///      flag's constant `initial_value`), so downstream queries recognise the
///      class;
///   2. one link per non-flag shape property that appears in `inst.props`
///      (predicate = property path, target = literal-encoded value).
///
/// Unknown/extra fields in `inst.props` are dropped — the LLM cannot inject
/// links outside the declared class shape.
pub fn instance_links(shape: &ModelShape, inst: &ProposedInstance, base: &str) -> Vec<Link> {
    let mut out = Vec::new();
    // 1. type-flag links (constant value marking the class).
    for prop in &shape.properties {
        if prop.is_flag {
            if let Some(target) = prop.initial_value.as_ref() {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target: target.clone(),
                });
            }
        }
    }
    // 2. scalar field links (the LLM-filled values).
    out.extend(scalar_property_links(shape, inst, base));
    out
}

/// Just the scalar (non-flag, non-relation) property links the instance fills —
/// the mutable part of a node. Used both by [`instance_links`] (create: flags +
/// scalars) and by updates (patch scalars, leave the type flag in place).
pub fn scalar_property_links(shape: &ModelShape, inst: &ProposedInstance, base: &str) -> Vec<Link> {
    let rel_preds = relation_predicates(shape);
    let mut out = Vec::new();
    for prop in &shape.properties {
        if prop.is_flag {
            continue;
        }
        // Relation targets are instance URIs, not literals — never literal-encode.
        if rel_preds.contains(prop.predicate.as_str()) {
            continue;
        }
        if let Some(value) = inst.props.get(&prop.name) {
            if let Some(target) = value_to_literal_uri(value) {
                out.push(Link {
                    source: base.to_string(),
                    predicate: Some(prop.predicate.clone()),
                    target,
                });
            }
        }
    }
    out
}

/// Encode a JSON scalar into an AD4M `literal:` URI by delegating to the
/// canonical [`crate::languages::literal::literal_encode`] — the same encoder
/// the literal Language uses and that `model_query`'s `parse_literal_value`
/// round-trips against — and prefixing the `literal:` scheme it omits. `null`
/// is skipped so a missing optional field never becomes a `literal:json:null`
/// link.
fn value_to_literal_uri(value: &serde_json::Value) -> Option<String> {
    if value.is_null() {
        return None;
    }
    Some(format!(
        "literal:{}",
        crate::languages::literal::literal_encode(value)
    ))
}

/// parse a raw LLM response and turn it into the set of links that
/// would be written into the perspective. Callers minted a fresh instance base
/// URI per proposed instance under `base_prefix` and delegate to
/// [`instance_links`] for the actual shape-driven link construction.
///
/// The lookup from `inst.class` to a `ModelShape` is by local class name
/// (final segment of `target_class`). Proposed instances whose class doesn't
/// match any provided shape are silently dropped — the LLM cannot inject
/// links outside the caller's declared shape set.
///
/// Returned tuples pair each fresh base URI with the links anchored on it, so
/// the caller ([`run_extraction`] or a test) can decide how to persist them.
pub fn apply_extraction_raw(
    shapes: &[ModelShape],
    raw: &str,
    base_prefix: &str,
) -> anyhow::Result<Vec<(String, Vec<Link>)>> {
    let proposed = parse_extraction_response(raw)?;
    Ok(place_instances(shapes, &proposed, base_prefix))
}

/// Core of [`apply_extraction_raw`], factored out so [`run_extraction`] can
/// reuse it without a redundant JSON round-trip. Same semantics: unknown-class
/// instances are dropped; every kept instance gets a fresh UUID-tagged base.
pub fn place_instances(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<(String, Vec<Link>)> {
    let mut out = Vec::with_capacity(proposed.len());
    for inst in proposed {
        let Some(shape) = shapes
            .iter()
            .find(|s| class_local_name(&s.target_class) == inst.class)
        else {
            log::debug!(
                "extraction: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let base = format!(
            "{base_prefix}{}/{}",
            inst.class.to_lowercase(),
            Uuid::new_v4()
        );
        let links = instance_links(shape, inst, &base);
        out.push((base, links));
    }
    out
}

/// A single write the extractor wants to make. `Create` mints a new instance;
/// `Update` patches the scalar fields of an existing one (its `id`), leaving the
/// type flag in place — this is how the extractor grows/refines a tree node
/// (Flux "grouping": continue an existing subgroup vs. start a new one).
#[derive(Debug, Clone, PartialEq)]
pub enum ExtractionOp {
    Create {
        base: String,
        links: Vec<Link>,
    },
    Update {
        base: String,
        set: Vec<Link>,
    },
    /// Append relation links onto an existing (Update-target) instance. Purely
    /// additive — a relation from an existing node to a freshly-minted one grows
    /// the graph and must not clear sibling relations (unlike scalar `Update`,
    /// which replaces-per-predicate). Removing a relation is out of scope
    /// (Phase 3 semantic diff).
    AddLinks {
        base: String,
        links: Vec<Link>,
    },
}

/// Turn proposed instances into create/update ops with no relation context.
/// Thin wrapper over [`plan_extraction_ops_with_context`] with an empty
/// existing-id set — kept for callers/tests that don't need relations resolved
/// against the graph's existing instances.
pub fn plan_extraction_ops(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<ExtractionOp> {
    plan_extraction_ops_with_context(shapes, proposed, base_prefix, &HashSet::new())
}

/// Turn proposed instances into create/update/add-links ops (Phase 2:
/// relation-aware). A proposal with an `id` becomes an `Update` on that existing
/// base (scalar fields); otherwise a `Create` under `base_prefix`. On top of
/// that, forward relation fields are resolved into real `Link`s and either
/// folded into a `Create`'s links or emitted as an additive `AddLinks` on an
/// Update target. Unknown-class proposals are dropped.
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
/// up; `run_extraction` guarantees this by dedup-filtering in place rather than
/// re-partitioning.
pub fn plan_extraction_ops_with_context(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
    known_existing_ids: &HashSet<String>,
) -> Vec<ExtractionOp> {
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
                "extraction: dropping proposed instance for unknown class '{}'",
                inst.class
            );
            continue;
        };
        let (base, is_update) = match &inst.id {
            Some(existing) => (existing.clone(), true),
            None => (
                format!(
                    "{base_prefix}{}/{}",
                    inst.class.to_lowercase(),
                    Uuid::new_v4()
                ),
                false,
            ),
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
        let rel_links = resolve_relation_links(p.shape, p.inst, &p.base, &per_class, &existing_ids);
        if p.is_update {
            let set = scalar_property_links(p.shape, p.inst, &p.base);
            if !set.is_empty() {
                out.push(ExtractionOp::Update {
                    base: p.base.clone(),
                    set,
                });
            }
            if !rel_links.is_empty() {
                out.push(ExtractionOp::AddLinks {
                    base: p.base.clone(),
                    links: rel_links,
                });
            }
        } else {
            let mut links = instance_links(p.shape, p.inst, &p.base);
            links.extend(rel_links);
            out.push(ExtractionOp::Create {
                base: p.base.clone(),
                links,
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
                "extraction: relation '{}' on '{}' had a non-string ref value; skipping",
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
                            "extraction: single-cardinality relation '{}' on '{}' got extra ref '{}'; ignoring",
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
                        "extraction: dropping unresolved relation ref '{}' on '{}.{}'",
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

/// The set of all existing instance `id`s across a context snapshot — the valid
/// targets for an existing-id relation ref (exactly what the LLM was shown in
/// each class's `existing` list). Feeds [`plan_extraction_ops_with_context`].
pub fn ids_from_context(ctx: &HashMap<String, Vec<InstanceContext>>) -> HashSet<String> {
    ctx.values()
        .flat_map(|rows| rows.iter().map(|r| r.id.clone()))
        .collect()
}

/// minimal transcript gatherer. Reads links `source ⇒ predicate ⇒ literal`
/// from a perspective where `predicate` matches `message_predicate` and the
/// target is a `literal:string:` URI (i.e., a message body). Returns turns in
/// the order the store returned them. Speaker is the link author.
///
/// Kept intentionally small — flows/channel-aware traversal is deferred to a
/// later PR. Callers that already have a curated `Vec<(speaker, text)>` should
/// pass it straight to [`run_extraction`] and skip this helper.
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

fn decode_literal_string(uri: &str) -> Option<String> {
    use ad4m_client::literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        _ => None,
    }
}

/// One existing instance the extractor should know about — the LLM sees these
/// so it can decide whether an extracted item is a genuinely new node (no `id`
/// on the output) or the continuation/refinement of an existing one (emit this
/// entry's `id` to trigger the upsert path in [`plan_extraction_ops`]).
///
/// `class` is redundant with the enclosing map key, but kept on each row so the
/// JSON entry rendered into the prompt is self-contained and unambiguous when
/// the LLM scans a mixed-class list.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceContext {
    /// Base URI of the existing instance — what the LLM emits as `id` to update.
    pub id: String,
    /// The `title` scalar, decoded (no `literal:string:` wrapper).
    pub title: String,
    /// Local class name (e.g. "Task"), matching the map key of the returned map.
    pub class: String,
}

/// Snapshot the graph's existing instances per class, richer than
/// [`existing_instance_titles`]: each entry carries the instance's `id` (base
/// URI) alongside its title and class. Feeds [`build_extraction_input`] so the
/// LLM can emit an `id` to upsert an existing node instead of creating a
/// duplicate.
///
/// Locates instances the same way as [`existing_instance_titles`] (type-flag
/// link -> title link), so both paths agree on what counts as an "existing"
/// instance. Instances without a decodable `title` literal are skipped.
pub async fn existing_instance_context(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<InstanceContext>>> {
    use crate::types::LinkQuery;
    let mut out: HashMap<String, Vec<InstanceContext>> = HashMap::new();
    for shape in shapes {
        let Some(flag) = shape
            .properties
            .iter()
            .find(|p| p.is_flag && p.initial_value.is_some())
        else {
            continue;
        };
        let Some(title_prop) = shape.properties.iter().find(|p| p.name == "title") else {
            continue;
        };
        let flag_value = flag.initial_value.as_ref().unwrap();
        let class_name = class_local_name(&shape.target_class).to_string();

        let flag_links = perspective
            .get_links(&LinkQuery {
                predicate: Some(flag.predicate.clone()),
                ..Default::default()
            })
            .await
            .map_err(|e| {
                anyhow::anyhow!("existing_instance_context: get_links(flag) failed: {e:#}")
            })?;
        let bases: Vec<String> = flag_links
            .into_iter()
            .filter(|l| &l.data.target == flag_value)
            .map(|l| l.data.source)
            .collect();

        let mut rows = Vec::new();
        for base in bases {
            let title_links = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    predicate: Some(title_prop.predicate.clone()),
                    ..Default::default()
                })
                .await
                .map_err(|e| {
                    anyhow::anyhow!("existing_instance_context: get_links(title) failed: {e:#}")
                })?;
            // Take the first decodable title link — a well-formed instance has
            // exactly one under max_count=1.
            let Some(title) = title_links
                .into_iter()
                .find_map(|tl| decode_literal_string(&tl.data.target))
            else {
                continue;
            };
            rows.push(InstanceContext {
                id: base,
                title,
                class: class_name.clone(),
            });
        }
        if !rows.is_empty() {
            out.insert(class_name, rows);
        }
    }
    Ok(out)
}

/// Derive the title-only view of an [`existing_instance_context`] snapshot for
/// [`filter_already_present`], which only compares titles. Keeps the dedup path
/// working unchanged against the richer context type.
pub fn titles_from_context(
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

/// Legacy title-only view of the graph, kept for callers/tests that only need
/// titles (e.g. dedup snapshots). Prefer [`existing_instance_context`] when
/// building the LLM prompt — it also carries `id`s so the model can upsert.
pub async fn existing_instance_titles(
    perspective: &PerspectiveInstance,
    shapes: &[ModelShape],
) -> anyhow::Result<HashMap<String, Vec<String>>> {
    let ctx = existing_instance_context(perspective, shapes).await?;
    Ok(titles_from_context(&ctx))
}
