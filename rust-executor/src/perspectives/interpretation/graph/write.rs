use super::*;
use crate::perspectives::interpretation::ProposedInstance;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::Link;
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

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
/// [`ExistingInstances`] map.
pub fn plan_interpretation_ops(
    shapes: &[ModelShape],
    proposed: &[ProposedInstance],
    base_prefix: &str,
) -> Vec<InterpretationOp> {
    plan_interpretation_ops_with_context(shapes, proposed, base_prefix, &ExistingInstances::new())
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
    existing: &ExistingInstances,
) -> Vec<InterpretationOp> {
    struct Placed<'a> {
        shape: &'a ModelShape,
        inst: &'a ProposedInstance,
        base: String,
        is_update: bool,
    }

    // Pass 1: place proposals + build the per-class ordinal index.
    let mut per_class: HashMap<String, Vec<String>> = HashMap::new();
    // Ids the graph holds, extended with proposals we route to Update, so
    // relation refs can resolve against real *and* just-planned bases.
    let mut existing_ids: HashSet<String> = existing.keys().cloned().collect();
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
            Some(id) if existing.contains_key(id) => (id.clone(), true),
            _ => {
                if let Some(hallucinated) = &inst.id {
                    log::debug!(
                        "interpretation: proposed id {hallucinated:?} not among existing instances for class {}; routing to Create",
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation::*;
    use crate::perspectives::interpretation_test_support::*;

    #[test]
    fn plan_ops_creates_without_id_and_updates_with_id() {
        // An `id` field marks an upsert: patch the existing node's scalar fields
        // (no fresh base, no re-run constructor). Absence of `id` = a create.
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        let raw = r#"[
      {"class":"Intention","title":"Write the design doc"},
      {"class":"Intention","id":"soa://existing/intention/42","title":"Write the design doc and circulate it","owner":"Nico"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // `id` is parsed into its own field, kept out of `props`.
        assert_eq!(
            proposed[1].id.as_deref(),
            Some("soa://existing/intention/42")
        );
        assert!(!proposed[1].props.contains_key("id"));

        // The upsert only fires when the graph actually holds that id — seed
        // known_existing_ids to mimic what `existing_instance_context` would
        // return in production. Without this the planner treats the id as
        // hallucinated and routes to Create.
        let known = existing_ids(&["soa://existing/intention/42"]);
        let ops = plan_interpretation_ops_with_context(&shapes, &proposed, "soa://ext/", &known);
        assert_eq!(ops.len(), 2);

        match &ops[0] {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                assert!(base.starts_with("soa://ext/intention/"));
                assert_eq!(class, "Intention");
                // The scalar payload is handed to `create_subject`; the type flag
                // comes from the class constructor, never from the planner.
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the design doc")
                );
                assert!(
                    !values.contains_key("type"),
                    "planner must not carry the type flag; got {values:?}"
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
        match &ops[1] {
            InterpretationOp::Update {
                base,
                class,
                values,
            } => {
                assert_eq!(base, "soa://existing/intention/42");
                assert_eq!(class, "Intention");
                // update patches scalar fields on the EXISTING base…
                assert_eq!(
                    values.get("title").and_then(|v| v.as_str()),
                    Some("Write the design doc and circulate it")
                );
                assert_eq!(values.get("owner").and_then(|v| v.as_str()), Some("Nico"));
                // …and never re-writes the type flag.
                assert!(!values.contains_key("type"));
            }
            other => panic!("expected Update, got {other:?}"),
        }
    }

    #[test]
    fn plan_ops_hallucinated_id_routes_to_create() {
        // The LLM sometimes invents an `id` that names no real base — copying the
        // format from the prompt but with a made-up path. Trusting it produces an
        // Update against a base that has no type flag, i.e. a scalar write no
        // reader will ever find. The planner must fall back to Create so the
        // instance lands somewhere visible.
        let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
        let raw = r#"[
      {"class":"Intention","id":"soa://existing/intention/999","title":"Ghost"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // existing intentionally does NOT contain the proposed id.
        let ops = plan_interpretation_ops_with_context(
            &shapes,
            &proposed,
            "soa://ext/",
            &ExistingInstances::new(),
        );
        assert_eq!(ops.len(), 1);
        match &ops[0] {
            InterpretationOp::Create {
                base,
                class,
                values,
            } => {
                assert!(
                    base.starts_with("soa://ext/intention/"),
                    "hallucinated id must be replaced by a fresh minted base, got {base}"
                );
                assert_ne!(
                    base, "soa://existing/intention/999",
                    "planner must not reuse the hallucinated id"
                );
                assert_eq!(class, "Intention");
                assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Ghost"));
            }
            other => panic!("expected Create for hallucinated id, got {other:?}"),
        }
    }

    #[test]
    fn plan_ops_drops_unknown_class() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        let raw = r#"[
      {"class":"Task","title":"Real"},
      {"class":"Hallucinated","title":"Nope"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&shapes, &proposed, "soa://ext/");
        assert_eq!(ops.len(), 1, "unknown-class proposal must be dropped");
        assert!(matches!(ops[0], InterpretationOp::Create { .. }));
    }

    #[test]
    fn plan_ops_empty_input_yields_no_ops() {
        let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
        assert!(plan_interpretation_ops(&shapes, &[], "soa://ext/").is_empty());
    }

    #[test]
    fn relation_properties_are_excluded_from_interpretation() {
        // A shape whose interpretation hint also declares a link-typed relation
        // (`blocks`). load_shape lists that relation in `properties` too, so
        // without the guard it would be offered to the LLM as a scalar field and —
        // if the LLM emits it — literal-encoded by a setter into a bogus target.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        // Sanity: the relation really is present in both lists.
        assert!(
            shape.include_relations.iter().any(|r| r.name == "blocks"),
            "fixture must declare a `blocks` relation"
        );
        assert!(
            shape.properties.iter().any(|p| p.name == "blocks"),
            "load_shape is expected to also list the relation in properties"
        );

        // 1. build_interpretation_input must not offer the relation as a field.
        let input = build_interpretation_input(
            &[shape.clone()],
            &[("Nico".into(), "block it".into())],
            &no_existing(),
        );
        let v: serde_json::Value = serde_json::from_str(&input).unwrap();
        let field_names: Vec<&str> = v["classes"][0]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|f| f["name"].as_str())
            .collect();
        assert!(field_names.contains(&"title"), "scalar field must remain");
        assert!(
            !field_names.contains(&"blocks"),
            "relation must not be offered to the LLM as a field; got {field_names:?}"
        );

        // Forward relations surface in a dedicated `relations` block (so the LLM
        // knows what refs it *can* fill), separate from the scalar `fields`. The
        // `blocks` relation on `TASK_WITH_RELATION_SDNA` is `hasMany` forward, so
        // it belongs there with its target class + hint.
        let relations = v["classes"][0]["relations"].as_array().unwrap();
        let blocks_rel = relations
            .iter()
            .find(|r| r["name"].as_str() == Some("blocks"))
            .expect("hasMany forward relation must appear in `relations` block");
        assert_eq!(
            blocks_rel["targetClass"].as_str(),
            Some("Task"),
            "relation targetClass must reflect ShapeRelation.target_class_name"
        );
        assert_eq!(
            blocks_rel["hint"].as_str(),
            Some("Other tasks this one blocks."),
            "relation hint must reflect the sibling property's interpretationHint"
        );

        // 2. The scalar write payload must not carry the relation either, even
        //    when the LLM emits it (here as an array — exactly the case a setter
        //    would literal-encode into a bogus `literal:json:` target).
        let raw = r#"[{"class":"Task","title":"Do X","blocks":["soa://t2","soa://t3"]}]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");
        let InterpretationOp::Create { values, .. } = &ops[0] else {
            panic!("expected a Create, got {:?}", ops[0]);
        };
        assert!(
            !values.contains_key("blocks"),
            "relation must never reach the scalar write path; got {values:?}"
        );
        // The scalar title still lands.
        assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Do X"));
    }

    #[test]
    fn relations_write_link_to_new_sibling() {
        // Two Tasks minted in one pass; the first `blocks` the second via a
        // `new:Task:2` ref. The relation must resolve to the second Task's freshly
        // minted base and land as a real `ns://blocks` link (not a literal).
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let raw = r#"[
      {"class":"Task","title":"Ship the API","blocks":["new:Task:2"]},
      {"class":"Task","title":"Write the client"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let first_base = nth_create_base(&ops, 0);
        let second_base = nth_create_base(&ops, 1);
        let targets = targets_of(addlinks_for(&ops, &first_base), "ns://blocks");
        assert_eq!(
            targets,
            vec![second_base],
            "blocks ref `new:Task:2` must point at the second Task's base"
        );
        // The target is an instance base, never a literal URI.
        assert!(
            !targets[0].starts_with("literal:"),
            "relation target must be an instance URI, not a literal"
        );
    }

    #[test]
    fn relations_write_link_to_existing_id() {
        // A single new Task blocks an existing one, referenced by its id. Only ids
        // the model was shown (in `known_existing_ids`) are accepted as targets.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let existing_id = "soa://ext/task/already-here".to_string();
        let known = existing_ids(&[existing_id.as_str()]);
        let raw = format!(r#"[{{"class":"Task","title":"New work","blocks":["{existing_id}"]}}]"#,);
        let proposed = parse_interpretation_response(&raw).unwrap();
        let ops = plan_interpretation_ops_with_context(&[shape], &proposed, "soa://ext/", &known);

        let base = nth_create_base(&ops, 0);
        assert_eq!(
            targets_of(addlinks_for(&ops, &base), "ns://blocks"),
            vec![existing_id],
            "existing-id blocks ref must resolve to that id"
        );
    }

    #[test]
    fn relations_drop_unresolved_ref() {
        // An out-of-range ordinal and an invented id are both unresolvable — the
        // node still lands, just with no relation link and no panic.
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let raw = r#"[
      {"class":"Task","title":"Lonely","blocks":["new:Task:99","soa://ext/task/never-shown"]}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        // Empty known-ids: the bare id ref is "invented" from the model's POV.
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let base = nth_create_base(&ops, 0);
        assert!(
            addlinks_for(&ops, &base).is_empty(),
            "unresolved refs must not become links; got {ops:#?}"
        );
        // The node still lands with its scalar — a dropped relation never drops it.
        let InterpretationOp::Create { values, .. } = &ops[0] else {
            panic!("expected a Create, got {:?}", ops[0]);
        };
        assert_eq!(values.get("title").and_then(|v| v.as_str()), Some("Lonely"));
    }

    #[test]
    fn relations_hasone_takes_first_of_array() {
        // A single-cardinality (`hasOne`) relation given an array keeps only the
        // first resolved ref. `parent` is hasOne forward -> Task.
        let sdna = r#"{
      "target_class":"ns://Task",
      "interpretation_hint":"A task.",
      "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://task"}],
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]},
        {"path":"ns://parent","name":"parent","relation_kind":"hasOne","target_class_name":"Task","class":"ns://TaskShape","interpretation_hint":"The parent task."}
      ]
    }"#;
        let shape = shape_from_sdna("Task", sdna);
        // Sanity: the fixture really is single-cardinality forward.
        let parent_rel = shape
            .include_relations
            .iter()
            .find(|r| r.name == "parent")
            .expect("parent relation present");
        assert_eq!(parent_rel.direction, "forward");
        assert!(
            parent_rel.kind == "hasOne" || parent_rel.max_count == Some(1),
            "parent must be single-cardinality"
        );

        let raw = r#"[
      {"class":"Task","title":"Child","parent":["new:Task:2","new:Task:3"]},
      {"class":"Task","title":"First parent"},
      {"class":"Task","title":"Second parent"}
    ]"#;
        let proposed = parse_interpretation_response(raw).unwrap();
        let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

        let child_base = nth_create_base(&ops, 0);
        let first_parent_base = nth_create_base(&ops, 1);
        assert_eq!(
            targets_of(addlinks_for(&ops, &child_base), "ns://parent"),
            vec![first_parent_base],
            "hasOne must keep only the first resolved ref (new:Task:2)"
        );
    }

    #[test]
    fn relations_from_update_target_emit_addlinks() {
        // A relation whose source is an *existing* (upsert) instance must not fold
        // into that instance's scalar Update — it becomes an additive `AddLinks`
        // op (relations to a fresh sibling grow the graph, never replace scalars).
        let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
        let existing_id = "soa://ext/task/existing".to_string();
        let raw = format!(
            r#"[
          {{"class":"Task","id":"{existing_id}","title":"Renamed","blocks":["new:Task:2"]}},
          {{"class":"Task","title":"Fresh sibling"}}
        ]"#,
        );
        let proposed = parse_interpretation_response(&raw).unwrap();
        let known = existing_ids(&[existing_id.as_str()]);
        let ops = plan_interpretation_ops_with_context(&[shape], &proposed, "soa://ext/", &known);

        // The Update carries the retitled scalar, no relation value.
        let update = ops
            .iter()
            .find_map(|op| match op {
                InterpretationOp::Update { base, values, .. } if base == &existing_id => {
                    Some(values)
                }
                _ => None,
            })
            .expect("expected an Update on the existing id");
        assert!(
            !update.contains_key("blocks"),
            "scalar Update must not carry the relation; got {update:?}"
        );
        assert_eq!(
            update.get("title").and_then(|v| v.as_str()),
            Some("Renamed")
        );

        // The relation lands as an additive AddLinks on the same base, pointing at
        // the fresh sibling.
        let sibling_base = nth_create_base(&ops, 0);
        assert_eq!(
            targets_of(addlinks_for(&ops, &existing_id), "ns://blocks"),
            vec![sibling_base],
            "AddLinks must point the blocks relation at the fresh sibling's base"
        );
    }

    #[tokio::test]
    async fn apply_ops_upsert_replaces_scalar_without_touching_type_flag() {
        // End-to-end (no LLM): seed a real perspective with an Intention whose
        // title/owner are already set, then apply an Update op that patches the
        // scalar fields on that same base. The old scalar values must be GONE (the
        // setters are `setSingleTarget` = replace-per-predicate) and the new ones
        // present; the type flag stays untouched.
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
        let base = "soa://existing/intention/upsert-target";

        // Seed: original title + owner on the target instance.
        seed_instance(&mut perspective, &ctx, &shapes[0], base, "Draft the design").await;
        apply_one(
            &mut perspective,
            &shapes,
            &ctx,
            proposal(
                "Intention",
                Some(base),
                &[("owner", serde_json::json!("Nico"))],
            ),
        )
        .await;
        assert_eq!(
            decoded_targets(&perspective, base, "ns://owner").await,
            vec![serde_json::json!("Nico")],
            "sanity: the seeding update wrote the owner"
        );

        // Now upsert: same base, revised title + new owner.
        let ops = apply_one(
            &mut perspective,
            &shapes,
            &ctx,
            proposal(
                "Intention",
                Some(base),
                &[
                    (
                        "title",
                        serde_json::json!("Draft the design and circulate it"),
                    ),
                    ("owner", serde_json::json!("Josh")),
                ],
            ),
        )
        .await;
        assert!(matches!(ops[0], InterpretationOp::Update { .. }));

        // Type flag survives (the constructor wrote it; updates never touch it).
        let type_links = decoded_targets(&perspective, base, "ns://type").await;
        assert_eq!(
            type_links,
            vec![serde_json::json!("ns://intention")],
            "type flag must remain exactly once"
        );
        // Title: exactly the new value, no residue of the old one.
        assert_eq!(
            decoded_targets(&perspective, base, "ns://title").await,
            vec![serde_json::json!("Draft the design and circulate it")],
            "title must have been replaced (no old-value residue)"
        );
        // Owner: exactly the new value ("Nico" gone).
        assert_eq!(
            decoded_targets(&perspective, base, "ns://owner").await,
            vec![serde_json::json!("Josh")],
            "owner must have been replaced"
        );
    }

    #[tokio::test]
    async fn apply_ops_addlinks_are_additive() {
        // Two `blocks` edges on the same base must coexist: relations are appended,
        // never replaced-per-predicate the way scalars are.
        use crate::types::LinkQuery;
        let (mut perspective, shapes, ctx) =
            setup_perspective_no_llm(&[("Task", TASK_WITH_RELATION_SDNA)]).await;
        let base = "soa://existing/task/hub";
        seed_instance(&mut perspective, &ctx, &shapes[0], base, "Hub task").await;

        for target in ["soa://ext/task/a", "soa://ext/task/b"] {
            apply_interpretation_ops(
                &mut perspective,
                &[InterpretationOp::AddLinks {
                    source: base.to_string(),
                    links: vec![Link {
                        source: base.to_string(),
                        predicate: Some("ns://blocks".to_string()),
                        target: target.to_string(),
                    }],
                }],
                &ctx,
            )
            .await
            .expect("apply AddLinks");
        }

        let links = perspective
            .get_links(&LinkQuery {
                source: Some(base.to_string()),
                predicate: Some("ns://blocks".to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links");
        let mut targets: Vec<String> = links.iter().map(|l| l.data.target.clone()).collect();
        targets.sort();
        assert_eq!(
            targets,
            vec![
                "soa://ext/task/a".to_string(),
                "soa://ext/task/b".to_string()
            ],
            "AddLinks must accumulate, not replace"
        );
    }
}
