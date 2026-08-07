use super::*;
use crate::db::Ad4mDb;
use crate::perspectives::interpretation_test_support::*;
use crate::types::{AITask, Link};
use std::collections::HashMap;

/// An empty existing-instance context, typed — `build_interpretation_input`
/// takes the richer `InstanceContext` map now, so a bare `HashMap::new()` can't
/// be inferred.
fn no_existing() -> HashMap<String, Vec<InstanceContext>> {
    HashMap::new()
}

#[test]
fn interpretation_hint_lands_in_prompt() {
    let shapes = vec![
        shape_from_sdna("Belief", BELIEF_SDNA),
        shape_from_sdna("Intention", INTENTION_SDNA),
    ];
    let input = build_interpretation_input(
        &shapes,
        &[(
            "Nico".into(),
            "I'll extract the LLM processing into ADAM".into(),
        )],
        &no_existing(),
    );

    // class-level hints reach the prompt
    assert!(input.contains("A claim a participant holds to be true"));
    assert!(input.contains("A first-person commitment to do something"));
    // per-field hint + required flag
    assert!(input.contains("Imperative summary of the work"));
    assert!(input.contains("\"required\":true"));
    // transcript included
    assert!(input.contains("Nico") && input.contains("extract the LLM processing"));

    // valid JSON, two classes, type-flag excluded from fields
    let v: serde_json::Value = serde_json::from_str(&input).unwrap();
    assert_eq!(v["classes"].as_array().unwrap().len(), 2);
    let intention = v["classes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "Intention")
        .expect("Intention class in prompt");
    let field_names: Vec<&str> = intention["fields"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|f| f["name"].as_str())
        .collect();
    assert!(field_names.contains(&"title") && field_names.contains(&"owner"));
    assert!(
        !field_names.contains(&"type"),
        "type flag must not be a field"
    );
    assert!(
        intention["relations"].as_array().unwrap().is_empty(),
        "relation-free shape must render an empty relations block"
    );
}

#[test]
fn existing_context_renders_id_title_class_in_prompt() {
    // With the richer `existing_instance_context` snapshot in play,
    // build_interpretation_input must render each existing entry as an object
    // carrying `id`, `title`, and `class` — that's the handle the LLM needs to
    // emit an upsert instead of a duplicate create. Also proves the system
    // prompt still describes the `id` upsert path.
    let shapes = vec![shape_from_sdna("Task", TASK_SDNA)];
    let mut existing: HashMap<String, Vec<InstanceContext>> = HashMap::new();
    existing.insert(
        "Task".to_string(),
        vec![InstanceContext {
            id: "soa://existing/task/42".to_string(),
            title: "Draft the design doc".to_string(),
            class: "Task".to_string(),
        }],
    );
    let input = build_interpretation_input(
        &shapes,
        &[("Nico".into(), "About that design doc…".into())],
        &existing,
    );
    let v: serde_json::Value = serde_json::from_str(&input).unwrap();
    let task_class = v["classes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "Task")
        .expect("Task class in prompt");
    let existing_arr = task_class["existing"]
        .as_array()
        .expect("existing must be an array");
    assert_eq!(existing_arr.len(), 1);
    assert_eq!(existing_arr[0]["id"], "soa://existing/task/42");
    assert_eq!(existing_arr[0]["title"], "Draft the design doc");
    assert_eq!(existing_arr[0]["class"], "Task");
    // System prompt still teaches the id-upsert semantics — regression guard
    // against silently dropping that instruction while refactoring the schema.
    assert!(
        INTERPRETATION_SYSTEM_PROMPT.contains("id"),
        "system prompt must document the `id` upsert semantics"
    );
}

#[test]
fn system_prompt_documents_relation_ref_syntax() {
    // The system prompt must teach the LLM both the shape of the per-class
    // `relations` block AND the two allowed reference forms it can put into a
    // relation value. Without this instruction the LLM only sees an unfamiliar
    // array in the input schema — the planner can only resolve refs it was told
    // to emit.
    let p = INTERPRETATION_SYSTEM_PROMPT;
    assert!(
        p.contains("`relations`"),
        "system prompt must introduce the relations block on each class"
    );
    assert!(
        p.contains("instance-reference") || p.contains("instance reference"),
        "system prompt must frame a relation value as an instance reference"
    );
    // The `new:<TargetClass>:<n>` placeholder is the only way to link two
    // freshly-minted siblings in the same response; a missing description
    // would silently downgrade sibling-linking to unresolved refs at plan
    // time. Assert both the literal token and the 1-based ordinal wording.
    assert!(
        p.contains("new:<TargetClass>:<n>"),
        "system prompt must document the `new:<TargetClass>:<n>` ref form"
    );
    assert!(
        p.contains("1-based"),
        "system prompt must state the ordinal is 1-based"
    );
    // Existing-id path must still be described alongside the new-ref path so
    // the LLM picks the right form per case (upsert-target vs sibling-mint).
    assert!(
        p.contains("existing") && p.contains("`id`") && p.contains("existing entry's `id`"),
        "system prompt must document linking via an existing entry's id"
    );
    // Guardrail wording: unresolved refs and fabricated ids are the two
    // failure modes; without explicit prohibitions small LLMs invent both.
    assert!(
        p.contains("Never invent an `id`"),
        "system prompt must forbid fabricated ids"
    );
    assert!(
        p.contains("never emit a") || p.contains("Never emit a"),
        "system prompt must forbid unresolved `new:` refs"
    );
}

#[test]
fn relations_few_shot_example_is_present_and_upsert_is_last() {
    // The few-shot set must include a dedicated relations example demonstrating
    // the `new:<Class>:<n>` ref syntax the system prompt teaches. It need NOT be
    // last: empirically, putting the relations example (all-new-instances) in
    // the recency slot made gemma3:12b create-happy and regressed the id-upsert
    // behavior (`e2e_updates_existing_instance_via_id` 0/5). So the *upsert*
    // example owns the last slot; relations sit earlier and still fire reliably.
    let examples = interpretation_examples();
    assert_eq!(examples.len(), 4, "expected exactly four few-shot examples");

    // Find the relations example wherever it sits: the one whose output uses
    // the `new:<Class>:<n>` ref syntax on both endpoints of a
    // SemanticRelationship.
    let relations_ex = examples
        .iter()
        .find(|e| e.output.contains("\"new:Topic:1\"") && e.output.contains("\"new:Message:1\""))
        .expect(
            "a relations few-shot example demonstrating `new:<Class>:<n>` refs must be present",
        );
    // Its input must render the `relations` block so the LLM sees the schema
    // half of the ref-syntax lesson.
    let v: serde_json::Value = serde_json::from_str(&relations_ex.input).unwrap();
    let rel_class = v["classes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "SemanticRelationship")
        .expect("relations few-shot must declare a SemanticRelationship class");
    let rels = rel_class["relations"]
        .as_array()
        .expect("SemanticRelationship must render a relations block");
    let rel_names: Vec<&str> = rels.iter().filter_map(|r| r["name"].as_str()).collect();
    assert!(
        rel_names.contains(&"tag") && rel_names.contains(&"expression"),
        "relations example must declare both `tag` and `expression` relations; got {rel_names:?}"
    );

    // The LAST example must be the upsert one — it carries an `existing` entry
    // with an `id` and re-emits that `id` in its output (the fragile behavior
    // that needs the recency slot). Guard against a future reshuffle silently
    // regressing upsert recall again.
    let last = examples.last().unwrap();
    let lv: serde_json::Value = serde_json::from_str(&last.input).unwrap();
    let has_existing_with_id = lv["classes"].as_array().unwrap().iter().any(|c| {
        c["existing"]
            .as_array()
            .map(|xs| xs.iter().any(|x| x.get("id").is_some()))
            .unwrap_or(false)
    });
    assert!(
        has_existing_with_id,
        "the last few-shot example must be the upsert one (an `existing` entry \
         carrying an `id`), so id-upsert keeps the recency slot; last input was: {}",
        last.input
    );
}

fn titles(instances: &[ProposedInstance]) -> Vec<&str> {
    instances
        .iter()
        .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
        .collect()
}

#[test]
fn parses_clean_json_array() {
    let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing from Flux into ADAM","owner":"Nico"},
          {"class":"Belief","title":"Graph viz is the hardest part"}
        ]"#;
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(out[0].class, "Intention");
    assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("Nico"));
    assert_eq!(
        titles(&out),
        vec![
            "Extract LLM processing from Flux into ADAM",
            "Graph viz is the hardest part"
        ]
    );
}

#[test]
fn strips_code_fences() {
    let raw = "```json\n[{\"class\":\"Belief\",\"title\":\"X\"}]\n```";
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].class, "Belief");
}

#[test]
fn strips_think_block() {
    let raw =
            "<think>Let me find the intentions...</think>\n[{\"class\":\"Intention\",\"title\":\"Do X\"}]";
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].class, "Intention");
}

#[test]
fn tolerates_trailing_commas() {
    let raw = r#"[
          {"class":"Task","title":"A",},
          {"class":"Task","title":"B"},
        ]"#;
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(titles(&out), vec!["A", "B"]);
}

#[test]
fn empty_array_yields_no_instances() {
    assert!(parse_interpretation_response("[]").unwrap().is_empty());
    // and empty inside a fence / with whitespace
    assert!(parse_interpretation_response("```json\n[]\n```")
        .unwrap()
        .is_empty());
}

#[test]
fn garbage_is_an_error_not_a_panic() {
    assert!(parse_interpretation_response("not json at all").is_err());
}

// ---- planner: create vs. update ------------------------------------------

/// Base URI of the Nth (0-based) `Create` op, in op order. Panics if absent.
fn nth_create_base(ops: &[InterpretationOp], n: usize) -> String {
    ops.iter()
        .filter_map(|op| match op {
            InterpretationOp::Create { base, .. } => Some(base.clone()),
            _ => None,
        })
        .nth(n)
        .expect("expected a Create op at that index")
}

/// The links of the `AddLinks` op anchored on `source`, or an empty slice.
fn addlinks_for<'a>(ops: &'a [InterpretationOp], source: &str) -> &'a [Link] {
    ops.iter()
        .find_map(|op| match op {
            InterpretationOp::AddLinks { source: s, links } if s == source => {
                Some(links.as_slice())
            }
            _ => None,
        })
        .unwrap_or(&[])
}

fn targets_of(links: &[Link], predicate: &str) -> Vec<String> {
    links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some(predicate))
        .map(|l| l.target.clone())
        .collect()
}

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

    let ops = plan_interpretation_ops(&shapes, &proposed, "soa://ext/");
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

// ---- relation write-path (pure planner) ----------------------------------

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
    let known: std::collections::HashSet<String> = [existing_id.clone()].into_iter().collect();
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
    let ops = plan_interpretation_ops(&[shape], &proposed, "soa://ext/");

    // The Update carries the retitled scalar, no relation value.
    let update = ops
        .iter()
        .find_map(|op| match op {
            InterpretationOp::Update { base, values, .. } if base == &existing_id => Some(values),
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

// ---- write path against a real perspective (no LLM) -----------------------

/// Decoded targets of `(base, predicate)` in the store, sorted — the shape
/// assertions want, independent of the non-deterministic signed-envelope
/// encoding a `literal` resolve-language produces.
async fn decoded_targets(
    perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
    base: &str,
    predicate: &str,
) -> Vec<serde_json::Value> {
    use crate::perspectives::model_query::utils::parse_literal_value;
    use crate::types::LinkQuery;
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            predicate: Some(predicate.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    let mut out: Vec<serde_json::Value> = links
        .iter()
        .map(|l| parse_literal_value(&l.data.target))
        .collect();
    out.sort_by_key(|v| v.to_string());
    out
}

/// Plan + apply a single proposal against a live perspective.
async fn apply_one(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    shapes: &[crate::perspectives::model_query::types::ModelShape],
    ctx: &crate::agent::AgentContext,
    inst: ProposedInstance,
) -> Vec<InterpretationOp> {
    let ops = plan_interpretation_ops(shapes, std::slice::from_ref(&inst), "soa://ext/");
    apply_interpretation_ops(perspective, &ops, ctx)
        .await
        .expect("apply_interpretation_ops");
    ops
}

fn proposal(
    class: &str,
    id: Option<&str>,
    props: &[(&str, serde_json::Value)],
) -> ProposedInstance {
    ProposedInstance {
        class: class.to_string(),
        id: id.map(|s| s.to_string()),
        props: props
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect(),
    }
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

#[tokio::test]
async fn strip_noop_updates_drops_same_value_upsert_keeps_real_change() {
    // Seed an Intention with title+owner. Then plan three ops on it:
    //   (1) same title + same owner   -> no-op, must be dropped.
    //   (2) new title (different value) -> real change, must survive.
    //   (3) a Create -> passed through unchanged (dedup happens elsewhere).
    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
    let base = "soa://existing/intention/noop-target";
    seed_instance(&mut perspective, &ctx, &shapes[0], base, "Ship the parser").await;
    // Seed an owner too so the no-op check covers a multi-field state.
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

    let planned = plan_interpretation_ops(
        &shapes,
        &[
            // No-op update: title + owner identical to the seeded state.
            proposal(
                "Intention",
                Some(base),
                &[
                    ("title", serde_json::json!("Ship the parser")),
                    ("owner", serde_json::json!("Nico")),
                ],
            ),
            // Real update: same base, but a rewritten title.
            proposal(
                "Intention",
                Some(base),
                &[("title", serde_json::json!("Ship the parser this week"))],
            ),
            // A Create (no id) — strip_noop_updates only looks at Updates.
            proposal(
                "Intention",
                None,
                &[("title", serde_json::json!("A brand new idea"))],
            ),
        ],
        "soa://ext/",
    );
    assert_eq!(planned.len(), 3, "sanity: planner emitted all three");

    let kept = strip_noop_updates(&perspective, &shapes, planned)
        .await
        .expect("strip_noop_updates");

    let updates: Vec<&InterpretationOp> = kept
        .iter()
        .filter(|op| matches!(op, InterpretationOp::Update { .. }))
        .collect();
    let creates: Vec<&InterpretationOp> = kept
        .iter()
        .filter(|op| matches!(op, InterpretationOp::Create { .. }))
        .collect();
    assert_eq!(
        updates.len(),
        1,
        "no-op Update dropped, real Update kept; got {kept:#?}"
    );
    assert_eq!(creates.len(), 1, "Create pass-through; got {kept:#?}");
    let InterpretationOp::Update { values, .. } = updates[0] else {
        unreachable!()
    };
    assert_eq!(
        values.get("title").and_then(|v| v.as_str()),
        Some("Ship the parser this week"),
        "kept Update must be the real one"
    );
}

#[tokio::test]
async fn existing_instance_context_reads_id_and_identity() {
    // The context snapshot the prompt + relation resolver rely on: one row per
    // persisted instance, carrying the base URI as `id` and the class's declared
    // identity value as `title`.
    let (mut perspective, shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "soa://existing/task/1",
        "Migrate the SHACL parser",
    )
    .await;

    let ctx_map = existing_instance_context(&perspective, &shapes)
        .await
        .expect("existing_instance_context");
    let rows = ctx_map.get("Task").expect("Task rows present");
    assert_eq!(rows.len(), 1, "one seeded instance; got {rows:#?}");
    assert_eq!(rows[0].id, "soa://existing/task/1");
    assert_eq!(rows[0].title, "Migrate the SHACL parser");
    assert_eq!(rows[0].class, "Task");

    // The two derived views feed the dedup net and the relation resolver.
    assert_eq!(
        identities_from_context(&ctx_map).get("Task"),
        Some(&vec!["Migrate the SHACL parser".to_string()])
    );
    assert!(ids_from_context(&ctx_map).contains("soa://existing/task/1"));
}

#[test]
fn ensure_interpretation_task_registers_and_is_idempotent() {
    ensure_db_init();

    // Guard: some other test may have inserted the row already; wipe just
    // our name so the first call below is a real insert. (Global DB is
    // shared across the single-threaded test run.)
    let existing: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
        .unwrap()
        .into_iter()
        .filter(|t| t.name == INTERPRETATION_TASK_NAME)
        .collect();
    for t in existing {
        Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
    }

    let first = ensure_interpretation_task().unwrap();
    assert_eq!(first.name, INTERPRETATION_TASK_NAME);
    assert_eq!(first.model_id, "default");
    assert!(first.system_prompt.contains("You extract typed instances"));
    assert!(!first.task_id.is_empty());

    // Second call must find the same row, not insert a duplicate.
    let second = ensure_interpretation_task().unwrap();
    assert_eq!(first.task_id, second.task_id);

    let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
        .unwrap()
        .into_iter()
        .filter(|t| t.name == INTERPRETATION_TASK_NAME)
        .collect();
    assert_eq!(
        rows.len(),
        1,
        "expected exactly one interpretation task row"
    );
}

// ---- retry_interpretation_parse --------------------------------------

#[tokio::test]
async fn retry_interpretation_parse_succeeds_on_first_attempt() {
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let out = retry_interpretation_parse(move |_| {
        let a = attempts_clone.clone();
        async move {
            a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            Ok(r#"[{"class":"Belief","title":"X"}]"#.to_string())
        }
    })
    .await
    .unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 1);
}

#[tokio::test]
async fn retry_interpretation_parse_recovers_after_bad_parse() {
    // First attempt returns unparseable garbage; second returns valid JSON.
    // retry_interpretation_parse must call again and succeed within budget.
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let out = retry_interpretation_parse(move |_| {
        let a = attempts_clone.clone();
        async move {
            let n = a.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1;
            if n == 1 {
                Ok("total garbage, not json".to_string())
            } else {
                Ok(r#"[{"class":"Intention","title":"Y"}]"#.to_string())
            }
        }
    })
    .await
    .unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(attempts.load(std::sync::atomic::Ordering::SeqCst), 2);
}

#[tokio::test]
async fn retry_interpretation_parse_fails_after_max_attempts() {
    // Every attempt returns garbage → we exhaust INTERPRETATION_MAX_ATTEMPTS
    // and propagate the last parse error rather than looping forever.
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let result: anyhow::Result<Vec<ProposedInstance>> = retry_interpretation_parse(move |_| {
        let a = attempts_clone.clone();
        async move {
            a.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            Ok("never parseable".to_string())
        }
    })
    .await;
    assert!(result.is_err());
    assert_eq!(
        attempts.load(std::sync::atomic::Ordering::SeqCst),
        INTERPRETATION_MAX_ATTEMPTS
    );
}

#[test]
fn filter_already_present_drops_known_titles() {
    // Tasks declare `title` as their identity. Four Tasks proposed: one
    // duplicates an existing title (case-insensitive), one duplicates it under
    // whitespace normalization, one is new, and a same-title item of a
    // DIFFERENT class (which has no identity here) is untouched (dedup is per
    // class, and only for classes with a declared identity).
    let proposed = parse_interpretation_response(
        r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  ship   the   mvp  "},
              {"class":"Task","title":"Write the docs"},
              {"class":"Belief","title":"ship the mvp"}
            ]"#,
    )
    .unwrap();
    let mut existing = HashMap::new();
    existing.insert("Task".to_string(), vec!["ship the MVP".to_string()]);
    // Only Task declares `title` as its identity; Belief has none ⇒ no dedup.
    let mut identity_props = HashMap::new();
    identity_props.insert("Task".to_string(), "title".to_string());

    let kept = filter_already_present(proposed, &existing, &identity_props);
    let kept_titles: Vec<&str> = kept
        .iter()
        .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
        .collect();
    assert!(
        !kept_titles.contains(&"Ship the MVP"),
        "existing Task title must be dropped (case-insensitive); got {kept_titles:?}"
    );
    assert!(
        !kept_titles.contains(&"  ship   the   mvp  "),
        "whitespace-normalized duplicate Task title must be dropped; got {kept_titles:?}"
    );
    assert!(
        kept_titles.contains(&"Write the docs"),
        "new Task must survive"
    );
    assert!(
        kept_titles.contains(&"ship the mvp"),
        "same title on a class with no declared identity must NOT be dropped; got {kept_titles:?}"
    );
}

#[test]
fn filter_already_present_keeps_upserts_and_preserves_order() {
    // An `id`-carrying proposal is an explicit upsert target: its title
    // deliberately matches an existing one, so dedup must never drop it. And
    // filtering happens IN PLACE — the surviving order is the LLM's emission
    // order, which is what `new:<Class>:<n>` ordinals count against. Both
    // properties together are what make relation ordinals resolve correctly
    // after a dedup pass.
    let proposed = parse_interpretation_response(
        r#"[
              {"class":"Task","title":"Alpha"},
              {"class":"Task","id":"soa://existing/task/1","title":"Ship the MVP"},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Omega"}
            ]"#,
    )
    .unwrap();
    let mut existing = HashMap::new();
    existing.insert("Task".to_string(), vec!["Ship the MVP".to_string()]);
    let mut identity_props = HashMap::new();
    identity_props.insert("Task".to_string(), "title".to_string());

    let kept = filter_already_present(proposed, &existing, &identity_props);
    let kept_titles: Vec<&str> = kept
        .iter()
        .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
        .collect();
    assert_eq!(
        kept_titles,
        vec!["Alpha", "Ship the MVP", "Omega"],
        "upsert survives, plain duplicate is dropped, order preserved"
    );
    assert_eq!(
        kept[1].id.as_deref(),
        Some("soa://existing/task/1"),
        "the surviving 'Ship the MVP' must be the id-carrying upsert"
    );
}
