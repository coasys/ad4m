use super::*;
use crate::db::Ad4mDb;
use crate::perspectives::extraction_test_support::*;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::AITask;
use std::collections::HashMap;

#[test]
fn extraction_hint_lands_in_prompt() {
    let shapes = vec![
        shape_from_sdna("Belief", BELIEF_SDNA),
        shape_from_sdna("Intention", INTENTION_SDNA),
    ];
    let input = build_extraction_input(
        &shapes,
        &[(
            "Nico".into(),
            "I'll extract the LLM processing into ADAM".into(),
        )],
        &HashMap::<String, Vec<InstanceContext>>::new(),
    );

    // class-level hints reach the prompt
    assert!(input.contains("A claim a participant holds to be true"));
    assert!(input.contains("first-person commitment to do something"));
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
    // A relation-free shape still renders an empty `relations` array — a
    // stable schema key means the LLM (and the future prompt-side few-shot
    // for relations) can rely on the block always being present.
    assert!(
        intention["relations"].as_array().unwrap().is_empty(),
        "relation-free shape must render an empty relations block"
    );
}

#[test]
fn existing_context_renders_id_title_class_in_prompt() {
    // With the richer `existing_instance_context` snapshot in play,
    // build_extraction_input must render each existing entry as an object
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
    let input = build_extraction_input(
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
        EXTRACTION_SYSTEM_PROMPT.contains("id"),
        "system prompt must document the `id` upsert semantics"
    );
}

#[test]
fn system_prompt_documents_relation_ref_syntax() {
    // Phase 2 step 2: the system prompt must teach the LLM both the shape of
    // the per-class `relations` block AND the two allowed reference forms it
    // can put into a relation value. Without this instruction the LLM only
    // sees an unfamiliar array in the input schema — the parser can only
    // resolve refs it was told to emit.
    let p = EXTRACTION_SYSTEM_PROMPT;
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
    // would silently downgrade sibling-linking to unresolved refs at parse
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
    // Phase 2 step 3: the few-shot set must include a dedicated relations
    // example demonstrating the `new:<Class>:<n>` ref syntax the system prompt
    // teaches. It need NOT be last: empirically, putting the relations example
    // (all-new-instances) in the recency slot made gemma3:12b create-happy and
    // regressed the id-upsert behavior (`e2e_updates_existing_instance_via_id`
    // 0/5). So the *upsert* example owns the last slot; relations sit earlier
    // and still fire reliably (the topic-relation e2e is 3/3).
    let examples = extraction_examples();
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
    let out = parse_extraction_response(raw).unwrap();
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
    let out = parse_extraction_response(raw).unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].class, "Belief");
}

#[test]
fn strips_think_block() {
    let raw =
            "<think>Let me find the intentions...</think>\n[{\"class\":\"Intention\",\"title\":\"Do X\"}]";
    let out = parse_extraction_response(raw).unwrap();
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].class, "Intention");
}

#[test]
fn tolerates_trailing_commas() {
    let raw = r#"[
          {"class":"Task","title":"A",},
          {"class":"Task","title":"B"},
        ]"#;
    let out = parse_extraction_response(raw).unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(titles(&out), vec!["A", "B"]);
}

#[test]
fn empty_array_yields_no_instances() {
    assert!(parse_extraction_response("[]").unwrap().is_empty());
    // and empty inside a fence / with whitespace
    assert!(parse_extraction_response("```json\n[]\n```")
        .unwrap()
        .is_empty());
}

#[test]
fn garbage_is_an_error_not_a_panic() {
    assert!(parse_extraction_response("not json at all").is_err());
}

// ---- instance_links ---------------------------------------------

fn find_shape<'a>(shapes: &'a [ModelShape], class_uri: &str) -> &'a ModelShape {
    shapes
        .iter()
        .find(|s| s.target_class == class_uri)
        .unwrap_or_else(|| panic!("shape not found: {class_uri}"))
}

#[test]
fn instance_links_emit_type_flag_and_scalar_fields() {
    let shapes = vec![
        shape_from_sdna("Belief", BELIEF_SDNA),
        shape_from_sdna("Intention", INTENTION_SDNA),
    ];
    let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing","owner":"Nico"},
          {"class":"Belief","title":"This will work"}
        ]"#;
    let proposed = parse_extraction_response(raw).unwrap();

    let intent_links = instance_links(
        find_shape(&shapes, "ns://Intention"),
        &proposed[0],
        "soa://i1",
    );
    // Type flag: predicate = the flag's path, target = the flag's constant value.
    assert!(
        intent_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://intention"),
        "expected intention type flag; got {intent_links:#?}"
    );
    // Owner (literal-string) landed as a link at the correct predicate.
    assert!(
        intent_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")
                && l.target == "literal:string:Nico")
    );
    // Title (literal-string, percent-encoded space).
    assert!(intent_links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")
            && l.target == "literal:string:Extract%20LLM%20processing"));
    // Every emitted link is anchored at the given base.
    assert!(intent_links.iter().all(|l| l.source == "soa://i1"));

    let belief_links = instance_links(find_shape(&shapes, "ns://Belief"), &proposed[1], "soa://b1");
    // Belief has no `owner` field → no owner link even though the JSON above
    // does not carry it either (defensive: shape drives what gets emitted).
    assert!(!belief_links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://owner")));
    // Belief's own type flag with its own constant value.
    assert!(belief_links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://belief"));
}

#[test]
fn instance_links_drop_unknown_fields() {
    // The LLM hallucinates a `secret` field the shape doesn't declare.
    // instance_links must NOT emit a link for it (shape is the source of truth).
    let shape = shape_from_sdna("Belief", BELIEF_SDNA);
    let raw = r#"[{"class":"Belief","title":"X","secret":"leaked"}]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    let links = instance_links(&shape, &proposed[0], "soa://b1");
    assert!(
        !links.iter().any(|l| l.target.contains("leaked")),
        "unknown field must not become a link; got {links:#?}"
    );
}

#[test]
fn instance_links_skip_missing_optional_fields() {
    // Intention shape has an optional `owner`; instance omits it → no owner link.
    let shape = shape_from_sdna("Intention", INTENTION_SDNA);
    let raw = r#"[{"class":"Intention","title":"Ship it"}]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    let links = instance_links(&shape, &proposed[0], "soa://i2");
    assert!(!links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://owner")));
    assert!(links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")));
}

#[tokio::test]
async fn apply_ops_upsert_replaces_scalar_link_in_perspective() {
    // End-to-end (no LLM): seed a real perspective with an Intention whose
    // title/owner are already set, then apply an Update op that patches the
    // scalar fields on that same base. The old scalar links must be GONE
    // (SPARQL "set" semantics per predicate) and the new ones present; the
    // type flag stays untouched.
    use crate::perspectives::extraction_test_support::{seed_instance, setup_perspective_no_llm};
    use crate::types::{LinkQuery, LinkStatus};

    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
    let shape = &shapes[0];
    let base = "soa://existing/intention/upsert-target";

    // Seed: original title on the target instance.
    seed_instance(&mut perspective, &ctx, shape, base, "Draft the design").await;

    // Extra: give it an owner too (via ProposedInstance route).
    let mut owner_props = HashMap::new();
    owner_props.insert("owner".to_string(), serde_json::json!("Nico"));
    let owner_seed_inst = ProposedInstance {
        class: "Intention".to_string(),
        id: Some(base.to_string()),
        props: owner_props,
    };
    let owner_ops = plan_extraction_ops(
        &shapes,
        std::slice::from_ref(&owner_seed_inst),
        "soa://ext/",
    );
    apply_extraction_ops(&mut perspective, &owner_ops, LinkStatus::Local, &ctx)
        .await
        .expect("seed owner via update");

    // Now upsert: same base, revised title + new owner.
    let mut props = HashMap::new();
    props.insert(
        "title".to_string(),
        serde_json::json!("Draft the design and circulate it"),
    );
    props.insert("owner".to_string(), serde_json::json!("Josh"));
    let upsert = ProposedInstance {
        class: "Intention".to_string(),
        id: Some(base.to_string()),
        props,
    };
    let ops = plan_extraction_ops(&shapes, std::slice::from_ref(&upsert), "soa://ext/");
    assert!(matches!(ops[0], ExtractionOp::Update { .. }));

    apply_extraction_ops(&mut perspective, &ops, LinkStatus::Local, &ctx)
        .await
        .expect("apply upsert");

    // Read back everything anchored on the base.
    let stored = perspective
        .get_links(&LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links after upsert");
    let by_pred: std::collections::HashMap<String, Vec<String>> = stored
        .into_iter()
        .filter_map(|l| l.data.predicate.map(|p| (p, l.data.target)))
        .fold(std::collections::HashMap::new(), |mut m, (p, t)| {
            m.entry(p).or_default().push(t);
            m
        });

    // Type flag survives (create path wrote it; updates never touch it).
    assert_eq!(
        by_pred.get("ns://type").map(|v| v.as_slice()),
        Some(&["ns://intention".to_string()][..]),
        "type flag must remain; got {by_pred:?}"
    );
    // Title: exactly the new value, no residue of the old one.
    assert_eq!(
        by_pred.get("ns://title").map(|v| v.as_slice()),
        Some(&["literal:string:Draft%20the%20design%20and%20circulate%20it".to_string()][..]),
        "title must have been replaced (no old-value residue); got {by_pred:?}"
    );
    // Owner: exactly the new value (old "Nico" gone).
    assert_eq!(
        by_pred.get("ns://owner").map(|v| v.as_slice()),
        Some(&["literal:string:Josh".to_string()][..]),
        "owner must have been replaced; got {by_pred:?}"
    );
}

#[tokio::test]
async fn strip_noop_updates_drops_same_value_upsert_keeps_real_change() {
    // Seed an Intention with title+owner. Then plan two Updates on it:
    //   (1) same title + same owner   -> no-op, must be dropped.
    //   (2) new title (different value) -> real change, must survive.
    // The Create arm is passed through unchanged (dedup happens elsewhere).
    use crate::perspectives::extraction_test_support::{seed_instance, setup_perspective_no_llm};
    use crate::types::LinkStatus;

    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("Intention", INTENTION_SDNA)]).await;
    let shape = &shapes[0];
    let base = "soa://existing/intention/noop-target";
    seed_instance(&mut perspective, &ctx, shape, base, "Ship the parser").await;
    // Seed an owner too so the noop check covers a multi-field state.
    let mut owner_props = HashMap::new();
    owner_props.insert("owner".to_string(), serde_json::json!("Nico"));
    let seed_owner = ProposedInstance {
        class: "Intention".to_string(),
        id: Some(base.to_string()),
        props: owner_props,
    };
    let owner_ops = plan_extraction_ops(&shapes, std::slice::from_ref(&seed_owner), "soa://ext/");
    apply_extraction_ops(&mut perspective, &owner_ops, LinkStatus::Local, &ctx)
        .await
        .expect("seed owner");

    // No-op update: title + owner identical to the seeded state.
    let mut noop_props = HashMap::new();
    noop_props.insert("title".to_string(), serde_json::json!("Ship the parser"));
    noop_props.insert("owner".to_string(), serde_json::json!("Nico"));
    let noop = ProposedInstance {
        class: "Intention".to_string(),
        id: Some(base.to_string()),
        props: noop_props,
    };
    // Real update: same base, but a rewritten title.
    let mut real_props = HashMap::new();
    real_props.insert(
        "title".to_string(),
        serde_json::json!("Ship the parser this week"),
    );
    let real = ProposedInstance {
        class: "Intention".to_string(),
        id: Some(base.to_string()),
        props: real_props,
    };
    // A Create (no id) — should pass through unchanged; strip_noop_updates
    // only looks at Update ops.
    let mut create_props = HashMap::new();
    create_props.insert("title".to_string(), serde_json::json!("A brand new idea"));
    let create = ProposedInstance {
        class: "Intention".to_string(),
        id: None,
        props: create_props,
    };

    let planned = plan_extraction_ops(&shapes, &[noop, real, create], "soa://ext/");
    assert_eq!(planned.len(), 3, "sanity: planner emitted all three");
    let kept = strip_noop_updates(&perspective, planned)
        .await
        .expect("strip_noop_updates");

    // No-op update must be gone; real update + create must remain.
    let updates: Vec<&ExtractionOp> = kept
        .iter()
        .filter(|op| matches!(op, ExtractionOp::Update { .. }))
        .collect();
    let creates: Vec<&ExtractionOp> = kept
        .iter()
        .filter(|op| matches!(op, ExtractionOp::Create { .. }))
        .collect();
    assert_eq!(
        updates.len(),
        1,
        "no-op Update dropped, real Update kept; got {kept:#?}"
    );
    assert_eq!(creates.len(), 1, "Create pass-through; got {kept:#?}");
    if let ExtractionOp::Update { set, .. } = updates[0] {
        assert!(
            set.iter()
                .any(|l| l.predicate.as_deref() == Some("ns://title")
                    && l.target == "literal:string:Ship%20the%20parser%20this%20week"),
            "kept Update must be the real one; got {set:#?}"
        );
    }
}

#[test]
fn plan_ops_creates_without_id_and_updates_with_id() {
    // An `id` field marks an upsert: patch the existing node's scalar fields
    // (no fresh base, no re-written type flag). Absence of `id` = a create.
    let shapes = vec![shape_from_sdna("Intention", INTENTION_SDNA)];
    let raw = r#"[
      {"class":"Intention","title":"Write the design doc"},
      {"class":"Intention","id":"soa://existing/intention/42","title":"Write the design doc and circulate it","owner":"Nico"}
    ]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    // `id` is parsed into its own field, kept out of `props`.
    assert_eq!(
        proposed[1].id.as_deref(),
        Some("soa://existing/intention/42")
    );
    assert!(!proposed[1].props.contains_key("id"));

    let ops = plan_extraction_ops(&shapes, &proposed, "soa://ext/");
    assert_eq!(ops.len(), 2);

    match &ops[0] {
        ExtractionOp::Create { base, links } => {
            assert!(base.starts_with("soa://ext/intention/"));
            // create carries the type flag…
            assert!(links.iter().any(
                |l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://intention"
            ));
            assert!(links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://title")));
        }
        other => panic!("expected Create, got {other:?}"),
    }
    match &ops[1] {
        ExtractionOp::Update { base, set } => {
            assert_eq!(base, "soa://existing/intention/42");
            // update patches scalar fields on the EXISTING base…
            assert!(set
                .iter()
                .all(|l| l.source == "soa://existing/intention/42"));
            // …and never re-writes the type flag.
            assert!(!set
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://type")));
            assert!(set
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://title")));
            assert!(set
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://owner")));
        }
        other => panic!("expected Update, got {other:?}"),
    }
}

#[test]
fn relation_properties_are_excluded_from_extraction() {
    // A shape whose extraction hint also declares a link-typed relation
    // (`blocks`). load_shape lists that relation in `properties` too, so
    // without the guard it would be offered to the LLM and — if the LLM
    // emits it — written through value_to_literal_uri as a bogus literal.
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

    // 1. build_extraction_input must not offer the relation as a field.
    let input = build_extraction_input(
        &[shape.clone()],
        &[("Nico".into(), "block it".into())],
        &HashMap::<String, Vec<InstanceContext>>::new(),
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
        "relation must not be offered to the LLM; got {field_names:?}"
    );

    // Phase 2 rendering: forward relations surface in a dedicated `relations`
    // block (so the LLM knows what refs it *can* fill), separate from the
    // scalar `fields`. The `blocks` relation on `TASK_WITH_RELATION_SDNA` is
    // `hasMany` forward, so it belongs there with its target class + hint.
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
        "relation hint must reflect the sibling property's extractionHint"
    );

    // 2. instance_links must not write a link even if the LLM emits the
    //    relation (here as an array — exactly the bogus literal:json case).
    let raw = r#"[{"class":"Task","title":"Do X","blocks":["soa://t2","soa://t3"]}]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    let links = instance_links(&shape, &proposed[0], "soa://t1");
    assert!(
        !links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://blocks")),
        "relation must not become a link; got {links:#?}"
    );
    assert!(
        !links.iter().any(|l| l.target.starts_with("literal:json:")),
        "no bogus literal:json relation target; got {links:#?}"
    );
    // The scalar title still lands.
    assert!(links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")));
}

// --- Phase 2: relation write-path (pure planner) --------------------------

/// Base URI of the Nth (0-based) `Create` op, in op order. Panics if absent.
fn nth_create_base(ops: &[ExtractionOp], n: usize) -> String {
    ops.iter()
        .filter_map(|op| match op {
            ExtractionOp::Create { base, .. } => Some(base.clone()),
            _ => None,
        })
        .nth(n)
        .expect("expected a Create op at that index")
}

/// All links of the first `Create` op whose base matches `base`.
fn create_links_for<'a>(ops: &'a [ExtractionOp], base: &str) -> &'a [crate::types::Link] {
    ops.iter()
        .find_map(|op| match op {
            ExtractionOp::Create { base: b, links } if b == base => Some(links.as_slice()),
            _ => None,
        })
        .expect("no Create op for that base")
}

fn blocks_targets(links: &[crate::types::Link]) -> Vec<String> {
    links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some("ns://blocks"))
        .map(|l| l.target.clone())
        .collect()
}

#[test]
fn apply_relations_writes_link_to_new_sibling() {
    // Two Tasks minted in one pass; the first `blocks` the second via a
    // `new:Task:2` ref. The relation must resolve to the second Task's freshly
    // minted base and land as a real `ns://blocks` link (not a literal).
    let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
    let raw = r#"[
      {"class":"Task","title":"Ship the API","blocks":["new:Task:2"]},
      {"class":"Task","title":"Write the client"}
    ]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    let ops = plan_extraction_ops(&[shape], &proposed, "soa://ext/");

    let first_base = nth_create_base(&ops, 0);
    let second_base = nth_create_base(&ops, 1);
    let targets = blocks_targets(create_links_for(&ops, &first_base));
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
fn apply_relations_writes_link_to_existing_id() {
    // A single new Task blocks an existing one, referenced by its id. Only ids
    // the model was shown (in `known_existing_ids`) are accepted as targets.
    let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
    let existing_id = "soa://ext/task/already-here".to_string();
    let known: std::collections::HashSet<String> = [existing_id.clone()].into_iter().collect();
    let raw = format!(r#"[{{"class":"Task","title":"New work","blocks":["{existing_id}"]}}]"#,);
    let proposed = parse_extraction_response(&raw).unwrap();
    let ops = plan_extraction_ops_with_context(&[shape], &proposed, "soa://ext/", &known);

    let base = nth_create_base(&ops, 0);
    assert_eq!(
        blocks_targets(create_links_for(&ops, &base)),
        vec![existing_id],
        "existing-id blocks ref must resolve to that id"
    );
}

#[test]
fn apply_relations_drops_unresolved_ref() {
    // An out-of-range ordinal and an invented id are both unresolvable — the
    // node still lands, just with no relation link and no panic.
    let shape = shape_from_sdna("Task", TASK_WITH_RELATION_SDNA);
    let raw = r#"[
      {"class":"Task","title":"Lonely","blocks":["new:Task:99","soa://ext/task/never-shown"]}
    ]"#;
    let proposed = parse_extraction_response(raw).unwrap();
    // Empty known-ids: the bare id ref is "invented" from the model's POV.
    let ops = plan_extraction_ops(&[shape], &proposed, "soa://ext/");

    let base = nth_create_base(&ops, 0);
    let links = create_links_for(&ops, &base);
    assert!(
        blocks_targets(links).is_empty(),
        "unresolved refs must not become links; got {links:#?}"
    );
    // The scalar title still lands — a dropped relation never drops the node.
    assert!(links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")));
}

#[test]
fn apply_relations_hasone_takes_first_of_array() {
    // A single-cardinality (`hasOne`) relation given an array keeps only the
    // first resolved ref. `parent` is hasOne forward -> Task.
    let sdna = r#"{
      "target_class":"ns://Task",
      "extraction_hint":"A task.",
      "properties":[
        {"path":"ns://type","name":"type","has_value":"ns://task","min_count":1,"max_count":1},
        {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal"},
        {"path":"ns://parent","name":"parent","relation_kind":"hasOne","target_class_name":"Task","class":"ns://TaskShape","extraction_hint":"The parent task."}
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
    let proposed = parse_extraction_response(raw).unwrap();
    let ops = plan_extraction_ops(&[shape], &proposed, "soa://ext/");

    let child_base = nth_create_base(&ops, 0);
    let first_parent_base = nth_create_base(&ops, 1);
    let parent_targets: Vec<String> = create_links_for(&ops, &child_base)
        .iter()
        .filter(|l| l.predicate.as_deref() == Some("ns://parent"))
        .map(|l| l.target.clone())
        .collect();
    assert_eq!(
        parent_targets,
        vec![first_parent_base],
        "hasOne must keep only the first resolved ref (new:Task:2)"
    );
}

#[test]
fn apply_relations_from_update_target_emits_addlinks() {
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
    let proposed = parse_extraction_response(&raw).unwrap();
    let ops = plan_extraction_ops(&[shape], &proposed, "soa://ext/");

    // The Update carries the retitled scalar, no relation link.
    let update = ops
        .iter()
        .find_map(|op| match op {
            ExtractionOp::Update { base, set } if base == &existing_id => Some(set),
            _ => None,
        })
        .expect("expected an Update on the existing id");
    assert!(
        update
            .iter()
            .all(|l| l.predicate.as_deref() != Some("ns://blocks")),
        "scalar Update must not carry relation links"
    );
    assert!(update
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")));

    // The relation lands as an additive AddLinks on the same base, pointing at
    // the fresh sibling.
    let sibling_base = nth_create_base(&ops, 0);
    let add = ops
        .iter()
        .find_map(|op| match op {
            ExtractionOp::AddLinks { base, links } if base == &existing_id => Some(links),
            _ => None,
        })
        .expect("expected an AddLinks op on the existing id");
    assert_eq!(
        blocks_targets(add),
        vec![sibling_base],
        "AddLinks must point the blocks relation at the fresh sibling's base"
    );
}

#[test]
fn ensure_extraction_task_registers_and_is_idempotent() {
    ensure_db_init();

    // Guard: some other test may have inserted the row already; wipe just
    // our name so the first call below is a real insert. (Global DB is
    // shared across the single-threaded test run.)
    let existing: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
        .unwrap()
        .into_iter()
        .filter(|t| t.name == EXTRACTION_TASK_NAME)
        .collect();
    for t in existing {
        Ad4mDb::with_global_instance(|db| db.remove_task(t.task_id.clone())).unwrap();
    }

    let first = ensure_extraction_task().unwrap();
    assert_eq!(first.name, EXTRACTION_TASK_NAME);
    assert_eq!(first.model_id, "default");
    assert!(first.system_prompt.contains("You extract typed instances"));
    assert!(!first.task_id.is_empty());

    // Second call must find the same row, not insert a duplicate.
    let second = ensure_extraction_task().unwrap();
    assert_eq!(first.task_id, second.task_id);

    let rows: Vec<AITask> = Ad4mDb::with_global_instance(|db| db.get_tasks())
        .unwrap()
        .into_iter()
        .filter(|t| t.name == EXTRACTION_TASK_NAME)
        .collect();
    assert_eq!(rows.len(), 1, "expected exactly one extraction task row");
}

// ---- apply_extraction_raw + retry_extraction_parse ---------------

#[test]
fn apply_extraction_raw_wires_parse_and_links() {
    // Hand-fed raw = what the LLM would return; no live model in the loop.
    // We verify the whole parse→link wiring: each proposed instance gets a fresh
    // base under our prefix, its links are shape-driven (type flag + fields
    // only), and multi-class output is split correctly.
    let shapes = vec![
        shape_from_sdna("Belief", BELIEF_SDNA),
        shape_from_sdna("Intention", INTENTION_SDNA),
    ];
    let raw = r#"[
          {"class":"Intention","title":"Extract LLM processing","owner":"Nico"},
          {"class":"Belief","title":"This will work"}
        ]"#;

    let placements = apply_extraction_raw(&shapes, raw, "soa://ext/").unwrap();
    assert_eq!(placements.len(), 2, "expected two placements");

    // Bases are unique, prefixed, and class-tagged (lowercased).
    let (b0, links0) = &placements[0];
    let (b1, links1) = &placements[1];
    assert_ne!(b0, b1, "each instance must get its own base URI");
    assert!(b0.starts_with("soa://ext/intention/"));
    assert!(b1.starts_with("soa://ext/belief/"));
    assert!(links0.iter().all(|l| &l.source == b0));
    assert!(links1.iter().all(|l| &l.source == b1));

    // Intention: type flag + title + owner reached the link set.
    assert!(links0
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://intention"));
    assert!(links0
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://title")
            && l.target == "literal:string:Extract%20LLM%20processing"));
    assert!(
        links0
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")
                && l.target == "literal:string:Nico")
    );

    // Belief: type flag with its own constant, no owner predicate.
    assert!(links1
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://belief"));
    assert!(!links1
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://owner")));
}

#[test]
fn apply_extraction_raw_drops_unknown_class() {
    // Only Belief is registered; the LLM hallucinates a Frob. It must be
    // silently dropped (defensive: shapes are the source of truth for which
    // classes can be instantiated).
    let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
    let raw = r#"[
          {"class":"Belief","title":"A"},
          {"class":"Frob","title":"B"}
        ]"#;
    let placements = apply_extraction_raw(&shapes, raw, "soa://ext/").unwrap();
    assert_eq!(placements.len(), 1);
    assert!(placements[0].0.starts_with("soa://ext/belief/"));
}

#[test]
fn apply_extraction_raw_empty_array_yields_no_placements() {
    let shapes = vec![shape_from_sdna("Belief", BELIEF_SDNA)];
    assert!(apply_extraction_raw(&shapes, "[]", "soa://ext/")
        .unwrap()
        .is_empty());
}

#[tokio::test]
async fn retry_extraction_parse_succeeds_on_first_attempt() {
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let out = retry_extraction_parse(move |_| {
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
async fn retry_extraction_parse_recovers_after_bad_parse() {
    // First attempt returns unparseable garbage; second returns valid JSON.
    // retry_extraction_parse must call again and succeed within budget.
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let out = retry_extraction_parse(move |_| {
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
async fn retry_extraction_parse_fails_after_max_attempts() {
    // Every attempt returns garbage → we exhaust EXTRACTION_MAX_ATTEMPTS
    // and propagate the last parse error rather than looping forever.
    let attempts = std::sync::Arc::new(std::sync::atomic::AtomicU8::new(0));
    let attempts_clone = attempts.clone();
    let result: anyhow::Result<Vec<ProposedInstance>> = retry_extraction_parse(move |_| {
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
        EXTRACTION_MAX_ATTEMPTS
    );
}

#[test]
fn filter_already_present_drops_known_titles() {
    // Two Tasks proposed; one duplicates an existing title (case-insensitive),
    // one is new. Only the new one survives; a same-title item of a DIFFERENT
    // class is untouched (dedup is per class).
    let proposed = parse_extraction_response(
        r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Write the docs"},
              {"class":"Belief","title":"ship the mvp"}
            ]"#,
    )
    .unwrap();
    let mut existing = HashMap::new();
    existing.insert("Task".to_string(), vec!["ship the MVP".to_string()]);

    let kept = filter_already_present(proposed, &existing);
    let kept_titles: Vec<&str> = kept
        .iter()
        .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
        .collect();
    assert!(
        !kept_titles.contains(&"Ship the MVP"),
        "existing Task title must be dropped (case-insensitive); got {kept_titles:?}"
    );
    assert!(
        kept_titles.contains(&"Write the docs"),
        "new Task must survive"
    );
    assert!(
        kept_titles.contains(&"ship the mvp"),
        "same title on a different class must NOT be dropped; got {kept_titles:?}"
    );
}
