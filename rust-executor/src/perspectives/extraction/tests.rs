use super::*;
use crate::db::Ad4mDb;
use crate::perspectives::extraction_test_support::*;
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
        &HashMap::new(),
    );

    // class-level hints reach the prompt
    assert!(input.contains("A claim a participant holds to be true"));
    assert!(input.contains("actionable outcome with a plausible owner"));
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

// ---- relation exclusion ------------------------------------------

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
        &HashMap::new(),
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

// ---- retry_extraction_parse --------------------------------------

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
