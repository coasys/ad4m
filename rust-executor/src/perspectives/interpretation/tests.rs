use super::*;
use crate::db::Ad4mDb;
use crate::perspectives::interpretation_test_support::*;
use crate::types::AITask;
use std::collections::HashMap;

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
        &HashMap::new(),
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

#[test]
fn identity_field_name_lands_in_prompt_for_non_title_identity() {
    // A class whose `identity` is `name` (not `title`) must have its identity
    // field name surfaced in the prompt so the model emits that field for
    // dedup to work. Two `existing` values simulate previously-seen persons.
    let shape = shape_from_sdna("Person", PERSON_SDNA);
    let mut existing = HashMap::new();
    existing.insert("Person".to_string(), vec!["Alice".into(), "Bob".into()]);
    let mut identity_props = HashMap::new();
    identity_props.insert("Person".to_string(), "name".to_string());

    let input = build_interpretation_input(
        &[shape],
        &[("Nico".into(), "Carol joined us today.".into())],
        &existing,
        &identity_props,
    );

    let v: serde_json::Value = serde_json::from_str(&input).unwrap();
    let person = v["classes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "Person")
        .expect("Person class in prompt");
    assert_eq!(
        person["identity"].as_str(),
        Some("name"),
        "prompt must expose the declared identity field name"
    );
    let existing_values: Vec<&str> = person["existing"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|v| v.as_str())
        .collect();
    assert_eq!(existing_values, vec!["Alice", "Bob"]);

    // System prompt must describe `existing` in terms of the identity field,
    // not hard-coded titles — otherwise the model may emit `title` for a
    // class whose identity is `name` and bypass dedup.
    assert!(
        INTERPRETATION_SYSTEM_PROMPT.contains("`identity`"),
        "system prompt must mention the identity field"
    );
    assert!(
        !INTERPRETATION_SYSTEM_PROMPT.contains("titles already present"),
        "system prompt must not hard-code 'titles'"
    );
}

#[test]
fn identity_field_absent_for_class_without_declared_identity() {
    // A class with no `identity` property in the SDNA must NOT get an
    // `identity` key emitted — otherwise the model would infer a dedup field
    // where the framework does not perform dedup.
    let shape = shape_from_sdna("Belief", BELIEF_SDNA);
    let input = build_interpretation_input(
        &[shape],
        &[("A".into(), "It rained.".into())],
        &HashMap::new(),
        &HashMap::new(), // no identity_props entry ⇒ no identity key
    );

    let v: serde_json::Value = serde_json::from_str(&input).unwrap();
    let belief = v["classes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "Belief")
        .expect("Belief class in prompt");
    assert!(
        belief.get("identity").is_none(),
        "identity key must be omitted when no identity property was declared"
    );
}

/// Pull a named property's string value off each parsed instance. These are
/// pure parse-level assertions over the raw LLM JSON — there is no graph and no
/// dedup here, so this takes the field name explicitly rather than assuming a
/// `title`. (Dedup identity is class-declared and handled graph-side in
/// `filter_already_present` / `existing_instance_identities`.)
fn prop_values<'a>(instances: &'a [ProposedInstance], key: &str) -> Vec<&'a str> {
    instances
        .iter()
        .filter_map(|i| i.props.get(key).and_then(|v| v.as_str()))
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
        prop_values(&out, "title"),
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
    assert_eq!(prop_values(&out, "title"), vec!["A", "B"]);
}

#[test]
fn trailing_comma_cleanup_preserves_commas_inside_strings() {
    let raw = r#"[
          {"class":"Belief","title":"Hello, world}"},
          {"class":"Task","title":"A, B, and C]"},
        ]"#;
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(
        out[0].props.get("title").unwrap().as_str(),
        Some("Hello, world}")
    );
    assert_eq!(
        out[1].props.get("title").unwrap().as_str(),
        Some("A, B, and C]")
    );
}

#[test]
fn trailing_commas_stripped_despite_odd_quotes_in_prose() {
    // Regression: `clean_llm_json` must extract the JSON block BEFORE stripping
    // trailing commas. The prose prefix here carries an odd number of `"`
    // (one, before "here's"), which — if the comma-stripper scanned the whole
    // text — inverts its `in_string` flag before the real JSON begins, so the
    // genuine trailing commas below would not be stripped and the payload would
    // fail to parse. Extracting first confines the scanner to actual JSON.
    let raw = "The model replied: \"here's your data\n[\n  {\"class\":\"Task\",\"title\":\"A\",},\n  {\"class\":\"Task\",\"title\":\"B\"},\n]";
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 2);
    assert_eq!(prop_values(&out, "title"), vec!["A", "B"]);
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

#[test]
fn extracts_array_from_surrounding_prose() {
    // Real gemma3:12b output observed on CI 2026-08-07 (job 19580):
    // wrapped its reply in <analysis> narration followed by the JSON array.
    let raw = r#"<analysis>
    Turn 1: Nico assigns work to James.
    Turn 2: Sure -> commitment (Task); "still think the WS layer is cleanest" -> Belief.
    Turn 3: Nico asks about perspectives with no subject classes -> Question.
</analysis>


[
  {"class": "ExtTask", "title": "Write the integration test for the interpretation endpoint", "owner": "James"},
  {"class": "ExtBelief", "title": "The WS layer is the cleanest way to expose this"},
  {"class": "ExtQuestion", "title": "How do we handle a perspective that has no subject classes registered?"}
]"#;
    let out = parse_interpretation_response(raw).unwrap();
    assert_eq!(out.len(), 3);
    assert_eq!(out[0].class, "ExtTask");
    assert_eq!(out[0].props.get("owner").unwrap().as_str(), Some("James"));
    assert_eq!(out[2].class, "ExtQuestion");
}

#[test]
fn extracts_single_object_when_no_array() {
    let raw = "Here is the extracted item:\n{\"class\":\"Belief\",\"title\":\"X\"}\nthanks";
    // A bare object isn't the interpretation contract (array of instances) so
    // this must still error — but extract_bracketed shouldn't panic.
    let err = parse_interpretation_response(raw).unwrap_err();
    let msg = format!("{err}");
    assert!(
        msg.contains("interpretation JSON parse failed"),
        "got: {msg}"
    );
}

#[test]
fn parse_error_does_not_leak_llm_payload() {
    // The cleaned LLM payload can carry the raw conversation transcript. It
    // must not appear in the error message, because retry_interpretation_parse
    // logs this error on every failed attempt. Only safe metadata (length) is
    // allowed to surface.
    let secret = "TOP_SECRET_DINNER_PLAN alice met bob at the safehouse";
    let raw = format!("[{{ \"class\":\"Note\", \"title\":\"{secret}\", NOT_JSON");
    let err = parse_interpretation_response(&raw).unwrap_err();
    let msg = format!("{err}");
    assert!(
        !msg.contains(secret),
        "parse error must not include the LLM payload; got: {msg}"
    );
    assert!(
        msg.contains("payload length"),
        "parse error must include the payload length metadata; got: {msg}"
    );
}

// ---- relation exclusion ------------------------------------------

#[test]
fn relation_properties_are_excluded_from_interpretation() {
    // A shape whose interpretation hint also declares a link-typed relation
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

    // 1. build_interpretation_input must not offer the relation as a field.
    let input = build_interpretation_input(
        &[shape.clone()],
        &[("Nico".into(), "block it".into())],
        &HashMap::new(),
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

    // Target the DB-only primitive: it registers the row without touching the
    // AIService, so this stays a no-model/no-GPU unit test. (The async
    // `ensure_interpretation_task` wrapper additionally spawns the task.)
    let (first, created) = register_interpretation_task().unwrap();
    assert!(created, "first call after wipe must insert the row");
    assert_eq!(first.name, INTERPRETATION_TASK_NAME);
    assert_eq!(first.model_id, "default");
    assert!(first.system_prompt.contains("You extract typed instances"));
    assert!(!first.task_id.is_empty());

    // Second call must find the same row, not insert a duplicate.
    let (second, created_again) = register_interpretation_task().unwrap();
    assert!(!created_again, "second call must find the existing row");
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
fn filter_already_present_dedupes_within_same_response() {
    // The LLM sometimes emits the same (class, identity) twice in one response
    // (verbatim, or under whitespace/case variation). Without intra-response
    // dedup those slip past `filter_already_present` because the pre-existing
    // `known` set does not yet contain them — and `run_interpretation` then
    // mints two subjects for the same identity. Fix: accumulate accepted
    // identities as we scan the response, dropping later same-key proposals
    // exactly like already-persisted ones.
    let proposed = parse_interpretation_response(
        r#"[
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"  SHIP  the  mvp  "},
              {"class":"Task","title":"Ship the MVP"},
              {"class":"Task","title":"Write the docs"}
            ]"#,
    )
    .unwrap();
    let existing: HashMap<String, Vec<String>> = HashMap::new(); // graph empty
    let mut identity_props = HashMap::new();
    identity_props.insert("Task".to_string(), "title".to_string());

    let kept = filter_already_present(proposed, &existing, &identity_props);
    let kept_titles: Vec<&str> = kept
        .iter()
        .filter_map(|i| i.props.get("title").and_then(|v| v.as_str()))
        .collect();

    // First occurrence wins; every subsequent normalized-equal proposal drops.
    assert_eq!(
        kept_titles,
        vec!["Ship the MVP", "Write the docs"],
        "intra-response duplicates must be dropped after the first occurrence; got {kept_titles:?}"
    );
}
