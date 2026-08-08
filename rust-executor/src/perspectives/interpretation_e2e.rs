//! Real-LLM end-to-end tests for generic interpretation.
//!
//! These are the "look, the whole thing works" tests: a transcript goes in, a
//! real local model runs, and typed SoA instances come out and are persisted.
//! Split into their own file (from the pure unit tests in `interpretation.rs`) so a
//! reviewer can read *just this* to understand what the feature does end-to-end.
//!
//! They talk to an OpenAI-compatible endpoint (Ollama), NOT the embedded CUDA
//! LLM — so no GPU build is needed, only a reachable model. Endpoint + model are
//! env-overridable (`INTERPRETATION_E2E_BASE_URL` / `INTERPRETATION_E2E_MODEL` /
//! `INTERPRETATION_E2E_API_KEY`); defaults hit Ollama at `localhost:11434` with
//! `gemma3:12b` (fits the GPU, ~10s for the suite, Flux's summary model). On CI
//! (self-hosted runner = Marvin) that endpoint is local; from a dev box, tunnel
//! it (`ssh -L 11434:localhost:11434 marvin`).
//!
//! Requires that endpoint to be up — they are NOT `#[ignore]`d, so a `cargo test`
//! with no model reachable will fail here by design (that is the CI signal).
//! Run just this suite: `cargo test --release --lib perspectives::interpretation_e2e
//! -- --test-threads=1 --nocapture`.

#![cfg(test)]

use super::interpretation_test_support::*;
use super::model_query::types::ModelShape;
use super::perspective_instance::PerspectiveInstance;
use crate::types::Link;

// ---- create_subject write path (no LLM — runs without Ollama) ---------------

/// The interpretation write path goes through `create_subject`. This proves the
/// SDNA fixtures are real subject classes: constructor mints the type flag, the
/// `title` setter writes a literal that round-trips back through
/// `parse_literal_value`. Calls no AIService, so it runs with no model up.
#[tokio::test]
async fn create_subject_roundtrips_soa_instance() {
    use crate::perspectives::perspective_instance::SubjectClassOption;
    use crate::types::LinkQuery;
    let (mut perspective, _shapes, ctx) =
        setup_interpretation_e2e(&[("Intention", INTENTION_SDNA)]).await;
    let base = "soa://ext/intention/rt-test";
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Intention".into()),
                query: None,
            },
            base.to_string(),
            Some(serde_json::json!({ "title": "Ship the MVP", "owner": "Nico" })),
            None,
            &ctx,
        )
        .await
        .expect("create_subject");
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(base.into()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    assert!(
        links
            .iter()
            .any(|l| l.data.predicate.as_deref() == Some("ns://type")
                && l.data.target == "ns://intention"),
        "type flag; got {links:#?}"
    );
    let title = links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some("ns://title"))
        .map(|l| crate::perspectives::model_query::utils::parse_literal_value(&l.data.target));
    assert_eq!(
        title,
        Some(serde_json::Value::String("Ship the MVP".into())),
        "title round-trip; got {title:?}"
    );
}

// ---- basic per-class interpretation (DRY via the shared `run_e2e` harness) ------

/// Intention + Belief: an intent with an owner and a claim.
#[tokio::test]
async fn e2e_intention_and_belief() {
    // gemma3:12b occasionally files the belief-shaped utterance as another
    // intention (and vice-versa) on a single sample — mirrors the LLM-flake
    // guard used by upsert/selector neighbours. Retry with a fresh perspective
    // until both classes land, or fall through to the same asserts on the last
    // attempt so a genuine regression still surfaces detailed diagnostics.
    let (p, shapes, bases) = run_e2e_until(
        &[("Belief", BELIEF_SDNA), ("Intention", INTENTION_SDNA)],
        &[
            (
                "Nico",
                "I'll extract the LLM call-processing from Flux into a generic AD4M core service.",
            ),
            (
                "James",
                "Cool. One English hint per class should be enough to steer this.",
            ),
        ],
        3,
        |counts| {
            counts.get("intention").copied().unwrap_or(0) >= 1
                && counts.get("belief").copied().unwrap_or(0) >= 1
        },
    )
    .await;
    assert_persisted(&p, &shapes, &bases).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    assert!(
        counts.get("intention").copied().unwrap_or(0) >= 1,
        "expected an intention; got {counts:?}"
    );
    assert!(
        counts.get("belief").copied().unwrap_or(0) >= 1,
        "expected a belief; got {counts:?}"
    );
    // The intention should carry Nico as owner.
    let owners = graph_owners_lower(&p, &shapes).await;
    assert!(
        owners.iter().any(|o| o.contains("nico")),
        "expected the intention to be owned by Nico; got {owners:?}"
    );
}

/// Task-tracking conversation -> only Tasks, with owners. Three assignments in
/// the transcript should yield 2–4 tasks (LLM may merge/split slightly).
#[tokio::test]
async fn e2e_task_tracking_counts() {
    let (p, shapes, bases) = run_e2e(
        &[("Task", TASK_SDNA)],
        &[
            (
                "Nico",
                "James, can you finish the WebRTC call module in WE by Monday?",
            ),
            (
                "James",
                "Yes, I'll wrap up the call module and port the transcription over.",
            ),
            (
                "Josh",
                "I'll set up the wind-tunnel Docker scenario for the agent test.",
            ),
        ],
    )
    .await;
    assert_persisted(&p, &shapes, &bases).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    let tasks = counts.get("task").copied().unwrap_or(0);
    assert!(
        (2..=4).contains(&tasks),
        "expected 2-4 tasks from three assignments; got {counts:?}"
    );
    assert!(
        counts.keys().all(|k| k == "task"),
        "only Task was offered; got {counts:?}"
    );
    let owners = graph_owners_lower(&p, &shapes).await;
    assert!(
        !owners.is_empty(),
        "expected at least one task to carry an owner; got {owners:?}"
    );
}

/// Mixed epistemic conversation -> the three distinct modalities. The question
/// (ends in "?") is the clearest signal and should always be picked up.
#[tokio::test]
async fn e2e_mixed_epistemic_modalities() {
    let (p, shapes, bases) = run_e2e(
        &[
            ("Observation", OBSERVATION_SDNA),
            ("Belief", BELIEF_SDNA),
            ("Question", QUESTION_SDNA),
        ],
        &[
            (
                "Josh",
                "The executor was sitting at 100% CPU during the whole call.",
            ),
            ("Nico", "I think named graphs would fix that."),
            (
                "James",
                "But how do we merge when three LLMs write to the graph at once?",
            ),
        ],
    )
    .await;
    assert_persisted(&p, &shapes, &bases).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    let distinct = counts.len();
    assert!(
        distinct >= 2,
        "expected >=2 distinct modalities; got {counts:?}"
    );
    assert!(
        counts.get("question").copied().unwrap_or(0) >= 1,
        "the explicit question should be captured; got {counts:?}"
    );
}

/// Strategy conversation -> a Vision (the dream) and a Plan (the concrete path).
#[tokio::test]
async fn e2e_vision_and_plan() {
    let (p, shapes, bases) = run_e2e(
        &[("Vision", VISION_SDNA), ("Plan", PLAN_SDNA)],
        &[
            (
                "Nico",
                "The dream is a holonic collective-intelligence network where humans and AIs think together.",
            ),
            (
                "Nico",
                "Concretely, we start by shipping the SoA-flow MVP, then layer Synergy Fuel on top.",
            ),
        ],
    )
    .await;
    assert_persisted(&p, &shapes, &bases).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    assert!(
        counts.get("vision").copied().unwrap_or(0) >= 1
            || counts.get("plan").copied().unwrap_or(0) >= 1,
        "expected a Vision and/or Plan; got {counts:?}"
    );
}

// ---- longer conversation: the model must pick the right amounts -------------

/// A realistic multi-topic standup across seven SoA classes. Exercises "the
/// right amount of each comes out" over a longer transcript, not just "something
/// came out".
#[tokio::test]
async fn e2e_longer_standup_conversation() {
    let (p, shapes, bases) = run_e2e_until(
        &[
            ("Task", TASK_SDNA),
            ("Belief", BELIEF_SDNA),
            ("Question", QUESTION_SDNA),
            ("Observation", OBSERVATION_SDNA),
            ("Vision", VISION_SDNA),
            ("Plan", PLAN_SDNA),
            ("Intention", INTENTION_SDNA),
        ],
        &[
            ("Nico", "Morning everyone. Quick standup before the Holochain call."),
            ("Josh", "Heads up: CI on Marvin was red overnight, the p-diff-sync test flaked twice."),
            ("James", "I'll fix the flaky p-diff-sync test today and add a retry guard."),
            ("Nico", "Good. I still believe named graphs are the right substrate for the merge problem."),
            ("Josh", "How are we going to handle three agents writing to the same node concurrently?"),
            ("Nico", "Josh, can you draft the conflict-resolution design doc by Thursday?"),
            ("Josh", "Sure, I'll write up the CRDT-vs-lattice comparison and share it."),
            ("Nico", "The long game is a network where every community runs its own Eve and they federate."),
            ("James", "Concretely, the plan is: land interpretation, then flows, then the Synergy ledger."),
            ("Nico", "The interpretation e2e suite is now green on Marvin, by the way."),
        ],
        5,
        |c| c.get("task").copied().unwrap_or(0) >= 1,
    )
    .await;
    assert_persisted(&p, &shapes, &bases).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    let total: usize = counts.values().sum();
    // A 10-turn transcript with several concrete items — but we err on fewer.
    assert!(
        (4..=16).contains(&total),
        "expected a sane number of instances from a longer transcript; got {counts:?} (total {total})"
    );
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 1,
        "at least one task (fix flaky test / draft design doc); got {counts:?}"
    );
    assert!(
        counts.get("question").copied().unwrap_or(0) >= 1,
        "the concurrency question should be captured; got {counts:?}"
    );
    // Distinct modalities: a good interpretation spans more than one class here.
    assert!(
        counts.len() >= 3,
        "expected >=3 distinct classes across a rich transcript; got {counts:?}"
    );
}

// ---- selector against a non-empty graph -------------------------------------

/// Interpretation into a perspective that already holds an unrelated graph. The
/// selector must still place NEW instances correctly (fresh bases under
/// `soa://ext/`) without disturbing or colliding with the pre-existing nodes.
#[tokio::test]
async fn e2e_selector_over_prepopulated_graph() {
    // gemma3:12b occasionally hijacks a seeded task's id when the transcript
    // topic is only loosely related — a legal upsert, but not what this test is
    // about. Retry with a fresh perspective per attempt; if every attempt hits
    // the same glitch, fall through to the assertion with a real failure
    // message. Bumped from 3→5: on a bad night, 3 samples is not enough head-
    // room for a small-model flake to clear.
    const MAX_ATTEMPTS: usize = 5;
    let mut last: Option<(
        PerspectiveInstance,
        Vec<ModelShape>,
        Vec<(String, Vec<Link>)>,
    )> = None;
    for attempt in 1..=MAX_ATTEMPTS {
        let (mut perspective, shapes, ctx) =
            setup_interpretation_e2e(&[("Task", TASK_SDNA), ("Belief", BELIEF_SDNA)]).await;
        let task_shape = &shapes[0];
        let belief_shape = &shapes[1];

        // Seed a small existing graph unrelated to the new conversation.
        seed_instance(
            &mut perspective,
            &ctx,
            task_shape,
            "soa://existing/task/1",
            "Migrate the SHACL parser",
        )
        .await;
        seed_instance(
            &mut perspective,
            &ctx,
            task_shape,
            "soa://existing/task/2",
            "Ship the MCP server",
        )
        .await;
        seed_instance(
            &mut perspective,
            &ctx,
            belief_shape,
            "soa://existing/belief/1",
            "Local-first beats cloud-first",
        )
        .await;

        let placements = run_interpretation_e2e(
            &mut perspective,
            &shapes,
            &[
                (
                    "Nico",
                    "James, please write the integration test for the interpretation websocket endpoint.",
                ),
                (
                    "James",
                    "On it — I'll add the WS runInterpretation test this afternoon.",
                ),
            ],
            &ctx,
        )
        .await;

        let clean = placements
            .iter()
            .all(|(base, _)| !base.starts_with("soa://existing/"));
        last = Some((perspective, shapes, placements));
        if clean {
            if attempt > 1 {
                println!("[e2e] selector predicate satisfied on attempt {attempt}/{MAX_ATTEMPTS}");
            }
            break;
        }
        println!("[e2e] attempt {attempt}/{MAX_ATTEMPTS}: LLM emitted op on seeded base; retrying");
    }
    let (perspective, shapes, placements) = last.expect("retry loop ran at least once");

    assert_persisted(&perspective, &shapes, &placements).await;

    // New instances land under the interpretation prefix, never on the seeded bases.
    // (Where an instance is *minted* is inherently a placement property, so these
    // two checks stay on the affected placements.)
    assert!(
        placements
            .iter()
            .all(|(base, _)| base.starts_with("soa://ext/")),
        "new instances must be minted under soa://ext/, not reuse existing bases"
    );
    assert!(
        placements
            .iter()
            .all(|(base, _)| !base.starts_with("soa://existing/")),
        "interpretation must not overwrite pre-existing instance bases"
    );
    // And it should have found the new task in the conversation: the graph now
    // holds the 2 seeded tasks plus at least one freshly extracted one.
    let counts = graph_count_by_type(&perspective, &shapes).await;
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 3,
        "expected the 2 seeded tasks + the new WS-test task; got {counts:?}"
    );

    // The pre-existing instances are still present in the graph afterwards.
    let titles = graph_titles_lower(&perspective, &shapes).await;
    assert!(
        titles
            .iter()
            .any(|t| t.contains("migrate the shacl parser")),
        "seeded task must survive interpretation; got {titles:?}"
    );
}

// ---- dedup: don't recreate what's already in the graph ----------------------

/// Pre-seed a Task, then run interpretation on a transcript that *restates* that
/// same task and adds a genuinely new one. The existing task must NOT be
/// recreated (deterministic guarantee via `filter_already_present`), while the
/// new task is.
///
/// "Not recreated" is asserted on where instances live, not on title counts:
/// now that the upsert path exists, the model may legitimately land an `id`
/// update on the seeded base and reword its title. What must never happen is a
/// *fresh* instance under `soa://ext/` carrying the already-present title.
#[tokio::test]
async fn e2e_does_not_recreate_existing_task() {
    const SEEDED_BASE: &str = "soa://existing/task/webrtc";
    const SEEDED_TITLE: &str = "Finish the WebRTC call module";
    // Emitting *any* op for the brand-new CI-docs task is a small-model
    // reliability concern — one bad sample must not redden the suite. Retry
    // with a fresh perspective, matching the pattern used by neighbouring
    // upsert/selector e2e tests.
    const MAX_ATTEMPTS: usize = 3;

    let transcript = [
        // Restates the existing task…
        (
            "Nico",
            "Reminder: James still needs to finish the WebRTC call module.",
        ),
        // …and introduces a brand-new unrelated task. Deliberately in a
        // different topic area so the LLM cannot plausibly merge them into
        // a single upsert on the seeded base.
        ("Josh", "I'll update the CI documentation this evening."),
    ];

    let mut last: Option<(
        PerspectiveInstance,
        Vec<ModelShape>,
        Vec<(String, Vec<Link>)>,
    )> = None;
    for attempt in 1..=MAX_ATTEMPTS {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            SEEDED_BASE,
            SEEDED_TITLE,
        )
        .await;
        let placements = run_interpretation_e2e(&mut perspective, &shapes, &transcript, &ctx).await;
        let task_count = graph_count_by_type(&perspective, &shapes)
            .await
            .get("task")
            .copied()
            .unwrap_or(0);
        last = Some((perspective, shapes, placements));
        if task_count >= 2 {
            if attempt > 1 {
                println!("[e2e] second task emitted on attempt {attempt}/{MAX_ATTEMPTS}");
            }
            break;
        }
        println!(
            "[e2e] attempt {attempt}/{MAX_ATTEMPTS}: LLM emitted no op for the new CI-docs task \
             (task_count={task_count}); retrying"
        );
    }
    let (perspective, shapes, placements) = last.expect("retry loop ran at least once");
    assert_persisted(&perspective, &shapes, &placements).await;

    // The already-present task is never RECREATED: no freshly-minted instance
    // carries the seeded title. (An upsert landing on the seeded base and
    // refining its title is fine — that's the id-upsert path doing its job.)
    let seeded_lower = SEEDED_TITLE.to_lowercase();
    let rows = model_instances(&perspective, "Task", &["title"]).await;
    let minted_with_seeded_title: Vec<&serde_json::Value> = rows
        .iter()
        .filter(|r| {
            r.get("id")
                .and_then(|i| i.as_str())
                .map(|id| id.starts_with("soa://ext/"))
                .unwrap_or(false)
                && r.get("title")
                    .and_then(|t| t.as_str())
                    .map(|t| t.to_lowercase() == seeded_lower)
                    .unwrap_or(false)
        })
        .collect();
    assert!(
        minted_with_seeded_title.is_empty(),
        "must not mint a fresh instance with the already-present title; graph rows = {rows:#?}"
    );
    // A new task should still have been interpreted (the CI docs task), so the
    // graph holds the seeded task plus at least one new one.
    let counts = graph_count_by_type(&perspective, &shapes).await;
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 2,
        "expected the seeded task + a newly interpreted one after {MAX_ATTEMPTS} attempts; \
         got {counts:?}"
    );
}

// ---- upsert path: LLM chooses UPDATE over CREATE via `id` ------------------

/// Pre-seed a Task, then run interpretation on a transcript that explicitly
/// RENAMES it and assigns a NEW OWNER. The interpreter should recognise the
/// continuity (same underlying task) and emit the existing `id`, driving the
/// upsert path: the seeded instance's title/owner scalars end up REPLACED (the
/// class's `setSingleTarget` setters), its type flag stays put, and no duplicate
/// Task base is minted.
///
/// This exercises the tree-aware "attach" contract end-to-end: existing entries
/// in the prompt carry `{id, title, class}`, the system prompt + few-shot example
/// teach `id`-emission, and `plan_interpretation_ops_with_context` ->
/// `apply_interpretation_ops` routes those emissions through `update_subject`.
/// If the LLM refuses to emit `id`, the test surfaces that as a real failure —
/// the prompt/example engineering needs work.
#[tokio::test]
async fn e2e_updates_existing_instance_via_id() {
    const SEEDED_BASE: &str = "soa://existing/task/webrtc";
    const SEEDED_TITLE: &str = "Finish the WebRTC call module";
    // Emitting an `id` is the most fragile behaviour on small models; retry with
    // a fresh perspective rather than let one bad sample redden the suite.
    const MAX_ATTEMPTS: usize = 3;

    // Transcript renames the seeded task and assigns a new owner — a clear
    // continuation, not a fresh idea. The `id` handle to the existing task is
    // in the prompt; the LLM should emit it.
    let transcript = [
        (
            "Nico",
            "Update on the WebRTC work: let's rename that task to \
             'Complete the WebRTC call module and add a retry guard' and \
             assign it to Josh.",
        ),
        (
            "Josh",
            "Got it — I'll take over the WebRTC call module and add the retry guard.",
        ),
    ];

    let mut last: Option<(
        PerspectiveInstance,
        Vec<ModelShape>,
        Vec<(String, Vec<Link>)>,
    )> = None;
    for attempt in 1..=MAX_ATTEMPTS {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
        seed_instance(
            &mut perspective,
            &ctx,
            &shapes[0],
            SEEDED_BASE,
            SEEDED_TITLE,
        )
        .await;
        let placements = run_interpretation_e2e(&mut perspective, &shapes, &transcript, &ctx).await;
        let touched_seeded = placements.iter().any(|(base, _)| base == SEEDED_BASE);
        last = Some((perspective, shapes, placements));
        if touched_seeded {
            if attempt > 1 {
                println!("[e2e] upsert satisfied on attempt {attempt}/{MAX_ATTEMPTS}");
            }
            break;
        }
        println!("[e2e] attempt {attempt}/{MAX_ATTEMPTS}: LLM did not emit an id; retrying");
    }
    let (perspective, shapes, placements) = last.expect("retry loop ran at least once");
    assert_persisted(&perspective, &shapes, &placements).await;

    // Primary assertion: the run touched the seeded base. Creates always mint a
    // fresh UUID base under `soa://ext/`, so a placement on the seeded base can
    // only come from an Update — i.e. the LLM emitted the existing `id`.
    assert!(
        placements.iter().any(|(base, _)| base == SEEDED_BASE),
        "expected the LLM to emit id={SEEDED_BASE:?} for the renamed task \
         (upsert path); got placements = {placements:#?}"
    );

    // The update wrote the new owner and replaced the title, and left the type
    // flag alone — read back off the seeded base itself.
    let seeded_links = &placements
        .iter()
        .find(|(base, _)| base == SEEDED_BASE)
        .expect("seeded placement")
        .1;
    assert!(
        seeded_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == "ns://task"),
        "the update must leave the type flag in place; got {seeded_links:#?}"
    );
    assert!(
        seeded_links
            .iter()
            .any(|l| l.predicate.as_deref() == Some("ns://owner")),
        "expected the upsert to write the new owner on the seeded base; \
         got {seeded_links:#?}"
    );
    assert_eq!(
        seeded_links
            .iter()
            .filter(|l| l.predicate.as_deref() == Some("ns://title"))
            .count(),
        1,
        "title is single-cardinality: the setter must replace, not accumulate; \
         got {seeded_links:#?}"
    );

    // And no duplicate: the seeded title must not also exist on a fresh base.
    let seeded_lower = SEEDED_TITLE.to_lowercase();
    let rows = model_instances(&perspective, "Task", &["title"]).await;
    assert!(
        !rows.iter().any(|r| {
            r.get("id")
                .and_then(|i| i.as_str())
                .map(|id| id != SEEDED_BASE)
                .unwrap_or(false)
                && r.get("title")
                    .and_then(|t| t.as_str())
                    .map(|t| t.to_lowercase() == seeded_lower)
                    .unwrap_or(false)
        }),
        "the renamed task must not also exist as a duplicate; graph rows = {rows:#?}"
    );
}

// ---- relations: a reified edge links two freshly-minted nodes ---------------

/// SDNA for a `Topic` node (title only) — the endpoint a `SemanticRelationship`
/// tags. Declared inline so the relations e2e is self-contained.
const TOPIC_SDNA: &str = r#"{
  "target_class":"ns://Topic",
  "interpretation_hint":"A distinct subject or theme the participants discuss.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://topic"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://topic","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"Short topic label.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

/// SDNA for a reified edge: a scalar `relevance` plus a forward `tag` relation
/// to a `Topic`. This is the shape of Flux's `SemanticRelationship`, minus the
/// second (Message) endpoint to keep the e2e's class set small. No `identity`
/// property — edges are not deduped by their relevance score.
const SEMANTIC_RELATIONSHIP_SDNA: &str = r#"{
  "target_class":"ns://SemanticRelationship",
  "interpretation_hint":"An edge that tags a discussion point with a Topic and a relevance score.",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://semrel"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://semrel","min_count":1,"max_count":1},
    {"path":"ns://relevance","name":"relevance","min_count":1,"max_count":1,"resolve_language":"literal","interpretation_hint":"A number from 0 to 1: how strongly the tag applies.","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://relevance","target":"value"}]},
    {"path":"ns://tag","name":"tag","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape","interpretation_hint":"The Topic this edge tags. Reference an existing Topic id or a new:Topic:<n> sibling."}
  ]
}"#;

fn has_type(links: &[Link], type_value: &str) -> bool {
    links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == type_value)
}

/// True if some `SemanticRelationship` placement carries a `ns://tag` link whose
/// target is the base of an emitted `Topic` — i.e. the model filled the relation
/// with a resolvable reference (existing id or `new:Topic:<n>`) and the two-pass
/// planner turned it into a real edge, not a dropped literal.
fn tag_resolves_to_topic(pl: &[(String, Vec<Link>)]) -> bool {
    let topic_bases: std::collections::HashSet<&str> = pl
        .iter()
        .filter(|(_, links)| has_type(links, "ns://topic"))
        .map(|(b, _)| b.as_str())
        .collect();
    pl.iter().any(|(_, links)| {
        links.iter().any(|l| {
            l.predicate.as_deref() == Some("ns://tag") && topic_bases.contains(l.target.as_str())
        })
    })
}

/// The payoff test for the relations write path: from a two-topic transcript,
/// the model must mint the Topics AND a SemanticRelationship whose `tag`
/// relation *references* one of them (via `new:Topic:<n>` or an existing id),
/// which the two-pass planner resolves into a real `ns://tag` link between the
/// two minted nodes. gemma3:12b is the canary — if it emits the topic *title*
/// instead of a ref, no edge lands and the retry predicate fails, surfacing
/// prompt work rather than silently passing. Paraphrased from the few-shot so
/// it isn't verbatim.
#[tokio::test]
async fn e2e_interprets_topic_relation_from_transcript() {
    let (p, shapes, placements) = run_e2e_until_placements(
        &[
            ("Topic", TOPIC_SDNA),
            ("SemanticRelationship", SEMANTIC_RELATIONSHIP_SDNA),
        ],
        &[
            (
                "Ana",
                "We keep dropping failed webhook retries on the floor — if we logged every one, chasing down that payments outage would be far less painful.",
            ),
            (
                "Ben",
                "Agreed. Honestly retry logging is an observability problem more than a payments one.",
            ),
        ],
        4,
        |pl| {
            pl.iter()
                .filter(|(_, links)| has_type(links, "ns://topic"))
                .count()
                >= 2
                && tag_resolves_to_topic(pl)
        },
    )
    .await;
    assert_persisted(&p, &shapes, &placements).await;

    // Two clear topics: webhook/retry logging and observability.
    let counts = graph_count_by_type(&p, &shapes).await;
    assert!(
        counts.get("topic").copied().unwrap_or(0) >= 2,
        "expected >=2 Topics (retry logging + observability); got {counts:?}"
    );
    // At least one SemanticRelationship whose `tag` resolves to an emitted Topic.
    assert!(
        tag_resolves_to_topic(&placements),
        "expected a SemanticRelationship whose tag references an emitted Topic; \
         placements = {placements:#?}"
    );
    // The edge carries its scalar relevance too — the relation and the scalar
    // came out of the same interpretation pass, written by different halves of
    // the pipeline (setter vs. additive AddLinks).
    let semrel_has_relevance = placements.iter().any(|(_, links)| {
        has_type(links, "ns://semrel")
            && links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://relevance"))
    });
    assert!(
        semrel_has_relevance,
        "expected the SemanticRelationship to carry a relevance scalar; \
         placements = {placements:#?}"
    );
}

// ---- Flux-shaped grouping: persistent topics + rolling summary ------------

/// The Flux-grouping payoff: with multiple existing subgroups in the graph, a
/// transcript continuing ONE of their topics must resolve back to that seeded
/// subgroup's `id` (upsert) and *grow its rolling summary*, not mint a fresh
/// duplicate — and must leave the unrelated subgroup alone. This is what
/// replaces Flux's hard-coded grouping/topics LLM pass: the model sees the
/// existing subgroups via `existing_instance_context`, picks the right id, and
/// [`plan_interpretation_ops_with_context`] routes the proposal to an Update.
/// Persistent-topics reasoning is baked in: seeding two subgroups (not just
/// one) forces the model to discriminate rather than always update the last
/// one.
#[tokio::test]
async fn e2e_flux_grouping_updates_seeded_subgroup_on_topic_continuation() {
    let payments_base = "soa://existing/subgroup/payments";
    let onboarding_base = "soa://existing/subgroup/onboarding";
    let attempts = 3u8;
    let mut last_err: Option<String> = None;
    for i in 1..=attempts {
        let (mut perspective, shapes, ctx) =
            setup_interpretation_e2e(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;
        let sg_shape = &shapes[0];

        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            payments_base,
            serde_json::json!({
                "name": "Payments infrastructure",
                "summary": "The team discussed dropped webhook retries during a recent payments outage and the need for better observability on failure payloads."
            }),
        )
        .await;
        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            onboarding_base,
            serde_json::json!({
                "name": "Onboarding UX",
                "summary": "Ideas about smoothing the first-run flow for brand-new users, including copy tweaks and default profile fields."
            }),
        )
        .await;

        // Continuing turns on payments/webhooks — the model must resolve to the
        // seeded payments subgroup's id and update its summary in place.
        let placements = run_interpretation_e2e(
            &mut perspective,
            &shapes,
            &[
                (
                    "Ana",
                    "Following up on that webhook retry problem — we should persist the failed payloads so we can replay them after an outage.",
                ),
                (
                    "Ben",
                    "Yeah, a small retry ledger tied to the payments queue would let us reconstruct exactly what dropped last time.",
                ),
            ],
            &ctx,
        )
        .await;
        assert_persisted(&perspective, &shapes, &placements).await;

        let counts = graph_count_by_type(&perspective, &shapes).await;
        let n = counts.get("conversationsubgroup").copied().unwrap_or(0);
        if n != 2 {
            last_err = Some(format!(
                "attempt {i}/{attempts}: expected exactly 2 subgroups (seeds reused, no dupe); got {counts:?}"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }

        let rows =
            model_instances(&perspective, "ConversationSubgroup", &["name", "summary"]).await;
        let payments_summary = rows
            .iter()
            .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(payments_base))
            .and_then(|r| r.get("summary").and_then(|s| s.as_str()))
            .unwrap_or("")
            .to_lowercase();
        let onboarding_summary = rows
            .iter()
            .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(onboarding_base))
            .and_then(|r| r.get("summary").and_then(|s| s.as_str()))
            .unwrap_or("")
            .to_lowercase();

        let payments_grew = ["ledger", "replay", "persist", "payload", "queue"]
            .iter()
            .any(|kw| payments_summary.contains(kw));
        let onboarding_untouched = !onboarding_summary.contains("webhook")
            && !onboarding_summary.contains("ledger")
            && !onboarding_summary.contains("payment");

        if payments_grew && onboarding_untouched {
            return;
        }
        last_err = Some(format!(
            "attempt {i}/{attempts}: payments_grew={payments_grew} onboarding_untouched={onboarding_untouched}; \
             payments_summary={payments_summary:?}; onboarding_summary={onboarding_summary:?}"
        ));
        eprintln!("[e2e] {}", last_err.as_ref().unwrap());
    }
    panic!(
        "Flux-grouping continuation e2e failed after {attempts} attempts: {}",
        last_err.unwrap_or_default()
    );
}

/// A transcript on a *new* topic must mint a fresh `ConversationSubgroup`, not
/// mis-update the seeded one. This is the topic-shift half of the Flux-grouping
/// checkbox: paired with the continuation test above, together they prove the
/// extractor makes the attach-vs-grow-vs-create decision the way Flux's grouping
/// pass does — via `plan_interpretation_ops_with_context` routing on the model's
/// proposed `id`.
/// TODO(gemma3-model-gap, 2026-08-07): documents a known limitation, not a
/// framework capability. With one existing `ConversationSubgroup` seeded,
/// The topic-shift half of the Flux-grouping e2e checkbox. Seeds one
/// `ConversationSubgroup` on payments/webhooks, then feeds a transcript that
/// explicitly switches topic to a Q3 retrospective. A well-behaved extractor
/// mints a fresh subgroup for the new topic and leaves the seeded payments
/// summary untouched.
///
/// Reliably green on Marvin gemma3:12b (first-attempt on repeated local runs)
/// once three model-centric levers combine — no external code guardrail:
///   1. `existing_instance_context` carries each instance's secondary scalars
///      (the rolling `summary`), rendered into the prompt under `properties`,
///      so the model sees an existing subgroup's *state*, not just its label.
///   2. The system prompt's "partition, do not broadcast" rule: each turn's
///      content belongs to exactly one instance; minting a new instance must
///      not also fold that content into an unrelated existing entry.
///   3. The `summary` property's `interpretation_hint` scopes incorporation to
///      *this* subgroup's own topic and tells the model to leave an off-topic
///      subgroup's `id`/`summary` out of its output entirely.
/// Together these stop the two prior failure modes (pollution: minting the
/// fresh subgroup but also growing the seeded one's summary; and reuse-drift:
/// upserting the seeded id under a renamed topic). The retry loop stays as a
/// cheap guard against LLM non-determinism.
#[tokio::test]
async fn e2e_flux_grouping_creates_new_subgroup_on_topic_shift() {
    let seeded_base = "soa://existing/subgroup/payments";
    let attempts = 5u8;
    let mut last_err: Option<String> = None;
    for i in 1..=attempts {
        let (mut perspective, shapes, ctx) =
            setup_interpretation_e2e(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;
        let sg_shape = &shapes[0];

        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            seeded_base,
            serde_json::json!({
                "name": "Payments infrastructure",
                "summary": "The team discussed dropped webhook retries during a recent payments outage."
            }),
        )
        .await;

        // A completely unrelated topic — a well-behaved extractor mints a new
        // subgroup and does NOT cram this into the payments one.
        let placements = run_interpretation_e2e(
            &mut perspective,
            &shapes,
            &[
                (
                    "Ana",
                    "Switching topics entirely — Josh wants to run a Q3 retrospective focused on how Holograph shipped. Nothing to do with payments or webhooks.",
                ),
                (
                    "Ben",
                    "Good idea. Let's block off a Wednesday afternoon for retro prep and invite the mobile folks.",
                ),
            ],
            &ctx,
        )
        .await;
        assert_persisted(&perspective, &shapes, &placements).await;

        let counts = graph_count_by_type(&perspective, &shapes).await;
        let n = counts.get("conversationsubgroup").copied().unwrap_or(0);
        let rows =
            model_instances(&perspective, "ConversationSubgroup", &["name", "summary"]).await;
        let seeded_summary = rows
            .iter()
            .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(seeded_base))
            .and_then(|r| r.get("summary").and_then(|s| s.as_str()))
            .unwrap_or("")
            .to_lowercase();
        let seeded_untouched = !seeded_summary.contains("retro")
            && !seeded_summary.contains("holograph")
            && !seeded_summary.contains("q3");

        if n >= 2 && seeded_untouched {
            return;
        }
        last_err = Some(format!(
            "attempt {i}/{attempts}: subgroup_count={n} seeded_untouched={seeded_untouched}; \
             seeded_summary={seeded_summary:?}; counts={counts:?}"
        ));
        eprintln!("[e2e] {}", last_err.as_ref().unwrap());
    }
    panic!(
        "Flux-grouping topic-shift e2e failed after {attempts} attempts: {}",
        last_err.unwrap_or_default()
    );
}

// ---- semantic dedup: reject SEMANTICALLY-similar duplicate identity ---------

/// The `DedupStrategy::Semantic` path drops proposals whose identity string is
/// close in embedding space to something already in the graph — not just those
/// that string-normalize equal (the default). This test seeds a Task with one
/// wording, then feeds a transcript that reintroduces the SAME task under
/// different words + adds a genuinely new one. The default (`NormalizedString`)
/// strategy would let the reworded duplicate through; the semantic strategy
/// (via `nomic-embed-text` on the same Ollama base URL as the LLM) must catch
/// it, so no fresh instance carries a title semantically equal to the seed.
///
/// Requires `nomic-embed-text` to be pulled on the embeddings endpoint
/// (`ollama pull nomic-embed-text` on Marvin). Base URL/model overridable via
/// `INTERPRETATION_EMBED_BASE_URL` / `INTERPRETATION_EMBED_MODEL` (defaults
/// hit `http://localhost:11434/v1` + `nomic-embed-text`, matching the LLM
/// tunnel).
#[tokio::test]
async fn e2e_semantic_dedup_drops_reworded_duplicate() {
    use crate::perspectives::interpretation::DedupStrategy;

    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
    let task_shape = &shapes[0];

    // Seed with one wording; transcript uses a different wording for the SAME
    // work + genuinely-new work. String-normalize would keep the rewording.
    let seeded_title = "Finish the WebRTC call module";
    seed_instance(
        &mut perspective,
        &ctx,
        task_shape,
        "soa://existing/task/webrtc",
        seeded_title,
    )
    .await;

    let placements = run_interpretation_e2e_with_strategy(
        &mut perspective,
        &shapes,
        &[
            (
                "Nico",
                "Reminder: James still needs to wrap up the WebRTC calling module for the app.",
            ),
            ("Josh", "I'll update the CI documentation this evening."),
        ],
        &ctx,
        &DedupStrategy::semantic_from_env(0.75),
    )
    .await;
    assert_persisted(&perspective, &shapes, &placements).await;

    // No fresh instance under `soa://ext/` may carry a title whose embedding
    // is close to the seeded title. Rather than re-embed here, we just check
    // that no *newly-minted* task exists whose title lexically overlaps the
    // seeded one on the key salient tokens ("webrtc" + a "call/calling" or
    // "module"/"wrap up" verb). If the semantic filter did its job, the LLM's
    // reworded proposal was dropped BEFORE it reached the write path.
    let rows = model_instances(&perspective, "Task", &["title"]).await;
    let minted_dup: Vec<&serde_json::Value> = rows
        .iter()
        .filter(|r| {
            r.get("id")
                .and_then(|i| i.as_str())
                .map(|id| id.starts_with("soa://ext/"))
                .unwrap_or(false)
                && r.get("title")
                    .and_then(|t| t.as_str())
                    .map(|t| {
                        let l = t.to_lowercase();
                        l.contains("webrtc") && (l.contains("call") || l.contains("module"))
                    })
                    .unwrap_or(false)
        })
        .collect();
    assert!(
        minted_dup.is_empty(),
        "semantic dedup must drop the reworded WebRTC task; freshly-minted duplicates = {minted_dup:#?}"
    );
    // The genuinely-new CI-docs task should still land.
    let counts = graph_count_by_type(&perspective, &shapes).await;
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 2,
        "expected the seeded task + the new CI-docs task; got {counts:?}"
    );
}

// ---- auto_processor P-B2b smoke (real LLM) ---------------------------------

/// P-B2b end-to-end smoke: the full auto-processor wiring — config written to
/// the shared graph, read back via `load_processors`, transcript surfaced via
/// the config's SPARQL scope, `WatcherState` batches the turns, `run_one_pass`
/// wins the claim and drives the real LLM through the interpretation engine —
/// produces an actual typed instance on the perspective.
///
/// This is the "does the whole stack breathe end-to-end with a real model"
/// gate before P-C (multi-peer demo). The async polling of
/// `auto_processor_watch_loop` itself is exhaustively unit-tested elsewhere;
/// what this test uniquely exercises is the LLM round-trip on top of that
/// scaffolding.
#[tokio::test]
async fn auto_processor_pass_lands_interpretation_instance() {
    use crate::perspectives::auto_processor::config::{
        load_processors, write_processor, AutoProcessorConfig,
    };
    use crate::perspectives::auto_processor::watcher::{
        run_one_pass, turn_id, PassOutcome, WatcherState,
    };
    use crate::perspectives::interpretation::gather_transcript_sparql;
    use crate::types::{LinkQuery, LinkStatus};
    use std::time::{SystemTime, UNIX_EPOCH};

    let (mut perspective, _shapes, ctx) =
        setup_interpretation_e2e(&[("Intention", INTENTION_SDNA)]).await;

    // Seed two transcript turns as (msg -> body, msg -> author) link pairs.
    // Both are first-person commitments so gemma3:12b classifies them as
    // Intentions with reasonable reliability.
    for (uri, author, body) in [
        (
            "msg://smoke-1",
            "did:key:alice",
            "I'll finish the interpretation refactor tonight.",
        ),
        (
            "msg://smoke-2",
            "did:key:bob",
            "I plan to review the diff first thing tomorrow morning.",
        ),
    ] {
        perspective
            .add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://body".into()),
                    target: format!("literal:string:{body}"),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed body");
        perspective
            .add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://author".into()),
                    target: author.into(),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed author");
    }

    // Write the processor config into the shared graph.
    let cfg = AutoProcessorConfig {
        processor_id: "smoke-auto-processor".into(),
        source_scope_query: "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . \
                             ?m <ns://author> ?speaker . } ORDER BY ?m"
            .into(),
        interpretation_classes: vec!["ns://Intention".into()],
        debounce_ms: 50,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        llm_base_url: None,
        llm_model: None,
        dedup_strategy_json: None,
    };
    write_processor(&mut perspective, &cfg, &ctx)
        .await
        .expect("write_processor");

    // Round-trip the config back through `load_processors` — validates the
    // shared-graph link shape holds under the same perspective the watcher
    // would poll in production.
    let loaded = load_processors(&perspective)
        .await
        .expect("load_processors");
    assert_eq!(loaded.len(), 1, "single processor written and loaded back");
    let cfg_loaded = &loaded[0];
    assert_eq!(cfg_loaded.processor_id, "smoke-auto-processor");
    assert_eq!(
        cfg_loaded.interpretation_classes,
        vec!["ns://Intention".to_string()]
    );

    // Mimic one watch-loop tick: fetch transcript, feed WatcherState, drain,
    // run. The polling loop itself is not driven here; the pure state and its
    // debounce/cap contract are covered by unit tests in
    // `auto_processor::watcher::tests`.
    let transcript = gather_transcript_sparql(&perspective, &cfg_loaded.source_scope_query)
        .await
        .expect("gather_transcript_sparql");
    assert_eq!(
        transcript.len(),
        2,
        "SPARQL scope must surface both seeded turns; got {transcript:#?}"
    );

    let mut watcher = WatcherState::new();
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64;
    for (speaker, text) in &transcript {
        watcher.record_item(&cfg_loaded.processor_id, turn_id(speaker, text), now_ms);
    }
    // Advance `now` past the debounce window so `drain_ready_batch` releases.
    let drain_at = now_ms + cfg_loaded.debounce_ms + 10;
    let batch = watcher
        .drain_ready_batch(cfg_loaded, drain_at)
        .expect("drain returns the ready batch");
    assert_eq!(
        batch.len(),
        2,
        "both seeded turns should drain in one batch"
    );

    let outcome = run_one_pass(&mut perspective, cfg_loaded, &batch, drain_at, &ctx)
        .await
        .expect("run_one_pass");
    let bases = match outcome {
        PassOutcome::Won { bases } => bases,
        other => panic!(
            "expected PassOutcome::Won; got {other:?}. Bare private perspective has no other \
             claimants or online peers, so Won is the only legitimate outcome once shapes and \
             transcript both resolve."
        ),
    };
    assert!(
        !bases.is_empty(),
        "at least one Intention instance must have been minted"
    );

    // Confirm each base actually carries state in the perspective — proves the
    // pass wrote through `create_subject`, not just returned a URI.
    for base in &bases {
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(base.clone()),
                ..Default::default()
            })
            .await
            .expect("get_links readback");
        assert!(
            !links.is_empty(),
            "instance `{base}` must carry at least one link (type flag + title setter)"
        );
        assert!(
            links
                .iter()
                .any(|l| l.data.predicate.as_deref() == Some("ns://type")
                    && l.data.target == "ns://intention"),
            "instance `{base}` must carry the Intention type flag; got links={links:#?}"
        );
    }
}

// ---- auto_processor P-C — Flux-parity concurrent-processors demo -----------

/// P-C: two [`AutoProcessorConfig`]s on ONE perspective, sharing one SPARQL
/// scope over the same seeded turns, but with DISJOINT
/// `interpretation_classes` (`ns://Intention` vs. `ns://Task`). Proves the
/// Flux-parity invariant that a single perspective can host multiple
/// concurrent processors without cross-contamination:
///
/// * `run_one_pass` only feeds each processor its OWN configured shape into
///   the interpretation engine, so the Intention-only processor can NEVER
///   mint a Task and vice versa — an isolation guarantee that holds under
///   any LLM classification (code-enforced, not model-dependent).
/// * Distinct `processor_id`s produce distinct `batch_key`s → each processor
///   claims independently; neither pass backs off, both win.
/// * Distinct `base_prefix`es (`ad4m://autoprocessor/<processor_id>/instance/`)
///   guarantee the two processors' base URI sets are disjoint at the write
///   layer — no double-writing to the same base for the same turn.
///
/// This is the P-B closure milestone: the whole auto-processor stack
/// (config → shared graph → SPARQL scope → WatcherState → claim election →
/// per-processor interpretation) has now been driven end-to-end for a
/// multi-processor perspective with a real model.
#[tokio::test]
async fn auto_processor_two_configs_no_cross_contamination() {
    use crate::perspectives::auto_processor::config::{
        load_processors, write_processor, AutoProcessorConfig,
    };
    use crate::perspectives::auto_processor::watcher::{
        run_one_pass, turn_id, PassOutcome, WatcherState,
    };
    use crate::perspectives::interpretation::gather_transcript_sparql;
    use crate::types::{LinkQuery, LinkStatus};
    use std::time::{SystemTime, UNIX_EPOCH};

    // Both classes registered on the same perspective so each processor's
    // shape resolves under `load_shape_from_store`.
    let (mut perspective, _shapes, ctx) =
        setup_interpretation_e2e(&[("Intention", INTENTION_SDNA), ("Task", TASK_SDNA)]).await;

    // Seed one clear first-person commitment (Intention-shaped) and one
    // clear third-person assignment (Task-shaped). Note that whichever way
    // gemma3:12b classifies each turn is IRRELEVANT to the invariants
    // asserted here — the class isolation is code-enforced (each processor
    // only receives its own shape). The realism of the transcript is just
    // to keep the model's output non-degenerate.
    for (uri, author, body) in [
        (
            "msg://pc-1",
            "did:key:alice",
            "I'll finalize the executor watcher wiring tonight.",
        ),
        (
            "msg://pc-2",
            "did:key:bob",
            "Alice, can you get the CI dashboards live by Friday?",
        ),
    ] {
        perspective
            .add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://body".into()),
                    target: format!("literal:string:{body}"),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed body");
        perspective
            .add_link(
                Link {
                    source: uri.into(),
                    predicate: Some("ns://author".into()),
                    target: author.into(),
                },
                LinkStatus::Local,
                None,
                &ctx,
            )
            .await
            .expect("seed author");
    }

    let source_scope = "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . \
                        ?m <ns://author> ?speaker . } ORDER BY ?m";

    let intent_cfg = AutoProcessorConfig {
        processor_id: "pc-intent-proc".into(),
        source_scope_query: source_scope.into(),
        interpretation_classes: vec!["ns://Intention".into()],
        debounce_ms: 50,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        llm_base_url: None,
        llm_model: None,
        dedup_strategy_json: None,
    };
    let task_cfg = AutoProcessorConfig {
        processor_id: "pc-task-proc".into(),
        source_scope_query: source_scope.into(),
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms: 50,
        batch_max: 32,
        claim_ttl_ms: 60_000,
        llm_base_url: None,
        llm_model: None,
        dedup_strategy_json: None,
    };

    write_processor(&mut perspective, &intent_cfg, &ctx)
        .await
        .expect("write intent");
    write_processor(&mut perspective, &task_cfg, &ctx)
        .await
        .expect("write task");

    let loaded = load_processors(&perspective)
        .await
        .expect("load_processors");
    assert_eq!(loaded.len(), 2, "both processors written and loaded back");

    let transcript = gather_transcript_sparql(&perspective, source_scope)
        .await
        .expect("gather_transcript_sparql");
    assert_eq!(
        transcript.len(),
        2,
        "SPARQL scope must surface both seeded turns; got {transcript:#?}"
    );

    // Feed both processors the same turns via one WatcherState. Per-processor
    // pending state is isolated (unit-tested in
    // `auto_processor::watcher::tests::per_processor_state_is_isolated`); each
    // drain returns that processor's own copy of the batch.
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64;
    let mut watcher = WatcherState::new();
    for cfg_ref in &loaded {
        for (speaker, text) in &transcript {
            watcher.record_item(&cfg_ref.processor_id, turn_id(speaker, text), now_ms);
        }
    }

    // Advance past the debounce window so `drain_ready_batch` releases.
    let drain_at = now_ms + intent_cfg.debounce_ms + 10;

    // Run each processor once and collect (processor_id, class_uri, bases).
    let mut runs: Vec<(String, String, Vec<String>)> = Vec::with_capacity(2);
    for cfg_ref in &loaded {
        let batch = watcher
            .drain_ready_batch(cfg_ref, drain_at)
            .unwrap_or_else(|| {
                panic!(
                    "processor `{}` must have a ready batch after debounce",
                    cfg_ref.processor_id
                )
            });
        assert_eq!(
            batch.len(),
            2,
            "each processor's batch must contain both turns; got {batch:?} for `{}`",
            cfg_ref.processor_id
        );
        let outcome = run_one_pass(&mut perspective, cfg_ref, &batch, drain_at, &ctx)
            .await
            .expect("run_one_pass");
        let bases = match outcome {
            PassOutcome::Won { bases } => bases,
            other => panic!(
                "processor `{}` expected PassOutcome::Won; got {other:?}. Distinct \
                 processor_ids produce distinct batch_keys, so each pass MUST win its \
                 own claim election on this bare private perspective.",
                cfg_ref.processor_id
            ),
        };
        assert!(
            !bases.is_empty(),
            "processor `{}` must mint at least one instance for its class `{}`",
            cfg_ref.processor_id,
            cfg_ref.interpretation_classes[0]
        );
        runs.push((
            cfg_ref.processor_id.clone(),
            cfg_ref.interpretation_classes[0].clone(),
            bases,
        ));
    }

    // Invariant 1: each base carries its processor's expected type flag, and
    // NEVER the other processor's flag.
    let flag_for = |class_uri: &str| -> String {
        // `ns://Intention` → `ns://intention` (the constructor's type-flag
        // target — lowercase local name).
        let local = class_uri
            .rsplit_once("://")
            .map(|(_, l)| l)
            .unwrap_or(class_uri);
        format!("ns://{}", local.to_lowercase())
    };
    for (proc_id, class_uri, bases) in &runs {
        let own_flag = flag_for(class_uri);
        let foreign_flag = if own_flag == "ns://intention" {
            "ns://task".to_string()
        } else {
            "ns://intention".to_string()
        };
        for base in bases {
            let links = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    ..Default::default()
                })
                .await
                .expect("get_links readback");
            assert!(
                !links.is_empty(),
                "instance `{base}` (from `{proc_id}`) must carry at least one link"
            );
            assert!(
                links
                    .iter()
                    .any(|l| l.data.predicate.as_deref() == Some("ns://type")
                        && l.data.target == own_flag),
                "instance `{base}` (from `{proc_id}`) must carry its own class flag \
                 `{own_flag}`; got links={links:#?}"
            );
            assert!(
                !links
                    .iter()
                    .any(|l| l.data.predicate.as_deref() == Some("ns://type")
                        && l.data.target == foreign_flag),
                "instance `{base}` (from `{proc_id}`) MUST NOT carry the foreign class \
                 flag `{foreign_flag}` — cross-contamination between processors on the \
                 same perspective is a P-C invariant violation; got links={links:#?}"
            );
        }
    }

    // Invariant 2: base URI sets are disjoint. Distinct `base_prefix`es
    // (`ad4m://autoprocessor/<processor_id>/instance/`) make double-writing
    // to the same base for the same turn structurally impossible.
    let intent_bases: std::collections::HashSet<&String> = runs
        .iter()
        .find(|(id, _, _)| id == "pc-intent-proc")
        .map(|(_, _, b)| b.iter().collect())
        .expect("intent-proc run captured");
    let task_bases: std::collections::HashSet<&String> = runs
        .iter()
        .find(|(id, _, _)| id == "pc-task-proc")
        .map(|(_, _, b)| b.iter().collect())
        .expect("task-proc run captured");
    let overlap: Vec<&&String> = intent_bases.intersection(&task_bases).collect();
    assert!(
        overlap.is_empty(),
        "processor base URI sets must be disjoint (distinct base_prefix per \
         processor_id); overlap = {overlap:?}"
    );
    for b in &intent_bases {
        assert!(
            b.starts_with("ad4m://autoprocessor/pc-intent-proc/instance/"),
            "intent-proc base `{b}` must carry the intent-proc base_prefix"
        );
    }
    for b in &task_bases {
        assert!(
            b.starts_with("ad4m://autoprocessor/pc-task-proc/instance/"),
            "task-proc base `{b}` must carry the task-proc base_prefix"
        );
    }
}
