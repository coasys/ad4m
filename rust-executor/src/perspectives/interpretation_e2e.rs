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
    let (p, shapes, bases) = run_e2e_until(
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
        3,
        |c| (2..=4).contains(&c.get("task").copied().unwrap_or(0)),
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
    let (p, shapes, bases) = run_e2e_until(
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
        3,
        |c| c.len() >= 2 && c.get("question").copied().unwrap_or(0) >= 1,
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
    let (p, shapes, bases) = run_e2e_until(
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
        3,
        |c| {
            c.get("vision").copied().unwrap_or(0) >= 1
                || c.get("plan").copied().unwrap_or(0) >= 1
        },
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
        // In production the seeded task would have been minted by a prior
        // interpretation pass; seed its overlay so the §4 gate lets this pass
        // replace the title in place (real title == last inference), exercising
        // the LLM-owns → overwrite branch this test is about.
        seed_llm_overlay(
            &mut perspective,
            &ctx,
            &shapes[0],
            SEEDED_BASE,
            serde_json::json!({ "title": SEEDED_TITLE }),
        )
        .await;
        let placements = run_interpretation_e2e(&mut perspective, &shapes, &transcript, &ctx).await;
        let touched_seeded = placements.iter().any(|(base, _)| base == SEEDED_BASE);
        // The test is about the LLM-owned overwrite branch, so a "successful"
        // attempt requires BOTH the id-emission (touched the seeded base) AND
        // an actual title change through the gate. Otherwise gemma3:12b's
        // occasional owner-only Update passes the retry gate but leaves the
        // strengthened title-changed assertion to flake downstream.
        let title_changed_on_seeded = if touched_seeded {
            let rows = model_instances(&perspective, "Task", &["title"]).await;
            rows.iter()
                .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(SEEDED_BASE))
                .and_then(|r| r.get("title").and_then(|t| t.as_str()))
                .is_some_and(|t| !t.eq_ignore_ascii_case(SEEDED_TITLE))
        } else {
            false
        };
        last = Some((perspective, shapes, placements));
        if touched_seeded && title_changed_on_seeded {
            if attempt > 1 {
                println!(
                    "[e2e] upsert + title-change satisfied on attempt {attempt}/{MAX_ATTEMPTS}"
                );
            }
            break;
        }
        if !touched_seeded {
            println!("[e2e] attempt {attempt}/{MAX_ATTEMPTS}: LLM did not emit an id; retrying");
        } else {
            println!(
                "[e2e] attempt {attempt}/{MAX_ATTEMPTS}: LLM touched the seeded base but left the title unchanged; retrying"
            );
        }
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

    // The single title link must carry a *new* value — the whole point of the
    // seeded-overlay branch is that the gate lets the LLM overwrite in place.
    // A test that only asserts one link exists would silently pass if the gate
    // held the seed unchanged (real == SEEDED_TITLE), so read the seeded base
    // back through model_query and require the persisted title to differ.
    let seeded_lower = SEEDED_TITLE.to_lowercase();
    let rows = model_instances(&perspective, "Task", &["title"]).await;
    let seeded_row_title = rows
        .iter()
        .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(SEEDED_BASE))
        .and_then(|r| r.get("title").and_then(|t| t.as_str()))
        .map(str::to_string);
    assert!(
        seeded_row_title
            .as_deref()
            .is_some_and(|t| !t.eq_ignore_ascii_case(SEEDED_TITLE)),
        "the upsert must have overwritten the seeded title; \
         got title={seeded_row_title:?}, seeded={SEEDED_TITLE:?}"
    );

    // And no duplicate: the seeded title must not also exist on a fresh base.
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

        let payments_props = serde_json::json!({
            "name": "Payments infrastructure",
            "summary": "The team discussed dropped webhook retries during a recent payments outage and the need for better observability on failure payloads."
        });
        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            payments_base,
            payments_props.clone(),
        )
        .await;
        // Seed the overlay too: in production this subgroup would have been minted
        // by a prior interpretation pass (which writes an overlay), so the §4 gate
        // must see it as LLM-authored to let this continuation grow its summary.
        seed_llm_overlay(
            &mut perspective,
            &ctx,
            sg_shape,
            payments_base,
            payments_props,
        )
        .await;

        let onboarding_props = serde_json::json!({
            "name": "Onboarding UX",
            "summary": "Ideas about smoothing the first-run flow for brand-new users, including copy tweaks and default profile fields."
        });
        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            onboarding_base,
            onboarding_props.clone(),
        )
        .await;
        seed_llm_overlay(
            &mut perspective,
            &ctx,
            sg_shape,
            onboarding_base,
            onboarding_props,
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

        let payments_grew = ["ledger", "replay", "persist", "queue"]
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
/// mis-update the seeded one. Seeds one subgroup on payments/webhooks, then
/// feeds a transcript that explicitly switches topic to a Q3 retrospective.
/// A well-behaved extractor mints a fresh subgroup for the new topic and leaves
/// the seeded payments summary untouched.
///
/// This is the topic-shift half of the Flux-grouping checkbox: paired with the
/// continuation test above, together they prove the extractor makes the
/// attach-vs-grow-vs-create decision via `plan_interpretation_ops_with_context`
/// routing on the model's proposed `id`.
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

// ---- highest-level Flux integration: scoped incremental grouping lifecycle --

/// Lower-cased `summary` of the subgroup whose base URI is `base`, or "" if
/// absent. The read the lifecycle assertions compare across passes.
async fn subgroup_summary(perspective: &PerspectiveInstance, base: &str) -> String {
    model_instances(perspective, "ConversationSubgroup", &["name", "summary"])
        .await
        .iter()
        .find(|r| r.get("id").and_then(|i| i.as_str()) == Some(base))
        .and_then(|r| r.get("summary").and_then(|s| s.as_str()))
        .unwrap_or("")
        .to_lowercase()
}

fn contains_any(haystack: &str, keywords: &[&str]) -> bool {
    keywords.iter().any(|k| haystack.contains(k))
}

/// Wire a freshly-minted subgroup into the channel sub-graph
/// (`<channel> ns://contains <base>`) — the containment a Flux/AutoProcessor
/// run writes so the next scoped pass can see (and update) it.
async fn link_under_channel(
    perspective: &mut PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    channel: &str,
    contains: &str,
    base: &str,
) {
    use crate::types::LinkStatus;
    perspective
        .add_link(
            Link {
                source: channel.into(),
                predicate: Some(contains.into()),
                target: base.into(),
            },
            LinkStatus::Local,
            None,
            ctx,
            None,
        )
        .await
        .expect("link subgroup under channel");
}

/// The highest-level Flux grouping integration test: a scoped, incremental
/// conversation-grouping lifecycle inside one channel, driven pass-by-pass the
/// way the AutoProcessor (#885) will drive it once messages accumulate past a
/// batch threshold. It proves the interpretation layer supports the full Flux
/// "grouping" loop **under a channel scope**:
///
///   0. A decoy subgroup exists OUTSIDE the channel (never linked under it).
///      The scope must hide it — it is never seen and never modified.
///   1. Empty channel + topic-A turns  → a NEW subgroup is minted.
///   2. Topic-A continues              → the SAME subgroup's summary is updated
///                                        (id reused, no duplicate).
///   3. Topic switches to B            → a fresh SECOND subgroup is minted;
///                                        subgroup #1 is left untouched.
///   4. Topic-B continues              → only subgroup #2's summary is updated;
///                                        subgroup #1 is still untouched.
///
/// Scoping is the load-bearing mechanism: every pass reads existing subgroups
/// through `existing_instance_context(scope = channel)`, so the model only ever
/// sees this channel's subgroups (never the decoy), and each create-vs-update
/// decision is taken against exactly that set. Between passes the test links
/// each freshly-minted subgroup under the channel, the containment the
/// AutoProcessor writes in production. (Message batching itself is #885's job;
/// here each pass stands in for one already-accumulated batch.)
///
/// Real-LLM (gemma3:12b on the configured Ollama). Wrapped in a retry loop —
/// four sequential model calls compound non-determinism — with a fresh graph
/// per attempt.
#[tokio::test]
async fn e2e_flux_grouping_scoped_incremental_lifecycle() {
    use crate::perspectives::interpretation::existing_instance_context;
    use crate::perspectives::model_query::types::Scope;

    let channel = "soa://channel/general";
    let contains = "ns://contains";
    let decoy_base = "soa://other/subgroup/decoy";
    let attempts = 3u8;
    let mut last_err: Option<String> = None;

    for attempt in 1..=attempts {
        let (mut perspective, shapes, ctx) =
            setup_interpretation_e2e(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;
        let sg_shape = &shapes[0];
        let scope = Scope::Raw {
            id: channel.into(),
            predicate: contains.into(),
        };

        // Decoy subgroup on an unrelated topic, deliberately NOT under the
        // channel — the scope must keep it invisible for the whole run.
        seed_instance_with_props(
            &mut perspective,
            &ctx,
            sg_shape,
            decoy_base,
            serde_json::json!({
                "name": "Onboarding UX",
                "summary": "Ideas for smoothing the first-run experience for brand-new users."
            }),
        )
        .await;
        let decoy_summary_0 = subgroup_summary(&perspective, decoy_base).await;

        macro_rules! fail_attempt {
            ($($arg:tt)*) => {{
                last_err = Some(format!("attempt {attempt}/{attempts}: {}", format!($($arg)*)));
                eprintln!("[e2e] {}", last_err.as_ref().unwrap());
                continue;
            }};
        }

        // ---- Pass 1: empty channel, topic A (payments/webhooks) → create ----
        let bases1 = run_interpretation_e2e_scoped(
            &mut perspective,
            &shapes,
            &[
                ("Ana", "Our webhook retries keep dropping during payment outages — we lose the failed events entirely."),
                ("Ben", "Right, the payments queue has no way to replay what got dropped last time."),
            ],
            &ctx,
            Some(&scope),
        )
        .await;
        for b in &bases1 {
            link_under_channel(&mut perspective, &ctx, channel, contains, b).await;
        }
        let scoped1 = existing_instance_context(&perspective, &shapes, Some(&scope))
            .await
            .expect("scoped ctx p1");
        if scoped1.len() != 1 {
            fail_attempt!(
                "pass1 expected exactly 1 channel subgroup, got {}",
                scoped1.len()
            );
        }
        let sg1 = scoped1.keys().next().unwrap().clone();
        let sum1_p1 = subgroup_summary(&perspective, &sg1).await;
        if !contains_any(
            &sum1_p1,
            &["webhook", "payment", "retry", "queue", "replay", "payload"],
        ) {
            fail_attempt!("pass1 subgroup summary not about payments: {sum1_p1:?}");
        }

        // ---- Pass 2: topic A continues → update the SAME subgroup ----
        run_interpretation_e2e_scoped(
            &mut perspective,
            &shapes,
            &[
                ("Ana", "Following up on the webhook drops — let's persist failed payloads to a retry ledger so we can replay them after an outage."),
                ("Ben", "A ledger tied to the payments queue would let us reconstruct exactly what dropped."),
            ],
            &ctx,
            Some(&scope),
        )
        .await;
        let scoped2 = existing_instance_context(&perspective, &shapes, Some(&scope))
            .await
            .expect("scoped ctx p2");
        if scoped2.len() != 1 {
            fail_attempt!(
                "pass2 expected still 1 channel subgroup (id reuse), got {}",
                scoped2.len()
            );
        }
        let sum1_p2 = subgroup_summary(&perspective, &sg1).await;
        if sum1_p2 == sum1_p1
            || !contains_any(&sum1_p2, &["ledger", "persist", "replay", "payload"])
        {
            fail_attempt!(
                "pass2 summary did not grow with new detail: before={sum1_p1:?} after={sum1_p2:?}"
            );
        }

        // ---- Pass 3: topic switches to B (Q3 retro / Holograph) → new subgroup ----
        let bases3 = run_interpretation_e2e_scoped(
            &mut perspective,
            &shapes,
            &[
                ("Ana", "Totally different subject: Josh wants a Q3 retrospective on how Holograph shipped — nothing to do with payments."),
                ("Ben", "Good call, let's block a Wednesday for retro prep and invite the mobile team."),
            ],
            &ctx,
            Some(&scope),
        )
        .await;
        for b in &bases3 {
            if b != &sg1 {
                link_under_channel(&mut perspective, &ctx, channel, contains, b).await;
            }
        }
        let scoped3 = existing_instance_context(&perspective, &shapes, Some(&scope))
            .await
            .expect("scoped ctx p3");
        if scoped3.len() != 2 {
            fail_attempt!(
                "pass3 expected 2 channel subgroups after topic shift, got {}",
                scoped3.len()
            );
        }
        let sum1_p3 = subgroup_summary(&perspective, &sg1).await;
        if sum1_p3 != sum1_p2 || contains_any(&sum1_p3, &["retro", "holograph", "q3"]) {
            fail_attempt!("pass3 polluted subgroup #1 on the topic shift: {sum1_p3:?}");
        }
        let sg2 = scoped3.keys().find(|k| **k != sg1).unwrap().clone();
        let sum2_p3 = subgroup_summary(&perspective, &sg2).await;
        if !contains_any(&sum2_p3, &["retro", "holograph", "q3", "retrospective"]) {
            fail_attempt!("pass3 new subgroup not about the retro topic: {sum2_p3:?}");
        }

        // ---- Pass 4: topic B continues → update ONLY subgroup #2 ----
        run_interpretation_e2e_scoped(
            &mut perspective,
            &shapes,
            &[
                ("Ana", "For the retro: let's capture what slowed Holograph down — the sync-module rewrites cost us two weeks."),
                ("Ben", "Agreed, and we should write up the kitsune substrate lessons while they're fresh."),
            ],
            &ctx,
            Some(&scope),
        )
        .await;
        let scoped4 = existing_instance_context(&perspective, &shapes, Some(&scope))
            .await
            .expect("scoped ctx p4");
        if scoped4.len() != 2 {
            fail_attempt!(
                "pass4 expected still 2 channel subgroups, got {}",
                scoped4.len()
            );
        }
        let sum2_p4 = subgroup_summary(&perspective, &sg2).await;
        let sum1_p4 = subgroup_summary(&perspective, &sg1).await;
        if sum2_p4 == sum2_p3
            || !contains_any(&sum2_p4, &["sync", "kitsune", "substrate", "week", "slow"])
        {
            fail_attempt!(
                "pass4 subgroup #2 summary did not grow: before={sum2_p3:?} after={sum2_p4:?}"
            );
        }
        if sum1_p4 != sum1_p2 || contains_any(&sum1_p4, &["retro", "holograph", "kitsune", "sync"])
        {
            fail_attempt!("pass4 disturbed subgroup #1: {sum1_p4:?}");
        }

        // ---- Scope isolation: the decoy outside the channel was never touched ----
        let decoy_now = subgroup_summary(&perspective, decoy_base).await;
        if decoy_now != decoy_summary_0 {
            fail_attempt!("decoy subgroup outside the scope was modified: {decoy_summary_0:?} -> {decoy_now:?}");
        }

        // Full scoped lifecycle held on this attempt.
        return;
    }
    panic!(
        "scoped incremental grouping e2e failed after {attempts} attempts: {}",
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
    // Semantic dedup embeds identity strings through AIService's own (local,
    // CPU) embedding model — register it before the run.
    super::interpretation_test_support::register_interpretation_embedding_model().await;
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
        run_one_pass, PassOutcome, PendingTurn, WatcherState,
    };
    use crate::perspectives::interpretation::{
        gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
    };
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
        source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
        base_prefix: None,
        interpretation_classes: vec!["ns://Intention".into()],
        debounce_ms: 50,
        batch_min: 1,
        batch_max: 32,
        max_wait_ms: None,
        claim_ttl_ms: 60_000,
        dedup_strategy_json: None,
        source_window_ms: None,
        existing_scope: None,
        mint_scope: None,
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
    for turn in &transcript {
        watcher.record_item(
            &cfg_loaded.processor_id,
            PendingTurn::from_transcript(turn),
            now_ms,
        );
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

    let outcome = run_one_pass(&mut perspective, cfg_loaded, &batch, drain_at, &ctx, false)
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
        run_one_pass, PassOutcome, PendingTurn, WatcherState,
    };
    use crate::perspectives::interpretation::{
        gather_transcript_sparql, BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY,
    };
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

    let source_scope = BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;

    let intent_cfg = AutoProcessorConfig {
        processor_id: "pc-intent-proc".into(),
        source_scope_query: source_scope.into(),
        base_prefix: None,
        interpretation_classes: vec!["ns://Intention".into()],
        debounce_ms: 50,
        batch_min: 1,
        batch_max: 32,
        max_wait_ms: None,
        claim_ttl_ms: 60_000,
        dedup_strategy_json: None,
        source_window_ms: None,
        existing_scope: None,
        mint_scope: None,
    };
    let task_cfg = AutoProcessorConfig {
        processor_id: "pc-task-proc".into(),
        source_scope_query: source_scope.into(),
        base_prefix: None,
        interpretation_classes: vec!["ns://Task".into()],
        debounce_ms: 50,
        batch_min: 1,
        batch_max: 32,
        max_wait_ms: None,
        claim_ttl_ms: 60_000,
        dedup_strategy_json: None,
        source_window_ms: None,
        existing_scope: None,
        mint_scope: None,
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
        for turn in &transcript {
            watcher.record_item(
                &cfg_ref.processor_id,
                PendingTurn::from_transcript(turn),
                now_ms,
            );
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
        let outcome = run_one_pass(&mut perspective, cfg_ref, &batch, drain_at, &ctx, false)
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

// ---- auto_processor P-B2c: high-level, signal-driven, no manual interpretation

/// The clean high-level auto-processor integration test: write a processor
/// config + seed a channel's messages, then let the **real watch-loop tick**
/// (`run_auto_processor_tick`) do everything — gather the transcript, debounce,
/// claim, run the LLM, write the instances — while the test only *observes* it
/// through the [`events`](crate::perspectives::auto_processor::events) signals
/// and asserts the outcome. No manual `run_interpretation`, no manual transcript
/// wrangling: exactly what a Flux channel does when new messages arrive.
///
/// Proves: (a) the loop settles and drains a batch (the debounce fix — a
/// re-gathered duplicate no longer resets the clock), (b) it emits the pass
/// lifecycle `BatchReady → Claimed → GatheringTranscript → RunningInterpretation
/// → Processed`, awaitable by a listener, and (c) it actually creates the
/// ConversationSubgroup on the perspective.
///
/// Real-LLM (gemma3:12b). Retry loop for model non-determinism.
#[tokio::test]
async fn auto_processor_high_level_signal_driven_pass() {
    use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
    use crate::perspectives::auto_processor::events::{
        next_event_matching, subscribe, AutoProcessorStep,
    };
    use crate::perspectives::auto_processor::watcher::WatcherState;
    use crate::types::{Link, LinkStatus};
    use std::time::Duration;

    let processor_id = "flux-channel-proc";
    let attempts = 3u8;
    let mut last_err: Option<String> = None;

    for attempt in 1..=attempts {
        let (mut perspective, _shapes, ctx) =
            setup_interpretation_e2e(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;

        // Seed a channel's worth of messages on one topic — link pairs, exactly
        // as a Flux channel perspective holds them.
        for (uri, author, body) in [
            ("msg://c1", "did:key:ana", "Our webhook retries keep dropping during payment outages — we lose the failed events."),
            ("msg://c2", "did:key:ben", "Right, the payments queue has no way to replay what got dropped last time."),
        ] {
            for (pred, target) in [
                ("ns://body", format!("literal:string:{body}")),
                ("ns://author", author.to_string()),
            ] {
                perspective
                    .add_link(
                        Link { source: uri.into(), predicate: Some(pred.into()), target },
                        LinkStatus::Local,
                        None,
                        &ctx,
                    )
                    .await
                    .expect("seed channel message link");
            }
        }

        // Write the processor into the shared graph — the loop reads it back.
        let cfg = AutoProcessorConfig {
            processor_id: processor_id.into(),
            source_scope_query:
                crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://ConversationSubgroup".into()],
            debounce_ms: 50,
            batch_min: 2,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
        };
        write_processor(&mut perspective, &cfg, &ctx)
            .await
            .expect("write_processor");

        // Observe the pass purely through the event stream.
        let mut rx = subscribe().await;
        let mut watcher = WatcherState::new();

        // Tick 1 records the two turns (debounce not yet elapsed → no drain).
        perspective
            .run_auto_processor_tick(&mut watcher, 1_000, &ctx)
            .await;
        // Tick 2, past the debounce window: the re-gathered duplicates don't
        // reset the clock (the fix), so the batch drains and the real pass runs.
        perspective
            .run_auto_processor_tick(&mut watcher, 1_100, &ctx)
            .await;

        // Await the terminal signal — this is what a WS client / test waits on
        // instead of polling the graph. Scope the predicate to THIS perspective:
        // `emit` publishes to a process-global channel, so another perspective
        // reusing the same `processor_id` could otherwise match.
        let persp_uuid = perspective.uuid.clone();
        let processed_ev = next_event_matching(&mut rx, Duration::from_secs(90), |e| {
            e.processor_id == processor_id
                && e.perspective_uuid == persp_uuid
                && e.step == AutoProcessorStep::Processed
        })
        .await;
        let Some(ev) = processed_ev else {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: no `Processed` signal within timeout"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        };
        if ev.bases.is_empty() {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: Processed signal carried no bases"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }

        // The subgroup must actually exist on the perspective, with a
        // payments-flavoured summary — created entirely by the loop.
        let rows =
            model_instances(&perspective, "ConversationSubgroup", &["name", "summary"]).await;
        if rows.is_empty() {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: loop signalled Processed but no ConversationSubgroup persisted"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }
        let any_payments = rows.iter().any(|r| {
            let name = r
                .get("name")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_lowercase();
            let summary = r
                .get("summary")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_lowercase();
            contains_any(&name, &["payment", "webhook", "retry", "queue"])
                || contains_any(
                    &summary,
                    &["payment", "webhook", "retry", "queue", "replay"],
                )
        });
        if !any_payments {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: subgroup created but not about the channel topic: {rows:#?}"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }

        // Full high-level pass held on this attempt.
        return;
    }
    panic!(
        "auto_processor high-level signal-driven e2e failed after {attempts} attempts: {}",
        last_err.unwrap_or_default()
    );
}

/// Two users, one executor (ad4m multi-tenancy), driven by the REAL spawned
/// background loop: the `ProcessingClaim` must stop the same channel batch from
/// being processed twice. This is the step toward the full two-executor
/// neighbourhood test (#881); here both "peers" live in one process and share
/// the perspective graph, so a claim written by one is immediately visible to
/// the other.
///
/// Rather than driving `run_auto_processor_tick` by hand, this spawns the actual
/// `auto_processor_watch_loop` — one per managed user — so it exercises the
/// perspective's real autonomous loop (poll → debounce → claim → LLM → write).
/// The claim election is min-DID over active claimants, so the smaller-DID
/// user's loop is started first: it is the sole claimant, wins, and processes;
/// then the other user's loop is started. Same-process, the winner's `sources`
/// are already in the graph, so the loser typically skips rather than
/// `BackedOff`. Load-bearing assertions: winner `Processed`, loser never
/// `Processed`, and exactly ONE ConversationSubgroup.
///
/// Real-LLM (gemma3:12b). Retry loop for model non-determinism.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn auto_processor_two_users_one_executor_no_double_processing() {
    use crate::agent::{did_for_context, AgentContext, AgentService};
    use crate::perspectives::auto_processor::config::{write_processor, AutoProcessorConfig};
    use crate::perspectives::auto_processor::events::{
        next_event_matching, subscribe, AutoProcessorStep,
    };
    use crate::types::{Link, LinkStatus};
    use std::time::Duration;

    let processor_id = "shared-channel-proc";
    let attempts = 3u8;
    let mut last_err: Option<String> = None;

    for attempt in 1..=attempts {
        let (mut perspective, _shapes, ctx_main) =
            setup_interpretation_e2e(&[("ConversationSubgroup", CONVERSATION_SUBGROUP_SDNA)]).await;

        // Two managed users on the one executor — distinct DIDs so the claim
        // election has two real candidates.
        AgentService::ensure_user_key_exists("alice@e2e").expect("user A key");
        AgentService::ensure_user_key_exists("bob@e2e").expect("user B key");
        let ctx_a = AgentContext::for_user_email("alice@e2e".to_string());
        let ctx_b = AgentContext::for_user_email("bob@e2e".to_string());
        let did_a = did_for_context(&ctx_a).expect("did A");
        let did_b = did_for_context(&ctx_b).expect("did B");
        assert_ne!(did_a, did_b, "two managed users must have distinct DIDs");

        // Smaller-DID user is the claim winner → start its loop first.
        let (ctx_win, did_win, ctx_lose, did_lose) = if did_a <= did_b {
            (ctx_a, did_a, ctx_b, did_b)
        } else {
            (ctx_b, did_b, ctx_a, did_a)
        };

        // Seed a channel's messages + register one processor.
        for (uri, author, body) in [
            ("msg://s1", "did:key:ana", "Our webhook retries keep dropping during payment outages — we lose the failed events."),
            ("msg://s2", "did:key:ben", "Right, the payments queue has no way to replay what got dropped last time."),
        ] {
            for (pred, target) in [
                ("ns://body", format!("literal:string:{body}")),
                ("ns://author", author.to_string()),
            ] {
                perspective
                    .add_link(
                        Link { source: uri.into(), predicate: Some(pred.into()), target },
                        LinkStatus::Local,
                        None,
                        &ctx_main,
                    )
                    .await
                    .expect("seed channel message link");
            }
        }
        let cfg = AutoProcessorConfig {
            processor_id: processor_id.into(),
            source_scope_query:
                crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
            base_prefix: None,
            interpretation_classes: vec!["ns://ConversationSubgroup".into()],
            debounce_ms: 100,
            batch_min: 2,
            batch_max: 32,
            max_wait_ms: None,
            claim_ttl_ms: 60_000,
            dedup_strategy_json: None,
            source_window_ms: None,
            existing_scope: None,
            mint_scope: None,
        };
        write_processor(&mut perspective, &cfg, &ctx_main)
            .await
            .expect("write_processor");

        // Scope event predicates to THIS perspective — `emit` is process-global,
        // so a concurrent test/perspective on the same `processor_id` must not match.
        let persp_uuid = perspective.uuid.clone();
        let mut rx = subscribe().await;

        // Spawn the winner's REAL background loop — it polls, debounces, claims
        // (sole candidate), runs the LLM and writes the subgroup autonomously.
        let p_win = perspective.clone();
        let win_loop = tokio::spawn(async move { p_win.auto_processor_watch_loop(ctx_win).await });
        let processed = next_event_matching(&mut rx, Duration::from_secs(90), |e| {
            e.processor_id == processor_id
                && e.perspective_uuid == persp_uuid
                && e.step == AutoProcessorStep::Processed
                && e.agent_did.as_deref() == Some(did_win.as_str())
        })
        .await;

        // Now start the loser's loop. Same-process, the winner's `sources` are
        // already in the graph, so the loser usually skips the batch outright
        // rather than reaching the claim — `BackedOff` is one valid outcome,
        // not a required one. What must hold either way is that the loser
        // never runs the pass, so wait for `Processed` from *them* and require
        // the wait to time out.
        let p_lose = perspective.clone();
        let lose_loop =
            tokio::spawn(async move { p_lose.auto_processor_watch_loop(ctx_lose).await });
        let loser_processed = next_event_matching(&mut rx, Duration::from_secs(4), |e| {
            e.processor_id == processor_id
                && e.perspective_uuid == persp_uuid
                && e.step == AutoProcessorStep::Processed
                && e.agent_did.as_deref() == Some(did_lose.as_str())
        })
        .await;

        // Stop both background loops (shared is_teardown flag).
        perspective.teardown_background_tasks().await;
        let _ = win_loop.await;
        let _ = lose_loop.await;

        // Not a retryable model wobble: if the loser ran the pass at all, the
        // claim + cursor failed to coordinate and no amount of retrying is
        // going to make that correct.
        assert!(
            loser_processed.is_none(),
            "loser ({did_lose}) processed the batch — claim + cursor did not coordinate"
        );

        if processed.is_none() {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: winner ({did_win}) never signalled Processed"
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }

        // Load-bearing: exactly ONE subgroup across the two loops.
        let subgroups = model_instances(&perspective, "ConversationSubgroup", &["name"]).await;
        if subgroups.len() != 1 {
            last_err = Some(format!(
                "attempt {attempt}/{attempts}: expected exactly 1 subgroup (claim must dedup), got {}: {subgroups:#?}",
                subgroups.len()
            ));
            eprintln!("[e2e] {}", last_err.as_ref().unwrap());
            continue;
        }

        // No double-processing held on this attempt.
        return;
    }
    panic!(
        "two-user (real background loop) no-double-processing e2e failed after {attempts} attempts: {}",
        last_err.unwrap_or_default()
    );
}

/// Real-telepresence authorship election (Option A — only *participants* process).
/// On a SHARED perspective with real managed users whose presence flows through
/// `online_agents()`, the pass must go to the **first online message-author in
/// message order**, skipping offline authors, and a peer that is not that author
/// stands down — while a peer whose batch has *no* online author waits rather
/// than processing a channel it doesn't participate in. No LLM: `run_one_pass`
/// returns the election verdict before the interpretation step.
#[tokio::test]
async fn auto_processor_election_only_online_participants_process() {
    use crate::agent::{did_for_context, AgentContext, AgentService};
    use crate::db::Ad4mDb;
    use crate::perspectives::auto_processor::config::AutoProcessorConfig;
    use crate::perspectives::auto_processor::watcher::{run_one_pass, PassOutcome, PendingTurn};
    use crate::perspectives::interpretation::BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;

    let (mut perspective, _shapes, _ctx_main) = setup_perspective_no_llm(&[]).await;

    // Four managed participants, each a real key + a listable user row (so the
    // co-located `online_agents` path can report their presence).
    let mk = |email: &str| -> (AgentContext, String) {
        AgentService::ensure_user_key_exists(email).expect("user key");
        let ctx = AgentContext::for_user_email(email.to_string());
        let did = did_for_context(&ctx).expect("did");
        Ad4mDb::with_global_instance(|db| db.add_user(email, &did, "pw")).expect("add_user");
        (ctx, did)
    };
    let (_ctx_carol, did_carol) = mk("carol@e2e");
    let (_ctx_bob, did_bob) = mk("bob@e2e");
    let (ctx_alice, did_alice) = mk("alice@e2e");
    let (ctx_dave, did_dave) = mk("dave@e2e");

    // Make the perspective a shared neighbourhood owned by all four, so
    // `online_agents()` reports co-located presence instead of erroring.
    {
        let mut h = perspective.persisted.lock().await;
        h.shared_url = Some("test://neighbourhood".into());
        h.owners = Some(vec![
            did_carol.clone(),
            did_bob.clone(),
            did_alice.clone(),
            did_dave.clone(),
        ]);
    }
    // "Log in" bob, alice, dave (recent last_seen ⇒ online). carol stays offline.
    for email in ["bob@e2e", "alice@e2e", "dave@e2e"] {
        Ad4mDb::with_global_instance(|db| db.update_user_last_seen(email)).expect("last_seen");
    }

    let cfg = AutoProcessorConfig {
        processor_id: "election".into(),
        source_scope_query: BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY.into(),
        base_prefix: None,
        interpretation_classes: vec!["ns://ConversationSubgroup".into()],
        debounce_ms: 0,
        batch_min: 1,
        batch_max: 32,
        max_wait_ms: None,
        claim_ttl_ms: 60_000,
        dedup_strategy_json: None,
        source_window_ms: None,
        existing_scope: None,
        mint_scope: None,
    };

    // Case 1 — a batch authored by carol (offline) then bob (online), in that
    // message order. alice is online but is NOT the first online author, so she
    // must stand down for bob (carol is skipped because she is offline).
    let batch = vec![
        PendingTurn {
            id: "m1".into(),
            speaker: did_carol.clone(),
            text: "m1".into(),
            timestamp: "t1".into(),
        },
        PendingTurn {
            id: "m2".into(),
            speaker: did_bob.clone(),
            text: "m2".into(),
            timestamp: "t2".into(),
        },
    ];
    let out = run_one_pass(&mut perspective, &cfg, &batch, 1_000, &ctx_alice, false)
        .await
        .expect("alice pass");
    assert!(
        matches!(out, PassOutcome::NotCandidate { ref winner } if winner == &did_bob),
        "alice stands down for the first ONLINE author (bob); carol offline is skipped — got {out:?}"
    );

    // Case 2 — a batch whose only author (carol) is offline. dave is online but
    // authored nothing here, so nobody processes: the pass waits for a
    // participant rather than letting a bystander run it.
    let batch2 = vec![PendingTurn {
        id: "solo".into(),
        speaker: did_carol.clone(),
        text: "solo".into(),
        timestamp: "t".into(),
    }];
    let out2 = run_one_pass(&mut perspective, &cfg, &batch2, 2_000, &ctx_dave, false)
        .await
        .expect("dave pass");
    assert!(
        matches!(out2, PassOutcome::AwaitingAuthor),
        "no online author for the batch ⇒ wait (bystander dave must not process) — got {out2:?}"
    );
}

/// Deterministic proof that the `DedupStrategy::Semantic` path — AIService's
/// local Bert embeddings + cosine threshold — actually drops a paraphrased
/// duplicate while keeping an unrelated item. The LLM-driven e2e above only
/// exercises this when the model happens to re-propose the seeded item; this
/// test removes that non-determinism by feeding hand-crafted proposals straight
/// into `filter_already_present_with_strategy`.
#[tokio::test]
async fn e2e_semantic_dedup_pure_drops_paraphrase_keeps_distinct() {
    use crate::perspectives::interpretation::{
        filter_already_present_with_strategy, DedupStrategy, ExistingInstances, InstanceContext,
        ProposedInstance,
    };
    use std::collections::{BTreeMap, HashMap};

    // Reuse the standard harness to initialise the DB + AIService global, then
    // register the embedding model. (The LLM this also registers is unused here.)
    let _ = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
    super::interpretation_test_support::register_interpretation_embedding_model().await;

    let existing: ExistingInstances = [(
        "soa://existing/task/webrtc".to_string(),
        vec![InstanceContext {
            id: "soa://existing/task/webrtc".to_string(),
            title: "Finish the WebRTC call module".to_string(),
            class: "Task".to_string(),
            properties: BTreeMap::new(),
        }],
    )]
    .into_iter()
    .collect();
    let identity_props: HashMap<String, String> =
        HashMap::from([("Task".to_string(), "title".to_string())]);

    let mk = |title: &str| ProposedInstance {
        class: "Task".to_string(),
        id: None,
        props: HashMap::from([(
            "title".to_string(),
            serde_json::Value::String(title.to_string()),
        )]),
    };
    let proposed = vec![
        mk("Wrap up the WebRTC calling module"), // paraphrase of the seed → drop
        mk("Update the CI documentation"),       // unrelated → keep
    ];

    let kept = filter_already_present_with_strategy(
        proposed,
        &existing,
        &identity_props,
        &DedupStrategy::Semantic {
            model: "interpretation-embed".to_string(),
            threshold: 0.6,
        },
    )
    .await
    .expect("semantic dedup");

    let titles: Vec<&str> = kept
        .iter()
        .filter_map(|p| p.props.get("title").and_then(|v| v.as_str()))
        .collect();
    assert!(
        titles.iter().any(|t| t.contains("CI documentation")),
        "unrelated task must survive semantic dedup; got {titles:?}"
    );
    assert!(
        !titles.iter().any(|t| t.contains("WebRTC")),
        "paraphrased WebRTC task must be dropped by semantic dedup; got {titles:?}"
    );
}

/// Full `run_interpretation` e2e that actually exercises the parent-scope
/// plumbing end-to-end (prompt build → LLM → dedup → write), not just the
/// isolated `existing_instance_context` helper. One existing Task lives under
/// parent B. A transcript restates it. Semantic dedup (Bert) removes wording
/// sensitivity so the assertions turn purely on *scope*:
///   - scoped to parent B (contains the seed): the restatement is deduped — no
///     fresh `ext/` task is minted (robust: holds whether or not the LLM
///     re-proposes it).
///   - scoped to parent A (empty): the seed is out of scope, so the restatement
///     is created as a new instance. Retried until the model proposes it.
#[tokio::test]
async fn e2e_run_interpretation_honours_parent_scope() {
    use crate::perspectives::interpretation::{
        run_interpretation_with_strategy, DedupStrategy, TranscriptTurn,
    };
    use crate::perspectives::model_query::types::Scope;
    use crate::types::{Link, LinkStatus};

    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
    super::interpretation_test_support::register_interpretation_embedding_model().await;

    // Existing task under parent B only.
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "soa://tree-b/task/staging-db",
        "Provision the staging database",
    )
    .await;
    perspective
        .add_link(
            Link {
                source: "soa://parent/b".into(),
                predicate: Some("ns://contains".into()),
                target: "soa://tree-b/task/staging-db".into(),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("parent link");

    let transcript = vec![TranscriptTurn::from_speaker_text(
        "Nico".to_string(),
        "Reminder for the team: we still need to provision the staging database — it's blocking QA."
            .to_string(),
    )];
    let semantic = DedupStrategy::Semantic {
        model: "interpretation-embed".to_string(),
        threshold: 0.6,
    };
    let in_scope = Scope::Raw {
        id: "soa://parent/b".into(),
        predicate: "ns://contains".into(),
    };
    let out_scope = Scope::Raw {
        id: "soa://parent/a".into(),
        predicate: "ns://contains".into(),
    };

    let minted_staging = |rows: &[serde_json::Value]| -> usize {
        rows.iter()
            .filter(|r| {
                r.get("id")
                    .and_then(|i| i.as_str())
                    .map(|id| id.starts_with("soa://ext/"))
                    .unwrap_or(false)
                    && r.get("title")
                        .and_then(|t| t.as_str())
                        .map(|t| t.to_lowercase().contains("staging"))
                        .unwrap_or(false)
            })
            .count()
    };

    // In-scope: the restatement must be deduped against the existing seed — no
    // fresh ext/ task minted.
    run_interpretation_with_strategy(
        &mut perspective,
        &shapes,
        &transcript,
        "soa://ext/",
        &ctx,
        &semantic,
        Some(&in_scope),
    )
    .await
    .expect("in-scope run");
    let rows = model_instances(&perspective, "Task", &["title"]).await;
    assert_eq!(
        minted_staging(&rows),
        0,
        "in-scope run must dedup the restatement against the parent-B seed; minted {:#?}",
        rows
    );

    // Out-of-scope: the parent-B seed is invisible, so the clearly-restated task
    // is a genuinely new instance. Retry until the model proposes it.
    let mut minted = 0;
    for _ in 0..4 {
        run_interpretation_with_strategy(
            &mut perspective,
            &shapes,
            &transcript,
            "soa://ext/",
            &ctx,
            &semantic,
            Some(&out_scope),
        )
        .await
        .expect("out-of-scope run");
        let rows = model_instances(&perspective, "Task", &["title"]).await;
        minted = minted_staging(&rows);
        if minted >= 1 {
            break;
        }
    }
    assert!(
        minted >= 1,
        "out-of-scope run must NOT dedup against the parent-B seed — expected a new staging task under ext/"
    );
}
