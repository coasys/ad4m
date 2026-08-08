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
    let (p, shapes, bases) = run_e2e(
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

    let bases = run_interpretation_e2e(
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
    assert_persisted(&perspective, &shapes, &bases).await;

    // New instances land under the interpretation prefix, never on the seeded bases.
    // (Where an instance is *minted* is inherently a placement property, so these
    // two checks stay on `bases`.)
    assert!(
        bases.iter().all(|base| base.starts_with("soa://ext/")),
        "new instances must be minted under soa://ext/, not reuse existing bases"
    );
    assert!(
        bases
            .iter()
            .all(|base| !base.starts_with("soa://existing/")),
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
#[tokio::test]
async fn e2e_does_not_recreate_existing_task() {
    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;
    let task_shape = &shapes[0];

    let existing_title = "Finish the WebRTC call module";
    seed_instance(
        &mut perspective,
        &ctx,
        task_shape,
        "soa://existing/task/webrtc",
        existing_title,
    )
    .await;

    let bases = run_interpretation_e2e(
        &mut perspective,
        &shapes,
        &[
            // Restates the existing task…
            (
                "Nico",
                "Reminder: James still needs to finish the WebRTC call module.",
            ),
            // …and introduces a brand-new one.
            (
                "James",
                "Right. I'll also write the end-to-end test for the call module afterwards.",
            ),
        ],
        &ctx,
    )
    .await;
    assert_persisted(&perspective, &shapes, &bases).await;

    // The already-present task is never duplicated: the seeded title appears
    // exactly once in the final graph, proving the restatement was deduped.
    let titles = graph_titles_lower(&perspective, &shapes).await;
    let seeded_lower = existing_title.to_lowercase();
    let dup_count = titles.iter().filter(|t| **t == seeded_lower).count();
    assert_eq!(
        dup_count, 1,
        "seeded task must appear exactly once (not recreated); graph titles = {titles:?}"
    );
    // A new task should still have been extracted (the e2e test task), so the
    // graph holds the seeded task plus at least one new one.
    let counts = graph_count_by_type(&perspective, &shapes).await;
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 2,
        "expected the seeded task + a newly extracted one; got {counts:?}"
    );
}
