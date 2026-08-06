//! Real-LLM end-to-end tests for generic extraction.
//!
//! These are the "look, the whole thing works" tests: a transcript goes in, a
//! real local model runs, and typed SoA instances come out and are persisted.
//! Split into their own file (from the pure unit tests in `extraction.rs`) so a
//! reviewer can read *just this* to understand what the feature does end-to-end.
//!
//! They talk to an OpenAI-compatible endpoint (Ollama), NOT the embedded CUDA
//! LLM — so no GPU build is needed, only a reachable model. Endpoint + model are
//! env-overridable (`EXTRACTION_E2E_BASE_URL` / `EXTRACTION_E2E_MODEL` /
//! `EXTRACTION_E2E_API_KEY`); defaults hit Ollama at `localhost:11434` with
//! `gemma3:12b` (fits the GPU, ~10s for the suite, Flux's summary model). On CI
//! (self-hosted runner = Marvin) that endpoint is local; from a dev box, tunnel
//! it (`ssh -L 11434:localhost:11434 marvin`).
//!
//! Requires that endpoint to be up — they are NOT `#[ignore]`d, so a `cargo test`
//! with no model reachable will fail here by design (that is the CI signal).
//! Run just this suite: `cargo test --release --lib perspectives::extraction_e2e
//! -- --test-threads=1 --nocapture`.

#![cfg(test)]

use super::extraction::existing_instance_titles;
use super::extraction_test_support::*;

// ---- basic per-class extraction (DRY via the shared `run_e2e` harness) ------

/// Intention + Belief: an intent with an owner and a claim.
#[tokio::test]
async fn e2e_intention_and_belief() {
    let (p, placements) = run_e2e(
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
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
    assert!(
        counts.get("intention").copied().unwrap_or(0) >= 1,
        "expected an intention; got {counts:?}"
    );
    assert!(
        counts.get("belief").copied().unwrap_or(0) >= 1,
        "expected a belief; got {counts:?}"
    );
    // The intention should carry Nico as owner.
    let owner_is_nico = placements.iter().any(|(_, links)| {
        links.iter().any(|l| {
            l.predicate.as_deref() == Some("ns://owner") && l.target.to_lowercase().contains("nico")
        })
    });
    assert!(owner_is_nico, "expected the intention to be owned by Nico");
}

/// Task-tracking conversation -> only Tasks, with owners. Three assignments in
/// the transcript should yield 2–4 tasks (LLM may merge/split slightly).
#[tokio::test]
async fn e2e_task_tracking_counts() {
    let (p, placements) = run_e2e(
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
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
    let tasks = counts.get("task").copied().unwrap_or(0);
    assert!(
        (2..=4).contains(&tasks),
        "expected 2-4 tasks from three assignments; got {counts:?}"
    );
    assert!(
        counts.keys().all(|k| k == "task"),
        "only Task was offered; got {counts:?}"
    );
    let owners = placements
        .iter()
        .filter(|(_, links)| {
            links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://owner"))
        })
        .count();
    assert!(owners >= 1, "expected at least one task to carry an owner");
}

/// Mixed epistemic conversation -> the three distinct modalities. The question
/// (ends in "?") is the clearest signal and should always be picked up.
#[tokio::test]
async fn e2e_mixed_epistemic_modalities() {
    let (p, placements) = run_e2e(
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
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
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
    let (p, placements) = run_e2e(
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
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
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
    let (p, placements) = run_e2e(
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
            ("James", "Concretely, the plan is: land extraction, then flows, then the Synergy ledger."),
            ("Nico", "The extraction e2e suite is now green on Marvin, by the way."),
        ],
    )
    .await;
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
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
    // Distinct modalities: a good extraction spans more than one class here.
    assert!(
        counts.len() >= 3,
        "expected >=3 distinct classes across a rich transcript; got {counts:?}"
    );
}

// ---- selector against a non-empty graph -------------------------------------

/// Extraction into a perspective that already holds an unrelated graph. The
/// selector must still place NEW instances correctly (fresh bases under
/// `soa://ext/`) without disturbing or colliding with the pre-existing nodes.
#[tokio::test]
async fn e2e_selector_over_prepopulated_graph() {
    let (mut perspective, shapes, ctx) =
        setup_extraction_e2e(&[("Task", TASK_SDNA), ("Belief", BELIEF_SDNA)]).await;
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

    let placements = run_extraction_e2e(
        &mut perspective,
        &shapes,
        &[
            (
                "Nico",
                "James, please write the integration test for the extraction websocket endpoint.",
            ),
            (
                "James",
                "On it — I'll add the WS runExtraction test this afternoon.",
            ),
        ],
        &ctx,
    )
    .await;
    assert_persisted(&perspective, &placements).await;

    // New instances land under the extraction prefix, never on the seeded bases.
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
        "extraction must not overwrite pre-existing instance bases"
    );
    // And it should have found the new task in the conversation.
    let counts = count_by_type(&placements);
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 1,
        "expected the new WS-test task; got {counts:?}"
    );

    // The pre-existing instances are still present in the graph afterwards.
    let existing = existing_titles_snapshot(&perspective, &shapes).await;
    assert!(
        existing
            .iter()
            .any(|t| t.contains("migrate the shacl parser")),
        "seeded task must survive extraction; got {existing:?}"
    );
}

// ---- dedup: don't recreate what's already in the graph ----------------------

/// Pre-seed a Task, then run extraction on a transcript that *restates* that
/// same task and adds a genuinely new one. The existing task must NOT be
/// recreated (deterministic guarantee via `filter_already_present`), while the
/// new task is.
#[tokio::test]
async fn e2e_does_not_recreate_existing_task() {
    let (mut perspective, shapes, ctx) = setup_extraction_e2e(&[("Task", TASK_SDNA)]).await;
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

    let placements = run_extraction_e2e(
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
    assert_persisted(&perspective, &placements).await;

    // The already-present task is never re-created as a NEW instance.
    let new_titles = placed_titles_lower(&placements);
    assert!(
        !new_titles.contains(&existing_title.to_lowercase()),
        "must not recreate the already-present task; new placements = {new_titles:?}"
    );
    // A new task should still have been extracted (the e2e test task).
    let counts = count_by_type(&placements);
    assert!(
        counts.get("task").copied().unwrap_or(0) >= 1,
        "expected the new task to be extracted; got {counts:?}"
    );
}

/// Snapshot of existing instance titles (lower-cased) across the given classes —
/// small helper local to the selector test.
async fn existing_titles_snapshot(
    perspective: &super::perspective_instance::PerspectiveInstance,
    shapes: &[super::model_query::types::ModelShape],
) -> Vec<String> {
    let map = existing_instance_titles(perspective, shapes)
        .await
        .expect("existing_instance_titles");
    map.into_values()
        .flatten()
        .map(|t| t.to_lowercase())
        .collect()
}
