//! Real-LLM end-to-end tests for the tool-calling harness dispatch of
//! generic interpretation.
//!
//! Sister suite to `interpretation_e2e.rs`. Same fixtures, same Ollama
//! endpoint (see that file's header for env-overrides + tunnel notes) — but
//! these drive the harness path (`run_interpretation_with_harness_and_model`)
//! rather than the single-shot JSON-blob path.
//!
//! The three scenarios mirror the PR #911 punch list:
//!   A. read-only budget: harness reaches its answer without any `_propose_*`
//!   B. propose-write budget: harness proposes typed instances via tools,
//!      buffer drains through `apply_with_overlay`, instances are persisted
//!   C. tight budget: `max_tool_calls=1` forces the loop to give up before
//!      any writes could complete — the pass must still return cleanly, no
//!      writes must land
//!
//! Run just this suite:
//!   cargo test --release --lib perspectives::interpretation_harness_e2e \
//!     -- --test-threads=1 --nocapture

#![cfg(test)]

use super::interpretation_test_support::*;

// ---- Scenario A: read-only pass (no _propose_* invoked) -------------------

/// A transcript that offers **nothing to classify** as a Task — a bare
/// factual/greeting utterance with no verb of commitment, no assignment,
/// nothing that reads as a to-do. With Task the only class on the surface,
/// the harness should terminate without ever calling `Task_propose_create`.
///
/// This is the harness analogue of "the JSON-blob path returns an empty
/// planned-ops list on empty input" — proves the loop terminates on its own
/// without needing to write, and that the drain-then-apply step is a no-op
/// when the buffer is empty.
///
/// Wrapped in a small retry-tolerant loop: small local models occasionally
/// hallucinate a task even when there is none (an over-classification
/// failure that the single-shot path masks — its full-transcript JSON-blob
/// prompt is stricter than a tool-calling multi-turn dialog). The point of
/// scenario A is that the harness *can* return zero writes, not that it
/// does so with 100% stochastic certainty on every attempt.
#[tokio::test]
async fn harness_read_only_transcript_produces_no_writes() {
    let (p, shapes, placements) = run_harness_e2e_until(
        &[("Task", TASK_SDNA)],
        &[("Nico", "Good morning everyone."), ("James", "Morning.")],
        8,
        8,
        |counts| counts.get("task").copied().unwrap_or(0) == 0,
    )
    .await;

    // `run_harness_e2e_until` returns the FIRST attempt that hits the guard;
    // if we get one back, `placements` is empty by construction. If no
    // attempt ever hit zero, `_until` returns the last (guard-failing)
    // attempt and the assertion below fires with the actual counts, keeping
    // the diagnostic parallel to scenario B.
    let counts = graph_count_by_type(&p, &shapes).await;
    assert!(
        counts.get("task").copied().unwrap_or(0) == 0,
        "no attempt of the read-only pass hit zero writes across the retry \
         budget — the harness is over-classifying casual chatter; got {counts:?}"
    );
    assert!(
        placements.is_empty(),
        "read-only pass should have persisted no instances on the returned \
         attempt; got {placements:?}"
    );
}

// ---- Scenario B: propose-write pass (tools land typed instances) ----------

/// The task-tracking conversation from `interpretation_e2e.rs::e2e_task_tracking_counts`,
/// but through the harness. Three assignments in the transcript should each
/// route through a `Task_propose_create` tool call, buffer drains via
/// `apply_with_overlay`, model_query reads back 2-4 Task instances.
///
/// Uses the same 2-4 tolerance as the single-shot equivalent (LLM may
/// merge/split slightly). Retries a small budget of times for LLM flake.
#[tokio::test]
async fn harness_propose_writes_land_typed_tasks() {
    let (p, shapes, placements) = run_harness_e2e_until(
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
        16,
        8,
        |c| (2..=4).contains(&c.get("task").copied().unwrap_or(0)),
    )
    .await;
    assert_persisted(&p, &shapes, &placements).await;

    let counts = graph_count_by_type(&p, &shapes).await;
    let tasks = counts.get("task").copied().unwrap_or(0);
    assert!(
        (2..=4).contains(&tasks),
        "harness expected 2-4 tasks from three assignments; got {counts:?}"
    );
    assert!(
        counts.keys().all(|k| k == "task"),
        "only Task was offered to the harness; got {counts:?}"
    );

    // At least one Task should carry an owner — harness must be actually
    // using the `owner` setter, not just minting bare type flags.
    let owners = graph_owners_lower(&p, &shapes).await;
    assert!(
        !owners.is_empty(),
        "expected at least one harness-proposed task to carry an owner; got {owners:?}"
    );
}

// ---- Scenario C: tight budget forces early termination --------------------

/// With `max_tool_calls=1`, the harness is allowed at most ONE tool call
/// before the loop is forced to end. That's not enough runway for a
/// query-then-propose pattern, so on a real-model gemma3 pass we expect to
/// land at most one Task (if the model happens to propose without querying)
/// or zero (if the model queries first, which is what the tools-system-prompt
/// recommends).
///
/// The contract this test locks in isn't "must land N" but "must not crash":
/// the pass must return `Ok(bases)` even when the budget is exhausted
/// mid-loop, and any writes that DID happen must apply cleanly through the
/// overlay (no half-applied state).
#[tokio::test]
async fn harness_tight_budget_exits_cleanly_no_crash() {
    // Deliberately don't wrap in `run_harness_e2e_until` — the whole point is
    // to exercise the budget-exhausted return path, not to iterate until we
    // see writes. A clean return with 0 placements is the primary success.
    let (p, shapes, placements) = run_harness_e2e(
        &[("Task", TASK_SDNA)],
        &[(
            "Nico",
            "James, can you ship the WebRTC call module by Monday?",
        )],
        1,
    )
    .await;

    // No half-applied state: whatever bases came back must be readable via
    // model_query as fully-typed Task instances.
    assert_persisted(&p, &shapes, &placements).await;

    // Sanity: task count matches placements count (nothing extra leaked in,
    // nothing dropped between buffer drain and model_query).
    let counts = graph_count_by_type(&p, &shapes).await;
    let tasks = counts.get("task").copied().unwrap_or(0);
    assert_eq!(
        tasks,
        placements.len(),
        "task count via model_query should equal placements length; got {counts:?} vs {} placements",
        placements.len()
    );
    assert!(
        tasks <= 1,
        "with max_tool_calls=1 at most one Task can land; got {tasks}"
    );
}
