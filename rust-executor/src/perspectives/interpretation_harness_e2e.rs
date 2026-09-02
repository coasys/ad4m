//! Real-LLM end-to-end tests for the tool-calling harness dispatch of
//! generic interpretation.
//!
//! Sister suite to `interpretation_e2e.rs`. Same fixtures, same Ollama
//! endpoint (see that file's header for env-overrides + tunnel notes) — but
//! these drive the harness path (`run_interpretation_with_harness_and_model`)
//! rather than the single-shot JSON-blob path.
//!
//! The scenarios mirror the PR #911 punch list:
//!   A. read-only budget: harness reaches its answer without any `_propose_*`
//!   B. propose-write budget: harness proposes typed instances via tools,
//!      buffer drains through `apply_with_overlay`, instances are persisted
//!   C. tight budget: `max_tool_calls=1` forces the loop to give up before
//!      any writes could complete — the pass must still return cleanly, no
//!      writes must land
//!   D. relation-typed hasMany against a pre-seeded graph of 3 beliefs — the
//!      LLM must discover URIs via `belief_query` and back-link via
//!      `intention_propose_link_child` on `ns://basedOn`
//!   E. navigation across a distractor-rich graph of 8 beliefs spanning 4
//!      disjoint topic clusters — the transcript's intention derives from
//!      only cluster A, so the LLM must semantically filter, not link
//!      indiscriminately. Locks in the "graph-navigation not graph-flooding"
//!      contract Nico surfaced in the 2026-08-24 review
//!
//! Gated behind `#[ignore = "llm-e2e"]` — regular CI skips them; the nightly
//! `llm-e2e` workflow on `dev` runs them all against Marvin's local Ollama.
//!
//! Run this suite locally:
//!   cargo test --release --lib perspectives::interpretation_harness_e2e \
//!     -- --ignored --test-threads=1 --nocapture
//! or the umbrella `scripts/run-llm-e2e.sh`.

#![cfg(test)]

use super::interpretation_test_support::*;
use crate::types::LinkQuery;

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
#[ignore = "llm-e2e"]
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
#[ignore = "llm-e2e"]
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
#[ignore = "llm-e2e"]
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

// ---- Scenario D: relation-typed hasMany against pre-seeded graph ----------

/// Local reproduction of the TS integration test
/// `tests/js/tests/model/run-interpretation-harness.test.ts`, but purely
/// Rust-side so it can be iterated against Marvin's Ollama without waiting on
/// the full JS suite.
///
/// Seeds three `Belief` instances at known URIs, offers both `Belief` and
/// `Intention` (with `basedOn` hasMany relation) to the harness, and submits a
/// transcript that expresses an intention grounded in those beliefs. The pass
/// must:
///   (a) land ≥1 `Intention` under `soa://ext/`
///   (b) have ≥1 intention linked back to a *seeded* belief URI via
///       `ns://basedOn` — proves the LLM used `belief_query` to discover the
///       existing URIs and `intention_propose_link_child` to attach them
///   (c) not recreate any seeded belief by title
///
/// Retry loop uses a fresh perspective per attempt (matching the seeded-graph
/// pattern in `interpretation_e2e.rs::e2e_selector_ignores_unrelated_seeds`)
/// — simpler than delete-between-attempts and structurally identical to how
/// production would look on each fresh pass.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn harness_intention_links_to_seeded_beliefs() {
    use crate::perspectives::interpretation_test_support::{
        graph_count_by_type, graph_titles_lower, seed_instance, setup_interpretation_e2e,
    };

    const SEEDED_BELIEF_BASES: &[&str] = &[
        "soa://existing/belief/1",
        "soa://existing/belief/2",
        "soa://existing/belief/3",
    ];
    const SEEDED_BELIEF_TITLES: &[&str] = &[
        "Local-first beats cloud-first for user data ownership",
        "Small models with tools outperform big models without tools for structured extraction",
        "Agent-centric architecture is the only way to escape platform capture",
    ];

    const MAX_ATTEMPTS: u8 = 8;
    let mut last: Option<(
        crate::perspectives::perspective_instance::PerspectiveInstance,
        Vec<crate::perspectives::model_query::types::ModelShape>,
        Vec<(String, Vec<crate::types::Link>)>,
    )> = None;

    let seeded_lower: Vec<String> = SEEDED_BELIEF_TITLES
        .iter()
        .map(|t| t.to_lowercase())
        .collect();

    for attempt in 1..=MAX_ATTEMPTS {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[
            ("Belief", BELIEF_SDNA),
            ("Intention", INTENTION_WITH_BASED_ON_SDNA),
        ])
        .await;
        let belief_shape = &shapes[0];

        for (base, title) in SEEDED_BELIEF_BASES.iter().zip(SEEDED_BELIEF_TITLES.iter()) {
            seed_instance(&mut perspective, &ctx, belief_shape, base, title).await;
        }

        let placements = run_interpretation_harness_e2e(
            &mut perspective,
            &shapes,
            &[
                (
                    "Nico",
                    "Given how strongly we believe that local-first beats cloud-first for user data ownership, and that small models with tools outperform big models without tools for structured extraction, I want to commit to shipping the tool-calling harness for interpretation this week — those two beliefs make it the highest-leverage move.",
                ),
                (
                    "James",
                    "Agreed. And given the third belief — that agent-centric architecture is the only way to escape platform capture — that harness lets every agent extract its own knowledge locally, which reinforces the whole stack.",
                ),
            ],
            &ctx,
            16,
        )
        .await;

        let counts = graph_count_by_type(&perspective, &shapes).await;
        let intentions = counts.get("intention").copied().unwrap_or(0);

        // Read intention→basedOn→belief links directly (bypasses model_query,
        // which doesn't surface hasMany relations as scalars). Any intention
        // linked to a seeded belief URI proves the LLM discovered it via
        // belief_query and attached via intention_propose_link_child.
        let mut linked_to_seeded = false;
        for (base, _) in &placements {
            let links = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    predicate: Some("ns://basedOn".into()),
                    ..Default::default()
                })
                .await
                .expect("get_links basedOn");
            if links.iter().any(|l| {
                SEEDED_BELIEF_BASES
                    .iter()
                    .any(|seed| *seed == l.data.target.as_str())
            }) {
                linked_to_seeded = true;
                break;
            }
        }

        // Also check for the "no seeded belief recreated by title"
        // invariant *inside* the retry — otherwise the loop can exit
        // early on `linked_to_seeded=true` while the same attempt
        // ALSO recreated a seeded title, and the panic at the outer
        // assertion (job 24186) becomes non-retryable. Gemma3-12B
        // sometimes back-links AND creates a new belief in the same
        // pass; both outcomes are LLM non-determinism, so both need
        // to gate the exit.
        let mut recreated_seeded_title = false;
        for (base, _) in &placements {
            if !base.starts_with("soa://ext/") {
                continue;
            }
            let links = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    predicate: Some("ns://title".into()),
                    ..Default::default()
                })
                .await
                .expect("get_links title (dup check inside retry)");
            for l in &links {
                if let serde_json::Value::String(t) =
                    crate::perspectives::model_query::utils::parse_literal_value(&l.data.target)
                {
                    if seeded_lower.iter().any(|s| s == &t.to_lowercase()) {
                        recreated_seeded_title = true;
                        break;
                    }
                }
            }
            if recreated_seeded_title {
                break;
            }
        }

        last = Some((perspective, shapes, placements));

        if intentions >= 1 && linked_to_seeded && !recreated_seeded_title {
            if attempt > 1 {
                eprintln!(
                    "[harness-e2e] relation-back-linking satisfied on attempt {attempt}/{MAX_ATTEMPTS}"
                );
            }
            break;
        }

        eprintln!(
            "[harness-e2e] attempt {attempt}/{MAX_ATTEMPTS}: intentions={intentions} \
             linked_to_seeded={linked_to_seeded} recreated_seeded_title={recreated_seeded_title}; retrying"
        );
    }

    let (perspective, shapes, placements) = last.expect("retry loop ran at least once");

    assert_persisted(&perspective, &shapes, &placements).await;

    let counts = graph_count_by_type(&perspective, &shapes).await;
    let intentions = counts.get("intention").copied().unwrap_or(0);
    assert!(
        intentions >= 1,
        "expected at least one intention to land across {MAX_ATTEMPTS} attempts; got {counts:?}"
    );

    // Assertion (b): at least one intention links back to a seeded belief URI.
    let mut evidence: Vec<(String, Vec<String>)> = Vec::new();
    for (base, _) in &placements {
        let links = perspective
            .get_links(&LinkQuery {
                source: Some(base.clone()),
                predicate: Some("ns://basedOn".into()),
                ..Default::default()
            })
            .await
            .expect("get_links basedOn (final)");
        let seeded_targets: Vec<String> = links
            .iter()
            .map(|l| l.data.target.clone())
            .filter(|t| SEEDED_BELIEF_BASES.iter().any(|seed| *seed == t.as_str()))
            .collect();
        if !seeded_targets.is_empty() {
            evidence.push((base.clone(), seeded_targets));
        }
    }
    assert!(
        !evidence.is_empty(),
        "expected at least one intention to be linked via `ns://basedOn` to a seeded belief URI \
         across {MAX_ATTEMPTS} attempts — proves the LLM used belief_query + \
         intention_propose_link_child; got placements={placements:?}"
    );

    // Assertion (c): no seeded belief recreated by title.
    let titles = graph_titles_lower(&perspective, &shapes).await;
    let seeded_lower: Vec<String> = SEEDED_BELIEF_TITLES
        .iter()
        .map(|t| t.to_lowercase())
        .collect();
    let recreated_bases: Vec<&String> = placements
        .iter()
        .map(|(b, _)| b)
        .filter(|b| b.starts_with("soa://ext/"))
        .collect();
    // If a placement under soa://ext/ carries a title matching any seeded
    // belief, the LLM recreated it instead of linking.
    for base in &recreated_bases {
        let links = perspective
            .get_links(&LinkQuery {
                source: Some((*base).clone()),
                predicate: Some("ns://title".into()),
                ..Default::default()
            })
            .await
            .expect("get_links title (dup check)");
        for l in &links {
            if let serde_json::Value::String(t) =
                crate::perspectives::model_query::utils::parse_literal_value(&l.data.target)
            {
                let t_lower = t.to_lowercase();
                assert!(
                    !seeded_lower.iter().any(|s| s == &t_lower),
                    "seeded belief title `{t}` was recreated under {base} instead of being \
                     linked via basedOn; titles={titles:?}"
                );
            }
        }
    }
}

// ---- Scenario E: LLM navigates a distractor-rich graph --------------------

/// Same shape as Scenario D but the seeded graph carries **eight** Belief
/// instances across four disjoint topic clusters — only cluster A is relevant
/// to the transcript. The pass must:
///
///   1. land ≥1 `Intention` under `soa://ext/`
///   2. link the intention via `ns://basedOn` to **at least one** cluster-A
///      belief URI (proves the LLM used `belief_query` to discover it, not
///      just guessed a URI)
///   3. NOT link to any cluster-B/C/D distractor belief URI (proves the LLM
///      actually filtered — a naïve model that just calls `belief_query` and
///      then links to everything it saw would fail here)
///
/// The classifier signal is unambiguous by construction: the transcript names
/// the exact phrasing of the two cluster-A beliefs. A well-behaving model
/// should hit this on the first attempt; the 8-attempt retry loop just
/// tolerates the normal LLM stochasticity that already justifies retries in
/// Scenarios B and D.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn harness_intention_selects_only_relevant_beliefs() {
    use crate::perspectives::interpretation_test_support::{
        graph_count_by_type, seed_instance, setup_interpretation_e2e,
    };

    // Cluster A — LLM tool-calling / interpretation (RELEVANT to transcript)
    // Cluster B — dog nutrition (distractor)
    // Cluster C — beginner guitar (distractor)
    // Cluster D — urban cycling (distractor)
    //
    // Same soa:// scheme + `existing/belief/{n}` shape as Scenario D so the
    // URI-scheme whitelist accepts them and the retry-fresh-perspective
    // pattern lines up. The first two URIs are the ONLY ones the transcript
    // grounds itself on.
    const SEEDED_BASES: &[&str] = &[
        // cluster A — relevant
        "soa://existing/belief/1",
        "soa://existing/belief/2",
        // cluster B — dog nutrition (distractor)
        "soa://existing/belief/3",
        "soa://existing/belief/4",
        // cluster C — beginner guitar (distractor)
        "soa://existing/belief/5",
        "soa://existing/belief/6",
        // cluster D — urban cycling (distractor)
        "soa://existing/belief/7",
        "soa://existing/belief/8",
    ];
    const SEEDED_TITLES: &[&str] = &[
        // cluster A
        "Small models with tools outperform big models without tools for structured extraction",
        "Grammar-constrained decoding eliminates hallucinated tool-call formats in local LLMs",
        // cluster B
        "Raw feeding produces better coat condition in medium-sized dogs than kibble",
        "Adult dogs need protein but do not need grain for a balanced diet",
        // cluster C
        "Nylon strings are gentler on beginner fingers than steel strings",
        "Cheap tuners are the biggest reason beginners give up guitar in the first month",
        // cluster D
        "Dedicated bike lanes reduce urban commute times by 30 percent over shared roads",
        "Electric bikes have made 20-kilometre commutes feasible for non-athletes",
    ];
    // Indices 0 and 1 (`belief/1`, `belief/2`) are the only relevant seeds.
    const RELEVANT_INDICES: &[usize] = &[0, 1];

    let relevant_set: std::collections::HashSet<&str> =
        RELEVANT_INDICES.iter().map(|&i| SEEDED_BASES[i]).collect();

    const MAX_ATTEMPTS: u8 = 8;
    let mut last: Option<(
        crate::perspectives::perspective_instance::PerspectiveInstance,
        Vec<crate::perspectives::model_query::types::ModelShape>,
        Vec<(String, Vec<crate::types::Link>)>,
        // (relevant_targets_seen, distractor_targets_seen) — captured for the
        // failure-path diagnostic so the assertion below prints what the LLM
        // actually linked, not just "nothing satisfied the guard".
        Vec<String>,
        Vec<String>,
    )> = None;

    for attempt in 1..=MAX_ATTEMPTS {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[
            ("Belief", BELIEF_SDNA),
            ("Intention", INTENTION_WITH_BASED_ON_SDNA),
        ])
        .await;
        let belief_shape = &shapes[0];

        for (base, title) in SEEDED_BASES.iter().zip(SEEDED_TITLES.iter()) {
            seed_instance(&mut perspective, &ctx, belief_shape, base, title).await;
        }

        // Transcript quotes the cluster-A belief phrases nearly verbatim so
        // the classification signal is unambiguous — the interesting thing
        // being tested is not "can the LLM understand the utterance" but
        // "does the LLM use `belief_query` to find the right existing URIs
        // instead of linking to everything it sees".
        let placements = run_interpretation_harness_e2e(
            &mut perspective,
            &shapes,
            &[
                (
                    "Nico",
                    "Given that small models with tools outperform big models without tools for structured extraction, and that grammar-constrained decoding eliminates hallucinated tool-call formats in local LLMs, I commit to shipping the tool-calling harness with grammar-constrained tool calls this sprint. Those two beliefs are the whole reason this is high-leverage.",
                ),
                (
                    "James",
                    "Yes — nothing else on the board today matters more than that intention right now.",
                ),
            ],
            &ctx,
            20,
        )
        .await;

        let counts = graph_count_by_type(&perspective, &shapes).await;
        let intentions = counts.get("intention").copied().unwrap_or(0);

        // Read the basedOn edges from every proposed intention, split into
        // (relevant_targets, distractor_targets) so we can enforce both
        // "linked to something relevant" AND "did not link to anything
        // irrelevant".
        let mut relevant_targets: Vec<String> = Vec::new();
        let mut distractor_targets: Vec<String> = Vec::new();
        for (base, _) in &placements {
            let links = perspective
                .get_links(&LinkQuery {
                    source: Some(base.clone()),
                    predicate: Some("ns://basedOn".into()),
                    ..Default::default()
                })
                .await
                .expect("get_links basedOn");
            for l in &links {
                let tgt = l.data.target.clone();
                if SEEDED_BASES.iter().any(|s| *s == tgt.as_str()) {
                    if relevant_set.contains(tgt.as_str()) {
                        relevant_targets.push(tgt);
                    } else {
                        distractor_targets.push(tgt);
                    }
                }
            }
        }

        let guard_ok =
            intentions >= 1 && !relevant_targets.is_empty() && distractor_targets.is_empty();

        last = Some((
            perspective,
            shapes,
            placements,
            relevant_targets.clone(),
            distractor_targets.clone(),
        ));

        if guard_ok {
            if attempt > 1 {
                eprintln!(
                    "[harness-e2e] navigation-across-distractors guard satisfied on attempt {attempt}/{MAX_ATTEMPTS}"
                );
            }
            break;
        }

        eprintln!(
            "[harness-e2e] attempt {attempt}/{MAX_ATTEMPTS}: intentions={intentions} \
             relevant={relevant_targets:?} distractors={distractor_targets:?}; retrying"
        );
    }

    let (perspective, shapes, placements, relevant_targets, distractor_targets) =
        last.expect("retry loop ran at least once");

    assert_persisted(&perspective, &shapes, &placements).await;

    let counts = graph_count_by_type(&perspective, &shapes).await;
    let intentions = counts.get("intention").copied().unwrap_or(0);
    assert!(
        intentions >= 1,
        "expected at least one intention to land across {MAX_ATTEMPTS} attempts; got {counts:?}"
    );

    // (2) Must be linked to at least one cluster-A belief URI.
    assert!(
        !relevant_targets.is_empty(),
        "expected at least one intention to be linked via `ns://basedOn` to a cluster-A \
         belief URI ({:?}) across {MAX_ATTEMPTS} attempts — proves the LLM discovered the \
         right existing URIs via belief_query rather than guessing; got placements={placements:?}",
        RELEVANT_INDICES
            .iter()
            .map(|&i| SEEDED_BASES[i])
            .collect::<Vec<_>>()
    );

    // (3) Must NOT link to any distractor belief URI. This is the real
    // navigation contract — a lazy LLM would call belief_query, see all 8
    // titles, and link to everything. That's failure.
    assert!(
        distractor_targets.is_empty(),
        "intention was linked via `ns://basedOn` to distractor belief URI(s) {distractor_targets:?} \
         — cluster A is `belief/1`+`belief/2` (LLM/tool-calling); \
         cluster B (`belief/3`+`belief/4`) is dog nutrition, \
         cluster C (`belief/5`+`belief/6`) is guitar, \
         cluster D (`belief/7`+`belief/8`) is urban cycling. \
         The transcript is unambiguously about cluster A only, so any link to a \
         distractor means the LLM is graph-flooding, not graph-navigating"
    );
}
