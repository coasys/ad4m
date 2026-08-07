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
///
/// Wrapped in [`run_e2e_retrying`] because gemma3:12b intermittently emits only
/// the intention (~20% observed empirically on 5-run local sweeps). Two extra
/// attempts push the flake rate well under 1% while keeping the assertion — we
/// want to know when *both* modalities are picked up, not shrug at the miss.
#[tokio::test]
async fn e2e_intention_and_belief() {
    let (p, placements) = run_e2e_retrying(
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
        |pl| {
            let c = count_by_type(pl);
            c.get("intention").copied().unwrap_or(0) >= 1
                && c.get("belief").copied().unwrap_or(0) >= 1
        },
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
            // …and introduces a brand-new unrelated task. Deliberately in a
            // different topic area so the LLM cannot plausibly merge them into
            // a single upsert on the seeded base.
            ("Josh", "I'll update the CI documentation this evening."),
        ],
        &ctx,
    )
    .await;
    assert_persisted(&perspective, &placements).await;

    // The already-present task is never RECREATED as a fresh instance under
    // the extraction prefix. An upsert that lands on the seeded base and
    // preserves/refines the existing title is fine — that's the id-context
    // upsert path doing its job, not a duplicate.
    let newly_minted_titles: Vec<String> = placements
        .iter()
        .filter(|(base, _)| !base.starts_with("soa://existing/"))
        .flat_map(|(_, links)| {
            links
                .iter()
                .filter(|l| l.predicate.as_deref() == Some("ns://title"))
                .filter_map(|l| decode_literal_string(&l.target))
                .map(|s| s.to_lowercase())
        })
        .collect();
    assert!(
        !newly_minted_titles.contains(&existing_title.to_lowercase()),
        "must not mint a fresh instance with the already-present title; \
         newly-minted titles = {newly_minted_titles:?}"
    );
    // A new task should still have been extracted (the CI docs task). Count
    // Create placements: they're the ones under the extraction prefix.
    let created_count = placements
        .iter()
        .filter(|(base, _)| base.starts_with("soa://ext/"))
        .filter(|(_, links)| {
            links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://type"))
        })
        .count();
    assert!(
        created_count >= 1,
        "expected at least one new task to be created; placements = {placements:#?}"
    );
}

// ---- upsert path: LLM chooses UPDATE over CREATE via `id` ------------------

/// Pre-seed a Task, then run extraction on a transcript that explicitly RENAMES
/// it and assigns a NEW OWNER. The extractor should recognise the continuity
/// (same underlying task) and emit the existing `id`, driving the upsert path.
/// The seeded instance's title/owner scalars end up REPLACED (SPARQL "set"
/// semantics), and no duplicate Task base URI is minted.
///
/// This exercises the Phase 1B contract end-to-end: existing entries in the
/// prompt now carry `{id, title, class}`, the system prompt + few-shot example
/// teach `id`-emission, and `plan_extraction_ops` -> `apply_extraction_ops`
/// routes those emissions to the update code path. If the LLM refuses to emit
/// `id`, the test surfaces that as a real failure — the prompt/example
/// engineering needs work.
#[tokio::test]
async fn e2e_updates_existing_instance_via_id() {
    use super::extraction::{run_extraction, ExtractionOp};
    use super::extraction_test_support::seed_instance;
    use crate::types::LinkStatus;

    let (mut perspective, shapes, ctx) = setup_extraction_e2e(&[("Task", TASK_SDNA)]).await;
    let task_shape = &shapes[0];

    let seeded_base = "soa://existing/task/webrtc";
    seed_instance(
        &mut perspective,
        &ctx,
        task_shape,
        seeded_base,
        "Finish the WebRTC call module",
    )
    .await;

    // Transcript renames the seeded task and assigns a new owner — a clear
    // continuation, not a fresh idea. The `id` handle to the existing task is
    // in the prompt; the LLM should emit it.
    let transcript: Vec<(String, String)> = vec![
        (
            "Nico".into(),
            "Update on the WebRTC work: let's rename that task to \
             'Complete the WebRTC call module and add a retry guard' and \
             assign it to Josh."
                .into(),
        ),
        (
            "Josh".into(),
            "Got it — I'll take over the WebRTC call module and add the retry guard.".into(),
        ),
    ];

    let ops = run_extraction(
        &mut perspective,
        &shapes,
        &transcript,
        "soa://ext/",
        LinkStatus::Local,
        &ctx,
    )
    .await
    .expect("run_extraction against real LLM");

    // Log the split for debugging when this ever regresses.
    let updates: Vec<&ExtractionOp> = ops
        .iter()
        .filter(|o| matches!(o, ExtractionOp::Update { .. }))
        .collect();
    let creates: Vec<&ExtractionOp> = ops
        .iter()
        .filter(|o| matches!(o, ExtractionOp::Create { .. }))
        .collect();
    println!(
        "e2e upsert: {} update(s), {} create(s); ops = {:#?}",
        updates.len(),
        creates.len(),
        ops
    );

    // Primary assertion: at least one Update landed on the seeded base — that
    // means the LLM chose to emit the existing `id`.
    let updated_seeded = updates.iter().any(|op| match op {
        ExtractionOp::Update { base, .. } => base == seeded_base,
        _ => false,
    });
    assert!(
        updated_seeded,
        "expected the LLM to emit id={seeded_base:?} for the renamed task \
         (upsert path); got ops={ops:#?}"
    );

    // Secondary: no fresh Task base collides with the seeded one, and the
    // Update actually wrote a new owner link (proving the scalar replacement).
    let placements = ops_to_placements(&ops);
    let created_bases: Vec<&String> = ops
        .iter()
        .filter_map(|o| match o {
            ExtractionOp::Create { base, .. } => Some(base),
            _ => None,
        })
        .collect();
    assert!(
        created_bases.iter().all(|b| b.as_str() != seeded_base),
        "no Create must land on the seeded base"
    );
    let owner_on_seeded = placements.iter().any(|(base, links)| {
        base == seeded_base
            && links
                .iter()
                .any(|l| l.predicate.as_deref() == Some("ns://owner"))
    });
    assert!(
        owner_on_seeded,
        "expected the upsert to write the new owner on the seeded base; \
         placements = {placements:#?}"
    );
}

// ---- relations: a reified edge links two freshly-minted nodes ---------------

/// SDNA for a `Topic` node (title only) — the endpoint a `SemanticRelationship`
/// tags. Declared inline so the relations e2e is self-contained.
const TOPIC_SDNA: &str = r#"{
  "target_class":"ns://Topic",
  "extraction_hint":"A distinct subject or theme the participants discuss.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://topic","min_count":1,"max_count":1},
    {"path":"ns://title","name":"title","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"Short topic label."}
  ]
}"#;

/// SDNA for a reified edge: a scalar `relevance` plus a forward `tag` relation
/// to a `Topic`. This is the shape of Flux's `SemanticRelationship`, minus the
/// second (Message) endpoint to keep the e2e's class set small.
const SEMANTIC_RELATIONSHIP_SDNA: &str = r#"{
  "target_class":"ns://SemanticRelationship",
  "extraction_hint":"An edge that tags a discussion point with a Topic and a relevance score.",
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://semrel","min_count":1,"max_count":1},
    {"path":"ns://relevance","name":"relevance","min_count":1,"max_count":1,"resolve_language":"literal","extraction_hint":"A number from 0 to 1: how strongly the tag applies."},
    {"path":"ns://tag","name":"tag","relation_kind":"hasOne","target_class_name":"Topic","class":"ns://TopicShape","extraction_hint":"The Topic this edge tags. Reference an existing Topic id or a new:Topic:<n> sibling."}
  ]
}"#;

fn has_type(links: &[crate::types::Link], type_value: &str) -> bool {
    links
        .iter()
        .any(|l| l.predicate.as_deref() == Some("ns://type") && l.target == type_value)
}

/// True if some `SemanticRelationship` placement carries a `ns://tag` link whose
/// target is the base of an emitted `Topic` — i.e. the model filled the relation
/// with a resolvable reference (existing id or `new:Topic:<n>`) and the two-pass
/// planner turned it into a real edge, not a dropped literal.
fn tag_resolves_to_topic(pl: &[(String, Vec<crate::types::Link>)]) -> bool {
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

/// The payoff test for Phase 2: from a two-topic transcript, the model must mint
/// the Topics AND a SemanticRelationship whose `tag` relation *references* one of
/// them (via `new:Topic:<n>` or an existing id), which the two-pass planner
/// resolves into a real `ns://tag` link between the two minted nodes. gemma3:12b
/// is the canary — if it emits the topic *title* instead of a ref, no edge lands
/// and the retry predicate fails, surfacing prompt work rather than silently
/// passing. Paraphrased from the few-shot so it isn't verbatim.
#[tokio::test]
async fn e2e_extracts_topic_relation_from_transcript() {
    let (p, placements) = run_e2e_retrying(
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
            let c = count_by_type(pl);
            c.get("topic").copied().unwrap_or(0) >= 2 && tag_resolves_to_topic(pl)
        },
    )
    .await;
    assert_persisted(&p, &placements).await;

    let counts = count_by_type(&placements);
    // Two clear topics: webhook/retry logging and observability.
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
    // came out of the same extraction pass.
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
