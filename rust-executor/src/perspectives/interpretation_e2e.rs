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
    let (p, shapes, placements) = run_e2e(
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
    assert_persisted(&p, &shapes, &placements).await;

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
    let (p, shapes, placements) = run_e2e(
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
    assert_persisted(&p, &shapes, &placements).await;

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
    let (p, shapes, placements) = run_e2e(
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
    assert_persisted(&p, &shapes, &placements).await;

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
    let (p, shapes, placements) = run_e2e(
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
    assert_persisted(&p, &shapes, &placements).await;

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
    let (p, shapes, placements) = run_e2e_until(
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
        3,
        |c| c.get("task").copied().unwrap_or(0) >= 1,
    )
    .await;
    assert_persisted(&p, &shapes, &placements).await;

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
    // participant matches the seeded owner (e.g. "James" appears both in the
    // seeded task's owner and the new conversation) — a legal upsert, but not
    // what this test is about. Retry up to 3× with a fresh perspective per
    // attempt; if every attempt hits the same glitch, fall through to the
    // assertion with a real failure message.
    const MAX_ATTEMPTS: usize = 3;
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
    // two checks stay on `placements`.)
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

    let placements = run_interpretation_e2e(
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
    assert_persisted(&perspective, &shapes, &placements).await;

    // The already-present task is never RECREATED: no freshly-minted instance
    // carries the seeded title. (An upsert landing on the seeded base and
    // refining its title is fine — that's the id-upsert path doing its job.)
    let seeded_lower = existing_title.to_lowercase();
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
        "expected the seeded task + a newly interpreted one; got {counts:?}"
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
