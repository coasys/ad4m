//! Real-LLM end-to-end test for the flow-aware extraction data path.
//!
//! Complements the sibling `e2e_tests` module (no-LLM composition proof —
//! walks the whole gather → prompt-builder chain against fixture inputs)
//! by running the *actual extraction pass* with a real OpenAI-compatible
//! model in the loop. That is the shell above pure composition: verify
//! the LLM's response is still parseable when the new
//! "## Active flows on this scope" prompt block is present, and that the
//! whole `run_interpretation` pipeline lands typed instances despite the
//! injected flow context.
//!
//! # What it proves
//!
//! 1. Prompt integration doesn't malform LLM output: the extractor
//!    still returns JSON the parser can decode when `contexts.len() > 0`.
//! 2. `run_interpretation` completes end-to-end with the flow-aware
//!    wiring live — one full pass against a real model.
//! 3. The extracted instance carries evidence the LLM's understanding of
//!    the scope conversation, i.e. the new prompt section didn't cause
//!    the model to hallucinate types outside the offered class set.
//!
//! 4. The engine's post pass acts on what the extraction wrote: with a
//!    `requires`-guarded, `semanticCheck`-hinted state in play, the real
//!    LLM answers the semantic-check question and a
//!    `FlowTransitionProposal` is minted (YES case) or withheld (NO
//!    case) — the `semantic_check_*` tests below. This is the live-model
//!    shell above `flow_evaluator_e2e`'s scripted `CannedLlm` coverage.
//!
//! # What it does *not* prove
//!
//! - The LLM proposes a `FlowTransitionProposal` on its own (covered by
//!   the `flow_proposals` output-field tests).
//!
//! # Endpoint + retry
//!
//! Uses the standard `setup_interpretation_e2e` harness: OpenAI-compatible
//! endpoint at `INTERPRETATION_E2E_BASE_URL` (default `localhost:11434/v1`,
//! i.e. Ollama on Marvin or over an SSH tunnel from a dev box), model
//! `INTERPRETATION_E2E_MODEL` (default `gemma3:12b`). Retries the whole
//! pass up to 3× to soak up single-sample LLM flake, matching the
//! sibling generic-interpretation e2e tests.
//!
//! # CI gating (PR #943)
//!
//! Gated behind `#[ignore = "llm-e2e"]` — regular CI skips it; the nightly
//! `llm-e2e` workflow on `dev` runs it against Marvin's local Ollama. Same
//! discipline as `interpretation_e2e.rs` / `interpretation_harness_e2e.rs`.
//! Run locally with `cargo test --release --lib
//! perspectives::flow_context::real_llm_e2e -- --ignored --test-threads=1
//! --nocapture`, or via the umbrella `scripts/run-llm-e2e.sh`.

#![cfg(test)]

use super::gather_active_flow_contexts;
use crate::perspectives::flow_classes::mint_flow_instance;
use crate::perspectives::interpretation::{run_interpretation, TranscriptTurn};
use crate::perspectives::interpretation_test_support::{
    graph_count_by_type, print_placements, read_back_placements, setup_interpretation_e2e,
    TASK_SDNA,
};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::parse_flow_to_links;
use crate::types::{LinkQuery, LinkStatus};

/// Delivery-flow JSON matching what `SHACLFlow.toJSON()` emits and what
/// `parse_flow_to_links` deserializes back. State names + hints are the
/// ones the assertions below (and the LLM prompt block) key on.
fn delivery_flow_json() -> String {
    serde_json::json!({
        "name": "Delivery",
        "namespace": "delivery://",
        "start_action": [],
        "states": [
            {
                "name": "identified",
                "value": 0.0,
                "state_check": {
                    "source": null,
                    "predicate": "delivery://state",
                    "target": "delivery://identified"
                },
                "interpretationHint":
                    "The team has named a piece of work but has not yet scoped it.",
            },
            {
                "name": "scoped",
                "value": 0.5,
                "state_check": {
                    "source": null,
                    "predicate": "delivery://state",
                    "target": "delivery://scoped"
                },
                "interpretationHint":
                    "The team has agreed what the work is and can begin execution.",
            }
        ],
        "transitions": [
            {
                "action_name": "Scope",
                "from_state": "identified",
                "to_state": "scoped",
                "actions": []
            }
        ],
        "interpretationHint":
            "A team-scale unit of work moving from identification to done.",
        "inputTypes": ["ns://Task"],
        "outputTypes": [],
    })
    .to_string()
}

/// Seed the perspective with the Delivery flow, then mint a running
/// `FlowInstance` on `base_uri`. Returns the URI of the minted instance
/// so the caller can key follow-up assertions. `parse_flow_to_links`
/// emits every declared field (interpretationHint at flow + state scope,
/// inputTypes, outputTypes, requires, semanticCheck, consensusRule,
/// creationHint, context), so the fixture is a single JSON blob
/// round-tripped through the writer — no hand-appended predicate
/// scaffolding.
async fn seed_delivery_flow_and_instance(
    perspective: &mut PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    base_uri: &str,
) -> String {
    seed_flow_and_instance(
        perspective,
        ctx,
        base_uri,
        &delivery_flow_json(),
        "e2e-real-llm-inst",
    )
    .await
}

/// [`seed_delivery_flow_and_instance`] for an arbitrary flow JSON blob and
/// instance id — the semantic-check tests seed a *gated* variant of the
/// Delivery flow, so the JSON is a parameter.
async fn seed_flow_and_instance(
    perspective: &mut PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    base_uri: &str,
    flow_json: &str,
    instance_id: &str,
) -> String {
    let flow_links = parse_flow_to_links(flow_json, "Delivery").expect("parse_flow_to_links");
    for link in flow_links {
        perspective
            .add_link(link, LinkStatus::Local, None, ctx)
            .await
            .expect("add_link(flow definition)");
    }

    // Flow URI (`${namespace}${name}Flow`), not bare name — see James
    // PR #929 R5.
    mint_flow_instance(
        perspective,
        "delivery://DeliveryFlow",
        base_uri,
        "identified",
        instance_id,
        None,
        ctx,
    )
    .await
    .expect("mint_flow_instance")
}

/// The Delivery flow with the `scoped` state gated: a `requires` guard
/// (at least one `Task` instance) plus the given `semanticCheck` hint.
/// The structural guard is what makes the transition *reach* the semantic
/// check; the hint is what the real LLM is asked to confirm.
fn gated_delivery_flow_json(semantic_check: &str) -> String {
    let mut flow: serde_json::Value =
        serde_json::from_str(&delivery_flow_json()).expect("delivery flow JSON parses");
    let scoped = flow["states"]
        .as_array_mut()
        .expect("states is an array")
        .iter_mut()
        .find(|s| s["name"] == "scoped")
        .expect("scoped state present");
    scoped["requires"] = serde_json::json!([{ "className": "Task", "count": { "min": 1 } }]);
    scoped["semanticCheck"] = serde_json::json!(semantic_check);
    flow.to_string()
}

/// Proposal URIs minted against `inst_uri` — every `FlowTransitionProposal`
/// carries an `ad4m://flow/instance` link back to its instance, so the
/// reverse query is the read-back.
async fn proposals_for_instance(perspective: &PerspectiveInstance, inst_uri: &str) -> Vec<String> {
    perspective
        .get_links(&LinkQuery {
            predicate: Some("ad4m://flow/instance".to_string()),
            target: Some(inst_uri.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links(proposals)")
        .into_iter()
        .map(|l| l.data.source)
        .collect()
}

/// Real LLM + active-flow context in the prompt — the onion-shell test
/// above the pure composition proof in the sibling `e2e_tests` module.
///
/// Verifies that when the extraction pass runs against a real model
/// with the flow-aware wiring live, the added `active_flows` prompt
/// block does not derail the LLM's ability to produce parseable output
/// — the pass still lands at least one typed instance for a transcript
/// unambiguously about scoping the referenced Task subject.
///
/// Retries up to 3× to soak single-sample flake from the small local model
/// (`gemma3:12b` occasionally files a scope-discussion turn as a `belief` on
/// one sample even when only `Task` is offered — the retry lets it converge
/// without hiding a real regression, since the last attempt's assertions
/// still fire with full diagnostics).
#[tokio::test(flavor = "multi_thread")]
#[ignore = "llm-e2e"]
async fn model_c_real_llm_extraction_with_active_flow_in_prompt() {
    let base_uri = "soa://ext/task/mvp";
    let mut last_counts: Option<std::collections::HashMap<String, usize>> = None;
    let attempts: u8 = 3;

    for i in 1..=attempts {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;

        let inst_uri = seed_delivery_flow_and_instance(&mut perspective, &ctx, base_uri).await;

        // Sanity check the wiring — the gather half must see the
        // just-minted instance, else the LLM can't possibly get flow
        // context and this test is measuring nothing.
        let contexts = gather_active_flow_contexts(&perspective, &[base_uri.to_string()]).await;
        assert_eq!(
            contexts.len(),
            1,
            "gather should surface the one active flow, got {contexts:?}"
        );
        assert_eq!(contexts[0].instance_uri, inst_uri);
        assert_eq!(contexts[0].current_state, "identified");
        assert_eq!(contexts[0].reachable_next_states.len(), 1);
        assert_eq!(contexts[0].reachable_next_states[0].name, "scoped");

        // Transcript: unambiguous scoping conversation about the MVP work.
        // If the LLM sees the active-flow block correctly, it will still
        // return at least one Task (concrete follow-up work) — proving the
        // extraction pipeline is not destabilised by the injected block.
        let transcript = vec![
            TranscriptTurn::from_speaker_text(
                "Nico",
                "Let's scope the MVP work. James, can you write the flow-runner engine spec?",
            ),
            TranscriptTurn::from_speaker_text(
                "James",
                "Sure. Josh, can you draft the two flow YAMLs while I'm on that?",
            ),
            TranscriptTurn::from_speaker_text(
                "Josh",
                "Yes, I'll start with the Delivery flow YAML today.",
            ),
        ];

        // Scope narrowing to the base subject the flow runs on — the exact
        // shape `run_interpretation_with_strategy_and_model` passes when the
        // pass is anchored on a specific model instance.
        let scope = Scope::Model {
            model: "Task".to_string(),
            id: base_uri.to_string(),
            field: None,
        };

        // The real payoff: run the whole extraction pipeline against a
        // live LLM with the flow-aware wiring on. If the added prompt
        // block malformed the model output, this Err()s at parse time.
        let bases = run_interpretation(
            &mut perspective,
            &shapes,
            &transcript,
            "soa://ext/",
            &ctx,
            Some(&scope),
        )
        .await
        .expect("run_interpretation against real LLM to succeed with flow context in prompt");

        let placements = read_back_placements(&perspective, &bases).await;
        print_placements(&placements);
        let counts = graph_count_by_type(&perspective, &shapes).await;
        eprintln!("[real-llm-e2e] attempt {i}/{attempts} counts={counts:?}");

        // Retry guard: at least one Task must have landed. That's the
        // minimum bar for "the LLM's understanding of the scope discussion
        // survived the injected prompt block."
        if counts.get("task").copied().unwrap_or(0) >= 1 {
            // Assertion on the retained placements — the pass wrote at
            // least one instance under the standard soa:// prefix.
            assert!(
                !placements.is_empty(),
                "at least one placement expected from the scope-conversation transcript"
            );
            return;
        }

        eprintln!(
            "[real-llm-e2e] attempt {i}/{attempts} did not extract a Task (got {counts:?}); retrying"
        );
        last_counts = Some(counts);
    }

    panic!(
        "run_interpretation with active-flow context in the prompt did not extract a Task in {attempts} attempts; last counts={:?}",
        last_counts
    );
}

/// One full pipeline run against the *gated* Delivery flow: real-LLM
/// extraction with flow context in the prompt, then the engine post pass
/// with the production `AIServiceSemanticCheck` (same task, same live
/// model) answering the `semanticCheck` question. Returns the perspective
/// plus what the run produced, so each test applies its own pass/retry
/// criterion.
async fn run_gated_pipeline_attempt(
    semantic_check: &str,
    instance_id: &str,
) -> (PerspectiveInstance, usize, String, Vec<String>) {
    let base_uri = "soa://ext/task/mvp";
    let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;

    let inst_uri = seed_flow_and_instance(
        &mut perspective,
        &ctx,
        base_uri,
        &gated_delivery_flow_json(semantic_check),
        instance_id,
    )
    .await;

    // Same unambiguous scoping conversation as the extraction test above:
    // named people take on named pieces of work.
    let transcript = vec![
        TranscriptTurn::from_speaker_text(
            "Nico",
            "Let's scope the MVP work. James, can you write the flow-runner engine spec?",
        ),
        TranscriptTurn::from_speaker_text(
            "James",
            "Sure. Josh, can you draft the two flow YAMLs while I'm on that?",
        ),
        TranscriptTurn::from_speaker_text(
            "Josh",
            "Yes, I'll start with the Delivery flow YAML today.",
        ),
    ];
    let scope = Scope::Model {
        model: "Task".to_string(),
        id: base_uri.to_string(),
        field: None,
    };

    let bases = run_interpretation(
        &mut perspective,
        &shapes,
        &transcript,
        "soa://ext/",
        &ctx,
        Some(&scope),
    )
    .await
    .expect("run_interpretation against real LLM to succeed with gated flow in play");

    let placements = read_back_placements(&perspective, &bases).await;
    print_placements(&placements);
    let counts = graph_count_by_type(&perspective, &shapes).await;
    let tasks = counts.get("task").copied().unwrap_or(0);
    let proposals = proposals_for_instance(&perspective, &inst_uri).await;
    (perspective, tasks, inst_uri, proposals)
}

/// YES path: the semantic-check hint is plainly satisfied by the scoping
/// conversation, so once the extraction lands a Task (satisfying the
/// structural `requires` guard), the real LLM's confirmation must let the
/// engine mint a `FlowTransitionProposal` for `identified → scoped`.
///
/// This is the test PR review asked for: an LLM integration test where a
/// proposal is *created from* a semantic check — no `CannedLlm`, the
/// production `AIServiceSemanticCheck` against the live model.
///
/// Retries up to 3× (fresh perspective each attempt): two sequential
/// real-LLM calls compound single-sample flake, and either the extraction
/// missing the Task or the check waffling to UNCLEAR is a flake, not a
/// regression — three misses in a row is a regression.
#[tokio::test(flavor = "multi_thread")]
#[ignore = "llm-e2e"]
async fn semantic_check_yes_mints_proposal_real_llm() {
    let hint =
        "The evidence describes concrete, actionable pieces of work that named team members \
         have agreed to take on.";
    let attempts: u8 = 3;
    let mut last: Option<(usize, Vec<String>)> = None;

    for i in 1..=attempts {
        let (perspective, tasks, _inst_uri, proposals) =
            run_gated_pipeline_attempt(hint, "e2e-semcheck-yes").await;
        eprintln!(
            "[real-llm-e2e/semcheck-yes] attempt {i}/{attempts} tasks={tasks} proposals={proposals:?}"
        );

        if tasks >= 1 && !proposals.is_empty() {
            // The minted proposal must be the gated transition, not some
            // other write: `to_state` is the guarded `scoped` state.
            let to_states: Vec<String> = perspective
                .get_links(&LinkQuery {
                    source: Some(proposals[0].clone()),
                    predicate: Some("ad4m://flow/to_state".to_string()),
                    ..Default::default()
                })
                .await
                .expect("get_links(to_state)")
                .into_iter()
                .map(|l| l.data.target)
                .collect();
            assert!(
                to_states.iter().any(|t| t == "literal:string:scoped"),
                "proposal must target the semantically-gated `scoped` state, got {to_states:?}"
            );
            return;
        }
        last = Some((tasks, proposals));
    }

    let (last_tasks, last_proposals) = last.unzip();
    panic!(
        "semantic-check YES path never minted a proposal in {attempts} attempts \
         (last attempt: tasks={last_tasks:?} proposals={last_proposals:?}); either \
         extraction kept missing the Task or the real LLM kept failing a \
         plainly-satisfied check"
    );
}

/// NO path: the semantic-check hint is plainly *contradicted* by the same
/// conversation — nothing in it was cancelled or abandoned — so even with
/// the structural `requires` guard satisfied, the gate must withhold the
/// proposal. Fail-closed is the property under test: an attempt that
/// extracted a Task (i.e. actually reached the check) and still minted a
/// proposal is a hard failure, no retry — a real model answering YES to a
/// blatantly contradicted check means the gate leaks. Attempts where no
/// Task landed never exercised the gate and are retried.
#[tokio::test(flavor = "multi_thread")]
#[ignore = "llm-e2e"]
async fn semantic_check_no_withholds_proposal_real_llm() {
    let hint = "The evidence shows the team explicitly cancelled this work and abandoned it with \
         nobody assigned.";
    let attempts: u8 = 3;

    for i in 1..=attempts {
        let (_perspective, tasks, _inst_uri, proposals) =
            run_gated_pipeline_attempt(hint, "e2e-semcheck-no").await;
        eprintln!(
            "[real-llm-e2e/semcheck-no] attempt {i}/{attempts} tasks={tasks} proposals={proposals:?}"
        );

        if tasks >= 1 {
            assert!(
                proposals.is_empty(),
                "semantic check on a contradicted hint must withhold the proposal \
                 (fail-closed), but the engine minted {proposals:?}"
            );
            return;
        }
        eprintln!(
            "[real-llm-e2e/semcheck-no] attempt {i}/{attempts} extracted no Task — gate never \
             reached; retrying"
        );
    }

    panic!(
        "no attempt out of {attempts} extracted a Task, so the NO-case semantic gate was \
         never exercised"
    );
}
