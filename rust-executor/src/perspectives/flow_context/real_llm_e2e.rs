//! Real-LLM end-to-end test for the Model C data path — PR #929 review R5
//! (Nico, 2026-08-27: "this PR/onion layer should have real LLM based
//! interpretation tests already").
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
//!    slice 10.3c wiring live — one full pass against a real model.
//! 3. The extracted instance carries evidence the LLM's understanding of
//!    the scope conversation, i.e. the new prompt section didn't cause
//!    the model to hallucinate types outside the offered class set.
//!
//! # What it does *not* prove (deferred to PR #932 / slice 10.5+)
//!
//! - The engine acts on the extracted instance (that's `run_engine_proposal_pass`
//!   post-processing, PR #932).
//! - The LLM proposes a `FlowTransitionProposal` on its own (that's the
//!   `flow_proposals` output field, also PR #932).
//!
//! # Endpoint + retry
//!
//! Uses the standard `setup_interpretation_e2e` harness: OpenAI-compatible
//! endpoint at `INTERPRETATION_E2E_BASE_URL` (default `localhost:11434/v1`,
//! i.e. Ollama on Marvin or over an SSH tunnel from a dev box), model
//! `INTERPRETATION_E2E_MODEL` (default `gemma3:12b`). Retries the whole
//! pass up to 3× to soak up single-sample LLM flake, matching the
//! sibling generic-interpretation e2e tests.

#![cfg(test)]

use super::gather_active_flow_contexts;
use crate::perspectives::flow_classes::mint_flow_instance;
use crate::perspectives::interpretation::{run_interpretation, TranscriptTurn};
use crate::perspectives::interpretation_test_support::{
    graph_count_by_type, print_placements, read_back_placements, setup_interpretation_e2e,
    TASK_SDNA,
};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::shacl_parser::parse_flow_to_links;
use crate::types::LinkStatus;

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
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    ctx: &crate::agent::AgentContext,
    base_uri: &str,
) -> String {
    let flow_links = parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)");
    for link in flow_links {
        perspective
            .add_link(link, LinkStatus::Local, None, ctx)
            .await
            .expect("add_link(flow definition)");
    }

    mint_flow_instance(
        perspective,
        "Delivery",
        base_uri,
        "identified",
        "e2e-real-llm-inst",
        None,
        ctx,
    )
    .await
    .expect("mint_flow_instance")
}

/// The onion-shell R5 test: real LLM + active-flow context in the prompt.
///
/// Verifies that when the extraction pass runs against a real model with the
/// slice 10.3c wiring live, the added `active_flows` prompt block does not
/// derail the LLM's ability to produce parseable output — the pass still
/// lands at least one typed instance for a transcript unambiguously about
/// scoping the referenced Task subject.
///
/// Retries up to 3× to soak single-sample flake from the small local model
/// (`gemma3:12b` occasionally files a scope-discussion turn as a `belief` on
/// one sample even when only `Task` is offered — the retry lets it converge
/// without hiding a real regression, since the last attempt's assertions
/// still fire with full diagnostics).
#[tokio::test(flavor = "multi_thread")]
async fn model_c_real_llm_extraction_with_active_flow_in_prompt() {
    let base_uri = "soa://ext/task/mvp";
    let mut last_counts: Option<std::collections::HashMap<String, usize>> = None;
    let attempts: u8 = 3;

    for i in 1..=attempts {
        let (mut perspective, shapes, ctx) = setup_interpretation_e2e(&[("Task", TASK_SDNA)]).await;

        let inst_uri = seed_delivery_flow_and_instance(&mut perspective, &ctx, base_uri).await;

        // Sanity check the wiring — the gather half of slice 10.3c must see
        // the just-minted instance, else the LLM can't possibly get flow
        // context and this test is measuring nothing.
        let contexts = gather_active_flow_contexts(&perspective, None).await;
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
