//! End-to-end integration test — the onion-shell test-cut Nico called
//! out on 2026-08-26 ("cut based on what we can test really, stack it
//! like onions").
//!
//! Extracted into its own file on 2026-08-27 as part of PR #929 review
//! R4 — the tests below cover the pure halves of the module against
//! hand-built inputs; this file covers the composition against a real
//! [`PerspectiveInstance`].
//!
//! # What it exercises
//!
//! Composition — the real path a live extraction pass walks:
//!
//! - real perspective (SPARQL + Prolog + SDNA/model_query stack, no LLM)
//! - SHACLFlow definition seeded as actual links (writer half of the
//!   parser round-trip: `parse_flow_to_links` → `add_link`)
//! - FlowInstance minted via the runtime primitive (`mint_flow_instance`
//!   — the same call the auto-processor will make)
//! - `gather_active_flow_contexts` walking the perspective through the
//!   model_query layer
//! - resulting `FlowContext[]` fed to `build_interpretation_input` —
//!   what `run.rs` does after slice 10.3c wired both call sites.
//!
//! If this test stays green, slice 10.3c's `run.rs` wiring is by
//! construction correct: the two lines it replaced (previously passing
//! `&[]` to `build_interpretation_input`) now receive the exact
//! `Vec<FlowContext>` this test builds, and slice 10.2's unit tests
//! (in `render.rs` / `interpretation::input_builder`) already cover
//! what happens on that value inside the prompt builder.
//!
//! No LLM is spun up — `setup_perspective_no_llm` gives a real
//! `PerspectiveInstance`. The only piece that would need Ollama is the
//! AIService, which none of the code paths under test touch.

use super::loader::gather_active_flow_contexts;
use crate::perspectives::flow_classes::mint_flow_instance;
use crate::perspectives::interpretation::{
    build_interpretation_input, ExistingInstances, TranscriptTurn,
};
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::perspectives::shacl_parser::parse_flow_to_links;
use crate::types::LinkStatus;

/// Delivery flow JSON matching what `SHACLFlow.toJSON()` emits on the TS
/// side (and what `parse_flow_to_links` deserializes into a `SHACLFlow`).
/// The state names + hints are the ones the assertions below key on;
/// keep them in sync when this fixture evolves.
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
                    "The team has agreed on what the work is and can begin execution.",
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
        "inputTypes": ["ad4m://Task"],
        "outputTypes": [],
    })
    .to_string()
}

#[tokio::test(flavor = "multi_thread")]
async fn gather_active_flow_contexts_wires_definition_and_instance_e2e() {
    // 1) Real perspective, no LLM — everything else is genuine
    //    (SPARQL store, Prolog engine, add_sdna registration path,
    //     model_query resolution).
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

    // 2) Seed the Delivery flow *definition* the way any producer would.
    //    `parse_flow_to_links` now emits every field the reader consumes
    //    (`interpretationHint` at flow + state scope, `inputTypes`,
    //    `outputTypes`, `requires`, `semanticCheck`, `consensusRule`,
    //    `creationHint`, `context`), so the fixture is a single JSON blob
    //    round-tripped through the writer — no hand-appended predicate
    //    scaffolding.
    let flow_links = parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)");
    for link in flow_links {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition)");
    }

    // 3) Mint a FlowInstance on the base URI the extraction pass
    //    would anchor on. This is the runtime primitive from slice
    //    9 — it also idempotently registers the FlowInstance model
    //    class, which is the class `load_flow_instances` queries.
    let base_uri = "ad4m://task/onboarding";
    // Pass the flow's canonical URI (`${namespace}${name}Flow`), not the
    // bare name — `mint_flow_instance` writes into the record's
    // `flowUri` property, which `build_flow_contexts` joins on
    // (James PR #929 R5).
    let inst_uri = mint_flow_instance(
        &mut perspective,
        "delivery://DeliveryFlow",
        base_uri,
        "identified",
        "e2e-inst-1",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    // 4) Batch-scoped gather — the drained batch includes the base
    //    URI the FlowInstance is anchored on. This is the shape
    //    `run_interpretation_with_strategy_and_model` calls on batch
    //    entry when the auto-processor drains an item whose URI is the
    //    FlowInstance's subject (J#1, PR #929 James review — the fix
    //    replaced `Option<&Scope>` on the dedup axis with
    //    `subjects: &[String]` sourced from the batch cursor).
    let contexts = gather_active_flow_contexts(&perspective, &[base_uri.to_string()]).await;
    assert_eq!(
        contexts.len(),
        1,
        "one active Delivery instance ⇒ exactly one FlowContext, got {contexts:?}"
    );
    let fc = &contexts[0];
    assert_eq!(fc.flow_name, "Delivery");
    assert_eq!(fc.instance_uri, inst_uri);
    assert_eq!(fc.subject, base_uri);
    assert_eq!(fc.current_state, "identified");
    assert_eq!(
        fc.flow_interpretation_hint.as_deref(),
        Some("A team-scale unit of work moving from identification to done.")
    );
    // `identified → scoped` is the only transition, so exactly one
    // reachable next-state, and its per-state hint is the one the
    // LLM will see in the prompt.
    assert_eq!(fc.reachable_next_states.len(), 1);
    let next = &fc.reachable_next_states[0];
    assert_eq!(next.name, "scoped");
    assert_eq!(
        next.interpretation_hint.as_deref(),
        Some("The team has agreed on what the work is and can begin execution.")
    );

    // 5) Multi-subject batch containing the instance's subject: still
    //    one context. Proves the multi-URI `where subject == ..` fallback
    //    (query-all + filter-in-Rust) correctly retains the match.
    let multi_matching = vec![
        base_uri.to_string(),
        "ad4m://task/other-batch-item".to_string(),
    ];
    let scoped = gather_active_flow_contexts(&perspective, &multi_matching).await;
    assert_eq!(
        scoped.len(),
        1,
        "multi-subject batch containing the instance's subject must still see it"
    );
    assert_eq!(scoped[0].instance_uri, inst_uri);

    // 6) Subjects that don't include the instance's base URI: empty.
    //    This is the property that lets batch-scoped passes ignore
    //    flows running on unrelated bases.
    let other =
        gather_active_flow_contexts(&perspective, &["ad4m://task/unrelated".to_string()]).await;
    assert!(
        other.is_empty(),
        "batch narrowed to a different subject must drop the running flow, got {other:?}"
    );

    // 6b) Empty subjects — belt-and-braces: the pre-fix `None` path
    //     used to sweep every FlowInstance on the perspective and
    //     inject it into every prompt (unbounded). The fix makes empty
    //     mean empty — no flow context, no unbounded sweep.
    let empty = gather_active_flow_contexts(&perspective, &[] as &[String]).await;
    assert!(
        empty.is_empty(),
        "empty subjects must not surface any flows, got {empty:?}"
    );

    // 7) The real payoff: feed the gathered context into the
    //    interpretation prompt builder — the same call `run.rs`
    //    makes after slice 10.3c substituted this vector for `&[]`.
    //    The prompt must carry an `active_flows` array whose only
    //    element identifies our Delivery instance by name.
    let existing = ExistingInstances::new();
    let transcript = vec![TranscriptTurn::from_speaker_text("A", "irrelevant")];
    let prompt = build_interpretation_input(&[], &transcript, &existing, &contexts);
    let parsed: serde_json::Value = serde_json::from_str(&prompt).expect("prompt is valid JSON");
    let flows_in_prompt = parsed
        .get("active_flows")
        .and_then(|v| v.as_array())
        .expect("active_flows array present in prompt when contexts are non-empty");
    assert_eq!(
        flows_in_prompt.len(),
        1,
        "one FlowContext ⇒ one prompt entry, got {flows_in_prompt:?}"
    );
    assert_eq!(
        flows_in_prompt[0]
            .get("flow")
            .and_then(|v| v.as_str())
            .expect("active_flows[0].flow is a string"),
        "Delivery"
    );
}
