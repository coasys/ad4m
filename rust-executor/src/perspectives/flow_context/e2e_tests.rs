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
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::shacl_parser::parse_flow_to_links;
use crate::types::{Link, LinkStatus};

/// URL-encoded string literal target, matching the wire shape
/// `parse_flow_from_links` decodes.
fn lit(s: &str) -> String {
    format!("literal:string:{}", urlencoding::encode(s))
}

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

    // 2) Seed the Delivery flow *definition* the way any producer
    //    would. The writer half `parse_flow_to_links` covers the v4
    //    predicates (type / flowName / hasState / stateName /
    //    stateValue / stateCheck / hasTransition / actionName /
    //    fromState / toState); the v5 predicates (`interpretationHint`
    //    at flow + state scope, `inputTypes`) are appended by hand
    //    below. This is deliberate — the writer will grow v5 emission
    //    later as part of the round-trip mirror-symmetry work, but
    //    the reader shipped in slice 10.3a already knows how to walk
    //    the v5 shape and Model C needs to consume it *today*. The
    //    test asserting the reader picks these up is what pins that
    //    guarantee down.
    let flow_uri = "delivery://DeliveryFlow";
    let identified_uri = "delivery://Delivery.identified";
    let scoped_uri = "delivery://Delivery.scoped";
    let flow_links = parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)");
    for link in flow_links {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    let v5_links = vec![
        Link {
            source: flow_uri.to_string(),
            predicate: Some("ad4m://interpretationHint".to_string()),
            target: lit("A team-scale unit of work moving from identification to done."),
        },
        Link {
            source: flow_uri.to_string(),
            predicate: Some("ad4m://inputTypes".to_string()),
            target: lit("[\"ad4m://Task\"]"),
        },
        Link {
            source: identified_uri.to_string(),
            predicate: Some("ad4m://interpretationHint".to_string()),
            target: lit("The team has named a piece of work but has not yet scoped it."),
        },
        Link {
            source: scoped_uri.to_string(),
            predicate: Some("ad4m://interpretationHint".to_string()),
            target: lit("The team has agreed on what the work is and can begin execution."),
        },
    ];
    for link in v5_links {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v5)");
    }

    // 3) Mint a FlowInstance on the base URI the extraction pass
    //    would anchor on. This is the runtime primitive from slice
    //    9 — it also idempotently registers the FlowInstance model
    //    class, which is the class `load_flow_instances` queries.
    let base_uri = "ad4m://task/onboarding";
    let inst_uri = mint_flow_instance(
        &mut perspective,
        "Delivery",
        base_uri,
        "identified",
        "e2e-inst-1",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    // 4) Perspective-wide gather — no scope narrowing. This is the
    //    shape `run_interpretation_with_strategy_and_model` calls
    //    on batch entry.
    let contexts = gather_active_flow_contexts(&perspective, None).await;
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

    // 5) Scope-narrowed gather on the same base URI: still one
    //    context. This is the shape run.rs calls when the pass is
    //    scoped to a specific model instance.
    let matching_scope = Scope::Model {
        model: "Task".to_string(),
        id: base_uri.to_string(),
        field: None,
    };
    let scoped = gather_active_flow_contexts(&perspective, Some(&matching_scope)).await;
    assert_eq!(
        scoped.len(),
        1,
        "scope-narrowed gather on the instance's own subject must still see it"
    );
    assert_eq!(scoped[0].instance_uri, inst_uri);

    // 6) Scope narrowing to a *different* base URI: empty. This is
    //    the property that lets scope-scoped passes ignore flows
    //    running on unrelated bases.
    let other_scope = Scope::Model {
        model: "Task".to_string(),
        id: "ad4m://task/unrelated".to_string(),
        field: None,
    };
    let other = gather_active_flow_contexts(&perspective, Some(&other_scope)).await;
    assert!(
        other.is_empty(),
        "scope narrowed to a different subject must drop the running flow, got {other:?}"
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
