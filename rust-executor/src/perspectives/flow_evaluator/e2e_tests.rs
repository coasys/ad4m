// ============================================================================
// Live-perspective integration test
// ============================================================================
//
// The stub-perspective tests above pin every failure mode inside the
// composer (unknown flow, guardless state, empty requires, unsatisfied
// cardinality, `or` composition, per-state consensus override, query
// error) but they route around the real `PerspectiveInstance::model_query`
// implementation. What they *cannot* prove is that the
// `ModelQuery → ModelQueryInput` translation `build_query_input_for_requires`
// emits is a shape the real SPARQL/SDNA-backed `model_query` actually
// accepts. This module closes that gap with one end-to-end pass against
// a genuine perspective (real store, real Prolog, real `add_sdna`, real
// `create_subject`, real `model_query`).
//
// Complements the read-side integration test in `flow_context/e2e_tests.rs`:
//   - Read side: definitions + minted instance → `FlowContext[]` +
//                rendered prompt block.
//   - Write side (this): same substrate → committed evidence flowing back
//                through `model_query` → deterministic
//                `SatisfiedTransition[]` → on-graph
//                `FlowTransitionProposal` writes.
//
// No LLM is spun up.
use super::engine_pass::write_engine_proposal;
use super::primitives::evidence_hash;
use super::queryable::evaluate_flow_transitions;
use super::*;
use crate::perspectives::flow_classes::mint_flow_instance;
use crate::perspectives::flow_context::{load_flow_instances, load_shacl_flows};
use crate::perspectives::interpretation_test_support::{
    seed_instance, setup_perspective_no_llm, TASK_SDNA,
};
use crate::perspectives::shacl_parser::parse_flow_to_links;
use crate::types::{Link, LinkStatus};
use std::collections::HashMap;

/// URL-encoded string-literal target, matching the wire shape the
/// `shacl_parser` link reader decodes.
fn lit(s: &str) -> String {
    format!("literal:string:{}", urlencoding::encode(s))
}

/// Minimal two-state Delivery flow: `identified → scoped`. The
/// `requires` guard on `scoped` (at least one `ns://Task`) is
/// appended as a v5 link *after* `parse_flow_to_links` emits the
/// v4 predicates — same seeding pattern as the 10.3d e2e test.
fn delivery_flow_json() -> String {
    serde_json::json!({
        "name": "Delivery",
        "namespace": "delivery://",
        "states": [
            {
                "name": "identified",
                "value": 0.0,
            },
            {
                "name": "scoped",
                "value": 0.5,
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
    })
    .to_string()
}

#[tokio::test(flavor = "multi_thread")]
async fn evaluate_flow_transitions_wires_definition_and_evidence_e2e() {
    // 1) Real perspective, no LLM, `ns://Task` registered via
    //    `add_sdna` — this is the same path the interpretation
    //    pipeline uses, so `model_query("ns://Task", ...)` will
    //    resolve against the real SPARQL store.
    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    // 2) Seed the Delivery flow definition. The v4 predicates
    //    (type / flowName / hasState / hasTransition / …) go
    //    through the writer; the one v5 predicate we need — the
    //    `requires` link on the `scoped` state — is added by hand
    //    because `parse_flow_to_links` does not yet emit v5. The
    //    link reader already walks the v5 shape, and the evaluator
    //    needs to consume it today; this test pins that contract
    //    until the writer catches up.
    let scoped_uri = "delivery://Delivery.scoped";
    for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)")
    {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    // Cardinality `{ min: 1 }` is deliberate — proves the guard
    // has genuinely got teeth (0 = unmet, 1 = met) rather than
    // trivially satisfied by unset-count-defaults.
    let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: lit(requires_json),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.requires)");

    // 3) Mint a FlowInstance in `identified` on a base URI. Same
    //    call site the auto-processor / Model C write-side will
    //    use.
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

    // 4) Load records + catalogue exactly as the auto-processor will.
    //    Same shape the read-side integration test exercises, but this
    //    time both are fed into the *write*-side gate.
    let records = load_flow_instances(&perspective, &[base_uri.to_string()])
        .await
        .expect("load_flow_instances");
    assert_eq!(records.len(), 1, "one active FlowInstance ⇒ one record");
    let flows_by_uri = load_shacl_flows(&perspective)
        .await
        .expect("load_shacl_flows");
    assert_eq!(
        flows_by_uri.len(),
        1,
        "one Delivery definition ⇒ one catalogue entry"
    );
    // Reader guarantee: the hand-seeded v5 `requires` link must
    // survive the round-trip back into a `ModelQuery[]` — otherwise
    // the evaluator would see `None` and silent-skip regardless of
    // the graph state, which would make the negative path below a
    // false positive.
    let scoped = flows_by_uri
        .get("delivery://DeliveryFlow")
        .expect("Delivery in catalogue")
        .states
        .iter()
        .find(|s| s.name == "scoped")
        .expect("scoped state parsed");
    let reqs = scoped
        .requires
        .as_deref()
        .expect("scoped.requires decoded from v5 link");
    assert_eq!(reqs.len(), 1);
    assert_eq!(reqs[0].class_name, "ns://Task");

    // 5) Negative pass: no Task in the graph ⇒ requires unmet ⇒
    //    zero satisfied transitions. Proves the "unmet" branch is
    //    reached through the real SPARQL/SDNA stack, not just the
    //    stub.
    let before =
        evaluate_flow_transitions(&perspective, &records, &flows_by_uri, "did:key:acting").await;
    assert!(
        before.is_empty(),
        "no Task instances ⇒ requires unmet ⇒ 0 satisfied, got {before:?}"
    );

    // 6) Seed a real Task via the same `create_subject` path the
    //    interpretation pipeline uses. This is what makes the
    //    positive assertion below a genuine end-to-end proof: the
    //    evidence has to come out of the store the evaluator
    //    queries, not out of a canned stub response.
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/1",
        "Onboard Ana",
    )
    .await;

    // 7) Positive pass: one FlowInstance × one reachable next-state
    //    × requires met ⇒ exactly one SatisfiedTransition.
    let after =
        evaluate_flow_transitions(&perspective, &records, &flows_by_uri, "did:key:acting").await;
    assert_eq!(
        after.len(),
        1,
        "requires met ⇒ 1 satisfied transition, got {after:?}"
    );
    let t = &after[0];
    assert_eq!(t.flow_name, "Delivery");
    assert_eq!(t.instance_uri, inst_uri);
    assert_eq!(t.subject, base_uri);
    assert_eq!(t.from_state, "identified");
    assert_eq!(t.to_state, "scoped");
    assert!(
        t.evidence_ids.contains(&"ad4m://task/1".to_string()),
        "evidence_ids must include the seeded Task URI, got {:?}",
        t.evidence_ids
    );
    assert_eq!(
        t.evidence_hash,
        evidence_hash(&["ns://Task".to_string()], &t.evidence_ids),
        "evidence_hash must be a deterministic seal over (class_names, evidence_ids)"
    );
    // Flow definition carries no per-state semanticCheck and no
    // consensus rule (flow-level or per-state) — both must fall
    // through unset. The semanticCheck path is exercised in the
    // gate-wired e2e test below; the stub tests above cover
    // consensus override precedence.
    assert!(t.semantic_check.is_none());
    assert!(t.consensus_rule.is_none());
}

/// Write-side end-to-end. Re-uses the fixture above (real perspective,
/// Delivery flow with `requires: 1 × ns://Task`, one active
/// FlowInstance, one seeded Task ⇒ one `SatisfiedTransition`). On top
/// of that, this test calls [`write_engine_proposal`] and asserts
/// every declared FlowTransitionProposal predicate landed on-graph
/// with the expected target.
///
/// Assertions cover the two silent-failure modes the writer has to
/// rule out:
/// - **Wrong-key drop** — 2026-08-20 bug shape where a mismatched
///   JSON key returns Ok without writing. The parity test in
///   `flow_classes.rs` guards static shape; this test proves the
///   dynamic write path is honest.
/// - **Collection under-write** — `create_subject` writing only the
///   first collection element (last-one-wins on a `setSingleTarget`
///   setter). The write path fans out subsequent elements through
///   `update_subject`; this test seeds 2 Tasks to exercise
///   fan-out and asserts both `flow/evidence` targets land.
#[tokio::test(flavor = "multi_thread")]
async fn write_engine_proposal_lands_all_declared_predicates_e2e() {
    use crate::types::LinkQuery;

    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    // Seed the Delivery flow definition (v4 predicates + hand-added
    // v5 requires link) exactly as the read-side test above.
    let scoped_uri = "delivery://Delivery.scoped";
    for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)")
    {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: lit(requires_json),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.requires)");

    let base_uri = "ad4m://task/onboarding";
    let inst_uri = mint_flow_instance(
        &mut perspective,
        "Delivery",
        base_uri,
        "identified",
        "e2e-inst-writer",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    // Seed TWO Tasks so the evidence collection has more than one
    // element — proves the create_subject + update_subject fan-out
    // (not a single-target overwrite) actually lands both.
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/1",
        "Onboard Ana",
    )
    .await;
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/2",
        "Onboard Bo",
    )
    .await;

    let records = load_flow_instances(&perspective, &[base_uri.to_string()])
        .await
        .expect("load_flow_instances");
    let flows_by_uri = load_shacl_flows(&perspective)
        .await
        .expect("load_shacl_flows");
    let satisfied =
        evaluate_flow_transitions(&perspective, &records, &flows_by_uri, "did:key:acting").await;
    assert_eq!(
        satisfied.len(),
        1,
        "one active FlowInstance × one reachable next-state × requires met ⇒ 1"
    );
    let t = &satisfied[0];
    assert_eq!(
        t.evidence_ids.len(),
        2,
        "two seeded Tasks ⇒ two evidence entries, got {:?}",
        t.evidence_ids
    );

    // Write the proposal via the convenience wrapper. Caller-supplied
    // id mirrors the mint_flow_instance contract; propose-time is
    // synthesised on-graph by `Ad4mModel`'s `createdAt`.
    let proposer_did = "did:key:acting";
    let proposal_uri = write_engine_proposal(
        &mut perspective,
        "e2e-prop-1",
        proposer_did,
        t,
        None, // rationale — this e2e is the engine-only path (no LLM attribution)
        None,
        &ctx,
    )
    .await
    .expect("write_engine_proposal");
    assert_eq!(
        proposal_uri, "ad4m://flow/proposal/e2e-prop-1",
        "URI scheme mirrors flow_transition_proposal_uri",
    );

    // Query every outgoing link from the proposal URI and index by
    // predicate. Every one of the seven declared SDNA properties
    // must have at least one link landed.
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("get_links(proposal)");
    let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
    for l in &links {
        if let Some(pred) = &l.data.predicate {
            by_pred
                .entry(pred.clone())
                .or_default()
                .push(l.data.target.clone());
        }
    }

    // URI-valued predicate: safe-IRI ⇒ stored raw, no literal wrap.
    let inst_targets = by_pred
        .get("ad4m://flow/instance")
        .expect("proposal must carry flow/instance link");
    assert!(
        inst_targets.contains(&inst_uri),
        "flow/instance target must be the minted instance URI, got {inst_targets:?}",
    );

    // Evidence: two safe-IRI targets ⇒ both stored raw, both must land.
    let evidence_targets = by_pred
        .get("ad4m://flow/evidence")
        .expect("proposal must carry flow/evidence links");
    for expected in ["ad4m://task/1", "ad4m://task/2"] {
        assert!(
            evidence_targets.iter().any(|t| t == expected),
            "flow/evidence collection must include `{expected}`, got {evidence_targets:?} \
             — write_flow_transition_proposal's create_subject + update_subject fan-out \
             must land every element (regression guard for silent last-one-wins)",
        );
    }

    // The proposer is a `did:key:…` URI. `looks_like_absolute_iri`
    // accepts it (starts with ASCII letter, has a colon), so
    // `resolve_property_value` stores it raw — not literal-wrapped.
    // Guarding both branches of the writer's encoding here so a
    // future is_safe_iri_target tweak that changes DID handling is
    // caught by test rather than by a downstream DID-lookup failure.
    let proposer_targets = by_pred
        .get("ad4m://flow/proposer")
        .expect("proposal must carry flow/proposer link");
    assert!(
        proposer_targets.iter().any(|t| t == proposer_did),
        "flow/proposer target must be the raw DID URI, got {proposer_targets:?}",
    );

    // Non-IRI string predicates: `literal:string:` wrapped because
    // they either start with a digit (timestamp), contain no `:`
    // (state names / hex hash), or otherwise fail is_safe_iri_target.
    // Assert the exact wire form so a future encoding change is
    // caught here (and reviewers reading a proposal on-graph can
    // tell at a glance which fields are typed literals).
    for (pred, expected) in [
        ("ad4m://flow/from_state", "identified"),
        ("ad4m://flow/to_state", "scoped"),
        ("ad4m://flow/evidence_hashes", t.evidence_hash.as_str()),
    ] {
        let targets = by_pred
            .get(pred)
            .unwrap_or_else(|| panic!("proposal must carry {pred} link"));
        let want = format!("literal:string:{}", urlencoding::encode(expected));
        assert!(
            targets.iter().any(|t| t == &want),
            "{pred} target must be `{want}`, got {targets:?}",
        );
    }
}

/// End-to-end onion shell for the auto-processor entry point. Verifies
/// that a single call to [`run_engine_proposal_pass`] against a live
/// perspective:
///
/// 1. Loads flows + records + evaluates + writes without any
///    caller-side plumbing.
/// 2. Returns the URIs of every minted proposal.
/// 3. Actually lands each proposal on-graph with the acting DID
///    as `proposer` — a per-transition detail that would silently
///    fail if `did_for_context` were bypassed.
///
/// This is the shape `interpretation::run::run_interpretation_with_strategy_and_model`
/// calls after `apply_with_overlay`. If this test passes, the
/// extraction pass becomes flow-post-processing-aware end-to-end.
#[tokio::test(flavor = "multi_thread")]
async fn run_engine_proposal_pass_lands_a_proposal_e2e() {
    use crate::types::LinkQuery;

    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    // Same Delivery + `requires` seed as the evaluator + writer
    // e2e tests. `requires: 1 × ns://Task` on the `scoped` state,
    // so an unseeded graph = 0 proposals, one seeded Task = 1.
    let scoped_uri = "delivery://Delivery.scoped";
    for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)")
    {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: lit(requires_json),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.requires)");

    let base_uri = "ad4m://task/onboarding";
    let inst_uri = mint_flow_instance(
        &mut perspective,
        "Delivery",
        base_uri,
        "identified",
        "e2e-10.4c-inst",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    // Empty graph — no Task seeded — must return zero proposals.
    // Proves the pass is genuinely gated on the guard's satisfaction
    // and doesn't optimistically mint on every pass.
    let before = crate::perspectives::flow_evaluator::run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        None,
        &[], // llm_hints — engine-only path (no LLM attribution)
    )
    .await;
    assert!(
        before.is_empty(),
        "empty graph ⇒ 0 satisfied ⇒ 0 proposals, got {before:?}",
    );

    // Now seed the evidence — the exact same `create_subject` path
    // the interpretation pipeline uses, so the eventual proposal
    // reflects real committed graph state, not a canned response.
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/1",
        "Onboard Ana",
    )
    .await;

    let minted = crate::perspectives::flow_evaluator::run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        None,
        &[], // llm_hints — engine-only path (no LLM attribution)
    )
    .await;
    assert_eq!(
        minted.len(),
        1,
        "one satisfied transition ⇒ one proposal, got {minted:?}",
    );
    let proposal_uri = &minted[0];
    assert!(
        proposal_uri.starts_with("ad4m://flow/proposal/"),
        "proposal URI must follow flow_transition_proposal_uri scheme, got {proposal_uri}",
    );

    // Resolve the acting DID the same way `run_engine_proposal_pass`
    // did internally, so the assertion below tests actual identity
    // threading rather than tolerating any DID that happens to land.
    let acting_did =
        crate::agent::did_for_context(&ctx).expect("did_for_context on test agent context");

    // Walk the proposal on-graph and confirm proposer + linked
    // instance + from/to states. The `write_engine_proposal` e2e
    // test above already covers every declared predicate; here we
    // spot-check the fields that would silently regress if the
    // acting-DID plumbing broke or `run_engine_proposal_pass`
    // truncated the SatisfiedTransition it hands to the writer.
    let links = perspective
        .get_links(&LinkQuery {
            source: Some(proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("get_links(proposal)");
    let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
    for l in &links {
        if let Some(pred) = &l.data.predicate {
            by_pred
                .entry(pred.clone())
                .or_default()
                .push(l.data.target.clone());
        }
    }

    assert!(
        by_pred
            .get("ad4m://flow/proposer")
            .map(|ts| ts.iter().any(|t| t == &acting_did))
            .unwrap_or(false),
        "proposer must be the acting DID resolved via did_for_context, \
         got {:?}",
        by_pred.get("ad4m://flow/proposer"),
    );
    assert!(
        by_pred
            .get("ad4m://flow/instance")
            .map(|ts| ts.iter().any(|t| t == &inst_uri))
            .unwrap_or(false),
        "flow/instance must be the minted instance URI, got {:?}",
        by_pred.get("ad4m://flow/instance"),
    );
    for (pred, expected) in [
        ("ad4m://flow/from_state", "identified"),
        ("ad4m://flow/to_state", "scoped"),
    ] {
        let want = format!("literal:string:{}", urlencoding::encode(expected));
        assert!(
            by_pred
                .get(pred)
                .map(|ts| ts.iter().any(|t| t == &want))
                .unwrap_or(false),
            "{pred} must carry `{want}`, got {:?}",
            by_pred.get(pred),
        );
    }
}

// -----------------------------------------------------------------
// Semantic-check gate wired into `run_engine_proposal_pass`
// -----------------------------------------------------------------

/// Stub [`SemanticCheckLlm`] whose `confirm` returns a canned response
/// verbatim. Records prompt + model_id per call so the tests can
/// assert the gate is threading the right context down to the LLM
/// (same pattern the async-layer unit tests in
/// `flow_semantic_check` use).
struct CannedLlm {
    response: String,
    error: Option<String>,
    calls: std::sync::Mutex<Vec<(String, String)>>, // (model_id, prompt)
}

impl CannedLlm {
    fn responding(text: &str) -> Self {
        Self {
            response: text.to_string(),
            error: None,
            calls: std::sync::Mutex::new(Vec::new()),
        }
    }
    fn erroring(msg: &str) -> Self {
        Self {
            response: String::new(),
            error: Some(msg.to_string()),
            calls: std::sync::Mutex::new(Vec::new()),
        }
    }
    fn call_count(&self) -> usize {
        self.calls.lock().unwrap().len()
    }
}

#[async_trait::async_trait]
impl crate::perspectives::flow_semantic_check::SemanticCheckLlm for CannedLlm {
    async fn confirm(&self, model_id: &str, prompt: &str) -> anyhow::Result<String> {
        self.calls
            .lock()
            .unwrap()
            .push((model_id.to_string(), prompt.to_string()));
        if let Some(msg) = &self.error {
            return Err(anyhow::anyhow!(msg.clone()));
        }
        Ok(self.response.clone())
    }
}

/// Seed the same Delivery + `requires` + FlowInstance shape the
/// 10.4c e2e uses, PLUS a per-state `ad4m://semantic_check` hint on
/// `scoped`. Returns `(perspective, ctx, instance_uri)`.
async fn seed_semantic_check_e2e_fixture(
    semantic_check_hint: &str,
) -> (
    crate::perspectives::perspective_instance::PerspectiveInstance,
    crate::agent::AgentContext,
    String,
) {
    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    let scoped_uri = "delivery://Delivery.scoped";
    for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)")
    {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: lit(requires_json),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.requires)");
    // The semantic-check payload — per-state hint (predicate is
    // `ad4m://semanticCheck` in camelCase to match the parser at
    // `shacl_parser::find_link`). The link reader mounts it on
    // `FlowState.semantic_check`, which
    // `evaluate_flow_transitions` threads into
    // `SatisfiedTransition.semantic_check`, which
    // `build_semantic_check_prompt` uses to produce a non-`None`
    // prompt — which is the whole reason `run_semantic_check`
    // actually calls the LLM here.
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://semanticCheck".to_string()),
                target: lit(semantic_check_hint),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.semanticCheck)");

    let base_uri = "ad4m://task/onboarding-10.5b";
    let inst_uri = mint_flow_instance(
        &mut perspective,
        "Delivery",
        base_uri,
        "identified",
        "e2e-10.5b-inst",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    // Seed the Task that satisfies `requires` — same
    // `create_subject` path the interpretation pipeline uses. The
    // deterministic guard is now met; the SEMANTIC-CHECK gate is the
    // only thing between the transition and the on-graph proposal.
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/1",
        "Onboard Ana",
    )
    .await;

    (perspective, ctx, inst_uri)
}

/// Semantic-check `Pass` (LLM returns "YES") ⇒ proposal fires,
/// exactly one LLM call, prompt was threaded through with the
/// correct model_id.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_pass_fires_proposal_e2e() {
    let (mut perspective, ctx, _inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    let llm = CannedLlm::responding("YES");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &[],
    )
    .await;
    assert_eq!(minted.len(), 1, "Pass verdict ⇒ 1 proposal, got {minted:?}",);
    assert_eq!(
        llm.call_count(),
        1,
        "gated transition ⇒ exactly one semantic-check LLM call, got {}",
        llm.call_count(),
    );
    let (model_id, prompt) = llm.calls.lock().unwrap()[0].clone();
    assert_eq!(model_id, "test-model-42", "model_id must thread through");
    assert!(
        prompt.contains("scoped") || prompt.to_lowercase().contains("scope"),
        "prompt must mention the target state / hint, got: {prompt}",
    );
}

/// Semantic-check `Fail` (LLM returns "NO") ⇒ transition is
/// discarded despite `requires` being satisfied. This is the
/// load-bearing property: an uncertain LLM cannot silently advance
/// a flow even when the deterministic guard says it could.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_fail_discards_transition_e2e() {
    let (mut perspective, ctx, _inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    let llm = CannedLlm::responding("NO");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &[],
    )
    .await;
    assert!(
        minted.is_empty(),
        "Fail verdict ⇒ 0 proposals despite requires-satisfied, got {minted:?}",
    );
    assert_eq!(
        llm.call_count(),
        1,
        "gate must still make the LLM call before deciding, got {}",
        llm.call_count(),
    );
}

/// Semantic-check LLM error ⇒ transition is discarded (fail-safe:
/// the flow layer must never break the extraction pass, and an
/// erroring LLM is treated the same as `Fail`).
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_llm_error_discards_transition_e2e() {
    let (mut perspective, ctx, _inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    let llm = CannedLlm::erroring("simulated LLM outage");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &[],
    )
    .await;
    assert!(
        minted.is_empty(),
        "LLM error ⇒ 0 proposals (fail-safe), got {minted:?}",
    );
    assert_eq!(
        llm.call_count(),
        1,
        "gate must attempt the LLM call before deciding, got {}",
        llm.call_count(),
    );
}

/// A transition WITHOUT a `semantic_check` hint ⇒ the LLM is not
/// called even when the gate is enabled (auto-pass short-circuit),
/// and the proposal still fires. Locks the "hint absent = no LLM
/// spend" invariant that keeps the gate cheap on flows without
/// explicit semantic checks.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_absent_hint_autopasses_no_llm_call_e2e() {
    let (mut perspective, shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    // Same shape as the 10.4c fixture — Delivery + requires on
    // scoped, NO semantic_check link on any state.
    let scoped_uri = "delivery://Delivery.scoped";
    for link in parse_flow_to_links(&delivery_flow_json(), "Delivery")
        .expect("parse_flow_to_links(Delivery)")
    {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition v4)");
    }
    let requires_json = r#"[{"className":"ns://Task","count":{"min":1}}]"#;
    perspective
        .add_link(
            Link {
                source: scoped_uri.to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: lit(requires_json),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link(scoped.requires)");
    let _ = mint_flow_instance(
        &mut perspective,
        "Delivery",
        "ad4m://task/no-hint",
        "identified",
        "e2e-10.5b-no-hint-inst",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");
    seed_instance(
        &mut perspective,
        &ctx,
        &shapes[0],
        "ad4m://task/nohint-1",
        "Onboard Ana",
    )
    .await;

    // An "erroring" LLM: if the gate ever calls it, the whole
    // transition would be discarded (per the LLM-error test above).
    // Since the hint is absent, `build_semantic_check_prompt`
    // returns None ⇒ `run_semantic_check` short-circuits to `Pass`
    // WITHOUT calling `.confirm()`. The proposal must still fire
    // and the LLM must record zero calls.
    let llm = CannedLlm::erroring("gate must not call me — no hint on this transition");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &[],
    )
    .await;
    assert_eq!(minted.len(), 1, "auto-pass ⇒ 1 proposal, got {minted:?}",);
    assert_eq!(
        llm.call_count(),
        0,
        "auto-pass short-circuit ⇒ zero LLM calls, got {}",
        llm.call_count(),
    );
}

// ---------------------------------------------------------------------
// LlmProposalHint matching / rationale attribution
// ---------------------------------------------------------------------

/// Helper: read the `ad4m://flow/rationale` link off a proposal URI and
/// return the decoded scalar value (or `None` if the property is absent).
/// The writer stores scalars as `literal:string:...` targets, so decode
/// by stripping the prefix. Kept local — the assertion shape (was the
/// rationale predicate set at all + does its value round-trip) is
/// specific enough that inlining across three tests would obscure it.
async fn read_rationale(
    perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
    proposal_uri: &str,
) -> Option<String> {
    let links = perspective
        .get_links(&crate::types::LinkQuery {
            source: Some(proposal_uri.to_string()),
            predicate: Some("ad4m://flow/rationale".to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links(proposal.rationale)");
    let target = links.into_iter().next()?.data.target;
    // `literal:string:<url-encoded>` — decode the same way callers of
    // `resolve_property_value` would; a minimal peel here is sufficient
    // because the write path uses a plain ASCII rationale in these tests.
    target.strip_prefix("literal:string:").map(|s| {
        // Cheap decode: real reader uses `urlencoding::decode`, but the
        // test strings are ASCII with no reserved chars, so a percent-
        // free string is byte-identical after decode. Fall back to the
        // full decoder if a reserved char shows up.
        urlencoding::decode(s)
            .map(|c| c.into_owned())
            .unwrap_or_else(|_| s.to_string())
    })
}

/// LLM hint matches a satisfied transition ⇒ the proposal fires with
/// the LLM's `reason` written as the on-graph `rationale`. This is the
/// load-bearing "LLM attribution rides through" property.
#[tokio::test(flavor = "multi_thread")]
async fn llm_hint_matches_transition_writes_rationale_e2e() {
    let (mut perspective, ctx, inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    let hints = vec![LlmProposalHint {
        instance_uri: inst_uri.clone(),
        to_state: "scoped".to_string(),
        reason: Some("LLM saw one Task and moved the flow forward".to_string()),
    }];
    let llm = CannedLlm::responding("YES");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &hints,
    )
    .await;
    assert_eq!(
        minted.len(),
        1,
        "matched hint + Pass verdict ⇒ 1 proposal, got {minted:?}",
    );
    let rationale = read_rationale(&perspective, &minted[0])
        .await
        .expect("matched hint MUST write a rationale link on the proposal");
    assert_eq!(
        rationale, "LLM saw one Task and moved the flow forward",
        "written rationale must round-trip the LLM's reason verbatim",
    );
}

/// LLM hint for a transition NOT in the satisfied set ⇒ hint is
/// silently dropped; the engine's own satisfied-transition proposal
/// still fires without a rationale. Documents the invariant that the
/// LLM cannot bypass the deterministic `requires` guard.
#[tokio::test(flavor = "multi_thread")]
async fn llm_hint_without_matching_transition_is_discarded_e2e() {
    let (mut perspective, ctx, inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    // Two hints: one names a state the flow never proposes on this
    // pass (`does-not-exist`); the other names the correct state on
    // an unknown instance URI. Both must be discarded.
    let hints = vec![
        LlmProposalHint {
            instance_uri: inst_uri.clone(),
            to_state: "does-not-exist".to_string(),
            reason: Some("LLM guessed a state".to_string()),
        },
        LlmProposalHint {
            instance_uri: "ad4m://flow/instance/never-minted".to_string(),
            to_state: "scoped".to_string(),
            reason: Some("LLM invented an instance URI".to_string()),
        },
    ];
    let llm = CannedLlm::responding("YES");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &hints,
    )
    .await;
    assert_eq!(
        minted.len(),
        1,
        "unmatched hints must not spawn extra proposals — engine still fires the satisfied one",
    );
    assert!(
        read_rationale(&perspective, &minted[0]).await.is_none(),
        "engine-emitted proposal (no matching hint) must NOT carry a rationale",
    );
}

/// LLM hint matches but `reason=None` (or empty string) ⇒ the write
/// path drops the rationale entirely rather than persisting an empty
/// scalar. Byte-identical to the pre-10.6c engine-only path for the
/// on-graph proposal shape.
#[tokio::test(flavor = "multi_thread")]
async fn llm_hint_with_no_reason_writes_no_rationale_e2e() {
    let (mut perspective, ctx, inst_uri) =
        seed_semantic_check_e2e_fixture("The scope is well-defined and actionable.").await;

    let hints = vec![LlmProposalHint {
        instance_uri: inst_uri.clone(),
        to_state: "scoped".to_string(),
        reason: None,
    }];
    let llm = CannedLlm::responding("YES");
    let minted = run_engine_proposal_pass(
        &mut perspective,
        None,
        &ctx,
        Some((&llm, "test-model-42")),
        &hints,
    )
    .await;
    assert_eq!(minted.len(), 1, "matched hint + Pass ⇒ 1 proposal");
    assert!(
        read_rationale(&perspective, &minted[0]).await.is_none(),
        "reason=None must NOT write a rationale — the SDNA allows omission and \
         writing an empty scalar bloats the graph with a link carrying no signal",
    );
}
