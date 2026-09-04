//! Live-perspective coverage for `flow_evaluator`: real store, real SDNA,
//! real `model_query`, no LLM. The unit tests in `flow_evaluator` stub the
//! query layer; these prove the translated `requires` query is a shape
//! `model_query` accepts, that a proposal lands with every declared
//! predicate, and that the semantic-check gate and LLM-proposal rationale
//! behave end to end.

use super::flow_classes::{mint_flow_instance, write_flow_transition_proposal};
use super::flow_context::{load_flow_instances, load_shacl_flows};
use super::flow_evaluator::{
    evaluate_flow_transitions, evidence_hash, run_engine_proposal_pass, SatisfiedTransition,
};
use super::flow_semantic_check::SemanticCheckLlm;
use super::interpretation::LlmFlowProposal;
use super::interpretation_test_support::{seed_instance, setup_perspective_no_llm, TASK_SDNA};
use super::model_query::types::ModelShape;
use super::perspective_instance::PerspectiveInstance;
use super::shacl_parser::parse_flow_to_links;
use crate::agent::AgentContext;
use crate::types::{Link, LinkQuery, LinkStatus};
use std::collections::HashMap;
use std::sync::Mutex;

const FLOW_URI: &str = "delivery://DeliveryFlow";
const BASE_URI: &str = "ad4m://task/onboarding";
const SCOPE_HINT: &str = "The scope is well-defined and actionable.";

struct Fixture {
    perspective: PerspectiveInstance,
    task_shape: ModelShape,
    ctx: AgentContext,
    instance_uri: String,
}

fn literal(s: &str) -> String {
    format!("literal:string:{}", urlencoding::encode(s))
}

/// Two-state Delivery flow (`identified → scoped`) whose `scoped` state
/// requires at least one `ns://Task` and optionally carries a
/// `semanticCheck` hint, plus one FlowInstance sitting in `identified`. The
/// `requires` and `semanticCheck` links are added by hand because
/// `parse_flow_to_links` does not emit them yet.
async fn seed_fixture(semantic_check: Option<&str>) -> Fixture {
    seed_fixture_with_requires(
        r#"[{"className":"ns://Task","count":{"min":1}}]"#,
        semantic_check,
    )
    .await
}

async fn seed_fixture_with_requires(requires_json: &str, semantic_check: Option<&str>) -> Fixture {
    let (mut perspective, mut shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    let flow_json = serde_json::json!({
        "name": "Delivery",
        "namespace": "delivery://",
        "states": [
            { "name": "identified", "value": 0.0 },
            { "name": "scoped", "value": 0.5 }
        ],
        "transitions": [
            { "action_name": "Scope", "from_state": "identified", "to_state": "scoped", "actions": [] }
        ],
    })
    .to_string();
    let mut links = parse_flow_to_links(&flow_json, "Delivery").expect("parse_flow_to_links");
    let scoped = "delivery://Delivery.scoped";
    links.push(Link {
        source: scoped.to_string(),
        predicate: Some("ad4m://requires".to_string()),
        target: literal(requires_json),
    });
    if let Some(hint) = semantic_check {
        links.push(Link {
            source: scoped.to_string(),
            predicate: Some("ad4m://semanticCheck".to_string()),
            target: literal(hint),
        });
    }
    for link in links {
        perspective
            .add_link(link, LinkStatus::Local, None, &ctx)
            .await
            .expect("add_link(flow definition)");
    }

    let instance_uri = mint_flow_instance(
        &mut perspective,
        FLOW_URI,
        BASE_URI,
        "identified",
        "e2e-inst",
        None,
        &ctx,
    )
    .await
    .expect("mint_flow_instance");

    Fixture {
        perspective,
        task_shape: shapes.remove(0),
        ctx,
        instance_uri,
    }
}

/// Fixture with the guard already satisfied by one Task.
async fn seed_satisfied_fixture(semantic_check: Option<&str>) -> Fixture {
    let mut f = seed_fixture(semantic_check).await;
    f.seed_task("ad4m://task/1", "Onboard Ana").await;
    f
}

impl Fixture {
    async fn seed_task(&mut self, uri: &str, title: &str) {
        seed_instance(
            &mut self.perspective,
            &self.ctx,
            &self.task_shape,
            uri,
            title,
        )
        .await;
    }

    async fn satisfied(&self) -> Vec<SatisfiedTransition> {
        let records = load_flow_instances(&self.perspective, &[BASE_URI.to_string()])
            .await
            .expect("load_flow_instances");
        let flows = load_shacl_flows(&self.perspective)
            .await
            .expect("load_shacl_flows");
        evaluate_flow_transitions(&self.perspective, &records, &flows, "did:key:acting").await
    }

    async fn run_pass(
        &mut self,
        llm_proposals: &[LlmFlowProposal],
        semantic_check: Option<&dyn SemanticCheckLlm>,
    ) -> Vec<String> {
        run_engine_proposal_pass(
            &mut self.perspective,
            None,
            &self.ctx,
            llm_proposals,
            semantic_check,
            None,
        )
        .await
    }

    async fn links_by_predicate(&self, source: &str) -> HashMap<String, Vec<String>> {
        let links = self
            .perspective
            .get_links(&LinkQuery {
                source: Some(source.to_string()),
                ..Default::default()
            })
            .await
            .expect("get_links");
        let mut by_pred: HashMap<String, Vec<String>> = HashMap::new();
        for l in links {
            if let Some(pred) = l.data.predicate {
                by_pred.entry(pred).or_default().push(l.data.target);
            }
        }
        by_pred
    }

    /// The proposal's `rationale` scalar, decoded, if one was written.
    async fn rationale(&self, proposal_uri: &str) -> Option<String> {
        let by_pred = self.links_by_predicate(proposal_uri).await;
        let target = by_pred.get("ad4m://flow/rationale")?.first()?;
        let encoded = target.strip_prefix("literal:string:")?;
        Some(urlencoding::decode(encoded).ok()?.into_owned())
    }
}

fn assert_has_target(by_pred: &HashMap<String, Vec<String>>, pred: &str, want: &str) {
    let targets = by_pred
        .get(pred)
        .unwrap_or_else(|| panic!("proposal must carry a `{pred}` link"));
    assert!(
        targets.iter().any(|t| t == want),
        "`{pred}` must carry `{want}`, got {targets:?}"
    );
}

/// Semantic-check LLM with a canned answer (or error); records its prompts.
struct CannedLlm {
    answer: Result<String, String>,
    prompts: Mutex<Vec<String>>,
}

impl CannedLlm {
    fn new(answer: Result<&str, &str>) -> Self {
        Self {
            answer: answer.map(str::to_string).map_err(str::to_string),
            prompts: Mutex::new(Vec::new()),
        }
    }
    fn calls(&self) -> usize {
        self.prompts.lock().unwrap().len()
    }
}

#[async_trait::async_trait]
impl SemanticCheckLlm for CannedLlm {
    async fn confirm(&self, prompt: &str) -> anyhow::Result<String> {
        self.prompts.lock().unwrap().push(prompt.to_string());
        self.answer.clone().map_err(|m| anyhow::anyhow!(m))
    }
}

fn proposal(instance: &str, to_state: &str, reason: Option<&str>) -> LlmFlowProposal {
    LlmFlowProposal {
        instance: instance.to_string(),
        to_state: to_state.to_string(),
        reason: reason.map(str::to_string),
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn evaluate_flow_transitions_e2e() {
    let mut f = seed_fixture(Some(SCOPE_HINT)).await;

    let flows = load_shacl_flows(&f.perspective).await.unwrap();
    let scoped = flows[FLOW_URI]
        .states
        .iter()
        .find(|s| s.name == "scoped")
        .expect("scoped state parsed");
    assert_eq!(
        scoped.requires.as_ref().map(|r| r[0].class_name.as_str()),
        Some("ns://Task"),
        "hand-seeded requires link must round-trip through the flow reader"
    );

    assert!(f.satisfied().await.is_empty(), "no Task yet → guard unmet");

    f.seed_task("ad4m://task/1", "Onboard Ana").await;
    let after = f.satisfied().await;
    assert_eq!(after.len(), 1, "got {after:?}");
    let t = &after[0];
    assert_eq!(t.flow_name, "Delivery");
    assert_eq!(t.instance_uri, f.instance_uri);
    assert_eq!(
        (t.from_state.as_str(), t.to_state.as_str()),
        ("identified", "scoped")
    );
    assert_eq!(t.evidence_ids, vec!["ad4m://task/1".to_string()]);
    assert_eq!(
        t.evidence_hash,
        evidence_hash(&["ns://Task".to_string()], &t.evidence)
    );
    assert_eq!(t.semantic_check.as_deref(), Some(SCOPE_HINT));
}

/// Two Tasks so the `evidence` collection has more than one element: the
/// writer passes the whole collection as a JSON array and `create_subject`
/// expands it into one `addLink` per element — both must land.
#[tokio::test(flavor = "multi_thread")]
async fn write_flow_transition_proposal_lands_all_predicates_e2e() {
    let mut f = seed_satisfied_fixture(None).await;
    f.seed_task("ad4m://task/2", "Onboard Bo").await;
    let t = f.satisfied().await.remove(0);
    assert_eq!(t.evidence_ids.len(), 2);

    let proposal_uri = write_flow_transition_proposal(
        &mut f.perspective,
        "e2e-prop-1",
        "did:key:acting",
        &t.instance_uri,
        &t.from_state,
        &t.to_state,
        &t.evidence_ids,
        &t.evidence_hash,
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write_flow_transition_proposal");
    assert_eq!(proposal_uri, "ad4m://flow/proposal/e2e-prop-1");

    let by_pred = f.links_by_predicate(&proposal_uri).await;
    // IRIs and DIDs are stored raw; plain strings are literal-wrapped.
    assert_has_target(&by_pred, "ad4m://flow/instance", &f.instance_uri);
    assert_has_target(&by_pred, "ad4m://flow/proposer", "did:key:acting");
    assert_has_target(&by_pred, "ad4m://flow/evidence", "ad4m://task/1");
    assert_has_target(&by_pred, "ad4m://flow/evidence", "ad4m://task/2");
    assert_has_target(&by_pred, "ad4m://flow/from_state", &literal("identified"));
    assert_has_target(&by_pred, "ad4m://flow/to_state", &literal("scoped"));
    assert_has_target(
        &by_pred,
        "ad4m://flow/evidence_hashes",
        &literal(&t.evidence_hash),
    );
    assert!(
        !by_pred.contains_key("ad4m://flow/rationale"),
        "no rationale was given"
    );
}

/// Design §6 (firing engine): a proposal synced from ANOTHER agent must not
/// suppress this agent's own mint — the consensus counter counts distinct
/// DIDs, so each needs its own countable row. Locks the proposer dimension
/// of the dedup key against the live store (raw-DID link shape included).
#[tokio::test(flavor = "multi_thread")]
async fn foreign_proposal_does_not_block_own_mint_e2e() {
    let mut f = seed_satisfied_fixture(None).await;

    // Simulate agent A's replica-synced proposal for the same satisfied
    // transition: same instance, same to_state, same evidence hash the
    // engine will compute for our own pass.
    let satisfied = f.satisfied().await;
    assert_eq!(
        satisfied.len(),
        1,
        "fixture must satisfy exactly one transition"
    );
    write_flow_transition_proposal(
        &mut f.perspective,
        "foreign-1",
        "did:key:agent-a",
        &f.instance_uri,
        &satisfied[0].from_state,
        &satisfied[0].to_state,
        &satisfied[0].evidence_ids,
        &satisfied[0].evidence_hash,
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write foreign proposal");

    // Our own pass still mints — quorum can grow past 1.
    let minted = f.run_pass(&[], None).await;
    assert_eq!(
        minted.len(),
        1,
        "own mint must not be suppressed by agent-a's row: {minted:?}"
    );

    // And a re-run of our own pass mints nothing (same-DID idempotency).
    let rerun = f.run_pass(&[], None).await;
    assert!(rerun.is_empty(), "own re-run re-proposed: {rerun:?}");
}

#[tokio::test(flavor = "multi_thread")]
async fn run_engine_proposal_pass_e2e() {
    let mut f = seed_fixture(None).await;

    let before = f.run_pass(&[], None).await;
    assert!(
        before.is_empty(),
        "guard unmet → no proposal, got {before:?}"
    );

    f.seed_task("ad4m://task/1", "Onboard Ana").await;
    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");
    assert!(minted[0].starts_with("ad4m://flow/proposal/"));

    let acting_did = crate::agent::did_for_context(&f.ctx).expect("did_for_context");
    let by_pred = f.links_by_predicate(&minted[0]).await;
    assert_has_target(&by_pred, "ad4m://flow/proposer", &acting_did);
    assert_has_target(&by_pred, "ad4m://flow/instance", &f.instance_uri);
    assert_has_target(&by_pred, "ad4m://flow/from_state", &literal("identified"));
    assert_has_target(&by_pred, "ad4m://flow/to_state", &literal("scoped"));

    // Idempotency: minting does not advance currentState, so a re-run sees
    // the same satisfied transition — it must skip, not mint a duplicate.
    let rerun = f.run_pass(&[], None).await;
    assert!(
        rerun.is_empty(),
        "unchanged evidence re-proposed: {rerun:?}"
    );
}

/// With a `semanticCheck` hint on the target state the gate consults the
/// LLM exactly once and only a YES lets the proposal through. NO, UNCLEAR
/// and an LLM error all discard it.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_gate_fires_only_on_yes_e2e() {
    for (answer, expected) in [
        (Ok("YES"), 1),
        (Ok("NO"), 0),
        (Ok("UNCLEAR"), 0),
        (Err("simulated LLM outage"), 0),
    ] {
        let mut f = seed_satisfied_fixture(Some(SCOPE_HINT)).await;
        let llm = CannedLlm::new(answer);
        let minted = f.run_pass(&[], Some(&llm)).await;
        assert_eq!(minted.len(), expected, "answer {answer:?} → {minted:?}");
        assert_eq!(llm.calls(), 1, "answer {answer:?}: exactly one LLM call");
        let prompt = &llm.prompts.lock().unwrap()[0];
        assert!(prompt.contains(SCOPE_HINT) && prompt.contains("TO:   scoped"));
        // The central claim of the gate: the LLM sees the evidence's
        // CONTENT (the seeded Task's title, hydrated through the real
        // model_query path), not just its URI — a hint like SCOPE_HINT is
        // only decidable from property values.
        assert!(
            prompt.contains("Onboard Ana"),
            "semantic-check prompt must carry hydrated evidence content:\n{prompt}"
        );
        assert!(prompt.contains("ad4m://task/1"));
    }
}

/// A `semanticCheck` whose transition carries no hydrated evidence (the
/// guard here is a negative `max: 0` — satisfied by the ABSENCE of matches)
/// discards fail-closed without consulting the LLM: there is no content a
/// yes/no could rest on, so asking would be a rubber stamp.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_without_evidence_discards_without_llm_call_e2e() {
    // Negative guard, no tasks seeded: satisfied with ZERO evidence.
    let mut f = seed_fixture_with_requires(
        r#"[{"className":"ns://Task","count":{"max":0}}]"#,
        Some(SCOPE_HINT),
    )
    .await;
    // The guard itself is satisfied (sanity check for this test's premise)…
    let satisfied = f.satisfied().await;
    assert_eq!(satisfied.len(), 1, "negative guard should be satisfied");
    assert!(satisfied[0].evidence.is_empty());
    // …but the semantic check has nothing to evaluate, so the pass
    // discards without asking the LLM.
    let llm = CannedLlm::new(Ok("YES"));
    let minted = f.run_pass(&[], Some(&llm)).await;
    assert_eq!(llm.calls(), 0, "no evidence → no LLM call");
    assert!(
        minted.is_empty(),
        "no evidence + semanticCheck must fail closed: {minted:?}"
    );
}

/// No hint on the target state → the gate is enabled but never calls the
/// LLM, and the proposal still fires.
#[tokio::test(flavor = "multi_thread")]
async fn semantic_check_without_hint_never_calls_the_llm_e2e() {
    let mut f = seed_satisfied_fixture(None).await;
    let llm = CannedLlm::new(Err("must not be called"));
    let minted = f.run_pass(&[], Some(&llm)).await;
    assert_eq!(minted.len(), 1);
    assert_eq!(llm.calls(), 0);
}

/// An LLM proposal naming the satisfied transition writes its `reason` as
/// the proposal's `rationale`. Proposals for other transitions or unknown
/// instances change nothing, and an empty reason writes no rationale.
#[tokio::test(flavor = "multi_thread")]
async fn llm_proposal_contributes_rationale_only_when_it_matches_e2e() {
    let mut f = seed_satisfied_fixture(None).await;
    let matched = [proposal(
        &f.instance_uri,
        "scoped",
        Some("LLM saw one Task"),
    )];
    let minted = f.run_pass(&matched, None).await;
    assert_eq!(minted.len(), 1);
    assert_eq!(
        f.rationale(&minted[0]).await.as_deref(),
        Some("LLM saw one Task")
    );

    let mut f = seed_satisfied_fixture(None).await;
    let unmatched = [
        proposal(&f.instance_uri, "does-not-exist", Some("guessed a state")),
        proposal(
            "ad4m://flow/instance/never-minted",
            "scoped",
            Some("invented an instance"),
        ),
    ];
    let minted = f.run_pass(&unmatched, None).await;
    assert_eq!(
        minted.len(),
        1,
        "the engine's own proposal still fires, nothing extra"
    );
    assert_eq!(f.rationale(&minted[0]).await, None);

    let mut f = seed_satisfied_fixture(None).await;
    let blank = [proposal(&f.instance_uri, "scoped", Some("   "))];
    let minted = f.run_pass(&blank, None).await;
    assert_eq!(minted.len(), 1);
    assert_eq!(f.rationale(&minted[0]).await, None);
}

/// Full harness path: a scripted LLM calls `Delivery_propose_transition`,
/// the decorator validates and buffers it, and the engine pass writes the
/// LLM's reason as the on-graph rationale.
#[tokio::test(flavor = "multi_thread")]
async fn harness_propose_transition_tool_call_routes_rationale_to_graph_e2e() {
    use crate::ai_service::harness::flow_propose::{
        propose_transition_tool_name, FlowProposalBuffer, FlowTransitionProposeProvider,
    };
    use crate::ai_service::harness::provider::{ToolProvider, ToolSchema};
    use crate::ai_service::harness::{
        run_with_tools, CompletionSource, HarnessCompletion, HarnessConfig, HarnessToolCall,
    };
    use crate::perspectives::flow_context::gather_active_flow_contexts;
    use serde_json::{json, Value};
    use std::sync::Arc;

    /// Returns the queued completions in order; records the tool names
    /// advertised on each call.
    struct ScriptedLlm {
        script: Mutex<Vec<HarnessCompletion>>,
        advertised: Mutex<Vec<Vec<String>>>,
    }

    #[async_trait::async_trait]
    impl CompletionSource for ScriptedLlm {
        async fn complete(
            &self,
            _model_id: &str,
            _messages: &[Value],
            tools: Vec<ToolSchema>,
        ) -> anyhow::Result<HarnessCompletion> {
            self.advertised
                .lock()
                .unwrap()
                .push(tools.iter().map(|t| t.name.clone()).collect());
            Ok(self.script.lock().unwrap().remove(0))
        }
    }

    /// Inner provider with no tools; only the flow decorator is exercised.
    struct EmptyInner;

    #[async_trait::async_trait]
    impl ToolProvider for EmptyInner {
        async fn tools(&self) -> Vec<ToolSchema> {
            Vec::new()
        }
        async fn call(&self, name: &str, _args: Value) -> anyhow::Result<String> {
            Err(anyhow::anyhow!(
                "EmptyInner advertises no tools; got `{name}`"
            ))
        }
    }

    let mut f = seed_satisfied_fixture(None).await;
    let active_flows =
        gather_active_flow_contexts(&f.perspective, &[BASE_URI.to_string()], None).await;
    assert_eq!(active_flows.len(), 1, "got {active_flows:?}");
    assert_eq!(active_flows[0].instance_uri, f.instance_uri);

    let buffer = FlowProposalBuffer::new();
    let provider: Arc<dyn ToolProvider> = Arc::new(FlowTransitionProposeProvider::new(
        Arc::new(EmptyInner),
        active_flows,
        buffer.clone(),
    ));

    let tool_name = propose_transition_tool_name("Delivery");
    let reason = "Task `ad4m://task/1` (Onboard Ana) has been scoped; advancing to `scoped`.";
    let llm = Arc::new(ScriptedLlm {
        script: Mutex::new(vec![
            HarnessCompletion {
                content: String::new(),
                tool_calls: vec![HarnessToolCall {
                    id: "call-1".to_string(),
                    name: tool_name.clone(),
                    arguments: json!({
                        "instance": f.instance_uri,
                        "toState": "scoped",
                        "reason": reason,
                    }),
                }],
            },
            HarnessCompletion {
                content: "done".to_string(),
                tool_calls: Vec::new(),
            },
        ]),
        advertised: Mutex::new(Vec::new()),
    });

    run_with_tools(
        "test-model",
        vec![json!({ "role": "user", "content": "extract" })],
        provider,
        llm.clone(),
        HarnessConfig::default(),
        None,
        None,
    )
    .await
    .expect("run_with_tools terminates on the plain answer");

    assert!(
        llm.advertised.lock().unwrap()[0].contains(&tool_name),
        "first round must advertise the flow's propose tool"
    );
    let proposals = buffer.drain();
    assert_eq!(
        proposals,
        vec![proposal(&f.instance_uri, "scoped", Some(reason))],
        "the tool call round-trips through the decorator verbatim"
    );

    let minted = f.run_pass(&proposals, None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");
    assert_eq!(f.rationale(&minted[0]).await.as_deref(), Some(reason));
}

// ---------------------------------------------------------------------------
// run_flow_consensus_pass — the firing half, against the live store.
// ---------------------------------------------------------------------------

/// Happy path with the default `{ n: 1 }` rule: the pass verifies the
/// minted proposal's evidence against the live graph, fires the
/// transition, keep-and-marks the proposal, and a re-run is a no-op.
#[tokio::test(flavor = "multi_thread")]
async fn consensus_pass_fires_marks_and_is_idempotent_e2e() {
    use super::flow_consensus::{load_flow_transition_proposals, run_flow_consensus_pass};

    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");

    let outcomes = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert_eq!(outcomes.len(), 1, "n=1 default rule fires immediately");
    assert_eq!(
        (
            outcomes[0].from_state.as_str(),
            outcomes[0].to_state.as_str()
        ),
        ("identified", "scoped")
    );
    assert_eq!(outcomes[0].contributing_proposal_uris, minted);

    // `currentState` advanced on-graph.
    let recs = load_flow_instances(&f.perspective, &[BASE_URI.to_string()])
        .await
        .expect("load instances");
    assert_eq!(recs.len(), 1);
    assert_eq!(recs[0].current_state, "scoped");

    // The fired proposal is KEPT (its links survive, marked `fired`) but no
    // longer loads as live — the Synergy flow-atom record.
    let live = load_flow_transition_proposals(&f.perspective, &f.instance_uri)
        .await
        .expect("load proposals");
    assert!(live.is_empty(), "fired proposal must be resolved: {live:?}");
    let by_pred = f.links_by_predicate(&minted[0]).await;
    assert_has_target(&by_pred, "ad4m://flow/resolved_as", &literal("fired"));
    assert_has_target(&by_pred, "ad4m://flow/instance", &f.instance_uri);

    // Idempotency: nothing live → nothing fires, state stays.
    let rerun = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert!(rerun.is_empty(), "re-run fired again: {rerun:?}");
}

/// Both auto-invalidation triggers, against the live store: a proposal
/// sealed with a hash the current graph cannot reproduce is deleted
/// (trigger b), a proposal whose `fromState` the instance already left is
/// deleted (trigger a), and neither fires the transition.
#[tokio::test(flavor = "multi_thread")]
async fn consensus_pass_invalidates_stale_seal_and_superseded_e2e() {
    use super::flow_consensus::run_flow_consensus_pass;

    let mut f = seed_satisfied_fixture(None).await;
    let acting_did = crate::agent::did_for_context(&f.ctx).expect("did_for_context");

    let stale_seal = write_flow_transition_proposal(
        &mut f.perspective,
        "stale-seal",
        &acting_did,
        &f.instance_uri.clone(),
        "identified",
        "scoped",
        &["ad4m://task/1".to_string()],
        "deadbeef-not-the-live-hash",
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write stale-seal proposal");

    let superseded = write_flow_transition_proposal(
        &mut f.perspective,
        "superseded",
        &acting_did,
        &f.instance_uri.clone(),
        "scoped",
        "identified",
        &[],
        "",
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write superseded proposal");

    // And one on the DECLARED edge with an EMPTY seal: unverifiable, and
    // with the guard satisfied it would fire n=1 if it were counted — the
    // sharpest form of the CWE-345 hole. It must be invalidated instead.
    let empty_seal = write_flow_transition_proposal(
        &mut f.perspective,
        "empty-seal",
        &acting_did,
        &f.instance_uri.clone(),
        "identified",
        "scoped",
        &[],
        "",
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write empty-seal proposal");

    let outcomes = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert!(outcomes.is_empty(), "nothing may fire: {outcomes:?}");

    // Hard-deleted, not marked: every source-link gone.
    assert!(
        f.links_by_predicate(&stale_seal).await.is_empty(),
        "stale-sealed proposal must be deleted"
    );
    assert!(
        f.links_by_predicate(&superseded).await.is_empty(),
        "superseded proposal must be deleted"
    );
    assert!(
        f.links_by_predicate(&empty_seal).await.is_empty(),
        "empty-seal proposal must be deleted"
    );

    // The instance did not move.
    let recs = load_flow_instances(&f.perspective, &[BASE_URI.to_string()])
        .await
        .expect("load instances");
    assert_eq!(recs[0].current_state, "identified");
}

/// Accept API + a real `{ n: 2 }` per-state rule: one DID holds, a second
/// DID's acceptance fires, and the immediate pass inside `accept` delivers
/// the outcome without waiting for the next transcript.
#[tokio::test(flavor = "multi_thread")]
async fn accept_api_n2_quorum_fires_e2e() {
    use super::flow_consensus::{
        accept_flow_proposal, run_flow_consensus_pass, ACCEPTED_BY_PREDICATE,
    };

    let mut f = seed_satisfied_fixture(None).await;
    f.perspective
        .add_link(
            Link {
                source: "delivery://Delivery.scoped".to_string(),
                predicate: Some("ad4m://consensusRule".to_string()),
                target: literal(r#"{"n":2}"#),
            },
            LinkStatus::Local,
            None,
            &f.ctx,
        )
        .await
        .expect("add consensusRule link");

    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");

    // One distinct DID < n=2: nothing fires.
    let outcomes = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert!(outcomes.is_empty(), "1 < n=2 must hold: {outcomes:?}");

    // Self-accept (proposer's own DID) is idempotent across calls and adds
    // no second distinct DID — still no fire.
    let acting_did = crate::agent::did_for_context(&f.ctx).expect("did_for_context");
    for _ in 0..2 {
        let fired = accept_flow_proposal(&mut f.perspective, &minted[0], &f.ctx)
            .await
            .expect("self-accept");
        assert!(
            fired.is_empty(),
            "self-accept must not reach n=2: {fired:?}"
        );
    }
    let by_pred = f.links_by_predicate(&minted[0]).await;
    assert_eq!(
        by_pred.get(ACCEPTED_BY_PREDICATE).map(Vec::len),
        Some(1),
        "repeat accepts by one DID must write exactly one acceptedBy link"
    );

    // A FORGED second acceptance — authored by *us*, naming a DID we do not
    // control — must not count toward quorum (votes are authorship claims,
    // not data claims: `l.author == l.data.target` is the identity binding).
    f.perspective
        .add_link(
            Link {
                source: minted[0].clone(),
                predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                target: "did:key:forged-victim".to_string(),
            },
            LinkStatus::Shared,
            None,
            &f.ctx,
        )
        .await
        .expect("forged acceptedBy link");
    let fired = accept_flow_proposal(&mut f.perspective, &minted[0], &f.ctx)
        .await
        .expect("accept with forged vote present");
    assert!(
        fired.is_empty(),
        "a forged acceptedBy (author != target) must not reach n=2: {fired:?}"
    );

    // A GENUINE second-agent acceptance arrives via sync: the link carries
    // the foreign author itself, exactly as `diff_from_link_language` would
    // deliver it. This one counts, quorum is met, the edge fires.
    f.perspective
        .add_link_expression(
            crate::types::LinkExpression {
                author: "did:key:other-agent".to_string(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                data: Link {
                    source: minted[0].clone(),
                    predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
                    target: "did:key:other-agent".to_string(),
                },
                proof: crate::types::ExpressionProof {
                    key: "did:key:other-agent#key".to_string(),
                    signature: "test-signature".to_string(),
                },
                status: Some(LinkStatus::Shared),
            },
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("second-agent acceptedBy link");
    let fired = accept_flow_proposal(&mut f.perspective, &minted[0], &f.ctx)
        .await
        .expect("accept with quorum");
    assert_eq!(fired.len(), 1, "n=2 met must fire: {fired:?}");
    assert_eq!(fired[0].fired_by_proposers.len(), 2);
    assert!(fired[0]
        .fired_by_proposers
        .contains(&"did:key:other-agent".to_string()));
    assert!(fired[0].fired_by_proposers.contains(&acting_did));

    // Resolved proposals are immutable to the API.
    assert!(
        accept_flow_proposal(&mut f.perspective, &minted[0], &f.ctx)
            .await
            .is_err(),
        "accept on a fired proposal must error"
    );
}

/// Reject hard-deletes a live proposal, errors on a second attempt (nothing
/// left to reject), and never moves the instance.
#[tokio::test(flavor = "multi_thread")]
async fn reject_deletes_live_proposal_and_errors_on_missing_e2e() {
    use super::flow_consensus::reject_flow_proposal;

    let mut f = seed_satisfied_fixture(None).await;
    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");

    reject_flow_proposal(&mut f.perspective, &minted[0])
        .await
        .expect("reject");
    assert!(
        f.links_by_predicate(&minted[0]).await.is_empty(),
        "rejected proposal must be hard-deleted"
    );
    assert!(
        reject_flow_proposal(&mut f.perspective, &minted[0])
            .await
            .is_err(),
        "rejecting a deleted proposal must error"
    );

    let recs = load_flow_instances(&f.perspective, &[BASE_URI.to_string()])
        .await
        .expect("load instances");
    assert_eq!(recs[0].current_state, "identified");
}

/// Only the DECLARED transition graph may fire (CWE-863 fix): a proposal
/// with a VALID evidence seal targeting an existing state that no declared
/// transition reaches from the current one is skipped — kept, not
/// invalidated — and the instance does not move.
#[tokio::test(flavor = "multi_thread")]
async fn consensus_pass_skips_undeclared_transitions_e2e() {
    use super::flow_consensus::run_flow_consensus_pass;
    use super::flow_evaluator::recompute_evidence_hash;

    let mut f = seed_satisfied_fixture(None).await;

    // Walk the declared edge first: identified → scoped.
    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");
    let outcomes = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert_eq!(outcomes.len(), 1, "declared edge must fire: {outcomes:?}");

    // Give "identified" a satisfiable guard so the backward proposal can
    // carry a REAL seal — isolating the declared-transition gate from the
    // empty-seal gate.
    f.perspective
        .add_link(
            Link {
                source: "delivery://Delivery.identified".to_string(),
                predicate: Some("ad4m://requires".to_string()),
                target: literal(r#"[{"className":"ns://Task","count":{"min":1}}]"#),
            },
            LinkStatus::Local,
            None,
            &f.ctx,
        )
        .await
        .expect("add requires to identified");

    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let flow = flows.get(FLOW_URI).expect("delivery flow");
    let recs = load_flow_instances(&f.perspective, &[BASE_URI.to_string()])
        .await
        .expect("records");
    let acting_did = crate::agent::did_for_context(&f.ctx).expect("did_for_context");
    let hash = recompute_evidence_hash(&f.perspective, flow, &recs[0], "identified", &acting_did)
        .await
        .expect("recompute")
        .expect("guard satisfied");

    // scoped → identified exists as states but NOT as a declared transition.
    let back = write_flow_transition_proposal(
        &mut f.perspective,
        "undeclared-edge",
        &acting_did,
        &f.instance_uri.clone(),
        "scoped",
        "identified",
        &["ad4m://task/1".to_string()],
        &hash,
        None,
        None,
        &f.ctx,
    )
    .await
    .expect("write undeclared-edge proposal");

    let outcomes = run_flow_consensus_pass(&mut f.perspective, None, &f.ctx, None).await;
    assert!(
        outcomes.is_empty(),
        "undeclared edge must not fire: {outcomes:?}"
    );

    // Skipped, NOT invalidated — the flow definition may gain the edge later.
    assert!(
        !f.links_by_predicate(&back).await.is_empty(),
        "undeclared-edge proposal must be kept"
    );
    let recs = load_flow_instances(&f.perspective, &[BASE_URI.to_string()])
        .await
        .expect("records");
    assert_eq!(recs[0].current_state, "scoped", "instance must not move");
}

/// Design principle #5 against the live store: an instance still carrying
/// an unaccepted interpretation overlay (its `ad4m://interp/kind` link)
/// neither satisfies a `requires` guard nor gets cited as evidence.
#[tokio::test(flavor = "multi_thread")]
async fn overlay_pending_evidence_is_excluded_e2e() {
    let mut f = seed_fixture(None).await;
    f.seed_task("ad4m://task/pending", "Pending Task").await;
    // What `apply_with_overlay` leaves on a base until a human accepts it.
    f.perspective
        .add_link(
            Link {
                source: "ad4m://task/pending".to_string(),
                predicate: Some("ad4m://interp/kind".to_string()),
                target: literal("create"),
            },
            LinkStatus::Local,
            None,
            &f.ctx,
        )
        .await
        .expect("add overlay kind link");

    let minted = f.run_pass(&[], None).await;
    assert!(
        minted.is_empty(),
        "overlay-pending evidence must not satisfy the guard: {minted:?}"
    );

    // A committed task satisfies it — and the proposal cites ONLY that one.
    f.seed_task("ad4m://task/committed", "Committed Task").await;
    let minted = f.run_pass(&[], None).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");
    let by_pred = f.links_by_predicate(&minted[0]).await;
    let evidence = by_pred
        .get("ad4m://flow/evidence")
        .cloned()
        .unwrap_or_default();
    assert!(
        evidence.iter().any(|t| t.contains("committed")),
        "committed instance must be cited: {evidence:?}"
    );
    assert!(
        !evidence.iter().any(|t| t.contains("pending")),
        "pending instance must not be cited: {evidence:?}"
    );
}
