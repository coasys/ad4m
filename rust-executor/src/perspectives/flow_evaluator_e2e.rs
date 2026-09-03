//! Live-perspective coverage for `flow_evaluator`: real store, real SDNA,
//! real `model_query`, no LLM. The unit tests in `flow_evaluator` stub the
//! query layer; these prove the translated `requires` query is a shape
//! `model_query` accepts and that a proposal lands with every declared
//! predicate.

use super::flow_classes::{mint_flow_instance, write_flow_transition_proposal};
use super::flow_context::{load_flow_instances, load_shacl_flows};
use super::flow_evaluator::{evaluate_flow_transitions, evidence_hash, run_engine_proposal_pass};
use super::interpretation_test_support::{seed_instance, setup_perspective_no_llm, TASK_SDNA};
use super::model_query::types::ModelShape;
use super::perspective_instance::PerspectiveInstance;
use super::shacl_parser::parse_flow_to_links;
use crate::agent::AgentContext;
use crate::types::{LinkQuery, LinkStatus};
use std::collections::HashMap;

const FLOW_URI: &str = "delivery://DeliveryFlow";
const BASE_URI: &str = "ad4m://task/onboarding";

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
/// requires at least one `ns://Task`, plus one FlowInstance sitting in
/// `identified`. `requires` is declared on the flow JSON so
/// `parse_flow_to_links` emits the production `ad4m://requires` link.
async fn seed_delivery_fixture() -> Fixture {
    seed_delivery_fixture_with_requires(serde_json::json!([
        { "className": "ns://Task", "count": { "min": 1 } }
    ]))
    .await
}

async fn seed_delivery_fixture_with_requires(requires: serde_json::Value) -> Fixture {
    let (mut perspective, mut shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    let flow_json = serde_json::json!({
        "name": "Delivery",
        "namespace": "delivery://",
        "states": [
            { "name": "identified", "value": 0.0 },
            { "name": "scoped", "value": 0.5, "requires": requires }
        ],
        "transitions": [
            { "action_name": "Scope", "from_state": "identified", "to_state": "scoped", "actions": [] }
        ],
    })
    .to_string();
    let links = parse_flow_to_links(&flow_json, "Delivery").expect("parse_flow_to_links");
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

    async fn satisfied(&self) -> Vec<super::flow_evaluator::SatisfiedTransition> {
        let records = load_flow_instances(&self.perspective, &[BASE_URI.to_string()])
            .await
            .expect("load_flow_instances");
        let flows = load_shacl_flows(&self.perspective)
            .await
            .expect("load_shacl_flows");
        evaluate_flow_transitions(&self.perspective, &records, &flows, "did:key:acting").await
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

#[tokio::test(flavor = "multi_thread")]
async fn evaluate_flow_transitions_e2e() {
    let mut f = seed_delivery_fixture().await;

    let flows = load_shacl_flows(&f.perspective).await.unwrap();
    let scoped = flows[FLOW_URI]
        .states
        .iter()
        .find(|s| s.name == "scoped")
        .expect("scoped state parsed");
    assert_eq!(
        scoped.requires.as_ref().map(|r| r[0].class_name.as_str()),
        Some("ns://Task"),
        "requires declared on the flow JSON must round-trip through parse_flow_to_links"
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
        evidence_hash(&["ns://Task".to_string()], &t.evidence_ids)
    );
}

/// A non-empty `where` must survive translation into a shape `model_query`
/// actually deserialises — the original Matches/Exists bug was a unit test
/// asserting emitted JSON, not that `ModelQueryInput` accepted it.
#[tokio::test(flavor = "multi_thread")]
async fn evaluate_flow_transitions_where_equals_e2e() {
    let mut f = seed_delivery_fixture_with_requires(serde_json::json!([
        { "className": "ns://Task", "where": { "title": "Onboard Ana" }, "count": { "min": 1 } }
    ]))
    .await;

    f.seed_task("ad4m://task/2", "Onboard Bo").await;
    assert!(
        f.satisfied().await.is_empty(),
        "wrong title must not satisfy the guard"
    );

    f.seed_task("ad4m://task/1", "Onboard Ana").await;
    let after = f.satisfied().await;
    assert_eq!(after.len(), 1, "got {after:?}");
    assert_eq!(after[0].evidence_ids, vec!["ad4m://task/1".to_string()]);
}

/// Two Tasks so the `evidence` collection has more than one element: the
/// writer seeds the first through `create_subject` and streams the rest
/// through `update_subject`, and both must land.
#[tokio::test(flavor = "multi_thread")]
async fn write_flow_transition_proposal_lands_all_predicates_e2e() {
    let mut f = seed_delivery_fixture().await;
    f.seed_task("ad4m://task/1", "Onboard Ana").await;
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

#[tokio::test(flavor = "multi_thread")]
async fn run_engine_proposal_pass_e2e() {
    let mut f = seed_delivery_fixture().await;

    let subjects = vec![BASE_URI.to_string()];
    let before = run_engine_proposal_pass(&mut f.perspective, &subjects, &f.ctx).await;
    assert!(
        before.is_empty(),
        "guard unmet → no proposal, got {before:?}"
    );

    f.seed_task("ad4m://task/1", "Onboard Ana").await;
    let minted = run_engine_proposal_pass(&mut f.perspective, &subjects, &f.ctx).await;
    assert_eq!(minted.len(), 1, "got {minted:?}");
    assert!(minted[0].starts_with("ad4m://flow/proposal/"));

    let acting_did = crate::agent::did_for_context(&f.ctx).expect("did_for_context");
    let by_pred = f.links_by_predicate(&minted[0]).await;
    assert_has_target(&by_pred, "ad4m://flow/proposer", &acting_did);
    assert_has_target(&by_pred, "ad4m://flow/instance", &f.instance_uri);
    assert_has_target(&by_pred, "ad4m://flow/from_state", &literal("identified"));
    assert_has_target(&by_pred, "ad4m://flow/to_state", &literal("scoped"));
}

/// Running the proposal pass twice without changing the graph must not
/// create a duplicate proposal — the second pass sees the first proposal's
/// evidence hash and skips the write.
#[tokio::test(flavor = "multi_thread")]
async fn run_engine_proposal_pass_is_idempotent_e2e() {
    let mut f = seed_delivery_fixture().await;
    f.seed_task("ad4m://task/1", "Onboard Ana").await;

    let subjects = vec![BASE_URI.to_string()];
    let first = run_engine_proposal_pass(&mut f.perspective, &subjects, &f.ctx).await;
    assert_eq!(first.len(), 1, "first pass must mint one proposal");

    let second = run_engine_proposal_pass(&mut f.perspective, &subjects, &f.ctx).await;
    assert!(
        second.is_empty(),
        "second pass must skip the duplicate, got {second:?}"
    );

    let all_proposals = f
        .perspective
        .get_links(&LinkQuery {
            predicate: Some("ad4m://flow/evidence_hashes".into()),
            ..Default::default()
        })
        .await
        .expect("get_links");
    assert_eq!(
        all_proposals.len(),
        1,
        "only one proposal should exist on the graph"
    );
}

/// Empty subjects → early return (no unbounded sweep). The extraction pass
/// wrote nothing, so there is nothing to re-evaluate.
#[tokio::test(flavor = "multi_thread")]
async fn run_engine_proposal_pass_empty_subjects_returns_empty() {
    let mut f = seed_delivery_fixture().await;
    f.seed_task("ad4m://task/1", "Onboard Ana").await;

    let result = run_engine_proposal_pass(&mut f.perspective, &[], &f.ctx).await;
    assert!(
        result.is_empty(),
        "empty subjects must return immediately, got {result:?}"
    );
}
