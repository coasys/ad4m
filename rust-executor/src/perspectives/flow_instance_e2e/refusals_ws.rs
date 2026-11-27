//! A refused flow move on the wire. Each test calls the real WS-RPC handler
//! (`perspective.proposeFlowTransition`, `.acceptFlowProposal` or
//! `.rejectFlowProposal`) and checks the reply: code 409 and the exact `data`.
//! A failure that is no refusal stays 500.

use super::super::flow_evaluator_e2e::seed_fixture;
use super::super::flow_instance::atom::FLOW_INSTANCE_PREDICATE;
use super::super::flow_instance::refusal::FlowRefusal;
use super::manual_path::fork_flow;
use super::*;
use crate::api::ws_handler::{build_handler_map, HandlerMap, WsRpcError};
use crate::types::RequestContext;
use serde_json::{json, Value};
use std::sync::Arc;

/// The fixture's perspective, registered so the handlers find it by uuid.
struct Wire {
    handlers: HandlerMap,
    uuid: String,
}

impl Wire {
    fn new(f: &Fixture) -> Self {
        let uuid = f.perspective.uuid.clone();
        crate::perspectives::register_perspective(uuid.clone(), f.perspective.clone());
        Wire {
            handlers: build_handler_map(),
            uuid,
        }
    }

    async fn call(&self, method: &str, mut params: Value) -> Result<Value, WsRpcError> {
        params["uuid"] = json!(self.uuid);
        let ctx = Arc::new(RequestContext::operator_for_test());
        self.handlers.dispatch(method, params, ctx).await
    }

    async fn propose(
        &self,
        instance: &str,
        to_state: &str,
        outputs: &[OutputRef],
    ) -> Result<Value, WsRpcError> {
        self.call(
            "perspective.proposeFlowTransition",
            json!({ "instanceUri": instance, "toState": to_state, "outputs": outputs }),
        )
        .await
    }

    async fn accept(&self, proposal: &str) -> Result<Value, WsRpcError> {
        self.call(
            "perspective.acceptFlowProposal",
            json!({ "proposalUri": proposal }),
        )
        .await
    }

    async fn reject(&self, proposal: &str) -> Result<Value, WsRpcError> {
        self.call(
            "perspective.rejectFlowProposal",
            json!({ "proposalUri": proposal }),
        )
        .await
    }
}

impl Drop for Wire {
    fn drop(&mut self) {
        crate::perspectives::unregister_perspective(&self.uuid);
    }
}

/// The `data` of a 409 reply. Fails when the call was not refused, or when the
/// message is not the sentence of the refusal in `data`.
fn refusal_data(reply: Result<Value, WsRpcError>) -> Value {
    let error = reply.expect_err("the call must be refused");
    assert_eq!(error.code, 409, "a refusal is 409: {}", error.message);
    let data = error.data.expect("a refusal carries data");
    let refusal: FlowRefusal = serde_json::from_value(data.clone()).expect("data is a FlowRefusal");
    assert_eq!(error.message, refusal.to_string());
    data
}

#[tokio::test(flavor = "multi_thread")]
async fn no_instance() {
    let f = seed_satisfied_fixture(None).await;
    let wire = Wire::new(&f);
    let data = refusal_data(
        wire.propose("ad4m://flow/instance/none", "scoped", &[])
            .await,
    );
    assert_eq!(
        data,
        json!({ "kind": "no-instance", "instance": "ad4m://flow/instance/none" })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn no_proposal() {
    let f = seed_satisfied_fixture(None).await;
    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept("ad4m://flow/proposal/none").await);
    assert_eq!(
        data,
        json!({ "kind": "no-proposal", "proposal": "ad4m://flow/proposal/none" })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn not_reachable() {
    let f = seed_satisfied_fixture(None).await;
    let wire = Wire::new(&f);
    let data = refusal_data(wire.propose(&f.instance_uri, "nowhere", &[]).await);
    assert_eq!(
        data,
        json!({ "kind": "not-reachable", "from": "identified", "to": "nowhere" })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn guard_unmet() {
    // `scoped` requires a Task, and this fixture has none.
    let f = seed_fixture(None).await;
    let wire = Wire::new(&f);
    let data = refusal_data(wire.propose(&f.instance_uri, "scoped", &[]).await);
    assert_eq!(data, json!({ "kind": "guard-unmet", "to": "scoped" }));
}

#[tokio::test(flavor = "multi_thread")]
async fn contested() {
    let mut f = seed_flow(fork_flow(), "start").await;
    f.seed_task(TASK, "Fork the road").await;
    propose(&mut f, "left-1", "start", "left").await;
    propose(&mut f, "right-1", "start", "right").await;
    consensus_pass(&mut f).await;
    assert!(
        f.derived().await.contested.is_some(),
        "the fixture is contested"
    );

    let wire = Wire::new(&f);
    let data = refusal_data(wire.propose(&f.instance_uri, "left", &[]).await);
    assert_eq!(
        data,
        json!({ "kind": "contested", "instance": f.instance_uri, "from": "start" })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn not_terminal() {
    let f = seed_review_flow().await;
    let wire = Wire::new(&f);
    let data = refusal_data(
        wire.propose(&f.instance_uri, "changes_requested", &[task_ref(TASK)])
            .await,
    );
    assert_eq!(
        data,
        json!({ "kind": "not-terminal", "to": "changes_requested" })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn output_not_instance_when_proposing() {
    let f = seed_satisfied_fixture(None).await;
    let wire = Wire::new(&f);
    let missing = task_ref("ad4m://task/never-written");
    let data = refusal_data(wire.propose(&f.instance_uri, "scoped", &[missing]).await);
    assert_eq!(
        data,
        json!({
            "kind": "output-not-instance",
            "output": { "className": "ns://Task", "id": "ad4m://task/never-written" },
            "proposal": null,
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn output_not_instance_when_voting() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let missing = task_ref("ad4m://task/never-written");
    let committed = honest_commitment(&f, &[task_ref(TASK)]).await;
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK), missing],
        Some(&committed),
    )
    .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(&proposal).await);
    assert_eq!(
        data,
        json!({
            "kind": "output-not-instance",
            "output": { "className": "ns://Task", "id": "ad4m://task/never-written" },
            "proposal": proposal,
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn outputs_uncommitted() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[],
        None,
    )
    .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(&proposal).await);
    assert_eq!(
        data,
        json!({ "kind": "outputs-uncommitted", "proposal": proposal })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn outputs_mismatch() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    const OTHER: &str = "ad4m://task/other";
    f.seed_task(OTHER, "Not an output").await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let widened = honest_commitment(&f, &[task_ref(TASK), task_ref(OTHER)]).await;
    let honest = honest_commitment(&f, &[task_ref(TASK)]).await;
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&widened),
    )
    .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(&proposal).await);
    assert_eq!(
        data,
        json!({
            "kind": "outputs-mismatch",
            "proposal": proposal,
            "committed": widened,
            "recomputed": honest,
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn different_outputs() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let committed = honest_commitment(&f, &[task_ref(TASK)]).await;
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&committed),
    )
    .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.propose(&f.instance_uri, "scoped", &[]).await);
    assert_eq!(
        data,
        json!({
            "kind": "different-outputs",
            "instance": f.instance_uri,
            "to": "scoped",
            "proposals": [bobs],
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn stale_proposal() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    // Bob's proposal leaves `scoped`, and the instance is in `identified`.
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "scoped", "identified", "seal").await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(&proposal).await);
    assert_eq!(
        data,
        json!({
            "kind": "stale-proposal",
            "proposal": proposal,
            "leaves": "scoped",
            "instance": f.instance_uri,
            "current": "identified",
        })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn not_engine_visible() {
    let mut f = seed_satisfied_fixture(None).await;
    // No flow-instance link at all: `reason` is null.
    let bare = "ad4m://flow/proposal/no-instance-link";
    f.link(bare, "ad4m://other", &literal("x"), LinkStatus::Shared)
        .await;
    // A flow-instance link and nothing else: the atom check gives a reason.
    let half = "ad4m://flow/proposal/half-synced";
    let instance = f.instance_uri.clone();
    f.link(half, FLOW_INSTANCE_PREDICATE, &instance, LinkStatus::Shared)
        .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(bare).await);
    assert_eq!(
        data,
        json!({ "kind": "not-engine-visible", "proposal": bare, "reason": null })
    );

    let data = refusal_data(wire.accept(half).await);
    assert_eq!(data["kind"], "not-engine-visible");
    assert_eq!(data["proposal"], half);
    assert!(
        data["reason"].as_str().is_some_and(|r| !r.is_empty()),
        "the atom check names its reason: {data}"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn evidence_unreproducible() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let proposal = sync_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        "a-seal-this-replica-cannot-reproduce",
    )
    .await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.accept(&proposal).await);
    assert_eq!(
        data,
        json!({ "kind": "evidence-unreproducible", "proposal": proposal })
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn not_your_proposal() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    let wire = Wire::new(&f);
    let data = refusal_data(wire.reject(&proposal).await);
    assert_eq!(
        data,
        json!({ "kind": "not-your-proposal", "proposal": proposal, "agent": acting_did(&f) })
    );
}

/// A failure is not a refusal: an output of a class this replica has no shape
/// for is a query error. It replies 500, with no `data`.
#[tokio::test(flavor = "multi_thread")]
async fn a_failure_stays_500() {
    let f = seed_satisfied_fixture(None).await;
    let wire = Wire::new(&f);
    let unknown_class = OutputRef {
        class_name: "ns://NoSuchClass".to_string(),
        id: TASK.to_string(),
    };
    let error = wire
        .propose(&f.instance_uri, "scoped", &[unknown_class])
        .await
        .expect_err("the output cannot load");
    assert_eq!(error.code, 500, "{}", error.message);
    assert!(error.data.is_none(), "a failure carries no refusal data");
}
