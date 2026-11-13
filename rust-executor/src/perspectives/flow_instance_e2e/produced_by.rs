use super::*;

// ---------------------------------------------------------------------------
// producedByFlow: valid outputs of a completed run (produced.rs)
// ---------------------------------------------------------------------------

/// Write `receipt`'s body to the graph exactly as any member could — the
/// receipt-content link off its content-derived node, plus the flow → receipt
/// index link under the flow it claims. Discovery only; whether it proves
/// anything is decided by verification, which is the point of half these
/// tests (so the forgery must be *found*, or those tests would pass on
/// discovery instead of on the verifier).
async fn plant_receipt(
    f: &mut Fixture,
    receipt: &crate::perspectives::flow_instance::receipt::FlowReceipt,
) {
    let body = ad4m_client::literal::Literal::from_json(
        serde_json::to_value(receipt).expect("receipt serialises"),
    )
    .to_url()
    .expect("literal url");
    let uri = receipt.uri().expect("uri");
    f.link(
        &uri,
        crate::perspectives::flow_instance::receipt::FLOW_RECEIPT_CONTENT_PREDICATE,
        &body,
        LinkStatus::Shared,
    )
    .await;
    f.link(
        &receipt.flow_uri,
        crate::perspectives::flow_instance::produced::FLOW_RECEIPT_INDEX_PREDICATE,
        &uri,
        LinkStatus::Shared,
    )
    .await;
}

/// The ids `model_query` returns for `ns://Task` under a `producedByFlow`
/// filter, plus the reported total.
async fn tasks_produced_by(f: &Fixture, query: serde_json::Value) -> (Vec<String>, usize) {
    ids_of_class(f, "ns://Task", query).await
}

/// The ids `model_query` returns for `class`, plus the reported total.
async fn ids_of_class(f: &Fixture, class: &str, query: serde_json::Value) -> (Vec<String>, usize) {
    let json = f
        .perspective
        .model_query(class, &query.to_string())
        .await
        .expect("model_query");
    let result: serde_json::Value = serde_json::from_str(&json).expect("result parses");
    let ids = result["instances"]
        .as_array()
        .expect("instances")
        .iter()
        .map(|i| i["id"].as_str().expect("id").to_string())
        .collect();
    let total = result["totalCount"].as_u64().expect("totalCount") as usize;
    (ids, total)
}

/// The happy path, end to end through the production paths only: a proposer
/// commits to the run's outputs (`propose_flow_transition`), a second real
/// signer co-signs (`accept_flow_proposal`, `{n: 2}`), the completion is
/// minted from the live material (`mint_flow_receipt`) — and the output
/// answers all three consumer surfaces: the enumeration, the verdict, and
/// the model-query filter.
///
/// Red if `mint_flow_receipt` cannot collect what `FlowReceipt::mint`
/// demands from a live run (read-set, outputs content, preimages), or if any
/// surface reads the discovery edges as trust instead of running the
/// verifier.
#[tokio::test(flavor = "multi_thread")]
async fn a_cosigned_completion_mints_and_its_outputs_answer_every_produced_by_surface() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, mint_flow_receipt, verify_flow_receipt,
    };

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let outcome = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("the proposer's own production path");
    assert!(outcome.recorded_vote, "the proposer voted");
    assert!(outcome.outcomes.is_empty(), "one vote is short of {{n: 2}}");

    let bob = second_agent("bob-produced-by@e2e.test");
    let fired = accept_flow_proposal(&mut f.perspective, &outcome.proposal_uri, &bob)
        .await
        .expect("the co-signer's production path");
    assert!(!fired.is_empty(), "the second vote settles the edge");
    assert_eq!(f.derived().await.state, "scoped");

    let receipt = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("a settled run into a terminal state mints");
    assert_eq!(receipt.terminal_state, "scoped");

    // Surface 1: the enumeration, state-filtered and not.
    for state in [None, Some("scoped")] {
        let outputs = flow_valid_outputs(&f.perspective, &f.flow_uri, state)
            .await
            .expect("valid outputs");
        assert_eq!(outputs.len(), 1, "state {state:?}: {outputs:?}");
        assert_eq!(outputs[0].output, task_ref(TASK));
        assert_eq!(outputs[0].terminal_state, "scoped");
        assert_eq!(outputs[0].receipt_uri, receipt.uri().expect("uri"));
    }

    // Surface 2: the verdict — verified, with BOTH voters counted.
    let verdict = verify_flow_receipt(&f.perspective, &receipt)
        .await
        .expect("verify");
    let crate::perspectives::flow_instance::verify::ReceiptVerdict::Verified { voters, .. } =
        &verdict
    else {
        panic!("the honest completion must verify, got: {verdict}");
    };
    assert_eq!(voters.len(), 2, "the quorum was two distinct DIDs");

    // Surface 3: the model-query filter.
    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": {
            "flow": f.flow_uri, "state": "scoped",
        }}}),
    )
    .await;
    assert_eq!(ids, vec![TASK.to_string()]);
    assert_eq!(total, 1);

    // A state the run did not settle into admits nothing — same flow, same
    // receipt, different question.
    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": {
            "flow": f.flow_uri, "state": "identified",
        }}}),
    )
    .await;
    assert!(ids.is_empty(), "got {ids:?}");
    assert_eq!(total, 0);
}

/// The #1104 re-mint against the live surfaces. A member takes the honest
/// receipt's public signed material and re-writes it naming their own node
/// (with that node's real live content, so only the signature chain can
/// catch it). It sits in the graph as a perfectly ordinary receipt body —
/// and neither the enumeration nor the filter honours it, while the honest
/// receipt beside it keeps answering.
///
/// Red if any surface trusts `receipt.outputs`, the `granted_by`/receipt
/// discovery edges, or drops every receipt once one is bad.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_receipt_in_the_graph_neither_lists_nor_passes_the_filter() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, mint_flow_receipt, verify_flow_receipt,
    };
    use crate::perspectives::flow_instance::verify::ReceiptVerdict;

    let mut f = seed_satisfied_fixture(None).await;
    const SMUGGLED: &str = "ad4m://task/smuggled";
    f.seed_task(SMUGGLED, "Never voted on").await;
    let instance = f.instance_uri.clone();

    let outcome = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose");
    assert!(
        !outcome.outcomes.is_empty(),
        "the default {{n: 1}} settles on the proposer's own vote"
    );

    let honest = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("mint");

    let mut forged = honest.clone();
    forged.outputs = f.task_outputs(&[SMUGGLED.to_string()]).await;
    plant_receipt(&mut f, &forged).await;

    let outputs = flow_valid_outputs(&f.perspective, &f.flow_uri, None)
        .await
        .expect("valid outputs");
    let ids: Vec<&str> = outputs.iter().map(|o| o.output.id.as_str()).collect();
    assert_eq!(
        ids,
        vec![TASK],
        "the smuggled node must not be listed, and the honest output must survive its neighbour"
    );

    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": { "flow": f.flow_uri } } }),
    )
    .await;
    assert_eq!(ids, vec![TASK.to_string()]);
    assert_eq!(total, 1);

    // And the verdict names exactly what is wrong with the forgery.
    let verdict = verify_flow_receipt(&f.perspective, &forged)
        .await
        .expect("verify");
    assert!(
        matches!(verdict, ReceiptVerdict::OutputsNotCommitted { .. }),
        "got: {verdict}"
    );
}

/// The live-content check: an output edited AFTER the run completed is no
/// longer what the quorum committed to, so the enumeration and the filter
/// stop returning it — while the receipt itself keeps verifying, because a
/// receipt attests to the content at completion and freezing that is the
/// whole ratchet.
///
/// Red if `flow_valid_outputs` skips the live re-read, or if the filter
/// admits by id alone.
#[tokio::test(flavor = "multi_thread")]
async fn an_output_edited_after_completion_stops_being_a_valid_output() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, mint_flow_receipt, verify_flow_receipt,
    };

    let mut f = seed_satisfied_fixture(None).await;
    let instance = f.instance_uri.clone();
    propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose settles on {n: 1}");
    let receipt = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("mint");

    assert_eq!(
        flow_valid_outputs(&f.perspective, &f.flow_uri, None)
            .await
            .expect("valid outputs")
            .len(),
        1,
        "precondition: at completion the output is listed"
    );

    // The edit. `content` is the instance's model_query hydration, so a
    // re-asserted title moves `updatedAt` even before the value differs.
    f.seed_task(TASK, "Renamed after the vote").await;

    assert!(
        flow_valid_outputs(&f.perspective, &f.flow_uri, None)
            .await
            .expect("valid outputs")
            .is_empty(),
        "the live content no longer matches the commitment"
    );
    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": { "flow": f.flow_uri } } }),
    )
    .await;
    assert!(ids.is_empty(), "got {ids:?}");
    assert_eq!(total, 0);

    // The receipt is NOT invalidated — its preimages are frozen inside it.
    // "No longer a valid output as it stands" and "the receipt is bad" are
    // different findings, and conflating them would slander every receipt
    // whose output later gained a link.
    let verdict = verify_flow_receipt(&f.perspective, &receipt)
        .await
        .expect("verify");
    assert!(verdict.is_verified(), "the ratchet holds — got: {verdict}");
}

/// The filter runs BEFORE the page is cut (the #1093 `limitPerAnchor`
/// guarantee, restated for verification): with forged receipts planted for
/// two other tasks, `limit: 2` still returns the two VALID outputs — never
/// a page thinned down after slicing, and never a forged row occupying a
/// place a valid one should have had.
///
/// Red if the filter were applied after pagination — the window would then
/// hold forged-but-planted ids and the valid outputs would fall off the
/// page. That only holds if the planted rows really come first in the
/// unfiltered order, so the order is explicit (`title DESC` puts "Planted
/// too" and "Planted" ahead) and pinned as a precondition — the default
/// timestamp order ties within a millisecond, and an earlier version of this
/// test let a post-pagination mutant pass on that tie-break. `limit: 1` must
/// also still report the full total, which no page-then-filter
/// implementation can, whatever the order.
#[tokio::test(flavor = "multi_thread")]
async fn the_page_is_cut_after_the_filter_so_limit_counts_only_valid_outputs() {
    use crate::perspectives::flow_instance::produced::mint_flow_receipt;

    let mut f = seed_satisfied_fixture(None).await;
    const T2: &str = "ad4m://task/planted-2";
    const T3: &str = "ad4m://task/genuine-3";
    const T4: &str = "ad4m://task/planted-4";
    f.seed_task(T2, "Planted").await;
    f.seed_task(T3, "Also delivered").await;
    f.seed_task(T4, "Planted too").await;
    let instance = f.instance_uri.clone();

    // One run, honestly committing to TWO outputs.
    propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK), task_ref(T3)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose settles on {n: 1}");
    let honest = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("mint");

    // Forged receipts for the two planted tasks sit between them.
    for planted in [T2, T4] {
        let mut forged = honest.clone();
        forged.outputs = f.task_outputs(&[planted.to_string()]).await;
        plant_receipt(&mut f, &forged).await;
    }

    // Precondition: unfiltered, the first page of 2 is exactly the planted
    // pair — so a filter applied to that page would leave nothing.
    let (ids, _) = tasks_produced_by(
        &f,
        serde_json::json!({ "order": { "title": "DESC" }, "limit": 2 }),
    )
    .await;
    assert_eq!(
        ids,
        vec![T4.to_string(), T2.to_string()],
        "precondition: the planted rows lead the unfiltered order"
    );

    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({
            "where": { "producedByFlow": { "flow": f.flow_uri } },
            "order": { "title": "DESC" },
            "limit": 2,
        }),
    )
    .await;
    let mut sorted = ids.clone();
    sorted.sort();
    assert_eq!(
        sorted,
        vec![TASK.to_string(), T3.to_string()],
        "a page of 2 is 2 VALID outputs — got {ids:?}"
    );
    assert_eq!(total, 2, "and the total counts only what verified");

    // A page smaller than the valid set still reports the whole valid set.
    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({
            "where": { "producedByFlow": { "flow": f.flow_uri } },
            "order": { "title": "DESC" },
            "limit": 1,
        }),
    )
    .await;
    assert_eq!(ids.len(), 1, "got {ids:?}");
    assert!(
        ids[0] == TASK || ids[0] == T3,
        "the one row is a valid output — got {ids:?}"
    );
    assert_eq!(total, 2, "the total counts the filtered set, not the page");
}

/// A second class the fixture's Task node also conforms to: it asks for a
/// `title` and nothing else, so every Task is a `ns://Role` instance too —
/// with other content, since the hydration reads through a different shape.
const ROLE_SDNA: &str = r#"{
  "target_class":"ns://Role",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://title","target":"value"}],
  "properties":[
    {"path":"ns://title","name":"title","identity":true,"min_count":1,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://title","target":"value"}]}
  ]
}"#;

/// The class dimension through the filter (#1108): a run that committed to a
/// node **as a Task** has said nothing about that node as a Role, even when
/// the node conforms to both. So `producedByFlow` admits it into the Task
/// query and refuses it from the Role query — same flow, same receipt, same
/// id.
///
/// Pins which guard answered: the unfiltered Role query DOES return the
/// node, so shape conformance is not what excludes it — only the class
/// match on the committed `(class, id)` can be.
///
/// Red if the filter admits by id alone (`output_matches_class` ignored, or
/// matching on `output.id` only).
#[tokio::test(flavor = "multi_thread")]
async fn an_output_committed_as_a_task_is_not_produced_by_the_flow_as_a_role() {
    use crate::perspectives::flow_instance::produced::mint_flow_receipt;
    use crate::perspectives::perspective_instance::SdnaType;

    let mut f = seed_satisfied_fixture(None).await;
    let ctx = f.ctx.clone();
    f.perspective
        .add_sdna(
            "ns://Role".to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(ROLE_SDNA.to_string()),
            &ctx,
        )
        .await
        .expect("add_sdna(Role)");

    // Precondition: the Task node IS a Role instance by conformance.
    let (ids, _) = ids_of_class(&f, "ns://Role", serde_json::json!({})).await;
    assert!(
        ids.contains(&TASK.to_string()),
        "precondition: the Task node conforms to ns://Role — got {ids:?}"
    );

    let instance = f.instance_uri.clone();
    propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose settles on {n: 1}");
    mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("mint");

    let filter = serde_json::json!({ "where": { "producedByFlow": { "flow": f.flow_uri } } });

    let (ids, total) = ids_of_class(&f, "ns://Task", filter.clone()).await;
    assert_eq!(ids, vec![TASK.to_string()], "control: the committed class");
    assert_eq!(total, 1);

    let (ids, total) = ids_of_class(&f, "ns://Role", filter).await;
    assert!(
        ids.is_empty(),
        "committed as a Task, so not a valid Role output — got {ids:?}"
    );
    assert_eq!(total, 0, "and the total agrees");
}

/// An output that is no longer an instance of the class it was committed as
/// is not a valid output as it stands — the `None` arm of the live check.
/// Distinct from the edited-content arm: here there is no content to compare
/// at all, and the failure direction must still exclude.
///
/// Pins which guard answered: the receipt itself keeps verifying, so it is
/// the live re-read that drops the output, not the verifier.
///
/// Red if `flow_valid_outputs` keeps a candidate it cannot re-read.
#[tokio::test(flavor = "multi_thread")]
async fn an_output_that_is_no_longer_its_class_stops_being_a_valid_output() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, mint_flow_receipt, verify_flow_receipt,
    };

    let mut f = seed_satisfied_fixture(None).await;
    let instance = f.instance_uri.clone();
    propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose settles on {n: 1}");
    let receipt = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("mint");
    assert_eq!(
        flow_valid_outputs(&f.perspective, &f.flow_uri, None)
            .await
            .expect("valid outputs")
            .len(),
        1,
        "precondition: at completion the output is listed"
    );

    // Drop the `ns://type` marker the Task shape requires: the node is no
    // longer loadable as a `ns://Task`.
    let marker: Vec<LinkExpression> = links_of(&f, TASK)
        .await
        .into_iter()
        .filter(|l| l.data.predicate.as_deref() == Some("ns://type"))
        .map(LinkExpression::from)
        .collect();
    assert!(!marker.is_empty(), "the type marker exists to remove");
    f.perspective
        .remove_links(marker, None)
        .await
        .expect("de-type the output");
    assert!(
        load_outputs(&f.perspective, &[task_ref(TASK)])
            .await
            .expect("load")
            .is_empty(),
        "precondition: the output no longer reads as a Task"
    );

    assert!(
        flow_valid_outputs(&f.perspective, &f.flow_uri, None)
            .await
            .expect("valid outputs")
            .is_empty(),
        "an output that cannot be re-read is not returned"
    );
    let verdict = verify_flow_receipt(&f.perspective, &receipt)
        .await
        .expect("verify");
    assert!(
        verdict.is_verified(),
        "the receipt still verifies; only the live re-read excludes — got: {verdict}"
    );
}

/// "No such flow here" and "no valid outputs" are different answers. A flow
/// URI the perspective's catalogue does not hold is an error — through the
/// enumeration and through the model-query filter — while the real flow
/// with no receipts is an honest empty list.
///
/// Red if `flow_valid_outputs` answers an unknown flow with `Ok([])`: a
/// typo'd flow URI in a payout query would then read as "nobody delivered".
#[tokio::test(flavor = "multi_thread")]
async fn an_unknown_flow_is_an_error_not_an_empty_answer() {
    use crate::perspectives::flow_instance::produced::flow_valid_outputs;

    let f = seed_satisfied_fixture(None).await;
    const UNKNOWN: &str = "delivery://NoSuchFlow";

    // Control: the real flow, nothing minted — an honest, successful empty.
    assert!(flow_valid_outputs(&f.perspective, &f.flow_uri, None)
        .await
        .expect("a known flow with no receipts answers")
        .is_empty());

    let err = flow_valid_outputs(&f.perspective, UNKNOWN, None)
        .await
        .expect_err("an unknown flow must not answer with an empty list");
    assert!(
        err.to_string().contains("not on this perspective"),
        "the error names the reason — got: {err:#}"
    );

    let query = serde_json::json!({ "where": { "producedByFlow": { "flow": UNKNOWN } } });
    assert!(
        f.perspective
            .model_query("ns://Task", &query.to_string())
            .await
            .is_err(),
        "the model-query filter surfaces the same error"
    );
}

/// Complete the fixture's run with `TASK` as its one output and mint the
/// receipt — the honest material the budget tests below crowd around.
pub(super) async fn mint_honest_task_receipt(
    f: &mut Fixture,
) -> crate::perspectives::flow_instance::receipt::FlowReceipt {
    let instance = f.instance_uri.clone();
    propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose settles on {n: 1}");
    crate::perspectives::flow_instance::produced::mint_flow_receipt(
        &mut f.perspective,
        &instance,
        &f.ctx,
    )
    .await
    .expect("mint")
}

/// `receipt`'s body as any member would write it under its node.
fn receipt_body(receipt: &crate::perspectives::flow_instance::receipt::FlowReceipt) -> String {
    ad4m_client::literal::Literal::from_json(
        serde_json::to_value(receipt).expect("receipt serialises"),
    )
    .to_url()
    .expect("literal url")
}

/// A receipt of the fixture's flow minted from fixture-signed material
/// rather than through `propose` + `mint_flow_receipt`, which need a live
/// run per receipt: one distinct completed run per `nonce`, honestly
/// committing to `task` with the content this replica reads for it. Same
/// DNA, real signatures, so it verifies under the perspective's own
/// catalogue exactly as a production mint does. It stands in for the n-th
/// honest completion of the same flow, which is what the index is keyed by.
async fn fixture_minted_receipt(
    f: &Fixture,
    nonce: &str,
    task: &str,
) -> crate::perspectives::flow_instance::receipt::FlowReceipt {
    fixture_receipt_signed_by(f, nonce, task, "alice").await
}

/// [`fixture_minted_receipt`] proposed and signed by the persona `signer`.
async fn fixture_receipt_signed_by(
    f: &Fixture,
    nonce: &str,
    task: &str,
    signer: &str,
) -> crate::perspectives::flow_instance::receipt::FlowReceipt {
    use crate::perspectives::flow_instance::test_support::{did_of, signed_link, T1};
    fixture_receipt_with(f, nonce, task, did_of(signer), |uri, predicate, target| {
        signed_link(uri, predicate, target, signer, true, None, T1)
    })
    .await
}

/// [`fixture_minted_receipt`] proposed by `proposer`, each link signed by
/// `sign(proposal uri, predicate, target)`.
async fn fixture_receipt_with(
    f: &Fixture,
    nonce: &str,
    task: &str,
    proposer: &str,
    sign: impl Fn(&str, &str, &str) -> crate::types::DecoratedLinkExpression,
) -> crate::perspectives::flow_instance::receipt::FlowReceipt {
    use crate::perspectives::flow_instance::atom::{
        proposal_uri, EVIDENCE_HASHES_PREDICATE, FLOW_INSTANCE_PREDICATE, FROM_STATE_PREDICATE,
        OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE, PROPOSAL_NONCE_PREDICATE, PROPOSER_PREDICATE,
    };
    use crate::perspectives::flow_instance::receipt::FlowReceipt;
    use crate::perspectives::flow_instance::ProposalLinks;

    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let flow = &flows[&f.flow_uri];
    let outputs = f.task_outputs(&[task.to_string()]).await;
    assert_eq!(
        outputs.len(),
        1,
        "precondition: `{task}` is a Task on this replica"
    );
    let committed = outputs_hash(&outputs);
    let seal = crate::perspectives::flow_evaluator::evidence_hash(&[], &[]);
    let instance_uri = format!("ad4m://flow/instance/honest-{nonce}");
    let uri = proposal_uri(
        &instance_uri,
        "identified",
        "scoped",
        &seal,
        Some(&committed),
        proposer,
        nonce,
    );
    let signed = |predicate: &str, target: &str| sign(&uri, predicate, target);
    let links = vec![
        signed(PROPOSER_PREDICATE, proposer),
        signed(FLOW_INSTANCE_PREDICATE, &instance_uri),
        signed(FROM_STATE_PREDICATE, &literal("identified")),
        signed(TO_STATE_PREDICATE, &literal("scoped")),
        signed(EVIDENCE_HASHES_PREDICATE, &literal(&seal)),
        signed(PROPOSAL_NONCE_PREDICATE, &literal(nonce)),
        signed(OUTPUT_PREDICATE, &literal(&task_ref(task).encode())),
        signed(OUTPUTS_HASH_PREDICATE, &literal(&committed)),
    ];
    let read_set = ReadSet {
        instance_uri,
        subject: "ad4m://task/onboarding".to_string(),
        genesis: "identified".to_string(),
        proposals: vec![ProposalLinks { uri, links }],
        role_grants: Vec::new(),
    };
    FlowReceipt::mint(flow, read_set, outputs, Vec::new()).expect("the fixture run mints")
}

/// **The 257th honest run** (#1177). The receipt index is keyed by the flow
/// *definition*, so every completed run of F files one more entry under F,
/// and a count cap on those entries made the 257th honest completion of
/// any flow refuse every receipt read for good, with nothing to retract: a
/// role-granting flow stopped granting after 256 grants. 257 honest
/// receipts, each naming its own task: every one loads, every surface
/// answers all 257, and nothing refuses.
///
/// Red on `dev`: `load_flow_receipts` returns `ReceiptBudgetExceeded` at
/// 257, and so do the enumeration and the filter.
#[tokio::test(flavor = "multi_thread")]
async fn two_hundred_and_fifty_seven_honest_receipts_all_load() {
    use crate::perspectives::flow_instance::produced::{flow_valid_outputs, load_flow_receipts};

    const N: usize = 257;
    let mut f = seed_satisfied_fixture(None).await;
    let flow = f.flow_uri.clone();

    let mut expected: Vec<String> = Vec::with_capacity(N);
    for i in 0..N {
        let task = format!("ad4m://task/delivered-{i:03}");
        f.seed_task(&task, &format!("Delivered {i}")).await;
        let receipt = fixture_minted_receipt(&f, &format!("run-{i:03}"), &task).await;
        plant_receipt(&mut f, &receipt).await;
        expected.push(task);
    }
    expected.sort();

    let receipts = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect("257 honest receipts are honest use, not a flood");
    assert_eq!(receipts.len(), N, "every honest receipt loads");

    let outputs = flow_valid_outputs(&f.perspective, &flow, Some("scoped"))
        .await
        .expect("the enumeration answers");
    let ids: Vec<String> = outputs.iter().map(|o| o.output.id.clone()).collect();
    assert_eq!(ids, expected, "every honest run's output is a valid output");

    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": { "flow": flow } }, "limit": N }),
    )
    .await;
    assert_eq!(total, N, "the filter counts every honest output");
    let mut ids = ids;
    ids.sort();
    assert_eq!(ids, expected);
}

/// A flood of junk under F's index, every kind at once, beside one honest
/// receipt: the junk is skipped entry by entry and the honest receipt still
/// answers every surface. Nothing is refused, and nothing genuine is
/// dropped. Each kind is dismissed by exactly one check, so removing any
/// check lets its kind through:
///
/// 1. targets that are not receipt URIs (a stray node, a receipt-prefixed
///    non-hash) with the honest body hung under them — only the URI shape
///    dismisses them;
/// 2. 64 hex characters in UPPERCASE, including the honest hash's own
///    uppercase alias with the honest body under it — only the lowercase
///    rule dismisses them, and it is what keeps two index links from
///    aliasing one receipt;
/// 3. canonical-looking URIs with no body at all;
/// 4. bodies that do not hash to their URI: a forgery (the honest receipt
///    with one output id changed) and the honest body itself, both under a
///    URI that is neither's hash — only the content-hash rule dismisses
///    them;
/// 5. a receipt of ANOTHER flow, indexed under F, which does hash to its
///    own URI — only the flow check dismisses it;
/// 6. junk under the honest URI itself: a non-receipt and the forgery.
///
/// Pinned on `load_flow_receipts` directly, because `verified_outputs` has
/// a flow check of its own that would mask the loader's (5), and on the
/// enumeration and the filter for the surfaces.
///
/// Red on `dev`: 600 index entries are over the count cap, so every surface
/// refuses with `ReceiptBudgetExceeded`.
#[tokio::test(flavor = "multi_thread")]
async fn a_flood_of_junk_index_entries_is_skipped_and_the_genuine_receipt_still_counts() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, load_flow_receipts, FLOW_RECEIPT_INDEX_PREDICATE,
    };
    use crate::perspectives::flow_instance::receipt::{
        FLOW_RECEIPT_CONTENT_PREDICATE, RECEIPT_URI_PREFIX,
    };

    const PER_KIND: usize = 100;
    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let honest_uri = honest.uri().expect("uri");
    let honest_hash = honest_uri
        .strip_prefix(RECEIPT_URI_PREFIX)
        .expect("a minted receipt's URI is canonical")
        .to_string();
    let honest_body = receipt_body(&honest);
    let flow = f.flow_uri.clone();

    let index = |target: String| Link {
        source: flow.clone(),
        predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
        target,
    };
    let body = |source: String, target: String| Link {
        source,
        predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
        target,
    };
    let canonical = |hex: String| {
        assert!(
            hex.len() == 64
                && hex
                    .bytes()
                    .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b)),
            "fixture: `{hex}` must look canonical"
        );
        format!("{RECEIPT_URI_PREFIX}{hex}")
    };

    let mut links = Vec::new();
    for i in 0..PER_KIND {
        // 1. Not a receipt URI at all.
        for target in [
            format!("ad4m://task/junk-{i:04}"),
            format!("{RECEIPT_URI_PREFIX}-junk-{i:04}"),
        ] {
            links.push(index(target.clone()));
            links.push(body(target, honest_body.clone()));
        }
        // 2. Uppercase hex, the honest hash's alias first.
        let upper = if i == 0 {
            honest_hash.to_uppercase()
        } else {
            format!("{:0>64}", format!("ABCDEF{i:04}"))
        };
        assert!(upper.chars().any(|c| c.is_ascii_uppercase()) && upper.len() == 64);
        let target = format!("{RECEIPT_URI_PREFIX}{upper}");
        links.push(index(target.clone()));
        links.push(body(target, honest_body.clone()));
        // 3. Body-less.
        links.push(index(canonical(format!(
            "{:0>64}",
            format!("b0d1e55{i:04}")
        ))));
        // 4. Bodies that do not hash to their URI.
        let mut forged = honest.clone();
        forged.outputs[0].id = format!("ad4m://task/forged-{i:04}");
        let wrong = canonical(format!("{:0>64}", format!("f0e{i:04}")));
        links.push(index(wrong.clone()));
        links.push(body(wrong.clone(), receipt_body(&forged)));
        links.push(body(wrong, honest_body.clone()));
        // 5. Another flow's receipt, under its own hash, indexed under F.
        let mut other = honest.clone();
        other.flow_uri = format!("other://Flow{i:04}");
        let other_uri = other.uri().expect("uri");
        links.push(index(other_uri.clone()));
        links.push(body(other_uri, receipt_body(&other)));
        // 6. Junk under the honest URI itself.
        links.push(body(
            honest_uri.clone(),
            literal(&format!("not a receipt {i}")),
        ));
        links.push(body(honest_uri.clone(), receipt_body(&forged)));
    }
    let ctx = f.ctx.clone();
    f.perspective
        .add_links(links, LinkStatus::Shared, None, &ctx)
        .await
        .expect("plant the flood");

    let receipts = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect("junk is skipped, never refused");
    assert_eq!(
        receipts,
        vec![honest.clone()],
        "exactly the honest receipt loads: {} candidates came back",
        receipts.len()
    );

    let outputs = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("the enumeration answers under a flood");
    assert_eq!(
        outputs.iter().map(|o| o.output.clone()).collect::<Vec<_>>(),
        vec![task_ref(TASK)]
    );
    let (ids, total) = tasks_produced_by(
        &f,
        serde_json::json!({ "where": { "producedByFlow": { "flow": flow } } }),
    )
    .await;
    assert_eq!(ids, vec![TASK.to_string()]);
    assert_eq!(total, 1);
}

/// A body over `MAX_RECEIPT_BYTES` is not a receipt (Marvin on #1201).
/// `mint` refuses to write one, so no honest receipt is over the cap and the
/// loader loses nothing by dismissing one — and it must, because a copy of
/// an honest receipt with `outputs` swapped for a huge list keeps the real
/// signatures, hashes to its own URI and names F: it passes all three
/// candidate checks, costs a full fold on every read, and comes back as
/// `OutputsNotCommitted { claimed: <the whole list> }`.
///
/// Red without the size check in `load_flow_receipts`: the bulky copy loads.
#[tokio::test(flavor = "multi_thread")]
async fn a_body_over_the_receipt_size_cap_is_skipped_and_the_honest_receipt_beside_it_counts() {
    use crate::perspectives::flow_instance::produced::{flow_valid_outputs, load_flow_receipts};
    use crate::perspectives::flow_instance::receipt::MAX_RECEIPT_BYTES;

    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let flow = f.flow_uri.clone();

    let mut bulky = honest.clone();
    bulky.outputs[0].content = "x".repeat(MAX_RECEIPT_BYTES + 1);
    let size = bulky.body().expect("body").len();
    assert!(
        size > MAX_RECEIPT_BYTES,
        "fixture: {size} bytes must be over the cap"
    );
    plant_receipt(&mut f, &bulky).await;

    let receipts = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect("an oversize body is skipped, never refused");
    assert_eq!(
        receipts,
        vec![honest.clone()],
        "exactly the honest receipt loads: {} candidates came back",
        receipts.len()
    );

    let outputs = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("the enumeration answers");
    assert_eq!(
        outputs.iter().map(|o| o.output.clone()).collect::<Vec<_>>(),
        vec![task_ref(TASK)]
    );
}

/// The perspective's own memo, end to end (#1177): the first enumeration
/// verifies the honest receipt, the second re-verifies nothing, and a DNA
/// edit written to the graph — a consensus rule on a state — misses the
/// memo and excludes the receipt as `DnaChanged`, on the enumeration and on
/// the verdict surface alike. Then the memo holds the new verdict too: a
/// third read under the new DNA re-verifies nothing either.
///
/// Red if `flow_valid_outputs` does not go through the perspective's memo
/// (the count keeps growing), or if the memo key ignores the DNA (the
/// receipt keeps answering after the edit).
#[tokio::test(flavor = "multi_thread")]
async fn the_perspectives_memo_saves_re_verification_and_follows_the_dna() {
    use crate::perspectives::flow_instance::produced::{flow_valid_outputs, verify_flow_receipt};
    use crate::perspectives::flow_instance::verify::ReceiptVerdict;

    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let flow = f.flow_uri.clone();
    let memo = f.perspective.receipt_verdict_memo.clone();
    let before = memo.verifications();

    let first = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("valid outputs");
    assert_eq!(first.len(), 1);
    assert_eq!(
        memo.verifications(),
        before + 1,
        "the first read verifies the one receipt through the perspective's memo"
    );

    let second = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("valid outputs");
    assert_eq!(second, first);
    assert_eq!(
        memo.verifications(),
        before + 1,
        "the second read of an unchanged set re-verifies nothing"
    );

    // The DNA edit: a quorum rule on a state is part of the definition, so
    // the held hash moves and every receipt minted before it stops verifying.
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    assert!(
        flow_valid_outputs(&f.perspective, &flow, None)
            .await
            .expect("valid outputs")
            .is_empty(),
        "after the DNA changed the memoised Verified must not be served"
    );
    let verdict = verify_flow_receipt(&f.perspective, &honest)
        .await
        .expect("verify");
    assert!(
        matches!(verdict, ReceiptVerdict::DnaChanged { .. }),
        "the same receipt reads DnaChanged, got: {verdict}"
    );
    assert_eq!(
        memo.verifications(),
        before + 2,
        "the changed DNA is a new key: verified once more, then memoised"
    );
    flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("valid outputs");
    assert_eq!(memo.verifications(), before + 2);
}

/// A person's log whose device `d1` signs a receipt for the fixture's flow,
/// with a resolver on a fake store that serves the log — the setup of the
/// receipt-read prepare tests. The cache feeds the executor's global key
/// map, as the production `KelCache::open` does, so what the prepare fills
/// is what the verifier reads.
struct DeviceSigned {
    log: crate::identity::cache::tests::TestLog,
    cache: std::sync::Arc<crate::identity::cache::KelCache>,
    store: std::sync::Arc<crate::identity::cache::tests::FakeStore>,
    resolver: std::sync::Arc<crate::identity::resolver::Resolver>,
    receipt: crate::perspectives::flow_instance::receipt::FlowReceipt,
    _dir: tempfile::TempDir,
}

impl DeviceSigned {
    async fn new(f: &mut Fixture, seed: u8, task: &str) -> Self {
        use crate::identity::cache::tests::{accept_signed, store_key, FakeStore, TestLog};
        use crate::identity::cache::{global_key_map, KelCache};
        use crate::identity::resolver::Resolver;
        use crate::perspectives::flow_instance::test_support::device_link;
        use ad4m_identity_core::build::Signer;
        use std::sync::Arc;

        let log = TestLog::new(seed);
        let dir = tempfile::tempdir().unwrap();
        let cache = Arc::new(
            KelCache::open(
                dir.path().join("ad4m_db.sqlite"),
                vec![store_key().multikey()],
            )
            .unwrap(),
        );
        accept_signed(&cache, &log.events).await;
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let backend: Arc<dyn crate::identity::backend::IdentityBackend> = store.clone();
        let resolver = Arc::new(Resolver::new(cache.clone(), backend, None, false));
        let now = chrono::Utc::now().timestamp() as u64;
        global_key_map().note_confirmed(&log.scid(), now);
        cache.note_refreshed(&log.scid(), now);

        f.seed_task(task, "Delivered by a device").await;
        let proposer = format!("did:key:{}", log.device.key());
        let device = log.device.clone();
        let receipt = fixture_receipt_with(f, "device-run", task, &proposer, |uri, p, t| {
            device_link(
                uri,
                p,
                t,
                &device,
                0,
                crate::perspectives::flow_instance::test_support::T1,
            )
        })
        .await;
        Self {
            log,
            cache,
            store,
            resolver,
            receipt,
            _dir: dir,
        }
    }

    /// The store last confirmed the signer's head, and this replica last
    /// read its log, longer ago than the admission window: a carried link is
    /// `cantCheck` until a prepare refreshes the log.
    fn age_past_the_window(&self) {
        use crate::identity::resolver::ADMIT_FRESHNESS;
        let stale = chrono::Utc::now().timestamp() as u64 - ADMIT_FRESHNESS - 10;
        crate::identity::cache::global_key_map().note_confirmed(&self.log.scid(), stale);
        self.cache.note_refreshed(&self.log.scid(), stale);
    }
}

impl Drop for DeviceSigned {
    fn drop(&mut self) {
        crate::identity::cache::global_key_map().forget(&self.log.scid());
    }
}

/// Data's P3 on #1597, point 1: `verify_flow_receipt` refreshes the carried
/// signers' logs before it judges them. An honest receipt signed by a
/// person's device verifies after the signer's head aged past the admission
/// window, because the entry point's prepare reads the log again; the store
/// answers, so nothing about the receipt is in doubt.
///
/// Red with `prepare_carried_signers` removed from `verify_flow_receipt`:
/// the stale head leaves every carried link `cantCheck`, so the honest
/// receipt stays undecided. The resolver stands in for the started identity
/// module's (`with_test_resolver`); before `init`, the prepare does nothing.
#[tokio::test(flavor = "multi_thread")]
async fn verify_flow_receipt_refreshes_the_carried_signers_before_it_judges() {
    use crate::perspectives::flow_instance::produced::verify_flow_receipt;
    use std::sync::atomic::Ordering;

    let mut f = seed_satisfied_fixture(None).await;
    let signed = DeviceSigned::new(&mut f, 140, "ad4m://task/by-device").await;
    signed.age_past_the_window();
    let reads = signed.store.log_calls.load(Ordering::SeqCst);

    let verdict = crate::identity::with_test_resolver(
        signed.resolver.clone(),
        verify_flow_receipt(&f.perspective, &signed.receipt),
    )
    .await
    .expect("verify");
    assert!(
        verdict.is_verified(),
        "the entry point refreshes the stale signer and the honest receipt verifies: {verdict}"
    );
    assert!(
        signed.store.log_calls.load(Ordering::SeqCst) > reads,
        "the signer's log was read again"
    );
}

/// Data's P3 on #1597, point 2: a read prepares the carried signers of all
/// its receipts in one call, so `DISCOVERY_PER_BATCH` caps the key-index
/// lookups of the read, not of each receipt. `DISCOVERY_PER_BATCH + 6`
/// receipts, each signed by a `did:key` no log lists: the first read asks
/// about one batch, and its receipts verify; the other six wait (`cantCheck`,
/// not memoised) and the next read asks about them only.
///
/// Red while `flow_valid_outputs` prepares receipt by receipt: each call
/// gets its own cap, so the first read asks about all `N` signers.
#[tokio::test(flavor = "multi_thread")]
async fn a_read_asks_the_key_index_about_one_batch_of_signers_across_its_receipts() {
    use crate::identity::cache::tests::{store_key, FakeStore};
    use crate::identity::cache::KelCache;
    use crate::identity::resolver::{Resolver, DISCOVERY_PER_BATCH};
    use crate::perspectives::flow_instance::produced::flow_valid_outputs;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    const N: usize = DISCOVERY_PER_BATCH + 6;
    let mut f = seed_satisfied_fixture(None).await;
    let flow = f.flow_uri.clone();
    for i in 0..N {
        let task = format!("ad4m://task/fresh-signer-{i:03}");
        f.seed_task(&task, &format!("Fresh {i}")).await;
        // A persona per receipt: a fresh `did:key`, unknown to the key map.
        let signer = format!("fresh-signer-{}-{i}", std::process::id());
        let receipt = fixture_receipt_signed_by(&f, &format!("fresh-{i:03}"), &task, &signer).await;
        plant_receipt(&mut f, &receipt).await;
    }
    // A cache on a scratch file that feeds the executor's global key map,
    // which the verifier reads.
    let dir = tempfile::tempdir().unwrap();
    let cache = KelCache::open(
        dir.path().join("ad4m_db.sqlite"),
        vec![store_key().multikey()],
    )
    .unwrap();
    let store = Arc::new(FakeStore::new(vec![]));
    let backend: Arc<dyn crate::identity::backend::IdentityBackend> = store.clone();
    let resolver = Arc::new(Resolver::new(Arc::new(cache), backend, None, false));
    let read = || {
        crate::identity::with_test_resolver(
            resolver.clone(),
            flow_valid_outputs(&f.perspective, &flow, None),
        )
    };

    let first = read().await.expect("valid outputs");
    assert_eq!(
        store.key_owner_calls.load(Ordering::SeqCst),
        DISCOVERY_PER_BATCH,
        "one read asks about one batch of new signers, whatever the receipt count"
    );
    assert_eq!(
        first.len(),
        DISCOVERY_PER_BATCH,
        "the asked signers' receipts verify"
    );

    let second = read().await.expect("valid outputs");
    assert_eq!(
        store.key_owner_calls.load(Ordering::SeqCst),
        N,
        "the next read asks about the six left, and about no signer it already knows"
    );
    assert_eq!(
        second.len(),
        N,
        "every receipt verifies once its signer was asked about"
    );
}

/// Data's P3 on #1597, point 2: a receipt the perspective's memo already
/// holds as settled is not prepared again. Its verdict is final under the
/// memo key, so refreshing its signers' logs could change nothing the read
/// returns. The device-signed receipt verifies on the first read; then its
/// signer's head ages past the window, and the second read still lists the
/// output, from the memo, without reading the signer's log again.
///
/// Red while every read prepares every receipt: the second read refreshes
/// the stale log once more.
#[tokio::test(flavor = "multi_thread")]
async fn a_receipt_the_memo_holds_as_settled_is_not_prepared_again() {
    use crate::perspectives::flow_instance::produced::flow_valid_outputs;
    use std::sync::atomic::Ordering;

    let mut f = seed_satisfied_fixture(None).await;
    let signed = DeviceSigned::new(&mut f, 141, "ad4m://task/memo-settled").await;
    plant_receipt(&mut f, &signed.receipt).await;
    let flow = f.flow_uri.clone();
    let read = || {
        crate::identity::with_test_resolver(
            signed.resolver.clone(),
            flow_valid_outputs(&f.perspective, &flow, None),
        )
    };

    let first = read().await.expect("valid outputs");
    assert_eq!(first.len(), 1, "the device-signed receipt verifies");

    signed.age_past_the_window();
    let reads = signed.store.log_calls.load(Ordering::SeqCst);
    let second = read().await.expect("valid outputs");
    assert_eq!(second, first, "the memo still answers for the settled run");
    assert_eq!(
        signed.store.log_calls.load(Ordering::SeqCst),
        reads,
        "a settled receipt's signers are not refreshed again"
    );
}

/// Lal's P3 on #1597: a receipt the prepare step skips because the memo
/// answers it keeps that answer for the whole read, even when the fold
/// evicts it from the bounded LRU before reaching it.
///
/// The schedule, on a memo of capacity 1 (full after the first read): the
/// device-signed receipt R verifies and is the one entry; its signer's head
/// ages past the window. A new receipt N, signed by another signer and
/// sorting before R (receipts load in URI order), is planted. The second read
/// skips R at the prepare step (memoised) and prepares N; the fold verifies N
/// first and `put`s it, which evicts R; then reaches R.
///
/// Red while the prepare step only peeks: R misses, re-verifies with its
/// signer's head stale and not refreshed, reads `cantCheck`, and its output
/// drops from the read. Green with the answer held: R answers as the prepare
/// step saw it, and its signer's log is still not read again.
#[tokio::test(flavor = "multi_thread")]
async fn a_memoised_receipt_evicted_by_an_earlier_miss_still_answers_for_the_read() {
    use crate::perspectives::flow_instance::produced::flow_valid_outputs;
    use crate::perspectives::flow_instance::verify::memo::VerdictMemo;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    let mut f = seed_satisfied_fixture(None).await;
    f.perspective.receipt_verdict_memo = Arc::new(VerdictMemo::with_capacity(1));
    let signed = DeviceSigned::new(&mut f, 142, "ad4m://task/memo-evicted").await;
    plant_receipt(&mut f, &signed.receipt).await;
    let held_uri = signed.receipt.uri().expect("uri");
    let flow = f.flow_uri.clone();

    let first = crate::identity::with_test_resolver(
        signed.resolver.clone(),
        flow_valid_outputs(&f.perspective, &flow, None),
    )
    .await
    .expect("valid outputs");
    assert_eq!(first.len(), 1, "the device-signed receipt verifies");

    // N: an honest receipt by another signer whose URI sorts before R's, so
    // the fold reaches it first.
    f.seed_task("ad4m://task/evicts-early", "Settled first in the read")
        .await;
    let mut early = None;
    for i in 0..64 {
        let receipt =
            fixture_minted_receipt(&f, &format!("evicts-early-{i}"), "ad4m://task/evicts-early")
                .await;
        if receipt.uri().expect("uri") < held_uri {
            early = Some(receipt);
            break;
        }
    }
    let early = early.expect("a receipt URI sorting before the held one");
    plant_receipt(&mut f, &early).await;

    signed.age_past_the_window();
    let reads = signed.store.log_calls.load(Ordering::SeqCst);
    let second = crate::identity::with_test_resolver(
        signed.resolver.clone(),
        flow_valid_outputs(&f.perspective, &flow, None),
    )
    .await
    .expect("valid outputs");

    let ids: Vec<&str> = second.iter().map(|v| v.output.id.as_str()).collect();
    assert!(
        ids.contains(&"ad4m://task/evicts-early"),
        "precondition: the earlier receipt verifies and is memoised, got {ids:?}"
    );
    assert!(
        ids.contains(&"ad4m://task/memo-evicted"),
        "the receipt the prepare step skipped still answers after the earlier one evicted it, got {ids:?}"
    );
    assert_eq!(
        signed.store.log_calls.load(Ordering::SeqCst),
        reads,
        "a held receipt's signers are still not refreshed"
    );
}
