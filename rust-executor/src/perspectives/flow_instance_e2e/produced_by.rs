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
async fn mint_honest_task_receipt(
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

/// Lal's #1127 flood: any member can write `receipt_content` links (and the
/// flow → receipt index links) whose URIs sort below every genuine receipt.
/// Once flow F's read would exceed `MAX_FLOW_RECEIPTS` the answer must be an
/// **error** on every surface — never an empty list, which a payout gate or
/// #1076 would read as a confident "nothing was produced". Same rule as the
/// unknown flow: "I could not read every receipt" is not "no valid outputs".
///
/// Pinned at the boundary: `MAX - 1` junk candidates plus the honest receipt
/// is exactly the budget and still answers; one more is over it. The honest
/// receipt keeps verifying throughout, and the error is the typed
/// `ReceiptBudgetExceeded`, so it is the budget that answered.
///
/// Red if the read truncates silently (the pre-#1127-review behaviour: the
/// honest receipt was evicted and every surface answered `[]`), if the
/// budget is off by one, or if any surface swallows the error.
#[tokio::test(flavor = "multi_thread")]
async fn a_receipt_flood_is_an_error_not_an_empty_answer() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, load_flow_receipts, verify_flow_receipt, ReceiptBudgetExceeded,
        FLOW_RECEIPT_INDEX_PREDICATE, MAX_FLOW_RECEIPTS,
    };
    use crate::perspectives::flow_instance::receipt::{
        FLOW_RECEIPT_CONTENT_PREDICATE, RECEIPT_URI_PREFIX,
    };

    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let honest_uri = honest.uri().expect("uri");
    let flow = f.flow_uri.clone();

    // `-` sorts before every hex digit and letter, so each junk node leads
    // the honest one under the same prefix. The body need not be a receipt.
    let junk = |i: usize| {
        let uri = format!("{RECEIPT_URI_PREFIX}-junk-{i:04}");
        vec![
            Link {
                source: flow.clone(),
                predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
                target: uri.clone(),
            },
            Link {
                source: uri,
                predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
                target: literal(&format!("not a receipt {i}")),
            },
        ]
    };
    assert!(
        format!("{RECEIPT_URI_PREFIX}-junk-0000") < honest_uri,
        "precondition: junk sorts first"
    );
    let filter = serde_json::json!({ "where": { "producedByFlow": { "flow": flow } } });

    let ctx = f.ctx.clone();
    f.perspective
        .add_links(
            (0..MAX_FLOW_RECEIPTS - 1).flat_map(junk).collect(),
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("plant MAX - 1 junk candidates");
    assert_eq!(
        flow_valid_outputs(&f.perspective, &flow, None)
            .await
            .expect("exactly the budget still answers")
            .len(),
        1,
        "MAX - 1 junk candidates leave room for the honest receipt"
    );
    let (ids, _) = tasks_produced_by(&f, filter.clone()).await;
    assert_eq!(ids, vec![TASK.to_string()]);

    f.perspective
        .add_links(junk(MAX_FLOW_RECEIPTS - 1), LinkStatus::Shared, None, &ctx)
        .await
        .expect("plant the MAX-th junk candidate");

    let over = |e: &anyhow::Error| e.downcast_ref::<ReceiptBudgetExceeded>().cloned();
    let expected = ReceiptBudgetExceeded {
        flow: flow.clone(),
        found: MAX_FLOW_RECEIPTS + 1,
        cap: MAX_FLOW_RECEIPTS,
    };

    let err = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect_err("the loader must not hand back a truncated list");
    assert_eq!(over(&err), Some(expected.clone()), "loader: {err:#}");

    let err = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect_err("flowValidOutputs must refuse, not answer []");
    assert_eq!(over(&err), Some(expected.clone()), "enumeration: {err:#}");

    let err = f
        .perspective
        .model_query("ns://Task", &filter.to_string())
        .await
        .expect_err("the producedByFlow filter must refuse, not return an empty page");
    assert_eq!(over(&err), Some(expected), "filter: {err:#}");

    let verdict = verify_flow_receipt(&f.perspective, &honest)
        .await
        .expect("verify");
    assert!(
        verdict.is_verified(),
        "the honest receipt is fine on its own — only the budget refused, got: {verdict}"
    );
}

/// The no-attacker half of Lal's review: the budget is per flow. Receipts of
/// other flows — more than `MAX_FLOW_RECEIPTS` of them, all sorting below
/// flow F's honest receipt — plus as many stray bodies nobody indexed, must
/// neither hide F's receipt nor push F's read over budget. A busy
/// perspective with many flows is the normal case, not an attack.
///
/// Red if the read is perspective-wide (the pre-review behaviour: the other
/// flows' bodies filled the shared cap and evicted F's receipt, answering
/// `[]`), or if it counts candidates outside F's index.
#[tokio::test(flavor = "multi_thread")]
async fn other_flows_receipts_do_not_spend_this_flows_budget() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, FLOW_RECEIPT_INDEX_PREDICATE, MAX_FLOW_RECEIPTS,
    };
    use crate::perspectives::flow_instance::receipt::{
        FLOW_RECEIPT_CONTENT_PREDICATE, RECEIPT_URI_PREFIX,
    };

    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let honest_uri = honest.uri().expect("uri");
    let flow = f.flow_uri.clone();

    let mut links = Vec::new();
    for i in 0..=MAX_FLOW_RECEIPTS {
        // A receipt of another flow, indexed under that flow.
        let mut other = honest.clone();
        other.flow_uri = format!("other://Flow{i:04}");
        let uri = format!("{RECEIPT_URI_PREFIX}-other-{i:04}");
        links.push(Link {
            source: other.flow_uri.clone(),
            predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
            target: uri.clone(),
        });
        links.push(Link {
            source: uri,
            predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
            target: ad4m_client::literal::Literal::from_json(
                serde_json::to_value(&other).expect("serialises"),
            )
            .to_url()
            .expect("literal url"),
        });
        // A stray body under no index at all.
        links.push(Link {
            source: format!("{RECEIPT_URI_PREFIX}-stray-{i:04}"),
            predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
            target: literal(&format!("stray {i}")),
        });
    }
    assert!(
        format!("{RECEIPT_URI_PREFIX}-other-0000") < honest_uri,
        "precondition: the other flows' receipts sort first"
    );
    let ctx = f.ctx.clone();
    f.perspective
        .add_links(links, LinkStatus::Shared, None, &ctx)
        .await
        .expect("plant the other flows' receipts and the strays");

    let outputs = flow_valid_outputs(&f.perspective, &flow, None)
        .await
        .expect("other flows' receipts do not spend this flow's budget");
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

/// The budget counts flow F's **bodies** on their own: many bodies hung
/// under one indexed receipt URI — even the honest receipt's own — must hit
/// the budget, not be parsed without bound or silently cut. Pinned at the
/// boundary (`MAX` bodies answer, `MAX + 1` refuse) with the index holding a
/// single entry throughout, so only the body check can answer.
///
/// Red if the body check is removed (every body is parsed and the honest
/// receipt answers) or truncates instead of refusing.
#[tokio::test(flavor = "multi_thread")]
async fn a_body_flood_under_one_indexed_receipt_is_over_budget() {
    use crate::perspectives::flow_instance::produced::{
        flow_valid_outputs, load_flow_receipts, ReceiptBudgetExceeded, MAX_FLOW_RECEIPTS,
    };
    use crate::perspectives::flow_instance::receipt::FLOW_RECEIPT_CONTENT_PREDICATE;

    let mut f = seed_satisfied_fixture(None).await;
    let honest = mint_honest_task_receipt(&mut f).await;
    let honest_uri = honest.uri().expect("uri");
    let flow = f.flow_uri.clone();
    let junk_body = |i: usize| Link {
        source: honest_uri.clone(),
        predicate: Some(FLOW_RECEIPT_CONTENT_PREDICATE.to_string()),
        target: literal(&format!("not a receipt {i}")),
    };

    let ctx = f.ctx.clone();
    f.perspective
        .add_links(
            (0..MAX_FLOW_RECEIPTS - 1).map(junk_body).collect(),
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("hang MAX - 1 junk bodies beside the honest one");
    assert_eq!(
        flow_valid_outputs(&f.perspective, &flow, None)
            .await
            .expect("MAX bodies are exactly the budget")
            .len(),
        1
    );

    f.perspective
        .add_links(
            vec![junk_body(MAX_FLOW_RECEIPTS - 1)],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("hang the MAX-th junk body");
    let err = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect_err("MAX + 1 bodies under one entry are over budget");
    assert_eq!(
        err.downcast_ref::<ReceiptBudgetExceeded>().cloned(),
        Some(ReceiptBudgetExceeded {
            flow,
            found: MAX_FLOW_RECEIPTS + 1,
            cap: MAX_FLOW_RECEIPTS,
        }),
        "{err:#}"
    );
}

/// And on the **index** on its own: flow F's index entries bound how many
/// receipt nodes one read visits, whether or not anything hangs under them.
/// An index flood with no bodies at all — the flood that costs the reader
/// one lookup per entry and costs the attacker one link each — must hit the
/// budget, while the bodies stay far below it.
///
/// Red if the index check is removed: the bodies-only count (one, the
/// honest body) would let the read visit every entry and answer.
#[tokio::test(flavor = "multi_thread")]
async fn an_index_flood_without_bodies_is_over_budget() {
    use crate::perspectives::flow_instance::produced::{
        load_flow_receipts, ReceiptBudgetExceeded, FLOW_RECEIPT_INDEX_PREDICATE, MAX_FLOW_RECEIPTS,
    };
    use crate::perspectives::flow_instance::receipt::RECEIPT_URI_PREFIX;

    let mut f = seed_satisfied_fixture(None).await;
    mint_honest_task_receipt(&mut f).await;
    let flow = f.flow_uri.clone();

    let ctx = f.ctx.clone();
    f.perspective
        .add_links(
            (0..MAX_FLOW_RECEIPTS)
                .map(|i| Link {
                    source: flow.clone(),
                    predicate: Some(FLOW_RECEIPT_INDEX_PREDICATE.to_string()),
                    target: format!("{RECEIPT_URI_PREFIX}-empty-{i:04}"),
                })
                .collect(),
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .expect("file MAX body-less entries under the flow");

    let err = load_flow_receipts(&f.perspective, &flow)
        .await
        .expect_err("MAX + 1 index entries are over budget, bodies or not");
    assert_eq!(
        err.downcast_ref::<ReceiptBudgetExceeded>().cloned(),
        Some(ReceiptBudgetExceeded {
            flow,
            found: MAX_FLOW_RECEIPTS + 1,
            cap: MAX_FLOW_RECEIPTS,
        }),
        "{err:#}"
    );
}
