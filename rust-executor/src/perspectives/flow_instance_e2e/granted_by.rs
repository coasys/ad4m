use super::produced_by::mint_honest_task_receipt;
use super::*;

// ---------------------------------------------------------------------------
// grantedByFlow on the real store: #1076 rebuilt on produced.rs
// ---------------------------------------------------------------------------

/// The whole `grantedByFlow` path against a real perspective: the delivery
/// run completes with `TASK` as its output, `mint_flow_receipt` files the
/// receipt under the flow's index, and a role gate "owns a Task that the
/// delivery flow produced into `scoped`" reads it back through
/// `PerspectiveInstance::flow_receipts` — `produced`'s own loader.
///
/// 1. The holder is granted, dated from the receipt's quorum. The owner
///    assignment is written **before** the run, so dating from it would be
///    the earlier, wider answer, and the assertion can tell them apart.
/// 2. A gate naming a state the run did not settle into grants nothing.
/// 3. A DID that owns no task is not a member.
/// 4. A flood of the flow's index is the typed budget error out of
///    `resolve_role_grants`, not "not a member".
///
/// Red if the gate reads anything but F's index (no receipt is found and
/// the holder is not granted), if it dates from the assignment link, or if
/// the loader's budget error is swallowed.
#[tokio::test(flavor = "multi_thread")]
async fn a_granted_by_flow_gate_grants_the_holder_through_the_real_store() {
    use crate::perspectives::flow_evaluator::requires_query_input;
    use crate::perspectives::flow_instance::produced::{
        ReceiptBudgetExceeded, FLOW_RECEIPT_INDEX_PREDICATE, MAX_FLOW_RECEIPTS,
    };
    use crate::perspectives::flow_instance::receipt::{
        FLOW_RECEIPT_CONTENT_PREDICATE, RECEIPT_URI_PREFIX,
    };
    use crate::perspectives::flow_instance::roles::resolve_role_grants;
    use crate::perspectives::flow_instance::time::parse_link_timestamp;
    use crate::perspectives::flow_instance::verify::{verify_receipt, ReceiptVerdict};
    use crate::perspectives::shacl_parser::ModelQuery;

    let mut f = seed_satisfied_fixture(None).await;
    grant_owner_role(&mut f).await;
    tick().await;
    let receipt = mint_honest_task_receipt(&mut f).await;

    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let ReceiptVerdict::Verified { settled_at, .. } = verify_receipt(&flows, &receipt) else {
        panic!("precondition: the honest receipt verifies");
    };
    let me = acting_did(&f);
    let record = f.instances().await.remove(0);
    let gate = |state: &str| -> ModelQuery {
        serde_json::from_value(serde_json::json!({
            "className": "ns://Task",
            "didProperty": "owner",
            "grantedByFlow": { "flow": f.flow_uri, "terminalState": state },
        }))
        .expect("role query")
    };
    let grant_for =
        |role: &ModelQuery,
         did: &str,
         evidence: &[crate::perspectives::flow_instance::roles::RoleGrantEvidence]| {
            let translated = requires_query_input(role, &record, did).expect("translates");
            evidence[0]
                .resolve(&translated, role, GrantContext::root(&flows))
                .expect("resolves")
        };

    // 1. The holder.
    let scoped = gate("scoped");
    let evidence = resolve_role_grants(
        &f.perspective,
        "delivery://Delivery.scoped",
        &scoped,
        &record,
        std::slice::from_ref(&me),
    )
    .await
    .expect("within budget");
    let instance = evidence[0]
        .instances
        .iter()
        .find(|i| i.instance_id == TASK)
        .expect("the owned task is a matched role instance");
    assert_eq!(
        instance.granting_receipts,
        vec![receipt.clone()],
        "the receipt is found through the flow's index and carried with the instance"
    );
    let assigned_at = instance.grant_links[0].timestamp.clone();
    let grant = grant_for(&scoped, &me, &evidence);
    let window = grant
        .windows
        .iter()
        .find(|w| w.instance_id == TASK)
        .expect("the holder is granted");
    assert_eq!(window.granted_at, settled_at, "dated from the quorum");
    assert!(
        parse_link_timestamp(&assigned_at) < parse_link_timestamp(&settled_at),
        "precondition: the assignment ({assigned_at}) is earlier than the quorum \
         ({settled_at}), so dating from it would be the wider, wrong answer"
    );

    // 2. The same receipt does not answer for another ending.
    let identified = gate("identified");
    let grant = grant_for(&identified, &me, &evidence);
    assert!(
        grant.windows.is_empty(),
        "the run settled into `scoped`, not `identified`"
    );

    // 3. A DID that owns nothing.
    let nobody = "did:key:z6MkNobodyOwnsAnything".to_string();
    let evidence = resolve_role_grants(
        &f.perspective,
        "delivery://Delivery.scoped",
        &scoped,
        &record,
        std::slice::from_ref(&nobody),
    )
    .await
    .expect("within budget");
    assert!(
        evidence[0].instances.is_empty(),
        "no role instance, so nothing for any receipt to grant"
    );

    // 4. The flood: the honest receipt plus MAX junk entries is one over.
    let flow = f.flow_uri.clone();
    let junk: Vec<Link> = (0..MAX_FLOW_RECEIPTS)
        .flat_map(|i| {
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
        })
        .collect();
    let ctx = f.ctx.clone();
    f.perspective
        .add_links(junk, LinkStatus::Shared, None, &ctx)
        .await
        .expect("plant the flood");
    let err = resolve_role_grants(
        &f.perspective,
        "delivery://Delivery.scoped",
        &scoped,
        &record,
        std::slice::from_ref(&me),
    )
    .await
    .expect_err("an over-budget index must be an error, not a denial");
    assert_eq!(
        err.downcast_ref::<ReceiptBudgetExceeded>(),
        Some(&ReceiptBudgetExceeded {
            flow,
            found: MAX_FLOW_RECEIPTS + 1,
            cap: MAX_FLOW_RECEIPTS,
        }),
        "the typed budget error: {err:#}"
    );
}
