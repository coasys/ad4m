use super::*;
// ---------------------------------------------------------------------------
// The content-addressed URI on the live graph (#1108)
// ---------------------------------------------------------------------------

/// **Lal's attack, live-graph variant.** Alice proposes the final edge
/// committing to [`TASK`], Bob co-signs, the edge settles — and Alice then
/// deletes her `outputs_hash`/`output` links and re-signs new ones under the
/// same URI. On a random URI the fold re-read the proposal with the swapped
/// commitment and both votes intact. The URI is a content address now, so
/// the re-signed fields no longer address it: the proposal stops being an
/// atom, both votes stop counting, and the instance falls back to genesis —
/// a run whose committed material was tampered with mid-air settles nothing.
///
/// Red while `from_links` skips the URI recompute: the state stays `scoped`
/// with the swapped commitment under it.
#[tokio::test(flavor = "multi_thread")]
async fn a_live_graph_outputs_swap_after_the_co_sign_uncounts_the_votes() {
    use crate::perspectives::flow_instance::atom::{OUTPUTS_HASH_PREDICATE, OUTPUT_PREDICATE};

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();

    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("propose");
    let bob = TestSigner::generate();
    sync_vote_from(&mut f, &bob, &out.proposal_uri).await;
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "precondition: the co-signed commitment settles the edge"
    );

    // The swap: withdraw the committed outputs, re-sign a different
    // commitment under the settled URI. Both writes are Alice's own.
    let stale: Vec<LinkExpression> = links_of(&f, &out.proposal_uri)
        .await
        .into_iter()
        .filter(|l| {
            l.data.predicate.as_deref() == Some(OUTPUTS_HASH_PREDICATE)
                || l.data.predicate.as_deref() == Some(OUTPUT_PREDICATE)
        })
        .map(LinkExpression::from)
        .collect();
    assert!(!stale.is_empty(), "the commitment links exist to remove");
    f.perspective
        .remove_links(stale, None)
        .await
        .expect("retract the committed outputs");
    f.link(
        &out.proposal_uri,
        OUTPUTS_HASH_PREDICATE,
        &literal(&outputs_hash(&[])),
        LinkStatus::Shared,
    )
    .await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "the swapped fields no longer address the voted URI, so the proposal \
         is not an atom and neither vote counts"
    );
    assert!(
        f.read_set().await.atoms().is_empty(),
        "the tampered proposal is dropped, not re-read with the new commitment"
    );
}

/// The honest control for everything above: a run completed through the
/// REAL co-sign path — a peer's committed proposal, this replica's
/// `accept_flow_proposal` (which re-derives the seal and recomputes the
/// outputs commitment before signing) — still mints a receipt that
/// verifies, content-addressed URIs and all.
#[tokio::test(flavor = "multi_thread")]
async fn an_honest_run_through_the_real_co_sign_path_mints_a_verifying_receipt() {
    use crate::perspectives::flow_instance::receipt::FlowReceipt;
    use crate::perspectives::flow_instance::verify::verify_receipt;

    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let committed = honest_commitment(&f, &[task_ref(TASK)]).await;
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-honest",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&committed),
    )
    .await;
    accept_flow_proposal(&mut f.perspective, &bobs, &f.ctx)
        .await
        .expect("the real co-sign path signs the committed proposal");
    assert_eq!(f.derived().await.state, "scoped", "n = 2 settled");

    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let flow = &flows[&f.flow_uri];
    let loaded = load_outputs(&f.perspective, &[task_ref(TASK)])
        .await
        .expect("load outputs");
    let outputs: Vec<_> = loaded.into_values().collect();
    let receipt = FlowReceipt::mint(
        flow,
        f.read_set().await,
        outputs,
        Vec::new(),
        crate::perspectives::flow_instance::grant::GrantContext::empty(),
    )
    .expect("the completed run mints");
    let verdict = verify_receipt(&flows, &receipt);
    assert!(
        verdict.is_verified(),
        "the honest end-to-end receipt verifies, got: {verdict}"
    );
}

/// The TS SDK registers its own `FlowTransitionProposal` shape
/// (`FlowInstance.start` → `Ad4mModel.registerAll`), and there `evidence` and
/// `outputs` are `@HasMany` relations: an `ad4m://adder`, no `ad4m://setter`.
/// That shape carries the `nonce` path, so `ensure_flow_model_classes` keeps
/// it — and `create_subject` writes values only through setters. The
/// engine's own proposal write must not depend on which shape a client
/// registered: the outputs commitment it signs has to land as links, or the
/// run completes with nothing to mint (the #1127 SDK test's CI failure).
///
/// Red if the writer hands the collections to `create_subject` — the
/// relation-shaped class drops both with a "declares no setter" warning.
#[tokio::test(flavor = "multi_thread")]
async fn a_client_registered_relation_shape_does_not_drop_the_proposal_collections() {
    use crate::perspectives::flow_classes::{
        FLOW_TRANSITION_PROPOSAL_CLASS, FLOW_TRANSITION_PROPOSAL_SDNA,
    };
    use crate::perspectives::flow_instance::atom::OUTPUT_PREDICATE;
    use crate::perspectives::flow_instance::produced::mint_flow_receipt;
    use crate::perspectives::perspective_instance::SdnaType;

    let mut f = seed_satisfied_fixture(None).await;

    // The relation-style shape: same class, same paths, but the two
    // collections carry an adder instead of a setter — what the TS SDK's
    // `@HasMany` generates.
    let mut shape: serde_json::Value =
        serde_json::from_str(FLOW_TRANSITION_PROPOSAL_SDNA).expect("hardwired SDNA parses");
    for property in shape["properties"].as_array_mut().expect("properties") {
        if matches!(property["name"].as_str(), Some("evidence" | "outputs")) {
            let setter = property
                .as_object_mut()
                .expect("property object")
                .remove("setter")
                .expect("the hardwired collection has a setter");
            property["adder"] = setter;
        }
    }
    let ctx = f.ctx.clone();
    f.perspective
        .add_sdna(
            FLOW_TRANSITION_PROPOSAL_CLASS.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(shape.to_string()),
            &ctx,
        )
        .await
        .expect("register the relation-style proposal shape");

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
    .expect("propose settles on {n: 1}");

    let proposal_links = links_of(&f, &outcome.proposal_uri).await;
    let with = |predicate: &str| {
        proposal_links
            .iter()
            .filter(|l| l.data.predicate.as_deref() == Some(predicate))
            .count()
    };
    assert_eq!(
        with(OUTPUT_PREDICATE),
        1,
        "the committed output must land as a link whatever shape is registered"
    );
    assert_eq!(
        with("ad4m://flow/evidence"),
        1,
        "and so must the cited evidence"
    );

    let receipt = mint_flow_receipt(&mut f.perspective, &instance, &f.ctx)
        .await
        .expect("a run whose outputs landed mints");
    assert_eq!(
        receipt
            .outputs
            .iter()
            .map(OutputRef::of)
            .collect::<Vec<_>>(),
        vec![task_ref(TASK)]
    );
}
