use super::*;
// ---------------------------------------------------------------------------
// The outputs commitment on the live store (#1104)
// ---------------------------------------------------------------------------

/// Delivery with an **unguarded** terminal `scoped`: its seal is over the
/// empty bag, so editing a Task cannot move it, and only the outputs check
/// can tell an edit apart. One Task is seeded.
async fn seed_unguarded_fixture() -> Fixture {
    let mut f = seed_flow(
        serde_json::json!({
            "name": "Delivery",
            "namespace": "delivery://",
            "states": [
                { "name": "identified", "value": 0.0 },
                { "name": "scoped", "value": 1.0 }
            ],
            "transitions": [
                { "action_name": "Scope", "from_state": "identified", "to_state": "scoped", "actions": [] }
            ],
        }),
        "identified",
    )
    .await;
    f.seed_task(TASK, "Onboard Ana").await;
    f
}

/// Required test (b) through the production co-sign path: Bob names the
/// Task as the output but signs a hash over the Task and another Task. The
/// seal reproduces, so only the outputs check can refuse, and it does,
/// writing nothing.
///
/// Red without the `check_outputs_commitment` call in
/// `accept_flow_proposal`.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_an_outputs_hash_that_does_not_match_the_named_outputs() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    const OTHER: &str = "ad4m://task/other";
    f.seed_task(OTHER, "Not an output").await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let widened = honest_commitment(&f, &[task_ref(TASK), task_ref(OTHER)]).await;
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

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("a mismatched outputs commitment must not be co-signed");
    let expected = OutputsRefusal::HashMismatch {
        committed: widened,
        recomputed: honest_commitment(&f, &[task_ref(TASK)]).await,
    };
    assert!(
        format!("{err:#}").contains(&expected.to_string()),
        "the refusal must be the hash mismatch, got: {err:#}"
    );
    assert!(
        !we_voted_on(&f, &proposal).await,
        "a refusal writes nothing"
    );
    assert_eq!(f.derived().await.state, "identified");
}

/// **Required test: a content edit after the proposal.** Bob commits to the
/// Task as it stands. The Task's title is then edited on this replica, and
/// the co-sign is refused as a hash mismatch: the id is the same, the
/// content is not. The seal cannot answer instead, because `scoped` is
/// unguarded. Control: a proposal committing to the edited content is
/// co-signed and settles.
///
/// Red if the voter hashes ids (or refs) instead of loaded content, or if
/// `load_outputs` reads something other than the current instance.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_an_output_edited_since_the_proposal() {
    let mut f = seed_unguarded_fixture().await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let as_proposed = honest_commitment(&f, &[task_ref(TASK)]).await;
    let proposal = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&as_proposed),
    )
    .await;

    f.link(
        TASK,
        "ns://title",
        &literal("Onboard Ana, renamed"),
        LinkStatus::Shared,
    )
    .await;
    let as_edited = honest_commitment(&f, &[task_ref(TASK)]).await;
    assert_ne!(as_edited, as_proposed, "precondition: the edit is content");
    assert_eq!(
        seal_for(&f, "scoped").await,
        seal,
        "precondition: the seal did not move"
    );

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("an output edited since the proposal must not be co-signed");
    let expected = OutputsRefusal::HashMismatch {
        committed: as_proposed,
        recomputed: as_edited.clone(),
    };
    assert!(
        format!("{err:#}").contains(&expected.to_string()),
        "the refusal must be the hash mismatch, got: {err:#}"
    );
    assert!(
        !we_voted_on(&f, &proposal).await,
        "a refusal writes nothing"
    );

    let current = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-2",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&as_edited),
    )
    .await;
    accept_flow_proposal(&mut f.perspective, &current, &f.ctx)
        .await
        .expect("a commitment to the current content is co-signed");
    assert_eq!(f.derived().await.state, "scoped");
}

/// Required test (c) through the production co-sign path: one named output
/// does not exist at all, and one exists but is not an instance of the
/// class it is named as. Both are refused as `OutputNotInstance`, naming
/// the output, before any hash is compared.
///
/// Red if `load_outputs` ignores the class (the Task read as a
/// `FlowInstance` would pass), or if the instance check is dropped from
/// `check_outputs_commitment` (the refusal becomes a hash mismatch).
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_a_named_output_that_is_not_an_instance_of_its_class() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let missing = task_ref("ad4m://task/never-written");
    let wrong_class = OutputRef {
        class_name: crate::perspectives::flow_classes::FLOW_INSTANCE_CLASS.to_string(),
        id: TASK.to_string(),
    };
    // Whatever these hash to, the instance check must answer first.
    let committed = honest_commitment(&f, &[task_ref(TASK)]).await;

    for (n, bad) in [missing, wrong_class].into_iter().enumerate() {
        let proposal = sync_committed_proposal_from(
            &mut f,
            &bob,
            &format!("bob-bad-{n}"),
            "identified",
            "scoped",
            &seal,
            &[task_ref(TASK), bad.clone()],
            Some(&committed),
        )
        .await;
        let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
            .await
            .expect_err("an output that is not an instance must not be co-signed");
        let expected = OutputsRefusal::OutputNotInstance { output: bad };
        assert!(
            format!("{err:#}").contains(&expected.to_string()),
            "the refusal must name the output, got: {err:#}"
        );
        assert!(
            !we_voted_on(&f, &proposal).await,
            "a refusal writes nothing"
        );
    }

    // Control: the same proposal naming only the Task is co-signed.
    let honest = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-2",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&committed),
    )
    .await;
    accept_flow_proposal(&mut f.perspective, &honest, &f.ctx)
        .await
        .expect("an honest commitment to an existing instance is co-signed");
    assert_eq!(f.derived().await.state, "scoped");
}

/// A proposal into a terminal state that commits to no outputs is refused:
/// co-signing it would complete a run no receipt can bind to anything.
///
/// Red if a missing commitment is read as the empty set.
#[tokio::test(flavor = "multi_thread")]
async fn a_co_signer_refuses_a_terminal_proposal_with_no_outputs_commitment() {
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

    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("an uncommitted terminal proposal must not be co-signed");
    assert!(
        format!("{err:#}").contains(&OutputsRefusal::Uncommitted.to_string()),
        "got: {err:#}"
    );
    assert!(!we_voted_on(&f, &proposal).await);
}

/// The proposer's side. `propose` into a terminal state writes the named
/// outputs, sorted and deduplicated, and a hash over their content, and a
/// voter reading the atom back finds a commitment it can recompute. Naming
/// something that is not an instance of its class is refused before
/// anything is written.
///
/// Red if `propose` does not pass `outputs` to the writer (the atom reads
/// `outputs_hash: None`), skips its instance check, or hashes something
/// other than what `load_outputs` reads.
#[tokio::test(flavor = "multi_thread")]
async fn propose_commits_to_the_named_outputs_and_refuses_a_missing_one() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    f.seed_task("ad4m://task/2", "Ship it").await;

    for bad in [
        task_ref("ad4m://deliverable/never-written"),
        OutputRef {
            class_name: crate::perspectives::flow_classes::FLOW_INSTANCE_CLASS.to_string(),
            id: TASK.to_string(),
        },
    ] {
        let err = propose_flow_transition(
            &mut f.perspective,
            &instance,
            "scoped",
            std::slice::from_ref(&bad),
            None,
            &f.ctx,
        )
        .await
        .expect_err("a proposer must not name an output that is not an instance");
        assert!(
            format!("{err:#}")
                .contains(&OutputsRefusal::OutputNotInstance { output: bad }.to_string()),
            "got: {err:#}"
        );
        assert!(
            f.read_set().await.proposals.is_empty(),
            "nothing is written on refusal"
        );
    }

    let named = vec![
        task_ref("ad4m://task/2"),
        task_ref(TASK),
        task_ref("ad4m://task/2"),
    ];
    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &named,
        None,
        &f.ctx,
    )
    .await
    .expect("propose");
    assert!(out.minted);
    let links = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(out.proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("links");
    let atom = TransitionAtom::from_links(&instance, &out.proposal_uri, &links).expect("an atom");
    let expected = vec![task_ref(TASK), task_ref("ad4m://task/2")];
    assert_eq!(atom.outputs, expected, "written sorted and deduplicated");
    assert_eq!(
        atom.outputs_hash,
        Some(honest_commitment(&f, &expected).await)
    );
}

/// A run does not end in a non-terminal state, so `propose` refuses to name
/// outputs there, even ones that exist, and writes nothing. The same call
/// without outputs goes through, so the refusal is the outputs rule and not
/// some other guard on the edge.
///
/// Red if `propose` drops the non-terminal branch: the call then writes a
/// proposal with no commitment and reports `minted: true`.
#[tokio::test(flavor = "multi_thread")]
async fn propose_refuses_outputs_on_a_non_terminal_state() {
    let mut f = seed_review_flow().await;
    let instance = f.instance_uri.clone();

    let err = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "changes_requested",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect_err("a non-terminal state has no outputs to name");
    assert!(
        format!("{err:#}").contains("is not terminal, so a run does not end there"),
        "the refusal must be the non-terminal outputs rule, got: {err:#}"
    );
    assert!(
        f.read_set().await.proposals.is_empty(),
        "nothing is written on refusal"
    );

    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "changes_requested",
        &[],
        None,
        &f.ctx,
    )
    .await
    .expect("the same edge without outputs is proposable");
    assert!(out.minted);
    let links = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(out.proposal_uri.clone()),
            ..Default::default()
        })
        .await
        .expect("links");
    let atom = TransitionAtom::from_links(&instance, &out.proposal_uri, &links).expect("an atom");
    assert!(atom.outputs.is_empty());
    assert_eq!(
        atom.outputs_hash, None,
        "no commitment on a non-terminal edge"
    );
}

/// An open proposal on this edge that names other outputs is neither joined
/// (that would sign outputs the caller did not name) nor twinned (the final
/// edge would carry two commitments, and no receipt for the run could
/// verify). The caller is told.
///
/// Red if `live_proposal_role` ignores the commitment: the call then joins
/// Bob's proposal and reports `minted: false`.
#[tokio::test(flavor = "multi_thread")]
async fn propose_neither_joins_nor_twins_a_proposal_naming_other_outputs() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
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

    let err = propose_flow_transition(&mut f.perspective, &instance, "scoped", &[], None, &f.ctx)
        .await
        .expect_err("different outputs must not be joined or twinned");
    assert!(
        format!("{err:#}").contains(&format!("{bobs} on this edge name different outputs")),
        "got: {err:#}"
    );
    assert_eq!(f.read_set().await.proposals.len(), 1, "no twin was minted");
    assert!(!we_voted_on(&f, &bobs).await, "and Bob's was not signed");

    // Control: naming the same outputs joins Bob's proposal.
    let joined = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("join");
    assert_eq!(joined.proposal_uri, bobs);
    assert!(!joined.minted && joined.recorded_vote);
}

/// The "different outputs" refusal above is reserved for a proposal a voter
/// COULD sign. Bob's terminal proposal here commits to a hash its named
/// outputs do not produce, so `accept_flow_proposal` refuses it and
/// `reject_flow_proposal` only retracts the caller's own links — telling the
/// caller to "co-sign one or reject it" would let one peer block the manual
/// path into the terminal state for everyone. An honest propose mints its
/// own proposal instead, does not sign Bob's, and the run completes once a
/// second voter co-signs it (Bob's vote does not pool with ours: its
/// commitment is a different group, #1108/#1118).
///
/// Red if `live_proposal_role` compares commitments without first validating
/// the live atom the way a co-signer would.
#[tokio::test(flavor = "multi_thread")]
async fn propose_mints_past_a_foreign_terminal_proposal_with_a_bad_commitment() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    // Bob names the Task but signs a hash the Task's content does not produce.
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref(TASK)],
        Some(&outputs_hash(&[])),
    )
    .await;

    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("a proposal no voter could sign must not block an honest propose");
    assert!(out.minted, "minted our own, not joined or refused: {out:?}");
    assert_ne!(out.proposal_uri, bobs);
    assert!(
        !we_voted_on(&f, &bobs).await,
        "the invalid proposal was not co-signed"
    );
    // Since #1108/#1118 a terminal edge pools votes per commitment, so Bob's
    // proposer vote on a commitment nobody can sign does not count toward
    // ours: one vote, short of `{n: 2}`. It used to pool — which is what let
    // an early foreign proposal land in the settled edge and make the run
    // unreceiptable.
    assert_eq!(out.derived_state, "identified");
    // The run is not wedged: a second voter co-signing our proposal settles
    // the edge.
    let carol = TestSigner::generate();
    sync_vote_from(&mut f, &carol, &out.proposal_uri).await;
    assert_eq!(f.derived().await.state, "scoped");
}

/// As above, with the other invalid shape: Bob names an output that does not
/// load on this replica (never written), under a commitment that matches
/// nothing. No voter could sign it, so it must not block; the honest propose
/// mints and, once co-signed, the run completes.
///
/// Red if `live_proposal_role` reads the commitment mismatch before asking
/// whether the proposal is signable at all.
#[tokio::test(flavor = "multi_thread")]
async fn propose_mints_past_a_foreign_terminal_proposal_whose_output_does_not_load() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "identified",
        "scoped",
        &seal,
        &[task_ref("ad4m://task/never-written")],
        Some(&outputs_hash(&[])),
    )
    .await;

    let out = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "scoped",
        &[task_ref(TASK)],
        None,
        &f.ctx,
    )
    .await
    .expect("an output that does not load must not block an honest propose");
    assert!(out.minted, "minted our own, not joined or refused: {out:?}");
    assert!(
        !we_voted_on(&f, &bobs).await,
        "the unloadable proposal was not co-signed"
    );
    // Bob's vote sits in its own commitment group (#1108/#1118): ours alone
    // is short of `{n: 2}` until a second voter co-signs it.
    assert_eq!(out.derived_state, "identified");
    let carol = TestSigner::generate();
    sync_vote_from(&mut f, &carol, &out.proposal_uri).await;
    assert_eq!(f.derived().await.state, "scoped");
}

/// Outputs are a terminal-edge concern: a co-signer ignores them anywhere
/// else (`check_outputs_commitment` is a no-op off the final edge). So a
/// non-terminal proposal carrying a stray `outputs_hash` stays joinable —
/// comparing commitments there would refuse to join a proposal `accept`
/// happily signs, since our own `committed` is always `None` off the final
/// edge.
///
/// Red if `live_proposal_role` compares commitments on a non-terminal
/// target.
#[tokio::test(flavor = "multi_thread")]
async fn propose_joins_a_non_terminal_proposal_carrying_a_stray_outputs_hash() {
    let mut f = seed_review_flow().await;
    set_consensus_rule(&mut f, "review://Review.changes_requested", r#"{"n":2}"#).await;
    let instance = f.instance_uri.clone();
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "changes_requested").await;
    let bobs = sync_committed_proposal_from(
        &mut f,
        &bob,
        "bob-1",
        "review",
        "changes_requested",
        &seal,
        &[],
        Some(&outputs_hash(&[])),
    )
    .await;

    let joined = propose_flow_transition(
        &mut f.perspective,
        &instance,
        "changes_requested",
        &[],
        None,
        &f.ctx,
    )
    .await
    .expect("a stray outputs_hash on a non-terminal proposal must not stop a join");
    assert_eq!(joined.proposal_uri, bobs, "joined Bob's, not a twin");
    assert!(!joined.minted && joined.recorded_vote);
    assert_eq!(
        joined.derived_state, "changes_requested",
        "the join fires the {{n:2}} edge"
    );
}
