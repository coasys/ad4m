use super::*;
// ---------------------------------------------------------------------------
// Quorum, accept, and the read-set
// ---------------------------------------------------------------------------

/// Test 11. The only test that walks accept → pass → fold end to end with a
/// real second key. Another agent's proposal syncs in carrying their vote;
/// this replica's own agent co-signs it through `accept_flow_proposal`, which
/// is the second distinct voter, and the edge settles.
#[tokio::test(flavor = "multi_thread")]
async fn n2_second_signer_accept_settles_and_replays() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;

    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "Bob's own vote alone is 1 < n = 2"
    );
    assert_eq!(f.derived().await.state, "identified");

    let fired = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect("accept must land this replica's vote and sweep");
    assert_eq!(fired.len(), 1, "n = 2 met must settle: {fired:?}");
    assert!(fired[0].voters.contains(&bob.did));
    assert!(fired[0].voters.contains(&acting_did(&f)));

    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");
    assert_eq!(derived.settled.len(), 1);
    let mut expected = vec![acting_did(&f), bob.did.clone()];
    expected.sort();
    assert_eq!(
        derived.settled[0].voters, expected,
        "the fold re-counts the votes"
    );
    assert_eq!(
        f.links_by_predicate(&proposal)
            .await
            .get(ACCEPTED_BY_PREDICATE)
            .map(Vec::len),
        Some(1),
        "one accept, one vote link"
    );

    // Lock the camelCase wire shape so the TS `FlowFireOutcome` interface
    // can never drift from what `serde_json::to_value(fired)` produces.
    let wire = serde_json::to_value(&fired).expect("serialize fired outcomes");
    let obj = wire[0]
        .as_object()
        .expect("outcome must serialize as object");
    assert_eq!(obj.len(), 5, "unexpected field count on the wire: {obj:?}");
    assert_eq!(wire[0]["instanceUri"], fired[0].instance_uri.as_str());
    assert_eq!(wire[0]["fromState"], fired[0].from_state.as_str());
    assert_eq!(wire[0]["toState"], fired[0].to_state.as_str());
    assert_eq!(wire[0]["voters"].as_array().map(Vec::len), Some(2));
    assert!(wire[0]["contributingProposalUris"]
        .as_array()
        .is_some_and(|a| a.contains(&serde_json::Value::String(proposal.clone()))));

    // Voting again on an edge that has already settled is refused as stale:
    // the proposal leaves `identified` and the instance is in `scoped`.
    let err = accept_flow_proposal(&mut f.perspective, &proposal, &f.ctx)
        .await
        .expect_err("a settled edge must not accept more votes");
    assert!(format!("{err:#}").contains("stale"), "got {err:#}");
}

/// The read-set travels: serialise everything the engine read, fold the JSON
/// back on a machine with no perspective, and reach the same verdict. This is
/// what a minted Synergy token would carry as its backing — signed links for
/// the proposals and votes, and signed links for the role history too, from
/// which the reader recomputes the windows instead of trusting ours.
#[tokio::test(flavor = "multi_thread")]
async fn a_serialised_read_set_re_derives_the_same_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    f.mint_one().await;
    let derived = f.derived().await;
    assert_eq!(derived.state, "scoped");

    let read_set = f.read_set().await;
    let records = f.instances().await;
    assert_eq!(
        read_set.subject, records[0].subject,
        "the run's base expression travels: without it a reader cannot substitute `$flow.base` \
         and would apply a different authority rule than we did"
    );
    let evidence = read_set
        .role_grants
        .iter()
        .find(|g| g.did == acting_did(&f))
        .unwrap_or_else(|| panic!("the voter's role evidence belongs in the proof: {read_set:?}"));
    // The reshape's whole point: what travels is raw material, not a
    // `granted_at` the minter computed. The rule carries
    // `didProperty: "owner"` and the fixture writes the assignment link
    // `TASK --ns://owner--> literal(did)`, so the assignment link itself must
    // travel: it is the only thing that can date the grant (#1063). An empty
    // `grant_links` is exactly the hole #1065's first review found: it was
    // empty for every `didProperty` role because the store query used the
    // property *name* where the graph holds the RDF *predicate*.
    assert!(
        !evidence.instances.is_empty(),
        "the voter's role query must have matched at least one instance: {evidence:?}"
    );
    assert!(
        evidence.instances.iter().all(|i| !i.grant_links.is_empty()),
        "every role instance must carry the assignment links the reader dates the grant from: \
         {evidence:?}"
    );
    // And the carried links must be real links — author, timestamp and
    // signature material present — not default-filled shells that happen to
    // satisfy the type. `.all()` over an empty iterator is vacuously true, so
    // this assert only means anything because of the one above.
    assert!(
        evidence
            .instances
            .iter()
            .flat_map(|i| i.grant_links.iter().chain(i.revocation_links.iter()))
            .all(|l| !l.author.is_empty()
                && !l.timestamp.is_empty()
                && !l.proof.signature.is_empty()),
        "the links themselves travel, author and signature intact: {evidence:?}"
    );

    let json = serde_json::to_string(&read_set).expect("a read-set serialises");
    let parsed: ReadSet = serde_json::from_str(&json).expect("and deserialises");
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    let flow = &flows[&f.flow_uri];
    assert_eq!(
        fold_read_set(flow, &parsed).expect("the carried evidence resolves off-perspective"),
        derived,
        "an off-perspective verifier must reach the same verdict"
    );

    // The reader's own translation input must match the one this replica used
    // — the same role query has to mean the same thing on both sides, or the
    // authority rule diverges silently.
    assert_eq!(
        parsed.as_record(flow),
        FlowInstance::from_record(&records[0], flow).as_record(),
        "the record rebuilt from carried fields must equal the live one"
    );
}
