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
    // travel. `asserted_instance_timestamp` is the *fallback* for instances
    // that carry no assignment link, and accepting it here would let the
    // assertion pass on a read-set where nothing travels at all — which is
    // exactly the hole #1065's first review found: `grant_links` was empty for
    // every `didProperty` role because the store query used the property
    // *name* where the graph holds the RDF *predicate*. No disjunction.
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

/// The proposal model's `acceptedBy` lists exactly the votes the fold counts,
/// and the voters it documents, `{ proposer } ∪ acceptedBy`, are the fold's.
/// A vote is an authorship claim with a valid signature, counted once per DID,
/// so each of these is on the graph and must not show up twice or at all:
/// a vote for Carol that Bob signed, a vote whose signature fails, Bob voting
/// twice, and the proposer accepting their own proposal. `resolvedAs` reads
/// only this replica's own mark, never a peer's Shared one.
#[tokio::test(flavor = "multi_thread")]
async fn the_proposal_model_lists_only_the_votes_the_fold_counts() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":4}"#).await;
    let minted = f.mint_one().await;
    let proposer = acting_did(&f);

    let sync_signed =
        |link: Link, signer: &TestSigner, at: Option<chrono::DateTime<chrono::Utc>>| {
            let expr = match at {
                Some(at) => signer.sign_at(link.normalize(), at),
                None => signer.sign(link.normalize()),
            };
            LinkExpression::from(expr)
        };
    let vote = |did: &str| Link {
        source: minted.clone(),
        predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
        target: did.to_string(),
    };

    let bob = TestSigner::generate();
    let carol = TestSigner::generate();
    let eve = TestSigner::generate();
    sync_vote_from(&mut f, &bob, &minted).await;
    let later = chrono::Utc::now() + chrono::Duration::seconds(1);
    let mut synced = vec![
        // Bob again, a second later: a second device, or a re-sync.
        sync_signed(vote(&bob.did), &bob, Some(later)),
        // Bob names Carol: a valid signature, but not Carol's.
        sync_signed(vote(&carol.did), &bob, None),
        // A peer's Shared `resolved_as`: says nothing about what happened here.
        sync_signed(
            Link {
                source: minted.clone(),
                predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                target: literal(FIRED_MARK),
            },
            &bob,
            None,
        ),
    ];
    // Eve names herself, but the signature does not cover what is stored.
    let mut tampered = eve.sign(vote(&eve.did).normalize());
    tampered.timestamp = (chrono::Utc::now() + chrono::Duration::seconds(5)).to_rfc3339();
    synced.push(LinkExpression::from(tampered));
    for link in synced {
        f.perspective
            .add_link_expression(link, LinkStatus::Shared, None)
            .await
            .expect("sync a link in");
    }
    // The proposer accepts their own proposal as well.
    let ctx = f.ctx.clone();
    accept_flow_proposal(&mut f.perspective, &minted, &ctx)
        .await
        .expect("the proposer may accept too");

    let proposal = |raw: String| -> serde_json::Value {
        let result: serde_json::Value = serde_json::from_str(&raw).expect("model_query JSON");
        result["instances"]
            .as_array()
            .expect("instances")
            .iter()
            .find(|i| i["id"].as_str() == Some(minted.as_str()))
            .cloned()
            .expect("the minted proposal is listed")
    };
    let accepted = |p: &serde_json::Value| -> Vec<String> {
        p["acceptedBy"]
            .as_array()
            .map(|a| {
                a.iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };

    let p = proposal(
        f.perspective
            .model_query("FlowTransitionProposal", "{}")
            .await
            .expect("query proposals"),
    );
    let mut listed = accepted(&p);
    listed.sort();
    let mut expected = vec![bob.did.clone(), proposer.clone()];
    expected.sort();
    assert_eq!(
        listed, expected,
        "Bob once and the proposer; not Carol, not Eve"
    );

    // Held to the fold, not to a list written here.
    let atom = f
        .read_set()
        .await
        .atoms()
        .into_iter()
        .find(|a| a.uri == minted)
        .expect("the proposal is an atom");
    let counted: std::collections::BTreeSet<String> =
        atom.votes.iter().map(|v| v.did.clone()).collect();
    let documented: std::collections::BTreeSet<String> = std::iter::once(proposer.clone())
        .chain(accepted(&p))
        .collect();
    assert_eq!(
        documented, counted,
        "{{ proposer }} ∪ acceptedBy is the fold's voter set"
    );
    assert!(
        p["resolvedAs"].is_null(),
        "a peer's Shared mark is not this replica's"
    );

    for _ in 0..2 {
        let signer = TestSigner::generate();
        sync_vote_from(&mut f, &signer, &minted).await;
    }
    consensus_pass(&mut f).await;
    let p = proposal(
        f.perspective
            .model_query("FlowTransitionProposal", "{}")
            .await
            .expect("query proposals"),
    );
    assert_eq!(
        p["resolvedAs"].as_str(),
        Some("fired"),
        "the pass marked the edge it fired"
    );
}
