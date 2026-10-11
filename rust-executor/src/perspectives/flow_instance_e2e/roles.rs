use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::shacl_parser::{parse_flow_to_links, CONSENSUS_RULE_PREDICATE};
use crate::types::{LinkInput, LinkMutations};
// ---------------------------------------------------------------------------
// Roles
// ---------------------------------------------------------------------------

/// Test 12. A vote from outside the rule's `fromRole` counts for nothing, and
/// a grant written *after* that vote does not retroactively enfranchise it —
/// eligibility is as-of each vote's own timestamp, the same rule the
/// revocation tests pin from the other side. A vote cast once the grant is
/// already in place settles the edge.
///
/// The middle assertion used to read `scoped`, and passed only because of the
/// bug #1065's review found: the grant-link query used the `didProperty`
/// *name* where the graph holds the RDF predicate, so `grant_links` came back
/// empty for every `didProperty` role and `granted_at` fell back to the
/// instance's own (much earlier) timestamp. Under that fallback every grant
/// looked retroactive. The assertion was a mirror of the defect, not a
/// contract.
#[tokio::test(flavor = "multi_thread")]
async fn a_non_role_member_vote_does_not_count() {
    const OWNER_RULE_HERE: &str =
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;

    let mut f = seed_satisfied_fixture(None).await;
    // Eligible = "there is a Task this DID owns". The seeded task has no
    // owner, so nobody is in the role yet.
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE_HERE).await;
    f.mint_one().await;

    assert_eq!(
        f.derived().await.state,
        "identified",
        "a proposer outside the role vouches for nothing"
    );
    assert!(consensus_pass(&mut f).await.is_empty());

    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "the grant postdates the vote, so it cannot reach back and make it count"
    );

    // Same rule, same single vote — but cast while the grant is already live.
    let mut g = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut g, "delivery://Delivery.scoped", OWNER_RULE_HERE).await;
    g.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&g)),
        LinkStatus::Local,
    )
    .await;
    g.mint_one().await;
    assert_eq!(
        g.derived().await.state,
        "scoped",
        "inside the role at the time of the vote, the same vote settles the edge"
    );
}

/// Test 20. Tombstone revocation: a role revocation is an explicit signed link
/// (`ad4m://flow/role_grant_revoked`), never a deletion. Because eligibility is
/// gated as-of each vote's own timestamp, a tombstone written AFTER a vote
/// cannot un-settle the edge that vote produced. The grant instance stays in the
/// graph, newcomers read the same history, and replicas always converge.
#[tokio::test(flavor = "multi_thread")]
async fn revoking_a_role_after_settlement_does_not_unsettle_the_edge() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    // Grant the role; the vote timestamp will be strictly later (wall-clock).
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "edge settles while voter holds the role"
    );

    // Revoke via tombstone (timestamp > vote timestamp — sequential write).
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // A newcomer deriving from scratch must reach the same state.
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "tombstone revocation does not un-settle history: the vote pre-dates the revocation"
    );
}

/// Test 21. Newcomer convergence: a replica that first derives AFTER a
/// tombstone is written reaches the same settled state as one that derived
/// before. Both are represented by sequential `derived()` calls — the fold
/// always re-derives from scratch so there is no separate newcomer code path.
#[tokio::test(flavor = "multi_thread")]
async fn newcomer_replica_converges_to_same_state_after_revocation() {
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
    consensus_pass(&mut f).await;
    let state_before = f.derived().await.state.clone();
    assert_eq!(state_before, "scoped");

    // Tombstone the role after settlement.
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // Re-derive from scratch: this is what a newcomer does.
    assert_eq!(
        f.derived().await.state,
        state_before,
        "newcomer convergence: revocation does not rewrite settled history"
    );
}

/// Test 22. A vote cast AFTER a tombstone revocation does not count. The
/// revocation only gates votes whose `at` timestamp follows the tombstone's.
#[tokio::test(flavor = "multi_thread")]
async fn late_syncing_revocation_stops_counting_votes_that_arrive_after_it() {
    let mut f = seed_satisfied_fixture(None).await;
    // n=2: need two distinct eligible voters to settle.
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        r#"{"n":2,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#,
    )
    .await;
    let bob = TestSigner::generate();

    // Grant Alice's role and write her proposal (1 vote, need 2 to settle).
    f.link(
        TASK,
        "ns://owner",
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;
    let seal = seal_for(&f, "scoped").await;
    // write_proposal takes a nonce; the content-addressed URI comes back.
    let proposal_uri = f
        .write_proposal(
            "revoke-timing",
            "identified",
            "scoped",
            &[TASK.to_string()],
            &seal,
        )
        .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "1 < n=2, not settled"
    );

    // Revoke Alice's role (tombstone, after her vote — strictly later wall-clock).
    f.link(
        TASK,
        ROLE_GRANT_REVOKED_PREDICATE,
        &literal(&acting_did(&f)),
        LinkStatus::Local,
    )
    .await;

    // Add Bob to the role (grant timestamp > revocation) and have Bob vote.
    f.link(TASK, "ns://owner", &literal(&bob.did), LinkStatus::Local)
        .await;
    let bob_vote = bob.sign(
        Link {
            source: proposal_uri.clone(),
            predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
            target: bob.did.clone(),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(bob_vote), LinkStatus::Shared, None)
        .await
        .expect("sync Bob's vote");

    // Alice's vote: T_alice < T_revoke → still counts (pre-revocation).
    // Bob's vote: T_bob > T_bob_grant > T_revoke, Bob's grant never revoked → counts.
    // Two eligible votes → n=2 settled.
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "Alice's pre-revocation vote + Bob's post-grant vote reach n=2"
    );
}

/// Gate every state of the review flow on "owns the task", so each edge of a
/// multi-hop history is a role-gated vote.
async fn seed_owner_gated_review_flow(rule: &str) -> Fixture {
    let mut f = seed_review_flow().await;
    for state in ["review", "changes_requested", "approved"] {
        set_consensus_rule(&mut f, &format!("review://Review.{state}"), rule).await;
    }
    f
}

/// A peer's tombstone revoking `revoked` on `role_instance`, delivered as sync would
/// deliver it: signed by the peer's real key.
async fn sync_revocation_from(
    f: &mut Fixture,
    signer: &TestSigner,
    role_instance: &str,
    revoked: &str,
) {
    let tombstone = signer.sign(
        Link {
            source: role_instance.to_string(),
            predicate: Some(ROLE_GRANT_REVOKED_PREDICATE.to_string()),
            target: literal(revoked),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(tombstone), LinkStatus::Shared, None)
        .await
        .expect("sync a peer's revocation");
}

/// A revocation gates only votes cast after it. The edge settled while the
/// agent held the role stays settled — full DerivedState unchanged, and the
/// pass reports nothing new — and a vote the same agent casts afterwards on a
/// gated edge settles nothing. (Fails on the live-evaluation fold: there the
/// post-revocation vote still counts.)
#[tokio::test(flavor = "multi_thread")]
async fn a_revocation_gates_later_votes_and_leaves_settled_history_alone() {
    let mut f = seed_owner_gated_review_flow(OWNER_RULE).await;
    grant_owner_role(&mut f).await;
    tick().await;
    settle(&mut f, "p1", "review", "changes_requested").await;
    let before = f.derived().await;
    assert_eq!(before.state, "changes_requested");

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    assert_eq!(
        f.derived().await,
        before,
        "a revocation must not un-settle history"
    );
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "nothing new settled"
    );

    tick().await;
    propose(&mut f, "p2", "changes_requested", "review").await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "a vote cast after the revocation settles nothing"
    );
    let after = f.derived().await;
    assert_eq!(
        after.state, "changes_requested",
        "post-revocation vote ignored: {after:?}"
    );
    assert_eq!(walked(&after), walked(&before));
}

/// A replica deriving for the first time after the revocation — every
/// derivation here is from scratch — reaches exactly the pre-revocation
/// state, and so does an off-perspective verifier folding the serialised
/// read-set, which now records the revocation it took into account.
#[tokio::test(flavor = "multi_thread")]
async fn a_newcomer_deriving_after_a_revocation_converges_on_the_settled_state() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    grant_owner_role(&mut f).await;
    tick().await;
    f.mint_one().await;
    consensus_pass(&mut f).await;
    let before = f.derived().await;
    assert_eq!(before.state, "scoped");

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    assert_eq!(
        f.derived().await,
        before,
        "derived from scratch after == before"
    );
    let read_set = f.read_set().await;
    let json = serde_json::to_string(&read_set).expect("serialises");
    let parsed: ReadSet = serde_json::from_str(&json).expect("deserialises");
    let flows = load_shacl_flows(&f.perspective).await.expect("flows");
    assert_eq!(
        fold_read_set(&flows[&f.flow_uri], &parsed).expect("the carried evidence resolves"),
        before
    );
    assert!(
        read_set.role_grants.iter().any(|g| g.did == acting_did(&f)
            && g.instances
                .iter()
                .any(|i| i.revocation_links.iter().any(|l| l.compute_proof_valid()))),
        "the read-set carries the tombstone link the verdict took into account — \
         its signature verifying from the carried material alone (the plain form \
         has no verdict flag to read), unfiltered by authority, so the reader \
         applies that rule itself: {read_set:?}"
    );
}

/// A revocation carries the grant's own authority rule. With the role pinned
/// to instances this agent authored (`where.author`), a peer's tombstone on
/// the instance is not a revocation — the grant stays live for later votes — while
/// this agent's own tombstone ends it.
#[tokio::test(flavor = "multi_thread")]
async fn a_revocation_from_outside_the_grants_authority_is_ignored() {
    let mut f = seed_review_flow().await;
    let me = acting_did(&f);
    let admin_only = format!(
        r#"{{"n":1,"fromRole":{{"className":"ns://Task","didProperty":"owner","where":{{"author":"{me}"}}}}}}"#
    );
    for state in ["review", "changes_requested", "approved"] {
        set_consensus_rule(&mut f, &format!("review://Review.{state}"), &admin_only).await;
    }
    grant_owner_role(&mut f).await;
    tick().await;
    settle(&mut f, "p1", "review", "changes_requested").await;

    tick().await;
    let mallory = TestSigner::generate();
    sync_revocation_from(&mut f, &mallory, TASK, &me).await;
    tick().await;
    settle(&mut f, "p2", "changes_requested", "review").await;
    assert_eq!(
        f.derived().await.state,
        "review",
        "an outsider's tombstone is not a revocation"
    );

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    tick().await;
    propose(&mut f, "p3", "review", "approved").await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "the admin's own tombstone ends the grant"
    );
    assert_eq!(f.derived().await.state, "review");
}

/// #1144, the role gates under test. `grantedByFlow` is the key #1076 renamed
/// to `producedByFlow`. Dropped, it leaves the plain `owner` gate.
const PLAIN_GATE: &str = r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;
const RENAMED_KEY_GATE: &str = r#"{"n":1,"fromRole":{"className":"ns://Task","didProperty":"owner","grantedByFlow":{"flow":"ns://GrantFlow","state":"Granted"}}}"#;
const DUPLICATE_KEY_GATE: &str =
    r#"{"n":1,"n":2,"fromRole":{"className":"ns://Task","didProperty":"owner"}}"#;

/// Deliver a state's `consensusRule` as sync does: a peer's signed link,
/// through `diff_from_link_language`. The local write paths refuse a rule the
/// reader could not read before storing it (#1144, the test below); sync
/// stores what the peer wrote, so this is how such a rule reaches a graph.
async fn sync_consensus_rule(f: &mut Fixture, state_uri: &str, rule: &str) {
    let peer = TestSigner::generate();
    let link = peer.sign(
        Link {
            source: state_uri.to_string(),
            predicate: Some(CONSENSUS_RULE_PREDICATE.to_string()),
            target: literal(rule),
        }
        .normalize(),
    );
    f.perspective
        .diff_from_link_language(PerspectiveDiff::from_additions(vec![LinkExpression::from(
            link,
        )]))
        .await
        .expect("sync a peer's consensusRule");
    // A rule is part of the definition, so this queued a sweep. Wait it out,
    // or it can record an edge before the test's own pass does.
    f.perspective.settle_flow_passes().await;
}

/// #1144, the read side. A role gate carrying a key the reader does not know
/// is refused, not read as the gate without that key. The rule arrives by
/// sync, past the write-time check: a definition a peer wrote, or one stored
/// before the check existed, must still fail closed.
///
/// Dropped, `grantedByFlow` leaves the plain `owner` gate, which the voter
/// below passes, so the edge settled on a vote the author meant to require a
/// flow receipt for. The first run is the positive control: the same fixture,
/// grant and vote under the rule without the stale key settle the edge, so the
/// refusal is the key's.
///
/// Killing mutation: drop the `role_gate_key_errors` check from
/// `decode_consensus_rule`. The rule then decodes as the plain `owner` gate and
/// the second run settles to `scoped`.
#[tokio::test(flavor = "multi_thread")]
async fn a_role_gate_with_an_unknown_key_refuses_the_edge() {
    for (rule, expected, why) in [
        (
            PLAIN_GATE,
            "scoped",
            "control: the voter holds the plain role, so the edge settles",
        ),
        (
            RENAMED_KEY_GATE,
            "identified",
            "an unknown key must refuse the edge, not fall back to the plain role",
        ),
    ] {
        let mut f = seed_satisfied_fixture(None).await;
        sync_consensus_rule(&mut f, "delivery://Delivery.scoped", rule).await;
        f.link(
            TASK,
            "ns://owner",
            &literal(&acting_did(&f)),
            LinkStatus::Local,
        )
        .await;
        f.mint_one().await;
        consensus_pass(&mut f).await;
        assert_eq!(f.derived().await.state, expected, "{why}");
    }
}

/// #1144, the write side. A flow definition whose `consensusRule` the reader
/// would refuse (an unknown key, or a key written twice) is refused by the
/// write paths `addFlow` (`link_mutations`) and `addSdna` (`add_links`) use,
/// with an error naming the state and the key, and nothing of it is stored.
/// The same definition with the rule spelt right adds through the same path.
///
/// The definition is built as `SHACLFlow.toLinks` / `parse_flow_to_links`
/// emit it, with the state's rule literal swapped for the one under test:
/// `parse_flow_to_links` decodes through serde, which would already drop the
/// unknown key.
///
/// Killing mutation: drop the `check_consensus_rule_links` call from
/// `link_mutations` or `add_links`. The definition is stored and no error
/// comes.
#[tokio::test(flavor = "multi_thread")]
async fn a_flow_definition_with_an_unreadable_role_gate_is_refused_before_it_is_stored() {
    const NAMESPACE: &str = "gated://";
    const STATE_URI: &str = "gated://Delivery.scoped";
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("ns://Task", TASK_SDNA)]).await;

    let definition = |rule: &str| -> Vec<Link> {
        let valid: serde_json::Value = serde_json::from_str(PLAIN_GATE).expect("valid rule");
        let flow = serde_json::json!({
            "name": "Delivery",
            "namespace": NAMESPACE,
            "states": [
                { "name": "identified", "value": 0.0 },
                { "name": "scoped", "value": 1.0, "consensusRule": valid }
            ],
            "transitions": [
                { "action_name": "Scope", "from_state": "identified", "to_state": "scoped", "actions": [] }
            ],
        });
        parse_flow_to_links(&flow.to_string(), "Delivery")
            .expect("parse_flow_to_links")
            .into_iter()
            .map(|mut link| {
                if link.predicate.as_deref() == Some(CONSENSUS_RULE_PREDICATE) {
                    assert_eq!(link.source, STATE_URI, "the rule sits on the state");
                    link.target = literal(rule);
                }
                link
            })
            .collect()
    };
    let as_mutations = |links: &[Link]| LinkMutations {
        additions: links
            .iter()
            .map(|l| LinkInput {
                source: l.source.clone(),
                predicate: l.predicate.clone(),
                target: l.target.clone(),
            })
            .collect(),
        removals: vec![],
    };
    async fn stored(perspective: &PerspectiveInstance, namespace: &str) -> usize {
        perspective
            .get_links(&LinkQuery::default())
            .await
            .expect("get_links")
            .into_iter()
            .filter(|l| l.data.source.starts_with(namespace))
            .count()
    }

    for (what, rule, names) in [
        (
            "an unknown key",
            RENAMED_KEY_GATE,
            "unknown key `fromRole.grantedByFlow` (renamed to `producedByFlow` in #1076)",
        ),
        ("a duplicate key", DUPLICATE_KEY_GATE, "duplicate key `n`"),
    ] {
        let links = definition(rule);
        let via_add_flow = perspective
            .link_mutations(as_mutations(&links), LinkStatus::Local, &ctx)
            .await
            .map(|_| ());
        let via_add_sdna = perspective
            .add_links(links.clone(), LinkStatus::Local, None, &ctx)
            .await
            .map(|_| ());
        for (path, result) in [
            ("link_mutations", via_add_flow),
            ("add_links", via_add_sdna),
        ] {
            let err = result
                .err()
                .unwrap_or_else(|| panic!("{path} stored a definition with {what}"))
                .to_string();
            assert!(
                err.contains(STATE_URI) && err.contains(names),
                "{path}, {what}: the error names the state and the key; got: {err}"
            );
        }
        assert_eq!(
            stored(&perspective, NAMESPACE).await,
            0,
            "{what}: nothing of the definition is stored"
        );
    }

    let links = definition(PLAIN_GATE);
    perspective
        .link_mutations(as_mutations(&links), LinkStatus::Local, &ctx)
        .await
        .expect("a definition whose rule reads adds");
    assert_eq!(stored(&perspective, NAMESPACE).await, links.len());
}
