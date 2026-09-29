use super::*;
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

/// Register `ns://Task` again with its `owner` property declared
/// `monotonic`, as the class author (this agent, the owner) would.
async fn declare_owner_monotonic(f: &mut Fixture) {
    declare_monotonic(f, &["ns://owner"]).await;
}

/// Register `ns://Task` again with the properties under `paths` declared
/// `monotonic`.
async fn declare_monotonic(f: &mut Fixture, paths: &[&str]) {
    use crate::perspectives::interpretation_test_support::TASK_SDNA;
    let shacl: serde_json::Value = serde_json::from_str(TASK_SDNA).expect("TASK_SDNA");
    register_task_class(f, shacl, paths).await;
}

/// Register `shacl` as `ns://Task` with the properties under `paths`
/// declared `monotonic`.
async fn register_task_class(f: &mut Fixture, mut shacl: serde_json::Value, paths: &[&str]) {
    use crate::perspectives::perspective_instance::SdnaType;
    for property in shacl["properties"].as_array_mut().expect("properties") {
        if paths.iter().any(|p| property["path"] == *p) {
            property["monotonic"] = serde_json::Value::Bool(true);
        }
    }
    f.perspective
        .add_sdna(
            "ns://Task".to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(shacl.to_string()),
            &f.ctx,
        )
        .await
        .expect("add_sdna");
}

async fn owner_grants(f: &Fixture) -> Vec<LinkExpression> {
    f.perspective
        .get_links(&LinkQuery {
            source: Some(TASK.to_string()),
            predicate: Some("ns://owner".to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(LinkExpression::from)
        .collect()
}

/// T4 (#1176). A role class that declares its DID property `monotonic`: the
/// Shared grant survives a peer's removal and a local one, and a generic
/// `ad4m://flow/retracted` tombstone from its own author does not end it
/// either, so later votes still count. Only `role_grant_revoked` ends it, as
/// of its timestamp. (Red without the flag: the peer's removal applies and
/// the vote in the middle settles nothing.)
#[tokio::test(flavor = "multi_thread")]
async fn a_declared_role_grant_ends_only_by_revocation() {
    use crate::perspectives::monotonic::retraction_for;

    let mut f = seed_owner_gated_review_flow(OWNER_RULE).await;
    declare_owner_monotonic(&mut f).await;
    let me = acting_did(&f);
    f.link(TASK, "ns://owner", &literal(&me), LinkStatus::Shared)
        .await;
    let grant = owner_grants(&f).await.pop().expect("the grant");

    f.perspective
        .diff_from_link_language(PerspectiveDiff {
            additions: vec![],
            removals: vec![grant.clone()],
        })
        .await
        .expect("sync a peer's removal");
    assert_eq!(
        owner_grants(&f).await,
        vec![grant.clone()],
        "peer removal dropped"
    );

    let err = f
        .perspective
        .remove_link(grant.clone(), None)
        .await
        .expect_err("a local removal of a declared grant is refused");
    assert!(format!("{err:#}").contains("is monotonic"), "{err:#}");

    let tombstone = retraction_for(&grant).expect("retraction_for");
    f.link(
        &tombstone.source,
        tombstone.predicate.as_deref().expect("predicate"),
        &tombstone.target,
        LinkStatus::Shared,
    )
    .await;
    assert_eq!(
        owner_grants(&f).await,
        vec![grant.clone()],
        "a generic retraction does not end a declared grant"
    );

    tick().await;
    settle(&mut f, "p1", "review", "changes_requested").await;

    tick().await;
    revoke_own_role(&mut f, TASK).await;
    tick().await;
    propose(&mut f, "p2", "changes_requested", "review").await;
    assert!(
        consensus_pass(&mut f).await.is_empty(),
        "the revocation ends the grant for votes after it"
    );
    assert_eq!(f.derived().await.state, "changes_requested");
    assert_eq!(
        owner_grants(&f).await,
        vec![grant],
        "and the grant stays in the graph"
    );
}

/// Seed the owner-gated flow with `TASK` granted to this agent and tagged
/// `ns://domain` "design", register `ns://Task` with an optional `domain`
/// property and `declared` monotonic, let a peer remove every `removed` link
/// of `TASK`, and report whether the `role` query still matches `TASK`. The
/// grant link itself stays in every case.
async fn a_peers_removal_keeps_the_role_instance(
    declared: &[&str],
    role: &str,
    removed: &str,
) -> bool {
    use crate::perspectives::flow_instance::roles::{resolve_role_grants, RoleGrantEvidence};
    use crate::perspectives::interpretation_test_support::TASK_SDNA;
    use crate::perspectives::shacl_parser::ModelQuery;

    let mut f = seed_owner_gated_review_flow(OWNER_RULE).await;
    let mut shacl: serde_json::Value = serde_json::from_str(TASK_SDNA).expect("TASK_SDNA");
    shacl["properties"]
        .as_array_mut()
        .expect("properties")
        .push(serde_json::json!({
            "path": "ns://domain", "name": "domain", "min_count": 0, "max_count": 1,
            "resolve_language": "literal",
            "setter": [{"action": "setSingleTarget", "source": "this",
                        "predicate": "ns://domain", "target": "value"}]
        }));
    register_task_class(&mut f, shacl, declared).await;
    let me = acting_did(&f);
    f.link(TASK, "ns://owner", &literal(&me), LinkStatus::Shared)
        .await;
    f.link(TASK, "ns://domain", &literal("design"), LinkStatus::Shared)
        .await;
    let role: ModelQuery = serde_json::from_str(role).expect("role");
    let record = f.instances().await.remove(0);
    let matched =
        |ev: &Vec<RoleGrantEvidence>| ev[0].instances.iter().any(|i| i.instance_id == TASK);
    let resolve = || {
        resolve_role_grants(
            &f.perspective,
            "delivery://Delivery.scoped",
            &role,
            &record,
            std::slice::from_ref(&me),
        )
    };
    assert!(
        matched(&resolve().await.expect("resolve")),
        "control: the instance is a role instance"
    );

    let removals: Vec<LinkExpression> = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(TASK.to_string()),
            predicate: Some(removed.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| {
            let mut l = LinkExpression::from(l);
            l.status = None;
            l
        })
        .collect();
    assert_eq!(removals.len(), 1, "one {removed} link to remove");
    f.perspective
        .diff_from_link_language(PerspectiveDiff {
            additions: vec![],
            removals,
        })
        .await
        .expect("sync a peer's removal");
    assert_eq!(owner_grants(&f).await.len(), 1, "the grant link stays");
    let after = resolve().await.expect("resolve");
    matched(&after)
}

/// T4, the instance side door (review of #1183, finding 1). A `fromRole`
/// gate matches a role *instance*, so a peer who removes the instance's class
/// flag un-grants the role even though the DID link stays. The role class
/// must declare its flag (and its other required properties) too; then the
/// peer's removal of the flag is dropped and the grant still resolves. The
/// control shows the side door with only the DID property declared.
#[tokio::test(flavor = "multi_thread")]
async fn a_declared_role_instance_keeps_its_flag_and_its_grant() {
    const ROLE: &str = r#"{"className":"ns://Task","didProperty":"owner"}"#;
    assert!(
        a_peers_removal_keeps_the_role_instance(
            &["ns://owner", "ns://type", "ns://title"],
            ROLE,
            "ns://type"
        )
        .await,
        "the flag is declared: the peer's removal is dropped, the grant resolves"
    );
    assert!(
        !a_peers_removal_keeps_the_role_instance(&["ns://owner"], ROLE, "ns://type").await,
        "control: only the DID declared, the flag goes and the instance drops out"
    );
}

/// T4, the `where` side door (review of #1183, finding 1). A role query that
/// filters on `domain` stops matching once a peer removes the instance's
/// `domain` link, unless the role class declares `domain` monotonic too.
#[tokio::test(flavor = "multi_thread")]
async fn a_declared_role_instance_keeps_its_where_field_and_its_grant() {
    const ROLE: &str =
        r#"{"className":"ns://Task","didProperty":"owner","where":{"domain":"design"}}"#;
    let required = ["ns://owner", "ns://type", "ns://title"];
    assert!(
        a_peers_removal_keeps_the_role_instance(
            &[&required[..], &["ns://domain"]].concat(),
            ROLE,
            "ns://domain"
        )
        .await,
        "the where field is declared: the peer's removal is dropped, the grant resolves"
    );
    assert!(
        !a_peers_removal_keeps_the_role_instance(&required, ROLE, "ns://domain").await,
        "control: the where field undeclared, it goes and the instance drops out"
    );
}

/// Register `ns://Task` with every property declared plus a `holder` DID
/// property on `ns://task_holder`, a predicate no other class writes. Give
/// `TASK` to this agent under `did_path`. Seed `NOTE`, a non-role node
/// (`ns://type ns://note`) whose `ns://owner` the admin (this agent) wrote for
/// a stranger, as an assignment on another class that shares `ns://owner`
/// would, and on which the stranger wrote `did_path -> themselves`. Then a
/// peer removes the `removed` link of the Task's `type` property shape (the
/// flag). Returns whether `role` matches the stranger on `NOTE` before and
/// after the removal. The admin stays matched on `TASK` throughout. `ADMIN`
/// in `role` stands for this agent's DID, which the fixture only mints.
async fn a_widened_role_matches_a_stranger(
    did_path: &str,
    role: &str,
    removed: &str,
) -> (bool, bool) {
    use crate::agent::signatures::TestSigner;
    use crate::perspectives::flow_instance::roles::resolve_role_grants;
    use crate::perspectives::interpretation_test_support::TASK_SDNA;
    use crate::perspectives::shacl_parser::ModelQuery;
    const NOTE: &str = "ad4m://note/1";

    let mut f = seed_owner_gated_review_flow(OWNER_RULE).await;
    let mut shacl: serde_json::Value = serde_json::from_str(TASK_SDNA).expect("TASK_SDNA");
    shacl["properties"]
        .as_array_mut()
        .expect("properties")
        .push(serde_json::json!({
            "path": "ns://task_holder", "name": "holder", "min_count": 0, "max_count": 1,
            "resolve_language": "literal",
            "setter": [{"action": "setSingleTarget", "source": "this",
                        "predicate": "ns://task_holder", "target": "value"}]
        }));
    register_task_class(
        &mut f,
        shacl,
        &["ns://owner", "ns://task_holder", "ns://type", "ns://title"],
    )
    .await;
    let me = acting_did(&f);
    let stranger = TestSigner::generate();
    f.link(TASK, did_path, &literal(&me), LinkStatus::Shared)
        .await;
    f.link(NOTE, "ns://type", "ns://note", LinkStatus::Shared)
        .await;
    f.link(NOTE, "ns://title", &literal("a note"), LinkStatus::Shared)
        .await;
    f.link(
        NOTE,
        "ns://owner",
        &literal(&stranger.did),
        LinkStatus::Shared,
    )
    .await;
    let own_claim = stranger.sign(
        Link {
            source: NOTE.to_string(),
            predicate: Some(did_path.to_string()),
            target: literal(&stranger.did),
        }
        .normalize(),
    );
    f.perspective
        .add_link_expression(LinkExpression::from(own_claim), LinkStatus::Shared, None)
        .await
        .expect("sync the stranger's own claim");

    let role: ModelQuery = serde_json::from_str(&role.replace("ADMIN", &me)).expect("role");
    let record = f.instances().await.remove(0);
    let matches = |who: &str, id: &str| {
        let (who, id) = (who.to_string(), id.to_string());
        let (f, role, record) = (&f, &role, &record);
        async move {
            resolve_role_grants(
                &f.perspective,
                "delivery://Delivery.scoped",
                role,
                record,
                std::slice::from_ref(&who),
            )
            .await
            .expect("resolve")[0]
                .instances
                .iter()
                .any(|i| i.instance_id == id)
        }
    };
    assert!(
        matches(&me, TASK).await,
        "control: the task is a role instance"
    );
    let before = matches(&stranger.did, NOTE).await;

    let type_shape = f
        .perspective
        .get_links(&LinkQuery {
            predicate: Some("sh://path".to_string()),
            target: Some("ns://type".to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| l.data.source)
        .find(|s| s.contains("Task"))
        .expect("the type property shape");
    let removals: Vec<LinkExpression> = f
        .perspective
        .get_links(&LinkQuery {
            source: Some(type_shape),
            predicate: Some(removed.to_string()),
            ..Default::default()
        })
        .await
        .expect("get_links")
        .into_iter()
        .map(|l| {
            let mut l = LinkExpression::from(l);
            l.status = None;
            l
        })
        .collect();
    // One per registration of the class: the peer removes them all.
    assert!(!removals.is_empty(), "a {removed} link to remove");
    f.perspective
        .diff_from_link_language(PerspectiveDiff {
            additions: vec![],
            removals,
        })
        .await
        .expect("sync a peer's removal");
    assert!(matches(&me, TASK).await, "the task stays a role instance");
    (before, matches(&stranger.did, NOTE).await)
}

/// The class shape side door, widening half (review of #1183, round 3;
/// tracked in #1195). The role class's own SHACL links are ordinary Shared
/// links. A peer who removes the flag's `sh://hasValue` or `sh://minCount`
/// makes every node that carries the role's predicates a role instance, so
/// the admin's `ns://owner` on a note grants the role to the stranger it
/// names, even under an admin-only rule. This pins the hole as it is today.
#[tokio::test(flavor = "multi_thread")]
async fn removing_the_flags_conformance_link_widens_the_role() {
    const ROLE: &str = r#"{"className":"ns://Task","didProperty":"owner"}"#;
    for removed in ["sh://hasValue", "sh://minCount"] {
        assert_eq!(
            a_widened_role_matches_a_stranger("ns://owner", ROLE, removed).await,
            (false, true),
            "{removed}: the note becomes a role instance"
        );
    }
}

/// The same with an admin-only rule: the admin wrote the note's `ns://owner`.
#[tokio::test(flavor = "multi_thread")]
async fn removing_the_flags_conformance_link_widens_an_admin_only_role() {
    const ROLE: &str =
        r#"{"className":"ns://Task","didProperty":"owner","where":{"author":"ADMIN"}}"#;
    for removed in ["sh://hasValue", "sh://minCount"] {
        assert_eq!(
            a_widened_role_matches_a_stranger("ns://owner", ROLE, removed).await,
            (false, true),
            "{removed}: the admin's owner link on the note grants the role"
        );
    }
}

/// The mitigation the docs give until #1195 lands: a DID property on a
/// predicate no other class writes, and an author condition. After either
/// removal the note still counts as an instance, but the only `holder` link
/// on it is the stranger's own, which the admin-only rule does not accept.
/// The control shows the author condition is needed: without it, the
/// stranger's own claim matches.
#[tokio::test(flavor = "multi_thread")]
async fn a_did_predicate_of_its_own_and_an_author_condition_keep_a_widened_role_closed() {
    const ADMIN_ONLY: &str =
        r#"{"className":"ns://Task","didProperty":"holder","where":{"author":"ADMIN"}}"#;
    for removed in ["sh://hasValue", "sh://minCount"] {
        assert_eq!(
            a_widened_role_matches_a_stranger("ns://task_holder", ADMIN_ONLY, removed).await,
            (false, false),
            "{removed}: the stranger is not matched"
        );
    }
    assert_eq!(
        a_widened_role_matches_a_stranger(
            "ns://task_holder",
            r#"{"className":"ns://Task","didProperty":"holder"}"#,
            "sh://hasValue"
        )
        .await,
        (false, true),
        "control: without the author condition the stranger's own claim matches"
    );
}
