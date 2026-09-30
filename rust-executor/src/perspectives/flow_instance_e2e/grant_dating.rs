use super::*;
// ---------------------------------------------------------------------------
// Grant dating (#1063): where a role grant's window starts
// ---------------------------------------------------------------------------
//
// Every test here casts the only vote BEFORE the genuine grant, then adds
// material that would date the grant earlier than that vote. The edge must
// stay unsettled: the vote was cast outside the role, and nothing but a
// verified link from someone the rule accepts may say otherwise.

/// Well before any vote a test casts: what a back-dating writer picks.
fn long_ago() -> chrono::DateTime<chrono::Utc> {
    chrono::Utc::now() - chrono::Duration::hours(1)
}

/// A link signed by `signer`'s own key and stamped `at`, delivered as sync
/// would deliver it. The signature verifies: whoever wrote it, wrote it.
async fn sync_signed_at(
    f: &mut Fixture,
    signer: &TestSigner,
    source: &str,
    predicate: &str,
    target: &str,
    at: chrono::DateTime<chrono::Utc>,
) {
    let signed = signer.sign_at(
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
        .normalize(),
        at,
    );
    f.perspective
        .add_link_expression(LinkExpression::from(signed), LinkStatus::Shared, None)
        .await
        .expect("sync a signed link");
}

/// A link stamped `at` that claims `author` over a signature that does not
/// verify, delivered as sync would deliver it.
async fn sync_forged_at(
    f: &mut Fixture,
    author: &str,
    source: &str,
    predicate: &str,
    target: &str,
    at: chrono::DateTime<chrono::Utc>,
) {
    f.perspective
        .add_link_expression(
            LinkExpression {
                author: author.to_string(),
                timestamp: at.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
                data: Link {
                    source: source.to_string(),
                    predicate: Some(predicate.to_string()),
                    target: target.to_string(),
                },
                proof: crate::types::ExpressionProof {
                    key: format!("{author}#key"),
                    signature: "not-a-signature".to_string(),
                },
                status: Some(LinkStatus::Shared),
            },
            LinkStatus::Shared,
            None,
        )
        .await
        .expect("sync a forged link");
}

fn rule(from_role: serde_json::Value) -> String {
    serde_json::json!({ "n": 1, "fromRole": from_role }).to_string()
}

/// Mallory back-dates a grant she is not allowed to make.
///
/// The rule: only `admin` grants the `owner` role. This replica's agent
/// votes, and admin grants it the role afterwards, so the vote was cast
/// outside the role. Mallory then syncs her own, genuinely signed `owner`
/// link naming the voter, stamped an hour before the vote.
///
/// Red on the base: the grant was dated from the earliest `owner` link
/// naming the DID from ANY author, so Mallory's link opened the window an
/// hour early and the vote settled the edge.
#[tokio::test(flavor = "multi_thread")]
async fn a_non_granters_back_dated_grant_link_does_not_move_the_edge() {
    let admin = TestSigner::generate();
    let mallory = TestSigner::generate();
    let mut f = seed_satisfied_fixture(None).await;
    let me = acting_did(&f);
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "didProperty": "owner",
            "where": { "author": admin.did },
        })),
    )
    .await;
    f.mint_one().await;
    tick().await;
    sync_signed_at(
        &mut f,
        &admin,
        TASK,
        "ns://owner",
        &literal(&me),
        chrono::Utc::now(),
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "precondition: admin's grant postdates the vote, so the vote does not count"
    );

    sync_signed_at(
        &mut f,
        &mallory,
        TASK,
        "ns://owner",
        &literal(&me),
        long_ago(),
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "Mallory may not grant this role, so her back-dated link must not date the grant"
    );
}

/// The same back-dating with a forged signature: a link that claims to be
/// admin's and is not.
///
/// Red on the base: grant links were not signature-checked, so the forgery
/// dated the grant an hour before the vote.
#[tokio::test(flavor = "multi_thread")]
async fn a_forged_back_dated_grant_link_does_not_move_the_edge() {
    let admin = TestSigner::generate();
    let mut f = seed_satisfied_fixture(None).await;
    let me = acting_did(&f);
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", OWNER_RULE).await;
    f.mint_one().await;
    tick().await;
    grant_owner_role(&mut f).await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "precondition: the grant postdates the vote"
    );

    sync_forged_at(
        &mut f,
        &admin.did,
        TASK,
        "ns://owner",
        &literal(&me),
        long_ago(),
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "a grant link whose signature does not verify is not a link anyone wrote"
    );
}

/// `author: "$did"` makes the grantee's own links the grant. A role instance
/// someone else created an hour before the vote is not the grantee's grant
/// until the grantee writes on it, and that write comes after the vote here.
///
/// Red on the base: such a role was dated from the instance's earliest link,
/// whoever wrote it, so admin's creation opened the window before the vote.
#[tokio::test(flavor = "multi_thread")]
async fn a_self_granted_role_is_dated_by_the_grantees_own_link() {
    const ROLE_TASK: &str = "ad4m://task/role";
    let admin = TestSigner::generate();
    let mut f = seed_satisfied_fixture(None).await;
    // Admin creates the role instance, an hour before anything else happens.
    sync_signed_at(
        &mut f,
        &admin,
        ROLE_TASK,
        "ns://type",
        "ns://task",
        long_ago(),
    )
    .await;
    sync_signed_at(
        &mut f,
        &admin,
        ROLE_TASK,
        "ns://title",
        &literal("Reviewer seat"),
        long_ago(),
    )
    .await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "where": { "author": "$did", "owner": "reviewer" },
        })),
    )
    .await;
    f.mint_one().await;
    tick().await;
    // The grantee takes the seat after voting.
    f.link(
        ROLE_TASK,
        "ns://owner",
        &literal("reviewer"),
        LinkStatus::Shared,
    )
    .await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "the grantee's own link postdates the vote; admin's earlier links are not the grant"
    );
}

/// A role rule that names the candidate somewhere no link can date: `$did`
/// sits in an `in` list, not as a field's value, and there is no
/// `author: "$did"`. Such a rule grants nothing (fail closed).
///
/// Red on the base: the grant fell back to the instance's own timestamp,
/// which predates the vote, so the edge settled.
#[tokio::test(flavor = "multi_thread")]
async fn a_rule_with_no_datable_did_grants_nothing() {
    let mut f = seed_satisfied_fixture(None).await;
    grant_owner_role(&mut f).await;
    tick().await;
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "where": { "owner": { "in": ["$did", "did:key:nobody"] } },
        })),
    )
    .await;
    f.mint_one().await;
    assert_eq!(
        f.derived().await.state,
        "identified",
        "no link can date a grant under this rule, so it grants nothing"
    );
}

/// More back-dated links from a non-granter than collection carries must
/// not push the genuine grant out: collection drops what cannot date the
/// grant before it caps what it keeps.
///
/// Red if collection caps before it filters: the cap keeps Mallory's
/// earliest links, the reader drops them all, and the genuine grant made
/// before the vote is lost.
#[tokio::test(flavor = "multi_thread")]
async fn links_that_cannot_date_a_grant_do_not_evict_one_that_can() {
    use crate::perspectives::flow_evaluator::MAX_GRANT_LINKS;
    let admin = TestSigner::generate();
    let mallory = TestSigner::generate();
    let mut f = seed_satisfied_fixture(None).await;
    let me = acting_did(&f);
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "didProperty": "owner",
            "where": { "author": admin.did },
        })),
    )
    .await;
    for i in 0..=MAX_GRANT_LINKS {
        sync_signed_at(
            &mut f,
            &mallory,
            TASK,
            "ns://owner",
            &literal(&me),
            long_ago() - chrono::Duration::minutes(i as i64),
        )
        .await;
    }
    sync_signed_at(
        &mut f,
        &admin,
        TASK,
        "ns://owner",
        &literal(&me),
        chrono::Utc::now(),
    )
    .await;
    tick().await;
    f.mint_one().await;
    assert_eq!(
        f.derived().await.state,
        "scoped",
        "admin granted the role before the vote, however many links Mallory wrote"
    );
}

/// The control for the tests above: the same kinds of role, granted by the
/// right author before the vote, do settle the edge. Without it, a gate that
/// admitted nobody would pass every test in this file.
#[tokio::test(flavor = "multi_thread")]
async fn a_grant_made_before_the_vote_by_an_accepted_author_settles_the_edge() {
    let admin = TestSigner::generate();

    // Admin-granted `didProperty` role, with Mallory's later link beside it.
    let mallory = TestSigner::generate();
    let mut f = seed_satisfied_fixture(None).await;
    let me = acting_did(&f);
    set_consensus_rule(
        &mut f,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "didProperty": "owner",
            "where": { "author": admin.did },
        })),
    )
    .await;
    sync_signed_at(
        &mut f,
        &admin,
        TASK,
        "ns://owner",
        &literal(&me),
        long_ago(),
    )
    .await;
    sync_signed_at(
        &mut f,
        &mallory,
        TASK,
        "ns://owner",
        &literal(&me),
        chrono::Utc::now(),
    )
    .await;
    tick().await;
    f.mint_one().await;
    assert_eq!(f.derived().await.state, "scoped", "admin-granted role");

    // Self-granted role: the grantee wrote on the instance before voting.
    let mut g = seed_satisfied_fixture(None).await;
    g.link(TASK, "ns://owner", &literal("reviewer"), LinkStatus::Shared)
        .await;
    set_consensus_rule(
        &mut g,
        "delivery://Delivery.scoped",
        &rule(serde_json::json!({
            "className": "ns://Task",
            "where": { "author": "$did", "owner": "reviewer" },
        })),
    )
    .await;
    tick().await;
    g.mint_one().await;
    assert_eq!(g.derived().await.state, "scoped", "self-granted role");
}
