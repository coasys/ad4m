//! Monotonic flow state (#1176): under the flow vocabulary a Shared link is
//! never removed by a diff, only ended by its author's signed tombstone.
//!
//! - T1: a removal arriving from the link language is dropped.
//! - T2: every local write path refuses a Shared removal, before anything is
//!   persisted or committed; a Local link in the same namespace still goes.
//! - T3: `ad4m://flow/retracted` ends the author's own link on every replica,
//!   in either arrival order, and cannot be forged or undone by a replay.
//!
//! A child of `perspective_instance` so the `commit_batch` backstop can queue
//! a removal past the refusing entry points.

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::types::{ExpressionProof, ExpressionProofInput, LinkExpressionInput, LinkInput};

const PROPOSAL: &str = "ad4m://flow/proposal/p1";
const ACCEPTED_BY: &str = "ad4m://acceptedBy";
const TO_STATE: &str = "ad4m://flow/to_state";
const CURRENT_STATE: &str = "ad4m://flow/current_state";
const RETRACTED: &str = "ad4m://flow/retracted";
const PLAIN: &str = "test://likes";

async fn fixture() -> (PerspectiveInstance, AgentContext) {
    let (perspective, _, ctx) = setup_perspective_no_llm(&[]).await;
    (perspective, ctx)
}

fn signed(signer: &TestSigner, source: &str, predicate: &str, target: &str) -> LinkExpression {
    let mut link = LinkExpression::from(
        signer.sign(
            Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            }
            .normalize(),
        ),
    );
    link.status = Some(LinkStatus::Shared);
    link
}

/// `link`'s author ending it: the retraction names the link's signature.
fn retraction_of(signer: &TestSigner, link: &LinkExpression) -> LinkExpression {
    let target = Literal::from_string(link.proof.signature.clone())
        .to_url()
        .expect("encode signature");
    signed(signer, &link.data.source, RETRACTED, &target)
}

async fn sync_in(
    p: &PerspectiveInstance,
    additions: Vec<LinkExpression>,
    removals: Vec<LinkExpression>,
) {
    p.diff_from_link_language(PerspectiveDiff {
        additions,
        removals,
    })
    .await
    .expect("diff_from_link_language");
}

fn present(p: &PerspectiveInstance, link: &LinkExpression) -> bool {
    p.sparql_store
        .get_link(
            &link.data.source,
            link.data.predicate.as_deref(),
            &link.data.target,
            &link.author,
            &link.timestamp,
        )
        .expect("get_link")
        .is_some()
}

/// A Shared link this replica's agent signed, as `add_link` writes it.
async fn own_shared(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    predicate: &str,
    target: &str,
) -> LinkExpression {
    LinkExpression::from(
        p.add_link(
            Link {
                source: PROPOSAL.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            LinkStatus::Shared,
            None,
            ctx,
        )
        .await
        .expect("add_link"),
    )
}

fn assert_monotonic_refusal<T: std::fmt::Debug>(result: Result<T, AnyError>, path: &str) {
    let err = result.expect_err(&format!("{path} must refuse a Shared monotonic removal"));
    assert!(
        format!("{err:#}").contains("is monotonic"),
        "{path}: unexpected error {err:#}"
    );
}

fn as_input(link: &LinkExpression) -> LinkExpressionInput {
    LinkExpressionInput {
        author: link.author.clone(),
        data: LinkInput {
            source: link.data.source.clone(),
            predicate: link.data.predicate.clone(),
            target: link.data.target.clone(),
        },
        proof: ExpressionProofInput {
            key: Some(link.proof.key.clone()),
            signature: Some(link.proof.signature.clone()),
            valid: None,
            invalid: None,
        },
        timestamp: link.timestamp.clone(),
        status: link.status.clone(),
    }
}

// ---------------------------------------------------------------------------
// T1: remote removals under monotonic predicates are dropped on ingest
// ---------------------------------------------------------------------------

/// Alice's vote and proposal field arrive, then a diff removes them next to an
/// ordinary link. The ordinary link goes, so the removal path works and only
/// the predicate check keeps the other two.
#[tokio::test(flavor = "multi_thread")]
async fn t1_remote_removal_of_a_monotonic_link_is_dropped() {
    let (p, _) = fixture().await;
    let alice = TestSigner::generate();
    let vote = signed(&alice, PROPOSAL, ACCEPTED_BY, &alice.did);
    let field = signed(&alice, PROPOSAL, TO_STATE, "literal:string:done");
    let plain = signed(&alice, PROPOSAL, PLAIN, "test://bob");
    sync_in(&p, vec![vote.clone(), field.clone(), plain.clone()], vec![]).await;
    assert!(present(&p, &vote) && present(&p, &field) && present(&p, &plain));

    sync_in(&p, vec![], vec![vote.clone(), field.clone(), plain.clone()]).await;

    assert!(
        present(&p, &vote),
        "an acceptedBy link is not removed by a diff"
    );
    assert!(
        present(&p, &field),
        "an ad4m://flow/* link is not removed by a diff"
    );
    assert!(!present(&p, &plain), "control: an ordinary link is removed");
}

// ---------------------------------------------------------------------------
// T2: local Shared removals are refused on every write path
// ---------------------------------------------------------------------------

#[tokio::test(flavor = "multi_thread")]
async fn t2_remove_link_refuses_a_shared_monotonic_link() {
    let (mut p, ctx) = fixture().await;
    let vote = own_shared(&mut p, &ctx, ACCEPTED_BY, "did:key:me").await;

    assert_monotonic_refusal(p.remove_link(vote.clone(), None).await, "remove_link");
    let batch = p.create_batch().await;
    assert_monotonic_refusal(
        p.remove_link(vote.clone(), Some(batch.clone())).await,
        "remove_link (batch)",
    );
    p.commit_batch(batch, &ctx)
        .await
        .expect("commit the empty batch");

    assert!(present(&p, &vote), "the vote is still on the graph");
}

#[tokio::test(flavor = "multi_thread")]
async fn t2_remove_links_refuses_and_removes_nothing() {
    let (mut p, ctx) = fixture().await;
    let field = own_shared(&mut p, &ctx, TO_STATE, "literal:string:done").await;
    let plain = own_shared(&mut p, &ctx, PLAIN, "test://bob").await;

    assert_monotonic_refusal(
        p.remove_links(vec![plain.clone(), field.clone()], None)
            .await,
        "remove_links",
    );
    let batch = p.create_batch().await;
    assert_monotonic_refusal(
        p.remove_links(vec![plain.clone(), field.clone()], Some(batch.clone()))
            .await,
        "remove_links (batch)",
    );
    p.commit_batch(batch, &ctx)
        .await
        .expect("commit the empty batch");

    assert!(present(&p, &field), "the flow link stays");
    assert!(
        present(&p, &plain),
        "all or nothing: the ordinary link in the same call stays too"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t2_update_link_refuses_to_replace_a_shared_monotonic_link() {
    let (mut p, ctx) = fixture().await;
    let field = own_shared(&mut p, &ctx, TO_STATE, "literal:string:done").await;
    let replacement = Link {
        source: PROPOSAL.to_string(),
        predicate: Some(TO_STATE.to_string()),
        target: "literal:string:other".to_string(),
    };

    assert_monotonic_refusal(
        p.update_link(field.clone(), replacement.clone(), None, &ctx)
            .await,
        "update_link",
    );
    let batch = p.create_batch().await;
    assert_monotonic_refusal(
        p.update_link(field.clone(), replacement, Some(batch.clone()), &ctx)
            .await,
        "update_link (batch)",
    );
    p.commit_batch(batch, &ctx)
        .await
        .expect("commit the empty batch");

    assert!(present(&p, &field));
    let targets: Vec<String> = p
        .sparql_store
        .query_links(Some(PROPOSAL), Some(TO_STATE), None, None, None, None)
        .unwrap()
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    assert_eq!(
        targets,
        vec!["literal:string:done".to_string()],
        "no replacement written"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t2_link_mutations_refuses_and_writes_nothing() {
    let (mut p, ctx) = fixture().await;
    let vote = own_shared(&mut p, &ctx, ACCEPTED_BY, "did:key:me").await;
    let mutations = LinkMutations {
        additions: vec![LinkInput {
            source: PROPOSAL.to_string(),
            predicate: Some(PLAIN.to_string()),
            target: "test://carol".to_string(),
        }],
        removals: vec![as_input(&vote)],
    };

    assert_monotonic_refusal(
        p.link_mutations(mutations, LinkStatus::Shared, &ctx).await,
        "link_mutations",
    );

    assert!(present(&p, &vote));
    assert!(
        p.sparql_store
            .query_links(Some(PROPOSAL), Some(PLAIN), None, None, None, None)
            .unwrap()
            .is_empty(),
        "the addition in the refused call is not written either"
    );
}

/// The backstop: a removal that reached a batch without passing a refusing
/// entry point is still refused at commit, and nothing in that batch lands.
#[tokio::test(flavor = "multi_thread")]
async fn t2_commit_batch_refuses_a_queued_shared_monotonic_removal() {
    let (mut p, ctx) = fixture().await;
    let vote = own_shared(&mut p, &ctx, ACCEPTED_BY, "did:key:me").await;
    let batch = p.create_batch().await;
    p.add_link(
        Link {
            source: PROPOSAL.to_string(),
            predicate: Some(PLAIN.to_string()),
            target: "test://carol".to_string(),
        },
        LinkStatus::Shared,
        Some(batch.clone()),
        &ctx,
    )
    .await
    .expect("queue an addition");
    p.batch_store
        .write()
        .await
        .get_mut(&batch)
        .expect("batch")
        .diff
        .removals
        .push(vote.clone());

    assert_monotonic_refusal(p.commit_batch(batch, &ctx).await, "commit_batch");

    assert!(present(&p, &vote));
    assert!(p
        .sparql_store
        .query_links(Some(PROPOSAL), Some(PLAIN), None, None, None, None)
        .unwrap()
        .is_empty());
}

/// The rule is about Shared links: the engine's own Local cache in the same
/// namespace is replaced by removal, as `flow_classes` does on every pass.
#[tokio::test(flavor = "multi_thread")]
async fn t2_a_local_flow_link_can_still_be_removed() {
    let (mut p, ctx) = fixture().await;
    let cache = LinkExpression::from(
        p.add_link(
            Link {
                source: PROPOSAL.to_string(),
                predicate: Some(CURRENT_STATE.to_string()),
                target: "literal:string:done".to_string(),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link"),
    );

    p.remove_link(cache.clone(), None)
        .await
        .expect("a Local flow link is removable");

    assert!(!present(&p, &cache));
}

// ---------------------------------------------------------------------------
// T3: the author's signed retraction ends the link
// ---------------------------------------------------------------------------

#[tokio::test(flavor = "multi_thread")]
async fn t3_the_authors_retraction_removes_her_link() {
    let (p, _) = fixture().await;
    let alice = TestSigner::generate();
    let vote = signed(&alice, PROPOSAL, ACCEPTED_BY, &alice.did);
    sync_in(&p, vec![vote.clone()], vec![]).await;

    let retraction = retraction_of(&alice, &vote);
    sync_in(&p, vec![retraction.clone()], vec![]).await;

    assert!(!present(&p, &vote), "Alice's retraction ends her vote");
    assert!(present(&p, &retraction), "the tombstone itself stays");
}

#[tokio::test(flavor = "multi_thread")]
async fn t3_a_retraction_by_anyone_else_or_unsigned_removes_nothing() {
    let (p, _) = fixture().await;
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let vote = signed(&alice, PROPOSAL, ACCEPTED_BY, &alice.did);
    sync_in(&p, vec![vote.clone()], vec![]).await;

    // Bob names Alice's signature, signed with his own key.
    let by_bob = retraction_of(&bob, &vote);
    // Alice's name on it, but a signature that does not verify.
    let mut forged = retraction_of(&alice, &vote);
    forged.proof = ExpressionProof {
        key: forged.proof.key.clone(),
        signature: "00".repeat(64),
    };
    sync_in(&p, vec![by_bob, forged], vec![]).await;

    assert!(
        present(&p, &vote),
        "only the link's author ends it, with a valid signature"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t3_replaying_a_retracted_link_does_not_bring_it_back() {
    let (p, _) = fixture().await;
    let alice = TestSigner::generate();
    let vote = signed(&alice, PROPOSAL, ACCEPTED_BY, &alice.did);
    sync_in(&p, vec![vote.clone()], vec![]).await;
    sync_in(&p, vec![retraction_of(&alice, &vote)], vec![]).await;
    assert!(!present(&p, &vote));

    sync_in(&p, vec![vote.clone()], vec![]).await;

    assert!(
        !present(&p, &vote),
        "the stored retraction covers the replayed addition"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t3_a_retraction_that_arrives_first_still_ends_the_link() {
    let (p, _) = fixture().await;
    let alice = TestSigner::generate();
    let vote = signed(&alice, PROPOSAL, ACCEPTED_BY, &alice.did);

    sync_in(&p, vec![retraction_of(&alice, &vote)], vec![]).await;
    sync_in(&p, vec![vote.clone()], vec![]).await;
    assert!(!present(&p, &vote), "retraction first, link second");

    let other = signed(&alice, PROPOSAL, TO_STATE, "literal:string:done");
    sync_in(
        &p,
        vec![retraction_of(&alice, &other), other.clone()],
        vec![],
    )
    .await;
    assert!(!present(&p, &other), "both in one diff");
}
