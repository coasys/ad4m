//! SHACL-declared monotonic predicates (#1176 PR B), T5: a property shape
//! flagged `monotonic` makes its predicate end only by tombstone, like the
//! fixed core, and only when the perspective's authority wrote the flag.
//!
//! The flag is `<propShape> --ad4m://monotonic--> literal:string:<predicate>`,
//! so the predicate is read from the flag alone. Removing `sh://path` does
//! not undo it, and a flag from anyone but the authority, even one naming the
//! authority's own predicate, declares nothing.
//!
//! T4 (role grants read back through the flow engine) is
//! `flow_instance_e2e/roles.rs::a_declared_role_grant_ends_only_by_revocation`.

use super::monotonic_tests::as_input;
use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
use crate::types::DecoratedNeighbourhoodExpression;

const ROLE: &str = "app://role/1";
const BADGE: &str = "app://badge/1";
const MEMBER: &str = "app://member";
const HOLDER: &str = "app://holder";
const FLAG: &str = "ad4m://monotonic";

/// A class with one property under `predicate`, flagged or not.
fn class_json(class: &str, predicate: &str, monotonic: bool) -> String {
    serde_json::json!({
        "target_class": format!("app://{class}"),
        "properties": [{
            "path": predicate,
            "name": "did",
            "min_count": 0,
            "monotonic": monotonic,
        }],
    })
    .to_string()
}

fn signed(signer: &TestSigner, link: Link) -> LinkExpression {
    let mut link = LinkExpression::from(signer.sign(link.normalize()));
    link.status = Some(LinkStatus::Shared);
    link
}

/// `class`'s SHACL links as `signer` publishes them.
fn class_links(
    signer: &TestSigner,
    class: &str,
    predicate: &str,
    monotonic: bool,
) -> Vec<LinkExpression> {
    parse_shacl_to_links(&class_json(class, predicate, monotonic), class)
        .expect("shacl")
        .into_iter()
        .map(|l| signed(signer, l))
        .collect()
}

fn with_predicate<'a>(links: &'a [LinkExpression], predicate: &str) -> &'a LinkExpression {
    links
        .iter()
        .find(|l| l.data.predicate.as_deref() == Some(predicate))
        .unwrap_or_else(|| panic!("a {predicate} link"))
}

fn grant(signer: &TestSigner, instance: &str, predicate: &str) -> LinkExpression {
    signed(
        signer,
        Link {
            source: instance.to_string(),
            predicate: Some(predicate.to_string()),
            target: signer.did.clone(),
        },
    )
}

/// A joined neighbourhood authored by `author`.
async fn neighbourhood_of(author: &TestSigner) -> PerspectiveInstance {
    let (p, _, _) = setup_perspective_no_llm(&[]).await;
    p.persisted.lock().await.neighbourhood = Some(DecoratedNeighbourhoodExpression {
        author: author.did.clone(),
        ..Default::default()
    });
    p
}

async fn sync_in(
    p: &mut PerspectiveInstance,
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

async fn register(p: &mut PerspectiveInstance, ctx: &AgentContext, shacl: serde_json::Value) {
    p.add_sdna(
        "Role".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(shacl.to_string()),
        ctx,
    )
    .await
    .expect("add_sdna");
}

fn links_under(p: &PerspectiveInstance, predicate: &str) -> Vec<DecoratedLinkExpression> {
    p.sparql_store
        .query_links(None, Some(predicate), None, None, None, None)
        .expect("query_links")
}

#[tokio::test(flavor = "multi_thread")]
async fn the_parser_emits_the_flag_with_the_predicate_as_target() {
    let links = parse_shacl_to_links(&class_json("Role", MEMBER, true), "Role").expect("shacl");
    let flags: Vec<_> = links
        .iter()
        .filter(|l| l.predicate.as_deref() == Some(FLAG))
        .collect();
    assert_eq!(flags.len(), 1, "{links:?}");
    assert_eq!(flags[0].target, "literal:string:app%3A%2F%2Fmember");

    let unflagged =
        parse_shacl_to_links(&class_json("Role", MEMBER, false), "Role").expect("shacl");
    assert!(
        unflagged
            .iter()
            .all(|l| l.predicate.as_deref() != Some(FLAG)),
        "`monotonic: false` declares nothing"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_only_the_neighbourhood_authors_flag_counts() {
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let mut p = neighbourhood_of(&alice).await;

    // Alice (the author) declares app://member; Bob declares app://holder.
    sync_in(&mut p, class_links(&alice, "Role", MEMBER, true), vec![]).await;
    sync_in(&mut p, class_links(&bob, "Badge", HOLDER, true), vec![]).await;
    let member = grant(&alice, ROLE, MEMBER);
    let holder = grant(&alice, BADGE, HOLDER);
    sync_in(&mut p, vec![member.clone(), holder.clone()], vec![]).await;

    sync_in(&mut p, vec![], vec![member.clone(), holder.clone()]).await;
    assert!(
        present(&p, &member),
        "the author's flag: a peer's removal is dropped"
    );
    assert!(
        !present(&p, &holder),
        "control: Bob's flag declares nothing, so the removal applies"
    );

    let err = p
        .remove_link(member.clone(), None)
        .await
        .expect_err("a local removal of a declared link is refused");
    assert!(format!("{err:#}").contains("is monotonic"), "{err:#}");
    assert!(present(&p, &member));
}

/// (i) Bob writes a flag on Alice's own property shape, naming Alice's
/// predicate. It is not hers, so it declares nothing.
#[tokio::test(flavor = "multi_thread")]
async fn t5_a_members_flag_naming_the_authors_predicate_has_no_effect() {
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let mut p = neighbourhood_of(&alice).await;

    let class = class_links(&alice, "Role", MEMBER, false);
    let prop_shape = with_predicate(&class, "sh://path").data.source.clone();
    sync_in(&mut p, class, vec![]).await;
    let bobs_flag = signed(
        &bob,
        Link {
            source: prop_shape,
            predicate: Some(FLAG.to_string()),
            target: Literal::from_string(MEMBER.to_string())
                .to_url()
                .expect("literal"),
        },
    );
    let member = grant(&alice, ROLE, MEMBER);
    sync_in(&mut p, vec![bobs_flag.clone(), member.clone()], vec![]).await;

    sync_in(&mut p, vec![], vec![member.clone()]).await;
    assert!(present(&p, &bobs_flag), "Bob's flag is stored");
    assert!(!present(&p, &member), "but Alice's grant is removable");
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_a_flag_with_the_authors_name_but_not_her_signature_does_not_count() {
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let mut p = neighbourhood_of(&alice).await;

    let mut forged = class_links(&bob, "Badge", HOLDER, true);
    for link in forged.iter_mut() {
        link.author = alice.did.clone();
    }
    sync_in(&mut p, forged, vec![]).await;
    let holder = grant(&bob, BADGE, HOLDER);
    sync_in(&mut p, vec![holder.clone()], vec![]).await;

    sync_in(&mut p, vec![], vec![holder.clone()]).await;
    assert!(
        !present(&p, &holder),
        "a flag that does not verify is no flag"
    );
}

/// (ii) `sh://path` is an ordinary link a peer's diff can remove. The flag
/// carries the predicate, so the declaration outlives it.
#[tokio::test(flavor = "multi_thread")]
async fn t5_removing_the_path_leaves_the_predicate_monotonic() {
    let alice = TestSigner::generate();
    let mut p = neighbourhood_of(&alice).await;
    let class = class_links(&alice, "Role", MEMBER, true);
    let path = with_predicate(&class, "sh://path").clone();
    sync_in(&mut p, class, vec![]).await;
    let member = grant(&alice, ROLE, MEMBER);
    sync_in(&mut p, vec![member.clone()], vec![]).await;

    sync_in(&mut p, vec![], vec![path.clone()]).await;
    assert!(!present(&p, &path), "the path removal applies");
    sync_in(&mut p, vec![], vec![member.clone()]).await;
    assert!(present(&p, &member), "the predicate is still monotonic");
}

/// Unshared, the owner is the authority. Re-registering a class removes
/// and rewrites its SHACL. The flag cannot be removed, so the refresh leaves
/// it in place (without a second copy) and still replaces everything else.
/// A path change adds a flag for the new predicate; the old predicate stays
/// monotonic.
#[tokio::test(flavor = "multi_thread")]
async fn t5_re_registering_a_class_keeps_its_declarations() {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    let class = |properties: serde_json::Value| serde_json::json!({ "target_class": "app://Role", "properties": properties });
    let did = |path: &str, monotonic: bool| serde_json::json!({ "path": path, "name": "did", "min_count": 0, "monotonic": monotonic });
    let note = serde_json::json!({ "path": "app://note", "name": "note", "min_count": 0 });

    register(
        &mut p,
        &ctx,
        class(serde_json::json!([did(MEMBER, true), note])),
    )
    .await;
    register(&mut p, &ctx, class(serde_json::json!([did(MEMBER, true)]))).await;
    assert_eq!(
        links_under(&p, FLAG).len(),
        1,
        "one flag, not a copy per refresh"
    );
    assert!(
        links_under(&p, "sh://path")
            .iter()
            .all(|l| l.data.target != "app://note"),
        "the rest of the old shape is gone: the refresh ran"
    );

    register(
        &mut p,
        &ctx,
        class(serde_json::json!([did("app://member2", true)])),
    )
    .await;
    assert_eq!(links_under(&p, FLAG).len(), 2, "the new path adds a flag");

    for predicate in [MEMBER, "app://member2"] {
        let link = LinkExpression::from(
            p.add_link(
                Link {
                    source: ROLE.to_string(),
                    predicate: Some(predicate.to_string()),
                    target: "did:key:someone".to_string(),
                },
                LinkStatus::Shared,
                None,
                &ctx,
            )
            .await
            .expect("add_link"),
        );
        let err = p
            .remove_link(link, None)
            .await
            .expect_err("both the old and the new predicate are declared");
        assert!(format!("{err:#}").contains("is monotonic"), "{err:#}");
    }
}

/// A Local role grant owned by this agent, under `MEMBER`, which the owner
/// (the authority of an unshared perspective) has declared monotonic.
async fn local_grant_under_declared(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
) -> LinkExpression {
    register(
        p,
        ctx,
        serde_json::json!({
            "target_class": "app://Role",
            "properties": [{ "path": MEMBER, "name": "did", "min_count": 0, "monotonic": true }],
        }),
    )
    .await;
    LinkExpression::from(
        p.add_link(
            Link {
                source: ROLE.to_string(),
                predicate: Some(MEMBER.to_string()),
                target: "did:key:someone".to_string(),
            },
            LinkStatus::Local,
            None,
            ctx,
        )
        .await
        .expect("add_link"),
    )
}

/// PR A's removal_status rule holds for a declared predicate too: the store
/// decides in both directions. A Shared-labelled removal (the JS client's
/// default) of a Local link under it goes; one of a link this store does not
/// hold is still refused.
#[tokio::test(flavor = "multi_thread")]
async fn t5_link_mutations_labelled_shared_removes_a_local_declared_link() {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    let local = local_grant_under_declared(&mut p, &ctx).await;
    let unknown = grant(&TestSigner::generate(), ROLE, MEMBER);

    p.link_mutations(
        LinkMutations {
            additions: vec![],
            removals: vec![as_input(&local)],
        },
        LinkStatus::Shared,
        &ctx,
    )
    .await
    .expect("a Shared-labelled removal of a Local declared link goes");
    assert!(!present(&p, &local));

    let err = p
        .link_mutations(
            LinkMutations {
                additions: vec![],
                removals: vec![as_input(&unknown)],
            },
            LinkStatus::Shared,
            &ctx,
        )
        .await
        .expect_err("a link this store does not hold keeps its Shared label");
    assert!(format!("{err:#}").contains("is monotonic"), "{err:#}");
}

/// PR A's batched updateLink fix holds for a declared predicate too: the WS
/// handler hands in the old link without a status, so the queued removal
/// must carry the stored one, or the commit backstop reads it as Shared.
#[tokio::test(flavor = "multi_thread")]
async fn t5_a_batched_update_of_a_local_declared_link_commits() {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    let local = local_grant_under_declared(&mut p, &ctx).await;
    let mut old = local.clone();
    old.status = None;
    let next = Link {
        source: ROLE.to_string(),
        predicate: Some(MEMBER.to_string()),
        target: "did:key:someone-else".to_string(),
    };

    let batch = p.create_batch().await;
    p.update_link(old, next, Some(batch.clone()), &ctx)
        .await
        .expect("queue the update");
    p.commit_batch(batch, &ctx)
        .await
        .expect("a batched update of a Local declared link commits");

    assert!(!present(&p, &local));
    let targets: Vec<String> = links_under(&p, MEMBER)
        .into_iter()
        .map(|l| l.data.target)
        .collect();
    assert_eq!(targets, vec!["did:key:someone-else".to_string()]);
}

/// A pulled diff can carry the author's declaration and a peer's removal
/// together (a squashed pull, a late joiner). The removal is dropped, as it
/// would be had they arrived in two diffs, so replicas do not diverge on how
/// the link language batches.
#[tokio::test(flavor = "multi_thread")]
async fn t5_a_flag_protects_a_removal_in_the_same_diff() {
    let alice = TestSigner::generate();
    let bob = TestSigner::generate();
    let mut p = neighbourhood_of(&alice).await;
    let member = grant(&alice, ROLE, MEMBER);
    let holder = grant(&alice, BADGE, HOLDER);
    sync_in(&mut p, vec![member.clone(), holder.clone()], vec![]).await;

    let mut additions = class_links(&alice, "Role", MEMBER, true);
    additions.extend(class_links(&bob, "Badge", HOLDER, true));
    sync_in(&mut p, additions, vec![member.clone(), holder.clone()]).await;

    assert!(present(&p, &member), "the author's flag in the same diff");
    assert!(
        !present(&p, &holder),
        "control: a member's flag in the same diff declares nothing"
    );
}

// ---------------------------------------------------------------------------
// Every local entry point refuses a declared link (review of #1183, finding 3)
// ---------------------------------------------------------------------------

async fn declared_perspective() -> (PerspectiveInstance, AgentContext) {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    register_member_class(&mut p, &ctx).await;
    (p, ctx)
}

async fn register_member_class(p: &mut PerspectiveInstance, ctx: &AgentContext) {
    let shacl: serde_json::Value =
        serde_json::from_str(&class_json("Role", MEMBER, true)).expect("json");
    register(p, ctx, shacl).await;
}

async fn shared_member_link(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    target: &str,
) -> LinkExpression {
    LinkExpression::from(
        p.add_link(
            Link {
                source: ROLE.to_string(),
                predicate: Some(MEMBER.to_string()),
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

fn refused<T: std::fmt::Debug>(r: Result<T, AnyError>, what: &str) {
    let err = r.expect_err(what);
    assert!(
        format!("{err:#}").contains("is monotonic"),
        "{what}: {err:#}"
    );
}

fn removal_of(link: &LinkExpression) -> LinkMutations {
    let mut input = as_input(link);
    input.status = None;
    LinkMutations {
        additions: vec![],
        removals: vec![input],
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_link_mutations_refuses_a_declared_link() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    refused(
        p.link_mutations(removal_of(&link), LinkStatus::Shared, &ctx)
            .await,
        "link_mutations",
    );
    assert!(present(&p, &link));
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_link_mutations_labelled_local_refuses_a_shared_declared_link() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    refused(
        p.link_mutations(removal_of(&link), LinkStatus::Local, &ctx)
            .await,
        "link_mutations labelled Local",
    );
    assert!(present(&p, &link));
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_update_link_refuses_a_declared_link() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    let new = Link {
        source: ROLE.to_string(),
        predicate: Some(MEMBER.to_string()),
        target: "did:key:b".to_string(),
    };
    refused(
        p.update_link(link.clone(), new, None, &ctx).await,
        "update_link",
    );
    assert!(present(&p, &link));
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_remove_links_refuses_a_declared_link() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    refused(
        p.remove_links(vec![link.clone()], None).await,
        "remove_links",
    );
    assert!(present(&p, &link));
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_a_batched_remove_link_refuses_a_declared_link() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    let batch = p.create_batch().await;
    refused(
        p.remove_link(link.clone(), Some(batch)).await,
        "remove_link into a batch",
    );
}

/// The commit backstop: a removal queued before the declaration is refused
/// at commit.
#[tokio::test(flavor = "multi_thread")]
async fn t5_commit_batch_refuses_a_removal_declared_after_it_was_queued() {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    let batch = p.create_batch().await;
    p.remove_link(link.clone(), Some(batch.clone()))
        .await
        .expect("queued before the declaration");
    register_member_class(&mut p, &ctx).await;
    refused(p.commit_batch(batch, &ctx).await, "commit_batch backstop");
    assert!(present(&p, &link));
}

/// The author's executor syncs a member's identical flag before registering
/// the class. That flag declares nothing, so it must not stop ours from
/// being written.
#[tokio::test(flavor = "multi_thread")]
async fn t5_a_peers_identical_flag_does_not_stand_in_for_ours() {
    let (mut p, _, ctx) = setup_perspective_no_llm(&[]).await;
    let bob = TestSigner::generate();
    sync_in(&mut p, class_links(&bob, "Role", MEMBER, true), vec![]).await;
    register_member_class(&mut p, &ctx).await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    refused(
        p.remove_link(link.clone(), None).await,
        "our own declaration must be written",
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn t5_an_authority_change_rebuilds_the_declared_set() {
    let (mut p, ctx) = declared_perspective().await;
    let link = shared_member_link(&mut p, &ctx, "did:key:a").await;
    refused(
        p.remove_link(link.clone(), None).await,
        "declared while this agent is the authority",
    );
    let alice = TestSigner::generate();
    p.persisted.lock().await.neighbourhood = Some(DecoratedNeighbourhoodExpression {
        author: alice.did.clone(),
        ..Default::default()
    });
    sync_in(&mut p, vec![], vec![link.clone()]).await;
    assert!(
        !present(&p, &link),
        "Alice is the authority now and declared nothing: a peer's removal applies"
    );
}
