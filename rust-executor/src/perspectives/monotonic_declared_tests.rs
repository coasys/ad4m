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
