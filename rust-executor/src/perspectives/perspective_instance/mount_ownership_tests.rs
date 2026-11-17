//! What a mount owns, and what it leaves alone: links someone else wrote
//! under `<lang>://`, whatever their timestamp, the schema a newer sync
//! installed, and another mount's shape nodes.

use super::mount_gates::gates;
use super::mount_gates::MountPoint;
use super::mount_lifecycle_tests::{links_at, project};
use super::mounts_tests::{create, mounted};
use super::*;
use crate::agent::{did_for_context, sign_for_context, signing_key_id_for_context};
use crate::languages::mount_tests::load_mount_test_language;
use serde_json::json;

const EPOCH: &str = "1970-01-01T00:00:00.000Z";

/// `link` signed by the main agent at the Unix epoch, the timestamp A9's
/// mount used as its marker.
fn main_agent_link_at_epoch(link: Link) -> LinkExpression {
    let context = AgentContext::main_agent();
    let data = link.normalize();
    let signature = hex::encode(
        sign_for_context(
            &crate::agent::signatures::hash_data_and_timestamp(
                &data,
                &chrono::DateTime::<chrono::Utc>::UNIX_EPOCH,
            ),
            &context,
        )
        .unwrap(),
    );
    LinkExpression::from(Expression {
        author: did_for_context(&context).unwrap(),
        timestamp: EPOCH.to_string(),
        data,
        proof: ExpressionProof {
            key: signing_key_id_for_context(&context).unwrap(),
            signature,
        },
    })
}

/// Whether the store holds a link with exactly this author and timestamp
/// on `source -[predicate]-> target`.
async fn holds(p: &PerspectiveInstance, expected: &LinkExpression) -> bool {
    links_at(
        p,
        &expected.data.source,
        expected.data.predicate.as_deref().unwrap(),
    )
    .await
    .iter()
    .any(|l| {
        l.author == expected.author
            && l.timestamp == expected.timestamp
            && l.data.target == expected.data.target
    })
}

// --- Only the mount's own links are the mount's ---------------------------

#[tokio::test]
async fn an_epoch_dated_link_of_a_foreign_author_survives_sync_and_unmount() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;
    let foreign = LinkExpression::from(Expression {
        author: "did:key:z6MkForeignAuthorOfAnAnnotation".to_string(),
        timestamp: EPOCH.to_string(),
        data: Link {
            source: format!("{lang}://t1"),
            predicate: Some("ex://note".to_string()),
            target: "literal:string:theirs".to_string(),
        },
        proof: ExpressionProof {
            key: "did:key:z6MkForeignAuthorOfAnAnnotation#key".to_string(),
            signature: "00".to_string(),
        },
    });
    p.add_link_expression(foreign.clone(), LinkStatus::Local, None)
        .await
        .unwrap();

    p.sync_mount(&lang).await.unwrap();
    assert!(holds(&p, &foreign).await, "sync removed it");
    p.unmount(&lang).await.unwrap();
    assert!(holds(&p, &foreign).await, "unmount removed it");
}

#[tokio::test]
async fn an_epoch_dated_link_of_the_main_agent_survives_sync_and_unmount() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;
    // The same triple the mount wrote for t1's title, through the ordinary
    // `perspective.addLinkExpression` path.
    let mine = main_agent_link_at_epoch(Link {
        source: format!("{lang}://t1"),
        predicate: Some("mounttest://title".to_string()),
        target: "literal:string:a".to_string(),
    });
    p.add_link_expression(mine.clone(), LinkStatus::Local, None)
        .await
        .unwrap();

    project(
        &lang,
        "outsideChange",
        json!({"address": "t1", "data": {"title": "b"}, "emit": false}),
    )
    .await;
    p.sync_mount(&lang).await.unwrap();
    assert!(holds(&p, &mine).await, "sync removed it");
    p.unmount(&lang).await.unwrap();
    assert!(holds(&p, &mine).await, "unmount removed it");
}

// --- A stale sync never rolls the schema back -------------------------------

#[tokio::test]
async fn an_older_sync_does_not_roll_back_newer_shapes() {
    let lang = load_mount_test_language().await;
    let (p, _ctx) = mounted(&lang).await;

    // Sync A reads version 2 and stops; sync B reads version 3 and applies.
    let gate = gates::arm(&lang, MountPoint::SnapshotFetched);
    let mut older = p.clone();
    let l = lang.clone();
    let a = tokio::spawn(async move { older.sync_mount(&l).await });
    gate.arrived().await;
    project(&lang, "setVersion", json!({"version": "3"})).await;
    p.clone().sync_mount(&lang).await.unwrap();
    gate.release();
    let _ = a.await.unwrap();

    let versions: Vec<String> =
        links_at(&p, &format!("{lang}://project"), "ad4m://mount_shapes_version")
            .await
            .into_iter()
            .map(|l| l.data.target)
            .collect();
    assert_eq!(
        versions,
        vec!["literal:string:3".to_string()],
        "the older sync reinstalled its shapes"
    );
}

// --- Shape nodes and declared names are owned too ---------------------------

#[tokio::test]
async fn a_shape_node_of_another_mount_is_refused() {
    let lang = load_mount_test_language().await;
    let lang2 = load_mount_test_language().await;
    project(
        &lang,
        "setShapes",
        json!({"shapes": [{"name": "XFoo", "target": "a://XFoo"}]}),
    )
    .await;
    // Name, basename and IRI all differ; only the shape node is `lang`'s.
    project(
        &lang2,
        "setShapes",
        json!({"shapes": [{"name": "Foo", "target": "b://Foo", "nodeShapeUri": "a://XFooShape"}]}),
    )
    .await;
    let (mut p, _ctx) = mounted(&lang).await;

    let error = p.mount(&lang2).await.unwrap_err();
    assert!(error.to_string().contains("a://XFooShape"), "{error}");
}

#[tokio::test]
async fn one_shape_set_naming_a_class_twice_is_refused() {
    let lang = load_mount_test_language().await;
    project(
        &lang,
        "setShapes",
        json!({"shapes": [
            {"name": "Dup", "target": "mounttest://DupA"},
            {"name": "Dup", "target": "mounttest://DupB"}
        ]}),
    )
    .await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let error = p.mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("Dup"), "{error}");
}
