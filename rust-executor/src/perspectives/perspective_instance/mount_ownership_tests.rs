//! What a mount owns, and what it leaves alone: links someone else wrote
//! under `<lang>://`, whatever their timestamp.

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
