//! Subscribers of the same query never interfere.
//!
//! Every `subscribe_and_query` / `model_subscribe_and_query` call opens its own
//! subscription, tied to the connection that made it. These tests prove the
//! invariant that matters to apps: one subscriber disposing must not stop
//! updates for another subscriber of the same query.
//!
//! The background `subscribed_queries_loop` is not running in these tests, so
//! `check_subscribed_queries` is driven directly after the matching link is
//! written. Updates are observed on the global pubsub topic the WS layer
//! forwards to clients.

use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
use serde_json::Value;
use tokio::sync::broadcast;

/// Wait for an update for `subscription_id` on `rx`, ignoring updates for
/// other perspectives (the topic is global).
async fn next_update_for(
    rx: &mut broadcast::Receiver<String>,
    perspective_uuid: &str,
    subscription_id: &str,
) -> Value {
    let deadline = Duration::from_secs(10);
    let wait = async {
        loop {
            let msg = rx.recv().await.expect("pubsub receiver closed");
            let update: Value = serde_json::from_str(&msg).expect("subscription update JSON");
            if update["perspectiveUuid"] == perspective_uuid
                && update["subscriptionId"] == subscription_id
            {
                return update;
            }
        }
    };
    tokio::time::timeout(deadline, wait)
        .await
        .unwrap_or_else(|_| {
            panic!(
                "no update for subscription {} within {:?}",
                subscription_id, deadline
            )
        })
}

async fn is_registered(perspective: &PerspectiveInstance, id: &str) -> bool {
    perspective.subscribed_queries.lock().await.contains_key(id)
}

#[tokio::test(flavor = "multi_thread")]
async fn a_model_subscriber_keeps_its_updates_when_another_disposes() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
    let query_json = r#"{"where":{"owner":"alice"}}"#;

    let (id1, _, _) = perspective
        .model_subscribe_and_query("Task".into(), query_json.into(), None, "c".into())
        .await
        .expect("first model subscribe");
    let (id2, _, _) = perspective
        .model_subscribe_and_query("Task".into(), query_json.into(), None, "c".into())
        .await
        .expect("second model subscribe");
    assert_ne!(id1, id2, "each subscriber gets its own subscription");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    assert!(perspective.dispose_query_subscription(&id1, "c").await);
    assert!(!is_registered(&perspective, &id1).await);
    assert!(
        is_registered(&perspective, &id2).await,
        "one subscriber's dispose must leave the other's subscription"
    );

    // A matching Task lands; the remaining subscriber still gets the update.
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Task".to_string()),
                query: None,
            },
            "ad4m://task/kept".to_string(),
            Some(serde_json::json!({ "title": "ship it", "owner": "alice" })),
            None,
            &ctx,
        )
        .await
        .expect("create_subject(Task)");
    perspective
        .check_subscribed_queries(ChangedPredicates::CheckAll)
        .await;

    let update = next_update_for(&mut rx, &perspective.uuid, &id2).await;
    assert!(
        update.to_string().contains("ad4m://task/kept"),
        "the update must carry the new Task, got: {update}"
    );

    assert!(perspective.dispose_query_subscription(&id2, "c").await);
    assert!(!is_registered(&perspective, &id2).await);
    assert!(
        !perspective.dispose_query_subscription(&id2, "c").await,
        "disposing an unknown id reports false"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn a_query_subscriber_keeps_its_updates_when_another_connection_disposes() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let query = "SELECT ?s ?o WHERE { ?s <ns://title> ?o . }".to_string();

    let (id1, _, _) = perspective
        .subscribe_and_query(query.clone(), None, "a".into())
        .await
        .expect("first subscribe");
    let (id2, _, _) = perspective
        .subscribe_and_query(query.clone(), None, "b".into())
        .await
        .expect("second subscribe");
    assert_ne!(id1, id2, "each subscriber gets its own subscription");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // A connection can end only its own subscriptions.
    assert!(!perspective.dispose_query_subscription(&id2, "a").await);
    assert!(perspective.dispose_query_subscription(&id1, "a").await);
    assert!(is_registered(&perspective, &id2).await);

    perspective
        .add_link(
            Link {
                source: "ns://thing/1".into(),
                predicate: Some("ns://title".into()),
                target: "literal://string:hello".into(),
            },
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .expect("add_link");
    perspective
        .check_subscribed_queries(ChangedPredicates::Specific(HashSet::from([
            "ns://title".to_string()
        ])))
        .await;

    let update = next_update_for(&mut rx, &perspective.uuid, &id2).await;
    assert!(
        update.to_string().contains("ns://thing/1"),
        "the update must carry the new link, got: {update}"
    );
    assert_eq!(
        update["connectionId"], "b",
        "addressed to its own connection"
    );
}
