//! Shared query subscriptions are reference-counted.
//!
//! `subscribe_and_query` and `model_subscribe_and_query` hand every caller
//! that registers the same (query, user) pair the same subscription id, so
//! one re-evaluation and one push serve all of them. These tests prove the
//! invariant that makes that sharing safe: a subscriber disposing its hold
//! must not stop updates for the other holders. The entry is removed only
//! when the last holder disposes.
//!
//! The background `subscribed_queries_loop` is not running in these tests, so
//! `check_subscribed_queries` is driven directly after the matching link is
//! written. Pushes are observed on the global pubsub topic the WS layer
//! forwards to clients.

use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::pubsub::get_global_pubsub;
use crate::types::PerspectiveQuerySubscriptionFilter;
use tokio::sync::broadcast;

/// Wait for a push for `subscription_id` on `rx`, ignoring pushes for other
/// perspectives (the topic is global).
async fn next_push_for(
    rx: &mut broadcast::Receiver<String>,
    perspective_uuid: &str,
    subscription_id: &str,
) -> PerspectiveQuerySubscriptionFilter {
    let deadline = Duration::from_secs(10);
    let wait = async {
        loop {
            let msg = rx.recv().await.expect("pubsub receiver closed");
            let filter: PerspectiveQuerySubscriptionFilter =
                serde_json::from_str(&msg).expect("subscription push JSON");
            if filter.uuid == perspective_uuid && filter.subscription_id == subscription_id {
                return filter;
            }
        }
    };
    tokio::time::timeout(deadline, wait)
        .await
        .unwrap_or_else(|_| {
            panic!(
                "no push for subscription {} within {:?}",
                subscription_id, deadline
            )
        })
}

async fn is_registered(perspective: &PerspectiveInstance, id: &str) -> bool {
    perspective.subscribed_queries.lock().await.contains_key(id)
}

#[tokio::test(flavor = "multi_thread")]
async fn model_subscription_shared_by_two_holders_survives_one_dispose() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
    let query_json = r#"{"where":{"owner":"alice"}}"#;

    let (id1, _) = perspective
        .model_subscribe_and_query("Task".into(), query_json.into(), None)
        .await
        .expect("first model subscribe");
    let (id2, _) = perspective
        .model_subscribe_and_query("Task".into(), query_json.into(), None)
        .await
        .expect("second model subscribe");
    assert_eq!(id1, id2, "same params must share one subscription id");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // First holder lets go: the entry must stay registered for the second one.
    assert!(perspective
        .dispose_query_subscription(id1.clone())
        .await
        .expect("first dispose"));
    assert!(
        is_registered(&perspective, &id1).await,
        "a shared subscription must survive one holder's dispose"
    );

    // A matching Task lands; the remaining holder must still be pushed to.
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Task".to_string()),
                query: None,
            },
            "ad4m://task/shared-sub".to_string(),
            Some(serde_json::json!({ "title": "ship it", "owner": "alice" })),
            None,
            &ctx,
        )
        .await
        .expect("create_subject(Task)");
    perspective
        .check_subscribed_queries(ChangedPredicates::CheckAll)
        .await;

    let push = next_push_for(&mut rx, &perspective.uuid, &id1).await;
    assert!(
        push.result.contains("ad4m://task/shared-sub"),
        "push must carry the new Task, got: {}",
        push.result
    );

    // Last holder lets go: now the entry is removed.
    assert!(perspective
        .dispose_query_subscription(id1.clone())
        .await
        .expect("second dispose"));
    assert!(
        !is_registered(&perspective, &id1).await,
        "the last dispose must remove the subscription"
    );
    assert!(
        !perspective
            .dispose_query_subscription(id1.clone())
            .await
            .expect("third dispose"),
        "disposing an unknown id reports false"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn legacy_subscription_shared_by_two_holders_survives_one_dispose() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;
    let query = "SELECT ?s ?o WHERE { ?s <ns://title> ?o . }".to_string();

    let (id1, _) = perspective
        .subscribe_and_query(query.clone(), None)
        .await
        .expect("first subscribe");
    let (id2, _) = perspective
        .subscribe_and_query(query.clone(), None)
        .await
        .expect("second subscribe");
    assert_eq!(id1, id2, "same query must share one subscription id");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    assert!(perspective
        .dispose_query_subscription(id1.clone())
        .await
        .expect("first dispose"));
    assert!(
        is_registered(&perspective, &id1).await,
        "a shared subscription must survive one holder's dispose"
    );

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

    let push = next_push_for(&mut rx, &perspective.uuid, &id1).await;
    assert!(
        push.result.contains("ns://thing/1"),
        "push must carry the new link, got: {}",
        push.result
    );

    assert!(perspective
        .dispose_query_subscription(id1.clone())
        .await
        .expect("second dispose"));
    assert!(
        !is_registered(&perspective, &id1).await,
        "the last dispose must remove the subscription"
    );
}

/// A class sharing `Task`'s flag predicate (with its own value) and declaring
/// a predicate `Task` does not: `ns://body`.
const NOTE_SDNA: &str = r#"{
  "target_class":"ns://Note",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://note"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://note","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]}
  ]
}"#;

/// One subscription over two classes (#1238) re-runs on a write to a predicate
/// only one of them declares, for each of the two. With the trigger set of
/// only one class, the other class's edit would be skipped as disjoint.
#[tokio::test(flavor = "multi_thread")]
async fn union_model_subscription_reruns_on_an_edit_to_either_class() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("Task", TASK_SDNA), ("Note", NOTE_SDNA)]).await;
    let classes =
        crate::perspectives::model_query::QueryClasses::Union(vec!["Task".into(), "Note".into()]);
    let (id, initial) = perspective
        .model_subscribe_and_query(classes, r#"{"includeUnverified":true}"#.into(), None)
        .await
        .expect("union model subscribe");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    for (class, base, props, changed, expected) in [
        (
            "Task",
            "ad4m://task/union-sub",
            serde_json::json!({ "title": "ship it", "owner": "alice" }),
            "ns://owner",
            "\"__subjectClass\":\"Task\"",
        ),
        (
            "Note",
            "ad4m://note/union-sub",
            serde_json::json!({ "body": "remember" }),
            "ns://body",
            "\"__subjectClass\":\"Note\"",
        ),
    ] {
        perspective
            .create_subject(
                SubjectClassOption {
                    class_name: Some(class.to_string()),
                    query: None,
                },
                base.to_string(),
                Some(props),
                None,
                &ctx,
            )
            .await
            .unwrap_or_else(|e| panic!("create_subject({class}): {e}"));
        // Only the predicate one class declares is reported as changed.
        perspective
            .check_subscribed_queries(ChangedPredicates::Specific(HashSet::from([
                changed.to_string()
            ])))
            .await;
        let push = next_push_for(&mut rx, &perspective.uuid, &id).await;
        assert!(
            push.result.contains(base) && push.result.contains(expected),
            "a {changed} write must re-run the union subscription, got: {}",
            push.result
        );
    }
}
