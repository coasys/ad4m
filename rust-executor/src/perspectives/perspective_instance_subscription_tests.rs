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
        .model_subscribe_and_query("Task".into(), query_json.into(), "c".into())
        .await
        .expect("first model subscribe");
    let (id2, _, _) = perspective
        .model_subscribe_and_query("Task".into(), query_json.into(), "c".into())
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
        .subscribe_and_query(query.clone(), "a".into())
        .await
        .expect("first subscribe");
    let (id2, _, _) = perspective
        .subscribe_and_query(query.clone(), "b".into())
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
        .check_subscribed_queries(ChangedPredicates::Specific(
            HashSet::from(["ns://title".to_string()]).into(),
        ))
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
        crate::perspectives::model_query::QueryClasses::union(vec!["Task".into(), "Note".into()]);
    let (id, _, initial) = perspective
        .model_subscribe_and_query(
            classes,
            r#"{"includeUnverified":true,"count":true}"#.into(),
            "c".into(),
        )
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
        // Only a link on the predicate one class declares is reported as
        // written, so the trigger must hold that class's rules.
        let mut writes = subscription_triggers::Writes::new();
        writes.record(base, changed, "literal://string:edited");
        perspective
            .check_subscribed_queries(ChangedPredicates::Specific(writes))
            .await;
        let update = next_update_for(&mut rx, &perspective.uuid, &id)
            .await
            .to_string();
        assert!(
            update.contains(base) && update.contains(expected),
            "a {changed} write must re-run the union subscription, got: {update}"
        );
    }
}

/// A diff whose changed predicates are outside every requested class's trigger
/// set does not re-run a union subscription. The Task is written for real, so
/// the query's result *has* changed: a re-run would push it. Reporting only
/// a link on `ns://unrelated` must therefore produce no push, which an empty
/// ("re-run on every write") trigger set would.
#[tokio::test(flavor = "multi_thread")]
async fn union_model_subscription_ignores_a_write_outside_its_classes() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("Task", TASK_SDNA), ("Note", NOTE_SDNA)]).await;
    let classes =
        crate::perspectives::model_query::QueryClasses::union(vec!["Task".into(), "Note".into()]);
    let (id, _, _) = perspective
        .model_subscribe_and_query(classes, r#"{"includeUnverified":true}"#.into(), "c".into())
        .await
        .expect("union model subscribe");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Task".to_string()),
                query: None,
            },
            "ad4m://task/unrelated-diff".to_string(),
            Some(serde_json::json!({ "title": "quiet", "owner": "bob" })),
            None,
            &ctx,
        )
        .await
        .expect("create_subject(Task)");
    perspective
        .check_subscribed_queries(ChangedPredicates::Specific({
            let mut writes = subscription_triggers::Writes::new();
            writes.record(
                "ad4m://task/unrelated-diff",
                "ns://unrelated",
                "literal://string:x",
            );
            writes
        }))
        .await;

    let quiet = tokio::time::timeout(
        Duration::from_millis(1500),
        next_update_for(&mut rx, &perspective.uuid, &id),
    )
    .await;
    assert!(
        quiet.is_err(),
        "a write outside both classes' trigger sets must not re-run the subscription"
    );
}

/// The same classes in another order, or named twice, are one union (#1311):
/// each subscriber still gets its own subscription (no sharing), and both
/// answer the same result.
#[tokio::test(flavor = "multi_thread")]
async fn union_model_subscriptions_answer_the_same_regardless_of_class_order() {
    use crate::perspectives::model_query::QueryClasses;
    let (perspective, _shapes, _ctx) =
        setup_perspective_no_llm(&[("Task", TASK_SDNA), ("Note", NOTE_SDNA)]).await;
    let query_json = r#"{"includeUnverified":true}"#;
    let (a, _, result_a) = perspective
        .model_subscribe_and_query(
            QueryClasses::union(vec!["Task".into(), "Note".into()]),
            query_json.into(),
            "c".into(),
        )
        .await
        .expect("first union subscribe");
    let (b, _, result_b) = perspective
        .model_subscribe_and_query(
            QueryClasses::union(vec!["Note".into(), "Task".into(), "Note".into()]),
            query_json.into(),
            "c".into(),
        )
        .await
        .expect("second union subscribe");
    assert_ne!(a, b, "each subscriber gets its own subscription");
    assert_eq!(result_a, result_b, "equal unions answer the same");
}

/// A model subscription re-runs one query on every matching write. With a
/// cursor it would re-run the same page forever, so `after` is refused before
/// anything is registered, while `"after": null` reads as no cursor.
#[tokio::test(flavor = "multi_thread")]
async fn a_model_subscription_refuses_a_cursor() {
    let (perspective, _shapes, _ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
    for query_json in [
        r#"{"limit":5,"after":""}"#,
        r#"{"limit":5,"after":"c1"}"#,
        r#"{"after":""}"#,
    ] {
        let err = perspective
            .model_subscribe_and_query("Task".into(), query_json.into(), "c".into())
            .await
            .expect_err(query_json);
        assert_eq!(err.to_string(), MODEL_SUBSCRIPTION_CURSOR_UNSUPPORTED);
    }
    assert!(
        perspective.subscribed_queries.lock().await.is_empty(),
        "a refused subscription registers nothing"
    );
    perspective
        .model_subscribe_and_query(
            "Task".into(),
            r#"{"limit":5,"after":null}"#.into(),
            "c".into(),
        )
        .await
        .expect("a null cursor is no cursor");
}
