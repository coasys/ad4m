//! Abstract classes on a perspective (#1435): queried through the ordinary
//! model query and subscription, never created, resolved once per change of
//! the installed shapes, and a subscription picks up a class installed after
//! it subscribed.
//!
//! As in `perspective_instance_subscription_tests`, the background
//! `subscribed_queries_loop` is not running: `check_subscribed_queries` is
//! driven directly, with the writes each step reports.

use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::perspectives::model_query::QueryClasses;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
use serde_json::Value;
use tokio::sync::broadcast;

/// Anything with `text`. With a setter, as the SDK writes one for every
/// property: a write through it would land on `ns://text`, which no
/// concrete class here reads.
const TEXT_CONTENT_SDNA: &str = r#"{
  "target_class":"ns://TextContent",
  "abstract":true,
  "properties":[{"path":"ns://text","name":"text","max_count":1,"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://text","target":"value"}]}]
}"#;

/// A chat message: `text` through `ns://message`.
const CHAT_SDNA: &str = r#"{
  "target_class":"ns://Chat",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://chat"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://chat","min_count":1,"max_count":1},
    {"path":"ns://message","name":"text","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://message","target":"value"}]}
  ]
}"#;

/// Flux-like: `body`, aliased `text` for TextContent. Its flag and body
/// predicates are its own, so a trigger built before it was installed cannot
/// match its writes by accident.
const MESSAGE_SDNA: &str = r#"{
  "target_class":"ns://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://entry_type","target":"ns://message"}],
  "properties":[
    {"path":"ns://entry_type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","min_count":0,"max_count":1,"resolve_language":"literal","alias":{"TextContent":"text"},"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]}
  ]
}"#;

async fn install(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    class: &str,
    sdna: &str,
) {
    perspective
        .add_sdna(
            class.to_string(),
            String::new(),
            SdnaType::SubjectClass,
            Some(sdna.to_string()),
            ctx,
        )
        .await
        .unwrap_or_else(|e| panic!("add_sdna({class}): {e}"));
}

async fn create(
    perspective: &mut PerspectiveInstance,
    ctx: &AgentContext,
    class: &str,
    base: &str,
    props: Value,
) -> Result<(), AnyError> {
    perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some(class.to_string()),
                query: None,
            },
            base.to_string(),
            Some(props),
            None,
            ctx,
        )
        .await
}

/// `(id, __subjectClass, text)` per row, sorted.
fn rows(json: &str) -> Vec<(String, String, String)> {
    let v: Value = serde_json::from_str(json).unwrap();
    let mut rows: Vec<_> = v["instances"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| {
            (
                r["id"].as_str().unwrap().to_string(),
                r["__subjectClass"].as_str().unwrap_or("").to_string(),
                r["text"].as_str().unwrap_or("").to_string(),
            )
        })
        .collect();
    rows.sort();
    rows
}

const ALL: &str = r#"{"includeUnverified":true}"#;

fn one(class: &str) -> QueryClasses {
    QueryClasses::One(class.to_string())
}

/// The ordinary model query over an abstract class reads every class with
/// `text`, the aliased one included; the concrete class read on its own
/// keeps its name. Creating an abstract class is refused.
#[tokio::test(flavor = "multi_thread")]
async fn an_abstract_class_is_queried_through_the_ordinary_api_and_never_created() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[
        ("TextContent", TEXT_CONTENT_SDNA),
        ("Chat", CHAT_SDNA),
        ("Message", MESSAGE_SDNA),
        ("Task", TASK_SDNA),
    ])
    .await;
    create(
        &mut perspective,
        &ctx,
        "Chat",
        "ad4m://chat/1",
        serde_json::json!({"text": "hi @data"}),
    )
    .await
    .unwrap();
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/1",
        serde_json::json!({"body": "<p>hello</p>"}),
    )
    .await
    .unwrap();
    create(
        &mut perspective,
        &ctx,
        "Task",
        "ad4m://task/1",
        serde_json::json!({"title": "t", "owner": "o"}),
    )
    .await
    .unwrap();

    let json = perspective
        .model_query_classes(&one("TextContent"), ALL)
        .await
        .unwrap();
    assert_eq!(
        rows(&json),
        vec![
            ("ad4m://chat/1".into(), "Chat".into(), "hi @data".into()),
            (
                "ad4m://message/1".into(),
                "Message".into(),
                "<p>hello</p>".into()
            ),
        ]
    );

    let own: Value = serde_json::from_str(
        &perspective
            .model_query_classes(&one("Message"), ALL)
            .await
            .unwrap(),
    )
    .unwrap();
    assert_eq!(own["instances"][0]["body"], "<p>hello</p>");
    assert!(own["instances"][0].get("text").is_none(), "{own}");

    let refused = create(
        &mut perspective,
        &ctx,
        "TextContent",
        "ad4m://text/1",
        serde_json::json!({"text": "x"}),
    )
    .await
    .unwrap_err();
    assert!(refused.to_string().contains("abstract class"), "{refused}");
    assert!(perspective
        .get_links_local(&LinkQuery {
            source: Some("ad4m://text/1".into()),
            ..Default::default()
        })
        .unwrap()
        .is_empty());

    // Never a record's class.
    let classes = perspective
        .subject_classes_of(&["ad4m://chat/1".to_string()])
        .unwrap();
    assert_eq!(classes["ad4m://chat/1"], vec!["Chat".to_string()]);
}

/// A record read through an abstract class is updated through its own
/// class. Through the abstract one, the setter would write the abstract
/// predicate (`ns://text`) onto a Message whose text is `ns://body`: refused,
/// and nothing is written.
#[tokio::test(flavor = "multi_thread")]
async fn a_record_is_never_updated_through_an_abstract_class() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[
        ("TextContent", TEXT_CONTENT_SDNA),
        ("Message", MESSAGE_SDNA),
    ])
    .await;
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/1",
        serde_json::json!({"body": "hello"}),
    )
    .await
    .unwrap();

    let refused = perspective
        .update_subject(
            SubjectClassOption {
                class_name: Some("TextContent".into()),
                query: None,
            },
            "ad4m://message/1".into(),
            serde_json::json!({"text": "edited"}),
            None,
            &ctx,
        )
        .await
        .unwrap_err()
        .to_string();
    assert!(
        refused.contains("'TextContent' is an abstract class")
            && refused.contains("cannot be updated through it")
            && refused.contains("__subjectClass"),
        "{refused}"
    );
    assert!(perspective
        .get_links_local(&LinkQuery {
            source: Some("ad4m://message/1".into()),
            predicate: Some("ns://text".into()),
            ..Default::default()
        })
        .unwrap()
        .is_empty());

    // Through its own class it goes through.
    perspective
        .update_subject(
            SubjectClassOption {
                class_name: Some("Message".into()),
                query: None,
            },
            "ad4m://message/1".into(),
            serde_json::json!({"body": "edited"}),
            None,
            &ctx,
        )
        .await
        .unwrap();
    let json = perspective
        .model_query_classes(&one("TextContent"), ALL)
        .await
        .unwrap();
    assert_eq!(
        rows(&json),
        vec![("ad4m://message/1".into(), "Message".into(), "edited".into())]
    );
}

/// Read on its own: in a union of several classes, refused. No conforming
/// class is an empty result; what a union refuses is refused even then.
#[tokio::test(flavor = "multi_thread")]
async fn an_abstract_class_is_read_on_its_own() {
    let (perspective, _shapes, _ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Task", TASK_SDNA)]).await;
    let err = perspective
        .model_query_classes(
            &QueryClasses::union(vec!["TextContent".into(), "Task".into()]),
            ALL,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("abstract class"), "{err}");

    let empty: Value = serde_json::from_str(
        &perspective
            .model_query_classes(&one("TextContent"), ALL)
            .await
            .expect("no conforming class is an empty result"),
    )
    .unwrap();
    assert_eq!(empty["instances"], serde_json::json!([]));
    assert_eq!(empty["totalCount"], 0);

    let err = perspective
        .model_query_classes(
            &one("TextContent"),
            r#"{"parent":{"ids":["ad4m://p"],"predicate":"ad4m://has_child","limitPerAnchor":2}}"#,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("limitPerAnchor"), "{err}");
}

/// The resolution is cached (the same `Arc` comes back) until the installed
/// shapes change; a class installed after the first resolution is then
/// included, and one re-registered as abstract drops out.
#[tokio::test(flavor = "multi_thread")]
async fn the_resolution_is_cached_and_dropped_when_shapes_change() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    let first = perspective.resolve_abstract("TextContent").unwrap();
    assert_eq!(first.classes, vec!["Chat".to_string()]);
    let again = perspective.resolve_abstract("TextContent").unwrap();
    assert!(Arc::ptr_eq(&first, &again), "served from the cache");

    install(&mut perspective, &ctx, "Message", MESSAGE_SDNA).await;
    let after = perspective.resolve_abstract("TextContent").unwrap();
    assert_eq!(
        after.classes,
        vec!["Chat".to_string(), "Message".to_string()]
    );

    // Re-registered as abstract: the marker comes with the refreshed shape,
    // and the index sees it.
    install(
        &mut perspective,
        &ctx,
        "Chat",
        &CHAT_SDNA.replacen("{\n", "{\"abstract\":true,\n", 1),
    )
    .await;
    assert!(perspective.abstract_index().unwrap().is_abstract("Chat"));
    let after = perspective.resolve_abstract("TextContent").unwrap();
    assert_eq!(after.classes, vec!["Message".to_string()]);

    // And back: the refresh purges the marker.
    install(&mut perspective, &ctx, "Chat", CHAT_SDNA).await;
    assert!(!perspective.abstract_index().unwrap().is_abstract("Chat"));

    // A shape synced in from a peer drops the cache too.
    let before = perspective.resolve_abstract("TextContent").unwrap();
    let generation = perspective.shapes_generation();
    perspective.clear_shape_cache();
    assert!(perspective.shapes_generation() > generation);
    let after = perspective.resolve_abstract("TextContent").unwrap();
    assert!(!Arc::ptr_eq(&before, &after), "resolved again");
}

/// Wait for an update for `subscription_id` on `rx`, ignoring others.
async fn next_update_for(
    rx: &mut broadcast::Receiver<String>,
    perspective_uuid: &str,
    subscription_id: &str,
) -> Option<Value> {
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
    tokio::time::timeout(Duration::from_secs(5), wait)
        .await
        .ok()
}

/// The writes `add_sdna` / `create_subject` report, as the loop would pass
/// them.
fn writes(links: &[(&str, &str, &str)]) -> ChangedPredicates {
    let mut writes = subscription_triggers::Writes::new();
    for (s, p, t) in links {
        writes.record(s, p, t);
    }
    ChangedPredicates::Specific(writes)
}

/// A class installed after a subscription over an abstract class was opened
/// starts producing rows, read through its alias: the subscription resolves
/// its classes again when the installed shapes change, and its trigger then
/// knows the new class. With the class list frozen at subscribe time, the
/// new class's first record is never pushed.
#[tokio::test(flavor = "multi_thread")]
async fn a_subscription_picks_up_a_class_installed_after_subscribe() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    let (id, _, initial) = perspective
        .model_subscribe_and_query(one("TextContent"), ALL.into(), "c".into())
        .await
        .expect("subscribe");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // Install Message and let its pass settle: no record yet, so no push.
    install(&mut perspective, &ctx, "Message", MESSAGE_SDNA).await;
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://MessageShape",
            "sh://property",
            "ns://Message.body",
        )]))
        .await;
    assert!(
        next_update_for(&mut rx, &perspective.uuid, &id)
            .await
            .is_none(),
        "installing a class changes no row"
    );

    // The write under test, alone: Message's first record.
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/late",
        serde_json::json!({"body": "@data hi"}),
    )
    .await
    .unwrap();
    perspective
        .check_subscribed_queries(writes(&[
            ("ad4m://message/late", "ns://entry_type", "ns://message"),
            (
                "ad4m://message/late",
                "ns://body",
                "literal://string:@data hi",
            ),
        ]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("the new class's record must be pushed")
        .to_string();
    assert!(
        update.contains("ad4m://message/late")
            && update.contains("\"__subjectClass\":\"Message\"")
            && update.contains("\"text\""),
        "{update}"
    );
}

/// Records can exist before their class does: a peer's data arrives before
/// its SHACL, or an app registers its class after writing. Installing the
/// class alone must push them: the subscription's classes changed, so it
/// re-runs whatever the write was.
#[tokio::test(flavor = "multi_thread")]
async fn installing_a_class_pushes_records_that_were_already_there() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    // A Message, written as raw links before its class exists.
    perspective
        .add_links(
            vec![
                Link {
                    source: "ad4m://message/early".into(),
                    predicate: Some("ns://entry_type".into()),
                    target: "ns://message".into(),
                },
                Link {
                    source: "ad4m://message/early".into(),
                    predicate: Some("ns://body".into()),
                    target: "literal://string:early".into(),
                },
            ],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();

    let (id, _, initial) = perspective
        .model_subscribe_and_query(one("TextContent"), ALL.into(), "c".into())
        .await
        .expect("subscribe");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // The write under test: Message's SHACL, and nothing else. Its
    // predicates are no record's, so only the re-resolution re-runs it.
    install(&mut perspective, &ctx, "Message", MESSAGE_SDNA).await;
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://MessageShape",
            "sh://property",
            "ns://Message.body",
        )]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("the record already there must be pushed once its class is installed")
        .to_string();
    assert!(
        update.contains("ad4m://message/early") && update.contains("\"text\""),
        "{update}"
    );
}

/// Message with its alias moved from `body` to `content`: the same class
/// list, read through other predicates.
const MESSAGE_MOVED_SDNA: &str = r#"{
  "target_class":"ns://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://entry_type","target":"ns://message"}],
  "properties":[
    {"path":"ns://entry_type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]},
    {"path":"ns://content","name":"content","min_count":0,"max_count":1,"alias":{"TextContent":"text"},"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://content","target":"value"}]}
  ]
}"#;

/// Drain whatever updates `id` gets within the wait.
async fn drain(rx: &mut broadcast::Receiver<String>, perspective_uuid: &str, id: &str) {
    while next_update_for(rx, perspective_uuid, id).await.is_some() {}
}

/// A change of the installed shapes that moves only a view (the alias, from
/// `body` to `content`) rebuilds the trigger too: the class list is the same,
/// but the trigger built over `body` would never see a write to `content`.
#[tokio::test(flavor = "multi_thread")]
async fn a_subscription_follows_an_alias_that_moved() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[
        ("TextContent", TEXT_CONTENT_SDNA),
        ("Message", MESSAGE_SDNA),
    ])
    .await;
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/1",
        serde_json::json!({"body": "old"}),
    )
    .await
    .unwrap();
    let (id, _, initial) = perspective
        .model_subscribe_and_query(one("TextContent"), ALL.into(), "c".into())
        .await
        .expect("subscribe");
    assert!(initial.contains("\"text\":\"old\""), "{initial}");
    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    install(&mut perspective, &ctx, "Message", MESSAGE_MOVED_SDNA).await;
    assert_eq!(
        perspective.resolve_abstract("TextContent").unwrap().classes,
        vec!["Message".to_string()],
        "the class list did not change"
    );
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://MessageShape",
            "sh://property",
            "ns://Message.content",
        )]))
        .await;
    drain(&mut rx, &perspective.uuid, &id).await;

    // The write under test, alone: `content`, which only the new view reads.
    perspective
        .add_links(
            vec![Link {
                source: "ad4m://message/1".into(),
                predicate: Some("ns://content".into()),
                target: "literal:string:moved".into(),
            }],
            LinkStatus::Shared,
            None,
            &ctx,
        )
        .await
        .unwrap();
    perspective
        .check_subscribed_queries(writes(&[(
            "ad4m://message/1",
            "ns://content",
            "literal:string:moved",
        )]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("a write to the moved alias must be pushed")
        .to_string();
    assert!(update.contains("\"text\":\"moved\""), "{update}");
}

/// A store error while a subscription's classes resolve, at subscribe time
/// or on a later pass, is not its final answer: the next pass resolves it
/// again. Kept, it would watch the abstract class's own predicates, which no
/// class writes, and never update again.
#[tokio::test(flavor = "multi_thread")]
async fn a_failed_resolution_is_tried_again_on_the_next_pass() {
    use super::abstract_classes::faults::fail_next_index;
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // At subscribe time.
    fail_next_index(&perspective.uuid);
    let (id, _, initial) = perspective
        .model_subscribe_and_query(one("TextContent"), ALL.into(), "c".into())
        .await
        .expect("subscribe");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");
    create(
        &mut perspective,
        &ctx,
        "Chat",
        "ad4m://chat/1",
        serde_json::json!({"text": "first"}),
    )
    .await
    .unwrap();
    perspective
        .check_subscribed_queries(writes(&[
            ("ad4m://chat/1", "ns://type", "ns://chat"),
            ("ad4m://chat/1", "ns://message", "literal://string:first"),
        ]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("resolved again on the next pass")
        .to_string();
    assert!(update.contains("ad4m://chat/1"), "{update}");

    // On a later pass: a class is installed and the re-resolution fails.
    install(&mut perspective, &ctx, "Message", MESSAGE_SDNA).await;
    fail_next_index(&perspective.uuid);
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://MessageShape",
            "sh://property",
            "ns://Message.body",
        )]))
        .await;
    drain(&mut rx, &perspective.uuid, &id).await;
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/1",
        serde_json::json!({"body": "second"}),
    )
    .await
    .unwrap();
    perspective
        .check_subscribed_queries(writes(&[
            ("ad4m://message/1", "ns://entry_type", "ns://message"),
            ("ad4m://message/1", "ns://body", "literal://string:second"),
        ]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("the class installed during the failure must be picked up")
        .to_string();
    assert!(update.contains("ad4m://message/1"), "{update}");
}

/// The built-in TextContent (#1446) is read by its URI, never from the
/// store: an app class registered under the name `TextContent`, here with
/// another property, changes neither what the built-in reads nor that it
/// cannot be created. Aliases name it `TextContent`.
#[tokio::test(flavor = "multi_thread")]
async fn the_builtin_text_content_is_read_by_its_uri() {
    use crate::perspectives::model_query::builtin_classes::TEXT_CONTENT_URI;
    const APP_TEXT_CONTENT_SDNA: &str = r#"{
      "target_class":"app://TextContent",
      "abstract":true,
      "properties":[{"path":"app://title","name":"title","max_count":1}]
    }"#;
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[
        ("TextContent", APP_TEXT_CONTENT_SDNA),
        ("Chat", CHAT_SDNA),
        ("Message", MESSAGE_SDNA),
    ])
    .await;
    create(
        &mut perspective,
        &ctx,
        "Chat",
        "ad4m://chat/b",
        serde_json::json!({"text": "chat text"}),
    )
    .await
    .unwrap();
    create(
        &mut perspective,
        &ctx,
        "Message",
        "ad4m://message/b",
        serde_json::json!({"body": "message body"}),
    )
    .await
    .unwrap();

    let builtin = perspective
        .model_query_classes(&one(TEXT_CONTENT_URI), ALL)
        .await
        .unwrap();
    assert_eq!(
        rows(&builtin),
        vec![
            ("ad4m://chat/b".into(), "Chat".into(), "chat text".into()),
            (
                "ad4m://message/b".into(),
                "Message".into(),
                "message body".into()
            ),
        ]
    );

    // The app's own `TextContent` asks for `title`: nothing has it.
    let app: Value = serde_json::from_str(
        &perspective
            .model_query_classes(&one("TextContent"), ALL)
            .await
            .unwrap(),
    )
    .unwrap();
    assert_eq!(app["totalCount"], 0, "{app}");

    assert_eq!(
        perspective
            .predicates_through(TEXT_CONTENT_URI, "text")
            .unwrap(),
        vec!["ns://body".to_string(), "ns://message".to_string()]
    );
    assert_eq!(
        perspective
            .property_through(TEXT_CONTENT_URI, "Message", "text")
            .unwrap(),
        Some("body".to_string())
    );

    let refused = create(
        &mut perspective,
        &ctx,
        TEXT_CONTENT_URI,
        "ad4m://text/b",
        serde_json::json!({"text": "x"}),
    )
    .await
    .unwrap_err();
    assert!(refused.to_string().contains("abstract class"), "{refused}");
    // Nor is a record written through it.
    let refused = perspective
        .update_subject(
            SubjectClassOption {
                class_name: Some(TEXT_CONTENT_URI.into()),
                query: None,
            },
            "ad4m://message/b".into(),
            serde_json::json!({"text": "edited"}),
            None,
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(
        refused.to_string().contains("cannot be updated through it"),
        "{refused}"
    );
    let err = perspective
        .model_query_classes(
            &QueryClasses::union(vec![TEXT_CONTENT_URI.into(), "Chat".into()]),
            ALL,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("abstract class"), "{err}");
}
