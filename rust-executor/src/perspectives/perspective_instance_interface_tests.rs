//! Interface queries on a perspective (#1435): the resolved class list is
//! cached and dropped when SDNA changes, named interfaces are registered
//! through `add_sdna`, and an interface subscription picks up a class
//! installed after it subscribed.
//!
//! As in `perspective_instance_subscription_tests`, the background
//! `subscribed_queries_loop` is not running: `check_subscribed_queries` is
//! driven directly, with the writes each step reports.

use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::perspectives::model_query::{ClassSelector, InterfaceRef, InterfaceSpec};
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
use serde_json::Value;
use tokio::sync::broadcast;

/// A chat message: `text` through `ns://message`.
const CHAT_SDNA: &str = r#"{
  "target_class":"ns://Chat",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://chat"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://chat","min_count":1,"max_count":1},
    {"path":"ns://message","name":"text","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://message","target":"value"}]}
  ]
}"#;

/// A document block: `text` through another predicate, `ns://text`.
const BLOCK_SDNA: &str = r#"{
  "target_class":"ns://Block",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://type","target":"ns://block"}],
  "properties":[
    {"path":"ns://type","name":"type","has_value":"ns://block","min_count":1,"max_count":1},
    {"path":"ns://text","name":"text","min_count":0,"max_count":1,"resolve_language":"literal","setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://text","target":"value"}]}
  ]
}"#;

const TEXT_INTERFACE: &str = r#"{
  "target_class":"ad4m://interface/Text",
  "properties":[{"path":"","name":"text"}]
}"#;

fn has_text() -> InterfaceRef {
    InterfaceRef::AdHoc(
        InterfaceSpec::new(
            vec![
                crate::perspectives::model_query::interfaces::InterfaceProperty {
                    name: "text".into(),
                    through: None,
                    datatype: None,
                },
            ],
            vec![],
        )
        .unwrap(),
    )
}

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
) {
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
        .unwrap_or_else(|e| panic!("create_subject({class}): {e}"));
}

fn result_rows(json: &str) -> Vec<(String, String)> {
    let v: Value = serde_json::from_str(json).unwrap();
    let mut rows: Vec<(String, String)> = v["instances"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| {
            (
                r["id"].as_str().unwrap().to_string(),
                r["__subjectClass"].as_str().unwrap_or("").to_string(),
            )
        })
        .collect();
    rows.sort();
    rows
}

const ALL: &str = r#"{"includeUnverified":true}"#;

/// The resolved list is cached (the same `Arc` comes back) until SDNA
/// changes; a class installed after the first resolution is then included.
#[tokio::test(flavor = "multi_thread")]
async fn the_class_list_is_cached_and_dropped_when_sdna_changes() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("Chat", CHAT_SDNA), ("Task", TASK_SDNA)]).await;

    let first = perspective.classes_implementing(&has_text()).unwrap();
    assert_eq!(*first, vec!["Chat".to_string()]);
    let again = perspective.classes_implementing(&has_text()).unwrap();
    assert!(
        Arc::ptr_eq(&first, &again),
        "a second resolution is served from the cache"
    );

    install(&mut perspective, &ctx, "Block", BLOCK_SDNA).await;
    let after = perspective.classes_implementing(&has_text()).unwrap();
    assert_eq!(*after, vec!["Block".to_string(), "Chat".to_string()]);
}

/// A shape synced in from a peer drops the cache too.
#[tokio::test(flavor = "multi_thread")]
async fn a_synced_shape_drops_the_class_list() {
    let (perspective, _shapes, _ctx) = setup_perspective_no_llm(&[("Chat", CHAT_SDNA)]).await;
    let before = perspective.classes_implementing(&has_text()).unwrap();
    let generation = perspective.shapes_generation();
    perspective.clear_shape_cache();
    assert!(perspective.shapes_generation() > generation);
    let after = perspective.classes_implementing(&has_text()).unwrap();
    assert!(
        !Arc::ptr_eq(&before, &after),
        "resolved again after the shapes changed"
    );
}

/// `model_query_selector` over an ad-hoc interface returns the rows of every
/// implementing class with `__subjectClass`; a class without `text` is out.
/// No implementing class is an empty result, not an error.
#[tokio::test(flavor = "multi_thread")]
async fn an_interface_query_returns_rows_of_every_implementing_class() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[
        ("Chat", CHAT_SDNA),
        ("Block", BLOCK_SDNA),
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
    .await;
    create(
        &mut perspective,
        &ctx,
        "Block",
        "ad4m://block/1",
        serde_json::json!({"text": "notes"}),
    )
    .await;
    create(
        &mut perspective,
        &ctx,
        "Task",
        "ad4m://task/1",
        serde_json::json!({"title": "t", "owner": "o"}),
    )
    .await;

    let json = perspective
        .model_query_selector(&ClassSelector::Interface(has_text()), ALL)
        .await
        .unwrap();
    assert_eq!(
        result_rows(&json),
        vec![
            ("ad4m://block/1".to_string(), "Block".to_string()),
            ("ad4m://chat/1".to_string(), "Chat".to_string()),
        ]
    );

    let nobody = InterfaceRef::AdHoc(
        InterfaceSpec::new(
            vec![
                crate::perspectives::model_query::interfaces::InterfaceProperty {
                    name: "nonexistent".into(),
                    through: None,
                    datatype: None,
                },
            ],
            vec![],
        )
        .unwrap(),
    );
    let empty = perspective
        .model_query_selector(&ClassSelector::Interface(nobody.clone()), ALL)
        .await
        .expect("no implementing class is an empty result");
    let empty: Value = serde_json::from_str(&empty).unwrap();
    assert_eq!(empty["instances"], serde_json::json!([]));
    assert_eq!(empty["totalCount"], 0);

    // The union's limits hold whatever is installed — here, nothing.
    let err = perspective
        .model_query_selector(
            &ClassSelector::Interface(nobody),
            r#"{"parent":{"ids":["ad4m://p"],"predicate":"ad4m://has_child","limitPerAnchor":2}}"#,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("limitPerAnchor"), "{err}");
}

/// A named interface is registered with `add_sdna(.., Interface, shacl)`,
/// read back from its SHACL, and refreshed by registering it again. It is not
/// a subject class and cannot be instantiated. An unknown name is an error.
#[tokio::test(flavor = "multi_thread")]
async fn a_named_interface_is_registered_queried_and_refreshed() {
    let (mut perspective, _shapes, ctx) =
        setup_perspective_no_llm(&[("Chat", CHAT_SDNA), ("Block", BLOCK_SDNA)]).await;
    create(
        &mut perspective,
        &ctx,
        "Chat",
        "ad4m://chat/n",
        serde_json::json!({"text": "hello"}),
    )
    .await;
    create(
        &mut perspective,
        &ctx,
        "Block",
        "ad4m://block/n",
        serde_json::json!({"text": "world"}),
    )
    .await;
    perspective
        .add_sdna(
            "Text".into(),
            String::new(),
            SdnaType::Interface,
            Some(TEXT_INTERFACE.into()),
            &ctx,
        )
        .await
        .expect("add_sdna(Text interface)");

    let named = ClassSelector::Interface(InterfaceRef::Named("Text".into()));
    let json = perspective.model_query_selector(&named, ALL).await.unwrap();
    assert_eq!(
        result_rows(&json),
        vec![
            ("ad4m://block/n".to_string(), "Block".to_string()),
            ("ad4m://chat/n".to_string(), "Chat".to_string()),
        ]
    );

    // Not a class: not listed, not instantiable.
    assert!(!perspective
        .get_subject_classes_from_shacl()
        .await
        .unwrap()
        .contains(&"Text".to_string()));
    let refused = perspective
        .create_subject(
            SubjectClassOption {
                class_name: Some("Text".to_string()),
                query: None,
            },
            "ad4m://text/1".to_string(),
            None,
            None,
            &ctx,
        )
        .await;
    assert!(refused.is_err(), "an interface cannot be instantiated");

    // Refreshed: pinned to Block's predicate, the old rule is gone.
    perspective
        .add_sdna(
            "Text".into(),
            String::new(),
            SdnaType::Interface,
            Some(
                r#"{"target_class":"ad4m://interface/Text",
                    "properties":[{"path":"ns://text","name":"text"}]}"#
                    .into(),
            ),
            &ctx,
        )
        .await
        .expect("re-register Text");
    let json = perspective.model_query_selector(&named, ALL).await.unwrap();
    assert_eq!(
        result_rows(&json),
        vec![("ad4m://block/n".to_string(), "Block".to_string())]
    );

    let unknown = perspective
        .model_query_selector(
            &ClassSelector::Interface(InterfaceRef::Named("Txet".into())),
            ALL,
        )
        .await
        .unwrap_err();
    assert!(
        unknown.to_string().contains("No interface 'Txet'"),
        "{unknown}"
    );

    // An interface needs its SHACL, and must not take a class's URI.
    assert!(perspective
        .add_sdna(
            "Bare".into(),
            String::new(),
            SdnaType::Interface,
            None,
            &ctx
        )
        .await
        .is_err());
    assert!(perspective
        .add_sdna(
            "Chat".into(),
            String::new(),
            SdnaType::Interface,
            Some(r#"{"target_class":"ns://Chat","properties":[{"path":"","name":"text"}]}"#.into()),
            &ctx,
        )
        .await
        .is_err());
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

/// The writes `add_sdna` / `create_subject` report, one link each, as the
/// loop would pass them: a SHACL link, then the new record's flag and text.
fn writes(links: &[(&str, &str, &str)]) -> ChangedPredicates {
    let mut writes = subscription_triggers::Writes::new();
    for (s, p, t) in links {
        writes.record(s, p, t);
    }
    ChangedPredicates::Specific(writes)
}

/// A class installed after an interface subscription was opened starts
/// producing rows: the subscription resolves its classes again when SDNA
/// changes, and its trigger then knows the new class. With the class list
/// frozen at subscribe time, the new class's first record is never pushed.
#[tokio::test(flavor = "multi_thread")]
async fn an_interface_subscription_picks_up_a_class_installed_after_subscribe() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[("Chat", CHAT_SDNA)]).await;
    let (id, _, initial) = perspective
        .model_subscribe_selector(ClassSelector::Interface(has_text()), ALL.into(), "c".into())
        .await
        .expect("interface subscribe");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    // Install Block and let its pass settle: no record yet, so no push.
    install(&mut perspective, &ctx, "Block", BLOCK_SDNA).await;
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://BlockShape",
            "sh://property",
            "ns://Block.text",
        )]))
        .await;
    assert!(
        next_update_for(&mut rx, &perspective.uuid, &id)
            .await
            .is_none(),
        "installing a class changes no row"
    );

    // The write under test, alone: Block's first record.
    create(
        &mut perspective,
        &ctx,
        "Block",
        "ad4m://block/late",
        serde_json::json!({"text": "@data hi"}),
    )
    .await;
    perspective
        .check_subscribed_queries(writes(&[
            ("ad4m://block/late", "ns://type", "ns://block"),
            (
                "ad4m://block/late",
                "ns://text",
                "literal://string:@data hi",
            ),
        ]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("the new class's record must be pushed")
        .to_string();
    assert!(
        update.contains("ad4m://block/late") && update.contains("\"__subjectClass\":\"Block\""),
        "{update}"
    );
}

/// Same, from no implementing class at all at subscribe time.
#[tokio::test(flavor = "multi_thread")]
async fn an_interface_subscription_with_no_class_yet_picks_one_up() {
    let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[("Task", TASK_SDNA)]).await;
    let (id, _, initial) = perspective
        .model_subscribe_selector(ClassSelector::Interface(has_text()), ALL.into(), "c".into())
        .await
        .expect("interface subscribe with no implementing class");
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    let mut rx = get_global_pubsub()
        .await
        .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
        .await;

    install(&mut perspective, &ctx, "Chat", CHAT_SDNA).await;
    perspective
        .check_subscribed_queries(writes(&[(
            "ns://ChatShape",
            "sh://property",
            "ns://Chat.text",
        )]))
        .await;
    assert!(next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .is_none());

    create(
        &mut perspective,
        &ctx,
        "Chat",
        "ad4m://chat/late",
        serde_json::json!({"text": "first"}),
    )
    .await;
    perspective
        .check_subscribed_queries(writes(&[
            ("ad4m://chat/late", "ns://type", "ns://chat"),
            ("ad4m://chat/late", "ns://message", "literal://string:first"),
        ]))
        .await;
    let update = next_update_for(&mut rx, &perspective.uuid, &id)
        .await
        .expect("the new class's record must be pushed")
        .to_string();
    assert!(
        update.contains("ad4m://chat/late") && update.contains("\"__subjectClass\":\"Chat\""),
        "{update}"
    );
}
