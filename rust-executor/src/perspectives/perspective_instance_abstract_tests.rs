//! Abstract classes on a perspective (#1435): writes dispatched to the
//! record's own classes, the resolution cache, reads on their own, and
//! subscriptions that follow the installed shapes. The background
//! `subscribed_queries_loop` is not running: `check_subscribed_queries` is
//! driven directly with the writes each step reports.

use super::*;
use crate::perspectives::interpretation_test_support::{setup_perspective_no_llm, TASK_SDNA};
use crate::perspectives::model_query::QueryClasses;
use crate::pubsub::{get_global_pubsub, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC};
use serde_json::{json, Value};
use tokio::sync::broadcast;

/// Anything with `text`. Its setter writes `ns://text`, which no concrete
/// class reads: a write through it that is not dispatched lands there.
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

/// Flux-like: `body`, aliased `text` for TextContent. Its own predicates,
/// so a trigger built before it was installed cannot match its writes.
const MESSAGE_SDNA: &str = r#"{
  "target_class":"ns://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://entry_type","target":"ns://message"}],
  "destructor_actions":[{"action":"removeLink","source":"this","predicate":"ns://entry_type","target":"*"},{"action":"removeLink","source":"this","predicate":"ns://tag","target":"*"}],
  "properties":[
    {"path":"ns://entry_type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","min_count":0,"max_count":1,"resolve_language":"literal","alias":{"TextContent":"text"},"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://body","target":"value"}]},
    {"path":"ns://tag","name":"tags","resolve_language":"literal","setter":[{"action":"addLink","source":"this","predicate":"ns://tag","target":"value"}]}
  ]
}"#;

/// Message with its alias moved from `body` to `content`.
const MESSAGE_MOVED_SDNA: &str = r#"{
  "target_class":"ns://Message",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://entry_type","target":"ns://message"}],
  "properties":[
    {"path":"ns://entry_type","name":"type","has_value":"ns://message","min_count":1,"max_count":1},
    {"path":"ns://body","name":"body","min_count":0,"max_count":1},
    {"path":"ns://content","name":"content","min_count":0,"max_count":1,"alias":{"TextContent":"text"}}
  ]
}"#;

/// `question`, aliased `text`; `tags` as Message has it. Signed literals:
/// each write mints a new link, so a second append would show.
const POLL_SDNA: &str = r#"{
  "target_class":"ns://Poll",
  "constructor_actions":[{"action":"addLink","source":"this","predicate":"ns://poll","target":"ns://open"}],
  "destructor_actions":[{"action":"removeLink","source":"this","predicate":"ns://poll","target":"*"},{"action":"removeLink","source":"this","predicate":"ns://tag","target":"*"}],
  "properties":[
    {"path":"ns://poll","name":"state","has_value":"ns://open","min_count":1,"max_count":1},
    {"path":"ns://question","name":"question","max_count":1,"alias":{"TextContent":"text"},"setter":[{"action":"setSingleTarget","source":"this","predicate":"ns://question","target":"value"}]},
    {"path":"ns://tag","name":"tags","resolve_language":"literal","setter":[{"action":"addLink","source":"this","predicate":"ns://tag","target":"value"}]}
  ]
}"#;

/// Anything with `tags`.
const TAGGED_SDNA: &str = r#"{
  "target_class":"ns://Tagged",
  "abstract":true,
  "properties":[{"path":"ns://tags","name":"tags","setter":[{"action":"addLink","source":"this","predicate":"ns://tags","target":"value"}]}]
}"#;

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

async fn install(p: &mut PerspectiveInstance, ctx: &AgentContext, class: &str, sdna: &str) {
    p.add_sdna(
        class.into(),
        String::new(),
        SdnaType::SubjectClass,
        Some(sdna.into()),
        ctx,
    )
    .await
    .unwrap_or_else(|e| panic!("add_sdna({class}): {e}"));
}

fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

fn one(class: &str) -> QueryClasses {
    QueryClasses::One(class.to_string())
}

async fn create(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    name: &str,
    base: &str,
    props: Value,
) -> Result<(), AnyError> {
    p.create_subject(class(name), base.into(), Some(props), None, ctx)
        .await
}

async fn update(
    p: &mut PerspectiveInstance,
    ctx: &AgentContext,
    name: &str,
    base: &str,
    values: Value,
) -> Result<(), AnyError> {
    p.update_subject(class(name), base.into(), values, None, ctx)
        .await
}

/// `prop` of the `class` instance at `id`, read as that class.
async fn read(p: &PerspectiveInstance, class: &str, id: &str, prop: &str) -> Value {
    let query = format!(r#"{{"includeUnverified":true,"where":{{"id":"{id}"}}}}"#);
    let json = p.model_query_classes(&one(class), &query).await.unwrap();
    serde_json::from_str::<Value>(&json).unwrap()["instances"][0][prop].clone()
}

fn links_on(p: &PerspectiveInstance, source: &str, predicate: &str) -> usize {
    p.get_links_local(&LinkQuery {
        source: Some(source.into()),
        predicate: Some(predicate.into()),
        ..Default::default()
    })
    .unwrap()
    .len()
}

/// `m1` is a Message, a Poll and a Task (outside TextContent); `m2` only a
/// Message; `t1` only a Task.
async fn dispatch_setup() -> (PerspectiveInstance, AgentContext) {
    let (mut p, _shapes, ctx) = setup_perspective_no_llm(&[
        ("TextContent", TEXT_CONTENT_SDNA),
        ("Tagged", TAGGED_SDNA),
        ("Message", MESSAGE_SDNA),
        ("Poll", POLL_SDNA),
        ("Task", TASK_SDNA),
    ])
    .await;
    let task = json!({"title": "t", "owner": "o"});
    for (class, base, props) in [
        ("Message", "ad4m://m1", json!({"body": "old"})),
        ("Poll", "ad4m://m1", json!({"question": "old?"})),
        ("Task", "ad4m://m1", task.clone()),
        ("Message", "ad4m://m2", json!({"body": "old"})),
        ("Task", "ad4m://t1", task),
    ] {
        create(&mut p, &ctx, class, base, props).await.unwrap();
    }
    (p, ctx)
}

/// An update through an abstract class runs, for each of the record's
/// classes it reads, that class's setter for the property answering to the
/// name: `text` writes Message.body and Poll.question on `m1`, only
/// Message.body on `m2`, never the abstract predicate. Values as given.
#[tokio::test(flavor = "multi_thread")]
async fn an_update_through_an_abstract_class_runs_each_classs_setter() {
    let (mut p, ctx) = dispatch_setup().await;
    for base in ["ad4m://m1", "ad4m://m2"] {
        let text = json!({"text": "<p>new</p>"});
        update(&mut p, &ctx, "TextContent", base, text)
            .await
            .unwrap();
    }
    assert_eq!(read(&p, "Message", "ad4m://m1", "body").await, "<p>new</p>");
    assert_eq!(
        read(&p, "Poll", "ad4m://m1", "question").await,
        "<p>new</p>"
    );
    assert_eq!(read(&p, "Message", "ad4m://m2", "body").await, "<p>new</p>");
    assert_eq!(links_on(&p, "ad4m://m2", "ns://question"), 0);
    for base in ["ad4m://m1", "ad4m://m2"] {
        assert_eq!(links_on(&p, base, "ns://text"), 0, "{base}");
    }
}

/// Message and Poll both write `tags` with `addLink` on `ns://tag`: through
/// Tagged, the append runs once.
#[tokio::test(flavor = "multi_thread")]
async fn a_setter_two_classes_share_runs_once() {
    let (mut p, ctx) = dispatch_setup().await;
    let tags = json!({"tags": ["a"]});
    update(&mut p, &ctx, "Tagged", "ad4m://m1", tags)
        .await
        .unwrap();
    assert_eq!(links_on(&p, "ad4m://m1", "ns://tag"), 1);
    assert_eq!(links_on(&p, "ad4m://m1", "ns://tags"), 0);
}

/// A delete through an abstract class runs the destructors of the record's
/// classes it reads; its Task, outside TextContent, stays.
#[tokio::test(flavor = "multi_thread")]
async fn a_delete_through_an_abstract_class_runs_each_destructor() {
    let (mut p, ctx) = dispatch_setup().await;
    p.remove_subject(class("TextContent"), "ad4m://m1".into(), None, &ctx)
        .await
        .unwrap();
    let classes = p.subject_classes_of(&["ad4m://m1".to_string()]).unwrap();
    assert_eq!(classes["ad4m://m1"], vec!["Task".to_string()]);
}

/// Through an abstract class: a name it does not have, a record of none of
/// its classes and a create are refused, and nothing is written.
#[tokio::test(flavor = "multi_thread")]
async fn writes_through_an_abstract_class_that_cannot_dispatch_are_refused() {
    let (mut p, ctx) = dispatch_setup().await;
    let body = json!({"body": "x"});
    let err = update(&mut p, &ctx, "TextContent", "ad4m://m2", body).await;
    let err = err.unwrap_err().to_string();
    assert!(
        err.contains("'body' is not a name of the abstract class"),
        "{err}"
    );
    assert_eq!(read(&p, "Message", "ad4m://m2", "body").await, "old");

    let text = json!({"text": "x"});
    let updated = update(&mut p, &ctx, "TextContent", "ad4m://t1", text).await;
    let deleted = p
        .remove_subject(class("TextContent"), "ad4m://t1".into(), None, &ctx)
        .await;
    for err in [updated.unwrap_err(), deleted.unwrap_err()] {
        let err = err.to_string();
        assert!(
            err.contains("is not a record of any class 'TextContent' reads"),
            "{err}"
        );
    }
    assert_eq!(links_on(&p, "ad4m://t1", "ns://text"), 0);
    assert_eq!(read(&p, "Task", "ad4m://t1", "title").await, "t");

    let text = json!({"text": "x"});
    let err = create(&mut p, &ctx, "TextContent", "ad4m://x", text).await;
    let err = err.unwrap_err().to_string();
    assert!(err.contains("cannot be created"), "{err}");
    assert_eq!(links_on(&p, "ad4m://x", "ns://text"), 0);
}

/// Read on its own: in a union of several classes, refused. No conforming
/// class is an empty result; what a union refuses is refused even then.
#[tokio::test(flavor = "multi_thread")]
async fn an_abstract_class_is_read_on_its_own() {
    let (p, _shapes, _ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Task", TASK_SDNA)]).await;
    let union = QueryClasses::union(vec!["TextContent".into(), "Task".into()]);
    let err = p.model_query_classes(&union, ALL).await.unwrap_err();
    assert!(err.to_string().contains("abstract class"), "{err}");

    let empty = p.model_query_classes(&one("TextContent"), ALL).await;
    let empty: Value = serde_json::from_str(&empty.unwrap()).unwrap();
    assert_eq!(empty["totalCount"], 0, "{empty}");

    let scoped =
        r#"{"parent":{"ids":["ad4m://p"],"predicate":"ad4m://has_child","limitPerAnchor":2}}"#;
    let err = p.model_query_classes(&one("TextContent"), scoped).await;
    assert!(err.unwrap_err().to_string().contains("limitPerAnchor"));
}

/// The resolution is cached (the same `Arc`) until the installed shapes
/// change: a class installed later is then included, one re-registered as
/// abstract drops out, and a synced shape drops the cache too.
#[tokio::test(flavor = "multi_thread")]
async fn the_resolution_is_cached_and_dropped_when_shapes_change() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    let resolve = |p: &PerspectiveInstance| p.resolve_abstract("TextContent").unwrap();
    let first = resolve(&p);
    assert_eq!(first.classes, vec!["Chat"]);
    assert!(Arc::ptr_eq(&first, &resolve(&p)), "served from the cache");

    install(&mut p, &ctx, "Message", MESSAGE_SDNA).await;
    assert_eq!(resolve(&p).classes, vec!["Chat", "Message"]);

    let abstract_chat = CHAT_SDNA.replacen("{\n", "{\"abstract\":true,\n", 1);
    install(&mut p, &ctx, "Chat", &abstract_chat).await;
    assert!(p.abstract_index().unwrap().is_abstract("Chat"));
    assert_eq!(resolve(&p).classes, vec!["Message"]);
    // And back: the refresh purges the marker.
    install(&mut p, &ctx, "Chat", CHAT_SDNA).await;
    assert!(!p.abstract_index().unwrap().is_abstract("Chat"));

    let before = resolve(&p);
    p.clear_shape_cache();
    assert!(!Arc::ptr_eq(&before, &resolve(&p)), "resolved again");
}

fn link(source: &str, predicate: &str, target: &str) -> Link {
    Link {
        source: source.into(),
        predicate: Some(predicate.into()),
        target: target.into(),
    }
}

/// A subscription over TextContent, its passes driven by hand.
struct Sub {
    id: String,
    rx: broadcast::Receiver<String>,
}

impl Sub {
    async fn open(p: &PerspectiveInstance) -> (Sub, String) {
        let (id, _, initial) = p
            .model_subscribe_and_query(one("TextContent"), ALL.into(), "c".into())
            .await
            .expect("subscribe");
        let pubsub = get_global_pubsub().await;
        let rx = pubsub
            .subscribe(&PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC)
            .await;
        (Sub { id, rx }, initial)
    }

    /// One pass over the writes a step reports; the update it pushed, if
    /// any, within 5 s.
    async fn pass(
        &mut self,
        p: &PerspectiveInstance,
        links: &[(&str, &str, &str)],
    ) -> Option<String> {
        let mut writes = subscription_triggers::Writes::new();
        for (s, pr, t) in links {
            writes.record(s, pr, t);
        }
        p.check_subscribed_queries(ChangedPredicates::Specific(writes))
            .await;
        let wait = async {
            loop {
                let msg = self.rx.recv().await.expect("pubsub receiver closed");
                let update: Value = serde_json::from_str(&msg).unwrap();
                if update["perspectiveUuid"] == p.uuid && update["subscriptionId"] == self.id {
                    return update.to_string();
                }
            }
        };
        tokio::time::timeout(Duration::from_secs(5), wait)
            .await
            .ok()
    }
}

/// A subscription over an abstract class follows the installed shapes:
/// installing a class re-runs it whatever was written, so records written
/// before their class are pushed; its trigger then watches the new class's
/// predicates; and an alias that moves (same class list) moves the trigger.
#[tokio::test(flavor = "multi_thread")]
async fn a_subscription_follows_the_installed_shapes() {
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;
    let early = "ad4m://message/early";
    let raw = vec![
        link(early, "ns://entry_type", "ns://message"),
        link(early, "ns://body", "literal://string:early"),
    ];
    p.add_links(raw, LinkStatus::Shared, None, &ctx)
        .await
        .unwrap();
    let (mut sub, initial) = Sub::open(&p).await;
    assert!(initial.contains("\"totalCount\":0"), "{initial}");

    // Message's SHACL, alone: its predicates are no record's.
    install(&mut p, &ctx, "Message", MESSAGE_SDNA).await;
    let shape = [("ns://MessageShape", "sh://property", "ns://Message.body")];
    let update = sub.pass(&p, &shape).await.expect("records already there");
    assert!(
        update.contains(early) && update.contains("\"text\""),
        "{update}"
    );

    // A new Message, alone in its pass: only a rebuilt trigger sees it.
    let late = "ad4m://message/late";
    let body = json!({"body": "@data hi"});
    create(&mut p, &ctx, "Message", late, body).await.unwrap();
    let writes = [
        (late, "ns://entry_type", "ns://message"),
        (late, "ns://body", "literal://string:@data hi"),
    ];
    let update = sub.pass(&p, &writes).await.expect("the new class's record");
    assert!(update.contains(late) && update.contains("\"__subjectClass\":\"Message\""));

    // The alias moves to `content`, which only the new view reads.
    install(&mut p, &ctx, "Message", MESSAGE_MOVED_SDNA).await;
    let shape = [("ns://MessageShape", "sh://property", "ns://Message.content")];
    sub.pass(&p, &shape).await;
    let moved = vec![link(early, "ns://content", "literal:string:moved")];
    p.add_links(moved, LinkStatus::Shared, None, &ctx)
        .await
        .unwrap();
    let writes = [(early, "ns://content", "literal:string:moved")];
    let update = sub
        .pass(&p, &writes)
        .await
        .expect("a write to the moved alias");
    assert!(update.contains("\"text\":\"moved\""), "{update}");
}

/// A store error while a subscription's classes resolve, at subscribe time
/// or on a later pass, is retried on the next pass. Kept, it would watch the
/// abstract class's own predicates, which no class writes.
#[tokio::test(flavor = "multi_thread")]
async fn a_failed_resolution_is_tried_again_on_the_next_pass() {
    use super::abstract_classes::faults::fail_next_index;
    let (mut p, _shapes, ctx) =
        setup_perspective_no_llm(&[("TextContent", TEXT_CONTENT_SDNA), ("Chat", CHAT_SDNA)]).await;

    fail_next_index(&p.uuid);
    let (mut sub, initial) = Sub::open(&p).await;
    assert!(initial.contains("\"totalCount\":0"), "{initial}");
    let chat = json!({"text": "first"});
    create(&mut p, &ctx, "Chat", "ad4m://chat/1", chat)
        .await
        .unwrap();
    let writes = [
        ("ad4m://chat/1", "ns://type", "ns://chat"),
        ("ad4m://chat/1", "ns://message", "literal://string:first"),
    ];
    let update = sub
        .pass(&p, &writes)
        .await
        .expect("resolved on the next pass");
    assert!(update.contains("ad4m://chat/1"), "{update}");

    // A class is installed and its re-resolution fails.
    install(&mut p, &ctx, "Message", MESSAGE_SDNA).await;
    fail_next_index(&p.uuid);
    let shape = [("ns://MessageShape", "sh://property", "ns://Message.body")];
    sub.pass(&p, &shape).await;
    let body = json!({"body": "second"});
    create(&mut p, &ctx, "Message", "ad4m://message/1", body)
        .await
        .unwrap();
    let writes = [
        ("ad4m://message/1", "ns://entry_type", "ns://message"),
        ("ad4m://message/1", "ns://body", "literal://string:second"),
    ];
    let update = sub
        .pass(&p, &writes)
        .await
        .expect("the class installed meanwhile");
    assert!(update.contains("ad4m://message/1"), "{update}");
}

/// The built-in TextContent (#1446) is read by its URI, never from the
/// store: an app class registered under the name `TextContent`, here with
/// another property, changes neither what the built-in reads, nor that it
/// cannot be created, nor where an update through it lands. Aliases name it
/// `TextContent`.
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
    // An update through it dispatches to the record's own class, under the
    // name the class aliases to `TextContent`; never to the built-in's
    // predicate.
    perspective
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
        .unwrap();
    assert_eq!(
        read(&perspective, "Message", "ad4m://message/b", "body").await,
        "edited"
    );
    assert_eq!(links_on(&perspective, "ad4m://message/b", "ad4m://text"), 0);
    // A delete through it runs the record's own destructor.
    perspective
        .remove_subject(
            SubjectClassOption {
                class_name: Some(TEXT_CONTENT_URI.into()),
                query: None,
            },
            "ad4m://message/b".into(),
            None,
            &ctx,
        )
        .await
        .unwrap();
    let classes = perspective
        .subject_classes_of(&["ad4m://message/b".to_string(), "ad4m://chat/b".to_string()])
        .unwrap();
    assert_eq!(classes.get("ad4m://message/b"), None, "{classes:?}");
    assert_eq!(classes["ad4m://chat/b"], vec!["Chat".to_string()]);
    let err = perspective
        .model_query_classes(
            &QueryClasses::union(vec![TEXT_CONTENT_URI.into(), "Chat".into()]),
            ALL,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("abstract class"), "{err}");
}
