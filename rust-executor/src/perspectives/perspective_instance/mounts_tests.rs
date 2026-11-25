//! The mount engine (design §1, §2): `mount(lang)` installs the language's
//! `mountShapes` and mirrors its `mountSnapshot` records as `Local` links —
//! model queries and subscriptions then see them like any other instances.
//! `mount-test` stands in for Acorn's language; its `project` address
//! serves `outsideChange`/`outsideRemove`/`setVersion` to drive syncs
//! without depending on signals.

use super::subscription_triggers;
use super::*;
use crate::languages::mount_tests::{interact, load_mount_test_language};
use crate::languages::mount_types::MountSyncReport;
use crate::perspectives::model_query::QueryClasses;
use crate::pubsub::get_global_pubsub;
use serde_json::{json, Value};
use tokio::sync::broadcast;

pub(super) fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

pub(super) const UNVERIFIED: &str = r#"{"includeUnverified":false}"#;

/// A private perspective with `lang` mounted.
pub(super) async fn mounted(lang: &str) -> (PerspectiveInstance, AgentContext) {
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.mount(lang).await.unwrap();
    (p, ctx)
}

/// A private perspective with `lang` mounted, registered in the global
/// map so `get_perspective` — and with it the pushed-delta routing —
/// finds it. Remove it with `crate::perspectives::remove_perspective`.
pub(super) async fn mounted_and_registered(lang: &str) -> (PerspectiveInstance, AgentContext) {
    let (p, ctx) = mounted(lang).await;
    crate::perspectives::register_perspective(p.uuid.clone(), p.clone());
    (p, ctx)
}

/// Poll `things` every 50 ms until `done` holds or `within` elapses;
/// returns the last result either way.
async fn eventually(
    p: &PerspectiveInstance,
    within: Duration,
    done: impl Fn(&[Value]) -> bool,
) -> Vec<Value> {
    let deadline = tokio::time::Instant::now() + within;
    loop {
        let instances = things(p).await;
        if done(&instances) || tokio::time::Instant::now() >= deadline {
            return instances;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
}

/// `model_query("MountThing")`'s instances parsed to JSON.
pub(super) async fn things(p: &PerspectiveInstance) -> Vec<Value> {
    let result = p
        .model_query("MountThing", UNVERIFIED)
        .await
        .expect("model_query");
    serde_json::from_str::<Value>(&result).unwrap()["instances"]
        .as_array()
        .unwrap()
        .clone()
}

/// Create a record straight in the language — the perspective learns of it
/// through `mount`/`sync_mount`, not through this call.
pub(super) async fn create(lang: &str, params: Value) {
    interact(&format!("{lang}://class/MountThing"), "create", params)
        .await
        .unwrap();
}

/// Every call the language has served, in order: `{address, name, params}`.
async fn calls(lang: &str) -> Vec<Value> {
    interact(&format!("{lang}://project"), "calls", json!({}))
        .await
        .unwrap()
        .as_array()
        .unwrap()
        .clone()
}

/// Every stored link whose source sits under `lang`.
pub(super) async fn lang_links(
    p: &PerspectiveInstance,
    lang: &str,
) -> Vec<DecoratedLinkExpression> {
    let prefix = format!("{lang}://");
    p.get_links(&LinkQuery::default())
        .await
        .unwrap()
        .into_iter()
        .filter(|l| l.data.source.starts_with(&prefix))
        .collect()
}

#[tokio::test]
async fn mounted_records_are_instances() {
    let lang = load_mount_test_language().await;
    create(
        &lang,
        json!({"title": "a", "done": true, "tags": ["x", "y"]}),
    )
    .await;
    let (p, _ctx) = mounted(&lang).await;

    let instances = things(&p).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    let thing = &instances[0];
    assert_eq!(thing["id"], json!(format!("{lang}://t1")));
    assert_eq!(thing["title"], "a");
    assert_eq!(thing["done"], true);
    assert_eq!(thing["tags"], json!(["x", "y"]));
}

#[tokio::test]
async fn mounted_links_are_local_and_never_committed() {
    let lang = load_mount_test_language().await;
    create(
        &lang,
        json!({"title": "a", "done": true, "tags": ["x", "y"]}),
    )
    .await;
    let (p, _ctx) = mounted(&lang).await;

    // Every link under the language's address space is a Local cache link —
    // including the mount's own bookkeeping under `<lang>://project`.
    let links = lang_links(&p, &lang).await;
    assert!(!links.is_empty());
    for link in &links {
        assert_eq!(
            link.status,
            Some(LinkStatus::Local),
            "mounted link must be Local: {:?}",
            link.data
        );
    }

    // The mount link is Local too: a mount is this agent's own.
    let mount_links = p
        .get_links(&LinkQuery {
            source: Some("ad4m://self".to_string()),
            predicate: Some("ad4m://mount".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(mount_links.len(), 1);
    assert_eq!(mount_links[0].status, Some(LinkStatus::Local));

    // No mount write may have queued anything for a commit.
    let (diff, _ids) = crate::db_backend::db_backend()
        .get_pending_diffs(&p.uuid, None)
        .unwrap();
    assert!(
        diff.additions.is_empty() && diff.removals.is_empty(),
        "mount writes queued pending diffs: {diff:?}"
    );
}

#[tokio::test]
async fn a_shared_mount_link_mounts_nothing() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    // What a peer would share: a mount link that is not this agent's.
    p.add_links(
        vec![Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://mount".to_string()),
            target: format!("{lang}://project"),
        }],
        LinkStatus::Shared,
        None,
        &ctx,
    )
    .await
    .unwrap();

    p.restore_mounts().await;
    let registered = crate::perspectives::perspectives_for_mount(&lang);
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(registered.is_empty(), "a shared mount link was activated");
    assert!(p.mounts().await.unwrap().is_empty());
    assert!(
        lang_links(&p, &lang).await.is_empty(),
        "records were mirrored"
    );
    assert!(p.sync_mount(&lang).await.is_err(), "a sync ran on it");
}

#[tokio::test]
async fn sync_applies_a_change_as_one_link() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "b"}, "emit": false}),
    )
    .await
    .unwrap();

    let report = p.sync_mount(&lang).await.unwrap();
    assert_eq!(
        report,
        MountSyncReport {
            added: 1,
            removed: 1
        }
    );

    let instances = things(&p).await;
    assert_eq!(instances.len(), 1);
    assert_eq!(instances[0]["title"], "b");
}

#[tokio::test]
async fn sync_removes_a_vanished_record() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    interact(
        &format!("{lang}://project"),
        "outsideRemove",
        json!({"address": "t1", "emit": false}),
    )
    .await
    .unwrap();

    p.sync_mount(&lang).await.unwrap();
    assert!(things(&p).await.is_empty());
    assert!(
        p.get_links(&LinkQuery {
            source: Some(format!("{lang}://t1")),
            ..Default::default()
        })
        .await
        .unwrap()
        .is_empty(),
        "a link under {lang}://t1 survived the sync"
    );
}

#[tokio::test]
async fn create_subject_is_visible_before_it_returns() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted(&lang).await;

    let url = p
        .create_subject(
            class("MountThing"),
            "ignored".to_string(),
            Some(json!({"title": "n"})),
            None,
            &ctx,
        )
        .await
        .unwrap();
    let url = url.expect("a call constructor names its record");

    let instances = things(&p).await;
    let thing = instances
        .iter()
        .find(|i| i["id"] == json!(url))
        .expect("the created record is not queryable yet");
    assert_eq!(thing["title"], "n");
}

#[tokio::test]
async fn unmount_leaves_nothing_under_the_language() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    p.unmount(&lang).await.unwrap();

    assert!(lang_links(&p, &lang).await.is_empty());
    // The mount link itself is gone.
    assert!(p
        .get_links(&LinkQuery {
            predicate: Some("ad4m://mount".to_string()),
            ..Default::default()
        })
        .await
        .unwrap()
        .is_empty());
    assert!(p.mounts().await.unwrap().is_empty());
    assert!(crate::perspectives::perspectives_for_mount(&lang).is_empty());
}

#[tokio::test]
async fn a_mount_survives_a_restart() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;

    // "Restart" only means something if the store is file-backed: tests
    // leave APP_DATA_PATH unset (in-memory stores), so point it at a temp
    // dir for this test and put it back afterwards.
    let dir = tempfile::tempdir().unwrap();
    let previous = crate::perspectives::get_app_data_path();
    crate::perspectives::set_app_data_path(dir.path().to_str().unwrap().to_string());

    let check = async {
        let (p, _ctx) = mounted(&lang).await;

        // "Restart": drop the instance, then build a new one on the same
        // uuid — the same RocksDB path. Tasks spawned by the mount's writes
        // hold clones of the instance, so hold a handle on the store and
        // wait for their clones to go before reopening (a second store on
        // the path while one is open is a RocksDB LOCK failure).
        let handle = p.persisted.lock().await.clone();
        let uuid = handle.uuid.clone();
        let store = p.sparql_store.clone();
        crate::perspectives::unregister_mount(&lang, &uuid);
        drop(p);
        for _ in 0..200 {
            if std::sync::Arc::strong_count(&store) == 1 {
                break;
            }
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        drop(store);

        let mut reopened = PerspectiveInstance::new(handle, None);
        reopened.restore_mounts().await;

        let instances = things(&reopened).await;
        assert_eq!(instances.len(), 1, "{instances:?}");
        assert_eq!(instances[0]["title"], "a");
        assert_eq!(
            crate::perspectives::perspectives_for_mount(&lang),
            vec![uuid.clone()]
        );
        crate::perspectives::unregister_mount(&lang, &uuid);
    }
    .await;
    *crate::perspectives::APP_DATA_PATH.write().unwrap() = previous;
    check
}

#[tokio::test]
async fn a_subscription_fires_on_sync() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    let subscription_id = uuid::Uuid::new_v4().to_string();
    let mut rx = get_global_pubsub().await.subscribe_events();
    let (id, _rev, initial) = p
        .model_subscribe_and_query(
            &crate::perspectives::sparql_store::EvalStop::none(),
            QueryClasses::One("MountThing".to_string()),
            UNVERIFIED.to_string(),
            subscription_id,
            "c".to_string(),
            None,
        )
        .await
        .expect("subscribe");
    let initial: Value = serde_json::from_str(&initial).unwrap();
    assert_eq!(initial["instances"][0]["title"], "a");

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "b"}, "emit": false}),
    )
    .await
    .unwrap();
    p.sync_mount(&lang).await.unwrap();

    // subscribed_queries_loop is not running; drive one pass over the
    // links the sync wrote (its -a +b title change).
    let mut writes = subscription_triggers::Writes::new();
    writes.record(
        &format!("{lang}://t1"),
        "mounttest://title",
        "literal:string:a",
    );
    writes.record(
        &format!("{lang}://t1"),
        "mounttest://title",
        "literal:string:b",
    );
    p.check_subscribed_queries(ChangedPredicates::Specific(writes))
        .await;

    let update = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let event = match rx.recv().await {
                Ok(event) => event,
                Err(broadcast::error::RecvError::Lagged(_)) => continue,
                Err(e) => panic!("pubsub receiver closed: {e}"),
            };
            if event.topic == Topic::PerspectiveQuerySubscription
                && event.payload["subscriptionId"] == id
            {
                return event.payload.to_string();
            }
        }
    })
    .await
    .expect("the subscription never pushed the sync's change");
    assert!(
        update.contains("\"b\""),
        "update misses the new title: {update}"
    );
}

#[tokio::test]
async fn a_second_mount_of_the_same_classes_is_refused() {
    let lang = load_mount_test_language().await;
    let lang2 = load_mount_test_language().await;
    let (mut p, _ctx) = mounted(&lang).await;

    let error = p.mount(&lang2).await.unwrap_err();
    assert!(
        error.to_string().contains("MountThing"),
        "the refusal must name the colliding class: {error}"
    );
}

#[tokio::test]
async fn a_language_without_mount_exports_is_refused() {
    crate::test_utils::init_v8_platform();
    crate::test_utils::test_languages_directory();
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();

    let source = format!(
        "export const name = \"plain\"; export async function init() {{}}\n// instance {}\n",
        uuid::Uuid::new_v4()
    );
    let controller = LanguageController::global_instance();
    let (_, path) = controller.save_language_bundle(&source, None).unwrap();
    let lang = controller.load_language(path, false).await.unwrap();

    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    let error = p.mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("cannot be mounted"), "{error}");
    assert!(
        p.get_links(&LinkQuery {
            predicate: Some("ad4m://mount".to_string()),
            ..Default::default()
        })
        .await
        .unwrap()
        .is_empty(),
        "a refused mount wrote a mount link"
    );
}

#[tokio::test]
async fn a_version_change_reinstalls_shapes() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    interact(
        &format!("{lang}://project"),
        "setVersion",
        json!({"version": "3"}),
    )
    .await
    .unwrap();
    p.restore_mounts().await;

    let versions = p
        .get_links(&LinkQuery {
            source: Some(format!("{lang}://project")),
            predicate: Some("ad4m://mount_shapes_version".to_string()),
            ..Default::default()
        })
        .await
        .unwrap();
    assert_eq!(versions.len(), 1, "{versions:?}");
    assert_eq!(versions[0].data.target, "literal:string:3");
}

/// Design R4: the first sync of a large language must not take minutes.
/// Ignored (runs on demand): it creates 2 000 records.
#[tokio::test]
#[ignore]
async fn two_thousand_records_first_sync() {
    let lang = load_mount_test_language().await;
    for i in 0..2000 {
        create(
            &lang,
            json!({"title": format!("record {i}"), "done": i % 2 == 0, "tags": ["a", "b"]}),
        )
        .await;
    }
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let start = std::time::Instant::now();
    p.mount(&lang).await.unwrap();
    let first_sync = start.elapsed();
    println!("two_thousand_records_first_sync: mount+first sync took {first_sync:?}");

    assert_eq!(things(&p).await.len(), 2000);
}

/// Design §1.5 path 2: a `mountDelta` the language pushes through
/// `emitSignal` lands in the mounting perspective — no `sync_mount` call.
/// Break check: dropping the `mountDelta` branch in `ad4m_signal_emitted`
/// turns this red.
#[tokio::test]
async fn a_pushed_delta_shows_without_sync() {
    let lang = load_mount_test_language().await;
    let (p, _ctx) = mounted_and_registered(&lang).await;

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "pushed"}, "emit": true}),
    )
    .await
    .unwrap();

    let instances = eventually(&p, Duration::from_secs(3), |i| {
        i.len() == 1 && i[0]["title"] == "pushed"
    })
    .await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["title"], "pushed");

    crate::perspectives::remove_perspective(&p.uuid).await;
}

#[tokio::test]
async fn a_pushed_removal_shows_without_sync() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted_and_registered(&lang).await;
    assert_eq!(things(&p).await.len(), 1);

    interact(
        &format!("{lang}://project"),
        "outsideRemove",
        json!({"address": "t1", "emit": true}),
    )
    .await
    .unwrap();

    let instances = eventually(&p, Duration::from_secs(3), |i| i.is_empty()).await;
    assert!(instances.is_empty(), "{instances:?}");

    crate::perspectives::remove_perspective(&p.uuid).await;
}

/// Design §1.5 path 3: a change the language did not signal is caught by
/// the resync loop on its next tick.
#[tokio::test]
async fn a_silent_change_shows_after_the_resync_interval() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted(&lang).await;
    super::mounts::set_mount_resync_interval_for_test(Duration::from_secs(1));
    let instance = p.clone();
    let loop_handle = tokio::spawn(async move { instance.mount_resync_loop().await });

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "silent"}, "emit": false}),
    )
    .await
    .unwrap();

    let instances = eventually(&p, Duration::from_secs(3), |i| {
        i.len() == 1 && i[0]["title"] == "silent"
    })
    .await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["title"], "silent");

    p.teardown_background_tasks().await;
    loop_handle.await.unwrap();
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    super::mounts::set_mount_resync_interval_for_test(Duration::from_secs(15));
}

#[tokio::test]
async fn removing_the_perspective_stops_the_loop() {
    let lang = load_mount_test_language().await;
    let (p, _ctx) = mounted(&lang).await;
    super::mounts::set_mount_resync_interval_for_test(Duration::from_millis(200));
    let instance = p.clone();
    let loop_handle = tokio::spawn(async move { instance.mount_resync_loop().await });

    p.teardown_background_tasks().await;

    let stopped = tokio::time::timeout(Duration::from_secs(1), loop_handle).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    super::mounts::set_mount_resync_interval_for_test(Duration::from_secs(15));
    assert!(
        stopped.is_ok(),
        "the resync loop did not stop within 1 s of teardown"
    );
}

/// A `mountDelta` that does not parse is dropped with a warning: the
/// perspective keeps its records and the language keeps working.
#[tokio::test]
async fn an_invalid_delta_is_dropped_with_a_warning() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted_and_registered(&lang).await;

    interact(
        &format!("{lang}://project"),
        "emitRaw",
        json!({"mountDelta": {"changed": "nope"}}),
    )
    .await
    .unwrap();

    // Were a malformed payload routed, its apply would be async — give it
    // a moment to land before checking the store is unchanged.
    tokio::time::sleep(Duration::from_millis(300)).await;
    let instances = things(&p).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["title"], "a");

    // The language is unharmed: a `create` still answers with its delta.
    let result = interact(
        &format!("{lang}://class/MountThing"),
        "create",
        json!({"title": "b"}),
    )
    .await
    .unwrap();
    assert_eq!(result["address"], "t2");

    crate::perspectives::remove_perspective(&p.uuid).await;
}

#[tokio::test]
async fn a_delta_reaches_every_perspective_that_mounts_the_language() {
    let lang = load_mount_test_language().await;
    let (p1, _c1) = mounted_and_registered(&lang).await;
    let (p2, _c2) = mounted_and_registered(&lang).await;

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "for all"}, "emit": true}),
    )
    .await
    .unwrap();

    for p in [&p1, &p2] {
        let instances = eventually(p, Duration::from_secs(3), |i| {
            i.len() == 1 && i[0]["title"] == "for all"
        })
        .await;
        assert_eq!(instances.len(), 1, "{instances:?}");
        assert_eq!(instances[0]["title"], "for all");
    }

    crate::perspectives::remove_perspective(&p1.uuid).await;
    crate::perspectives::remove_perspective(&p2.uuid).await;
}

/// `model_query` instances of `class_name` parsed to JSON (the `things`
/// helper for one class).
async fn query_instances(p: &PerspectiveInstance, class_name: &str) -> Vec<Value> {
    let result = p
        .model_query(class_name, UNVERIFIED)
        .await
        .expect("model_query");
    serde_json::from_str::<Value>(&result).unwrap()["instances"]
        .as_array()
        .unwrap()
        .clone()
}

/// Design §3.5: a mounted record reads through the built-in abstract class
/// `ad4m://TextContent` — `MountThing.title` answers to `text` through the
/// shape's `TextContent` alias.
#[tokio::test]
async fn mounted_records_read_as_text_content() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "hello"})).await;
    let (p, _ctx) = mounted(&lang).await;

    let instances = query_instances(&p, "ad4m://TextContent").await;
    let row = instances
        .iter()
        .find(|i| i["id"] == json!(format!("{lang}://t1")))
        .unwrap_or_else(|| panic!("the mounted record is not a TextContent row: {instances:?}"));
    assert_eq!(row["text"], "hello", "{row}");
}

/// The break test of design §3.5: a write through `ad4m://TextContent`
/// reaches the language as one `update` call keyed by the *concrete*
/// property name (`title`, not `text`), and the call's delta lands in the
/// store without a `sync_mount`. Passing the abstract name `text` to the
/// call turns this red.
#[tokio::test]
async fn a_text_content_write_is_one_call_with_the_concrete_key() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "hello"})).await;
    let (mut p, ctx) = mounted(&lang).await;

    p.update_subject(
        class("ad4m://TextContent"),
        format!("{lang}://t1"),
        json!({"text": "bye"}),
        None,
        &ctx,
    )
    .await
    .unwrap();

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 1, "one grouped update call: {calls:?}");
    assert_eq!(updates[0]["address"], "t1", "{calls:?}");
    assert_eq!(
        updates[0]["params"],
        json!({"title": "bye"}),
        "the call's params must key the concrete name `title`, not `text`: {calls:?}"
    );

    let instances = things(&p).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(
        instances[0]["title"], "bye",
        "the call's delta was applied without a sync_mount"
    );
}

/// Two writes through `ad4m://TextContent` are two calls, applied in order.
#[tokio::test]
async fn two_writes_through_text_content_in_a_row() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "hello"})).await;
    let (mut p, ctx) = mounted(&lang).await;

    for text in ["a", "b"] {
        p.update_subject(
            class("ad4m://TextContent"),
            format!("{lang}://t1"),
            json!({"text": text}),
            None,
            &ctx,
        )
        .await
        .unwrap();
    }

    let calls = calls(&lang).await;
    let updates: Vec<&Value> = calls.iter().filter(|c| c["name"] == "update").collect();
    assert_eq!(updates.len(), 2, "{calls:?}");
    assert_eq!(updates[0]["params"], json!({"title": "a"}));
    assert_eq!(updates[1]["params"], json!({"title": "b"}));

    let instances = things(&p).await;
    assert_eq!(instances[0]["title"], "b", "{instances:?}");
}

/// `MountThing.children` is a `mounttest://child` relation aliased to
/// `Container.children`: a mounted record holding a child reads through
/// `ad4m://Container`.
#[tokio::test]
async fn mounted_records_read_as_containers() {
    let lang = load_mount_test_language().await;
    create(
        &lang,
        json!({"title": "parent", "children": [format!("{lang}://t2")]}),
    )
    .await;
    create(&lang, json!({"title": "kid"})).await;
    let (p, _ctx) = mounted(&lang).await;

    let instances = query_instances(&p, "ad4m://Container").await;
    let row = instances
        .iter()
        .find(|i| i["id"] == json!(format!("{lang}://t1")))
        .unwrap_or_else(|| panic!("the mounted record is not a Container row: {instances:?}"));
    assert_eq!(row["children"], json!([format!("{lang}://t2")]), "{row}");
}

/// `ad4m://TextContent` names what a record has, not what it is: creating
/// through it is refused (`refuse_abstract_create`) and no call is made.
#[tokio::test]
async fn creating_through_text_content_is_still_refused() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "hello"})).await;
    let (mut p, ctx) = mounted(&lang).await;

    let before = calls(&lang).await;
    let err = p
        .create_subject(
            class("ad4m://TextContent"),
            "ignored".to_string(),
            Some(json!({"text": "new"})),
            None,
            &ctx,
        )
        .await
        .unwrap_err();
    assert!(err.to_string().contains("abstract class"), "{err}");
    assert_eq!(calls(&lang).await, before, "a refused create made a call");
}

/// A managed user can write a `Local` link into a perspective they own. A
/// mount link written that way is not marked in the mount ledger, so no
/// restore, resync tick or sync acts on it.
#[tokio::test]
async fn a_managed_users_raw_mount_link_mounts_nothing() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    let email = format!("alice-{}@example.org", uuid::Uuid::new_v4().simple());
    let alice = AgentContext::managed(crate::test_utils::create_test_user(&email));
    p.add_link(
        Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://mount".to_string()),
            target: format!("{lang}://project"),
        },
        LinkStatus::Local,
        None,
        &alice,
    )
    .await
    .unwrap();

    p.restore_mounts().await;
    let registered = crate::perspectives::perspectives_for_mount(&lang);
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(registered.is_empty(), "the raw mount link was activated");
    assert!(p.mounts().await.unwrap().is_empty());
    assert!(p.sync_mount(&lang).await.is_err(), "a sync ran on it");

    // The resync loop activates nothing either.
    super::mounts::set_mount_resync_interval_for_test(Duration::from_millis(200));
    let instance = p.clone();
    let loop_handle = tokio::spawn(async move { instance.mount_resync_loop().await });
    tokio::time::sleep(Duration::from_millis(900)).await;
    let registered = crate::perspectives::perspectives_for_mount(&lang);
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    p.teardown_background_tasks().await;
    loop_handle.await.unwrap();
    super::mounts::set_mount_resync_interval_for_test(Duration::from_secs(15));
    assert!(registered.is_empty(), "the resync loop activated it");
    assert!(
        lang_links(&p, &lang).await.is_empty(),
        "records were mirrored"
    );
}

/// `mount` next to a raw mount link still mounts, through its own marked
/// link.
#[tokio::test]
async fn mount_works_next_to_a_raw_mount_link() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_link(
        Link {
            source: "ad4m://self".to_string(),
            predicate: Some("ad4m://mount".to_string()),
            target: format!("{lang}://project"),
        },
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();

    p.mount(&lang).await.unwrap();
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert_eq!(p.mounts().await.unwrap(), vec![lang.clone()]);
    assert_eq!(things(&p).await.len(), 1);
}

// --- A resync reads the ledger once per snapshot -----------------------------

/// The mount ledger is read a fixed number of times per sync, not once per
/// record: an unchanged snapshot of ten records costs the same reads as
/// one of a single record.
#[tokio::test]
async fn a_resync_reads_the_ledger_once_per_snapshot() {
    let lang = load_mount_test_language().await;
    for i in 0..10 {
        create(&lang, json!({"title": format!("t{i}")})).await;
    }
    let (mut p, _ctx) = mounted(&lang).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);

    use std::sync::atomic::Ordering;
    let before = super::mount_links::LEDGER_READS.load(Ordering::SeqCst);
    let report = p.sync_mount(&lang).await.unwrap();
    let reads = super::mount_links::LEDGER_READS.load(Ordering::SeqCst) - before;
    assert_eq!((report.added, report.removed), (0, 0));
    assert!(
        reads <= 3,
        "an unchanged sync of ten records read the ledger {reads} times"
    );
}

/// A mount shape whose name is not its target class's basename
/// (`MountThing`, `mounttest://Thing`) gets its name node from `add_sdna`:
/// one class-type link and one shape link, and queries by the name work.
#[tokio::test]
async fn a_mounted_class_name_gets_one_alias_node() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted(&lang).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);

    let node = "literal:string:shacl://MountThing".to_string();
    for predicate in ["rdf://type", "ad4m://shape"] {
        let links = p
            .get_links(&LinkQuery {
                source: Some(node.clone()),
                predicate: Some(predicate.to_string()),
                ..Default::default()
            })
            .await
            .unwrap();
        assert_eq!(links.len(), 1, "{predicate} on the name node: {links:?}");
    }
    let instances = things(&p).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["title"], "a");
}

// Two distinct classes allow both languages to be mounted in one perspective.
async fn deadline_mount_pair() -> (PerspectiveInstance, AgentContext, String, String) {
    let first = load_mount_test_language().await;
    let controller = crate::languages::LanguageController::global_instance();
    let source = format!(
        "{}\n// instance {}\n",
        crate::languages::mount_tests::MOUNT_TEST_SOURCE
            .replace("MountThing", "DeadlineOtherThing")
            .replace("mounttest://Thing", "deadlineother://DeadlineOtherThing")
            .replace("mounttest://", "deadlineother://"),
        uuid::Uuid::new_v4(),
    );
    let (_, path) = controller.save_language_bundle(&source, None).unwrap();
    let second = controller.load_language(path, false).await.unwrap();
    let (mut p, ctx) = mounted(&first).await;
    p.mount(&second).await.unwrap();
    for (lang, class) in [(&first, "MountThing"), (&second, "DeadlineOtherThing")] {
        interact(
            &format!("{lang}://class/{class}"),
            "create",
            json!({"title": "new"}),
        )
        .await
        .unwrap();
    }
    // Use the actual traversal order, not hash/address ordering assumptions.
    let langs = p.mounts().await.unwrap();
    (p, ctx, langs[0].clone(), langs[1].clone())
}

/// Advance only while the read is stopped; the Deno fixture setup uses real time.
async fn expire_mount_read(gate: &super::mount_gates::gates::Gate) {
    tokio::time::timeout(Duration::from_secs(5), gate.arrived())
        .await
        .unwrap();
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(76)).await;
    tokio::task::yield_now().await;
    tokio::time::resume();
}

#[tokio::test]
async fn pending_sync_read_times_out_and_resync_reaches_next_language() {
    use super::mount_gates::{gates::arm, MountPoint};
    use crate::languages::mount_init::MountError;
    let (p, _ctx, stuck, healthy) = deadline_mount_pair().await;
    let gate = arm(&stuck, MountPoint::SnapshotReading);
    let mut syncing = p.clone();
    let lang = stuck.clone();
    let sync = tokio::spawn(async move { syncing.sync_mount(&lang).await });
    expire_mount_read(&gate).await;
    let result = tokio::time::timeout(Duration::from_secs(2), sync).await;
    assert!(result.is_ok(), "pending sync did not release its waiter");
    let error = result.unwrap().unwrap().unwrap_err();
    assert!(matches!(error.downcast_ref::<MountError>(),
        Some(MountError::ReadTimeout { language_address, after_ms: 75_000 })
        if language_address == &stuck));

    // Exercise the real sequential resync loop, with the first language stuck again.
    let gate = arm(&stuck, MountPoint::SnapshotReading);
    let next = arm(&healthy, MountPoint::SnapshotFetched);
    super::mounts::set_mount_resync_interval_for_test(Duration::from_millis(1));
    let instance = p.clone();
    let loop_handle = tokio::spawn(async move { instance.mount_resync_loop().await });
    expire_mount_read(&gate).await;
    super::mounts::set_mount_resync_interval_for_test(Duration::from_secs(15));
    tokio::time::timeout(Duration::from_secs(5), next.arrived())
        .await
        .expect("resync must reach the next language in the same pass");
    next.release();
    p.teardown_background_tasks().await;
    loop_handle.await.unwrap();
    assert!(lang_links(&p, &healthy)
        .await
        .iter()
        .any(|l| l.data.source == format!("{healthy}://t1")));
    for lang in [stuck, healthy] {
        crate::perspectives::unregister_mount(&lang, &p.uuid);
    }
}

#[tokio::test]
async fn restore_continues_past_pending_mount_read() {
    use super::mount_gates::{gates::arm, MountPoint};
    let (p, _ctx, stuck, healthy) = deadline_mount_pair().await;
    let gate = arm(&stuck, MountPoint::SnapshotReading);
    let mut restoring = p.clone();
    let restore = tokio::spawn(async move { restoring.restore_mounts().await });
    expire_mount_read(&gate).await;
    tokio::time::timeout(Duration::from_secs(5), restore)
        .await
        .expect("restore must continue past the pending language")
        .unwrap();
    assert!(lang_links(&p, &healthy)
        .await
        .iter()
        .any(|l| l.data.source == format!("{healthy}://t1")));
    for lang in [stuck, healthy] {
        crate::perspectives::unregister_mount(&lang, &p.uuid);
    }
}
