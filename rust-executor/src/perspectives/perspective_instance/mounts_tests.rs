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

fn class(name: &str) -> SubjectClassOption {
    SubjectClassOption {
        class_name: Some(name.to_string()),
        query: None,
    }
}

const UNVERIFIED: &str = r#"{"includeUnverified":false}"#;

/// A private perspective with `lang` mounted.
async fn mounted(lang: &str) -> (PerspectiveInstance, AgentContext) {
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.mount(lang).await.unwrap();
    (p, ctx)
}

/// A private perspective with `lang` mounted, registered in the global
/// map so `get_perspective` — and with it the pushed-delta routing —
/// finds it. Remove it with `crate::perspectives::remove_perspective`.
async fn mounted_and_registered(lang: &str) -> (PerspectiveInstance, AgentContext) {
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
async fn things(p: &PerspectiveInstance) -> Vec<Value> {
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
async fn create(lang: &str, params: Value) {
    interact(&format!("{lang}://class/MountThing"), "create", params)
        .await
        .unwrap();
}

/// Every stored link whose source sits under `lang`.
async fn lang_links(p: &PerspectiveInstance, lang: &str) -> Vec<DecoratedLinkExpression> {
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
    create(&lang, json!({"title": "a", "done": true, "tags": ["x", "y"]})).await;
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
    create(&lang, json!({"title": "a", "done": true, "tags": ["x", "y"]})).await;
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

    // The mount link is Shared but this perspective has no link language,
    // and no mount write may have queued anything for a commit either way.
    let (diff, _ids) = crate::db_backend::db_backend()
        .get_pending_diffs(&p.uuid, None)
        .unwrap();
    assert!(
        diff.additions.is_empty() && diff.removals.is_empty(),
        "mount writes queued pending diffs: {diff:?}"
    );
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
    assert!(
        p.get_links(&LinkQuery {
            predicate: Some("ad4m://mount".to_string()),
            ..Default::default()
        })
        .await
        .unwrap()
        .is_empty()
    );
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
    writes.record(&format!("{lang}://t1"), "mounttest://title", "literal:string:a");
    writes.record(&format!("{lang}://t1"), "mounttest://title", "literal:string:b");
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
    assert!(update.contains("\"b\""), "update misses the new title: {update}");
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
    assert!(
        error.to_string().contains("cannot be mounted"),
        "{error}"
    );
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
        json!({"version": "2"}),
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
    assert_eq!(versions[0].data.target, "literal:string:2");
}

/// Design R4: the first sync of a large language must not take minutes.
/// Ignored (runs on demand): it creates 2 000 records.
#[tokio::test]
#[ignore]
async fn two_thousand_records_first_sync() {
    let lang = load_mount_test_language().await;
    for i in 0..2000 {
        create(&lang, json!({"title": format!("record {i}"), "done": i % 2 == 0, "tags": ["a", "b"]}))
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
