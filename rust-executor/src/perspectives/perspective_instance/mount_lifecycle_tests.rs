//! The mount's lifecycle and the order of its applies: an unmount wins
//! against every sync, delta and call result in flight; snapshots and
//! deltas apply one at a time and never regress a record; a mount owns
//! only the classes it installed and removes only links it wrote; a failed
//! mount leaves nothing behind and can be retried. The races stop one run
//! at a chosen point with a `mount_gates` gate, run the other side, then
//! let it go.

use super::mount_gates::gates;
use super::mount_gates::MountPoint;
use super::mounts_tests::{class, create, lang_links, mounted, mounted_and_registered, things};
use super::*;
use crate::languages::mount_tests::{interact, load_mount_test_language};
use crate::languages::mount_types::{MountDelta, MountEntry};
use crate::perspectives::handle_mount_delta_from_language_impl;
use serde_json::{json, Value};

/// A one-record delta setting `address`'s data to `data`.
fn delta_for(address: &str, data: Value) -> MountDelta {
    MountDelta {
        address: Some(address.to_string()),
        changed: vec![MountEntry {
            r#class: "MountThing".to_string(),
            address: address.to_string(),
            data,
        }],
        removed: vec![],
    }
}

/// Every stored link with `source` and `predicate`.
pub(super) async fn links_at(
    p: &PerspectiveInstance,
    source: &str,
    predicate: &str,
) -> Vec<DecoratedLinkExpression> {
    p.get_links(&LinkQuery {
        source: Some(source.to_string()),
        predicate: Some(predicate.to_string()),
        ..Default::default()
    })
    .await
    .unwrap()
}

pub(super) async fn project(lang: &str, name: &str, params: Value) {
    interact(&format!("{lang}://project"), name, params)
        .await
        .unwrap();
}

// --- An unmount wins against everything in flight -------------------------

#[tokio::test]
async fn a_sync_blocked_across_an_unmount_brings_nothing_back() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    let gate = gates::arm(&lang, MountPoint::SnapshotFetched);
    let mut syncing = p.clone();
    let l = lang.clone();
    let sync = tokio::spawn(async move { syncing.sync_mount(&l).await });
    gate.arrived().await;
    p.unmount(&lang).await.unwrap();
    gate.release();
    let _ = sync.await.unwrap();

    let left = lang_links(&p, &lang).await;
    assert!(left.is_empty(), "the sync brought links back: {left:?}");
    assert!(p.mounts().await.unwrap().is_empty());
}

#[tokio::test]
async fn sync_mount_of_a_language_never_mounted_is_refused() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let error = p.sync_mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("not mounted"), "{error}");
    assert!(lang_links(&p, &lang).await.is_empty());
    assert!(
        !p.class_names().unwrap().iter().any(|c| c == "MountThing"),
        "a refused sync installed the shapes"
    );
}

#[tokio::test]
async fn a_delta_blocked_across_an_unmount_brings_nothing_back() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted_and_registered(&lang).await;

    let gate = gates::arm(&lang, MountPoint::DeltaReceived);
    let l = lang.clone();
    let delta = tokio::spawn(async move {
        handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "late"})), l).await
    });
    gate.arrived().await;
    p.unmount(&lang).await.unwrap();
    gate.release();
    delta.await.unwrap();

    let left = lang_links(&p, &lang).await;
    crate::perspectives::remove_perspective(&p.uuid).await;
    assert!(left.is_empty(), "the delta brought links back: {left:?}");
}

#[tokio::test]
async fn a_call_result_after_an_unmount_brings_nothing_back() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, ctx) = mounted(&lang).await;

    let gate = gates::arm(&lang, MountPoint::DeltaReceived);
    let mut writer = p.clone();
    let url = format!("{lang}://t1");
    let update = tokio::spawn(async move {
        writer
            .update_subject(class("MountThing"), url, json!({"title": "b"}), None, &ctx)
            .await
    });
    gate.arrived().await;
    p.unmount(&lang).await.unwrap();
    gate.release();
    let _ = update.await.unwrap();

    let left = lang_links(&p, &lang).await;
    assert!(
        left.is_empty(),
        "the call's delta brought links back: {left:?}"
    );
}

#[tokio::test]
async fn a_restore_blocked_across_an_unmount_brings_nothing_back() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _ctx) = mounted(&lang).await;

    let gate = gates::arm(&lang, MountPoint::SnapshotFetched);
    let mut restoring = p.clone();
    let restore = tokio::spawn(async move { restoring.restore_mounts().await });
    gate.arrived().await;
    p.unmount(&lang).await.unwrap();
    gate.release();
    restore.await.unwrap();

    let left = lang_links(&p, &lang).await;
    let registered = crate::perspectives::perspectives_for_mount(&lang);
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert!(left.is_empty(), "the restore brought links back: {left:?}");
    assert!(registered.is_empty(), "the restore re-registered the mount");
}

// --- One ordered apply per mount ---------------------------------------------

#[tokio::test]
async fn two_concurrent_scalar_updates_leave_one_value() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted_and_registered(&lang).await;

    // `b` stops after reading t1's current links; `c` runs meanwhile.
    let gate = gates::arm(&lang, MountPoint::EntryRead);
    let l = lang.clone();
    let b = tokio::spawn(async move {
        handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "b"})), l).await
    });
    gate.arrived().await;
    let l = lang.clone();
    let mut c = tokio::spawn(async move {
        handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "c"})), l).await
    });
    // Unserialized, `c` finishes while `b` is stopped; serialized, it waits
    // for `b` (the timeout only bounds that wait, the outcome does not
    // depend on it). Then `b` goes on, and `c` after it.
    let _ = tokio::time::timeout(Duration::from_millis(500), &mut c).await;
    gate.release();
    b.await.unwrap();
    if !c.is_finished() {
        c.await.unwrap();
    }

    let titles = links_at(&p, &format!("{lang}://t1"), "mounttest://title").await;
    crate::perspectives::remove_perspective(&p.uuid).await;
    let values: Vec<&str> = titles.iter().map(|l| l.data.target.as_str()).collect();
    assert_eq!(
        values,
        vec!["literal:string:c"],
        "a scalar holds {values:?}"
    );
}

#[tokio::test]
async fn a_snapshot_taken_before_a_delta_keeps_the_delta_record() {
    let lang = load_mount_test_language().await;
    let (p, _ctx) = mounted_and_registered(&lang).await;

    let gate = gates::arm(&lang, MountPoint::SnapshotFetched);
    let mut syncing = p.clone();
    let l = lang.clone();
    let sync = tokio::spawn(async move { syncing.sync_mount(&l).await });
    gate.arrived().await;

    // The record appears after the snapshot was read, and its delta lands.
    project(
        &lang,
        "outsideChange",
        json!({"address": "t1", "data": {"title": "new"}, "emit": false}),
    )
    .await;
    handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "new"})), lang.clone())
        .await;
    assert_eq!(things(&p).await.len(), 1);

    gate.release();
    let _ = sync.await.unwrap();
    let instances = things(&p).await;
    crate::perspectives::remove_perspective(&p.uuid).await;
    assert_eq!(
        instances.len(),
        1,
        "the older snapshot swept t1: {instances:?}"
    );
    assert_eq!(instances[0]["title"], "new");
}

#[tokio::test]
async fn an_older_delta_after_a_newer_one_does_not_regress() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _ctx) = mounted_and_registered(&lang).await;

    let gate = gates::arm(&lang, MountPoint::DeltaReceived);
    let l = lang.clone();
    let older = tokio::spawn(async move {
        handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "old"})), l).await
    });
    gate.arrived().await;
    handle_mount_delta_from_language_impl(delta_for("t1", json!({"title": "new"})), lang.clone())
        .await;
    gate.release();
    older.await.unwrap();

    let instances = things(&p).await;
    crate::perspectives::remove_perspective(&p.uuid).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["title"], "new", "the older delta regressed t1");
}

// --- A mount owns only its own classes ----------------------------------------

#[tokio::test]
async fn mounting_over_an_existing_class_is_refused() {
    let lang = load_mount_test_language().await;
    let (mut p, _shapes, ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.add_sdna(
        "MountThing".to_string(),
        String::new(),
        SdnaType::SubjectClass,
        Some(
            json!({
                "target_class": "todo://MountThing",
                "properties": [{"path": "todo://title", "name": "title", "max_count": 1}]
            })
            .to_string(),
        ),
        &ctx,
    )
    .await
    .unwrap();

    let error = p.mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("MountThing"), "{error}");
    assert_eq!(
        p.get_shape("MountThing").unwrap().target_class,
        "todo://MountThing",
        "the refused mount replaced the existing class"
    );
}

#[tokio::test]
async fn a_second_name_for_a_mounted_class_iri_is_refused() {
    let lang = load_mount_test_language().await;
    let lang2 = load_mount_test_language().await;
    project(
        &lang2,
        "setShapes",
        json!({"shapes": [{"name": "OtherThing", "target": "mounttest://Thing"}]}),
    )
    .await;
    let (mut p, _ctx) = mounted(&lang).await;

    let error = p.mount(&lang2).await.unwrap_err();
    assert!(error.to_string().contains("mounttest://Thing"), "{error}");
}

#[tokio::test]
async fn a_version_refresh_cannot_take_another_mounts_class() {
    let lang = load_mount_test_language().await;
    let lang2 = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    project(
        &lang2,
        "setShapes",
        json!({"shapes": [{"name": "OtherThing", "target": "mounttest://Other"}]}),
    )
    .await;
    let (mut p, _ctx) = mounted(&lang).await;
    p.mount(&lang2).await.unwrap();

    project(
        &lang2,
        "setShapes",
        json!({"version": "3", "shapes": [
            {"name": "OtherThing", "target": "mounttest://Other"},
            {"name": "MountThing", "target": "mounttest://Stolen"}
        ]}),
    )
    .await;
    let error = p.sync_mount(&lang2).await.unwrap_err();
    assert!(error.to_string().contains("MountThing"), "{error}");
    let instances = things(&p).await;
    assert_eq!(instances.len(), 1, "{instances:?}");
    assert_eq!(instances[0]["id"], json!(format!("{lang}://t1")));
}

// --- A mount removes only links it wrote -------------------------------------

#[tokio::test]
async fn a_foreign_local_link_under_the_prefix_survives_sync_and_unmount() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, ctx) = mounted(&lang).await;
    let source = format!("{lang}://t1");
    p.add_links(
        vec![Link {
            source: source.clone(),
            predicate: Some("ex://note".to_string()),
            target: "literal:string:mine".to_string(),
        }],
        LinkStatus::Local,
        None,
        &ctx,
    )
    .await
    .unwrap();

    p.sync_mount(&lang).await.unwrap();
    assert_eq!(
        links_at(&p, &source, "ex://note").await.len(),
        1,
        "sync removed it"
    );
    p.unmount(&lang).await.unwrap();
    assert_eq!(
        links_at(&p, &source, "ex://note").await.len(),
        1,
        "unmount removed it"
    );
}

#[tokio::test]
async fn a_foreign_reifier_of_a_mount_triple_survives() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, ctx) = mounted(&lang).await;
    let source = format!("{lang}://t1");
    // A link is its author, triple and timestamp. Both writers here are the
    // main agent, so in the same millisecond as the mount's own title link
    // this would be that link, not a second one.
    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    let mine = p
        .add_links(
            vec![Link {
                source: source.clone(),
                predicate: Some("mounttest://title".to_string()),
                target: "literal:string:a".to_string(),
            }],
            LinkStatus::Local,
            None,
            &ctx,
        )
        .await
        .unwrap()
        .remove(0);

    project(
        &lang,
        "outsideChange",
        json!({"address": "t1", "data": {"title": "b"}, "emit": false}),
    )
    .await;
    p.sync_mount(&lang).await.unwrap();

    let titles = links_at(&p, &source, "mounttest://title").await;
    assert!(
        titles
            .iter()
            .any(|l| l.timestamp == mine.timestamp && l.data.target == "literal:string:a"),
        "the user's own title link was removed: {titles:?}"
    );
}

#[tokio::test]
async fn a_bookkeeping_refresh_spares_shared_links() {
    let lang = load_mount_test_language().await;
    let (mut p, ctx) = mounted(&lang).await;
    let project_node = format!("{lang}://project");
    p.add_links(
        vec![Link {
            source: project_node.clone(),
            predicate: Some("ad4m://mount_shapes_version".to_string()),
            target: "literal:string:peer".to_string(),
        }],
        LinkStatus::Shared,
        None,
        &ctx,
    )
    .await
    .unwrap();

    project(&lang, "setVersion", json!({"version": "3"})).await;
    p.sync_mount(&lang).await.unwrap();

    let versions = links_at(&p, &project_node, "ad4m://mount_shapes_version").await;
    assert!(
        versions
            .iter()
            .any(|l| l.status == Some(LinkStatus::Shared) && l.data.target == "literal:string:peer"),
        "the refresh removed a Shared link: {versions:?}"
    );
}

// --- A failed mount leaves nothing and can be retried -------------------------

#[tokio::test]
async fn a_failed_snapshot_leaves_no_mount_and_a_retry_works() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    project(&lang, "setSnapshotFault", json!({"fail": "boom"})).await;
    let error = p.mount(&lang).await.unwrap_err();
    assert!(error.to_string().contains("boom"), "{error}");
    assert!(
        p.mounts().await.unwrap().is_empty(),
        "a failed mount is listed"
    );
    assert!(crate::perspectives::perspectives_for_mount(&lang).is_empty());

    project(&lang, "setSnapshotFault", json!({})).await;
    p.mount(&lang).await.unwrap();
    let instances = things(&p).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert_eq!(
        instances.len(),
        1,
        "the retry imported nothing: {instances:?}"
    );
}

#[tokio::test]
async fn an_invalid_entry_leaves_no_records_and_a_retry_works() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    project(
        &lang,
        "setSnapshotFault",
        json!({"extra": [{"class": "NoSuchClass", "address": "bad", "data": {}}]}),
    )
    .await;
    assert!(p.mount(&lang).await.is_err());
    let records: Vec<_> = lang_links(&p, &lang)
        .await
        .into_iter()
        .filter(|l| l.data.source != format!("{lang}://project"))
        .collect();
    assert!(
        records.is_empty(),
        "a failed mount wrote records: {records:?}"
    );
    assert!(
        p.mounts().await.unwrap().is_empty(),
        "a failed mount is listed"
    );

    project(&lang, "setSnapshotFault", json!({})).await;
    p.mount(&lang).await.unwrap();
    let instances = things(&p).await;
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert_eq!(
        instances.len(),
        1,
        "the retry imported nothing: {instances:?}"
    );
}
