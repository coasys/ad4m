//! The mount's lifecycle and the order of its applies: an unmount wins
//! against every sync, delta and call result in flight; snapshots and
//! deltas apply one at a time and never regress a record; a mount owns
//! only the classes it installed. The races stop one run at a chosen point with a
//! `mount_gates` gate, run the other side, then let it go.

use super::mount_gates::gates;
use super::mount_gates::MountPoint;
use super::mounts_tests::{
    class, create, lang_links, mounted, mounted_and_registered, things,
};
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
async fn links_at(
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

async fn project(lang: &str, name: &str, params: Value) {
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
    assert!(left.is_empty(), "the call's delta brought links back: {left:?}");
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
    assert_eq!(values, vec!["literal:string:c"], "a scalar holds {values:?}");
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
    assert_eq!(instances.len(), 1, "the older snapshot swept t1: {instances:?}");
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
