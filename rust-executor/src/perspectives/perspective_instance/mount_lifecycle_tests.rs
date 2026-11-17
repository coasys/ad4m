//! The mount's lifecycle: an unmount wins against every sync, delta and
//! call result in flight. The races stop one run at a chosen point with a
//! `mount_gates` gate, run the other side, then let it go.

use super::mount_gates::gates;
use super::mount_gates::MountPoint;
use super::mounts_tests::{class, create, lang_links, mounted, mounted_and_registered};
use crate::languages::mount_tests::load_mount_test_language;
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
