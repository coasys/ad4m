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
        handle_mount_delta_from_language_impl(
            delta_for("t1", json!({"title": "late"})),
            l,
            crate::agent::AgentContext::main_agent(),
        )
        .await
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
        handle_mount_delta_from_language_impl(
            delta_for("t1", json!({"title": "b"})),
            l,
            crate::agent::AgentContext::main_agent(),
        )
        .await
    });
    gate.arrived().await;
    let l = lang.clone();
    let mut c = tokio::spawn(async move {
        handle_mount_delta_from_language_impl(
            delta_for("t1", json!({"title": "c"})),
            l,
            crate::agent::AgentContext::main_agent(),
        )
        .await
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
    handle_mount_delta_from_language_impl(
        delta_for("t1", json!({"title": "new"})),
        lang.clone(),
        crate::agent::AgentContext::main_agent(),
    )
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
        handle_mount_delta_from_language_impl(
            delta_for("t1", json!({"title": "old"})),
            l,
            crate::agent::AgentContext::main_agent(),
        )
        .await
    });
    gate.arrived().await;
    handle_mount_delta_from_language_impl(
        delta_for("t1", json!({"title": "new"})),
        lang.clone(),
        crate::agent::AgentContext::main_agent(),
    )
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

// --- An interrupted first mount leaves nothing behind -----------------------

/// A first mount writes its pending mark before any record, and drops it
/// once the mount link is written.
#[tokio::test]
async fn a_first_mount_is_pending_until_its_mount_link() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let gate = gates::arm(&lang, MountPoint::EntryRead);
    let mut mounting = p.clone();
    let l = lang.clone();
    let mount = tokio::spawn(async move { mounting.mount(&l).await });
    gate.arrived().await;
    let pending = links_at(&p, &format!("{lang}://project"), "ad4m://mount_pending").await;
    gate.release();
    mount.await.unwrap().unwrap();
    crate::perspectives::unregister_mount(&lang, &p.uuid);

    assert_eq!(
        pending.len(),
        1,
        "no pending mark while the records were written"
    );
    assert!(
        links_at(&p, &format!("{lang}://project"), "ad4m://mount_pending")
            .await
            .is_empty(),
        "the pending mark outlived the mount"
    );
}

/// A start that finds a pending first mount without its mount link (the
/// executor stopped between the records and the mount link) removes what
/// that mount wrote.
#[tokio::test]
async fn a_restart_removes_what_an_interrupted_first_mount_wrote() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;

    let dir = tempfile::tempdir().unwrap();
    let previous = crate::perspectives::get_app_data_path();
    crate::perspectives::set_app_data_path(dir.path().to_str().unwrap().to_string());

    let check = async {
        let (mut p, _ctx) = mounted(&lang).await;
        // What a first mount leaves when the executor stops before its
        // mount link: its records, its version link and its pending mark.
        let mount_links: Vec<LinkExpression> = p
            .get_links(&LinkQuery {
                predicate: Some("ad4m://mount".to_string()),
                ..Default::default()
            })
            .await
            .unwrap()
            .into_iter()
            .map(LinkExpression::from)
            .collect();
        p.remove_links(mount_links, None).await.unwrap();
        p.add_mount_links(
            &lang,
            vec![Link {
                source: format!("{lang}://project"),
                predicate: Some("ad4m://mount_pending".to_string()),
                target: "literal:string:pending".to_string(),
            }],
        )
        .await
        .unwrap();
        assert!(!lang_links(&p, &lang).await.is_empty());

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
        let left = lang_links(&reopened, &lang).await;
        assert!(
            left.is_empty(),
            "the interrupted mount's links stayed: {left:?}"
        );
        assert!(things(&reopened).await.is_empty());

        // A new mount works.
        reopened.mount(&lang).await.unwrap();
        crate::perspectives::unregister_mount(&lang, &uuid);
        assert_eq!(things(&reopened).await.len(), 1);
    }
    .await;
    *crate::perspectives::APP_DATA_PATH.write().unwrap() = previous;
    check
}

/// A `mount` future dropped while it runs leaves the mount `Inactive` with
/// nothing written, so a retry mounts.
#[tokio::test]
async fn a_dropped_mount_can_be_retried() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "a"})).await;
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;

    let gate = gates::arm(&lang, MountPoint::EntryRead);
    let mut mounting = p.clone();
    let l = lang.clone();
    let mount = tokio::spawn(async move { mounting.mount(&l).await });
    gate.arrived().await;
    mount.abort();
    let _ = mount.await;

    let mut phase = super::mount_lifecycle::MountPhase::Mounting;
    for _ in 0..40 {
        phase = p.mount_lifecycles.lock(&lang).await.phase.clone();
        if phase == super::mount_lifecycle::MountPhase::Inactive {
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    assert_eq!(
        phase,
        super::mount_lifecycle::MountPhase::Inactive,
        "the dropped mount stayed Mounting"
    );
    let left = lang_links(&p, &lang).await;
    assert!(
        left.is_empty(),
        "the dropped mount's links stayed: {left:?}"
    );

    p.mount(&lang).await.unwrap();
    crate::perspectives::unregister_mount(&lang, &p.uuid);
    assert_eq!(things(&p).await.len(), 1);
}

// --- A mount runs as its perspective's owner (#1775) -----------------------

/// A delta the language pushes while it runs for one agent lands only in the
/// perspectives that agent owns: another owner's mount reads the language
/// through its own Holochain cell, and the main agent's mount is the main
/// agent's.
#[tokio::test]
async fn a_pushed_delta_reaches_only_the_emitting_agents_perspectives() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceDeltaOwner";
    let (alices, _) = mounted_and_registered(&lang).await;
    alices.persisted.lock().await.add_owner(alice);
    let (mains, _) = mounted_and_registered(&lang).await;

    let as_alice = crate::agent::AgentContext::managed(alice.to_string());
    handle_mount_delta_from_language_impl(
        delta_for("t9", json!({"title": "alice's"})),
        lang.clone(),
        as_alice,
    )
    .await;
    let (in_alices, in_mains) = (things(&alices).await, things(&mains).await);

    handle_mount_delta_from_language_impl(
        delta_for("t9", json!({"title": "main's"})),
        lang.clone(),
        crate::agent::AgentContext::main_agent(),
    )
    .await;
    let (alices_after, mains_after) = (things(&alices).await, things(&mains).await);
    crate::perspectives::remove_perspective(&alices.uuid).await;
    crate::perspectives::remove_perspective(&mains.uuid).await;

    assert_eq!(in_alices.len(), 1, "Alice's delta missed her perspective");
    assert!(
        in_mains.is_empty(),
        "Alice's delta reached the main agent's: {in_mains:?}"
    );
    assert_eq!(
        mains_after.len(),
        1,
        "the main agent's delta missed its perspective"
    );
    assert_eq!(
        alices_after, in_alices,
        "the main agent's delta reached Alice's"
    );
}

/// mount and syncMount read the language as the perspective's owner.
#[tokio::test]
async fn a_mount_reads_the_language_as_the_perspectives_owner() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceMountOwner";
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.persisted.lock().await.add_owner(alice);
    p.mount(&lang).await.unwrap();
    p.sync_mount(&lang).await.unwrap();

    let agents = interact(&format!("{lang}://project"), "agents", json!({}))
        .await
        .unwrap();
    let reads: Vec<&Value> = agents
        .as_array()
        .unwrap()
        .iter()
        .filter(|a| a["what"] == "mountSnapshot" || a["what"] == "mountShapes")
        .collect();
    assert_eq!(reads.len(), 4, "{agents}");
    for read in reads {
        assert_eq!(read["agent"], json!(alice), "{agents}");
    }
}

// --- Until mount storage is per agent, a mount needs a single owner (#1775) --

fn main_did() -> String {
    crate::agent::did_for_context(&crate::agent::AgentContext::main_agent()).unwrap()
}

/// How often the language's `mountSnapshot` ran so far.
async fn snapshot_reads(lang: &str) -> usize {
    let agents = interact(&format!("{lang}://project"), "agents", json!({}))
        .await
        .unwrap();
    agents
        .as_array()
        .unwrap()
        .iter()
        .filter(|a| a["what"] == "mountSnapshot")
        .count()
}

/// A perspective owned by main + Alice, or by Bob + Alice, mounts nothing: the language's
/// snapshot would be read as one owner into a store the other one queries.
#[tokio::test]
async fn a_perspective_with_two_owners_mounts_nothing() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceTwoOwners";
    let bob = "did:key:z6MkBobTwoOwners";
    for owners in [
        vec![main_did(), alice.to_string()],
        vec![bob.to_string(), alice.to_string()],
    ] {
        let (mut p, _shapes, _ctx) =
            crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
        for owner in &owners {
            p.persisted.lock().await.add_owner(owner);
        }
        let reads = snapshot_reads(&lang).await;
        let mounted = p.mount(&lang).await;
        crate::perspectives::unregister_mount(&lang, &p.uuid);
        assert!(
            mounted.is_err(),
            "owners {owners:?}: the mount went through"
        );
        assert!(p.mounts().await.unwrap().is_empty(), "owners {owners:?}");
        assert!(lang_links(&p, &lang).await.is_empty(), "owners {owners:?}");
        assert_eq!(
            snapshot_reads(&lang).await,
            reads,
            "owners {owners:?}: the snapshot was read"
        );
    }
}

/// A second owner added to a mounted perspective stops its syncs, pushed deltas and call
/// results (which apply through `apply_mount_delta`): none writes the first owner's records
/// where the second one reads.
#[tokio::test]
async fn a_second_owner_stops_syncs_and_deltas() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceFirstOwner";
    let bob = "did:key:z6MkBobSecondOwner";
    let (mut p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    p.persisted.lock().await.add_owner(alice);
    p.mount(&lang).await.unwrap();
    crate::perspectives::register_perspective(p.uuid.clone(), p.clone());
    p.persisted.lock().await.add_owner(bob);

    interact(
        &format!("{lang}://project"),
        "outsideChange",
        json!({"address": "t1", "data": {"title": "alice's"}, "emit": false}),
    )
    .await
    .unwrap();
    let synced = p.sync_mount(&lang).await;
    handle_mount_delta_from_language_impl(
        delta_for("t2", json!({"title": "pushed"})),
        lang.clone(),
        crate::agent::AgentContext::managed(alice.to_string()),
    )
    .await;
    let generation = p.active_mount_generation(&lang).await.unwrap();
    let applied = p
        .apply_mount_delta(
            &lang,
            &delta_for("t3", json!({"title": "call result"})),
            generation,
            super::next_mount_seq(),
        )
        .await;
    let seen = things(&p).await;
    crate::perspectives::remove_perspective(&p.uuid).await;

    assert!(synced.is_err(), "a sync with two owners went through");
    assert!(
        applied.is_err(),
        "a call result with two owners was applied"
    );
    assert!(seen.is_empty(), "records written with two owners: {seen:?}");
}

/// Another local agent joining a neighbourhood whose perspective mounts a language is
/// refused, so it never reads the mirror of the owner's snapshot. The owner itself may
/// rejoin.
#[tokio::test]
async fn joining_a_mounted_neighbourhood_is_refused() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceMountedHood";
    let bob = "did:key:z6MkBobJoinsLate";
    let (p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    let url = format!("neighbourhood://mounted-{}", uuid::Uuid::new_v4());
    {
        let mut handle = p.persisted.lock().await;
        handle.add_owner(alice);
        handle.shared_url = Some(url.clone());
        handle.state = PerspectiveState::Synced;
        crate::db::Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    }
    let mut p = p;
    p.mount(&lang).await.unwrap();
    crate::perspectives::register_perspective(p.uuid.clone(), p.clone());

    let as_bob = crate::agent::AgentContext::managed(bob.to_string());
    let bob_joined =
        crate::neighbourhoods::install_neighbourhood_with_context(url.clone(), &as_bob).await;
    let main_joined = crate::neighbourhoods::install_neighbourhood_with_context(
        url.clone(),
        &crate::agent::AgentContext::main_agent(),
    )
    .await;
    let owners = p.persisted.lock().await.get_owners();
    let as_alice = crate::agent::AgentContext::managed(alice.to_string());
    let alice_rejoined =
        crate::neighbourhoods::install_neighbourhood_with_context(url.clone(), &as_alice).await;
    crate::perspectives::remove_perspective(&p.uuid).await;

    assert!(bob_joined.is_err(), "Bob joined a mounted neighbourhood");
    assert!(
        main_joined.is_err(),
        "the main agent joined Alice's mounted neighbourhood"
    );
    assert_eq!(owners, vec![alice.to_string()]);
    assert!(alice_rejoined.is_ok(), "{:?}", alice_rejoined.err());
}

/// A `mountDelta` emitted with the async context cleared (no call token) is dropped: it
/// counts as no agent, not as the main agent, so neither the main agent's nor Alice's mirror
/// changes.
#[tokio::test]
async fn a_delta_with_no_call_token_reaches_no_mirror() {
    let lang = load_mount_test_language().await;
    let alice = "did:key:z6MkAliceNoToken";
    let (alices, _) = mounted_and_registered(&lang).await;
    alices.persisted.lock().await.add_owner(alice);
    let (mains, _) = mounted_and_registered(&lang).await;

    crate::languages::LanguageController::global_instance()
        .expression_interact(
            &format!("{lang}://project"),
            &crate::types::InteractionCall {
                name: "emitCleared".to_string(),
                parameters_stringified: json!({"mountDelta": {
                    "changed": [{"class": "MountThing", "address": "t7", "data": {"title": "tokenless"}}],
                    "removed": []
                }})
                .to_string(),
            },
            &crate::agent::AgentContext::managed(alice.to_string()),
        )
        .await
        .unwrap();
    // A routed delta applies asynchronously: give it a moment to land.
    tokio::time::sleep(Duration::from_millis(300)).await;
    let (in_alices, in_mains) = (things(&alices).await, things(&mains).await);
    crate::perspectives::remove_perspective(&alices.uuid).await;
    crate::perspectives::remove_perspective(&mains.uuid).await;

    assert!(
        in_mains.is_empty(),
        "the main agent's mirror changed: {in_mains:?}"
    );
    assert!(
        in_alices.is_empty(),
        "Alice's mirror changed: {in_alices:?}"
    );
}

/// A join between the first snapshot read and its writes must not acquire access to
/// the owner's mirror, even though no mount link has been written yet.
#[tokio::test]
async fn joining_during_the_first_mount_snapshot_is_refused() {
    let lang = load_mount_test_language().await;
    create(&lang, json!({"title": "private snapshot"})).await;
    let alice = "did:key:z6MkAliceMountPreparing";
    let bob = "did:key:z6MkBobJoinsDuringSnapshot";
    let (p, _shapes, _ctx) =
        crate::perspectives::interpretation_test_support::setup_perspective_no_llm(&[]).await;
    let url = format!("neighbourhood://preparing-{}", uuid::Uuid::new_v4());
    {
        let mut handle = p.persisted.lock().await;
        handle.add_owner(alice);
        handle.shared_url = Some(url.clone());
        handle.state = PerspectiveState::Synced;
        crate::db::Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    }
    crate::perspectives::register_perspective(p.uuid.clone(), p.clone());
    let gate = gates::arm(&lang, MountPoint::InitialSnapshotFetched);
    let mut mounting = p.clone();
    let l = lang.clone();
    let mount = tokio::spawn(async move { mounting.mount(&l).await });
    tokio::time::timeout(Duration::from_secs(10), gate.arrived())
        .await
        .expect("the first snapshot never reached the gate");
    assert!(p.mounts().await.unwrap().is_empty());
    let joined = tokio::time::timeout(
        Duration::from_secs(5),
        crate::neighbourhoods::install_neighbourhood_with_context(
            url,
            &AgentContext::managed(bob.to_string()),
        ),
    )
    .await;
    let owners = p.persisted.lock().await.get_owners();
    gate.release();
    let mounted = tokio::time::timeout(Duration::from_secs(10), mount)
        .await
        .expect("mount deadlocked")
        .unwrap();
    crate::perspectives::remove_perspective(&p.uuid).await;

    let error = joined
        .expect("join waited on the snapshot")
        .expect_err("Bob became an owner between the private snapshot read and its writes");
    assert!(
        error
            .to_string()
            .contains("another local agent cannot join"),
        "{error}"
    );
    assert_eq!(owners, vec![alice.to_string()]);
    mounted.unwrap();
}
