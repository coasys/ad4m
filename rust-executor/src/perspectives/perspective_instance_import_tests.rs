//! What `PerspectiveInstance::import_links_file` leaves behind after the
//! store write: the import publishes no diff, so the instance flags every
//! derived state a real write would have invalidated — the notification
//! and subscription checks and the whole-predicate rescan. Prolog engines
//! rebuild lazily (`mark_prolog_engine_dirty` / dropping the pools), which
//! leaves nothing observable in the `Disabled` mode these tests run under.

use super::*;
use crate::agent::signatures::TestSigner;
use std::sync::atomic::Ordering;

/// A private perspective with a real main agent: the notification check
/// resolves owner DIDs against `AgentService`'s.
fn import_test_perspective() -> PerspectiveInstance {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("import test".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: PerspectiveState::Private,
            owners: None,
        },
        None,
    )
}

/// The `{"links":[…]}` document `runtime.exportData` writes, containing
/// `links`, in a tempfile for `import_links_file`.
fn export_file(links: Vec<crate::types::DecoratedLinkExpression>) -> tempfile::NamedTempFile {
    let file = tempfile::NamedTempFile::new().unwrap();
    let snapshot = crate::types::domain::Perspective { links };
    std::fs::write(file.path(), serde_json::to_string(&snapshot).unwrap()).unwrap();
    file
}

/// One Shared link signed by `signer`.
fn link(signer: &TestSigner, source: &str, predicate: &str, target: &str) -> LinkExpression {
    let signed = signer.sign(
        crate::types::Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
        .normalize(),
    );
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(LinkStatus::Shared),
    }
}

/// An import takes `persist_lock` like every perspective-level write: while a diff's
/// write holds it, the import waits, and it commits once the lock is free.
#[tokio::test(flavor = "multi_thread")]
async fn import_waits_for_persist_lock() {
    let perspective = import_test_perspective();
    let signer = TestSigner::generate();
    let file = export_file(vec![(
        link(&signer, "ad4m://s", "ad4m://p", "ad4m://t"),
        LinkStatus::Shared,
    )
        .into()]);
    let held = perspective
        .persist_lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let importing = perspective.clone();
    let path = file.path().to_path_buf();
    let mut task = tokio::spawn(async move { importing.import_links_file(&path).await });
    // Nothing signals "still waiting": the bound is how long the import gets to
    // show it did not wait.
    assert!(
        tokio::time::timeout(std::time::Duration::from_millis(300), &mut task)
            .await
            .is_err(),
        "the import finished while persist_lock was held"
    );
    assert!(!perspective.sparql_store.has_data());
    drop(held);
    let stats = task.await.unwrap().expect("the import succeeds");
    assert_eq!(stats.imported, 1);
}

/// A caller that gives up (the socket drops the RPC's future) does not stop a load that
/// already started: it still clears the caches and queues the checks.
#[tokio::test(flavor = "multi_thread")]
async fn a_cancelled_import_still_queues_its_checks() {
    let perspective = import_test_perspective();
    let signer = TestSigner::generate();
    let file = export_file(vec![(
        link(&signer, "ad4m://s", "ad4m://p", "ad4m://t"),
        LinkStatus::Shared,
    )
        .into()]);
    let held = perspective
        .persist_lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    {
        // One poll starts the import; dropping the future then is what a cancelled
        // RPC does. The held lock keeps the load from finishing before the drop.
        let mut call = Box::pin(perspective.import_links_file(file.path()));
        assert!(futures::poll!(&mut call).is_pending());
    }
    assert!(!perspective
        .trigger_subscription_check
        .load(Ordering::Acquire));
    drop(held);

    tokio::time::timeout(std::time::Duration::from_secs(30), async {
        // The last check the import queues.
        while !perspective
            .trigger_notification_check
            .load(Ordering::Acquire)
        {
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    })
    .await
    .expect("the cancelled import never queued its checks");
    assert!(perspective
        .trigger_subscription_check
        .load(Ordering::Acquire));
    assert!(perspective.sparql_store.has_data());
}

/// After an import, a notification whose trigger only the imported links
/// satisfy is queued for evaluation, subscriptions re-check every query,
/// and the imported link answers the trigger — all without a published
/// diff.
#[tokio::test(flavor = "multi_thread")]
async fn import_queues_every_derived_state_check_a_diff_would_have() {
    let perspective = import_test_perspective();
    let main_did =
        AgentService::with_global_instance(|a| a.did.clone()).expect("the test agent has a DID");
    // The trigger matches only the imported link's triple.
    crate::db_backend::db_backend()
        .add_notification(
            crate::types::NotificationInput {
                description: "fires on the imported link".to_string(),
                app_name: "import-test".to_string(),
                app_url: "https://example.org".to_string(),
                app_icon_path: String::new(),
                trigger: "SELECT ?p ?o WHERE { <ad4m://notify/s> ?p ?o }".to_string(),
                perspective_ids: vec![perspective.uuid.clone()],
                webhook_url: "http://localhost:1/hook".to_string(),
                webhook_auth: crate::Secret::new(""),
            },
            &main_did,
            true,
        )
        .expect("register the notification");

    let before = perspective.notification_trigger_snapshot().await;
    assert!(
        before.values().all(|matches| matches.is_empty()),
        "nothing matches before the import"
    );

    let signer = TestSigner::generate();
    let file = export_file(vec![(
        link(&signer, "ad4m://notify/s", "ad4m://p", "ad4m://notify/t"),
        LinkStatus::Shared,
    )
        .into()]);
    perspective
        .import_links_file(file.path())
        .await
        .expect("the import succeeds");

    assert!(
        perspective
            .trigger_notification_check
            .load(Ordering::Acquire),
        "the notification check was not queued"
    );
    assert!(
        perspective
            .trigger_subscription_check
            .load(Ordering::Acquire),
        "the subscription check was not queued"
    );
    assert!(
        matches!(
            *perspective.changed_predicates.lock().await,
            ChangedPredicates::CheckAll
        ),
        "a diff-less load must mark every predicate changed"
    );

    let after = perspective
        .calc_notification_trigger_matches()
        .await
        .unwrap();
    assert!(
        after.values().any(|matches| !matches.is_empty()),
        "the imported link satisfies the notification's trigger"
    );
}

// ---------------------------------------------------------------------------------------------
// Verdicts of imported links. The import is offline: it prepares no signers, so a signer whose
// key log the key map does not hold leaves an unanchored link `cantCheck`, for the periodic
// re-check (`Recheck::CantCheck`) to judge with the log fetched.
// ---------------------------------------------------------------------------------------------

mod verdicts {
    use super::*;
    use crate::identity::cache::tests::{store_key, FakeStore, TestLog};
    use crate::identity::cache::{global_key_map, TrustedLog};
    use crate::identity::resolver::Resolver;
    use crate::identity::verdict::LinkVerdict;
    use ad4m_identity_core::build::Signer;
    use std::sync::Arc;

    /// A link by `signer`, a key of `log`: anchored at `anchor`, or over the old signed bytes
    /// (no anchor).
    fn device_link(
        signer: &dyn Signer,
        anchor: Option<u64>,
        target: &str,
    ) -> crate::types::DecoratedLinkExpression {
        let key = signer.key();
        let data = crate::types::Link {
            source: "ad4m://s".to_string(),
            predicate: Some("ad4m://p".to_string()),
            target: target.to_string(),
        }
        .normalize();
        let at = chrono::Utc::now();
        let timestamp = at.to_rfc3339_opts(chrono::SecondsFormat::Millis, true);
        let bytes = match anchor {
            Some(anchor) => {
                crate::agent::signatures::anchored_signing_bytes(&data, &timestamp, anchor).unwrap()
            }
            None => crate::agent::signatures::hash_data_and_timestamp(&data, &at),
        };
        (
            LinkExpression {
                author: format!("did:key:{key}"),
                timestamp,
                data,
                proof: crate::types::ExpressionProof {
                    key: format!("did:key:{key}#{key}"),
                    signature: hex::encode(signer.sign(&bytes)),
                    anchor,
                },
                status: Some(LinkStatus::Shared),
            },
            LinkStatus::Shared,
        )
            .into()
    }

    /// A resolver on a fake store serving `log`, as the agent language would; `down`: the key
    /// index never answers. Its cache feeds the process-wide key map, which the import and the
    /// sweep judge by; the directory holds the cache's database.
    fn resolver_for(log: &TestLog, down: bool) -> (Arc<Resolver>, tempfile::TempDir) {
        let store = Arc::new(FakeStore::new(log.events.clone()));
        if down {
            store
                .key_index_down
                .store(true, std::sync::atomic::Ordering::SeqCst);
        }
        let backend: Arc<dyn crate::identity::backend::IdentityBackend> = store;
        let dir = tempfile::tempdir().unwrap();
        let cache = Arc::new(
            crate::identity::cache::KelCache::open(
                dir.path().join("ad4m_db.sqlite"),
                vec![store_key().multikey()],
            )
            .unwrap(),
        );
        (Arc::new(Resolver::new(cache, backend, None, false)), dir)
    }

    /// Imports `links` through the entry the RPC reaches (`import_links_file`).
    async fn import(
        perspective: &PerspectiveInstance,
        links: Vec<crate::types::DecoratedLinkExpression>,
    ) {
        let file = export_file(links);
        let stats = perspective
            .import_links_file(file.path())
            .await
            .expect("the import succeeds");
        assert_eq!(stats.imported, 1);
    }

    fn stored_verdict(perspective: &PerspectiveInstance) -> Option<LinkVerdict> {
        let links = perspective
            .sparql_store
            .get_all_links(&crate::perspectives::sparql_store::EvalStop::none())
            .unwrap();
        assert_eq!(links.len(), 1);
        links[0].verdict.clone()
    }

    /// A correctly signed, unanchored link by a device key its identity log has revoked, imported
    /// while that log was never fetched: stored `cantCheck` (never `valid`), and the sweep, once
    /// the log is available, judges it as the live path would — `flagged`.
    #[tokio::test(flavor = "multi_thread")]
    async fn an_unanchored_link_of_an_unfetched_revoked_key_is_cant_check_until_the_sweep() {
        let perspective = import_test_perspective();
        let mut log = TestLog::new(171);
        log.revoke_d1();
        let map = global_key_map();
        map.forget(&log.scid());

        import(
            &perspective,
            vec![device_link(&log.device, None, "ad4m://t")],
        )
        .await;
        assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::CantCheck));

        let (resolver, _dir) = resolver_for(&log, false);
        let changed = crate::identity::with_test_resolver(
            resolver,
            perspective.recheck_links(&crate::perspectives::Recheck::CantCheck),
        )
        .await
        .unwrap();
        assert_eq!(changed, 1, "the sweep rewrites the imported link");
        assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::Flagged));
        map.forget(&log.scid());
    }

    /// The key index never answers: the sweep cannot learn the log, and the link stays
    /// `cantCheck` — not `valid`.
    #[tokio::test(flavor = "multi_thread")]
    async fn an_imported_link_stays_cant_check_while_the_index_never_answers() {
        let perspective = import_test_perspective();
        let mut log = TestLog::new(172);
        log.revoke_d1();
        global_key_map().forget(&log.scid());

        import(
            &perspective,
            vec![device_link(&log.device, None, "ad4m://t")],
        )
        .await;
        assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::CantCheck));

        let (resolver, _dir) = resolver_for(&log, true);
        for _ in 0..2 {
            let changed = crate::identity::with_test_resolver(
                resolver.clone(),
                perspective.recheck_links(&crate::perspectives::Recheck::CantCheck),
            )
            .await
            .unwrap();
            assert_eq!(changed, 0);
            assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::CantCheck));
        }
        global_key_map().forget(&log.scid());
    }

    /// A signer whose log the store holds is judged normally on import: no `cantCheck`.
    /// An anchored link by a live key is `valid` with its person; an unanchored link by a
    /// revoked key is `flagged` at once.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_signer_with_a_held_log_is_judged_normally_on_import() {
        let map = global_key_map();

        let live = TestLog::new(173);
        let chain = live.chain();
        map.publish(
            &live.scid(),
            TrustedLog {
                state: chain.state().clone(),
                trusted: chain.head().seq,
            },
        );
        map.note_confirmed(&live.scid(), chrono::Utc::now().timestamp() as u64);
        let perspective = import_test_perspective();
        import(
            &perspective,
            vec![device_link(&live.device, Some(0), "ad4m://t")],
        )
        .await;
        assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::Valid));
        map.forget(&live.scid());

        let mut revoked = TestLog::new(174);
        revoked.revoke_d1();
        let chain = revoked.chain();
        map.publish(
            &revoked.scid(),
            TrustedLog {
                state: chain.state().clone(),
                trusted: chain.head().seq,
            },
        );
        map.note_confirmed(&revoked.scid(), chrono::Utc::now().timestamp() as u64);
        let perspective = import_test_perspective();
        import(
            &perspective,
            vec![device_link(&revoked.device, None, "ad4m://t")],
        )
        .await;
        assert_eq!(stored_verdict(&perspective), Some(LinkVerdict::Flagged));
        map.forget(&revoked.scid());
    }
}
