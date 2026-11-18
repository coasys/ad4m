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
