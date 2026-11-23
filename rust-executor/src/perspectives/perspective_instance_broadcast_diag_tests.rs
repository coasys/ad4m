//! `neighbourhood.sendBroadcast` slower than 1 s logs one `[call-diag] event=broadcast.slow`
//! line naming where the time went. On 2026-11-19 broadcasts took 14-29 s and nothing in the
//! log said why; the link language's queue was the suspect. Since #1855 the remote send is
//! queued for a background task (`language_broadcasts`), so the language's queue and call no
//! longer count towards the RPC; the line still names the phases the RPC waits on.

use super::*;
use crate::call_diag::capture;
use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use serde_json::json;
use std::collections::HashSet;

/// A broadcast that waits 1.2 s for the link-language lock (a language being installed or
/// swapped holds it): the line splits the RPC's time into its phases, and that wait is the
/// largest.
#[tokio::test]
async fn a_slow_broadcast_logs_its_phases_and_names_the_largest() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();

    // A link language this test plays: it answers each call at once.
    let address = format!("QmSlowBroadcast{}", Uuid::new_v4().simple());
    let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel::<
        crate::languages::language_runtime::LanguageRuntimeRequest,
    >();
    let handle = LanguageRuntimeHandle::from_channel(address.clone(), tx);
    LanguageController::global_instance()
        .set_runtime_for_test(&address, Some(handle.clone()))
        .await;
    register_capabilities(
        &address,
        HashSet::from([Capability::TelepresenceSendBroadcast]),
    );
    tokio::spawn(async move {
        while let Some(request) = requests.recv().await {
            let _ = request.started.set(std::time::Instant::now());
            let _ = request.response_tx.send(Ok("null".to_string()));
        }
    });

    let handle_record = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("slow broadcast".to_string()),
        shared_url: Some(format!("neighbourhood://{}", Uuid::new_v4())),
        state: PerspectiveState::Synced,
        ..Default::default()
    };
    let instance = PerspectiveInstance::new(handle_record, None);
    *instance.link_language.write().await = Some(Language::new(address.clone()));
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());

    // Something else holds the link-language lock for 1.2 s.
    let (locked_tx, locked_rx) = tokio::sync::oneshot::channel();
    let holder = tokio::spawn({
        let link_language = instance.link_language.clone();
        async move {
            let _guard = link_language.write().await;
            let _ = locked_tx.send(());
            tokio::time::sleep(Duration::from_millis(1200)).await;
        }
    });
    locked_rx.await.unwrap();

    let map = crate::api::ws_handler::build_handler_map();
    let sent = map
        .dispatch(
            "neighbourhood.sendBroadcast",
            json!({ "uuid": instance.uuid, "payload": { "links": [] } }),
            Arc::new(crate::types::RequestContext::operator_for_test()),
        )
        .await
        .unwrap();
    assert_eq!(sent, json!(true));
    holder.await.unwrap();

    let perspective = format!("perspective={}", instance.uuid);
    let lines = capture::lines("broadcast.slow", &[&perspective]);
    assert_eq!(lines.len(), 1, "{lines:?}");
    let line = &lines[0].line;
    assert_eq!(lines[0].level, log::Level::Warn);
    capture::assert_format(line);
    let ms = |key: &str| -> u64 {
        capture::field(line, key)
            .unwrap_or_else(|| panic!("no {key} in {line}"))
            .parse()
            .unwrap()
    };
    let phases = [
        "perspective_lookup_ms",
        "sign_ms",
        "handle_lock_ms",
        "local_delivery_ms",
        "link_language_lock_ms",
        "language_queue_ms",
        "language_call_ms",
        "language_other_ms",
    ];
    let largest = phases.iter().max_by_key(|p| ms(p)).unwrap();
    assert_eq!(*largest, "link_language_lock_ms", "{line}");
    assert!(ms("link_language_lock_ms") >= 1100, "{line}");
    assert!(ms("total_ms") >= 1100, "{line}");
    assert!(ms("payload_bytes") > 0, "{line}");
    assert_eq!(capture::field(line, "outcome").as_deref(), Some("ok"));
    assert_eq!(capture::field(line, "did").as_deref(), Some("admin"));

    crate::perspectives::unregister_perspective(&instance.uuid);
    LanguageController::global_instance()
        .set_runtime_for_test(&address, None)
        .await;
    remove_capabilities(&address);
}

/// A fast broadcast logs nothing.
#[tokio::test]
async fn a_fast_broadcast_logs_nothing() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();
    let handle_record = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("fast broadcast".to_string()),
        shared_url: Some(format!("neighbourhood://{}", Uuid::new_v4())),
        state: PerspectiveState::Synced,
        ..Default::default()
    };
    let instance = PerspectiveInstance::new(handle_record, None);
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());
    let map = crate::api::ws_handler::build_handler_map();
    // No link language: it fails at the network hop, at once.
    let _ = map
        .dispatch(
            "neighbourhood.sendBroadcast",
            json!({ "uuid": instance.uuid, "payload": { "links": [] } }),
            Arc::new(crate::types::RequestContext::operator_for_test()),
        )
        .await;
    let perspective = format!("perspective={}", instance.uuid);
    assert!(capture::lines("broadcast.slow", &[&perspective]).is_empty());
    crate::perspectives::unregister_perspective(&instance.uuid);
}
