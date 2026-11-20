//! `neighbourhood.sendBroadcast` slower than 1 s logs one `[call-diag] event=broadcast.slow`
//! line naming where the time went. On 2026-11-19 broadcasts took 14-29 s and nothing in the
//! log said why; the link language's queue was the suspect.

use super::*;
use crate::call_diag::capture;
use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use serde_json::json;
use std::collections::HashSet;

/// A link language that waits behind a 300 ms call and then takes 1.2 s for the broadcast:
/// the line splits its time into the queue and the call, and the call is the largest phase.
#[tokio::test]
async fn a_slow_broadcast_logs_its_phases_and_the_language_call_is_the_largest() {
    crate::test_utils::setup_wallet();
    crate::test_utils::setup_agent();

    // A link language this test plays: each call runs for the time its script asks.
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
        // One call at a time, in order, as a language runtime runs them.
        while let Some(request) = requests.recv().await {
            let crate::languages::language_runtime::LanguageOperation::Execute(script, _) =
                &request.operation
            else {
                continue;
            };
            let _ = request.started.set(std::time::Instant::now());
            let ms = if script.contains("telepresenceSendBroadcast") {
                1200
            } else {
                300
            };
            tokio::time::sleep(Duration::from_millis(ms)).await;
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

    // Another call holds the language first (a key-grant job, a commit).
    let busy = tokio::spawn({
        let handle = handle.clone();
        async move { handle.execute("await somethingElse()".to_string()).await }
    });
    tokio::time::sleep(Duration::from_millis(20)).await;

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
    busy.await.unwrap().unwrap();

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
    assert_eq!(*largest, "language_call_ms", "{line}");
    assert!(ms("language_call_ms") >= 1200, "{line}");
    assert!(ms("language_queue_ms") >= 200, "{line}");
    assert!(ms("total_ms") >= 1400, "{line}");
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
