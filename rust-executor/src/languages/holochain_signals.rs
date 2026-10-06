//! Route Holochain app signals (the `holochain.conductor` `signal` event) to
//! the language that registered the cell (`registerHolochainSignalHandler`).

use serde_json::{json, Value};

use crate::services::builtins::{event_type, Builtin};

/// Start routing; runs for as long as the executor does. Later calls do
/// nothing, so no signal is routed twice.
pub fn start_router() {
    static STARTED: std::sync::OnceLock<()> = std::sync::OnceLock::new();
    if STARTED.set(()).is_err() {
        return;
    }
    let mut signals =
        crate::services::host().watch(event_type(Builtin::HolochainConductor, "signal"), None);
    tokio::spawn(async move {
        while let Some(signal) = signals.recv().await {
            route(signal);
        }
    });
}

fn route(signal: Value) {
    let Some(cell_id) = signal.get("cellId").and_then(Value::as_str) else {
        return;
    };
    let language = crate::js_core::languages_extension::HOLOCHAIN_SIGNAL_HANDLERS
        .read()
        .unwrap_or_else(|p| p.into_inner())
        .get(cell_id)
        .cloned();
    let Some(language) = language else {
        log::debug!(
            "No language registered for Holochain signal from cell {}",
            cell_id
        );
        return;
    };
    // The argument travels as JSON, so no signal data is interpolated into
    // executable JS.
    let args = json!({
        "cell_id": [signal["dnaHash"], signal["agentPubKey"]],
        "zome_name": signal["zomeName"],
        "payload": signal["payload"],
    });
    let script = format!("await globalThis.__handleHolochainSignal__({})", args);
    tokio::spawn(async move {
        let controller = super::LanguageController::global_instance();
        if let Err(e) = controller.execute_on_language(&language, &script).await {
            log::warn!(
                "Failed to route Holochain signal to language {}: {}",
                language,
                e
            );
        }
    });
}
