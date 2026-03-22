use deno_core::op2;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::{
    types::PerspectiveDiff,
    types::{PerspectiveExpression, PerspectiveState},
};

lazy_static::lazy_static! {
    /// Global registry mapping cell_id hex key ("dnaHash:agentPubkey") → language_address.
    /// Populated by the per-language runtimes when they call registerDNAs.
    /// Read by the central Holochain signal loop to route signals to per-language runtimes.
    pub static ref HOLOCHAIN_SIGNAL_HANDLERS: Arc<RwLock<HashMap<String, String>>> =
        Arc::new(RwLock::new(HashMap::new()));
}

#[op2]
#[serde]
fn perspective_diff_received(#[serde] diff: PerspectiveDiff, #[string] language_address: String) {
    crate::perspectives::handle_perspective_diff_from_link_language(diff, language_address);
}

#[op2]
#[serde]
fn sync_state_changed(#[serde] state: PerspectiveState, #[string] language_address: String) {
    crate::perspectives::handle_sync_state_changed_from_link_language(state, language_address);
}

#[op2]
#[serde]
fn telepresence_signal_received(
    #[serde] signal: PerspectiveExpression,
    #[string] language_address: String,
    #[string] recipient_did: Option<String>,
) {
    crate::perspectives::handle_telepresence_signal_from_link_language(
        signal,
        language_address,
        recipient_did,
    );
}

#[op2(fast)]
fn register_holochain_signal_handler(
    #[string] cell_id_key: String,
    #[string] language_address: String,
) {
    let handlers = HOLOCHAIN_SIGNAL_HANDLERS.clone();
    tokio::spawn(async move {
        let mut map = handlers.write().await;
        map.insert(cell_id_key, language_address);
    });
}

#[op2]
fn ad4m_signal_emitted(#[serde] signal: JsonValue, #[string] language_address: String) {
    let signal_json = serde_json::to_string(&serde_json::json!({
        "signal": signal,
        "languageAddress": language_address,
    }))
    .unwrap_or_default();

    tokio::spawn(async move {
        crate::pubsub::get_global_pubsub()
            .await
            .publish(&crate::pubsub::NEIGHBOURHOOD_SIGNAL_TOPIC, &signal_json)
            .await;
    });
}

deno_core::extension!(
    language_service,
    ops = [perspective_diff_received, sync_state_changed, telepresence_signal_received, register_holochain_signal_handler, ad4m_signal_emitted],
    esm_entry_point = "ext:language_service/languages_extension.js",
    esm = [dir "src/js_core", "languages_extension.js"]
);
