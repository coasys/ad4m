use deno_core::op2;
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use crate::{
    graphql::graphql_types::{PerspectiveExpression, PerspectiveState},
    js_core::error::AnyhowWrapperError,
    types::PerspectiveDiff,
};

use super::IsolateState;

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

/// Drop every `HOLOCHAIN_SIGNAL_HANDLERS` entry that routes to the given
/// language address. Called from `LanguageController::language_remove`
/// so signals arriving after a removal don't keep trying to execute on a
/// teardown-ed runtime and spamming NotFound warnings in the log.
pub fn drop_holochain_signal_handlers_for_language(language_address: &str) {
    match HOLOCHAIN_SIGNAL_HANDLERS.write() {
        Ok(mut map) => {
            map.retain(|_cell_id, addr| addr != language_address);
        }
        Err(poisoned) => {
            log::warn!(
                "HOLOCHAIN_SIGNAL_HANDLERS lock was poisoned; recovering and retaining anyway"
            );
            let mut map = poisoned.into_inner();
            map.retain(|_cell_id, addr| addr != language_address);
        }
    }
}

#[op2(fast)]
fn register_holochain_signal_handler(
    #[string] cell_id_key: String,
    #[string] language_address: String,
) {
    // Synchronous write into the global map. The previous implementation
    // used a `tokio::sync::RwLock` and a fire-and-forget `tokio::spawn` to
    // perform the write, which returned from the op before the mapping
    // was actually installed. Any Holochain signal arriving during that
    // async gap (e.g. a peer-join from the DNA that was literally just
    // registered) would miss the handler entry and drop the signal with
    // only a debug-level log. Using a `std::sync::RwLock` lets us write
    // synchronously inside the op, so `registerDNAs` has full
    // happens-before ordering by the time it returns to JS.
    match HOLOCHAIN_SIGNAL_HANDLERS.write() {
        Ok(mut map) => {
            map.insert(cell_id_key, language_address);
        }
        Err(poisoned) => {
            // Recover and proceed — a poisoned lock means some previous
            // writer panicked, but the map itself is still usable.
            log::warn!(
                "HOLOCHAIN_SIGNAL_HANDLERS lock was poisoned; recovering and inserting anyway"
            );
            let mut map = poisoned.into_inner();
            map.insert(cell_id_key, language_address);
        }
    }
}

#[op2]
fn ad4m_signal_emitted(#[serde] signal: JsonValue, #[string] language_address: String) {
    // Spec §7.5 `emitSignal` is defined as an arbitrary language→runtime
    // signal — no perspective/recipient envelope, no specific shape.
    //
    // The legacy implementation published onto NEIGHBOURHOOD_SIGNAL_TOPIC
    // wrapped in `{ signal, languageAddress }`, but every subscriber on
    // that topic deserializes payloads as `NeighbourhoodSignalFilter`
    // (`{ perspective, signal, recipient }`). The wrong-shape publish
    // silently failed deserialization in each subscriber — so every
    // `emitSignal` from a flat Language quietly disappeared, with no
    // error surface for the author to notice.
    //
    // Until a dedicated `AD4M_SIGNAL_TOPIC` + GraphQL subscription is
    // wired (tracked as a post-refactor follow-up — see Phase F of
    // docs-src/language-interface-migration-plan.md), route through the
    // telepresence path when the payload happens to be a
    // PerspectiveExpression — that's the only shape a current subscriber
    // can actually deliver. For everything else, warn loudly instead of
    // publishing onto a topic whose consumers cannot deserialize us.
    match serde_json::from_value::<PerspectiveExpression>(signal.clone()) {
        Ok(perspective_signal) => {
            crate::perspectives::handle_telepresence_signal_from_link_language(
                perspective_signal,
                language_address,
                None,
            );
        }
        Err(_) => {
            log::warn!(
                "emitSignal from language `{}` dropped: payload is not a \
                 PerspectiveExpression and no generic ad4m-signal topic is \
                 wired yet. Payload: {}",
                language_address,
                serde_json::to_string(&signal).unwrap_or_default()
            );
        }
    }
}

// ============================================================================
// Language context getters — set by JsCore before calling init()
// ============================================================================

thread_local! {
    static ISOLATE_STATE: RefCell<Option<IsolateState>> = RefCell::new(None);
}

/// Initialize the thread-local isolate state with the language context.
/// Must be called by LanguageRuntime before init_for_language().
pub fn init_isolate_state(state: IsolateState) {
    ISOLATE_STATE.with(|cell| {
        *cell.borrow_mut() = Some(state);
    });
}

/// Read the current isolate state (for use in op functions).
///
/// Returns `None` if the isolate state has not been initialized yet so
/// callers can convert the absence into an op-level error instead of a
/// panic that would abort the entire v8 isolate. The previous
/// `expect("IsolateState not set")` behavior meant any op call in a
/// pre-init code path (e.g. a language's module-top-level `emitSignal`
/// logging) crashed the whole Language worker instead of throwing a
/// catchable error into JS.
pub fn try_with_isolate_state<R>(f: impl FnOnce(&IsolateState) -> R) -> Option<R> {
    ISOLATE_STATE.with(|cell| {
        let borrowed = cell.borrow();
        borrowed.as_ref().map(f)
    })
}

/// Panicking variant kept for callers that are structurally past init.
pub fn with_isolate_state<R>(f: impl FnOnce(&IsolateState) -> R) -> R {
    ISOLATE_STATE.with(|cell| {
        let borrowed = cell.borrow();
        let state = borrowed.as_ref().expect("IsolateState not set");
        f(state)
    })
}

#[op2]
#[string]
fn language_storage_directory() -> Result<String, AnyhowWrapperError> {
    try_with_isolate_state(|state| state.language_storage_directory.clone())
        .flatten()
        .ok_or_else(|| anyhow::anyhow!("languageStorageDirectory not set yet").into())
}

#[op2]
#[string]
fn language_address() -> Result<String, AnyhowWrapperError> {
    try_with_isolate_state(|state| state.language_address.clone())
        .flatten()
        .ok_or_else(|| anyhow::anyhow!("languageAddress not set yet").into())
}

#[op2]
#[string]
fn language_settings() -> Result<String, AnyhowWrapperError> {
    try_with_isolate_state(|state| state.language_settings.clone())
        .flatten()
        .ok_or_else(|| anyhow::anyhow!("languageSettings not set yet").into())
}

deno_core::extension!(
    language_service,
    ops = [
        perspective_diff_received,
        sync_state_changed,
        telepresence_signal_received,
        register_holochain_signal_handler,
        ad4m_signal_emitted,
        language_storage_directory,
        language_address,
        language_settings,
    ],
    esm_entry_point = "ext:language_service/languages_extension.js",
    esm = [dir "src/js_core", "languages_extension.js"]
);
