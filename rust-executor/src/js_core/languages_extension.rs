use deno_core::op2;

use crate::{
    graphql::graphql_types::{PerspectiveExpression, PerspectiveState},
    types::PerspectiveDiff,
};

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

deno_core::extension!(
    language_service,
    ops = [perspective_diff_received, sync_state_changed, telepresence_signal_received, install_wasm_language],
    esm_entry_point = "ext:language_service/languages_extension.js",
    esm = [dir "src/js_core", "languages_extension.js"]
);

#[cfg(feature = "wasm-languages")]
#[op2]
#[string]
fn install_wasm_language(#[string] wasm_path: String, #[string] address: String) -> Result<String, crate::js_core::error::AnyhowWrapperError> {
    use std::path::Path;
    log::info!("Installing WASM language from {} as {}", wasm_path, address);
    crate::wasm_core::register_wasm_language(Path::new(&wasm_path), &address)
        .map_err(|e| crate::js_core::error::AnyhowWrapperError::from(anyhow::anyhow!("{}", e)))?;
    Ok(address)
}

#[cfg(not(feature = "wasm-languages"))]
#[op2]
#[string]
fn install_wasm_language(#[string] _wasm_path: String, #[string] _address: String) -> Result<String, crate::js_core::error::AnyhowWrapperError> {
    Err(crate::js_core::error::AnyhowWrapperError::from(anyhow::anyhow!("WASM languages not enabled")))
}
