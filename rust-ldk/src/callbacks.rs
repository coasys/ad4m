//! Callback triggers for AD4M languages
//!
//! These functions allow languages to trigger callbacks back into the AD4M host,
//! such as when links change or direct messages are received.

use crate::types::{PerspectiveDiff, Provenance};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Callback triggers
    #[wasm_bindgen(js_namespace = __callbacks)]
    fn links_trigger(proof: &JsValue, diff: &JsValue);

    #[wasm_bindgen(js_namespace = __callbacks)]
    fn dm_trigger(proof: &JsValue, data: &[u8]);
}

/// Triggers the links callback with a perspective diff
///
/// This should be called when the language detects changes to links
/// that need to be propagated to the AD4M host.
///
/// # Arguments
///
/// * `proof` - Provenance information about who made the change
/// * `diff` - The perspective diff containing additions and removals
///
/// # Example
///
/// ```rust
/// use ad4m_ldk::{links_trigger_callback, Provenance, PerspectiveDiff};
///
/// let provenance = Provenance::new("did:key:abc123", 1234567890);
/// let diff = PerspectiveDiff::empty();
/// links_trigger_callback(provenance, diff);
/// ```
pub fn links_trigger_callback(proof: Provenance, diff: PerspectiveDiff) {
    let proof_js = match serde_wasm_bindgen::to_value(&proof) {
        Ok(v) => v,
        Err(_) => return,
    };
    let diff_js = match serde_wasm_bindgen::to_value(&diff) {
        Ok(v) => v,
        Err(_) => return,
    };
    links_trigger(&proof_js, &diff_js);
}

/// Triggers the direct message callback
///
/// This should be called when the language receives a direct message
/// from another agent.
///
/// # Arguments
///
/// * `proof` - Provenance information about who sent the message
/// * `data` - The raw message data
///
/// # Example
///
/// ```rust
/// use ad4m_ldk::{dm_trigger_callback, Provenance};
///
/// let provenance = Provenance::new("did:key:abc123", 1234567890);
/// let data = vec![1, 2, 3, 4];
/// dm_trigger_callback(provenance, data);
/// ```
pub fn dm_trigger_callback(proof: Provenance, data: Vec<u8>) {
    let proof_js = match serde_wasm_bindgen::to_value(&proof) {
        Ok(v) => v,
        Err(_) => return,
    };
    dm_trigger(&proof_js, &data);
}

// serde_wasm_bindgen helper
mod serde_wasm_bindgen {
    use wasm_bindgen::prelude::*;

    pub fn to_value<T: serde::Serialize>(value: &T) -> Result<JsValue, serde_json::Error> {
        let json = serde_json::to_string(value)?;
        Ok(JsValue::from_str(&json))
    }
}
