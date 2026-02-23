//! WASM Language ABI definitions for AD4M.
//!
//! This module defines the formal ABI contract between the AD4M executor (host)
//! and WASM language modules (guest). All WASM languages must conform to this ABI.
//!
//! ## Versioning
//! The ABI is versioned from day one. The host checks `ad4m_abi_version()` on load
//! and rejects modules with incompatible versions.
//!
//! ## Memory Protocol
//! Data is passed across the WASM boundary using a pointer+length encoding:
//! - Guest exports `ad4m_alloc(size: u32) -> u32` and `ad4m_dealloc(ptr: u32, size: u32)`
//! - Strings and structured data are serialised as JSON (UTF-8 bytes)
//! - A "fat pointer" (u64) encodes ptr in the upper 32 bits and len in the lower 32 bits
//! - Host writes input data into guest-allocated memory, calls the function with (ptr, len)
//! - Guest returns a fat pointer; host reads result from guest memory, then deallocates

use serde::{Deserialize, Serialize};

// ============================================================================
// ABI Version
// ============================================================================

/// Current ABI version. Increment on breaking changes.
pub const AD4M_LANGUAGE_ABI_VERSION: u32 = 1;

/// Minimum ABI version the host can still load (for forward compat).
pub const AD4M_LANGUAGE_ABI_MIN_VERSION: u32 = 1;

// ============================================================================
// Fat Pointer Encoding
// ============================================================================

/// Encode a (ptr, len) pair into a single u64 "fat pointer".
/// Upper 32 bits = ptr, lower 32 bits = len.
#[inline]
pub fn encode_fat_ptr(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

/// Decode a fat pointer into (ptr, len).
#[inline]
pub fn decode_fat_ptr(fat: u64) -> (u32, u32) {
    let ptr = (fat >> 32) as u32;
    let len = (fat & 0xFFFF_FFFF) as u32;
    (ptr, len)
}

// ============================================================================
// Required Guest Exports
// ============================================================================

/// Names of functions that every WASM language module MUST export.
pub const REQUIRED_EXPORTS: &[&str] = &[
    "ad4m_abi_version",
    "ad4m_alloc",
    "ad4m_dealloc",
    "ad4m_language_name",
];

/// Names of optional exports for expression languages.
pub const EXPRESSION_EXPORTS: &[&str] = &[
    "ad4m_expression_get",
    "ad4m_expression_put",
];

/// Names of optional exports for link languages.
pub const LINK_EXPORTS: &[&str] = &[
    "ad4m_link_add",
    "ad4m_link_remove",
    "ad4m_link_get_links",
];

/// Names of optional exports.
/// Names of optional exports for links adapter (sync/commit/render).
pub const LINKS_ADAPTER_EXPORTS: &[&str] = &[
    "ad4m_sync",
    "ad4m_commit",
    "ad4m_render",
    "ad4m_current_revision",
    "ad4m_others",
];
pub const OPTIONAL_EXPORTS: &[&str] = &[
    "ad4m_interactions",
    "ad4m_teardown",
    "ad4m_is_immutable_expression",
];

// ============================================================================
// Host Function Names (imports provided to the guest)
// ============================================================================

/// The WASM import module name for AD4M host functions.
pub const HOST_MODULE_NAME: &str = "env";

/// Host function names available to guest modules.
pub mod host_functions {
    pub const AGENT_DID: &str = "agent_did";
    pub const AGENT_SIGN: &str = "agent_sign";
    pub const AGENT_VERIFY: &str = "agent_verify";
    pub const AGENT_CREATE_SIGNED_EXPRESSION: &str = "agent_create_signed_expression";
    pub const LOG_MESSAGE: &str = "log_message";
    pub const HASH: &str = "hash";
    pub const HC_CALL: &str = "hc_call";
    pub const PERSPECTIVE_DIFF_RECEIVED: &str = "perspective_diff_received";
    pub const SYNC_STATE_CHANGED: &str = "sync_state_changed";
    pub const HC_INSTALL_APP: &str = "hc_install_app";
    pub const HC_REMOVE_APP: &str = "hc_remove_app";
    pub const HC_GET_AGENT_KEY: &str = "hc_get_agent_key";
}

// ============================================================================
// Serialisable ABI Types
// ============================================================================

/// Expression as passed across the WASM boundary (JSON-serialised).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AbiExpression {
    pub author: String,
    pub timestamp: String,
    pub data: serde_json::Value,
    pub proof: AbiExpressionProof,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiExpressionProof {
    pub key: String,
    pub signature: String,
}

/// Link as passed across the WASM boundary.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AbiLink {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
}

/// LinkExpression with proof, as passed across the WASM boundary.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AbiLinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: AbiLink,
    pub proof: AbiExpressionProof,
    pub status: Option<String>,
}

/// A perspective diff (additions and removals of link expressions).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AbiPerspectiveDiff {
    pub additions: Vec<AbiLinkExpression>,
    pub removals: Vec<AbiLinkExpression>,
}

/// An interaction definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiInteraction {
    pub label: String,
    pub name: String,
    pub parameters: Vec<AbiInteractionParameter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiInteractionParameter {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: String,
}

/// Request to call a Holochain zome function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiHcCallRequest {
    pub dna_nick: String,
    pub zome_name: String,
    pub fn_name: String,
    pub payload: Vec<u8>,
}

/// Request to verify a signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiVerifyRequest {
    pub did: String,
    pub data: String,
    pub signed_data: String,
}

/// Capabilities of a loaded WASM language module.
#[derive(Debug, Clone)]
pub struct LanguageCapabilities {
    pub has_expression_adapter: bool,
    pub has_put_adapter: bool,
    pub has_link_adapter: bool,
    pub has_interactions: bool,
    pub has_teardown: bool,
    pub has_is_immutable_expression: bool,
    pub has_links_adapter: bool,
}

// ============================================================================
// Serialisation helpers
// ============================================================================

/// Serialise a value to JSON bytes for passing across the WASM boundary.
pub fn to_json_bytes<T: Serialize>(value: &T) -> Result<Vec<u8>, serde_json::Error> {
    serde_json::to_vec(value)
}

/// Deserialise JSON bytes received from the WASM boundary.
pub fn from_json_bytes<'a, T: Deserialize<'a>>(bytes: &'a [u8]) -> Result<T, serde_json::Error> {
    serde_json::from_slice(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fat_ptr_roundtrip() {
        let ptr = 0x1234_5678u32;
        let len = 0xABCD_EF01u32;
        let fat = encode_fat_ptr(ptr, len);
        let (p, l) = decode_fat_ptr(fat);
        assert_eq!(p, ptr);
        assert_eq!(l, len);
    }

    #[test]
    fn test_fat_ptr_zero() {
        let fat = encode_fat_ptr(0, 0);
        let (p, l) = decode_fat_ptr(fat);
        assert_eq!(p, 0);
        assert_eq!(l, 0);
    }

    #[test]
    fn test_json_roundtrip_expression() {
        let expr = AbiExpression {
            author: "did:key:z6Mk...".to_string(),
            timestamp: "2026-02-20T12:00:00Z".to_string(),
            data: serde_json::json!({"title": "Hello", "body": "World"}),
            proof: AbiExpressionProof {
                key: "key123".to_string(),
                signature: "sig456".to_string(),
            },
        };
        let bytes = to_json_bytes(&expr).unwrap();
        let decoded: AbiExpression = from_json_bytes(&bytes).unwrap();
        assert_eq!(decoded.author, expr.author);
        assert_eq!(decoded.timestamp, expr.timestamp);
    }

    #[test]
    fn test_json_roundtrip_link() {
        let link = AbiLink {
            source: "did:key:abc".to_string(),
            target: "expression://xyz".to_string(),
            predicate: Some("foaf:knows".to_string()),
        };
        let bytes = to_json_bytes(&link).unwrap();
        let decoded: AbiLink = from_json_bytes(&bytes).unwrap();
        assert_eq!(decoded.source, link.source);
        assert_eq!(decoded.target, link.target);
        assert_eq!(decoded.predicate, link.predicate);
    }
}

// ============================================================================
// Holochain DNA Installation ABI Types
// ============================================================================

/// Request to install a Holochain app from raw .happ bundle bytes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiHcInstallAppRequest {
    /// Raw .happ file bytes
    pub happ_bytes: Vec<u8>,
}

/// Request to remove a Holochain app by its installed app ID.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AbiHcRemoveAppRequest {
    pub app_id: String,
}
