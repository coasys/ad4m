//! Types for the language mount capability: what a language's `mountShapes()`
//! and `mountSnapshot()` exports return. The mount engine consumes them when a
//! perspective mounts the language (design §2.1, overview Correction 14).
//! The delta type joins them here with the resync work.

use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// What `mountShapes()` returns.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct MountShapes {
    pub version: String,
    pub shapes: Vec<MountShape>,
}

/// One named SHACL shape in the `mountShapes` envelope: `name` is the mounted
/// class's name, carried beside the shape because the SHACL JSON has no slot for it.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct MountShape {
    pub name: String,
    pub shape: serde_json::Value,
}

/// One record of `mountSnapshot()`, and of a delta's `changed` list.
/// `address` is the expression address without the `<lang>://` prefix.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct MountEntry {
    pub r#class: String,
    pub address: String,
    pub data: serde_json::Value,
}

/// What `mountSnapshot()` returns.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Default)]
pub struct MountSnapshot {
    pub entries: Vec<MountEntry>,
}

/// What a mount sync reports back: how many links the expansion added to
/// and removed from the perspective's store (one link per changed property
/// value — a title edit is `{added: 1, removed: 1}`).
#[derive(Debug, Clone, Copy, Default, Deserialize, Serialize, PartialEq, Eq, TS)]
#[ts(export)]
pub struct MountSyncReport {
    pub added: usize,
    pub removed: usize,
}

/// What a `call` action's interaction returns: the address of the primary
/// expression it touched (required of a `create`), plus the records that
/// changed or were removed — the same `changed`/`removed` delta a signal
/// push carries (design §3.4). Addresses carry no `<lang>://` prefix.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Default)]
pub struct MountDelta {
    #[serde(default)]
    pub address: Option<String>,
    #[serde(default)]
    pub changed: Vec<MountEntry>,
    #[serde(default)]
    pub removed: Vec<String>,
}
