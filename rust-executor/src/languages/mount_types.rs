//! Types for the language mount capability: what a language's `mountShapes()`
//! and `mountSnapshot()` exports return. The mount engine consumes them when a
//! perspective mounts the language (design §2.1, overview Correction 14).
//! The delta type joins them here with the resync work.

use serde::{Deserialize, Serialize};

/// What `mountShapes()` returns.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct MountShapes {
    pub version: String,
    pub shapes: Vec<MountShape>,
}

/// One named SHACL shape; `name` is the class name `add_sdna_batch` needs per
/// shape, since the name is not part of the SHACL JSON itself.
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
