//! Why an instance write did not happen (or happened in part).

use super::ValidationError;
use deno_core::anyhow::Error as AnyError;
use serde::{Serialize, Serializer};
use ts_rs::TS;

/// A refused or failed instance write. Serialized (camelCase, tagged by `kind`) as the data of
/// the RPC error; MCP writes its own text from it.
#[derive(Debug, Serialize, TS)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export)]
pub enum InstanceError {
    /// No subject class of that name; `registered` lists the perspective's classes, sorted.
    UnknownClass {
        class_name: String,
        registered: Vec<String>,
    },
    /// Collection membership is written through the record's own class, not an abstract one.
    AbstractClass { class_name: String },
    /// An instance id (`field`) that is not an absolute URI.
    InvalidUri { field: String, value: String },
    /// An update that names no property.
    EmptyProperties,
    /// The batch the caller named is not open.
    BatchNotFound { batch_id: String },
    /// The write breaks the class's shape. Nothing was written.
    Validation {
        class_name: String,
        errors: Vec<ValidationError>,
    },
    /// No instance of the class at that URI.
    NotFound {
        class_name: String,
        base_uri: String,
    },
    /// A create at a URI where an instance of the class already lives.
    AlreadyExists {
        class_name: String,
        base_uri: String,
    },
    /// A collection whose property declares a `resolveLanguage` stores each member as an
    /// expression address, so `itemUri` matched none of the stored `members`: pass the member
    /// as a read lists it.
    StoredAsExpressions {
        class_name: String,
        collection: String,
        item_uri: String,
        language: String,
        members: Vec<String>,
    },
    /// The store failed at `step`. Before the commit nothing was written in the write's own
    /// batch; a write into the caller's batch may have added to it.
    Failed {
        class_name: String,
        step: WriteStep,
        #[serde(serialize_with = "error_chain")]
        #[ts(type = "string")]
        error: AnyError,
    },
    /// A remove outside a batch stopped part way: `removed` links had gone already.
    PartiallyRemoved {
        class_name: String,
        base_uri: String,
        removed: usize,
        step: CascadeStep,
        #[serde(serialize_with = "error_chain")]
        #[ts(type = "string")]
        error: AnyError,
    },
}

/// What a write was doing when the store failed.
#[derive(Debug, Clone, Serialize, PartialEq, Eq, TS)]
#[serde(
    tag = "step",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export)]
pub enum WriteStep {
    /// Reading the class's shape (whether it is abstract).
    ReadShape,
    /// Looking up the instance.
    ReadInstance,
    /// Checking that no instance lives at the URI a create names.
    CheckExisting {
        base_uri: String,
    },
    /// Finding a collection's predicate.
    ResolveCollection {
        collection: String,
    },
    /// Reading a collection's links.
    ReadCollection {
        collection: String,
    },
    CreateSubject,
    UpdateSubject,
    RemoveSubject,
    /// Writing one collection membership link.
    AddCollectionItem {
        collection: String,
        item: String,
    },
    /// Removing a collection's membership links; `removed` had gone already.
    RemoveCollectionItem {
        collection: String,
        item: String,
        removed: usize,
    },
    /// Putting the new instance under its parent.
    AddToParent {
        parent: String,
    },
    /// Committing the write's own batch.
    Commit,
}

/// What a remove's link cascade was doing when the store failed.
#[derive(Debug, Clone, Serialize, PartialEq, Eq, TS)]
#[serde(
    tag = "step",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[ts(export)]
pub enum CascadeStep {
    /// Listing the links of `uri`.
    List { uri: String },
    /// Removing one link.
    Remove {
        source: String,
        predicate: Option<String>,
        target: String,
    },
}

/// An error with its causes, as `{:#}` prints it.
fn error_chain<S: Serializer>(error: &AnyError, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_str(&format_args!("{error:#}"))
}
