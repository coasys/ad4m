use chrono::{DateTime, Utc};
use hdi::prelude::*;

// The shared wire types live in `perspective-diff-types`; this crate
// re-exports them so the rest of p-diff-sync can keep importing from
// `perspective_diff_sync_integrity::*` while the algorithm crate
// consumes the same struct shapes via `perspective_diff_types`.
pub use perspective_diff_types::{
    null_node, CommitInput, ExpressionProof, HasDiffParents, HashBroadcast, HashReference,
    LinkExpression, LocalHashReference, OpId, PerspectiveDiff, PerspectiveDiffEntryReference,
    PullResult, Snapshot, Triple,
};

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct LocalTimestampReference {
    pub timestamp_reference: DateTime<Utc>,
}

app_entry!(LocalTimestampReference);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct Anchor(pub String);

app_entry!(Anchor);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct PerspectiveExpression {
    pub author: String,
    pub data: Perspective,
    pub timestamp: String,
    pub proof: ExpressionProof,
}

app_entry!(PerspectiveExpression);

/// Signal payload that includes recipient DID for multi-user routing.
/// Flattened structure to avoid Holochain extracting nested PerspectiveExpression.
#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct RoutedSignalPayload {
    pub recipient_did: String,
    pub author: String,
    pub data: Perspective,
    pub timestamp: String,
    pub proof: ExpressionProof,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct OnlineAgent {
    pub did: String,
    pub status: Option<PerspectiveExpression>,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct OnlineAgentAndAction {
    pub did: String,
    pub status: Option<PerspectiveExpression>,
    pub status_action: Option<ActionHash>,
}

#[hdk_entry_types]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_type(visibility = "public")]
    Snapshot(Snapshot),
    #[entry_type(visibility = "public")]
    HashReference(HashReference),
    #[entry_type(visibility = "public")]
    PerspectiveDiffEntryReference(PerspectiveDiffEntryReference),
    #[entry_type(visibility = "private")]
    LocalHashReference(LocalHashReference),
    #[entry_type(visibility = "private")]
    LocalTimestampReference(LocalTimestampReference),
    #[entry_type(visibility = "public")]
    Anchor(Anchor),
    #[entry_type(visibility = "private")]
    PrivateOnlineStatus(PerspectiveExpression),
}

#[hdk_link_types]
pub enum LinkTypes {
    Snapshot,
    ActiveAgent,
    HashRef,
    TimePath,
    Index,
    DidLink,
}

impl Anchor {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl PerspectiveExpression {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl RoutedSignalPayload {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl OnlineAgent {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

#[hdk_extern]
pub fn validate(op: Op) -> ExternResult<ValidateCallbackResult> {
    match op {
        // HC 0.7 renamed Op::StoreRecord -> Op::CreateRecord (see holochain_integrity_types::op).
        Op::CreateRecord(CreateRecord { record }) => {
            // Only care about our PerspectiveDiffEntryReference entries
            let maybe_entry = record
                .entry()
                .to_app_option::<PerspectiveDiffEntryReference>();

            if let Ok(Some(pdiff_ref)) = maybe_entry {
                let mut missing: Vec<AnyDhtHash> = Vec::new();

                // Validate parent dependencies
                if let Some(parents) = pdiff_ref.parents {
                    for parent_action_hash in parents {
                        if must_get_valid_record(parent_action_hash.clone()).is_err() {
                            missing.push(parent_action_hash.into());
                        }
                    }
                }

                // Validate chunk dependencies — chunks must be available before
                // the parent entry can be validated.
                if let Some(diff_chunks) = pdiff_ref.diff_chunks {
                    for chunk_action_hash in diff_chunks {
                        if must_get_valid_record(chunk_action_hash.clone()).is_err() {
                            missing.push(chunk_action_hash.into());
                        }
                    }
                }

                if !missing.is_empty() {
                    return Ok(ValidateCallbackResult::UnresolvedDependencies(
                        UnresolvedDependencies::Hashes(missing),
                    ));
                }
            }

            Ok(ValidateCallbackResult::Valid)
        }
        _ => Ok(ValidateCallbackResult::Valid),
    }
}
