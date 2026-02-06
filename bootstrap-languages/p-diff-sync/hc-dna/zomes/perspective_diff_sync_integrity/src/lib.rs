use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[derive(
    Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq, Eq, Hash, Ord, PartialOrd,
)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[derive(
    Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq, Eq, Hash, Ord, PartialOrd,
)]
pub struct Triple {
    pub source: Option<String>,
    pub target: Option<String>,
    pub predicate: Option<String>,
}

#[derive(Clone, Deserialize, Serialize, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct LinkExpression {
    pub author: String,
    pub data: Triple,
    pub timestamp: DateTime<Utc>,
    pub proof: ExpressionProof,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct CommitInput {
    pub diff: PerspectiveDiff,
    pub my_did: String,
}

///The reference that is sent to other agents, denotes the position in the DAG as well as the data at that position
#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct HashBroadcast {
    pub reference_hash: HoloHash<holo_hash::hash_type::Action>,
    pub reference: PerspectiveDiffEntryReference,
    pub broadcast_author: String,
}

impl PerspectiveDiff {
    pub fn new() -> Self {
        Self {
            additions: Vec::new(),
            removals: Vec::new(),
        }
    }
    pub fn total_diff_number(&self) -> usize {
        self.additions.len() + self.removals.len()
    }
    
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct Snapshot {
    pub diff_chunks: Vec<HoloHash<holo_hash::hash_type::Action>>,
    pub included_diffs: Vec<HoloHash<holo_hash::hash_type::Action>>,
}

app_entry!(Snapshot);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes, PartialEq, Eq, Hash)]
pub struct PerspectiveDiffEntryReference {
    pub diff: PerspectiveDiff,
    pub parents: Option<Vec<HoloHash<holo_hash::hash_type::Action>>>,
    pub diffs_since_snapshot: usize,
    /// Optional hashes of chunked diff entries for large diffs.
    /// When this is Some and non-empty, the `diff` field should be empty/default
    /// and the actual diff data is stored in separate chunk entries.
    #[serde(default)]
    pub diff_chunks: Option<Vec<HoloHash<holo_hash::hash_type::Action>>>,
}

app_entry!(PerspectiveDiffEntryReference);

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

//TODO: this can likely be removed and instead just reference the PerspectiveDiffEntry/MergeEntry directly?
#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct HashReference {
    pub hash: HoloHash<holo_hash::hash_type::Action>,
    pub timestamp: DateTime<Utc>,
}

app_entry!(HashReference);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct LocalHashReference {
    pub hash: HoloHash<holo_hash::hash_type::Action>,
    pub timestamp: DateTime<Utc>,
}

app_entry!(LocalHashReference);

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
    pub timestamp: DateTime<Utc>,
    pub proof: ExpressionProof,
}

app_entry!(PerspectiveExpression);

/// Signal payload that includes recipient DID for multi-user routing
/// Flattened structure to avoid Holochain extracting nested PerspectiveExpression
#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct RoutedSignalPayload {
    pub recipient_did: String,
    pub author: String,
    pub data: Perspective,
    pub timestamp: DateTime<Utc>,
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

#[derive(Debug, Serialize, Deserialize, SerializedBytes)]
pub struct PullResult {
    pub diff: PerspectiveDiff,
    pub current_revision: Option<HoloHash<holo_hash::hash_type::Action>>,
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

impl HashBroadcast {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl PerspectiveDiffEntryReference {
    pub fn new(
        diff: PerspectiveDiff,
        parents: Option<Vec<HoloHash<holo_hash::hash_type::Action>>>,
    ) -> Self {
        Self {
            diff: diff,
            parents: parents,
            diffs_since_snapshot: 0,
            diff_chunks: None,
        }
    }

    /// Create a new entry reference with chunked diffs
    pub fn new_chunked(
        diff_chunks: Vec<HoloHash<holo_hash::hash_type::Action>>,
        parents: Option<Vec<HoloHash<holo_hash::hash_type::Action>>>,
        diffs_since_snapshot: usize,
    ) -> Self {
        Self {
            diff: PerspectiveDiff::new(), // Empty diff when using chunks
            parents: parents,
            diffs_since_snapshot: diffs_since_snapshot,
            diff_chunks: Some(diff_chunks),
        }
    }

    /// Check if this entry uses chunked storage
    pub fn is_chunked(&self) -> bool {
        self.diff_chunks.as_ref().map_or(false, |chunks| !chunks.is_empty())
    }

    /// Backward compatibility method to extract the diff data
    pub fn to_perspective_diff(&self) -> PerspectiveDiff {
        self.diff.clone()
    }

    /// Helper method to get the comparison key for ordering
    // Compare using tuple ordering: entries with parents come first,
    // then by parent hashes, then by diffs_since_snapshot,
    // then by total diff count, then by diff contents
    fn comparison_key(&self) -> (bool, &Option<Vec<HoloHash<holo_hash::hash_type::Action>>>, usize, usize, &PerspectiveDiff) {
        let has_parents = self.parents.is_some();
        (!has_parents, &self.parents, self.diffs_since_snapshot, self.diff.total_diff_number(), &self.diff)
    }
}

impl PartialOrd for PerspectiveDiffEntryReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PerspectiveDiffEntryReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.comparison_key().cmp(&other.comparison_key())
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
        Op::StoreRecord(StoreRecord { record }) => {
            // Only care about our PerspectiveDiffEntryReference entries
            let maybe_entry = record
                .entry()
                .to_app_option::<PerspectiveDiffEntryReference>();

            if let Ok(Some(pdiff_ref)) = maybe_entry {
                if let Some(parents) = pdiff_ref.parents {
                    let mut missing: Vec<AnyDhtHash> = Vec::new();
                    for parent_action_hash in parents {
                        // Ensure each declared parent exists and is valid in the source chain/DHT
                        if must_get_valid_record(parent_action_hash.clone()).is_err() {
                            missing.push(parent_action_hash.into());
                        }
                    }
                    if !missing.is_empty() {
                        return Ok(ValidateCallbackResult::UnresolvedDependencies(UnresolvedDependencies::Hashes(missing)));
                    }
                }
            }

            Ok(ValidateCallbackResult::Valid)
        }
        // For all other ops in this zome we accept by default
        _ => Ok(ValidateCallbackResult::Valid),
    }
}
