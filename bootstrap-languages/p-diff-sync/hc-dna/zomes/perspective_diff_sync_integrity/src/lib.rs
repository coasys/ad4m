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

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes, Default, PartialEq, Eq, Hash)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
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
        }
    }

    /// Backward compatibility method to extract the diff data
    pub fn to_perspective_diff(&self) -> PerspectiveDiff {
        self.diff.clone()
    }
}

impl PartialOrd for PerspectiveDiffEntryReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // Explicit handling of all four cases for consistent ordering:
        // 1. Entries with parents are ordered before entries without parents
        // 2. Among entries with parents, compare by first parent hash, then diffs_since_snapshot, then diff content
        // 3. Among entries without parents, compare by diffs_since_snapshot, then diff content
        // 4. Mixed cases: entries with parents < entries without parents
        
        match (&self.parents, &other.parents) {
            // Case 1: Both have parents - compare by first parent hash, then other fields
            (Some(self_parents), Some(other_parents)) => {
                match (self_parents.first(), other_parents.first()) {
                    (Some(self_first), Some(other_first)) => {
                        // Compare first parent hash
                        match self_first.partial_cmp(other_first)? {
                            // If parent hashes are equal, compare diffs_since_snapshot
                            std::cmp::Ordering::Equal => {
                                match self.diffs_since_snapshot.partial_cmp(&other.diffs_since_snapshot)? {
                                    // If diffs_since_snapshot are equal, compare total diff numbers
                                    std::cmp::Ordering::Equal => {
                                        self.diff.total_diff_number().partial_cmp(&other.diff.total_diff_number())
                                    }
                                    // Otherwise return the diffs_since_snapshot ordering
                                    other_ordering => Some(other_ordering),
                                }
                            }
                            // Otherwise return the parent hash ordering
                            parent_ordering => Some(parent_ordering),
                        }
                    }
                    // If one has empty parents vec, treat as None case
                    (Some(_), None) => Some(std::cmp::Ordering::Less),    // self has parent, other doesn't
                    (None, Some(_)) => Some(std::cmp::Ordering::Greater), // other has parent, self doesn't
                    (None, None) => Some(std::cmp::Ordering::Equal),      // both have empty parent vecs
                }
            }
            // Case 2: Only self has parents - self comes first
            (Some(_), None) => Some(std::cmp::Ordering::Less),
            // Case 3: Only other has parents - other comes first
            (None, Some(_)) => Some(std::cmp::Ordering::Greater),
            // Case 4: Neither has parents - compare by diffs_since_snapshot, then diff content
            (None, None) => {
                // First compare diffs_since_snapshot
                match self.diffs_since_snapshot.partial_cmp(&other.diffs_since_snapshot)? {
                    // If diffs_since_snapshot are equal, compare total diff numbers
                    std::cmp::Ordering::Equal => {
                        self.diff.total_diff_number().partial_cmp(&other.diff.total_diff_number())
                    }
                    // Otherwise return the diffs_since_snapshot ordering
                    other_ordering => Some(other_ordering),
                }
            }
        }
    }
}

impl Ord for PerspectiveDiffEntryReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Explicit handling of all four cases for consistent ordering:
        // 1. Entries with parents are ordered before entries without parents
        // 2. Among entries with parents, compare by first parent hash, then diffs_since_snapshot, then diff content
        // 3. Among entries without parents, compare by diffs_since_snapshot, then diff content
        // 4. Mixed cases: entries with parents < entries without parents
        
        match (&self.parents, &other.parents) {
            // Case 1: Both have parents - compare by first parent hash, then other fields
            (Some(self_parents), Some(other_parents)) => {
                match (self_parents.first(), other_parents.first()) {
                    (Some(self_first), Some(other_first)) => {
                        // Compare first parent hash
                        match self_first.cmp(other_first) {
                            // If parent hashes are equal, compare diffs_since_snapshot
                            std::cmp::Ordering::Equal => {
                                match self.diffs_since_snapshot.cmp(&other.diffs_since_snapshot) {
                                    // If diffs_since_snapshot are equal, compare total diff numbers
                                    std::cmp::Ordering::Equal => {
                                        self.diff.total_diff_number().cmp(&other.diff.total_diff_number())
                                    }
                                    // Otherwise return the diffs_since_snapshot ordering
                                    other_ordering => other_ordering,
                                }
                            }
                            // Otherwise return the parent hash ordering
                            parent_ordering => parent_ordering,
                        }
                    }
                    // If one has empty parents vec, treat as None case
                    (Some(_), None) => std::cmp::Ordering::Less,    // self has parent, other doesn't
                    (None, Some(_)) => std::cmp::Ordering::Greater, // other has parent, self doesn't
                    (None, None) => std::cmp::Ordering::Equal,      // both have empty parent vecs
                }
            }
            // Case 2: Only self has parents - self comes first
            (Some(_), None) => std::cmp::Ordering::Less,
            // Case 3: Only other has parents - other comes first
            (None, Some(_)) => std::cmp::Ordering::Greater,
            // Case 4: Neither has parents - compare by diffs_since_snapshot, then diff content
            (None, None) => {
                // First compare diffs_since_snapshot
                match self.diffs_since_snapshot.cmp(&other.diffs_since_snapshot) {
                    // If diffs_since_snapshot are equal, compare total diff numbers
                    std::cmp::Ordering::Equal => {
                        self.diff.total_diff_number().cmp(&other.diff.total_diff_number())
                    }
                    // Otherwise return the diffs_since_snapshot ordering
                    other_ordering => other_ordering,
                }
            }
        }
    }
}

impl OnlineAgent {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}
