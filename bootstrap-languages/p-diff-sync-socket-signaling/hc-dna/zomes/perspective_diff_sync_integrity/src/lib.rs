use chrono::{DateTime, Utc};
use hdi::prelude::*;

pub mod impls;

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

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes, Default)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

///The reference that is sent to other agents, denotes the position in the DAG as well as the data at that position
#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct HashBroadcast {
    pub reference_hash: HoloHash<holo_hash::hash_type::Action>,
    pub reference: PerspectiveDiffEntryReference,
    pub diff: PerspectiveDiff,
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
}

app_entry!(PerspectiveDiff);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct Snapshot {
    pub diff_chunks: Vec<HoloHash<holo_hash::hash_type::Action>>,
    pub included_diffs: Vec<HoloHash<holo_hash::hash_type::Action>>,
}

app_entry!(Snapshot);

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes, PartialEq, Eq, Hash)]
pub struct PerspectiveDiffEntryReference {
    pub diff: HoloHash<holo_hash::hash_type::Action>,
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
    PerspectiveDiff(PerspectiveDiff),
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
