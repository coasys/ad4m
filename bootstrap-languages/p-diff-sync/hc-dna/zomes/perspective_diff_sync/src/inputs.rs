use hdk::prelude::*;
use perspective_diff_sync_integrity::PerspectiveExpression;

use crate::Hash;

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq, Eq, Hash)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq, Eq, Hash)]
pub struct Triple {
    pub source: Option<String>,
    pub target: Option<String>,
    pub predicate: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct SignalData {
    pub remote_agent_did: String,
    pub payload: PerspectiveExpression,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct PullArguments {
    pub hash: Hash,
    pub is_scribe: bool,
}
