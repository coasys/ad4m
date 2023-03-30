use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[hdk_entry_helper]
pub struct FileChunk(SerializedBytes);

#[hdk_entry_helper]
pub struct FileMetadata {
    pub name: String,
    pub size: usize,
    pub file_type: String,
    pub checksum: String,
    pub chunks_hashes: Vec<EntryHash>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[hdk_entry_helper]
pub struct FileExpression {
    pub author: String,
    pub proof: ExpressionProof,
    pub timestamp: DateTime<Utc>,
    pub data: FileMetadata,
}

#[hdk_entry_defs]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_def(visibility = "public")]
    FileExpression(FileExpression),

    #[entry_def(visibility = "public")]
    FileChunk(FileChunk),
}
