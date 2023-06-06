use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[hdk_entry_helper]
pub struct LanguageChunk(SerializedBytes);

#[hdk_entry_helper]
pub struct LanguageMetadata {
    pub name: String,
    pub description: String,
    pub size: usize,
    pub checksum: String,
    pub chunks_hashes: Vec<EntryHash>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[hdk_entry_helper]
pub struct LanguageExpression {
    pub author: String,
    pub proof: ExpressionProof,
    pub timestamp: DateTime<Utc>,
    pub data: LanguageMetadata,
}

#[hdk_entry_defs]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_def(visibility = "public")]
    LanguageExpression(LanguageExpression),

    #[entry_def(visibility = "public")]
    LanguageChunk(LanguageChunk),
}
