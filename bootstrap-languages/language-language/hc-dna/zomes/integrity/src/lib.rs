use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[hdk_entry_helper]
pub struct LanguageChunk(SerializedBytes);

#[hdk_entry_helper]
pub struct LanguageAddress(pub String);

#[hdk_entry_helper]
pub struct LanguageMetadata {
    pub name: String,
    pub description: String,
    pub address: String,
    pub author: String,
    pub templated: Option<bool>,
    #[serde(rename = "templateSourceLanguageAddress")]
    pub template_source_language_address: Option<String>,
    #[serde(rename = "templateAppliedParams")]
    pub template_applied_params: Option<String>,
    #[serde(rename = "possibleTemplateParams")]
    pub possbile_template_params: Option<Vec<String>>,
    #[serde(rename = "sourceCodeLink")]
    pub source_code_link: Option<String>,
    pub size: usize,
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

    #[entry_def(visibility = "public")]
    LanguageAddress(LanguageAddress),
}

#[hdk_link_types]
pub enum LinkTypes {
    LanguageLink,
}
