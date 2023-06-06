use chrono::{DateTime, Utc};
use hdi::prelude::*;
use std::any::{Any};

#[hdk_entry_helper]
pub struct NeighbourhoodChunk(SerializedBytes);

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct Link {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct LinkExpression {
    author: String,
    timestamp: DateTime<Utc>,
    data: Link,
    proof: ExpressionProof,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[hdk_entry_helper]
pub struct NeighbourhoodMetadata {
    pub linkLanguage: String,
    pub meta: Perspective,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[hdk_entry_helper]
pub struct NeighbourhoodExpression {
    pub author: String,
    pub proof: ExpressionProof,
    pub timestamp: DateTime<Utc>,
    pub data: NeighbourhoodMetadata,
}

#[hdk_entry_helper]
pub struct NeighbourhoodParam {
    pub neighbourhood: NeighbourhoodExpression,
    pub address: String,
}

#[hdk_entry_helper]
pub struct NeighbourhoodAddress(pub String);


#[hdk_entry_defs]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_def(visibility = "public")]
    NeighbourhoodExpression(NeighbourhoodExpression),

    #[entry_def(visibility = "public")]
    NeighbourhoodChunk(NeighbourhoodChunk),

    #[entry_def(visibility = "public")]
    NeighbourhoodAddress(NeighbourhoodAddress),
}

#[hdk_link_types]
pub enum LinkTypes {
    NeighbourhoodLink,
}