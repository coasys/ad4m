use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct Link {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug, PartialEq)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
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

#[hdk_entry_defs]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_def(visibility = "public")]
    Did(Did),
    #[entry_def(visibility = "public")]
    AgentExpression(AgentExpression),
}

#[hdk_link_types]
pub enum LinkTypes {
    ProfileLink,
}

#[derive(Clone, Debug, Deserialize, Serialize, SerializedBytes)]
pub struct Did(pub String);

app_entry!(Did);

#[derive(Clone, Debug, Deserialize, Serialize, SerializedBytes)]
pub struct AgentExpression {
    pub author: String,
    pub timestamp: DateTime<Utc>,
    pub data: AgentExpressionData,
    pub proof: ExpressionProof,
}

app_entry!(AgentExpression);

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct AgentExpressionData {
    did: String,
    perspective: Option<Perspective>,
    #[serde(rename(serialize = "directMessageLanguage"))]
    #[serde(rename(deserialize = "directMessageLanguage"))]
    direct_message_language: Option<String>,
}
