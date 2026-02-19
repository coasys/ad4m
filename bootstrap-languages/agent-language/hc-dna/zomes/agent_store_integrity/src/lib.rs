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

#[hdk_entry_types]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_type(visibility = "public")]
    Did(Did),
    #[entry_type(visibility = "public")]
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
pub struct KeyAuthorisation {
    pub authorising_key: String,
    pub signature: String,
    /// ISO 8601 timestamp that was included in the signed message
    pub timestamp: String,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct AuthorisedKey {
    pub key: String,
    pub name: String,
    pub added_at: DateTime<Utc>,
    pub added_by: String,
    pub proof: KeyAuthorisation,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct KeyRevocation {
    pub revoked_key: String,
    pub revoked_at: DateTime<Utc>,
    pub revoked_by: String,
    /// The authorised key that signed this revocation
    pub revoked_by_key: String,
    pub signature: String,
    pub reason: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct AddAuthorisedKeyInput {
    pub did: String,
    pub key: String,
    pub name: String,
    pub proof: KeyAuthorisation,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct RevokeKeyInput {
    pub did: String,
    pub key: String,
    /// The authorised key used to sign the revocation
    pub revoked_by_key: String,
    pub signature: String,
    /// ISO 8601 timestamp that was included in the signed message
    pub timestamp: String,
    pub reason: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct IsKeyValidInput {
    pub did: String,
    pub key: String,
}

#[derive(Serialize, Deserialize, Clone, SerializedBytes, Debug)]
pub struct AgentExpressionData {
    pub did: String,
    pub perspective: Option<Perspective>,
    #[serde(rename(serialize = "directMessageLanguage"))]
    #[serde(rename(deserialize = "directMessageLanguage"))]
    pub direct_message_language: Option<String>,
    #[serde(default)]
    pub authorised_keys: Vec<AuthorisedKey>,
    #[serde(default)]
    pub revoked_keys: Vec<KeyRevocation>,
}
