//! Types mirroring the canonical AD4M Language data shapes. Spec §4.

use serde::{Deserialize, Serialize};

pub type Did = String;
pub type Address = String;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub valid: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub invalid: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Expression<T = serde_json::Value> {
    pub author: Did,
    pub timestamp: String,
    pub data: T,
    pub proof: ExpressionProof,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LinkData {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub predicate: Option<String>,
}

pub type Link = LinkData;
pub type LinkExpression = Expression<LinkData>;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PerspectiveDiff {
    #[serde(default)]
    pub additions: Vec<LinkExpression>,
    #[serde(default)]
    pub removals: Vec<LinkExpression>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Perspective {
    #[serde(default)]
    pub links: Vec<LinkExpression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct QueryRequest {
    pub kind: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryResponse {
    #[serde(default)]
    pub results: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DnaSpec {
    pub nick: String,
    pub bundle: Vec<u8>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Interaction {
    pub label: String,
    pub name: String,
    #[serde(default)]
    pub parameters: Vec<InteractionParameter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InteractionParameter {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: String,
}
