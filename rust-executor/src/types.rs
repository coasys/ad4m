use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use juniper::{
    GraphQLEnum, GraphQLObject,
};

#[derive(Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Expression {
    author: String,
    timestamp: String,
    data: JsonValue,
    proof: ExpressionProof,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct ExpressionProof {
    pub key: String,
    pub signature: String,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}


#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpression {
    pub author: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub timestamp: String,
    pub status: Option<LinkStatus>,
}

#[derive(GraphQLEnum, Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum LinkStatus {
    #[serde(rename = "shared")]
    Shared,
    #[serde(rename = "local")]
    Local,
}


#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}