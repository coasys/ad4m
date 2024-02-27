use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use juniper::{
    GraphQLEnum, GraphQLObject,
};

#[derive(Default, Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Expression<T> {
    pub author: String,
    pub timestamp: String,
    pub data: T,
    pub proof: ExpressionProof,
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


#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]

pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct NeighbourhoodExpression {
    pub author: String,
    pub data: Neighbourhood,
    pub proof: ExpressionProof,
    pub timestamp: String,
}
