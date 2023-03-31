use chrono::{DateTime, Utc};
use hdi::prelude::*;

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct Link {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct LinkExpression {
    author: String,
    timestamp: DateTime<Utc>,
    data: Link,
    proof: ExpressionProof,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct PerspectiveExpression {
    pub author: String,
    pub timestamp: DateTime<Utc>,
    pub data: Perspective,
    pub proof: ExpressionProof,
}
