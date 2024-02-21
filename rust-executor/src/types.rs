use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expression {
    pub data: serde_json::Value,
    pub timestamp: String,
    pub author: String,
    pub proof: ExpressionProof,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}
