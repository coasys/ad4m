use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expression<T> {
    pub data: T,
    pub timestamp: String,
    pub author: String,
    pub proof: ExpressionProof,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}
