//! Core AD4M types for WASM language modules.

use serde::{Deserialize, Serialize};

/// An AD4M Expression with proof of authorship.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Expression {
    pub author: String,
    pub timestamp: String,
    pub data: serde_json::Value,
    pub proof: ExpressionProof,
}

/// Cryptographic proof attached to an Expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionProof {
    pub key: String,
    pub signature: String,
}

/// A link between two expressions.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Link {
    pub source: String,
    pub target: String,
    pub predicate: Option<String>,
}

/// A link with proof of authorship.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub status: Option<String>,
}

/// A perspective diff (additions and removals).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

/// An interaction that can be performed on an expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Interaction {
    pub label: String,
    pub name: String,
    pub parameters: Vec<InteractionParameter>,
}

/// A parameter for an interaction.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InteractionParameter {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: String,
}

/// Trait for languages that support getting and putting expressions.
pub trait ExpressionLanguage {
    /// Get an expression by address. Returns None if not found.
    fn get(&mut self, address: &str) -> Option<Expression>;

    /// Put (create) an expression and return its address.
    fn put(&mut self, content: &serde_json::Value) -> String;
}

/// Trait for languages that support link operations.
pub trait LinkLanguage {
    /// Add a link, returning the signed link expression.
    fn add_link(&mut self, link: &Link) -> LinkExpression;

    /// Remove a link.
    fn remove_link(&mut self, link: &LinkExpression);

    /// Query links matching a filter.
    fn get_links(&mut self, query: &serde_json::Value) -> Vec<LinkExpression>;
}

/// Trait for defining interactions on expressions.
pub trait LanguageInteractions {
    /// Return available interactions for an expression at the given address.
    fn interactions(&self, address: &str) -> Vec<Interaction>;
}

/// Trait for language teardown/cleanup.
/// Provides a default no-op implementation. Language authors can override.
pub trait LanguageTeardown {
    /// Called when the language is being unloaded. Default is no-op.
    fn teardown(&mut self) {}
}
