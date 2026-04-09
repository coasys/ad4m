//! Common types for AD4M languages

use serde::{Deserialize, Serialize};

/// A link between three URIs: source, predicate, and target
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

impl Link {
    /// Creates a new link
    pub fn new(source: impl Into<String>, predicate: Option<impl Into<String>>, target: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            predicate: predicate.map(|p| p.into()),
            target: target.into(),
        }
    }
}

/// Proof of authenticity for an expression
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct ExpressionProof {
    pub key: String,
    pub signature: String,
}

/// A link expression with author, timestamp, and proof
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub status: Option<String>,
}

impl LinkExpression {
    /// Creates a new link expression
    pub fn new(author: impl Into<String>, timestamp: impl Into<String>, data: Link, proof: ExpressionProof) -> Self {
        Self {
            author: author.into(),
            timestamp: timestamp.into(),
            data,
            proof,
            status: None,
        }
    }
}

/// A perspective is a collection of link expressions
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

impl Perspective {
    /// Creates an empty perspective
    pub fn new() -> Self {
        Self { links: vec![] }
    }

    /// Creates a perspective with the given links
    pub fn with_links(links: Vec<LinkExpression>) -> Self {
        Self { links }
    }
}

/// A diff representing additions and removals of links
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

impl PerspectiveDiff {
    /// Creates a diff with only additions
    pub fn from_additions(additions: Vec<LinkExpression>) -> Self {
        Self {
            additions,
            removals: vec![],
        }
    }

    /// Creates a diff with only removals
    pub fn from_removals(removals: Vec<LinkExpression>) -> Self {
        Self {
            additions: vec![],
            removals,
        }
    }

    /// Creates a diff with both additions and removals
    pub fn from(additions: Vec<LinkExpression>, removals: Vec<LinkExpression>) -> Self {
        Self {
            additions,
            removals,
        }
    }

    /// Creates an empty diff
    pub fn empty() -> Self {
        Self {
            additions: vec![],
            removals: vec![],
        }
    }
}

/// Provenance information for a callback
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Provenance {
    pub author: String,
    pub timestamp: i64,
}

impl Provenance {
    /// Creates new provenance information
    pub fn new(author: impl Into<String>, timestamp: i64) -> Self {
        Self {
            author: author.into(),
            timestamp,
        }
    }
}

/// An entry to be stored in Holochain
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Entry {
    pub content: Vec<u8>,
}

impl Entry {
    /// Creates a new entry from bytes
    pub fn new(content: Vec<u8>) -> Self {
        Self { content }
    }

    /// Creates a new entry from a serializable type
    pub fn from_serializable<T: Serialize>(value: &T) -> Result<Self, serde_json::Error> {
        let content = serde_json::to_vec(value)?;
        Ok(Self { content })
    }
}

/// A hash referencing an entry in Holochain
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct EntryHash(pub Vec<u8>);

impl EntryHash {
    /// Creates a new entry hash from bytes
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    /// Returns the hash as a base64-encoded string
    pub fn to_base64(&self) -> String {
        use base64::{Engine as _, engine::general_purpose};
        general_purpose::STANDARD.encode(&self.0)
    }
}

/// An interaction between agents
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Interaction {
    pub from: String,
    pub to: String,
    pub data: Vec<u8>,
}

impl Interaction {
    /// Creates a new interaction
    pub fn new(from: impl Into<String>, to: impl Into<String>, data: Vec<u8>) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
            data,
        }
    }
}


