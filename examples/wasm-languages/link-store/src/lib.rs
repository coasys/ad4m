//! Link Store — an AD4M WASM link language.
//!
//! A simple in-memory link language that stores links and supports
//! the full LinksAdapter interface (sync, commit, render, current_revision, others).

use ad4m_wasm_language_sdk::prelude::*;
use ad4m_wasm_language_sdk::{ad4m_language, ad4m_links_adapter};
use serde_json;
use std::collections::HashMap;

pub struct LinkStoreLanguage {
    /// All committed links, keyed by a simple incrementing revision.
    links: Vec<LinkExpression>,
    /// Current revision counter.
    revision: u64,
    /// Known peer DIDs.
    peers: Vec<String>,
}

impl Default for LinkStoreLanguage {
    fn default() -> Self {
        Self {
            links: Vec::new(),
            revision: 0,
            peers: Vec::new(),
        }
    }
}

impl ExpressionLanguage for LinkStoreLanguage {
    fn get(&mut self, address: &str) -> Option<Expression> {
        log(&format!("link-store: get({})", address));
        // Find a link by index
        let idx: usize = address.parse().ok()?;
        let link = self.links.get(idx)?;
        Some(Expression {
            author: link.author.clone(),
            timestamp: link.timestamp.clone(),
            data: serde_json::to_value(&link.data).unwrap_or_default(),
            proof: link.proof.clone(),
        })
    }

    fn put(&mut self, content: &serde_json::Value) -> String {
        log(&format!("link-store: put({:?})", content));
        let idx = self.links.len();
        // Create a link expression from the content
        if let Ok(link) = serde_json::from_value::<Link>(content.clone()) {
            let expr = match create_signed_expression(content) {
                Some(e) => LinkExpression {
                    author: e.author,
                    timestamp: e.timestamp,
                    data: link,
                    proof: e.proof,
                    status: Some("shared".to_string()),
                },
                None => LinkExpression {
                    author: agent_did().unwrap_or_else(|| "unknown".to_string()),
                    timestamp: "1970-01-01T00:00:00Z".to_string(),
                    data: link,
                    proof: ExpressionProof {
                        key: String::new(),
                        signature: String::new(),
                    },
                    status: Some("shared".to_string()),
                },
            };
            self.links.push(expr);
        }
        format!("{}", idx)
    }
}

impl LinksAdapter for LinkStoreLanguage {
    fn sync(&mut self) -> Result<(), String> {
        log("link-store: sync()");
        Ok(())
    }

    fn commit(&mut self, diff: &PerspectiveDiff) -> Result<Option<String>, String> {
        log(&format!("link-store: commit() - {} additions, {} removals",
            diff.additions.len(), diff.removals.len()));

        // Add new links
        for link in &diff.additions {
            self.links.push(link.clone());
        }

        // Remove links (by matching source+target+predicate)
        for removal in &diff.removals {
            self.links.retain(|l| {
                !(l.data.source == removal.data.source
                    && l.data.target == removal.data.target
                    && l.data.predicate == removal.data.predicate)
            });
        }

        self.revision += 1;
        let rev = format!("{}", self.revision);
        log(&format!("link-store: new revision: {}", rev));
        Ok(Some(rev))
    }

    fn render(&mut self) -> Result<Option<Vec<LinkExpression>>, String> {
        log(&format!("link-store: render() - {} links", self.links.len()));
        if self.links.is_empty() {
            Ok(None)
        } else {
            Ok(Some(self.links.clone()))
        }
    }

    fn current_revision(&mut self) -> Result<Option<String>, String> {
        if self.revision == 0 {
            Ok(None)
        } else {
            Ok(Some(format!("{}", self.revision)))
        }
    }

    fn others(&mut self) -> Result<Vec<String>, String> {
        Ok(self.peers.clone())
    }
}

impl LanguageInteractions for LinkStoreLanguage {
    fn interactions(&self, _address: &str) -> Vec<Interaction> {
        Vec::new()
    }
}

impl LanguageTeardown for LinkStoreLanguage {
    fn teardown(&mut self) {
        log("link-store: teardown");
        self.links.clear();
        self.revision = 0;
    }
}

// Generate WASM exports
ad4m_language!(LinkStoreLanguage, "link-store");
ad4m_links_adapter!(LinkStoreLanguage);
