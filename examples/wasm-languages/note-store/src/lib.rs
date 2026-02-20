//! Note Store — an example AD4M WASM language.
//!
//! This is a port of `tests/js/languages/note-store/` to Rust, compiled to WASM.
//! It stores expressions in an in-memory HashMap, using the content hash as the address.
//! Expressions are signed using the host's agent signing functions.

use ad4m_wasm_language_sdk::prelude::*;
use ad4m_wasm_language_sdk::ad4m_language;
use serde_json;
use std::collections::HashMap;

/// The note store language implementation.
pub struct NoteStoreLanguage {
    /// In-memory storage: address → serialised Expression JSON.
    store: HashMap<String, String>,
}

impl Default for NoteStoreLanguage {
    fn default() -> Self {
        Self {
            store: HashMap::new(),
        }
    }
}

impl ExpressionLanguage for NoteStoreLanguage {
    fn get(&mut self, address: &str) -> Option<Expression> {
        log(&format!("note-store: get({})", address));
        let json_str = self.store.get(address)?;
        let expr: Expression = serde_json::from_str(json_str).ok()?;
        Some(expr)
    }

    fn put(&mut self, content: &serde_json::Value) -> String {
        log(&format!("note-store: put({:?})", content));

        // Create a signed expression via the host
        let expr = match create_signed_expression(content) {
            Some(e) => e,
            None => {
                log("note-store: failed to create signed expression");
                // Fallback: create an unsigned expression
                Expression {
                    author: agent_did().unwrap_or_else(|| "unknown".to_string()),
                    timestamp: "1970-01-01T00:00:00Z".to_string(),
                    data: content.clone(),
                    proof: ExpressionProof {
                        key: String::new(),
                        signature: String::new(),
                    },
                }
            }
        };

        // Serialise and hash to get the address
        let expr_json = serde_json::to_string(&expr).unwrap_or_default();
        let address = match hash(&expr_json) {
            Some(h) => h,
            None => {
                log("note-store: hash failed, using fallback");
                format!("addr-{}", self.store.len())
            }
        };

        // Store
        self.store.insert(address.clone(), expr_json);
        log(&format!("note-store: stored at {}", address));

        address
    }
}

impl LanguageInteractions for NoteStoreLanguage {
    fn interactions(&self, _address: &str) -> Vec<Interaction> {
        Vec::new()
    }
}

impl LanguageTeardown for NoteStoreLanguage {
    fn teardown(&mut self) {
        log("note-store: teardown");
        self.store.clear();
    }
}

// Generate all WASM exports
ad4m_language!(NoteStoreLanguage, "note-store");
