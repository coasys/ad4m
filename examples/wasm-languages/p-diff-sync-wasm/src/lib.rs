//! p-diff-sync WASM — a real Holochain-backed AD4M link language.
//!
//! Embeds the Perspective-Diff-Sync .happ bundle and proxies all
//! LinksAdapter calls to the Holochain conductor via zome calls.

use ad4m_wasm_language_sdk::prelude::*;
use ad4m_wasm_language_sdk::{ad4m_language, ad4m_links_adapter};
use serde::{Deserialize, Serialize};

/// The compiled .happ bundle, embedded at build time.
const HAPP_BYTES: &[u8] = include_bytes!("../../../../bootstrap-languages/p-diff-sync/hc-dna/workdir/Perspective-Diff-Sync.happ");

const DNA_ROLE: &str = "perspective-diff-sync";
const ZOME_NAME: &str = "perspective_diff_sync";

// ── Zome-compatible types (msgpack serialized) ──────────────────────────

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ZomeTriple {
    source: Option<String>,
    target: Option<String>,
    predicate: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ZomeExpressionProof {
    key: String,
    signature: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ZomeLinkExpression {
    author: String,
    data: ZomeTriple,
    timestamp: String, // ISO 8601
    proof: ZomeExpressionProof,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ZomePerspectiveDiff {
    additions: Vec<ZomeLinkExpression>,
    removals: Vec<ZomeLinkExpression>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct ZomePerspective {
    links: Vec<ZomeLinkExpression>,
}

// ── Conversions ─────────────────────────────────────────────────────────

fn sdk_to_zome_link(le: &LinkExpression) -> ZomeLinkExpression {
    ZomeLinkExpression {
        author: le.author.clone(),
        data: ZomeTriple {
            source: Some(le.data.source.clone()),
            target: Some(le.data.target.clone()),
            predicate: le.data.predicate.clone(),
        },
        timestamp: le.timestamp.clone(),
        proof: ZomeExpressionProof {
            key: le.proof.key.clone(),
            signature: le.proof.signature.clone(),
        },
    }
}

fn zome_to_sdk_link(zle: &ZomeLinkExpression) -> LinkExpression {
    LinkExpression {
        author: zle.author.clone(),
        timestamp: zle.timestamp.clone(),
        data: Link {
            source: zle.data.source.clone().unwrap_or_default(),
            target: zle.data.target.clone().unwrap_or_default(),
            predicate: zle.data.predicate.clone(),
        },
        proof: ExpressionProof {
            key: zle.proof.key.clone(),
            signature: zle.proof.signature.clone(),
        },
        status: Some("shared".to_string()),
    }
}

fn sdk_to_zome_diff(diff: &PerspectiveDiff) -> ZomePerspectiveDiff {
    ZomePerspectiveDiff {
        additions: diff.additions.iter().map(sdk_to_zome_link).collect(),
        removals: diff.removals.iter().map(sdk_to_zome_link).collect(),
    }
}

// ── Language implementation ─────────────────────────────────────────────

pub struct PDiffSyncLanguage {
    installed: bool,
}

impl Default for PDiffSyncLanguage {
    fn default() -> Self {
        Self { installed: false }
    }
}

impl PDiffSyncLanguage {
    fn call_zome(&self, fn_name: &str, payload: &[u8]) -> Result<Vec<u8>, String> {
        if !self.installed {
            return Err("DNA not installed yet".to_string());
        }
        holochain_call(DNA_ROLE, ZOME_NAME, fn_name, payload)
    }
}

impl ExpressionLanguage for PDiffSyncLanguage {
    fn get(&mut self, address: &str) -> Option<Expression> {
        log(&format!("p-diff-sync-wasm: get({})", address));
        // p-diff-sync doesn't have individual expression get — return None
        None
    }

    fn put(&mut self, content: &serde_json::Value) -> String {
        log(&format!("p-diff-sync-wasm: put({:?})", content));
        // Not applicable for link language
        String::new()
    }
}

impl LinksAdapter for PDiffSyncLanguage {
    fn sync(&mut self) -> Result<(), String> {
        log("p-diff-sync-wasm: sync()");
        let payload = rmp_serde::to_vec(&()).map_err(|e| format!("msgpack error: {}", e))?;
        let result = self.call_zome("sync", &payload)?;
        log(&format!("p-diff-sync-wasm: sync result: {} bytes", result.len()));
        Ok(())
    }

    fn commit(&mut self, diff: &PerspectiveDiff) -> Result<Option<String>, String> {
        log(&format!("p-diff-sync-wasm: commit() - {} additions, {} removals",
            diff.additions.len(), diff.removals.len()));

        let zome_diff = sdk_to_zome_diff(diff);
        let payload = rmp_serde::to_vec(&zome_diff)
            .map_err(|e| format!("msgpack error: {}", e))?;

        let result = self.call_zome("commit", &payload)?;

        // Result is a msgpack-encoded Action hash
        let hash_str = if result.is_empty() {
            None
        } else {
            // Try to decode as a string (the hash)
            match rmp_serde::from_slice::<serde_json::Value>(&result) {
                Ok(v) => v.as_str().map(|s| s.to_string()),
                Err(_) => {
                    // Fallback: hex encode the raw bytes
                    Some(result.iter().map(|b| format!("{:02x}", b)).collect::<String>())
                }
            }
        };

        log(&format!("p-diff-sync-wasm: commit result: {:?}", hash_str));
        Ok(hash_str)
    }

    fn render(&mut self) -> Result<Option<Vec<LinkExpression>>, String> {
        log("p-diff-sync-wasm: render()");
        let payload = rmp_serde::to_vec(&()).map_err(|e| format!("msgpack error: {}", e))?;
        let result = self.call_zome("render", &payload)?;

        if result.is_empty() {
            return Ok(None);
        }

        let perspective: ZomePerspective = rmp_serde::from_slice(&result)
            .map_err(|e| format!("msgpack decode error: {}", e))?;

        if perspective.links.is_empty() {
            Ok(None)
        } else {
            Ok(Some(perspective.links.iter().map(zome_to_sdk_link).collect()))
        }
    }

    fn current_revision(&mut self) -> Result<Option<String>, String> {
        log("p-diff-sync-wasm: current_revision()");
        let payload = rmp_serde::to_vec(&()).map_err(|e| format!("msgpack error: {}", e))?;
        let result = self.call_zome("current_revision", &payload)?;

        if result.is_empty() {
            return Ok(None);
        }

        match rmp_serde::from_slice::<Option<serde_json::Value>>(&result) {
            Ok(Some(v)) => Ok(v.as_str().map(|s| s.to_string())),
            Ok(None) => Ok(None),
            Err(_) => Ok(Some(result.iter().map(|b| format!("{:02x}", b)).collect::<String>())),
        }
    }

    fn others(&mut self) -> Result<Vec<String>, String> {
        log("p-diff-sync-wasm: others()");
        let payload = rmp_serde::to_vec(&()).map_err(|e| format!("msgpack error: {}", e))?;
        let result = self.call_zome("get_others", &payload)?;

        if result.is_empty() {
            return Ok(vec![]);
        }

        rmp_serde::from_slice::<Vec<String>>(&result)
            .map_err(|e| format!("msgpack decode error: {}", e))
    }
}

impl LanguageInteractions for PDiffSyncLanguage {
    fn interactions(&self, _address: &str) -> Vec<Interaction> {
        Vec::new()
    }
}

impl LanguageTeardown for PDiffSyncLanguage {
    fn teardown(&mut self) {
        log("p-diff-sync-wasm: teardown");
        if self.installed {
            if let Ok(did) = agent_did().ok_or("no DID".to_string()) {
                // Use agent DID as app_id (matches how the host installs it)
                let _ = holochain_remove_app(&did);
            }
            self.installed = false;
        }
    }
}

impl LanguageInit for PDiffSyncLanguage {
    fn init(&mut self) -> Result<(), String> {
        log("p-diff-sync-wasm: init() - installing DNA...");
        log(&format!("p-diff-sync-wasm: .happ bundle size: {} bytes", HAPP_BYTES.len()));

        match holochain_install_app(HAPP_BYTES) {
            Ok(info) => {
                log(&format!("p-diff-sync-wasm: DNA installed successfully: {:?}", info));
                self.installed = true;
                Ok(())
            }
            Err(e) => {
                log(&format!("p-diff-sync-wasm: DNA install failed: {}", e));
                Err(format!("Failed to install DNA: {}", e))
            }
        }
    }
}

// Generate WASM exports
ad4m_language!(PDiffSyncLanguage, "p-diff-sync-wasm");
ad4m_links_adapter!(PDiffSyncLanguage);
