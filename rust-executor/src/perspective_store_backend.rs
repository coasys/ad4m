//! Perspective store backend for link sync/durability.
//!
//! The local OxiGraph SPARQL store handles all querying in every mode.
//! This backend only covers the **sync/durability** layer:
//! - `LocalPerspectiveStore`: no-op (links already persist in OxiGraph's RocksDB)
//! - `SharedPerspectiveStore`: mirrors link mutations to the platform Worker's D1
//!   for cross-device durability and cloud backup.
//!
//! Config: `PERSPECTIVE_STORE_BACKEND` env var or `perspective_store_backend`.
//! - "local" (default): no-op sync (OxiGraph RocksDB persists locally)
//! - "shared": dual-write to platform Worker via HTTP

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::sync::Arc;
use tokio::sync::OnceCell;

// ── Link data transfer type ────────────────────────────────────────────────────

/// Lightweight link representation for sync — matches the platform Worker schema.
/// Decoupled from the full DecoratedLinkExpression to keep the trait clean.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SyncLink {
    pub link_hash: String,
    pub source: String,
    pub predicate: String,
    pub target: String,
    pub author: String,
    pub timestamp: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proof: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<String>,
}

// ── Trait ──────────────────────────────────────────────────────────────────────

/// Abstracts link sync/durability so the executor can mirror mutations
/// to a remote platform or stay purely local.
///
/// Methods are synchronous — matches the WalletBackend pattern.
/// SharedPerspectiveStore uses `reqwest::blocking` internally.
pub trait PerspectiveStoreBackend: Send + Sync {
    /// Push links to the remote store for durability.
    /// Returns the number of links accepted.
    fn push_links(
        &self,
        did: &str,
        perspective_id: &str,
        links: &[SyncLink],
    ) -> Result<usize, AnyError>;

    /// Remove links from the remote store.
    /// Returns the number of links removed.
    fn remove_links(
        &self,
        did: &str,
        perspective_id: &str,
        hashes: &[String],
    ) -> Result<usize, AnyError>;

    /// Fetch links from the remote store (for hydration on startup).
    /// The `since` parameter enables incremental sync.
    fn fetch_links(
        &self,
        did: &str,
        perspective_id: &str,
        since: Option<&str>,
    ) -> Result<Vec<SyncLink>, AnyError>;

    /// Downcast support.
    fn as_any(&self) -> &dyn Any;
}

// ── Global accessor ────────────────────────────────────────────────────────────

static PS_BACKEND: OnceCell<Arc<dyn PerspectiveStoreBackend>> = OnceCell::const_new();

/// Get the global perspective store backend. Panics if not initialised.
pub fn perspective_store_backend() -> &'static Arc<dyn PerspectiveStoreBackend> {
    PS_BACKEND
        .get()
        .expect("perspective store backend not initialised")
}

/// Initialise the global perspective store backend. Returns false if already set.
pub fn init_perspective_store_backend(backend: Arc<dyn PerspectiveStoreBackend>) -> bool {
    PS_BACKEND.set(backend).is_ok()
}

// ── LocalPerspectiveStore ──────────────────────────────────────────────────────

/// No-op implementation — OxiGraph's RocksDB already persists locally.
/// All methods succeed immediately (push/remove return 0, fetch returns empty).
pub struct LocalPerspectiveStore;

impl LocalPerspectiveStore {
    pub fn new() -> Self {
        LocalPerspectiveStore
    }
}

impl PerspectiveStoreBackend for LocalPerspectiveStore {
    fn push_links(
        &self,
        _did: &str,
        _perspective_id: &str,
        links: &[SyncLink],
    ) -> Result<usize, AnyError> {
        // Local mode — OxiGraph already persisted these. Return count for consistency.
        Ok(links.len())
    }

    fn remove_links(
        &self,
        _did: &str,
        _perspective_id: &str,
        hashes: &[String],
    ) -> Result<usize, AnyError> {
        Ok(hashes.len())
    }

    fn fetch_links(
        &self,
        _did: &str,
        _perspective_id: &str,
        _since: Option<&str>,
    ) -> Result<Vec<SyncLink>, AnyError> {
        // Local mode — hydration from remote not applicable.
        Ok(Vec::new())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── SharedPerspectiveStore ─────────────────────────────────────────────────────

/// HTTP client that mirrors link mutations to the platform Worker's
/// `/internal/perspectives/` API for cross-device durability.
pub struct SharedPerspectiveStore {
    base_url: String,
    token: String,
    client: reqwest::blocking::Client,
}

impl SharedPerspectiveStore {
    pub fn new(base_url: String, token: String) -> Self {
        SharedPerspectiveStore {
            base_url: base_url.trim_end_matches('/').to_string(),
            token,
            client: reqwest::blocking::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .expect("Failed to build SharedPerspectiveStore HTTP client"),
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }
}

impl PerspectiveStoreBackend for SharedPerspectiveStore {
    fn push_links(
        &self,
        did: &str,
        perspective_id: &str,
        links: &[SyncLink],
    ) -> Result<usize, AnyError> {
        if links.is_empty() {
            return Ok(0);
        }

        let url = format!("{}/{}/{}/links", self.base_url, did, perspective_id);
        let body = serde_json::json!({ "links": links });

        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .json(&body)
            .send()
            .map_err(|e| anyhow!("SharedPerspectiveStore push failed: {}", e))?;

        if !resp.status().is_success() {
            return Err(anyhow!(
                "SharedPerspectiveStore push returned {}",
                resp.status()
            ));
        }

        let result: serde_json::Value = resp
            .json()
            .map_err(|e| anyhow!("SharedPerspectiveStore parse: {}", e))?;
        let inserted = result
            .get("inserted")
            .and_then(|v| v.as_u64())
            .unwrap_or(links.len() as u64);

        Ok(inserted as usize)
    }

    fn remove_links(
        &self,
        did: &str,
        perspective_id: &str,
        hashes: &[String],
    ) -> Result<usize, AnyError> {
        if hashes.is_empty() {
            return Ok(0);
        }

        let url = format!("{}/{}/{}/links", self.base_url, did, perspective_id);
        let body = serde_json::json!({ "hashes": hashes });

        let resp = self
            .client
            .delete(&url)
            .header("Authorization", self.auth_header())
            .json(&body)
            .send()
            .map_err(|e| anyhow!("SharedPerspectiveStore remove failed: {}", e))?;

        if !resp.status().is_success() {
            return Err(anyhow!(
                "SharedPerspectiveStore remove returned {}",
                resp.status()
            ));
        }

        let result: serde_json::Value = resp
            .json()
            .map_err(|e| anyhow!("SharedPerspectiveStore parse: {}", e))?;
        let removed = result
            .get("removed")
            .and_then(|v| v.as_u64())
            .unwrap_or(hashes.len() as u64);

        Ok(removed as usize)
    }

    fn fetch_links(
        &self,
        did: &str,
        perspective_id: &str,
        since: Option<&str>,
    ) -> Result<Vec<SyncLink>, AnyError> {
        let mut all_links = Vec::new();
        let mut cursor: Option<String> = None;

        loop {
            let url = format!("{}/{}/{}/links", self.base_url, did, perspective_id);
            let mut query_params: Vec<(&str, String)> = Vec::new();

            if let Some(ref c) = cursor {
                query_params.push(("cursor", c.clone()));
            }
            if let Some(s) = since {
                query_params.push(("since", s.to_string()));
            }
            query_params.push(("limit", "1000".to_string()));

            let resp = self
                .client
                .get(&url)
                .header("Authorization", self.auth_header())
                .query(&query_params)
                .send()
                .map_err(|e| anyhow!("SharedPerspectiveStore fetch failed: {}", e))?;

            if !resp.status().is_success() {
                return Err(anyhow!(
                    "SharedPerspectiveStore fetch returned {}",
                    resp.status()
                ));
            }

            let result: serde_json::Value = resp
                .json()
                .map_err(|e| anyhow!("SharedPerspectiveStore parse: {}", e))?;

            if let Some(links_arr) = result.get("links").and_then(|v| v.as_array()) {
                let page: Vec<SyncLink> = links_arr
                    .iter()
                    .filter_map(|v| serde_json::from_value(v.clone()).ok())
                    .collect();
                all_links.extend(page);
            }

            // Check for next cursor — if absent, null, or unchanged, stop
            match result.get("nextCursor") {
                Some(serde_json::Value::String(next)) => {
                    if cursor.as_ref() == Some(next) {
                        break; // Cursor unchanged — prevent infinite loop
                    }
                    cursor = Some(next.clone());
                }
                _ => break,
            }
        }

        Ok(all_links)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
