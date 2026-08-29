//! Database backend trait abstraction for stateless executor mode.
//!
//! `LocalDb` wraps the existing Ad4mDb singleton — zero behaviour change.
//! `SharedDb` calls the platform Worker's `/internal/db/` API via HTTP.
//!
//! Config: `DB_BACKEND` env var or `db_backend` in Ad4mConfig.
//! - "local" (default): uses Ad4mDb SQLite in-process
//! - "shared": delegates to platform Worker via HTTP

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;
use std::any::Any;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tokio::sync::OnceCell;

use crate::db::Ad4mDb;

// ── Trait ──────────────────────────────────────────────────────────────────────

/// Abstracts key-value database operations so the executor can run
/// against either local SQLite or a remote platform Worker.
///
/// Methods are synchronous — matches the WalletBackend pattern.
/// SharedDb uses `reqwest::blocking` internally.
pub trait DbBackend: Send + Sync {
    /// Get a single row by ID. Returns None if not found.
    fn get(&self, did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError>;

    /// List all rows for an agent in a table.
    fn list(&self, did: &str, table: &str) -> Result<Vec<Value>, AnyError>;

    /// Insert or update a row.
    fn upsert(&self, did: &str, table: &str, row_id: &str, data: Value) -> Result<(), AnyError>;

    /// Delete a row.
    fn delete(&self, did: &str, table: &str, row_id: &str) -> Result<(), AnyError>;

    /// Downcast support.
    fn as_any(&self) -> &dyn Any;
}

// ── Global accessor ────────────────────────────────────────────────────────────

static DB_BACKEND: OnceCell<Arc<dyn DbBackend>> = OnceCell::const_new();

/// Get the global database backend. Panics if not initialised.
pub fn db_backend() -> &'static Arc<dyn DbBackend> {
    DB_BACKEND.get().expect("db backend not initialised")
}

/// Initialise the global database backend. Returns false if already set.
pub fn init_db_backend(backend: Arc<dyn DbBackend>) -> bool {
    DB_BACKEND.set(backend).is_ok()
}

// ── LocalDb ────────────────────────────────────────────────────────────────────

/// Wraps Ad4mDb singleton — zero behaviour change for self-hosted mode.
/// All operations delegate to `Ad4mDb::with_global_instance(|db| ...)`.
pub struct LocalDb;

impl LocalDb {
    pub fn new() -> Self {
        LocalDb
    }
}

impl DbBackend for LocalDb {
    fn get(&self, _did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        Ad4mDb::with_global_instance(|db| {
            // Route to the appropriate Ad4mDb method based on table name
            match table {
                "users" => {
                    let user = db.get_user_by_email(row_id);
                    match user {
                        Ok(Some(u)) => Ok(Some(serde_json::to_value(u)?)),
                        Ok(None) => Ok(None),
                        Err(e) => Err(e),
                    }
                }
                "settings" => {
                    let val = db.get_setting(row_id);
                    match val {
                        Ok(Some(v)) => Ok(Some(Value::String(v))),
                        Ok(None) => Ok(None),
                        Err(e) => Err(e),
                    }
                }
                _ => Err(anyhow!("LocalDb: unknown table '{}'", table)),
            }
        })
    }

    fn list(&self, _did: &str, table: &str) -> Result<Vec<Value>, AnyError> {
        Ad4mDb::with_global_instance(|db| match table {
            "users" => {
                let users = db.get_all_users()?;
                Ok(users
                    .into_iter()
                    .filter_map(|u| serde_json::to_value(u).ok())
                    .collect())
            }
            "notifications" => {
                let notifs = db.get_notifications()?;
                Ok(notifs
                    .into_iter()
                    .filter_map(|n| serde_json::to_value(n).ok())
                    .collect())
            }
            _ => Err(anyhow!("LocalDb: unknown table '{}'", table)),
        })
    }

    fn upsert(&self, _did: &str, table: &str, row_id: &str, data: Value) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| match table {
            "settings" => {
                let val = data
                    .as_str()
                    .ok_or_else(|| anyhow!("settings value must be a string"))?;
                db.set_setting(row_id, val)
            }
            _ => Err(anyhow!("LocalDb: upsert not implemented for '{}'", table)),
        })
    }

    fn delete(&self, _did: &str, table: &str, row_id: &str) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|_db| {
            Err(anyhow!("LocalDb: delete not implemented for '{}'", table))
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── SharedDb ───────────────────────────────────────────────────────────────────

/// HTTP client that calls the platform Worker's `/internal/db/` API.
/// Uses `reqwest::blocking` for consistency with SharedWallet.
pub struct SharedDb {
    base_url: String,
    token: String,
    client: reqwest::blocking::Client,
    cache: RwLock<HashMap<String, CachedRow>>,
}

const SHARED_DB_CACHE_TTL_SECS: u64 = 30;

struct CachedRow {
    data: Value,
    fetched_at: std::time::Instant,
}

impl SharedDb {
    pub fn new(base_url: String, token: String) -> Self {
        SharedDb {
            base_url: base_url.trim_end_matches('/').to_string(),
            token,
            client: reqwest::blocking::Client::new(),
            cache: RwLock::new(HashMap::new()),
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }

    fn cache_key(did: &str, table: &str, row_id: &str) -> String {
        format!("{}:{}:{}", did, table, row_id)
    }
}

impl DbBackend for SharedDb {
    fn get(&self, did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        let key = Self::cache_key(did, table, row_id);

        // Check cache
        if let Ok(cache) = self.cache.read() {
            if let Some(entry) = cache.get(&key) {
                if entry.fetched_at.elapsed().as_secs() < SHARED_DB_CACHE_TTL_SECS {
                    return Ok(Some(entry.data.clone()));
                }
            }
        }

        let url = format!("{}/{}/{}/{}", self.base_url, did, table, row_id);
        let resp = self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedDb get failed: {}", e))?;

        if resp.status().as_u16() == 404 {
            return Ok(None);
        }
        if !resp.status().is_success() {
            return Err(anyhow!("SharedDb get returned {}", resp.status()));
        }

        let body: Value = resp.json().map_err(|e| anyhow!("SharedDb parse: {}", e))?;
        let data_str = body
            .get("data")
            .and_then(|d| d.as_str())
            .ok_or_else(|| anyhow!("SharedDb: missing data field"))?;
        let data: Value = serde_json::from_str(data_str)?;

        // Cache
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(
                key,
                CachedRow {
                    data: data.clone(),
                    fetched_at: std::time::Instant::now(),
                },
            );
        }

        Ok(Some(data))
    }

    fn list(&self, did: &str, table: &str) -> Result<Vec<Value>, AnyError> {
        let url = format!("{}/{}/{}", self.base_url, did, table);
        let resp = self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedDb list failed: {}", e))?;

        if !resp.status().is_success() {
            return Err(anyhow!("SharedDb list returned {}", resp.status()));
        }

        let body: Value = resp.json().map_err(|e| anyhow!("SharedDb parse: {}", e))?;
        let rows = body
            .get("rows")
            .and_then(|r| r.as_array())
            .cloned()
            .unwrap_or_default();

        Ok(rows
            .into_iter()
            .filter_map(|row| {
                row.get("data")
                    .and_then(|d| d.as_str())
                    .and_then(|s| serde_json::from_str(s).ok())
            })
            .collect())
    }

    fn upsert(&self, did: &str, table: &str, row_id: &str, data: Value) -> Result<(), AnyError> {
        let url = format!("{}/{}/{}", self.base_url, did, table);
        let body = serde_json::json!({
            "rowId": row_id,
            "data": serde_json::to_string(&data)?,
        });

        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .json(&body)
            .send()
            .map_err(|e| anyhow!("SharedDb upsert failed: {}", e))?;

        if !resp.status().is_success() {
            return Err(anyhow!("SharedDb upsert returned {}", resp.status()));
        }

        // Invalidate cache
        let key = Self::cache_key(did, table, row_id);
        if let Ok(mut cache) = self.cache.write() {
            cache.remove(&key);
        }

        Ok(())
    }

    fn delete(&self, did: &str, table: &str, row_id: &str) -> Result<(), AnyError> {
        let url = format!("{}/{}/{}/{}", self.base_url, did, table, row_id);
        let resp = self
            .client
            .delete(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedDb delete failed: {}", e))?;

        if !resp.status().is_success() {
            return Err(anyhow!("SharedDb delete returned {}", resp.status()));
        }

        // Invalidate cache
        let key = Self::cache_key(did, table, row_id);
        if let Ok(mut cache) = self.cache.write() {
            cache.remove(&key);
        }

        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
