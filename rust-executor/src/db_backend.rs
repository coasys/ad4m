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

use crate::db::{Ad4mDb, Ad4mDbResult, ComputeLogEntry, PaymentRequest};
use crate::types::{
    AIModelLoadingStatus, AIPromptExamples, AITask, EntanglementProof, Expression, LinkExpression,
    LinkStatus, Model, ModelInput, ModelType, Notification, NotificationInput, PerspectiveDiff,
    PerspectiveExpression, PerspectiveHandle, SentMessage, UserInfo, UserStatistics,
};

// ── Trait ──────────────────────────────────────────────────────────────────────

/// Abstracts database operations so the executor can run
/// against either local SQLite or a remote platform Worker.
///
/// Methods mirror Ad4mDb's public API one-to-one.
/// LocalDb delegates each call to `Ad4mDb::with_global_instance`.
/// SharedDb returns explicit errors for methods not yet available via HTTP.
pub trait DbBackend: Send + Sync {
    // ── Generic key-value API (existing, used by shared-backend rehydration) ──

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

    // ── Settings ──────────────────────────────────────────────────────────────

    fn get_setting(&self, key: &str) -> Ad4mDbResult<Option<String>>;
    fn set_setting(&self, key: &str, value: &str) -> Ad4mDbResult<()>;
    fn get_multi_user_enabled(&self) -> Ad4mDbResult<bool>;
    fn set_multi_user_enabled(&self, enabled: bool) -> Ad4mDbResult<()>;
    fn get_free_hosting_enabled(&self) -> Ad4mDbResult<bool>;
    fn set_free_hosting_enabled(&self, enabled: bool) -> Ad4mDbResult<()>;

    // ── Users ─────────────────────────────────────────────────────────────────

    fn add_user(&self, username: &str, did: &str, password: &str) -> Ad4mDbResult<()>;
    fn add_user_prehashed(
        &self,
        username: &str,
        did: &str,
        password_hash: &str,
    ) -> Ad4mDbResult<()>;
    fn get_user(&self, username: &str) -> Ad4mDbResult<UserInfo>;
    fn update_user_last_seen(&self, email: &str) -> Ad4mDbResult<()>;
    fn list_users(&self) -> Ad4mDbResult<Vec<UserInfo>>;
    fn list_user_statistics(&self) -> Ad4mDbResult<Vec<UserStatistics>>;
    fn verify_user_password(&self, username: &str, password: &str) -> Ad4mDbResult<bool>;
    fn get_user_credits(&self, email: &str) -> Ad4mDbResult<f64>;
    fn set_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
    fn add_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
    fn deduct_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
    fn deduct_user_credits_if_available(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
    fn deduct_credits_and_log(
        &self,
        email: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Ad4mDbResult<(i64, f64)>;
    fn get_user_hot_wallet(&self, email: &str) -> Ad4mDbResult<Option<String>>;
    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Ad4mDbResult<()>;
    fn get_user_by_hot_wallet_address(&self, address: &str) -> Ad4mDbResult<Option<String>>;
    fn get_user_free_access(&self, email: &str) -> Ad4mDbResult<bool>;
    fn set_user_free_access(&self, email: &str, enabled: bool) -> Ad4mDbResult<()>;

    // ── Compute log ───────────────────────────────────────────────────────────

    fn insert_compute_log(
        &self,
        email: &str,
        operation: &str,
        summary: Option<&str>,
        cost: f64,
        credits_after: f64,
    ) -> Ad4mDbResult<i64>;
    fn get_compute_log(
        &self,
        email: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Ad4mDbResult<Vec<ComputeLogEntry>>;
    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Ad4mDbResult<Vec<ComputeLogEntry>>;
    fn cleanup_compute_log(&self, before: &str) -> Ad4mDbResult<usize>;

    // ── AI / Models ───────────────────────────────────────────────────────────

    fn create_or_update_model_status(
        &self,
        model: &str,
        progress: f64,
        status: &str,
        downloaded: bool,
        loaded: bool,
    ) -> Ad4mDbResult<()>;
    fn get_model_status(&self, model: &str) -> Ad4mDbResult<Option<AIModelLoadingStatus>>;
    fn add_task(
        &self,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Ad4mDbResult<String>;
    fn remove_task(&self, id: String) -> Ad4mDbResult<()>;
    fn get_task(&self, id: String) -> Ad4mDbResult<Option<AITask>>;
    fn get_tasks(&self) -> Ad4mDbResult<Vec<AITask>>;
    fn update_task(
        &self,
        id: String,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Ad4mDbResult<bool>;
    fn add_model(&self, model: &ModelInput) -> Ad4mDbResult<String>;
    fn get_model(&self, model_id: String) -> Ad4mDbResult<Option<Model>>;
    fn get_models(&self) -> Ad4mDbResult<Vec<Model>>;
    fn update_model(&self, id: &str, model: &ModelInput) -> Ad4mDbResult<()>;
    fn remove_model(&self, id: &str) -> Ad4mDbResult<()>;
    fn set_default_model(&self, model_type: ModelType, model_id: &str) -> Ad4mDbResult<()>;
    fn get_default_model(&self, model_type: ModelType) -> Ad4mDbResult<Option<String>>;

    // ── Notifications ─────────────────────────────────────────────────────────

    fn add_notification(
        &self,
        notification: NotificationInput,
        user_email: Option<String>,
    ) -> Ad4mDbResult<String>;
    fn get_notification(&self, id: String) -> Ad4mDbResult<Option<Notification>>;
    fn get_notifications(&self) -> Ad4mDbResult<Vec<Notification>>;
    fn get_notifications_for_user(
        &self,
        user_email: Option<String>,
    ) -> Ad4mDbResult<Vec<Notification>>;
    fn update_notification(&self, id: String, notification: &Notification) -> Ad4mDbResult<bool>;
    fn remove_notification(&self, id: String) -> Ad4mDbResult<()>;

    // ── Social ────────────────────────────────────────────────────────────────

    fn add_friends(&self, friends: Vec<String>) -> Ad4mDbResult<()>;
    fn remove_friends(&self, friends: Vec<String>) -> Ad4mDbResult<()>;
    fn get_all_friends(&self) -> Ad4mDbResult<Vec<String>>;
    fn add_trusted_agents(&self, agents: Vec<String>) -> Ad4mDbResult<()>;
    fn remove_trusted_agents(&self, agents: Vec<String>) -> Ad4mDbResult<()>;
    fn get_all_trusted_agents(&self) -> Ad4mDbResult<Vec<String>>;
    fn add_entanglement_proofs(&self, proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()>;
    fn remove_entanglement_proofs(&self, proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()>;
    fn get_all_entanglement_proofs(&self) -> Ad4mDbResult<Vec<EntanglementProof>>;
    fn add_to_outbox(&self, message: &PerspectiveExpression, recipient: String)
        -> Ad4mDbResult<()>;
    fn get_all_from_outbox(&self) -> Ad4mDbResult<Vec<SentMessage>>;
    fn add_known_link_languages(&self, languages: Vec<String>) -> Ad4mDbResult<()>;
    fn remove_known_link_languages(&self, languages: Vec<String>) -> Ad4mDbResult<()>;
    fn get_all_known_link_languages(&self) -> Ad4mDbResult<Vec<String>>;

    // ── Perspectives ──────────────────────────────────────────────────────────

    fn add_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()>;
    fn get_all_perspectives(&self) -> Ad4mDbResult<Vec<PerspectiveHandle>>;
    fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()>;
    fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()>;
    fn add_owner_to_neighbourhood(
        &self,
        neighbourhood_url: &str,
        user_did: &str,
    ) -> Ad4mDbResult<()>;
    fn get_neighbourhood_owners(&self, neighbourhood_url: &str) -> Ad4mDbResult<Vec<String>>;

    // ── Links ─────────────────────────────────────────────────────────────────

    fn add_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()>;
    fn add_many_links(
        &self,
        perspective_uuid: &str,
        links: Vec<LinkExpression>,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()>;
    fn update_link(
        &self,
        perspective_uuid: &str,
        old_link: &LinkExpression,
        new_link: &LinkExpression,
    ) -> Ad4mDbResult<()>;
    fn remove_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()>;
    fn get_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
    ) -> Ad4mDbResult<Option<(LinkExpression, LinkStatus)>>;
    fn get_all_links(
        &self,
        perspective_uuid: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
    fn get_links_by_source(
        &self,
        perspective_uuid: &str,
        source: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
    fn get_links_by_target(
        &self,
        perspective_uuid: &str,
        target: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
    fn get_links_by_predicate(
        &self,
        perspective_uuid: &str,
        predicate: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
    fn is_perspective_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<bool>;
    fn mark_perspective_as_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<()>;
    fn delete_all_links_for_perspective(&self, perspective_uuid: &str) -> Ad4mDbResult<usize>;
    fn add_pending_diff(&self, perspective_uuid: &str, diff: &PerspectiveDiff) -> Ad4mDbResult<()>;
    fn get_pending_diffs(
        &self,
        perspective_uuid: &str,
        max_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>;
    fn get_pending_diffs_by_size(
        &self,
        perspective_uuid: &str,
        max_bytes: usize,
        initial_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>;
    fn clear_pending_diffs(&self, perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()>;

    // ── Expressions ───────────────────────────────────────────────────────────

    fn add_expression(&self, url: &str, expression: &Expression<Value>) -> Ad4mDbResult<()>;
    fn get_expression(&self, url: &str) -> Ad4mDbResult<Option<Expression<Value>>>;

    // ── Billing / Hosting ─────────────────────────────────────────────────────

    fn set_host_rates(&self, rates: &[(String, f64)]) -> Ad4mDbResult<()>;
    fn get_host_rates(&self) -> Ad4mDbResult<Vec<(String, f64)>>;
    fn get_host_rate(&self, description: &str) -> Ad4mDbResult<Option<f64>>;
    fn create_payment_request(
        &self,
        email: &str,
        amount_hot: &str,
        action_hash: &str,
    ) -> Ad4mDbResult<i64>;
    fn complete_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()>;
    fn reject_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()>;
    fn get_payment_request_by_hash(
        &self,
        action_hash: &str,
    ) -> Ad4mDbResult<Option<PaymentRequest>>;
    fn get_pending_payment_requests(&self) -> Ad4mDbResult<Vec<PaymentRequest>>;
    fn get_all_payment_requests(&self) -> Ad4mDbResult<Vec<(String, String, String, String)>>;
    fn create_pending_send(
        &self,
        recipient: &str,
        amount_hot: &str,
        proposal_hash: &str,
    ) -> Ad4mDbResult<i64>;
    fn complete_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()>;
    fn reject_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()>;
    fn get_pending_send_by_hash(
        &self,
        proposal_hash: &str,
    ) -> Ad4mDbResult<Option<(String, String, String)>>;
    fn get_pending_sends(&self) -> Ad4mDbResult<Vec<(String, String, String)>>;
    fn get_all_sends(&self) -> Ad4mDbResult<Vec<(String, String, String, String)>>;

    // ── Auth / Verification ───────────────────────────────────────────────────

    fn has_verification_code(&self, email: &str) -> Ad4mDbResult<bool>;
    fn create_verification_code(
        &self,
        email: &str,
        verification_type: &str,
    ) -> Ad4mDbResult<String>;
    fn verify_code(&self, email: &str, code: &str, verification_type: &str) -> Ad4mDbResult<bool>;
    fn cleanup_expired_codes(&self) -> Ad4mDbResult<()>;
    fn check_and_update_rate_limit(&self, email: &str) -> Ad4mDbResult<()>;
    fn set_verification_code_expiry(
        &self,
        email: &str,
        verification_type: &str,
        expires_at: i64,
    ) -> Ad4mDbResult<()>;

    // ── Import / Export ───────────────────────────────────────────────────────

    fn export_all_to_json(&self) -> Ad4mDbResult<Value>;
    fn import_from_json(&self, data: Value) -> Ad4mDbResult<crate::types::ImportResult>;
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
    // ── Generic key-value (kept for shared-backend rehydration code) ──────

    fn get(&self, _did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        Ad4mDb::with_global_instance(|db| match table {
            "users" => match db.get_user(row_id) {
                Ok(u) => Ok(Some(serde_json::to_value(u)?)),
                Err(_) => Ok(None),
            },
            "settings" => match db.get_setting(row_id) {
                Ok(Some(v)) => Ok(Some(Value::String(v))),
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            },
            _ => Err(anyhow!("LocalDb: unknown table '{}'", table)),
        })
    }

    fn list(&self, _did: &str, table: &str) -> Result<Vec<Value>, AnyError> {
        Ad4mDb::with_global_instance(|db| match table {
            "users" => {
                let users = db.list_users()?;
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
        log::debug!(
            "LocalDb::delete: no-op for table '{}', row '{}'",
            table,
            row_id
        );
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    // ── Settings ──────────────────────────────────────────────────────────

    fn get_setting(&self, key: &str) -> Ad4mDbResult<Option<String>> {
        Ad4mDb::with_global_instance(|db| db.get_setting(key))
    }
    fn set_setting(&self, key: &str, value: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_setting(key, value))
    }
    fn get_multi_user_enabled(&self) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.get_multi_user_enabled())
    }
    fn set_multi_user_enabled(&self, enabled: bool) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_multi_user_enabled(enabled))
    }
    fn get_free_hosting_enabled(&self) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
    }
    fn set_free_hosting_enabled(&self, enabled: bool) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(enabled))
    }

    // ── Users ─────────────────────────────────────────────────────────────

    fn add_user(&self, username: &str, did: &str, password: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_user(username, did, password))
    }
    fn add_user_prehashed(
        &self,
        username: &str,
        did: &str,
        password_hash: &str,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_user_prehashed(username, did, password_hash))
    }
    fn get_user(&self, username: &str) -> Ad4mDbResult<UserInfo> {
        Ad4mDb::with_global_instance(|db| db.get_user(username))
    }
    fn update_user_last_seen(&self, email: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.update_user_last_seen(email))
    }
    fn list_users(&self) -> Ad4mDbResult<Vec<UserInfo>> {
        Ad4mDb::with_global_instance(|db| db.list_users())
    }
    fn list_user_statistics(&self) -> Ad4mDbResult<Vec<UserStatistics>> {
        Ad4mDb::with_global_instance(|db| db.list_user_statistics())
    }
    fn verify_user_password(&self, username: &str, password: &str) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.verify_user_password(username, password))
    }
    fn get_user_credits(&self, email: &str) -> Ad4mDbResult<f64> {
        Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
    }
    fn set_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_user_credits(email, amount))
    }
    fn add_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_user_credits(email, amount))
    }
    fn deduct_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.deduct_user_credits(email, amount))
    }
    fn deduct_user_credits_if_available(&self, email: &str, amount: f64) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.deduct_user_credits_if_available(email, amount))
    }
    fn deduct_credits_and_log(
        &self,
        email: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Ad4mDbResult<(i64, f64)> {
        Ad4mDb::with_global_instance(|db| {
            db.deduct_credits_and_log(email, amount, operation, summary)
        })
    }
    fn get_user_hot_wallet(&self, email: &str) -> Ad4mDbResult<Option<String>> {
        Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(email))
    }
    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(email, address))
    }
    fn get_user_by_hot_wallet_address(&self, address: &str) -> Ad4mDbResult<Option<String>> {
        Ad4mDb::with_global_instance(|db| db.get_user_by_hot_wallet_address(address))
    }
    fn get_user_free_access(&self, email: &str) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
    }
    fn set_user_free_access(&self, email: &str, enabled: bool) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_user_free_access(email, enabled))
    }

    // ── Compute log ───────────────────────────────────────────────────────

    fn insert_compute_log(
        &self,
        email: &str,
        operation: &str,
        summary: Option<&str>,
        cost: f64,
        credits_after: f64,
    ) -> Ad4mDbResult<i64> {
        Ad4mDb::with_global_instance(|db| {
            db.insert_compute_log(email, operation, summary, cost, credits_after)
        })
    }
    fn get_compute_log(
        &self,
        email: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Ad4mDbResult<Vec<ComputeLogEntry>> {
        Ad4mDb::with_global_instance(|db| db.get_compute_log(email, since, limit))
    }
    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Ad4mDbResult<Vec<ComputeLogEntry>> {
        Ad4mDb::with_global_instance(|db| db.get_compute_log_all(since, limit))
    }
    fn cleanup_compute_log(&self, before: &str) -> Ad4mDbResult<usize> {
        Ad4mDb::with_global_instance(|db| db.cleanup_compute_log(before))
    }

    // ── AI / Models ───────────────────────────────────────────────────────

    fn create_or_update_model_status(
        &self,
        model: &str,
        progress: f64,
        status: &str,
        downloaded: bool,
        loaded: bool,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.create_or_update_model_status(model, progress, status, downloaded, loaded)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_model_status(&self, model: &str) -> Ad4mDbResult<Option<AIModelLoadingStatus>> {
        Ad4mDb::with_global_instance(|db| db.get_model_status(model).map_err(|e| anyhow!("{}", e)))
    }
    fn add_task(
        &self,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Ad4mDbResult<String> {
        Ad4mDb::with_global_instance(|db| {
            db.add_task(name, model_id, system_prompt, prompt_examples, metadata)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn remove_task(&self, id: String) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_task(id).map_err(|e| anyhow!("{}", e)))
    }
    fn get_task(&self, id: String) -> Ad4mDbResult<Option<AITask>> {
        Ad4mDb::with_global_instance(|db| db.get_task(id).map_err(|e| anyhow!("{}", e)))
    }
    fn get_tasks(&self) -> Ad4mDbResult<Vec<AITask>> {
        Ad4mDb::with_global_instance(|db| db.get_tasks().map_err(|e| anyhow!("{}", e)))
    }
    fn update_task(
        &self,
        id: String,
        name: String,
        model_id: String,
        system_prompt: String,
        prompt_examples: Vec<AIPromptExamples>,
        metadata: Option<String>,
    ) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| {
            db.update_task(id, name, model_id, system_prompt, prompt_examples, metadata)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn add_model(&self, model: &ModelInput) -> Ad4mDbResult<String> {
        Ad4mDb::with_global_instance(|db| db.add_model(model))
    }
    fn get_model(&self, model_id: String) -> Ad4mDbResult<Option<Model>> {
        Ad4mDb::with_global_instance(|db| db.get_model(model_id))
    }
    fn get_models(&self) -> Ad4mDbResult<Vec<Model>> {
        Ad4mDb::with_global_instance(|db| db.get_models())
    }
    fn update_model(&self, id: &str, model: &ModelInput) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.update_model(id, model))
    }
    fn remove_model(&self, id: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_model(id))
    }
    fn set_default_model(&self, model_type: ModelType, model_id: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_default_model(model_type, model_id))
    }
    fn get_default_model(&self, model_type: ModelType) -> Ad4mDbResult<Option<String>> {
        Ad4mDb::with_global_instance(|db| db.get_default_model(model_type))
    }

    // ── Notifications ─────────────────────────────────────────────────────

    fn add_notification(
        &self,
        notification: NotificationInput,
        user_email: Option<String>,
    ) -> Ad4mDbResult<String> {
        Ad4mDb::with_global_instance(|db| {
            db.add_notification(notification, user_email)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_notification(&self, id: String) -> Ad4mDbResult<Option<Notification>> {
        Ad4mDb::with_global_instance(|db| db.get_notification(id).map_err(|e| anyhow!("{}", e)))
    }
    fn get_notifications(&self) -> Ad4mDbResult<Vec<Notification>> {
        Ad4mDb::with_global_instance(|db| db.get_notifications().map_err(|e| anyhow!("{}", e)))
    }
    fn get_notifications_for_user(
        &self,
        user_email: Option<String>,
    ) -> Ad4mDbResult<Vec<Notification>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_notifications_for_user(user_email)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn update_notification(&self, id: String, notification: &Notification) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| {
            db.update_notification(id, notification)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn remove_notification(&self, id: String) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_notification(id).map_err(|e| anyhow!("{}", e)))
    }

    // ── Social ────────────────────────────────────────────────────────────

    fn add_friends(&self, friends: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_friends(friends).map_err(|e| anyhow!("{}", e)))
    }
    fn remove_friends(&self, friends: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_friends(friends).map_err(|e| anyhow!("{}", e)))
    }
    fn get_all_friends(&self) -> Ad4mDbResult<Vec<String>> {
        Ad4mDb::with_global_instance(|db| db.get_all_friends().map_err(|e| anyhow!("{}", e)))
    }
    fn add_trusted_agents(&self, agents: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.add_trusted_agents(agents).map_err(|e| anyhow!("{}", e))
        })
    }
    fn remove_trusted_agents(&self, agents: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.remove_trusted_agents(agents)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_all_trusted_agents(&self) -> Ad4mDbResult<Vec<String>> {
        Ad4mDb::with_global_instance(|db| db.get_all_trusted_agents().map_err(|e| anyhow!("{}", e)))
    }
    fn add_entanglement_proofs(&self, proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.add_entanglement_proofs(proofs)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn remove_entanglement_proofs(&self, proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.remove_entanglement_proofs(proofs)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_all_entanglement_proofs(&self) -> Ad4mDbResult<Vec<EntanglementProof>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_all_entanglement_proofs()
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn add_to_outbox(
        &self,
        message: &PerspectiveExpression,
        recipient: String,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.add_to_outbox(message, recipient)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_all_from_outbox(&self) -> Ad4mDbResult<Vec<SentMessage>> {
        Ad4mDb::with_global_instance(|db| db.get_all_from_outbox().map_err(|e| anyhow!("{}", e)))
    }
    fn add_known_link_languages(&self, languages: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.add_known_link_languages(languages)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn remove_known_link_languages(&self, languages: Vec<String>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.remove_known_link_languages(languages)
                .map_err(|e| anyhow!("{}", e))
        })
    }
    fn get_all_known_link_languages(&self) -> Ad4mDbResult<Vec<String>> {
        Ad4mDb::with_global_instance(|db| {
            db.get_all_known_link_languages()
                .map_err(|e| anyhow!("{}", e))
        })
    }

    // ── Perspectives ──────────────────────────────────────────────────────

    fn add_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_perspective(perspective))
    }
    fn get_all_perspectives(&self) -> Ad4mDbResult<Vec<PerspectiveHandle>> {
        Ad4mDb::with_global_instance(|db| db.get_all_perspectives())
    }
    fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.update_perspective(perspective))
    }
    fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_perspective(uuid))
    }
    fn add_owner_to_neighbourhood(
        &self,
        neighbourhood_url: &str,
        user_did: &str,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.add_owner_to_neighbourhood(neighbourhood_url, user_did)
        })
    }
    fn get_neighbourhood_owners(&self, neighbourhood_url: &str) -> Ad4mDbResult<Vec<String>> {
        Ad4mDb::with_global_instance(|db| db.get_neighbourhood_owners(neighbourhood_url))
    }

    // ── Links ─────────────────────────────────────────────────────────────

    fn add_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_link(perspective_uuid, link, status))
    }
    fn add_many_links(
        &self,
        perspective_uuid: &str,
        links: Vec<LinkExpression>,
        status: &LinkStatus,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_many_links(perspective_uuid, links, status))
    }
    fn update_link(
        &self,
        perspective_uuid: &str,
        old_link: &LinkExpression,
        new_link: &LinkExpression,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.update_link(perspective_uuid, old_link, new_link))
    }
    fn remove_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.remove_link(perspective_uuid, link))
    }
    fn get_link(
        &self,
        perspective_uuid: &str,
        link: &LinkExpression,
    ) -> Ad4mDbResult<Option<(LinkExpression, LinkStatus)>> {
        Ad4mDb::with_global_instance(|db| db.get_link(perspective_uuid, link))
    }
    fn get_all_links(
        &self,
        perspective_uuid: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        Ad4mDb::with_global_instance(|db| db.get_all_links(perspective_uuid))
    }
    fn get_links_by_source(
        &self,
        perspective_uuid: &str,
        source: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        Ad4mDb::with_global_instance(|db| db.get_links_by_source(perspective_uuid, source))
    }
    fn get_links_by_target(
        &self,
        perspective_uuid: &str,
        target: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        Ad4mDb::with_global_instance(|db| db.get_links_by_target(perspective_uuid, target))
    }
    fn get_links_by_predicate(
        &self,
        perspective_uuid: &str,
        predicate: &str,
    ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>> {
        Ad4mDb::with_global_instance(|db| db.get_links_by_predicate(perspective_uuid, predicate))
    }
    fn is_perspective_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.is_perspective_migrated(perspective_uuid))
    }
    fn mark_perspective_as_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.mark_perspective_as_migrated(perspective_uuid))
    }
    fn delete_all_links_for_perspective(&self, perspective_uuid: &str) -> Ad4mDbResult<usize> {
        Ad4mDb::with_global_instance(|db| db.delete_all_links_for_perspective(perspective_uuid))
    }
    fn add_pending_diff(&self, perspective_uuid: &str, diff: &PerspectiveDiff) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.add_pending_diff(perspective_uuid, diff))
    }
    fn get_pending_diffs(
        &self,
        perspective_uuid: &str,
        max_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)> {
        Ad4mDb::with_global_instance(|db| db.get_pending_diffs(perspective_uuid, max_count))
    }
    fn get_pending_diffs_by_size(
        &self,
        perspective_uuid: &str,
        max_bytes: usize,
        initial_count: Option<usize>,
    ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)> {
        Ad4mDb::with_global_instance(|db| {
            db.get_pending_diffs_by_size(perspective_uuid, max_bytes, initial_count)
        })
    }
    fn clear_pending_diffs(&self, perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.clear_pending_diffs(perspective_uuid, ids))
    }

    // ── Expressions ───────────────────────────────────────────────────────

    fn add_expression(&self, url: &str, expression: &Expression<Value>) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db._add_expression(url, expression))
    }
    fn get_expression(&self, url: &str) -> Ad4mDbResult<Option<Expression<Value>>> {
        Ad4mDb::with_global_instance(|db| db._get_expression(url))
    }

    // ── Billing / Hosting ─────────────────────────────────────────────────

    fn set_host_rates(&self, rates: &[(String, f64)]) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.set_host_rates(rates))
    }
    fn get_host_rates(&self) -> Ad4mDbResult<Vec<(String, f64)>> {
        Ad4mDb::with_global_instance(|db| db.get_host_rates())
    }
    fn get_host_rate(&self, description: &str) -> Ad4mDbResult<Option<f64>> {
        Ad4mDb::with_global_instance(|db| db.get_host_rate(description))
    }
    fn create_payment_request(
        &self,
        email: &str,
        amount_hot: &str,
        action_hash: &str,
    ) -> Ad4mDbResult<i64> {
        Ad4mDb::with_global_instance(|db| db.create_payment_request(email, amount_hot, action_hash))
    }
    fn complete_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.complete_payment_request(action_hash))
    }
    fn reject_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.reject_payment_request(action_hash))
    }
    fn get_payment_request_by_hash(
        &self,
        action_hash: &str,
    ) -> Ad4mDbResult<Option<PaymentRequest>> {
        Ad4mDb::with_global_instance(|db| db.get_payment_request_by_hash(action_hash))
    }
    fn get_pending_payment_requests(&self) -> Ad4mDbResult<Vec<PaymentRequest>> {
        Ad4mDb::with_global_instance(|db| db.get_pending_payment_requests())
    }
    fn get_all_payment_requests(&self) -> Ad4mDbResult<Vec<(String, String, String, String)>> {
        Ad4mDb::with_global_instance(|db| db.get_all_payment_requests())
    }
    fn create_pending_send(
        &self,
        recipient: &str,
        amount_hot: &str,
        proposal_hash: &str,
    ) -> Ad4mDbResult<i64> {
        Ad4mDb::with_global_instance(|db| {
            db.create_pending_send(recipient, amount_hot, proposal_hash)
        })
    }
    fn complete_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.complete_pending_send(proposal_hash))
    }
    fn reject_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.reject_pending_send(proposal_hash))
    }
    fn get_pending_send_by_hash(
        &self,
        proposal_hash: &str,
    ) -> Ad4mDbResult<Option<(String, String, String)>> {
        Ad4mDb::with_global_instance(|db| db.get_pending_send_by_hash(proposal_hash))
    }
    fn get_pending_sends(&self) -> Ad4mDbResult<Vec<(String, String, String)>> {
        Ad4mDb::with_global_instance(|db| db.get_pending_sends())
    }
    fn get_all_sends(&self) -> Ad4mDbResult<Vec<(String, String, String, String)>> {
        Ad4mDb::with_global_instance(|db| db.get_all_sends())
    }

    // ── Auth / Verification ───────────────────────────────────────────────

    fn has_verification_code(&self, email: &str) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.has_verification_code(email))
    }
    fn create_verification_code(
        &self,
        email: &str,
        verification_type: &str,
    ) -> Ad4mDbResult<String> {
        Ad4mDb::with_global_instance(|db| db.create_verification_code(email, verification_type))
    }
    fn verify_code(&self, email: &str, code: &str, verification_type: &str) -> Ad4mDbResult<bool> {
        Ad4mDb::with_global_instance(|db| db.verify_code(email, code, verification_type))
    }
    fn cleanup_expired_codes(&self) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.cleanup_expired_codes())
    }
    fn check_and_update_rate_limit(&self, email: &str) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| db.check_and_update_rate_limit(email))
    }
    fn set_verification_code_expiry(
        &self,
        email: &str,
        verification_type: &str,
        expires_at: i64,
    ) -> Ad4mDbResult<()> {
        Ad4mDb::with_global_instance(|db| {
            db.set_verification_code_expiry(email, verification_type, expires_at)
        })
    }

    // ── Import / Export ───────────────────────────────────────────────────

    fn export_all_to_json(&self) -> Ad4mDbResult<Value> {
        Ad4mDb::with_global_instance(|db| db.export_all_to_json())
    }
    fn import_from_json(&self, data: Value) -> Ad4mDbResult<crate::types::ImportResult> {
        Ad4mDb::with_global_instance(|db| db.import_from_json(data))
    }
}

// ── SharedDb ───────────────────────────────────────────────────────────────────

/// HTTP client that calls the platform Worker's `/internal/db/` API.
/// Uses `reqwest::blocking` for consistency with SharedWallet.
///
/// Domain methods return explicit errors until matching Worker endpoints exist.
/// The generic get/list/upsert/delete API remains for shared-backend rehydration
/// code that routes by table name.
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
            client: reqwest::blocking::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .expect("Failed to build SharedDb HTTP client"),
            cache: RwLock::new(HashMap::new()),
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }

    fn cache_key(did: &str, table: &str, row_id: &str) -> String {
        format!("{}:{}:{}", did, table, row_id)
    }

    /// Return a standardised error for domain methods not yet available via HTTP.
    fn not_supported(method: &str) -> AnyError {
        anyhow!(
            "SharedDb: {} not yet available in shared mode — \
             add a Worker endpoint and implement the HTTP call here",
            method
        )
    }
}

/// Macro to generate SharedDb domain method stubs that return "not supported".
/// Each method signature must match the trait exactly.
macro_rules! shared_not_supported {
    // No-arg methods returning Result<T, AnyError>
    ($name:ident () -> $ret:ty) => {
        fn $name(&self) -> $ret {
            Err(SharedDb::not_supported(stringify!($name)))
        }
    };
    // Methods with args
    ($name:ident ($($arg:ident : $ty:ty),+) -> $ret:ty) => {
        fn $name(&self, $($arg: $ty),+) -> $ret {
            $(let _ = $arg;)+
            Err(SharedDb::not_supported(stringify!($name)))
        }
    };
}

impl DbBackend for SharedDb {
    // ── Generic key-value API ─────────────────────────────────────────────

    fn get(&self, did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        let key = Self::cache_key(did, table, row_id);
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
        const MAX_ROWS: usize = 10_000;

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
        let total = rows.len();

        if total > MAX_ROWS {
            log::warn!(
                "SharedDb::list: table '{}/{}' returned {} rows, truncating to {}",
                did,
                table,
                total,
                MAX_ROWS
            );
        }

        Ok(rows
            .into_iter()
            .take(MAX_ROWS)
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

        let key = Self::cache_key(did, table, row_id);
        if let Ok(mut cache) = self.cache.write() {
            cache.remove(&key);
        }

        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    // ── Domain methods — all return explicit errors in shared mode ─────────
    // When the platform Worker gains a domain-specific endpoint for a method,
    // replace the stub with an HTTP call to that endpoint.

    // Settings
    shared_not_supported!(get_setting(key: &str) -> Ad4mDbResult<Option<String>>);
    shared_not_supported!(set_setting(key: &str, value: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_multi_user_enabled() -> Ad4mDbResult<bool>);
    shared_not_supported!(set_multi_user_enabled(enabled: bool) -> Ad4mDbResult<()>);
    shared_not_supported!(get_free_hosting_enabled() -> Ad4mDbResult<bool>);
    shared_not_supported!(set_free_hosting_enabled(enabled: bool) -> Ad4mDbResult<()>);

    // Users
    shared_not_supported!(add_user(username: &str, did: &str, password: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(add_user_prehashed(username: &str, did: &str, password_hash: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_user(username: &str) -> Ad4mDbResult<UserInfo>);
    shared_not_supported!(update_user_last_seen(email: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(list_users() -> Ad4mDbResult<Vec<UserInfo>>);
    shared_not_supported!(list_user_statistics() -> Ad4mDbResult<Vec<UserStatistics>>);
    shared_not_supported!(verify_user_password(username: &str, password: &str) -> Ad4mDbResult<bool>);
    shared_not_supported!(get_user_credits(email: &str) -> Ad4mDbResult<f64>);
    shared_not_supported!(set_user_credits(email: &str, amount: f64) -> Ad4mDbResult<()>);
    shared_not_supported!(add_user_credits(email: &str, amount: f64) -> Ad4mDbResult<()>);
    shared_not_supported!(deduct_user_credits(email: &str, amount: f64) -> Ad4mDbResult<()>);
    shared_not_supported!(deduct_user_credits_if_available(email: &str, amount: f64) -> Ad4mDbResult<()>);
    shared_not_supported!(deduct_credits_and_log(email: &str, amount: f64, operation: &str, summary: Option<&str>) -> Ad4mDbResult<(i64, f64)>);
    shared_not_supported!(get_user_hot_wallet(email: &str) -> Ad4mDbResult<Option<String>>);
    shared_not_supported!(set_user_hot_wallet(email: &str, address: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_user_by_hot_wallet_address(address: &str) -> Ad4mDbResult<Option<String>>);
    shared_not_supported!(get_user_free_access(email: &str) -> Ad4mDbResult<bool>);
    shared_not_supported!(set_user_free_access(email: &str, enabled: bool) -> Ad4mDbResult<()>);

    // Compute log
    shared_not_supported!(insert_compute_log(email: &str, operation: &str, summary: Option<&str>, cost: f64, credits_after: f64) -> Ad4mDbResult<i64>);
    shared_not_supported!(get_compute_log(email: &str, since: Option<&str>, limit: i64) -> Ad4mDbResult<Vec<ComputeLogEntry>>);
    shared_not_supported!(get_compute_log_all(since: Option<&str>, limit: i64) -> Ad4mDbResult<Vec<ComputeLogEntry>>);
    shared_not_supported!(cleanup_compute_log(before: &str) -> Ad4mDbResult<usize>);

    // AI / Models
    shared_not_supported!(create_or_update_model_status(model: &str, progress: f64, status: &str, downloaded: bool, loaded: bool) -> Ad4mDbResult<()>);
    shared_not_supported!(get_model_status(model: &str) -> Ad4mDbResult<Option<AIModelLoadingStatus>>);
    shared_not_supported!(add_task(name: String, model_id: String, system_prompt: String, prompt_examples: Vec<AIPromptExamples>, metadata: Option<String>) -> Ad4mDbResult<String>);
    shared_not_supported!(remove_task(id: String) -> Ad4mDbResult<()>);
    shared_not_supported!(get_task(id: String) -> Ad4mDbResult<Option<AITask>>);
    shared_not_supported!(get_tasks() -> Ad4mDbResult<Vec<AITask>>);
    shared_not_supported!(update_task(id: String, name: String, model_id: String, system_prompt: String, prompt_examples: Vec<AIPromptExamples>, metadata: Option<String>) -> Ad4mDbResult<bool>);
    shared_not_supported!(add_model(model: &ModelInput) -> Ad4mDbResult<String>);
    shared_not_supported!(get_model(model_id: String) -> Ad4mDbResult<Option<Model>>);
    shared_not_supported!(get_models() -> Ad4mDbResult<Vec<Model>>);
    shared_not_supported!(update_model(id: &str, model: &ModelInput) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_model(id: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(set_default_model(model_type: ModelType, model_id: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_default_model(model_type: ModelType) -> Ad4mDbResult<Option<String>>);

    // Notifications
    shared_not_supported!(add_notification(notification: NotificationInput, user_email: Option<String>) -> Ad4mDbResult<String>);
    shared_not_supported!(get_notification(id: String) -> Ad4mDbResult<Option<Notification>>);
    shared_not_supported!(get_notifications() -> Ad4mDbResult<Vec<Notification>>);
    shared_not_supported!(get_notifications_for_user(user_email: Option<String>) -> Ad4mDbResult<Vec<Notification>>);
    shared_not_supported!(update_notification(id: String, notification: &Notification) -> Ad4mDbResult<bool>);
    shared_not_supported!(remove_notification(id: String) -> Ad4mDbResult<()>);

    // Social
    shared_not_supported!(add_friends(friends: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_friends(friends: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_friends() -> Ad4mDbResult<Vec<String>>);
    shared_not_supported!(add_trusted_agents(agents: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_trusted_agents(agents: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_trusted_agents() -> Ad4mDbResult<Vec<String>>);
    shared_not_supported!(add_entanglement_proofs(proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_entanglement_proofs(proofs: Vec<EntanglementProof>) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_entanglement_proofs() -> Ad4mDbResult<Vec<EntanglementProof>>);
    shared_not_supported!(add_to_outbox(message: &PerspectiveExpression, recipient: String) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_from_outbox() -> Ad4mDbResult<Vec<SentMessage>>);
    shared_not_supported!(add_known_link_languages(languages: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_known_link_languages(languages: Vec<String>) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_known_link_languages() -> Ad4mDbResult<Vec<String>>);

    // Perspectives
    shared_not_supported!(add_perspective(perspective: &PerspectiveHandle) -> Ad4mDbResult<()>);
    shared_not_supported!(get_all_perspectives() -> Ad4mDbResult<Vec<PerspectiveHandle>>);
    shared_not_supported!(update_perspective(perspective: &PerspectiveHandle) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_perspective(uuid: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(add_owner_to_neighbourhood(neighbourhood_url: &str, user_did: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_neighbourhood_owners(neighbourhood_url: &str) -> Ad4mDbResult<Vec<String>>);

    // Links
    shared_not_supported!(add_link(perspective_uuid: &str, link: &LinkExpression, status: &LinkStatus) -> Ad4mDbResult<()>);
    shared_not_supported!(add_many_links(perspective_uuid: &str, links: Vec<LinkExpression>, status: &LinkStatus) -> Ad4mDbResult<()>);
    shared_not_supported!(update_link(perspective_uuid: &str, old_link: &LinkExpression, new_link: &LinkExpression) -> Ad4mDbResult<()>);
    shared_not_supported!(remove_link(perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()>);
    shared_not_supported!(get_link(perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<Option<(LinkExpression, LinkStatus)>>);
    shared_not_supported!(get_all_links(perspective_uuid: &str) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>);
    shared_not_supported!(get_links_by_source(perspective_uuid: &str, source: &str) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>);
    shared_not_supported!(get_links_by_target(perspective_uuid: &str, target: &str) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>);
    shared_not_supported!(get_links_by_predicate(perspective_uuid: &str, predicate: &str) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>);
    shared_not_supported!(is_perspective_migrated(perspective_uuid: &str) -> Ad4mDbResult<bool>);
    shared_not_supported!(mark_perspective_as_migrated(perspective_uuid: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(delete_all_links_for_perspective(perspective_uuid: &str) -> Ad4mDbResult<usize>);
    shared_not_supported!(add_pending_diff(perspective_uuid: &str, diff: &PerspectiveDiff) -> Ad4mDbResult<()>);
    shared_not_supported!(get_pending_diffs(perspective_uuid: &str, max_count: Option<usize>) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>);
    shared_not_supported!(get_pending_diffs_by_size(perspective_uuid: &str, max_bytes: usize, initial_count: Option<usize>) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>);
    shared_not_supported!(clear_pending_diffs(perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()>);

    // Expressions
    shared_not_supported!(add_expression(url: &str, expression: &Expression<Value>) -> Ad4mDbResult<()>);
    shared_not_supported!(get_expression(url: &str) -> Ad4mDbResult<Option<Expression<Value>>>);

    // Billing / Hosting
    shared_not_supported!(set_host_rates(rates: &[(String, f64)]) -> Ad4mDbResult<()>);
    shared_not_supported!(get_host_rates() -> Ad4mDbResult<Vec<(String, f64)>>);
    shared_not_supported!(get_host_rate(description: &str) -> Ad4mDbResult<Option<f64>>);
    shared_not_supported!(create_payment_request(email: &str, amount_hot: &str, action_hash: &str) -> Ad4mDbResult<i64>);
    shared_not_supported!(complete_payment_request(action_hash: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(reject_payment_request(action_hash: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_payment_request_by_hash(action_hash: &str) -> Ad4mDbResult<Option<PaymentRequest>>);
    shared_not_supported!(get_pending_payment_requests() -> Ad4mDbResult<Vec<PaymentRequest>>);
    shared_not_supported!(get_all_payment_requests() -> Ad4mDbResult<Vec<(String, String, String, String)>>);
    shared_not_supported!(create_pending_send(recipient: &str, amount_hot: &str, proposal_hash: &str) -> Ad4mDbResult<i64>);
    shared_not_supported!(complete_pending_send(proposal_hash: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(reject_pending_send(proposal_hash: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(get_pending_send_by_hash(proposal_hash: &str) -> Ad4mDbResult<Option<(String, String, String)>>);
    shared_not_supported!(get_pending_sends() -> Ad4mDbResult<Vec<(String, String, String)>>);
    shared_not_supported!(get_all_sends() -> Ad4mDbResult<Vec<(String, String, String, String)>>);

    // Auth / Verification
    shared_not_supported!(has_verification_code(email: &str) -> Ad4mDbResult<bool>);
    shared_not_supported!(create_verification_code(email: &str, verification_type: &str) -> Ad4mDbResult<String>);
    shared_not_supported!(verify_code(email: &str, code: &str, verification_type: &str) -> Ad4mDbResult<bool>);
    shared_not_supported!(cleanup_expired_codes() -> Ad4mDbResult<()>);
    shared_not_supported!(check_and_update_rate_limit(email: &str) -> Ad4mDbResult<()>);
    shared_not_supported!(set_verification_code_expiry(email: &str, verification_type: &str, expires_at: i64) -> Ad4mDbResult<()>);

    // Import / Export
    shared_not_supported!(export_all_to_json() -> Ad4mDbResult<Value>);
    shared_not_supported!(import_from_json(data: Value) -> Ad4mDbResult<crate::types::ImportResult>);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shared_db_get() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/did:test/settings/theme")
            .match_header("Authorization", "Bearer db-tok")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"data": "{\"value\":\"dark\"}"}"#)
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let result = db.get("did:test", "settings", "theme").unwrap();
        assert!(result.is_some());
        assert_eq!(
            result.unwrap().get("value").unwrap().as_str().unwrap(),
            "dark"
        );
        mock.assert();
    }

    #[test]
    fn test_shared_db_get_not_found() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/did:test/settings/missing")
            .with_status(404)
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let result = db.get("did:test", "settings", "missing").unwrap();
        assert!(result.is_none());
        mock.assert();
    }

    #[test]
    fn test_shared_db_list() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/did:test/users")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{"rows":[
                    {"rowId":"u1","data":"{\"email\":\"a@b.com\"}"},
                    {"rowId":"u2","data":"{\"email\":\"c@d.com\"}"}
                ]}"#,
            )
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let result = db.list("did:test", "users").unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].get("email").unwrap().as_str().unwrap(), "a@b.com");
        mock.assert();
    }

    #[test]
    fn test_shared_db_upsert() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/did:test/settings")
            .match_header("Authorization", "Bearer db-tok")
            .with_status(200)
            .with_body("{}")
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let data = serde_json::json!({"value": "light"});
        let result = db.upsert("did:test", "settings", "theme", data);

        assert!(result.is_ok());
        mock.assert();
    }

    #[test]
    fn test_shared_db_delete() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("DELETE", "/did:test/settings/theme")
            .match_header("Authorization", "Bearer db-tok")
            .with_status(200)
            .with_body("{}")
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let result = db.delete("did:test", "settings", "theme");

        assert!(result.is_ok());
        mock.assert();
    }

    #[test]
    fn test_shared_db_cache_invalidated_by_upsert() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock_get1 = server
            .mock("GET", "/did:test/settings/cached")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"data": "{\"v\":1}"}"#)
            .create();

        let db = SharedDb::new(url.clone(), "db-tok".to_string());
        let r1 = db.get("did:test", "settings", "cached").unwrap();
        assert_eq!(r1.unwrap().get("v").unwrap().as_i64().unwrap(), 1);
        mock_get1.assert();

        let mock_upsert = server
            .mock("POST", "/did:test/settings")
            .with_status(200)
            .with_body("{}")
            .create();
        db.upsert(
            "did:test",
            "settings",
            "cached",
            serde_json::json!({"v": 2}),
        )
        .unwrap();
        mock_upsert.assert();

        let mock_get2 = server
            .mock("GET", "/did:test/settings/cached")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"data": "{\"v\":2}"}"#)
            .create();
        let r2 = db.get("did:test", "settings", "cached").unwrap();
        assert_eq!(r2.unwrap().get("v").unwrap().as_i64().unwrap(), 2);
        mock_get2.assert();
    }

    #[test]
    fn test_shared_db_server_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/did:test/settings/err")
            .with_status(500)
            .create();

        let db = SharedDb::new(url, "db-tok".to_string());
        let result = db.get("did:test", "settings", "err");

        assert!(result.is_err());
        mock.assert();
    }

    #[test]
    fn test_shared_db_domain_methods_return_not_supported() {
        let db = SharedDb::new("http://unused".to_string(), "tok".to_string());

        // Spot-check a few domain methods
        let err = db.get_setting("key").unwrap_err();
        assert!(err.to_string().contains("not yet available in shared mode"));

        let err = db.list_users().unwrap_err();
        assert!(err.to_string().contains("not yet available in shared mode"));

        let err = db.get_tasks().unwrap_err();
        assert!(err.to_string().contains("not yet available in shared mode"));

        let err = db.export_all_to_json().unwrap_err();
        assert!(err.to_string().contains("not yet available in shared mode"));
    }
}
