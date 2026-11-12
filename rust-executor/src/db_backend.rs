//! The executor's database, behind one trait.
//!
//! Executor code reads and writes its database through [`db_backend()`], never through
//! `Ad4mDb` directly. The trait has two parts:
//!
//! - The domain methods: one per `Ad4mDb` method that executor code calls, with the same
//!   name, arguments and result. They are listed once, in [`ad4m_db_methods!`].
//! - The key-value methods (`get`, `list`, `upsert`, `delete`): rows of the platform's shared
//!   database. Executor code uses them for the records that executors share: users and
//!   perspective metadata under the `shared:platform` namespace.
//!
//! Two backends:
//!
//! - [`LocalDb`] runs every domain method on the executor's SQLite database (the global
//!   `Ad4mDb`).
//! - [`SharedDb`] serves the key-value methods from the platform Worker's `/internal/db/` API
//!   over HTTP, and runs every domain method on the executor's SQLite database, as `LocalDb`
//!   does. A shared-mode executor keeps its own SQLite database; only the shared records go
//!   to the Worker.
//!
//! Config: `DB_BACKEND` env var or `db_backend` in Ad4mConfig.
//! - "local" (default): [`LocalDb`]
//! - "shared": [`SharedDb`]

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;
use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};
use tokio::sync::OnceCell;

use crate::db::{
    Ad4mDb, Ad4mDbResult, ComputeLogEntry, FlowReplicaStateRow, GrantRow, JoinAccept, JoinAdded,
    JoinCancel, JoinRow, OperatorRecordRow, OperatorSetBy, PaymentRequest,
};
use crate::types::{
    AIModelLoadingStatus, AIPromptExamples, AITask, EntanglementProof, Expression, ImportResult,
    LinkExpression, LinkStatus, Model, ModelInput, ModelType, Notification, NotificationInput,
    PerspectiveDiff, PerspectiveExpression, PerspectiveHandle, SentMessage, UserInfo,
    UserStatistics,
};

// ── Domain methods ─────────────────────────────────────────────────────────────

/// The domain methods of [`DbBackend`]: every `Ad4mDb` method that executor code calls.
/// Passes the list to `$callback`, which declares the methods in the trait or implements
/// them for one backend. A method that executor code starts to call is added here, once.
macro_rules! ad4m_db_methods {
    ($callback:ident) => {
        $callback! {
            // Settings
            fn get_setting(&self, key: &str) -> Ad4mDbResult<Option<String>>;
            fn set_setting(&self, key: &str, value: &str) -> Ad4mDbResult<()>;
            fn get_multi_user_enabled(&self) -> Ad4mDbResult<bool>;
            fn set_multi_user_enabled(&self, enabled: bool) -> Ad4mDbResult<()>;
            fn get_free_hosting_enabled(&self) -> Ad4mDbResult<bool>;
            fn set_free_hosting_enabled(&self, enabled: bool) -> Ad4mDbResult<()>;

            // Users
            fn add_user(&self, username: &str, did: &str, password: &str) -> Ad4mDbResult<()>;
            fn add_user_prehashed(
                &self,
                username: &str,
                did: &str,
                password_hash: &str
            ) -> Ad4mDbResult<()>;
            fn get_user(&self, username: &str) -> Ad4mDbResult<UserInfo>;
            fn get_username_by_did(&self, did: &str) -> Ad4mDbResult<Option<String>>;
            fn update_user_last_seen(&self, email: &str) -> Ad4mDbResult<()>;
            fn update_agent_last_seen(&self, agent_did: &str) -> Ad4mDbResult<()>;
            fn get_user_by_did(&self, agent_did: &str) -> Ad4mDbResult<Option<UserInfo>>;
            fn list_users(&self) -> Ad4mDbResult<Vec<UserInfo>>;
            fn list_user_statistics(&self) -> Ad4mDbResult<Vec<UserStatistics>>;
            fn verify_user_password(&self, username: &str, password: &str) -> Ad4mDbResult<bool>;

            // Credits, free access and hot wallets of users and agents
            fn get_user_credits(&self, email: &str) -> Ad4mDbResult<f64>;
            fn set_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
            fn add_user_credits(&self, email: &str, amount: f64) -> Ad4mDbResult<()>;
            fn add_agent_credits(&self, agent_did: &str, amount: f64) -> Ad4mDbResult<String>;
            fn deduct_agent_credits_if_available(
                &self,
                agent_did: &str,
                amount: f64
            ) -> Ad4mDbResult<String>;
            fn deduct_credits_and_log(
                &self,
                agent_did: &str,
                amount: f64,
                operation: &str,
                summary: Option<&str>
            ) -> Ad4mDbResult<(i64, f64, String)>;
            fn get_agent_credits(&self, agent_did: &str) -> Ad4mDbResult<f64>;
            fn get_agent_free_access(&self, agent_did: &str) -> Ad4mDbResult<bool>;
            fn get_user_free_access(&self, email: &str) -> Ad4mDbResult<bool>;
            fn set_user_free_access(&self, email: &str, enabled: bool) -> Ad4mDbResult<()>;
            fn get_user_hot_wallet(&self, email: &str) -> Ad4mDbResult<Option<String>>;
            fn set_user_hot_wallet(&self, email: &str, address: &str) -> Ad4mDbResult<()>;
            fn get_user_by_hot_wallet_address(
                &self,
                address: &str
            ) -> Ad4mDbResult<Option<(String, String)>>;

            // Compute log
            fn insert_compute_log(
                &self,
                agent_did: &str,
                operation: &str,
                summary: Option<&str>,
                cost: f64,
                credits_after: f64
            ) -> Ad4mDbResult<i64>;
            fn get_compute_log(
                &self,
                agent_did: &str,
                since: Option<&str>,
                limit: i64
            ) -> Ad4mDbResult<Vec<ComputeLogEntry>>;

            // Hosting rates, payment requests and pending sends
            fn set_host_rates(&self, rates: &[(String, f64)]) -> Ad4mDbResult<()>;
            fn get_host_rates(&self) -> Ad4mDbResult<Vec<(String, f64)>>;
            fn get_host_rate(&self, description: &str) -> Ad4mDbResult<Option<f64>>;
            fn complete_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()>;
            fn reject_payment_request(&self, action_hash: &str) -> Ad4mDbResult<()>;
            fn get_payment_request_by_hash(
                &self,
                action_hash: &str
            ) -> Ad4mDbResult<Option<PaymentRequest>>;
            fn get_pending_payment_requests(&self) -> Ad4mDbResult<Vec<PaymentRequest>>;
            fn create_pending_send(
                &self,
                recipient: &str,
                amount_hot: &str,
                proposal_hash: &str
            ) -> Ad4mDbResult<i64>;
            fn complete_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()>;
            fn reject_pending_send(&self, proposal_hash: &str) -> Ad4mDbResult<()>;
            fn get_pending_send_by_hash(
                &self,
                proposal_hash: &str
            ) -> Ad4mDbResult<Option<(String, String, String)>>;
            fn get_pending_sends(&self) -> Ad4mDbResult<Vec<(String, String, String)>>;

            // Email verification and rate limits
            fn create_verification_code(
                &self,
                email: &str,
                verification_type: &str
            ) -> Ad4mDbResult<String>;
            fn verify_code(
                &self,
                email: &str,
                code: &str,
                verification_type: &str
            ) -> Ad4mDbResult<bool>;
            fn cleanup_expired_codes(&self) -> Ad4mDbResult<()>;
            fn has_verification_code(&self, email: &str) -> Ad4mDbResult<bool>;
            fn check_and_update_rate_limit(&self, email: &str) -> Ad4mDbResult<()>;
            fn set_verification_code_expiry(
                &self,
                email: &str,
                verification_type: &str,
                expires_at: i64
            ) -> Ad4mDbResult<()>;

            // Grants: one row per token the executor issued (db/grants.rs)
            fn add_grant(&self, grant: &GrantRow) -> Ad4mDbResult<()>;
            fn grants(&self) -> Ad4mDbResult<Vec<GrantRow>>;
            fn revoke_grants(&self, ids: &[String]) -> Ad4mDbResult<()>;
            fn delete_grants(&self, ids: &[String]) -> Ad4mDbResult<()>;
            fn delete_grants_expired_before(&self, cutoff: u64) -> Ad4mDbResult<()>;
            fn renew_grant(
                &self,
                id: &str,
                expires_at: u64,
                head_seq: u64,
                checked_at: u64
            ) -> Ad4mDbResult<usize>;
            fn note_key_check(&self, id: &str, head_seq: u64, checked_at: u64) -> Ad4mDbResult<usize>;
            fn note_pending_grant_writes(&self, ids: &[String], op: &str) -> Ad4mDbResult<()>;
            fn pending_grant_writes(&self) -> Ad4mDbResult<Vec<(String, String)>>;
            fn clear_pending_grant_writes(&self, ids: &[String], op: &str) -> Ad4mDbResult<()>;

            // AI: model status, tasks, models
            fn create_or_update_model_status(
                &self,
                model: &str,
                progress: f64,
                status: &str,
                downloaded: bool,
                loaded: bool
            ) -> Result<(), rusqlite::Error>;
            fn get_model_status(
                &self,
                model: &str
            ) -> Result<Option<AIModelLoadingStatus>, rusqlite::Error>;
            fn add_task(
                &self,
                name: String,
                model_id: String,
                system_prompt: String,
                prompt_examples: Vec<AIPromptExamples>,
                metadata: Option<String>,
                agent_did: Option<&str>
            ) -> Result<String, rusqlite::Error>;
            fn remove_task(&self, id: String) -> Result<(), rusqlite::Error>;
            fn get_task(&self, id: String) -> Result<Option<AITask>, rusqlite::Error>;
            fn get_tasks(&self) -> Result<Vec<AITask>, rusqlite::Error>;
            fn update_task(
                &self,
                id: String,
                name: String,
                model_id: String,
                system_prompt: String,
                prompt_examples: Vec<AIPromptExamples>,
                metadata: Option<String>
            ) -> Result<bool, rusqlite::Error>;
            fn get_task_owner(&self, id: &str) -> Result<Option<Option<String>>, rusqlite::Error>;
            fn get_tasks_owned_by(
                &self,
                agent_did: Option<&str>
            ) -> Result<Vec<AITask>, rusqlite::Error>;
            fn add_model(&self, model: &ModelInput) -> Ad4mDbResult<String>;
            fn get_model(&self, model_id: String) -> Ad4mDbResult<Option<Model>>;
            fn get_models(&self) -> Ad4mDbResult<Vec<Model>>;
            fn update_model(&self, id: &str, model: &ModelInput) -> Ad4mDbResult<()>;
            fn remove_model(&self, id: &str) -> Ad4mDbResult<()>;
            fn set_default_model(&self, model_type: ModelType, model_id: &str) -> Ad4mDbResult<()>;
            fn get_default_model(&self, model_type: ModelType) -> Ad4mDbResult<Option<String>>;

            // Notifications
            fn add_notification(
                &self,
                notification: NotificationInput,
                owner_did: &str,
                granted: bool
            ) -> Result<String, rusqlite::Error>;
            fn get_notifications(&self) -> Result<Vec<Notification>, rusqlite::Error>;
            fn get_notifications_for_owner(
                &self,
                owner_did: &str
            ) -> Result<Vec<Notification>, rusqlite::Error>;
            fn get_notification(&self, id: String) -> Result<Option<Notification>, rusqlite::Error>;
            fn migrate_notification_owners(&self, main_agent_did: &str) -> Result<(), AnyError>;
            fn remove_notification(&self, id: String) -> Result<(), rusqlite::Error>;
            fn update_notification(
                &self,
                id: String,
                updated_notification: &Notification
            ) -> Result<bool, rusqlite::Error>;

            // Social: entanglement proofs, outbox, friends, link languages, trusted agents
            fn add_entanglement_proofs(
                &self,
                proofs: Vec<EntanglementProof>
            ) -> Result<(), rusqlite::Error>;
            fn get_all_entanglement_proofs(&self) -> Result<Vec<EntanglementProof>, rusqlite::Error>;
            fn remove_entanglement_proofs(
                &self,
                proofs: Vec<EntanglementProof>
            ) -> Result<(), rusqlite::Error>;
            fn add_to_outbox(
                &self,
                message: &PerspectiveExpression,
                recipient: String
            ) -> Result<(), rusqlite::Error>;
            fn get_all_from_outbox(&self) -> Result<Vec<SentMessage>, rusqlite::Error>;
            fn add_friends(&self, friends: Vec<String>) -> Result<(), rusqlite::Error>;
            fn remove_friends(&self, friends: Vec<String>) -> Result<(), rusqlite::Error>;
            fn get_all_friends(&self) -> Result<Vec<String>, rusqlite::Error>;
            fn add_known_link_languages(&self, languages: Vec<String>) -> Result<(), rusqlite::Error>;
            fn remove_known_link_languages(
                &self,
                languages: Vec<String>
            ) -> Result<(), rusqlite::Error>;
            fn get_all_known_link_languages(&self) -> Result<Vec<String>, rusqlite::Error>;
            fn add_trusted_agents(&self, agents: Vec<String>) -> Result<(), rusqlite::Error>;
            fn get_all_trusted_agents(&self) -> Result<Vec<String>, rusqlite::Error>;
            fn remove_trusted_agents(&self, agents: Vec<String>) -> Result<(), rusqlite::Error>;

            // Perspectives and neighbourhood owners
            fn add_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()>;
            fn get_all_perspectives(&self) -> Ad4mDbResult<Vec<PerspectiveHandle>>;
            fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()>;
            fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()>;
            fn add_owner_to_neighbourhood(
                &self,
                neighbourhood_url: &str,
                user_did: &str
            ) -> Ad4mDbResult<()>;
            fn ensure_neighbourhood_member(
                &self,
                neighbourhood_url: &str,
                user_did: &str
            ) -> Ad4mDbResult<()>;
            fn get_neighbourhood_owners(&self, neighbourhood_url: &str) -> Ad4mDbResult<Vec<String>>;

            // Flow replica state
            fn remove_flow_replica_rows(&self, perspective: &str) -> Ad4mDbResult<()>;
            fn get_flow_replica_states(
                &self,
                perspective: &str,
                agent_did: &str,
                instances: Option<&[String]>
            ) -> Ad4mDbResult<Vec<FlowReplicaStateRow>>;
            fn get_flow_replica_state(
                &self,
                perspective: &str,
                agent_did: &str,
                instance: &str
            ) -> Ad4mDbResult<Option<FlowReplicaStateRow>>;
            // The seed and pass writes are `*_unless_torn_down`, outside this list.
            fn get_flow_fired_among(
                &self,
                perspective: &str,
                agent_did: &str,
                proposals: &[String]
            ) -> Ad4mDbResult<HashSet<String>>;
            fn get_flow_fired_for_instance(
                &self,
                perspective: &str,
                agent_did: &str,
                instance: &str
            ) -> Ad4mDbResult<Vec<String>>;

            // Links, link migration and pending diffs
            fn add_link(
                &self,
                perspective_uuid: &str,
                link: &LinkExpression,
                status: &LinkStatus
            ) -> Ad4mDbResult<()>;
            fn add_many_links(
                &self,
                perspective_uuid: &str,
                links: Vec<LinkExpression>,
                status: &LinkStatus
            ) -> Ad4mDbResult<()>;
            fn update_link(
                &self,
                perspective_uuid: &str,
                old_link: &LinkExpression,
                new_link: &LinkExpression
            ) -> Ad4mDbResult<()>;
            fn remove_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()>;
            fn get_link(
                &self,
                perspective_uuid: &str,
                link: &LinkExpression
            ) -> Ad4mDbResult<Option<(LinkExpression, LinkStatus)>>;
            fn get_all_links(
                &self,
                perspective_uuid: &str
            ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
            fn get_links_by_source(
                &self,
                perspective_uuid: &str,
                source: &str
            ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
            fn get_links_by_predicate(
                &self,
                perspective_uuid: &str,
                predicate: &str
            ) -> Ad4mDbResult<Vec<(LinkExpression, LinkStatus)>>;
            fn is_perspective_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<bool>;
            fn mark_perspective_as_migrated(&self, perspective_uuid: &str) -> Ad4mDbResult<()>;
            fn delete_all_links_for_perspective(&self, perspective_uuid: &str) -> Ad4mDbResult<usize>;
            fn add_pending_diff(&self, perspective_uuid: &str, diff: &PerspectiveDiff) -> Ad4mDbResult<()>;
            fn get_pending_diffs(
                &self,
                perspective_uuid: &str,
                max_count: Option<usize>
            ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>;
            fn get_pending_diffs_by_size(
                &self,
                perspective_uuid: &str,
                max_bytes: usize,
                initial_count: Option<usize>
            ) -> Ad4mDbResult<(PerspectiveDiff, Vec<u64>)>;
            fn clear_pending_diffs(&self, perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()>;
            fn park_pending_diffs(&self, perspective_uuid: &str, ids: Vec<u64>) -> Ad4mDbResult<()>;
            fn get_unsent_additions(&self, perspective_uuid: &str) -> Ad4mDbResult<Vec<LinkExpression>>;

            // Identity: the operator record, the identity each local agent speaks for, the
            // claims this executor made (`db/identity.rs`)
            fn operator_record(&self) -> Ad4mDbResult<Option<OperatorRecordRow>>;
            fn set_operator_record_if_empty(&self, scid: &str, set_at: u64, set_by: OperatorSetBy) -> Ad4mDbResult<OperatorRecordRow>;
            fn agent_identity(&self, agent_did: &str) -> Ad4mDbResult<Option<String>>;
            fn set_agent_identity(&self, agent_did: &str, scid: &str, set_at: u64) -> Ad4mDbResult<()>;
            fn set_agent_identity_of_join(&self, agent_did: &str, scid: &str, set_at: u64, offer_id: &str) -> Ad4mDbResult<()>;
            fn set_agent_identity_and_first_run_operator(&self, agent_did: &str, scid: &str, operator_did: &str, set_at: u64, offer_id: &str) -> Ad4mDbResult<OperatorRecordRow>;
            fn agents_of_identity(&self, scid: &str) -> Ad4mDbResult<Vec<String>>;
            fn local_identities(&self) -> Ad4mDbResult<Vec<String>>;
            fn add_join(&self, join: &JoinRow) -> Ad4mDbResult<JoinAdded>;
            fn join(&self, offer_id: &str) -> Ad4mDbResult<Option<JoinRow>>;
            fn set_join_accept(&self, offer_id: &str, accept: JoinAccept) -> Ad4mDbResult<bool>;
            fn cancel_join(&self, offer_id: &str, agent_did: &str) -> Ad4mDbResult<JoinCancel>;

            // Re-key of the main agent: the rows that move to its new DID (`db/rekey.rs`)
            fn move_notification_owner(&self, old: &str, new: &str) -> Ad4mDbResult<usize>;
            fn move_flow_rows(&self, old: &str, new: &str) -> Ad4mDbResult<usize>;
            fn clear_joins(&self) -> Ad4mDbResult<usize>;

            // Expression cache
            fn _add_expression(&self, url: &str, expression: &Expression<Value>) -> Ad4mDbResult<()>;
            fn _get_expression(&self, url: &str) -> Ad4mDbResult<Option<Expression<Value>>>;

            // Export and import of the whole database
            fn export_all_to_json(&self) -> Ad4mDbResult<Value>;
            fn import_from_json(
                &self,
                data: Value,
                main_agent_did: Option<&str>
            ) -> Ad4mDbResult<ImportResult>;
        }
    };
}

/// Declares the listed methods (trait items).
macro_rules! declare_methods {
    ($(fn $name:ident(&self $(, $arg:ident: $ty:ty)* $(,)?) -> $ret:ty;)*) => {
        $(fn $name(&self $(, $arg: $ty)*) -> $ret;)*
    };
}

/// Implements the listed methods on the executor's SQLite database (the global `Ad4mDb`).
macro_rules! run_on_ad4m_db {
    ($(fn $name:ident(&self $(, $arg:ident: $ty:ty)* $(,)?) -> $ret:ty;)*) => {
        $(fn $name(&self $(, $arg: $ty)*) -> $ret {
            on_ad4m_db(|db| db.$name($($arg),*))
        })*
    };
}

/// Implements the listed methods with the backend's `local` field, a [`LocalDb`].
macro_rules! run_on_local_db {
    ($(fn $name:ident(&self $(, $arg:ident: $ty:ty)* $(,)?) -> $ret:ty;)*) => {
        $(fn $name(&self $(, $arg: $ty)*) -> $ret {
            self.local.$name($($arg),*)
        })*
    };
}

/// The error a domain method returns when the global `Ad4mDb` is not open, for each error
/// type that `Ad4mDb` methods return.
trait DbNotOpen {
    fn db_not_open(reason: &str) -> Self;
}

impl DbNotOpen for AnyError {
    fn db_not_open(reason: &str) -> Self {
        anyhow!("the database is not open: {}", reason)
    }
}

impl DbNotOpen for rusqlite::Error {
    fn db_not_open(reason: &str) -> Self {
        rusqlite::Error::SqliteFailure(
            rusqlite::ffi::Error::new(rusqlite::ffi::SQLITE_CANTOPEN),
            Some(format!("the database is not open: {}", reason)),
        )
    }
}

/// Runs `f` on the global `Ad4mDb` under its lock. An error, not a panic, when the database
/// is not open: some executor code runs before the database opens (unlock, Holochain start)
/// and handles the error. A lock poisoned by a panic elsewhere still serves the database,
/// as `agent/grants.rs` does: an error there would read as "no data" at call sites that
/// fall back on one.
fn on_ad4m_db<T, E: DbNotOpen>(f: impl FnOnce(&Ad4mDb) -> Result<T, E>) -> Result<T, E> {
    let instance = Ad4mDb::global_instance();
    let guard = instance
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    match guard.as_ref() {
        Some(db) => f(db),
        None => Err(E::db_not_open("not initialised")),
    }
}

// ── Trait ──────────────────────────────────────────────────────────────────────

/// The executor's database. See the module docs for the two parts and the two backends.
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

    ad4m_db_methods!(declare_methods);

    /// `Ad4mDb::seed_flow_replica_state`, unless `torn_down` is set: then `Ok(false)` and
    /// nothing is written. The flag is read under the database lock that the write takes.
    /// `perspectives::remove_perspective` sets the flag before it deletes the perspective's
    /// rows, so a seed either lands before the delete or writes nothing.
    fn seed_flow_replica_state_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<bool>;

    /// `Ad4mDb::record_flow_replica_pass`, unless `torn_down` is set: then `Ok(None)` and
    /// nothing is written. The flag is read under the database lock that the write takes, as
    /// in [`DbBackend::seed_flow_replica_state_unless_torn_down`].
    #[allow(clippy::too_many_arguments)]
    fn record_flow_replica_pass_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        state: Option<&str>,
        fired: &[String],
        at: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<Option<FlowPassRow>>;
}

/// What `Ad4mDb::record_flow_replica_pass` returns: the row before the pass, and the fired
/// marks the pass was the first to write.
pub type FlowPassRow = (Option<FlowReplicaStateRow>, Vec<String>);

// ── Global accessor ────────────────────────────────────────────────────────────

static DB_BACKEND: OnceCell<Arc<dyn DbBackend>> = OnceCell::const_new();

/// The executor's database backend. [`LocalDb`] until [`init_db_backend`] sets one: an
/// executor without a configured backend (lib tests, the startup code before the backend is
/// set) runs on its SQLite database.
pub fn db_backend() -> &'static dyn DbBackend {
    match DB_BACKEND.get() {
        Some(backend) => backend.as_ref(),
        None => &LocalDb,
    }
}

/// Initialise the global database backend. Returns false if already set.
pub fn init_db_backend(backend: Arc<dyn DbBackend>) -> bool {
    DB_BACKEND.set(backend).is_ok()
}

// ── LocalDb ────────────────────────────────────────────────────────────────────

/// The executor's SQLite database: the global `Ad4mDb`. Every domain method runs the
/// `Ad4mDb` method of the same name.
pub struct LocalDb;

impl LocalDb {
    pub fn new() -> Self {
        LocalDb
    }
}

impl Default for LocalDb {
    fn default() -> Self {
        LocalDb
    }
}

impl DbBackend for LocalDb {
    ad4m_db_methods!(run_on_ad4m_db);

    fn seed_flow_replica_state_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<bool> {
        on_ad4m_db(|db| {
            if torn_down.load(Ordering::Acquire) {
                return Ok(false);
            }
            db.seed_flow_replica_state(perspective, agent_did, instance)
        })
    }

    fn record_flow_replica_pass_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        state: Option<&str>,
        fired: &[String],
        at: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<Option<FlowPassRow>> {
        on_ad4m_db(|db| {
            if torn_down.load(Ordering::Acquire) {
                return Ok(None);
            }
            db.record_flow_replica_pass(perspective, agent_did, instance, state, fired, at)
                .map(Some)
        })
    }

    fn get(&self, _did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        on_ad4m_db(|db| {
            // Route to the appropriate Ad4mDb method based on table name
            match table {
                "users" => match db.get_user(row_id) {
                    Ok(u) => Ok(Some(serde_json::to_value(u)?)),
                    Err(_) => Ok(None), // QueryReturnedNoRows → not found
                },
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
        on_ad4m_db(|db| match table {
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
                    .filter_map(|n| serde_json::to_value(n.view()).ok())
                    .collect())
            }
            _ => Err(anyhow!("LocalDb: unknown table '{}'", table)),
        })
    }

    fn upsert(&self, _did: &str, table: &str, row_id: &str, data: Value) -> Result<(), AnyError> {
        on_ad4m_db(|db| match table {
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
        // LocalDb does not expose per-row deletion through Ad4mDb. Log and
        // return Ok — callers treat delete as best-effort. The row stays in
        // SQLite until the next full sync or manual cleanup.
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
}

// ── SharedDb ───────────────────────────────────────────────────────────────────

/// The backend of a shared-mode executor. The key-value methods call the platform Worker's
/// `/internal/db/` API (`reqwest::blocking`, for consistency with SharedWallet). Every domain
/// method runs on the executor's SQLite database through `local`: the Worker holds only the
/// records that executors share (users and perspective metadata, written and read through
/// the key-value methods).
pub struct SharedDb {
    base_url: String,
    token: String,
    client: reqwest::blocking::Client,
    cache: RwLock<HashMap<String, CachedRow>>,
    local: LocalDb,
}

const SHARED_DB_CACHE_TTL_SECS: u64 = 30;

/// Cached single-row read from the platform DB.
///
/// **Deployment invariant (do not break silently):** this cache is
/// executor-local, invalidated only by writes going through *this*
/// executor's `upsert`/`delete`. Correctness therefore depends on one
/// executor instance per user session — a second executor writing to the
/// same row would not invalidate this executor's cache, and reads inside
/// the 30 s TTL window would return the pre-write value.
///
/// If the deployment ever grows to multiple executors per user (read
/// replicas, HA, load balancing), the cache invalidation strategy has to
/// change — either shrink `SHARED_DB_CACHE_TTL_SECS` to zero (disable),
/// or add a shared invalidation channel (Worker-side write notifications).
/// Do not simply raise the TTL; stale-read bugs of the form "data doesn't
/// update until the user reconnects" are the failure mode.
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
            local: LocalDb,
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
    ad4m_db_methods!(run_on_local_db);

    fn seed_flow_replica_state_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<bool> {
        self.local.seed_flow_replica_state_unless_torn_down(
            perspective,
            agent_did,
            instance,
            torn_down,
        )
    }

    fn record_flow_replica_pass_unless_torn_down(
        &self,
        perspective: &str,
        agent_did: &str,
        instance: &str,
        state: Option<&str>,
        fired: &[String],
        at: &str,
        torn_down: &AtomicBool,
    ) -> Ad4mDbResult<Option<FlowPassRow>> {
        self.local.record_flow_replica_pass_unless_torn_down(
            perspective,
            agent_did,
            instance,
            state,
            fired,
            at,
            torn_down,
        )
    }

    fn get(&self, did: &str, table: &str, row_id: &str) -> Result<Option<Value>, AnyError> {
        let key = Self::cache_key(did, table, row_id);

        // Check cache — stale entries (> 30s) fall through to a network fetch.
        // The cache only covers single-row gets; list() always hits the network
        // to avoid returning an incomplete/outdated view of the full table.
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
        /// Maximum rows to accept from a single list() call.
        /// Guards against unbounded memory growth if the Worker returns a
        /// very large table. Increase if a legitimate table exceeds this.
        ///
        /// **Silent truncation:** when the Worker returns more rows than
        /// this, the excess is dropped and only a `log::warn!` records it.
        /// The trait method returns a plain `Vec<Value>` with no
        /// `truncated: bool` — callers that need to know whether the
        /// result is complete cannot tell from the return value alone.
        /// Grep the executor log for the warning below, or (better) get
        /// the Worker to add cursor pagination so a full read is possible.
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
            // Keep at warn: silent truncation of a user-scoped table read
            // is the shape of bug that shows up months later as "my
            // notification list is missing entries."
            log::warn!(
                "SharedDb::list: table '{}/{}' returned {} rows, truncating to {} (increase MAX_ROWS or add cursor pagination on the Worker)",
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::GrantKeyRow;

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

        // First GET populates cache
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

        // Upsert invalidates cache
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

        // Second GET must hit network (cache invalidated)
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

    /// A fake platform Worker: the rows of the shared database in memory, served on the
    /// routes that `SharedDb` calls for the `shared:platform` namespace. Records each request
    /// it serves. A request to any other route gets mockito's 501, which fails the `SharedDb`
    /// call.
    struct FakeSharedStore {
        server: mockito::ServerGuard,
        requests: Arc<std::sync::Mutex<Vec<String>>>,
        /// The routes live as long as the store: mockito removes a dropped mock.
        _routes: Vec<mockito::Mock>,
    }

    /// The rows of the fake store: route of the table (`/shared:platform/users`) → row id →
    /// the row's data, as the JSON string `SharedDb` sends.
    type FakeRows = Arc<std::sync::Mutex<HashMap<String, HashMap<String, String>>>>;

    impl FakeSharedStore {
        fn start() -> Self {
            use mockito::Matcher;
            let mut server = mockito::Server::new();
            let rows: FakeRows = Default::default();
            let requests: Arc<std::sync::Mutex<Vec<String>>> = Default::default();
            let table_route = Matcher::Regex(r"^/shared:platform/[a-z_]+$".to_string());
            let row_route = Matcher::Regex(r"^/shared:platform/[a-z_]+/[^/]+$".to_string());

            let upsert = {
                let (rows, requests) = (rows.clone(), requests.clone());
                server
                    .mock("POST", table_route.clone())
                    .with_status(200)
                    .with_body_from_request(move |req| {
                        requests
                            .lock()
                            .unwrap()
                            .push(format!("POST {}", req.path()));
                        let body: Value = serde_json::from_slice(req.body().unwrap()).unwrap();
                        rows.lock()
                            .unwrap()
                            .entry(req.path().to_string())
                            .or_default()
                            .insert(
                                body["rowId"].as_str().unwrap().to_string(),
                                body["data"].as_str().unwrap().to_string(),
                            );
                        b"{}".to_vec()
                    })
                    .create()
            };
            let get_row = {
                let (rows, requests) = (rows.clone(), requests.clone());
                server
                    .mock("GET", row_route)
                    .with_status(200)
                    .with_body_from_request(move |req| {
                        requests.lock().unwrap().push(format!("GET {}", req.path()));
                        let (table, row_id) = req.path().rsplit_once('/').unwrap();
                        let data = rows
                            .lock()
                            .unwrap()
                            .get(table)
                            .and_then(|table| table.get(row_id).cloned())
                            .unwrap_or_else(|| "null".to_string());
                        serde_json::json!({ "data": data }).to_string().into_bytes()
                    })
                    .create()
            };
            let list = {
                let (rows, requests) = (rows.clone(), requests.clone());
                server
                    .mock("GET", table_route)
                    .with_status(200)
                    .with_body_from_request(move |req| {
                        requests.lock().unwrap().push(format!("GET {}", req.path()));
                        let listed: Vec<Value> = rows
                            .lock()
                            .unwrap()
                            .get(req.path())
                            .map(|table| {
                                table
                                    .values()
                                    .map(|data| serde_json::json!({ "data": data }))
                                    .collect()
                            })
                            .unwrap_or_default();
                        serde_json::json!({ "rows": listed })
                            .to_string()
                            .into_bytes()
                    })
                    .create()
            };
            FakeSharedStore {
                server,
                requests,
                _routes: vec![upsert, get_row, list],
            }
        }

        fn backend(&self) -> SharedDb {
            SharedDb::new(self.server.url(), "db-tok".to_string())
        }

        fn requests(&self) -> Vec<String> {
            self.requests.lock().unwrap().clone()
        }
    }

    /// Opens an in-memory `Ad4mDb` unless a test opened one.
    fn ensure_local_db() {
        if Ad4mDb::global_instance().lock().unwrap().is_none() {
            Ad4mDb::init_global_instance(":memory:").unwrap();
        }
    }

    // A database that is not open is an error, not a panic (dev panicked).
    #[test]
    fn a_closed_database_is_an_error() {
        let _closed = crate::test_utils::DbClosed::now();
        let err = LocalDb.get_free_hosting_enabled().unwrap_err();
        assert!(
            err.to_string().contains("the database is not open"),
            "{err}"
        );
    }

    // A lock that a panic poisoned still serves the database. An error would reach call sites
    // that read it as free hosting or single-user mode (dev panicked, so they refused).
    #[test]
    fn a_poisoned_lock_still_serves_the_database() {
        ensure_local_db();
        let _ = std::thread::spawn(|| {
            let instance = Ad4mDb::global_instance();
            let _guard = instance
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            panic!("poisoning the database lock");
        })
        .join();
        assert!(Ad4mDb::global_instance().is_poisoned());
        let read = LocalDb.get_free_hosting_enabled();
        Ad4mDb::global_instance().clear_poison();
        assert!(read.is_ok(), "{read:?}");
    }

    // A shared-mode executor keeps its own SQLite database: every domain method (users, grants,
    // tasks, perspectives) runs there and sends nothing to the shared database.
    #[test]
    fn a_shared_executor_serves_domain_methods_from_its_sqlite_database() {
        ensure_local_db();
        let store = FakeSharedStore::start();
        let db = store.backend();
        let tag = uuid::Uuid::new_v4().simple().to_string();
        let email = format!("local-{tag}@example.org");
        let did = format!("did:key:z6MkLocal{tag}");

        // Users
        db.add_user_prehashed(&email, &did, "hash")
            .expect("add_user_prehashed runs on SQLite");
        let user = db.get_user(&email).expect("get_user runs on SQLite");
        assert_eq!(user.did, did);
        assert_eq!(
            db.get_user_by_did(&did).unwrap().map(|u| u.username),
            Some(email.clone())
        );

        // Grants: a revoke ends the sessions of the grant's token.
        let grant = GrantRow {
            id: format!("grant-{tag}"),
            request_id: format!("request-{tag}"),
            agent_did: Some(did.clone()),
            auth_info: "{}".to_string(),
            revoked: false,
            expires_at: u64::MAX / 4,
            created_at: 1,
            ..GrantRow::default()
        };
        db.add_grant(&grant).expect("add_grant runs on SQLite");
        assert!(db.grants().unwrap().contains(&grant));
        db.revoke_grants(std::slice::from_ref(&grant.id))
            .expect("revoke_grants runs on SQLite");
        let stored = db
            .grants()
            .unwrap()
            .into_iter()
            .find(|row| row.id == grant.id)
            .expect("the grant stays, revoked");
        assert!(stored.revoked);

        // A key-bound grant: a renewal moves its expiry and the verdict it was read at.
        let key_grant = GrantRow {
            id: format!("key-grant-{tag}"),
            request_id: format!("key-request-{tag}"),
            agent_did: Some(did.clone()),
            auth_info: "{}".to_string(),
            expires_at: 100,
            created_at: 1,
            key: Some(GrantKeyRow {
                scid: format!("did:scid:coasys:1:{tag}"),
                signing_key: format!("z6MkKey{tag}"),
                key_id: "d2".to_string(),
                head_seq: 1,
                checked_at: 10,
            }),
            ..GrantRow::default()
        };
        db.add_grant(&key_grant).expect("add_grant runs on SQLite");
        db.renew_grant(&key_grant.id, 5_000, 3, 4_000)
            .expect("renew_grant runs on SQLite");
        let renewed = db
            .grants()
            .unwrap()
            .into_iter()
            .find(|row| row.id == key_grant.id)
            .expect("the key grant stays");
        assert_eq!(renewed.expires_at, 5_000);
        let key = renewed.key.expect("the key binding stays");
        assert_eq!((key.head_seq, key.checked_at), (3, 4_000));

        // Tasks, owned by the user's agent
        let task_id = db
            .add_task(
                format!("task-{tag}"),
                "model".to_string(),
                "prompt".to_string(),
                vec![],
                None,
                Some(&did),
            )
            .expect("add_task runs on SQLite");
        assert_eq!(
            db.get_task_owner(&task_id).unwrap(),
            Some(Some(did.clone()))
        );
        let owned = db.get_tasks_owned_by(Some(&did)).unwrap();
        assert_eq!(
            owned.iter().map(|t| t.task_id.clone()).collect::<Vec<_>>(),
            vec![task_id.clone()]
        );
        assert!(db.get_task(task_id).unwrap().is_some());

        // Perspectives
        let handle = PerspectiveHandle::new_from_name(format!("perspective-{tag}"));
        db.add_perspective(&handle)
            .expect("add_perspective runs on SQLite");
        assert!(db
            .get_all_perspectives()
            .unwrap()
            .iter()
            .any(|p| p.uuid == handle.uuid));

        // The local backend reads the same rows: one SQLite database.
        assert_eq!(LocalDb.get_user(&email).unwrap().did, did);

        assert_eq!(store.requests(), Vec::<String>::new());
    }

    // Users and perspective metadata stay on the shared database: written and read through the
    // key-value methods, as `user_management` and `perspectives` do. A user from another
    // executor reaches this executor's SQLite only when it is adopted on login.
    #[test]
    fn a_shared_executor_keeps_users_and_perspective_metadata_on_the_shared_database() {
        ensure_local_db();
        let store = FakeSharedStore::start();
        let db = store.backend();
        let tag = uuid::Uuid::new_v4().simple().to_string();
        let email = format!("remote-{tag}@example.org");
        let did = format!("did:key:z6MkRemote{tag}");
        let record = serde_json::json!({
            "username": &email,
            "did": &did,
            "password_hash": "hash",
        });

        // Another executor created the user.
        db.upsert("shared:platform", "users", &email, record.clone())
            .unwrap();
        assert_eq!(
            db.get("shared:platform", "users", &email).unwrap(),
            Some(record)
        );
        assert!(
            db.get_user(&email).is_err(),
            "the shared record is not in this executor's SQLite before login"
        );

        // Login adopts it into SQLite with `add_user_prehashed` (`adopt_shared_user`); the
        // login path itself (`verify_credentials`) is not run here.
        db.add_user_prehashed(&email, &did, "hash").unwrap();
        assert_eq!(db.get_user(&email).unwrap().did, did);

        // Perspective metadata
        let uuid = format!("perspective-{tag}");
        let meta = serde_json::json!({ "uuid": &uuid, "creator_did": &did });
        db.upsert("shared:platform", "perspectives", &uuid, meta.clone())
            .unwrap();
        assert_eq!(
            db.list("shared:platform", "perspectives").unwrap(),
            vec![meta]
        );

        assert_eq!(
            store.requests(),
            vec![
                "POST /shared:platform/users".to_string(),
                format!("GET /shared:platform/users/{email}"),
                "POST /shared:platform/perspectives".to_string(),
                "GET /shared:platform/perspectives".to_string(),
            ]
        );
    }
}
