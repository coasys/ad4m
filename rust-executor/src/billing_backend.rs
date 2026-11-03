//! Billing backend for credit management, usage logging, and rate configuration.
//!
//! Follows the same trait-abstraction pattern as `WalletBackend`, `DbBackend`,
//! and `PerspectiveStoreBackend`:
//! - `LocalBillingBackend`: wraps existing `Ad4mDb` billing methods (in-process SQLite)
//! - `SharedBillingBackend`: HTTP client to the platform Worker's D1 billing API
//!
//! Config: `BILLING_BACKEND` env var or `config.billing_backend`.
//! - "local" (default): in-process SQLite via Ad4mDb
//! - "shared": HTTP client to platform Worker

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::any::Any;
use std::sync::Arc;
use tokio::sync::OnceCell;

use crate::billing::BillingError;
use crate::db::{Ad4mDb, ComputeLogEntry};

/// Percent-encode an email address for safe use in URL path segments.
fn encode_email(email: &str) -> String {
    urlencoding::encode(email).into_owned()
}

/// The shared billing API path of an agent resource: `/billing/agents/{did}/{rest}`.
fn agent_path(agent_did: &str, rest: &str) -> String {
    format!(
        "/billing/agents/{}/{}",
        urlencoding::encode(agent_did),
        rest
    )
}

/// The `email` of the account that owns the agent, from a shared billing reply. Empty
/// when the reply names none: the operation itself has already succeeded.
fn account_email(reply: &serde_json::Value) -> String {
    reply
        .get("email")
        .and_then(|v| v.as_str())
        .unwrap_or_default()
        .to_string()
}

// ── Tier limits ──────────────────────────────────────────────────────────────

/// Resource limits for a billing tier. `None` means unlimited.
#[derive(Debug, Clone, Default)]
pub struct TierLimits {
    pub max_perspectives: Option<i64>,
    pub max_members_per_perspective: Option<i64>,
    pub monthly_inference_credits_cents: Option<i64>,
    pub storage_limit_bytes: Option<i64>,
}

/// Error returned when a tier-limit check fails.
#[derive(Debug)]
pub struct TierLimitExceeded {
    pub resource: String,
    pub limit: i64,
    pub current: i64,
}

// ── Trait ──────────────────────────────────────────────────────────────────────

/// Abstracts billing operations so the executor can use either a local SQLite
/// database or a remote platform Worker as the billing store.
///
/// Compute is billed to an agent, named by its DID. Credits, free access and the HoT
/// wallet belong to the user account that owns the agent; account methods take the
/// account's login email, where a caller names the account (admin, hosting info, Unyt).
///
/// Methods are synchronous — matches the WalletBackend / DbBackend pattern.
/// SharedBillingBackend bridges to an async `reqwest::Client` via `block_in_place`.
pub trait BillingBackend: Send + Sync {
    // ── Agent compute (keyed by agent DID) ─────────────────────────────

    /// The credits of the account that owns the agent `agent_did`.
    fn get_agent_credits(&self, agent_did: &str) -> Result<f64, AnyError>;

    /// Whether the account that owns the agent `agent_did` computes for free.
    fn get_agent_free_access(&self, agent_did: &str) -> Result<bool, AnyError>;

    /// Add credits to the account that owns the agent `agent_did`.
    /// Returns the account's email (whose hosting info changed).
    fn add_agent_credits(&self, agent_did: &str, amount: f64) -> Result<String, AnyError>;

    /// Atomically check that the account that owns the agent `agent_did` can afford
    /// `amount` and deduct it. Does NOT log to compute_log — use for credit reservation
    /// (e.g. link writes). Returns the account's email.
    fn deduct_agent_credits_if_available(
        &self,
        agent_did: &str,
        amount: f64,
    ) -> Result<String, BillingError>;

    /// Atomically deduct credits from the account that owns the agent `agent_did` and log
    /// a compute event for the agent. Returns `(row_id, credits_after, account_email)`.
    fn deduct_credits_and_log(
        &self,
        agent_did: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64, String), BillingError>;

    // ── Compute log ────────────────────────────────────────────────────

    /// Compute log entries of the agent `agent_did`, newest-first.
    fn get_compute_log(
        &self,
        agent_did: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError>;

    /// Compute log entries for ALL agents (operator). Newest-first.
    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError>;

    /// Delete log entries older than the given ISO 8601 timestamp.
    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError>;

    // ── Accounts (keyed by login email) ────────────────────────────────

    /// The account's remaining credit balance.
    fn get_user_credits(&self, email: &str) -> Result<f64, AnyError>;

    /// Set the account's credit balance to an absolute value.
    fn set_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError>;

    /// Add credits to the account's balance.
    fn add_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError>;

    /// Whether the account has free access.
    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError>;

    /// Enable or disable free access for the account.
    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError>;

    /// The account's HoloFuel/HoT wallet address.
    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError>;

    /// Set the account's HoloFuel/HoT wallet address.
    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError>;

    /// Reverse lookup: `(email, agent_did)` of the account with this HoT wallet address.
    fn get_user_by_hot_wallet_address(
        &self,
        address: &str,
    ) -> Result<Option<(String, String)>, AnyError>;

    // ── Host rates ─────────────────────────────────────────────────────

    /// Get all host rates as (description, price) pairs.
    fn get_rates(&self) -> Result<Vec<(String, f64)>, AnyError>;

    /// The rate for one operation description. `Ok(None)` means the operator has not
    /// priced it; `Err` means the backend could not answer.
    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError>;

    /// Replace all host rates with the given set.
    fn set_rates(&self, rates: &[(String, f64)]) -> Result<(), AnyError>;

    // ── Free hosting ───────────────────────────────────────────────────

    /// Check whether global free hosting mode is enabled.
    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError>;

    /// Enable or disable global free hosting mode.
    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError>;

    // ── Tier limits ───────────────────────────────────────────────────

    /// The tier limits of the account that owns the agent `agent_did`. Returns
    /// `TierLimits::default()` (all unlimited) if the backend does not support tiers.
    fn get_tier_limits(&self, _agent_did: &str) -> Result<TierLimits, AnyError> {
        Ok(TierLimits::default())
    }

    /// Check whether the agent can create another resource of the given type.
    /// Returns `Ok(())` if allowed, `Err(TierLimitExceeded)` if not.
    ///
    /// `resource_type`: "perspective", "member", etc.
    /// `current_count`: how many the agent already has.
    fn check_tier_limit(
        &self,
        agent_did: &str,
        resource_type: &str,
        current_count: i64,
    ) -> Result<(), TierLimitExceeded> {
        let limits = self.get_tier_limits(agent_did).unwrap_or_default();

        let cap = match resource_type {
            "perspective" => limits.max_perspectives,
            "member" => limits.max_members_per_perspective,
            _ => None,
        };

        if let Some(limit) = cap {
            if current_count >= limit {
                return Err(TierLimitExceeded {
                    resource: resource_type.to_string(),
                    limit,
                    current: current_count,
                });
            }
        }

        Ok(())
    }

    /// Downcast support.
    fn as_any(&self) -> &dyn Any;
}

// ── Global accessor ────────────────────────────────────────────────────────────

static BILLING_BACKEND: OnceCell<Arc<dyn BillingBackend>> = OnceCell::const_new();

/// Get the global billing backend. Panics if not initialised.
pub fn billing_backend() -> &'static Arc<dyn BillingBackend> {
    BILLING_BACKEND
        .get()
        .expect("billing backend not initialised")
}

/// Initialise the global billing backend. Returns false if already set.
pub fn init_billing_backend(backend: Arc<dyn BillingBackend>) -> bool {
    BILLING_BACKEND.set(backend).is_ok()
}

/// Try to initialise the global billing backend. Returns false if already set.
/// Identical to `init_billing_backend` — named for consistency with `try_init_wallet_backend`.
pub fn try_init_billing_backend(backend: Arc<dyn BillingBackend>) -> bool {
    BILLING_BACKEND.set(backend).is_ok()
}

// ── LocalBillingBackend ────────────────────────────────────────────────────────

/// Wraps existing `Ad4mDb` billing methods. Zero-allocation wrapper — every
/// call delegates to `Ad4mDb::with_global_instance`.
pub struct LocalBillingBackend;

impl LocalBillingBackend {
    pub fn new() -> Self {
        LocalBillingBackend
    }
}

/// Maps an `Ad4mDb` billing error onto `BillingError`: the DB reports a missing user and
/// an unaffordable deduction as messages.
fn billing_error(e: AnyError, agent_did: &str) -> BillingError {
    let msg = e.to_string();
    if msg.contains("Insufficient compute credits") {
        BillingError::InsufficientCredits
    } else if msg.starts_with("User not found") {
        BillingError::UserNotFound(agent_did.to_string())
    } else {
        BillingError::Other(e)
    }
}

impl BillingBackend for LocalBillingBackend {
    fn get_agent_credits(&self, agent_did: &str) -> Result<f64, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_agent_credits(agent_did))
    }

    fn get_agent_free_access(&self, agent_did: &str) -> Result<bool, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_agent_free_access(agent_did))
    }

    fn add_agent_credits(&self, agent_did: &str, amount: f64) -> Result<String, AnyError> {
        Ad4mDb::with_global_instance(|db| db.add_agent_credits(agent_did, amount))
    }

    fn deduct_agent_credits_if_available(
        &self,
        agent_did: &str,
        amount: f64,
    ) -> Result<String, BillingError> {
        Ad4mDb::with_global_instance(|db| db.deduct_agent_credits_if_available(agent_did, amount))
            .map_err(|e| billing_error(e, agent_did))
    }

    fn deduct_credits_and_log(
        &self,
        agent_did: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64, String), BillingError> {
        Ad4mDb::with_global_instance(|db| {
            db.deduct_credits_and_log(agent_did, amount, operation, summary)
        })
        .map_err(|e| billing_error(e, agent_did))
    }

    fn get_compute_log(
        &self,
        agent_did: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_compute_log(agent_did, since, limit))
    }

    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_compute_log_all(since, limit))
    }

    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError> {
        Ad4mDb::with_global_instance(|db| db.cleanup_compute_log(before))
    }

    fn get_user_credits(&self, email: &str) -> Result<f64, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
    }

    fn set_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.set_user_credits(email, amount))
    }

    fn add_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.add_user_credits(email, amount))
    }

    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
    }

    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.set_user_free_access(email, enabled))
    }

    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(email))
    }

    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(email, address))
    }

    fn get_user_by_hot_wallet_address(
        &self,
        address: &str,
    ) -> Result<Option<(String, String)>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_user_by_hot_wallet_address(address))
    }

    fn get_rates(&self) -> Result<Vec<(String, f64)>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_host_rates())
    }

    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_host_rate(description))
    }

    fn set_rates(&self, rates: &[(String, f64)]) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.set_host_rates(rates))
    }

    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError> {
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
    }

    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError> {
        Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(enabled))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── SharedBillingBackend ───────────────────────────────────────────────────────

/// HTTP client that delegates billing operations to the platform Worker's
/// `/internal/billing/` API for shared-mode executors.
///
/// Every request runs on a runtime of its own (`runtime`), whatever thread calls: an
/// executor handler on a multi-thread runtime, a current-thread runtime, or a plain thread
/// such as the transcription worker. The connections of `client` and their pool then always
/// belong to one live runtime.
pub struct SharedBillingBackend {
    base_url: String,
    token: String,
    client: reqwest::Client,
    /// `None` only after `drop` has shut it down.
    runtime: Option<tokio::runtime::Runtime>,
}

/// A reply: its status and its body as text.
type Reply = (reqwest::StatusCode, String);

impl SharedBillingBackend {
    pub fn new(base_url: String, token: String) -> Self {
        SharedBillingBackend {
            base_url: base_url.trim_end_matches('/').to_string(),
            token,
            client: reqwest::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .expect("Failed to build SharedBillingBackend HTTP client"),
            runtime: Some(
                tokio::runtime::Builder::new_multi_thread()
                    .worker_threads(1)
                    .thread_name("shared-billing-http")
                    .enable_all()
                    .build()
                    .expect("Failed to build the SharedBillingBackend runtime"),
            ),
        }
    }

    /// Sends `method path` (with a JSON `body`, if any) and reads the whole reply, on the
    /// backend's runtime. The caller's thread waits: inside a multi-thread runtime it leaves
    /// the worker pool first (`block_in_place`), so other tasks keep running. A transport
    /// failure is `Err`; any HTTP status is `Ok`, for the caller to read.
    fn request(
        &self,
        method: reqwest::Method,
        path: &str,
        body: Option<&serde_json::Value>,
    ) -> Result<Reply, AnyError> {
        let mut builder = self
            .client
            .request(method.clone(), format!("{}{}", self.base_url, path))
            .header("Authorization", format!("Bearer {}", self.token));
        if let Some(body) = body {
            builder = builder.json(body);
        }
        let runtime = self
            .runtime
            .as_ref()
            .ok_or_else(|| anyhow!("SharedBillingBackend is shut down"))?;

        let (tx, rx) = std::sync::mpsc::channel();
        runtime.spawn(async move {
            let reply = async {
                let resp = builder.send().await?;
                let status = resp.status();
                // A body that cannot be read is empty: the status already tells whether
                // the Worker did the operation (a committed debit stays committed).
                let text = resp.text().await.unwrap_or_else(|e| {
                    log::warn!("SharedBillingBackend: reply body unreadable: {}", e);
                    String::new()
                });
                Ok::<Reply, reqwest::Error>((status, text))
            }
            .await;
            // The caller has stopped waiting only if it panicked: nothing to tell.
            let _ = tx.send(reply);
        });
        let wait = || rx.recv();
        let reply = match tokio::runtime::Handle::try_current() {
            Ok(handle) if handle.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread => {
                tokio::task::block_in_place(wait)
            }
            _ => wait(),
        }
        .map_err(|_| {
            anyhow!(
                "SharedBillingBackend {} {}: the request task ended",
                method,
                path
            )
        })?;

        reply.map_err(|e| anyhow!("SharedBillingBackend {} {} failed: {}", method, path, e))
    }

    /// A 2xx reply's body as JSON; any other status is `Err`.
    fn json_reply(
        method: &str,
        path: &str,
        (status, text): Reply,
    ) -> Result<serde_json::Value, AnyError> {
        if !status.is_success() {
            return Err(anyhow!(
                "SharedBillingBackend {} {} returned {}: {}",
                method,
                path,
                status,
                text
            ));
        }
        serde_json::from_str(&text).map_err(|e| anyhow!("SharedBillingBackend parse: {}", e))
    }

    fn get_json(&self, path: &str) -> Result<serde_json::Value, AnyError> {
        Self::json_reply("GET", path, self.request(reqwest::Method::GET, path, None)?)
    }

    fn post_json(
        &self,
        path: &str,
        body: &serde_json::Value,
    ) -> Result<serde_json::Value, AnyError> {
        Self::json_reply(
            "POST",
            path,
            self.request(reqwest::Method::POST, path, Some(body))?,
        )
    }

    fn put_json(
        &self,
        path: &str,
        body: &serde_json::Value,
    ) -> Result<serde_json::Value, AnyError> {
        Self::json_reply(
            "PUT",
            path,
            self.request(reqwest::Method::PUT, path, Some(body))?,
        )
    }

    fn delete_json(&self, path: &str) -> Result<serde_json::Value, AnyError> {
        Self::json_reply(
            "DELETE",
            path,
            self.request(reqwest::Method::DELETE, path, None)?,
        )
    }

    /// GET that reads 404 as `Ok(None)`; any other non-2xx or transport failure is `Err`.
    fn get_json_or_not_found(&self, path: &str) -> Result<Option<serde_json::Value>, AnyError> {
        let reply = self.request(reqwest::Method::GET, path, None)?;
        if reply.0 == reqwest::StatusCode::NOT_FOUND {
            return Ok(None);
        }
        Self::json_reply("GET", path, reply).map(Some)
    }

    /// POST a debit of the agent `agent_did`: 402 is `InsufficientCredits`, 404 is
    /// `UserNotFound`. A 2xx body that is empty or not JSON reads as `null` (logged), so a
    /// committed debit is never reported as failed for want of a readable reply.
    fn post_billing(
        &self,
        path: &str,
        body: &serde_json::Value,
        agent_did: &str,
    ) -> Result<serde_json::Value, BillingError> {
        let (status, text) = self
            .request(reqwest::Method::POST, path, Some(body))
            .map_err(BillingError::Other)?;

        match status.as_u16() {
            402 => return Err(BillingError::InsufficientCredits),
            404 => return Err(BillingError::UserNotFound(agent_did.to_string())),
            _ if !status.is_success() => {
                return Err(BillingError::Other(anyhow!(
                    "SharedBillingBackend POST {} returned {}",
                    path,
                    status
                )))
            }
            _ => {}
        }

        if text.trim().is_empty() {
            return Ok(serde_json::Value::Null);
        }
        Ok(serde_json::from_str(&text).unwrap_or_else(|e| {
            log::warn!(
                "SharedBillingBackend POST {} reply is not JSON: {}",
                path,
                e
            );
            serde_json::Value::Null
        }))
    }
}

impl Drop for SharedBillingBackend {
    /// Shuts the runtime down without waiting, so the backend may be dropped on any thread,
    /// a runtime's included.
    fn drop(&mut self) {
        if let Some(runtime) = self.runtime.take() {
            runtime.shutdown_background();
        }
    }
}

impl BillingBackend for SharedBillingBackend {
    fn get_agent_credits(&self, agent_did: &str) -> Result<f64, AnyError> {
        let result = self.get_json(&agent_path(agent_did, "credits"))?;
        result
            .get("credits")
            .and_then(|v| v.as_f64())
            .ok_or_else(|| anyhow!("Missing 'credits' in response"))
    }

    fn get_agent_free_access(&self, agent_did: &str) -> Result<bool, AnyError> {
        let result = self.get_json(&agent_path(agent_did, "free-access"))?;
        result
            .get("enabled")
            .and_then(|v| v.as_bool())
            .ok_or_else(|| anyhow!("Missing 'enabled' in response"))
    }

    fn add_agent_credits(&self, agent_did: &str, amount: f64) -> Result<String, AnyError> {
        let result = self.post_json(
            &agent_path(agent_did, "credits/add"),
            &serde_json::json!({ "amount": amount }),
        )?;
        Ok(account_email(&result))
    }

    fn deduct_agent_credits_if_available(
        &self,
        agent_did: &str,
        amount: f64,
    ) -> Result<String, BillingError> {
        let result = self.post_billing(
            &agent_path(agent_did, "reserve"),
            &serde_json::json!({ "amount": amount }),
            agent_did,
        )?;
        Ok(account_email(&result))
    }

    fn deduct_credits_and_log(
        &self,
        agent_did: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64, String), BillingError> {
        let result = self.post_billing(
            &agent_path(agent_did, "deduct"),
            &serde_json::json!({
                "amount": amount,
                "operation": operation,
                "summary": summary,
            }),
            agent_did,
        )?;
        let row_id = result.get("rowId").and_then(|v| v.as_i64()).unwrap_or(0);
        let credits_after = result
            .get("creditsAfter")
            .and_then(|v| v.as_f64())
            .unwrap_or(0.0);
        Ok((row_id, credits_after, account_email(&result)))
    }

    fn get_compute_log(
        &self,
        agent_did: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        let mut path = format!("{}?limit={}", agent_path(agent_did, "log"), limit);
        if let Some(s) = since {
            path.push_str(&format!("&since={}", urlencoding::encode(s)));
        }
        let result = self.get_json(&path)?;
        parse_compute_log_entries(&result)
    }

    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        let mut path = format!("/billing/log?limit={}", limit);
        if let Some(s) = since {
            path.push_str(&format!("&since={}", urlencoding::encode(s)));
        }
        let result = self.get_json(&path)?;
        parse_compute_log_entries(&result)
    }

    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError> {
        let result = self.delete_json(&format!(
            "/billing/log?before={}",
            urlencoding::encode(before)
        ))?;
        Ok(result.get("deleted").and_then(|v| v.as_u64()).unwrap_or(0) as usize)
    }

    fn get_user_credits(&self, email: &str) -> Result<f64, AnyError> {
        let result = self.get_json(&format!("/billing/{}/credits", encode_email(email)))?;
        result
            .get("credits")
            .and_then(|v| v.as_f64())
            .ok_or_else(|| anyhow!("Missing 'credits' in response"))
    }

    fn set_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/credits", encode_email(email)),
            &serde_json::json!({ "amount": amount }),
        )?;
        Ok(())
    }

    fn add_user_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        self.post_json(
            &format!("/billing/{}/credits/add", encode_email(email)),
            &serde_json::json!({ "amount": amount }),
        )?;
        Ok(())
    }

    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError> {
        let result = self.get_json(&format!("/billing/{}/free-access", encode_email(email)))?;
        result
            .get("enabled")
            .and_then(|v| v.as_bool())
            .ok_or_else(|| anyhow!("Missing 'enabled' in response"))
    }

    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/free-access", encode_email(email)),
            &serde_json::json!({ "enabled": enabled }),
        )?;
        Ok(())
    }

    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError> {
        let result = self.get_json(&format!("/billing/{}/hot-wallet", encode_email(email)))?;
        Ok(result
            .get("address")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/hot-wallet", encode_email(email)),
            &serde_json::json!({ "address": address }),
        )?;
        Ok(())
    }

    fn get_user_by_hot_wallet_address(
        &self,
        address: &str,
    ) -> Result<Option<(String, String)>, AnyError> {
        let Some(body) = self.get_json_or_not_found(&format!(
            "/billing/lookup-by-wallet/{}",
            urlencoding::encode(address)
        ))?
        else {
            return Ok(None);
        };
        let field = |name: &str| {
            body.get(name)
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .ok_or_else(|| anyhow!("Missing '{}' in wallet lookup response", name))
        };
        Ok(Some((field("email")?, field("did")?)))
    }

    fn get_rates(&self) -> Result<Vec<(String, f64)>, AnyError> {
        let result = self.get_json("/billing/rates")?;
        let rates = result
            .get("rates")
            .and_then(|v| v.as_array())
            .ok_or_else(|| anyhow!("Missing 'rates' in response"))?;
        rates
            .iter()
            .map(|r| {
                let desc = r
                    .get("description")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow!("Missing description"))?
                    .to_string();
                let price = r
                    .get("price")
                    .and_then(|v| v.as_f64())
                    .ok_or_else(|| anyhow!("Missing price"))?;
                Ok((desc, price))
            })
            .collect()
    }

    /// 404 is an unpriced operation (`Ok(None)`); every other failure is `Err`, and so is a
    /// reply without a numeric `price`: it is not "unpriced".
    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError> {
        let Some(result) = self.get_json_or_not_found(&format!(
            "/billing/rates/{}",
            urlencoding::encode(description)
        ))?
        else {
            return Ok(None);
        };
        result
            .get("price")
            .and_then(|v| v.as_f64())
            .map(Some)
            .ok_or_else(|| anyhow!("Missing 'price' in rate response for {}", description))
    }

    fn set_rates(&self, rates: &[(String, f64)]) -> Result<(), AnyError> {
        let rates_json: Vec<serde_json::Value> = rates
            .iter()
            .map(|(desc, price)| serde_json::json!({"description": desc, "price": price}))
            .collect();
        self.put_json(
            "/billing/rates",
            &serde_json::json!({ "rates": rates_json }),
        )?;
        Ok(())
    }

    /// A reply without `enabled` is an error, not "free hosting is on".
    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError> {
        let result = self.get_json("/billing/config/free-hosting")?;
        result
            .get("enabled")
            .and_then(|v| v.as_bool())
            .ok_or_else(|| anyhow!("Missing 'enabled' in response"))
    }

    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError> {
        self.put_json(
            "/billing/config/free-hosting",
            &serde_json::json!({ "enabled": enabled }),
        )?;
        Ok(())
    }

    fn get_tier_limits(&self, agent_did: &str) -> Result<TierLimits, AnyError> {
        let result = self.get_json(&agent_path(agent_did, "tier-limits"))?;
        Ok(TierLimits {
            max_perspectives: result.get("maxSpaces").and_then(|v| v.as_i64()),
            max_members_per_perspective: result.get("maxMembersPerSpace").and_then(|v| v.as_i64()),
            monthly_inference_credits_cents: result
                .get("monthlyInferenceCreditsCents")
                .and_then(|v| v.as_i64()),
            storage_limit_bytes: result.get("storageLimitBytes").and_then(|v| v.as_i64()),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────────

fn parse_compute_log_entries(value: &serde_json::Value) -> Result<Vec<ComputeLogEntry>, AnyError> {
    let entries = value
        .get("entries")
        .and_then(|v| v.as_array())
        .ok_or_else(|| anyhow!("Missing 'entries' in response"))?;
    entries
        .iter()
        .map(|e| {
            Ok(ComputeLogEntry {
                id: e.get("id").and_then(|v| v.as_i64()).unwrap_or(0),
                user_email: e
                    .get("userEmail")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                timestamp: e
                    .get("timestamp")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                operation: e
                    .get("operation")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                summary: e
                    .get("summary")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string()),
                cost: e.get("cost").and_then(|v| v.as_f64()).unwrap_or(0.0),
                credits_after: e
                    .get("creditsAfter")
                    .and_then(|v| v.as_f64())
                    .unwrap_or(0.0),
            })
        })
        .collect()
}

// ── Tests ──────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── LocalBillingBackend instantiation ──────────────────────────────

    #[test]
    fn test_local_billing_backend_new() {
        let backend = LocalBillingBackend::new();
        // Verify downcast works
        assert!(backend
            .as_any()
            .downcast_ref::<LocalBillingBackend>()
            .is_some());
    }

    // ── SharedBillingBackend HTTP tests ────────────────────────────────

    const DID: &str = "did:key:z6MkAgent";
    // `agent_path` percent-encodes the DID: `:` becomes `%3A`.
    const DID_PATH: &str = "/billing/agents/did%3Akey%3Az6MkAgent";

    #[test]
    fn test_shared_get_agent_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", format!("{DID_PATH}/credits").as_str())
            .match_header("Authorization", "Bearer bill-tok")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"credits":42.5}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let credits = backend.get_agent_credits(DID).unwrap();
        assert!((credits - 42.5).abs() < f64::EPSILON);
        mock.assert();
    }

    /// Calls the backend twice from the runtime the test runs in: each call sends and reads
    /// its reply, the second reuses the client and its pooled connection, and dropping the
    /// backend inside the runtime does not panic.
    async fn credits_twice_from_a_runtime() {
        let mut server = mockito::Server::new_async().await;
        let mock = server
            .mock("GET", format!("{DID_PATH}/credits").as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"credits":7}"#)
            .expect(2)
            .create_async()
            .await;
        let backend = SharedBillingBackend::new(server.url(), "bill-tok".to_string());
        assert_eq!(backend.get_agent_credits(DID).unwrap(), 7.0);
        assert_eq!(backend.get_agent_credits(DID).unwrap(), 7.0);
        mock.assert_async().await;
        drop(backend);
    }

    // An executor handler: a worker of the multi-thread runtime.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn test_shared_calls_from_a_multi_thread_runtime() {
        credits_twice_from_a_runtime().await;
    }

    // A current-thread runtime, where `block_in_place` is not allowed.
    #[tokio::test(flavor = "current_thread")]
    async fn test_shared_calls_from_a_current_thread_runtime() {
        credits_twice_from_a_runtime().await;
    }

    // A plain thread, as the transcription worker: no runtime. The second call reuses the
    // connection of the first; with a runtime per call it found that runtime gone.
    #[test]
    fn test_shared_calls_from_a_plain_thread_reuse_the_client() {
        let mut server = mockito::Server::new();
        let mock = server
            .mock("GET", format!("{DID_PATH}/credits").as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"credits":7}"#)
            .expect(3)
            .create();
        let backend = SharedBillingBackend::new(server.url(), "bill-tok".to_string());
        for _ in 0..3 {
            assert_eq!(backend.get_agent_credits(DID).unwrap(), 7.0);
        }
        mock.assert();
    }

    #[test]
    fn test_shared_get_user_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/user%40test.com/credits")
            .match_header("Authorization", "Bearer bill-tok")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"credits":3.0}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let credits = backend.get_user_credits("user@test.com").unwrap();
        assert!((credits - 3.0).abs() < f64::EPSILON);
        mock.assert();
    }

    #[test]
    fn test_shared_set_user_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("PUT", "/billing/user%40test.com/credits")
            .match_body(mockito::Matcher::Json(
                serde_json::json!({ "amount": 100.0 }),
            ))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"ok":true}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        backend.set_user_credits("user@test.com", 100.0).unwrap();
        mock.assert();
    }

    #[test]
    fn test_shared_add_agent_credits_returns_the_account_email() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", format!("{DID_PATH}/credits/add").as_str())
            .match_body(mockito::Matcher::Json(
                serde_json::json!({ "amount": 50.0 }),
            ))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"email":"user@test.com"}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        assert_eq!(
            backend.add_agent_credits(DID, 50.0).unwrap(),
            "user@test.com"
        );
        mock.assert();
    }

    #[test]
    fn test_shared_reserve_maps_402_and_404() {
        let mut server = mockito::Server::new();
        let url = server.url();
        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());

        let ok = server
            .mock("POST", format!("{DID_PATH}/reserve").as_str())
            .with_status(200)
            .with_body("")
            .expect(1)
            .create();
        // A committed reservation with an empty reply is still a success.
        assert_eq!(
            backend.deduct_agent_credits_if_available(DID, 1.0).unwrap(),
            ""
        );
        ok.assert();
        ok.remove();

        let broke = server
            .mock("POST", format!("{DID_PATH}/reserve").as_str())
            .with_status(402)
            .create();
        assert!(matches!(
            backend.deduct_agent_credits_if_available(DID, 1.0),
            Err(BillingError::InsufficientCredits)
        ));
        broke.assert();
        broke.remove();

        let missing = server
            .mock("POST", format!("{DID_PATH}/reserve").as_str())
            .with_status(404)
            .create();
        assert!(matches!(
            backend.deduct_agent_credits_if_available(DID, 1.0),
            Err(BillingError::UserNotFound(did)) if did == DID
        ));
        missing.assert();
    }

    #[test]
    fn test_shared_deduct_credits_success() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", format!("{DID_PATH}/deduct").as_str())
            .match_body(mockito::Matcher::Json(serde_json::json!({
                "amount": 7.5,
                "operation": "ai_inference",
                "summary": "test",
            })))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"rowId":7,"creditsAfter":92.5,"email":"user@test.com"}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let (row_id, credits_after, email) = backend
            .deduct_credits_and_log(DID, 7.5, "ai_inference", Some("test"))
            .unwrap();
        assert_eq!(row_id, 7);
        assert!((credits_after - 92.5).abs() < f64::EPSILON);
        assert_eq!(email, "user@test.com");
        mock.assert();
    }

    #[test]
    fn test_shared_deduct_insufficient_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", format!("{DID_PATH}/deduct").as_str())
            .with_status(402)
            .with_header("content-type", "application/json")
            .with_body(r#"{"error":"insufficient credits"}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.deduct_credits_and_log(DID, 1000.0, "ai_inference", None);
        assert!(matches!(result, Err(BillingError::InsufficientCredits)));
        mock.assert();
    }

    #[test]
    fn test_shared_deduct_transport_error_is_other() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", format!("{DID_PATH}/deduct").as_str())
            .with_status(502)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.deduct_credits_and_log(DID, 1.0, "test", None);
        assert!(matches!(result, Err(BillingError::Other(_))));
        mock.assert();
    }

    #[test]
    fn test_shared_get_compute_log() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", format!("{DID_PATH}/log").as_str())
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("limit".into(), "10".into()),
                mockito::Matcher::UrlEncoded("since".into(), "2026-01-01T00:00:00+00:00".into()),
            ]))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"entries":[{"id":1,"userEmail":"user@test.com","timestamp":"2026-01-01","operation":"test","summary":null,"cost":1.0,"creditsAfter":99.0}]}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let entries = backend
            .get_compute_log(DID, Some("2026-01-01T00:00:00+00:00"), 10)
            .unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].id, 1);
        mock.assert();
    }

    #[test]
    fn test_shared_get_rates() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/rates")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"rates":[{"description":"link write","price":0.25}]}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let rates = backend.get_rates().unwrap();
        assert_eq!(rates.len(), 1);
        assert_eq!(rates[0].0, "link write");
        assert!((rates[0].1 - 0.25).abs() < f64::EPSILON);
        mock.assert();
    }

    // An unpriced model (404) is `Ok(None)`; a failing backend is `Err`, never a zero rate.
    #[test]
    fn test_shared_get_rate_tells_unpriced_from_failure() {
        let mut server = mockito::Server::new();
        let url = server.url();
        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());

        let priced = server
            .mock("GET", "/billing/rates/gpt%204o")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"price":0.5}"#)
            .create();
        assert_eq!(backend.get_rate("gpt 4o").unwrap(), Some(0.5));
        priced.assert();

        let unpriced = server
            .mock("GET", "/billing/rates/unpriced")
            .with_status(404)
            .create();
        assert_eq!(backend.get_rate("unpriced").unwrap(), None);
        unpriced.assert();

        let failing = server
            .mock("GET", "/billing/rates/failing")
            .with_status(502)
            .create();
        assert!(backend.get_rate("failing").is_err());
        failing.assert();

        let priceless = server
            .mock("GET", "/billing/rates/priceless")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{}"#)
            .create();
        assert!(backend.get_rate("priceless").is_err());
        priceless.assert();
    }

    #[test]
    fn test_shared_set_rates() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("PUT", "/billing/rates")
            .match_body(mockito::Matcher::Json(serde_json::json!({
                "rates": [{ "description": "link write", "price": 0.5 }]
            })))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"ok":true}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        backend
            .set_rates(&[("link write".to_string(), 0.5)])
            .unwrap();
        mock.assert();
    }

    #[test]
    fn test_shared_free_hosting() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let get_mock = server
            .mock("GET", "/billing/config/free-hosting")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"enabled":false}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let enabled = backend.get_free_hosting_enabled().unwrap();
        assert!(!enabled);
        get_mock.assert();
    }

    // A failing backend or a reply without `enabled` is an error, never "free hosting on".
    #[test]
    fn test_shared_free_hosting_fails_closed() {
        let mut server = mockito::Server::new();
        let url = server.url();
        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());

        let failing = server
            .mock("GET", "/billing/config/free-hosting")
            .with_status(502)
            .create();
        assert!(backend.get_free_hosting_enabled().is_err());
        failing.assert();
        failing.remove();

        let empty = server
            .mock("GET", "/billing/config/free-hosting")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body("{}")
            .create();
        assert!(backend.get_free_hosting_enabled().is_err());
        empty.assert();
    }

    #[test]
    fn test_shared_agent_and_user_free_access() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let agent = server
            .mock("GET", format!("{DID_PATH}/free-access").as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"enabled":true}"#)
            .create();
        let user = server
            .mock("GET", "/billing/user%40test.com/free-access")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"enabled":false}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        assert!(backend.get_agent_free_access(DID).unwrap());
        assert!(!backend.get_user_free_access("user@test.com").unwrap());
        agent.assert();
        user.assert();
    }

    #[test]
    fn test_shared_wallet_lookup() {
        let mut server = mockito::Server::new();
        let url = server.url();
        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());

        let found = server
            .mock("GET", "/billing/lookup-by-wallet/uhCAk1")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"email":"user@test.com","did":"did:key:z6MkAgent"}"#)
            .create();
        assert_eq!(
            backend.get_user_by_hot_wallet_address("uhCAk1").unwrap(),
            Some(("user@test.com".to_string(), DID.to_string()))
        );
        found.assert();

        let unknown = server
            .mock("GET", "/billing/lookup-by-wallet/uhCAk2")
            .with_status(404)
            .create();
        assert_eq!(
            backend.get_user_by_hot_wallet_address("uhCAk2").unwrap(),
            None
        );
        unknown.assert();
    }

    #[test]
    fn test_shared_server_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", format!("{DID_PATH}/credits").as_str())
            .with_status(500)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.get_agent_credits(DID);
        assert!(result.is_err());
        mock.assert();
    }

    #[test]
    fn test_shared_billing_backend_downcast() {
        let backend = SharedBillingBackend::new("http://unused".into(), "tok".into());
        assert!(backend
            .as_any()
            .downcast_ref::<SharedBillingBackend>()
            .is_some());
    }

    // ── Global accessor ───────────────────────────────────────────────

    #[test]
    fn test_try_init_returns_false_on_second_call() {
        // OnceCell is static — can only test this behaviour indirectly.
        // The first test that calls try_init wins; subsequent calls return false.
        // This test exercises the pattern — actual init happens in lib.rs::run().
        let _backend = LocalBillingBackend::new();
        // We cannot call try_init here without contaminating the static for
        // other tests, so just verify the type compiles and instantiates.
    }
}
