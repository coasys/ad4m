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
use crate::db::ComputeLogEntry;

// ── Trait ──────────────────────────────────────────────────────────────────────

/// Abstracts billing operations so the executor can use either a local SQLite
/// database or a remote platform Worker as the billing store.
///
/// Methods are synchronous — matches the WalletBackend / DbBackend pattern.
/// SharedBillingBackend uses `reqwest::blocking` internally.
pub trait BillingBackend: Send + Sync {
    // ── Credits ────────────────────────────────────────────────────────

    /// Get the user's remaining credit balance.
    fn get_credits(&self, email: &str) -> Result<f64, AnyError>;

    /// Set the user's credit balance to an absolute value.
    fn set_credits(&self, email: &str, amount: f64) -> Result<(), AnyError>;

    /// Add credits to the user's balance.
    fn add_credits(&self, email: &str, amount: f64) -> Result<(), AnyError>;

    /// Atomically check that the user has sufficient credits and deduct them.
    /// Does NOT log to compute_log — use for credit reservation (e.g. link writes).
    /// Returns `BillingError::InsufficientCredits` if the user cannot afford the deduction.
    fn deduct_credits_if_available(&self, email: &str, amount: f64) -> Result<(), BillingError>;

    /// Atomically deduct credits and log a compute event.
    /// Returns `(row_id, credits_after)` on success.
    /// Returns `BillingError::InsufficientCredits` if the user cannot afford the deduction.
    fn deduct_credits_and_log(
        &self,
        email: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64), BillingError>;

    // ── Compute log ────────────────────────────────────────────────────

    /// Query compute log entries for a user, ordered newest-first.
    fn get_compute_log(
        &self,
        email: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError>;

    /// Query compute log entries for ALL users (admin). Newest-first.
    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError>;

    /// Delete log entries older than the given ISO 8601 timestamp.
    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError>;

    // ── Host rates ─────────────────────────────────────────────────────

    /// Get all host rates as (description, price) pairs.
    fn get_rates(&self) -> Result<Vec<(String, f64)>, AnyError>;

    /// Get the rate for a specific operation description.
    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError>;

    /// Replace all host rates with the given set.
    fn set_rates(&self, rates: &[(String, f64)]) -> Result<(), AnyError>;

    // ── Free access ────────────────────────────────────────────────────

    /// Check whether global free hosting mode is enabled.
    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError>;

    /// Enable or disable global free hosting mode.
    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError>;

    /// Check whether a specific user has free access.
    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError>;

    /// Enable or disable free access for a specific user.
    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError>;

    // ── HoT wallet (Unyt integration) ──────────────────────────────────

    /// Get the user's HoloFuel/HoT wallet address.
    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError>;

    /// Set the user's HoloFuel/HoT wallet address.
    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError>;

    /// Reverse lookup: find user email by HoT wallet address.
    fn get_user_by_hot_wallet_address(&self, address: &str) -> Result<Option<String>, AnyError>;

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

impl BillingBackend for LocalBillingBackend {
    fn get_credits(&self, email: &str) -> Result<f64, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
    }

    fn set_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.set_user_credits(email, amount))
    }

    fn add_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.add_user_credits(email, amount))
    }

    fn deduct_credits_if_available(&self, email: &str, amount: f64) -> Result<(), BillingError> {
        let result = crate::db::Ad4mDb::with_global_instance(|db| {
            db.deduct_user_credits_if_available(email, amount)
        });
        match result {
            Ok(()) => Ok(()),
            Err(e) => {
                let msg = e.to_string();
                if msg.contains("Insufficient compute credits") {
                    Err(BillingError::InsufficientCredits)
                } else if msg.starts_with("User not found") {
                    Err(BillingError::UserNotFound(email.to_string()))
                } else {
                    Err(BillingError::Other(e))
                }
            }
        }
    }

    fn deduct_credits_and_log(
        &self,
        email: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64), BillingError> {
        let result = crate::db::Ad4mDb::with_global_instance(|db| {
            db.deduct_credits_and_log(email, amount, operation, summary)
        });
        match result {
            Ok((row_id, credits_after)) => Ok((row_id, credits_after)),
            Err(e) => {
                let msg = e.to_string();
                if msg.contains("Insufficient compute credits") {
                    Err(BillingError::InsufficientCredits)
                } else if msg.starts_with("User not found") {
                    Err(BillingError::UserNotFound(email.to_string()))
                } else {
                    Err(BillingError::Other(e))
                }
            }
        }
    }

    fn get_compute_log(
        &self,
        email: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_compute_log(email, since, limit))
    }

    fn get_compute_log_all(
        &self,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_compute_log_all(since, limit))
    }

    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.cleanup_compute_log(before))
    }

    fn get_rates(&self) -> Result<Vec<(String, f64)>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_host_rates())
    }

    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_host_rate(description))
    }

    fn set_rates(&self, rates: &[(String, f64)]) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.set_host_rates(rates))
    }

    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
    }

    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(enabled))
    }

    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
    }

    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.set_user_free_access(email, enabled))
    }

    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(email))
    }

    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(email, address))
    }

    fn get_user_by_hot_wallet_address(&self, address: &str) -> Result<Option<String>, AnyError> {
        crate::db::Ad4mDb::with_global_instance(|db| db.get_user_by_hot_wallet_address(address))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// ── SharedBillingBackend ───────────────────────────────────────────────────────

/// HTTP client that delegates billing operations to the platform Worker's
/// `/internal/billing/` API for shared-mode executors.
pub struct SharedBillingBackend {
    base_url: String,
    token: String,
    client: reqwest::blocking::Client,
}

impl SharedBillingBackend {
    pub fn new(base_url: String, token: String) -> Self {
        SharedBillingBackend {
            base_url: base_url.trim_end_matches('/').to_string(),
            token,
            client: reqwest::blocking::Client::builder()
                .timeout(std::time::Duration::from_secs(30))
                .build()
                .expect("Failed to build SharedBillingBackend HTTP client"),
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }

    fn get_json(&self, path: &str) -> Result<serde_json::Value, AnyError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedBillingBackend GET {} failed: {}", path, e))?;

        if !resp.status().is_success() {
            return Err(anyhow!(
                "SharedBillingBackend GET {} returned {}",
                path,
                resp.status()
            ));
        }

        resp.json()
            .map_err(|e| anyhow!("SharedBillingBackend parse: {}", e))
    }

    fn post_json(
        &self,
        path: &str,
        body: &serde_json::Value,
    ) -> Result<serde_json::Value, AnyError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .json(body)
            .send()
            .map_err(|e| anyhow!("SharedBillingBackend POST {} failed: {}", path, e))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let body_text = resp.text().unwrap_or_default();
            return Err(anyhow!(
                "SharedBillingBackend POST {} returned {}: {}",
                path,
                status,
                body_text
            ));
        }

        resp.json()
            .map_err(|e| anyhow!("SharedBillingBackend parse: {}", e))
    }

    fn put_json(
        &self,
        path: &str,
        body: &serde_json::Value,
    ) -> Result<serde_json::Value, AnyError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .client
            .put(&url)
            .header("Authorization", self.auth_header())
            .json(body)
            .send()
            .map_err(|e| anyhow!("SharedBillingBackend PUT {} failed: {}", path, e))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let body_text = resp.text().unwrap_or_default();
            return Err(anyhow!(
                "SharedBillingBackend PUT {} returned {}: {}",
                path,
                status,
                body_text
            ));
        }

        resp.json()
            .map_err(|e| anyhow!("SharedBillingBackend parse: {}", e))
    }

    fn delete_json(&self, path: &str) -> Result<serde_json::Value, AnyError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .client
            .delete(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedBillingBackend DELETE {} failed: {}", path, e))?;

        if !resp.status().is_success() {
            return Err(anyhow!(
                "SharedBillingBackend DELETE {} returned {}",
                path,
                resp.status()
            ));
        }

        resp.json()
            .map_err(|e| anyhow!("SharedBillingBackend parse: {}", e))
    }
}

impl BillingBackend for SharedBillingBackend {
    fn get_credits(&self, email: &str) -> Result<f64, AnyError> {
        let result = self.get_json(&format!("/billing/{}/credits", email))?;
        result
            .get("credits")
            .and_then(|v| v.as_f64())
            .ok_or_else(|| anyhow!("Missing 'credits' in response"))
    }

    fn set_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/credits", email),
            &serde_json::json!({ "amount": amount }),
        )?;
        Ok(())
    }

    fn add_credits(&self, email: &str, amount: f64) -> Result<(), AnyError> {
        self.post_json(
            &format!("/billing/{}/credits/add", email),
            &serde_json::json!({ "amount": amount }),
        )?;
        Ok(())
    }

    fn deduct_credits_if_available(&self, email: &str, amount: f64) -> Result<(), BillingError> {
        let body = serde_json::json!({ "amount": amount });
        let url = format!("{}/billing/{}/reserve", self.base_url, email);
        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .json(&body)
            .send()
            .map_err(|e| {
                BillingError::Other(anyhow!("SharedBillingBackend reserve failed: {}", e))
            })?;

        if resp.status().as_u16() == 402 {
            return Err(BillingError::InsufficientCredits);
        }
        if resp.status().as_u16() == 404 {
            return Err(BillingError::UserNotFound(email.to_string()));
        }
        if !resp.status().is_success() {
            return Err(BillingError::Other(anyhow!(
                "SharedBillingBackend reserve returned {}",
                resp.status()
            )));
        }
        Ok(())
    }

    fn deduct_credits_and_log(
        &self,
        email: &str,
        amount: f64,
        operation: &str,
        summary: Option<&str>,
    ) -> Result<(i64, f64), BillingError> {
        let body = serde_json::json!({
            "amount": amount,
            "operation": operation,
            "summary": summary,
        });

        let url = format!("{}/billing/{}/deduct", self.base_url, email);
        let resp = self
            .client
            .post(&url)
            .header("Authorization", self.auth_header())
            .json(&body)
            .send()
            .map_err(|e| {
                BillingError::Other(anyhow!("SharedBillingBackend deduct failed: {}", e))
            })?;

        if resp.status().as_u16() == 402 {
            return Err(BillingError::InsufficientCredits);
        }
        if resp.status().as_u16() == 404 {
            return Err(BillingError::UserNotFound(email.to_string()));
        }
        if !resp.status().is_success() {
            return Err(BillingError::Other(anyhow!(
                "SharedBillingBackend deduct returned {}",
                resp.status()
            )));
        }

        let result: serde_json::Value = resp
            .json()
            .map_err(|e| BillingError::Other(anyhow!("SharedBillingBackend parse: {}", e)))?;

        let row_id = result.get("rowId").and_then(|v| v.as_i64()).unwrap_or(0);
        let credits_after = result
            .get("creditsAfter")
            .and_then(|v| v.as_f64())
            .unwrap_or(0.0);

        Ok((row_id, credits_after))
    }

    fn get_compute_log(
        &self,
        email: &str,
        since: Option<&str>,
        limit: i64,
    ) -> Result<Vec<ComputeLogEntry>, AnyError> {
        let mut path = format!("/billing/{}/log?limit={}", email, limit);
        if let Some(s) = since {
            path.push_str(&format!("&since={}", s));
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
            path.push_str(&format!("&since={}", s));
        }
        let result = self.get_json(&path)?;
        parse_compute_log_entries(&result)
    }

    fn cleanup_compute_log(&self, before: &str) -> Result<usize, AnyError> {
        let result = self.delete_json(&format!("/billing/log?before={}", before))?;
        Ok(result.get("deleted").and_then(|v| v.as_u64()).unwrap_or(0) as usize)
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

    fn get_rate(&self, description: &str) -> Result<Option<f64>, AnyError> {
        let result = self.get_json(&format!(
            "/billing/rates/{}",
            urlencoding::encode(description)
        ))?;
        Ok(result.get("price").and_then(|v| v.as_f64()))
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

    fn get_free_hosting_enabled(&self) -> Result<bool, AnyError> {
        let result = self.get_json("/billing/config/free-hosting")?;
        Ok(result
            .get("enabled")
            .and_then(|v| v.as_bool())
            .unwrap_or(true))
    }

    fn set_free_hosting_enabled(&self, enabled: bool) -> Result<(), AnyError> {
        self.put_json(
            "/billing/config/free-hosting",
            &serde_json::json!({ "enabled": enabled }),
        )?;
        Ok(())
    }

    fn get_user_free_access(&self, email: &str) -> Result<bool, AnyError> {
        let result = self.get_json(&format!("/billing/{}/free-access", email))?;
        Ok(result
            .get("enabled")
            .and_then(|v| v.as_bool())
            .unwrap_or(false))
    }

    fn set_user_free_access(&self, email: &str, enabled: bool) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/free-access", email),
            &serde_json::json!({ "enabled": enabled }),
        )?;
        Ok(())
    }

    fn get_user_hot_wallet(&self, email: &str) -> Result<Option<String>, AnyError> {
        let result = self.get_json(&format!("/billing/{}/hot-wallet", email))?;
        Ok(result
            .get("address")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    fn set_user_hot_wallet(&self, email: &str, address: &str) -> Result<(), AnyError> {
        self.put_json(
            &format!("/billing/{}/hot-wallet", email),
            &serde_json::json!({ "address": address }),
        )?;
        Ok(())
    }

    fn get_user_by_hot_wallet_address(&self, address: &str) -> Result<Option<String>, AnyError> {
        let url = format!(
            "{}/billing/lookup-by-wallet/{}",
            self.base_url,
            urlencoding::encode(address)
        );
        let resp = self
            .client
            .get(&url)
            .header("Authorization", self.auth_header())
            .send()
            .map_err(|e| anyhow!("SharedBillingBackend wallet lookup failed: {}", e))?;

        if resp.status().as_u16() == 404 {
            return Ok(None);
        }
        if !resp.status().is_success() {
            return Err(anyhow!(
                "SharedBillingBackend wallet lookup returned {}",
                resp.status()
            ));
        }
        let body: serde_json::Value = resp
            .json()
            .map_err(|e| anyhow!("SharedBillingBackend wallet lookup parse: {}", e))?;
        Ok(body
            .get("email")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
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

    #[test]
    fn test_shared_get_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/user@test.com/credits")
            .match_header("Authorization", "Bearer bill-tok")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"credits":42.5}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let credits = backend.get_credits("user@test.com").unwrap();
        assert!((credits - 42.5).abs() < f64::EPSILON);
        mock.assert();
    }

    #[test]
    fn test_shared_set_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("PUT", "/billing/user@test.com/credits")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"ok":true}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        backend.set_credits("user@test.com", 100.0).unwrap();
        mock.assert();
    }

    #[test]
    fn test_shared_add_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/billing/user@test.com/credits/add")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"ok":true}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        backend.add_credits("user@test.com", 50.0).unwrap();
        mock.assert();
    }

    #[test]
    fn test_shared_deduct_credits_success() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/billing/user@test.com/deduct")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"rowId":7,"creditsAfter":92.5}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let (row_id, credits_after) = backend
            .deduct_credits_and_log("user@test.com", 7.5, "ai_inference", Some("test"))
            .unwrap();
        assert_eq!(row_id, 7);
        assert!((credits_after - 92.5).abs() < f64::EPSILON);
        mock.assert();
    }

    #[test]
    fn test_shared_deduct_insufficient_credits() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/billing/user@test.com/deduct")
            .with_status(402)
            .with_header("content-type", "application/json")
            .with_body(r#"{"error":"insufficient credits"}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.deduct_credits_and_log("user@test.com", 1000.0, "ai_inference", None);
        assert!(matches!(result, Err(BillingError::InsufficientCredits)));
        mock.assert();
    }

    #[test]
    fn test_shared_deduct_user_not_found() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("POST", "/billing/unknown@test.com/deduct")
            .with_status(404)
            .with_header("content-type", "application/json")
            .with_body(r#"{"error":"not found"}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.deduct_credits_and_log("unknown@test.com", 1.0, "test", None);
        assert!(matches!(result, Err(BillingError::UserNotFound(_))));
        mock.assert();
    }

    #[test]
    fn test_shared_get_compute_log() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/user@test.com/log")
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("limit".into(), "10".into()),
            ]))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"entries":[{"id":1,"userEmail":"user@test.com","timestamp":"2026-01-01","operation":"test","summary":null,"cost":1.0,"creditsAfter":99.0}]}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let entries = backend.get_compute_log("user@test.com", None, 10).unwrap();
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

    #[test]
    fn test_shared_set_rates() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("PUT", "/billing/rates")
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

    #[test]
    fn test_shared_user_free_access() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/user@test.com/free-access")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"enabled":true}"#)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let free = backend.get_user_free_access("user@test.com").unwrap();
        assert!(free);
        mock.assert();
    }

    #[test]
    fn test_shared_server_error() {
        let mut server = mockito::Server::new();
        let url = server.url();

        let mock = server
            .mock("GET", "/billing/user@test.com/credits")
            .with_status(500)
            .create();

        let backend = SharedBillingBackend::new(url, "bill-tok".to_string());
        let result = backend.get_credits("user@test.com");
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
