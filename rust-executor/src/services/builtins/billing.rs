//! Billing: `billing.ledger` (credits, rates, free access, the compute log)
//! and `billing.settlement` (the HoT wallet a user is paid out to).

use async_trait::async_trait;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::sync::OnceCell;

use super::Builtin;
use super::{internal, params, require_admin, to_value, NoParams};
use crate::api::types::HostRate;
use crate::api::ws_handler::WsRpcError;
use crate::db::Ad4mDb;
use crate::services::builtin::{
    CallContext, EventEmitter, EventOwner, ServiceError, ServiceHealth, ServiceImplementation,
    StartContext,
};
use crate::services::interface::{Risk, Selection};
use crate::services::schema_export::{InterfaceBuilder, MethodOptions};
use crate::types::domain::ComputeLogEntry;
use crate::types::HostingUserInfo;

// ── billing.ledger contract ─────────────────────────────────────────────────

/// The caller's account. `null` outside multi-user mode.
#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub struct Account {
    pub email: String,
    /// `null` when the credits cannot be read.
    pub credits: Option<f64>,
    /// The user, or the whole executor, is free of charge.
    pub free_access: bool,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CheckParams {
    /// The metered operation, e.g. `ai.prompt`.
    pub operation: String,
    /// Check another account than the caller's (admin credential only).
    #[serde(default)]
    pub user_email: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChargeParams {
    /// Credits to deduct.
    #[schemars(range(min = 0.0))]
    pub amount: f64,
    /// What the charge is for, e.g. `link_write`; goes to the compute log.
    pub operation: String,
    #[serde(default)]
    pub summary: Option<String>,
    /// Charge another account than the caller's (admin credential only).
    #[serde(default)]
    pub user_email: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ChargeUsageParams {
    /// The model the usage is priced by: its name is the host-rate key.
    pub model_id: String,
    pub operation: String,
    pub units: u64,
    /// What a unit is, e.g. `tokens`, `words`; goes to the compute log.
    pub unit_label: String,
    #[serde(default)]
    pub user_email: Option<String>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct RateParams {
    /// The host-rate key, e.g. `link write`.
    pub key: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SetRatesParams {
    pub rates: Vec<HostRate>,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct EnabledParams {
    pub enabled: bool,
}

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct ComputeLogParams {
    /// Defaults to the caller. Another user's log needs the admin credential.
    #[serde(default)]
    pub user_email: Option<String>,
    /// ISO 8601; only entries after this timestamp.
    #[serde(default)]
    pub since: Option<String>,
    /// Defaults to 100.
    #[serde(default)]
    pub limit: Option<i64>,
}

pub fn ledger_interface() -> Value {
    InterfaceBuilder::new(
        "billing.ledger",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "Compute credits, host rates, free access and the compute log of this executor.",
    )
    .action(
        "READ",
        "See credits and rates",
        "Read your credits, the host rates and your compute log.",
        Risk::Safe,
    )
    .action("CHARGE", "Charge credits", "Deduct compute credits for work done.", Risk::Spend)
    .action(
        "ADMIN",
        "Administer billing",
        "Set host rates and free hosting.",
        Risk::Admin,
    )
    .method::<NoParams, Option<Account>>(
        "account",
        "READ",
        "The caller's credits and free access; `null` outside multi-user mode.",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<CheckParams, bool>(
        "check",
        "READ",
        "May the caller run a metered operation now?",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<ChargeParams, bool>(
        "charge",
        "CHARGE",
        "Deduct credits and log the operation. Answers `false` when no account is charged (single-user executor).",
        MethodOptions { errors: vec![("InsufficientCredits", 409), ("UnknownAccount", 422)], ..Default::default() },
    )
    .method::<ChargeUsageParams, bool>(
        "chargeUsage",
        "CHARGE",
        "Charge `units` of a model's usage at the model's host rate. Answers `false` when no account is charged.",
        MethodOptions { errors: vec![("InsufficientCredits", 409), ("UnknownAccount", 422)], ..Default::default() },
    )
    .method::<RateParams, Option<f64>>(
        "rate",
        "READ",
        "The host rate of a key, with the executor's defaults; `null` when unpriced.",
        MethodOptions { read: true, ..Default::default() },
    )
    .method::<NoParams, Vec<HostRate>>(
        "rates",
        "READ",
        "The host rates.",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<SetRatesParams, bool>(
        "setRates",
        "ADMIN",
        "Replace the host rates. Needs the admin credential.",
        MethodOptions {
            errors: vec![("InvalidRates", 422)],
            ..Default::default()
        },
    )
    .method::<NoParams, bool>(
        "freeHostingEnabled",
        "READ",
        "Is hosting free for everyone?",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<EnabledParams, bool>(
        "setFreeHostingEnabled",
        "ADMIN",
        "Make hosting free for everyone, or not; answers the new state.",
        MethodOptions::default(),
    )
    .method::<ComputeLogParams, Vec<ComputeLogEntry>>(
        "computeLog",
        "READ",
        "Compute log entries, newest first.",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .event::<HostingUserInfo>(
        "account-changed",
        "READ",
        "A user's credits, wallet or free access changed.",
        None,
    )
    .build()
}

// ── billing.settlement contract ─────────────────────────────────────────────

#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct LinkWalletParams {
    pub address: String,
}

#[derive(Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct RequestPaymentParams {
    #[serde(rename = "amountHOT")]
    pub amount_hot: String,
}

#[derive(Serialize, Deserialize, JsonSchema)]
pub struct PaymentRequested {
    pub success: bool,
    #[serde(rename = "amountHOT")]
    pub amount_hot: String,
}

pub fn settlement_interface() -> Value {
    InterfaceBuilder::new(
        "billing.settlement",
        super::AUTHOR,
        "1.0.0",
        Selection::Executor,
        "The HoT wallet a user's credits settle to.",
    )
    .action(
        "READ",
        "See your wallet",
        "Read the wallet linked to your account.",
        Risk::Safe,
    )
    .action(
        "LINK",
        "Link a wallet",
        "Link a HoT wallet to your account.",
        Risk::Write,
    )
    .action(
        "PAY",
        "Request payments",
        "Ask for a payment to your wallet.",
        Risk::Spend,
    )
    .method::<LinkWalletParams, bool>(
        "linkWallet",
        "LINK",
        "Link a HoT wallet address to the caller's account.",
        MethodOptions {
            errors: vec![("NoAccount", 409)],
            ..Default::default()
        },
    )
    .method::<NoParams, Option<String>>(
        "linkedWallet",
        "READ",
        "The caller's linked wallet address.",
        MethodOptions {
            read: true,
            ..Default::default()
        },
    )
    .method::<RequestPaymentParams, PaymentRequested>(
        "requestPayment",
        "PAY",
        "Request a payment of `amountHOT`.",
        MethodOptions::default(),
    )
    .build()
}

// ── Implementation ──────────────────────────────────────────────────────────

fn validate_rates(rates: Vec<HostRate>) -> Result<Vec<(String, f64)>, ServiceError> {
    let mut seen = std::collections::HashSet::new();
    rates
        .into_iter()
        .enumerate()
        .map(|(i, r)| {
            if r.description.is_empty() || !r.price_in_hot.is_finite() || r.price_in_hot < 0.0 {
                return Err(ServiceError::method(
                    "InvalidRates",
                    format!(
                        "Rate {} needs a description and a non-negative priceInHOT",
                        i
                    ),
                ));
            }
            // `description` is the table's primary key.
            if !seen.insert(r.description.clone()) {
                return Err(ServiceError::method(
                    "InvalidRates",
                    format!("Rate {} repeats description '{}'", i, r.description),
                ));
            }
            Ok((r.description, r.price_in_hot))
        })
        .collect()
}

fn global_free() -> bool {
    Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true)
}

/// The account a call acts on: `explicit` (admin credential only, unless it
/// is the caller's own), else the caller's.
fn target_email(
    ctx: &CallContext,
    explicit: Option<String>,
) -> Result<Option<String>, ServiceError> {
    match explicit {
        Some(e) if Some(&e) != email(ctx).as_ref() => {
            require_admin(ctx)?;
            Ok(Some(e))
        }
        Some(e) => Ok(Some(e)),
        None => Ok(email(ctx)),
    }
}

fn charge_error(e: crate::billing::BillingError) -> ServiceError {
    match e {
        crate::billing::BillingError::InsufficientCredits => {
            ServiceError::method("InsufficientCredits", "Insufficient compute credits")
        }
        crate::billing::BillingError::UserNotFound(u) => {
            ServiceError::method("UnknownAccount", format!("User not found: {}", u))
        }
        other => internal(format!("{:?}", other)),
    }
}

/// The caller's account email: the session's user, else the one in its token.
fn email(ctx: &CallContext) -> Option<String> {
    ctx.user.clone().or_else(|| {
        ctx.auth_token
            .clone()
            .and_then(crate::agent::capabilities::user_email_from_token)
    })
}

/// May the caller start a metered operation now? Free hosting, free access
/// and single-user executors always may; otherwise the account needs credits.
/// Fails closed when the account cannot be read.
pub fn may_spend(ctx: &CallContext) -> bool {
    may_spend_email(email(ctx).as_deref())
}

fn may_spend_email(email: Option<&str>) -> bool {
    if global_free() {
        return true;
    }
    let Some(email) = email else {
        return true;
    };
    match Ad4mDb::with_global_instance(|db| db.get_user_free_access(email)) {
        Ok(true) => true,
        Ok(false) => {
            Ad4mDb::with_global_instance(|db| db.get_user_credits(email)).is_ok_and(|c| c > 0.0)
        }
        Err(_) => false,
    }
}

/// Every 2 s, announce `account-changed` for the accounts whose credits,
/// free access or wallet changed since (`pubsub::mark_credits_dirty`).
async fn announce_account_changes(events: EventEmitter) {
    loop {
        tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        let dirty: Vec<String> = match crate::pubsub::DIRTY_CREDIT_USERS.lock() {
            Ok(mut set) => set.drain().collect(),
            Err(e) => {
                log::error!("billing: dirty-account set poisoned: {}", e);
                continue;
            }
        };
        for email in dirty {
            match account_info(&email) {
                Ok(info) => {
                    let payload = serde_json::to_value(&info).unwrap_or_default();
                    if let Err(e) = events
                        .emit("account-changed", EventOwner::User(email), payload)
                        .await
                    {
                        log::error!("billing: account-changed dropped: {}", e);
                    }
                }
                Err(e) => log::error!("billing: cannot read account {}: {}", email, e),
            }
        }
    }
}

fn account_info(email: &str) -> Result<HostingUserInfo, String> {
    let free_access = global_free()
        || Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))
            .map_err(|e| e.to_string())?;
    let remaining_credits = if free_access {
        "unlimited".to_string()
    } else {
        Ad4mDb::with_global_instance(|db| db.get_user_credits(email))
            .map_err(|e| e.to_string())?
            .to_string()
    };
    let hot_wallet_address = Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(email))
        .map_err(|e| e.to_string())?;
    Ok(HostingUserInfo {
        email: email.to_string(),
        remaining_credits,
        hot_wallet_address,
        free_access,
    })
}

// ── Executor-internal billing, through the ledger ───────────────────────────

fn system_ctx(module: &str, email: &str) -> CallContext {
    CallContext::system(
        crate::services::Caller::Executor {
            module: module.into(),
        },
        Some(email.to_string()),
        None,
    )
}

/// May `email` start compute now? Fails closed when the ledger is unreachable.
pub async fn check_user(module: &str, email: &str) -> bool {
    super::call::<bool>(
        Builtin::BillingLedger,
        "check",
        json!({ "operation": module }),
        &system_ctx(module, email),
    )
    .await
    .unwrap_or(false)
}

/// Charge `email` for executor work. `Ok(false)` when nothing was charged.
pub async fn charge_user(
    module: &str,
    email: &str,
    amount: f64,
    operation: &str,
    summary: Option<String>,
) -> Result<bool, WsRpcError> {
    super::call(
        Builtin::BillingLedger,
        "charge",
        json!({ "amount": amount, "operation": operation, "summary": summary }),
        &system_ctx(module, email),
    )
    .await
}

/// Charge `email` for `units` of a model's usage at its host rate.
pub async fn charge_usage(
    module: &str,
    email: &str,
    model_id: &str,
    operation: &str,
    units: u64,
    unit_label: &str,
) -> Result<bool, WsRpcError> {
    super::call(
        Builtin::BillingLedger,
        "chargeUsage",
        json!({ "modelId": model_id, "operation": operation, "units": units, "unitLabel": unit_label }),
        &system_ctx(module, email),
    )
    .await
}

/// The host rate of `key`, with the executor's defaults.
pub async fn rate(key: &str) -> Option<f64> {
    let ctx = CallContext::system(
        crate::services::Caller::Executor {
            module: "billing".into(),
        },
        None,
        None,
    );
    super::call::<Option<f64>>(Builtin::BillingLedger, "rate", json!({ "key": key }), &ctx)
        .await
        .ok()
        .flatten()
}

#[derive(Default)]
pub struct Billing {
    started: OnceCell<()>,
}

#[async_trait]
impl ServiceImplementation for Billing {
    async fn start(&self, ctx: StartContext) -> Result<(), String> {
        if self.started.set(()).is_ok() {
            tokio::spawn(announce_account_changes(ctx.events));
        }
        Ok(())
    }

    async fn stop(&self) -> Result<(), String> {
        Ok(())
    }

    async fn health(&self) -> ServiceHealth {
        ServiceHealth::Running
    }

    async fn call(&self, method: &str, p: Value, ctx: CallContext) -> Result<Value, ServiceError> {
        match method {
            // billing.ledger
            "account" => {
                let Some(email) = ctx.user.clone() else {
                    return to_value(Option::<Account>::None);
                };
                let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(&email)).ok();
                let free_access = global_free()
                    || Ad4mDb::with_global_instance(|db| db.get_user_free_access(&email))
                        .unwrap_or(false);
                to_value(Some(Account {
                    email,
                    credits,
                    free_access,
                }))
            }
            "check" => {
                let p: CheckParams = params(p)?;
                let email = target_email(&ctx, p.user_email)?;
                to_value(may_spend_email(email.as_deref()))
            }
            "charge" => {
                let p: ChargeParams = params(p)?;
                let Some(email) = target_email(&ctx, p.user_email)? else {
                    return to_value(false);
                };
                crate::billing::bill_compute(&email, p.amount, &p.operation, p.summary.as_deref())
                    .map_err(charge_error)?;
                to_value(true)
            }
            "chargeUsage" => {
                let p: ChargeUsageParams = params(p)?;
                let Some(email) = target_email(&ctx, p.user_email)? else {
                    return to_value(false);
                };
                crate::billing::bill_ai_operation(
                    &email,
                    &p.model_id,
                    &p.operation,
                    p.units as usize,
                    &p.unit_label,
                )
                .map_err(charge_error)?;
                to_value(true)
            }
            "rate" => {
                let p: RateParams = params(p)?;
                to_value(crate::billing::host_rate(&p.key))
            }
            "rates" => {
                let rates: Vec<HostRate> = Ad4mDb::with_global_instance(|db| db.get_host_rates())
                    .map_err(internal)?
                    .into_iter()
                    .map(|(description, price_in_hot)| HostRate {
                        description,
                        price_in_hot,
                    })
                    .collect();
                to_value(rates)
            }
            "setRates" => {
                require_admin(&ctx)?;
                let p: SetRatesParams = params(p)?;
                let rates = validate_rates(p.rates)?;
                Ad4mDb::with_global_instance(|db| db.set_host_rates(&rates)).map_err(internal)?;
                to_value(true)
            }
            "freeHostingEnabled" => to_value(
                Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())
                    .map_err(internal)?,
            ),
            "setFreeHostingEnabled" => {
                let p: EnabledParams = params(p)?;
                Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(p.enabled))
                    .map_err(internal)?;
                to_value(p.enabled)
            }
            "computeLog" => {
                let p: ComputeLogParams = params(p)?;
                let own = ctx.user.clone();
                let email = p.user_email.or_else(|| own.clone()).unwrap_or_default();
                // Reading another account's log is an operator task.
                if own.as_deref() != Some(email.as_str()) && own.is_some() {
                    require_admin(&ctx)?;
                }
                let limit = p.limit.unwrap_or(100);
                let logs = Ad4mDb::with_global_instance(|db| {
                    db.get_compute_log(&email, p.since.as_deref(), limit)
                })
                .map_err(internal)?;
                to_value(logs)
            }
            // billing.settlement
            "linkWallet" => {
                let p: LinkWalletParams = params(p)?;
                let email = ctx
                    .user
                    .clone()
                    .ok_or_else(|| ServiceError::method("NoAccount", "User email required"))?;
                Ad4mDb::with_global_instance(|db| db.set_user_hot_wallet(&email, &p.address))
                    .map_err(internal)?;
                to_value(true)
            }
            "linkedWallet" => {
                let Some(email) = ctx.user.clone() else {
                    return to_value(Option::<String>::None);
                };
                to_value(
                    Ad4mDb::with_global_instance(|db| db.get_user_hot_wallet(&email))
                        .ok()
                        .flatten(),
                )
            }
            "requestPayment" => {
                let p: RequestPaymentParams = params(p)?;
                to_value(PaymentRequested {
                    success: true,
                    amount_hot: p.amount_hot,
                })
            }
            other => Err(internal(format!("billing has no method {}", other))),
        }
    }
}
