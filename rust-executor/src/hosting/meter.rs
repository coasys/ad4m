//! The tenant's usage (AR-05 4.6): what `host.usage` reports. It only reports; no limit
//! uses it (decision 22.11), and it is not the billing meter (billing records nothing while
//! access is free, so the AI runs have their own count: [`count_ai_run`]).
//!
//! Storage is measured, not tracked per write: a tenant's total is recomputed from the
//! database and the disk when it is older than [`REMEASURE_SECS`], and kept in memory
//! between. A removal or an import shows after the next measure.

use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::policy;
use super::tenants::{self, Tenant};
use super::HostRefusal;
use crate::types::PerspectiveHandle;

/// How long a measured storage total stands.
pub const REMEASURE_SECS: u64 = 15 * 60;

/// `host.usage`'s answer, for the session's own tenant.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantUsage {
    pub tenant_id: String,
    pub did_scid: String,
    /// `active` or `held`.
    pub state: String,
    #[ts(type = "number | null")]
    pub activated_at: Option<u64>,
    pub host: UsageHost,
    /// Neighbourhood perspectives this tenant holds here.
    pub spaces: Vec<HeldSpace>,
    /// Perspectives with no neighbourhood.
    pub private_perspectives: u32,
    pub storage: StorageUsage,
    pub ai_runs: AiRunsUsage,
    pub spend: TenantSpend,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsageHost {
    pub name: String,
    pub run_by_coasys: bool,
    pub operator_did: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HeldSpace {
    pub perspective_uuid: String,
    pub name: Option<String>,
    pub neighbourhood_url: String,
    /// Another tenant of this executor holds it too.
    pub shared_with_other_tenants: bool,
}

/// No limit (decision 22.11).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageUsage {
    #[ts(type = "number")]
    pub used_bytes: u64,
    /// RFC 3339, when the total was measured.
    pub measured_at: String,
}

/// No limit (decision 22.11).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AiRunsUsage {
    /// `YYYY-MM-DD`, UTC.
    pub day: String,
    #[ts(type = "number")]
    pub count: u64,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantSpend {
    /// The host's `free_hosting_enabled` or the tenant's own free access.
    pub free_access: bool,
    /// `None` when the credits cannot be read.
    pub credits: Option<f64>,
    pub rates: Option<Vec<UsageRate>>,
    /// The sum of the tenant's `compute_log` costs; 0 while access is free.
    pub charged_so_far: f64,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct UsageRate {
    pub description: String,
    pub price_in_hot: f64,
}

/// A measured storage total and when.
#[derive(Clone, Copy)]
struct Measured {
    bytes: u64,
    at: u64,
}

static STORAGE: std::sync::LazyLock<Mutex<HashMap<String, Measured>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

/// Forgets the measured totals (a test, or a tenant whose data went away).
pub fn forget_storage(tenant_id: &str) {
    STORAGE
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .remove(tenant_id);
}

/// Counts one admitted AI run of the managed agent `agent_did` today (UTC): a prompt, an
/// embedding, a `/v1` chat, completion, embedding or speech call, or one opened transcription
/// stream. A caller that is no tenant (the operator, the main agent, an agent with no record)
/// is not counted. Never fails the call it counts: a database error is logged.
pub fn count_ai_run(agent_did: Option<&str>) {
    let Some(did) = agent_did else { return };
    let db = crate::db_backend::db_backend();
    let day = chrono::Utc::now().format("%Y-%m-%d").to_string();
    match db.live_tenant_of_device(did) {
        Ok(Some(tenant)) => {
            if let Err(e) = db.count_ai_run(&tenant.id, &day) {
                log::warn!("could not count an AI run of tenant {}: {e}", tenant.id);
            }
        }
        Ok(None) => {}
        Err(e) => log::warn!("could not look up the tenant of {did}: {e}"),
    }
}

/// The bytes of one perspective: its link rows plus its folder on disk.
fn perspective_bytes(uuid: &str) -> Result<u64, HostRefusal> {
    let rows = crate::db_backend::db_backend()
        .link_bytes(uuid)
        .map_err(|e| HostRefusal::internal(e.to_string()))?;
    let disk = crate::perspectives::perspective_dir(uuid)
        .map(|dir| crate::perspectives::memory_diagnostics::dir_size(&dir))
        .unwrap_or(0);
    Ok(rows + disk)
}

/// The tenant's storage: the measured total while it stands, else measured now.
fn storage(
    tenant_id: &str,
    owned: &[PerspectiveHandle],
    now: u64,
) -> Result<Measured, HostRefusal> {
    let mut stored = STORAGE.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(measured) = stored.get(tenant_id) {
        if measured.at + REMEASURE_SECS > now {
            return Ok(*measured);
        }
    }
    let mut bytes = 0;
    for handle in owned {
        bytes += perspective_bytes(&handle.uuid)?;
    }
    let measured = Measured { bytes, at: now };
    stored.insert(tenant_id.to_string(), measured);
    Ok(measured)
}

/// The usage of `tenant`, given the perspectives it owns and the devices of every live tenant.
/// A neighbourhood perspective two tenants hold counts in full against each.
pub fn usage_of(
    tenant: &Tenant,
    owned: &[PerspectiveHandle],
    tenant_devices: &HashSet<String>,
    operator_did: &str,
    now: u64,
) -> Result<TenantUsage, HostRefusal> {
    let db = crate::db_backend::db_backend();
    let internal = |e: anyhow::Error| HostRefusal::internal(e.to_string());
    let device = tenant.row.device_did.clone().unwrap_or_default();
    let spaces: Vec<HeldSpace> = owned
        .iter()
        .filter_map(|h| {
            h.shared_url.as_ref().map(|url| HeldSpace {
                perspective_uuid: h.uuid.clone(),
                name: h.name.clone(),
                neighbourhood_url: url.clone(),
                shared_with_other_tenants: h
                    .owners
                    .iter()
                    .flatten()
                    .any(|o| o != &device && tenant_devices.contains(o)),
            })
        })
        .collect();
    let measured = storage(&tenant.row.id, owned, now)?;
    let day = chrono::Utc::now().format("%Y-%m-%d").to_string();
    let global_free = db.get_free_hosting_enabled().map_err(internal)?;
    let free_access = global_free || db.get_agent_free_access(&device).unwrap_or(false);
    let display = policy::current()?.display;
    Ok(TenantUsage {
        tenant_id: tenant.row.id.clone(),
        did_scid: tenant.row.did_scid.clone(),
        state: tenant.state.as_str().to_string(),
        activated_at: tenant.row.activated_at,
        host: UsageHost {
            name: display.name,
            run_by_coasys: display.run_by_coasys,
            operator_did: operator_did.to_string(),
        },
        private_perspectives: (owned.len() - spaces.len()) as u32,
        spaces,
        storage: StorageUsage {
            used_bytes: measured.bytes,
            measured_at: chrono::DateTime::from_timestamp(measured.at as i64, 0)
                .map(|t| t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true))
                .unwrap_or_default(),
        },
        ai_runs: AiRunsUsage {
            count: db.ai_runs(&tenant.row.id, &day).map_err(internal)?,
            day,
        },
        spend: TenantSpend {
            free_access,
            credits: db.get_agent_credits(&device).ok(),
            rates: db.get_host_rates().ok().map(|rates| {
                rates
                    .into_iter()
                    .map(|(description, price_in_hot)| UsageRate {
                        description,
                        price_in_hot,
                    })
                    .collect()
            }),
            charged_so_far: db.compute_charged(&device).map_err(internal)?,
        },
    })
}

/// The usage of `tenant` now, from the perspectives it owns on this executor.
pub async fn usage(tenant: &Tenant) -> Result<TenantUsage, HostRefusal> {
    let device = tenant.row.device_did.clone().unwrap_or_default();
    let mut owned = Vec::new();
    for instance in crate::perspectives::all_perspectives() {
        let handle = instance.persisted.lock().await.clone();
        if handle.is_owned_by(&device) {
            owned.push(handle);
        }
    }
    let devices: HashSet<String> = tenants::live_tenants()?
        .into_iter()
        .filter_map(|t| t.row.device_did)
        .collect();
    usage_of(tenant, &owned, &devices, &crate::agent::did(), now_secs())
}

fn now_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

#[cfg(test)]
mod tests;
