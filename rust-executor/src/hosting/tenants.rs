//! Tenants (AR-05 4.4, 4.5, 4.9): the states, the join gate and its sequence, activation and
//! expiry, the revocation sweep, leaving, removal, and the gate for a claimed guest.

use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use ad4m_identity_core::link::OfferString;
use ad4m_identity_core::{did_of, scid_of};
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use base64::Engine;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use ts_rs::TS;

use crate::db::{InviteRow, TenantInsert, TenantRow};
use crate::db_backend::db_backend;

use super::policy::{self, SignupPolicy};
use super::{HostEnv, HostEvent, HostRefusal, KeyStanding, LeaveResult};

/// How long a `Pending` record lives: the offer's 5 minutes plus the confirm step.
pub const PENDING_SECS: u64 = 600;
/// Open invites a host keeps (AR-05 6.3).
pub const MAX_OPEN_INVITES: u32 = 100;
/// `host.join` calls per network address per window (AR-05 6.3).
pub const JOINS_PER_HOUR: usize = 10;

/// Where a tenant is in its life on this host.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "snake_case")]
#[ts(export)]
pub enum TenantState {
    /// Admitted, the offer claimed, the key not yet in the person's log.
    Pending,
    /// The device key holds validity in the person's log: sessions open.
    Active,
    /// A claimed guest the setting or the capacity did not admit: reads, exports, usage and
    /// leave only.
    Held,
    /// The person left with keep, or the key was revoked elsewhere: no device key, data stays.
    Detached,
    /// The data, its key and the record's content are gone; a tombstone stays.
    Deleted,
}

impl TenantState {
    pub fn as_str(self) -> &'static str {
        match self {
            TenantState::Pending => "pending",
            TenantState::Active => "active",
            TenantState::Held => "held",
            TenantState::Detached => "detached",
            TenantState::Deleted => "deleted",
        }
    }

    fn parse(text: &str) -> Result<Self, HostRefusal> {
        Ok(match text {
            "pending" => TenantState::Pending,
            "active" => TenantState::Active,
            "held" => TenantState::Held,
            "detached" => TenantState::Detached,
            "deleted" => TenantState::Deleted,
            other => {
                return Err(HostRefusal::internal(format!(
                    "unknown tenant state {other}"
                )))
            }
        })
    }
}

/// How the tenant came in, kept as the JSON of the record's `joined_via`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", tag = "kind")]
#[ts(export)]
pub enum JoinRoute {
    /// The setting was `Anyone`.
    Anyone,
    /// An invite admitted it.
    #[serde(rename_all = "camelCase")]
    Invite { invite_id: String },
    /// A `Detached` record reattached.
    #[serde(rename_all = "camelCase")]
    Rejoin { previous_device_did: String },
    /// A guest's "Keep this" on this host.
    GuestClaim,
}

impl JoinRoute {
    fn json(&self) -> String {
        serde_json::to_string(self).expect("a route is plain data")
    }

    fn of(row: &TenantRow) -> Result<Self, HostRefusal> {
        serde_json::from_str(&row.joined_via)
            .map_err(|e| HostRefusal::internal(format!("unreadable join route: {e}")))
    }

    /// The route's name in `tenant-joined`.
    pub fn name(&self) -> &'static str {
        match self {
            JoinRoute::Anyone => "anyone",
            JoinRoute::Invite { .. } => "invite",
            JoinRoute::Rejoin { .. } => "rejoin",
            JoinRoute::GuestClaim => "guest-claim",
        }
    }
}

/// `host.join`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct JoinRequest {
    pub did_scid: String,
    /// The `coasys:offer:1:...` string or link.
    pub offer: String,
    /// Base64url, from the invite link's fragment.
    pub invite_secret: Option<String>,
}

/// `host.join`'s answer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct JoinResult {
    pub join_handle: String,
    pub tenant_id: String,
    pub device_did: String,
    /// Kept data was reattached.
    pub rejoin: bool,
}

/// `host.joinStatus`'s answer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostJoinStatus {
    /// `pending`, `active`, `expired` or `failed`.
    pub state: String,
    pub reason: Option<String>,
}

/// What `admit_claimed_guest` decided (AR-05 4.5.7).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClaimAdmission {
    /// The host serves the person as a tenant from now on.
    Admitted { tenant_id: String },
    /// The setting or the capacity does not admit the person: the record is `Held`.
    HeldOnly {
        tenant_id: String,
        reason: HostRefusal,
    },
    /// No record made.
    Refused(HostRefusal),
}

/// A tenant as a record shows it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tenant {
    pub row: TenantRow,
    pub state: TenantState,
}

impl Tenant {
    pub fn of(row: TenantRow) -> Result<Self, HostRefusal> {
        let state = TenantState::parse(&row.state)?;
        Ok(Tenant { row, state })
    }
}

fn db_err(e: impl std::fmt::Display) -> HostRefusal {
    HostRefusal::internal(e.to_string())
}

/// The tenant record `id`, in any state.
pub fn tenant(id: &str) -> Result<Option<Tenant>, HostRefusal> {
    db_backend()
        .tenant(id)
        .map_err(db_err)?
        .map(Tenant::of)
        .transpose()
}

/// The live (not deleted) record of `did_scid`.
pub fn live_tenant_of(did_scid: &str) -> Result<Option<Tenant>, HostRefusal> {
    db_backend()
        .live_tenant_of(did_scid)
        .map_err(db_err)?
        .map(Tenant::of)
        .transpose()
}

/// The live record whose device key is `device_did`: the tenant a session of that managed
/// agent acts as.
pub fn live_tenant_of_device(device_did: &str) -> Result<Option<Tenant>, HostRefusal> {
    db_backend()
        .live_tenant_of_device(device_did)
        .map_err(db_err)?
        .map(Tenant::of)
        .transpose()
}

/// Whether a call by the managed agent `device_did` is admitted (AR-05 4.5.7, R64): a `Held`
/// tenant's session may only read, export (`data.export*`), see its usage and leave; every
/// other call answers `not-admitted`. `read` is whether the method is an idempotent read.
/// Every other agent, and a tenant in any other state, is admitted here.
pub fn admit_call(device_did: &str, method: &str, read: bool) -> Result<(), HostRefusal> {
    let may = read
        || method.starts_with("data.export")
        || method == "host.usage"
        || method == "host.leave";
    if may {
        return Ok(());
    }
    match live_tenant_of_device(device_did)? {
        Some(t) if t.state == TenantState::Held => Err(HostRefusal::NotAdmitted),
        _ => Ok(()),
    }
}

/// Every live record.
pub fn live_tenants() -> Result<Vec<Tenant>, HostRefusal> {
    db_backend()
        .live_tenants()
        .map_err(db_err)?
        .into_iter()
        .map(Tenant::of)
        .collect()
}

// ---- Rate limit -----------------------------------------------------------------------

/// A sliding window of calls per key.
pub struct RateLimiter {
    max: usize,
    window_secs: u64,
    hits: Mutex<HashMap<String, Vec<u64>>>,
}

impl RateLimiter {
    pub fn new(max: usize, window_secs: u64) -> Self {
        RateLimiter {
            max,
            window_secs,
            hits: Mutex::new(HashMap::new()),
        }
    }

    /// Counts a call of `key` at `now`; `rate-limited` with the seconds until the oldest call
    /// leaves the window, when `max` calls are in it.
    pub fn check(&self, key: &str, now: u64) -> Result<(), HostRefusal> {
        let mut hits = self.hits.lock().unwrap_or_else(|e| e.into_inner());
        let calls = hits.entry(key.to_string()).or_default();
        calls.retain(|at| at + self.window_secs > now);
        if calls.len() >= self.max {
            let oldest = calls.iter().copied().min().unwrap_or(now);
            return Err(HostRefusal::RateLimited {
                retry_after_secs: (oldest + self.window_secs).saturating_sub(now).max(1),
            });
        }
        calls.push(now);
        Ok(())
    }
}

/// The `host.join` limiter of this executor.
pub fn join_limiter() -> &'static RateLimiter {
    static LIMITER: std::sync::OnceLock<RateLimiter> = std::sync::OnceLock::new();
    LIMITER.get_or_init(|| RateLimiter::new(JOINS_PER_HOUR, 3600))
}

// ---- Invites --------------------------------------------------------------------------

fn secret_hash(secret: &str) -> Result<Vec<u8>, HostRefusal> {
    let bytes = URL_SAFE_NO_PAD
        .decode(secret.trim())
        .map_err(|_| HostRefusal::InviteInvalid)?;
    Ok(Sha256::digest(bytes).to_vec())
}

/// Makes an invite; answers its id and its secret (base64url, shown once).
pub fn create_invite(
    env: &dyn HostEnv,
    created_by: &str,
    expires_in_days: Option<u32>,
    note: Option<String>,
) -> Result<(String, String), HostRefusal> {
    let days = expires_in_days.unwrap_or(7);
    if !(1..=30).contains(&days) {
        return Err(HostRefusal::internal("expiresInDays is 1 to 30"));
    }
    let now = env.now();
    if db_backend().count_open_invites(now).map_err(db_err)? >= MAX_OPEN_INVITES {
        return Err(HostRefusal::internal(format!(
            "this host keeps at most {MAX_OPEN_INVITES} open invites"
        )));
    }
    let secret: [u8; 32] = rand::random();
    let id = format!("i-{}", uuid::Uuid::new_v4());
    db_backend()
        .insert_invite(&InviteRow {
            id: id.clone(),
            secret_hash: Sha256::digest(secret).to_vec(),
            created_by: created_by.to_string(),
            created_at: now,
            expires_at: now + u64::from(days) * 86_400,
            used_by: None,
            revoked: false,
            note,
        })
        .map_err(db_err)?;
    Ok((id, URL_SAFE_NO_PAD.encode(secret)))
}

/// Every invite, without secrets.
pub fn list_invites() -> Result<Vec<InviteRow>, HostRefusal> {
    db_backend().invites().map_err(db_err)
}

/// Revokes invite `id`; `false` when there is none.
pub fn revoke_invite(id: &str) -> Result<bool, HostRefusal> {
    db_backend().revoke_invite(id).map_err(db_err)
}

/// The invite `secret` names, when it is not used (other than by `reusable_by`), revoked or
/// expired.
fn open_invite(
    secret: &str,
    now: u64,
    reusable_by: Option<&str>,
) -> Result<InviteRow, HostRefusal> {
    let hash = secret_hash(secret)?;
    let invite = db_backend()
        .invite_by_hash(&hash)
        .map_err(db_err)?
        .ok_or(HostRefusal::InviteInvalid)?;
    let used = invite
        .used_by
        .as_deref()
        .is_some_and(|by| Some(by) != reusable_by);
    if used || invite.revoked || invite.expires_at <= now {
        return Err(HostRefusal::InviteInvalid);
    }
    Ok(invite)
}

// ---- The gate ---------------------------------------------------------------------------

/// Check 6: the sign-up setting. Answers the invite it needs, when it does.
fn setting_gate(
    signup: SignupPolicy,
    invite_secret: Option<&str>,
    now: u64,
    reusable_by: Option<&str>,
) -> Result<Option<InviteRow>, HostRefusal> {
    match signup {
        SignupPolicy::NoOne => Err(HostRefusal::SignupsClosed),
        SignupPolicy::Anyone => Ok(None),
        SignupPolicy::ByInvite => {
            let secret = invite_secret.ok_or(HostRefusal::InviteInvalid)?;
            open_invite(secret, now, reusable_by).map(Some)
        }
    }
}

/// Check 7: the cap on `Active` plus `Pending`; `freed` records leave the count first.
fn capacity_gate(max_tenants: Option<u32>, freed: u32) -> Result<(), HostRefusal> {
    let Some(cap) = max_tenants else {
        return Ok(());
    };
    let serving = db_backend().count_serving_tenants().map_err(db_err)?;
    if serving.saturating_sub(freed) + 1 > cap {
        return Err(HostRefusal::HostFull);
    }
    Ok(())
}

/// Checks 1 and 3: multi-user mode, and a key log at the store for `did_scid`.
async fn identity_gate(env: &dyn HostEnv, did_scid: &str) -> Result<super::LogFacts, HostRefusal> {
    if !env.multi_user() {
        return Err(HostRefusal::NotAHost);
    }
    if scid_of(did_scid).is_none_or(str::is_empty) {
        return Err(HostRefusal::UnknownIdentity);
    }
    env.log_facts(did_scid).await
}

/// Check 8: a vouching note beside the log.
fn email_gate(facts: super::LogFacts) -> Result<(), HostRefusal> {
    facts
        .vouched
        .then_some(())
        .ok_or(HostRefusal::EmailCheckRequired)
}

/// Whether the host takes no more tenants (`HostInfo.full`).
pub fn is_full() -> Result<bool, HostRefusal> {
    Ok(capacity_gate(policy::current()?.max_tenants, 0).is_err())
}

// ---- Join -------------------------------------------------------------------------------

/// Join handles by tenant id, held in memory: a handle reveals nothing but the state.
fn handles() -> &'static Mutex<HashMap<String, String>> {
    static HANDLES: std::sync::OnceLock<Mutex<HashMap<String, String>>> =
        std::sync::OnceLock::new();
    HANDLES.get_or_init(|| Mutex::new(HashMap::new()))
}

fn new_handle(tenant_id: &str) -> String {
    let handle = hex::encode(rand::random::<[u8; 32]>());
    handles()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(handle.clone(), tenant_id.to_string());
    handle
}

/// `host.joinStatus`: where the join of `handle` stands. An unknown handle is `expired`.
pub fn join_status(handle: &str) -> Result<HostJoinStatus, HostRefusal> {
    let id = handles()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .get(handle)
        .cloned();
    let state = |state: &str, reason: Option<&str>| HostJoinStatus {
        state: state.into(),
        reason: reason.map(str::to_string),
    };
    let Some(id) = id else {
        return Ok(state("expired", None));
    };
    Ok(match tenant(&id)? {
        None => state("expired", None),
        Some(t) => match t.state {
            TenantState::Pending => state("pending", None),
            TenantState::Active | TenantState::Held => state("active", None),
            TenantState::Detached => {
                state("failed", Some("the tenant left or its key was revoked"))
            }
            TenantState::Deleted => state("failed", Some("the tenant was removed")),
        },
    })
}

fn pending_row(
    row: &TenantRow,
    key: Option<(String, String)>,
    route: &JoinRoute,
    now: u64,
) -> TenantRow {
    TenantRow {
        device_key_name: key.as_ref().map(|k| k.0.clone()),
        device_did: key.map(|k| k.1),
        state: TenantState::Pending.as_str().into(),
        joined_via: route.json(),
        activated_at: None,
        detached_at: None,
        pending_expires_at: Some(now + PENDING_SECS),
        ..row.clone()
    }
}

/// Undoes a `Pending` record: its key, its person (a first join) and its invite. A record
/// that reattached kept data goes back to `Detached`; any other is removed.
async fn abandon(env: &dyn HostEnv, row: &TenantRow) -> Result<(), HostRefusal> {
    let route = JoinRoute::of(row)?;
    let rejoin = matches!(route, JoinRoute::Rejoin { .. });
    let device = row
        .device_did
        .as_deref()
        .zip(row.device_key_name.as_deref());
    if rejoin {
        if let Some((did, key)) = device {
            env.end_device(did, key).map_err(HostRefusal::internal)?;
        }
        let back = TenantRow {
            device_key_name: None,
            device_did: match &route {
                JoinRoute::Rejoin {
                    previous_device_did,
                } if !previous_device_did.is_empty() => Some(previous_device_did.clone()),
                _ => None,
            },
            state: TenantState::Detached.as_str().into(),
            pending_expires_at: None,
            detached_at: Some(env.now()),
            ..row.clone()
        };
        db_backend()
            .transition_tenant(&row.id, &["pending"], &back)
            .map_err(db_err)?;
    } else {
        env.undo_first_join(&row.id, device)
            .await
            .map_err(HostRefusal::internal)?;
        db_backend().remove_tenant_row(&row.id).map_err(db_err)?;
    }
    db_backend().unspend_invite(&row.id).map_err(db_err)?;
    handles()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .retain(|_, id| *id != row.id);
    Ok(())
}

fn activate_row(env: &dyn HostEnv, row: &TenantRow, from: &[&str]) -> Result<bool, HostRefusal> {
    let active = TenantRow {
        state: TenantState::Active.as_str().into(),
        activated_at: Some(env.now()),
        pending_expires_at: None,
        detached_at: None,
        ..row.clone()
    };
    db_backend()
        .transition_tenant(&row.id, from, &active)
        .map_err(db_err)
}

fn bind_and_announce(
    env: &dyn HostEnv,
    row: &TenantRow,
    route: &JoinRoute,
) -> Result<(), HostRefusal> {
    if let Some(device) = &row.device_did {
        env.bind_device(device, &row.did_scid, env.now())
            .map_err(HostRefusal::internal)?;
    }
    env.emit(HostEvent::TenantJoined {
        tenant_id: row.id.clone(),
        did_scid: row.did_scid.clone(),
        route: route.name().into(),
    });
    Ok(())
}

/// `host.join` (AR-05 4.5.3): the gate in order, then the record, the key, the person and the
/// claim. `peer` is the caller's network address.
pub async fn join(
    env: &dyn HostEnv,
    limiter: &RateLimiter,
    peer: &str,
    request: JoinRequest,
) -> Result<JoinResult, HostRefusal> {
    // 1 multi-user mode, 2 rate limit, 3 a key log at the store.
    if !env.multi_user() {
        return Err(HostRefusal::NotAHost);
    }
    limiter.check(peer, env.now())?;
    let facts = identity_gate(env, &request.did_scid).await?;
    let now = env.now();

    // 4 and 5: the record this identity has.
    let existing = live_tenant_of(&request.did_scid)?;
    if let Some(t) = &existing {
        if t.state == TenantState::Active {
            return Err(HostRefusal::AlreadyATenant);
        }
    }
    let state = existing.as_ref().map(|t| t.state);

    // 6 the setting, 7 capacity, 8 the email check.
    let policy = policy::current()?;
    let reusable_by = existing
        .as_ref()
        .filter(|t| t.state == TenantState::Pending)
        .map(|t| t.row.id.as_str());
    let invite = setting_gate(
        policy.signup,
        request.invite_secret.as_deref(),
        now,
        reusable_by,
    )?;
    let freed = u32::from(state == Some(TenantState::Pending));
    capacity_gate(policy.max_tenants, freed)?;
    email_gate(facts)?;

    let route = match &invite {
        Some(invite) => JoinRoute::Invite {
            invite_id: invite.id.clone(),
        },
        None => JoinRoute::Anyone,
    };

    // A held record turns active with its key; no offer is claimed.
    if let Some(held) = existing.as_ref().filter(|t| t.state == TenantState::Held) {
        if let Some(invite) = &invite {
            if !db_backend()
                .spend_invite(&invite.id, &held.row.id, now)
                .map_err(db_err)?
            {
                return Err(HostRefusal::InviteInvalid);
            }
        }
        if !activate_row(env, &held.row, &["held"])? {
            return Err(HostRefusal::AlreadyATenant);
        }
        let device_did = held.row.device_did.clone().unwrap_or_default();
        let active = tenant(&held.row.id)?
            .map(|t| t.row)
            .unwrap_or_else(|| held.row.clone());
        bind_and_announce(env, &active, &route)?;
        return Ok(JoinResult {
            join_handle: new_handle(&held.row.id),
            tenant_id: held.row.id.clone(),
            device_did,
            rejoin: false,
        });
    }

    // 9 the offer link.
    let offer = crate::identity::service::parse_offer(&request.offer)
        .map_err(|_| HostRefusal::OfferInvalid)?;
    check_offer_names(&offer, &request.did_scid)?;

    // A pending record is replaced: its key, person and invite go first.
    if let Some(pending) = existing
        .as_ref()
        .filter(|t| t.state == TenantState::Pending)
    {
        abandon(env, &pending.row).await?;
    }
    let existing = live_tenant_of(&request.did_scid)?;

    // The record, the invite spent with it.
    let (row, rejoin) = match existing {
        Some(detached) if detached.state == TenantState::Detached => {
            let route = JoinRoute::Rejoin {
                previous_device_did: detached.row.device_did.clone().unwrap_or_default(),
            };
            let next = pending_row(&detached.row, None, &route, now);
            if !db_backend()
                .transition_tenant(&detached.row.id, &["detached"], &next)
                .map_err(db_err)?
            {
                return Err(HostRefusal::AlreadyATenant);
            }
            (next, true)
        }
        Some(_) => return Err(HostRefusal::AlreadyATenant),
        None => {
            let id = format!("t-{}", uuid::Uuid::new_v4());
            let next = TenantRow {
                id,
                did_scid: request.did_scid.clone(),
                device_key_name: None,
                device_did: None,
                state: TenantState::Pending.as_str().into(),
                joined_via: route.json(),
                created_at: now,
                activated_at: None,
                detached_at: None,
                pending_expires_at: Some(now + PENDING_SECS),
            };
            if db_backend().insert_tenant(&next).map_err(db_err)? == TenantInsert::LiveExists {
                return Err(HostRefusal::AlreadyATenant);
            }
            (next, false)
        }
    };
    if let Some(invite) = &invite {
        if !db_backend()
            .spend_invite(&invite.id, &row.id, now)
            .map_err(db_err)?
        {
            abandon(env, &row).await?;
            return Err(HostRefusal::InviteInvalid);
        }
    }

    // The device key and, for a first join, the person; then the claim.
    let key = match env.make_device(&row.id, !rejoin).await {
        Ok(key) => key,
        Err(e) => {
            abandon(env, &row).await?;
            return Err(HostRefusal::internal(e));
        }
    };
    let keyed = TenantRow {
        device_key_name: Some(key.0.clone()),
        device_did: Some(key.1.clone()),
        ..row.clone()
    };
    if let Err(e) = db_backend().transition_tenant(&row.id, &["pending"], &keyed) {
        abandon(env, &keyed).await?;
        return Err(db_err(e));
    }
    let claimed = match env.claim(&key.1, &request.offer).await {
        Ok(claimed) => claimed,
        Err(refusal) => {
            abandon(env, &keyed).await?;
            return Err(refusal);
        }
    };
    if !claimed.is_node || claimed.expires <= env.now() {
        abandon(env, &keyed).await?;
        return Err(HostRefusal::OfferInvalid);
    }
    Ok(JoinResult {
        join_handle: new_handle(&row.id),
        tenant_id: row.id,
        device_did: key.1,
        rejoin,
    })
}

fn check_offer_names(offer: &OfferString, did_scid: &str) -> Result<(), HostRefusal> {
    if did_of(&offer.scid) == did_scid {
        Ok(())
    } else {
        Err(HostRefusal::OfferInvalid)
    }
}

// ---- Activation, expiry, revocation ------------------------------------------------------

/// What one pass of [`sweep`] did.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SweepReport {
    pub activated: Vec<String>,
    pub expired: Vec<String>,
    pub detached: Vec<String>,
}

/// One pass over the records (AR-05 4.5.5, 4.5.6): a `Pending` tenant whose key holds
/// validity becomes `Active`, one past its time is removed with its key, person and invite,
/// and an `Active` or `Held` tenant whose key is no longer valid becomes `Detached`.
pub async fn sweep(env: &dyn HostEnv) -> Result<SweepReport, HostRefusal> {
    sweep_with(env, true).await
}

/// The pass that only settles `Pending` tenants (the 5 s poll): it asks the store about no
/// `Active` or `Held` tenant's key.
pub async fn sweep_pending(env: &dyn HostEnv) -> Result<SweepReport, HostRefusal> {
    sweep_with(env, false).await
}

async fn sweep_with(env: &dyn HostEnv, check_active: bool) -> Result<SweepReport, HostRefusal> {
    let mut report = SweepReport::default();
    let now = env.now();
    for t in live_tenants()? {
        match t.state {
            TenantState::Pending => {
                let Some(device) = t.row.device_did.clone() else {
                    continue;
                };
                if env.key_standing(&t.row.did_scid, &device).await == KeyStanding::Valid {
                    if activate_row(env, &t.row, &["pending"])? {
                        let route = JoinRoute::of(&t.row)?;
                        bind_and_announce(env, &t.row, &route)?;
                        report.activated.push(t.row.id.clone());
                    }
                } else if t.row.pending_expires_at.is_some_and(|at| at <= now) {
                    abandon(env, &t.row).await?;
                    report.expired.push(t.row.id.clone());
                }
            }
            TenantState::Active | TenantState::Held if check_active => {
                let Some(device) = t.row.device_did.clone() else {
                    continue;
                };
                if env.key_standing(&t.row.did_scid, &device).await == KeyStanding::Gone {
                    detach(env, &t.row, "revoked").await?;
                    report.detached.push(t.row.id.clone());
                }
            }
            TenantState::Active
            | TenantState::Held
            | TenantState::Detached
            | TenantState::Deleted => {}
        }
    }
    Ok(report)
}

/// The tenant's device key is gone: end its sessions and key, keep its data, `Detached`.
async fn detach(env: &dyn HostEnv, row: &TenantRow, cause: &str) -> Result<(), HostRefusal> {
    if let (Some(did), Some(key)) = (&row.device_did, &row.device_key_name) {
        env.end_device(did, key).map_err(HostRefusal::internal)?;
    }
    let detached = TenantRow {
        device_key_name: None,
        state: TenantState::Detached.as_str().into(),
        detached_at: Some(env.now()),
        ..row.clone()
    };
    if db_backend()
        .transition_tenant(&row.id, &["active", "held"], &detached)
        .map_err(db_err)?
    {
        env.emit(HostEvent::TenantLeft {
            tenant_id: row.id.clone(),
            did_scid: row.did_scid.clone(),
            kept: true,
            cause: cause.into(),
        });
    }
    Ok(())
}

// ---- Leave and removal -------------------------------------------------------------------

/// Tenants whose leave runs: they take no new writes (AR-05 4.9.2 step 1).
fn leaving() -> &'static Mutex<HashSet<String>> {
    static LEAVING: std::sync::OnceLock<Mutex<HashSet<String>>> = std::sync::OnceLock::new();
    LEAVING.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Whether `tenant_id`'s leave is running.
pub fn is_leaving(tenant_id: &str) -> bool {
    leaving()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .contains(tenant_id)
}

fn did_hash(did_scid: &str) -> String {
    hex::encode(Sha256::digest(did_scid.as_bytes()))
}

/// `host.leave` (AR-05 4.9.2): the tenant takes no new writes, its sessions and key end,
/// then its data stays (`Detached`) or goes with its data key (`Deleted`).
pub async fn leave(
    env: &dyn HostEnv,
    tenant_id: &str,
    keep_data: bool,
) -> Result<LeaveResult, HostRefusal> {
    let t = tenant(tenant_id)?.ok_or(HostRefusal::NotATenant)?;
    if !matches!(t.state, TenantState::Active | TenantState::Held) {
        return Err(HostRefusal::NotATenant);
    }
    leaving()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(tenant_id.to_string());
    let done = finish_leave(env, &t, keep_data).await;
    leaving()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .remove(tenant_id);
    done
}

async fn finish_leave(
    env: &dyn HostEnv,
    t: &Tenant,
    keep_data: bool,
) -> Result<LeaveResult, HostRefusal> {
    if let (Some(did), Some(key)) = (&t.row.device_did, &t.row.device_key_name) {
        env.end_device(did, key).map_err(HostRefusal::internal)?;
    }
    let detached = TenantRow {
        device_key_name: None,
        state: TenantState::Detached.as_str().into(),
        detached_at: Some(env.now()),
        ..t.row.clone()
    };
    db_backend()
        .transition_tenant(&t.row.id, &["active", "held"], &detached)
        .map_err(db_err)?;
    let result = env
        .leave_person(&t.row.id, keep_data)
        .await
        .map_err(HostRefusal::internal)?;
    if !keep_data {
        tombstone(&t.row, env.now())?;
    }
    env.emit(HostEvent::TenantLeft {
        tenant_id: t.row.id.clone(),
        did_scid: t.row.did_scid.clone(),
        kept: keep_data,
        cause: "leave".into(),
    });
    Ok(result)
}

fn tombstone(row: &TenantRow, now: u64) -> Result<(), HostRefusal> {
    db_backend().delete_ai_runs(&row.id).map_err(db_err)?;
    db_backend()
        .tombstone_tenant(&row.id, &did_hash(&row.did_scid), now)
        .map_err(db_err)?;
    Ok(())
}

/// `host.removeTenant` (AR-05 4.13.1): deletes a `Detached` tenant's data. `false` when there
/// is no such record or it is deleted already; an `Active`, `Held` or `Pending` one is
/// refused: the person leaves or revokes the key first.
pub async fn remove_tenant(env: &dyn HostEnv, tenant_id: &str) -> Result<bool, HostRefusal> {
    let Some(t) = tenant(tenant_id)? else {
        return Ok(false);
    };
    match t.state {
        TenantState::Deleted => Ok(false),
        TenantState::Detached => {
            env.leave_person(&t.row.id, false)
                .await
                .map_err(HostRefusal::internal)?;
            tombstone(&t.row, env.now())?;
            env.emit(HostEvent::TenantLeft {
                tenant_id: t.row.id.clone(),
                did_scid: t.row.did_scid.clone(),
                kept: false,
                cause: "removed".into(),
            });
            Ok(true)
        }
        _ => Err(HostRefusal::AlreadyATenant),
    }
}

// ---- A claimed guest ----------------------------------------------------------------------

/// The gate for a guest who kept their work (AR-05 4.5.7): checks 1, 3, 5, 8, then 6 (only
/// `Anyone` admits: a claim carries no invite) and 7. Called by the guest claim inside the
/// executor, not an RPC method. The record keeps the guest's `users` row (`username` is its
/// id), wallet key and data key.
pub async fn admit_claimed_guest(
    env: &dyn HostEnv,
    username: &str,
    did_scid: &str,
) -> ClaimAdmission {
    let facts = match identity_gate(env, did_scid).await {
        Ok(facts) => facts,
        Err(refusal) => return ClaimAdmission::Refused(refusal),
    };
    match live_tenant_of(did_scid) {
        Ok(None) => {}
        Ok(Some(_)) => return ClaimAdmission::Refused(HostRefusal::AlreadyATenant),
        Err(e) => return ClaimAdmission::Refused(e),
    }
    if let Err(refusal) = email_gate(facts) {
        return ClaimAdmission::Refused(refusal);
    }
    let (key_name, device_did) = match env.guest_device(username) {
        Ok(device) => device,
        Err(e) => return ClaimAdmission::Refused(HostRefusal::internal(e)),
    };
    let held_reason = match policy::current() {
        Err(e) => return ClaimAdmission::Refused(e),
        Ok(policy) => {
            if policy.signup != SignupPolicy::Anyone {
                Some(HostRefusal::SignupsClosed)
            } else {
                capacity_gate(policy.max_tenants, 0).err()
            }
        }
    };
    let now = env.now();
    let state = if held_reason.is_some() {
        TenantState::Held
    } else {
        TenantState::Active
    };
    let row = TenantRow {
        id: username.to_string(),
        did_scid: did_scid.to_string(),
        device_key_name: Some(key_name),
        device_did: Some(device_did),
        state: state.as_str().into(),
        joined_via: JoinRoute::GuestClaim.json(),
        created_at: now,
        activated_at: (state == TenantState::Active).then_some(now),
        detached_at: None,
        pending_expires_at: None,
    };
    match db_backend().insert_tenant(&row) {
        Ok(TenantInsert::Inserted) => {}
        Ok(TenantInsert::LiveExists) => {
            return ClaimAdmission::Refused(HostRefusal::AlreadyATenant)
        }
        Err(e) => return ClaimAdmission::Refused(db_err(e)),
    }
    if let Some(device) = &row.device_did {
        if let Err(e) = env.bind_device(device, did_scid, now) {
            return ClaimAdmission::Refused(HostRefusal::internal(e));
        }
    }
    match held_reason {
        None => {
            env.emit(HostEvent::TenantJoined {
                tenant_id: row.id.clone(),
                did_scid: did_scid.to_string(),
                route: JoinRoute::GuestClaim.name().into(),
            });
            ClaimAdmission::Admitted { tenant_id: row.id }
        }
        Some(reason) => ClaimAdmission::HeldOnly {
            tenant_id: row.id,
            reason,
        },
    }
}

#[cfg(test)]
mod tests;
