//! An executor as a host (AR-05): tenants, the sign-up setting, invites, the join gate, the
//! tenant's usage and its leave.
//!
//! | File | What |
//! |---|---|
//! | `export.rs` | `ExportFile`, `export_owned` and `import_into`: links move with their proofs as stored |
//! | `live.rs` | `LiveEnv`: the real wallet, users table, person store and identity service |
//! | `policy.rs` | `HostPolicy` (sign-up setting, cap, label, pinned fields) in the `settings` row `host_policy`; `seed_from_config` |
//! | `tenants.rs` | `TenantRecord`, its states, the gate and `join`, activation, expiry and the revocation sweep, `leave`, `remove_tenant`, `admit_claimed_guest`, invites |
//!
//! Every effect outside the tenants table goes through [`HostEnv`], so the gate and the
//! states run in unit tests without a wallet, a keyring or a store.

pub mod export;
pub mod live;
pub mod policy;
pub mod tenants;

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::api::ws_handler::WsRpcError;

/// Why a call on the host was refused (AR-05 4.13.3). The `code` is the first word of the
/// message, as the other refusals of the executor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostRefusal {
    NotAHost,
    SignupsClosed,
    InviteInvalid,
    EmailCheckRequired,
    AlreadyATenant,
    HostFull,
    UnknownIdentity,
    OfferInvalid,
    RateLimited { retry_after_secs: u64 },
    StoreUnreachable,
    PinnedByConfig,
    NotAdmitted,
    NotATenant,
    Internal(String),
}

impl HostRefusal {
    pub fn internal(text: impl Into<String>) -> Self {
        HostRefusal::Internal(text.into())
    }

    /// The fixed code of the refusal.
    pub fn code(&self) -> &'static str {
        match self {
            HostRefusal::NotAHost => "not-a-host",
            HostRefusal::SignupsClosed => "signups-closed",
            HostRefusal::InviteInvalid => "invite-invalid",
            HostRefusal::EmailCheckRequired => "email-check-required",
            HostRefusal::AlreadyATenant => "already-a-tenant",
            HostRefusal::HostFull => "host-full",
            HostRefusal::UnknownIdentity => "unknown-identity",
            HostRefusal::OfferInvalid => "offer-invalid",
            HostRefusal::RateLimited { .. } => "rate-limited",
            HostRefusal::StoreUnreachable => "store-unreachable",
            HostRefusal::PinnedByConfig => "pinned-by-config",
            HostRefusal::NotAdmitted => "not-admitted",
            HostRefusal::NotATenant => "not-a-tenant",
            HostRefusal::Internal(_) => "internal",
        }
    }

    /// The HTTP-equivalent status.
    pub fn status(&self) -> u16 {
        match self {
            HostRefusal::NotAHost | HostRefusal::UnknownIdentity => 404,
            HostRefusal::SignupsClosed
            | HostRefusal::InviteInvalid
            | HostRefusal::EmailCheckRequired
            | HostRefusal::NotAdmitted
            | HostRefusal::NotATenant => 403,
            HostRefusal::AlreadyATenant | HostRefusal::PinnedByConfig => 409,
            HostRefusal::HostFull | HostRefusal::StoreUnreachable => 503,
            HostRefusal::OfferInvalid => 400,
            HostRefusal::RateLimited { .. } => 429,
            HostRefusal::Internal(_) => 500,
        }
    }
}

impl std::fmt::Display for HostRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            HostRefusal::NotAHost => "multi-user mode is off",
            HostRefusal::SignupsClosed => "the host's sign-up setting is No one",
            HostRefusal::InviteInvalid => "no invite, or it is used, revoked, expired or unknown",
            HostRefusal::EmailCheckRequired => "no vouching note sits beside the identity's log",
            HostRefusal::AlreadyATenant => "a live record exists for this identity",
            HostRefusal::HostFull => "the host has reached its tenant cap",
            HostRefusal::UnknownIdentity => "the store has no key log for this identity",
            HostRefusal::OfferInvalid => {
                "the offer link fails to parse, is expired, or names another identity"
            }
            HostRefusal::RateLimited { .. } => "too many requests",
            HostRefusal::StoreUnreachable => "the shared store or the key index does not answer",
            HostRefusal::PinnedByConfig => "the config file fixes this field",
            HostRefusal::NotAdmitted => "this tenant is held: only reads, exports, usage and leave",
            HostRefusal::NotATenant => "the session is the operator or the main agent",
            HostRefusal::Internal(text) => text,
        };
        write!(f, "{}: {text}", self.code())
    }
}

impl From<HostRefusal> for WsRpcError {
    fn from(refusal: HostRefusal) -> Self {
        let data = match &refusal {
            HostRefusal::RateLimited { retry_after_secs } => {
                Some(serde_json::json!({ "retryAfter": retry_after_secs }))
            }
            _ => None,
        };
        WsRpcError {
            code: refusal.status(),
            message: refusal.to_string(),
            data,
        }
    }
}

/// What the store says of an identity's log, as the gate needs it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LogFacts {
    /// A valid vouching note sits beside the log (`LogPage.vouched`).
    pub vouched: bool,
}

/// What the store's claim answer says of the offer (C8).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ClaimFacts {
    /// The stored offer's kind is `node`.
    pub is_node: bool,
    /// When the stored offer expires, unix seconds.
    pub expires: u64,
}

/// What the person's key log says of a tenant's device key.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyStanding {
    /// Valid at the log's head, no bot mark.
    Valid,
    /// Not valid: revoked, never listed, or a bot key.
    Gone,
    /// The log cannot be read now: no verdict.
    Unknown,
}

/// A host event as the tenants logic raises it; [`HostEvent::publish`] sends it to the
/// operator.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostEvent {
    TenantJoined {
        tenant_id: String,
        did_scid: String,
        route: String,
    },
    TenantLeft {
        tenant_id: String,
        did_scid: String,
        kept: bool,
        /// `leave`, `revoked` or `removed`.
        cause: String,
    },
}

/// `tenant-joined`: a record became `Active`. To the operator.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantJoinedEvent {
    pub tenant_id: String,
    pub did_scid: String,
    pub route: String,
}

/// `tenant-left`: a record became `Detached` or `Deleted`. To the operator.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantLeftEvent {
    pub tenant_id: String,
    pub did_scid: String,
    pub kept: bool,
    /// `leave`, `revoked` or `removed`.
    pub cause: String,
}

/// `tenant-usage-changed`: a tenant's usage moved. To the tenant's agent.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantUsageChangedEvent {
    pub tenant_id: String,
}

impl HostEvent {
    /// Publishes the event to the operator's sockets.
    pub fn publish(self) {
        use crate::events::{Audience, Event, Topic};
        let pubsub = crate::pubsub::get_global_pubsub_sync();
        match self {
            HostEvent::TenantJoined {
                tenant_id,
                did_scid,
                route,
            } => pubsub.publish_event(Event::new(
                Topic::TenantJoined,
                Audience::Operator,
                TenantJoinedEvent {
                    tenant_id,
                    did_scid,
                    route,
                },
            )),
            HostEvent::TenantLeft {
                tenant_id,
                did_scid,
                kept,
                cause,
            } => pubsub.publish_event(Event::new(
                Topic::TenantLeft,
                Audience::Operator,
                TenantLeftEvent {
                    tenant_id,
                    did_scid,
                    kept,
                    cause,
                },
            )),
        }
    }
}

/// Everything the host does outside its own tables.
#[async_trait]
pub trait HostEnv: Send + Sync {
    /// Unix seconds.
    fn now(&self) -> u64;
    /// Whether multi-user mode is on.
    fn multi_user(&self) -> bool;
    /// The store's log of `did_scid`: `unknown-identity` for none, `store-unreachable` when
    /// it does not answer.
    async fn log_facts(&self, did_scid: &str) -> Result<LogFacts, HostRefusal>;
    /// Makes the tenant's device key (wallet key, saved) and, for a first join, its legacy
    /// `users` row and its person (data key). Answers `(key name, did:key)`.
    async fn make_device(
        &self,
        tenant_id: &str,
        first_join: bool,
    ) -> Result<(String, String), String>;
    /// Claims `offer` for the tenant's device key (`ClaimService::claim_for_tenant`).
    async fn claim(&self, device_did: &str, offer: &str) -> Result<ClaimFacts, HostRefusal>;
    /// Whether the person's log holds `device_did` valid.
    async fn key_standing(&self, did_scid: &str, device_did: &str) -> KeyStanding;
    /// Records that the agent `device_did` speaks for `did_scid` (key login finds it there).
    fn bind_device(&self, device_did: &str, did_scid: &str, now: u64) -> Result<(), String>;
    /// Ends the tenant's sessions and its binding, then destroys its wallet key (saved).
    fn end_device(&self, device_did: &str, key_name: &str) -> Result<(), String>;
    /// Removes the key (`(did, key name)`, when it was made) and the person of a first join
    /// that did not complete, and the `users` row, with nothing else of the tenant's.
    async fn undo_first_join(
        &self,
        tenant_id: &str,
        device: Option<(&str, &str)>,
    ) -> Result<(), String>;
    /// The wallet key (`(key name, did:key)`) of the guest's managed user `username`, which a
    /// claimed guest keeps as its device key.
    fn guest_device(&self, username: &str) -> Result<(String, String), String>;
    /// The tenant's data, kept or deleted (the person's leave of AR-06).
    async fn leave_person(&self, tenant_id: &str, keep_data: bool) -> Result<LeaveResult, String>;
    fn emit(&self, event: HostEvent);
}

/// What `host.leave` did (AR-05 4.13.1).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LeaveResult {
    pub kept: bool,
    pub removed_perspectives: u32,
    pub left_shared_perspectives: u32,
    /// Delete only: when the tenant's data key was destroyed.
    #[ts(type = "string | null")]
    pub key_destroyed_at: Option<chrono::DateTime<chrono::Utc>>,
    /// Delete only: equal to `key_destroyed_at`, as the closed beta keeps no backups.
    #[ts(type = "string | null")]
    pub shredded_everywhere_at: Option<chrono::DateTime<chrono::Utc>>,
}

impl From<crate::at_rest::persons::PersonLeave> for LeaveResult {
    fn from(leave: crate::at_rest::persons::PersonLeave) -> Self {
        LeaveResult {
            kept: leave.kept,
            removed_perspectives: leave.removed_perspectives,
            left_shared_perspectives: leave.left_shared_perspectives,
            key_destroyed_at: leave.key_destroyed_at,
            shredded_everywhere_at: leave.shredded_everywhere_at,
        }
    }
}
