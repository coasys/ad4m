//! Which spaces a revoked device key reached (AR-08 section 4.11.3, slice 3.B part A9),
//! read by `agent.revocationReach`.
//!
//! - When the key cache learns that a device key is revoked ([`VerdictChange`] with
//!   [`KeyVerdict::Revoked`]), [`record_revocation`] writes one row per space its person holds
//!   on this executor: a neighbourhood that an agent bound to the person owns and, in a
//!   governed space, of which the person is a member.
//! - A space's link language reports when its server dropped a key or its room key rotated
//!   past one (`roomKeyEvent`); [`note_room_key_event`] puts the time on the key's row. A
//!   space whose link language never reports (Holochain, or a legacy space) keeps both times
//!   empty: the UI shows "not told".
//! - Rows live [`KEEP_MS`] (30 days): a read leaves older rows out and [`run`] prunes them at
//!   start and daily.

use std::time::Duration;

use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};
use tokio::sync::broadcast;
use ts_rs::TS;

use crate::db::ReachMark;
use crate::db_backend::db_backend;
use crate::identity::cache::global_key_map;
use crate::identity::verdict::{KeyVerdict, VerdictChange};
use crate::types::PerspectiveHandle;

use super::{RoomKeyEvent, RoomKeyEventKind};

/// How long a row lives: 30 days, in ms.
pub const KEEP_MS: u64 = 30 * 24 * 60 * 60 * 1000;

/// One space a revoked key reached (`agent.revocationReach`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RevocationReach {
    /// The revoked key, `did:key:<multikey>`.
    pub did_key: String,
    pub perspective_uuid: String,
    /// The space's name when the revocation was recorded.
    pub name: String,
    /// When the space's server closed the key's connection (ISO 8601); none until its link
    /// language says so.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub dropped_at: Option<String>,
    /// When the space's room key rotated past the key (ISO 8601); none until its link
    /// language says so.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub rotated_at: Option<String>,
}

/// The spaces the person `scid` (bare) holds here: `(perspective uuid, name)`, by uuid.
///
/// A space is a neighbourhood (a perspective with a neighbourhood or a shared URL) whose
/// events reach an agent bound to the person (its owners, or the main agent for an unowned
/// one), and, when the space is governed, of which the person is a member.
pub async fn held_spaces(scid: &str) -> Vec<(String, String)> {
    let agents = match db_backend().agents_of_identity(scid) {
        Ok(agents) => agents,
        Err(e) => {
            log::warn!("revocation reach: could not read the agents of {scid}: {e}");
            return Vec::new();
        }
    };
    if agents.is_empty() {
        return Vec::new();
    }
    let person = ad4m_identity_core::did_of(scid);
    let mut held = Vec::new();
    for perspective in crate::perspectives::all_perspectives() {
        let handle = perspective.persisted.lock().await.clone();
        if handle.neighbourhood.is_none() && handle.shared_url.is_none() {
            continue;
        }
        let reaches_person = crate::perspectives::event_recipients(&handle)
            .iter()
            .any(|did| agents.contains(did));
        if !reaches_person {
            continue;
        }
        let governed = perspective
            .sparql_store
            .membership()
            .graph()
            .filter(|graph| graph.rules().is_some());
        if governed.is_some_and(|graph| !graph.is_member(&person)) {
            continue;
        }
        let name = space_name(&handle);
        held.push((handle.uuid, name));
    }
    held.sort();
    held
}

/// What a row calls a space: its name, else its URL, else its uuid.
fn space_name(handle: &PerspectiveHandle) -> String {
    handle
        .name
        .clone()
        .filter(|name| !name.is_empty())
        .or_else(|| handle.shared_url.clone())
        .unwrap_or_else(|| handle.uuid.clone())
}

/// `did:key:<multikey>` for a multikey or a `did:key`.
fn did_key_of(key: &str) -> String {
    if key.starts_with("did:key:") {
        key.to_string()
    } else {
        format!("did:key:{key}")
    }
}

/// Unix ms as ISO 8601 with ms, UTC.
fn iso(ms: u64) -> String {
    chrono::DateTime::from_timestamp_millis(i64::try_from(ms).unwrap_or(i64::MAX))
        .unwrap_or_default()
        .to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
}

fn now_ms() -> u64 {
    u64::try_from(chrono::Utc::now().timestamp_millis()).unwrap_or(0)
}

/// Records the spaces a revoked device key reached; `Ok(false)` for any other change, and for
/// a key recorded before.
///
/// Only a standing device key of the person counts: a master never signs for the person
/// (it reaches no space), and a bot key is the bot's, not the person's device.
pub async fn record_revocation(change: &VerdictChange, now: u64) -> Result<bool, AnyError> {
    if change.verdict != KeyVerdict::Revoked {
        return Ok(false);
    }
    let Some(log) = global_key_map().log(&change.scid) else {
        return Ok(false);
    };
    let standing_device = log
        .state
        .keys
        .values()
        .any(|record| record.sign == change.key && record.scopes.is_empty());
    if !standing_device {
        return Ok(false);
    }
    let spaces = held_spaces(&change.scid).await;
    db_backend().add_revocation_reach(&did_key_of(&change.key), &spaces, now)
}

/// Puts the time of a space's room key change on the rows of the keys it shut out.
pub fn note_room_key_event(handle: &PerspectiveHandle, event: &RoomKeyEvent, now: u64) {
    let mark = match event.kind {
        RoomKeyEventKind::Dropped => ReachMark::Dropped,
        RoomKeyEventKind::Rotated => ReachMark::Rotated,
    };
    let name = space_name(handle);
    for key in &event.removed_keys {
        if let Err(e) =
            db_backend().mark_revocation_reach(&did_key_of(key), &handle.uuid, &name, mark, now)
        {
            log::warn!(
                "revocation reach: could not note the room key event of {} for {key}: {e}",
                handle.uuid
            );
        }
    }
}

/// The spaces `did_key` reached, recorded within [`KEEP_MS`] of `now`.
pub fn reach_of(did_key: &str, now: u64) -> Result<Vec<RevocationReach>, AnyError> {
    let rows = db_backend().revocation_reach(did_key, now.saturating_sub(KEEP_MS))?;
    Ok(rows
        .into_iter()
        .map(|row| RevocationReach {
            did_key: row.did_key,
            perspective_uuid: row.perspective_uuid,
            name: row.name,
            dropped_at: row.dropped_at.map(iso),
            rotated_at: row.rotated_at.map(iso),
        })
        .collect())
}

/// Deletes the rows older than [`KEEP_MS`] at `now`; the count deleted.
pub fn prune(now: u64) -> Result<usize, AnyError> {
    db_backend().prune_revocation_reach(now.saturating_sub(KEEP_MS))
}

fn prune_logged(now: u64) {
    match prune(now) {
        Ok(0) => {}
        Ok(n) => log::debug!("revocation reach: pruned {n} rows"),
        Err(e) => log::warn!("revocation reach: the prune failed: {e}"),
    }
}

/// Records each revocation the key cache reports (`changes`, subscribed before the cache
/// fires) once the perspectives are open, and prunes at start and daily.
pub async fn run(mut changes: broadcast::Receiver<VerdictChange>) {
    use broadcast::error::RecvError;
    crate::perspectives::opened().await;
    let mut daily = tokio::time::interval(Duration::from_secs(24 * 60 * 60));
    daily.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
    loop {
        tokio::select! {
            change = changes.recv() => match change {
                Ok(change) => {
                    if let Err(e) = record_revocation(&change, now_ms()).await {
                        log::warn!("revocation reach: could not record {}: {e}", change.key);
                    }
                }
                Err(RecvError::Lagged(missed)) => {
                    log::warn!("revocation reach: missed {missed} key changes");
                }
                Err(RecvError::Closed) => return,
            },
            // The first tick is at once: the prune at start.
            _ = daily.tick() => prune_logged(now_ms()),
        }
    }
}
