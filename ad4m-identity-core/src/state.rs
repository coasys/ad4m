//! The replayed state of one log (section 6 of `docs-src/identity/did-scid-coasys-1.md`). The
//! head and the hash at each seq live in `KelChain`, not here.

use ad4m_hash_chain::{Hash, Multikey};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::event::{BotScope, Kind};
use crate::KeyId;

/// The replayed state of one log.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct KeyState {
    pub did: String,
    pub scid: String,
    pub kind: Kind,
    /// m0, m1, …: each valid from the `incept` or `rotate` that named it.
    pub masters: Vec<KeyRecord>,
    /// The commitment to the next master key; `None` once ended.
    pub next: Option<Hash>,
    /// Every device key ever added, bot keys included.
    pub keys: BTreeMap<KeyId, KeyRecord>,
    pub ended: bool,
}

/// One key of a log: a master (`m<n>`, no `enc`) or a device key (`d<n>`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct KeyRecord {
    pub id: KeyId,
    pub sign: Multikey,
    /// Masters carry no encryption key.
    pub enc: Option<Multikey>,
    /// The seq that added it.
    pub added: u64,
    /// The seq that revoked it, replaced it (a master at a `rotate`) or ended the log.
    pub revoked: Option<u64>,
    /// The `expires` of a key added with `KeyLife::Until`.
    pub expires: Option<u64>,
    /// A bot key: `(seq, scope)` per `add` or `scope`, in seq order. Empty for every other key.
    pub scopes: Vec<(u64, BotScope)>,
}

impl KeyRecord {
    fn valid_at(&self, seq: u64) -> bool {
        self.added <= seq && self.revoked.is_none_or(|r| seq < r)
    }
}

impl KeyState {
    /// The record of `key`, a key id (`m0`, `d3`) or a signing multikey.
    pub fn record(&self, key: &str) -> Option<&KeyRecord> {
        if let Some(record) = self.keys.get(key) {
            return Some(record);
        }
        if let Some(record) = self.masters.iter().find(|m| m.id == key) {
            return Some(record);
        }
        self.masters
            .iter()
            .chain(self.keys.values())
            .find(|r| r.sign == key)
    }

    /// True when `key` (a key id or a signing multikey) was added at or before `seq`
    /// and not revoked or replaced at or before `seq`. It reads no expiry.
    pub fn key_valid_at(&self, key: &str, seq: u64) -> bool {
        self.record(key).is_some_and(|r| r.valid_at(seq))
    }

    /// `key_valid_at`, and `now` (the caller's trusted time, unix seconds) before the `expires`
    /// of a `KeyLife::Until` key or of the bot scope in force at `seq`.
    pub fn key_valid_at_time(&self, key: &str, seq: u64, now: u64) -> bool {
        let Some(record) = self.record(key) else {
            return false;
        };
        if !record.valid_at(seq) {
            return false;
        }
        if let Some(expires) = record.expires {
            if now >= expires {
                return false;
            }
        }
        if !record.scopes.is_empty() {
            return scope_at(record, seq).is_some_and(|s| now < s.expires);
        }
        true
    }

    /// The bot scope of `key` in force at `seq`: the latest `add` or `scope` at or before it.
    /// `None` for every key without the bot mark.
    pub fn bot_scope_at(&self, key: &str, seq: u64) -> Option<&BotScope> {
        scope_at(self.record(key)?, seq)
    }

    /// The master key in force at the head; `None` once the log has ended.
    pub fn current_master(&self) -> Option<&KeyRecord> {
        self.masters.last().filter(|m| m.revoked.is_none())
    }

    /// The key id of a signing multikey, master or device.
    pub fn key_id_of(&self, sign: &Multikey) -> Option<&KeyId> {
        self.masters
            .iter()
            .chain(self.keys.values())
            .find(|r| &r.sign == sign)
            .map(|r| &r.id)
    }

    /// The id the next added device key must carry (`d1`, `d2`, …; spec rule K7).
    pub fn next_device_id(&self) -> KeyId {
        format!("d{}", self.keys.len() + 1)
    }

    /// The id the master revealed by the next `rotate` takes (`m1`, `m2`, …).
    pub fn next_master_id(&self) -> KeyId {
        format!("m{}", self.masters.len())
    }

    /// The device keys valid at `seq`, with their encryption keys.
    pub fn encryption_keys_at(&self, seq: u64) -> Vec<(&KeyId, &Multikey)> {
        self.keys
            .values()
            .filter(|r| r.valid_at(seq))
            .filter_map(|r| r.enc.as_ref().map(|enc| (&r.id, enc)))
            .collect()
    }
}

fn scope_at(record: &KeyRecord, seq: u64) -> Option<&BotScope> {
    record
        .scopes
        .iter()
        .rev()
        .find(|(at, _)| *at <= seq)
        .map(|(_, s)| s)
}
