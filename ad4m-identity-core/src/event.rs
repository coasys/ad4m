//! The five events of a key log (AR-01 4.3.1, 4.3.2) and their strict JSON form.
//!
//! An event is parsed only through [`Event::from_json`] (the serde `Deserialize` impl calls it),
//! which refuses unknown ops, unknown fields, floats, integers at or above 2^53, `kind` other
//! than `human` and `bot`, and any JSON that would not serialise back to the same value. The
//! last rule keeps the event hash a receiver computes equal to the hash of the bytes it got.

use ad4m_hash_chain::{canonical_json, CanonError, Entry, Hash, Multikey, Proof};
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{Map, Value};

use crate::policy::{FoldError, FormatError};
use crate::KeyId;

/// One key-log event. Its fields do not change (AR-01 4.3.2).
#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub format: String,
    pub scid: String,
    pub seq: u64,
    pub prev: Option<Hash>,
    pub op: Op,
    /// Data Integrity proofs; outside the event hash. Serialised only when not empty.
    pub proof: Vec<Proof>,
}

/// The operation of an event, serialised flat into the event with its tag in `op`.
#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Incept {
        kind: Kind,
        master: Multikey,
        next: Hash,
        add: Vec<DeviceKey>,
    },
    /// Exactly one key. `co_sig` is the new key's claim co-signature (AR-02); `fold` does not read it.
    Add {
        add: Vec<DeviceKey>,
        offer: String,
        co_sig: String,
    },
    Revoke {
        revoke: Vec<KeyId>,
    },
    /// `revoke` is left out of the JSON when empty; an explicit `"revoke": []` is refused.
    Rotate {
        master: Multikey,
        salt: String,
        next: Option<Hash>,
        revoke: Vec<KeyId>,
    },
    /// A bot key only.
    Scope {
        key: KeyId,
        can: Vec<String>,
        expires: u64,
    },
}

impl Op {
    /// The wire name of the operation (`"incept"`, `"add"`, …).
    pub fn name(&self) -> &'static str {
        match self {
            Op::Incept { .. } => "incept",
            Op::Add { .. } => "add",
            Op::Revoke { .. } => "revoke",
            Op::Rotate { .. } => "rotate",
            Op::Scope { .. } => "scope",
        }
    }
}

/// A device key as the log lists it. Serialises flat:
/// `{id, sign, enc}` · `{id, sign, enc, expires}` · `{id, sign, enc, kind: "bot", can, expires}`.
#[derive(Clone, Debug, PartialEq)]
pub struct DeviceKey {
    pub id: KeyId,
    pub sign: Multikey,
    pub enc: Multikey,
    pub life: KeyLife,
}

/// How long a device key lives.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeyLife {
    /// No `kind`, no `expires`: valid until revoked.
    Standing,
    /// No `kind`, `expires` set (unix seconds), no `can`: valid until revoked or until `expires`.
    Until(u64),
    /// `kind: "bot"` with `can` and `expires`.
    Bot(BotScope),
}

/// A bot key's actions and expiry. `expires` is unix seconds; `can` holds `"read"`,
/// `"write"`, `"join"` (AR-04 4.14).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BotScope {
    pub can: Vec<String>,
    pub expires: u64,
}

/// The actions a bot scope may name.
pub const BOT_ACTIONS: &[&str] = &["read", "write", "join"];

/// The kind of identity. `"group"` is reserved: format 1 refuses it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Kind {
    Human,
}

impl Entry for Event {
    fn seq(&self) -> u64 {
        self.seq
    }
    fn prev(&self) -> Option<&Hash> {
        self.prev.as_ref()
    }
    fn proofs(&self) -> &[Proof] {
        &self.proof
    }
}

impl Serialize for DeviceKey {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("id", &self.id)?;
        map.serialize_entry("sign", &self.sign)?;
        map.serialize_entry("enc", &self.enc)?;
        match &self.life {
            KeyLife::Standing => {}
            KeyLife::Until(expires) => map.serialize_entry("expires", expires)?,
            KeyLife::Bot(scope) => {
                map.serialize_entry("kind", "bot")?;
                map.serialize_entry("can", &scope.can)?;
                map.serialize_entry("expires", &scope.expires)?;
            }
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for DeviceKey {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = Value::deserialize(deserializer)?;
        check_canonical(&value).map_err(serde::de::Error::custom)?;
        parse_device_key(&value).map_err(serde::de::Error::custom)
    }
}

impl Serialize for Event {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(None)?;
        map.serialize_entry("format", &self.format)?;
        map.serialize_entry("scid", &self.scid)?;
        map.serialize_entry("seq", &self.seq)?;
        map.serialize_entry("prev", &self.prev)?;
        map.serialize_entry("op", self.op.name())?;
        match &self.op {
            Op::Incept {
                kind,
                master,
                next,
                add,
            } => {
                map.serialize_entry("kind", kind)?;
                map.serialize_entry("master", master)?;
                map.serialize_entry("next", next)?;
                map.serialize_entry("add", add)?;
            }
            Op::Add { add, offer, co_sig } => {
                map.serialize_entry("add", add)?;
                map.serialize_entry("offer", offer)?;
                map.serialize_entry("coSig", co_sig)?;
            }
            Op::Revoke { revoke } => map.serialize_entry("revoke", revoke)?,
            Op::Rotate {
                master,
                salt,
                next,
                revoke,
            } => {
                map.serialize_entry("master", master)?;
                map.serialize_entry("salt", salt)?;
                map.serialize_entry("next", next)?;
                if !revoke.is_empty() {
                    map.serialize_entry("revoke", revoke)?;
                }
            }
            Op::Scope { key, can, expires } => {
                map.serialize_entry("key", key)?;
                map.serialize_entry("can", can)?;
                map.serialize_entry("expires", expires)?;
            }
        }
        if !self.proof.is_empty() {
            map.serialize_entry("proof", &self.proof)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for Event {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = Value::deserialize(deserializer)?;
        Event::from_json(&value).map_err(serde::de::Error::custom)
    }
}

/// The members every event carries, whatever its op.
const COMMON_FIELDS: &[&str] = &["format", "scid", "seq", "prev", "op", "proof"];
/// The members of a device key.
const DEVICE_KEY_FIELDS: &[&str] = &["id", "sign", "enc", "kind", "can", "expires"];

impl Event {
    /// Parses one event from JSON under the format-1 rules (AR-01 4.3.3 rule 10, R13).
    pub fn from_json(value: &Value) -> Result<Event, FoldError> {
        check_canonical(value)?;
        let map = value
            .as_object()
            .ok_or_else(|| format_err(FormatError::Type("event".into())))?;
        let format = string(map, "format")?;
        if format != crate::FORMAT {
            return Err(format_err(FormatError::Version(format)));
        }
        let op_name = string(map, "op")?;
        let op_fields: &[&str] = match op_name.as_str() {
            "incept" => &["kind", "master", "next", "add"],
            "add" => &["add", "offer", "coSig"],
            "revoke" => &["revoke"],
            "rotate" => &["master", "salt", "next", "revoke"],
            "scope" => &["key", "can", "expires"],
            _ => return Err(format_err(FormatError::UnknownOp(op_name))),
        };
        for key in map.keys() {
            if !COMMON_FIELDS.contains(&key.as_str()) && !op_fields.contains(&key.as_str()) {
                return Err(format_err(FormatError::UnknownField(key.clone())));
            }
        }
        let op = match op_name.as_str() {
            "incept" => Op::Incept {
                kind: parse_kind(&string(map, "kind")?)?,
                master: string(map, "master")?,
                next: string(map, "next")?,
                add: device_keys(map)?,
            },
            "add" => Op::Add {
                add: device_keys(map)?,
                offer: string(map, "offer")?,
                co_sig: string(map, "coSig")?,
            },
            "revoke" => Op::Revoke {
                revoke: strings(map, "revoke")?,
            },
            "rotate" => Op::Rotate {
                master: string(map, "master")?,
                salt: string(map, "salt")?,
                next: optional_string(map, "next")?,
                revoke: match map.get("revoke") {
                    None => Vec::new(),
                    Some(_) => {
                        let list = strings(map, "revoke")?;
                        if list.is_empty() {
                            return Err(format_err(FormatError::NotCanonical(
                                "an empty `revoke` on rotate must be left out".into(),
                            )));
                        }
                        list
                    }
                },
            },
            _ => Op::Scope {
                key: string(map, "key")?,
                can: strings(map, "can")?,
                expires: integer(map, "expires")?,
            },
        };
        let proof = match map.get("proof") {
            None => Vec::new(),
            Some(Value::Array(items)) if items.is_empty() => {
                return Err(format_err(FormatError::NotCanonical(
                    "an empty `proof` must be left out".into(),
                )))
            }
            Some(Value::Array(items)) => items
                .iter()
                .map(|p| {
                    serde_json::from_value::<Proof>(p.clone())
                        .map_err(|e| format_err(FormatError::Proof(e.to_string())))
                })
                .collect::<Result<_, _>>()?,
            Some(_) => return Err(format_err(FormatError::Type("proof".into()))),
        };
        let event = Event {
            format,
            scid: string(map, "scid")?,
            seq: integer(map, "seq")?,
            prev: optional_string(map, "prev")?,
            op,
            proof,
        };
        let back = serde_json::to_value(&event)
            .map_err(|e| format_err(FormatError::NotCanonical(e.to_string())))?;
        if &back != value {
            return Err(format_err(FormatError::NotCanonical(
                "the event does not serialise back to the same JSON".into(),
            )));
        }
        Ok(event)
    }

    /// The event as canonical JSON text, proofs included: the form on the wire and on disk.
    pub fn to_canonical_json(&self) -> Result<String, FormatError> {
        ad4m_hash_chain::entry_json(self).map_err(FormatError::from)
    }
}

fn format_err(e: FormatError) -> FoldError {
    FoldError::Format(e)
}

/// Refuses floats and integers outside ±(2^53 − 1) anywhere in `value`.
fn check_canonical(value: &Value) -> Result<(), FoldError> {
    canonical_json(value)
        .map(|_| ())
        .map_err(|e| format_err(FormatError::from(e)))
}

impl From<CanonError> for FormatError {
    fn from(e: CanonError) -> Self {
        match e {
            CanonError::Float(n) => FormatError::Float(n),
            CanonError::Integer(n) => FormatError::BigInteger(n),
            CanonError::NotObject => FormatError::Type("event".into()),
            CanonError::Serde(reason) => FormatError::NotCanonical(reason),
        }
    }
}

fn field<'a>(map: &'a Map<String, Value>, name: &str) -> Result<&'a Value, FoldError> {
    map.get(name)
        .ok_or_else(|| format_err(FormatError::Missing(name.to_string())))
}

fn string(map: &Map<String, Value>, name: &str) -> Result<String, FoldError> {
    field(map, name)?
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| format_err(FormatError::Type(name.to_string())))
}

fn optional_string(map: &Map<String, Value>, name: &str) -> Result<Option<String>, FoldError> {
    match field(map, name)? {
        Value::Null => Ok(None),
        Value::String(s) => Ok(Some(s.clone())),
        _ => Err(format_err(FormatError::Type(name.to_string()))),
    }
}

fn integer(map: &Map<String, Value>, name: &str) -> Result<u64, FoldError> {
    field(map, name)?
        .as_u64()
        .ok_or_else(|| format_err(FormatError::Type(name.to_string())))
}

fn strings(map: &Map<String, Value>, name: &str) -> Result<Vec<String>, FoldError> {
    let items = field(map, name)?
        .as_array()
        .ok_or_else(|| format_err(FormatError::Type(name.to_string())))?;
    items
        .iter()
        .map(|v| {
            v.as_str()
                .map(str::to_string)
                .ok_or_else(|| format_err(FormatError::Type(name.to_string())))
        })
        .collect()
}

fn device_keys(map: &Map<String, Value>) -> Result<Vec<DeviceKey>, FoldError> {
    field(map, "add")?
        .as_array()
        .ok_or_else(|| format_err(FormatError::Type("add".into())))?
        .iter()
        .map(parse_device_key)
        .collect()
}

fn parse_kind(kind: &str) -> Result<Kind, FoldError> {
    match kind {
        "human" => Ok(Kind::Human),
        other => Err(format_err(FormatError::UnsupportedKind(other.to_string()))),
    }
}

fn parse_device_key(value: &Value) -> Result<DeviceKey, FoldError> {
    let map = value
        .as_object()
        .ok_or_else(|| format_err(FormatError::Type("add".into())))?;
    for key in map.keys() {
        if !DEVICE_KEY_FIELDS.contains(&key.as_str()) {
            return Err(format_err(FormatError::UnknownField(key.clone())));
        }
    }
    let life = match map.get("kind") {
        None if map.contains_key("can") => return Err(FoldError::CanWithoutBotMark),
        None => match map.get("expires") {
            None => KeyLife::Standing,
            Some(_) => KeyLife::Until(integer(map, "expires")?),
        },
        Some(Value::String(kind)) if kind == "bot" => KeyLife::Bot(BotScope {
            can: strings(map, "can")?,
            expires: integer(map, "expires")?,
        }),
        Some(Value::String(kind)) => {
            return Err(format_err(FormatError::UnsupportedKind(kind.clone())))
        }
        Some(_) => return Err(format_err(FormatError::Type("kind".into()))),
    };
    Ok(DeviceKey {
        id: string(map, "id")?,
        sign: string(map, "sign")?,
        enc: string(map, "enc")?,
        life,
    })
}
