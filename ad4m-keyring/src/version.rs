use std::collections::BTreeMap;
use std::fmt;

use serde::{Deserialize, Serialize};

use crate::KeyringError;

/// The version of a key: 1 for the first, one more for each new one, never reused.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct KeyVersion(pub u32);

impl KeyVersion {
    /// The version of a key's first value.
    pub const FIRST: KeyVersion = KeyVersion(1);

    /// The version after this one, or `None` after `u32::MAX`.
    pub fn next(self) -> Option<KeyVersion> {
        self.0.checked_add(1).map(KeyVersion)
    }
}

impl fmt::Display for KeyVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

/// The versions of one key, each held as a `W` (a wrap of it, or the key itself in memory).
///
/// The rules, which every constructor and the deserializer keep:
/// - there is always a current version, and it is the highest one held: new data is written
///   under it;
/// - a new version is one more than the current one, so a version is never reused, not even
///   after the newest older one was retired;
/// - older versions stay, to read what was written under them, until [`VersionedKey::retire`]
///   removes them; the current version cannot be retired.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(
    try_from = "RawVersionedKey<W>",
    bound(deserialize = "W: Deserialize<'de>")
)]
pub struct VersionedKey<W> {
    current: KeyVersion,
    versions: BTreeMap<KeyVersion, W>,
}

#[derive(Deserialize)]
struct RawVersionedKey<W> {
    current: KeyVersion,
    versions: BTreeMap<KeyVersion, W>,
}

impl<W> TryFrom<RawVersionedKey<W>> for VersionedKey<W> {
    type Error = KeyringError;

    fn try_from(raw: RawVersionedKey<W>) -> Result<Self, KeyringError> {
        let Some((&highest, _)) = raw.versions.last_key_value() else {
            return Err(KeyringError::InvalidVersions("no version".into()));
        };
        if raw.versions.contains_key(&KeyVersion(0)) {
            return Err(KeyringError::InvalidVersions("version 0".into()));
        }
        if raw.current != highest {
            return Err(KeyringError::InvalidVersions(format!(
                "current is {}, the highest held is {}",
                raw.current, highest
            )));
        }
        Ok(VersionedKey {
            current: raw.current,
            versions: raw.versions,
        })
    }
}

impl<W> VersionedKey<W> {
    /// A key with its first value as version 1.
    pub fn new(first: W) -> Self {
        VersionedKey {
            current: KeyVersion::FIRST,
            versions: BTreeMap::from([(KeyVersion::FIRST, first)]),
        }
    }

    /// The version new data is written under.
    pub fn current(&self) -> KeyVersion {
        self.current
    }

    /// The value of the current version.
    pub fn current_value(&self) -> &W {
        &self.versions[&self.current]
    }

    /// The value of `version`, current or older.
    pub fn get(&self, version: KeyVersion) -> Result<&W, KeyringError> {
        self.versions
            .get(&version)
            .ok_or(KeyringError::UnknownVersion(version))
    }

    /// The value of `version`, to replace it in place (a re-wrap under a new wrapping key
    /// keeps the version).
    pub fn get_mut(&mut self, version: KeyVersion) -> Result<&mut W, KeyringError> {
        self.versions
            .get_mut(&version)
            .ok_or(KeyringError::UnknownVersion(version))
    }

    /// The version the next [`VersionedKey::push`] gives.
    pub fn next_version(&self) -> Result<KeyVersion, KeyringError> {
        self.current
            .next()
            .ok_or(KeyringError::VersionsExhausted(self.current))
    }

    /// Adds `value` as the next version and makes it current. The older versions stay.
    pub fn push(&mut self, value: W) -> Result<KeyVersion, KeyringError> {
        let version = self.next_version()?;
        self.versions.insert(version, value);
        self.current = version;
        Ok(version)
    }

    /// Removes an older version and returns its value. Refuses the current version.
    pub fn retire(&mut self, version: KeyVersion) -> Result<W, KeyringError> {
        if version == self.current {
            return Err(KeyringError::RetireCurrent(version));
        }
        self.versions
            .remove(&version)
            .ok_or(KeyringError::UnknownVersion(version))
    }

    /// Every version held, oldest first.
    pub fn iter(&self) -> impl Iterator<Item = (KeyVersion, &W)> {
        self.versions
            .iter()
            .map(|(version, value)| (*version, value))
    }

    /// The number of versions held.
    pub fn len(&self) -> usize {
        self.versions.len()
    }

    /// Always `false`: a key has at least its current version.
    pub fn is_empty(&self) -> bool {
        self.versions.is_empty()
    }
}
