//! The chunked carriage of a person's export file and of an import (AR-06 4.10, 6.x limits).
//!
//! The file is gzip JSON ([`super::export::ExportFile`]). An export waits here, in memory, for
//! the client to read its chunks: it never rests on the host's disk. An import collects its
//! chunks here until the client commits. Both belong to one person (a managed agent's DID, or
//! the main agent) and expire 15 minutes after their last chunk.
//!
//! Limits: one export and one import at a time per person, 4 exports per hour per person,
//! chunks of 1 MiB, an import file over [`MAX_IMPORT_BYTES`] refused at `begin`.

use std::collections::HashMap;
use std::io::{Read, Write};
use std::sync::Mutex;

use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use sha2::{Digest, Sha256};

use super::export::{check_file, ExportFile, ImportRefusal};

/// The size of every chunk but the last.
pub const CHUNK_BYTES: usize = 1 << 20;
/// An export or import that sees no chunk for this long is dropped.
pub const EXPIRY_SECS: u64 = 15 * 60;
/// The largest import file (AR-06 4.10.2: 1 GiB, a provisional figure). No per-tenant limit.
pub const MAX_IMPORT_BYTES: u64 = 1 << 30;
/// Exports one person may prepare per hour.
pub const EXPORTS_PER_HOUR: usize = 4;
const HOUR_SECS: u64 = 3600;

/// Why a transfer call is refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransferError {
    /// No such transfer for this person (it may have expired).
    NotFound,
    /// An export or import of this person is open already.
    Busy,
    RateLimited {
        retry_after_secs: u64,
    },
    /// The caller named a perspective it does not own.
    NotYours(String),
    /// The file's declared or real size is over the limit.
    TooLarge {
        bytes: u64,
        max: u64,
    },
    Invalid(String),
}

impl std::fmt::Display for TransferError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransferError::NotFound => write!(f, "not-found: no such export or import"),
            TransferError::Busy => write!(f, "busy: an export or import is open already"),
            TransferError::RateLimited { retry_after_secs } => {
                write!(f, "rate-limited: retry in {retry_after_secs} s")
            }
            TransferError::NotYours(uuid) => {
                write!(f, "not-found: no perspective {uuid} of yours")
            }
            TransferError::TooLarge { bytes, max } => {
                write!(f, "too-large: {bytes} bytes is over the limit of {max}")
            }
            TransferError::Invalid(why) => write!(f, "invalid: {why}"),
        }
    }
}

impl From<ImportRefusal> for TransferError {
    fn from(refusal: ImportRefusal) -> Self {
        match refusal {
            ImportRefusal::TooLarge { bytes, max } => TransferError::TooLarge { bytes, max },
            other => TransferError::Invalid(other.to_string()),
        }
    }
}

/// What `prepare` answers: the file's size and hash, and how to read it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prepared {
    pub export_id: String,
    pub size: u64,
    pub sha256: String,
    pub chunks: u32,
    pub expires_at: u64,
}

struct Export {
    person: String,
    bytes: Vec<u8>,
    expires_at: u64,
}

struct Import {
    person: String,
    size: u64,
    sha256: String,
    bytes: Vec<u8>,
    chunks: u32,
    expires_at: u64,
}

#[derive(Default)]
struct Transfers {
    exports: HashMap<String, Export>,
    imports: HashMap<String, Import>,
    /// When each person prepared its exports, for the hourly limit.
    prepared: HashMap<String, Vec<u64>>,
}

impl Transfers {
    fn purge(&mut self, now: u64) {
        self.exports.retain(|_, e| e.expires_at > now);
        self.imports.retain(|_, i| i.expires_at > now);
    }

    /// `person`'s exports prepared in the last hour; forgets the older ones.
    fn recent(&mut self, person: &str, now: u64) -> Vec<u64> {
        let times = self.prepared.entry(person.to_string()).or_default();
        times.retain(|at| at + HOUR_SECS > now);
        times.clone()
    }
}

static TRANSFERS: std::sync::LazyLock<Mutex<Transfers>> =
    std::sync::LazyLock::new(|| Mutex::new(Transfers::default()));

fn transfers() -> std::sync::MutexGuard<'static, Transfers> {
    TRANSFERS.lock().unwrap_or_else(|e| e.into_inner())
}

fn new_id() -> String {
    uuid::Uuid::new_v4().simple().to_string()
}

/// The gzip JSON a person saves.
pub fn encode(file: &ExportFile) -> Result<Vec<u8>, TransferError> {
    let json = serde_json::to_vec(file).map_err(|e| TransferError::Invalid(e.to_string()))?;
    let mut gz = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    gz.write_all(&json)
        .and_then(|_| gz.finish())
        .map_err(|e| TransferError::Invalid(e.to_string()))
}

/// The export file inside `bytes`, read no further than `max` bytes of JSON (a gzip bomb
/// stops there), its format and version checked.
pub fn decode(bytes: &[u8], max: u64) -> Result<ExportFile, TransferError> {
    let mut json = Vec::new();
    flate2::read::GzDecoder::new(bytes)
        .take(max.saturating_add(1))
        .read_to_end(&mut json)
        .map_err(|e| TransferError::Invalid(format!("not a gzip file: {e}")))?;
    let file: ExportFile = serde_json::from_slice(&json).map_err(|e| {
        TransferError::Invalid(format!("not a {} file: {e}", super::export::FORMAT))
    })?;
    check_file(&file, json.len() as u64, max)?;
    Ok(file)
}

/// Whether `person` may start an export now: none open, fewer than [`EXPORTS_PER_HOUR`] in the
/// last hour. Asked before the file is built, because building it costs.
pub fn may_prepare(person: &str, now: u64) -> Result<(), TransferError> {
    let mut t = transfers();
    t.purge(now);
    if t.exports.values().any(|e| e.person == person) {
        return Err(TransferError::Busy);
    }
    let recent = t.recent(person, now);
    if recent.len() >= EXPORTS_PER_HOUR {
        let oldest = recent.iter().min().copied().unwrap_or(now);
        return Err(TransferError::RateLimited {
            retry_after_secs: (oldest + HOUR_SECS).saturating_sub(now).max(1),
        });
    }
    Ok(())
}

/// Keeps `bytes` for `person` to read in chunks. Counts against the hourly limit.
pub fn prepare(person: &str, bytes: Vec<u8>, now: u64) -> Result<Prepared, TransferError> {
    may_prepare(person, now)?;
    let mut t = transfers();
    let prepared = Prepared {
        export_id: new_id(),
        size: bytes.len() as u64,
        sha256: hex::encode(Sha256::digest(&bytes)),
        chunks: bytes.len().div_ceil(CHUNK_BYTES) as u32,
        expires_at: now + EXPIRY_SECS,
    };
    t.prepared.entry(person.to_string()).or_default().push(now);
    t.exports.insert(
        prepared.export_id.clone(),
        Export {
            person: person.to_string(),
            bytes,
            expires_at: prepared.expires_at,
        },
    );
    Ok(prepared)
}

/// Chunk `index` of `person`'s export, base64. Reading the last chunk ends the export; any
/// chunk renews its 15 minutes.
pub fn chunk(person: &str, export_id: &str, index: u32, now: u64) -> Result<String, TransferError> {
    let mut t = transfers();
    t.purge(now);
    let export = t
        .exports
        .get_mut(export_id)
        .filter(|e| e.person == person)
        .ok_or(TransferError::NotFound)?;
    let start = (index as usize)
        .checked_mul(CHUNK_BYTES)
        .filter(|start| *start < export.bytes.len())
        .ok_or_else(|| TransferError::Invalid(format!("no chunk {index}")))?;
    let end = (start + CHUNK_BYTES).min(export.bytes.len());
    let data = STANDARD.encode(&export.bytes[start..end]);
    export.expires_at = now + EXPIRY_SECS;
    if end == export.bytes.len() {
        t.exports.remove(export_id);
    }
    Ok(data)
}

/// Opens an import of `size` bytes that hash to `sha256`.
pub fn begin_import(
    person: &str,
    size: u64,
    sha256: &str,
    max: u64,
    now: u64,
) -> Result<String, TransferError> {
    if size > max {
        return Err(TransferError::TooLarge { bytes: size, max });
    }
    let mut t = transfers();
    t.purge(now);
    if t.imports.values().any(|i| i.person == person) {
        return Err(TransferError::Busy);
    }
    let import_id = new_id();
    t.imports.insert(
        import_id.clone(),
        Import {
            person: person.to_string(),
            size,
            sha256: sha256.to_ascii_lowercase(),
            bytes: Vec::new(),
            chunks: 0,
            expires_at: now + EXPIRY_SECS,
        },
    );
    Ok(import_id)
}

/// Adds chunk `index` (the next one in order) to an import; answers how many chunks it holds.
pub fn import_chunk(
    person: &str,
    import_id: &str,
    index: u32,
    data: &str,
    now: u64,
) -> Result<u32, TransferError> {
    let bytes = STANDARD
        .decode(data)
        .map_err(|e| TransferError::Invalid(format!("chunk is not base64: {e}")))?;
    if bytes.len() > CHUNK_BYTES {
        return Err(TransferError::Invalid(format!(
            "a chunk is at most {CHUNK_BYTES} bytes"
        )));
    }
    let mut t = transfers();
    t.purge(now);
    let import = t
        .imports
        .get_mut(import_id)
        .filter(|i| i.person == person)
        .ok_or(TransferError::NotFound)?;
    if index != import.chunks {
        return Err(TransferError::Invalid(format!(
            "expected chunk {}, got {index}",
            import.chunks
        )));
    }
    if (import.bytes.len() + bytes.len()) as u64 > import.size {
        return Err(TransferError::Invalid(
            "more bytes than the import declared".into(),
        ));
    }
    import.bytes.extend_from_slice(&bytes);
    import.chunks += 1;
    import.expires_at = now + EXPIRY_SECS;
    Ok(import.chunks)
}

/// Ends an import and answers its bytes, once they are the size and hash it declared. An
/// import that does not match stays open so the client may send the rest.
pub fn take_import(person: &str, import_id: &str, now: u64) -> Result<Vec<u8>, TransferError> {
    let mut t = transfers();
    t.purge(now);
    let import = t
        .imports
        .get(import_id)
        .filter(|i| i.person == person)
        .ok_or(TransferError::NotFound)?;
    if import.bytes.len() as u64 != import.size {
        return Err(TransferError::Invalid(format!(
            "{} of {} bytes received",
            import.bytes.len(),
            import.size
        )));
    }
    if hex::encode(Sha256::digest(&import.bytes)) != import.sha256 {
        return Err(TransferError::Invalid("the sha256 does not match".into()));
    }
    Ok(t.imports
        .remove(import_id)
        .map(|i| i.bytes)
        .unwrap_or_default())
}

#[cfg(test)]
mod tests;
