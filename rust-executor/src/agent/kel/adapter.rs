//! KEL storage adapter trait and types.
//!
//! A Language acts as a DID-host adapter: append an event, fetch a log, read the
//! head. v1 puts a centralized HTTPS service behind the trait; Holochain becomes
//! a later adapter. Migration means swapping the adapter, since a KEL replays
//! byte-identically onto any store.

use super::{fold, KelError, KeyEvent};
use chrono::{DateTime, Utc};
use rusqlite::{params, Connection};
use std::collections::HashMap;
use std::sync::{Mutex, RwLock};

// ─── error types ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum AdapterError {
    /// Event fails `fold` against the current head.
    FoldRejected(KelError),
    /// Network or HTTP transport failure.
    Transport(String),
    /// Database read/write failure.
    Storage(String),
    /// Signed head fails signature verification.
    HeadSignatureInvalid,
    /// Server returned a lower seq than one already cached for this DID.
    MonotonicityViolation { cached: u64, received: u64 },
    /// SCID not found in this adapter.
    NotFound,
}

impl std::fmt::Display for AdapterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdapterError::FoldRejected(e) => write!(f, "fold rejected: {}", e),
            AdapterError::Transport(e) => write!(f, "transport: {}", e),
            AdapterError::Storage(e) => write!(f, "storage: {}", e),
            AdapterError::HeadSignatureInvalid => write!(f, "head signature invalid"),
            AdapterError::MonotonicityViolation { cached, received } => {
                write!(
                    f,
                    "monotonicity violation: cached={}, received={}",
                    cached, received
                )
            }
            AdapterError::NotFound => write!(f, "SCID not found"),
        }
    }
}

impl std::error::Error for AdapterError {}

// ─── SignedHead ──────────────────────────────────────────────────────────────

/// A signed, timestamped head statement. A lying server produces evidence.
#[derive(Debug, Clone)]
pub struct SignedHead {
    pub scid: String,
    pub seq: u64,
    pub at: DateTime<Utc>,
    pub server_key: String,
    pub signature: String,
}

// ─── adapter trait ───────────────────────────────────────────────────────────

/// Storage-agnostic KEL adapter. No verification logic — that lives in `fold`.
pub trait KelAdapter: Send + Sync {
    /// Append an event to the log. Validates through `fold` before storing.
    fn append(&self, scid: &str, event: KeyEvent) -> Result<(), AdapterError>;
    /// Fetch the log from `from_seq` onward.
    fn get_log(&self, scid: &str, from_seq: u64) -> Result<Vec<KeyEvent>, AdapterError>;
    /// Return the head — a signed, timestamped statement of the current seq.
    fn head(&self, scid: &str) -> Result<SignedHead, AdapterError>;
}

// ─── memory adapter (for testing and local-only mode) ────────────────────────

/// An in-memory KEL adapter. Validates through `fold` on every append.
/// Suitable for testing and local-only (no server) mode.
#[derive(Default)]
pub struct MemoryAdapter {
    logs: RwLock<HashMap<String, Vec<KeyEvent>>>,
}

impl MemoryAdapter {
    pub fn new() -> Self {
        Self {
            logs: RwLock::new(HashMap::new()),
        }
    }

    /// Seed the adapter with a pre-built log (for testing).
    pub fn seed(&self, scid: &str, events: Vec<KeyEvent>) {
        if let Ok(mut logs) = self.logs.write() {
            logs.insert(scid.to_string(), events);
        }
    }
}

impl KelAdapter for MemoryAdapter {
    fn append(&self, scid: &str, event: KeyEvent) -> Result<(), AdapterError> {
        let mut logs = self
            .logs
            .write()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;
        let log = logs.entry(scid.to_string()).or_default();
        // Validate: replay the full log + the new event through fold.
        let mut candidate = log.clone();
        candidate.push(event.clone());
        fold(&candidate).map_err(AdapterError::FoldRejected)?;
        log.push(event);
        Ok(())
    }

    fn get_log(&self, scid: &str, from_seq: u64) -> Result<Vec<KeyEvent>, AdapterError> {
        let logs = self
            .logs
            .read()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;
        match logs.get(scid) {
            Some(events) => Ok(events
                .iter()
                .filter(|e| e.seq >= from_seq)
                .cloned()
                .collect()),
            None => Err(AdapterError::NotFound),
        }
    }

    fn head(&self, scid: &str) -> Result<SignedHead, AdapterError> {
        let logs = self
            .logs
            .read()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;
        match logs.get(scid) {
            Some(events) => {
                let last = events.last().ok_or(AdapterError::NotFound)?;
                Ok(SignedHead {
                    scid: scid.to_string(),
                    seq: last.seq,
                    at: Utc::now(),
                    server_key: "memory-adapter".to_string(),
                    signature: "unsigned-memory".to_string(),
                })
            }
            None => Err(AdapterError::NotFound),
        }
    }
}

// ─── SQLite adapter (persistent, local-only) ────────────────────────────────

/// A persistent KEL adapter backed by SQLite.
///
/// Stores events as JCS-serialized JSON blobs keyed by (scid, seq). Validates
/// through `fold` on every append — the database only holds valid logs.
pub struct SqliteAdapter {
    conn: Mutex<Connection>,
}

impl SqliteAdapter {
    /// Open (or create) the KEL database at the given path.
    pub fn open(path: &str) -> Result<Self, AdapterError> {
        let conn =
            Connection::open(path).map_err(|e| AdapterError::Storage(format!("open: {}", e)))?;
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS kel_events (
                scid TEXT NOT NULL,
                seq  INTEGER NOT NULL,
                json TEXT NOT NULL,
                PRIMARY KEY (scid, seq)
            );",
        )
        .map_err(|e| AdapterError::Storage(format!("schema: {}", e)))?;
        Ok(Self {
            conn: Mutex::new(conn),
        })
    }

    /// Open an in-memory SQLite database (for testing without files).
    #[cfg(test)]
    pub fn open_memory() -> Result<Self, AdapterError> {
        Self::open(":memory:")
    }
}

impl KelAdapter for SqliteAdapter {
    fn append(&self, scid: &str, event: KeyEvent) -> Result<(), AdapterError> {
        let conn = self
            .conn
            .lock()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;

        // Read existing log for this SCID.
        let mut stmt = conn
            .prepare("SELECT json FROM kel_events WHERE scid = ?1 ORDER BY seq ASC")
            .map_err(|e| AdapterError::Storage(e.to_string()))?;
        let existing: Vec<KeyEvent> = stmt
            .query_map(params![scid], |row| {
                let json: String = row.get(0)?;
                serde_json::from_str(&json)
                    .map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))
            })
            .map_err(|e| AdapterError::Storage(e.to_string()))?
            .collect::<Result<_, _>>()
            .map_err(|e| AdapterError::Storage(e.to_string()))?;

        // Validate: replay the full log + the new event through fold.
        let mut candidate = existing;
        candidate.push(event.clone());
        fold(&candidate).map_err(AdapterError::FoldRejected)?;

        // Persist.
        let json = serde_json::to_string(&event)
            .map_err(|e| AdapterError::Storage(format!("serialize: {}", e)))?;
        conn.execute(
            "INSERT INTO kel_events (scid, seq, json) VALUES (?1, ?2, ?3)",
            params![scid, event.seq as i64, json],
        )
        .map_err(|e| AdapterError::Storage(e.to_string()))?;
        Ok(())
    }

    fn get_log(&self, scid: &str, from_seq: u64) -> Result<Vec<KeyEvent>, AdapterError> {
        let conn = self
            .conn
            .lock()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;
        let mut stmt = conn
            .prepare("SELECT json FROM kel_events WHERE scid = ?1 AND seq >= ?2 ORDER BY seq ASC")
            .map_err(|e| AdapterError::Storage(e.to_string()))?;
        let events: Vec<KeyEvent> = stmt
            .query_map(params![scid, from_seq as i64], |row| {
                let json: String = row.get(0)?;
                serde_json::from_str(&json)
                    .map_err(|e| rusqlite::Error::ToSqlConversionFailure(Box::new(e)))
            })
            .map_err(|e| AdapterError::Storage(e.to_string()))?
            .collect::<Result<_, _>>()
            .map_err(|e| AdapterError::Storage(e.to_string()))?;
        if events.is_empty() {
            // Distinguish "SCID exists but no events after from_seq" from "SCID unknown".
            let count: i64 = conn
                .query_row(
                    "SELECT COUNT(*) FROM kel_events WHERE scid = ?1",
                    params![scid],
                    |row| row.get(0),
                )
                .map_err(|e| AdapterError::Storage(e.to_string()))?;
            if count == 0 {
                return Err(AdapterError::NotFound);
            }
        }
        Ok(events)
    }

    fn head(&self, scid: &str) -> Result<SignedHead, AdapterError> {
        let conn = self
            .conn
            .lock()
            .map_err(|_| AdapterError::Storage("lock poisoned".into()))?;
        let seq: i64 = conn
            .query_row(
                "SELECT MAX(seq) FROM kel_events WHERE scid = ?1",
                params![scid],
                |row| row.get(0),
            )
            .map_err(|e| match e {
                rusqlite::Error::QueryReturnedNoRows => AdapterError::NotFound,
                _ => AdapterError::Storage(e.to_string()),
            })?;
        Ok(SignedHead {
            scid: scid.to_string(),
            seq: seq as u64,
            at: Utc::now(),
            server_key: "sqlite-adapter".to_string(),
            signature: "local-only".to_string(),
        })
    }
}

// ─── monotonicity cache ──────────────────────────────────────────────────────

/// Remembers the highest seq seen per DID. Refuses to go backwards.
#[derive(Default)]
pub struct MonotonicityCache {
    heads: RwLock<HashMap<String, u64>>,
}

impl MonotonicityCache {
    pub fn new() -> Self {
        Self {
            heads: RwLock::new(HashMap::new()),
        }
    }

    /// Record a head seq. Returns Ok if the new seq >= the cached seq,
    /// Err(MonotonicityViolation) if the received seq goes backwards.
    pub fn check_and_update(&self, scid: &str, received_seq: u64) -> Result<(), AdapterError> {
        let mut heads = self
            .heads
            .write()
            .map_err(|_| AdapterError::Storage("cache lock poisoned".into()))?;
        let cached = heads.entry(scid.to_string()).or_insert(0);
        if received_seq < *cached {
            return Err(AdapterError::MonotonicityViolation {
                cached: *cached,
                received: received_seq,
            });
        }
        *cached = received_seq;
        Ok(())
    }

    /// Get the cached seq for a DID (if any).
    pub fn get(&self, scid: &str) -> Option<u64> {
        self.heads.read().ok().and_then(|h| h.get(scid).copied())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::kel::recovery::did_key_of;
    use crate::agent::kel::{
        incept_human, recovery, KeyEntry, KeyEventBody, RecoveryAuthority, Scope,
    };
    use did_key::{generate, Ed25519KeyPair};

    fn keypair() -> (did_key::PatchedKeyPair, String) {
        let kp = generate::<Ed25519KeyPair>(None);
        let did = did_key_of(&kp);
        (kp, did)
    }

    fn full_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::full(),
        }
    }

    fn dummy_commitment() -> String {
        recovery::recovery_commitment(&RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkDummy".to_string()],
        })
    }

    #[test]
    fn append_rejects_invalid() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = MemoryAdapter::new();
        adapter.append(&scid, ev0).unwrap();

        // Try appending an event with wrong seq (gap).
        let (_kp1, did1) = keypair();
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 5,
        };
        // seq 5 instead of 1 → fold rejects with SeqGap.
        let bad_ev = super::super::KeyEvent::new(5, Some("fake".to_string()), body, &key_id0, &kp0);
        let result = adapter.append(&scid, bad_ev);
        assert!(matches!(result, Err(AdapterError::FoldRejected(_))));
    }

    #[test]
    fn head_returns_current_seq() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = MemoryAdapter::new();
        adapter.append(&scid, ev0.clone()).unwrap();

        let head = adapter.head(&scid).unwrap();
        assert_eq!(head.seq, 0);
        assert_eq!(head.scid, scid);

        // Delegate a key, head should advance to seq 1.
        let (_, did1) = keypair();
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 1,
        };
        let ev1 = super::super::KeyEvent::new(1, Some(ev0.hash.clone()), body, &key_id0, &kp0);
        adapter.append(&scid, ev1).unwrap();
        let head = adapter.head(&scid).unwrap();
        assert_eq!(head.seq, 1);
    }

    #[test]
    fn cache_refuses_rollback() {
        let cache = MonotonicityCache::new();
        cache.check_and_update("did:scid:ke:1:Etest", 5).unwrap();
        cache.check_and_update("did:scid:ke:1:Etest", 5).unwrap(); // same seq ok
        cache.check_and_update("did:scid:ke:1:Etest", 7).unwrap(); // forward ok
        let result = cache.check_and_update("did:scid:ke:1:Etest", 3);
        assert!(matches!(
            result,
            Err(AdapterError::MonotonicityViolation {
                cached: 7,
                received: 3
            })
        ));
    }

    #[test]
    fn get_log_from_seq() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = MemoryAdapter::new();
        adapter.append(&scid, ev0.clone()).unwrap();

        let (_, did1) = keypair();
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 1,
        };
        let ev1 = super::super::KeyEvent::new(1, Some(ev0.hash.clone()), body, &key_id0, &kp0);
        adapter.append(&scid, ev1).unwrap();

        // Full log.
        let log = adapter.get_log(&scid, 0).unwrap();
        assert_eq!(log.len(), 2);

        // From seq 1 onward.
        let log = adapter.get_log(&scid, 1).unwrap();
        assert_eq!(log.len(), 1);
        assert_eq!(log[0].seq, 1);
    }

    // ── SQLite adapter tests ────────────────────────────────────────────

    #[test]
    fn sqlite_round_trip() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = SqliteAdapter::open_memory().unwrap();
        adapter.append(&scid, ev0.clone()).unwrap();

        let head = adapter.head(&scid).unwrap();
        assert_eq!(head.seq, 0);

        let log = adapter.get_log(&scid, 0).unwrap();
        assert_eq!(log.len(), 1);
        assert_eq!(log[0].seq, ev0.seq);
        assert_eq!(log[0].hash, ev0.hash);
    }

    #[test]
    fn sqlite_append_rejects_invalid() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = SqliteAdapter::open_memory().unwrap();
        adapter.append(&scid, ev0).unwrap();

        let (_kp1, did1) = keypair();
        let body = KeyEventBody::Delegate {
            key: full_key(&format!("{}#key-1", did1), &did1),
            from_seq: 5,
        };
        let bad_ev = super::super::KeyEvent::new(5, Some("fake".to_string()), body, &key_id0, &kp0);
        let result = adapter.append(&scid, bad_ev);
        assert!(matches!(result, Err(AdapterError::FoldRejected(_))));
    }

    #[test]
    fn sqlite_not_found() {
        let adapter = SqliteAdapter::open_memory().unwrap();
        let result = adapter.get_log("did:scid:ke:1:ENonexistent", 0);
        assert!(matches!(result, Err(AdapterError::NotFound)));
    }

    #[test]
    fn sqlite_multi_scid() {
        let (kp_a, did_a) = keypair();
        let key_id_a = format!("{}#key-0", did_a);
        let key_a = full_key(&key_id_a, &did_a);
        let (ev_a, scid_a) = incept_human(vec![key_a], dummy_commitment(), &key_id_a, &kp_a);

        let (kp_b, did_b) = keypair();
        let key_id_b = format!("{}#key-0", did_b);
        let key_b = full_key(&key_id_b, &did_b);
        let (ev_b, scid_b) = incept_human(vec![key_b], dummy_commitment(), &key_id_b, &kp_b);

        let adapter = SqliteAdapter::open_memory().unwrap();
        adapter.append(&scid_a, ev_a).unwrap();
        adapter.append(&scid_b, ev_b).unwrap();

        let log_a = adapter.get_log(&scid_a, 0).unwrap();
        let log_b = adapter.get_log(&scid_b, 0).unwrap();
        assert_eq!(log_a.len(), 1);
        assert_eq!(log_b.len(), 1);
        assert_ne!(log_a[0].hash, log_b[0].hash);
    }
}
