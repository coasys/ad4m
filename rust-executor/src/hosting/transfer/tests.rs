//! The carriage's guards: one at a time, the hourly limit, ownership, expiry, size and hash.
//! Each test runs on its own `Transfers`: the store sweeps by the caller's clock, so tests
//! that shared the global one would expire each other's transfers.

use super::*;
use crate::hosting::export::{ExportFile, FORMAT, VERSION};

const T0: u64 = 5_000_000;

fn file() -> ExportFile {
    ExportFile {
        format: FORMAT.into(),
        version: VERSION,
        created_at: "2026-01-02T00:00:00Z".into(),
        executor_did: "did:key:zA".into(),
        person_did_scid: None,
        device_did: "did:key:zP".into(),
        perspectives: vec![],
    }
}

fn big(len: usize) -> Vec<u8> {
    (0..len).map(|i| (i % 251) as u8).collect()
}

fn read_all(t: &mut Transfers, person: &str, p: &Prepared, now: u64) -> Vec<u8> {
    let mut all = Vec::new();
    for index in 0..p.chunks {
        all.extend(
            STANDARD
                .decode(t.chunk(person, &p.export_id, index, now).unwrap())
                .unwrap(),
        );
    }
    all
}

#[test]
fn an_export_is_read_in_one_mib_chunks_that_join_to_the_hashed_file() {
    let mut t = Transfers::default();
    let bytes = big(CHUNK_BYTES * 2 + 17);
    let p = t.prepare("p-chunks", bytes.clone(), T0).unwrap();
    assert_eq!(p.chunks, 3);
    assert_eq!(p.size, bytes.len() as u64);
    assert_eq!(p.sha256, hex::encode(Sha256::digest(&bytes)));
    let joined = read_all(&mut t, "p-chunks", &p, T0 + 1);
    assert_eq!(joined, bytes);
    // The last chunk ended it.
    assert_eq!(
        t.chunk("p-chunks", &p.export_id, 0, T0 + 2),
        Err(TransferError::NotFound)
    );
}

#[test]
fn an_export_belongs_to_the_person_that_prepared_it() {
    let mut t = Transfers::default();
    let p = t.prepare("p-owner", big(10), T0).unwrap();
    assert_eq!(
        t.chunk("p-other", &p.export_id, 0, T0),
        Err(TransferError::NotFound)
    );
    assert!(t.chunk("p-owner", &p.export_id, 0, T0).is_ok());
}

#[test]
fn one_export_at_a_time_and_four_an_hour() {
    let mut t = Transfers::default();
    let p = t.prepare("p-rate", big(10), T0).unwrap();
    assert_eq!(
        t.prepare("p-rate", big(10), T0 + 1),
        Err(TransferError::Busy)
    );
    read_all(&mut t, "p-rate", &p, T0 + 1);
    for n in 1..EXPORTS_PER_HOUR as u64 {
        let p = t.prepare("p-rate", big(10), T0 + 10 * n).unwrap();
        read_all(&mut t, "p-rate", &p, T0 + 10 * n);
    }
    let limited = t.prepare("p-rate", big(10), T0 + 100);
    assert_eq!(
        limited,
        Err(TransferError::RateLimited {
            retry_after_secs: HOUR_SECS - 100
        })
    );
    // An hour after the first, one more is allowed.
    assert!(t.prepare("p-rate", big(10), T0 + HOUR_SECS).is_ok());
}

#[test]
fn an_export_expires_fifteen_minutes_after_its_last_chunk() {
    let mut t = Transfers::default();
    let p = t.prepare("p-expiry", big(CHUNK_BYTES + 1), T0).unwrap();
    t.chunk("p-expiry", &p.export_id, 0, T0 + EXPIRY_SECS - 1)
        .unwrap();
    // The chunk renewed it.
    t.chunk("p-expiry", &p.export_id, 1, T0 + 2 * EXPIRY_SECS - 2)
        .unwrap();
    let q = t
        .prepare("p-expiry", big(10), T0 + 3 * EXPIRY_SECS)
        .unwrap();
    assert_eq!(
        t.chunk("p-expiry", &q.export_id, 0, T0 + 4 * EXPIRY_SECS),
        Err(TransferError::NotFound)
    );
}

#[test]
fn an_import_over_the_limit_is_refused_at_begin() {
    let mut t = Transfers::default();
    assert_eq!(
        t.begin_import("p-big", 11, "00", 10, T0),
        Err(TransferError::TooLarge { bytes: 11, max: 10 })
    );
    assert!(t.begin_import("p-big", 10, "00", 10, T0).is_ok());
}

#[test]
fn an_import_commits_only_the_bytes_it_declared_in_order() {
    let mut t = Transfers::default();
    let bytes = big(CHUNK_BYTES + 5);
    let sha = hex::encode(Sha256::digest(&bytes));
    let id = t
        .begin_import("p-imp", bytes.len() as u64, &sha, MAX_IMPORT_BYTES, T0)
        .unwrap();
    assert_eq!(
        t.begin_import("p-imp", 1, &sha, MAX_IMPORT_BYTES, T0),
        Err(TransferError::Busy),
        "one import at a time per person"
    );
    let send = |t: &mut Transfers, index: u32, part: &[u8]| {
        t.import_chunk("p-imp", &id, index, &STANDARD.encode(part), T0 + 1)
    };
    assert!(
        matches!(
            send(&mut t, 1, &bytes[CHUNK_BYTES..]),
            Err(TransferError::Invalid(_))
        ),
        "a chunk out of order"
    );
    assert_eq!(send(&mut t, 0, &bytes[..CHUNK_BYTES]), Ok(1));
    assert!(
        matches!(
            t.take_import("p-imp", &id, T0 + 2),
            Err(TransferError::Invalid(_))
        ),
        "short of the declared size"
    );
    assert_eq!(send(&mut t, 1, &bytes[CHUNK_BYTES..]), Ok(2));
    assert_eq!(
        t.take_import("p-other", &id, T0 + 2),
        Err(TransferError::NotFound)
    );
    assert_eq!(t.take_import("p-imp", &id, T0 + 2).unwrap(), bytes);
    assert_eq!(
        t.take_import("p-imp", &id, T0 + 3),
        Err(TransferError::NotFound)
    );
}

#[test]
fn an_import_with_the_wrong_hash_or_too_many_bytes_is_refused() {
    let mut t = Transfers::default();
    let id = t
        .begin_import("p-hash", 4, &"0".repeat(64), MAX_IMPORT_BYTES, T0)
        .unwrap();
    assert!(matches!(
        t.import_chunk("p-hash", &id, 0, &STANDARD.encode([1u8; 5]), T0),
        Err(TransferError::Invalid(_))
    ));
    t.import_chunk("p-hash", &id, 0, &STANDARD.encode([1u8; 4]), T0)
        .unwrap();
    assert_eq!(
        t.take_import("p-hash", &id, T0),
        Err(TransferError::Invalid("the sha256 does not match".into()))
    );
}

#[test]
fn a_file_goes_through_gzip_and_back_and_a_bomb_stops_at_the_limit() {
    let bytes = encode(&file()).unwrap();
    assert_eq!(&bytes[..2], &[0x1f, 0x8b], "gzip");
    assert_eq!(decode(&bytes, MAX_IMPORT_BYTES).unwrap(), file());
    assert!(matches!(
        decode(&bytes, 10),
        Err(TransferError::Invalid(_)) | Err(TransferError::TooLarge { .. })
    ));
    assert!(matches!(
        decode(b"not gzip", MAX_IMPORT_BYTES),
        Err(TransferError::Invalid(_))
    ));
    let mut other = file();
    other.version = 2;
    let other = encode(&other).unwrap();
    assert!(matches!(
        decode(&other, MAX_IMPORT_BYTES),
        Err(TransferError::Invalid(_))
    ));
}
