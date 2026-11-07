//! The operator record and the first-link rule.
//!
//! The record names the identity that operates this executor: `ad4m-executor init --operator
//! <did:scid>` sets it while it is empty, and a different value never replaces it (only the
//! recorded operator may change it, through a `runtime.setOperator` call that does not exist
//! yet). A claim for the main agent while its key is in no valid log runs
//! [`first_link_check`] (in `ClaimService::claim_for_main`): only an offer of the recorded
//! identity passes. The one exception is a desktop app's executor at its first run
//! (`--custody-channel`, no record, the main agent of no identity yet): the desktop never runs
//! `init --operator` and does not know its person before the first offer arrives, so it may claim
//! any offer, and the identity its main agent joins (the holder confirmed, the add landed:
//! `ClaimService::join_status`) is recorded as the operator (`OperatorSetBy::FirstRun`). A claim
//! that never completes records nothing. From then on the rule holds as for every executor.
//! While the key is valid in a log, `claim_for_main` refuses every offer
//! (`already-linked`).
//!
//! The same rule holds for a new identity: with an operator record the main key may speak only
//! for the operator's identity, so `identity.cosignIncept` for the main agent runs
//! [`main_incept_check`] and refuses an incept of any other identity. Managed users are not
//! bound by the operator record.

use std::path::Path;

use crate::db::{OperatorRecordRow, OperatorSetBy};

/// Why a claim for the main agent is refused before it reaches the store.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum FirstLinkRefusal {
    /// No operator record: a server started with no `--operator` opens no link route (a desktop
    /// app's executor at its first run excepted: [`FirstLink::FirstRun`]).
    #[error("no-operator: this executor has no operator; start it with `ad4m-executor init --operator <did:scid>`")]
    NoOperator,
    /// The main key is in no log and the offer is not of the recorded operator's identity.
    #[error("first-link: this executor's first link must come from its operator {operator}, not from {offer}")]
    FirstLink { operator: String, offer: String },
    /// An executor with an operator record: the main key joins the operator's identity
    /// through a claim and co-signs no incept of a new one.
    #[error("first-link: this executor's main key may speak only for its operator {operator}; it co-signs no incept of {incept}")]
    OperatorIncept { operator: String, incept: String },
}

/// `OperatorRecord::admits_first_link`: true only when the record is set and
/// names `offer_did`.
pub fn admits_first_link(record: Option<&OperatorRecordRow>, offer_did: &str) -> bool {
    record.is_some_and(|r| r.scid == offer_did)
}

/// What the first-link rule lets a claim for the main agent do.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FirstLink {
    /// The offer is of the recorded operator.
    Operator,
    /// A desktop app's executor at its first run, with no record: the claim goes ahead, and the
    /// identity the main agent joins through it becomes the operator (`OperatorSetBy::FirstRun`).
    FirstRun,
}

/// The check `claim_for_main` runs before it claims, while the main key is in no valid log: an
/// operator record must exist and the offer must be of the recorded identity. `desktop_first_run`
/// (a `--custody-channel` executor whose main agent speaks for no identity yet) lets a claim
/// with no record through as [`FirstLink::FirstRun`]; it changes nothing once a record exists.
pub fn first_link_check(
    record: Option<&OperatorRecordRow>,
    offer_did: &str,
    desktop_first_run: bool,
) -> Result<FirstLink, FirstLinkRefusal> {
    let Some(record) = record else {
        return if desktop_first_run {
            Ok(FirstLink::FirstRun)
        } else {
            Err(FirstLinkRefusal::NoOperator)
        };
    };
    if admits_first_link(Some(record), offer_did) {
        return Ok(FirstLink::Operator);
    }
    Err(FirstLinkRefusal::FirstLink {
        operator: record.scid.clone(),
        offer: offer_did.to_string(),
    })
}

/// The check `identity.cosignIncept` runs for the main agent: without an operator record the
/// main key may start an identity (a desktop executor's first run); with one, only an incept of
/// the recorded identity passes, which a new incept never is (its SCID derives from it).
pub fn main_incept_check(
    record: Option<&OperatorRecordRow>,
    incept_did: &str,
) -> Result<(), FirstLinkRefusal> {
    match record {
        Some(record) if !admits_first_link(Some(record), incept_did) => {
            Err(FirstLinkRefusal::OperatorIncept {
                operator: record.scid.clone(),
                incept: incept_did.to_string(),
            })
        }
        _ => Ok(()),
    }
}

/// Why `init --operator` did not record the operator.
#[derive(Debug, thiserror::Error)]
pub enum OperatorInitError {
    #[error("{0} is not a did:scid:coasys:1 identity")]
    NotAnIdentity(String),
    /// The record names another identity (`init --operator` stops with this message).
    #[error("This executor's operator is {0}. Only that identity can change it. To start with a new operator, use a new data folder.")]
    Recorded(String),
    #[error("the executor database: {0}")]
    Database(String),
}

/// Records `did` as the operator in the executor database at `db_path` while the record is
/// empty. The same identity again is fine; another one stops with
/// [`OperatorInitError::Recorded`]'s message.
pub fn record_operator_at_init(
    db_path: &Path,
    did: &str,
    now: u64,
) -> Result<OperatorRecordRow, OperatorInitError> {
    if ad4m_identity_core::scid_of(did).is_none() {
        return Err(OperatorInitError::NotAnIdentity(did.to_string()));
    }
    let conn = rusqlite::Connection::open(db_path)
        .map_err(|e| OperatorInitError::Database(e.to_string()))?;
    let record = crate::db::set_operator_record_if_empty_in(&conn, did, now, OperatorSetBy::Cli)
        .map_err(|e| OperatorInitError::Database(e.to_string()))?;
    if record.scid != did {
        return Err(OperatorInitError::Recorded(record.scid));
    }
    Ok(record)
}

#[cfg(test)]
mod tests {
    use super::*;

    const A: &str = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";
    const B: &str = "did:scid:coasys:1:QmT78zSuBmuS4z925WZfrqQ1qHaJ56DQaTfyMUF7F8ff5o";

    fn operator_at(db_path: &Path) -> Option<OperatorRecordRow> {
        let conn = rusqlite::Connection::open(db_path).unwrap();
        crate::db::operator_record_in(&conn).unwrap()
    }

    fn record(scid: &str) -> OperatorRecordRow {
        OperatorRecordRow {
            scid: scid.into(),
            set_at: 1,
            set_by: OperatorSetBy::Cli,
        }
    }

    #[test]
    fn admits_first_link_only_for_the_recorded_identity() {
        assert!(!admits_first_link(None, A));
        assert!(admits_first_link(Some(&record(A)), A));
        assert!(!admits_first_link(Some(&record(A)), B));
    }

    #[test]
    fn the_first_link_check_refuses_no_operator_and_a_stranger() {
        for first_run in [false, true] {
            assert_eq!(
                first_link_check(Some(&record(A)), A, first_run),
                Ok(FirstLink::Operator)
            );
            // Once a record exists, a desktop's first run changes nothing.
            let refusal = first_link_check(Some(&record(A)), B, first_run).unwrap_err();
            assert!(matches!(refusal, FirstLinkRefusal::FirstLink { .. }));
            assert!(refusal.to_string().starts_with("first-link"));
        }
        assert_eq!(
            first_link_check(None, A, false),
            Err(FirstLinkRefusal::NoOperator)
        );
        // A desktop app's executor at its first run: any offer may be claimed; the join records.
        assert_eq!(first_link_check(None, A, true), Ok(FirstLink::FirstRun));
    }

    #[test]
    fn the_main_key_co_signs_an_incept_only_without_an_operator() {
        assert_eq!(main_incept_check(None, B), Ok(()));
        let refusal = main_incept_check(Some(&record(A)), B).unwrap_err();
        assert!(matches!(refusal, FirstLinkRefusal::OperatorIncept { .. }));
        assert!(refusal.to_string().starts_with("first-link"), "{refusal}");
        assert_eq!(main_incept_check(Some(&record(A)), A), Ok(()));
    }

    #[test]
    fn init_records_the_operator_once() {
        let dir = tempfile::tempdir().unwrap();
        let db = dir.path().join("ad4m_db.sqlite");
        assert!(matches!(
            record_operator_at_init(&db, "did:key:z6Mk", 1),
            Err(OperatorInitError::NotAnIdentity(_))
        ));
        assert_eq!(operator_at(&db), None);
        assert_eq!(record_operator_at_init(&db, A, 1).unwrap().scid, A);
        // The same identity again is fine; the record keeps its first time.
        assert_eq!(record_operator_at_init(&db, A, 2).unwrap().set_at, 1);
        match record_operator_at_init(&db, B, 3) {
            Err(OperatorInitError::Recorded(held)) => assert_eq!(held, A),
            other => panic!("expected the stop message, got {other:?}"),
        }
        let msg = record_operator_at_init(&db, B, 3).unwrap_err().to_string();
        assert!(msg.contains("Only that identity can change it"), "{msg}");
        assert_eq!(operator_at(&db).unwrap().scid, A);
    }
}
