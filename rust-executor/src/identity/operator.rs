//! The operator record and the first-link rule (AR-04 4.9, decision D5 of decisions §20.3).
//!
//! The record names the identity that operates this executor: `ad4m-executor init --operator
//! <did:scid>` sets it while it is empty, and a different value never replaces it (decision
//! 22.7: only the recorded operator changes it, through `runtime.setOperator`, which slice 1
//! does not have). Every claim for the main agent while its key is in no log runs
//! [`first_link_check`] (in `ClaimService::claim_for_main`): only an offer of the recorded
//! identity passes.

use std::path::Path;

use crate::db::{OperatorRecordRow, OperatorSetBy};

/// Why a claim for the main agent is refused before it reaches the store.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum FirstLinkRefusal {
    /// No operator record: a server started with no `--operator` opens no link route.
    #[error("no-operator: this executor has no operator; start it with `ad4m-executor init --operator <did:scid>`")]
    NoOperator,
    /// The main key is in no log and the offer is not of the recorded operator's identity.
    #[error("first-link: this executor's first link must come from its operator {operator}, not from {offer}")]
    FirstLink { operator: String, offer: String },
}

impl FirstLinkRefusal {
    /// The refusal's name on the wire (`no-operator`, `first-link`).
    pub fn name(&self) -> &'static str {
        match self {
            FirstLinkRefusal::NoOperator => "no-operator",
            FirstLinkRefusal::FirstLink { .. } => "first-link",
        }
    }
}

/// `OperatorRecord::admits_first_link` (AR-04 4.9.3): true only when the record is set and
/// names `offer_did`.
pub fn admits_first_link(record: Option<&OperatorRecordRow>, offer_did: &str) -> bool {
    record.is_some_and(|r| r.scid == offer_did)
}

/// The checks `claim_for_main` runs before it claims: an operator record must exist, and while
/// the main key is in no valid log (`linked` false) the offer must be of the recorded identity.
pub fn first_link_check(
    record: Option<&OperatorRecordRow>,
    linked: bool,
    offer_did: &str,
) -> Result<(), FirstLinkRefusal> {
    let Some(record) = record else {
        return Err(FirstLinkRefusal::NoOperator);
    };
    if linked || admits_first_link(Some(record), offer_did) {
        return Ok(());
    }
    Err(FirstLinkRefusal::FirstLink {
        operator: record.scid.clone(),
        offer: offer_did.to_string(),
    })
}

/// Why `init --operator` did not record the operator.
#[derive(Debug, thiserror::Error)]
pub enum OperatorInitError {
    #[error("{0} is not a did:scid:coasys:1 identity")]
    NotAnIdentity(String),
    /// The record names another identity (AR-04 4.9.2's stop message).
    #[error("This executor's operator is {0}. Only that identity can change it. To start with a new operator, use a new data folder.")]
    Recorded(String),
    #[error("the executor database: {0}")]
    Database(String),
}

/// Records `did` as the operator in the executor database at `db_path` while the record is
/// empty. The same identity again is fine; another one stops with AR-04's message.
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

/// The operator record in the executor database at `db_path`, if set.
pub fn operator_at(db_path: &Path) -> Result<Option<OperatorRecordRow>, OperatorInitError> {
    let conn = rusqlite::Connection::open(db_path)
        .map_err(|e| OperatorInitError::Database(e.to_string()))?;
    crate::db::operator_record_in(&conn).map_err(|e| OperatorInitError::Database(e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    const A: &str = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";
    const B: &str = "did:scid:coasys:1:QmT78zSuBmuS4z925WZfrqQ1qHaJ56DQaTfyMUF7F8ff5o";

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
        assert_eq!(
            first_link_check(None, false, A),
            Err(FirstLinkRefusal::NoOperator)
        );
        // Linked or not, no record means no link route.
        assert_eq!(
            first_link_check(None, true, A),
            Err(FirstLinkRefusal::NoOperator)
        );
        assert_eq!(first_link_check(Some(&record(A)), false, A), Ok(()));
        let refusal = first_link_check(Some(&record(A)), false, B).unwrap_err();
        assert_eq!(refusal.name(), "first-link");
        assert!(refusal.to_string().starts_with("first-link"));
        // Once the main key is in a log, later links follow AR-02, not the first-link rule.
        assert_eq!(first_link_check(Some(&record(A)), true, B), Ok(()));
    }

    #[test]
    fn init_records_the_operator_once() {
        let dir = tempfile::tempdir().unwrap();
        let db = dir.path().join("ad4m_db.sqlite");
        assert!(matches!(
            record_operator_at_init(&db, "did:key:z6Mk", 1),
            Err(OperatorInitError::NotAnIdentity(_))
        ));
        assert_eq!(operator_at(&db).unwrap(), None);
        assert_eq!(record_operator_at_init(&db, A, 1).unwrap().scid, A);
        // The same identity again is fine; the record keeps its first time.
        assert_eq!(record_operator_at_init(&db, A, 2).unwrap().set_at, 1);
        match record_operator_at_init(&db, B, 3) {
            Err(OperatorInitError::Recorded(held)) => assert_eq!(held, A),
            other => panic!("expected the stop message, got {other:?}"),
        }
        let msg = record_operator_at_init(&db, B, 3).unwrap_err().to_string();
        assert!(msg.contains("Only that identity can change it"), "{msg}");
        assert_eq!(operator_at(&db).unwrap().unwrap().scid, A);
    }
}
