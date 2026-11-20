//! `host.usage`'s figures (AR-05 4.6): spaces, private perspectives, storage, AI runs, spend.

use std::collections::HashSet;

use super::*;
use crate::db::TenantRow;
use crate::perspectives::interpretation_test_support::ensure_db_init;
use crate::types::{ExpressionProof, Link, LinkExpression, LinkStatus, PerspectiveState};

const ME: &str = "did:key:meter-me";
const OTHER: &str = "did:key:meter-other";
const T0: u64 = 2_000_000;
const OPERATOR: &str = "did:key:meter-operator";

fn row(id: &str, device: &str) -> TenantRow {
    TenantRow {
        id: id.into(),
        did_scid: format!("did:scid:coasys:1:{id}"),
        device_key_name: Some(format!("user:{id}")),
        device_did: Some(device.into()),
        state: "active".into(),
        joined_via: "\"anyone\"".into(),
        created_at: 10,
        activated_at: Some(11),
        detached_at: None,
        pending_expires_at: None,
    }
}

fn world() -> Tenant {
    ensure_db_init();
    crate::db::Ad4mDb::with_global_instance(|db| {
        db.clear_hosting_for_test().unwrap();
        db.clear_usage_for_test().unwrap();
    });
    forget_storage("t-me");
    let db = crate::db_backend::db_backend();
    db.insert_tenant(&row("t-me", ME)).unwrap();
    db.insert_tenant(&row("t-other", OTHER)).unwrap();
    Tenant::of(db.tenant("t-me").unwrap().unwrap()).unwrap()
}

fn handle(uuid: &str, url: Option<&str>, owners: &[&str]) -> PerspectiveHandle {
    PerspectiveHandle {
        uuid: uuid.into(),
        name: Some(format!("name-{uuid}")),
        neighbourhood: None,
        shared_url: url.map(str::to_string),
        state: if url.is_some() {
            PerspectiveState::NeighbourhoodCreationInitiated
        } else {
            PerspectiveState::Private
        },
        owners: Some(owners.iter().map(|o| o.to_string()).collect()),
    }
}

fn link(source: &str, target: &str) -> LinkExpression {
    LinkExpression {
        author: "did:key:a".into(),
        timestamp: format!("2026-01-01T00:00:{:02}Z", target.len() % 60),
        data: Link {
            source: source.into(),
            predicate: Some("p".into()),
            target: target.into(),
        },
        proof: ExpressionProof {
            signature: "sig".into(),
            key: "key".into(),
            anchor: None,
        },
        status: None,
    }
}

fn devices() -> HashSet<String> {
    [ME, OTHER].iter().map(|d| d.to_string()).collect()
}

#[test]
fn spaces_and_private_perspectives_are_counted_and_a_shared_space_says_so() {
    let me = world();
    let owned = vec![
        handle("s-alone", Some("neighbourhood://a"), &[ME]),
        handle("s-shared", Some("neighbourhood://b"), &[ME, OTHER]),
        handle("p-1", None, &[ME]),
        handle("p-2", None, &[ME]),
    ];
    let usage = usage_of(&me, &owned, &devices(), OPERATOR, T0).unwrap();
    assert_eq!(usage.private_perspectives, 2);
    assert_eq!(usage.spaces.len(), 2);
    assert_eq!(usage.spaces[0].neighbourhood_url, "neighbourhood://a");
    assert!(!usage.spaces[0].shared_with_other_tenants);
    assert!(usage.spaces[1].shared_with_other_tenants);
    assert_eq!(usage.tenant_id, "t-me");
    assert_eq!(usage.state, "active");
    assert_eq!(usage.host.operator_did, OPERATOR);
}

#[test]
fn storage_is_the_bytes_of_the_link_rows_of_the_tenants_perspectives_only() {
    let me = world();
    let db = crate::db_backend::db_backend();
    db.add_link("p-mine", &link("a", "b"), &LinkStatus::Local)
        .unwrap();
    db.add_link("p-foreign", &link("aaaa", "bbbbbbbb"), &LinkStatus::Local)
        .unwrap();
    // source a, predicate p, target b, author "did:key:a" (9), signature sig, key key.
    let row_bytes = 1 + 1 + 1 + 9 + 3 + 3;
    let owned = vec![handle("p-mine", None, &[ME])];
    let usage = usage_of(&me, &owned, &devices(), OPERATOR, T0).unwrap();
    assert_eq!(usage.storage.used_bytes, row_bytes);
    assert_eq!(usage.storage.measured_at, "1970-01-24T03:33:20Z");
}

#[test]
fn a_measured_total_stands_for_fifteen_minutes_then_is_measured_again() {
    let me = world();
    let db = crate::db_backend::db_backend();
    let owned = vec![handle("p-grow", None, &[ME])];
    assert_eq!(
        usage_of(&me, &owned, &devices(), OPERATOR, T0)
            .unwrap()
            .storage
            .used_bytes,
        0
    );
    db.add_link("p-grow", &link("a", "b"), &LinkStatus::Local)
        .unwrap();
    let within = usage_of(&me, &owned, &devices(), OPERATOR, T0 + REMEASURE_SECS - 1).unwrap();
    assert_eq!(within.storage.used_bytes, 0, "still the measured total");
    let after = usage_of(&me, &owned, &devices(), OPERATOR, T0 + REMEASURE_SECS).unwrap();
    assert_eq!(after.storage.used_bytes, 18);
}

#[test]
fn ai_runs_count_for_a_tenant_today_and_not_for_anyone_else() {
    let me = world();
    count_ai_run(Some(ME));
    count_ai_run(Some(ME));
    count_ai_run(Some("did:key:no-record"));
    count_ai_run(None);
    let usage = usage_of(&me, &[], &devices(), OPERATOR, T0).unwrap();
    assert_eq!(usage.ai_runs.count, 2);
    assert_eq!(
        usage.ai_runs.day,
        chrono::Utc::now().format("%Y-%m-%d").to_string()
    );
    let other = Tenant::of(
        crate::db_backend::db_backend()
            .tenant("t-other")
            .unwrap()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        usage_of(&other, &[], &devices(), OPERATOR, T0)
            .unwrap()
            .ai_runs
            .count,
        0
    );
}

#[test]
fn spend_sums_the_tenants_compute_log_and_shows_free_access() {
    let me = world();
    let db = crate::db_backend::db_backend();
    db.insert_compute_log(ME, "ai", None, 1.5, 8.5).unwrap();
    db.insert_compute_log(ME, "ai", None, 0.25, 8.25).unwrap();
    db.insert_compute_log(OTHER, "ai", None, 9.0, 1.0).unwrap();
    db.set_free_hosting_enabled(false).unwrap();
    let spend = usage_of(&me, &[], &devices(), OPERATOR, T0).unwrap().spend;
    assert_eq!(spend.charged_so_far, 1.75);
    assert!(!spend.free_access);
    db.set_free_hosting_enabled(true).unwrap();
    assert!(
        usage_of(&me, &[], &devices(), OPERATOR, T0)
            .unwrap()
            .spend
            .free_access
    );
}
