//! The gate, the states, the sweep, leave and removal, against a fake `HostEnv`.

use std::collections::HashMap;
use std::sync::Mutex;

use async_trait::async_trait;

use super::*;
use crate::hosting::policy::{HostPolicyPatch, SignupPolicy};
use crate::hosting::{ClaimFacts, LogFacts};
use crate::perspectives::interpretation_test_support::ensure_db_init;

/// An env whose answers a test sets and whose effects it reads back.
struct FakeEnv {
    now: Mutex<u64>,
    multi_user: bool,
    logs: Mutex<HashMap<String, Result<LogFacts, HostRefusal>>>,
    claim: Mutex<Result<ClaimFacts, HostRefusal>>,
    standing: Mutex<KeyStanding>,
    calls: Mutex<Vec<String>>,
    events: Mutex<Vec<HostEvent>>,
    fail_device: bool,
    made: Mutex<u32>,
}

const T0: u64 = 1_000_000;

impl FakeEnv {
    fn new() -> Self {
        FakeEnv {
            now: Mutex::new(T0),
            multi_user: true,
            logs: Mutex::new(HashMap::new()),
            claim: Mutex::new(Ok(ClaimFacts {
                is_node: true,
                expires: T0 + 300,
            })),
            standing: Mutex::new(KeyStanding::Unknown),
            calls: Mutex::new(vec![]),
            events: Mutex::new(vec![]),
            fail_device: false,
            made: Mutex::new(0),
        }
    }

    fn knows(&self, did: &str, vouched: bool) {
        self.logs
            .lock()
            .unwrap()
            .insert(did.into(), Ok(LogFacts { vouched }));
    }

    fn advance(&self, secs: u64) {
        *self.now.lock().unwrap() += secs;
    }

    fn calls(&self) -> Vec<String> {
        self.calls.lock().unwrap().clone()
    }

    fn events(&self) -> Vec<HostEvent> {
        self.events.lock().unwrap().clone()
    }

    fn note(&self, call: String) {
        self.calls.lock().unwrap().push(call);
    }
}

#[async_trait]
impl HostEnv for FakeEnv {
    fn now(&self) -> u64 {
        *self.now.lock().unwrap()
    }
    fn multi_user(&self) -> bool {
        self.multi_user
    }
    async fn log_facts(&self, did_scid: &str) -> Result<LogFacts, HostRefusal> {
        self.logs
            .lock()
            .unwrap()
            .get(did_scid)
            .cloned()
            .unwrap_or(Err(HostRefusal::UnknownIdentity))
    }
    async fn make_device(
        &self,
        tenant_id: &str,
        first_join: bool,
    ) -> Result<(String, String), String> {
        if self.fail_device {
            return Err("wallet locked".into());
        }
        *self.made.lock().unwrap() += 1;
        let n = *self.made.lock().unwrap();
        self.note(format!("make {tenant_id} first={first_join}"));
        Ok((format!("key-{n}"), format!("did:key:dev{n}")))
    }
    async fn claim(&self, device_did: &str, _offer: &str) -> Result<ClaimFacts, HostRefusal> {
        self.note(format!("claim {device_did}"));
        self.claim.lock().unwrap().clone()
    }
    async fn key_standing(&self, _did_scid: &str, _device_did: &str) -> KeyStanding {
        *self.standing.lock().unwrap()
    }
    fn bind_device(&self, device_did: &str, did_scid: &str, _now: u64) -> Result<(), String> {
        self.note(format!("bind {device_did} {did_scid}"));
        Ok(())
    }
    fn end_device(&self, device_did: &str, key_name: &str) -> Result<(), String> {
        self.note(format!("end {device_did} {key_name}"));
        Ok(())
    }
    async fn undo_first_join(
        &self,
        tenant_id: &str,
        device: Option<(&str, &str)>,
    ) -> Result<(), String> {
        self.note(format!("undo {tenant_id} key={}", device.is_some()));
        Ok(())
    }
    fn guest_device(&self, username: &str) -> Result<(String, String), String> {
        Ok((
            format!("guest-{username}"),
            format!("did:key:guest-{username}"),
        ))
    }
    async fn leave_person(&self, tenant_id: &str, keep_data: bool) -> Result<LeaveResult, String> {
        self.note(format!("leave {tenant_id} keep={keep_data}"));
        Ok(LeaveResult {
            kept: keep_data,
            removed_perspectives: 0,
            left_shared_perspectives: 0,
            key_destroyed_at: None,
            shredded_everywhere_at: None,
        })
    }
    fn emit(&self, event: HostEvent) {
        self.events.lock().unwrap().push(event);
    }
}

fn fresh() {
    ensure_db_init();
    crate::db::Ad4mDb::with_global_instance(|db| {
        db.clear_hosting_for_test().unwrap();
        db.clear_setting_for_test("host_policy").unwrap();
    });
}

fn open(signup: SignupPolicy) {
    policy::set(HostPolicyPatch {
        signup: Some(signup),
        max_tenants: Some(0),
        ..Default::default()
    })
    .unwrap();
}

/// The identity of the test person `name`: a valid sha2-256 SCID made from the name.
fn did(name: &str) -> String {
    did_of(&scid(name))
}

fn scid(name: &str) -> String {
    ad4m_hash_chain::hash_bytes(name.as_bytes())
}

fn offer_for(name: &str) -> String {
    OfferString {
        store: "https://store.test".into(),
        scid: scid(name),
        offer_id: bs58::encode([5u8; 16]).into_string(),
        s: bs58::encode([7u8; 32]).into_string(),
    }
    .encode()
}

fn request(scid: &str, invite: Option<&str>) -> JoinRequest {
    JoinRequest {
        did_scid: did(scid),
        offer: offer_for(scid),
        invite_secret: invite.map(str::to_string),
    }
}

async fn join_as(
    env: &FakeEnv,
    scid: &str,
    invite: Option<&str>,
) -> Result<JoinResult, HostRefusal> {
    join(
        env,
        &RateLimiter::new(1000, 3600),
        "10.0.0.1",
        request(scid, invite),
    )
    .await
}

fn row_of(id: &str) -> Tenant {
    tenant(id).unwrap().unwrap()
}

// ---- The gate, check by check ----------------------------------------------------------

#[tokio::test]
async fn check_1_multi_user_off_is_not_a_host() {
    fresh();
    open(SignupPolicy::Anyone);
    let mut env = FakeEnv::new();
    env.multi_user = false;
    env.knows(&did("a"), true);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::NotAHost
    );
}

#[tokio::test]
async fn check_2_the_eleventh_join_in_an_hour_is_rate_limited_with_retry_after() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let limiter = RateLimiter::new(JOINS_PER_HOUR, 3600);
    for _ in 0..JOINS_PER_HOUR {
        // Unknown identities refuse at check 3, after the limiter counted the call.
        let _ = join(&env, &limiter, "10.0.0.9", request("ghost", None)).await;
    }
    env.advance(60);
    let refused = join(&env, &limiter, "10.0.0.9", request("ghost", None)).await;
    assert_eq!(
        refused.unwrap_err(),
        HostRefusal::RateLimited {
            retry_after_secs: 3540
        }
    );
    // Another address is not limited.
    let other = join(&env, &limiter, "10.0.0.8", request("ghost", None)).await;
    assert_eq!(other.unwrap_err(), HostRefusal::UnknownIdentity);
}

#[tokio::test]
async fn check_3_no_key_log_is_unknown_identity_and_a_down_store_is_store_unreachable() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    assert_eq!(
        join_as(&env, "nolog", None).await.unwrap_err(),
        HostRefusal::UnknownIdentity
    );
    env.logs
        .lock()
        .unwrap()
        .insert(did("down"), Err(HostRefusal::StoreUnreachable));
    assert_eq!(
        join_as(&env, "down", None).await.unwrap_err(),
        HostRefusal::StoreUnreachable
    );
    let mut bad = request("x", None);
    bad.did_scid = "not-a-did".into();
    let refused = join(&env, &RateLimiter::new(9, 9), "p", bad).await;
    assert_eq!(refused.unwrap_err(), HostRefusal::UnknownIdentity);
}

#[tokio::test]
async fn check_5_an_active_record_is_already_a_tenant() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let joined = join_as(&env, "a", None).await.unwrap();
    *env.standing.lock().unwrap() = KeyStanding::Valid;
    sweep(&env).await.unwrap();
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Active);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::AlreadyATenant
    );
}

#[tokio::test]
async fn check_6_no_one_refuses_and_a_new_executor_starts_there() {
    fresh();
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    // No policy row at all: the start value.
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::SignupsClosed
    );
    open(SignupPolicy::NoOne);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::SignupsClosed
    );
    assert!(env.calls().is_empty(), "nothing is made for a refused join");
}

#[tokio::test]
async fn check_6_by_invite_needs_an_invite_that_is_open() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );
    assert_eq!(
        join_as(&env, "a", Some("AAAA")).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );
    assert_eq!(
        join_as(&env, "a", Some("not base64!")).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );

    let (id, secret) = create_invite(&env, "operator", None, None).unwrap();
    let joined = join_as(&env, "a", Some(&secret)).await.unwrap();
    assert_eq!(
        row_of(&joined.tenant_id).row.joined_via,
        JoinRoute::Invite {
            invite_id: id.clone()
        }
        .json()
    );
    // It is spent: another identity cannot use it.
    env.knows(&did("b"), true);
    assert_eq!(
        join_as(&env, "b", Some(&secret)).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );
}

#[tokio::test]
async fn check_6_a_revoked_or_expired_invite_is_invalid() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let (id, secret) = create_invite(&env, "operator", Some(1), None).unwrap();
    assert!(revoke_invite(&id).unwrap());
    assert_eq!(
        join_as(&env, "a", Some(&secret)).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );

    let (_, expiring) = create_invite(&env, "operator", Some(1), None).unwrap();
    env.advance(86_400);
    assert_eq!(
        join_as(&env, "a", Some(&expiring)).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );
}

#[tokio::test]
async fn check_6_anyone_takes_a_join_without_an_invite() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let joined = join_as(&env, "a", None).await.unwrap();
    assert_eq!(
        row_of(&joined.tenant_id).row.joined_via,
        JoinRoute::Anyone.json()
    );
    assert!(!joined.rejoin);
}

#[tokio::test]
async fn check_7_the_cap_counts_active_and_pending() {
    fresh();
    open(SignupPolicy::Anyone);
    policy::set(HostPolicyPatch {
        max_tenants: Some(1),
        ..Default::default()
    })
    .unwrap();
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    env.knows(&did("b"), true);
    join_as(&env, "a", None).await.unwrap();
    assert_eq!(
        join_as(&env, "b", None).await.unwrap_err(),
        HostRefusal::HostFull
    );
    assert!(is_full().unwrap());
}

#[tokio::test]
async fn check_8_no_vouching_note_is_email_check_required() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), false);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::EmailCheckRequired
    );
    assert!(env.calls().is_empty());
}

#[tokio::test]
async fn check_9_an_offer_that_does_not_parse_or_names_another_identity_is_offer_invalid() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let mut garbled = request("a", None);
    garbled.offer = "coasys:offer:1:zzz".into();
    let limiter = RateLimiter::new(99, 9);
    assert_eq!(
        join(&env, &limiter, "p", garbled).await.unwrap_err(),
        HostRefusal::OfferInvalid
    );
    let mut other = request("a", None);
    other.offer = offer_for("someone-else");
    assert_eq!(
        join(&env, &limiter, "p", other).await.unwrap_err(),
        HostRefusal::OfferInvalid
    );
    assert!(env.calls().is_empty(), "no key is made before check 9");
}

#[tokio::test]
async fn the_gate_stops_at_the_first_refusal_in_order() {
    fresh();
    // Setting closed, identity not vouched, offer garbage: the setting answers first.
    let env = FakeEnv::new();
    env.knows(&did("a"), false);
    let mut bad = request("a", None);
    bad.offer = "junk".into();
    let refused = join(&env, &RateLimiter::new(9, 9), "p", bad).await;
    assert_eq!(refused.unwrap_err(), HostRefusal::SignupsClosed);
    // Open, still unvouched and garbage: the email check answers before the offer.
    open(SignupPolicy::Anyone);
    let mut bad = request("a", None);
    bad.offer = "junk".into();
    let refused = join(&env, &RateLimiter::new(9, 9), "p", bad).await;
    assert_eq!(refused.unwrap_err(), HostRefusal::EmailCheckRequired);
}

// ---- The join sequence ---------------------------------------------------------------------

#[tokio::test]
async fn a_join_makes_a_pending_record_a_key_and_a_claim_and_activates_when_the_key_is_listed() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let joined = join_as(&env, "a", None).await.unwrap();
    let pending = row_of(&joined.tenant_id);
    assert_eq!(pending.state, TenantState::Pending);
    assert_eq!(pending.row.pending_expires_at, Some(T0 + PENDING_SECS));
    assert_eq!(
        pending.row.device_did.as_deref(),
        Some(joined.device_did.as_str())
    );
    assert_eq!(
        env.calls(),
        vec![
            format!("make {} first=true", joined.tenant_id),
            format!("claim {}", joined.device_did)
        ]
    );
    assert_eq!(join_status(&joined.join_handle).unwrap().state, "pending");

    // The key is not listed yet: nothing changes.
    assert_eq!(sweep(&env).await.unwrap(), SweepReport::default());
    *env.standing.lock().unwrap() = KeyStanding::Valid;
    let report = sweep(&env).await.unwrap();
    assert_eq!(report.activated, vec![joined.tenant_id.clone()]);
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Active);
    assert_eq!(join_status(&joined.join_handle).unwrap().state, "active");
    assert!(env
        .calls()
        .contains(&format!("bind {} {}", joined.device_did, did("a"))));
    assert_eq!(
        env.events(),
        vec![HostEvent::TenantJoined {
            tenant_id: joined.tenant_id,
            did_scid: did("a"),
            route: "anyone".into()
        }]
    );
}

#[tokio::test]
async fn an_offer_that_is_not_a_node_offer_or_expired_removes_the_record_and_unspends_the_invite() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let (id, secret) = create_invite(&env, "operator", None, None).unwrap();
    *env.claim.lock().unwrap() = Ok(ClaimFacts {
        is_node: false,
        expires: T0 + 300,
    });
    assert_eq!(
        join_as(&env, "a", Some(&secret)).await.unwrap_err(),
        HostRefusal::OfferInvalid
    );
    assert!(live_tenant_of(&did("a")).unwrap().is_none());
    assert!(env
        .calls()
        .iter()
        .any(|c| c.starts_with("undo ") && c.ends_with("key=true")));
    assert_eq!(db_backend().invite(&id).unwrap().unwrap().used_by, None);

    *env.claim.lock().unwrap() = Ok(ClaimFacts {
        is_node: true,
        expires: T0,
    });
    assert_eq!(
        join_as(&env, "a", Some(&secret)).await.unwrap_err(),
        HostRefusal::OfferInvalid
    );
    // The invite is whole again and a good claim works.
    *env.claim.lock().unwrap() = Ok(ClaimFacts {
        is_node: true,
        expires: T0 + 300,
    });
    assert!(join_as(&env, "a", Some(&secret)).await.is_ok());
}

#[tokio::test]
async fn a_failed_claim_removes_the_record_the_key_and_the_person_and_unspends_the_invite() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let (id, secret) = create_invite(&env, "operator", None, None).unwrap();
    *env.claim.lock().unwrap() = Err(HostRefusal::StoreUnreachable);
    assert_eq!(
        join_as(&env, "a", Some(&secret)).await.unwrap_err(),
        HostRefusal::StoreUnreachable
    );
    assert!(live_tenant_of(&did("a")).unwrap().is_none());
    assert!(env.calls().iter().any(|c| c.starts_with("undo ")));
    assert_eq!(db_backend().invite(&id).unwrap().unwrap().used_by, None);
}

#[tokio::test]
async fn a_device_that_cannot_be_made_leaves_no_record() {
    fresh();
    open(SignupPolicy::Anyone);
    let mut env = FakeEnv::new();
    env.fail_device = true;
    env.knows(&did("a"), true);
    assert!(matches!(
        join_as(&env, "a", None).await,
        Err(HostRefusal::Internal(_))
    ));
    assert!(live_tenant_of(&did("a")).unwrap().is_none());
}

#[tokio::test]
async fn a_pending_record_past_its_time_is_removed_with_its_key_and_person_and_its_invite_comes_back(
) {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let (id, secret) = create_invite(&env, "operator", None, None).unwrap();
    let joined = join_as(&env, "a", Some(&secret)).await.unwrap();
    env.advance(PENDING_SECS - 1);
    assert!(sweep(&env).await.unwrap().expired.is_empty());
    env.advance(1);
    let report = sweep(&env).await.unwrap();
    assert_eq!(report.expired, vec![joined.tenant_id.clone()]);
    assert!(tenant(&joined.tenant_id).unwrap().is_none());
    assert!(env
        .calls()
        .iter()
        .any(|c| c == &format!("undo {} key=true", joined.tenant_id)));
    assert_eq!(db_backend().invite(&id).unwrap().unwrap().used_by, None);
    assert_eq!(join_status(&joined.join_handle).unwrap().state, "expired");
}

#[tokio::test]
async fn a_new_join_replaces_a_pending_record_of_the_same_identity_even_on_the_same_invite() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let (_, secret) = create_invite(&env, "operator", None, None).unwrap();
    let first = join_as(&env, "a", Some(&secret)).await.unwrap();
    let second = join_as(&env, "a", Some(&secret)).await.unwrap();
    assert_ne!(first.tenant_id, second.tenant_id);
    assert!(tenant(&first.tenant_id).unwrap().is_none());
    assert_eq!(live_tenants().unwrap().len(), 1);
    assert!(env
        .calls()
        .iter()
        .any(|c| c == &format!("undo {} key=true", first.tenant_id)));
}

#[tokio::test]
async fn a_pending_replacement_does_not_count_against_the_cap_twice() {
    fresh();
    open(SignupPolicy::Anyone);
    policy::set(HostPolicyPatch {
        max_tenants: Some(1),
        ..Default::default()
    })
    .unwrap();
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    join_as(&env, "a", None).await.unwrap();
    assert!(join_as(&env, "a", None).await.is_ok());
}

// ---- Revocation and rejoin -------------------------------------------------------------------

async fn active(env: &FakeEnv, scid: &str) -> JoinResult {
    env.knows(&did(scid), true);
    let joined = join_as(env, scid, None).await.unwrap();
    *env.standing.lock().unwrap() = KeyStanding::Valid;
    sweep(env).await.unwrap();
    *env.standing.lock().unwrap() = KeyStanding::Unknown;
    joined
}

#[tokio::test]
async fn the_pending_poll_does_not_ask_about_active_keys() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let joined = active(&env, "a").await;
    *env.standing.lock().unwrap() = KeyStanding::Gone;
    assert_eq!(sweep_pending(&env).await.unwrap(), SweepReport::default());
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Active);
    // The full pass does.
    assert_eq!(sweep(&env).await.unwrap().detached, vec![joined.tenant_id]);
}

#[tokio::test]
async fn the_sweep_detaches_an_active_tenant_whose_key_is_gone_and_keeps_it_on_no_verdict() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let joined = active(&env, "a").await;
    // The log cannot be read: no change.
    assert_eq!(sweep(&env).await.unwrap(), SweepReport::default());
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Active);

    *env.standing.lock().unwrap() = KeyStanding::Gone;
    let report = sweep(&env).await.unwrap();
    assert_eq!(report.detached, vec![joined.tenant_id.clone()]);
    let detached = row_of(&joined.tenant_id);
    assert_eq!(detached.state, TenantState::Detached);
    assert_eq!(detached.row.device_key_name, None, "the wallet key is gone");
    assert_eq!(
        detached.row.device_did.as_deref(),
        Some(joined.device_did.as_str()),
        "the record remembers its last device"
    );
    assert!(env.calls().iter().any(|c| c.starts_with("end did:key:dev")));
    assert_eq!(
        env.events().last(),
        Some(&HostEvent::TenantLeft {
            tenant_id: joined.tenant_id,
            did_scid: did("a"),
            kept: true,
            cause: "revoked".into()
        })
    );
}

#[tokio::test]
async fn a_detached_tenant_rejoins_with_a_new_key_and_keeps_its_person() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let first = active(&env, "a").await;
    leave(&env, &first.tenant_id, true).await.unwrap();
    assert_eq!(row_of(&first.tenant_id).state, TenantState::Detached);

    let again = join_as(&env, "a", None).await.unwrap();
    assert!(again.rejoin);
    assert_eq!(again.tenant_id, first.tenant_id);
    assert_ne!(again.device_did, first.device_did);
    assert!(env
        .calls()
        .contains(&format!("make {} first=false", first.tenant_id)));
    let pending = row_of(&again.tenant_id);
    assert_eq!(pending.state, TenantState::Pending);
    assert_eq!(
        JoinRoute::of(&pending.row).unwrap(),
        JoinRoute::Rejoin {
            previous_device_did: first.device_did.clone()
        }
    );
    *env.standing.lock().unwrap() = KeyStanding::Valid;
    sweep(&env).await.unwrap();
    assert_eq!(row_of(&again.tenant_id).state, TenantState::Active);
}

#[tokio::test]
async fn a_rejoin_passes_the_setting_like_a_first_join() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let first = active(&env, "a").await;
    leave(&env, &first.tenant_id, true).await.unwrap();
    open(SignupPolicy::NoOne);
    assert_eq!(
        join_as(&env, "a", None).await.unwrap_err(),
        HostRefusal::SignupsClosed
    );
}

#[tokio::test]
async fn an_abandoned_rejoin_returns_to_detached_with_its_data() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let first = active(&env, "a").await;
    leave(&env, &first.tenant_id, true).await.unwrap();
    let again = join_as(&env, "a", None).await.unwrap();
    env.advance(PENDING_SECS);
    sweep(&env).await.unwrap();
    assert_eq!(row_of(&again.tenant_id).state, TenantState::Detached);
    assert!(
        !env.calls().iter().any(|c| c.starts_with("undo ")),
        "the person stays"
    );
}

// ---- Leave and removal ----------------------------------------------------------------------

#[tokio::test]
async fn leave_keep_ends_the_key_first_then_detaches_and_keeps_the_data() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let joined = active(&env, "a").await;
    let result = leave(&env, &joined.tenant_id, true).await.unwrap();
    assert!(result.kept);
    let calls = env.calls();
    let end = calls.iter().position(|c| c.starts_with("end ")).unwrap();
    let left = calls.iter().position(|c| c.starts_with("leave ")).unwrap();
    assert!(end < left, "the key ends before the data is touched");
    assert!(calls.contains(&format!("leave {} keep=true", joined.tenant_id)));
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Detached);
    assert!(!is_leaving(&joined.tenant_id));
    assert_eq!(
        env.events().last(),
        Some(&HostEvent::TenantLeft {
            tenant_id: joined.tenant_id,
            did_scid: did("a"),
            kept: true,
            cause: "leave".into()
        })
    );
}

#[tokio::test]
async fn leave_delete_tombstones_the_record_and_frees_the_identity_for_a_new_join() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let joined = active(&env, "a").await;
    db_backend()
        .count_ai_run(&joined.tenant_id, "2026-11-20")
        .unwrap();
    leave(&env, &joined.tenant_id, false).await.unwrap();
    assert!(env
        .calls()
        .contains(&format!("leave {} keep=false", joined.tenant_id)));
    let gone = row_of(&joined.tenant_id);
    assert_eq!(gone.state, TenantState::Deleted);
    assert_eq!(
        gone.row.did_scid,
        did_hash(&did("a")),
        "only a hash of the identity stays"
    );
    assert_eq!(
        db_backend()
            .ai_runs(&joined.tenant_id, "2026-11-20")
            .unwrap(),
        0
    );
    assert!(live_tenant_of(&did("a")).unwrap().is_none());
    let again = join_as(&env, "a", None).await.unwrap();
    assert!(!again.rejoin);
    assert_ne!(again.tenant_id, joined.tenant_id);
}

#[tokio::test]
async fn only_an_active_or_held_tenant_leaves() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("a"), true);
    let pending = join_as(&env, "a", None).await.unwrap();
    assert_eq!(
        leave(&env, &pending.tenant_id, true).await.unwrap_err(),
        HostRefusal::NotATenant
    );
    assert_eq!(
        leave(&env, "t-nobody", true).await.unwrap_err(),
        HostRefusal::NotATenant
    );
}

#[tokio::test]
async fn remove_tenant_deletes_a_detached_tenants_data_and_refuses_the_others() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    let joined = active(&env, "a").await;
    assert_eq!(
        remove_tenant(&env, &joined.tenant_id).await.unwrap_err(),
        HostRefusal::AlreadyATenant
    );
    leave(&env, &joined.tenant_id, true).await.unwrap();
    assert!(remove_tenant(&env, &joined.tenant_id).await.unwrap());
    assert!(env
        .calls()
        .contains(&format!("leave {} keep=false", joined.tenant_id)));
    assert_eq!(row_of(&joined.tenant_id).state, TenantState::Deleted);
    assert_eq!(
        env.events().last(),
        Some(&HostEvent::TenantLeft {
            tenant_id: joined.tenant_id.clone(),
            did_scid: did("a"),
            kept: false,
            cause: "removed".into()
        })
    );
    assert!(!remove_tenant(&env, &joined.tenant_id).await.unwrap());
    assert!(!remove_tenant(&env, "t-nobody").await.unwrap());
}

// ---- A claimed guest ----------------------------------------------------------------------

#[tokio::test]
async fn a_claimed_guest_is_admitted_under_anyone_and_keeps_its_username_as_the_id() {
    fresh();
    open(SignupPolicy::Anyone);
    let env = FakeEnv::new();
    env.knows(&did("g"), true);
    let admitted = admit_claimed_guest(&env, "guest-1", &did("g")).await;
    assert_eq!(
        admitted,
        ClaimAdmission::Admitted {
            tenant_id: "guest-1".into()
        }
    );
    let t = row_of("guest-1");
    assert_eq!(t.state, TenantState::Active);
    assert_eq!(JoinRoute::of(&t.row).unwrap(), JoinRoute::GuestClaim);
    assert_eq!(t.row.device_did.as_deref(), Some("did:key:guest-guest-1"));
    assert!(env
        .calls()
        .contains(&format!("bind did:key:guest-guest-1 {}", did("g"))));
    assert_eq!(env.events().len(), 1);
}

#[tokio::test]
async fn a_claimed_guest_is_held_when_the_setting_or_the_capacity_does_not_admit_it() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("g"), true);
    let held = admit_claimed_guest(&env, "guest-1", &did("g")).await;
    assert_eq!(
        held,
        ClaimAdmission::HeldOnly {
            tenant_id: "guest-1".into(),
            reason: HostRefusal::SignupsClosed
        }
    );
    assert_eq!(row_of("guest-1").state, TenantState::Held);
    assert!(
        env.events().is_empty(),
        "a held guest is not a joined tenant"
    );

    open(SignupPolicy::Anyone);
    policy::set(HostPolicyPatch {
        max_tenants: Some(1),
        ..Default::default()
    })
    .unwrap();
    env.knows(&did("h"), true);
    let first = admit_claimed_guest(&env, "guest-2", &did("h")).await;
    assert!(matches!(first, ClaimAdmission::Admitted { .. }));
    env.knows(&did("i"), true);
    let full = admit_claimed_guest(&env, "guest-3", &did("i")).await;
    assert_eq!(
        full,
        ClaimAdmission::HeldOnly {
            tenant_id: "guest-3".into(),
            reason: HostRefusal::HostFull
        }
    );
}

#[tokio::test]
async fn a_claimed_guest_makes_no_record_for_checks_1_3_5_and_8() {
    fresh();
    open(SignupPolicy::Anyone);
    let mut off = FakeEnv::new();
    off.multi_user = false;
    off.knows(&did("g"), true);
    assert_eq!(
        admit_claimed_guest(&off, "g1", &did("g")).await,
        ClaimAdmission::Refused(HostRefusal::NotAHost)
    );
    let env = FakeEnv::new();
    assert_eq!(
        admit_claimed_guest(&env, "g1", &did("g")).await,
        ClaimAdmission::Refused(HostRefusal::UnknownIdentity)
    );
    env.knows(&did("g"), false);
    assert_eq!(
        admit_claimed_guest(&env, "g1", &did("g")).await,
        ClaimAdmission::Refused(HostRefusal::EmailCheckRequired)
    );
    env.knows(&did("g"), true);
    assert!(matches!(
        admit_claimed_guest(&env, "g1", &did("g")).await,
        ClaimAdmission::Admitted { .. }
    ));
    assert_eq!(
        admit_claimed_guest(&env, "g2", &did("g")).await,
        ClaimAdmission::Refused(HostRefusal::AlreadyATenant)
    );
    assert!(tenant("g2").unwrap().is_none());
}

#[tokio::test]
async fn a_held_tenant_turns_active_on_a_join_that_passes_the_gate_with_no_new_key() {
    fresh();
    open(SignupPolicy::ByInvite);
    let env = FakeEnv::new();
    env.knows(&did("g"), true);
    admit_claimed_guest(&env, "guest-1", &did("g")).await;
    let calls_before = env.calls().len();
    // Still ByInvite and no invite: refused.
    assert_eq!(
        join_as(&env, "g", None).await.unwrap_err(),
        HostRefusal::InviteInvalid
    );
    open(SignupPolicy::Anyone);
    let joined = join_as(&env, "g", None).await.unwrap();
    assert_eq!(joined.tenant_id, "guest-1");
    assert_eq!(joined.device_did, "did:key:guest-guest-1");
    assert_eq!(row_of("guest-1").state, TenantState::Active);
    assert!(
        !env.calls()[calls_before..]
            .iter()
            .any(|c| c.starts_with("make ") || c.starts_with("claim ")),
        "a held record is activated with its key; no offer is claimed"
    );
}

// ---- Invites ------------------------------------------------------------------------------

#[test]
fn a_host_keeps_at_most_a_hundred_open_invites() {
    fresh();
    let env = FakeEnv::new();
    for _ in 0..MAX_OPEN_INVITES {
        create_invite(&env, "operator", None, None).unwrap();
    }
    assert!(create_invite(&env, "operator", None, None).is_err());
    let first = list_invites().unwrap().remove(0);
    revoke_invite(&first.id).unwrap();
    assert!(create_invite(&env, "operator", None, None).is_ok());
}

#[test]
fn an_invite_stores_only_the_hash_of_its_secret_and_expires_in_one_to_thirty_days() {
    fresh();
    let env = FakeEnv::new();
    let (id, secret) = create_invite(&env, "operator", Some(30), Some("for Sam".into())).unwrap();
    let row = db_backend().invite(&id).unwrap().unwrap();
    assert_eq!(row.expires_at, T0 + 30 * 86_400);
    assert_eq!(row.secret_hash, secret_hash(&secret).unwrap());
    assert_ne!(row.secret_hash, secret.as_bytes());
    assert!(create_invite(&env, "operator", Some(0), None).is_err());
    assert!(create_invite(&env, "operator", Some(31), None).is_err());
}

#[test]
fn the_rate_limiter_forgets_calls_that_left_the_window() {
    let limiter = RateLimiter::new(2, 10);
    limiter.check("a", 100).unwrap();
    limiter.check("a", 105).unwrap();
    assert_eq!(
        limiter.check("a", 106).unwrap_err(),
        HostRefusal::RateLimited {
            retry_after_secs: 4
        }
    );
    limiter.check("a", 110).unwrap();
}

#[test]
fn every_refusal_has_its_code_and_status() {
    let table = [
        (HostRefusal::NotAHost, "not-a-host", 404),
        (HostRefusal::SignupsClosed, "signups-closed", 403),
        (HostRefusal::InviteInvalid, "invite-invalid", 403),
        (HostRefusal::EmailCheckRequired, "email-check-required", 403),
        (HostRefusal::AlreadyATenant, "already-a-tenant", 409),
        (HostRefusal::HostFull, "host-full", 503),
        (HostRefusal::UnknownIdentity, "unknown-identity", 404),
        (HostRefusal::OfferInvalid, "offer-invalid", 400),
        (
            HostRefusal::RateLimited {
                retry_after_secs: 7,
            },
            "rate-limited",
            429,
        ),
        (HostRefusal::StoreUnreachable, "store-unreachable", 503),
        (HostRefusal::PinnedByConfig, "pinned-by-config", 409),
        (HostRefusal::NotAdmitted, "not-admitted", 403),
        (HostRefusal::NotATenant, "not-a-tenant", 403),
    ];
    for (refusal, code, status) in table {
        assert_eq!(refusal.code(), code);
        assert_eq!(refusal.status(), status);
        assert!(refusal.to_string().starts_with(&format!("{code}: ")));
        let rpc: crate::api::ws_handler::WsRpcError = refusal.into();
        assert_eq!(rpc.code, status);
    }
    let limited: crate::api::ws_handler::WsRpcError = HostRefusal::RateLimited {
        retry_after_secs: 7,
    }
    .into();
    assert_eq!(limited.data, Some(serde_json::json!({ "retryAfter": 7 })));
}
