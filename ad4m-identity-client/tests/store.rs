//! The real flows through `ViaExecutor<ViaStore>`: the phone's transport against a real
//! `identity-store` router in process, the way `tests/flows.rs` drives them through a test
//! executor. Both sides are `ViaStore` devices here — the "holder" is a phone too — so every
//! signed store call is checked end to end: the device key signs `X-Coasys-Request` and the
//! pre-join auth, `SoftEncKey` opens inbox messages and unwraps record keys, and `RouterHttp`
//! is the `HttpClient` a phone's surface implements.
#![cfg(not(target_arch = "wasm32"))]

use ad4m_hash_chain::Multikey;
use ad4m_identity_client::flows::create::{CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::LinkFlow;
use ad4m_identity_client::flows::master::take_master;
use ad4m_identity_client::flows::requests::{answer, ask_for_master, check_poster, Answered};
use ad4m_identity_client::flows::revoke::revoke;
use ad4m_identity_client::record::{self, Change};
use ad4m_identity_client::store::{
    EncKey, EncOpenError, HttpClient, HttpError, HttpRequest, HttpResponse, SoftEncKey,
    StoreEndpoint, ViaStore,
};
use ad4m_identity_client::{
    ClientError, ExecutorRpc, MemoryVault, OsRandom, Secret, SystemClock, Vault, ViaExecutor,
};
use ad4m_identity_core::inbox::{InboxAddress, InboxError, InboxPayload};
use ad4m_identity_core::link::{DeviceString, OfferKind};
use ad4m_identity_core::record::{
    DeviceEntry, DeviceKind, HoldsMaster, RecordError, RECORD_KEY_LEN,
};
use ad4m_identity_core::words;
use async_trait::async_trait;
use axum::body::Body;
use axum::http::Request;
use axum::Router;
use ed25519_dalek::SigningKey;
use http_body_util::BodyExt;
use identity_store::db::Db;
use identity_store::{router, system_clock, Settings, Store, StoreKey};
use serde_json::{json, Value};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tower::ServiceExt;

const STORE_URL: &str = "http://store.test";
const STORE_SECRET: [u8; 32] = [7; 32];

/// The test `HttpClient`: the store's router, in process.
struct RouterHttp {
    router: Router,
}

#[async_trait(?Send)]
impl HttpClient for RouterHttp {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError> {
        let mut builder = Request::builder()
            .method(request.method.as_str())
            .uri(request.url.as_str());
        for (name, value) in request.headers {
            builder = builder.header(name, value);
        }
        let response = self
            .router
            .clone()
            .oneshot(builder.body(Body::from(request.body)).unwrap())
            .await
            .map_err(|e| HttpError(e.to_string()))?;
        let status = response.status().as_u16();
        let headers = response
            .headers()
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_str().unwrap_or_default().to_string()))
            .collect();
        let body = response
            .into_body()
            .collect()
            .await
            .map_err(|e| HttpError(e.to_string()))?
            .to_bytes()
            .to_vec();
        Ok(HttpResponse {
            status,
            headers,
            body,
        })
    }
}

/// A store router on a temp SQLite file (as `flows.rs`'s `TestStore`).
fn store() -> (Router, tempfile::TempDir) {
    let dir = tempfile::tempdir().unwrap();
    let db = Db::open(dir.path().join("store.sqlite")).unwrap();
    let store = Store::new(
        db,
        StoreKey::from_secret(STORE_SECRET),
        system_clock(),
        Settings {
            pow_bits: 8,
            identity_burst: 100,
            ..Settings::default()
        },
    );
    (router(store), dir)
}

/// The store key's multikey, for `StoreEndpoint::keys`.
fn store_keys() -> Vec<String> {
    let did = StoreKey::from_secret(STORE_SECRET).did();
    vec![did.strip_prefix("did:key:").unwrap().to_string()]
}

/// A phone on the store: `seed` makes its Ed25519 device key and its `enc` (as an executor's
/// wallet derives both from one secret).
fn phone(router: &Router, seed: u8, identity: Option<String>) -> ViaExecutor<ViaStore> {
    let key = SigningKey::from_bytes(&[seed; 32]);
    let enc = SoftEncKey::from_ed25519(&key);
    ViaExecutor::new(ViaStore::new(
        Box::new(RouterHttp {
            router: router.clone(),
        }),
        Box::new(key),
        Box::new(enc),
        Box::new(SystemClock),
        Box::new(OsRandom),
        StoreEndpoint {
            url: STORE_URL.to_string(),
            keys: store_keys(),
        },
        identity,
    ))
}

/// An `EncKey` over `SoftEncKey` that answers `Unavailable` while `lock` is set — a keystore
/// that cannot open right now (an Android Keystore error, a locked key, an I/O failure).
struct LockableEncKey {
    inner: SoftEncKey,
    lock: Arc<AtomicBool>,
}

impl EncKey for LockableEncKey {
    fn enc(&self) -> Multikey {
        self.inner.enc()
    }

    fn open(
        &self,
        address: &InboxAddress,
        sealed: &str,
    ) -> Result<InboxPayload, EncOpenError<InboxError>> {
        if self.lock.load(Ordering::Relaxed) {
            return Err(EncOpenError::Unavailable("the keystore is locked".into()));
        }
        self.inner.open(address, sealed)
    }

    fn unwrap_record_key(
        &self,
        scid: &str,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, EncOpenError<RecordError>> {
        if self.lock.load(Ordering::Relaxed) {
            return Err(EncOpenError::Unavailable("the keystore is locked".into()));
        }
        self.inner.unwrap_record_key(scid, wrapped)
    }
}

/// A phone whose `enc` the test locks and unlocks on demand.
fn lockable_phone(router: &Router, seed: u8, lock: Arc<AtomicBool>) -> ViaExecutor<ViaStore> {
    let key = SigningKey::from_bytes(&[seed; 32]);
    let enc = LockableEncKey {
        inner: SoftEncKey::from_ed25519(&key),
        lock,
    };
    ViaExecutor::new(ViaStore::new(
        Box::new(RouterHttp {
            router: router.clone(),
        }),
        Box::new(key),
        Box::new(enc),
        Box::new(SystemClock),
        Box::new(OsRandom),
        StoreEndpoint {
            url: STORE_URL.to_string(),
            keys: store_keys(),
        },
        None,
    ))
}

/// `identity.status` of a `ViaStore` executor.
async fn status(e: &ViaExecutor<ViaStore>) -> Value {
    e.rpc().call("identity.status", json!({})).await.unwrap()
}

/// `identity.claimOffer` on the joining phone.
async fn claim(e: &ViaExecutor<ViaStore>, offer: &str, hint: Option<&str>) -> Value {
    e.rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": offer, "hint": hint, "canKeepMaster": true }),
        )
        .await
        .unwrap()
}

/// `identity.joinStatus` on the joining phone.
async fn join_status(e: &ViaExecutor<ViaStore>, offer_id: &Value) -> Value {
    e.rpc()
        .call("identity.joinStatus", json!({ "offerId": offer_id }))
        .await
        .unwrap()
}

/// `identity.inbox` on the phone: the waiting messages, by `id` and `kind` (with `did`,
/// `offerId` and `matchCode` when a ticket was claimed).
async fn inbox(e: &ViaExecutor<ViaStore>) -> Vec<Value> {
    e.rpc()
        .call("identity.inbox", json!({}))
        .await
        .unwrap()
        .as_array()
        .unwrap()
        .clone()
}

/// A `DeviceEntry` for a linked device, as `flows.rs` builds one.
fn entry(name: &str, kind: DeviceKind) -> DeviceEntry {
    DeviceEntry {
        name: name.to_string(),
        kind,
        place: None,
        address: None,
        runs_executor: false,
        holds_master: HoldsMaster::No,
        master_index: None,
        operator: None,
    }
}

fn this_phone() -> DeviceEntry {
    entry("My phone", DeviceKind::PhoneApp)
}

/// A `create` flow over fixed words, at the confirmed step.
fn confirmed(entropy: u8) -> CreateFlow {
    let (mut flow, screen) = CreateFlow::from_words(words::from_entropy(&[entropy; 16]));
    let list = shown(screen);
    let CreateScreen::ConfirmWords { positions } = flow.words_written(&OsRandom).unwrap() else {
        panic!("expected the confirm screen");
    };
    flow.confirm([&list[positions[0] - 1], &list[positions[1] - 1]])
        .unwrap();
    flow
}

fn shown(screen: CreateScreen) -> Vec<String> {
    match screen {
        CreateScreen::ShowWords(secret) => secret.expose().clone(),
        _ => panic!("expected the words"),
    }
}

/// `create` on a `ViaStore` executor; answers the identity.
async fn create(executor: &ViaExecutor<ViaStore>, vault: &MemoryVault, entropy: u8) -> String {
    confirmed(entropy)
        .finish(executor, vault, this_phone(), &OsRandom)
        .await
        .unwrap()
        .did
}

/// The SCID of a `did:scid` DID.
fn scid(did: &str) -> &str {
    ad4m_identity_core::scid_of(did).unwrap()
}

// A phone-side claimer joins through a holder's offer: `identity.claimOffer` claims, the
// holder confirms with "keep the master key", the phone reads it from its joined inbox over
// the store — the vault gets it, the message is acknowledged, and the record says it holds
// it.
#[tokio::test]
async fn a_phone_claims_an_offer_and_takes_the_master_key_from_its_inbox() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 42).await;

    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();

    let phone_rpc = phone(&router, 2, None);
    let phone_vault = MemoryVault::new();
    let claimed = claim(&phone_rpc, &flow.offer_string(), Some("My phone")).await;
    assert_eq!(claimed["did"].as_str().unwrap(), did);
    assert_eq!(claimed["kind"].as_str().unwrap(), "device");

    let seen = flow.wait_for_claim().await.unwrap();
    assert_eq!(seen.match_code, claimed["matchCode"].as_str().unwrap());
    let linked = flow
        .confirm(&seen, entry("My phone", DeviceKind::PhoneApp), true)
        .await
        .unwrap();
    assert_eq!(linked.key_id, "d2");

    let join = join_status(&phone_rpc, &claimed["offerId"]).await;
    assert_eq!(join["state"].as_str().unwrap(), "spent");
    assert_eq!(join["keyId"].as_str().unwrap(), "d2");
    assert_eq!(join["did"].as_str().unwrap(), did);

    let taken = take_master(&phone_rpc, &phone_vault, &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits in the phone's inbox");
    assert_eq!(taken.index, 0);
    assert_eq!(phone_vault.master_index(scid(&taken.did)).unwrap(), Some(0));
    assert_eq!(
        phone_rpc.rpc().identity().as_deref(),
        Some(scid(&taken.did))
    );

    // The message is acknowledged: the inbox is empty.
    assert_eq!(inbox(&phone_rpc).await, Vec::<Value>::new());

    // The record, read by the phone's own entry, says it holds the master key.
    let opened = record::read(&phone_rpc, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices["d2"].holds_master, HoldsMaster::Yes);
    assert_eq!(opened.record.devices["d2"].master_index, Some(0));

    // `identity.status` settles the identity from the key index: a fresh phone object for
    // the same key discovers it too.
    assert_eq!(status(&phone_rpc).await["did"].as_str().unwrap(), did);
    let reloaded = phone(&router, 2, None);
    assert_eq!(status(&reloaded).await["did"].as_str().unwrap(), did);
}

// A holder scans the phone's device string: the ticket lands in the phone's pre-join inbox;
// the phone's `identity.inbox` opens it, checks it against the log and claims the offer —
// the holder sees the same match code and confirms; the phone's `joinStatus` is `spent`.
#[tokio::test]
async fn a_phone_claims_a_ticket_from_its_pre_join_inbox() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 43).await;

    // `identity.prepareDevice` answers the device string the holder scans.
    let phone_rpc = phone(&router, 3, None);
    let prepared = phone_rpc
        .rpc()
        .call("identity.prepareDevice", json!({}))
        .await
        .unwrap();
    let device = DeviceString::parse(prepared["device"].as_str().unwrap()).unwrap();

    let flow = LinkFlow::scan(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        &device,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();

    // The ticket is in the phone's pre-join inbox; `identity.inbox` claims it at once.
    let entries = inbox(&phone_rpc).await;
    assert_eq!(entries.len(), 1);
    let seen_ticket = &entries[0];
    assert_eq!(seen_ticket["kind"].as_str().unwrap(), "offer-ticket");
    assert_eq!(seen_ticket["did"].as_str().unwrap(), did);

    let claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(
        claimed.match_code,
        seen_ticket["matchCode"].as_str().unwrap()
    );
    flow.confirm(&claimed, entry("My phone", DeviceKind::PhoneApp), true)
        .await
        .unwrap();

    let join = join_status(&phone_rpc, &seen_ticket["offerId"]).await;
    assert_eq!(join["state"].as_str().unwrap(), "spent");
    assert_eq!(join["keyId"].as_str().unwrap(), "d2");

    // The ticket was acknowledged at the claim; the joined inbox carries the master key.
    let entries = inbox(&phone_rpc).await;
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0]["kind"].as_str().unwrap(), "master");
}

// A phone without the master key posts `send-master` (signed by its device key); the holder
// checks the poster against the log, answers it into the phone's joined inbox, and the phone
// takes it from there.
#[tokio::test]
async fn a_phone_posts_send_master_and_the_holder_answers() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 44).await;

    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let phone_rpc = phone(&router, 4, None);
    let claimed = claim(&phone_rpc, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    // No "keep the master key": the phone joins without it.
    flow.confirm(&seen, entry("My phone", DeviceKind::PhoneApp), false)
        .await
        .unwrap();
    assert_eq!(
        join_status(&phone_rpc, &claimed["offerId"]).await["state"]
            .as_str()
            .unwrap(),
        "spent"
    );
    assert!(take_master(&phone_rpc, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_none());

    // The phone asks; the request is signed by its device key.
    let id = ask_for_master(&phone_rpc, None).await.unwrap();

    let listed = holder.requests(0).await.unwrap();
    let request = listed
        .iter()
        .find(|r| r.id == id)
        .expect("the holder sees the request");
    let events = holder.events(&did).await.unwrap();
    let chain = ad4m_identity_core::fold(&events).unwrap();
    check_poster(request, &did, &chain).unwrap();
    let answered = answer(
        &holder,
        &holder_vault,
        &did,
        request,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert!(matches!(answered, Answered::MasterSent { .. }));

    let phone_vault = MemoryVault::new();
    let taken = take_master(&phone_rpc, &phone_vault, &OsRandom)
        .await
        .unwrap()
        .expect("the master key arrived");
    assert_eq!(taken.index, 0);
    assert_eq!(phone_vault.master_index(scid(&taken.did)).unwrap(), Some(0));
    assert!(inbox(&phone_rpc).await.is_empty());
}

// The phone reads and writes the private record with its own device key: the write's receipt
// is the version stored; a version that moved under it merges and writes again.
#[tokio::test]
async fn a_phone_reads_and_writes_the_record_with_the_receipt_check() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 45).await;

    let phone_rpc = phone(&router, 5, None);
    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed = claim(&phone_rpc, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    flow.confirm(&seen, entry("My phone", DeviceKind::PhoneApp), false)
        .await
        .unwrap();
    join_status(&phone_rpc, &claimed["offerId"]).await;

    // The phone writes its own name into the record (its device key signs); the answer is the
    // stored version.
    let version = record::write(
        &phone_rpc,
        &did,
        &[Change::Rename {
            key_id: "d2".to_string(),
            name: "Josh's phone".to_string(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();

    // The holder writes meanwhile; the phone's next write merges onto the newer version.
    let holder_version = record::write(
        &holder,
        &did,
        &[Change::Rename {
            key_id: "d1".to_string(),
            name: "Holder's phone".to_string(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(holder_version, version + 1);
    let merged = record::write(
        &phone_rpc,
        &did,
        &[Change::Rename {
            key_id: "d2".to_string(),
            name: "My other phone".to_string(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(merged, version + 2);

    // Both writes landed in the record the phone reads through its own entry.
    let opened = record::read(&phone_rpc, &did).await.unwrap().unwrap();
    assert_eq!(opened.version, version + 2);
    assert_eq!(opened.record.devices["d1"].name, "Holder's phone");
    assert_eq!(opened.record.devices["d2"].name, "My other phone");
}

// The phone as the holder's side: it opens an offer for another device, confirms the claim,
// and revokes that device — all with the master key it took from the first holder's inbox.
#[tokio::test]
async fn a_phone_links_another_device_and_revokes_it() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 46).await;

    // The phone claims the holder's offer and takes the master key.
    let phone_rpc = phone(&router, 6, None);
    let phone_vault = MemoryVault::new();
    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed = claim(&phone_rpc, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    flow.confirm(&seen, entry("Phone one", DeviceKind::PhoneApp), true)
        .await
        .unwrap();
    join_status(&phone_rpc, &claimed["offerId"]).await;
    take_master(&phone_rpc, &phone_vault, &OsRandom)
        .await
        .unwrap()
        .unwrap();

    // The phone, now a holder, opens an offer for a second device and confirms it.
    let other = phone(&router, 7, None);
    let flow = LinkFlow::open(
        &phone_rpc,
        &phone_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed_other = claim(&other, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    let linked = flow
        .confirm(&seen, entry("Phone two", DeviceKind::PhoneApp), false)
        .await
        .unwrap();
    assert_eq!(linked.key_id, "d3");
    join_status(&other, &claimed_other["offerId"]).await;

    // The phone revokes it: `identity.devices` marks `d3` revoked and the record drops it.
    let revoked = revoke(
        &phone_rpc,
        &phone_vault,
        &did,
        &["d3".to_string()],
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    revoked.record.unwrap();
    let devices = phone_rpc
        .rpc()
        .call("identity.devices", json!({}))
        .await
        .unwrap();
    let d3 = devices
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["keyId"] == "d3")
        .unwrap();
    assert!(d3["revokedSeq"].is_number());
    let opened = record::read(&phone_rpc, &did).await.unwrap().unwrap();
    assert!(!opened.record.devices.contains_key("d3"));

    // The revoked device speaks for the identity no longer: its signed calls refuse.
    let refusal = other
        .rpc()
        .call("identity.requests", json!({ "since": 0 }))
        .await
        .unwrap_err();
    assert_eq!(refusal.code, 409);
}

// The restore's record reads and writes: a record sealed with `recoveryKey` is read through
// the words' recovery secret, and a master-signed request passes the phone through to the
// store — as `record::read_as`/`write_as` with `Reader::Recovery` do it. Then the full
// `RestoreFlow` on a phone that never joined: it claims a bound offer of its own, publishes
// the rotation and writes the record.
#[tokio::test]
async fn a_restore_reads_and_writes_the_record_with_the_recovery_key() {
    use ad4m_identity_client::flows::restore::RestoreFlow;
    use ad4m_identity_client::record::Reader;
    use ad4m_identity_client::vault::KeySigner;

    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 47).await;

    // The recovery key of `confirmed(47)`'s words opens the record the create wrote.
    let seed = words::seed(&words::from_entropy(&[47; 16]));
    let recovery = words::recovery_secret(&seed);
    let master = words::master_key(&seed, 0).unwrap();
    let signer = KeySigner::new(&master.to_bytes(), 0);

    let fresh = phone(&router, 8, None);
    let events = fresh.events(&did).await.unwrap();
    let chain = ad4m_identity_core::fold(&events).unwrap();

    // The recovery reader sees the record through the restore's master-signed request.
    let reader = Reader::Recovery {
        secret: &recovery,
        master: &signer,
        clock: &SystemClock,
    };
    let opened = record::read_as(&fresh, &did, &chain, &reader, &OsRandom)
        .await
        .unwrap()
        .expect("the record is readable before this device joins");
    assert_eq!(opened.record.devices["d1"].name, "My phone");

    // The restore's enrol claims a bound offer of the restore flow's own — here: the full
    // `RestoreFlow` on a device that never joined, which drives `link_this`, `claim_offer`,
    // `recordPut` with the master-signed request, and the take.
    let new_phone = phone(&router, 9, None);
    let new_vault = MemoryVault::new();
    let mut flow = RestoreFlow::start(
        &new_phone,
        &new_vault,
        words::from_entropy(&[47; 16]).phrase(),
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    // Lock out the first holder (d1 is a phone: ticked by default).
    let lock_out: Vec<String> = flow.devices().iter().map(|d| d.key_id.clone()).collect();
    let restored = flow.finish(&lock_out, this_phone()).await.unwrap();
    assert_eq!(restored.did, did);
    let enrolled = restored.enrolled.as_ref().expect("the phone enrolled");
    assert_eq!(new_vault.master_index(scid(&did)).unwrap(), Some(1));
    assert_eq!(status(&new_phone).await["did"].as_str().unwrap(), did);

    // The restored phone reads and writes the record as a device of the identity.
    let opened = record::read(&new_phone, &did).await.unwrap().unwrap();
    assert!(opened.record.devices.contains_key(&enrolled.key_id));
}

// Methods the phone has no use for answer a clear refusal, and a call through the flow keeps
// working after one.
#[tokio::test]
async fn methods_the_phone_does_not_serve_are_refused() {
    let (router, _dir) = store();
    let e = phone(&router, 1, None);
    for method in ["identity.resolve", "identity.rekey"] {
        let refusal = e
            .rpc()
            .call(method, json!({ "did": "did:scid:coasys:1:xyz" }))
            .await
            .unwrap_err();
        assert_eq!(refusal.code, 501);
        assert!(refusal.message.contains("not served"), "{refusal}");
    }
    // `identity.inbox` before any join answers an empty list.
    assert_eq!(inbox(&e).await, Vec::<Value>::new());
    // `identity.takeMaster` before any join answers `null`.
    assert!(take_master(&e, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_none());
}

// A keystore that cannot open right now is not a sealed message: `takeMaster` and the inbox
// listing answer 503 (`unavailable: the wallet is locked, so the inbox does not open`, as the
// executor) and keep the master key for the next try — once the key opens again it is taken.
#[tokio::test]
async fn a_keystore_that_cannot_open_keeps_the_master_key_in_the_inbox() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 48).await;

    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();

    let lock = Arc::new(AtomicBool::new(true));
    let phone_rpc = lockable_phone(&router, 10, lock.clone());
    let phone_vault = MemoryVault::new();
    let claimed = claim(&phone_rpc, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    flow.confirm(&seen, entry("My phone", DeviceKind::PhoneApp), true)
        .await
        .unwrap();
    join_status(&phone_rpc, &claimed["offerId"]).await;

    // The keystore cannot open: `takeMaster` is a 503 and the message stays.
    let refusal = take_master(&phone_rpc, &phone_vault, &OsRandom)
        .await
        .expect_err("an unavailable keystore answers 503 and keeps the message");
    assert!(
        matches!(&refusal, ClientError::Rpc(e) if e.code == 503),
        "{refusal}"
    );

    // The inbox listing answers 503 too, rather than dropping what it cannot open.
    let refusal = phone_rpc
        .rpc()
        .call("identity.inbox", json!({}))
        .await
        .unwrap_err();
    assert_eq!(refusal.code, 503);
    assert!(refusal.message.contains("unavailable"), "{refusal}");

    // With the key back the master key is still waiting and is taken.
    lock.store(false, Ordering::Relaxed);
    let entries = inbox(&phone_rpc).await;
    assert_eq!(entries.len(), 1, "the master key waited in the inbox");
    assert_eq!(entries[0]["kind"].as_str().unwrap(), "master");
    let taken = take_master(&phone_rpc, &phone_vault, &OsRandom)
        .await
        .unwrap()
        .expect("the master key waited");
    assert_eq!(taken.index, 0);
    assert_eq!(phone_vault.master_index(scid(&taken.did)).unwrap(), Some(0));
    assert!(inbox(&phone_rpc).await.is_empty());
}

// The same for a pre-join ticket: while the keystore cannot open, `identity.inbox` answers
// 503 and keeps the ticket; once it opens, the ticket is claimed.
#[tokio::test]
async fn a_keystore_that_cannot_open_keeps_the_pre_join_ticket() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 49).await;

    let lock = Arc::new(AtomicBool::new(true));
    let phone_rpc = lockable_phone(&router, 11, lock.clone());
    let prepared = phone_rpc
        .rpc()
        .call("identity.prepareDevice", json!({}))
        .await
        .unwrap();
    let device = DeviceString::parse(prepared["device"].as_str().unwrap()).unwrap();

    let flow = LinkFlow::scan(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        &device,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();

    // The ticket waits in the pre-join inbox but the keystore cannot open it: 503, kept.
    let refusal = phone_rpc
        .rpc()
        .call("identity.inbox", json!({}))
        .await
        .unwrap_err();
    assert_eq!(refusal.code, 503);
    assert!(refusal.message.contains("unavailable"), "{refusal}");

    // With the key back the ticket is still there and is claimed at once.
    lock.store(false, Ordering::Relaxed);
    let entries = inbox(&phone_rpc).await;
    assert_eq!(entries.len(), 1, "the ticket waited in the pre-join inbox");
    assert_eq!(entries[0]["kind"].as_str().unwrap(), "offer-ticket");
    assert_eq!(entries[0]["did"].as_str().unwrap(), did);
    let claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(
        claimed.match_code,
        entries[0]["matchCode"].as_str().unwrap()
    );
}

/// A `HttpClient` over the router that can swap the head of every log page for one identity
/// (`foreign_head`), or serve one recorded log page for it (`replay`), as a store that lies
/// about the log would. Tests of the head binding and the rollback checkpoint (review of
/// coasys/ad4m#1673).
struct LyingHttp {
    inner: RouterHttp,
    /// The log whose pages are tampered with, by SCID.
    scid: Rc<RefCell<String>>,
    /// A signed head of another log, put on every page of `scid`'s log.
    foreign_head: Rc<RefCell<Option<Value>>>,
    /// A recorded page body, served for every page of `scid`'s log.
    replay: Rc<RefCell<Option<Vec<u8>>>>,
}

#[async_trait(?Send)]
impl HttpClient for LyingHttp {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError> {
        let logs = request.method == "GET"
            && request
                .url
                .starts_with(&format!("{STORE_URL}/v1/logs/{}", self.scid.borrow()));
        let mut response = self.inner.send(request).await?;
        if logs && response.status == 200 {
            if let Some(body) = self.replay.borrow().as_ref() {
                response.body = body.clone();
            } else if let Some(head) = self.foreign_head.borrow().as_ref() {
                let mut page: Value = serde_json::from_slice(&response.body).unwrap();
                page["head"] = head.clone();
                response.body = serde_json::to_vec(&page).unwrap();
            }
        }
        Ok(response)
    }
}

/// `GET /v1/logs/{scid}?from=0` straight from the router: the page as the store serves it.
async fn log_page(router: &Router, scid: &str) -> Vec<u8> {
    RouterHttp {
        router: router.clone(),
    }
    .send(HttpRequest {
        method: "GET".into(),
        url: format!("{STORE_URL}/v1/logs/{scid}?from=0"),
        headers: Vec::new(),
        body: Vec::new(),
    })
    .await
    .unwrap()
    .body
}

/// A phone on `http` speaking for `identity`.
fn phone_over(http: LyingHttp, seed: u8, identity: Option<String>) -> ViaExecutor<ViaStore> {
    let key = SigningKey::from_bytes(&[seed; 32]);
    let enc = SoftEncKey::from_ed25519(&key);
    ViaExecutor::new(ViaStore::new(
        Box::new(http),
        Box::new(key),
        Box::new(enc),
        Box::new(SystemClock),
        Box::new(OsRandom),
        StoreEndpoint {
            url: STORE_URL.to_string(),
            keys: store_keys(),
        },
        identity,
    ))
}

/// Review coasys/ad4m#1673 at 98211551e (Lal's P2): `log_from` checks only the head's
/// signature, never that the head names the requested log, or the folded chain's seq and
/// hash. A store can put the (validly signed) head of another log on the page, and the phone
/// takes the events as confirmed under it. Fails at 98211551e.
#[tokio::test]
async fn a_signed_head_of_another_log_does_not_confirm_this_one() {
    let (router, _dir) = store();
    let a = phone(&router, 1, None);
    let did_a = create(&a, &MemoryVault::new(), 61).await;
    let b = phone(&router, 2, None);
    let did_b = create(&b, &MemoryVault::new(), 62).await;
    let page_b: Value = serde_json::from_slice(&log_page(&router, scid(&did_b)).await).unwrap();

    let lying = LyingHttp {
        inner: RouterHttp {
            router: router.clone(),
        },
        scid: Rc::new(RefCell::new(scid(&did_a).to_string())),
        foreign_head: Rc::new(RefCell::new(Some(page_b["head"].clone()))),
        replay: Rc::new(RefCell::new(None)),
    };
    let fooled = phone_over(lying, 1, Some(scid(&did_a).to_string()));
    let answer = fooled.rpc().call("identity.status", json!({})).await;
    assert!(
        answer.is_err(),
        "a head of another log confirmed this one: {answer:?}"
    );
    let message = answer.unwrap_err().message;
    assert!(message.contains("head"), "{message}");
}

/// Review coasys/ad4m#1673 at 98211551e (Lal's P2): the phone keeps no checkpoint, so after it
/// has seen the log at seq 1 a store can serve the (validly signed) page of seq 0 again and
/// the phone goes back to the older history. Fails at 98211551e.
#[tokio::test]
async fn a_lower_signed_head_after_a_newer_one_is_refused() {
    let (router, _dir) = store();
    let lied_about = Rc::new(RefCell::new(String::new()));
    let replay = Rc::new(RefCell::new(None));
    let lying = LyingHttp {
        inner: RouterHttp {
            router: router.clone(),
        },
        scid: lied_about.clone(),
        foreign_head: Rc::new(RefCell::new(None)),
        replay: replay.clone(),
    };
    // The holder's own transport is the lying one; it is honest until `replay` is set.
    let holder_vault = MemoryVault::new();
    let holder = phone_over(lying, 1, None);
    let did = create(&holder, &holder_vault, 63).await;
    let old_page = log_page(&router, scid(&did)).await;
    assert_eq!(
        serde_json::from_slice::<Value>(&old_page).unwrap()["head"]["seq"],
        json!(0)
    );

    // The log grows to seq 1 (a device joins), and the holder reads it there.
    let other = phone(&router, 8, None);
    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed = claim(&other, &flow.offer_string(), None).await;
    let seen = flow.wait_for_claim().await.unwrap();
    flow.confirm(&seen, entry("Other", DeviceKind::PhoneApp), false)
        .await
        .unwrap();
    join_status(&other, &claimed["offerId"]).await;
    let status = holder
        .rpc()
        .call("identity.status", json!({}))
        .await
        .unwrap();
    assert_eq!(status["head"]["seq"], json!(1), "{status}");

    // The store serves the seq-0 page again.
    *lied_about.borrow_mut() = scid(&did).to_string();
    *replay.borrow_mut() = Some(old_page);
    let answer = holder.rpc().call("identity.status", json!({})).await;
    assert!(
        answer.is_err(),
        "the phone went back to seq 0 after reading seq 1: {answer:?}"
    );
}
