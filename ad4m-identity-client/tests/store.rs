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
    Checkpoint, CheckpointError, EncKey, EncOpenError, HttpClient, HttpError, HttpRequest,
    HttpResponse, LogCheckpoints, MemoryCheckpoints, SoftEncKey, StoreEndpoint, ViaStore,
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
use std::cell::{Cell, RefCell};
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
        vault: None,
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
        .finish(executor, vault, this_phone(), &SystemClock, &OsRandom)
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
    let opened = record::read(&phone_rpc, &phone_vault, &did)
        .await
        .unwrap()
        .unwrap();
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

// The phone's device reads and writes the private record with its own device key: the
// write's receipt is the version stored; a version that moved under it merges and writes
// again, and the read shows both names.
#[tokio::test]
async fn a_device_reads_writes_and_merges_the_record() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 45).await;

    let phone_rpc = phone(&router, 5, None);
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
    flow.confirm(&seen, entry("My phone", DeviceKind::PhoneApp), false)
        .await
        .unwrap();
    join_status(&phone_rpc, &claimed["offerId"]).await;

    // The phone writes its own name into the record (its device key signs); the answer is the
    // stored version.
    let version = record::write(
        &phone_rpc,
        &phone_vault,
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
        &holder_vault,
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
        &phone_vault,
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
    let opened = record::read(&phone_rpc, &phone_vault, &did)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(opened.version, version + 2);
    assert_eq!(opened.record.devices["d1"].name, "Holder's phone");
    assert_eq!(opened.record.devices["d2"].name, "My other phone");
}

/// A `HttpClient` over the router that rewrites the `receipt` of every
/// `GET /v1/private/{scid}` answer through `tamper` — a store that serves the record it
/// holds under a receipt that is not for it: one naming another envelope, or signed for a
/// store key the phone does not trust.
struct LyingReceiptHttp {
    inner: RouterHttp,
    tamper: Rc<dyn Fn(&mut Value)>,
}

#[async_trait(?Send)]
impl HttpClient for LyingReceiptHttp {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError> {
        let private =
            request.method == "GET" && request.url.starts_with(&format!("{STORE_URL}/v1/private/"));
        let mut response = self.inner.send(request).await?;
        if private && response.status == 200 {
            let mut body: Value = serde_json::from_slice(&response.body).unwrap();
            (self.tamper)(&mut body["receipt"]);
            response.body = serde_json::to_vec(&body).unwrap();
        }
        Ok(response)
    }
}

// Review of coasys/ad4m#1673 (Lal): `ViaStore` passes the store's `{record, receipt}` of
// `GET /v1/private/{scid}` through as `identity.record`, and `record::read` checks the
// receipt before it trusts a field (`decrypt_record` → `verify_stored`): the store key the
// phone trusts for signed heads vouches for this identity, version and envelope at the seq
// it accepted it. A record served under a receipt that is not for it — the envelope of
// another record, or a key that is no trusted store key — is refused; the honest store
// reads.
#[tokio::test]
async fn a_record_served_under_a_receipt_that_is_not_for_it_is_refused() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did = create(&holder, &holder_vault, 101).await;

    // The honest store reads: the create wrote the record's first version.
    let opened = record::read(&holder, &holder_vault, &did)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(opened.version, 1);
    assert_eq!(opened.record.devices["d1"].name, "My phone");

    // A receipt naming another envelope: the store never accepted this record as it.
    let foreign = ad4m_hash_chain::hash_bytes(b"another record's envelope");
    let lying = phone_with(
        LyingReceiptHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            tamper: Rc::new(move |receipt: &mut Value| {
                receipt["envelope"] = json!(foreign);
            }),
        },
        1,
        Some(scid(&did).to_string()),
    );
    let refused = record::read(&lying, &MemoryVault::new(), &did).await;
    assert!(
        matches!(refused, Err(ClientError::Record(RecordError::Receipt(_)))),
        "a receipt of another envelope passed: {refused:?}"
    );

    // A receipt of a key that is not the store key the phone trusts for signed heads.
    let not_the_store = StoreKey::from_secret([9; 32]).did();
    let lying = phone_with(
        LyingReceiptHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            tamper: Rc::new(move |receipt: &mut Value| {
                receipt["store"] = json!(not_the_store);
            }),
        },
        1,
        Some(scid(&did).to_string()),
    );
    let refused = record::read(&lying, &MemoryVault::new(), &did).await;
    assert!(
        matches!(refused, Err(ClientError::Record(RecordError::Receipt(_)))),
        "a receipt of an untrusted store key passed: {refused:?}"
    );
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
    let opened = record::read(&phone_rpc, &phone_vault, &did)
        .await
        .unwrap()
        .unwrap();
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
    let fresh_vault = MemoryVault::new();
    let events = fresh.events(&did).await.unwrap();
    let chain = ad4m_identity_core::fold(&events).unwrap();

    // The recovery reader sees the record through the restore's master-signed request.
    let reader = Reader::Recovery {
        secret: &recovery,
        master: &signer,
        clock: &SystemClock,
    };
    let opened = record::read_as(&fresh, &fresh_vault, &did, &chain, &reader, &OsRandom)
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
    let opened = record::read(&new_phone, &new_vault, &did)
        .await
        .unwrap()
        .unwrap();
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
    phone_with(http, seed, identity)
}

/// A phone on `http` keeping its accepted heads in `checkpoints`.
fn phone_over_checkpoints(
    http: impl HttpClient + 'static,
    seed: u8,
    checkpoints: impl LogCheckpoints + 'static,
) -> ViaExecutor<ViaStore> {
    let key = SigningKey::from_bytes(&[seed; 32]);
    let enc = SoftEncKey::from_ed25519(&key);
    ViaExecutor::new(
        ViaStore::new(
            Box::new(http),
            Box::new(key),
            Box::new(enc),
            Box::new(SystemClock),
            Box::new(OsRandom),
            StoreEndpoint {
                url: STORE_URL.to_string(),
                keys: store_keys(),
            },
            None,
        )
        .with_checkpoints(Box::new(checkpoints)),
    )
}

/// A phone on any `HttpClient`.
fn phone_with(
    http: impl HttpClient + 'static,
    seed: u8,
    identity: Option<String>,
) -> ViaExecutor<ViaStore> {
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

/// Review coasys/ad4m#1673 at 4c1de3a66 (Lal): `identity.log`, the path the flows read the log
/// through (`request::log_of`), went through `log_from` alone: the head's signature and its
/// SCID, but no binding to the folded log and no checkpoint. Here the store serves the seq-0
/// events under the log's own (validly signed) seq-1 head: the read must fail, and at
/// 4c1de3a66 it answered the events as confirmed. Then the checkpoint: after the holder read
/// seq 1, the seq-0 page replayed is refused, until `identity.resetLog` names this identity
/// (a reset of another identity changes nothing). Fails at 4c1de3a66.
#[tokio::test]
async fn identity_log_binds_the_head_to_the_events_and_keeps_the_checkpoint() {
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
    let holder_vault = MemoryVault::new();
    let holder = phone_over(lying, 1, None);
    let did = create(&holder, &holder_vault, 73).await;
    let other_did = create(&phone(&router, 2, None), &MemoryVault::new(), 74).await;
    let old_page = log_page(&router, scid(&did)).await;

    // The log grows to seq 1, and the holder reads it there.
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
    let read = |from: u64| {
        holder
            .rpc()
            .call("identity.log", json!({ "did": did, "from": from }))
    };
    let at_one = read(0).await.unwrap();
    assert_eq!(at_one["head"]["seq"], json!(1), "{at_one}");
    let new_page: Value = serde_json::from_slice(&log_page(&router, scid(&did)).await).unwrap();

    // The seq-0 events under the seq-1 head: a valid head of this log, not of these events.
    let mut mismatched: Value = serde_json::from_slice(&old_page).unwrap();
    mismatched["head"] = new_page["head"].clone();
    *lied_about.borrow_mut() = scid(&did).to_string();
    *replay.borrow_mut() = Some(serde_json::to_vec(&mismatched).unwrap());
    let answer = read(0).await;
    assert!(
        answer.is_err(),
        "identity.log confirmed seq-0 events under the seq-1 head: {answer:?}"
    );

    // The seq-0 page as the store once served it: under the seq 1 read before.
    *replay.borrow_mut() = Some(old_page);
    let answer = read(0).await;
    assert!(
        answer.is_err(),
        "identity.log went back to seq 0 after reading seq 1: {answer:?}"
    );
    holder
        .rpc()
        .call("identity.resetLog", json!({ "did": other_did }))
        .await
        .unwrap();
    assert!(
        read(0).await.is_err(),
        "a reset of another identity let it through"
    );
    holder
        .rpc()
        .call("identity.resetLog", json!({ "did": did }))
        .await
        .unwrap();
    let back = read(0).await.unwrap();
    assert_eq!(back["head"]["seq"], json!(0), "{back}");
}

/// A `HttpClient` over the router that answers every `GET /v1/keys/{key}` with a chosen
/// `did` — a key index whose answer does not come from the logs — and that can fail every
/// log read (`fail_logs`). Review of coasys/ad4m#1673 (Lal P2): the index's answer
/// installed the binding before any log was read.
struct LyingIndexHttp {
    inner: RouterHttp,
    /// The `did` every key-index answer names.
    did: Rc<RefCell<Option<String>>>,
    /// While set, `GET /v1/logs/…` is a transport error.
    fail_logs: Rc<Cell<bool>>,
}

#[async_trait(?Send)]
impl HttpClient for LyingIndexHttp {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError> {
        if request.method == "GET" {
            if request.url.starts_with(&format!("{STORE_URL}/v1/keys/")) {
                if let Some(did) = self.did.borrow().as_ref() {
                    return Ok(HttpResponse {
                        status: 200,
                        headers: Vec::new(),
                        body: serde_json::to_vec(&json!({
                            "did": did,
                            "keyId": "d1",
                            "added": 0,
                            "head": {
                                "scid": "", "seq": 0, "hash": "",
                                "time": 0, "store": "", "sig": "",
                            },
                        }))
                        .unwrap(),
                    });
                }
            }
            if self.fail_logs.get() && request.url.starts_with(&format!("{STORE_URL}/v1/logs/")) {
                return Err(HttpError("the connection was refused".into()));
            }
        }
        self.inner.send(request).await
    }
}

/// Review coasys/ad4m#1673 (Lal P2): `scid_of_self` installed the key index's answer as the
/// binding before fetching — or validating — the log it names, so a lying index bound this
/// device's key to a foreign identity: `status` reported it and `claim`/`cosignIncept`
/// refused on it. The index's answer is a candidate now: the binding stands only when the
/// admitted log lists the key (as the executor's `trusted_listing`). Fails at 1bf063ca7,
/// where the index's answer was installed at once.
#[tokio::test]
async fn an_index_answer_naming_another_identity_binds_nothing() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let holder_vault = MemoryVault::new();
    let did_a = create(&holder, &holder_vault, 81).await;

    // The index answers the other identity for every key.
    let index_did = Rc::new(RefCell::new(Some(did_a.clone())));
    let fresh = phone_with(
        LyingIndexHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            did: index_did.clone(),
            fail_logs: Rc::new(Cell::new(false)),
        },
        20,
        None,
    );
    let before = status(&fresh).await;
    assert!(
        before["did"].is_null(),
        "the key index bound this key to a foreign identity: {before}"
    );
    assert_eq!(fresh.rpc().identity(), None);

    // A claim is still possible (no false `already-linked`): the holder opens an offer,
    // the phone claims it.
    let flow = LinkFlow::open(
        &holder,
        &holder_vault,
        &did_a,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed = claim(&fresh, &flow.offer_string(), None).await;
    assert_eq!(claimed["did"].as_str().unwrap(), did_a);

    // And a create: `cosignIncept` and `publish` are not refused.
    let did_new = create(&fresh, &MemoryVault::new(), 82).await;

    // When the index answers truthfully again, the new identity settles.
    *index_did.borrow_mut() = None;
    let settled = status(&fresh).await;
    assert_eq!(settled["did"].as_str().unwrap(), did_new, "{settled}");
    assert_eq!(fresh.rpc().identity().as_deref(), Some(scid(&did_new)));
}

/// Same review: a candidate whose log the store will not serve installs nothing — no
/// binding is cached or exposed.
#[tokio::test]
async fn a_candidate_whose_log_fails_to_read_binds_nothing() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let did_a = create(&holder, &MemoryVault::new(), 83).await;

    let fresh = phone_with(
        LyingIndexHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            did: Rc::new(RefCell::new(Some(did_a))),
            fail_logs: Rc::new(Cell::new(true)),
        },
        21,
        None,
    );
    let answer = fresh.rpc().call("identity.status", json!({})).await;
    assert!(
        answer.is_err(),
        "the failed candidate was ignored: {answer:?}"
    );
    assert_eq!(
        fresh.rpc().identity(),
        None,
        "a failed candidate installed a binding"
    );
}

/// A `HttpClient` that answers `GET /v1/logs/{scid}?from=` itself — `make_page` computes
/// the body for the requested `from` — and passes everything else to the router. A store
/// whose pages never reach the head it signs, whose head moves mid-read, or whose pages
/// run past the byte budget. Past 100 pages it refuses, so a client without a page cap
/// fails here instead of hanging the test.
struct PagingHttp {
    inner: RouterHttp,
    scid: String,
    /// The page body for a requested `from`, as the store would serve it.
    make_page: Rc<dyn Fn(u64) -> Vec<u8>>,
    /// How many log pages were served.
    served: Rc<Cell<usize>>,
}

#[async_trait(?Send)]
impl HttpClient for PagingHttp {
    async fn send(&self, request: HttpRequest) -> Result<HttpResponse, HttpError> {
        let prefix = format!("{STORE_URL}/v1/logs/{}?from=", self.scid);
        if request.method == "GET" && request.url.starts_with(&prefix) {
            let cursor: u64 = request.url[prefix.len()..]
                .parse()
                .expect("the from query is a seq");
            self.served.set(self.served.get() + 1);
            if self.served.get() > 100 {
                return Err(HttpError("the fake store gave up".into()));
            }
            return Ok(HttpResponse {
                status: 200,
                headers: Vec::new(),
                body: (self.make_page)(cursor),
            });
        }
        self.inner.send(request).await
    }
}

/// `page` (a real log page, as `log_page` serves it) reshaped: the events and the head's
/// seq replaced; `head_hash` changes the head's hash (and so the head) when given.
fn fake_page(page: &Value, events: Value, head_seq: u64, head_hash: Option<String>) -> Vec<u8> {
    let mut page = page.clone();
    page["appendedAt"] = json!(vec![0; events.as_array().map(Vec::len).unwrap_or_default()]);
    page["events"] = events;
    page["head"]["seq"] = json!(head_seq);
    if let Some(hash) = head_hash {
        page["head"]["hash"] = json!(hash);
    }
    serde_json::to_vec(&page).unwrap()
}

/// Review coasys/ad4m#1673 (Lal P2): `log_from` pages until the served events pass the
/// page's head seq — a store that signs a head its pages never reach keeps the client
/// reading forever. The read is bounded now: a strictly advancing cursor, at most 64
/// pages (the executor's `MAX_PAGES`), then the read is refused. Fails at 1bf063ca7, which
/// paged forever (the fake store's own 100-page give-up answers here instead).
#[tokio::test]
async fn a_log_whose_pages_never_reach_the_head_is_refused() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let did = create(&holder, &MemoryVault::new(), 91).await;
    let page: Value = serde_json::from_slice(&log_page(&router, scid(&did)).await).unwrap();

    // Every page serves one "event" at the cursor and signs a head at seq 1_000_000: the
    // read advances strictly and never reaches it.
    let served = Rc::new(Cell::new(0));
    let e = phone_with(
        PagingHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            scid: scid(&did).to_string(),
            served: served.clone(),
            make_page: Rc::new(move |from| {
                fake_page(&page, json!([{ "seq": from }]), 1_000_000, None)
            }),
        },
        30,
        None,
    );
    let answer = e.rpc().call("identity.log", json!({ "did": did })).await;
    let message = answer.expect_err("the read must end").message;
    assert!(message.contains("pages"), "{message}");
    assert_eq!(served.get(), 64, "the read stops at the page cap");
}

/// Same review: the head a read checks at the end is the last page's — a store that moves
/// it between pages (a different signature over another statement) is never caught. One
/// read now wants one head across all pages.
#[tokio::test]
async fn a_head_that_moves_between_pages_is_refused() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let did = create(&holder, &MemoryVault::new(), 92).await;
    let page: Value = serde_json::from_slice(&log_page(&router, scid(&did)).await).unwrap();

    // The head's seq stays at 5 but its hash changes every page: the read can never hold
    // one confirmed head.
    let e = phone_with(
        PagingHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            scid: scid(&did).to_string(),
            served: Rc::new(Cell::new(0)),
            make_page: Rc::new(move |from| {
                fake_page(
                    &page,
                    json!([{ "seq": from }]),
                    5,
                    Some(format!("hash-{from}")),
                )
            }),
        },
        30,
        None,
    );
    let answer = e.rpc().call("identity.log", json!({ "did": did })).await;
    let message = answer
        .expect_err("a moved head is no confirmed log")
        .message;
    assert!(
        message.contains("changed"),
        "the failure names the moved head: {message}"
    );
}

/// Same review: a store that serves the same old events on every page never advances the
/// read; the cursor must advance strictly.
#[tokio::test]
async fn a_log_page_that_does_not_advance_is_refused() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let did = create(&holder, &MemoryVault::new(), 93).await;
    let page: Value = serde_json::from_slice(&log_page(&router, scid(&did)).await).unwrap();

    // Every page serves the seq-0 "event" again under a head at seq 5.
    let served = Rc::new(Cell::new(0));
    let e = phone_with(
        PagingHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            scid: scid(&did).to_string(),
            served: served.clone(),
            make_page: Rc::new(move |_| fake_page(&page, json!([{ "seq": 0 }]), 5, None)),
        },
        30,
        None,
    );
    let answer = e.rpc().call("identity.log", json!({ "did": did })).await;
    let message = answer.expect_err("no progress is no read").message;
    assert!(message.contains("advance"), "{message}");
    assert_eq!(
        served.get(),
        2,
        "the second page is the first that cannot advance"
    );
}

/// Same review: a reply's size is bounded per page, but nothing bounded what a read holds
/// over all its pages — the executor's 32 MiB (`MAX_STORE_LOG_BYTES`) now does, copies
/// included.
#[tokio::test]
async fn a_log_past_the_byte_budget_is_refused() {
    let (router, _dir) = store();
    let holder = phone(&router, 1, None);
    let did = create(&holder, &MemoryVault::new(), 94).await;
    let page: Value = serde_json::from_slice(&log_page(&router, scid(&did)).await).unwrap();

    // Each page is padded (trailing whitespace parses) to the per-reply maximum; the
    // second page passes 32 MiB together with the first.
    let mut padded = fake_page(&page, json!([{ "seq": 0 }]), 5, None);
    padded.resize(ad4m_ldk::identity::MAX_LOG_PAGE_BYTES, b' ');
    let served = Rc::new(Cell::new(0));
    let e = phone_with(
        PagingHttp {
            inner: RouterHttp {
                router: router.clone(),
            },
            scid: scid(&did).to_string(),
            served: served.clone(),
            make_page: Rc::new(move |_| padded.clone()),
        },
        30,
        None,
    );
    let answer = e.rpc().call("identity.log", json!({ "did": did })).await;
    let message = answer
        .expect_err("a read past the budget is refused")
        .message;
    assert!(message.contains("bytes"), "{message}");
    assert_eq!(served.get(), 2);
}

/// Review coasys/ad4m#1673 (Lal P3): a `HttpRequest` derives `Debug`, printing the signed
/// `X-Coasys-*` headers and the body (claim secrets, sealed messages) into any log a
/// transport writes.
#[test]
fn a_request_debug_never_prints_headers_or_body() {
    let request = HttpRequest {
        method: "POST".into(),
        url: format!("{STORE_URL}/v1/offers/scid/offer/claim"),
        headers: vec![
            ("x-coasys-request".into(), "the-signed-request".into()),
            ("content-type".into(), "application/json".into()),
        ],
        body: b"{\"s\":\"the-claim-secret\"}".to_vec(),
    };
    let debug = format!("{request:?}");
    assert!(!debug.contains("the-signed-request"), "{debug}");
    assert!(!debug.contains("the-claim-secret"), "{debug}");
}

/// Review coasys/ad4m#1673: `identity.keyOwner` maps a store `404` — a key in no log — to
/// the error path instead of `null` (its documented contract), so a restore from words
/// whose `m0` no identity has answers an RPC failure instead of `NotTheseWords`. Fails at
/// 1bf063ca7.
#[tokio::test]
async fn a_key_in_no_log_answers_null_and_the_restore_not_these_words() {
    use ad4m_identity_client::flows::restore::RestoreFlow;

    let (router, _dir) = store();
    let e = phone(&router, 22, None);

    // A device key in no log: `keyOwner` answers `null`.
    let key = ad4m_hash_chain::encode_ed25519_multikey(
        SigningKey::from_bytes(&[99; 32]).verifying_key().as_bytes(),
    );
    let owner = e.key_owner(&key).await.unwrap();
    assert!(owner.is_none(), "a key in no log answers null: {owner:?}");

    // Words whose master #0 sits in no log: the restore names them, not an RPC error.
    let vault = MemoryVault::new();
    let refusal = RestoreFlow::start(
        &e,
        &vault,
        words::from_entropy(&[230; 16]).phrase(),
        &SystemClock,
        &OsRandom,
    )
    .await;
    let Err(refusal) = refusal else {
        panic!("words of no identity are refused")
    };
    assert!(
        matches!(refusal, ClientError::NotTheseWords(_)),
        "{refusal}"
    );
}

/// Review coasys/ad4m#1673: the accepted-head checkpoint lived in the `ViaStore`
/// instance, so a restart (a new `ViaStore` over the same surface state) forgot it and a
/// store could replay the older page again. The checkpoint is a caller-supplied port now:
/// a persisting one keeps the refusal across `ViaStore`s.
#[tokio::test]
async fn a_checkpoint_kept_by_the_port_survives_a_new_via_store() {
    let (router, _dir) = store();
    let lied_about = Rc::new(RefCell::new(String::new()));
    let replay = Rc::new(RefCell::new(None));
    let http = || LyingHttp {
        inner: RouterHttp {
            router: router.clone(),
        },
        scid: lied_about.clone(),
        foreign_head: Rc::new(RefCell::new(None)),
        replay: replay.clone(),
    };
    let port = MemoryCheckpoints::default();
    let holder_vault = MemoryVault::new();
    let holder = phone_over_checkpoints(http(), 1, port.clone());
    let did = create(&holder, &holder_vault, 95).await;
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
    let at_one = status(&holder).await;
    assert_eq!(at_one["head"]["seq"], json!(1), "{at_one}");

    // A restart: a new `ViaStore` over the same port. The store replays the seq-0 page —
    // the port's checkpoint still refuses it.
    *lied_about.borrow_mut() = scid(&did).to_string();
    *replay.borrow_mut() = Some(old_page);
    let restarted = phone_over_checkpoints(http(), 1, port);
    let answer = restarted.rpc().call("identity.status", json!({})).await;
    let message = answer
        .expect_err("the replayed seq-0 page stays refused across a restart")
        .message;
    assert!(message.contains("store-behind"), "{message}");

    // A port that kept nothing (the in-memory default after a real restart without
    // persistence) accepts the same page: the refusal lived in the port.
    let fresh = phone_over_checkpoints(http(), 1, MemoryCheckpoints::default());
    let forgotten = status(&fresh).await;
    assert_eq!(forgotten["head"]["seq"], json!(0), "{forgotten}");
}

/// A checkpoint port whose storage fails (a persistent adapter's disk error): the phone fails
/// closed, it admits no log it cannot hold to its checkpoint, and `identity.resetLog` reports
/// the failure rather than pretending to forget. Lal's P3 on coasys/ad4m#1673, so WE's
/// persistent adapter has an error channel before it is written.
struct BrokenCheckpoints;

impl LogCheckpoints for BrokenCheckpoints {
    fn get(&self, _scid: &str) -> Result<Option<Checkpoint>, CheckpointError> {
        Err(CheckpointError("the disk is full".into()))
    }

    fn set(&self, _scid: &str, _checkpoint: Checkpoint) -> Result<(), CheckpointError> {
        Err(CheckpointError("the disk is full".into()))
    }

    fn forget(&self, _scid: &str) -> Result<(), CheckpointError> {
        Err(CheckpointError("the disk is full".into()))
    }
}

#[tokio::test]
async fn a_checkpoint_port_that_fails_closes_the_read() {
    let (router, _dir) = store();
    let did = create(&phone(&router, 1, None), &MemoryVault::new(), 81).await;
    let http = || RouterHttp {
        router: router.clone(),
    };
    let broken = phone_over_checkpoints(http(), 1, BrokenCheckpoints);
    let read = broken
        .rpc()
        .call("identity.log", json!({ "did": did, "from": 0 }))
        .await;
    assert!(
        matches!(&read, Err(e) if e.code == 503 && e.message.contains("checkpoints")),
        "{read:?}"
    );
    let reset = broken
        .rpc()
        .call("identity.resetLog", json!({ "did": did }))
        .await;
    assert!(
        matches!(&reset, Err(e) if e.code == 503 && e.message.contains("checkpoints")),
        "{reset:?}"
    );
    // The same phone over a working port reads fine: the failure was the port's.
    let working = phone_over_checkpoints(http(), 1, MemoryCheckpoints::default());
    assert!(working
        .rpc()
        .call("identity.log", json!({ "did": did, "from": 0 }))
        .await
        .is_ok());
}
