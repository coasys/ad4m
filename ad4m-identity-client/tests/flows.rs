//! The flows against a real `identity-store` (its router, in process, on a temp SQLite file),
//! through a test executor that answers the executor's `identity.*` calls the way
//! `rust-executor/src/identity` does: it holds one device key, co-signs, claims, and passes
//! everything else to the store's routes. The real executor runs these flows in `tests/js`
//! (`identity.test.ts`).
#![cfg(not(target_arch = "wasm32"))]

use std::cell::{Cell, RefCell};
use std::sync::Arc;

use ad4m_hash_chain::canonical_json;
use ad4m_identity_client::flows::create::{resume, CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::LinkFlow;
use ad4m_identity_client::flows::revoke::{key_id_of, revoke};
use ad4m_identity_client::{
    ClientError, ExecutorRpc, MasterUse, MemoryVault, OsRandom, Random, RpcError, SystemClock,
    Vault, ViaExecutor,
};
use ad4m_identity_core::build::{cosign, Signer};
use ad4m_identity_core::link::{self, OfferKind, OfferString};
use ad4m_identity_core::words;
use ad4m_identity_core::{did_of, Event, Op};
use async_trait::async_trait;
use axum::body::Body;
use axum::http::Request;
use axum::Router;
use ed25519_dalek::SigningKey;
use http_body_util::BodyExt;
use identity_store::db::Db;
use identity_store::request::{encode_header, REQUEST_HEADER};
use identity_store::{router, system_clock, Settings, Store, StoreKey};
use serde_json::{json, Value};
use tower::ServiceExt;

const STORE_URL: &str = "http://store.test";

struct TestStore {
    router: Router,
    _dir: tempfile::TempDir,
}

impl TestStore {
    fn new() -> Arc<TestStore> {
        let dir = tempfile::tempdir().unwrap();
        let db = Db::open(dir.path().join("store.sqlite")).unwrap();
        let store = Store::new(
            db,
            StoreKey::from_secret([7; 32]),
            system_clock(),
            Settings {
                pow_bits: 8,
                feed_token: None,
                cors_origins: Vec::new(),
            },
        );
        Arc::new(TestStore {
            router: router(store),
            _dir: dir,
        })
    }

    async fn send(
        &self,
        method: &str,
        path: &str,
        body: Option<&Value>,
        header: Option<String>,
    ) -> (u16, Value) {
        let mut request = Request::builder().method(method).uri(path);
        if body.is_some() {
            request = request.header("content-type", "application/json");
        }
        if let Some(header) = header {
            request = request.header(REQUEST_HEADER, header);
        }
        let body = body
            .map(|b| Body::from(canonical_json(b).unwrap()))
            .unwrap_or_else(Body::empty);
        let response = self
            .router
            .clone()
            .oneshot(request.body(body).unwrap())
            .await
            .unwrap();
        let status = response.status().as_u16();
        let raw = response.into_body().collect().await.unwrap().to_bytes();
        let value = serde_json::from_slice(&raw).unwrap_or(Value::Null);
        (status, value)
    }
}

/// An executor with one device key on the shared store.
struct TestExecutor {
    store: Arc<TestStore>,
    device: SigningKey,
    identity: RefCell<Option<String>>,
    joins: RefCell<Vec<(String, String)>>,
    /// `identity.publish` calls that reached the store.
    publishes: Cell<usize>,
    /// The next `identity.publish` commits, then its answer is lost (a transport failure).
    lose_publish_answer: Cell<bool>,
    /// The next `identity.publish` fails before it reaches the store.
    refuse_publish: Cell<bool>,
}

impl TestExecutor {
    fn new(store: &Arc<TestStore>, seed: u8) -> Self {
        TestExecutor {
            store: store.clone(),
            device: SigningKey::from_bytes(&[seed; 32]),
            identity: RefCell::new(None),
            joins: RefCell::new(Vec::new()),
            publishes: Cell::new(0),
            lose_publish_answer: Cell::new(false),
            refuse_publish: Cell::new(false),
        }
    }

    fn device_key(&self) -> String {
        Signer::key(&self.device)
    }

    fn device_enc(&self) -> String {
        let montgomery = self.device.verifying_key().to_montgomery();
        ad4m_identity_core::encode_x25519_multikey(montgomery.as_bytes())
    }

    fn refused(code: u16, body: &Value) -> RpcError {
        RpcError {
            code,
            message: body.to_string(),
        }
    }

    async fn log(&self, scid: &str) -> Result<Value, RpcError> {
        let (code, page) = self
            .store
            .send("GET", &format!("/v1/logs/{scid}?from=0"), None, None)
            .await;
        if code != 200 {
            return Err(Self::refused(code, &page));
        }
        Ok(page)
    }

    /// Records the identity when its log lists this executor's key.
    async fn record_if_listed(&self, scid: &str) -> Result<Option<String>, RpcError> {
        let page = self.log(scid).await?;
        let events: Vec<Event> = page["events"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| Event::from_json(e).unwrap())
            .collect();
        let chain = ad4m_identity_core::fold(&events).unwrap();
        let id = chain.state().key_id_of(&self.device_key()).cloned();
        if id.is_some() {
            self.identity.borrow_mut().get_or_insert(scid.to_string());
        }
        Ok(id)
    }
}

#[async_trait(?Send)]
impl ExecutorRpc for TestExecutor {
    async fn call(&self, method: &str, p: Value) -> Result<Value, RpcError> {
        match method {
            "identity.status" => {
                let scid = self.identity.borrow().clone();
                let mut key_id = None;
                let mut revoked = None;
                if let Some(scid) = &scid {
                    let page = self.log(scid).await?;
                    let events: Vec<Event> = page["events"]
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(|e| Event::from_json(e).unwrap())
                        .collect();
                    let chain = ad4m_identity_core::fold(&events).unwrap();
                    key_id = chain.state().key_id_of(&self.device_key()).cloned();
                    revoked = key_id
                        .as_ref()
                        .and_then(|id| chain.state().keys.get(id))
                        .and_then(|r| r.revoked);
                }
                Ok(json!({
                    "did": scid.map(|s| did_of(&s)),
                    "deviceKey": self.device_key(),
                    "deviceEnc": self.device_enc(),
                    "keyId": key_id,
                    "revoked": revoked,
                    "head": null,
                    "fork": null,
                }))
            }
            "identity.cosignIncept" => {
                if self.identity.borrow().is_some() {
                    return Err(RpcError {
                        code: 409,
                        message: "identity-exists".into(),
                    });
                }
                let mut event = Event::from_json(&p["event"]).unwrap();
                let Op::Incept { add, .. } = &event.op else {
                    return Err(RpcError {
                        code: 400,
                        message: "not an incept".into(),
                    });
                };
                let listed = add
                    .iter()
                    .find(|d| d.sign == self.device_key() && d.enc == self.device_enc())
                    .ok_or(RpcError {
                        code: 400,
                        message: "not-listed".into(),
                    })?;
                let id = listed.id.clone();
                cosign(&mut event, &id, &self.device).unwrap();
                ad4m_identity_core::fold(std::slice::from_ref(&event)).unwrap();
                Ok(serde_json::to_value(&event).unwrap())
            }
            "identity.publish" => {
                let events: Vec<Event> = p["events"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|e| Event::from_json(e).unwrap())
                    .collect();
                let scid = events[0].scid.clone();
                if self.refuse_publish.replace(false) {
                    return Err(RpcError {
                        code: 503,
                        message: "unavailable: the store did not answer".into(),
                    });
                }
                self.publishes.set(self.publishes.get() + 1);
                let (code, body) = self
                    .store
                    .send(
                        "POST",
                        &format!("/v1/logs/{scid}/events"),
                        Some(&json!({ "events": p["events"] })),
                        None,
                    )
                    .await;
                let head = match code {
                    200 => body["head"].clone(),
                    // As the executor does: a 409 whose log holds these very events (the
                    // append happened before) answers the store's head.
                    409 => {
                        let page = self.log(&scid).await?;
                        let held: Vec<Event> = page["events"]
                            .as_array()
                            .unwrap()
                            .iter()
                            .map(|e| Event::from_json(e).unwrap())
                            .collect();
                        let same = events.iter().all(|ours| {
                            held.iter().any(|t| {
                                t.seq == ours.seq
                                    && ad4m_identity_core::event_hash(t).unwrap()
                                        == ad4m_identity_core::event_hash(ours).unwrap()
                            })
                        });
                        if !same {
                            return Err(Self::refused(code, &body));
                        }
                        page["head"].clone()
                    }
                    _ => return Err(Self::refused(code, &body)),
                };
                self.record_if_listed(&scid).await?;
                if self.lose_publish_answer.replace(false) {
                    return Err(RpcError {
                        code: 503,
                        message: "the connection closed".into(),
                    });
                }
                Ok(head)
            }
            "identity.log" => {
                let did = p["did"].as_str().unwrap();
                let scid = ad4m_identity_core::scid_of(did).unwrap();
                let page = self.log(scid).await?;
                Ok(json!({
                    "did": did, "events": page["events"], "head": page["head"], "fork": null,
                }))
            }
            "identity.openOffer" => {
                let offer = &p["signedOffer"];
                let scid = offer["offer"]["scid"].as_str().unwrap();
                let (code, body) = self
                    .store
                    .send("POST", &format!("/v1/offers/{scid}"), Some(offer), None)
                    .await;
                if code != 201 {
                    return Err(Self::refused(code, &body));
                }
                Ok(body)
            }
            "identity.offers" => {
                let request: ad4m_ldk::identity::SignedRequest =
                    serde_json::from_value(p["request"].clone()).unwrap();
                let scid = ad4m_identity_core::scid_of(&request.req.did)
                    .unwrap()
                    .to_string();
                let wait = p["wait"].as_u64().unwrap_or(0);
                let (code, body) = self
                    .store
                    .send(
                        "GET",
                        &format!("/v1/offers/{scid}?wait={wait}"),
                        None,
                        Some(encode_header(&request)),
                    )
                    .await;
                if code != 200 {
                    return Err(Self::refused(code, &body));
                }
                Ok(body)
            }
            "identity.claimOffer" => {
                let offer = OfferString::parse_link(p["offer"].as_str().unwrap())
                    .or_else(|_| OfferString::parse(p["offer"].as_str().unwrap()))
                    .unwrap();
                let s = link::decode_secret(&offer.s).unwrap();
                let claim = link::make_claim(
                    &offer.scid,
                    &offer.offer_id,
                    s,
                    &self.device,
                    self.device_enc(),
                    false,
                    p["hint"].as_str().map(str::to_string),
                )
                .unwrap();
                let (code, body) = self
                    .store
                    .send(
                        "POST",
                        &format!("/v1/offers/{}/{}/claim", offer.scid, offer.offer_id),
                        Some(&serde_json::to_value(&claim).unwrap()),
                        None,
                    )
                    .await;
                if code != 201 && code != 200 {
                    return Err(Self::refused(code, &body));
                }
                self.joins
                    .borrow_mut()
                    .push((offer.offer_id.clone(), offer.scid.clone()));
                Ok(json!({
                    "did": did_of(&offer.scid),
                    "offerId": offer.offer_id,
                    "matchCode": link::match_code(&offer.scid, &offer.offer_id, &self.device_key()),
                    "kind": body["kind"],
                    "expires": body["expires"],
                }))
            }
            "identity.joinStatus" => {
                let offer_id = p["offerId"].as_str().unwrap();
                let scid = self
                    .joins
                    .borrow()
                    .iter()
                    .find(|(o, _)| o == offer_id)
                    .map(|(_, s)| s.clone())
                    .ok_or(RpcError {
                        code: 404,
                        message: "not-found".into(),
                    })?;
                let (code, body) = self
                    .store
                    .send(
                        "GET",
                        &format!(
                            "/v1/offers/{scid}/{offer_id}/state?key={}",
                            self.device_key()
                        ),
                        None,
                        None,
                    )
                    .await;
                if code != 200 {
                    return Err(Self::refused(code, &body));
                }
                let mut key_id = None;
                if body["state"] == "spent" {
                    key_id = self.record_if_listed(&scid).await?;
                }
                Ok(json!({
                    "state": body["state"], "did": did_of(&scid), "keyId": key_id, "seq": body["seq"],
                }))
            }
            other => Err(RpcError {
                code: 404,
                message: format!("the test executor has no {other}"),
            }),
        }
    }
}

/// A `Random` that counts up, so word positions are known.
struct Counting(RefCell<u8>);

impl Random for Counting {
    fn fill(&self, out: &mut [u8]) {
        for b in out.iter_mut() {
            let next = *self.0.borrow();
            *b = next;
            *self.0.borrow_mut() = next.wrapping_add(1);
        }
    }
}

fn shown(screen: CreateScreen) -> Vec<String> {
    match screen {
        CreateScreen::ShowWords(words) => words,
        other => panic!("expected the words, got {other:?}"),
    }
}

/// `identity.claimOffer` on the joining executor (the operator's call; the CLI makes it).
async fn claim(
    b: &ViaExecutor<TestExecutor>,
    offer: &str,
    hint: Option<&str>,
) -> Result<Value, RpcError> {
    b.rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": offer, "hint": hint }),
        )
        .await
}

/// `identity.joinStatus` on the joining executor.
async fn join(b: &ViaExecutor<TestExecutor>, offer_id: &Value) -> Value {
    b.rpc()
        .call("identity.joinStatus", json!({ "offerId": offer_id }))
        .await
        .unwrap()
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

/// `create` on `executor` with fixed words; answers the identity.
async fn create(executor: &ViaExecutor<TestExecutor>, vault: &MemoryVault, entropy: u8) -> String {
    confirmed(entropy)
        .finish(executor, vault)
        .await
        .unwrap()
        .did
}

#[tokio::test]
async fn create_makes_an_identity_that_lists_the_executor_as_d1() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();

    let (mut flow, screen) = CreateFlow::start(&Counting(RefCell::new(3)));
    let list = shown(screen);
    assert_eq!(list.len(), 12);
    // Finishing before the words are confirmed is refused.
    assert!(matches!(
        flow.confirm(["a", "b"]),
        Err(ClientError::Step(_))
    ));
    let CreateScreen::ConfirmWords { positions } = flow.words_written(&OsRandom).unwrap() else {
        panic!("expected the confirm screen");
    };
    assert!(positions[0] < positions[1] && positions[1] <= 12);
    // A wrong word asks again for the same positions.
    assert!(matches!(
        flow.confirm(["zzz", &list[positions[1] - 1]]),
        Err(ClientError::WrongWords)
    ));
    flow.confirm([
        &list[positions[0] - 1],
        &list[positions[1] - 1].to_uppercase(),
    ])
    .unwrap();
    let created = flow.finish(&a, &vault).await.unwrap();

    assert_eq!(created.device_key_id, "d1");
    assert_eq!(created.head.seq, 0);
    assert!(vault.pending_incepts().unwrap().is_empty());
    assert_eq!(a.rpc().publishes.get(), 1);
    let status = a.status().await.unwrap();
    assert_eq!(status.did.as_deref(), Some(created.did.as_str()));
    assert_eq!(status.key_id.as_deref(), Some("d1"));
    let events = a.events(&created.did).await.unwrap();
    assert_eq!(events.len(), 1);
    let kept = vault.unlock(&created.scid, MasterUse::Link, 0).unwrap();
    assert_eq!(kept.signer(0).unwrap().index(), 0);
    // The master key is the words' master #0, and the log commits to master #1.
    let seed = words::seed(&words::from_entropy(&{
        let mut e = [0u8; 16];
        Counting(RefCell::new(3)).fill(&mut e);
        e
    }));
    let chain = ad4m_identity_core::fold(&events).unwrap();
    assert_eq!(
        chain.state().masters[0].sign,
        Signer::key(&words::master_key(&seed, 0).unwrap())
    );
    assert_eq!(
        chain.state().next.clone().unwrap(),
        words::next_commitment(&seed, 1).unwrap().0
    );

    // A second identity for the same executor is refused before anything is published.
    let again = CreateFlow::from_words(words::from_entropy(&[9; 16]));
    let (mut again, screen) = again;
    let list = shown(screen);
    let CreateScreen::ConfirmWords { positions } = again.words_written(&OsRandom).unwrap() else {
        panic!()
    };
    again
        .confirm([&list[positions[0] - 1], &list[positions[1] - 1]])
        .unwrap();
    assert!(matches!(
        again.finish(&a, &vault).await,
        Err(ClientError::HasIdentity(did)) if did == created.did
    ));
}

/// A vault that refuses to keep a master key (the person cancelled, no screen lock, …).
struct RefusingVault(MemoryVault);

impl Vault for RefusingVault {
    fn store_master(
        &self,
        _scid: &str,
        _index: u32,
        _secret: ad4m_identity_client::Secret<[u8; 32]>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        Err(ad4m_identity_client::VaultError::Cancelled)
    }

    fn unlock(
        &self,
        scid: &str,
        why: MasterUse,
        now: u64,
    ) -> Result<ad4m_identity_client::Unlocked, ad4m_identity_client::VaultError> {
        self.0.unlock(scid, why, now)
    }

    fn store_pending_incept(&self, incept: &Event) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.store_pending_incept(incept)
    }

    fn pending_incepts(&self) -> Result<Vec<Event>, ad4m_identity_client::VaultError> {
        self.0.pending_incepts()
    }

    fn incept_published(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.incept_published(scid)
    }
}

#[tokio::test]
async fn create_publishes_nothing_when_the_vault_cannot_keep_the_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));

    let refused = confirmed(6)
        .finish(&a, &RefusingVault(MemoryVault::new()))
        .await;
    assert!(
        matches!(
            refused,
            Err(ClientError::Vault(
                ad4m_identity_client::VaultError::Cancelled
            ))
        ),
        "{refused:?}"
    );
    assert_eq!(a.rpc().publishes.get(), 0, "nothing reached the store");
    assert_eq!(a.status().await.unwrap().did, None);
    let refusing = RefusingVault(MemoryVault::new());
    let _ = confirmed(6).finish(&a, &refusing).await;
    assert!(refusing.pending_incepts().unwrap().is_empty());

    // Nothing public happened, so the same words make the identity once the vault works.
    let vault = MemoryVault::new();
    let created = confirmed(6).finish(&a, &vault).await.unwrap();
    assert_eq!(a.status().await.unwrap().did, Some(created.did.clone()));
    assert!(vault.unlock(&created.scid, MasterUse::Link, 0).is_ok());
}

#[tokio::test]
async fn create_finishes_with_the_same_words_after_a_lost_publish_answer() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();

    // The store takes the incept and the executor records it, but the answer never arrives.
    a.rpc().lose_publish_answer.set(true);
    let lost = confirmed(8).finish(&a, &vault).await;
    assert!(
        matches!(&lost, Err(ClientError::Rpc(e)) if e.code == 503),
        "{lost:?}"
    );
    let did = a
        .status()
        .await
        .unwrap()
        .did
        .expect("the publish committed");
    assert_eq!(vault.pending_incepts().unwrap().len(), 1);

    // The same words finish the same identity: no second incept, the same master key.
    let created = confirmed(8).finish(&a, &vault).await.unwrap();
    assert_eq!(created.did, did);
    assert_eq!(created.head.seq, 0);
    assert_eq!(a.events(&did).await.unwrap().len(), 1);
    assert!(vault.pending_incepts().unwrap().is_empty());
    let seed = words::seed(&words::from_entropy(&[8; 16]));
    let kept = vault.unlock(&created.scid, MasterUse::Link, 0).unwrap();
    assert_eq!(
        Signer::key(kept.signer(0).unwrap()),
        Signer::key(&words::master_key(&seed, 0).unwrap())
    );
}

#[tokio::test]
async fn resume_finishes_a_create_from_the_vault_alone() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    assert!(resume(&a, &vault).await.unwrap().is_none());

    // The publish answer is lost and the flow (with the words) is gone.
    a.rpc().lose_publish_answer.set(true);
    assert!(confirmed(10).finish(&a, &vault).await.is_err());
    let did = a
        .status()
        .await
        .unwrap()
        .did
        .expect("the publish committed");

    // Other words are refused while the kept incept is not finished: no second identity.
    let other = confirmed(11).finish(&a, &vault).await;
    assert!(
        matches!(&other, Err(ClientError::CreatePending(d)) if *d == did),
        "{other:?}"
    );

    let created = resume(&a, &vault).await.unwrap().expect("a kept incept");
    assert_eq!(
        (created.did.as_str(), created.device_key_id.as_str()),
        (did.as_str(), "d1")
    );
    assert_eq!(created.head.seq, 0);
    assert_eq!(a.events(&did).await.unwrap().len(), 1);
    assert_eq!(a.rpc().publishes.get(), 2);
    assert!(vault.unlock(&created.scid, MasterUse::Link, 0).is_ok());
    // Done: nothing left to resume, and another create is refused as before.
    assert!(resume(&a, &vault).await.unwrap().is_none());
    assert!(matches!(
        confirmed(11).finish(&a, &vault).await,
        Err(ClientError::HasIdentity(d)) if d == did
    ));
}

#[tokio::test]
async fn resume_publishes_an_incept_whose_publish_never_reached_the_store() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();

    a.rpc().refuse_publish.set(true);
    let failed = confirmed(12).finish(&a, &vault).await;
    assert!(
        matches!(&failed, Err(ClientError::Rpc(e)) if e.code == 503),
        "{failed:?}"
    );
    assert_eq!(a.status().await.unwrap().did, None);
    let kept = vault.pending_incepts().unwrap();
    assert_eq!(kept.len(), 1);
    // The master key is kept already, under the identity the incept makes.
    assert!(vault.unlock(&kept[0].scid, MasterUse::Link, 0).is_ok());

    let created = resume(&a, &vault).await.unwrap().expect("a kept incept");
    assert_eq!(created.scid, kept[0].scid);
    assert_eq!(a.status().await.unwrap().did, Some(created.did));
    assert!(vault.pending_incepts().unwrap().is_empty());
}

#[tokio::test]
async fn link_adds_a_second_executor_with_one_unlock_and_one_match_code() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 5).await;

    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert!(flow
        .link()
        .starts_with("https://coasys.id/o#coasys:offer:1:"));
    let parsed = OfferString::parse_link(&flow.link()).unwrap();
    assert_eq!(parsed.store, STORE_URL);

    // The second executor claims the link and shows its match code.
    let claimed_b = claim(&b, &flow.link(), Some("server")).await.unwrap();
    assert_eq!(claimed_b["did"], did);
    assert_eq!(claimed_b["kind"], "node");
    assert_eq!(join(&b, &claimed_b["offerId"]).await["state"], "claimed");

    // The holder sees the claim and the same code, then confirms.
    let claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(claimed.match_code, claimed_b["matchCode"]);
    assert_eq!(claimed.device_key, b.rpc().device_key());
    assert_eq!(claimed.hint.as_deref(), Some("server"));
    let head = flow.confirm(&claimed).await.unwrap();
    assert_eq!(head.seq, 1);

    let joined = join(&b, &claimed_b["offerId"]).await;
    assert_eq!(
        (&joined["state"], &joined["keyId"], &joined["seq"]),
        (&json!("spent"), &json!("d2"), &json!(1))
    );
    assert_eq!(b.status().await.unwrap().did.as_deref(), Some(did.as_str()));
    // One unlock served the whole link: offer, list requests and the add.
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
}

#[tokio::test]
async fn revoke_ends_the_second_executors_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 6).await;
    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed_b = claim(&b, &flow.offer_string(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    flow.confirm(&claimed).await.unwrap();
    join(&b, &claimed_b["offerId"]).await;

    let d2 = key_id_of(&a, &did, &b.rpc().device_key())
        .await
        .unwrap()
        .unwrap();
    assert_eq!(d2, "d2");
    // A key the log does not list is refused before the vault opens.
    assert!(matches!(
        revoke(&a, &vault, &did, &["d9".to_string()], &SystemClock).await,
        Err(ClientError::Protocol(_))
    ));
    let head = revoke(&a, &vault, &did, std::slice::from_ref(&d2), &SystemClock)
        .await
        .unwrap();
    assert_eq!(head.seq, 2);
    let status = b.status().await.unwrap();
    assert_eq!(status.key_id.as_deref(), Some("d2"));
    assert_eq!(status.revoked, Some(2));
    assert_eq!(a.status().await.unwrap().revoked, None);
    assert_eq!(vault.unlocks(), vec![MasterUse::Link, MasterUse::Revoke]);

    // A revoked key cannot join again: the store keeps every key it ever listed (the store's
    // one-key check).
    let again = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let refused = claim(&b, &again.link(), None).await.unwrap_err();
    assert_eq!(refused.code, 409, "{refused:?}");
}

#[tokio::test]
async fn a_link_without_the_master_key_never_opens_an_offer() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 7).await;
    let empty = MemoryVault::new();
    assert!(matches!(
        LinkFlow::open(
            &a,
            &empty,
            &did,
            OfferKind::Node,
            STORE_URL,
            &SystemClock,
            &OsRandom
        )
        .await,
        Err(ClientError::Vault(
            ad4m_identity_client::VaultError::NotFound
        ))
    ));
}
