//! The flows against a real `identity-store` (its router, in process, on a temp SQLite file),
//! through a test executor that answers the executor's `identity.*` calls the way
//! `rust-executor/src/identity` does: it holds one device key, co-signs, claims, and passes
//! everything else to the store's routes. The real executor runs these flows in `tests/js`
//! (`identity.test.ts`).
#![cfg(not(target_arch = "wasm32"))]

use std::cell::{Cell, RefCell};
use std::sync::Arc;

use ad4m_hash_chain::{canonical_json, encode_proof_value, hash_bytes};
use ad4m_identity_client::flows::create::{resume, CreateFlow, CreateScreen};
use ad4m_identity_client::flows::link::{Claimed, LinkFlow};
use ad4m_identity_client::flows::master::{retry_pushes, send_master, take_master};
use ad4m_identity_client::flows::requests::{answer, check_poster, Answered};
use ad4m_identity_client::flows::revoke::{key_id_of, revoke};
use ad4m_identity_client::record::{self, Change};
use ad4m_identity_client::{
    ClientError, Clock, ExecutorRpc, MasterUse, MemoryVault, OsRandom, PendingPush, Random,
    RpcError, SystemClock, Vault, ViaExecutor,
};
use ad4m_identity_core::build::{cosign, Signer};
use ad4m_identity_core::inbox::{
    self, check_payload, pre_address, sign_pre_auth, Accept, InboxAddress, InboxBody,
};
use ad4m_identity_core::link::{self, DeviceString, OfferKind, OfferString};
use ad4m_identity_core::record::{
    self as record_core, DeviceEntry, DeviceKind, HoldsMaster, PendingRevocation, RevocationReason,
};
use ad4m_identity_core::words;
use ad4m_identity_core::{did_of, signing_bytes, Event, KelChain, Op, REQUEST_DOMAIN};
use ad4m_ldk::identity::{DeviceRequestBody, RequestBody, SignedRequest};
use async_trait::async_trait;
use axum::body::Body;
use axum::http::Request;
use axum::Router;
use ed25519_dalek::SigningKey;
use http_body_util::BodyExt;
use identity_store::db::Db;
use identity_store::inbox::PRE_INBOX_HEADER;
use identity_store::request::{encode_header, REQUEST_HEADER};
use identity_store::{router, system_clock, Settings, Store, StoreKey};
use serde_json::{json, Value};
use tower::ServiceExt;

const STORE_URL: &str = "http://store.test";
const STORE_SECRET: [u8; 32] = [7; 32];

/// The log of a store page, folded.
fn fold_page(page: &Value) -> KelChain {
    let events: Vec<Event> = page["events"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| Event::from_json(e).unwrap())
        .collect();
    ad4m_identity_core::fold(&events).unwrap()
}

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
            StoreKey::from_secret(STORE_SECRET),
            system_clock(),
            Settings {
                pow_bits: 8,
                // A test makes an identity's writes (offers, inbox puts, record writes, posts)
                // within a second; the store's bucket is not under test here.
                identity_burst: 100,
                ..Settings::default()
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
        match header {
            Some(header) => {
                self.send_with(method, path, body, REQUEST_HEADER, header)
                    .await
            }
            None => self.request(method, path, body, None).await,
        }
    }

    /// `send` with the header `name` (a pre-join read's).
    async fn send_with(
        &self,
        method: &str,
        path: &str,
        body: Option<&Value>,
        name: &str,
        header: String,
    ) -> (u16, Value) {
        self.request(method, path, body, Some((name, header))).await
    }

    async fn request(
        &self,
        method: &str,
        path: &str,
        body: Option<&Value>,
        header: Option<(&str, String)>,
    ) -> (u16, Value) {
        let mut request = Request::builder().method(method).uri(path);
        if body.is_some() {
            request = request.header("content-type", "application/json");
        }
        if let Some((name, value)) = header {
            request = request.header(name, value);
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
    /// The next `identity.inboxPut` fails before it reaches the store.
    refuse_inbox_put: Cell<bool>,
    /// The next `identity.recordPut` answers a stale version, as when another writer was first.
    stale_record_put: Cell<bool>,
    /// `identity.recordPut` calls that reached the store.
    record_puts: Cell<usize>,
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
            refuse_inbox_put: Cell::new(false),
            stale_record_put: Cell::new(false),
            record_puts: Cell::new(0),
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

    /// The identity this executor's key joined, its key's id there, the log and the head the
    /// store signed.
    async fn position(&self) -> Result<(String, String, KelChain, Value), RpcError> {
        let no_identity = || RpcError {
            code: 409,
            message: "no-identity".into(),
        };
        let scid = self.identity.borrow().clone().ok_or_else(no_identity)?;
        let page = self.log(&scid).await?;
        let chain = fold_page(&page);
        let key_id = chain
            .state()
            .key_id_of(&self.device_key())
            .cloned()
            .ok_or_else(no_identity)?;
        Ok((scid, key_id, chain, page["head"].clone()))
    }

    /// A store request for `method path` with the JSON `body` (none for a read), signed by
    /// this executor's device key at the log's head, as `DevicePosition::request` does.
    async fn device_request(
        &self,
        method: &str,
        path: &str,
        body: Option<&Value>,
    ) -> Result<String, RpcError> {
        let (scid, key_id, chain, head) = self.position().await?;
        let bytes = body
            .map(|b| canonical_json(b).unwrap().into_bytes())
            .unwrap_or_default();
        let mut nonce = [0u8; 16];
        OsRandom.fill(&mut nonce);
        let req = RequestBody {
            did: did_of(&scid),
            store: head["store"].as_str().unwrap().to_string(),
            key_id,
            kel_seq: chain.head().seq,
            kel_hash: chain.head().hash.clone(),
            method: method.into(),
            path: path.into(),
            body_hash: hash_bytes(&bytes),
            nonce: bs58::encode(nonce).into_string(),
            expires: SystemClock.now() + 60,
        };
        let bytes = signing_bytes(REQUEST_DOMAIN, &req).unwrap();
        let sig = encode_proof_value(&Signer::sign(&self.device, &bytes));
        Ok(encode_header(&SignedRequest { req, sig }))
    }

    /// `method path` at the store with a device-signed request; a refusal as the executor
    /// answers it.
    async fn as_device(
        &self,
        method: &str,
        path: &str,
        body: Option<&Value>,
    ) -> Result<(u16, Value), RpcError> {
        let signed_path = path.split('?').next().unwrap();
        let header = self.device_request(method, signed_path, body).await?;
        let (code, answer) = self.store.send(method, path, body, Some(header)).await;
        if code >= 400 && code != 404 {
            return Err(Self::refused(code, &answer));
        }
        Ok((code, answer))
    }

    /// The device's X25519 `enc` secret.
    fn enc_secret(&self) -> ad4m_identity_core::X25519Secret {
        ad4m_identity_core::x25519_secret_of_ed25519(&self.device)
    }

    /// What this executor does while its device string `r` shows (`identity.prepareDevice`):
    /// reads its pre-join inbox, opens the ticket, checks it against the log, and claims it
    /// (saying it may keep the master key, as the desktop's executor does). Answers the claim.
    async fn take_ticket(&self, r: &str) -> Result<Value, RpcError> {
        let address = pre_address(&self.device_key(), r).unwrap();
        let path = format!("/v1/inbox/pre/{address}");
        let mut nonce = [0u8; 16];
        OsRandom.fill(&mut nonce);
        let auth = sign_pre_auth(
            r,
            &StoreKey::from_secret(STORE_SECRET).did(),
            "GET",
            &path,
            &bs58::encode(nonce).into_string(),
            SystemClock.now() + 60,
            &self.device,
        )
        .unwrap();
        let header = {
            use base64::Engine;
            base64::engine::general_purpose::URL_SAFE_NO_PAD
                .encode(canonical_json(&serde_json::to_value(&auth).unwrap()).unwrap())
        };
        let (code, messages) = self
            .store
            .send_with("GET", &path, None, PRE_INBOX_HEADER, header)
            .await;
        assert_eq!(code, 200, "{messages}");
        let sealed = messages[0]["sealed"].as_str().unwrap();
        let payload = inbox::open(&self.enc_secret(), &InboxAddress::Pre(address), sealed).unwrap();
        let InboxBody::OfferTicket(ticket) = &payload.body else {
            panic!("expected a ticket");
        };
        let chain = fold_page(&self.log(&ticket.scid).await?);
        let accept =
            check_payload(&payload, &self.device_key(), &chain, SystemClock.now()).unwrap();
        let Accept::ClaimOffer(ticket) = accept else {
            panic!("expected a ticket to claim");
        };
        self.call(
            "identity.claimOffer",
            json!({ "offer": ticket.offer_string().encode(), "canKeepMaster": true }),
        )
        .await
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
                    p["canKeepMaster"].as_bool().unwrap_or(false),
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
            "identity.inboxPut" => {
                if self.refuse_inbox_put.replace(false) {
                    return Err(RpcError {
                        code: 503,
                        message: "unavailable: the store is down".into(),
                    });
                }
                let path = match &p["address"] {
                    a if a["pre"].is_string() => {
                        format!("/v1/inbox/pre/{}", a["pre"].as_str().unwrap())
                    }
                    a => format!(
                        "/v1/inbox/{}/{}",
                        a["scid"].as_str().unwrap(),
                        a["keyId"].as_str().unwrap()
                    ),
                };
                let mut write = json!({ "sealed": p["sealed"] });
                if !p["bind"].is_null() {
                    write["bind"] = p["bind"].clone();
                }
                let request: SignedRequest = serde_json::from_value(p["request"].clone()).unwrap();
                let (code, body) = self
                    .store
                    .send("PUT", &path, Some(&write), Some(encode_header(&request)))
                    .await;
                if code != 201 {
                    return Err(Self::refused(code, &body));
                }
                Ok(body)
            }
            // As the executor with the custody channel: the oldest current master key of its
            // joined inbox, checked against the log; a stale one is deleted.
            "identity.takeMaster" => {
                let (scid, key_id, chain, _) = self.position().await?;
                let inbox = format!("/v1/inbox/{scid}/{key_id}");
                let (_, messages) = self.as_device("GET", &inbox, None).await?;
                let address = InboxAddress::Joined { scid, key_id };
                for message in messages.as_array().unwrap() {
                    let id = message["id"].as_str().unwrap();
                    let sealed = message["sealed"].as_str().unwrap();
                    let payload = inbox::open(&self.enc_secret(), &address, sealed).unwrap();
                    let accept =
                        check_payload(&payload, &self.device_key(), &chain, SystemClock.now());
                    if let (
                        Ok(Accept::KeepMaster { index }),
                        InboxBody::Master { did, secret, .. },
                    ) = (accept, &payload.body)
                    {
                        return Ok(
                            json!({ "messageId": id, "did": did, "index": index, "secret": secret }),
                        );
                    }
                    self.as_device("DELETE", &format!("{inbox}/{id}"), None)
                        .await?;
                }
                Ok(Value::Null)
            }
            "identity.inboxAck" => {
                let (scid, key_id, _, _) = self.position().await?;
                let id = p["id"].as_str().unwrap();
                self.as_device("DELETE", &format!("/v1/inbox/{scid}/{key_id}/{id}"), None)
                    .await?;
                Ok(json!(true))
            }
            "identity.record" => {
                let (scid, ..) = self.position().await?;
                let (code, body) = self
                    .as_device("GET", &format!("/v1/private/{scid}"), None)
                    .await?;
                Ok(if code == 404 { Value::Null } else { body })
            }
            "identity.recordPut" => {
                if self.stale_record_put.replace(false) {
                    return Err(Self::refused(409, &json!({ "error": "version" })));
                }
                self.record_puts.set(self.record_puts.get() + 1);
                let (scid, ..) = self.position().await?;
                let (_, body) = self
                    .as_device("PUT", &format!("/v1/private/{scid}"), Some(&p["record"]))
                    .await?;
                Ok(body)
            }
            "identity.unwrapRecordKey" => {
                let (scid, ..) = self.position().await?;
                let key = record_core::unwrap_key(
                    p["wrapped"].as_str().unwrap(),
                    &scid,
                    &self.enc_secret(),
                )
                .map_err(|e| RpcError {
                    code: 400,
                    message: format!("open: {e}"),
                })?;
                Ok(json!({ "key": bs58::encode(key.as_ref()).into_string() }))
            }
            "identity.requests" => {
                let (scid, ..) = self.position().await?;
                let since = p["since"].as_u64().unwrap_or(0);
                let (_, body) = self
                    .as_device("GET", &format!("/v1/requests/{scid}?since={since}"), None)
                    .await?;
                Ok(body)
            }
            "identity.postRequest" => {
                let (scid, ..) = self.position().await?;
                let expires = p["expires"]
                    .as_u64()
                    .unwrap_or(SystemClock.now() + 7 * 86_400);
                let post = json!({ "body": p["body"], "expires": expires });
                let (_, body) = self
                    .as_device("POST", &format!("/v1/requests/{scid}"), Some(&post))
                    .await?;
                Ok(body)
            }
            "identity.deleteRequest" => {
                let (scid, ..) = self.position().await?;
                let request: SignedRequest = serde_json::from_value(p["request"].clone()).unwrap();
                let (code, body) = self
                    .store
                    .send(
                        "DELETE",
                        &format!("/v1/requests/{scid}/{}", p["id"].as_str().unwrap()),
                        None,
                        Some(encode_header(&request)),
                    )
                    .await;
                if code != 204 {
                    return Err(Self::refused(code, &body));
                }
                Ok(json!(true))
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

    fn keep_push(&self, push: &PendingPush) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.keep_push(push)
    }

    fn pending_pushes(&self) -> Result<Vec<PendingPush>, ad4m_identity_client::VaultError> {
        self.0.pending_pushes()
    }

    fn push_done(&self, push: &PendingPush) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.push_done(push)
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
        None,
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
    assert!(!claimed.can_keep_master && !flow.may_keep(&claimed));
    // A node's claim offers no master key.
    let linked = flow
        .confirm(&claimed, entry("node.example", DeviceKind::Server), false)
        .await
        .unwrap();
    assert_eq!((linked.head.seq, linked.key_id.as_str()), (1, "d2"));
    assert!(linked.master.is_none());
    assert_eq!(linked.record.unwrap(), 1);

    let joined = join(&b, &claimed_b["offerId"]).await;
    assert_eq!(
        (&joined["state"], &joined["keyId"], &joined["seq"]),
        (&json!("spent"), &json!("d2"), &json!(1))
    );
    // The record lists the new device, and both executors read it.
    for reader in [&a, &b] {
        let opened = record::read(reader, &did).await.unwrap().unwrap();
        assert_eq!(opened.version, 1);
        assert_eq!(
            opened.record.devices["d2"],
            entry("node.example", DeviceKind::Server)
        );
    }
    assert_eq!(b.status().await.unwrap().did.as_deref(), Some(did.as_str()));
    // One unlock served the whole link: offer, list requests and the add.
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
}

/// An offer bound to a scanned device key: its ticket carries the offer string's fields and
/// the signed offer bound to that key, another key's claim is refused, and the bound device
/// links as with an unbound offer.
#[tokio::test]
async fn a_bound_offer_takes_only_its_device_and_gives_its_ticket() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let stranger = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 8).await;

    let device_key = b.rpc().device_key();
    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Device,
        Some(&device_key),
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let ticket = flow.ticket();
    assert_eq!(ticket.offer_string().encode(), flow.offer_string());
    assert_eq!(
        ticket.signed_offer.offer.device_key.as_deref(),
        Some(device_key.as_str())
    );
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    let now = SystemClock.now();
    link::verify_offer(&ticket.signed_offer, &chain, now).unwrap();

    let refused = claim(&stranger, &flow.offer_string(), None)
        .await
        .unwrap_err();
    assert_eq!(refused.code, 403, "{}", refused.message);
    let claimed_b = claim(&b, &flow.offer_string(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(claimed.match_code, claimed_b["matchCode"]);
    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), false)
        .await
        .unwrap();
    assert_eq!(linked.head.seq, 1);
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
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed_b = claim(&b, &flow.offer_string(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    flow.confirm(&claimed, entry("node.example", DeviceKind::Server), false)
        .await
        .unwrap();
    join(&b, &claimed_b["offerId"]).await;

    let d2 = key_id_of(&a, &did, &b.rpc().device_key())
        .await
        .unwrap()
        .unwrap();
    assert_eq!(d2, "d2");
    // A revocation waiting for an app with the master key, written by any device.
    let pending = PendingRevocation {
        key_id: d2.clone(),
        reason: RevocationReason::LeftHost,
        at: 1,
    };
    for _ in 0..2 {
        record::write(
            &b,
            &did,
            &[Change::PendingRevocation(pending.clone())],
            &OsRandom,
        )
        .await
        .unwrap();
    }
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.pending_revocations,
        vec![pending],
        "one per key"
    );
    // A key the log does not list is refused before the vault opens.
    assert!(matches!(
        revoke(
            &a,
            &vault,
            &did,
            &["d9".to_string()],
            &SystemClock,
            &OsRandom
        )
        .await,
        Err(ClientError::Protocol(_))
    ));
    let revoked = revoke(
        &a,
        &vault,
        &did,
        std::slice::from_ref(&d2),
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(revoked.head.seq, 2);
    // The record leaves the revoked device out, and is no longer wrapped to it.
    assert_eq!(revoked.record.unwrap(), 4);
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert!(opened.record.devices.is_empty());
    assert!(opened.record.pending_revocations.is_empty());
    let sealed = a.record().await.unwrap().unwrap();
    assert_eq!(sealed.wrapped.keys().collect::<Vec<_>>(), ["d1"]);
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
        None,
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
            None,
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

/// A device's record entry as a surface writes it at the confirm.
fn entry(name: &str, kind: DeviceKind) -> DeviceEntry {
    DeviceEntry {
        name: name.into(),
        kind,
        place: None,
        address: None,
        runs_executor: true,
        holds_master: HoldsMaster::No,
        operator: None,
    }
}

/// The device string `device` shows (`identity.prepareDevice`).
fn device_string(device: &ViaExecutor<TestExecutor>, store: &str, exp: u64) -> DeviceString {
    let mut r = [0u8; 16];
    OsRandom.fill(&mut r);
    DeviceString {
        sign: device.rpc().device_key(),
        enc: device.rpc().device_enc(),
        r: bs58::encode(r).into_string(),
        exp,
        hint: Some("Desk".into()),
        store: store.into(),
    }
}

/// `a` links `b` with an unbound `device` offer and `entry`, without the master key; answers
/// the new key's id.
async fn link_app(
    a: &ViaExecutor<TestExecutor>,
    vault: &MemoryVault,
    did: &str,
    b: &ViaExecutor<TestExecutor>,
    entry: DeviceEntry,
) -> String {
    let flow = LinkFlow::open(
        a,
        vault,
        did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed_b = claim(b, &flow.offer_string(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    let linked = flow.confirm(&claimed, entry, false).await.unwrap();
    linked.record.unwrap();
    join(b, &claimed_b["offerId"]).await;
    linked.key_id
}

/// The holder scans the desktop's device string: a bound offer, its ticket in the pre-join
/// inbox, the desktop's claim, the same match code; on confirm with "keep the master key" the
/// key goes into the desktop's inbox and the record lists it as offered; the desktop's custody
/// service takes it and marks itself as holding it. One unlock on the holder's side.
#[tokio::test]
async fn a_holder_scans_a_device_string_and_gives_the_master_key_to_the_desktop() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 13).await;
    let now = SystemClock.now();

    // An expired string, or one for another store, is refused before any unlock.
    for (device, why) in [
        (device_string(&desk, STORE_URL, now - 1), "expired"),
        (
            device_string(&desk, "http://elsewhere.test", now + 300),
            "elsewhere",
        ),
    ] {
        let refused = LinkFlow::scan(
            &a,
            &vault,
            &did,
            OfferKind::Device,
            &device,
            STORE_URL,
            &SystemClock,
            &OsRandom,
        )
        .await;
        assert!(
            matches!(&refused, Err(ClientError::Protocol(m)) if m.contains(why)),
            "{why}"
        );
    }
    assert!(vault.unlocks().is_empty());

    // The same store with a trailing slash is the same store.
    let device = device_string(&desk, &format!("{STORE_URL}/"), now + 300);
    let flow = LinkFlow::scan(
        &a,
        &vault,
        &did,
        OfferKind::Device,
        &device,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    let claimed_desk = desk.rpc().take_ticket(&device.r).await.unwrap();
    let claimed: Claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(claimed.match_code, claimed_desk["matchCode"]);
    assert_eq!(claimed.device_key, desk.rpc().device_key());
    assert!(claimed.can_keep_master && flow.may_keep(&claimed));

    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert_eq!(linked.key_id, "d2");
    assert!(matches!(&linked.master, Some(Ok(_))), "{:?}", linked.master);
    assert_eq!(linked.record.unwrap(), 1);
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert!(vault.pending_pushes().unwrap().is_empty());
    join(&desk, &claimed_desk["offerId"]).await;
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices["d2"].holds_master,
        HoldsMaster::Offered
    );

    // The desktop's custody service, after its own unlock.
    let desk_vault = MemoryVault::new();
    let taken = take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");
    assert_eq!((taken.did.as_str(), taken.index), (did.as_str(), 0));
    assert_eq!(taken.record.unwrap(), 2);
    let scid = ad4m_identity_core::scid_of(&did).unwrap();
    let held = vault.unlock(scid, MasterUse::Link, 0).unwrap();
    let kept = desk_vault.unlock(scid, MasterUse::Link, 0).unwrap();
    assert_eq!(
        Signer::key(kept.signer(0).unwrap()),
        Signer::key(held.signer(0).unwrap())
    );
    // Taken once: the message is acknowledged.
    assert!(take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .is_none());
    // Both devices read the same record: the desktop holds the master key now.
    for reader in [&a, &desk] {
        let opened = record::read(reader, &did).await.unwrap().unwrap();
        assert_eq!(opened.version, 2);
        assert_eq!(opened.record.devices["d2"].holds_master, HoldsMaster::Yes);
        assert_eq!(opened.record.devices["d2"].name, "Desk");
    }
}

/// A claim that does not say it may keep the master key gets none: refused before the add.
#[tokio::test]
async fn a_claim_without_can_keep_master_gets_no_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 14).await;
    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Device,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    claim(&b, &flow.offer_string(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    let refused = flow
        .confirm(&claimed, entry("Phone", DeviceKind::PhoneApp), true)
        .await;
    assert!(
        matches!(refused, Err(ClientError::NotAKeeper(_))),
        "{refused:?}"
    );
    assert_eq!(a.events(&did).await.unwrap().len(), 1, "nothing was added");
}

/// A push the store did not take stays on the vault's retry list; the next round pushes it
/// with one unlock and marks the device offered; a push to a key the log does not hold is
/// forgotten.
#[tokio::test]
async fn a_failed_push_stays_on_the_retry_list_until_it_passes() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 15).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;

    // Nothing to retry: no unlock.
    let idle = retry_pushes(&a, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert!(idle.pushes.is_empty() && idle.record.is_none());
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);

    a.rpc().refuse_inbox_put.set(true);
    let failed = send_master(&a, &vault, &did, &d2, &SystemClock, &OsRandom).await;
    assert!(
        matches!(&failed, Err(ClientError::Rpc(e)) if e.code == 503),
        "{failed:?}"
    );
    let pending = PendingPush {
        scid: scid.clone(),
        key_id: d2.clone(),
    };
    assert_eq!(vault.pending_pushes().unwrap(), vec![pending]);
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::No);

    vault
        .keep_push(&PendingPush {
            scid,
            key_id: "d9".into(),
        })
        .unwrap();
    let retried = retry_pushes(&a, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(retried.pushes.len(), 1);
    assert_eq!(retried.pushes[0].0, d2);
    assert!(retried.pushes[0].1.is_ok());
    assert!(matches!(retried.record, Some(Ok(_))));
    assert!(vault.pending_pushes().unwrap().is_empty());
    assert_eq!(
        vault.unlocks(),
        vec![
            MasterUse::Link,
            MasterUse::SendMaster,
            MasterUse::SendMaster
        ]
    );
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );
    assert!(take_master(&desk, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_some());
}

/// An executor whose record writes always meet a newer version (as the agent language
/// passes the store's `409 {version}`).
struct AlwaysStale<'a>(&'a TestExecutor);

#[async_trait(?Send)]
impl ExecutorRpc for AlwaysStale<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.recordPut" {
            return Err(RpcError {
                code: 400,
                message: "invalid: version".into(),
            });
        }
        self.0.call(method, params).await
    }
}

/// Each write applies its change to the newest version: a rename by the device and the
/// holder's `holdsMaster` both stay, and a write that meets a stale version reads and merges
/// again, a bounded number of times.
#[tokio::test]
async fn record_writes_merge_into_the_newest_version() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 16).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;

    record::write(
        &desk,
        &did,
        &[Change::Rename {
            key_id: d2.clone(),
            name: "Studio Mac".into(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    a.rpc().stale_record_put.set(true);
    let puts = a.rpc().record_puts.get();
    let version = record::write(
        &a,
        &did,
        &[Change::HoldsMaster {
            key_id: d2.clone(),
            holds: HoldsMaster::Offered,
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(version, 3);
    assert_eq!(a.rpc().record_puts.get(), puts + 1);
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].name, "Studio Mac");
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );

    let busy = ViaExecutor::new(AlwaysStale(a.rpc()));
    let refused = record::write(&busy, &did, &[], &OsRandom).await;
    assert!(
        matches!(refused, Err(ClientError::RecordBusy)),
        "{refused:?}"
    );
}

/// A device asks for the master key; the holder checks the poster, sends the key, marks the
/// device offered and deletes the request. A request whose body changed after it was signed,
/// or that names another poster, is refused; one asking for a rotation is left for it.
#[tokio::test]
async fn the_holder_answers_a_devices_send_master_request() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 17).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;

    let id = desk
        .post_request(&DeviceRequestBody::SendMaster {}, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    assert_eq!(listed.len(), 1);
    let request = &listed[0];
    assert_eq!(
        (request.id.as_str(), request.by.as_str()),
        (id.as_str(), d2.as_str())
    );
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    check_poster(request, &did, &chain).unwrap();
    let mut changed = request.clone();
    changed.expires += 1;
    assert!(matches!(
        check_poster(&changed, &did, &chain),
        Err(ClientError::Poster(_))
    ));
    let mut other = request.clone();
    other.by = "d1".into();
    assert!(matches!(
        check_poster(&other, &did, &chain),
        Err(ClientError::Poster(_))
    ));

    let Answered::MasterSent(sent) = answer(&a, &vault, &did, request, &SystemClock, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected the master key sent");
    };
    assert_eq!(sent.record.unwrap(), 2);
    assert!(a.requests(0).await.unwrap().is_empty());
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );
    assert!(take_master(&desk, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_some());

    desk.post_request(
        &DeviceRequestBody::ConfirmRevoke {
            key_ids: vec!["d1".into()],
            rotate: true,
        },
        None,
    )
    .await
    .unwrap();
    let listed = a.requests(0).await.unwrap();
    assert!(matches!(
        answer(&a, &vault, &did, &listed[0], &SystemClock, &OsRandom).await,
        Err(ClientError::NeedsWords)
    ));
    assert_eq!(
        a.requests(0).await.unwrap().len(),
        1,
        "left for the rotation"
    );
}

/// The master key goes only to a device the record lists as an app: neither on the server's
/// request nor by the person's choice, and the vault is never unlocked for it.
#[tokio::test]
async fn a_server_that_asks_for_the_master_key_gets_none() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let server = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 18).await;
    let d2 = link_app(&a, &vault, &did, &server, entry("node", DeviceKind::Server)).await;
    server
        .post_request(&DeviceRequestBody::SendMaster {}, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    assert!(matches!(
        answer(&a, &vault, &did, &listed[0], &SystemClock, &OsRandom).await,
        Err(ClientError::NotAKeeper(_))
    ));
    assert!(matches!(
        send_master(&a, &vault, &did, &d2, &SystemClock, &OsRandom).await,
        Err(ClientError::NotAKeeper(_))
    ));
    // Nor to a device key the record does not list at all.
    assert!(matches!(
        send_master(&a, &vault, &did, "d1", &SystemClock, &OsRandom).await,
        Err(ClientError::NotAKeeper(_))
    ));
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert!(vault.pending_pushes().unwrap().is_empty());
}

/// A clock `ahead` seconds past the system's.
struct Ahead(u64);

impl Clock for Ahead {
    fn now(&self) -> u64 {
        SystemClock.now() + self.0
    }
}

/// A request answered after it expired is refused before the unlock, and stays in the box.
#[tokio::test]
async fn an_expired_request_is_refused() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 20).await;
    link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    desk.post_request(
        &DeviceRequestBody::SendMaster {},
        Some(SystemClock.now() + 60),
    )
    .await
    .unwrap();
    let listed = a.requests(0).await.unwrap();
    let refused = answer(&a, &vault, &did, &listed[0], &Ahead(61), &OsRandom).await;
    assert!(
        matches!(&refused, Err(ClientError::Protocol(m)) if m.contains("expired")),
        "{refused:?}"
    );
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert_eq!(a.requests(0).await.unwrap().len(), 1);
}

/// A device without the master key asks to revoke another; the holder revokes it, the record
/// drops it and is no longer wrapped to it, and the request is deleted.
#[tokio::test]
async fn the_holder_answers_a_confirm_revoke_request() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 19).await;
    link_app(
        &a,
        &vault,
        &did,
        &phone,
        entry("Phone", DeviceKind::PhoneApp),
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
    )
    .await;

    phone
        .post_request(
            &DeviceRequestBody::ConfirmRevoke {
                key_ids: vec![d3.clone()],
                rotate: false,
            },
            None,
        )
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    let Answered::Revoked(revoked) = answer(&a, &vault, &did, &listed[0], &SystemClock, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected a revocation");
    };
    revoked.record.unwrap();
    assert_eq!(host.status().await.unwrap().revoked, Some(revoked.head.seq));
    assert!(a.requests(0).await.unwrap().is_empty());
    let opened = record::read(&a, &did).await.unwrap().unwrap();
    assert!(!opened.record.devices.contains_key(&d3));
    let sealed = a.record().await.unwrap().unwrap();
    assert_eq!(sealed.wrapped.keys().collect::<Vec<_>>(), ["d1", "d2"]);
    assert_eq!(
        vault.unlocks(),
        vec![MasterUse::Link, MasterUse::Link, MasterUse::Revoke]
    );
}
