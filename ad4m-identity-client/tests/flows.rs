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
use ad4m_identity_client::flows::master::{
    refresh_master, remove_master, retry_pushes, send_master, take_master, Refreshed, Taken,
};
use ad4m_identity_client::flows::requests::{
    answer, answer_with_words, ask_confirm_revoke, ask_for_master, check_poster, Answered,
};
use ad4m_identity_client::flows::restore::{ListedDevice, RestoreFlow};
use ad4m_identity_client::flows::revoke::{key_id_of, revoke, revoke_and_rotate};
use ad4m_identity_client::flows::rotate::{rotate, MAX_TRIES};
use ad4m_identity_client::record::{self, Change};
use ad4m_identity_client::{
    ClientError, Clock, ExecutorRpc, MasterUse, MemoryVault, OsRandom, PendingPush, Random,
    RpcError, StageToken, SystemClock, Vault, ViaExecutor,
};
use ad4m_identity_core::build::{self, cosign, Signer};
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

/// The signed request a record call passes, if any.
fn given_request(p: &Value) -> Option<SignedRequest> {
    (!p["request"].is_null()).then(|| serde_json::from_value(p["request"].clone()).unwrap())
}

/// The scid of a given record request, refused (as the executor does) unless it is a
/// `method` of `/v1/private/{scid}`.
fn record_scid(request: &SignedRequest, method: &str) -> Result<String, RpcError> {
    let scid = request
        .req
        .path
        .strip_prefix("/v1/private/")
        .filter(|_| request.req.method == method)
        .ok_or(RpcError {
            code: 400,
            message: "invalid: the request is not for this identity's record".into(),
        })?;
    Ok(scid.to_string())
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
    /// The next `identity.recordPut` fails before it reaches the store.
    refuse_record_put: Cell<bool>,
    /// The next so many `identity.recordPut` calls fail before they reach the store (an
    /// outage over more than one write).
    refuse_record_puts: Cell<usize>,
    /// `identity.record` answers this stored record instead of the store's (a store that
    /// serves back an older version).
    serve_record: RefCell<Option<Value>>,
    /// The next `identity.recordPut` answers a stale version, as when another writer was first.
    stale_record_put: Cell<bool>,
    /// `identity.recordPut` calls that reached the store.
    record_puts: Cell<usize>,
    /// Changes every claim view `identity.offers` answers (a store that swaps a field).
    tamper_claim: Cell<Option<fn(&mut Value)>>,
    /// `identity.cosignIncept` answers another valid incept (its own master) listing this key.
    swap_incept: Cell<bool>,
    /// `identity.openOffer` answers another offer id.
    other_offer_id: Cell<bool>,
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
            refuse_record_put: Cell::new(false),
            refuse_record_puts: Cell::new(0),
            serve_record: RefCell::new(None),
            stale_record_put: Cell::new(false),
            record_puts: Cell::new(0),
            tamper_claim: Cell::new(None),
            swap_incept: Cell::new(false),
            other_offer_id: Cell::new(false),
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
                    "storeBehind": null,
                    "store": { "url": STORE_URL, "trusted": true },
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
                if self.swap_incept.get() {
                    use ad4m_identity_core::{DeviceKey, KeyLife};
                    let master = SigningKey::from_bytes(&[77; 32]);
                    let next = ad4m_identity_core::commitment(&Signer::key(&master), "salt");
                    let device = DeviceKey {
                        id: id.clone(),
                        sign: self.device_key(),
                        enc: self.device_enc(),
                        life: KeyLife::Standing,
                    };
                    event = ad4m_identity_core::build::incept_human(&master, next, vec![device])
                        .unwrap();
                }
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
                let mut body = body;
                if self.other_offer_id.get() {
                    body["offerId"] = json!("another-offer");
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
                let mut body = body;
                if let (Some(tamper), Some(listings)) =
                    (self.tamper_claim.get(), body.as_array_mut())
                {
                    for listing in listings {
                        if listing["state"]["state"] == "claimed" {
                            tamper(&mut listing["state"]["claim"]);
                        }
                    }
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
            // As the executor does: a caller's signed request (the master key's, on a restore)
            // goes to the store unchanged; without one, its device key signs.
            "identity.record" => {
                if let Some(served) = self.serve_record.borrow().as_ref() {
                    return Ok(served.clone());
                }
                let (code, body) = match given_request(&p) {
                    Some(request) => {
                        let scid = record_scid(&request, "GET")?;
                        let path = format!("/v1/private/{scid}");
                        let (code, body) = self
                            .store
                            .send("GET", &path, None, Some(encode_header(&request)))
                            .await;
                        if code >= 400 && code != 404 {
                            return Err(Self::refused(code, &body));
                        }
                        (code, body)
                    }
                    None => {
                        let (scid, ..) = self.position().await?;
                        self.as_device("GET", &format!("/v1/private/{scid}"), None)
                            .await?
                    }
                };
                Ok(if code == 404 { Value::Null } else { body })
            }
            "identity.recordPut" => {
                if self.refuse_record_put.replace(false) || self.refuse_record_puts.get() > 0 {
                    self.refuse_record_puts
                        .set(self.refuse_record_puts.get().saturating_sub(1));
                    return Err(RpcError {
                        code: 503,
                        message: "unavailable: the store is down".into(),
                    });
                }
                if self.stale_record_put.replace(false) {
                    return Err(Self::refused(409, &json!({ "error": "version" })));
                }
                self.record_puts.set(self.record_puts.get() + 1);
                // As `DevicePosition::sign_envelope`: an unsigned envelope naming this device's
                // key is signed by it.
                let mut sealed: record_core::SealedRecord =
                    serde_json::from_value(p["record"].clone()).map_err(|e| RpcError {
                        code: 400,
                        message: e.to_string(),
                    })?;
                if sealed.sig.is_empty() {
                    let (_, key_id, chain, _) = self.position().await?;
                    if sealed.signer != key_id {
                        return Err(RpcError {
                            code: 400,
                            message: format!("signer: not this device's key {key_id}"),
                        });
                    }
                    record_core::sign_envelope(&chain, &mut sealed, &self.device).unwrap();
                }
                let record = serde_json::to_value(&sealed).unwrap();
                if let Some(request) = given_request(&p) {
                    let scid = record_scid(&request, "PUT")?;
                    let (code, body) = self
                        .store
                        .send(
                            "PUT",
                            &format!("/v1/private/{scid}"),
                            Some(&record),
                            Some(encode_header(&request)),
                        )
                        .await;
                    if code >= 400 {
                        return Err(Self::refused(code, &body));
                    }
                    return Ok(body);
                }
                let (scid, ..) = self.position().await?;
                let (_, body) = self
                    .as_device("PUT", &format!("/v1/private/{scid}"), Some(&record))
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
            "identity.keyOwner" => {
                let key = p["key"].as_str().unwrap();
                let (code, body) = self
                    .store
                    .send("GET", &format!("/v1/keys/{key}"), None, None)
                    .await;
                match code {
                    200 => Ok(json!({
                        "did": body["did"], "keyId": body["keyId"],
                        "added": body["added"], "revoked": body["revoked"],
                    })),
                    404 => Ok(Value::Null),
                    _ => Err(Self::refused(code, &body)),
                }
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
        CreateScreen::ShowWords(words) => words.expose().clone(),
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
        .finish(executor, vault, this_computer(), &SystemClock, &OsRandom)
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
    let created = flow
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await
        .unwrap();

    assert_eq!(created.device_key_id, "d1");
    assert_eq!(created.head.seq, 0);
    assert!(vault.pending_incepts().unwrap().is_empty());
    assert_eq!(a.rpc().publishes.get(), 1);
    let status = a.status().await.unwrap();
    assert_eq!(status.did.as_deref(), Some(created.did.as_str()));
    assert_eq!(status.key_id.as_deref(), Some("d1"));
    // The record's first version lists this computer as holding master #0.
    assert_eq!(created.record.as_ref().unwrap().as_ref().unwrap(), &1);
    let opened = record::read(&a, &vault, &created.did)
        .await
        .unwrap()
        .unwrap();
    let d1 = &opened.record.devices["d1"];
    assert_eq!(
        (d1.name.as_str(), d1.holds_master, d1.master_index),
        ("This computer", HoldsMaster::Yes, Some(0))
    );
    // The typed status carries the executor's store, as `identity.status` answers it.
    assert_eq!(
        status.store,
        Some(ad4m_identity_client::executor::StoreInfo {
            url: STORE_URL.into(),
            trusted: true,
        })
    );
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
        again.finish(&a, &vault, this_computer(), &SystemClock, &OsRandom).await,
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

    fn master_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.master_index(scid)
    }

    fn forget_master(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.forget_master(scid)
    }

    fn stage_master(
        &self,
        _scid: &str,
        _index: u32,
        _secret: ad4m_identity_client::Secret<[u8; 32]>,
        _owner: &StageToken,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        Err(ad4m_identity_client::VaultError::Cancelled)
    }

    fn staged_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.staged_index(scid)
    }

    fn promote_master(
        &self,
        scid: &str,
        index: u32,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.promote_master(scid, index)
    }

    fn discard_staged(
        &self,
        scid: &str,
        index: u32,
        owner: Option<&StageToken>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.discard_staged(scid, index, owner)
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

    fn record_seen(
        &self,
        scid: &str,
        version: u64,
    ) -> Result<Option<u64>, ad4m_identity_client::VaultError> {
        self.0.record_seen(scid, version)
    }

    fn record_seen_reset(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.record_seen_reset(scid)
    }
}

#[tokio::test]
async fn create_publishes_nothing_when_the_vault_cannot_keep_the_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));

    let refused = confirmed(6)
        .finish(
            &a,
            &RefusingVault(MemoryVault::new()),
            this_computer(),
            &SystemClock,
            &OsRandom,
        )
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
    let _ = confirmed(6)
        .finish(&a, &refusing, this_computer(), &SystemClock, &OsRandom)
        .await;
    assert!(refusing.pending_incepts().unwrap().is_empty());

    // Nothing public happened, so the same words make the identity once the vault works.
    let vault = MemoryVault::new();
    let created = confirmed(6)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await
        .unwrap();
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
    let lost = confirmed(8)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await;
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
    let created = confirmed(8)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await
        .unwrap();
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
    assert!(confirmed(10)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await
        .is_err());
    let did = a
        .status()
        .await
        .unwrap()
        .did
        .expect("the publish committed");

    // Other words are refused while the kept incept is not finished: no second identity.
    let other = confirmed(11)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await;
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
        confirmed(11).finish(&a, &vault, this_computer(), &SystemClock, &OsRandom).await,
        Err(ClientError::HasIdentity(d)) if d == did
    ));
}

#[tokio::test]
async fn resume_publishes_an_incept_whose_publish_never_reached_the_store() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();

    a.rpc().refuse_publish.set(true);
    let failed = confirmed(12)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await;
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
        .starts_with("https://id.coasys.org/o#coasys:offer:1:"));
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
    assert_eq!(linked.record.unwrap(), 2);

    let joined = join(&b, &claimed_b["offerId"]).await;
    assert_eq!(
        (&joined["state"], &joined["keyId"], &joined["seq"]),
        (&json!("spent"), &json!("d2"), &json!(1))
    );
    // The record lists the new device, and both executors read it.
    for reader in [&a, &b] {
        let opened = record::read(reader, &vault, &did).await.unwrap().unwrap();
        assert_eq!(opened.version, 2);
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
    // The genuine device's claim passes the holder's bound check (`verify_claim_view`).
    let claimed = flow.wait_for_claim().await.unwrap();
    assert_eq!(claimed.device_key, device_key);
    assert_eq!(claimed.match_code, claimed_b["matchCode"]);
    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), false)
        .await
        .unwrap();
    assert_eq!(linked.head.seq, 1);
}

/// A confirm whose publish fails keeps the flow: confirming again, while the claim is still
/// `claimed`, spends it under the same unlock.
#[tokio::test]
async fn a_confirm_after_a_failed_publish_succeeds() {
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
    let claimed_b = claim(&b, &flow.link(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();

    a.rpc().refuse_publish.set(true);
    flow.confirm(&claimed, entry("node.example", DeviceKind::Server), false)
        .await
        .expect_err("the publish failed");
    assert_eq!(join(&b, &claimed_b["offerId"]).await["state"], "claimed");
    // The record is still the one `create` wrote.
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        1
    );

    let linked = flow
        .confirm(&claimed, entry("node.example", DeviceKind::Server), false)
        .await
        .unwrap();
    assert_eq!(linked.head.seq, 1);
    assert_eq!(linked.record.unwrap(), 2);
    assert_eq!(join(&b, &claimed_b["offerId"]).await["state"], "spent");
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
}

/// A confirm with "keep the master key" whose publish failed pushed nothing and wrote no
/// record; its retry pushes the key once and writes the record once; a confirm after the add
/// is in is refused before a second push or record write.
#[tokio::test]
async fn a_retried_confirm_pushes_the_master_key_and_writes_the_record_once() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 14).await;
    let device = device_string(&desk, STORE_URL, SystemClock.now() + 300);
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
    let claimed = flow.wait_for_claim().await.unwrap();

    a.rpc().refuse_publish.set(true);
    flow.confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .expect_err("the publish failed");
    assert!(vault.pending_pushes().unwrap().is_empty());
    // The record is still the one `create` wrote.
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        1
    );

    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert!(matches!(&linked.master, Some(Ok(_))), "{:?}", linked.master);
    // Version 2 lists the device, version 3 marks it offered after the push.
    assert_eq!(linked.record.unwrap(), 3);

    flow.confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .expect_err("the offer is spent");
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        3
    );
    assert!(vault.pending_pushes().unwrap().is_empty());
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);

    // One master key waits in the desktop's inbox, not two.
    join(&desk, &claimed_desk["offerId"]).await;
    let desk_vault = MemoryVault::new();
    take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");
    assert!(take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .is_none());
}

/// The store took the `add` but its answer was lost: the confirm failed with the offer
/// already `spent`. Confirming again finds this claim's add in the log and, without a second
/// publish, pushes the master key once and writes the record once, under the flow's one
/// unlock. A confirm after that is refused before a second push or record write.
#[tokio::test]
async fn a_confirm_whose_publish_answer_was_lost_pushes_and_writes_on_its_retry() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 14).await;
    let device = device_string(&desk, STORE_URL, SystemClock.now() + 300);
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
    let claimed = flow.wait_for_claim().await.unwrap();

    a.rpc().lose_publish_answer.set(true);
    let publishes = a.rpc().publishes.get();
    flow.confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .expect_err("the publish answer was lost");
    assert_eq!(a.rpc().publishes.get(), publishes + 1);
    assert_eq!(
        join(&desk, &claimed_desk["offerId"]).await["state"],
        "spent"
    );
    assert!(vault.pending_pushes().unwrap().is_empty());
    // The record is still the one `create` wrote.
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        1
    );

    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert_eq!(a.rpc().publishes.get(), publishes + 1, "no second publish");
    assert_eq!((linked.head.seq, linked.key_id.as_str()), (1, "d2"));
    assert!(matches!(&linked.master, Some(Ok(_))), "{:?}", linked.master);
    // Version 2 lists the device, version 3 marks it offered after the push.
    assert_eq!(linked.record.unwrap(), 3);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.version, 3);
    assert_eq!(opened.record.devices["d2"].name, "Desk");
    assert_eq!(
        opened.record.devices["d2"].holds_master,
        HoldsMaster::Offered
    );
    assert!(vault.pending_pushes().unwrap().is_empty());

    // After the full success a confirm is refused: no second push, the record unchanged.
    flow.confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .expect_err("the offer is spent");
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        3
    );
    assert!(vault.pending_pushes().unwrap().is_empty());
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);

    // One master key waits in the desktop's inbox, not two.
    join(&desk, &claimed_desk["offerId"]).await;
    let desk_vault = MemoryVault::new();
    take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");
    assert!(take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .is_none());
}

/// A spent offer whose add in the log is not this claim's is still refused: a claim of
/// another key on the spent offer, and the added key's claim on another offer. The claim the
/// add does spend still finishes its confirm.
#[tokio::test]
async fn a_confirm_of_a_spent_offer_whose_add_is_another_claim_is_refused() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let c = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 8).await;
    let open = || {
        LinkFlow::open(
            &a,
            &vault,
            &did,
            OfferKind::Node,
            None,
            STORE_URL,
            &SystemClock,
            &OsRandom,
        )
    };
    let flow_b = open().await.unwrap();
    let flow_c = open().await.unwrap();
    claim(&b, &flow_b.link(), None).await.unwrap();
    claim(&c, &flow_c.link(), None).await.unwrap();
    let claimed_b = flow_b.wait_for_claim().await.unwrap();
    let claimed_c = flow_c.wait_for_claim().await.unwrap();

    a.rpc().lose_publish_answer.set(true);
    flow_b
        .confirm(&claimed_b, entry("b.example", DeviceKind::Server), false)
        .await
        .expect_err("the publish answer was lost");

    // The spent offer's add is b's key: c's claim is refused on it.
    flow_b
        .confirm(&claimed_c, entry("c.example", DeviceKind::Server), false)
        .await
        .expect_err("the offer is spent by another key");
    // b's key is in the log, but by the other offer: refused on this one.
    flow_c
        .confirm(&claimed_b, entry("b.example", DeviceKind::Server), false)
        .await
        .expect_err("the add spends another offer");
    // The record is still the one `create` wrote.
    assert_eq!(
        record::read(&a, &vault, &did)
            .await
            .unwrap()
            .unwrap()
            .version,
        1
    );

    let linked = flow_b
        .confirm(&claimed_b, entry("b.example", DeviceKind::Server), false)
        .await
        .unwrap();
    assert_eq!((linked.head.seq, linked.key_id.as_str()), (1, "d2"));
    assert_eq!(linked.record.unwrap(), 2);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    // The creator (listed by `create`) and b.
    assert_eq!(opened.record.devices.len(), 2);
    assert_eq!(opened.record.devices["d2"].name, "b.example");
}

/// The system clock moved ahead by a number of seconds: the holder's time runs on while the
/// store's stays real (the store would still take the `add`).
struct Ahead(Cell<u64>);

impl Clock for Ahead {
    fn now(&self) -> u64 {
        SystemClock.now() + self.0.get()
    }
}

/// The store takes the `add` until the offer's expiry plus `CONFIRM_GRACE_S`: a holder who
/// confirms after the offer's expiry, within the grace, still signs it with the flow's one
/// unlock.
#[tokio::test]
async fn confirm_after_the_offers_expiry_within_the_grace_signs_the_add() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 13).await;
    let clock = Ahead(Cell::new(0));
    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        None,
        STORE_URL,
        &clock,
        &OsRandom,
    )
    .await
    .unwrap();
    claim(&b, &flow.link(), None).await.unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();

    clock.0.set(link::OFFER_TTL_S + link::CONFIRM_GRACE_S - 60);
    let linked = flow
        .confirm(&claimed, entry("node.example", DeviceKind::Server), false)
        .await
        .unwrap();
    assert_eq!(linked.head.seq, 1);
    assert_eq!(linked.record.unwrap(), 2);
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
}

/// Past the offer's expiry `wait_for_claim` answers that the offer expired, even when the
/// unlock has ended too: the offer's end is what the surface shows, not the vault's.
#[tokio::test]
async fn waiting_past_the_offers_expiry_answers_expired_not_the_vault() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 14).await;
    let clock = Ahead(Cell::new(0));
    let flow = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        None,
        STORE_URL,
        &clock,
        &OsRandom,
    )
    .await
    .unwrap();
    clock.0.set(link::OFFER_TTL_S + link::CONFIRM_GRACE_S + 100);
    let ended = flow.wait_for_claim().await;
    assert!(
        matches!(&ended, Err(ClientError::OfferEnded(why)) if why == "expired"),
        "{ended:?}"
    );
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
            &vault,
            &did,
            &[Change::PendingRevocation(pending.clone())],
            &OsRandom,
        )
        .await
        .unwrap();
    }
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
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
    assert_eq!(revoked.record.unwrap(), 5);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    // Only the creator (listed by `create`) is left.
    assert_eq!(opened.record.devices.keys().collect::<Vec<_>>(), ["d1"]);
    assert!(opened.record.pending_revocations.is_empty());
    let sealed = a.record().await.unwrap().unwrap();
    assert_eq!(
        sealed.record.wrapped.keys().collect::<Vec<_>>(),
        ["d1", "recovery"]
    );
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

/// A claim view the store changed after the device signed it: the holder refuses it in
/// `wait_for_claim`, so the master signs nothing and the log stays at the incept.
async fn a_swapped_claim_view_is_refused(entropy: u8, tamper: fn(&mut Value)) {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, entropy).await;
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
    claim(&b, &flow.link(), None).await.unwrap();
    let publishes = a.rpc().publishes.get();
    a.rpc().tamper_claim.set(Some(tamper));

    let refused = flow.wait_for_claim().await;
    assert!(
        matches!(refused, Err(ClientError::Link(link::LinkError::CoSig))),
        "{refused:?}"
    );
    assert_eq!(a.rpc().publishes.get(), publishes);
    let log = a.log(&did, 0, true).await.unwrap();
    assert_eq!(log.events.len(), 1);
}

#[tokio::test]
async fn a_claim_view_with_a_swapped_enc_key_is_never_signed() {
    a_swapped_claim_view_is_refused(8, |claim| {
        let other = SigningKey::from_bytes(&[9; 32])
            .verifying_key()
            .to_montgomery();
        claim["encKey"] = json!(ad4m_identity_core::encode_x25519_multikey(other.as_bytes()));
    })
    .await;
}

#[tokio::test]
async fn a_claim_view_with_a_swapped_can_keep_master_is_never_signed() {
    a_swapped_claim_view_is_refused(9, |claim| {
        claim["canKeepMaster"] = json!(true);
    })
    .await;
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
        master_index: None,
        operator: None,
    }
}

/// The creating computer's entry, as a surface names it at `create`.
fn this_computer() -> DeviceEntry {
    entry("This computer", DeviceKind::DesktopApp)
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

/// `a` links `b` with an unbound `device` offer and `entry`, without the master key; `b`'s
/// claim says `can_keep_master`. Answers the new key's id.
async fn link_app(
    a: &ViaExecutor<TestExecutor>,
    vault: &MemoryVault,
    did: &str,
    b: &ViaExecutor<TestExecutor>,
    entry: DeviceEntry,
    can_keep_master: bool,
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
    let claimed_b = b
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": can_keep_master }),
        )
        .await
        .unwrap();
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
    // Version 2 lists the desktop before the key goes, version 3 marks it offered.
    assert_eq!(linked.record.unwrap(), 3);
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert!(vault.pending_pushes().unwrap().is_empty());
    join(&desk, &claimed_desk["offerId"]).await;
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
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
    assert_eq!(taken.record.unwrap(), 4);
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
        let opened = record::read(reader, &vault, &did).await.unwrap().unwrap();
        assert_eq!(opened.version, 4);
        assert_eq!(opened.record.devices["d2"].holds_master, HoldsMaster::Yes);
        assert_eq!(opened.record.devices["d2"].name, "Desk");
    }
}

thread_local! {
    /// The claim view `swap_whole_claim` puts in place of the one the store holds.
    static SWAPPED_VIEW: RefCell<Option<Value>> = const { RefCell::new(None) };
}

/// A store that answers another device's whole claim: its own key, `encKey` and a valid
/// co-signature of its own.
fn swap_whole_claim(claim: &mut Value) {
    SWAPPED_VIEW.with(|v| *claim = v.borrow().clone().expect("a swapped view is set"));
}

/// The holder scanned the desktop's string, so the offer is bound to the desktop's key. A
/// store that answers another device's claim, consistent and co-signed by that device, is
/// refused as `Bound` in `wait_for_claim`: no unlock beyond the scan's, nothing signed, the
/// log stays at the incept.
#[tokio::test]
async fn a_scanned_offer_refuses_a_claim_by_another_whole_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let other = TestExecutor::new(&store, 3);
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 18).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let device = device_string(&desk, STORE_URL, SystemClock.now() + 300);
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
    desk.rpc().take_ticket(&device.r).await.unwrap();

    // Another device's claim on this offer id, signed by that device: valid on its own.
    let swapped = link::make_claim(
        &scid,
        flow.offer_id(),
        [5; link::SECRET_LEN],
        &other.device,
        other.device_enc(),
        true,
        Some("Desk".into()),
    )
    .unwrap();
    link::verify_claim(&scid, &swapped).unwrap();
    SWAPPED_VIEW
        .with(|v| *v.borrow_mut() = Some(json!(link::claim_view(&swapped, SystemClock.now()))));
    let publishes = a.rpc().publishes.get();
    a.rpc().tamper_claim.set(Some(swap_whole_claim));

    let refused = flow.wait_for_claim().await;
    assert!(
        matches!(refused, Err(ClientError::Link(link::LinkError::Bound))),
        "{refused:?}"
    );
    assert_eq!(a.rpc().publishes.get(), publishes);
    assert_eq!(a.log(&did, 0, true).await.unwrap().events.len(), 1);
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
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
        true,
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
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
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
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
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
        true,
    )
    .await;

    record::write(
        &desk,
        &vault,
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
        &vault,
        &did,
        &[Change::HoldsMaster {
            key_id: d2.clone(),
            holds: HoldsMaster::Offered,
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(version, 4);
    assert_eq!(a.rpc().record_puts.get(), puts + 1);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].name, "Studio Mac");
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );

    let busy = ViaExecutor::new(AlwaysStale(a.rpc()));
    let refused = record::write(&busy, &vault, &did, &[], &OsRandom).await;
    assert!(
        matches!(refused, Err(ClientError::RecordBusy)),
        "{refused:?}"
    );
}

/// `write_if` checks the version it builds on, and again on the version it reads after a
/// stale one: a check that fails there writes nothing and answers the check's error.
#[tokio::test]
async fn a_conditional_write_checks_every_version_it_builds_on() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 19).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        true,
    )
    .await;
    let named = |name: &'static str| {
        let d2 = d2.clone();
        move |r: Option<&record_core::PrivateRecord>, _: &KelChain| match r {
            Some(r) if r.devices[&d2].name == name => Ok(()),
            _ => Err(ClientError::Step("renamed under the write")),
        }
    };
    let rename = |name: &str| Change::Rename {
        key_id: d2.clone(),
        name: name.into(),
    };
    let desk_named = named("Desk");
    assert_eq!(
        record::write_if(
            &a,
            &vault,
            &did,
            &[rename("Studio")],
            &desk_named,
            &OsRandom
        )
        .await
        .unwrap(),
        3
    );
    // The check failed on the version read: nothing is written.
    let puts = a.rpc().record_puts.get();
    let refused =
        record::write_if(&a, &vault, &did, &[rename("Den")], &desk_named, &OsRandom).await;
    assert!(matches!(refused, Err(ClientError::Step(_))), "{refused:?}");
    assert_eq!(a.rpc().record_puts.get(), puts);

    // It held on the version first read, the put met a newer one, and it fails there.
    struct RenamedFirst<'a> {
        inner: &'a TestExecutor,
        desk: &'a ViaExecutor<TestExecutor>,
        vault: &'a MemoryVault,
        did: &'a str,
        d2: &'a str,
        done: Cell<bool>,
    }
    #[async_trait(?Send)]
    impl ExecutorRpc for RenamedFirst<'_> {
        async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
            if method == "identity.recordPut" && !self.done.replace(true) {
                let rename = Change::Rename {
                    key_id: self.d2.into(),
                    name: "Den".into(),
                };
                record::write(self.desk, self.vault, self.did, &[rename], &OsRandom)
                    .await
                    .unwrap();
            }
            self.inner.call(method, params).await
        }
    }
    let raced = ViaExecutor::new(RenamedFirst {
        inner: a.rpc(),
        desk: &desk,
        vault: &vault,
        did: &did,
        d2: &d2,
        done: Cell::new(false),
    });
    let studio_named = named("Studio");
    let refused = record::write_if(
        &raced,
        &vault,
        &did,
        &[rename("Loft")],
        &studio_named,
        &OsRandom,
    )
    .await;
    assert!(matches!(refused, Err(ClientError::Step(_))), "{refused:?}");
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        (opened.version, opened.record.devices[&d2].name.as_str()),
        (4, "Den")
    );
}

/// The holder's executor, whose first record write after a master-key push the store takes
/// only once `released` is set: the device took the key and wrote `holdsMaster: true` in
/// between (the push and its `"offered"` race the device's take). `pushed` is set when the
/// store took the push.
struct HeldAfterPush<'a> {
    inner: &'a TestExecutor,
    pushed: &'a Cell<bool>,
    released: &'a Cell<bool>,
    held: Cell<bool>,
}

#[async_trait(?Send)]
impl ExecutorRpc for HeldAfterPush<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.recordPut" && self.pushed.get() && !self.held.replace(true) {
            while !self.released.get() {
                tokio::task::yield_now().await;
            }
        }
        let answer = self.inner.call(method, params).await;
        if method == "identity.inboxPut" && answer.is_ok() {
            self.pushed.set(true);
        }
        answer
    }
}

/// The device's side of the race: once the push landed, it takes the master key (its record
/// write `holdsMaster: true` lands first), then lets the holder's write go.
async fn take_when_pushed(
    device: &ViaExecutor<TestExecutor>,
    pushed: &Cell<bool>,
    released: &Cell<bool>,
    offer_id: Option<&Value>,
) -> u64 {
    while !pushed.get() {
        tokio::task::yield_now().await;
    }
    if let Some(offer_id) = offer_id {
        join(device, offer_id).await;
    }
    let taken = take_master(device, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");
    released.set(true);
    taken.record.unwrap()
}

/// A device that took the master key before the holder's `"offered"` landed keeps
/// `holdsMaster: true`: the late `"offered"` meets the newer version, reads it again, and
/// writes nothing over the take.
#[tokio::test]
async fn a_late_offered_leaves_a_device_that_took_the_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 23).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        true,
    )
    .await;
    let (pushed, released) = (Cell::new(false), Cell::new(false));
    let holder = ViaExecutor::new(HeldAfterPush {
        inner: a.rpc(),
        pushed: &pushed,
        released: &released,
        held: Cell::new(false),
    });
    let (sent, taken) = tokio::join!(
        send_master(&holder, &vault, &did, &d2, &SystemClock, &OsRandom),
        take_when_pushed(&desk, &pushed, &released, None),
    );
    let sent = sent.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);
    assert_eq!(opened.version, taken);
    assert!(
        matches!(&sent.record, Err(ClientError::HoldsMaster(k)) if *k == d2),
        "{:?}",
        sent.record
    );
}

/// On a first link the device's entry is in the record before the master key goes, so a
/// device that takes the key at once finds its entry and ends `holdsMaster: true`, and the
/// holder's `"offered"` after the push writes nothing over it.
#[tokio::test]
async fn a_device_that_takes_the_master_key_at_once_at_its_link_ends_holding_it() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 24).await;
    let (pushed, released) = (Cell::new(false), Cell::new(false));
    let holder = ViaExecutor::new(HeldAfterPush {
        inner: a.rpc(),
        pushed: &pushed,
        released: &released,
        held: Cell::new(false),
    });
    let flow = LinkFlow::open(
        &holder,
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
    let claimed_desk = desk
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": true }),
        )
        .await
        .unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    let (linked, taken) = tokio::join!(
        flow.confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true),
        take_when_pushed(&desk, &pushed, &released, Some(&claimed_desk["offerId"])),
    );
    let linked = linked.unwrap();
    assert!(matches!(&linked.master, Some(Ok(_))), "{:?}", linked.master);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let listed = &opened.record.devices[&linked.key_id];
    assert_eq!(
        (listed.name.as_str(), listed.holds_master),
        ("Desk", HoldsMaster::Yes)
    );
    assert_eq!(opened.version, taken);
    assert!(
        matches!(&linked.record, Err(ClientError::HoldsMaster(k)) if *k == linked.key_id),
        "{:?}",
        linked.record
    );
}

/// The retry list pushes again to a device that took the key since and to one that did not:
/// one record write marks the second offered and leaves the first `true`.
#[tokio::test]
async fn a_retry_marks_offered_only_the_devices_that_do_not_hold_the_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let laptop = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 25).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        true,
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &laptop,
        entry("Laptop", DeviceKind::DesktopApp),
        true,
    )
    .await;
    send_master(&a, &vault, &did, &d2, &SystemClock, &OsRandom)
        .await
        .unwrap();
    take_master(&desk, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");

    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    for key_id in [&d2, &d3] {
        vault
            .keep_push(&PendingPush {
                scid: scid.clone(),
                key_id: key_id.clone(),
            })
            .unwrap();
    }
    let before = record::read(&a, &vault, &did)
        .await
        .unwrap()
        .unwrap()
        .version;
    let retried = retry_pushes(&a, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(retried.pushes.len(), 2);
    assert!(retried.pushes.iter().all(|(_, sent)| sent.is_ok()));
    assert_eq!(retried.record.unwrap().unwrap(), before + 1);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);
    assert_eq!(
        opened.record.devices[&d3].holds_master,
        HoldsMaster::Offered
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
        true,
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
    assert_eq!(sent.record.unwrap(), 3);
    assert!(a.requests(0).await.unwrap().is_empty());
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
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

/// The master key goes only to a device the key log lets keep it: not to a server whose claim
/// said no, neither on its request nor by the person's choice, nor to a key the log does not
/// list; and the vault is never unlocked for it.
#[tokio::test]
async fn a_server_that_asks_for_the_master_key_gets_none() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let server = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 18).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &server,
        entry("node", DeviceKind::Server),
        false,
    )
    .await;
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
    // Nor to a device key the log does not list at all.
    assert!(matches!(
        send_master(&a, &vault, &did, "d9", &SystemClock, &OsRandom).await,
        Err(ClientError::NotAKeeper(_))
    ));
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert!(vault.pending_pushes().unwrap().is_empty());
}

/// A device whose claim did not say `canKeepMaster` writes itself into the record as a desktop
/// app (any linked device can write the record) and asks for the master key: the holder
/// refuses, before any unlock, because the log decides and the record's kind grants nothing.
#[tokio::test]
async fn a_device_the_record_calls_an_app_but_the_log_does_not_let_keep_gets_no_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let device = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 21).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &device,
        entry("node", DeviceKind::Server),
        false,
    )
    .await;
    // The device rewrites its own entry: now the record says it is a desktop app.
    record::write(
        &device,
        &vault,
        &did,
        &[Change::Device {
            key_id: d2.clone(),
            entry: entry("Desk", DeviceKind::DesktopApp),
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].kind, DeviceKind::DesktopApp);

    device
        .post_request(&DeviceRequestBody::SendMaster {}, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    let refused = answer(&a, &vault, &did, &listed[0], &SystemClock, &OsRandom).await;
    assert!(
        matches!(&refused, Err(ClientError::NotAKeeper(k)) if *k == d2),
        "{refused:?}"
    );
    let refused = send_master(&a, &vault, &did, &d2, &SystemClock, &OsRandom).await;
    assert!(
        matches!(&refused, Err(ClientError::NotAKeeper(k)) if *k == d2),
        "{refused:?}"
    );
    assert_eq!(vault.unlocks(), vec![MasterUse::Link]);
    assert!(vault.pending_pushes().unwrap().is_empty());
    assert!(take_master(&device, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_none());
}

/// A device whose claim said `canKeepMaster`, listed in the record as a server, asks for the
/// master key: the log lets it keep it, so the holder sends it; the record's kind refuses
/// nothing either.
#[tokio::test]
async fn a_device_the_log_lets_keep_gets_the_master_key_whatever_the_record_says() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let device = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 22).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &device,
        entry("node", DeviceKind::Server),
        true,
    )
    .await;
    device
        .post_request(&DeviceRequestBody::SendMaster {}, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    let sent = answer(&a, &vault, &did, &listed[0], &SystemClock, &OsRandom).await;
    assert!(matches!(&sent, Ok(Answered::MasterSent(_))), "{sent:?}");
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].kind, DeviceKind::Server);
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );
    assert!(take_master(&device, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap()
        .is_some());
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
        true,
    )
    .await;
    desk.post_request(
        &DeviceRequestBody::SendMaster {},
        Some(SystemClock.now() + 60),
    )
    .await
    .unwrap();
    let listed = a.requests(0).await.unwrap();
    let refused = answer(
        &a,
        &vault,
        &did,
        &listed[0],
        &Ahead(Cell::new(61)),
        &OsRandom,
    )
    .await;
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
        true,
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
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
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert!(!opened.record.devices.contains_key(&d3));
    let sealed = a.record().await.unwrap().unwrap();
    assert_eq!(
        sealed.record.wrapped.keys().collect::<Vec<_>>(),
        ["d1", "d2", "recovery"]
    );
    assert_eq!(
        vault.unlocks(),
        vec![MasterUse::Link, MasterUse::Link, MasterUse::Revoke]
    );
}

/// An executor that answers `cosignIncept` with another valid incept (its own master, the
/// same device key) is refused: the vault keeps nothing and nothing is published.
#[tokio::test]
async fn a_swapped_co_signed_incept_is_never_kept_or_published() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    a.rpc().swap_incept.set(true);
    let vault = MemoryVault::new();
    let refused = confirmed(15)
        .finish(&a, &vault, this_computer(), &SystemClock, &OsRandom)
        .await;
    assert!(
        matches!(&refused, Err(ClientError::Protocol(_))),
        "{refused:?}"
    );
    assert_eq!(a.rpc().publishes.get(), 0);
    assert!(vault.pending_incepts().unwrap().is_empty());
    assert_eq!(a.status().await.unwrap().did, None);
}

/// An `openOffer` answer for another offer id is refused: the flow never hands out a link
/// whose offer the store did not take.
#[tokio::test]
async fn an_open_offer_answer_for_another_offer_is_refused() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 16).await;
    a.rpc().other_offer_id.set(true);
    let refused = LinkFlow::open(
        &a,
        &vault,
        &did,
        OfferKind::Node,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await;
    assert!(
        matches!(&refused, Err(ClientError::Protocol(m)) if m.contains("another offer id")),
        "{:?}",
        refused.err()
    );
}

/// A vault whose key for the identity is not the log's current master opens no offer: the
/// flow checks before it signs.
#[tokio::test]
async fn a_vault_key_that_is_not_the_current_master_opens_no_offer() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 17).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let wrong = MemoryVault::new();
    wrong
        .store_master(&scid, 0, ad4m_identity_client::Secret::new([42; 32]))
        .unwrap();
    let refused = LinkFlow::open(
        &a,
        &wrong,
        &did,
        OfferKind::Node,
        None,
        STORE_URL,
        &SystemClock,
        &OsRandom,
    )
    .await;
    assert!(
        matches!(&refused, Err(ClientError::Protocol(m)) if m.contains("current master")),
        "{:?}",
        refused.err()
    );
}

/// A screen's `Debug` never shows the words: a surface that logs a screen, or a test that
/// panics with one, must not put the 12 words in a log or a crash report.
#[test]
fn a_create_screen_never_shows_the_words_in_debug_output() {
    let (_flow, screen) = CreateFlow::start(&OsRandom);
    assert!(matches!(&screen, CreateScreen::ShowWords(words) if words.expose().len() == 12));
    assert_eq!(format!("{screen:?}"), "ShowWords(<12 words>)");
}

/// Makes `b` an app that holds the master key of `did`: linked, it asks with `send-master`,
/// `a` answers, and `b`'s vault takes the key (the record marks it holding). Answers `b`'s key
/// id.
async fn holding_app(
    a: &ViaExecutor<TestExecutor>,
    vault: &MemoryVault,
    did: &str,
    b: &ViaExecutor<TestExecutor>,
    b_vault: &MemoryVault,
    entry: DeviceEntry,
) -> String {
    let key_id = link_app(a, vault, did, b, entry, true).await;
    let asked = ask_for_master(b, None).await.unwrap();
    let listed = a.requests(0).await.unwrap();
    let request = listed.iter().find(|r| r.id == asked).unwrap();
    answer(a, vault, did, request, &SystemClock, &OsRandom)
        .await
        .unwrap();
    let taken = take_master(b, b_vault, &OsRandom).await.unwrap().unwrap();
    taken.record.unwrap();
    key_id
}

/// Checks that `seed` derives every master and commitment of `events`: the incept lists master
/// #0 and commits to master #1; the rotation to master *n* reveals the words' master *n* with
/// its salt and commits to master *n*+1. Answers the last master's index.
fn assert_words_derive(events: &[Event], seed: &words::Seed) -> u32 {
    let mut n = 0u32;
    for event in events {
        match &event.op {
            Op::Incept { master, next, .. } => {
                assert_eq!(master, &Signer::key(&words::master_key(seed, 0).unwrap()));
                assert_eq!(next, &words::next_commitment(seed, 1).unwrap().0);
            }
            Op::Rotate {
                master, salt, next, ..
            } => {
                n += 1;
                let (_, expected_salt) = words::next_commitment(seed, n).unwrap();
                assert_eq!(master, &Signer::key(&words::master_key(seed, n).unwrap()));
                assert_eq!(salt, &expected_salt);
                assert_eq!(
                    next.as_ref(),
                    Some(&words::next_commitment(seed, n + 1).unwrap().0)
                );
                assert_eq!(
                    event.proof.len(),
                    1,
                    "a rotation is signed by its new master alone"
                );
            }
            _ => {}
        }
    }
    n
}

/// A rotation with the words reveals master #1 (signed by it alone), the vault keeps it in
/// place of master #0, the app that kept the master key gets it in its inbox, and the record
/// keeps the recovery key. The app's vault, still on master #0, takes master #1 from its inbox.
#[tokio::test]
async fn a_rotation_reveals_the_next_master_and_hands_it_to_the_kept_holder() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 23).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[23; 16]);
    let seed = words::seed(&words);

    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!((rotated.index, rotated.tries), (1, 1));
    assert!(rotated.revoked.is_empty());
    assert_eq!(rotated.pushes.len(), 1);
    assert_eq!(rotated.pushes[0].0, d2);
    assert!(rotated.pushes[0].1.is_ok(), "{:?}", rotated.pushes[0].1);
    rotated.record.as_ref().unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(
        assert_words_derive(&a.events(&did).await.unwrap(), &seed),
        1
    );
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );
    assert_eq!(
        opened.recovery.map(|r| r.key),
        Some(ad4m_identity_core::x25519_multikey_of(
            &words::recovery_secret(&seed)
        ))
    );

    assert_eq!(desk_vault.master_index(&scid).unwrap(), Some(0));
    let Refreshed::Taken(taken) = refresh_master(&desk, &desk_vault, &did, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected master #1 taken from the inbox");
    };
    assert_eq!(taken.index, 1);
    taken.record.unwrap();
    assert_eq!(desk_vault.master_index(&scid).unwrap(), Some(1));
    assert!(matches!(
        refresh_master(&desk, &desk_vault, &did, &OsRandom)
            .await
            .unwrap(),
        Refreshed::Current
    ));
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);
    // The words alone rotated: the vault was never unlocked for it.
    assert!(!vault.unlocks().contains(&MasterUse::Revoke));
}

/// An app whose push of the new master failed holds a replaced key: it forgets it and asks
/// for the current one with `send-master`, and the holder's answer gives it master #1.
#[tokio::test]
async fn a_stale_holder_gets_the_new_master_through_send_master() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let phone_vault = MemoryVault::new();
    let did = create(&a, &vault, 24).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &phone,
        &phone_vault,
        entry("Phone", DeviceKind::PhoneApp),
    )
    .await;
    let words = words::from_entropy(&[24; 16]);

    a.rpc().refuse_inbox_put.set(true);
    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert!(rotated.pushes[0].1.is_err());
    // A push that may pass later stays on the retry list, and the record says "offered".
    assert_eq!(vault.pending_pushes().unwrap().len(), 1);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );

    let Refreshed::Asked(id) = refresh_master(&phone, &phone_vault, &did, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected a send-master request");
    };
    assert_eq!(phone_vault.master_index(&scid).unwrap(), None);
    let listed = a.requests(0).await.unwrap();
    let request = listed.iter().find(|r| r.id == id).unwrap();
    assert_eq!(
        (&request.by, &request.body),
        (&d2, &DeviceRequestBody::SendMaster {})
    );
    let Answered::MasterSent(_) = answer(&a, &vault, &did, request, &SystemClock, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected the master key sent");
    };
    let taken = take_master(&phone, &phone_vault, &OsRandom)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(taken.index, 1);
    assert_eq!(phone_vault.master_index(&scid).unwrap(), Some(1));
}

/// A device removes its master key only while another app holds it, and the record says so
/// before the vault forgets it; the last holder is refused.
#[tokio::test]
async fn removing_the_master_key_is_refused_on_its_only_holder() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 28).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();

    // Before any app holds it, `a` is the only holder.
    assert!(matches!(
        remove_master(&a, &vault, &did, &OsRandom).await,
        Err(ClientError::OnlyCopy)
    ));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));

    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    remove_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), None);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);

    // Now the desk is the only holder.
    assert!(matches!(
        remove_master(&desk, &desk_vault, &did, &OsRandom).await,
        Err(ClientError::OnlyCopy)
    ));
    assert_eq!(desk_vault.master_index(&scid).unwrap(), Some(0));
}

/// An executor whose first `identity.recordPut` waits until every executor sharing `arrived`
/// reached its own: the writers have all read the same record version before any writes.
struct Gated<'a> {
    inner: &'a TestExecutor,
    arrived: &'a Cell<usize>,
    writers: usize,
    waited: Cell<bool>,
}

#[async_trait(?Send)]
impl ExecutorRpc for Gated<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.recordPut" && !self.waited.replace(true) {
            self.arrived.set(self.arrived.get() + 1);
            while self.arrived.get() < self.writers {
                tokio::task::yield_now().await;
            }
        }
        self.inner.call(method, params).await
    }
}

/// Two holders removing the master key at once: each saw the other in the record, but the
/// check runs again on the version each write builds on, so one removes it and the other is
/// refused with `OnlyCopy`; one copy remains.
#[tokio::test]
async fn two_holders_removing_the_master_key_at_once_leave_one_copy() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let phone_vault = MemoryVault::new();
    let did = create(&a, &vault, 30).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    holding_app(
        &a,
        &vault,
        &did,
        &phone,
        &phone_vault,
        entry("Phone", DeviceKind::PhoneApp),
    )
    .await;
    // The creator (d1, listed holding master #0) removes its copy first: the desk and the
    // phone are the two holders.
    remove_master(&a, &vault, &did, &OsRandom).await.unwrap();

    let arrived = Cell::new(0);
    let gated = |inner| {
        ViaExecutor::new(Gated {
            inner,
            arrived: &arrived,
            writers: 2,
            waited: Cell::new(false),
        })
    };
    let (desk_gated, phone_gated) = (gated(desk.rpc()), gated(phone.rpc()));
    let (by_desk, by_phone) = tokio::join!(
        remove_master(&desk_gated, &desk_vault, &did, &OsRandom),
        remove_master(&phone_gated, &phone_vault, &did, &OsRandom),
    );
    assert_eq!(arrived.get(), 2, "both read the record before either wrote");
    let refused = [&by_desk, &by_phone]
        .iter()
        .filter(|r| matches!(r, Err(ClientError::OnlyCopy)))
        .count();
    let removed = [&by_desk, &by_phone].iter().filter(|r| r.is_ok()).count();
    assert_eq!((removed, refused), (1, 1), "{by_desk:?} {by_phone:?}");
    let kept = [&desk_vault, &phone_vault]
        .iter()
        .filter(|v| v.master_index(&scid).unwrap() == Some(0))
        .count();
    assert_eq!(kept, 1);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let holders = opened
        .record
        .devices
        .values()
        .filter(|e| e.holds_master == HoldsMaster::Yes)
        .count();
    assert_eq!(holders, 1);
}

/// An executor that yields to the scheduler before each call, so two flows that run at once
/// interleave at every RPC boundary (as two devices on a network do).
struct Yielding<'a>(&'a TestExecutor);

#[async_trait(?Send)]
impl ExecutorRpc for Yielding<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        tokio::task::yield_now().await;
        self.0.call(method, params).await
    }
}

/// From the review of #1667: two holders that remove the master key while their flows
/// interleave at every call. The creator removes its copy first, so the desk and the phone are
/// the two holders; exactly one of them removes its copy and one copy is left.
#[tokio::test]
async fn two_holders_removing_at_once_leave_at_least_one_copy() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let phone_vault = MemoryVault::new();
    let did = create(&a, &vault, 41).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let d3 = holding_app(
        &a,
        &vault,
        &did,
        &phone,
        &phone_vault,
        entry("Phone", DeviceKind::PhoneApp),
    )
    .await;
    remove_master(&a, &vault, &did, &OsRandom).await.unwrap();
    let opened = record::read(&desk, &desk_vault, &did)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);
    assert_eq!(opened.record.devices[&d3].holds_master, HoldsMaster::Yes);
    assert!(opened
        .record
        .devices
        .get("d1")
        .is_none_or(|e| e.holds_master == HoldsMaster::No));

    let yd = ViaExecutor::new(Yielding(desk.rpc()));
    let yp = ViaExecutor::new(Yielding(phone.rpc()));
    let (first, second) = tokio::join!(
        remove_master(&yd, &desk_vault, &did, &OsRandom),
        remove_master(&yp, &phone_vault, &did, &OsRandom)
    );
    let removed = [&first, &second].iter().filter(|r| r.is_ok()).count();
    let refused = [&first, &second]
        .iter()
        .filter(|r| matches!(r, Err(ClientError::OnlyCopy)))
        .count();
    assert_eq!((removed, refused), (1, 1), "{first:?} {second:?}");
    let left = [
        desk_vault.master_index(&scid).unwrap(),
        phone_vault.master_index(&scid).unwrap(),
    ];
    assert_eq!(
        left.iter().filter(|k| **k == Some(0)).count(),
        1,
        "{left:?}"
    );
    let opened = record::read(&desk, &desk_vault, &did)
        .await
        .unwrap()
        .unwrap();
    let holders = [&d2, &d3]
        .iter()
        .filter(|d| opened.record.devices[**d].holds_master == HoldsMaster::Yes)
        .count();
    assert_eq!(holders, 1);
}

/// `holdsMaster: true` counts as another copy only for the current master: a device whose
/// entry names an older master index (its vault kept the key a rotation replaced) is not one.
#[tokio::test]
async fn a_holder_of_a_replaced_master_is_not_another_copy() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 31).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].master_index, Some(0));

    let words = words::from_entropy(&[31; 16]);
    rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    // The desk, still on master #0, says it holds the master key.
    record::write(
        &desk,
        &desk_vault,
        &did,
        &[Change::Holds {
            key_id: d2.clone(),
            index: 0,
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    assert!(matches!(
        remove_master(&a, &vault, &did, &OsRandom).await,
        Err(ClientError::OnlyCopy)
    ));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));

    // Once it takes master #1 it is one.
    let Refreshed::Taken(taken) = refresh_master(&desk, &desk_vault, &did, &OsRandom)
        .await
        .unwrap()
    else {
        panic!("expected master #1 taken from the inbox");
    };
    taken.record.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        (
            opened.record.devices[&d2].holds_master,
            opened.record.devices[&d2].master_index
        ),
        (HoldsMaster::Yes, Some(1))
    );
    remove_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), None);
}

/// The record's device kind grants nothing: a desktop app whose claim said it does not keep the
/// master key writes `holdsMaster: true` for itself in the record. It does not count as another
/// holder for `remove_master`, and a rotation does not send it the new master.
#[tokio::test]
async fn a_device_the_log_does_not_let_keep_the_master_key_is_no_holder() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 29).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        false,
    )
    .await;
    record::write(
        &desk,
        &vault,
        &did,
        &[Change::HoldsMaster {
            key_id: d2.clone(),
            holds: HoldsMaster::Yes,
        }],
        &OsRandom,
    )
    .await
    .unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        (
            opened.record.devices[&d2].kind,
            opened.record.devices[&d2].holds_master
        ),
        (DeviceKind::DesktopApp, HoldsMaster::Yes)
    );

    assert!(matches!(
        remove_master(&a, &vault, &did, &OsRandom).await,
        Err(ClientError::OnlyCopy)
    ));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));

    let words = words::from_entropy(&[29; 16]);
    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert!(rotated.pushes.is_empty(), "{:?}", rotated.pushes);
    assert!(vault.pending_pushes().unwrap().is_empty());
    rotated.record.unwrap();
    assert!(matches!(
        remove_master(&a, &vault, &did, &OsRandom).await,
        Err(ClientError::OnlyCopy)
    ));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
}

/// What [`Racing`] appends before the next publish reaches the store.
enum Race {
    /// Another app rotates with the same words.
    Rotate,
    /// Another app revokes this key with the current master.
    Revoke(String),
}

/// An executor where another app's event lands on the head just before each of the next
/// publishes, so they meet a `409`.
struct Racing<'a> {
    inner: &'a TestExecutor,
    seed: words::Seed,
    races: RefCell<Vec<Race>>,
}

#[async_trait(?Send)]
impl ExecutorRpc for Racing<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        let race = (method == "identity.publish" && !self.races.borrow().is_empty())
            .then(|| self.races.borrow_mut().remove(0));
        if let Some(race) = race {
            let scid = self.inner.identity.borrow().clone().unwrap();
            let chain = fold_page(&self.inner.log(&scid).await?);
            let n = (chain.state().masters.len() - 1) as u32;
            let event = match race {
                Race::Rotate => {
                    let (_, salt) = words::next_commitment(&self.seed, n + 1).unwrap();
                    let (next, _) = words::next_commitment(&self.seed, n + 2).unwrap();
                    let master = words::master_key(&self.seed, n + 1).unwrap();
                    build::rotate(&chain, &master, &salt, Some(next), &[]).unwrap()
                }
                Race::Revoke(id) => {
                    let master = words::master_key(&self.seed, n).unwrap();
                    build::revoke(&chain, &master, &[id]).unwrap()
                }
            };
            self.inner
                .call(
                    "identity.publish",
                    json!({ "events": [serde_json::to_value(&event).unwrap()] }),
                )
                .await?;
        }
        self.inner.call(method, params).await
    }
}

/// A rotation that meets a `409` is built again on the new head: it revokes only the keys
/// still valid there, and after another app's rotation with the same words it reveals the
/// master after that one. A rotation whose answer was lost is found in the log, not published
/// twice. A log that keeps moving stops it after `MAX_TRIES` builds.
#[tokio::test]
async fn a_rotation_that_meets_a_409_is_built_again_on_the_new_head() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let host = ViaExecutor::new(TestExecutor::new(&store, 2));
    let server = ViaExecutor::new(TestExecutor::new(&store, 3));
    let extra = ViaExecutor::new(TestExecutor::new(&store, 4));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 25).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &server,
        entry("server", DeviceKind::Server),
        false,
    )
    .await;
    let words = words::from_entropy(&[25; 16]);
    let racing = |races| {
        ViaExecutor::new(Racing {
            inner: a.rpc(),
            seed: words::seed(&words),
            races: RefCell::new(races),
        })
    };

    // Another app rotated to master #1 first: this one reveals master #2.
    let rotated = rotate(
        &racing(vec![Race::Rotate]),
        &vault,
        &did,
        &words,
        std::slice::from_ref(&d2),
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!((rotated.index, rotated.tries), (2, 2));
    assert_eq!(rotated.revoked, vec![d2.clone()]);
    assert_eq!(host.status().await.unwrap().revoked, Some(4));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(2));

    // Another app revoked d3 first: the rebuilt rotation revokes nothing.
    let rotated = rotate(
        &racing(vec![Race::Revoke(d3.clone())]),
        &vault,
        &did,
        &words,
        std::slice::from_ref(&d3),
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!((rotated.index, rotated.tries), (3, 2));
    assert!(rotated.revoked.is_empty());
    assert_eq!(server.status().await.unwrap().revoked, Some(5));

    // The rotation landed but its answer was lost: found in the log, not published again.
    a.rpc().lose_publish_answer.set(true);
    let publishes = a.rpc().publishes.get();
    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!((rotated.index, rotated.tries), (4, 1));
    assert_eq!(a.rpc().publishes.get(), publishes + 1);
    assert_eq!(rotated.head.seq, 7);

    // Another app with the same words made the very same rotation first: the store holds
    // this event, so it stands.
    let rotated = rotate(
        &racing(vec![Race::Rotate]),
        &vault,
        &did,
        &words,
        &[],
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!((rotated.index, rotated.tries, rotated.head.seq), (5, 1, 8));

    // Every build meets a 409.
    let d4 = link_app(
        &a,
        &vault,
        &did,
        &extra,
        entry("server 2", DeviceKind::Server),
        false,
    )
    .await;
    let publishes = a.rpc().publishes.get();
    let busy = rotate(
        &racing((0..MAX_TRIES).map(|_| Race::Rotate).collect()),
        &vault,
        &did,
        &words,
        &[d4],
        &SystemClock,
        &OsRandom,
    )
    .await;
    assert!(matches!(busy, Err(ClientError::LogBusy)), "{busy:?}");
    assert_eq!(a.rpc().publishes.get(), publishes + 2 * MAX_TRIES);
    assert_eq!(vault.master_index(&scid).unwrap(), Some(5), "nothing kept");

    let events = a.events(&did).await.unwrap();
    assert_eq!(
        assert_words_derive(&events, &words::seed(&words)),
        5 + MAX_TRIES as u32
    );
}

/// "Also replace my master key": a device asks for a revocation with a rotation, which the
/// holder answers with the words; the holder revokes with a rotation by itself too. Wrong
/// words, an empty list or an unknown key are refused before anything is published.
#[tokio::test]
async fn a_revocation_with_a_rotation_needs_the_words() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 26).await;
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &phone,
        entry("Phone", DeviceKind::PhoneApp),
        true,
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let words = words::from_entropy(&[26; 16]);

    for refused in [vec![], vec!["d9".to_string()]] {
        assert!(matches!(
            ask_confirm_revoke(&phone, &did, &refused, true, None).await,
            Err(ClientError::Protocol(_))
        ));
    }
    assert!(a.requests(0).await.unwrap().is_empty());
    let id = ask_confirm_revoke(&phone, &did, std::slice::from_ref(&d3), true, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    let request = listed.iter().find(|r| r.id == id).unwrap();
    assert_eq!(
        request.body,
        DeviceRequestBody::ConfirmRevoke {
            key_ids: vec![d3.clone()],
            rotate: true
        }
    );
    let other = words::from_entropy(&[99; 16]);
    assert!(matches!(
        answer_with_words(&a, &vault, &did, request, &other, &SystemClock, &OsRandom).await,
        Err(ClientError::NotTheseWords(_))
    ));
    assert_eq!(a.events(&did).await.unwrap().len(), 3, "nothing published");

    let Answered::Rotated { rotated, deleted } =
        answer_with_words(&a, &vault, &did, request, &words, &SystemClock, &OsRandom)
            .await
            .unwrap()
    else {
        panic!("expected a rotation");
    };
    deleted.unwrap();
    rotated.kept.as_ref().unwrap();
    assert_eq!(
        (rotated.index, rotated.revoked.clone()),
        (1, vec![d3.clone()])
    );
    assert_eq!(host.status().await.unwrap().revoked, Some(rotated.head.seq));
    assert!(
        a.requests(0).await.unwrap().is_empty(),
        "deleted by master #1"
    );
    rotated.record.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert!(!opened.record.devices.contains_key(&d3));

    assert!(matches!(
        revoke_and_rotate(&a, &vault, &did, &[], &words, &SystemClock, &OsRandom).await,
        Err(ClientError::Protocol(_))
    ));
    assert!(matches!(
        revoke_and_rotate(
            &a,
            &vault,
            &did,
            &["d1".into()],
            &words,
            &SystemClock,
            &OsRandom
        )
        .await,
        Err(ClientError::Protocol(_))
    ));
    let rotated = revoke_and_rotate(
        &a,
        &vault,
        &did,
        std::slice::from_ref(&d2),
        &words,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!((rotated.index, rotated.revoked), (2, vec![d2.clone()]));
    assert_eq!(
        phone.status().await.unwrap().revoked,
        Some(rotated.head.seq)
    );
    assert_eq!(
        assert_words_derive(&a.events(&did).await.unwrap(), &words::seed(&words)),
        2
    );
}

/// The words alone restore the identity on a new device: a wrong word is named, words of no
/// identity are refused; the right ones find the identity by its master #0, list its devices
/// by name (apps ticked), rotate to master #1 revoking the ticked ones, link this device, and
/// write the record listing it as the holder.
#[tokio::test]
async fn the_words_restore_the_identity_on_a_new_device() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let new = ViaExecutor::new(TestExecutor::new(&store, 4));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let new_vault = MemoryVault::new();
    let did = create(&a, &vault, 27).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let words = words::from_entropy(&[27; 16]);

    let mut typed = words.words();
    typed[4] = "zebrra";
    match RestoreFlow::start(&new, &new_vault, &typed.join(" "), &SystemClock, &OsRandom).await {
        Err(ClientError::Words(words::WordsError::UnknownWord { position, word, .. })) => {
            assert_eq!((position, word.as_str()), (5, "zebrra"));
        }
        Err(e) => panic!("expected the word named, got {e:?}"),
        Ok(_) => panic!("expected the word named"),
    }
    let unknown = words::from_entropy(&[98; 16]);
    assert!(matches!(
        RestoreFlow::start(&new, &new_vault, unknown.phrase(), &SystemClock, &OsRandom).await,
        Err(ClientError::NotTheseWords(_))
    ));

    let mut flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(flow.did(), did);
    assert!(flow.record_error().is_none(), "{:?}", flow.record_error());
    let mut devices = flow.devices().to_vec();
    devices.sort_by(|x, y| x.key_id.cmp(&y.key_id));
    assert_eq!(
        devices,
        vec![
            ListedDevice {
                key_id: "d1".into(),
                name: Some("This computer".into()),
                kind: Some(DeviceKind::DesktopApp),
                lock_out: true
            },
            ListedDevice {
                key_id: d2.clone(),
                name: Some("Desk".into()),
                kind: Some(DeviceKind::DesktopApp),
                lock_out: true
            },
            ListedDevice {
                key_id: d3.clone(),
                name: Some("host".into()),
                kind: Some(DeviceKind::CommunityHost),
                lock_out: false
            },
        ]
    );
    assert!(vault.unlocks().iter().all(|u| *u != MasterUse::Revoke));

    let lock_out: Vec<String> = devices
        .iter()
        .filter(|d| d.lock_out)
        .map(|d| d.key_id.clone())
        .collect();
    let restored = flow
        .finish(&lock_out, entry("New phone", DeviceKind::PhoneApp))
        .await
        .unwrap();
    assert_eq!((restored.index, restored.tries), (1, 1));
    assert_eq!(restored.revoked, lock_out);
    let enrolled = restored.enrolled.as_ref().unwrap();
    assert_eq!(enrolled.key_id, "d4");
    assert!(enrolled.pushes.is_empty(), "the kept holder was locked out");
    enrolled.record.as_ref().unwrap();
    assert_eq!(new_vault.staged_index(&scid).unwrap(), None);
    // The rotation is published: a second `finish` on the flow is refused, not rotated again.
    assert!(matches!(
        flow.finish(&[], entry("New phone", DeviceKind::PhoneApp))
            .await,
        Err(ClientError::Protocol(_))
    ));
    assert_eq!(new_vault.master_index(&scid).unwrap(), Some(1));

    let status = new.status().await.unwrap();
    assert_eq!(
        (status.did.as_deref(), status.key_id.as_deref()),
        (Some(did.as_str()), Some("d4"))
    );
    assert!(a.status().await.unwrap().revoked.is_some());
    assert!(desk.status().await.unwrap().revoked.is_some());
    assert!(host.status().await.unwrap().revoked.is_none());

    let opened = record::read(&new, &new_vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices["d4"].name, "New phone");
    assert_eq!(opened.record.devices["d4"].holds_master, HoldsMaster::Yes);
    assert!(opened.record.devices.contains_key(&d3));
    assert!(!opened.record.devices.contains_key(&d2));
    let sealed = new.record().await.unwrap().unwrap();
    assert_eq!(
        sealed.record.wrapped.keys().collect::<Vec<_>>(),
        [d3.as_str(), "d4", "recovery"]
    );
    assert_eq!(
        assert_words_derive(&new.events(&did).await.unwrap(), &words::seed(&words)),
        1
    );

    // The restored device holds the current master: it links and rotates as any holder.
    let rotated = rotate(&new, &new_vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(rotated.index, 2);
}

// ─── custody before publish in a rotation; the creator listed (review of #1667) ───

/// The creating computer is in the record's first version, `holdsMaster: true` with
/// `masterIndex` 0: another holder may remove its copy, and a rotation on another device sends
/// the creator the new master.
#[tokio::test]
async fn the_creating_computer_counts_as_a_holder() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 49).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    // The creator holds master #0, so the desk's copy is not the only one.
    remove_master(&desk, &desk_vault, &did, &OsRandom)
        .await
        .unwrap();
    assert_eq!(desk_vault.master_index(&scid).unwrap(), None);
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));

    // A rotation on the desk (the words alone) sends master #1 to the creator.
    let words = words::from_entropy(&[49; 16]);
    let rotated = rotate(
        &desk,
        &desk_vault,
        &did,
        &words,
        &[],
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    rotated.kept.as_ref().unwrap();
    assert_eq!(rotated.pushes.len(), 1);
    assert_eq!(rotated.pushes[0].0, "d1");
    assert!(rotated.pushes[0].1.is_ok(), "{:?}", rotated.pushes[0].1);
    let Refreshed::Taken(taken) = refresh_master(&a, &vault, &did, &OsRandom).await.unwrap() else {
        panic!("expected master #1 taken by the creator");
    };
    assert_eq!(taken.index, 1);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    for id in ["d1", d2.as_str()] {
        let e = &opened.record.devices[id];
        assert_eq!(
            (e.holds_master, e.master_index),
            (HoldsMaster::Yes, Some(1))
        );
    }
}

/// From the review of #1667, inverted: a rotation whose vault refuses the new key. The vault
/// stages master *n*+1 before the publish, so the refusal leaves nothing public: both devices
/// keep master #0, which stays current, and a rotation on a working vault goes through.
#[tokio::test]
async fn a_rotation_whose_vault_refuses_the_stage_publishes_nothing() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 42).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[42; 16]);
    let publishes = a.rpc().publishes.get();

    let refusing = RefusingVault(vault);
    let failed = rotate(&a, &refusing, &did, &words, &[], &SystemClock, &OsRandom).await;
    assert!(
        matches!(
            failed,
            Err(ClientError::Vault(
                ad4m_identity_client::VaultError::Cancelled
            ))
        ),
        "{failed:?}"
    );
    let vault = refusing.0;
    assert_eq!(a.rpc().publishes.get(), publishes, "nothing published");
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 1, "master #0 is still current");
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));
    assert_eq!(desk_vault.master_index(&scid).unwrap(), Some(0));
    assert!(matches!(
        refresh_master(&desk, &desk_vault, &did, &OsRandom)
            .await
            .unwrap(),
        Refreshed::Current
    ));
    assert!(matches!(
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.record.devices[&d2].holds_master, HoldsMaster::Yes);

    // A working vault rotates to #1 and the desk is sent it.
    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(rotated.index, 1);
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(rotated.pushes.len(), 1);
    assert!(rotated.pushes[0].1.is_ok(), "{:?}", rotated.pushes[0].1);
    let taken = take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(taken.index, 1);
}

/// An executor that refuses `identity.publish` for good (a `400`: the store will not take it).
struct RefusingPublish<'a>(&'a TestExecutor);

#[async_trait(?Send)]
impl ExecutorRpc for RefusingPublish<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.publish" {
            return Err(RpcError {
                code: 400,
                message: "invalid: refused".into(),
            });
        }
        self.0.call(method, params).await
    }
}

/// A rotation whose publish is refused: the key staged for it is discarded, the vault keeps
/// master #0, and the device it would revoke is not revoked.
#[tokio::test]
async fn a_rotation_whose_publish_fails_discards_the_staged_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 46).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let words = words::from_entropy(&[46; 16]);

    let refusing = ViaExecutor::new(RefusingPublish(a.rpc()));
    let failed = rotate(
        &refusing,
        &vault,
        &did,
        &words,
        std::slice::from_ref(&d2),
        &SystemClock,
        &OsRandom,
    )
    .await;
    assert!(
        matches!(&failed, Err(ClientError::Rpc(e)) if e.code == 400),
        "{failed:?}"
    );
    assert_eq!(vault.staged_index(&scid).unwrap(), None, "discarded");
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 1);
    assert!(chain.state().key_valid_at(&d2, chain.head().seq));
    assert!(
        desk.status().await.unwrap().revoked.is_none(),
        "nothing revoked"
    );
}

/// An executor whose `identity.publish` reaches the store, after which the flow is cut off:
/// the call never answers (the app quit, the machine slept).
struct CutOffAfterPublish<'a> {
    inner: &'a TestExecutor,
    published: Cell<bool>,
}

#[async_trait(?Send)]
impl ExecutorRpc for CutOffAfterPublish<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        let answer = self.inner.call(method, params).await;
        if method == "identity.publish" {
            self.published.set(true);
            std::future::pending::<()>().await;
        }
        answer
    }
}

/// A rotation cut off after its publish landed, before the vault promoted master #1: the key
/// stays staged, and the next `refresh_master` promotes it and lists this device holding it.
/// A staged key the log does not name is discarded.
#[tokio::test]
async fn a_rotation_cut_off_after_its_publish_is_promoted_by_refresh() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 47).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[47; 16]);

    let cut = ViaExecutor::new(CutOffAfterPublish {
        inner: a.rpc(),
        published: Cell::new(false),
    });
    tokio::select! {
        rotated = rotate(&cut, &vault, &did, &words, &[], &SystemClock, &OsRandom) => {
            panic!("the rotation was not cut off: {rotated:?}")
        }
        _ = async {
            while !cut.rpc().published.get() {
                tokio::task::yield_now().await;
            }
        } => {}
    }
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 2, "the rotation landed");
    assert_eq!(vault.master_index(&scid).unwrap(), Some(0));
    assert_eq!(vault.staged_index(&scid).unwrap(), Some(1));

    let Refreshed::Promoted { index, record } =
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap()
    else {
        panic!("expected the staged master #1 promoted");
    };
    assert_eq!(index, 1);
    record.unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(vault.staged_index(&scid).unwrap(), None);
    let unlocked = vault.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 1);
    drop(unlocked);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let d1 = &opened.record.devices["d1"];
    assert_eq!(
        (d1.holds_master, d1.master_index),
        (HoldsMaster::Yes, Some(1))
    );

    // A key staged ahead of the log (#5 is in no log yet) may belong to a rotation whose
    // publish is still on its way: refresh leaves it staged.
    vault
        .stage_master(
            &scid,
            5,
            ad4m_identity_client::Secret::new([5; 32]),
            &StageToken::new(&OsRandom),
        )
        .unwrap();
    assert!(matches!(
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    assert_eq!(vault.staged_index(&scid).unwrap(), Some(5));
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));

    // One behind the log (#0: a rotation the log is past) is discarded.
    vault
        .stage_master(
            &scid,
            0,
            ad4m_identity_client::Secret::new([0; 32]),
            &StageToken::new(&OsRandom),
        )
        .unwrap();
    assert!(matches!(
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    assert_eq!(vault.staged_index(&scid).unwrap(), None);
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
}

/// An executor whose `identity.publish` is held until `release` is set: before it reaches the
/// store (`before`), or after the store took it and before the answer comes back. `held` says
/// the publish reached the hold.
struct HeldPublish<'a> {
    inner: &'a dyn ExecutorRpc,
    before: bool,
    held: Cell<bool>,
    release: Cell<bool>,
}

impl<'a> HeldPublish<'a> {
    fn new(inner: &'a dyn ExecutorRpc, before: bool) -> Self {
        HeldPublish {
            inner,
            before,
            held: Cell::new(false),
            release: Cell::new(false),
        }
    }

    async fn hold(&self) {
        self.held.set(true);
        while !self.release.get() {
            tokio::task::yield_now().await;
        }
    }

    async fn until_held(&self) {
        while !self.held.get() {
            tokio::task::yield_now().await;
        }
    }
}

#[async_trait(?Send)]
impl ExecutorRpc for HeldPublish<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method != "identity.publish" {
            return self.inner.call(method, params).await;
        }
        if self.before {
            self.hold().await;
        }
        let answer = self.inner.call(method, params).await;
        if !self.before {
            self.hold().await;
        }
        answer
    }
}

/// From the review of #1667 (custody read mid-publish): another app on this computer reads
/// the custody status (`refresh_master` through its own handle on the same vault storage)
/// while a rotation's publish is on its way. The staged #1 is one ahead of the log, the normal
/// state then: the refresh leaves it, the publish lands, and the rotation keeps #1 here.
/// Nothing asks another device for it.
#[tokio::test]
async fn a_refresh_while_a_rotation_publishes_keeps_its_staged_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 48).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[48; 16]);
    // The other app's handle on the same storage.
    let other: &dyn Vault = &vault;

    let held = ViaExecutor::new(HeldPublish::new(a.rpc(), true));
    let (rotated, refreshed) = tokio::join!(
        rotate(&held, &vault, &did, &words, &[], &SystemClock, &OsRandom),
        async {
            held.rpc().until_held().await;
            assert_eq!(other.staged_index(&scid).unwrap(), Some(1), "staged");
            let refreshed = refresh_master(&a, other, &did, &OsRandom).await;
            held.rpc().release.set(true);
            refreshed
        }
    );
    let refreshed = refreshed.unwrap();
    assert!(matches!(refreshed, Refreshed::Current), "{refreshed:?}");
    let rotated = rotated.unwrap();
    assert_eq!(rotated.index, 1);
    assert!(rotated.kept.is_ok(), "{:?}", rotated.kept);
    rotated.record.unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(vault.staged_index(&scid).unwrap(), None);
    let unlocked = vault.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 1);
    drop(unlocked);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let d1 = &opened.record.devices["d1"];
    assert_eq!(
        (d1.holds_master, d1.master_index),
        (HoldsMaster::Yes, Some(1))
    );
    assert!(matches!(
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    assert!(a.requests(0).await.unwrap().is_empty(), "nothing asked");
}

/// A refresh through another handle that runs after the rotation landed and before its answer
/// came back promotes the staged #1 first: the rotation's own promote finds nothing staged and
/// the vault keeping #1, and reports the key kept.
#[tokio::test]
async fn a_refresh_that_promotes_first_leaves_the_rotation_kept() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 49).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[49; 16]);
    let other: &dyn Vault = &vault;

    let held = ViaExecutor::new(HeldPublish::new(a.rpc(), false));
    let (rotated, refreshed) = tokio::join!(
        rotate(&held, &vault, &did, &words, &[], &SystemClock, &OsRandom),
        async {
            held.rpc().until_held().await;
            let refreshed = refresh_master(&a, other, &did, &OsRandom).await;
            held.rpc().release.set(true);
            refreshed
        }
    );
    let refreshed = refreshed.unwrap();
    assert!(
        matches!(refreshed, Refreshed::Promoted { index: 1, .. }),
        "{refreshed:?}"
    );
    let rotated = rotated.unwrap();
    assert!(rotated.kept.is_ok(), "{:?}", rotated.kept);
    rotated.record.unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(vault.staged_index(&scid).unwrap(), None);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let d1 = &opened.record.devices["d1"];
    assert_eq!(
        (d1.holds_master, d1.master_index),
        (HoldsMaster::Yes, Some(1))
    );
    assert!(a.requests(0).await.unwrap().is_empty(), "nothing asked");
}

/// A key staged behind the log's current master (a later rotation superseded it, or its
/// publish was refused long ago) is discarded by refresh; the kept key is untouched.
#[tokio::test]
async fn a_key_staged_behind_the_log_is_discarded_by_refresh() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 50).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[50; 16]);
    rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(2));

    let seed = words::seed(&words);
    let one = words::master_key(&seed, 1).unwrap();
    vault
        .stage_master(
            &scid,
            1,
            ad4m_identity_client::Secret::new(one.to_bytes()),
            &StageToken::new(&OsRandom),
        )
        .unwrap();
    assert!(matches!(
        refresh_master(&a, &vault, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    assert_eq!(vault.staged_index(&scid).unwrap(), None, "discarded");
    assert_eq!(vault.master_index(&scid).unwrap(), Some(2));
    let unlocked = vault.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 2);
}

/// A vault shared with another app that promotes the staged key just before this flow does
/// (its `refresh_master` ran between this flow's read and its promote): this flow's promote
/// finds nothing staged.
struct PromotedFirst(MemoryVault);

impl Vault for PromotedFirst {
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.store_master(scid, index, secret)
    }
    fn master_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.master_index(scid)
    }
    fn forget_master(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.forget_master(scid)
    }
    fn stage_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
        owner: &StageToken,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.stage_master(scid, index, secret, owner)
    }
    fn staged_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.staged_index(scid)
    }
    fn promote_master(
        &self,
        scid: &str,
        index: u32,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        // The other app's promote, then this flow's.
        self.0.promote_master(scid, index)?;
        self.0.promote_master(scid, index)
    }
    fn discard_staged(
        &self,
        scid: &str,
        index: u32,
        owner: Option<&StageToken>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.discard_staged(scid, index, owner)
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

    fn record_seen(
        &self,
        scid: &str,
        version: u64,
    ) -> Result<Option<u64>, ad4m_identity_client::VaultError> {
        self.0.record_seen(scid, version)
    }

    fn record_seen_reset(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.record_seen_reset(scid)
    }
}

/// A restore whose staged key another app promoted first: the enrolment still keeps #1.
#[tokio::test]
async fn a_restore_whose_staged_key_was_promoted_first_enrols() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let new = ViaExecutor::new(TestExecutor::new(&store, 4));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 51).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[51; 16]);

    let new_vault = PromotedFirst(MemoryVault::new());
    let mut flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    let restored = flow
        .finish(&[], entry("New laptop", DeviceKind::DesktopApp))
        .await
        .unwrap();
    assert_eq!(restored.index, 1);
    let enrolled = restored.enrolled.as_ref().unwrap();
    enrolled.record.as_ref().unwrap();
    assert_eq!(new_vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(new_vault.staged_index(&scid).unwrap(), None);
}

/// From the review of #1667, inverted: a restore whose vault refuses the new key. Staged
/// before the publish, the refusal leaves nothing public: no device is revoked, master #0
/// stays current, and the words restore on a working vault.
#[tokio::test]
async fn a_restore_whose_vault_refuses_after_the_publish_is_recovered_by_the_words() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let new = ViaExecutor::new(TestExecutor::new(&store, 4));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 43).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[43; 16]);

    let refusing = RefusingVault(MemoryVault::new());
    let mut flow = RestoreFlow::start(&new, &refusing, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    let lock_out: Vec<String> = flow.devices().iter().map(|d| d.key_id.clone()).collect();
    assert_eq!(lock_out.len(), 2);
    let failed = flow
        .finish(&lock_out, entry("New laptop", DeviceKind::DesktopApp))
        .await;
    assert!(
        matches!(
            failed,
            Err(ClientError::Vault(
                ad4m_identity_client::VaultError::Cancelled
            ))
        ),
        "{failed:?}"
    );
    drop(flow);
    // Nothing public: master #0 current, both devices valid, the new computer not linked.
    let chain = ad4m_identity_core::fold(&new.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 1);
    let head = chain.head().seq;
    assert!(chain.state().key_valid_at("d1", head));
    assert!(chain.state().key_valid_at(&d2, head));
    assert_eq!(new.status().await.unwrap().did, None);
    assert!(a.status().await.unwrap().revoked.is_none());
    assert!(desk.status().await.unwrap().revoked.is_none());
    assert_eq!(refusing.0.master_index(&scid).unwrap(), None);
    assert_eq!(refusing.0.staged_index(&scid).unwrap(), None);

    // The words again, on a working vault: one rotation to #1, this computer linked.
    let new_vault = MemoryVault::new();
    let mut flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(flow.devices().len(), 2);
    let restored = flow
        .finish(&lock_out, entry("New laptop", DeviceKind::DesktopApp))
        .await
        .unwrap();
    assert_eq!(restored.index, 1);
    let enrolled = restored.enrolled.as_ref().unwrap();
    enrolled.record.as_ref().unwrap();
    assert_eq!(new_vault.master_index(&scid).unwrap(), Some(1));
    let status = new.status().await.unwrap();
    assert_eq!(status.did.as_deref(), Some(did.as_str()));
    assert_eq!(status.key_id.as_deref(), Some(enrolled.key_id.as_str()));
}

/// An executor that refuses `identity.claimOffer` while `refuse` is set (the link of the
/// restoring computer fails after the rotation landed).
struct RefusingClaim<'a> {
    inner: &'a TestExecutor,
    refuse: Cell<bool>,
}

#[async_trait(?Send)]
impl ExecutorRpc for RefusingClaim<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.claimOffer" && self.refuse.get() {
            return Err(RpcError {
                code: 503,
                message: "the store did not answer".into(),
            });
        }
        self.inner.call(method, params).await
    }
}

/// From the review of #1667, inverted: a restore whose link fails after the rotation landed.
/// `finish` answers the rotation (it stands) with the enrolment's error, and
/// `RestoreFlow::enrol` links this computer on the same flow: no words typed again, no second
/// rotation.
#[tokio::test]
async fn a_restore_whose_link_fails_after_the_publish_holds_a_key_it_cannot_use() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let new_rpc = TestExecutor::new(&store, 4);
    let vault = MemoryVault::new();
    let new_vault = MemoryVault::new();
    let did = create(&a, &vault, 44).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[44; 16]);

    let new = ViaExecutor::new(RefusingClaim {
        inner: &new_rpc,
        refuse: Cell::new(true),
    });
    let mut flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    let restored = flow
        .finish(
            &["d1".to_string()],
            entry("New laptop", DeviceKind::DesktopApp),
        )
        .await
        .unwrap();
    assert_eq!(
        (restored.index, restored.revoked.clone()),
        (1, vec!["d1".to_string()])
    );
    assert!(
        matches!(&restored.enrolled, Err(ClientError::Rpc(e)) if e.code == 503),
        "{:?}",
        restored.enrolled
    );
    assert_eq!(new_vault.master_index(&scid).unwrap(), Some(1), "kept");
    assert_eq!(
        new.status().await.unwrap().did,
        None,
        "not a device of the log yet"
    );
    assert!(a.status().await.unwrap().revoked.is_some());

    // The retry enrols on the same flow.
    new.rpc().refuse.set(false);
    let enrolled = flow
        .enrol(entry("New laptop", DeviceKind::DesktopApp))
        .await
        .unwrap();
    enrolled.record.as_ref().unwrap();
    let status = new.status().await.unwrap();
    assert_eq!(status.did.as_deref(), Some(did.as_str()));
    assert_eq!(status.key_id.as_deref(), Some(enrolled.key_id.as_str()));
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 2, "no second rotation");
    let opened = record::read(&new, &new_vault, &did).await.unwrap().unwrap();
    let listed = &opened.record.devices[&enrolled.key_id];
    assert_eq!(
        (
            listed.name.as_str(),
            listed.holds_master,
            listed.master_index
        ),
        ("New laptop", HoldsMaster::Yes, Some(1))
    );
    // An enrolment again finds the device in the log and does not link it twice.
    let again = flow
        .enrol(entry("New laptop", DeviceKind::DesktopApp))
        .await
        .unwrap();
    assert_eq!(again.key_id, enrolled.key_id);
}

/// A vault that writes a promoted key's item, then fails to write what vouches for its index:
/// it reports the index as unknown (`None`) from then on, as the `Vault::master_index`
/// contract says (authenticated, from the item; `None` when unknown).
struct LaggingIndex {
    inner: MemoryVault,
    lost: std::sync::Mutex<bool>,
}

impl Vault for LaggingIndex {
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.store_master(scid, index, secret)
    }
    fn master_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        if *self.lost.lock().unwrap() {
            return Ok(None);
        }
        self.inner.master_index(scid)
    }
    fn forget_master(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.forget_master(scid)
    }
    fn stage_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
        owner: &StageToken,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.stage_master(scid, index, secret, owner)
    }
    fn staged_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.inner.staged_index(scid)
    }
    fn promote_master(
        &self,
        scid: &str,
        index: u32,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        // The item is written; what vouches for its index is not.
        self.inner.promote_master(scid, index)?;
        *self.lost.lock().unwrap() = true;
        Err(ad4m_identity_client::VaultError::Unavailable(
            "the index could not be written".into(),
        ))
    }
    fn discard_staged(
        &self,
        scid: &str,
        index: u32,
        owner: Option<&StageToken>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.discard_staged(scid, index, owner)
    }
    fn unlock(
        &self,
        scid: &str,
        why: MasterUse,
        now: u64,
    ) -> Result<ad4m_identity_client::Unlocked, ad4m_identity_client::VaultError> {
        self.inner.unlock(scid, why, now)
    }
    fn store_pending_incept(&self, incept: &Event) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.store_pending_incept(incept)
    }
    fn pending_incepts(&self) -> Result<Vec<Event>, ad4m_identity_client::VaultError> {
        self.inner.pending_incepts()
    }
    fn incept_published(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.incept_published(scid)
    }
    fn keep_push(&self, push: &PendingPush) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.keep_push(push)
    }
    fn pending_pushes(&self) -> Result<Vec<PendingPush>, ad4m_identity_client::VaultError> {
        self.inner.pending_pushes()
    }
    fn push_done(&self, push: &PendingPush) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.push_done(push)
    }

    fn record_seen(
        &self,
        scid: &str,
        version: u64,
    ) -> Result<Option<u64>, ad4m_identity_client::VaultError> {
        self.inner.record_seen(scid, version)
    }

    fn record_seen_reset(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.inner.record_seen_reset(scid)
    }
}

/// From the review of #1667, adapted: after a rotation whose promote failed half way, the
/// vault holds the current master #1 but cannot tell its index. `rotate` reports the failed
/// promote (the rotation stands), and `refresh_master` never forgets a key on an unknown
/// index: the only copy of #1 stays.
#[tokio::test]
async fn a_refresh_that_trusts_a_stale_index_forgets_the_current_master() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let inner = MemoryVault::new();
    let did = create(&a, &inner, 45).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[45; 16]);
    let vault = LaggingIndex {
        inner,
        lost: std::sync::Mutex::new(false),
    };

    let rotated = rotate(&a, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(rotated.index, 1);
    assert!(
        matches!(
            rotated.kept,
            Err(ClientError::Vault(
                ad4m_identity_client::VaultError::Unavailable(_)
            ))
        ),
        "{:?}",
        rotated.kept
    );
    let unlocked = vault.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 1);
    drop(unlocked);
    assert_eq!(vault.master_index(&scid).unwrap(), None, "unknown");

    let refreshed = refresh_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert!(matches!(refreshed, Refreshed::Unknown), "{refreshed:?}");
    let unlocked = vault.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(
        unlocked.signer(0).unwrap().index(),
        1,
        "the current master #1, the only copy, is kept"
    );
    assert!(a.requests(0).await.unwrap().is_empty(), "nothing asked");
}

/// An executor that refuses `identity.deleteRequest` (the store did not answer).
struct RefusingDelete<'a>(&'a TestExecutor);

#[async_trait(?Send)]
impl ExecutorRpc for RefusingDelete<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.deleteRequest" {
            return Err(RpcError {
                code: 503,
                message: "the store did not answer".into(),
            });
        }
        self.0.call(method, params).await
    }
}

/// A `confirm-revoke` answered with the words whose delete fails after the rotation: the
/// answer is the rotation, with the delete's error, not an error.
#[tokio::test]
async fn answering_with_words_reports_a_failed_delete_with_the_rotation() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let phone = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 48).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    link_app(
        &a,
        &vault,
        &did,
        &phone,
        entry("Phone", DeviceKind::PhoneApp),
        true,
    )
    .await;
    let d3 = link_app(
        &a,
        &vault,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let words = words::from_entropy(&[48; 16]);
    let id = ask_confirm_revoke(&phone, &did, std::slice::from_ref(&d3), true, None)
        .await
        .unwrap();
    let listed = a.requests(0).await.unwrap();
    let request = listed.iter().find(|r| r.id == id).unwrap();

    let refusing = ViaExecutor::new(RefusingDelete(a.rpc()));
    let Answered::Rotated { rotated, deleted } = answer_with_words(
        &refusing,
        &vault,
        &did,
        request,
        &words,
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap() else {
        panic!("expected the rotation");
    };
    assert!(
        matches!(&deleted, Err(ClientError::Rpc(e)) if e.code == 503),
        "{deleted:?}"
    );
    assert_eq!(
        (rotated.index, rotated.revoked.clone()),
        (1, vec![d3.clone()])
    );
    rotated.kept.as_ref().unwrap();
    assert_eq!(vault.master_index(&scid).unwrap(), Some(1));
    assert_eq!(host.status().await.unwrap().revoked, Some(rotated.head.seq));
}

/// One vault store seen through two handles: the custody folder is shared by every desktop app
/// on a computer (we `host.rs`), and each app's custody service opens it on its own.
#[derive(Clone)]
struct SharedVault(Arc<MemoryVault>);

impl Vault for SharedVault {
    fn store_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.store_master(scid, index, secret)
    }
    fn master_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.master_index(scid)
    }
    fn forget_master(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.forget_master(scid)
    }
    fn stage_master(
        &self,
        scid: &str,
        index: u32,
        secret: ad4m_identity_client::Secret<[u8; 32]>,
        owner: &StageToken,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.stage_master(scid, index, secret, owner)
    }
    fn staged_index(&self, scid: &str) -> Result<Option<u32>, ad4m_identity_client::VaultError> {
        self.0.staged_index(scid)
    }
    fn promote_master(
        &self,
        scid: &str,
        index: u32,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.promote_master(scid, index)
    }
    fn discard_staged(
        &self,
        scid: &str,
        index: u32,
        owner: Option<&StageToken>,
    ) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.discard_staged(scid, index, owner)
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

    fn record_seen(
        &self,
        scid: &str,
        version: u64,
    ) -> Result<Option<u64>, ad4m_identity_client::VaultError> {
        self.0.record_seen(scid, version)
    }

    fn record_seen_reset(&self, scid: &str) -> Result<(), ad4m_identity_client::VaultError> {
        self.0.record_seen_reset(scid)
    }
}

/// An executor whose first `identity.publish` is preceded by another desktop app's custody
/// read on the same computer: `refresh_master` through that app's executor and its own handle
/// on the shared vault (we's `refresh_stale` calls it whenever a key is staged). Then the
/// publish goes through.
struct ReadBeforePublish<'a> {
    inner: &'a TestExecutor,
    other: &'a ViaExecutor<TestExecutor>,
    other_vault: SharedVault,
    did: String,
    /// What the other app's read answered, once it ran.
    read: RefCell<Option<String>>,
}

#[async_trait(?Send)]
impl ExecutorRpc for ReadBeforePublish<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.publish" && self.read.borrow().is_none() {
            let read = refresh_master(self.other, &self.other_vault, &self.did, &OsRandom).await;
            *self.read.borrow_mut() = Some(format!("{read:?}"));
        }
        self.inner.call(method, params).await
    }
}

/// From Data's review of #1667 (gist 46bcb3766dd925a120f3e80b6bb61ee1). Between
/// `stage_master` and the publish, the staged index is one ahead of the log: that is the normal
/// state of a rotation in flight, not a failed one. Another desktop app on this computer
/// (linked, not a holder) shares the custody folder; its status read in that window must leave
/// the staged key alone, so the publish that lands has a key to promote and this computer still
/// holds the master key afterwards.
#[tokio::test]
async fn a_custody_read_between_the_stage_and_the_publish_leaves_the_staged_key_alone() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let other_app = ViaExecutor::new(TestExecutor::new(&store, 2));
    let shared = Arc::new(MemoryVault::new());
    let mine = SharedVault(shared.clone());
    let theirs = SharedVault(shared.clone());
    let did = create(&a, &shared, 48).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &shared,
        &did,
        &other_app,
        entry("Other app", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let words = words::from_entropy(&[48; 16]);

    let reading = ViaExecutor::new(ReadBeforePublish {
        inner: a.rpc(),
        other: &other_app,
        other_vault: theirs,
        did: did.clone(),
        read: RefCell::new(None),
    });
    let rotated = rotate(&reading, &mine, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(rotated.index, 1);
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 2, "the rotation landed");
    let read = reading.rpc().read.borrow().clone();
    assert!(read.is_some(), "the other app read its status mid-publish");
    let opened = record::read(&a, &mine, &did).await.unwrap().unwrap();
    let d1 = opened.record.devices.get("d1").cloned();
    assert!(
        rotated.kept.is_ok(),
        "the promote found no staged master #1: {:?}\n  the other app's read answered {read:?}\n  \
         the shared vault keeps {:?}, staged {:?}\n  the record lists d1 as {:?}, {d2} as {:?}",
        rotated.kept,
        mine.master_index(&scid),
        mine.staged_index(&scid),
        d1.as_ref().map(|e| (e.holds_master, e.master_index)),
        opened
            .record
            .devices
            .get(&d2)
            .map(|e| (e.holds_master, e.master_index)),
    );
    assert_eq!(mine.master_index(&scid).unwrap(), Some(1));
    assert_eq!(mine.staged_index(&scid).unwrap(), None);
    assert_eq!(
        d1.map(|e| (e.holds_master, e.master_index)),
        Some((HoldsMaster::Yes, Some(1)))
    );
    // Both apps' next reads find the current key kept.
    assert!(matches!(
        refresh_master(&a, &mine, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
    assert!(matches!(
        refresh_master(&other_app, &theirs_again(&shared), &did, &OsRandom)
            .await
            .unwrap(),
        Refreshed::Current
    ));
}

fn theirs_again(shared: &Arc<MemoryVault>) -> SharedVault {
    SharedVault(shared.clone())
}

/// From Data's review of #1667: two apps on one computer rotate at once through the shared
/// vault. Both stage master #1; A's publish is refused for good and A discards what it staged,
/// which must not be the key B staged after it: B's publish lands and B keeps #1.
#[tokio::test]
async fn two_apps_rotating_at_once_keep_the_key_of_the_one_that_landed() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let shared = Arc::new(MemoryVault::new());
    let did = create(&a, &shared, 52).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    link_app(
        &a,
        &shared,
        &did,
        &b,
        entry("Other app", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let words = words::from_entropy(&[52; 16]);
    let (vault_a, vault_b) = (SharedVault(shared.clone()), SharedVault(shared.clone()));

    let refusing = RefusingPublish(a.rpc());
    let held_a = ViaExecutor::new(HeldPublish::new(&refusing, true));
    let held_b = ViaExecutor::new(HeldPublish::new(b.rpc(), true));
    let a_done = Cell::new(false);
    let (by_a, by_b, ()) = tokio::join!(
        async {
            let rotated = rotate(
                &held_a,
                &vault_a,
                &did,
                &words,
                &[],
                &SystemClock,
                &OsRandom,
            )
            .await;
            a_done.set(true);
            rotated
        },
        async {
            held_a.rpc().until_held().await;
            rotate(
                &held_b,
                &vault_b,
                &did,
                &words,
                &[],
                &SystemClock,
                &OsRandom,
            )
            .await
        },
        async {
            held_a.rpc().until_held().await;
            held_b.rpc().until_held().await;
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(1),
                "both staged #1"
            );
            // A's publish is refused and A discards what it staged; then B's publish goes.
            held_a.rpc().release.set(true);
            while !a_done.get() {
                tokio::task::yield_now().await;
            }
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(1),
                "A's discard left B's staged key"
            );
            held_b.rpc().release.set(true);
        }
    );
    assert!(
        matches!(&by_a, Err(ClientError::Rpc(e)) if e.code == 400),
        "{by_a:?}"
    );
    let by_b = by_b.unwrap();
    assert_eq!(by_b.index, 1);
    assert!(by_b.kept.is_ok(), "{:?}", by_b.kept);
    assert_eq!(shared.master_index(&scid).unwrap(), Some(1));
    assert_eq!(shared.staged_index(&scid).unwrap(), None);
    let unlocked = shared.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 1);
}

/// Logout removes the master key from this computer: a key staged at the log's current master
/// (a rotation cut off after its publish leaves one there) goes with the kept key, so the next
/// `refresh_master` has nothing to promote and the key does not come back.
#[tokio::test]
async fn removing_the_master_key_discards_a_key_staged_at_the_current_master() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let desk_vault = MemoryVault::new();
    let did = create(&a, &vault, 54).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    holding_app(
        &a,
        &vault,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let seed = words::seed(&words::from_entropy(&[54; 16]));
    let zero = words::master_key(&seed, 0).unwrap();
    vault
        .stage_master(
            &scid,
            0,
            ad4m_identity_client::Secret::new(zero.to_bytes()),
            &StageToken::new(&OsRandom),
        )
        .unwrap();

    remove_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert_eq!(
        vault.master_index(&scid).unwrap(),
        None,
        "kept key forgotten"
    );
    assert_eq!(vault.staged_index(&scid).unwrap(), None, "stage discarded");

    let refreshed = refresh_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert!(
        !matches!(refreshed, Refreshed::Promoted { .. }),
        "the removed key came back: {refreshed:?}"
    );
    assert_eq!(vault.master_index(&scid).unwrap(), None);
    assert_eq!(vault.staged_index(&scid).unwrap(), None);
    assert!(matches!(
        vault.unlock(&scid, MasterUse::Link, 0),
        Err(ad4m_identity_client::VaultError::NotFound)
    ));
}

/// Logout in one app while another app sharing the vault rotates: the other app staged #1 (one
/// ahead of the log) with its own token and its publish is on its way. Removing the master key
/// leaves that stage: the rotation lands, its owner promotes #1, and the vault holds #1.
#[tokio::test]
async fn removing_the_master_key_leaves_a_rotation_in_flight_in_another_app() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let other_app = ViaExecutor::new(TestExecutor::new(&store, 2));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 3));
    let shared = Arc::new(MemoryVault::new());
    let desk_vault = MemoryVault::new();
    let did = create(&a, &shared, 55).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    link_app(
        &a,
        &shared,
        &did,
        &other_app,
        entry("Other app", DeviceKind::DesktopApp),
        false,
    )
    .await;
    // Another holder of master #0, so this computer may remove its copy.
    holding_app(
        &a,
        &shared,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[55; 16]);
    let (mine, theirs) = (SharedVault(shared.clone()), SharedVault(shared.clone()));

    let held = ViaExecutor::new(HeldPublish::new(other_app.rpc(), true));
    let (rotated, removed) = tokio::join!(
        rotate(&held, &theirs, &did, &words, &[], &SystemClock, &OsRandom),
        async {
            held.rpc().until_held().await;
            assert_eq!(shared.staged_index(&scid).unwrap(), Some(1), "staged");
            let removed = remove_master(&a, &mine, &did, &OsRandom).await;
            assert_eq!(
                shared.master_index(&scid).unwrap(),
                None,
                "kept key forgotten"
            );
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(1),
                "the rotation's stage stays"
            );
            held.rpc().release.set(true);
            removed
        }
    );
    removed.unwrap();
    let rotated = rotated.unwrap();
    assert_eq!(rotated.index, 1);
    assert!(rotated.kept.is_ok(), "{:?}", rotated.kept);
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 2, "the rotation landed");
    assert_eq!(shared.master_index(&scid).unwrap(), Some(1));
    assert_eq!(shared.staged_index(&scid).unwrap(), None);
    let unlocked = shared.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 1);
    drop(unlocked);
    assert!(matches!(
        refresh_master(&a, &mine, &did, &OsRandom).await.unwrap(),
        Refreshed::Current
    ));
}

/// An executor whose `identity.publish` calls are each held before they reach the store until
/// `opened` counts past them: the first waits for `opened >= 1`, the second for `opened >= 2`.
/// `reached` counts the publishes that reached their hold.
struct GatedPublishes<'a> {
    inner: &'a dyn ExecutorRpc,
    reached: Cell<usize>,
    opened: Cell<usize>,
}

impl<'a> GatedPublishes<'a> {
    fn new(inner: &'a dyn ExecutorRpc) -> Self {
        GatedPublishes {
            inner,
            reached: Cell::new(0),
            opened: Cell::new(0),
        }
    }

    async fn until_reached(&self, publishes: usize) {
        while self.reached.get() < publishes {
            tokio::task::yield_now().await;
        }
    }
}

#[async_trait(?Send)]
impl ExecutorRpc for GatedPublishes<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.publish" {
            let nth = self.reached.get();
            self.reached.set(nth + 1);
            while self.opened.get() <= nth {
                tokio::task::yield_now().await;
            }
        }
        self.inner.call(method, params).await
    }
}

/// Two apps on one computer rotate at once through the shared vault and both stage #1. B's
/// publish lands; A's (it revokes a host, so it is not B's event) is answered `409`, so A
/// rebuilds on the new head and stages #2, which replaces B's staged #1 before B promotes it.
/// B's promote finds nothing staged at #1: B still keeps #1 (from the words, as a restore does)
/// and leaves A's #2 staged for A to promote.
#[tokio::test]
async fn a_rotation_whose_staged_key_another_rebuild_replaced_still_keeps_the_landed_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let shared = Arc::new(MemoryVault::new());
    let did = create(&a, &shared, 53).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    link_app(
        &a,
        &shared,
        &did,
        &b,
        entry("Other app", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let d3 = link_app(
        &a,
        &shared,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let words = words::from_entropy(&[53; 16]);
    let (vault_a, vault_b) = (SharedVault(shared.clone()), SharedVault(shared.clone()));

    let gated_a = ViaExecutor::new(GatedPublishes::new(a.rpc()));
    // B's publish reaches the store and lands; its answer is held.
    let held_b = ViaExecutor::new(HeldPublish::new(b.rpc(), false));
    let b_done = Cell::new(false);
    let (by_a, by_b, ()) = tokio::join!(
        rotate(
            &gated_a,
            &vault_a,
            &did,
            &words,
            std::slice::from_ref(&d3),
            &SystemClock,
            &OsRandom,
        ),
        async {
            gated_a.rpc().until_reached(1).await;
            let rotated = rotate(
                &held_b,
                &vault_b,
                &did,
                &words,
                &[],
                &SystemClock,
                &OsRandom,
            )
            .await;
            b_done.set(true);
            rotated
        },
        async {
            gated_a.rpc().until_reached(1).await;
            held_b.rpc().until_held().await;
            let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
            assert_eq!(chain.state().masters.len(), 2, "B's rotation to #1 landed");
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(1),
                "both staged #1"
            );
            // A's first publish is answered 409; A rebuilds on #1 and stages #2 over B's #1.
            gated_a.rpc().opened.set(1);
            gated_a.rpc().until_reached(2).await;
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(2),
                "A's rebuild re-staged the slot"
            );
            assert_eq!(shared.master_index(&scid).unwrap(), Some(0));
            // B's answer comes back and B promotes #1, which is no longer staged.
            held_b.rpc().release.set(true);
            while !b_done.get() {
                tokio::task::yield_now().await;
            }
            assert_eq!(
                shared.master_index(&scid).unwrap(),
                Some(1),
                "B keeps the key that landed"
            );
            assert_eq!(
                shared.staged_index(&scid).unwrap(),
                Some(2),
                "A's #2 stays staged"
            );
            // A's rotation to #2 goes and lands; A promotes it.
            gated_a.rpc().opened.set(2);
        }
    );
    let by_b = by_b.unwrap();
    assert_eq!(by_b.index, 1);
    assert!(by_b.kept.is_ok(), "{:?}", by_b.kept);
    let by_a = by_a.unwrap();
    assert_eq!((by_a.index, by_a.revoked.clone()), (2, vec![d3]));
    assert!(by_a.kept.is_ok(), "{:?}", by_a.kept);
    assert_eq!(shared.master_index(&scid).unwrap(), Some(2));
    assert_eq!(shared.staged_index(&scid).unwrap(), None);
    let unlocked = shared.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 2);
}

/// Lal's P2-1 at 7117f0b10: the restore's enrolment claims its offer with `canKeepMaster:
/// false`, so the log never lets the restored device keep the master key, although its vault
/// holds the new master. It then counts for nothing: not sent later rotations, not a keeper for
/// `send_master`, and not "another copy" when an older holder removes its key.
#[tokio::test]
async fn a_restored_device_is_a_keeper_in_the_log() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let new = ViaExecutor::new(TestExecutor::new(&store, 4));
    let vault = MemoryVault::new();
    let new_vault = MemoryVault::new();
    let did = create(&a, &vault, 54).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let words = words::from_entropy(&[54; 16]);

    let mut flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    let restored = flow
        .finish(&[], entry("New laptop", DeviceKind::DesktopApp))
        .await
        .unwrap();
    let enrolled = restored.enrolled.unwrap();
    assert_eq!(new_vault.master_index(&scid).unwrap(), Some(1));
    let events = a.events(&did).await.unwrap();
    assert!(
        link::may_keep_master(&events, &enrolled.key_id),
        "the log lets the restored device keep the master key it holds"
    );

    // d1 kept #0 and was sent #1; it takes it, then removes its copy: the restored laptop holds
    // the other one.
    let refreshed = refresh_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert!(matches!(refreshed, Refreshed::Taken(_)), "{refreshed:?}");
    remove_master(&a, &vault, &did, &OsRandom)
        .await
        .expect("the restored device holds the current master key");
    assert_eq!(vault.master_index(&scid).unwrap(), None);
}

/// Lal's P2-2 at 7117f0b10. As `a_rotation_whose_staged_key_another_rebuild_replaced_still_keeps_the_landed_key`,
/// but B's answer comes back only after A's rotation to #2 landed and A promoted #2. B's promote
/// of #1 finds nothing staged, and its fallback stores #1 from the words over the kept #2: the
/// vault goes back a generation while the log is at #2.
#[tokio::test]
async fn a_late_rotation_answer_does_not_overwrite_a_newer_kept_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let host = ViaExecutor::new(TestExecutor::new(&store, 3));
    let shared = Arc::new(MemoryVault::new());
    let did = create(&a, &shared, 55).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    link_app(
        &a,
        &shared,
        &did,
        &b,
        entry("Other app", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let d3 = link_app(
        &a,
        &shared,
        &did,
        &host,
        entry("host", DeviceKind::CommunityHost),
        false,
    )
    .await;
    let words = words::from_entropy(&[55; 16]);
    let (vault_a, vault_b) = (SharedVault(shared.clone()), SharedVault(shared.clone()));

    let gated_a = ViaExecutor::new(GatedPublishes::new(a.rpc()));
    let held_b = ViaExecutor::new(HeldPublish::new(b.rpc(), false));
    let a_done = Cell::new(false);
    let (by_a, by_b, ()) = tokio::join!(
        async {
            let rotated = rotate(
                &gated_a,
                &vault_a,
                &did,
                &words,
                std::slice::from_ref(&d3),
                &SystemClock,
                &OsRandom,
            )
            .await;
            a_done.set(true);
            rotated
        },
        async {
            gated_a.rpc().until_reached(1).await;
            rotate(
                &held_b,
                &vault_b,
                &did,
                &words,
                &[],
                &SystemClock,
                &OsRandom,
            )
            .await
        },
        async {
            gated_a.rpc().until_reached(1).await;
            held_b.rpc().until_held().await;
            // B's #1 landed; A's first publish is answered 409, A rebuilds and stages #2.
            gated_a.rpc().opened.set(1);
            gated_a.rpc().until_reached(2).await;
            assert_eq!(shared.staged_index(&scid).unwrap(), Some(2));
            // A's #2 lands and A promotes it, all before B hears back.
            gated_a.rpc().opened.set(2);
            while !a_done.get() {
                tokio::task::yield_now().await;
            }
            assert_eq!(shared.master_index(&scid).unwrap(), Some(2), "A kept #2");
            assert_eq!(shared.staged_index(&scid).unwrap(), None);
            held_b.rpc().release.set(true);
        }
    );
    let by_a = by_a.unwrap();
    assert_eq!(by_a.index, 2);
    assert!(by_a.kept.is_ok(), "{:?}", by_a.kept);
    let by_b = by_b.unwrap();
    assert_eq!(by_b.index, 1);
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 3, "the log is at #2");
    assert_eq!(
        shared.master_index(&scid).unwrap(),
        Some(2),
        "B's late completion of #1 must not replace the kept #2 (B's kept: {:?})",
        by_b.kept
    );
    let unlocked = shared.unlock(&scid, MasterUse::Link, 0).unwrap();
    assert_eq!(unlocked.signer(0).unwrap().index(), 2);
}

/// An executor whose `identity.takeMaster` is preceded by another app installing master #1 in
/// the shared vault (its own take or promote landing), as a refresh in this app awaits the call.
struct InstallDuringTake<'a> {
    inner: &'a TestExecutor,
    other_vault: SharedVault,
    scid: String,
    key: [u8; 32],
    installed: Cell<bool>,
}

#[async_trait(?Send)]
impl ExecutorRpc for InstallDuringTake<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.takeMaster" && !self.installed.replace(true) {
            self.other_vault
                .store_master(&self.scid, 1, ad4m_identity_client::Secret::new(self.key))
                .unwrap();
        }
        self.inner.call(method, params).await
    }
}

/// Lal's P2-3 at 7117f0b10: `refresh_master` reads the kept index (#0, behind the log at #1),
/// awaits `take_master`, and forgets on the index it read. Another app sharing the vault installs
/// #1 while the call is in flight, and the forget deletes the current key.
#[tokio::test]
async fn a_refresh_forgets_only_the_key_it_decided_on() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let shared = Arc::new(MemoryVault::new());
    let desk_vault = MemoryVault::new();
    let did = create(&a, &shared, 56).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    holding_app(
        &a,
        &shared,
        &did,
        &desk,
        &desk_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[56; 16]);
    // The desk rotates to #1; its push of #1 to d1 fails, so d1's inbox stays empty.
    desk.rpc().refuse_inbox_put.set(true);
    let rotated = rotate(
        &desk,
        &desk_vault,
        &did,
        &words,
        &[],
        &SystemClock,
        &OsRandom,
    )
    .await
    .unwrap();
    assert_eq!(rotated.index, 1);
    assert_eq!(shared.master_index(&scid).unwrap(), Some(0));
    let key1 = words::master_key(&words::seed(&words), 1)
        .unwrap()
        .to_bytes();

    let racing = ViaExecutor::new(InstallDuringTake {
        inner: a.rpc(),
        other_vault: SharedVault(shared.clone()),
        scid: scid.clone(),
        key: key1,
        installed: Cell::new(false),
    });
    let refreshed = refresh_master(&racing, &SharedVault(shared.clone()), &did, &OsRandom)
        .await
        .unwrap();
    assert!(
        racing.rpc().installed.get(),
        "the other app installed #1 mid-refresh"
    );
    assert_eq!(
        shared.master_index(&scid).unwrap(),
        Some(1),
        "the current master installed by another app survives this refresh ({refreshed:?})"
    );
}

/// A store can serve back an older version with a receipt that still verifies: the read keeps
/// the highest version seen per identity in the vault and refuses a lower one, until the
/// caller resets the mark.
#[tokio::test]
async fn a_store_serving_back_an_older_record_is_refused() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 27).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        false,
    )
    .await;
    let served_v1 = a.rpc().call("identity.record", json!({})).await.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.version, 2);
    record::write(
        &a,
        &vault,
        &did,
        &[Change::Rename {
            key_id: d2.clone(),
            name: "Studio".into(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();

    *a.rpc().serve_record.borrow_mut() = Some(served_v1);
    let refused = record::read(&a, &vault, &did).await;
    assert!(
        matches!(
            refused,
            Err(ClientError::ServedRecordVersion { served: 2, seen: 3 })
        ),
        "{refused:?}"
    );
    vault.record_seen_reset(&scid).unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(opened.version, 2);
}

/// A version a device wrote before its revocation still opens for the revoke flow's own
/// write: the store's receipt names the seq the write was accepted at, where the writer was
/// valid. The write after the publish builds on it, so what the revoked device wrote last
/// survives; only its own entry and pending revocation go.
#[tokio::test]
async fn what_the_revoked_device_wrote_last_survives_the_revokes_record_write() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 28).await;
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
    // d2's last write lists d1's executor under a new name.
    record::write(
        &b,
        &vault,
        &did,
        &[Change::Device {
            key_id: "d1".into(),
            entry: entry("Holder rig", DeviceKind::Server),
        }],
        &OsRandom,
    )
    .await
    .unwrap();

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
    revoked.record.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices["d1"],
        entry("Holder rig", DeviceKind::Server),
        "d2's last write survives the record write over it"
    );
    assert!(!opened.record.devices.contains_key(&d2));
}

/// Review coasys/ad4m#1649 at 25aecbb57 (Lal's P2): `retry_pushes` reads the listed devices
/// once, then writes a missing device's entry with a plain `record::write`, which replaces the
/// entry whole on the newest version. A device whose entry landed in between, holding the key
/// (`holdsMaster: true`), is overwritten with the generic `"no"` entry, and the push that
/// follows marks it `"offered"`. The entry write must be insert-if-absent, checked on every
/// attempt. Fails at 25aecbb57.
#[tokio::test]
async fn a_retry_does_not_overwrite_an_entry_that_landed_holding_the_master_key() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 24).await;
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
    let claimed_desk = desk
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": true }),
        )
        .await
        .unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    // The entry write and its fallback both fail: no entry, the push waits on the retry list.
    a.rpc().refuse_record_puts.set(2);
    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert!(linked.record.is_err(), "{:?}", linked.record);
    assert_eq!(vault.pending_pushes().unwrap().len(), 1);
    join(&desk, &claimed_desk["offerId"]).await;

    // Between the retry's look at the record (empty) and its entry write, the desk's entry
    // lands, holding the master key (as after another retry's push and the desk's take).
    let lands = EntryLandsBetween {
        inner: a.rpc(),
        a: &a,
        did: &did,
        key_id: &linked.key_id,
        reads: Cell::new(0),
    };
    let racing = ViaExecutor::new(lands);
    let retried = retry_pushes(&racing, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(retried.pushes.len(), 1, "{:?}", retried.pushes);
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let listed = &opened.record.devices[&linked.key_id];
    assert_eq!(
        (listed.name.as_str(), listed.holds_master),
        ("Desk", HoldsMaster::Yes),
        "the retry overwrote the entry that landed holding the master key"
    );
}

/// Forwards to `inner`; on the second `identity.record` read (the entry write's own read
/// inside the retry) it first has `a` write the desk's entry as holding the master key (the
/// desk's own device cannot open the record yet: its key is not in `wrapped`).
struct EntryLandsBetween<'a> {
    inner: &'a TestExecutor,
    a: &'a ViaExecutor<TestExecutor>,
    did: &'a str,
    key_id: &'a str,
    reads: Cell<usize>,
}

#[async_trait(?Send)]
impl ExecutorRpc for EntryLandsBetween<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.record" {
            let n = self.reads.get() + 1;
            self.reads.set(n);
            if n == 2 {
                record::write(
                    self.a,
                    &MemoryVault::new(),
                    self.did,
                    &[Change::Device {
                        key_id: self.key_id.to_string(),
                        entry: DeviceEntry {
                            holds_master: HoldsMaster::Yes,
                            ..entry("Desk", DeviceKind::DesktopApp)
                        },
                    }],
                    &OsRandom,
                )
                .await
                .unwrap();
            }
        }
        self.inner.call(method, params).await
    }
}

/// Review coasys/ad4m#1649 at 25aecbb57 (Lal's P2): `read_at` answers `Ok(None)` for a
/// missing record before it looks at the high-water mark, so a store that answers "missing"
/// after this client read version 2 hides what it read (holders, pending revocations) instead
/// of failing as a rollback. Absence is version 0, under any version seen. Fails at 25aecbb57.
#[tokio::test]
async fn a_store_answering_no_record_after_a_version_was_read_is_refused() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 27).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        false,
    )
    .await;
    record::write(
        &a,
        &vault,
        &did,
        &[Change::Rename {
            key_id: d2,
            name: "Studio".into(),
        }],
        &OsRandom,
    )
    .await
    .unwrap();

    *a.rpc().serve_record.borrow_mut() = Some(Value::Null);
    let refused = record::read(&a, &vault, &did).await;
    assert!(
        matches!(
            refused,
            Err(ClientError::ServedRecordVersion { served: 0, seen: 3 })
        ),
        "a missing record after version 3 was read: {refused:?}"
    );
    // A reset says the caller means it: then "no record" is an answer again.
    vault.record_seen_reset(&scid).unwrap();
    assert!(record::read(&a, &vault, &did).await.unwrap().is_none());
}

/// Review coasys/ad4m#1649 at d9b5f4a28: the entry write before the push fails (a transient
/// store error). The master key must not go: a device that took it would mark an entry that
/// does not exist (a silent no-op), and the fallback would then write the entry as
/// `"offered"`, so the holder reads "offered" with an empty inbox. The push waits on the retry
/// list instead. Fails at d9b5f4a28: the key is pushed although the entry write failed.
#[tokio::test]
async fn a_failed_entry_write_holds_the_master_key_back() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 24).await;
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
    let claimed_desk = desk
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": true }),
        )
        .await
        .unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    // The entry write, the first `recordPut` of this confirm, fails before the store.
    a.rpc().refuse_record_put.set(true);
    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert!(
        !matches!(&linked.master, Some(Ok(_))),
        "the master key was pushed although the entry write failed: {:?}",
        linked.master
    );
    join(&desk, &claimed_desk["offerId"]).await;
    let waiting = take_master(&desk, &MemoryVault::new(), &OsRandom)
        .await
        .unwrap();
    assert!(
        waiting.is_none(),
        "a master key waits in the device's inbox although the entry write failed"
    );
    if let Some(opened) = record::read(&a, &vault, &did).await.unwrap() {
        assert_ne!(
            opened
                .record
                .devices
                .get(&linked.key_id)
                .map(|e| e.holds_master),
            Some(HoldsMaster::Offered),
            "the entry says offered for a key that was never sent"
        );
    }
    // The push waits for `retry_pushes`.
    assert_eq!(vault.pending_pushes().unwrap().len(), 1);
}

/// Review coasys/ad4m#1649 (Lal, P3): a double failure in one outage — the entry write before
/// the push and the confirm's fallback both fail — leaves a pending push whose device the
/// record does not list. The retry must write the entry before the key goes again: without it
/// the device takes the key, acks it, and its `holdsMaster: true` write is refused for naming
/// a device the record does not list, so the key is held with no entry. Here the retry writes
/// the entry, pushes, and the device ends `Yes`.
#[tokio::test]
async fn a_retried_push_writes_the_devices_entry_before_the_key_goes() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 25).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
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
    let claimed_desk = desk
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": true }),
        )
        .await
        .unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    // Both writes of the entry fail: the one before the push and the confirm's fallback.
    a.rpc().refuse_record_puts.set(2);
    let linked = flow
        .confirm(&claimed, entry("Desk", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    assert!(
        matches!(&linked.master, Some(Err(ClientError::Step(_)))),
        "{:?}",
        linked.master
    );
    assert!(linked.record.is_err());
    let key_id = linked.key_id.clone();
    let pending = PendingPush {
        scid: scid.clone(),
        key_id: key_id.clone(),
    };
    assert_eq!(vault.pending_pushes().unwrap(), vec![pending]);
    join(&desk, &claimed_desk["offerId"]).await;

    // With the store back the retry writes the entry, then pushes; the device takes the key
    // and marks the entry the retry wrote `Yes`.
    let retried = retry_pushes(&a, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(retried.pushes.len(), 1);
    assert!(retried.pushes[0].1.is_ok(), "{:?}", retried.pushes);
    assert!(matches!(retried.record, Some(Ok(_))));
    assert!(vault.pending_pushes().unwrap().is_empty());
    let desk_vault = MemoryVault::new();
    let taken = take_master(&desk, &desk_vault, &OsRandom)
        .await
        .unwrap()
        .expect("a master key waits");
    taken.record.unwrap();
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&key_id].holds_master,
        HoldsMaster::Yes
    );
}

/// The same outage over two pending pushes: one device whose entry landed and one whose it
/// never did, and the retry's entry write for it fails too. The entry-less device stays on
/// the retry list; the other's push goes and is marked `"offered"`: one cannot block the rest.
#[tokio::test]
async fn a_retry_batch_with_an_entry_less_device_still_marks_the_others() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let desk = ViaExecutor::new(TestExecutor::new(&store, 2));
    let laptop = ViaExecutor::new(TestExecutor::new(&store, 3));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 26).await;
    let scid = ad4m_identity_core::scid_of(&did).unwrap().to_string();
    let d2 = link_app(
        &a,
        &vault,
        &did,
        &desk,
        entry("Desk", DeviceKind::DesktopApp),
        true,
    )
    .await;
    // The laptop links to keep the master key, but both writes of its entry fail.
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
    laptop
        .rpc()
        .call(
            "identity.claimOffer",
            json!({ "offer": flow.offer_string(), "canKeepMaster": true }),
        )
        .await
        .unwrap();
    let claimed = flow.wait_for_claim().await.unwrap();
    a.rpc().refuse_record_puts.set(2);
    let linked = flow
        .confirm(&claimed, entry("Laptop", DeviceKind::DesktopApp), true)
        .await
        .unwrap();
    let d3 = linked.key_id.clone();
    vault
        .keep_push(&PendingPush {
            scid: scid.clone(),
            key_id: d2.clone(),
        })
        .unwrap();

    // The retry's entry write for the laptop fails too: its push is skipped and stays on the
    // retry list; the desk's push goes and is marked.
    a.rpc().refuse_record_puts.set(1);
    let retried = retry_pushes(&a, &vault, &did, &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(retried.pushes.len(), 2, "{:?}", retried.pushes);
    assert_eq!(retried.pushes[0].0, d3);
    assert!(retried.pushes[0].1.is_err(), "{:?}", retried.pushes);
    assert_eq!(retried.pushes[1].0, d2);
    assert!(retried.pushes[1].1.is_ok(), "{:?}", retried.pushes);
    assert_eq!(
        vault.pending_pushes().unwrap(),
        vec![PendingPush {
            scid,
            key_id: d3.clone()
        }]
    );
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    assert_eq!(
        opened.record.devices[&d2].holds_master,
        HoldsMaster::Offered
    );
    assert!(!opened.record.devices.contains_key(&d3));
}

/// Lal's P2-1 at 8f8a46e56, end to end. A rotation lands and its flow is cut off before the
/// master-signed rewrite of the record (the record still carries master #0's attestation of the
/// recovery key). The next custody read promotes the staged key and writes `holdsMaster` as the
/// device: that write carries no recovery key, since the old attestation no longer counts, and
/// the store takes it. From then on the words cannot open the record: a restore on a fresh
/// executor finds a record no recovery key wraps.
#[tokio::test]
#[ignore = "known gap (review of #1667 at 8f8a46e56): a device write after a rotation drops the recovery key the words need; the fix is a design call (see the PR). Run with --ignored."]
async fn a_device_write_after_a_rotation_keeps_the_words_access_to_the_record() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let vault = MemoryVault::new();
    let did = create(&a, &vault, 57).await;
    let words = words::from_entropy(&[57; 16]);

    let cut = ViaExecutor::new(CutOffAfterPublish {
        inner: a.rpc(),
        published: Cell::new(false),
    });
    tokio::select! {
        rotated = rotate(&cut, &vault, &did, &words, &[], &SystemClock, &OsRandom) => {
            panic!("the rotation was not cut off: {rotated:?}")
        }
        _ = async {
            while !cut.rpc().published.get() {
                tokio::task::yield_now().await;
            }
        } => {}
    }
    let chain = ad4m_identity_core::fold(&a.events(&did).await.unwrap()).unwrap();
    assert_eq!(chain.state().masters.len(), 2, "the rotation landed");

    // The next custody read finishes the rotation on this device: an ordinary device write.
    let refreshed = refresh_master(&a, &vault, &did, &OsRandom).await.unwrap();
    assert!(
        matches!(refreshed, Refreshed::Promoted { .. }),
        "{refreshed:?}"
    );
    let opened = record::read(&a, &vault, &did).await.unwrap().unwrap();
    let recovery_after_refresh = opened.recovery.is_some();

    // The computer is lost; the words on a fresh executor.
    let new = ViaExecutor::new(TestExecutor::new(&store, 4));
    let new_vault = MemoryVault::new();
    let flow = RestoreFlow::start(&new, &new_vault, words.phrase(), &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert!(
        flow.record_error().is_none(),
        "the words cannot open the record after the device write: {:?} (the record carried a \
         recovery key that counts after the refresh: {recovery_after_refresh})",
        flow.record_error()
    );
    assert!(
        !flow.devices().is_empty() && flow.devices().iter().any(|d| d.name.is_some()),
        "the restore lists the devices by their recorded names: {:?}",
        flow.devices()
    );
}

/// An executor whose first `identity.record` read after `armed` is preceded by another device
/// taking the master key from its inbox and recording that it holds it.
struct TakeBeforeRecordRead<'a> {
    inner: &'a TestExecutor,
    taker: &'a ViaExecutor<TestExecutor>,
    taker_vault: &'a MemoryVault,
    armed: Cell<bool>,
    taken: RefCell<Option<Taken>>,
}

#[async_trait(?Send)]
impl ExecutorRpc for TakeBeforeRecordRead<'_> {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
        if method == "identity.inboxPut" {
            self.armed.set(true);
        }
        if method == "identity.record" && self.armed.get() && self.taken.borrow().is_none() {
            let taken = take_master(self.taker, self.taker_vault, &OsRandom)
                .await
                .unwrap()
                .expect("the pushed master key waits in the inbox");
            taken.record.as_ref().unwrap();
            *self.taken.borrow_mut() = Some(taken);
        }
        self.inner.call(method, params).await
    }
}

/// Lal's P2-3 at 8f8a46e56. A rotates to #1 and pushes it to B, a holder. B takes it from its
/// inbox and records `holdsMaster: true, masterIndex: 1` before A's hand-over writes the record.
/// A's write builds on that newest version and must not put `"offered"` (and no index) over
/// B's confirmation: afterwards A could not count B as another copy, and B's next refresh,
/// finding the current key kept, would never repair it.
#[tokio::test]
async fn a_hand_over_never_overwrites_a_holder_that_already_took_the_new_master() {
    let store = TestStore::new();
    let a = ViaExecutor::new(TestExecutor::new(&store, 1));
    let b = ViaExecutor::new(TestExecutor::new(&store, 2));
    let vault = MemoryVault::new();
    let b_vault = MemoryVault::new();
    let did = create(&a, &vault, 58).await;
    let d2 = holding_app(
        &a,
        &vault,
        &did,
        &b,
        &b_vault,
        entry("Desk", DeviceKind::DesktopApp),
    )
    .await;
    let words = words::from_entropy(&[58; 16]);

    let racing = ViaExecutor::new(TakeBeforeRecordRead {
        inner: a.rpc(),
        taker: &b,
        taker_vault: &b_vault,
        armed: Cell::new(false),
        taken: RefCell::new(None),
    });
    let rotated = rotate(&racing, &vault, &did, &words, &[], &SystemClock, &OsRandom)
        .await
        .unwrap();
    assert_eq!(rotated.index, 1);
    assert!(
        rotated.pushes.iter().any(|(k, r)| *k == d2 && r.is_ok()),
        "{:?}",
        rotated.pushes
    );
    let taken = racing.rpc().taken.borrow().as_ref().map(|t| t.index);
    assert_eq!(taken, Some(1), "B took #1 before A's record write");
    rotated.record.as_ref().unwrap();
    assert_eq!(
        b_vault
            .master_index(ad4m_identity_core::scid_of(&did).unwrap())
            .unwrap(),
        Some(1)
    );

    let opened = record::read(&b, &b_vault, &did).await.unwrap().unwrap();
    let entry_b = &opened.record.devices[&d2];
    assert_eq!(
        (entry_b.holds_master, entry_b.master_index),
        (HoldsMaster::Yes, Some(1)),
        "A's hand-over overwrote B's confirmation"
    );
}
