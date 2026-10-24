//! The store under test: a real `Store` on a temporary SQLite file with an injected clock,
//! called through its router (the same `Router` that `identity-store serve` runs).
#![allow(dead_code)]

pub mod agent;

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use ad4m_identity_core::link::{Claim, ClaimView, OfferKind, SignedOffer};
use ad4m_identity_core::Event;
use axum::body::Body;
use axum::http::{Request, StatusCode};
use axum::Router;
use http_body_util::BodyExt;
use identity_store::db::Db;
use identity_store::request::REQUEST_HEADER;
use identity_store::{router, Settings, Store, StoreKey};
use serde_json::{json, Value};
use tower::ServiceExt;

use agent::{Device, Person};

/// True when `head` is signed by its `store` key and that key is one of `trusted`, as an
/// executor checks it.
pub fn verify_head(head: &ad4m_ldk::identity::SignedHead, trusted: &[String]) -> bool {
    ad4m_identity_core::HeadStatement {
        scid: &head.scid,
        seq: head.seq,
        hash: &head.hash,
        time: head.time,
        store: &head.store,
    }
    .verify(&head.sig, trusted)
}

/// The store's clock at the start of every test.
pub const T0: u64 = 1_800_000_000;

pub struct TestStore {
    pub store: Arc<Store>,
    pub router: Router,
    clock: Arc<AtomicU64>,
    pub dir: tempfile::TempDir,
}

/// One answer: status, body as JSON (`Null` for an empty body) and the raw body.
pub struct Reply {
    pub status: StatusCode,
    pub body: Value,
    pub raw: Vec<u8>,
    pub headers: axum::http::HeaderMap,
}

impl TestStore {
    pub fn new() -> TestStore {
        Self::with_settings(Settings {
            pow_bits: 8,
            feed_token: Some("mirror-token".into()),
            cors_origins: Vec::new(),
        })
    }

    pub fn with_settings(settings: Settings) -> TestStore {
        let dir = tempfile::tempdir().unwrap();
        let db = Db::open(dir.path().join("store.sqlite")).unwrap();
        let clock = Arc::new(AtomicU64::new(T0));
        let read = clock.clone();
        let store = Store::new(
            db,
            StoreKey::from_secret([7; 32]),
            Arc::new(move || read.load(Ordering::SeqCst)),
            settings,
        );
        TestStore {
            router: router(store.clone()),
            store,
            clock,
            dir,
        }
    }

    pub fn now(&self) -> u64 {
        self.clock.load(Ordering::SeqCst)
    }

    pub fn set_now(&self, now: u64) {
        self.clock.store(now, Ordering::SeqCst);
    }

    pub fn store_key(&self) -> String {
        self.store.key.multikey()
    }

    pub async fn send(
        &self,
        method: &str,
        path: &str,
        body: Option<Vec<u8>>,
        signed: Option<String>,
    ) -> Reply {
        let mut request = Request::builder().method(method).uri(path);
        if body.is_some() {
            request = request.header("content-type", "application/json");
        }
        if let Some(header) = signed {
            request = request.header(REQUEST_HEADER, header);
        }
        let request = request
            .body(body.map(Body::from).unwrap_or_else(Body::empty))
            .unwrap();
        let response = self.router.clone().oneshot(request).await.unwrap();
        let status = response.status();
        let headers = response.headers().clone();
        let raw = response
            .into_body()
            .collect()
            .await
            .unwrap()
            .to_bytes()
            .to_vec();
        let body = if raw.is_empty() {
            Value::Null
        } else {
            serde_json::from_slice(&raw)
                .unwrap_or(Value::String(String::from_utf8_lossy(&raw).into()))
        };
        Reply {
            status,
            body,
            raw,
            headers,
        }
    }

    pub async fn get(&self, path: &str) -> Reply {
        self.send("GET", path, None, None).await
    }

    pub async fn post(&self, path: &str, body: &Value) -> Reply {
        self.send("POST", path, Some(serde_json::to_vec(body).unwrap()), None)
            .await
    }

    /// `POST /v1/logs/{scid}/events` with `events`.
    pub async fn append(&self, scid: &str, events: &[&Event]) -> Reply {
        let events: Vec<Value> = events
            .iter()
            .map(|e| serde_json::to_value(e).unwrap())
            .collect();
        self.post(
            &format!("/v1/logs/{scid}/events"),
            &json!({ "events": events }),
        )
        .await
    }

    /// Appends the person's #0 and checks the `200`.
    pub async fn publish(&self, person: &Person) {
        let reply = self.append(&person.scid(), &[&person.events[0]]).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    }

    /// Appends `event` and, on `200`, pushes it to the person's copy.
    pub async fn append_push(&self, person: &mut Person, event: Event) -> Reply {
        let reply = self.append(&person.scid(), &[&event]).await;
        if reply.status == StatusCode::OK {
            person.push(event);
        }
        reply
    }

    pub async fn open_offer(&self, offer: &SignedOffer) -> Reply {
        self.post(
            &format!("/v1/offers/{}", offer.offer.scid),
            &serde_json::to_value(offer).unwrap(),
        )
        .await
    }

    pub async fn claim(&self, scid: &str, claim: &Claim) -> Reply {
        self.post(
            &format!("/v1/offers/{scid}/{}/claim", claim.offer_id),
            &serde_json::to_value(claim).unwrap(),
        )
        .await
    }

    /// Opens an offer of `kind`, has `device` claim it, and answers the offer and the claim as
    /// the holder reads it.
    pub async fn claimed_offer(
        &self,
        person: &mut Person,
        device: &Device,
        kind: OfferKind,
        bot: Option<ad4m_identity_core::BotScope>,
    ) -> (SignedOffer, ClaimView) {
        let (offer, s) = person.offer(kind, None, bot, self.now() + 300);
        let reply = self.open_offer(&offer).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
        let claim = device.claim(&person.scid(), &offer.offer.offer_id, s, true);
        let reply = self.claim(&person.scid(), &claim).await;
        assert_eq!(reply.status, StatusCode::CREATED, "{}", reply.body);
        let view = device.view(&claim, self.now());
        (offer, view)
    }

    /// Links `device` to `person` through an offer, a claim and the `add` that spends it.
    /// Answers the new key id.
    pub async fn link(
        &self,
        person: &mut Person,
        device: &Device,
        kind: OfferKind,
        bot: Option<ad4m_identity_core::BotScope>,
        expires: Option<u64>,
    ) -> String {
        let id = person.chain.state().next_device_id();
        let (offer, view) = self.claimed_offer(person, device, kind, bot).await;
        let add = person.add_claimed(&offer.offer, &view, expires);
        let reply = self.append_push(person, add).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
        id
    }

    /// Every text cell of every table, for scans of what the store keeps.
    pub fn all_cells(&self) -> Vec<String> {
        let conn = self.store.db.lock().unwrap();
        let tables: Vec<String> = {
            let mut stmt = conn
                .prepare("SELECT name FROM sqlite_master WHERE type = 'table'")
                .unwrap();
            stmt.query_map([], |r| r.get(0))
                .unwrap()
                .collect::<Result<_, _>>()
                .unwrap()
        };
        let mut cells = Vec::new();
        for table in tables {
            let mut stmt = conn.prepare(&format!("SELECT * FROM \"{table}\"")).unwrap();
            let columns = stmt.column_count();
            let mut rows = stmt.query([]).unwrap();
            while let Some(row) = rows.next().unwrap() {
                for i in 0..columns {
                    if let Ok(text) = row.get::<_, String>(i) {
                        cells.push(text);
                    }
                }
            }
        }
        cells
    }

    /// Rows of `table`.
    pub fn count(&self, table: &str) -> i64 {
        let conn = self.store.db.lock().unwrap();
        conn.query_row(&format!("SELECT COUNT(*) FROM \"{table}\""), [], |r| {
            r.get(0)
        })
        .unwrap()
    }
}
