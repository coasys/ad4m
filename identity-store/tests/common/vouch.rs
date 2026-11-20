//! A vouching service on 127.0.0.1 that implements exactly the redeem contract the store calls
//! (`POST /api/vouch/redeem {token, did}` with the store's bearer credential), until the
//! platform's worker can run in the tests:
//!
//! - a token it was given ([`VouchServer::give`]) for an email address answers `200` and a note
//!   for `did`, signed by the test vouching key in the `coasys:vouch` domain;
//! - the same token for the same `did` again answers the same note;
//! - the same token for another `did`, a token it never gave, and a spent one
//!   ([`VouchServer::spend`]) answer `410`;
//! - a token for an address that already vouched another identity answers `409`;
//! - another credential answers `401`; [`VouchServer::fail`] makes every redeem a `500`.
#![allow(dead_code)]

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::vouch::METHOD_EMAIL;
use ad4m_identity_core::{sign_note, VouchNote};
use axum::extract::State;
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use ed25519_dalek::SigningKey;
use serde::Deserialize;
use serde_json::json;

/// The credential the test store sends.
pub const CREDENTIAL: &str = "test-store-credential";

/// The test vouching key.
pub fn vouching_key() -> SigningKey {
    SigningKey::from_bytes(&ad4m_hash_chain::sha256(b"test vouching key"))
}

/// Its multikey, as `VOUCH_KEYS` lists it.
pub fn vouching_multikey() -> String {
    vouching_key().key()
}

#[derive(Default)]
struct Ledger {
    /// token -> the email address it checked.
    given: HashMap<String, String>,
    /// token -> the note it was redeemed for.
    redeemed: HashMap<String, VouchNote>,
    /// email address -> the identity it vouched.
    vouched: HashMap<String, String>,
    spent: Vec<String>,
    failing: bool,
    redeems: u32,
    /// While set, a redeem waits (after it counts) until [`VouchServer::release`].
    gate: Option<Arc<tokio::sync::Semaphore>>,
}

struct Shared {
    ledger: Mutex<Ledger>,
    key: SigningKey,
    time: u64,
}

/// The running service.
pub struct VouchServer {
    pub addr: SocketAddr,
    shared: Arc<Shared>,
}

#[derive(Deserialize)]
struct Redeem {
    token: String,
    did: String,
}

impl VouchServer {
    /// Starts the service; its notes carry `time`.
    pub async fn start(time: u64) -> VouchServer {
        let shared = Arc::new(Shared {
            ledger: Mutex::new(Ledger::default()),
            key: vouching_key(),
            time,
        });
        let app = Router::new()
            .route("/api/vouch/redeem", post(redeem))
            .route("/api/vouch/key", get(key))
            .with_state(shared.clone());
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
        VouchServer { addr, shared }
    }

    /// The `VOUCH_URL` of the service.
    pub fn url(&self) -> String {
        format!("http://{}", self.addr)
    }

    /// Gives `token` for a check of `email`.
    pub fn give(&self, token: &str, email: &str) {
        let mut ledger = self.shared.ledger.lock().unwrap();
        ledger.given.insert(token.into(), email.into());
    }

    /// Spends `token` (expired, or used at the platform): its redeem answers `410`.
    pub fn spend(&self, token: &str) {
        self.shared.ledger.lock().unwrap().spent.push(token.into());
    }

    /// Makes every redeem answer `500`.
    pub fn fail(&self, failing: bool) {
        self.shared.ledger.lock().unwrap().failing = failing;
    }

    /// Makes every later redeem wait, once it reached the service, until [`Self::release`].
    pub fn hold(&self) {
        self.shared.ledger.lock().unwrap().gate = Some(Arc::new(tokio::sync::Semaphore::new(0)));
    }

    /// Lets the redeems waiting at [`Self::hold`] go, and every later one.
    pub fn release(&self) {
        if let Some(gate) = self.shared.ledger.lock().unwrap().gate.take() {
            gate.add_permits(1 << 20);
        }
    }

    /// How many redeems reached the service.
    pub fn redeems(&self) -> u32 {
        self.shared.ledger.lock().unwrap().redeems
    }
}

async fn key(State(shared): State<Arc<Shared>>) -> Response {
    let multikey = shared.key.key();
    Json(json!({ "keyId": format!("did:key:{multikey}"), "publicKeyMultibase": multikey }))
        .into_response()
}

async fn redeem(
    State(shared): State<Arc<Shared>>,
    headers: HeaderMap,
    Json(req): Json<Redeem>,
) -> Response {
    let bearer = format!("Bearer {CREDENTIAL}");
    if headers.get("authorization").and_then(|v| v.to_str().ok()) != Some(bearer.as_str()) {
        return StatusCode::UNAUTHORIZED.into_response();
    }
    let gate = {
        let mut ledger = shared.ledger.lock().unwrap();
        ledger.redeems += 1;
        ledger.gate.clone()
    };
    if let Some(gate) = gate {
        gate.acquire().await.unwrap().forget();
    }
    let mut ledger = shared.ledger.lock().unwrap();
    if ledger.failing {
        return StatusCode::INTERNAL_SERVER_ERROR.into_response();
    }
    if ledger.spent.contains(&req.token) {
        return StatusCode::GONE.into_response();
    }
    if let Some(note) = ledger.redeemed.get(&req.token) {
        return if note.did == req.did {
            Json(note.clone()).into_response()
        } else {
            StatusCode::GONE.into_response()
        };
    }
    let Some(email) = ledger.given.get(&req.token).cloned() else {
        return StatusCode::GONE.into_response();
    };
    if ledger
        .vouched
        .get(&email)
        .is_some_and(|did| *did != req.did)
    {
        return StatusCode::CONFLICT.into_response();
    }
    let note = sign_note(&req.did, METHOD_EMAIL, shared.time, &shared.key).unwrap();
    ledger.vouched.insert(email, req.did);
    ledger.redeemed.insert(req.token, note.clone());
    Json(note).into_response()
}
