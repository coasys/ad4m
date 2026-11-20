use super::*;
use axum::extract::State;
use axum::http::{HeaderMap, StatusCode};
use axum::routing::{delete, get, post};
use axum::{Json, Router};
use ed25519_dalek::SigningKey;
use std::sync::{Arc, Mutex};

const SCID: &str = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";
const INDEX_DID: &str = "did:key:z6MkindexindexindexindexindexindexindexindexXX";
const CHALLENGE: &str = "Zm9vYmFyZm9vYmFyZm9vYmFyZm9vYmFyZm9vYmFyZm9vYmE";

fn signer() -> SigningKey {
    SigningKey::from_bytes(&[7; 32])
}

/// A vector produced by the platform's own code: `worker/src/index/encoding.ts` of
/// coasys/platform PR #17 (`signingBytes('coasys:session', statement)`, `base58Encode`,
/// `publicKeyToMultikey`), run under Node 26 with the Ed25519 seed `[7; 32]` (node:crypto
/// signs; Ed25519 is deterministic). `KEY` is the multikey it derives, `BYTES` the exact JCS
/// text it signs and `SIG` the `z`-prefixed base58btc signature.
const KEY: &str = "z6MkvDqGT54cXesYGvABpF1UapVNwjCqRcafi4Px6Thv5T3Z";
const BYTES: &str = r#"{"challenge":"Zm9vYmFyZm9vYmFyZm9vYmFyZm9vYmFyZm9vYmFyZm9vYmE","client":{"kind":"native"},"domain":"coasys:session","executor":"did:key:z6MkindexindexindexindexindexindexindexindexXX","issuedAt":1760500000,"key":"z6MkvDqGT54cXesYGvABpF1UapVNwjCqRcafi4Px6Thv5T3Z","purpose":"login","scid":"did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG","v":1}"#;
const SIG: &str =
    "z2aPbjkwLufyaeYugai2xu6EYxLqWWGgKMK7Kpf152UvUSLGGsP7M9Yy9TQyjxYRvsKnne6njZeeDPZ1ZH6xmbZHr";

#[test]
fn the_statement_encodes_as_the_platforms_signin_does() {
    assert_eq!(Signer::key(&signer()), KEY);
    let signed =
        sign_index_statement(&signer(), SCID, INDEX_DID, CHALLENGE, 1_760_500_000).unwrap();
    let bytes = signing_bytes(SESSION_DOMAIN, &signed.statement).unwrap();
    assert_eq!(String::from_utf8(bytes).unwrap(), BYTES);
    assert_eq!(signed.sig, SIG);
    // The body the index parses: only its eight fields, `client` as `{kind: native}`.
    let body = serde_json::to_value(&signed.statement).unwrap();
    let mut keys: Vec<_> = body.as_object().unwrap().keys().cloned().collect();
    keys.sort();
    assert_eq!(
        keys,
        [
            "challenge",
            "client",
            "executor",
            "issuedAt",
            "key",
            "purpose",
            "scid",
            "v"
        ]
    );
    assert_eq!(body["client"], json!({ "kind": "native" }));
}

// ── a local index ──────────────────────────────────────────────────────────

#[derive(Default)]
struct Fake {
    /// The audience `/challenge` names (`/info` always names `INDEX_DID`).
    challenge_audience: Mutex<Option<String>>,
    sessions_opened: Mutex<u32>,
    listing: Mutex<Option<Value>>,
    /// Answer 401 `unauthorized` to the next authenticated call.
    expire_next: Mutex<bool>,
    /// Refuse sign-in as `not-allowed`.
    refuse_sign_in: Mutex<bool>,
}

type Shared = Arc<Fake>;

fn error(status: u16, code: &str) -> (StatusCode, Json<Value>) {
    (
        StatusCode::from_u16(status).unwrap(),
        Json(json!({ "error": { "code": code, "message": format!("{code} happened") } })),
    )
}

async fn info() -> Json<Value> {
    Json(json!({ "indexDid": INDEX_DID, "version": "1", "closedBetaFree": true }))
}

async fn challenge(State(f): State<Shared>, Json(b): Json<Value>) -> Json<Value> {
    assert!(b["key"].as_str().unwrap().starts_with("z6Mk"));
    let audience = f
        .challenge_audience
        .lock()
        .unwrap()
        .clone()
        .unwrap_or_else(|| INDEX_DID.into());
    Json(json!({ "challenge": CHALLENGE, "indexDid": audience, "expiresAt": 4_000_000_000u64 }))
}

async fn session(
    State(f): State<Shared>,
    Json(b): Json<Value>,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    if *f.refuse_sign_in.lock().unwrap() {
        return Err(error(403, "not-allowed"));
    }
    // What signin.ts checks: the shape, the audience, the challenge, the signature.
    let proof: SignedIndexStatement =
        serde_json::from_value(b["proof"].clone()).map_err(|_| error(400, "malformed"))?;
    if proof.statement.executor != INDEX_DID {
        return Err(error(401, "wrong-audience"));
    }
    if proof.statement.challenge != CHALLENGE {
        return Err(error(401, "challenge"));
    }
    let sig =
        ad4m_hash_chain::decode_proof_value(&proof.sig).ok_or_else(|| error(400, "malformed"))?;
    if !ad4m_identity_core::verify_signed(
        SESSION_DOMAIN,
        &proof.statement,
        &proof.statement.key,
        &sig,
    ) {
        return Err(error(401, "bad-signature"));
    }
    let n = {
        let mut n = f.sessions_opened.lock().unwrap();
        *n += 1;
        *n
    };
    Ok(Json(json!({
        "token": format!("token-{n}"),
        "expiresAt": 4_000_000_000u64,
        "scid": proof.statement.scid,
        "runByCoasys": false
    })))
}

fn authorise(f: &Fake, h: &HeaderMap) -> Result<u32, (StatusCode, Json<Value>)> {
    let opened = *f.sessions_opened.lock().unwrap();
    let want = format!("Bearer token-{opened}");
    let got = h.get("authorization").and_then(|v| v.to_str().ok());
    let mut expire = f.expire_next.lock().unwrap();
    if got != Some(want.as_str()) || *expire {
        *expire = false;
        return Err(error(401, "unauthorized"));
    }
    Ok(opened)
}

fn own(draft: &Value) -> Value {
    json!({
        "listingId": "l1", "name": draft["name"], "description": draft["description"],
        "location": draft["location"], "url": draft["url"],
        "executorDid": format!("did:key:{KEY}"), "operatorDid": SCID, "runByCoasys": false,
        "aiModels": draft["aiModels"], "rates": draft["rates"],
        "signup": "anyone", "admitsGuests": true, "freeAccess": true, "full": false,
        "approval": { "kind": "allow-list", "since": "2026-01-01" },
        "lastSeenAt": "2026-11-20T00:00:00.000Z", "version": "1", "state": "listed"
    })
}

async fn me(
    State(f): State<Shared>,
    h: HeaderMap,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    authorise(&f, &h)?;
    let listing = f.listing.lock().unwrap().clone().unwrap_or(Value::Null);
    Ok(Json(
        json!({ "scid": SCID, "allowed": true, "listing": listing, "proof": null }),
    ))
}

async fn put_listing(
    State(f): State<Shared>,
    h: HeaderMap,
    Json(draft): Json<Value>,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    authorise(&f, &h)?;
    let view = own(&draft);
    *f.listing.lock().unwrap() = Some(view.clone());
    Ok(Json(view))
}

async fn delete_listing(
    State(f): State<Shared>,
    h: HeaderMap,
) -> Result<Json<Value>, (StatusCode, Json<Value>)> {
    authorise(&f, &h)?;
    let mut listing = f.listing.lock().unwrap();
    if let Some(l) = listing.as_mut() {
        l["state"] = json!("withdrawn");
    }
    Ok(Json(json!({ "state": "withdrawn" })))
}

async fn start() -> (IndexClient, Shared) {
    let fake: Shared = Arc::default();
    let app = Router::new()
        .route("/v1/index/info", get(info))
        .route("/v1/index/challenge", post(challenge))
        .route("/v1/index/session", post(session))
        .route("/v1/index/me", get(me))
        .route(
            "/v1/index/listing",
            axum::routing::put(put_listing).merge(delete(delete_listing)),
        )
        .with_state(fake.clone());
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = listener.local_addr().unwrap().port();
    tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
    let client = IndexClient::new(&format!("http://127.0.0.1:{port}/v1/index/")).unwrap();
    (client, fake)
}

fn draft() -> ListingDraft {
    ListingDraft {
        text: ListingText {
            name: "Home".into(),
            description: "A host".into(),
            location: "Berlin".into(),
            url: "https://host.example.org".into(),
        },
        ai_models: vec![ListedModel {
            name: "llama".into(),
            kind: "llm".into(),
        }],
        rates: vec![PricingItem {
            description: "link write".into(),
            price_in_hot: 0.5,
            unit: "credits".into(),
        }],
    }
}

#[tokio::test]
async fn signs_in_publishes_reads_and_withdraws() {
    let (client, fake) = start().await;
    let session = client
        .sign_in(&signer(), SCID, 1_760_500_000)
        .await
        .unwrap();
    assert_eq!(session.scid, SCID);
    assert!(session.is_live(1_760_500_000));
    assert!(!session.is_live(4_000_000_000));

    let me = client.me(&session).await.unwrap();
    assert!(me.allowed && me.listing.is_none());

    let listed = client.publish(&session, &draft()).await.unwrap();
    assert_eq!(listed.state, "listed");
    assert_eq!(listed.name, "Home");
    assert_eq!(listed.ai_models, draft().ai_models);
    assert_eq!(listed.rates, draft().rates);

    // An update is the same call; `me` reads it back.
    let mut changed = draft();
    changed.text.name = "Home 2".into();
    client.publish(&session, &changed).await.unwrap();
    assert_eq!(
        client.me(&session).await.unwrap().listing.unwrap().name,
        "Home 2"
    );

    client.withdraw(&session).await.unwrap();
    assert_eq!(
        client.me(&session).await.unwrap().listing.unwrap().state,
        "withdrawn"
    );
    assert_eq!(*fake.sessions_opened.lock().unwrap(), 1);
}

#[tokio::test]
async fn nothing_is_signed_when_the_challenge_names_another_audience() {
    let (client, fake) = start().await;
    *fake.challenge_audience.lock().unwrap() = Some("did:key:z6Mkother".into());
    let err = client.sign_in(&signer(), SCID, 1).await.unwrap_err();
    assert!(matches!(err, IndexError::AudienceMismatch { .. }), "{err}");
    assert_eq!(*fake.sessions_opened.lock().unwrap(), 0);
}

#[tokio::test]
async fn a_refusal_keeps_the_indexs_status_and_code() {
    let (client, fake) = start().await;
    *fake.refuse_sign_in.lock().unwrap() = true;
    let err = client.sign_in(&signer(), SCID, 1).await.unwrap_err();
    match err {
        IndexError::Refused { status, code, .. } => {
            assert_eq!((status, code.as_str()), (403, "not-allowed"))
        }
        other => panic!("{other}"),
    }
}

#[tokio::test]
async fn an_ended_session_is_reported_as_unauthorized() {
    let (client, fake) = start().await;
    let session = client.sign_in(&signer(), SCID, 1).await.unwrap();
    *fake.expire_next.lock().unwrap() = true;
    let err = client.me(&session).await.unwrap_err();
    assert!(err.is_unauthorized(), "{err}");
}

#[tokio::test]
async fn an_unreachable_index_is_a_transport_error() {
    let client = IndexClient::new("http://127.0.0.1:1/v1/index").unwrap();
    let err = client.info().await.unwrap_err();
    assert!(matches!(err, IndexError::Transport(_)), "{err}");
}

#[test]
fn only_https_or_loopback_http_urls_are_accepted() {
    assert!(IndexClient::new("https://api.coasys.org/v1/index").is_ok());
    assert!(IndexClient::new("http://127.0.0.1:9/v1/index").is_ok());
    assert!(IndexClient::new("http://localhost:9/v1/index").is_ok());
    assert!(IndexClient::new("http://index.example.org/v1/index").is_err());
    assert!(IndexClient::new("not a url").is_err());
    assert!(IndexClient::new("").is_err());
}

fn local(state: &str, published_at: u64, hash: &str) -> LocalListing {
    LocalListing {
        listing_id: "l".into(),
        draft: ListingText {
            name: "n".into(),
            description: String::new(),
            location: "l".into(),
            url: "https://h.example".into(),
        },
        state: state.into(),
        reason: None,
        published_at,
        rates_hash: hash.into(),
    }
}

#[test]
fn the_rates_hash_ignores_order_and_sees_every_change() {
    let model = |n: &str| ListedModel {
        name: n.into(),
        kind: "llm".into(),
    };
    let rate = |d: &str, p: f64| PricingItem {
        description: d.into(),
        price_in_hot: p,
        unit: "credits".into(),
    };
    let base = rates_hash(
        &[model("a"), model("b")],
        &[rate("link write", 0.25), rate("a", 1.0)],
    );
    assert_eq!(base.len(), 64);
    assert_eq!(
        base,
        rates_hash(
            &[model("b"), model("a")],
            &[rate("a", 1.0), rate("link write", 0.25)]
        )
    );
    assert_ne!(
        base,
        rates_hash(&[model("a")], &[rate("link write", 0.25), rate("a", 1.0)])
    );
    assert_ne!(
        base,
        rates_hash(
            &[model("a"), model("b")],
            &[rate("link write", 0.5), rate("a", 1.0)]
        )
    );
}

#[test]
fn a_change_is_published_after_sixty_quiet_seconds_and_another_change_restarts_the_wait() {
    let listing = local("listed", 1_000, "old");
    let mut pending = None;
    // Nothing changed: nothing to do, nothing pending.
    assert!(!republish_due(&listing, "old", &mut pending, 1_010));
    assert_eq!(pending, None);
    // Seen at 1_020.
    assert!(!republish_due(&listing, "new", &mut pending, 1_020));
    assert!(!republish_due(&listing, "new", &mut pending, 1_079));
    // Changed again at 1_070: the quiet period starts over from there.
    assert!(!republish_due(&listing, "newer", &mut pending, 1_070));
    assert!(!republish_due(&listing, "newer", &mut pending, 1_129));
    assert!(republish_due(&listing, "newer", &mut pending, 1_130));
    // Changed back to what was sent before the wait ended: nothing to publish.
    let mut pending = None;
    assert!(!republish_due(&listing, "new", &mut pending, 1_020));
    assert!(!republish_due(&listing, "old", &mut pending, 1_050));
    assert_eq!(pending, None);
}

#[test]
fn a_listing_is_published_daily_while_it_is_on_the_index() {
    let mut pending = None;
    let day = REPUBLISH_DAILY_S;
    assert!(!republish_due(
        &local("listed", 0, "h"),
        "h",
        &mut pending,
        day - 1
    ));
    assert!(republish_due(
        &local("listed", 0, "h"),
        "h",
        &mut pending,
        day
    ));
    assert!(republish_due(
        &local("unreachable", 0, "h"),
        "h",
        &mut pending,
        day
    ));
    // Withdrawn or delisted: the operator publishes again; nothing does it for them.
    for state in ["withdrawn", "delisted"] {
        assert!(!republish_due(
            &local(state, 0, "h"),
            "x",
            &mut pending,
            3 * day
        ));
        assert_eq!(pending, None);
    }
}
