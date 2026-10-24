//! The HTTP routes (AR-01 4.5.3, AR-02 4.3.6), base path `/v1`, JSON bodies.

use std::collections::HashMap;
use std::sync::Arc;

use ad4m_identity_core::link::{Claim, SignedOffer};
use ad4m_ldk::identity::{AgentRecord, AppendRequest, PowParams};
use axum::body::Bytes;
use axum::extract::rejection::BytesRejection;
use axum::extract::{DefaultBodyLimit, Path, Query, State};
use axum::http::{header, HeaderMap, HeaderValue, Method, StatusCode, Uri};
use axum::response::{IntoResponse, Response};
use axum::routing::{delete, get, post};
use axum::{Json, Router};
use rusqlite::params;
use serde::de::DeserializeOwned;
use serde_json::{json, Value};
use tower_http::cors::{AllowOrigin, CorsLayer};

use crate::db::{from_sql, to_sql};
use crate::error::ApiError;
use crate::offers::ClaimAnswer;
use crate::request::{Incoming, Signer, REQUEST_HEADER};
use crate::{Store, MAX_WAIT_S};

/// The largest body a route takes.
pub const MAX_BODY: usize = 64 * 1024;
/// Feed items per page.
pub const FEED_PAGE: u32 = 256;

type Shared = State<Arc<Store>>;
type Answer = Result<Response, ApiError>;

/// The store's routes on `store`.
pub fn router(store: Arc<Store>) -> Router {
    let origins: Vec<HeaderValue> = store
        .settings
        .cors_origins
        .iter()
        .filter_map(|o| HeaderValue::from_str(o).ok())
        .collect();
    let cors = CorsLayer::new()
        .allow_origin(AllowOrigin::list(origins))
        .allow_methods([Method::GET, Method::POST, Method::PUT, Method::DELETE])
        .allow_headers([
            header::CONTENT_TYPE,
            header::HeaderName::from_static(REQUEST_HEADER),
        ]);
    Router::new()
        .route("/v1/health", get(health))
        .route("/v1/pow", get(pow_params))
        .route("/v1/logs/{scid}", get(log_page))
        .route("/v1/logs/{scid}/events", post(append))
        .route("/v1/heads/{scid}", get(head))
        .route("/v1/keys/{multikey}", get(key_owner))
        .route("/v1/notes/{scid}", post(attach_note))
        .route("/v1/agents/{did}", get(agent).put(put_agent))
        .route("/v1/offers/{scid}", post(open_offer).get(list_offers))
        .route("/v1/offers/{scid}/{offer_id}", delete(cancel_offer))
        .route("/v1/offers/{scid}/{offer_id}/claim", post(claim_offer))
        .route("/v1/offers/{scid}/{offer_id}/state", get(offer_state))
        .route("/v1/feed", get(feed))
        .layer(DefaultBodyLimit::max(MAX_BODY))
        .layer(cors)
        .with_state(store)
}

fn body(bytes: Result<Bytes, BytesRejection>) -> Result<Bytes, ApiError> {
    bytes.map_err(|e| {
        if e.status() == StatusCode::PAYLOAD_TOO_LARGE {
            ApiError::too_large()
        } else {
            ApiError::invalid("malformed", e.body_text())
        }
    })
}

fn json_body<T: DeserializeOwned>(bytes: &[u8]) -> Result<T, ApiError> {
    serde_json::from_slice(bytes).map_err(|e| ApiError::invalid("malformed", e.to_string()))
}

fn number(query: &HashMap<String, String>, name: &str) -> Result<Option<u64>, ApiError> {
    query
        .get(name)
        .map(|v| {
            v.parse::<u64>()
                .map_err(|_| ApiError::invalid("malformed", format!("{name} is a whole number")))
        })
        .transpose()
}

fn wait_of(query: &HashMap<String, String>) -> Result<u64, ApiError> {
    let wait = number(query, "wait")?.unwrap_or(0);
    if wait > MAX_WAIT_S {
        return Err(ApiError::invalid(
            "malformed",
            format!("wait is 0 to {MAX_WAIT_S} seconds"),
        ));
    }
    Ok(wait)
}

fn incoming<'a>(
    headers: &'a HeaderMap,
    method: &'a Method,
    uri: &'a Uri,
    body: &'a [u8],
) -> Incoming<'a> {
    Incoming {
        header: headers.get(REQUEST_HEADER).and_then(|v| v.to_str().ok()),
        method: method.as_str(),
        path: uri.path(),
        body,
    }
}

fn ok(value: impl serde::Serialize) -> Answer {
    Ok(Json(value).into_response())
}

async fn health() -> Answer {
    ok(json!({ "ok": true }))
}

async fn pow_params(State(store): Shared) -> Answer {
    ok(PowParams {
        bits: store.settings.pow_bits,
        previous_bits: store.settings.pow_bits,
        changed_at: store.started_at(),
    })
}

async fn log_page(
    State(store): Shared,
    Path(scid): Path<String>,
    Query(q): Query<HashMap<String, String>>,
) -> Answer {
    let from = number(&q, "from")?.unwrap_or(0);
    match store.log_page(&scid, from)? {
        Some(page) => ok(page),
        None => Err(ApiError::not_found(format!("no log {scid}"))),
    }
}

async fn append(
    State(store): Shared,
    Path(scid): Path<String>,
    bytes: Result<Bytes, BytesRejection>,
) -> Answer {
    let bytes = body(bytes)?;
    let req: AppendRequest = json_body(&bytes)?;
    let head = store.append(&scid, &req)?;
    ok(json!({ "head": head }))
}

async fn head(State(store): Shared, Path(scid): Path<String>) -> Answer {
    match store.head(&scid)? {
        Some(head) => ok(head),
        None => Err(ApiError::not_found(format!("no log {scid}"))),
    }
}

async fn key_owner(
    State(store): Shared,
    Path(multikey): Path<String>,
    Query(q): Query<HashMap<String, String>>,
) -> Answer {
    let wait = wait_of(&q)?;
    let found = store.wait_for(wait, || store.key_entry(&multikey)).await?;
    match found {
        Some(entry) => ok(entry),
        None => Err(ApiError::not_found("the key is in no log")),
    }
}

/// `POST /v1/notes/{scid}`. The signed request is checked as AR-01 4.6 route 2 says (a non-bot
/// device key valid at the head, or the current master key); then the store would redeem the
/// token with the vouching service. Slice 1 configures none, so a valid request gets
/// `503 {error:"vouch-unavailable"}` and nothing is stored.
async fn attach_note(
    State(store): Shared,
    Path(scid): Path<String>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
    bytes: Result<Bytes, BytesRejection>,
) -> Answer {
    let bytes = body(bytes)?;
    let did = ad4m_identity_core::did_of(&scid);
    store.verify_request(
        &incoming(&headers, &method, &uri, &bytes),
        Some(&did),
        Signer::AnyKey,
    )?;
    let note: Value = json_body(&bytes)?;
    let well_formed = note
        .as_object()
        .is_some_and(|o| o.len() == 1 && o.get("token").is_some_and(Value::is_string));
    if !well_formed {
        return Err(ApiError::invalid("malformed", "the body is {token}"));
    }
    Err(ApiError::unavailable("vouch-unavailable"))
}

async fn agent(State(store): Shared, Path(did): Path<String>) -> Answer {
    match store.agent(&did)? {
        Some(record) => ok(record),
        None => Err(ApiError::not_found("no record")),
    }
}

async fn put_agent(
    State(store): Shared,
    Path(did): Path<String>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
    bytes: Result<Bytes, BytesRejection>,
) -> Answer {
    let bytes = body(bytes)?;
    let record: AgentRecord = json_body(&bytes)?;
    store.put_agent(&did, &record, &incoming(&headers, &method, &uri, &bytes))?;
    ok(json!({}))
}

async fn open_offer(
    State(store): Shared,
    Path(scid): Path<String>,
    bytes: Result<Bytes, BytesRejection>,
) -> Answer {
    let bytes = body(bytes)?;
    let signed: SignedOffer = json_body(&bytes)?;
    let opened = store.open_offer(&scid, &signed)?;
    Ok((StatusCode::CREATED, Json(opened)).into_response())
}

async fn list_offers(
    State(store): Shared,
    Path(scid): Path<String>,
    Query(q): Query<HashMap<String, String>>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
) -> Answer {
    let wait = wait_of(&q)?;
    let did = ad4m_identity_core::did_of(&scid);
    store.verify_request(
        &incoming(&headers, &method, &uri, &[]),
        Some(&did),
        Signer::Master,
    )?;
    let first = store.list_offers(&scid)?;
    if wait == 0 {
        return ok(first);
    }
    let changed = store
        .wait_for(wait, || {
            let now = store.list_offers(&scid)?;
            Ok((now != first).then_some(now))
        })
        .await?;
    match changed {
        Some(list) => ok(list),
        None => ok(store.list_offers(&scid)?),
    }
}

async fn cancel_offer(
    State(store): Shared,
    Path((scid, offer_id)): Path<(String, String)>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
) -> Answer {
    let did = ad4m_identity_core::did_of(&scid);
    store.verify_request(
        &incoming(&headers, &method, &uri, &[]),
        Some(&did),
        Signer::Master,
    )?;
    store.cancel_offer(&scid, &offer_id)?;
    Ok(StatusCode::NO_CONTENT.into_response())
}

async fn claim_offer(
    State(store): Shared,
    Path((scid, offer_id)): Path<(String, String)>,
    bytes: Result<Bytes, BytesRejection>,
) -> Answer {
    let bytes = body(bytes)?;
    let claim: Claim = json_body(&bytes)?;
    match store.claim_offer(&scid, &offer_id, &claim)? {
        ClaimAnswer::Claimed(receipt) => Ok((StatusCode::CREATED, Json(receipt)).into_response()),
        ClaimAnswer::Repeat(receipt) => ok(receipt),
    }
}

async fn offer_state(
    State(store): Shared,
    Path((scid, offer_id)): Path<(String, String)>,
    Query(q): Query<HashMap<String, String>>,
) -> Answer {
    let wait = wait_of(&q)?;
    let key = q
        .get("key")
        .cloned()
        .ok_or_else(|| ApiError::invalid("malformed", "key names the claiming device key"))?;
    let first = store
        .offer_progress(&scid, &offer_id, &key)?
        .ok_or_else(|| ApiError::not_found("no outcome for this key"))?;
    let live = first["state"] == "claimed" || first["state"] == "open";
    if wait == 0 || !live {
        return ok(first);
    }
    let changed = store
        .wait_for(wait, || {
            let now = store.offer_progress(&scid, &offer_id, &key)?;
            Ok(now.filter(|n| *n != first))
        })
        .await?;
    ok(changed.unwrap_or(first))
}

/// `a == b` in a time that depends on the lengths only, so the feed token cannot be guessed
/// byte by byte from response times.
fn same_secret(a: &str, b: &str) -> bool {
    a.len() == b.len()
        && a.bytes()
            .zip(b.bytes())
            .fold(0u8, |diff, (x, y)| diff | (x ^ y))
            == 0
}

async fn feed(
    State(store): Shared,
    headers: HeaderMap,
    Query(q): Query<HashMap<String, String>>,
) -> Answer {
    let token = headers
        .get(header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.strip_prefix("Bearer "));
    match (&store.settings.feed_token, token) {
        (Some(want), Some(got)) if same_secret(want, got) => {}
        _ => return Err(ApiError::unauthorized()),
    }
    let after = number(&q, "after")?.unwrap_or(0);
    let conn = store.db.lock()?;
    let mut stmt = conn.prepare_cached(
        "SELECT cursor, scid, seq, hash FROM feed WHERE cursor > ?1 ORDER BY cursor LIMIT ?2",
    )?;
    let items = stmt
        .query_map(params![to_sql(after)?, FEED_PAGE], |r| {
            Ok(json!({
                "cursor": from_sql(r.get(0)?),
                "scid": r.get::<_, String>(1)?,
                "seq": from_sql(r.get(2)?),
                "hash": r.get::<_, String>(3)?,
            }))
        })?
        .collect::<Result<Vec<Value>, _>>()?;
    let next = items
        .last()
        .and_then(|i| i["cursor"].as_u64())
        .unwrap_or(after);
    ok(json!({ "items": items, "next": next }))
}

#[cfg(test)]
mod tests {
    use super::same_secret;

    #[test]
    fn the_feed_token_matches_only_itself() {
        assert!(same_secret("tok-123", "tok-123"));
        assert!(!same_secret("tok-123", "tok-124"));
        assert!(!same_secret("tok-123", "tok-12"));
        assert!(!same_secret("", "x"));
    }
}
