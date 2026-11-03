//! Signed requests: header `X-Coasys-Request` = base64url(JCS(SignedRequest)).
//!
//! A request names the store it is for: `store` is this store key's `did:key`, as a signed head
//! names its store (`403 store` otherwise), so a request for one store cannot be replayed to
//! another that holds the same log. A pre-join inbox read (`X-Coasys-Inbox`) names it the same
//! way and passes the same check. A client knows the key from its configuration (the
//! executor's `identity.storeKeys`, the `store-key` line `serve` prints); the `store` of a
//! signed head checked against those keys names it too.
//!
//! A request passes when its key is valid at the store's head by the store's clock and carries
//! no bot mark; `kelSeq`/`kelHash` name a position the store holds; `expires` lies ahead, at
//! most 5 minutes; `method`, `path` and `bodyHash` match the request; the nonce is new in the
//! last 10 minutes; and the key's `coasys:store-request` signature verifies. Routes that need
//! the master key (the offer routes) also require `keyId` to be the current master's.

use ad4m_hash_chain::{decode_proof_value, hash_bytes};
use ad4m_identity_core::{scid_of, verify_signed, KelChain, REQUEST_DOMAIN};
use ad4m_ldk::identity::SignedRequest;
use base64::Engine;
use rusqlite::{params, Connection};

use crate::db::{self, to_sql};
use crate::error::ApiError;
use crate::Store;

/// The header that carries a signed request.
pub const REQUEST_HEADER: &str = "x-coasys-request";
/// How far ahead a request's `expires` may lie.
pub const MAX_AHEAD_S: u64 = 300;
/// How long a nonce is remembered.
pub const NONCE_WINDOW_S: u64 = 600;

/// Which keys a route accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Signer {
    /// A non-bot device key or the current master key (log, note and agent-record
    /// routes). A bot key never signs a request.
    AnyKey,
    /// Only the current master key (the offer routes).
    Master,
}

/// A request that passed: the identity, its replayed log and the signing key's id.
pub struct Verified {
    pub scid: String,
    pub chain: KelChain,
    pub key_id: String,
}

/// What a route hands the check.
pub struct Incoming<'a> {
    pub header: Option<&'a str>,
    pub method: &'a str,
    /// The path as received, without the query: a query only shapes a read (`wait`), so a
    /// client signs the route's path alone.
    pub path: &'a str,
    pub body: &'a [u8],
}

fn refuse(detail: impl Into<String>) -> ApiError {
    ApiError::forbidden("signer", detail)
}

/// Records `nonce` (16 bytes, base58btc) as seen at `now`: `403 signer` when it was seen in
/// the last [`NONCE_WINDOW_S`]. Signed requests and pre-join inbox reads share the store.
pub(crate) fn take_nonce(conn: &mut Connection, nonce: &str, now: u64) -> Result<(), ApiError> {
    // 16 bytes take at most 22 base58 characters; longer text is refused before decoding.
    let nonce_ok = nonce.len() <= 22 && bs58::decode(nonce).into_vec().is_ok_and(|b| b.len() == 16);
    if !nonce_ok {
        return Err(ApiError::invalid(
            "request",
            "the nonce is 16 bytes of base58btc",
        ));
    }
    let tx = conn.transaction()?;
    tx.execute(
        "DELETE FROM nonces WHERE seen_at <= ?1",
        params![to_sql(now.saturating_sub(NONCE_WINDOW_S))?],
    )?;
    let fresh = tx.execute(
        "INSERT OR IGNORE INTO nonces (nonce, seen_at) VALUES (?1, ?2)",
        params![nonce, to_sql(now)?],
    )?;
    if fresh == 0 {
        return Err(refuse("the nonce was seen in the last 10 minutes"));
    }
    tx.commit()?;
    Ok(())
}

/// Decodes the header. Padded and unpadded base64url are both taken.
pub fn decode_header(header: &str) -> Result<SignedRequest, ApiError> {
    decode_json_header(header, "X-Coasys-Request")
}

/// Decodes a header that carries base64url(JSON), padded or not (`400 request` otherwise).
pub(crate) fn decode_json_header<T: serde::de::DeserializeOwned>(
    header: &str,
    name: &str,
) -> Result<T, ApiError> {
    let trimmed = header.trim().trim_end_matches('=');
    let bytes = base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(trimmed)
        .map_err(|_| ApiError::invalid("request", format!("{name} is not base64url")))?;
    serde_json::from_slice(&bytes).map_err(|e| ApiError::invalid("request", format!("{name}: {e}")))
}

/// Encodes a signed request for the header.
pub fn encode_header(req: &SignedRequest) -> String {
    let value = serde_json::to_value(req).expect("a signed request serialises");
    let text =
        ad4m_hash_chain::canonical_json(&value).expect("a signed request has a canonical form");
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(text)
}

/// The path with its `%XX` escapes decoded, so a client that escapes a `:` in a DID and one
/// that does not name the same path. `None` for a broken escape or bytes that are not UTF-8.
pub fn percent_decode(path: &str) -> Option<String> {
    let bytes = path.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            let hex = bytes.get(i + 1..i + 3)?;
            if !hex.iter().all(u8::is_ascii_hexdigit) {
                return None;
            }
            out.push(u8::from_str_radix(std::str::from_utf8(hex).ok()?, 16).ok()?);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).ok()
}

impl Store {
    /// Checks that a signed request or a pre-join inbox read names this store (`403 store`
    /// otherwise), so it cannot be replayed to another store.
    pub(crate) fn check_store(&self, store: &str) -> Result<(), ApiError> {
        if store != self.key.did() {
            return Err(ApiError::forbidden(
                "store",
                "the request is signed for another store",
            ));
        }
        Ok(())
    }

    /// Checks a signed request for the identity `did` (when the route names one) and records
    /// its nonce.
    pub fn verify_request(
        &self,
        incoming: &Incoming<'_>,
        did: Option<&str>,
        signer: Signer,
    ) -> Result<Verified, ApiError> {
        let header = incoming
            .header
            .ok_or_else(|| refuse("the route needs a signed request (X-Coasys-Request)"))?;
        let signed = decode_header(header)?;
        let req = &signed.req;
        let now = self.now();
        self.check_store(&req.store)?;
        if let Some(did) = did {
            if req.did != did {
                return Err(refuse("the request is signed for another identity"));
            }
        }
        let scid = scid_of(&req.did).ok_or_else(|| refuse("the request names no did:scid"))?;
        if !req.method.eq_ignore_ascii_case(incoming.method)
            || percent_decode(incoming.path).as_deref() != Some(req.path.as_str())
        {
            return Err(refuse("the request is signed for another method or path"));
        }
        if req.body_hash != hash_bytes(incoming.body) {
            return Err(refuse("the body does not match bodyHash"));
        }
        if req.expires <= now {
            return Err(refuse("the request has expired"));
        }
        if req.expires > now + MAX_AHEAD_S {
            return Err(refuse("the request expires more than 5 minutes ahead"));
        }
        let mut conn = self.db.lock()?;
        let chain = db::load_chain(&conn, scid)?
            .ok_or_else(|| refuse("the store holds no log of the identity"))?;
        if chain.hash_at(req.kel_seq) != Some(&req.kel_hash) {
            return Err(refuse("kelSeq and kelHash name no position of the log"));
        }
        let state = chain.state();
        let head = chain.head();
        let record = state
            .record(&req.key_id)
            .filter(|r| r.id == req.key_id)
            .ok_or_else(|| refuse("the key is not in the log"))?;
        if !record.scopes.is_empty() {
            return Err(refuse("a bot key signs no request for its owner"));
        }
        if !state.key_valid_at_time(&req.key_id, head.seq, now) {
            return Err(refuse("the key is not valid at the head"));
        }
        if signer == Signer::Master && state.current_master().map(|m| &m.id) != Some(&req.key_id) {
            return Err(refuse("the route needs the current master key"));
        }
        let sig =
            decode_proof_value(&signed.sig).ok_or_else(|| refuse("the signature is malformed"))?;
        if !verify_signed(REQUEST_DOMAIN, req, &record.sign, &sig) {
            return Err(refuse("the signature does not verify"));
        }
        take_nonce(&mut conn, &req.nonce, now)?;
        Ok(Verified {
            scid: scid.to_string(),
            key_id: req.key_id.clone(),
            chain,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::percent_decode;

    #[test]
    fn percent_decode_names_the_same_path_with_and_without_escapes() {
        assert_eq!(
            percent_decode("/v1/agents/did%3Ascid%3acoasys%3A1%3AQm").as_deref(),
            Some("/v1/agents/did:scid:coasys:1:Qm")
        );
        assert_eq!(percent_decode("/v1/pow").as_deref(), Some("/v1/pow"));
        assert_eq!(percent_decode("/v1/%2"), None);
        assert_eq!(percent_decode("/v1/%+1"), None);
        assert_eq!(percent_decode("/v1/%FF"), None);
    }
}
