//! The hosting index client (AR-07 4.5, 4.6): this executor lists itself in the Coasys hosting
//! index.
//!
//! The executor, not a browser, signs in: it asks the index for a challenge, signs a
//! `KeyStatement` under `coasys:session` with its **main device key** (the key `host.info`
//! names as `executorDid`, AR-05-R54; there is no separate host key), and holds the session
//! token in memory only. With the token it publishes, updates and withdraws its one listing
//! and reads its own view of it (`me`).
//!
//! The statement is the one the platform's `worker/src/index/signin.ts` accepts: the AR-04
//! `KeyStatement` without `audience` (the index takes no other field than `v`, `purpose`,
//! `scid`, `key`, `executor`, `challenge`, `client`, `issuedAt`), with `executor` the index's
//! own `did:key`. The signed bytes are `signing_bytes("coasys:session", statement)` and the
//! signature is `z` + base58btc, exactly as `worker/src/index/encoding.ts` makes them; a test
//! pins both against a vector produced by that file.
//!
//! [`IndexClient`] touches no global state (the URL, the signer, the operator identity and
//! the draft are arguments), so it is tested against a local index. The RPC methods that use
//! it live in `api/index_ws.rs`.

use std::time::Duration;

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::{signing_bytes, SESSION_DOMAIN};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use ts_rs::TS;

use crate::identity::types::SessionClient;

/// Each index request takes at most this long.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(15);

/// A session is renewed this long before the index says it ends.
const SESSION_MARGIN_S: u64 = 60;

/// Where this executor's index is and whether it may call it (AR-07 4.3.3 `IndexConfig`). Kept
/// in the `settings` table (`hosting_index.config`); the default is off, so no call ever leaves
/// for an index until the operator sets one.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", default)]
#[ts(export)]
pub struct IndexConfig {
    /// The index's base URL, e.g. `https://api.coasys.org/v1/index`. Empty: none.
    pub url: String,
    /// `false`: no call leaves for the index.
    pub enabled: bool,
}

/// An AI model a listing shows.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ListedModel {
    pub name: String,
    /// `llm`, `embedding` or `transcription`.
    pub kind: String,
}

/// One per-operation rate a listing shows.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PricingItem {
    pub description: String,
    #[serde(rename = "priceInHOT")]
    #[ts(rename = "priceInHOT")]
    pub price_in_hot: f64,
    /// Always `credits`.
    pub unit: String,
}

/// The text an operator types for a listing (AR-07 `ListingDraftFields`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ListingText {
    pub name: String,
    pub description: String,
    pub location: String,
    /// The address the index reaches this executor at (`https`, a public host).
    pub url: String,
}

/// What the index receives with `PUT /listing` (the platform's `ListingDraft`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ListingDraft {
    #[serde(flatten)]
    pub text: ListingText,
    pub ai_models: Vec<ListedModel>,
    pub rates: Vec<PricingItem>,
}

/// The listing as the index shows it to the operator (the platform's `OwnListing`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct OwnListing {
    pub listing_id: String,
    pub name: String,
    pub description: String,
    pub location: String,
    pub url: String,
    pub executor_did: String,
    pub operator_did: String,
    pub run_by_coasys: bool,
    pub ai_models: Vec<ListedModel>,
    pub rates: Vec<PricingItem>,
    /// `anyone`, `by_invite` or `no_one`.
    pub signup: String,
    pub admits_guests: bool,
    pub free_access: bool,
    pub full: bool,
    #[ts(type = "{ kind: string, since: string }")]
    pub approval: Value,
    pub last_seen_at: String,
    pub version: String,
    /// `listed`, `unreachable`, `delisted` or `withdrawn`.
    pub state: String,
    pub reason: Option<String>,
}

/// What this executor remembers of its listing (AR-07 4.3.3 `LocalListing`), kept in the
/// `settings` table as the JSON row `index_listing` (no secret in it). It is the operator's text
/// and the last state the index answered, so `index.status` can show the listing while the
/// index is down, and the republisher knows what was last sent.
///
/// Differs from the spec's record in two encodings: `published_at` is unix seconds and
/// `rates_hash` is lower-case hex, so the row is plain JSON that crosses the SDK as it is.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LocalListing {
    pub listing_id: String,
    pub draft: ListingText,
    /// `listed`, `unreachable`, `delisted` or `withdrawn`: the last state the index answered.
    pub state: String,
    pub reason: Option<String>,
    /// Unix seconds of the last publish.
    #[ts(type = "number")]
    pub published_at: u64,
    /// SHA-256 (hex) of the models and rates last sent (`rates_hash`).
    pub rates_hash: String,
}

/// The SHA-256 of the models and rates a listing carries, in an order that does not depend on
/// the order the tables answer in.
pub fn rates_hash(models: &[ListedModel], rates: &[PricingItem]) -> String {
    use sha2::{Digest, Sha256};
    let mut models: Vec<_> = models.iter().map(|m| (&m.name, &m.kind)).collect();
    models.sort();
    let mut rates: Vec<_> = rates
        .iter()
        .map(|r| (&r.description, r.price_in_hot.to_bits(), &r.unit))
        .collect();
    rates.sort();
    let bytes =
        serde_json::to_vec(&json!({ "models": models, "rates": rates })).unwrap_or_default();
    hex::encode(Sha256::digest(bytes))
}

/// How long the models and rates stay as they are before a changed listing is published again.
pub const REPUBLISH_QUIET_S: u64 = 60;

/// A listing is published again this long after the last publish, which renews the index's
/// key check.
pub const REPUBLISH_DAILY_S: u64 = 24 * 60 * 60;

/// A change of models or rates seen, and when it was first seen at this value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PendingChange {
    pub hash: String,
    pub since: u64,
}

/// Whether the listing is published again at `now` (AR-07 4.6 "Republish on change"): when it
/// is on the index (`listed` or `unreachable`) and either the models and rates (`current`)
/// differ from the last sent and have not changed again for [`REPUBLISH_QUIET_S`] (a debounce:
/// another change restarts the wait), or the last publish is [`REPUBLISH_DAILY_S`] old.
/// `pending` carries the debounce between calls.
pub fn republish_due(
    local: &LocalListing,
    current: &str,
    pending: &mut Option<PendingChange>,
    now: u64,
) -> bool {
    if !matches!(local.state.as_str(), "listed" | "unreachable") {
        *pending = None;
        return false;
    }
    if now.saturating_sub(local.published_at) >= REPUBLISH_DAILY_S {
        return true;
    }
    if current == local.rates_hash {
        *pending = None;
        return false;
    }
    match pending {
        Some(p) if p.hash == current => now.saturating_sub(p.since) >= REPUBLISH_QUIET_S,
        _ => {
            *pending = Some(PendingChange {
                hash: current.to_string(),
                since: now,
            });
            false
        }
    }
}

/// `GET /me`: the signed-in operator and its listing.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IndexMe {
    pub scid: String,
    pub allowed: bool,
    pub listing: Option<OwnListing>,
}

/// `GET /info`.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IndexInfo {
    pub index_did: String,
    #[serde(default)]
    pub closed_beta_free: bool,
}

/// An open index session. In memory only: a restart signs in again.
#[derive(Clone, Debug)]
pub struct IndexSession {
    token: String,
    /// Unix seconds, by the index's clock.
    pub expires_at: u64,
    /// The operator identity the index signed in.
    pub scid: String,
    pub run_by_coasys: bool,
}

impl IndexSession {
    /// Whether the session still has [`SESSION_MARGIN_S`] left at `now`.
    pub fn is_live(&self, now: u64) -> bool {
        self.expires_at > now + SESSION_MARGIN_S
    }
}

/// Why an index call failed.
#[derive(Debug, thiserror::Error)]
pub enum IndexError {
    #[error("the index is not reachable: {0}")]
    Transport(String),
    /// The index answered with an error: its HTTP status, error code (`not-allowed`,
    /// `unreachable`, `charging`, ...) and message.
    #[error("the index refused ({status} {code}): {message}")]
    Refused {
        status: u16,
        code: String,
        message: String,
    },
    /// The answer is not what the index contract says.
    #[error("the index answered something unexpected: {0}")]
    Malformed(String),
    /// The challenge named another audience than `/info` did: nothing is signed.
    #[error(
        "the index named two different identities ({info} and {challenge}); nothing was signed"
    )]
    AudienceMismatch { info: String, challenge: String },
    /// The statement could not be signed.
    #[error("could not sign the index statement: {0}")]
    Signing(String),
}

impl IndexError {
    /// Whether the index said the session is not valid (a new sign-in may fix it).
    pub fn is_unauthorized(&self) -> bool {
        matches!(self, IndexError::Refused { status: 401, code, .. } if code == "unauthorized")
    }
}

/// The statement the index accepts: AR-04's `KeyStatement` for the index audience.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IndexStatement {
    pub v: u8,
    pub purpose: String,
    pub scid: String,
    pub key: String,
    /// The index's own `did:key`.
    pub executor: String,
    pub challenge: String,
    pub client: SessionClient,
    pub issued_at: u64,
}

/// The statement and its signature, the body of `POST /session` under `proof`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignedIndexStatement {
    pub statement: IndexStatement,
    pub sig: String,
}

/// Builds the whole statement for the index `index_did` and signs it with `signer`
/// (AR-07 4.5: `index_client::sign_index_statement`). The caller gives the audience the index
/// itself answered; no other audience can be signed for here.
pub fn sign_index_statement(
    signer: &dyn Signer,
    scid: &str,
    index_did: &str,
    challenge: &str,
    issued_at: u64,
) -> Result<SignedIndexStatement, IndexError> {
    let statement = IndexStatement {
        v: 1,
        purpose: "login".into(),
        scid: scid.into(),
        key: signer.key(),
        executor: index_did.into(),
        challenge: challenge.into(),
        client: SessionClient::Native,
        issued_at,
    };
    let bytes = signing_bytes(SESSION_DOMAIN, &statement)
        .map_err(|e| IndexError::Signing(e.to_string()))?;
    let sig = ad4m_hash_chain::encode_proof_value(&signer.sign(&bytes));
    Ok(SignedIndexStatement { statement, sig })
}

/// The index's HTTP API (`/v1/index`) at one base URL.
pub struct IndexClient {
    http: reqwest::Client,
    base: String,
}

impl IndexClient {
    /// A client for the index at `url`. Only `https` is accepted, and `http` for a loopback
    /// host (a local index for development and tests).
    pub fn new(url: &str) -> Result<Self, IndexError> {
        let base = url.trim().trim_end_matches('/').to_string();
        let parsed = reqwest::Url::parse(&base)
            .map_err(|e| IndexError::Malformed(format!("the index URL {base:?}: {e}")))?;
        let loopback = parsed.host_str().is_some_and(is_loopback_host);
        if parsed.scheme() != "https" && !(parsed.scheme() == "http" && loopback) {
            return Err(IndexError::Malformed(format!(
                "the index URL {base} must be https (http only on loopback)"
            )));
        }
        let http = reqwest::Client::builder()
            .timeout(REQUEST_TIMEOUT)
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .map_err(|e| IndexError::Transport(e.to_string()))?;
        Ok(IndexClient { http, base })
    }

    async fn send<T: serde::de::DeserializeOwned>(
        &self,
        request: reqwest::RequestBuilder,
    ) -> Result<T, IndexError> {
        let response = request
            .header(reqwest::header::ACCEPT, "application/json")
            .send()
            .await
            .map_err(|e| IndexError::Transport(e.to_string()))?;
        let status = response.status();
        let body = response
            .bytes()
            .await
            .map_err(|e| IndexError::Transport(e.to_string()))?;
        if !status.is_success() {
            let v: Value = serde_json::from_slice(&body).unwrap_or(Value::Null);
            return Err(IndexError::Refused {
                status: status.as_u16(),
                code: v["error"]["code"].as_str().unwrap_or("unknown").to_string(),
                message: v["error"]["message"]
                    .as_str()
                    .unwrap_or("no message")
                    .to_string(),
            });
        }
        serde_json::from_slice(&body).map_err(|e| IndexError::Malformed(e.to_string()))
    }

    /// `GET /info`: the index's identity.
    pub async fn info(&self) -> Result<IndexInfo, IndexError> {
        self.send(self.http.get(format!("{}/info", self.base)))
            .await
    }

    /// Signs in with `signer` as a device of `scid`: `/info`, `/challenge`, the signed statement
    /// at `now` (unix seconds), `/session`. Refuses to sign when the challenge's audience is not
    /// the one `/info` gave.
    pub async fn sign_in(
        &self,
        signer: &(dyn Signer + Sync),
        scid: &str,
        now: u64,
    ) -> Result<IndexSession, IndexError> {
        let info = self.info().await?;
        #[derive(Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct Challenge {
            challenge: String,
            index_did: String,
        }
        let challenge: Challenge = self
            .send(
                self.http
                    .post(format!("{}/challenge", self.base))
                    .json(&json!({ "key": signer.key() })),
            )
            .await?;
        if challenge.index_did != info.index_did {
            return Err(IndexError::AudienceMismatch {
                info: info.index_did,
                challenge: challenge.index_did,
            });
        }
        let proof = sign_index_statement(signer, scid, &info.index_did, &challenge.challenge, now)?;
        #[derive(Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct Opened {
            token: String,
            expires_at: u64,
            scid: String,
            run_by_coasys: bool,
        }
        let opened: Opened = self
            .send(
                self.http
                    .post(format!("{}/session", self.base))
                    .json(&json!({ "proof": proof })),
            )
            .await?;
        Ok(IndexSession {
            token: opened.token,
            expires_at: opened.expires_at,
            scid: opened.scid,
            run_by_coasys: opened.run_by_coasys,
        })
    }

    /// `DELETE /session`.
    pub async fn sign_out(&self, session: &IndexSession) -> Result<(), IndexError> {
        let response = self
            .http
            .delete(format!("{}/session", self.base))
            .bearer_auth(&session.token)
            .send()
            .await
            .map_err(|e| IndexError::Transport(e.to_string()))?;
        if response.status().is_success() {
            Ok(())
        } else {
            Err(IndexError::Refused {
                status: response.status().as_u16(),
                code: "session".into(),
                message: "the index did not end the session".into(),
            })
        }
    }

    /// `GET /me`: the operator and its listing for this session's device key.
    pub async fn me(&self, session: &IndexSession) -> Result<IndexMe, IndexError> {
        self.send(
            self.http
                .get(format!("{}/me", self.base))
                .bearer_auth(&session.token),
        )
        .await
    }

    /// `PUT /listing`: publishes the listing, or updates it (one per device key).
    pub async fn publish(
        &self,
        session: &IndexSession,
        draft: &ListingDraft,
    ) -> Result<OwnListing, IndexError> {
        self.send(
            self.http
                .put(format!("{}/listing", self.base))
                .bearer_auth(&session.token)
                .json(draft),
        )
        .await
    }

    /// `DELETE /listing`: withdraws the listing; it is hidden at once.
    pub async fn withdraw(&self, session: &IndexSession) -> Result<(), IndexError> {
        let _: Value = self
            .send(
                self.http
                    .delete(format!("{}/listing", self.base))
                    .bearer_auth(&session.token),
            )
            .await?;
        Ok(())
    }
}

fn is_loopback_host(host: &str) -> bool {
    let host = host.trim_start_matches('[').trim_end_matches(']');
    host == "localhost"
        || host
            .parse::<std::net::IpAddr>()
            .is_ok_and(|ip| ip.is_loopback())
}

#[cfg(test)]
mod tests;
