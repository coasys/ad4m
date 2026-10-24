//! The operations behind the `identity.*` RPC calls (AR-01 4.10, AR-02 4.15). The handlers in
//! `api/identity_ws.rs` parse params and call these with the session's agent
//! (`RequestContext::acts_as()`); each call reaches logs only through [`IdentityBackend`] and
//! keeps what it learns in the [`KelCache`].

use std::sync::Arc;

use ad4m_identity_core::build::cosign;
use ad4m_identity_core::link::{self, OfferString};
use ad4m_identity_core::{did_of, Event, Op};
use serde_json::Value;

use super::backend::{IdentityBackend, IdentityError};
use super::cache::{scid_from, CacheError, KelCache};
use super::device::{device_key, enc_key_of, WalletSigner};
use super::operator::FirstLinkRefusal;
use super::types::{
    convert, BotView, DeviceView, ForkRecord, HoldsMaster, IdentityLog, IdentityStatus,
    OfferListing, OfferOpened, PowParams, PublishParams, SignedHead, SignedOffer, SignedRequest,
};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::db::Ad4mDb;

pub(super) fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

pub(super) fn error(code: u16, message: impl Into<String>) -> WsRpcError {
    WsRpcError {
        code,
        message: message.into(),
    }
}

/// The RPC answer for a refusal of the identity interface: the HTTP status the store gave,
/// and a message that starts with the refusal's name.
pub fn backend_error(e: IdentityError) -> WsRpcError {
    match e {
        IdentityError::Conflict(head) => error(
            409,
            format!(
                "conflict: the store's head is seq {} ({})",
                head.seq, head.hash
            ),
        ),
        IdentityError::KeyTaken(key) => error(409, format!("key-taken: {key} sits in a log")),
        IdentityError::Forbidden(reason) => error(
            403,
            format!(
                "forbidden: {}",
                serde_json::to_string(&reason).unwrap_or_default()
            ),
        ),
        IdentityError::Invalid(reason) => error(400, format!("invalid: {reason}")),
        IdentityError::RateLimited {
            scope,
            retry_after_s,
        } => error(
            429,
            format!("rate-limited: {scope:?}, retry after {retry_after_s} s"),
        ),
        IdentityError::Unavailable(why) => error(503, format!("unavailable: {why}")),
        IdentityError::BadHeadSignature => error(
            502,
            "bad-head-signature: the store's head is not signed by a key of identity.storeKeys",
        ),
        IdentityError::Gone(state) => error(410, format!("gone: the offer is {state}")),
        IdentityError::NotFound => WsRpcError::not_found("not-found"),
    }
}

/// The RPC answer for a cache refusal.
pub fn cache_error(e: CacheError) -> WsRpcError {
    match e {
        CacheError::Fork(fork) => error(
            409,
            format!(
                "identity-fork: two logs at seq {} (ours {}, theirs {}); the cache keeps its history",
                fork.seq, fork.ours, fork.theirs
            ),
        ),
        CacheError::Fold(e) => error(
            400,
            format!("invalid: {}", ad4m_identity_core::error_code(&e)),
        ),
        CacheError::Gap { have, first } => error(
            400,
            format!("invalid: the events start at seq {first}, the log ends at {have:?}"),
        ),
        CacheError::NotFound => WsRpcError::not_found("not-found: no log of this identity"),
        CacheError::BadHeadSignature => backend_error(IdentityError::BadHeadSignature),
        CacheError::Unavailable(why) => error(503, format!("unavailable: {why}")),
        CacheError::Storage(why) => WsRpcError::internal(format!("the key-log cache: {why}")),
    }
}

pub(super) fn db<T>(
    f: impl FnOnce(&Ad4mDb) -> crate::db::Ad4mDbResult<T>,
) -> Result<T, WsRpcError> {
    Ad4mDb::with_global_instance(f).map_err(|e| WsRpcError::internal(e.to_string()))
}

/// The identity a local agent (`did:key`) speaks for, or `None` (also when the database is not
/// open yet, as in unit tests that never open it).
pub fn identity_of_agent(agent_did: &str) -> Option<String> {
    let instance = Ad4mDb::global_instance();
    let guard = instance.lock().ok()?;
    let db = guard.as_ref()?;
    db.agent_identity(agent_did)
        .ok()
        .flatten()
        .map(|scid| did_of(&scid))
}

fn first_fork(forks: &[ad4m_identity_core::ForkEvidence]) -> Option<ForkRecord> {
    forks.first().map(|f| ForkRecord {
        seq: f.seq,
        ours: f.ours.clone(),
        theirs: f.theirs.clone(),
    })
}

fn mirror_head(head: &ad4m_ldk::identity::SignedHead) -> Result<SignedHead, WsRpcError> {
    convert(head).map_err(|e| WsRpcError::internal(e.to_string()))
}

/// Normalises a store base URL for comparison.
pub(super) fn store_base(url: &str) -> &str {
    url.trim_end_matches('/')
}

/// The executor's identity module: the backend, the cache, and the configured store URL.
pub struct IdentityService {
    pub backend: Arc<dyn IdentityBackend>,
    pub cache: Arc<KelCache>,
    /// `identity.storeUrl`: the store the agent language reaches; an offer must name it.
    pub store_url: Option<String>,
}

impl IdentityService {
    /// Refreshes `scid` from the agent language when it can; with a cached log, a failed
    /// refresh (an outage, or a fork the cache recorded) falls back to the cache.
    pub(super) async fn refreshed_or_cached(
        &self,
        scid: &str,
    ) -> Result<Option<ad4m_identity_core::KelChain>, WsRpcError> {
        match self.cache.refresh(self.backend.as_ref(), scid).await {
            Ok(chain) => Ok(Some(chain)),
            Err(e) => {
                let cached = self.cache.chain(scid).await.map_err(cache_error)?;
                if cached.is_none() && !matches!(e, CacheError::NotFound) {
                    return Err(cache_error(e));
                }
                log::info!("identity: refresh of {scid} failed ({e}); answering from the cache");
                Ok(cached)
            }
        }
    }

    /// `identity.status` for `agent`.
    pub async fn status(&self, agent: &AgentContext) -> Result<IdentityStatus, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let enc = enc_key_of(&key).map_err(WsRpcError::internal)?;
        let mut status = IdentityStatus {
            did: None,
            device_key: key.clone(),
            device_enc: enc,
            key_id: None,
            revoked: None,
            vouched: false,
            head: None,
            fork: None,
        };
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(status);
        };
        status.did = Some(did_of(&scid));
        if let Some(chain) = self.refreshed_or_cached(&scid).await? {
            let state = chain.state();
            status.key_id = state.key_id_of(&key).cloned();
            status.revoked = status
                .key_id
                .as_ref()
                .and_then(|id| state.keys.get(id))
                .and_then(|record| record.revoked);
        }
        if let Some((head, vouched)) = self.cache.accepted_head(&scid).map_err(cache_error)? {
            status.head = Some(mirror_head(&head)?);
            status.vouched = vouched;
        }
        status.fork = first_fork(&self.cache.forks(&scid).await.map_err(cache_error)?);
        Ok(status)
    }

    /// `identity.log`: the log through the cache; refreshes first when asked, or when the cache
    /// holds nothing of it.
    pub async fn log(&self, did: &str, from: u64, fresh: bool) -> Result<IdentityLog, WsRpcError> {
        let scid = ad4m_identity_core::scid_of(did)
            .ok_or_else(|| WsRpcError::bad_request(format!("{did} is not a did:scid:coasys:1")))?
            .to_string();
        let cached = self.cache.chain(&scid).await.map_err(cache_error)?;
        if fresh || cached.is_none() {
            self.refreshed_or_cached(&scid)
                .await?
                .ok_or_else(|| cache_error(CacheError::NotFound))?;
        }
        let events = self
            .cache
            .stored_events(&scid, from)
            .await
            .map_err(cache_error)?
            .iter()
            .map(serde_json::to_value)
            .collect::<Result<Vec<Value>, _>>()?;
        let accepted = self.cache.accepted_head(&scid).map_err(cache_error)?;
        Ok(IdentityLog {
            did: did_of(&scid),
            events,
            head: accepted
                .as_ref()
                .map(|(head, _)| mirror_head(head))
                .transpose()?,
            vouched: accepted.is_some_and(|(_, vouched)| vouched),
            fork: first_fork(&self.cache.forks(&scid).await.map_err(cache_error)?),
        })
    }

    /// `identity.powParams`.
    pub async fn pow_params(&self) -> Result<PowParams, WsRpcError> {
        let params = self.backend.pow_params().await.map_err(backend_error)?;
        convert(&params).map_err(|e| WsRpcError::internal(e.to_string()))
    }

    /// `identity.cosignIncept`: adds `agent`'s device-key proof to an `incept` that lists that
    /// key with its `enc` key, while the agent has no identity yet; the event must fold with it.
    pub async fn cosign_incept(
        &self,
        agent: &AgentContext,
        event: &Value,
    ) -> Result<Value, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        if let Some(held) = db(|db| db.agent_identity(&agent_did))? {
            return Err(error(
                409,
                format!(
                    "identity-exists: this agent speaks for {} already",
                    did_of(&held)
                ),
            ));
        }
        let mut event = Event::from_json(event)
            .map_err(|e| WsRpcError::bad_request(format!("invalid: {} ({e})", e.name())))?;
        let Op::Incept { add, .. } = &event.op else {
            return Err(WsRpcError::bad_request(format!(
                "invalid: a {} is not an incept",
                event.op.name()
            )));
        };
        let listed = add.iter().find(|d| d.sign == key).ok_or_else(|| {
            WsRpcError::bad_request("not-listed: the incept does not list this agent's device key")
        })?;
        let enc = enc_key_of(&key).map_err(WsRpcError::internal)?;
        if listed.enc != enc {
            return Err(WsRpcError::bad_request(format!(
                "wrong-enc: the incept lists this key with enc {}, not its own {enc}",
                listed.enc
            )));
        }
        let id = listed.id.clone();
        let signer = WalletSigner::new(agent).map_err(WsRpcError::internal)?;
        cosign(&mut event, &id, &signer)
            .map_err(|e| WsRpcError::bad_request(format!("invalid: {e}")))?;
        signer.check().map_err(WsRpcError::internal)?;
        ad4m_identity_core::fold(std::slice::from_ref(&event)).map_err(|e| {
            WsRpcError::bad_request(format!(
                "invalid: the incept does not fold with this proof: {}",
                ad4m_identity_core::error_code(&e)
            ))
        })?;
        Ok(serde_json::to_value(&event)?)
    }

    /// Records that `agent` speaks for the log `scid` when its device key is valid at the head.
    pub(super) async fn record_agent_if_listed(
        &self,
        agent: &AgentContext,
        scid: &str,
    ) -> Result<Option<String>, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let Some(chain) = self.cache.chain(scid).await.map_err(cache_error)? else {
            return Ok(None);
        };
        let state = chain.state();
        let Some(id) = state.key_id_of(&key).cloned() else {
            return Ok(None);
        };
        if state.key_valid_at_time(&key, chain.head().seq, now()) {
            db(|db| db.set_agent_identity(&agent_did, scid, now()))
                .map_err(|e| error(409, format!("identity-exists: {}", e.message)))?;
            // The main agent's status gains its identity (a managed agent's status event names
            // the main agent's, so only the main agent's changes here).
            if agent.is_main_agent() {
                let status = crate::agent::AgentService::with_global_instance(|a| a.dump());
                crate::pubsub::get_global_pubsub()
                    .await
                    .publish_event(crate::events::Event::agent_status(&status.with_identity()));
            }
        }
        Ok(Some(id))
    }

    /// `identity.publish`: appends through the agent language, then takes the events into the
    /// cache and records the identity for `agent` when the log lists its key. A `409` whose log
    /// already holds these events (the append happened elsewhere) answers the store's head.
    pub async fn publish(
        &self,
        agent: &AgentContext,
        params: &PublishParams,
    ) -> Result<SignedHead, WsRpcError> {
        let events = params
            .events
            .iter()
            .map(Event::from_json)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| WsRpcError::bad_request(format!("invalid: {} ({e})", e.name())))?;
        let first = events
            .iter()
            .min_by_key(|e| e.seq)
            .ok_or_else(|| WsRpcError::bad_request("invalid: no events"))?;
        let scid = first.scid.clone();
        if events.iter().any(|e| e.scid != scid) {
            return Err(WsRpcError::bad_request("invalid: the events name two logs"));
        }
        let did = did_of(&scid);
        let request = ad4m_ldk::identity::AppendRequest {
            events: params.events.clone(),
            vouch: params.vouch.clone(),
            pow: params
                .pow
                .as_ref()
                .map(convert)
                .transpose()
                .map_err(|e| WsRpcError::bad_request(e.to_string()))?,
        };
        let head = match self.backend.append(&did, &request).await {
            Ok(head) => head,
            Err(IdentityError::Conflict(stored)) => {
                self.appended_elsewhere(&did, &events, stored).await?
            }
            Err(e) => return Err(backend_error(e)),
        };
        let vouched = self
            .cache
            .accepted_head(&scid)
            .map_err(cache_error)?
            .is_some_and(|(_, v)| v);
        match self.cache.accept(&scid, &head, vouched, &events).await {
            Ok(_) => {}
            // The cache lacks the log's earlier events: read the whole log.
            Err(CacheError::Gap { .. }) => {
                self.cache
                    .refresh(self.backend.as_ref(), &scid)
                    .await
                    .map_err(cache_error)?;
            }
            Err(e) => return Err(cache_error(e)),
        }
        self.record_agent_if_listed(agent, &scid).await?;
        mirror_head(&head)
    }

    /// After a `409`: the store's head when its log holds `events` at their seqs (same hashes),
    /// else the conflict.
    async fn appended_elsewhere(
        &self,
        did: &str,
        events: &[Event],
        stored: ad4m_ldk::identity::SignedHead,
    ) -> Result<ad4m_ldk::identity::SignedHead, WsRpcError> {
        let from = events.iter().map(|e| e.seq).min().unwrap_or(0);
        let page = self
            .backend
            .log(did, from)
            .await
            .map_err(backend_error)?
            .ok_or_else(|| backend_error(IdentityError::Conflict(stored.clone())))?;
        let theirs: Vec<Event> = page
            .events
            .iter()
            .map(Event::from_json)
            .collect::<Result<_, _>>()
            .map_err(|e| WsRpcError::internal(format!("the store served {e}")))?;
        let same = events.iter().all(|ours| {
            theirs.iter().any(|t| {
                t.seq == ours.seq
                    && ad4m_identity_core::event_hash(t).ok()
                        == ad4m_identity_core::event_hash(ours).ok()
            })
        });
        if !same {
            return Err(backend_error(IdentityError::Conflict(stored)));
        }
        Ok(page.head)
    }

    /// `identity.openOffer`: passes an offer the custody service signed to the agent language.
    pub async fn open_offer(&self, offer: &SignedOffer) -> Result<OfferOpened, WsRpcError> {
        let offer = convert(offer).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
        let opened = self
            .backend
            .open_offer(&offer)
            .await
            .map_err(backend_error)?;
        convert(&opened).map_err(|e| WsRpcError::internal(e.to_string()))
    }

    /// `identity.offers`: passes a list request signed by the current master key.
    pub async fn offers(
        &self,
        request: &SignedRequest,
        wait: u8,
    ) -> Result<Vec<OfferListing>, WsRpcError> {
        let ldk: ad4m_ldk::identity::SignedRequest =
            convert(request).map_err(|e| WsRpcError::bad_request(e.to_string()))?;
        let listings = self
            .backend
            .list_offers(&request.req.did, &ldk, wait.min(8))
            .await
            .map_err(backend_error)?;
        convert(&listings).map_err(|e| WsRpcError::internal(e.to_string()))
    }

    /// `identity.devices`: the identity's device keys, from the log (no private record in
    /// slice 1, so no names).
    pub async fn devices(&self, agent: &AgentContext) -> Result<Vec<DeviceView>, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(vec![]);
        };
        let Some(chain) = self.refreshed_or_cached(&scid).await? else {
            return Ok(vec![]);
        };
        Ok(device_views(chain.state(), &key))
    }
}

/// The device keys of `state` as `DeviceView`s, in key-id order; `this` marks this device's key.
pub fn device_views(state: &ad4m_identity_core::KeyState, this: &str) -> Vec<DeviceView> {
    let mut views: Vec<DeviceView> = state
        .keys
        .values()
        .map(|record| {
            let scope = record.scopes.last().map(|(_, scope)| scope);
            DeviceView {
                key_id: record.id.clone(),
                sign: record.sign.clone(),
                name: None,
                kind: None,
                place: None,
                address: None,
                operator: None,
                runs_executor: false,
                holds_master: HoldsMaster::No,
                bot: scope.map(|s| BotView {
                    can: s.can.clone(),
                    expires: s.expires,
                    paused: false,
                }),
                expires: record.expires.or(scope.map(|s| s.expires)),
                added_seq: record.added,
                revoked_seq: record.revoked,
                this_device: record.sign == this,
            }
        })
        .collect();
    views.sort_by_key(|v| {
        v.key_id
            .trim_start_matches('d')
            .parse::<u64>()
            .unwrap_or(u64::MAX)
    });
    views
}

pub(super) fn first_link_error(refusal: FirstLinkRefusal) -> WsRpcError {
    WsRpcError::forbidden(refusal.to_string())
}

/// An offer string (`coasys:offer:1:…`) or link (`https://coasys.id/o#…`).
pub fn parse_offer(text: &str) -> Result<OfferString, WsRpcError> {
    let text = text.trim();
    let parsed = if text.starts_with(link::OFFER_STRING_PREFIX) {
        OfferString::parse(text)
    } else {
        OfferString::parse_link(text)
    };
    parsed.map_err(|e| WsRpcError::bad_request(format!("invalid: the offer: {e}")))
}

/// `scid_from` re-exported for the RPC layer.
pub fn scid_of_did(did: &str) -> &str {
    scid_from(did)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::identity::cache::tests::TestLog;

    #[test]
    fn device_views_follow_the_log() {
        let mut log = TestLog::new(4);
        log.revoke_d1();
        let chain = log.chain();
        let device = ad4m_identity_core::build::Signer::key(&log.device);
        let views = device_views(chain.state(), &device);
        assert_eq!(views.len(), 1);
        let d1 = &views[0];
        assert_eq!(d1.key_id, "d1");
        assert_eq!(d1.sign, device);
        assert_eq!(d1.added_seq, 0);
        assert_eq!(d1.revoked_seq, Some(1));
        assert!(d1.this_device);
        assert_eq!(d1.holds_master, HoldsMaster::No);
        assert!(device_views(chain.state(), "z6MkOther")
            .iter()
            .all(|v| !v.this_device));
    }

    #[test]
    fn an_offer_parses_from_its_string_or_its_link() {
        let offer = OfferString {
            store: "http://127.0.0.1:1".into(),
            scid: TestLog::new(4).scid(),
            offer_id: bs58::encode([1u8; 16]).into_string(),
            s: bs58::encode([2u8; 32]).into_string(),
        };
        assert_eq!(parse_offer(&offer.encode()).unwrap(), offer);
        assert_eq!(parse_offer(&format!(" {} ", offer.link())).unwrap(), offer);
        assert_eq!(parse_offer("coasys:device:1:abc").unwrap_err().code, 400);
    }

    #[test]
    fn refusals_keep_their_names_and_statuses() {
        let e = backend_error(IdentityError::KeyTaken("z6Mk".into()));
        assert_eq!((e.code, e.message.starts_with("key-taken")), (409, true));
        let e = backend_error(IdentityError::Gone("expired".into()));
        assert_eq!(e.code, 410);
        let e = first_link_error(FirstLinkRefusal::NoOperator);
        assert_eq!((e.code, e.message.starts_with("no-operator")), (403, true));
        let e = cache_error(CacheError::Fork(ad4m_identity_core::ForkEvidence {
            chain: "Qm".into(),
            seq: 2,
            ours: "a".into(),
            theirs: "b".into(),
        }));
        assert_eq!(
            (e.code, e.message.starts_with("identity-fork")),
            (409, true)
        );
    }
}
