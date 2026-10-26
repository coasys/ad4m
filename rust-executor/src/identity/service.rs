//! The operations behind the `identity.*` RPC calls. The handlers in
//! `api/identity_ws.rs` parse params and call these with the session's agent
//! (`RequestContext::acts_as()`); each call reaches logs only through [`IdentityBackend`] and
//! keeps what it learns in the [`KelCache`].

use std::sync::Arc;

use ad4m_identity_core::build::cosign;
use ad4m_identity_core::link::{self, OfferString};
use ad4m_identity_core::{did_of, Event, Op};
use serde_json::Value;

use super::backend::{IdentityBackend, IdentityError};
use super::cache::{CacheError, KelCache};
use super::device::{device_key, enc_key_of, WalletSigner};
use super::operator::{main_incept_check, FirstLinkRefusal};
use super::types::{
    convert, BotView, DeviceView, ForkRecord, IdentityLog, IdentityStatus, OfferListing,
    OfferOpened, PublishParams, SignedHead, SignedOffer, SignedRequest,
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
    /// DID resolution (`identity.resolve`) over the same cache and backend.
    pub resolver: Arc<super::resolver::Resolver>,
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
        if let Some(head) = self.cache.accepted_head(&scid).map_err(cache_error)? {
            status.head = Some(mirror_head(&head)?);
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
            head: accepted.as_ref().map(mirror_head).transpose()?,
            fork: first_fork(&self.cache.forks(&scid).await.map_err(cache_error)?),
        })
    }

    /// `identity.resetLog` (the operator): drops this executor's copy of `did`'s log (its
    /// events, accepted head and recorded forks, and what the resolver remembers of it), so the
    /// next read refreshes from the configured store and takes the store's branch. The way out
    /// of a fork: the cache never leaves a branch it holds by itself.
    pub async fn reset_log(&self, did: &str) -> Result<bool, WsRpcError> {
        let scid = ad4m_identity_core::scid_of(did)
            .ok_or_else(|| WsRpcError::bad_request(format!("{did} is not a did:scid:coasys:1")))?
            .to_string();
        self.cache.reset(&scid).await.map_err(cache_error)?;
        self.resolver.forget(&scid);
        log::warn!(
            "identity: the operator reset this executor's copy of the log of {} (events, accepted \
             head, forks); the next read takes the configured store's branch",
            did_of(&scid)
        );
        Ok(true)
    }

    /// `identity.cosignIncept`: adds `agent`'s device-key proof to an `incept` that lists that
    /// key with its `enc` key, while the agent has no identity yet; the event must fold with it.
    /// On an executor with an operator record the main agent co-signs no incept (`first-link`):
    /// its key may speak only for the operator's identity.
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
        if agent.is_main_agent() {
            let record = db(|db| db.operator_record())?;
            main_incept_check(record.as_ref(), &did_of(&event.scid)).map_err(first_link_error)?;
        }
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

    /// Records that `agent` speaks for the log `scid` when its device key is valid at the log's
    /// trusted seq (the accepted, store-signed head); the key's id when listed up to there.
    pub(super) async fn record_agent_if_listed(
        &self,
        agent: &AgentContext,
        scid: &str,
    ) -> Result<Option<String>, WsRpcError> {
        let (agent_did, key) = device_key(agent).map_err(WsRpcError::internal)?;
        // Only up to the store-signed head: a log lists any key its master adds, so events a
        // foreign `src` served never bind this agent to a person.
        let Some((chain, trusted)) = self.cache.trusted(scid).await.map_err(cache_error)? else {
            return Ok(None);
        };
        // As a device key: a log may name the agent's key as its master, which signs the log
        // and never speaks for the person.
        let state = chain.state();
        let Some(id) = super::resolver::device_key_id(state, &key)
            .filter(|id| state.keys.get(id).is_some_and(|r| r.added <= trusted))
        else {
            return Ok(None);
        };
        if state.key_valid_at_time(&id, trusted, now()) {
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

    /// Refuses the calls that make the store commit something irreversible (`publish`, a claim)
    /// while `identity.storeKeys` is empty: the cache then accepts no head, so the executor
    /// could never bind the agent to the log, but the store would already hold the events and
    /// the agent's key would be taken for every identity.
    pub(super) fn refuse_without_store_keys(&self) -> Result<(), WsRpcError> {
        if self.cache.trusts_a_store() {
            return Ok(());
        }
        Err(error(
            503,
            "no-store-keys: identity.storeKeys is not set, so this executor accepts no head from \
             the identity store and cannot bind an agent to a log; set identity.storeKeys first",
        ))
    }

    /// `identity.publish`: appends through the agent language, then takes the events into the
    /// cache and records the identity for `agent` when the log lists its key. A `409` whose log
    /// already holds these events (the append happened elsewhere) answers the store's head.
    /// Refused with `no-store-keys` while `identity.storeKeys` is empty, before the store commits.
    pub async fn publish(
        &self,
        agent: &AgentContext,
        params: &PublishParams,
    ) -> Result<SignedHead, WsRpcError> {
        self.refuse_without_store_keys()?;
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
            vouch: None,
            pow: None,
        };
        let head = match self.backend.append(&did, &request).await {
            Ok(head) => head,
            Err(IdentityError::Conflict(stored)) => {
                self.appended_elsewhere(&did, &events, stored).await?
            }
            Err(e) => return Err(backend_error(e)),
        };
        match self.cache.accept(&scid, &head, &events).await {
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

    /// `identity.devices`: the identity's device keys, from the log (the executor has no
    /// private record yet, so no names).
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
                bot: scope.map(|s| BotView {
                    can: s.can.clone(),
                    expires: s.expires,
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

    /// Sets the operator record of the shared test database; clears it when dropped, even if
    /// the test panics.
    struct OperatorRecord;

    impl OperatorRecord {
        fn set(did: &str) -> Self {
            Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(Some(did))).unwrap();
            OperatorRecord
        }
    }

    impl Drop for OperatorRecord {
        fn drop(&mut self) {
            let _ = Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None));
        }
    }

    /// A new person's incept listing `agent`'s device key as `d1`, signed by the master only.
    fn incept_listing(agent: &AgentContext, seed: u8) -> Value {
        use ad4m_identity_core::build::{incept_human, Signer};
        let (_, key) = device_key(agent).unwrap();
        let master = ed25519_dalek::SigningKey::from_bytes(&[seed; 32]);
        let next = ad4m_identity_core::commitment(&Signer::key(&master), "salt");
        let incept = incept_human(
            &master,
            next,
            vec![ad4m_identity_core::DeviceKey {
                id: "d1".into(),
                enc: enc_key_of(&key).unwrap(),
                sign: key,
                life: ad4m_identity_core::KeyLife::Standing,
            }],
        )
        .unwrap();
        serde_json::to_value(&incept).unwrap()
    }

    fn test_service() -> IdentityService {
        use crate::identity::cache::tests::{trusting, FakeStore};
        let backend: Arc<dyn IdentityBackend> = Arc::new(FakeStore::new(vec![]));
        let cache = Arc::new(trusting());
        IdentityService {
            resolver: Arc::new(crate::identity::resolver::Resolver::new(
                cache.clone(),
                backend.clone(),
                None,
                false,
            )),
            backend,
            cache,
            store_url: None,
        }
    }

    #[tokio::test]
    async fn with_an_operator_record_the_main_agent_co_signs_no_incept() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let service = test_service();
        let main = AgentContext::main_agent();
        let incept = incept_listing(&main, 51);
        let operator = did_of(&TestLog::new(4).scid());
        let proofs = |v: &Value| v["proof"].as_array().map(Vec::len);

        // No operator record (a desktop executor's first run): the main key starts an identity.
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let cosigned = service.cosign_incept(&main, &incept).await.unwrap();
        assert_eq!(proofs(&cosigned), Some(2));

        // With a record the main key may speak only for the operator: no new identity.
        let _record = OperatorRecord::set(&operator);
        let refused = service.cosign_incept(&main, &incept).await.unwrap_err();
        assert_eq!(refused.code, 403, "{refused:?}");
        assert!(
            refused.message.starts_with("first-link") && refused.message.contains(&operator),
            "{}",
            refused.message
        );

        // A managed user is not bound by the operator record.
        let user = crate::test_utils::create_test_user(&format!(
            "incept-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let managed = AgentContext::managed(user);
        let cosigned = service
            .cosign_incept(&managed, &incept_listing(&managed, 52))
            .await
            .unwrap();
        assert_eq!(proofs(&cosigned), Some(2));
    }

    /// Any controller can `add` an agent's key to its log (the `add` carries only the master's
    /// signature): the agent is bound to that person only once a store-signed head covers it.
    #[tokio::test]
    async fn an_agent_is_bound_to_a_person_only_by_a_store_signed_listing() {
        use crate::identity::cache::tests::accept_signed;
        use ad4m_identity_core::build::{add_unsigned, cosign};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let service = test_service();
        let user = crate::test_utils::create_test_user(&format!(
            "bind-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, key) = device_key(&agent).unwrap();

        let mut log = TestLog::new(62);
        let chain = log.chain();
        let device = ad4m_identity_core::DeviceKey {
            id: chain.state().next_device_id(),
            enc: enc_key_of(&key).unwrap(),
            sign: key.clone(),
            life: ad4m_identity_core::KeyLife::Standing,
        };
        let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut add, "m0", &log.master).unwrap();
        log.events.push(add);
        let scid = log.scid();

        // The store signed seq 0; the add at seq 1 came from elsewhere (a foreign src).
        accept_signed(&service.cache, &log.events[..1]).await;
        service.cache.ingest(&scid, &log.events[1..]).await.unwrap();
        assert_eq!(
            service.record_agent_if_listed(&agent, &scid).await.unwrap(),
            None
        );
        assert_eq!(db(|db| db.agent_identity(&agent_did)).unwrap(), None);

        accept_signed(&service.cache, &log.events).await;
        assert_eq!(
            service.record_agent_if_listed(&agent, &scid).await.unwrap(),
            Some("d2".to_string())
        );
        assert_eq!(db(|db| db.agent_identity(&agent_did)).unwrap(), Some(scid));
    }

    /// After a fork the cache keeps its branch; `resetLog` drops this executor's copy, and the
    /// next read takes the store's branch: a key revoked on the store's branch speaks no more.
    #[tokio::test]
    async fn a_reset_log_takes_the_stores_branch_after_a_fork() {
        use crate::identity::cache::tests::{trusting, FakeStore};
        use crate::identity::resolver::{key_speaks_for, test_profiles::signed_profile};
        use ad4m_identity_core::build::{add_unsigned, cosign, Signer};
        // Ours: seq 1 adds a second device key. The store's branch: seq 1 revokes d1.
        let mut ours = TestLog::new(71);
        let second = ed25519_dalek::SigningKey::from_bytes(&[72; 32]);
        let chain = ours.chain();
        let device = ad4m_identity_core::DeviceKey {
            id: chain.state().next_device_id(),
            sign: second.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[72; 32]),
            life: ad4m_identity_core::KeyLife::Standing,
        };
        let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut add, "m0", &ours.master).unwrap();
        ours.events.push(add);
        let mut theirs = TestLog::new(71);
        theirs.revoke_d1();
        assert_eq!(ours.scid(), theirs.scid());

        let store = Arc::new(FakeStore::new(ours.events.clone()));
        let backend: Arc<dyn IdentityBackend> = store.clone();
        let cache = Arc::new(trusting());
        let service = IdentityService {
            resolver: Arc::new(crate::identity::resolver::Resolver::new(
                cache.clone(),
                backend.clone(),
                None,
                false,
            )),
            backend,
            cache,
            store_url: None,
        };
        let scid = ours.scid();
        let did = did_of(&scid);
        let d1 = ours.device.key();
        service.log(&did, 0, true).await.unwrap();
        assert!(key_speaks_for(&service.cache, &did, &d1, now()).await);

        // The store now serves the other branch: the cache records the fork, keeps its own.
        *store.events.lock().unwrap() = theirs.events.clone();
        service.log(&did, 0, true).await.unwrap();
        assert_eq!(service.cache.forks(&scid).await.unwrap().len(), 1);
        assert!(
            key_speaks_for(&service.cache, &did, &d1, now()).await,
            "the cache keeps its branch"
        );

        assert!(service.reset_log(&did).await.unwrap());
        assert!(service.cache.chain(&scid).await.unwrap().is_none());
        assert!(service.cache.accepted_head(&scid).unwrap().is_none());
        assert!(service.cache.forks(&scid).await.unwrap().is_empty());

        // An ordinary verification reads the log again and takes the store's branch.
        let expr: crate::types::Expression<Value> =
            serde_json::from_value(signed_profile(&did, &ours.device)).unwrap();
        let reader: &dyn IdentityBackend = store.as_ref();
        assert!(
            !crate::identity::verify_expression(&service.cache, Some(reader), &expr, None)
                .await
                .unwrap(),
            "d1 is revoked on the store's branch"
        );
        let log = service.log(&did, 0, false).await.unwrap();
        assert_eq!(log.head.unwrap().hash, theirs.chain().head().hash);
        assert!(log.fork.is_none());

        // Not a did:scid: a bad request.
        assert_eq!(
            service.reset_log("did:key:z6Mk").await.unwrap_err().code,
            400
        );
    }

    // ---- probe (second review pass): publish before the executor can accept the head ----

    /// A store that records appends and answers them with a head signed by the test store
    /// key, so the log it then serves is the one it was given.
    struct RecordingStore {
        inner: crate::identity::cache::tests::FakeStore,
        appends: std::sync::atomic::AtomicUsize,
    }

    #[async_trait::async_trait]
    impl IdentityBackend for RecordingStore {
        async fn log(
            &self,
            did: &str,
            from: u64,
        ) -> Result<Option<ad4m_ldk::identity::LogPage>, IdentityError> {
            self.inner.log(did, from).await
        }
        async fn key_owner(
            &self,
            multikey: &str,
            wait_s: u8,
        ) -> Result<Option<ad4m_ldk::identity::KeyIndexEntry>, IdentityError> {
            self.inner.key_owner(multikey, wait_s).await
        }
        async fn append(
            &self,
            _did: &str,
            req: &ad4m_ldk::identity::AppendRequest,
        ) -> Result<ad4m_ldk::identity::SignedHead, IdentityError> {
            self.appends
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let events: Vec<Event> = req
                .events
                .iter()
                .map(|e| Event::from_json(e).unwrap())
                .collect();
            let mut held = self.inner.events.lock().unwrap();
            held.extend(events);
            let chain = ad4m_identity_core::fold(&held).unwrap();
            let head = chain.head();
            Ok(self
                .inner
                .key
                .sign_head(chain.id(), head.seq, &head.hash, 1))
        }
        async fn open_offer(
            &self,
            offer: &ad4m_ldk::identity::SignedOffer,
        ) -> Result<ad4m_ldk::identity::OfferOpened, IdentityError> {
            self.inner.open_offer(offer).await
        }
        async fn list_offers(
            &self,
            did: &str,
            req: &ad4m_ldk::identity::SignedRequest,
            wait_s: u8,
        ) -> Result<Vec<ad4m_ldk::identity::OfferListing>, IdentityError> {
            self.inner.list_offers(did, req, wait_s).await
        }
        async fn claim_offer(
            &self,
            did: &str,
            offer_id: &str,
            claim: &ad4m_ldk::identity::Claim,
        ) -> Result<ad4m_ldk::identity::ClaimReceipt, IdentityError> {
            self.inner.claim_offer(did, offer_id, claim).await
        }
        async fn offer_state(
            &self,
            did: &str,
            offer_id: &str,
            key: &str,
            wait_s: u8,
        ) -> Result<ad4m_ldk::identity::OfferProgress, IdentityError> {
            self.inner.offer_state(did, offer_id, key, wait_s).await
        }
    }

    /// With `identity.storeKeys` unset (the default), the executor can accept no store head, so
    /// a publish can never bind the agent. It must refuse before the store commits the events:
    /// once the store holds the incept, the device key is taken for every identity, forever.
    #[tokio::test]
    async fn without_store_keys_publish_refuses_before_the_store_commits() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let user = crate::test_utils::create_test_user(&format!(
            "nokeys-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore {
            inner: crate::identity::cache::tests::FakeStore::new(vec![]),
            appends: std::sync::atomic::AtomicUsize::new(0),
        });
        let cache = Arc::new(crate::identity::cache::tests::in_memory(vec![]));
        let service = IdentityService {
            resolver: Arc::new(crate::identity::resolver::Resolver::new(
                cache.clone(),
                backend.clone(),
                None,
                false,
            )),
            backend: backend.clone(),
            cache,
            store_url: None,
        };
        let incept = incept_listing(&agent, 53);
        let cosigned = service.cosign_incept(&agent, &incept).await.unwrap();
        let result = service
            .publish(
                &agent,
                &PublishParams {
                    events: vec![cosigned],
                },
            )
            .await;
        let err = result.expect_err("a publish the executor cannot accept is refused");
        assert_eq!(
            backend.appends.load(std::sync::atomic::Ordering::SeqCst),
            0,
            "the store was asked to commit an incept the executor can never accept ({})",
            err.message
        );
        assert_eq!(err.code, 503, "{err:?}");
        assert!(err.message.starts_with("no-store-keys"), "{}", err.message);
        assert_eq!(db(|db| db.agent_identity(&agent_did)).unwrap(), None);

        // A claim is refused the same way, before every other check (here: before `no-operator`).
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let offer = ad4m_identity_core::link::OfferString {
            store: "https://store.example".into(),
            scid: "QmScid".into(),
            offer_id: "offer".into(),
            s: "secret".into(),
        };
        let refused = service
            .claims()
            .claim_for_main(&crate::identity::types::ClaimOfferParams {
                offer: offer.encode(),
                hint: None,
            })
            .await
            .expect_err("a claim the executor cannot finish is refused");
        assert!(
            refused.message.starts_with("no-store-keys"),
            "{}",
            refused.message
        );
    }
}
