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
    OfferOpened, PublishParams, SignedHead, SignedOffer, SignedRequest, StoreInfo,
};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::db_backend::{db_backend, DbBackend};

pub(super) fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

pub(super) fn error(code: u16, message: impl Into<String>) -> WsRpcError {
    WsRpcError {
        data: None,
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
        IdentityError::NotServed(call) => error(
            503,
            format!("unavailable: the agent language does not serve {call}"),
        ),
        IdentityError::BadHeadSignature => error(
            502,
            "bad-head-signature: the store's head is not signed by a key of identity.storeKeys",
        ),
        IdentityError::Gone(state) => error(410, format!("gone: the offer is {state}")),
        IdentityError::NotFound => WsRpcError::not_found("not-found"),
    }
}

/// Why a `publish` failed, and whether the store certainly did not take the events.
pub(super) struct PublishFailure {
    pub(super) error: WsRpcError,
    /// `true` only when nothing reached the store (a check before the append) or the store
    /// answered that the events are unacceptable (`400`, `403`, a key already in a log). A rate
    /// limit, a `5xx`, a transport failure, a conflict, and anything after the append (the cache)
    /// may follow a commit and are never a refusal.
    pub(super) refused: bool,
}

impl PublishFailure {
    fn refused(error: WsRpcError) -> Self {
        PublishFailure {
            error,
            refused: true,
        }
    }

    fn unsure(error: WsRpcError) -> Self {
        PublishFailure {
            error,
            refused: false,
        }
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
        e @ CacheError::Conflict(_) => error(409, format!("identity-conflict: {e}")),
        e @ CacheError::Behind { .. } => error(503, format!("unavailable: {e}")),
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

/// Runs `f` on the executor's database, a database error as a 500.
pub(super) fn db<T>(
    f: impl FnOnce(&dyn DbBackend) -> crate::db::Ad4mDbResult<T>,
) -> Result<T, WsRpcError> {
    f(db_backend()).map_err(|e| WsRpcError::internal(e.to_string()))
}

/// The identity a local agent (`did:key`) speaks for, or `None` (also before the database and
/// the identity module start, as in unit tests that never open them): see
/// [`IdentityService::identity_of`].
pub async fn identity_of_agent(agent_did: &str) -> Option<String> {
    super::service().ok()?.identity_of(agent_did).await
}

/// The SCID `identity_agents` binds `agent_did` to (`None` also without an open database).
fn bound_identity(agent_did: &str) -> Option<String> {
    db_backend().agent_identity(agent_did).ok().flatten()
}

fn first_fork(forks: &[ad4m_identity_core::ForkEvidence]) -> Option<ForkRecord> {
    forks.first().map(|f| ForkRecord {
        seq: f.seq,
        ours: f.ours.clone(),
        theirs: f.theirs.clone(),
    })
}

impl IdentityService {
    /// The `did:scid` the local agent `agent_did` speaks for: bound to it in `identity_agents`
    /// and its device key listed, valid and without the bot mark at the log's trusted seq
    /// (`resolver::key_speaks_for`, no store read). After a revocation of the key reaches the
    /// trusted seq the agent speaks for no one, so `agent.get` and `agent.status` answer
    /// `identity: null`; the binding itself stays, and `identity.status` shows it with `revoked`.
    pub async fn identity_of(&self, agent_did: &str) -> Option<String> {
        let scid = bound_identity(agent_did)?;
        let key = agent_did.strip_prefix("did:key:")?;
        let did = did_of(&scid);
        super::resolver::key_speaks_for(&self.cache, &did, key, now())
            .await
            .then_some(did)
    }
}

pub(super) fn mirror_head(head: &ad4m_ldk::identity::SignedHead) -> Result<SignedHead, WsRpcError> {
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
    /// Where the store keys came from (`IdentitySettings::store_keys_source`), for the
    /// `no-store-keys` refusal.
    pub store_keys_source: super::StoreSource,
    /// DID resolution (`identity.resolve`) over the same cache and backend.
    pub resolver: Arc<super::resolver::Resolver>,
    /// The open challenges of key logins (`user.sessionChallenge`).
    pub challenges: super::key_login::Challenges,
    /// The origins a key statement's `audience` may name: `public_origins`, else this
    /// executor's own loopback origins — and none on a network listener without
    /// `public_origins` (`Audiences::None`).
    pub audiences: super::key_login::Audiences,
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

    /// The store this executor uses and whether it trusts a key for it; `None` without one.
    pub fn store_info(&self) -> Option<StoreInfo> {
        self.store_url.as_ref().map(|url| StoreInfo {
            url: url.clone(),
            trusted: self.cache.trusts_a_store(),
        })
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
            unlinked: None,
            head: None,
            fork: None,
            store_behind: None,
            store: self.store_info(),
        };
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(status);
        };
        status.did = Some(did_of(&scid));
        if let Some(chain) = self.refreshed_or_cached(&scid).await? {
            let state = chain.state();
            status.key_id = state.key_id_of(&key).cloned();
            let record = status.key_id.as_ref().and_then(|id| state.keys.get(id));
            status.revoked = record.and_then(|record| record.revoked);
            status.unlinked = record.and_then(|record| record.unlinked);
        }
        if let Some(head) = self.cache.accepted_head(&scid).map_err(cache_error)? {
            status.head = Some(mirror_head(&head)?);
        }
        status.fork = first_fork(&self.cache.forks(&scid).await.map_err(cache_error)?);
        status.store_behind = self.cache.store_behind(&scid);
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

    /// Records that `agent` speaks for the log `scid` when its device key is valid, without the
    /// bot mark, at the log's trusted seq (the accepted, store-signed head); the key's id when
    /// listed up to there. On the first binding, publishes the person's profile (a failure is
    /// logged; the next profile publish tries again).
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
        // A bot key acts for the person within its scope only; it never speaks for them, so it
        // binds no agent (as `key_speaks_for` and the person's profile read it).
        if state.bot_scope_at(&id, trusted).is_none()
            && state.key_valid_at_time(&id, trusted, now())
        {
            let bound_before = db(|db| db.agent_identity(&agent_did))?.is_some();
            db(|db| db.set_agent_identity(&agent_did, scid, now()))
                .map_err(|e| error(409, format!("identity-exists: {}", e.message)))?;
            // A local identity now: its log stays in the key map, so the agent's links carry
            // their anchor ([`super::anchor_for`]).
            self.cache.keep_local(scid).await.map_err(cache_error)?;
            // The main agent's status gains its identity (a managed agent's status event names
            // the main agent's, so only the main agent's changes here).
            if agent.is_main_agent() {
                let mut status = crate::agent::AgentService::with_global_instance(|a| a.dump());
                status.identity = self.identity_of(&agent_did).await;
                crate::pubsub::get_global_pubsub()
                    .await
                    .publish_event(crate::events::Event::agent_status(&status));
            }
            if !bound_before {
                if let Err(e) =
                    crate::agent::AgentService::publish_person_profile(agent, self).await
                {
                    log::warn!(
                        "identity: the person's profile of {agent_did} under {} was not published ({e}); the next profile publish tries again",
                        did_of(scid)
                    );
                }
            }
        }
        Ok(Some(id))
    }

    /// Refuses the calls that make the store commit something irreversible (`publish`, a claim)
    /// while the executor trusts no store key (no store, or a store without its keys, in the
    /// config or the seed: `super::no_store_keys_cause`): the cache then accepts no head, so the
    /// executor could never bind the agent to the log, but the store would already hold the
    /// events and the agent's key would be taken for every identity.
    pub(super) fn refuse_without_store_keys(&self) -> Result<(), WsRpcError> {
        if self.cache.trusts_a_store() {
            return Ok(());
        }
        let cause = super::no_store_keys_cause(self.store_url.as_deref(), self.store_keys_source);
        Err(error(
            503,
            format!(
                "no-store-keys: {cause}, so this executor accepts no head from a store and \
                 cannot bind an agent to a log"
            ),
        ))
    }

    /// `identity.publish`: appends through the agent language, then takes the events into the
    /// cache and records the identity for `agent` when the log lists its key. A `409` whose log
    /// already holds these events (the append happened elsewhere) answers the store's head, and
    /// reads the rest of the log when that head lies past them.
    /// Refused with `no-store-keys` while `identity.storeKeys` is empty, and for the main agent
    /// an incept of another identity than the recorded operator's (`first-link`), before the
    /// store commits.
    pub async fn publish(
        &self,
        agent: &AgentContext,
        params: &PublishParams,
    ) -> Result<SignedHead, WsRpcError> {
        self.publish_classified(agent, params)
            .await
            .map_err(|failure| failure.error)
    }

    /// [`IdentityService::publish`], with the failure told whether the store certainly did not
    /// take the events ([`PublishFailure::refused`]).
    pub(super) async fn publish_classified(
        &self,
        agent: &AgentContext,
        params: &PublishParams,
    ) -> Result<SignedHead, PublishFailure> {
        let (events, scid, did) = self
            .checked_before_append(agent, params)
            .map_err(PublishFailure::refused)?;
        let request = ad4m_ldk::identity::AppendRequest {
            events: params.events.clone(),
            vouch: None,
            pow: None,
        };
        let head = match self.backend.append(&did, &request).await {
            Ok(head) => head,
            Err(IdentityError::Conflict(stored)) => self
                .appended_elsewhere(&did, &events, stored)
                .await
                .map_err(PublishFailure::unsure)?,
            Err(e) => {
                // Only a 4xx that names the events as unacceptable says nothing was appended. A
                // rate limit, an outage, a bad head signature and the rest do not say.
                let certain = matches!(
                    e,
                    IdentityError::Invalid(_)
                        | IdentityError::Forbidden(_)
                        | IdentityError::KeyTaken(_)
                );
                let error = backend_error(e);
                return Err(if certain {
                    PublishFailure::refused(error)
                } else {
                    PublishFailure::unsure(error)
                });
            }
        };
        // From here on the store holds the events: no failure below is a refusal.
        self.take_into_cache(agent, &scid, &head, &events)
            .await
            .map_err(PublishFailure::unsure)?;
        mirror_head(&head).map_err(PublishFailure::unsure)
    }

    /// The checks of `publish` that run before anything is sent to the store: the events and
    /// the log they name, and their DID.
    fn checked_before_append(
        &self,
        agent: &AgentContext,
        params: &PublishParams,
    ) -> Result<(Vec<Event>, String, String), WsRpcError> {
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
        // The rule `cosignIncept` runs, again before the store commits: an incept co-signed
        // before the operator record existed must not bind the main key to another identity.
        if agent.is_main_agent() && events.iter().any(|e| matches!(e.op, Op::Incept { .. })) {
            let record = db(|db| db.operator_record())?;
            main_incept_check(record.as_ref(), &did).map_err(first_link_error)?;
        }
        Ok((events, scid, did))
    }

    /// Takes the events the store accepted into the cache, and records the identity for `agent`
    /// when the log lists its key.
    async fn take_into_cache(
        &self,
        agent: &AgentContext,
        scid: &str,
        head: &ad4m_ldk::identity::SignedHead,
        events: &[Event],
    ) -> Result<(), WsRpcError> {
        match self.cache.accept(scid, head, events).await {
            // The store's head lies past these events (a `409` replay of a log that moved on):
            // read the rest of the log, so the head is accepted.
            Ok(Some(ours)) if ours.seq < head.seq => {
                self.cache
                    .refresh(self.backend.as_ref(), scid)
                    .await
                    .map_err(cache_error)?;
            }
            Ok(_) => {}
            // The cache lacks the log's earlier events: read the whole log.
            Err(CacheError::Gap { .. }) => {
                self.cache
                    .refresh(self.backend.as_ref(), scid)
                    .await
                    .map_err(cache_error)?;
            }
            Err(e) => return Err(cache_error(e)),
        }
        self.record_agent_if_listed(agent, scid).await.map(|_| ())
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
                expires: scope.map(|s| s.expires),
                added_seq: record.added,
                revoked_seq: record.revoked,
                unlinked_seq: record.unlinked,
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
    use crate::db::Ad4mDb;
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
            challenges: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
        }
    }

    /// `identity.status` shows a configured store that answers the log behind the cache, from
    /// the first such answer, until it catches up.
    #[tokio::test]
    async fn status_shows_a_store_that_stays_behind_until_it_catches_up() {
        use crate::identity::cache::tests::{trusting, FakeStore};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let user = crate::test_utils::create_test_user(&format!(
            "behind-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, _) = device_key(&agent).unwrap();
        let mut log = TestLog::new(81);
        log.revoke_d1();
        let scid = log.scid();
        db(|db| db.set_agent_identity(&agent_did, &scid, now())).unwrap();
        let store = Arc::new(FakeStore::new(log.events.clone()));
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
            challenges: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
        };
        assert_eq!(service.status(&agent).await.unwrap().store_behind, None);

        // The store loses the last event: it answers seq 0, the cache holds seq 1.
        store.events.lock().unwrap().truncate(1);
        let status = service.status(&agent).await.unwrap();
        let behind = status.store_behind.expect("the store shows as behind");
        assert_eq!(behind.seq, Some(0));
        assert_eq!(
            serde_json::to_value(&status).unwrap()["storeBehind"]["seq"],
            0
        );
        let again = service.status(&agent).await.unwrap().store_behind.unwrap();
        assert_eq!(again.since, behind.since, "since the first answer behind");

        *store.events.lock().unwrap() = log.events.clone();
        assert_eq!(service.status(&agent).await.unwrap().store_behind, None);
    }

    /// `identity.status.store` names the store the executor uses and whether it trusts a key
    /// for it: the seed's store with its keys (trusted), a store URL without keys (untrusted),
    /// and `null` without a store. An agent with no identity gets it too.
    #[tokio::test]
    async fn status_names_the_store_and_whether_a_key_for_it_is_trusted() {
        use crate::identity::cache::tests::{in_memory, trusting, FakeStore};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let user = crate::test_utils::create_test_user(&format!(
            "store-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let service = |cache: KelCache, store_url: Option<&str>| {
            let backend: Arc<dyn IdentityBackend> = Arc::new(FakeStore::new(vec![]));
            let cache = Arc::new(cache);
            IdentityService {
                resolver: Arc::new(crate::identity::resolver::Resolver::new(
                    cache.clone(),
                    backend.clone(),
                    store_url,
                    false,
                )),
                backend,
                cache,
                store_url: store_url.map(String::from),
                challenges: Default::default(),
                audiences: Default::default(),
                store_keys_source: crate::identity::StoreSource::Config,
            }
        };

        let seeded = service(trusting(), Some("https://store.ad4m.dev"));
        let status = seeded.status(&agent).await.unwrap();
        assert_eq!(
            status.store,
            Some(StoreInfo {
                url: "https://store.ad4m.dev".into(),
                trusted: true,
            })
        );
        assert_eq!(
            serde_json::to_value(&status).unwrap()["store"],
            serde_json::json!({ "url": "https://store.ad4m.dev", "trusted": true })
        );

        let keyless = service(in_memory(vec![]), Some("http://127.0.0.1:9"));
        assert_eq!(
            keyless.status(&agent).await.unwrap().store,
            Some(StoreInfo {
                url: "http://127.0.0.1:9".into(),
                trusted: false,
            })
        );

        let none = service(in_memory(vec![]), None);
        let status = none.status(&agent).await.unwrap();
        assert_eq!(status.store, None);
        assert_eq!(
            serde_json::to_value(&status).unwrap()["store"],
            serde_json::Value::Null
        );
    }

    /// `no-store-keys` names its cause: no store, a configured store URL without its keys, or
    /// the seed's store without keys.
    #[test]
    fn the_no_store_keys_refusal_names_its_cause() {
        use crate::identity::cache::tests::{in_memory, FakeStore};
        let refusal_from = |store_url: Option<&str>, store_keys_source| {
            let backend: Arc<dyn IdentityBackend> = Arc::new(FakeStore::new(vec![]));
            let cache = Arc::new(in_memory(vec![]));
            IdentityService {
                resolver: Arc::new(crate::identity::resolver::Resolver::new(
                    cache.clone(),
                    backend.clone(),
                    store_url,
                    false,
                )),
                backend,
                cache,
                store_url: store_url.map(String::from),
                challenges: Default::default(),
                audiences: Default::default(),
                store_keys_source,
            }
            .refuse_without_store_keys()
            .unwrap_err()
        };
        let refusal =
            |store_url: Option<&str>| refusal_from(store_url, crate::identity::StoreSource::Config);
        let none = refusal(None);
        assert_eq!(none.code, 503);
        assert!(
            none.message.starts_with("no-store-keys: "),
            "{}",
            none.message
        );
        assert!(
            none.message.contains("has no identity store"),
            "{}",
            none.message
        );
        let keyless = refusal(Some("http://127.0.0.1:9"));
        assert!(
            keyless.message.starts_with("no-store-keys: "),
            "{}",
            keyless.message
        );
        assert!(
            keyless.message.contains("http://127.0.0.1:9")
                && keyless.message.contains("without its keys")
                && keyless.message.contains("identity.storeKeys"),
            "{}",
            keyless.message
        );
        // The seed's store with no keys: the refusal names the seed, not identity.storeKeys.
        let seeded = refusal_from(
            Some("https://seed.store"),
            crate::identity::StoreSource::Seed,
        );
        assert!(
            seeded.message.starts_with("no-store-keys: ")
                && seeded.message.contains("bootstrap seed")
                && seeded.message.contains("https://seed.store")
                && !seeded.message.contains("identity.storeKeys"),
            "{}",
            seeded.message
        );
    }

    /// The first-link rule runs inside `claim_for_main`: while the main key is in no log, an
    /// offer of an identity other than the recorded operator's is refused `first-link`, and
    /// with no record `no-operator`, before the claim reaches the store.
    #[tokio::test]
    async fn claim_for_main_refuses_an_offer_of_another_identity_than_the_operator() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let service = test_service();
        let operator = did_of(&TestLog::new(5).scid());
        let offer = ad4m_identity_core::link::OfferString {
            store: "https://store.example".into(),
            scid: TestLog::new(6).scid(),
            offer_id: bs58::encode([2u8; 16]).into_string(),
            s: bs58::encode([1u8; 32]).into_string(),
        };
        let claim = || async {
            service
                .claims()
                .claim_for_main(&crate::identity::types::ClaimOfferParams {
                    offer: offer.encode(),
                    hint: None,
                })
                .await
                .expect_err("the claim is refused")
        };

        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(Some(&operator)))
            .unwrap();
        let refused = claim().await;
        assert!(
            refused.message.starts_with("first-link"),
            "{}",
            refused.message
        );
        assert_eq!(refused.code, 403);

        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let refused = claim().await;
        assert!(
            refused.message.starts_with("no-operator"),
            "{}",
            refused.message
        );
    }

    #[tokio::test]
    async fn with_an_operator_record_the_main_agent_co_signs_no_incept() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
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
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
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

    /// A bot key acts for its person within its scope, never as them: a log that lists the
    /// agent's key as a bot, even unexpired and store-signed, binds no agent.
    #[tokio::test]
    async fn a_bot_key_binds_no_agent() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        use crate::identity::cache::tests::accept_signed;
        use ad4m_identity_core::build::{add_unsigned, cosign};
        use ad4m_identity_core::{BotScope, KeyLife};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let service = test_service();
        let user = crate::test_utils::create_test_user(&format!(
            "bot-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, key) = device_key(&agent).unwrap();

        let mut log = TestLog::new(63);
        let chain = log.chain();
        let device = ad4m_identity_core::DeviceKey {
            id: chain.state().next_device_id(),
            enc: enc_key_of(&key).unwrap(),
            sign: key,
            life: KeyLife::Bot(BotScope {
                can: vec!["read".into()],
                expires: 4_000_000_000,
            }),
        };
        let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut add, "m0", &log.master).unwrap();
        log.events.push(add);
        let scid = log.scid();
        accept_signed(&service.cache, &log.events).await;

        assert_eq!(
            service.record_agent_if_listed(&agent, &scid).await.unwrap(),
            Some("d2".to_string()),
            "the key is listed"
        );
        assert_eq!(db(|db| db.agent_identity(&agent_did)).unwrap(), None);
    }

    /// After a fork the cache keeps its branch; `resetLog` drops this executor's copy, and the
    /// next read takes the store's branch: a key revoked on the store's branch speaks no more.
    #[tokio::test]
    async fn a_reset_log_takes_the_stores_branch_after_a_fork() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        use crate::identity::cache::tests::{trusting, FakeStore};
        use crate::identity::resolver::key_valid_at_anchor;
        use crate::identity::resolver::test_profiles::{signed_profile, with_record};
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
            challenges: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
        };
        let scid = ours.scid();
        let did = did_of(&scid);
        let d1 = ours.device.key();
        service.log(&did, 0, true).await.unwrap();
        // Both branches hold seq 1; theirs revokes d1 there.
        assert!(key_valid_at_anchor(&service.cache, &did, &d1, 1, now()).await);

        // The store now serves the other branch: the cache records the fork, keeps its own.
        *store.events.lock().unwrap() = theirs.events.clone();
        service.log(&did, 0, true).await.unwrap();
        assert_eq!(service.cache.forks(&scid).await.unwrap().len(), 1);
        assert!(
            key_valid_at_anchor(&service.cache, &did, &d1, 1, now()).await,
            "the cache keeps its branch"
        );

        assert!(service.reset_log(&did).await.unwrap());
        assert!(service.cache.chain(&scid).await.unwrap().is_none());
        assert!(service.cache.accepted_head(&scid).unwrap().is_none());
        assert!(service.cache.forks(&scid).await.unwrap().is_empty());

        // An ordinary verification reads the log again and takes the store's branch.
        let anchored = with_record(&signed_profile(&did, &ours.device), &ours.device, 1, 1);
        let expr: crate::types::Expression<Value> =
            serde_json::from_value(anchored.clone()).unwrap();
        let reader: &dyn IdentityBackend = store.as_ref();
        assert!(
            !crate::identity::verify_expression(
                &service.cache,
                Some(reader),
                &expr,
                &anchored["proof"]["record"]
            )
            .await
            .unwrap(),
            "d1 is revoked at seq 1 on the store's branch"
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
        /// Answers every append `409` with the head of the log it holds, appending nothing.
        conflict: bool,
        /// Claims taken (each counts, whether or not its answer arrives).
        claims: std::sync::atomic::AtomicUsize,
        /// When set, a claim fails with this error, after it counted.
        claim_error: std::sync::Mutex<Option<IdentityError>>,
        /// What `offer_state` answers for every offer.
        offer: std::sync::Mutex<String>,
        /// The next append fails once: with the error before anything is committed (`false`), or
        /// after the store committed (`true`: the answer is lost).
        append_fault: std::sync::Mutex<Option<(IdentityError, bool)>>,
        /// While set, reading the log fails (a store outage on the read).
        log_fails: std::sync::atomic::AtomicBool,
        /// While set, reading the log fails from the first append on.
        log_fails_after_append: std::sync::atomic::AtomicBool,
    }

    impl RecordingStore {
        fn new(events: Vec<Event>, conflict: bool) -> Self {
            RecordingStore {
                inner: crate::identity::cache::tests::FakeStore::new(events),
                appends: std::sync::atomic::AtomicUsize::new(0),
                conflict,
                claims: std::sync::atomic::AtomicUsize::new(0),
                claim_error: std::sync::Mutex::new(None),
                offer: std::sync::Mutex::new("claimed".into()),
                append_fault: std::sync::Mutex::new(None),
                log_fails: std::sync::atomic::AtomicBool::new(false),
                log_fails_after_append: std::sync::atomic::AtomicBool::new(false),
            }
        }
    }

    /// A service on `backend` whose cache trusts the test store key.
    fn service_on(backend: Arc<RecordingStore>) -> IdentityService {
        let cache = Arc::new(crate::identity::cache::tests::trusting());
        let as_backend: Arc<dyn IdentityBackend> = backend;
        IdentityService {
            resolver: Arc::new(crate::identity::resolver::Resolver::new(
                cache.clone(),
                as_backend.clone(),
                None,
                false,
            )),
            backend: as_backend,
            cache,
            store_url: None,
            challenges: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
        }
    }

    #[async_trait::async_trait]
    impl IdentityBackend for RecordingStore {
        async fn log(
            &self,
            did: &str,
            from: u64,
        ) -> Result<Option<ad4m_ldk::identity::LogPage>, IdentityError> {
            if self.log_fails.load(std::sync::atomic::Ordering::SeqCst)
                || (self
                    .log_fails_after_append
                    .load(std::sync::atomic::Ordering::SeqCst)
                    && self.appends.load(std::sync::atomic::Ordering::SeqCst) > 1)
            {
                return Err(IdentityError::Unavailable("the read failed".into()));
            }
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
            let fault = self.append_fault.lock().unwrap().take();
            if let Some((error, false)) = fault {
                return Err(error);
            }
            if self.conflict {
                let held = self.inner.events.lock().unwrap().clone();
                let chain = ad4m_identity_core::fold(&held).unwrap();
                let head = chain.head();
                return Err(IdentityError::Conflict(self.inner.key.sign_head(
                    chain.id(),
                    head.seq,
                    &head.hash,
                    1,
                )));
            }
            let events: Vec<Event> = req
                .events
                .iter()
                .map(|e| Event::from_json(e).unwrap())
                .collect();
            let mut held = self.inner.events.lock().unwrap();
            held.extend(events);
            if let Some((error, true)) = fault {
                return Err(error);
            }
            let chain = ad4m_identity_core::fold(&held).unwrap();
            let head = chain.head();
            Ok(self
                .inner
                .key
                .sign_head(chain.id(), head.seq, &head.hash, 1))
        }
        async fn put_agent(
            &self,
            record: &ad4m_ldk::identity::AgentRecord,
            req: Option<&ad4m_ldk::identity::SignedRequest>,
        ) -> Result<(), IdentityError> {
            self.inner.put_agent(record, req).await
        }
        async fn agent_version(&self, did: &str) -> Result<Option<u64>, IdentityError> {
            self.inner.agent_version(did).await
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
            _did: &str,
            _offer_id: &str,
            _claim: &ad4m_ldk::identity::Claim,
        ) -> Result<ad4m_ldk::identity::ClaimReceipt, IdentityError> {
            self.claims
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            if let Some(error) = self.claim_error.lock().unwrap().clone() {
                return Err(error);
            }
            Ok(ad4m_ldk::identity::ClaimReceipt {
                state: "claimed".into(),
                kind: ad4m_ldk::identity::OfferKind::Device,
                expires: u64::MAX,
            })
        }
        async fn offer_state(
            &self,
            _did: &str,
            _offer_id: &str,
            _key: &str,
            _wait_s: u8,
        ) -> Result<ad4m_ldk::identity::OfferProgress, IdentityError> {
            let state = self.offer.lock().unwrap().clone();
            let seq = self
                .inner
                .events
                .lock()
                .unwrap()
                .len()
                .checked_sub(1)
                .map(|seq| seq as u64)
                .filter(|_| state == "spent");
            Ok(ad4m_ldk::identity::OfferProgress { state, seq })
        }
    }

    /// With no store key (`identity.storeUrl` set, `identity.storeKeys` unset), the executor
    /// can accept no store head, so a publish can never bind the agent. It must refuse before
    /// the store commits the events: once the store holds the incept, the device key is taken
    /// for every identity, forever.
    #[tokio::test]
    async fn without_store_keys_publish_refuses_before_the_store_commits() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let user = crate::test_utils::create_test_user(&format!(
            "nokeys-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
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
            challenges: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
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

    /// An incept the main agent co-signed before the operator record existed is refused
    /// `first-link` at `publish`, before the store commits it.
    #[tokio::test]
    async fn publish_runs_the_first_link_rule_for_the_main_agents_incept() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = AgentContext::main_agent();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let cosigned = service
            .cosign_incept(&main, &incept_listing(&main, 54))
            .await
            .unwrap();

        let operator = did_of(&TestLog::new(4).scid());
        let _record = OperatorRecord::set(&operator);
        let refused = service
            .publish(
                &main,
                &PublishParams {
                    events: vec![cosigned],
                },
            )
            .await
            .expect_err("refused");
        assert_eq!(refused.code, 403, "{refused:?}");
        assert!(
            refused.message.starts_with("first-link"),
            "{}",
            refused.message
        );
        assert_eq!(
            backend.appends.load(std::sync::atomic::Ordering::SeqCst),
            0,
            "the store was not asked"
        );
    }

    /// A `409` replay whose store head lies past the submitted events (the log moved on since)
    /// reads the rest of the log, so the head is accepted and the agent bound.
    #[tokio::test]
    async fn a_replayed_publish_behind_the_stores_head_reads_the_rest_and_binds() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        use ad4m_identity_core::build::{add_unsigned, cosign, Signer};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let user = crate::test_utils::create_test_user(&format!(
            "replay-{}@example.org",
            uuid::Uuid::new_v4().simple()
        ));
        let agent = AgentContext::managed(user);
        let (agent_did, _) = device_key(&agent).unwrap();
        let service = test_service();
        let incept = service
            .cosign_incept(&agent, &incept_listing(&agent, 55))
            .await
            .unwrap();
        let incept = Event::from_json(&incept).unwrap();
        // The store holds the incept and, since, an `add` of a second device.
        let master = ed25519_dalek::SigningKey::from_bytes(&[55; 32]);
        let chain = ad4m_identity_core::fold(std::slice::from_ref(&incept)).unwrap();
        let other = ed25519_dalek::SigningKey::from_bytes(&[56; 32]);
        let device = ad4m_identity_core::DeviceKey {
            id: chain.state().next_device_id(),
            sign: other.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[56; 32]),
            life: ad4m_identity_core::KeyLife::Standing,
        };
        let mut add = add_unsigned(&chain, device, "offer", "co-sig").unwrap();
        cosign(&mut add, "m0", &master).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![incept.clone(), add], true));
        let service = service_on(backend);

        let head = service
            .publish(
                &agent,
                &PublishParams {
                    events: vec![serde_json::to_value(&incept).unwrap()],
                },
            )
            .await
            .unwrap();
        assert_eq!(head.seq, 1);
        let (_, trusted) = service.cache.trusted(&incept.scid).await.unwrap().unwrap();
        assert_eq!(trusted, 1, "the store's head is accepted");
        assert_eq!(
            db(|db| db.agent_identity(&agent_did)).unwrap(),
            Some(incept.scid.clone())
        );
    }

    /// The join row goes in before the store call. A claim the store refused leaves it, and
    /// `joinStatus` reports the offer's state; a claim the store committed whose answer was
    /// lost leaves it too, so once the add spent the offer `joinStatus` binds the main agent
    /// (a retry would get `gone`).
    #[tokio::test]
    async fn a_claim_keeps_its_join_row_when_the_store_call_fails() {
        let _op = crate::test_utils::OPERATOR_RECORD.lock().await;
        use crate::identity::types::ClaimOfferParams;
        use ad4m_identity_core::link::OfferString;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = AgentContext::main_agent();
        let (main_did, _) = device_key(&main).unwrap();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let incept = test_service()
            .cosign_incept(&main, &incept_listing(&main, 57))
            .await
            .unwrap();
        let incept = Event::from_json(&incept).unwrap();
        let _record = OperatorRecord::set(&did_of(&incept.scid));
        let backend = Arc::new(RecordingStore::new(vec![incept.clone()], false));
        let service = service_on(backend.clone());
        let offer = |id: u8| OfferString {
            store: "https://store.example".into(),
            scid: incept.scid.clone(),
            offer_id: bs58::encode([id; 16]).into_string(),
            s: bs58::encode([1u8; 32]).into_string(),
        };
        let params = |offer: &OfferString| ClaimOfferParams {
            offer: offer.encode(),
            hint: None,
        };
        let claims = || backend.claims.load(std::sync::atomic::Ordering::SeqCst);

        // The store refuses the claim: the row stays and reports the offer's state.
        *backend.claim_error.lock().unwrap() = Some(IdentityError::Gone("expired".into()));
        let refused = offer(3);
        service
            .claims()
            .claim_for_main(&params(&refused))
            .await
            .expect_err("the store refused");
        assert_eq!(claims(), 1);
        *backend.offer.lock().unwrap() = "expired".into();
        let status = service
            .claims()
            .join_status(&main, &refused.offer_id)
            .await
            .expect("the row is there");
        assert_eq!(status.state, "expired");
        assert_eq!(status.key_id, None);
        assert_eq!(db(|db| db.agent_identity(&main_did)).unwrap(), None);

        // The store commits the claim and its answer is lost: once the add spends the offer,
        // `joinStatus` finds the row and binds the main agent.
        *backend.claim_error.lock().unwrap() =
            Some(IdentityError::Unavailable("the answer was lost".into()));
        let lost = offer(4);
        service
            .claims()
            .claim_for_main(&params(&lost))
            .await
            .expect_err("the answer was lost");
        assert_eq!(claims(), 2);
        *backend.offer.lock().unwrap() = "spent".into();
        let status = service
            .claims()
            .join_status(&main, &lost.offer_id)
            .await
            .expect("the row is there");
        assert_eq!(status.state, "spent");
        assert_eq!(status.key_id.as_deref(), Some("d1"));
        assert_eq!(
            db(|db| db.agent_identity(&main_did)).unwrap(),
            Some(incept.scid.clone())
        );
    }
    // ---- `identity.unlinkSelf` ----

    /// A signed-in agent on `agent`: a grant that the sign-out must end.
    fn grant_on(agent: &AgentContext) -> Arc<crate::agent::grants::LiveGrant> {
        crate::agent::grants::mint(
            agent.clone(),
            crate::agent::capabilities::AuthInfo {
                app_name: "an app".into(),
                ..Default::default()
            },
            Some(format!("unlink-{}", uuid::Uuid::new_v4())),
            3600,
            None,
        )
        .expect("a grant")
        .1
    }

    /// An agent service with a passphrase and its files in a fresh directory, as a re-key and a
    /// key retirement need.
    fn unlocked_agent_service() -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("ad4m")).unwrap();
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::AgentService::init_global_instance(dir.path().to_string_lossy().into_owned());
        crate::AgentService::with_mutable_global_instance(|svc| {
            svc.create_new_keys();
            svc.passphrase = Some("unlink".into());
            svc.store_agent_profile();
            svc.save("unlink".into());
        });
        dir
    }

    /// `agent` bound to a new person (master seed `seed`) in `service`'s store.
    async fn linked(service: &IdentityService, agent: &AgentContext, seed: u8) -> String {
        let incept = service
            .cosign_incept(agent, &incept_listing(agent, seed))
            .await
            .unwrap();
        let scid = Event::from_json(&incept).unwrap().scid;
        service
            .publish(
                agent,
                &PublishParams {
                    events: vec![incept],
                },
            )
            .await
            .unwrap();
        let (agent_did, _) = device_key(agent).unwrap();
        assert_eq!(
            db(|db| db.agent_identity(&agent_did)).unwrap(),
            Some(scid.clone())
        );
        scid
    }

    fn user_agent(tag: &str) -> AgentContext {
        AgentContext::managed(crate::test_utils::create_test_user(&format!(
            "{tag}-{}@example.org",
            uuid::Uuid::new_v4().simple()
        )))
    }

    /// The key `name` (a retired key of `did`) holds no secret: not in memory, and not in the
    /// keystore a save writes and a restart reads. Its public key is still known, and it still
    /// answers the DID document, so what it signed verifies and it is listed.
    fn assert_secret_destroyed(name: &str, did: &str) {
        let backend = crate::wallet::wallet_backend();
        assert!(backend.get_secret_key(name).is_none(), "the secret is gone");
        assert!(
            backend.sign(name, b"new link").is_none(),
            "it signs nothing"
        );
        assert_eq!(backend.get_did_document(name).expect("listed").id, did);
        let public = backend.get_public_key(name).expect("the public key stays");
        // Saved and read back, as after a restart.
        let cipher = backend.export("unlink");
        let mut reloaded = crate::wallet::Wallet::new();
        reloaded.load(cipher);
        reloaded
            .unlock("unlink".into())
            .expect("the keystore opens");
        let name = name.to_string();
        assert!(reloaded.get_secret_key(&name).is_none());
        assert!(reloaded.sign(&name, b"new link").is_none());
        assert_eq!(reloaded.get_public_key(&name), Some(public));
        assert!(reloaded.get_secret_key(&did.to_string()).is_none());
    }

    /// A user's device unlinks itself: the `unlink` reaches the store signed by the device key
    /// (no master key), the key leaves the wallet's working keys, the agent's grants end, and
    /// `identity.status` shows the key `unlinked`. A second call is refused: the key is dead.
    #[tokio::test]
    async fn a_device_unlinks_itself_and_its_key_is_dead_here() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("unlink");
        let (agent_did, key) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 70).await;
        let grant = grant_on(&agent);
        let other = grant_on(&user_agent("other"));

        let head = service.unlink_self(&agent).await.expect("unlinked");

        let held = backend.inner.events.lock().unwrap().clone();
        let unlink = held.last().unwrap();
        assert_eq!(unlink.seq, head.seq);
        assert!(matches!(unlink.op, Op::Unlink { .. }), "{:?}", unlink.op);
        assert_eq!(unlink.proof.len(), 1);
        assert!(
            unlink.proof[0].verification_method.ends_with("d1"),
            "signed by the device key itself: {}",
            unlink.proof[0].verification_method
        );
        assert_eq!(held.len(), 2);
        assert!(grant.is_ended(), "the agent is signed out");
        assert!(!other.is_ended(), "another agent is not");
        assert!(crate::AgentService::wallet_key_for_did(&agent_did).is_err());
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
        let status = service.status(&agent).await.unwrap();
        assert_eq!(status.unlinked, Some(head.seq));
        assert_eq!(status.revoked, None);
        assert_eq!(status.device_key, key);

        let again = service.unlink_self(&agent).await.expect_err("dead");
        assert_eq!(again.code, 409);
        assert!(again.message.starts_with("key-dead"), "{}", again.message);
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 2);
    }

    /// A device whose `unlink` the store committed but whose answer was lost: the intent and
    /// the fence stay, the key still works for nothing (signing is refused), and the retry
    /// finishes the cleanup instead of answering `key-dead`, with no second append.
    #[tokio::test]
    async fn an_unlink_whose_answer_is_lost_is_finished_by_the_retry() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("lost");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 72).await;
        let grant = grant_on(&agent);
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("timed out".into()), true));

        let lost = service.unlink_self(&agent).await.expect_err("no answer");

        assert_eq!(lost.code, 503);
        assert_eq!(
            backend.inner.events.lock().unwrap().len(),
            2,
            "it committed"
        );
        assert!(
            db(|db| db.unlink_intent(&agent_did)).unwrap().is_some(),
            "the intent stays"
        );
        assert!(crate::agent::fence::is_raised(&agent_did));
        assert!(crate::AgentService::wallet_key_for_did(&agent_did).is_ok());
        assert!(!grant.is_ended());
        let refused = crate::agent::sign_for_context(b"a link", &agent).expect_err("fenced");
        assert!(refused.to_string().contains("being unlinked"), "{refused}");

        let head = service
            .unlink_self(&agent)
            .await
            .expect("the retry cleans up");

        assert_eq!(head.seq, 1);
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 2);
        assert!(grant.is_ended());
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
        let dead = service
            .unlink_self(&agent)
            .await
            .expect_err("now it is dead");
        assert!(dead.message.starts_with("key-dead"), "{}", dead.message);
    }

    /// A failure before the store committed that does not say so (a 5xx): the intent stays, and
    /// the retry publishes the `unlink` again, then cleans up.
    #[tokio::test]
    async fn an_ambiguous_failure_keeps_the_intent_and_the_retry_publishes_again() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("ambiguous");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 73).await;
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("down".into()), false));

        let failed = service.unlink_self(&agent).await.expect_err("unavailable");

        assert_eq!(failed.code, 503);
        assert_eq!(
            backend.inner.events.lock().unwrap().len(),
            1,
            "nothing committed"
        );
        assert!(db(|db| db.unlink_intent(&agent_did)).unwrap().is_some());
        assert!(crate::agent::fence::is_raised(&agent_did));

        let head = service
            .unlink_self(&agent)
            .await
            .expect("the retry publishes");

        assert_eq!(head.seq, 1);
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 3);
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    /// A store that refuses the `unlink` for sure (a 4xx) leaves nothing behind: the intent and
    /// the fence go, and the key works as before.
    #[tokio::test]
    async fn a_refused_unlink_clears_the_intent_and_the_fence() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("refused");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 74).await;
        let grant = grant_on(&agent);
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Invalid("not today".into()), false));

        let refused = service.unlink_self(&agent).await.expect_err("refused");

        assert_eq!(refused.code, 400);
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
        assert!(!grant.is_ended());
        assert!(crate::agent::sign_for_context(b"a link", &agent).is_ok());
        assert!(crate::wallet::wallet_backend()
            .get_secret_key(&crate::AgentService::wallet_key_for_did(&agent_did).unwrap())
            .is_some());
    }

    /// A restart with a pending intent: the fence is loaded from the database before anything
    /// signs, and `resume_pending` finishes the unlink (here: the store holds it already).
    #[tokio::test]
    async fn a_restart_finishes_a_pending_unlink() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("restart");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 75).await;
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("lost".into()), true));
        service.unlink_self(&agent).await.expect_err("no answer");

        // The process stops: the in-memory fence is gone, the intent is in the database.
        crate::agent::fence::lift(&agent_did);
        assert!(crate::agent::sign_for_context(b"x", &agent).is_ok());
        let intents = db(|db| db.unlink_intents()).unwrap();
        crate::agent::fence::load(intents.into_iter().map(|(did, _)| did));
        assert!(crate::agent::sign_for_context(b"x", &agent).is_err());

        service.resume_pending().await;

        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    /// A key the log shows unlinked whose secret is still here (no intent, as after a crash
    /// before the intent was stored, or an unlink by another device) is cleaned up by the call,
    /// not refused as `key-dead`.
    #[tokio::test]
    async fn a_key_the_log_shows_unlinked_is_cleaned_up_by_the_call() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("dead-in-log");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 76).await;
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("lost".into()), true));
        service.unlink_self(&agent).await.expect_err("no answer");
        db(|db| db.clear_unlink_intent(&agent_did)).unwrap();
        crate::agent::fence::lift(&agent_did);

        let head = service.unlink_self(&agent).await.expect("cleaned up");

        assert_eq!(head.seq, 1);
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 2);
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
    }

    /// Only the `unlink` signs with a fenced key.
    #[tokio::test]
    async fn a_fenced_key_signs_nothing_but_its_unlink() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("fenced");
        let other = user_agent("unfenced");
        let (agent_did, _) = device_key(&agent).unwrap();

        crate::agent::fence::raise(&agent_did);
        let refused = crate::agent::sign_for_context(b"a link", &agent);
        let signer = WalletSigner::new(&agent).unwrap();
        ad4m_identity_core::build::Signer::sign(&signer, b"a link");
        let signer_failed = signer.check();
        let unlink_signer = WalletSigner::for_unlink(&agent).unwrap();
        ad4m_identity_core::build::Signer::sign(&unlink_signer, b"the unlink");
        let unlink_ok = unlink_signer.check();
        let other_signs = crate::agent::sign_for_context(b"a link", &other);
        crate::agent::fence::lift(&agent_did);
        let after = crate::agent::sign_for_context(b"a link", &agent);

        assert!(refused.is_err());
        assert!(signer_failed.is_err());
        assert!(unlink_ok.is_ok());
        assert!(other_signs.is_ok());
        assert!(after.is_ok());
    }

    /// A key another device ended is destroyed here and its agent signed out; an agent whose
    /// own unlink is pending is left to its cleanup.
    #[tokio::test]
    async fn a_key_another_device_ended_is_destroyed_here() {
        use crate::identity::verdict::{KeyVerdict, VerdictChange};
        let _dir = unlocked_agent_service();
        let agent = user_agent("remote");
        let (agent_did, key) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &agent, 77).await;
        let grant = grant_on(&agent);
        let change = VerdictChange {
            key,
            scid: scid.clone(),
            verdict: KeyVerdict::Revoked,
        };

        // Its own unlink is pending: the cleanup of that signs it out, not this.
        db(|db| db.set_unlink_intent(&agent_did, &scid, 1)).unwrap();
        crate::identity::unlink::end_dead_key(&change).await;
        assert!(!grant.is_ended());
        assert!(crate::AgentService::wallet_key_for_did(&agent_did).is_ok());
        db(|db| db.clear_unlink_intent(&agent_did)).unwrap();

        crate::identity::unlink::end_dead_key(&change).await;

        assert!(grant.is_ended());
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    /// The verdict channel lagged and the change was lost: the sweep checks every local agent
    /// against its own trusted log and ends the key the log shows unlinked.
    #[tokio::test]
    async fn the_sweep_after_a_lag_ends_a_key_the_log_shows_unlinked() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("lagged");
        let alive = user_agent("alive");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &agent, 78).await;
        let grant = grant_on(&agent);
        let alive_grant = grant_on(&alive);
        // The store holds an `unlink` of the key that this executor never heard of as a change.
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("lost".into()), true));
        service.unlink_self(&agent).await.expect_err("no answer");
        db(|db| db.clear_unlink_intent(&agent_did)).unwrap();
        crate::agent::fence::lift(&agent_did);
        service
            .cache
            .refresh(service.backend.as_ref(), &scid)
            .await
            .unwrap();
        assert!(!grant.is_ended());

        crate::identity::unlink::sweep_dead_keys(&service.cache).await;

        assert!(grant.is_ended());
        assert!(!alive_grant.is_ended());
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
    }

    /// A device whose first `unlink` committed but whose answer was lost, and whose retry the
    /// store then refuses (429, or a 400 before it looked at the log), with the log unreadable
    /// for the retry (so its cache is stale and the reconciliation read fails too): the first
    /// attempt's intent and fence stay, and the key still signs nothing. Once the store answers
    /// again, a retry finishes the cleanup.
    async fn lost_then_refused_retry(refusal: IdentityError, tag: &str, seed: u8) {
        let _dir = unlocked_agent_service();
        let agent = user_agent(tag);
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, seed).await;
        *backend.append_fault.lock().unwrap() =
            Some((IdentityError::Unavailable("timed out".into()), true));
        service.unlink_self(&agent).await.expect_err("no answer");
        assert_eq!(
            backend.inner.events.lock().unwrap().len(),
            2,
            "it committed"
        );

        backend
            .log_fails
            .store(true, std::sync::atomic::Ordering::SeqCst);
        *backend.append_fault.lock().unwrap() = Some((refusal, false));
        service.unlink_self(&agent).await.expect_err("refused");

        assert!(
            db(|db| db.unlink_intent(&agent_did)).unwrap().is_some(),
            "the first attempt's intent stays"
        );
        assert!(crate::agent::fence::is_raised(&agent_did));
        assert!(crate::agent::sign_for_context(b"a link", &agent).is_err());
        assert!(crate::AgentService::wallet_key_for_did(&agent_did).is_ok());

        backend
            .log_fails
            .store(false, std::sync::atomic::Ordering::SeqCst);
        service
            .unlink_self(&agent)
            .await
            .expect("the retry cleans up");
        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    #[tokio::test]
    async fn a_rate_limited_retry_keeps_the_intent_of_a_lost_attempt() {
        lost_then_refused_retry(
            IdentityError::RateLimited {
                scope: ad4m_ldk::identity::RateLimitScope::Address,
                retry_after_s: 5,
            },
            "limited",
            80,
        )
        .await;
    }

    #[tokio::test]
    async fn a_refused_retry_with_an_unreadable_log_keeps_the_intent_of_a_lost_attempt() {
        lost_then_refused_retry(IdentityError::Invalid("no".into()), "unreadable", 81).await;
    }

    /// A first attempt the store refuses for sure, with the log unreadable afterwards, keeps its
    /// intent: nothing shows that the store did not take the event. (The log goes unreadable
    /// when the append is made, so the read before it still works.)
    #[tokio::test]
    async fn a_refusal_whose_reconciliation_read_fails_keeps_its_own_intent() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("read-fails");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 82).await;
        *backend.append_fault.lock().unwrap() = Some((IdentityError::Invalid("no".into()), false));
        backend
            .log_fails_after_append
            .store(true, std::sync::atomic::Ordering::SeqCst);

        service.unlink_self(&agent).await.expect_err("refused");

        assert!(db(|db| db.unlink_intent(&agent_did)).unwrap().is_some());
        assert!(crate::agent::fence::is_raised(&agent_did));
        crate::agent::fence::lift(&agent_did);
        db(|db| db.clear_unlink_intent(&agent_did)).unwrap();
    }

    /// The end of a key seen while the wallet is locked is not forgotten: the key is fenced and
    /// an intent kept; the unlock's `resume_pending` destroys it.
    #[tokio::test]
    async fn a_remote_end_seen_while_the_wallet_is_locked_is_finished_at_unlock() {
        use crate::identity::verdict::{KeyVerdict, VerdictChange};
        let _dir = unlocked_agent_service();
        let agent = user_agent("locked");
        let (agent_did, key) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &agent, 83).await;
        let grant = grant_on(&agent);
        let change = VerdictChange {
            key,
            scid: scid.clone(),
            verdict: KeyVerdict::Revoked,
        };

        crate::wallet::wallet_backend().lock("unlink");
        crate::identity::unlink::end_dead_key(&change).await;
        crate::wallet::wallet_backend()
            .unlock("unlink")
            .expect("unlocks");

        assert!(grant.is_ended());
        assert!(
            db(|db| db.unlink_intent(&agent_did)).unwrap().is_some(),
            "the end is recorded"
        );
        assert!(crate::agent::fence::is_raised(&agent_did));
        assert!(
            crate::agent::sign_for_context(b"a link", &agent).is_err(),
            "the ended key does not sign after the unlock"
        );

        service.resume_pending().await;

        assert_secret_destroyed(
            &crate::agent::rekey::retired_managed_key_name(&agent_did),
            &agent_did,
        );
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    /// The main agent's key ended while the wallet was locked: the unlock re-keys it.
    #[tokio::test]
    async fn a_remote_end_of_the_main_key_seen_while_locked_rekeys_at_unlock() {
        use crate::identity::verdict::{KeyVerdict, VerdictChange};
        let _dir = unlocked_agent_service();
        let main = AgentContext::main_agent();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let old = crate::agent::did();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &main, 84).await;
        let change = VerdictChange {
            key: old.trim_start_matches("did:key:").to_string(),
            scid,
            verdict: KeyVerdict::Revoked,
        };

        crate::wallet::wallet_backend().lock("unlink");
        crate::identity::unlink::end_dead_key(&change).await;
        crate::wallet::wallet_backend()
            .unlock("unlink")
            .expect("unlocks");
        assert!(crate::agent::fence::is_raised(&old));
        assert_eq!(crate::agent::did(), old, "not re-keyed yet");

        service.resume_pending().await;

        assert_ne!(crate::agent::did(), old, "a new did:key");
        assert_secret_destroyed(&crate::agent::rekey::retired_key_name(&old), &old);
        assert_eq!(db(|db| db.unlink_intent(&old)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&old));
    }

    /// The legacy keystore backup holds the secrets of the wallet as it was before the rewrite:
    /// the deletion removes it, and when it cannot, the intent stays and the retry finishes.
    #[tokio::test]
    async fn the_deletion_removes_the_legacy_backup_or_keeps_the_intent() {
        let dir = unlocked_agent_service();
        let agent = user_agent("backup");
        let (agent_did, _) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &agent, 85).await;
        let backup = crate::AgentService::with_global_instance(|svc| svc.legacy_backup_file());
        assert!(backup.starts_with(&dir.path().to_string_lossy().into_owned()));
        // A backup that cannot be removed (a directory with a file in it).
        std::fs::create_dir_all(&backup).unwrap();
        std::fs::write(std::path::Path::new(&backup).join("keep"), b"x").unwrap();

        let failed = service.unlink_self(&agent).await.expect_err("backup stays");

        assert!(failed.message.contains("legacy"), "{}", failed.message);
        assert!(db(|db| db.unlink_intent(&agent_did)).unwrap().is_some());
        assert!(crate::agent::fence::is_raised(&agent_did));
        std::fs::remove_dir_all(&backup).unwrap();
        std::fs::write(&backup, b"the old file, with the secret").unwrap();

        service
            .unlink_self(&agent)
            .await
            .expect("the retry finishes");

        assert!(!std::path::Path::new(&backup).exists());
        assert_eq!(db(|db| db.unlink_intent(&agent_did)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&agent_did));
    }

    /// The main agent that unlinks itself gets a new key (`identity.rekey`'s move) and its apps
    /// are signed out.
    #[tokio::test]
    async fn the_main_agent_that_unlinks_itself_gets_a_new_key() {
        let _dir = unlocked_agent_service();
        let main = AgentContext::main_agent();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        let old = crate::agent::did();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        linked(&service, &main, 71).await;
        let grant = grant_on(&main);

        let head = service.unlink_self(&main).await.expect("unlinked");

        assert_eq!(head.seq, 1);
        assert!(grant.is_ended());
        assert_ne!(crate::agent::did(), old, "a new did:key");
        assert_secret_destroyed(&crate::agent::rekey::retired_key_name(&old), &old);
        assert_eq!(db(|db| db.unlink_intent(&old)).unwrap(), None);
        assert!(!crate::agent::fence::is_raised(&old));
        let status = service.status(&main).await.unwrap();
        assert_eq!(status.did, None, "the new key speaks for no identity");
    }

    /// An agent with no identity has nothing to unlink: refused before the store is asked.
    #[tokio::test]
    async fn unlinking_is_refused_for_an_agent_with_no_identity() {
        let _dir = unlocked_agent_service();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let alone = user_agent("alone");

        let refused = service.unlink_self(&alone).await.expect_err("no identity");

        assert_eq!(refused.code, 409);
        assert!(
            refused.message.starts_with("no-identity"),
            "{}",
            refused.message
        );
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 0);
    }

    /// A key the person's master key revoked is dead already: `unlinkSelf` refuses it.
    #[tokio::test]
    async fn a_revoked_key_cannot_unlink_itself() {
        let _dir = unlocked_agent_service();
        let agent = user_agent("revoked");
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &agent, 74).await;
        let master = ed25519_dalek::SigningKey::from_bytes(&[74; 32]);
        let chain = service.cache.chain(&scid).await.unwrap().unwrap();
        let revoke = ad4m_identity_core::build::revoke(&chain, &master, &["d1".to_string()], None)
            .expect("a revoke");
        backend.inner.events.lock().unwrap().push(revoke);

        // The key is dead in the log but its secret is still here: the call takes it out of
        // use without publishing anything more.
        service
            .unlink_self(&agent)
            .await
            .expect("the cleanup of a dead key");
        let (did, _) = device_key(&agent).unwrap();
        assert_secret_destroyed(&crate::agent::rekey::retired_managed_key_name(&did), &did);

        let refused = service.unlink_self(&agent).await.expect_err("dead");

        assert_eq!(refused.code, 409);
        assert!(
            refused.message.starts_with("key-dead"),
            "{}",
            refused.message
        );
        assert_eq!(backend.appends.load(std::sync::atomic::Ordering::SeqCst), 1);
    }

    /// An `unlink` of this executor's key by the master key (another device) ends the key here
    /// the same way: the cache's verdict change signs the agent out, and `identity.status` shows
    /// `unlinked`. A change for a key no agent here holds, or one that does not end a key, does
    /// nothing.
    #[tokio::test]
    async fn a_key_unlinked_by_another_device_signs_the_agent_out() {
        use crate::identity::verdict::{KeyVerdict, VerdictChange};
        let _dir = unlocked_agent_service();
        let agent = user_agent("remote");
        let (_, key) = device_key(&agent).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![], false));
        let service = service_on(backend.clone());
        let scid = linked(&service, &agent, 75).await;
        let grant = grant_on(&agent);
        let mut changes = service.cache.subscribe();

        let master = ed25519_dalek::SigningKey::from_bytes(&[75; 32]);
        let chain = service.cache.chain(&scid).await.unwrap().unwrap();
        let unlink = ad4m_identity_core::build::unlink(&chain, &master, &"d1".to_string()).unwrap();
        backend.inner.events.lock().unwrap().push(unlink);
        service
            .cache
            .refresh(backend.as_ref(), &scid)
            .await
            .unwrap();

        let change = std::iter::from_fn(|| changes.try_recv().ok())
            .find(|c| c.key == key)
            .expect("the cache fires a verdict change for the key");
        assert_eq!(change.verdict, KeyVerdict::Unlinked);
        let wrong_scid = VerdictChange {
            scid: "another".into(),
            ..change.clone()
        };
        let expired = VerdictChange {
            verdict: KeyVerdict::Expired,
            ..change.clone()
        };
        crate::identity::unlink::end_dead_key(&wrong_scid).await;
        crate::identity::unlink::end_dead_key(&expired).await;
        assert!(!grant.is_ended());

        crate::identity::unlink::end_dead_key(&change).await;

        assert!(grant.is_ended(), "the agent is signed out");
        let status = service.status(&agent).await.unwrap();
        assert_eq!(status.unlinked, Some(1));
        let refused = service.unlink_self(&agent).await.expect_err("dead");
        assert!(
            refused.message.starts_with("key-dead"),
            "{}",
            refused.message
        );
    }
}
