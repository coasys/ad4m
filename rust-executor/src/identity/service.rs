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
    convert, BotView, DeviceView, ForkRecord, IdentityLog, IdentityStatus, KeyOwner, OfferListing,
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
    /// Where the store keys came from (`IdentitySettings::store_keys_source`), for the
    /// `no-store-keys` refusal.
    pub store_keys_source: super::StoreSource,
    /// DID resolution (`identity.resolve`) over the same cache and backend.
    pub resolver: Arc<super::resolver::Resolver>,
    /// The open challenges of key logins (`user.sessionChallenge`).
    pub challenges: super::key_login::Challenges,
    /// The device inbox: what this executor read of it, the open pre-join read, the pull's
    /// wake-up.
    pub inbox: super::inbox::Inbox,
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

    /// `identity.keyOwner`: the identity the store's key index lists `key` under (a master key
    /// too: a restore finds its identity by master #0, which the words derive); `None` when no
    /// log lists it.
    pub async fn key_owner(&self, key: &str) -> Result<Option<KeyOwner>, WsRpcError> {
        if !ad4m_identity_core::is_ed25519_multikey(key) {
            return Err(WsRpcError::bad_request(format!(
                "{key} is not an Ed25519 multikey"
            )));
        }
        match self.backend.key_owner(key, 0).await {
            Ok(Some(entry)) => Ok(Some(KeyOwner {
                did: entry.did,
                key_id: entry.key_id,
                added: entry.added,
                revoked: entry.revoked,
            })),
            Ok(None) | Err(IdentityError::NotFound) => Ok(None),
            Err(e) => Err(backend_error(e)),
        }
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
        self.bind_agent_if_listed(agent, scid, false).await
    }

    /// [`IdentityService::record_agent_if_listed`], and with `first_run_operator` the end of a
    /// desktop app's first link (`identity.joinStatus` on `spent`): a still-empty operator
    /// record takes the identity (`OperatorSetBy::FirstRun`) in the same database write as the
    /// binding, so no stop between the two leaves a bound desktop without its operator.
    pub(super) async fn bind_agent_if_listed(
        &self,
        agent: &AgentContext,
        scid: &str,
        first_run_operator: bool,
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
            let exists = |e: WsRpcError| error(409, format!("identity-exists: {}", e.message));
            if first_run_operator {
                let did = did_of(scid);
                let recorded = db(|db| {
                    db.set_agent_identity_and_first_run_operator(&agent_did, scid, &did, now())
                })
                .map_err(exists)?;
                if recorded.scid == did && recorded.set_by == crate::db::OperatorSetBy::FirstRun {
                    log::info!(
                        "identity: this desktop executor joined {did}, its operator from now on"
                    );
                }
            } else {
                db(|db| db.set_agent_identity(&agent_did, scid, now())).map_err(exists)?;
            }
            // A local identity now: its log stays in the key map, so the agent's links carry
            // their anchor ([`super::anchor_for`]).
            self.cache.keep_local(scid).await.map_err(cache_error)?;
            // The main agent's status gains its identity (a managed agent's status event names
            // the main agent's, so only the main agent's changes here).
            if agent.is_main_agent() {
                // A device that joined reads its inbox at once.
                super::inbox::wake();
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
            // The store's head lies past these events (a `409` replay of a log that moved on):
            // read the rest of the log, so the head is accepted.
            Ok(Some(ours)) if ours.seq < head.seq => {
                self.cache
                    .refresh(self.backend.as_ref(), &scid)
                    .await
                    .map_err(cache_error)?;
            }
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

    /// `identity.devices`: the identity's device keys, from the log (the executor never opens
    /// the private record, so no names).
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

/// An offer string (`coasys:offer:1:…`) or link (`https://id.coasys.org/o#…`).
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

    /// `identity.keyOwner` answers the key index's entry, `None` for a key no log lists, and
    /// refuses what is not an Ed25519 multikey before asking the store.
    #[tokio::test]
    async fn key_owner_answers_the_key_index() {
        let world = crate::identity::record::tests::Bound::new(74).await;
        let owner = world.service.key_owner(&world.key).await.unwrap().unwrap();
        assert_eq!(owner.did, did_of(world.log.chain().id()));
        assert_eq!((owner.key_id.as_str(), owner.revoked), ("d2", None));
        let stranger = ad4m_identity_core::build::Signer::key(
            &ed25519_dalek::SigningKey::from_bytes(&[75; 32]),
        );
        assert!(world.service.key_owner(&stranger).await.unwrap().is_none());
        let calls = world
            .store
            .key_owner_calls
            .load(std::sync::atomic::Ordering::SeqCst);
        let refused = world.service.key_owner("not-a-key").await.unwrap_err();
        assert_eq!(refused.code, 400);
        assert_eq!(
            world
                .store
                .key_owner_calls
                .load(std::sync::atomic::Ordering::SeqCst),
            calls
        );
    }

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
            inbox: Default::default(),
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
            inbox: Default::default(),
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
                inbox: Default::default(),
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
                inbox: Default::default(),
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
                .claim_for_main(&offer, false, None)
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
            inbox: Default::default(),
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
            inbox: Default::default(),
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
        async fn inbox_put(
            &self,
            address: &ad4m_ldk::identity::InboxAddress,
            write: &ad4m_ldk::identity::InboxWrite,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<ad4m_ldk::identity::InboxStored, IdentityError> {
            self.inner.inbox_put(address, write, req).await
        }
        async fn inbox_list(
            &self,
            address: &ad4m_ldk::identity::InboxAddress,
            auth: &ad4m_ldk::identity::InboxAuth,
            wait_s: u8,
        ) -> Result<Vec<ad4m_ldk::identity::InboxMessage>, IdentityError> {
            self.inner.inbox_list(address, auth, wait_s).await
        }
        async fn inbox_ack(
            &self,
            address: &ad4m_ldk::identity::InboxAddress,
            id: &str,
            auth: &ad4m_ldk::identity::InboxAuth,
        ) -> Result<(), IdentityError> {
            self.inner.inbox_ack(address, id, auth).await
        }
        async fn get_record(
            &self,
            did: &str,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<Option<ad4m_ldk::identity::StoredRecord>, IdentityError> {
            self.inner.get_record(did, req).await
        }
        async fn put_record(
            &self,
            did: &str,
            record: &ad4m_ldk::identity::SealedRecord,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<ad4m_ldk::identity::RecordStored, IdentityError> {
            self.inner.put_record(did, record, req).await
        }
        async fn post_request(
            &self,
            did: &str,
            post: &ad4m_ldk::identity::PostedRequest,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<ad4m_ldk::identity::RequestPosted, IdentityError> {
            self.inner.post_request(did, post, req).await
        }
        async fn list_requests(
            &self,
            did: &str,
            since: u64,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<Vec<ad4m_ldk::identity::DeviceRequest>, IdentityError> {
            self.inner.list_requests(did, since, req).await
        }
        async fn delete_request(
            &self,
            did: &str,
            id: &str,
            req: &ad4m_ldk::identity::SignedRequest,
        ) -> Result<(), IdentityError> {
            self.inner.delete_request(did, id, req).await
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
            inbox: Default::default(),
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
            .claim_for_main(&offer, false, None)
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
        let claims = || backend.claims.load(std::sync::atomic::Ordering::SeqCst);

        // The store refuses the claim: the row stays and reports the offer's state.
        *backend.claim_error.lock().unwrap() = Some(IdentityError::Gone("expired".into()));
        let refused = offer(3);
        service
            .claims()
            .claim_for_main(&refused, false, None)
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
            .claim_for_main(&lost, false, None)
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

    /// A desktop app's executor at its first run (`--custody-channel`, no operator record, the
    /// main agent of no identity) claims. A claim the store took records nothing, so before the
    /// join the desktop may claim again, for another identity. Once the add spends the offer and
    /// the main agent binds to the identity, that identity is the operator (`first-run`), and an
    /// offer of another identity is refused `first-link`. Without the custody channel a missing
    /// record stays `no-operator`, before the store.
    #[tokio::test]
    async fn a_desktop_with_no_operator_record_records_the_identity_it_joins() {
        use crate::db::OperatorSetBy;
        use ad4m_identity_core::link::OfferString;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = AgentContext::main_agent();
        let (main_did, _) = device_key(&main).unwrap();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
        let incept = test_service()
            .cosign_incept(&main, &incept_listing(&main, 59))
            .await
            .unwrap();
        let incept = Event::from_json(&incept).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![incept.clone()], false));
        let service = service_on(backend.clone());
        let offer = |scid: &str, id: u8| OfferString {
            store: "https://store.example".into(),
            scid: scid.into(),
            offer_id: bs58::encode([id; 16]).into_string(),
            s: bs58::encode([1u8; 32]).into_string(),
        };
        let claims = || backend.claims.load(std::sync::atomic::Ordering::SeqCst);
        let record = || db(|db| db.operator_record()).unwrap();

        // Not a desktop app's executor: no record is `no-operator`, and nothing is recorded.
        let refused = service
            .claims()
            .claim_for_main(&offer(&incept.scid, 5), false, None)
            .await
            .expect_err("no operator");
        assert!(
            refused.message.starts_with("no-operator"),
            "{}",
            refused.message
        );
        assert_eq!((claims(), record()), (0, None));

        let channel = crate::config::CustodyChannelForTest::on();
        let first = offer(&incept.scid, 6);
        service
            .claims()
            .claim_for_main(&first, true, None)
            .await
            .expect("a desktop's first claim goes ahead");
        assert_eq!(claims(), 1);
        // The store took the claim, but the holder has not confirmed it: nothing is recorded.
        assert_eq!(record(), None, "a claim alone records no operator");

        // Before the join a desktop may claim again, for another identity (the person
        // scanned the wrong code, or the holder pressed No).
        let other = TestLog::new(60).scid();
        service
            .claims()
            .claim_for_main(&offer(&other, 7), true, None)
            .await
            .expect("a second claim before the join goes ahead");
        assert_eq!((claims(), record()), (2, None));

        // The holder confirms the first claim: the add lands and the main agent binds to its
        // identity, which becomes the operator.
        *backend.offer.lock().unwrap() = "spent".into();
        let status = service
            .claims()
            .join_status(&main, &first.offer_id)
            .await
            .unwrap();
        assert_eq!(status.key_id.as_deref(), Some("d1"));
        assert_eq!(
            db(|db| db.agent_identity(&main_did)).unwrap(),
            Some(incept.scid.clone())
        );
        let recorded = record().expect("the join recorded the operator");
        assert_eq!(
            (recorded.scid.as_str(), recorded.set_by),
            (did_of(&incept.scid).as_str(), OperatorSetBy::FirstRun)
        );

        // Bound: every offer is `already-linked`.
        let refused = service
            .claims()
            .claim_for_main(&offer(&other, 8), true, None)
            .await
            .expect_err("linked");
        assert!(
            refused.message.starts_with("already-linked"),
            "{}",
            refused.message
        );
        // With the main key in no log again (as after a revocation), the record stands: an
        // offer of another identity is refused `first-link` before the store.
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
        let refused = service
            .claims()
            .claim_for_main(&offer(&other, 9), true, None)
            .await
            .expect_err("another identity");
        assert!(
            refused.message.starts_with("first-link"),
            "{}",
            refused.message
        );
        assert_eq!(refused.code, 403);
        assert_eq!(claims(), 2);
        assert_eq!(record().unwrap().scid, did_of(&incept.scid));

        drop(channel);
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
    }

    /// A desktop at its first run (`--custody-channel`, no operator record, no identity) with
    /// claims on `offers` of one identity the store took, each offer now spent: the service, its
    /// store, the identity's SCID and the main agent's `did:key`, with the custody channel on
    /// until the first is dropped.
    async fn a_first_run_desktop_with_spent_claims(
        seed: u8,
        offers: &[u8],
    ) -> (
        crate::config::CustodyChannelForTest,
        IdentityService,
        String,
        String,
    ) {
        use ad4m_identity_core::link::OfferString;
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let main = AgentContext::main_agent();
        let (main_did, _) = device_key(&main).unwrap();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
        let incept = test_service()
            .cosign_incept(&main, &incept_listing(&main, seed))
            .await
            .unwrap();
        let incept = Event::from_json(&incept).unwrap();
        let backend = Arc::new(RecordingStore::new(vec![incept.clone()], false));
        let service = service_on(backend.clone());
        let channel = crate::config::CustodyChannelForTest::on();
        for id in offers {
            let offer = OfferString {
                store: "https://store.example".into(),
                scid: incept.scid.clone(),
                offer_id: bs58::encode([*id; 16]).into_string(),
                s: bs58::encode([1u8; 32]).into_string(),
            };
            service
                .claims()
                .claim_for_main(&offer, true, None)
                .await
                .expect("a desktop's claim before the join goes ahead");
        }
        *backend.offer.lock().unwrap() = "spent".into();
        (channel, service, incept.scid, main_did)
    }

    fn offer_id(id: u8) -> String {
        bs58::encode([id; 16]).into_string()
    }

    /// The binding and the first-run operator are one write: when the operator record's write
    /// fails (as a process that stops between them), the main agent is not bound either, so
    /// the next `joinStatus` finds the first link still open and completes both.
    #[tokio::test]
    async fn a_first_join_that_stops_before_the_operator_record_binds_nothing() {
        use crate::db::OperatorSetBy;
        let (channel, service, scid, main_did) =
            a_first_run_desktop_with_spent_claims(61, &[11]).await;
        let main = AgentContext::main_agent();

        Ad4mDb::with_global_instance(|db| db.fail_operator_record_writes_for_test(true)).unwrap();
        let stopped = service.claims().join_status(&main, &offer_id(11)).await;
        Ad4mDb::with_global_instance(|db| db.fail_operator_record_writes_for_test(false)).unwrap();
        assert!(stopped.is_err(), "{stopped:?}");
        assert_eq!(
            db(|db| db.agent_identity(&main_did)).unwrap(),
            None,
            "bound without its operator record"
        );
        assert_eq!(db(|db| db.operator_record()).unwrap(), None);

        let status = service
            .claims()
            .join_status(&main, &offer_id(11))
            .await
            .unwrap();
        assert_eq!(status.key_id.as_deref(), Some("d1"));
        assert_eq!(
            db(|db| db.agent_identity(&main_did)).unwrap(),
            Some(scid.clone())
        );
        let recorded = db(|db| db.operator_record())
            .unwrap()
            .expect("the operator");
        assert_eq!(
            (recorded.scid, recorded.set_by),
            (did_of(&scid), OperatorSetBy::FirstRun)
        );

        drop(channel);
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
    }

    /// Two `joinStatus` calls at once for two spent claims of the identity (the app polls both
    /// after a reconnect): both answer, the main agent binds once, and the identity is the
    /// operator, set once at the first run.
    #[tokio::test]
    async fn concurrent_first_joins_bind_once_and_record_one_operator() {
        use crate::db::OperatorSetBy;
        let (channel, service, scid, main_did) =
            a_first_run_desktop_with_spent_claims(62, &[12, 13]).await;
        let main = AgentContext::main_agent();

        let (claims, ids) = (service.claims(), [offer_id(12), offer_id(13)]);
        let (first, second) = tokio::join!(
            claims.join_status(&main, &ids[0]),
            claims.join_status(&main, &ids[1]),
        );
        let (first, second) = (first.unwrap(), second.unwrap());
        assert_eq!(
            (first.key_id.as_deref(), second.key_id.as_deref()),
            (Some("d1"), Some("d1"))
        );
        assert_eq!(
            db(|db| db.agent_identity(&main_did)).unwrap(),
            Some(scid.clone())
        );
        assert_eq!(
            db(|db| db.agents_of_identity(&scid)).unwrap(),
            vec![main_did.clone()]
        );
        let recorded = db(|db| db.operator_record())
            .unwrap()
            .expect("the operator");
        assert_eq!(
            (recorded.scid, recorded.set_by),
            (did_of(&scid), OperatorSetBy::FirstRun)
        );

        drop(channel);
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
    }
}
