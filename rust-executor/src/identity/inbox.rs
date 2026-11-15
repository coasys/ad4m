//! The executor's side of the device inbox (spec section 17): its own joined inbox, the
//! pre-join inbox an `identity.prepareDevice` reads, and `identity.takeMaster`.
//!
//! A message is sealed to the device's `enc` key, the X25519 form of its wallet key; only the
//! wallet opens it (`WalletBackend::open_sealed`), and every opened payload is wiped when it
//! drops ([`Opened`]). Its type sits inside the seal.
//!
//! - **The joined inbox** of the main agent is read at start, whenever the wallet unlocks or
//!   the agent is bound to an identity ([`wake`]), and every [`PULL_EVERY`]
//!   ([`run_pull`]). On an executor with the custody channel each new message is opened to
//!   learn its type, the plaintext wiped at once, the message left in the store, and
//!   `inbox-message {id, kind}` sent to the operator on this machine. Every other executor
//!   keeps no master key (a master key must not rest on a server), and the only message a
//!   joined inbox carries is a master key, so it deletes each message unopened and logs an
//!   alarm.
//! - **`identity.takeMaster`** (custody channel only) opens the oldest master message, checks
//!   it against the log (`check_payload`) and hands the key to the custody service; the
//!   custody service acknowledges it (`identity.inboxAck`) once stored. A master key that no
//!   longer counts, or one that fails the check, is acknowledged and dropped.
//! - **`identity.prepareDevice`** makes the main key's device string with a fresh `r` and
//!   reads the pre-join inbox `Pre(a)` until the string expires (one at a time; a new one
//!   ends the last). `ClaimService::claim_for_main` claims a valid offer ticket (the
//!   first-link rule; `canKeepMaster` exactly when the executor has the custody channel), and
//!   a claim sends `link-ticket` (the identity and the match code). A master key never
//!   travels before a device joins: one found there is dropped with an alarm.
//! - **`identity.endPrepared`** ends one device string (the person cancelled the code screen):
//!   its read stops after the round under way, and every claim the read made is cancelled
//!   (`ClaimService::cancel_join`'s rule), so a holder who confirms later binds nothing here
//!   (the store is not told: it may still accept the confirmation). A claim that bound already
//!   answers `already-joined`. The end runs in its own task, so a dropped call still cancels
//!   the claims, and its first answer is kept for every later call.
//!
//! The master key opens here only inside `identity.takeMaster`: it is written to no store,
//! log, event or language, and the result wipes itself when dropped (`TakenMaster`).

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use ad4m_identity_core::did_of;
use ad4m_identity_core::inbox::{
    check_payload, pre_address, sign_pre_auth, Accept, InboxAddress as SealedAt, InboxBody,
    InboxPayload,
};
use ad4m_identity_core::link::{self, DeviceString};
use ad4m_ldk::identity::{InboxAddress, InboxAuth, InboxMessage, InboxWrite};
use tokio::sync::Notify;
use tokio_util::sync::CancellationToken;
use zeroize::Zeroize;

use super::cache::CacheError;
use super::device::{device_key, enc_key_of, WalletSigner, REQUEST_TTL_S};
use super::record::DevicePosition;
use super::service::{backend_error, cache_error, db, error, now, IdentityService};
use super::types::{
    convert, InboxEntry, InboxMessageEvent, InboxMessageType, InboxPutParams, InboxStored,
    LinkTicketEvent, PreparedDevice, PreparedEnded, TakenMaster,
};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::db::JoinCancel;
use crate::events::{Audience, Event, Topic};

/// How often the joined inbox is read besides at start, unlock and binding.
pub const PULL_EVERY: Duration = Duration::from_secs(15 * 60);
/// How long a device string is valid, and so how long its pre-join inbox is read.
pub const DEVICE_STRING_TTL_S: u64 = 300;
/// The longest wait of one pre-join read (the store's longest).
const PRE_WAIT_S: u8 = 8;
/// The shortest round of the pre-join read: one that answered sooner waits out the rest, so
/// neither a store that ignores `wait` nor a message left for the next round makes a tight
/// loop.
const PRE_MIN_ROUND: Duration = Duration::from_secs(1);

/// What the executor holds of its inbox: never a body.
#[derive(Default)]
pub struct Inbox {
    /// The joined messages opened on this executor, by id: their type and times.
    seen: Mutex<BTreeMap<String, InboxEntry>>,
    /// Held through one read of the joined inbox: the pull and `identity.inbox` take turns, so
    /// a message is opened and announced once.
    pulling: tokio::sync::Mutex<()>,
    /// The device strings `identity.prepareDevice` made, oldest first, each until
    /// [`PREPARED_KEPT_S`] after it expires (dropped on the next access after that) or until
    /// [`PREPARED_KEPT`] newer ones were made: `identity.endPrepared` ends one by its string.
    prepared: Mutex<Vec<Arc<Prepared>>>,
    /// Wakes [`run_pull`].
    wake: Notify,
}

impl Inbox {
    fn seen(&self) -> std::sync::MutexGuard<'_, BTreeMap<String, InboxEntry>> {
        self.seen.lock().unwrap_or_else(|e| e.into_inner())
    }

    /// The device strings still kept; one past [`PREPARED_KEPT_S`] after its expiry goes here.
    fn prepared(&self) -> std::sync::MutexGuard<'_, Vec<Arc<Prepared>>> {
        let mut all = self.prepared.lock().unwrap_or_else(|e| e.into_inner());
        let at = now();
        all.retain(|p| p.expires.saturating_add(PREPARED_KEPT_S) > at);
        all
    }
}

/// How long after a device string expires `identity.endPrepared` still knows it: a claim its
/// read made stays open for its offer's life, after the string expired.
pub const PREPARED_KEPT_S: u64 = 3600;
/// The most device strings kept at once; the oldest goes first.
const PREPARED_KEPT: usize = 16;

/// One device string `identity.prepareDevice` made, and what its pre-join read did.
struct Prepared {
    device: String,
    expires: u64,
    /// Cancelled to end the pre-join read.
    done: CancellationToken,
    /// Held while the read takes the tickets of one round: `identity.endPrepared` waits for it.
    turn: tokio::sync::Mutex<()>,
    /// Every offer the read tried to claim, noted before the claim's row can go in.
    offers: Mutex<Vec<String>>,
    /// The answer of `identity.endPrepared`, once one was decided: every later call gives it.
    ended: tokio::sync::OnceCell<PreparedEnded>,
}

impl Prepared {
    fn offers(&self) -> std::sync::MutexGuard<'_, Vec<String>> {
        self.offers.lock().unwrap_or_else(|e| e.into_inner())
    }
}

/// An opened payload. A secret it carries (a master key, an offer's secret) is wiped when it
/// drops.
struct Opened(InboxPayload);

impl Drop for Opened {
    fn drop(&mut self) {
        match &mut self.0.body {
            InboxBody::Master { secret, .. } => secret.zeroize(),
            InboxBody::OfferTicket(ticket) => ticket.s.zeroize(),
        }
    }
}

impl Opened {
    fn kind(&self) -> InboxMessageType {
        match self.0.body {
            InboxBody::Master { .. } => InboxMessageType::Master,
            InboxBody::OfferTicket(_) => InboxMessageType::OfferTicket,
        }
    }
}

/// Why a message did not open.
enum Unopened {
    /// The wallet is locked or lacks the key: try again later, keep the message.
    NoKey,
    /// It does not open with this device's key at this address: it is for no one here.
    Sealed(String),
}

/// Opens `sealed`, a message for `address`, with `agent`'s wallet key.
fn open(agent: &AgentContext, address: &SealedAt, sealed: &str) -> Result<Opened, Unopened> {
    let wallet = crate::wallet::wallet_backend();
    let name = agent.wallet_key_name().map_err(|_| Unopened::NoKey)?;
    if !wallet.is_unlocked() || !wallet.key_exists(&name) {
        return Err(Unopened::NoKey);
    }
    wallet
        .open_sealed(&name, address, sealed)
        .map(Opened)
        .map_err(|e| Unopened::Sealed(e.to_string()))
}

/// The event to the operator on this machine.
fn publish(topic: Topic, payload: impl serde::Serialize) {
    crate::pubsub::get_global_pubsub_sync().publish_event(Event::new(
        topic,
        Audience::LocalOperator,
        payload,
    ));
}

fn joined(position: &DevicePosition) -> (InboxAddress, SealedAt, String) {
    (
        InboxAddress::Joined {
            scid: position.scid.clone(),
            key_id: position.key_id.clone(),
        },
        SealedAt::Joined {
            scid: position.scid.clone(),
            key_id: position.key_id.clone(),
        },
        format!("/v1/inbox/{}/{}", position.scid, position.key_id),
    )
}

impl IdentityService {
    async fn list_joined(
        &self,
        position: &DevicePosition,
    ) -> Result<Vec<InboxMessage>, WsRpcError> {
        let (address, _, path) = joined(position);
        let auth = InboxAuth::Request(position.request("GET", &path, b"")?);
        self.backend
            .inbox_list(&address, &auth, 0)
            .await
            .map_err(backend_error)
    }

    /// Deletes joined message `id` at the store and forgets it. One that is gone already
    /// counts as deleted.
    async fn ack_joined(&self, position: &DevicePosition, id: &str) -> Result<(), WsRpcError> {
        let (address, _, path) = joined(position);
        let auth = InboxAuth::Request(position.request("DELETE", &format!("{path}/{id}"), b"")?);
        self.inbox.seen().remove(id);
        match self.backend.inbox_ack(&address, id, &auth).await {
            Ok(()) | Err(super::backend::IdentityError::NotFound) => Ok(()),
            Err(e) => Err(backend_error(e)),
        }
    }

    /// Acknowledges a message this executor will not act on, logging why; a failure to
    /// acknowledge is logged too (the next read tries again).
    async fn drop_joined(&self, position: &DevicePosition, id: &str, why: &str) {
        log::warn!("identity: inbox message {id}: {why}; deleting it");
        self.delete_joined(position, id).await;
    }

    /// Deletes joined message `id`; a failure is logged (the next read tries again).
    async fn delete_joined(&self, position: &DevicePosition, id: &str) {
        if let Err(e) = self.ack_joined(position, id).await {
            log::warn!(
                "identity: inbox message {id} could not be deleted: {}",
                e.message
            );
        }
    }

    /// Reads `agent`'s joined inbox: the messages waiting, by id and type, never a body. With
    /// `custody` (the custody channel) each new message is opened for its type, wiped, kept in
    /// the store and announced (`inbox-message`); without it each message is deleted unopened.
    pub(crate) async fn pull_inbox(
        &self,
        agent: &AgentContext,
        custody: bool,
    ) -> Result<Vec<InboxEntry>, WsRpcError> {
        let Some(position) = self.device_position(agent).await? else {
            return Ok(vec![]);
        };
        let _turn = self.inbox.pulling.lock().await;
        let (_, sealed_at, _) = joined(&position);
        let mut entries = vec![];
        for message in self.list_joined(&position).await? {
            if !custody {
                log::error!(
                    "identity: a message waited in this device's inbox ({}). This executor has \
                     no custody channel and keeps no master key, so it deletes it unopened",
                    message.id
                );
                self.delete_joined(&position, &message.id).await;
                continue;
            }
            if let Some(entry) = self.inbox.seen().get(&message.id).cloned() {
                entries.push(entry);
                continue;
            }
            let kind = match open(agent, &sealed_at, &message.sealed) {
                Ok(opened) => opened.kind(),
                Err(Unopened::NoKey) => continue,
                Err(Unopened::Sealed(why)) => {
                    self.drop_joined(&position, &message.id, &format!("does not open ({why})"))
                        .await;
                    continue;
                }
            };
            let entry = InboxEntry {
                id: message.id.clone(),
                kind,
                stored_at: message.stored_at,
                expires: message.expires,
            };
            self.inbox.seen().insert(entry.id.clone(), entry.clone());
            publish(
                Topic::InboxMessage,
                InboxMessageEvent {
                    id: entry.id.clone(),
                    kind,
                },
            );
            entries.push(entry);
        }
        let listed: Vec<&str> = entries.iter().map(|e| e.id.as_str()).collect();
        self.inbox
            .seen()
            .retain(|id, _| listed.contains(&id.as_str()));
        Ok(entries)
    }

    /// `identity.takeMaster` for `agent` (the custody channel admitted the caller): the oldest
    /// master key that passes `check_payload` against the refreshed log, or `None`.
    /// A stale master key and one that fails the check are acknowledged and dropped. The
    /// message stays until the custody service acknowledges it.
    pub(crate) async fn take_master(
        &self,
        agent: &AgentContext,
    ) -> Result<Option<TakenMaster>, WsRpcError> {
        let Some(position) = self.device_position(agent).await? else {
            return Ok(None);
        };
        self.refreshed_or_cached(&position.scid).await?;
        let (chain, _) = self
            .cache
            .trusted(&position.scid)
            .await
            .map_err(cache_error)?
            .ok_or_else(|| cache_error(CacheError::NotFound))?;
        let (_, sealed_at, _) = joined(&position);
        for message in self.list_joined(&position).await? {
            let mut opened = match open(agent, &sealed_at, &message.sealed) {
                Ok(opened) => opened,
                Err(Unopened::NoKey) => {
                    return Err(error(
                        503,
                        "unavailable: the wallet is locked, so the inbox does not open",
                    ))
                }
                Err(Unopened::Sealed(why)) => {
                    self.drop_joined(&position, &message.id, &format!("does not open ({why})"))
                        .await;
                    continue;
                }
            };
            if opened.kind() != InboxMessageType::Master {
                continue;
            }
            match check_payload(&opened.0, &position.key, &chain, now()) {
                Ok(Accept::KeepMaster { index }) => {
                    let InboxBody::Master { did, secret, .. } = &mut opened.0.body else {
                        continue;
                    };
                    return Ok(Some(TakenMaster {
                        message_id: message.id,
                        did: did.clone(),
                        index,
                        secret: std::mem::take(secret),
                    }));
                }
                Ok(Accept::Stale) => {
                    drop(opened);
                    self.drop_joined(&position, &message.id, "a master key that no longer counts")
                        .await;
                }
                Ok(Accept::ClaimOffer(_)) => {}
                Err(e) => {
                    drop(opened);
                    self.drop_joined(
                        &position,
                        &message.id,
                        &format!("a master key that fails the check ({})", e.name()),
                    )
                    .await;
                }
            }
        }
        Ok(None)
    }

    /// `identity.inboxAck`: deletes `agent`'s joined message `id`.
    pub(crate) async fn inbox_ack(
        &self,
        agent: &AgentContext,
        id: &str,
    ) -> Result<bool, WsRpcError> {
        let position = self.bound_position(agent).await?;
        self.ack_joined(&position, id).await?;
        Ok(true)
    }

    /// `identity.inboxPut`: passes a master-signed inbox write to the agent language.
    pub async fn inbox_put(&self, params: &InboxPutParams) -> Result<InboxStored, WsRpcError> {
        let bad = |e: serde_json::Error| WsRpcError::bad_request(format!("invalid: {e}"));
        let write = InboxWrite {
            sealed: params.sealed.clone(),
            bind: params.bind.as_ref().map(convert).transpose().map_err(bad)?,
        };
        let stored = self
            .backend
            .inbox_put(
                &convert(&params.address).map_err(bad)?,
                &write,
                &convert(&params.request).map_err(bad)?,
            )
            .await
            .map_err(backend_error)?;
        convert(&stored).map_err(|e| WsRpcError::internal(e.to_string()))
    }

    /// Reads the main agent's joined inbox when the wallet is unlocked; logs a failure. The
    /// first run comes at start, before the agent service may exist: then it reads nothing.
    async fn pull_main(&self) {
        let ready = crate::agent::AgentService::global_instance()
            .lock()
            .ok()
            .and_then(|agent| {
                agent
                    .as_ref()
                    .map(|agent| agent.is_initialized() && agent.is_unlocked())
            })
            .unwrap_or(false);
        if !ready {
            return;
        }
        let custody = crate::config::custody_channel_on();
        if let Err(e) = self.pull_inbox(&AgentContext::main_agent(), custody).await {
            log::warn!(
                "identity: reading this device's inbox failed: {}",
                e.message
            );
        }
    }
}

/// `identity.prepareDevice`: the main key's device string with a fresh `r`, valid
/// [`DEVICE_STRING_TTL_S`]; its pre-join inbox is read until then (and the last one's read
/// ends). `custody` (the custody channel) is the claim's `canKeepMaster`.
pub async fn prepare_device(
    service: &Arc<IdentityService>,
    custody: bool,
) -> Result<PreparedDevice, WsRpcError> {
    let store_url = service.store_url.clone().ok_or_else(|| {
        error(
            503,
            "no-store-url: identity.storeUrl is not set, so a device string names no store",
        )
    })?;
    service.refuse_without_store_keys()?;
    let store = service
        .cache
        .store_keys()
        .first()
        .map(|key| format!("did:key:{key}"))
        .ok_or_else(|| WsRpcError::internal("identity.storeKeys is empty"))?;
    let agent = AgentContext::main_agent();
    let (_, sign) = device_key(&agent).map_err(WsRpcError::internal)?;
    let enc = enc_key_of(&sign).map_err(WsRpcError::internal)?;
    let r = bs58::encode(rand::random::<[u8; link::DEVICE_R_LEN]>()).into_string();
    let expires = now() + DEVICE_STRING_TTL_S;
    let address = pre_address(&sign, &r).map_err(|e| WsRpcError::internal(e.to_string()))?;
    let device = DeviceString {
        sign: sign.clone(),
        enc: enc.clone(),
        r: r.clone(),
        exp: expires,
        hint: None,
        store: store_url,
    }
    .encode();
    let prepared = Arc::new(Prepared {
        device: device.clone(),
        expires,
        done: CancellationToken::new(),
        turn: Default::default(),
        offers: Default::default(),
        ended: Default::default(),
    });
    {
        let mut all = service.inbox.prepared();
        for earlier in all.iter() {
            earlier.done.cancel();
        }
        while all.len() >= PREPARED_KEPT {
            all.remove(0);
        }
        all.push(prepared.clone());
    }
    let read = PreRead {
        agent,
        sign: sign.clone(),
        r,
        address,
        store,
        expires,
    };
    tokio::spawn(read_pre_inbox(service.clone(), read, prepared, custody));
    Ok(PreparedDevice {
        sign,
        enc,
        device,
        expires,
    })
}

/// One open pre-join inbox.
struct PreRead {
    agent: AgentContext,
    sign: String,
    r: String,
    address: String,
    /// The store's `did:key`, which a pre-join read names.
    store: String,
    expires: u64,
}

impl PreRead {
    fn path(&self) -> String {
        format!("/v1/inbox/pre/{}", self.address)
    }

    /// A pre-join read or acknowledgement of `method path`, signed by the device key.
    fn auth(&self, method: &str, path: &str) -> Result<InboxAuth, String> {
        let signer = WalletSigner::new(&self.agent)?;
        let nonce = bs58::encode(rand::random::<[u8; 16]>()).into_string();
        let auth = sign_pre_auth(
            &self.r,
            &self.store,
            method,
            path,
            &nonce,
            now() + REQUEST_TTL_S,
            &signer,
        )
        .map_err(|e| e.to_string())?;
        signer.check()?;
        Ok(InboxAuth::Pre(convert(&auth).map_err(|e| e.to_string())?))
    }
}

/// Reads the pre-join inbox of `read` until it expires, `prepared` is ended, or a ticket led
/// to a claim.
async fn read_pre_inbox(
    service: Arc<IdentityService>,
    read: PreRead,
    prepared: Arc<Prepared>,
    custody: bool,
) {
    let done = prepared.done.clone();
    let address = InboxAddress::Pre {
        pre: read.address.clone(),
    };
    while now() < read.expires && !done.is_cancelled() {
        let started = Instant::now();
        let listed = match read.auth("GET", &read.path()) {
            Ok(auth) => tokio::select! {
                _ = done.cancelled() => break,
                listed = service.backend.inbox_list(&address, &auth, PRE_WAIT_S) => {
                    listed.map_err(|e| e.to_string())
                }
            },
            Err(e) => Err(e),
        };
        let messages = listed.unwrap_or_else(|e| {
            log::debug!("identity: reading the pre-join inbox failed: {e}");
            vec![]
        });
        let mut claimed = false;
        {
            // `identity.endPrepared` cancels `done`, then waits for this turn: a round that
            // began before it ends first, and one after it takes no ticket.
            let _turn = prepared.turn.lock().await;
            for message in &messages {
                if done.is_cancelled() {
                    break;
                }
                claimed = service
                    .take_ticket(&read, &prepared, message, custody)
                    .await;
                if claimed {
                    break;
                }
            }
        }
        if claimed {
            break;
        }
        // A message left waiting (a locked wallet, a log not readable now) comes back at once.
        if let Some(rest) = PRE_MIN_ROUND.checked_sub(started.elapsed()) {
            tokio::select! {
                _ = done.cancelled() => break,
                _ = tokio::time::sleep(rest) => {}
            }
        }
    }
    done.cancel();
}

impl IdentityService {
    /// Acknowledges a pre-join message; a failure is logged.
    async fn ack_pre(&self, read: &PreRead, id: &str) {
        let address = InboxAddress::Pre {
            pre: read.address.clone(),
        };
        let acked = match read.auth("DELETE", &format!("{}/{id}", read.path())) {
            Ok(auth) => self
                .backend
                .inbox_ack(&address, id, &auth)
                .await
                .map_err(|e| e.to_string()),
            Err(e) => Err(e),
        };
        if let Err(e) = acked {
            log::warn!("identity: pre-join message {id} could not be deleted: {e}");
        }
    }

    /// One message of a pre-join inbox: a valid ticket is claimed for the main agent, and the
    /// claim sends `link-ticket`. True once a claim passed. Every message it acts on, or that is for
    /// no one here, is acknowledged; one whose identity's log cannot be read now waits for
    /// the next round.
    async fn take_ticket(
        &self,
        read: &PreRead,
        prepared: &Prepared,
        message: &InboxMessage,
        custody: bool,
    ) -> bool {
        let sealed_at = SealedAt::Pre(read.address.clone());
        let opened = match open(&read.agent, &sealed_at, &message.sealed) {
            Ok(opened) => opened,
            Err(Unopened::NoKey) => return false,
            Err(Unopened::Sealed(why)) => {
                log::warn!(
                    "identity: pre-join message {} does not open ({why})",
                    message.id
                );
                self.ack_pre(read, &message.id).await;
                return false;
            }
        };
        let scid = match &opened.0.body {
            InboxBody::OfferTicket(ticket) => ticket.scid.clone(),
            InboxBody::Master { .. } => {
                drop(opened);
                log::error!(
                    "identity: a master key arrived in the pre-join inbox ({}); a master key \
                     never travels before a device joins, so it is dropped",
                    message.id
                );
                self.ack_pre(read, &message.id).await;
                return false;
            }
        };
        let chain = match self.cache.refresh(self.backend.as_ref(), &scid).await {
            Ok(chain) => chain,
            // A store behind the cache, or whose head names another hash, is no fork: the
            // cached log up to its trusted seq answers, as in `refreshed_or_cached`.
            Err(e @ (CacheError::Behind { .. } | CacheError::Conflict(_))) => {
                match self.cache.trusted(&scid).await {
                    Ok(Some((chain, _))) => {
                        log::info!(
                            "identity: the store is behind for {} ({e}); checking the pre-join \
                             ticket against the cached log",
                            did_of(&scid)
                        );
                        chain
                    }
                    Ok(None) | Err(_) => {
                        log::warn!(
                            "identity: the log of {} for a pre-join ticket cannot be read now ({e})",
                            did_of(&scid)
                        );
                        return false;
                    }
                }
            }
            Err(e) => {
                log::warn!(
                    "identity: the log of {} for a pre-join ticket cannot be read now ({e})",
                    did_of(&scid)
                );
                return false;
            }
        };
        let mut ticket = match check_payload(&opened.0, &read.sign, &chain, now()) {
            Ok(Accept::ClaimOffer(ticket)) => ticket,
            Ok(_) => return false,
            Err(e) => {
                log::warn!(
                    "identity: pre-join message {} is refused ({})",
                    message.id,
                    e.name()
                );
                self.ack_pre(read, &message.id).await;
                return false;
            }
        };
        drop(opened);
        let mut offer = ticket.offer_string();
        ticket.s.zeroize();
        let did = did_of(&ticket.scid);
        {
            // Noted before the claim's row goes in, so `identity.endPrepared` cancels it.
            let mut offers = prepared.offers();
            if !offers.contains(&ticket.offer_id) {
                offers.push(ticket.offer_id.clone());
            }
        }
        let claimed = self.claims().claim_for_main(&offer, custody, None).await;
        offer.s.zeroize();
        let taken = match claimed {
            Ok(claim) => {
                log::info!(
                    "identity: claimed offer {} of {did} from the pre-join inbox. Match code: {}",
                    claim.offer_id,
                    claim.match_code
                );
                // After the claim: the code the device shows is the one the holder sees.
                publish(
                    Topic::LinkTicket,
                    LinkTicketEvent {
                        did,
                        offer_id: claim.offer_id,
                        match_code: claim.match_code,
                        needs_accept: claim.needs_accept,
                    },
                );
                true
            }
            Err(e) => {
                log::warn!(
                    "identity: the pre-join ticket for offer {} of {did} was not claimed: {}",
                    ticket.offer_id,
                    e.message
                );
                false
            }
        };
        self.ack_pre(read, &message.id).await;
        taken
    }

    /// `identity.endPrepared`: ends the device string `device`. Its pre-join read stops, a round that
    /// is taking tickets finishes first, and every claim the read made is cancelled, so none binds
    /// this executor when the holder confirms. The cancel is local, as `identity.cancelJoin`'s: the
    /// store is not told and may still accept the holder's confirmation. `already-joined` (with the
    /// offer and identity) when one bound already; a cancel does not undo it. The first answer is
    /// kept: every later call gives it. The end runs in a task of its own, so a call dropped while
    /// it waits (`request.cancel`, the session's end) still cancels the claims. `not-found` for a
    /// string this executor did not make since it started, or no longer keeps: from
    /// [`PREPARED_KEPT_S`] after it expired, or once [`PREPARED_KEPT`] newer ones were made.
    pub async fn end_prepared(&self, device: &str) -> Result<PreparedEnded, WsRpcError> {
        let prepared = self
            .inbox
            .prepared()
            .iter()
            .find(|p| p.device == device)
            .cloned()
            .ok_or_else(|| WsRpcError::not_found("not-found: no device string of this executor"))?;
        prepared.done.cancel();
        // Its own task: a dropped call stops waiting for it, not the cancel of the claims.
        tokio::spawn(async move {
            prepared
                .ended
                .get_or_try_init(|| end_claims(&prepared))
                .await
                .cloned()
        })
        .await
        .map_err(|e| WsRpcError::internal(format!("ending the device string failed: {e}")))?
    }
}

/// Waits for the round of `prepared`'s read under way (its `done` is cancelled, so no later
/// round takes a ticket), then cancels every claim the read made: the answer of
/// `identity.endPrepared`.
async fn end_claims(prepared: &Prepared) -> Result<PreparedEnded, WsRpcError> {
    drop(prepared.turn.lock().await);
    let (agent_did, _) = device_key(&AgentContext::main_agent()).map_err(WsRpcError::internal)?;
    let offers = prepared.offers().clone();
    let (mut cancelled, mut joined) = (vec![], None);
    for offer_id in offers {
        match db(|db| db.cancel_join(&offer_id, &agent_did))? {
            JoinCancel::Cancelled => cancelled.push(offer_id),
            // The claim was refused before its row went in.
            JoinCancel::NoClaim => {}
            JoinCancel::AlreadyJoined => {
                joined.get_or_insert(offer_id);
            }
        }
    }
    if let Some(offer_id) = joined {
        let scid = db(|db| db.join(&offer_id))?
            .map(|join| join.scid)
            .ok_or_else(|| WsRpcError::internal("a joined claim has no row"))?;
        return Ok(PreparedEnded::AlreadyJoined {
            offer_id,
            did: did_of(&scid),
        });
    }
    log::info!(
        "identity: the device string's link is ended; claims cancelled: {}",
        cancelled.len()
    );
    Ok(PreparedEnded::Ended { cancelled })
}

/// Wakes the inbox pull (the wallet unlocked, or the main agent was bound to an identity).
/// Does nothing before the identity module starts.
pub fn wake() {
    if let Ok(service) = super::service() {
        service.inbox.wake.notify_one();
    }
}

/// The inbox pull: at start, at each [`wake`], and [`PULL_EVERY`] after the last read.
pub async fn run_pull(service: Arc<IdentityService>) {
    let pulled = service.clone();
    run_schedule(&service.inbox.wake, PULL_EVERY, move || {
        let service = pulled.clone();
        async move { service.pull_main().await }
    })
    .await
}

/// Runs `pull` at once, at each notification of `wake`, and `every` after the last run.
pub(crate) async fn run_schedule<F, Fut>(wake: &Notify, every: Duration, mut pull: F)
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = ()>,
{
    let mut tick = tokio::time::interval(every);
    tick.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
    loop {
        tokio::select! {
            _ = tick.tick() => {}
            _ = wake.notified() => tick.reset(),
        }
        pull().await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use crate::identity::cache::tests::{trusting, FakeStore, TestLog};
    use crate::identity::record::tests::Bound;
    use ad4m_identity_core::inbox::{seal, sign_payload, verify_pre_auth, OfferTicket};
    use ad4m_identity_core::link::{offer_commit, sign_offer, Offer, OfferKind, SignedBy};
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// The position of `log`'s master `m0` at its head.
    fn by_master(log: &TestLog) -> SignedBy {
        let chain = log.chain();
        let head = chain.head();
        SignedBy {
            key_id: "m0".into(),
            kel_seq: head.seq,
            kel_hash: head.hash.clone(),
        }
    }

    /// `body` from `log`'s master to the device `to`, sealed to `to_enc` for `address`.
    fn sealed(
        log: &TestLog,
        to: &str,
        to_enc: &str,
        address: &SealedAt,
        body: InboxBody,
    ) -> String {
        let payload = sign_payload(to.into(), body, now(), by_master(log), &log.master).unwrap();
        seal(to_enc, address, &payload, &mut rand::rngs::OsRng).unwrap()
    }

    /// Master key `index` of `log` with the secret `secret`.
    fn master_body(log: &TestLog, index: u32, secret: &[u8; 32]) -> InboxBody {
        InboxBody::Master {
            did: did_of(&log.scid()),
            index,
            secret: bs58::encode(secret).into_string(),
        }
    }

    fn joined_of(world: &Bound) -> (InboxAddress, SealedAt) {
        let scid = world.log.scid();
        (
            InboxAddress::Joined {
                scid: scid.clone(),
                key_id: "d2".into(),
            },
            SealedAt::Joined {
                scid,
                key_id: "d2".into(),
            },
        )
    }

    /// The inbox events published since `events` subscribed.
    fn inbox_events(events: &mut tokio::sync::broadcast::Receiver<Arc<Event>>) -> Vec<Arc<Event>> {
        let mut seen = vec![];
        while let Ok(event) = events.try_recv() {
            if matches!(event.topic, Topic::InboxMessage | Topic::LinkTicket) {
                seen.push(event);
            }
        }
        seen
    }

    /// An executor without the custody channel keeps no master key: it deletes each message
    /// of its inbox at the pull, lists none, and announces none.
    #[tokio::test]
    async fn without_the_custody_channel_a_pulled_message_is_deleted_unopened() {
        let world = Bound::new(81).await;
        let (address, sealed_at) = joined_of(&world);
        let enc = enc_key_of(&world.key).unwrap();
        let master = sealed(
            &world.log,
            &world.key,
            &enc,
            &sealed_at,
            master_body(&world.log, 0, &world.log.master.to_bytes()),
        );
        world.store.deliver(address, "m1", master);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let listed = world.service.pull_inbox(&world.agent, false).await.unwrap();
        assert!(listed.is_empty());
        assert_eq!(*world.store.acks.lock().unwrap(), vec!["m1".to_string()]);
        assert!(world.store.inbox.lock().unwrap().is_empty());
        assert!(inbox_events(&mut events).is_empty());
        // Every read and the deletion are signed by the device key itself.
        for auth in world.store.inbox_auths.lock().unwrap().iter() {
            let InboxAuth::Request(request) = auth else {
                panic!("a joined inbox takes a signed request");
            };
            assert_eq!(request.req.key_id, "d2");
        }
    }

    /// With the custody channel the pull opens a new message for its type only: the inbox
    /// lists it by id and type, never a body, the message stays in the store, and
    /// `inbox-message` goes to the operator on this machine once. A message that does not open
    /// with this device's key is deleted.
    #[tokio::test]
    async fn with_the_custody_channel_the_inbox_lists_a_master_key_by_id_and_type_only() {
        let world = Bound::new(82).await;
        let (address, sealed_at) = joined_of(&world);
        let enc = enc_key_of(&world.key).unwrap();
        let secret = world.log.master.to_bytes();
        let master = sealed(
            &world.log,
            &world.key,
            &enc,
            &sealed_at,
            master_body(&world.log, 0, &secret),
        );
        world.store.deliver(address.clone(), "m1", master);
        let elsewhere = sealed(
            &world.log,
            &world.key,
            &enc,
            &SealedAt::Pre("elsewhere".into()),
            master_body(&world.log, 0, &secret),
        );
        world.store.deliver(address, "m2", elsewhere);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let listed = world.service.pull_inbox(&world.agent, true).await.unwrap();
        assert_eq!(listed.len(), 1);
        assert_eq!(
            (listed[0].id.as_str(), listed[0].kind),
            ("m1", InboxMessageType::Master)
        );
        let json = serde_json::to_value(&listed).unwrap();
        let mut fields: Vec<&String> = json[0].as_object().unwrap().keys().collect();
        fields.sort();
        assert_eq!(fields, ["expires", "id", "kind", "storedAt"]);
        assert_eq!(json[0]["kind"], "master");
        let secret_text = bs58::encode(secret).into_string();
        assert!(!json.to_string().contains(&secret_text));
        assert_eq!(*world.store.acks.lock().unwrap(), vec!["m2".to_string()]);
        assert_eq!(world.store.inbox.lock().unwrap().len(), 1, "m1 waits");

        let announced = inbox_events(&mut events);
        assert_eq!(announced.len(), 1);
        assert_eq!(announced[0].audience, Audience::LocalOperator);
        assert_eq!(
            announced[0].payload,
            serde_json::json!({ "id": "m1", "kind": "master" })
        );

        // A second read lists it again and announces nothing new.
        let again = world.service.pull_inbox(&world.agent, true).await.unwrap();
        assert_eq!(again, listed);
        assert!(inbox_events(&mut events).is_empty());
    }

    /// The pull and `identity.inbox` take turns: two reads at once open and announce a new
    /// message once.
    #[tokio::test]
    async fn two_reads_at_once_announce_a_message_once() {
        let world = Bound::new(89).await;
        let (address, sealed_at) = joined_of(&world);
        let enc = enc_key_of(&world.key).unwrap();
        let master = sealed(
            &world.log,
            &world.key,
            &enc,
            &sealed_at,
            master_body(&world.log, 0, &world.log.master.to_bytes()),
        );
        world.store.deliver(address, "m1", master);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let (first, second) = tokio::join!(
            world.service.pull_inbox(&world.agent, true),
            world.service.pull_inbox(&world.agent, true)
        );
        assert_eq!((first.unwrap().len(), second.unwrap().len()), (1, 1));
        assert_eq!(inbox_events(&mut events).len(), 1);
    }

    /// `identity.takeMaster`'s work: the current master key, checked against the log, in the
    /// answer only; the message waits for the acknowledgement. A master key whose secret is
    /// not the log's is deleted and never returned.
    #[tokio::test]
    async fn take_master_returns_the_checked_master_key_and_drops_a_false_one() {
        let world = Bound::new(83).await;
        let (address, sealed_at) = joined_of(&world);
        let enc = enc_key_of(&world.key).unwrap();
        let wrong = sealed(
            &world.log,
            &world.key,
            &enc,
            &sealed_at,
            master_body(&world.log, 0, &[9; 32]),
        );
        world.store.deliver(address.clone(), "m1", wrong);
        let secret = world.log.master.to_bytes();
        let right = sealed(
            &world.log,
            &world.key,
            &enc,
            &sealed_at,
            master_body(&world.log, 0, &secret),
        );
        world.store.deliver(address, "m2", right);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let taken = world
            .service
            .take_master(&world.agent)
            .await
            .unwrap()
            .expect("the current master key");
        assert_eq!(taken.message_id, "m2");
        assert_eq!(taken.did, did_of(&world.log.scid()));
        assert_eq!(taken.index, 0);
        assert_eq!(taken.secret, bs58::encode(secret).into_string());
        assert!(!format!("{taken:?}").contains(&taken.secret));
        assert_eq!(*world.store.acks.lock().unwrap(), vec!["m1".to_string()]);
        assert!(inbox_events(&mut events).is_empty(), "no event carries it");
        drop(taken);

        assert!(world.service.inbox_ack(&world.agent, "m2").await.unwrap());
        assert!(world.store.inbox.lock().unwrap().is_empty());
        assert!(world
            .service
            .take_master(&world.agent)
            .await
            .unwrap()
            .is_none());
    }

    /// The pull runs at once, at each wake-up, and every period after the last run (a wake-up
    /// starts the period again).
    #[tokio::test(start_paused = true)]
    async fn the_pull_runs_at_start_at_each_wake_and_every_period() {
        let wake = Arc::new(Notify::new());
        let runs = Arc::new(AtomicUsize::new(0));
        let every = Duration::from_secs(15 * 60);
        let schedule = {
            let (wake, runs) = (wake.clone(), runs.clone());
            tokio::spawn(async move {
                run_schedule(&wake, every, || {
                    let runs = runs.clone();
                    async move {
                        runs.fetch_add(1, Ordering::SeqCst);
                    }
                })
                .await
            })
        };
        let settle = || tokio::time::sleep(Duration::from_millis(1));
        settle().await;
        assert_eq!(runs.load(Ordering::SeqCst), 1, "at start");
        tokio::time::sleep(every - Duration::from_secs(1)).await;
        assert_eq!(runs.load(Ordering::SeqCst), 1);
        tokio::time::sleep(Duration::from_secs(2)).await;
        assert_eq!(runs.load(Ordering::SeqCst), 2, "after the period");
        wake.notify_one();
        settle().await;
        assert_eq!(runs.load(Ordering::SeqCst), 3, "at the wake-up");
        tokio::time::sleep(every - Duration::from_secs(1)).await;
        assert_eq!(
            runs.load(Ordering::SeqCst),
            3,
            "the wake-up started the period again"
        );
        tokio::time::sleep(Duration::from_secs(2)).await;
        assert_eq!(runs.load(Ordering::SeqCst), 4);
        schedule.abort();
    }

    /// The pull's first run comes at start, which may be before the agent service exists: it
    /// reads nothing, and neither panics nor poisons the agent service's lock (an executor
    /// that did failed to start).
    #[tokio::test]
    async fn the_pull_before_the_agent_service_exists_reads_nothing() {
        let log = TestLog::new(92);
        let (service, store) = holder_store(&log);
        let global = crate::agent::AgentService::global_instance();
        let saved = global.lock().unwrap_or_else(|e| e.into_inner()).take();
        let pulled = tokio::spawn(async move { service.pull_main().await }).await;
        let poisoned = global.is_poisoned();
        global.clear_poison();
        *global.lock().unwrap_or_else(|e| e.into_inner()) = saved;
        assert!(pulled.is_ok(), "the pull panicked");
        assert!(!poisoned);
        assert!(store.inbox_auths.lock().unwrap().is_empty());
    }

    /// The operator record of the shared test database for one test.
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

    /// A service over a store that holds `log`, the holder's identity.
    fn holder_store(log: &TestLog) -> (Arc<IdentityService>, Arc<FakeStore>) {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let store = Arc::new(FakeStore::new(log.events.clone()));
        let cache = Arc::new(trusting());
        let backend: Arc<dyn crate::identity::backend::IdentityBackend> = store.clone();
        let service = Arc::new(IdentityService {
            resolver: Arc::new(crate::identity::resolver::Resolver::new(
                cache.clone(),
                backend.clone(),
                None,
                false,
            )),
            backend,
            cache,
            store_url: Some("http://store.test".into()),
            challenges: Default::default(),
            inbox: Default::default(),
            audiences: Default::default(),
            store_keys_source: crate::identity::StoreSource::Config,
        });
        (service, store)
    }

    /// The holder's side after scanning `device`: an offer bound to its key, sealed in a
    /// ticket into its pre-join inbox. Answers the offer id.
    fn push_ticket(log: &TestLog, store: &FakeStore, device: &DeviceString) -> String {
        let s = [5u8; 32];
        let offer_id = bs58::encode(rand::random::<[u8; 16]>()).into_string();
        let offer = Offer {
            scid: log.scid(),
            offer_id: offer_id.clone(),
            kind: OfferKind::Device,
            commit: offer_commit(&s),
            expires: now() + 200,
            device_key: Some(device.sign.clone()),
            bot: None,
        };
        let signed_offer = sign_offer(offer, by_master(log), &log.master).unwrap();
        let ticket = InboxBody::OfferTicket(Box::new(OfferTicket {
            store: device.store.clone(),
            scid: log.scid(),
            offer_id: offer_id.clone(),
            s: bs58::encode(s).into_string(),
            signed_offer,
        }));
        let address = pre_address(&device.sign, &device.r).unwrap();
        let text = sealed(
            log,
            &device.sign,
            &device.enc,
            &SealedAt::Pre(address.clone()),
            ticket,
        );
        store.deliver(InboxAddress::Pre { pre: address }, "t1", text);
        offer_id
    }

    async fn until(what: &str, mut done: impl FnMut() -> bool) {
        for _ in 0..500 {
            if done() {
                return;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        panic!("timed out waiting for {what}");
    }

    fn ends(service: &IdentityService) {
        for open in service.inbox.prepared().drain(..) {
            open.done.cancel();
        }
    }

    /// `identity.prepareDevice`'s device string names the main key, its `enc` key and the
    /// store; the main key claims a ticket in its pre-join inbox with its `enc` key and, with
    /// the custody channel, `canKeepMaster`, and the claim sends `link-ticket` with the match
    /// code the holder shows. The reads are the device key's pre-join reads for this store.
    #[tokio::test]
    async fn a_ticket_in_the_pre_join_inbox_is_announced_and_claimed() {
        let log = TestLog::new(84);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();

        let prepared = prepare_device(&service, true).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let (_, main_key) = device_key(&AgentContext::main_agent()).unwrap();
        assert_eq!(
            (device.sign.as_str(), prepared.sign.as_str()),
            (main_key.as_str(), main_key.as_str())
        );
        assert_eq!(device.enc, enc_key_of(&main_key).unwrap());
        assert_eq!(device.store, "http://store.test");
        assert_eq!(device.exp, prepared.expires);
        let offer_id = push_ticket(&log, &store, &device);

        until("the claim", || !store.claims.lock().unwrap().is_empty()).await;
        let (claimed, claim) = store.claims.lock().unwrap()[0].clone();
        assert_eq!(claimed, offer_id);
        assert_eq!(claim.device_key, main_key);
        assert_eq!(claim.enc_key, device.enc);
        assert!(claim.can_keep_master);
        until("the acknowledgement", || {
            !store.acks.lock().unwrap().is_empty()
        })
        .await;
        assert_eq!(*store.acks.lock().unwrap(), vec!["t1".to_string()]);

        let announced = inbox_events(&mut events);
        assert_eq!(announced.len(), 1);
        assert_eq!(announced[0].topic, Topic::LinkTicket);
        assert_eq!(announced[0].audience, Audience::LocalOperator);
        let event: LinkTicketEvent = serde_json::from_value(announced[0].payload.clone()).unwrap();
        assert_eq!(event.did, did_of(&log.scid()));
        assert_eq!(
            event.match_code,
            link::match_code(&log.scid(), &offer_id, &main_key)
        );
        // With an operator record the claim sets none: it needs no accept.
        assert!(!event.needs_accept);

        let store_did = format!("did:key:{}", store.key.multikey());
        for auth in store.inbox_auths.lock().unwrap().iter() {
            let InboxAuth::Pre(auth) = auth else {
                panic!("a pre-join inbox takes a pre-join read");
            };
            let core: ad4m_identity_core::inbox::PreInboxAuth = convert(auth).unwrap();
            verify_pre_auth(&core).unwrap();
            assert_eq!(
                (core.sign.as_str(), core.store.as_str()),
                (main_key.as_str(), store_did.as_str())
            );
        }
        ends(&service);
    }

    /// A store behind the cache (restored from a backup: it answers no log of an identity whose
    /// head it signed) is no fork and no outage: the ticket is checked against the cached log
    /// and claimed, not left waiting.
    #[tokio::test]
    async fn a_ticket_is_claimed_against_the_cached_log_while_the_store_is_behind() {
        let log = TestLog::new(93);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        service
            .cache
            .refresh(store.as_ref(), &log.scid())
            .await
            .unwrap();
        store.events.lock().unwrap().clear();
        assert!(matches!(
            service.cache.refresh(store.as_ref(), &log.scid()).await,
            Err(CacheError::Behind { store: None, .. })
        ));
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let offer_id = push_ticket(&log, &store, &device);

        until("the claim", || !store.claims.lock().unwrap().is_empty()).await;
        assert_eq!(store.claims.lock().unwrap()[0].0, offer_id);
        ends(&service);
    }

    /// On the ticket path the first-link rule holds as on the RPC path: with an operator record
    /// for another identity the ticket is refused (`first-link`), deleted, nothing claimed, and
    /// no `link-ticket` shows a match code for a claim that never happened.
    #[tokio::test]
    async fn a_ticket_of_another_identity_than_the_operator_is_not_claimed() {
        let log = TestLog::new(85);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&TestLog::new(86).scid()));
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        push_ticket(&log, &store, &device);

        until("the acknowledgement", || {
            !store.acks.lock().unwrap().is_empty()
        })
        .await;
        assert!(store.claims.lock().unwrap().is_empty());
        assert!(inbox_events(&mut events).is_empty());
        ends(&service);
    }

    /// "Show my code" on a desktop app's executor at its first run (`--custody-channel`, no
    /// operator record, the main agent of no identity): the ticket is claimed, not refused
    /// `no-operator`, and the claim records no operator: until the main agent joins, "Show my
    /// code" again takes a ticket of another identity (the record is written at the join,
    /// `joinStatus`, as on the RPC path). The claim waits for the person here to accept it:
    /// `link-ticket` says `needsAccept` and the claim is held.
    #[tokio::test]
    async fn a_desktop_with_no_operator_record_claims_its_first_ticket() {
        let log = TestLog::new(88);
        let (service, store) = holder_store(&log);
        let (main_did, _) = device_key(&AgentContext::main_agent()).unwrap();
        Ad4mDb::with_global_instance(|db| db.set_operator_record_for_test(None)).unwrap();
        Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&main_did)).unwrap();
        let _record = OperatorRecord;
        let _channel = crate::config::CustodyChannelForTest::on();
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let prepared = prepare_device(&service, true).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let offer_id = push_ticket(&log, &store, &device);

        until("the claim", || !store.claims.lock().unwrap().is_empty()).await;
        assert_eq!(store.claims.lock().unwrap()[0].0, offer_id);
        let record = || Ad4mDb::with_global_instance(|db| db.operator_record()).unwrap();
        assert_eq!(record(), None, "a claim alone records no operator");
        until("the acknowledgement", || {
            !store.acks.lock().unwrap().is_empty()
        })
        .await;
        let announced = inbox_events(&mut events);
        assert_eq!(announced.len(), 1);
        assert_eq!(announced[0].topic, Topic::LinkTicket);
        let event: LinkTicketEvent = serde_json::from_value(announced[0].payload.clone()).unwrap();
        assert_eq!(event.offer_id, offer_id);
        assert!(event.needs_accept, "a first-run claim waits for accept");
        assert_eq!(
            Ad4mDb::with_global_instance(|db| db.join(&offer_id))
                .unwrap()
                .map(|j| j.accept),
            Some(crate::db::JoinAccept::Awaiting)
        );

        // The wrong code was scanned: "Show my code" again, and a ticket of another identity
        // is claimed, not refused `first-link`.
        let other = TestLog::new(89);
        *store.events.lock().unwrap() = other.events.clone();
        let prepared = prepare_device(&service, true).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let second = push_ticket(&other, &store, &device);
        until("the second claim", || {
            store.claims.lock().unwrap().len() == 2
        })
        .await;
        assert_eq!(store.claims.lock().unwrap()[1].0, second);
        assert_eq!(record(), None);
        ends(&service);
    }

    /// A ticket left for the next round (here: its identity's log cannot be read now) comes
    /// back at once on each read; the pre-join read still waits out its shortest round and
    /// does not spin on the store.
    #[tokio::test]
    async fn a_ticket_left_waiting_does_not_spin_the_pre_join_read() {
        let log = TestLog::new(90);
        let (service, store) = holder_store(&log);
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        push_ticket(&TestLog::new(91), &store, &device);

        tokio::time::sleep(Duration::from_millis(2500)).await;
        let reads = store.inbox_auths.lock().unwrap().len();
        assert!((1..=4).contains(&reads), "{reads} reads in 2.5 s");
        assert_eq!(store.inbox.lock().unwrap().len(), 1, "the ticket waits");
        ends(&service);
    }

    /// A master key never travels before a device joins: one in the pre-join inbox is
    /// deleted and nothing is claimed or announced.
    #[tokio::test]
    async fn a_master_key_in_the_pre_join_inbox_is_dropped() {
        let log = TestLog::new(87);
        let (service, store) = holder_store(&log);
        let mut events = crate::pubsub::get_global_pubsub().await.subscribe_events();
        let prepared = prepare_device(&service, true).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let address = pre_address(&device.sign, &device.r).unwrap();
        let text = sealed(
            &log,
            &device.sign,
            &device.enc,
            &SealedAt::Pre(address.clone()),
            master_body(&log, 0, &log.master.to_bytes()),
        );
        store.deliver(InboxAddress::Pre { pre: address }, "t1", text);

        until("the acknowledgement", || {
            !store.acks.lock().unwrap().is_empty()
        })
        .await;
        assert!(store.claims.lock().unwrap().is_empty());
        assert!(inbox_events(&mut events).is_empty());
        ends(&service);
    }

    /// A second `identity.prepareDevice` ends the first one's read.
    #[tokio::test]
    async fn a_new_device_string_ends_the_last_ones_read() {
        let log = TestLog::new(88);
        let (service, _store) = holder_store(&log);
        let first = prepare_device(&service, false).await.unwrap();
        let first_read = service.inbox.prepared().last().unwrap().done.clone();
        let second = prepare_device(&service, false).await.unwrap();
        assert!(first_read.is_cancelled());
        assert_ne!(
            DeviceString::parse(&first.device).unwrap().r,
            DeviceString::parse(&second.device).unwrap().r,
            "a fresh r"
        );
        assert!(!service.inbox.prepared().last().unwrap().done.is_cancelled());
        ends(&service);
    }

    /// The main agent bound to an identity for one test.
    struct Joined(String);

    impl Joined {
        fn to(scid: &str) -> Self {
            let (main_did, _) = device_key(&AgentContext::main_agent()).unwrap();
            Ad4mDb::with_global_instance(|db| db.set_agent_identity(&main_did, scid, now()))
                .unwrap();
            Joined(main_did)
        }
    }

    impl Drop for Joined {
        fn drop(&mut self) {
            let _ = Ad4mDb::with_global_instance(|db| db.clear_agent_identity_for_test(&self.0));
        }
    }

    /// Cancel on the code screen before any ticket came: `identity.endPrepared` stops the read
    /// and cancels nothing. A ticket the holder sends afterwards is not read, so it is never
    /// claimed. Again answers the same; a device string this executor never made is 404.
    #[tokio::test]
    async fn ending_a_code_with_no_ticket_stops_its_read_and_takes_no_later_ticket() {
        let log = TestLog::new(94);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();

        let ended = service.end_prepared(&prepared.device).await.unwrap();
        assert_eq!(ended, PreparedEnded::Ended { cancelled: vec![] });
        assert!(service.inbox.prepared().last().unwrap().done.is_cancelled());

        push_ticket(&log, &store, &device);
        tokio::time::sleep(Duration::from_millis(1500)).await;
        assert!(
            store.claims.lock().unwrap().is_empty(),
            "no claim after the end"
        );
        assert_eq!(
            store.inbox.lock().unwrap().len(),
            1,
            "the ticket is not read"
        );

        assert_eq!(
            service.end_prepared(&prepared.device).await.unwrap(),
            PreparedEnded::Ended { cancelled: vec![] }
        );
        let unknown = service
            .end_prepared("coasys:device:1:unknown")
            .await
            .unwrap_err();
        assert_eq!(unknown.code, 404);
        ends(&service);
    }

    /// A ticket being taken when the person cancels: `identity.endPrepared` waits for it, then
    /// cancels the claim it made, so the claim never binds, and a later claim of that offer is
    /// refused `cancelled`. Again answers the same.
    #[tokio::test]
    async fn ending_a_code_waits_for_the_ticket_being_taken_and_cancels_its_claim() {
        let log = TestLog::new(95);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let offer_id = push_ticket(&log, &store, &device);
        // The read opened the ticket and reads its identity's log.
        tokio::time::timeout(Duration::from_secs(5), entered.notified())
            .await
            .expect("the ticket is being taken");

        let ending = service.end_prepared(&prepared.device);
        tokio::pin!(ending);
        assert!(
            tokio::time::timeout(Duration::from_millis(300), &mut ending)
                .await
                .is_err(),
            "the end waits for the ticket being taken"
        );
        release.notify_one();
        let ended = ending.await.unwrap();
        assert_eq!(
            ended,
            PreparedEnded::Ended {
                cancelled: vec![offer_id.clone()]
            }
        );
        assert_eq!(
            store.claims.lock().unwrap()[0].0,
            offer_id,
            "it was claimed"
        );
        assert_eq!(
            Ad4mDb::with_global_instance(|db| db.join(&offer_id))
                .unwrap()
                .map(|j| j.accept),
            Some(crate::db::JoinAccept::Cancelled)
        );

        // A later claim of the offer (a second ticket, a pasted link) is refused.
        let offer = link::OfferString {
            scid: log.scid(),
            offer_id: offer_id.clone(),
            s: bs58::encode([5u8; 32]).into_string(),
            store: "http://store.test".into(),
        };
        let again = service
            .claims()
            .claim_for_main(&offer, false, None)
            .await
            .unwrap_err();
        assert_eq!(again.code, 409);
        assert!(again.message.starts_with("cancelled"), "{}", again.message);
        assert_eq!(store.claims.lock().unwrap().len(), 1);

        assert_eq!(service.end_prepared(&prepared.device).await.unwrap(), ended);
        ends(&service);
    }

    /// Cancel after the claim joined: `identity.endPrepared` does not undo it and answers
    /// `already-joined` with the offer and the identity, again too.
    #[tokio::test]
    async fn ending_a_code_whose_claim_joined_answers_already_joined() {
        let log = TestLog::new(96);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let offer_id = push_ticket(&log, &store, &device);
        until("the claim", || !store.claims.lock().unwrap().is_empty()).await;
        let _joined = Joined::to(&log.scid());

        let joined = PreparedEnded::AlreadyJoined {
            offer_id: offer_id.clone(),
            did: did_of(&log.scid()),
        };
        assert_eq!(
            service.end_prepared(&prepared.device).await.unwrap(),
            joined
        );
        assert_ne!(
            Ad4mDb::with_global_instance(|db| db.join(&offer_id))
                .unwrap()
                .map(|j| j.accept),
            Some(crate::db::JoinAccept::Cancelled),
            "a joined claim is not cancelled"
        );
        assert_eq!(
            service.end_prepared(&prepared.device).await.unwrap(),
            joined
        );
        ends(&service);
    }

    /// The call dropped (`request.cancel`, the session's end) while the end waits for the
    /// ticket being taken: the claim the read makes is still cancelled, so accepting it is
    /// refused and the holder's confirmation binds nothing.
    #[tokio::test]
    async fn an_end_dropped_while_a_ticket_is_taken_still_cancels_its_claim() {
        let log = TestLog::new(97);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let (entered, release) = (Arc::new(Notify::new()), Arc::new(Notify::new()));
        *store.pause_store.lock().unwrap() = Some((entered.clone(), release.clone()));
        let prepared = prepare_device(&service, false).await.unwrap();
        let device = DeviceString::parse(&prepared.device).unwrap();
        let offer_id = push_ticket(&log, &store, &device);
        tokio::time::timeout(Duration::from_secs(5), entered.notified())
            .await
            .expect("the ticket is being taken");

        let mut ending = Box::pin(service.end_prepared(&prepared.device));
        assert!(
            tokio::time::timeout(Duration::from_millis(300), &mut ending)
                .await
                .is_err(),
            "the end waits for the ticket being taken"
        );
        drop(ending);
        release.notify_one();

        let accept = || {
            Ad4mDb::with_global_instance(|db| db.join(&offer_id))
                .unwrap()
                .map(|j| j.accept)
        };
        until("the claim's cancel", || {
            accept() == Some(crate::db::JoinAccept::Cancelled)
        })
        .await;
        assert_eq!(
            store.claims.lock().unwrap()[0].0,
            offer_id,
            "it was claimed"
        );
        let refused = service
            .claims()
            .accept_join(&AgentContext::main_agent(), &offer_id)
            .unwrap_err();
        assert_eq!(refused.code, 409);
        assert!(
            refused.message.starts_with("cancelled"),
            "{}",
            refused.message
        );
        let (main_did, _) = device_key(&AgentContext::main_agent()).unwrap();
        assert!(
            Ad4mDb::with_global_instance(|db| {
                db.set_agent_identity_of_join(&main_did, &log.scid(), now(), &offer_id)
            })
            .is_err(),
            "the holder's confirmation binds nothing"
        );
        assert_eq!(
            Ad4mDb::with_global_instance(|db| db.agent_identity(&main_did)).unwrap(),
            None
        );
        assert_eq!(
            service.end_prepared(&prepared.device).await.unwrap(),
            PreparedEnded::Ended {
                cancelled: vec![offer_id]
            }
        );
        ends(&service);
    }

    /// End string A after its claim, then join the same identity through string B: ending A
    /// again still answers `ended` with A's claim, and B's join is B's.
    #[tokio::test]
    async fn ending_a_code_again_after_another_code_joined_answers_the_same() {
        let log = TestLog::new(98);
        let (service, store) = holder_store(&log);
        let _record = OperatorRecord::set(&did_of(&log.scid()));
        let first = prepare_device(&service, false).await.unwrap();
        let first_offer = push_ticket(&log, &store, &DeviceString::parse(&first.device).unwrap());
        until("the first claim", || {
            store.claims.lock().unwrap().len() == 1
        })
        .await;
        let ended = PreparedEnded::Ended {
            cancelled: vec![first_offer],
        };
        assert_eq!(service.end_prepared(&first.device).await.unwrap(), ended);

        let second = prepare_device(&service, false).await.unwrap();
        let second_offer = push_ticket(&log, &store, &DeviceString::parse(&second.device).unwrap());
        until("the second claim", || {
            store.claims.lock().unwrap().len() == 2
        })
        .await;
        let (main_did, _) = device_key(&AgentContext::main_agent()).unwrap();
        Ad4mDb::with_global_instance(|db| {
            db.set_agent_identity_of_join(&main_did, &log.scid(), now(), &second_offer)
        })
        .unwrap();
        let _joined = Joined(main_did);

        assert_eq!(service.end_prepared(&first.device).await.unwrap(), ended);
        assert_eq!(
            service.end_prepared(&second.device).await.unwrap(),
            PreparedEnded::AlreadyJoined {
                offer_id: second_offer,
                did: did_of(&log.scid()),
            }
        );
        ends(&service);
    }
}
