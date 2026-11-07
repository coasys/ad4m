//! `link`, the holder's side: add a device or an executor to the identity. The device's side
//! (the claim) runs on the executor (`identity.claimOffer`, or the ticket of its pre-join inbox).
//!
//! 1. [`LinkFlow::open`]: one unlock of the vault for the whole flow; an offer signed by the
//!    current master key at the log's head (`identity.openOffer`); the offer link carries the
//!    secret `s` in its fragment only.
//! 2. The surface hands the link over (QR code, paste, `ad4m-executor claim <link>`). Or the
//!    holder scanned the device's string ([`LinkFlow::scan`]): the offer is bound to the
//!    device's key, and its ticket, sealed to the device's `enc` key and signed by the master
//!    key, goes into the device's pre-join inbox (`identity.inboxPut` with `bind`).
//! 3. [`LinkFlow::wait_for_claim`]: master-signed list requests (`identity.offers`) until the
//!    offer is claimed; the device's co-signature must cover the claim the store answered
//!    (`link::verify_claim_view`); the claim's device key gives the match code both sides show.
//! 4. [`LinkFlow::confirm`] after the person sees the same code: the `add` that spends the
//!    claim, signed by the current master key (`identity.publish`); then, when the person
//!    ticked "Keep my master key on this device too", the master key into the device's inbox
//!    ([`super::master`]); then the device's entry in the private record.

use std::cell::Cell;

use ad4m_identity_core::build::add_claimed;
use ad4m_identity_core::inbox::{
    pre_address, seal, sign_payload, InboxAddress, InboxBody, OfferTicket,
};
use ad4m_identity_core::link::{
    self, ClaimView, DeviceString, Offer, OfferKind, OfferString, SignedOffer,
};
use ad4m_identity_core::record::{DeviceEntry, HoldsMaster};
use ad4m_identity_core::{BotScope, Event, KelChain, KeyId, Op};
use ad4m_ldk::identity::{InboxBind, InboxWrite, OfferState, SignedHead, SignedRequest};
use serde_json::json;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::record::{self, Change};
use crate::request::{
    b58_random, body_bytes, chain_of, log_of, master_at_head, master_request, scid_of, store_of,
    MasterPosition,
};
use crate::secret::Secret;
use crate::vault::{MasterUse, Unlocked, Vault};
use crate::{fold, ClientError, Clock, Random, RandomRng};

/// The seconds each list request waits for a change (the store's longest wait).
const LIST_WAIT_S: u8 = 8;

/// A claim that arrived: what the confirm card shows.
#[derive(Clone, Debug, PartialEq)]
pub struct Claimed {
    /// "R7K Q2M": the joining device shows the same code.
    pub match_code: String,
    pub device_key: String,
    pub hint: Option<String>,
    /// The device says it may keep the master key (an app): the card shows "Keep my master key
    /// on this device too" only then, and only for a `device` offer ([`LinkFlow::may_keep`]).
    pub can_keep_master: bool,
    claim: ClaimView,
}

/// A link confirmed.
#[derive(Debug)]
pub struct Linked {
    pub head: SignedHead,
    /// The new device key's id in the log.
    pub key_id: KeyId,
    /// The master key put into the device's inbox, when the person chose to: its message id,
    /// or why the push failed (a push that may pass later stays on the vault's retry list).
    pub master: Option<Result<String, ClientError>>,
    /// The record write that lists the device. The `add` stands either way.
    pub record: Result<u64, ClientError>,
}

/// The holder's side of one link.
pub struct LinkFlow<'a, R: ExecutorRpc> {
    executor: &'a ViaExecutor<R>,
    vault: &'a dyn Vault,
    clock: &'a dyn Clock,
    random: &'a dyn Random,
    unlocked: Unlocked,
    did: String,
    /// The store's `did:key`, from the head the executor accepted: what a signed request names.
    store: String,
    offer: Offer,
    signed: SignedOffer,
    offer_string: OfferString,
    _secret: Secret<[u8; 32]>,
    /// `confirm` got past the add and ran the steps after it (the push, the record write).
    followed_up: Cell<bool>,
}

impl<'a, R: ExecutorRpc> LinkFlow<'a, R> {
    /// Opens an offer of `kind` (`Node` for an executor) for the identity `did`, whose store
    /// is at `store_url`, and makes its link. With `device_key` (the signing key of a device
    /// string the holder scanned) the offer is bound to it: only that key may claim it, and its
    /// [`LinkFlow::ticket`] goes to the device's pre-join inbox ([`LinkFlow::scan`] does both).
    #[allow(clippy::too_many_arguments)]
    pub async fn open(
        executor: &'a ViaExecutor<R>,
        vault: &'a dyn Vault,
        did: &str,
        kind: OfferKind,
        device_key: Option<&str>,
        store_url: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<LinkFlow<'a, R>, ClientError> {
        if kind == OfferKind::Bot {
            return Err(ClientError::Protocol(
                "a bot offer carries a scope: open it with LinkFlow::open_bot".into(),
            ));
        }
        Self::open_with(
            executor, vault, did, kind, device_key, None, store_url, clock, random,
        )
        .await
    }

    /// Opens a bot offer for the identity `did`: the bot's key joins with the bot mark and
    /// `scope` (the actions it may take and until when, `expires` in seconds). An executor
    /// claims it with `bot.createAgent`, and the add the holder confirms lists the key as a
    /// bot's: it acts for the person within the scope and never speaks for them.
    #[allow(clippy::too_many_arguments)]
    pub async fn open_bot(
        executor: &'a ViaExecutor<R>,
        vault: &'a dyn Vault,
        did: &str,
        scope: BotScope,
        store_url: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<LinkFlow<'a, R>, ClientError> {
        Self::open_with(
            executor,
            vault,
            did,
            OfferKind::Bot,
            None,
            Some(scope),
            store_url,
            clock,
            random,
        )
        .await
    }

    #[allow(clippy::too_many_arguments)]
    async fn open_with(
        executor: &'a ViaExecutor<R>,
        vault: &'a dyn Vault,
        did: &str,
        kind: OfferKind,
        device_key: Option<&str>,
        bot: Option<BotScope>,
        store_url: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<LinkFlow<'a, R>, ClientError> {
        let scid = scid_of(did)?;
        let chain = chain_of(executor, did).await?;
        let store = store_of(executor, did).await?;
        // The unlock and the offer start at the same time, so the unlock lasts exactly to the
        // end of the store's confirm grace ([`MasterUse::ttl_s`]).
        let now = clock.now();
        let unlocked = vault.unlock(&scid, MasterUse::Link, now)?;
        let mut s = zeroize::Zeroizing::new([0u8; link::SECRET_LEN]);
        random.fill(&mut s[..]);
        let secret = Secret::new(*s);
        let offer = Offer {
            scid: scid.clone(),
            offer_id: b58_random(random, link::OFFER_ID_LEN),
            kind,
            commit: link::offer_commit(secret.expose()),
            expires: now + link::OFFER_TTL_S,
            device_key: device_key.map(str::to_string),
            bot,
        };
        link::check_offer_shape(&offer).map_err(|e| ClientError::Protocol(e.to_string()))?;
        let signer = unlocked.signer(clock.now())?;
        let by = master_at_head(&chain, signer)?;
        let signed = link::sign_offer(offer.clone(), by, signer)?;
        let wire: ad4m_ldk::identity::SignedOffer = serde_json::from_value(json!(&signed))
            .map_err(|e| ClientError::Protocol(e.to_string()))?;
        let opened = executor.open_offer(&wire).await?;
        if opened.offer_id != offer.offer_id {
            return Err(ClientError::Protocol(
                "the store answered another offer id".into(),
            ));
        }
        let offer_string = OfferString {
            store: store_url.to_string(),
            scid,
            offer_id: offer.offer_id.clone(),
            s: bs58::encode(secret.expose()).into_string(),
        };
        Ok(LinkFlow {
            executor,
            vault,
            clock,
            random,
            unlocked,
            did: did.to_string(),
            store,
            offer,
            signed,
            offer_string,
            _secret: secret,
            followed_up: Cell::new(false),
        })
    }

    /// The holder scanned `device`'s string: opens an offer of `kind` bound to the device's
    /// key and puts its [`LinkFlow::ticket`] into the device's pre-join inbox, sealed to the
    /// device's `enc` key and signed by the master key. Refused when the string expired or
    /// names another store than `store_url` (the device would never read the ticket).
    #[allow(clippy::too_many_arguments)]
    pub async fn scan(
        executor: &'a ViaExecutor<R>,
        vault: &'a dyn Vault,
        did: &str,
        kind: OfferKind,
        device: &DeviceString,
        store_url: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<LinkFlow<'a, R>, ClientError> {
        if clock.now() >= device.exp {
            return Err(ClientError::Protocol(
                "the device's code expired: it shows a new one".into(),
            ));
        }
        if device.store.trim_end_matches('/') != store_url.trim_end_matches('/') {
            return Err(ClientError::Protocol(format!(
                "the device reads its inbox at {}, not at {store_url}",
                device.store
            )));
        }
        let flow = Self::open(
            executor,
            vault,
            did,
            kind,
            Some(&device.sign),
            store_url,
            clock,
            random,
        )
        .await?;
        flow.put_ticket(device).await?;
        Ok(flow)
    }

    /// Puts the ticket into the pre-join inbox of `device`.
    async fn put_ticket(&self, device: &DeviceString) -> Result<(), ClientError> {
        let now = self.clock.now();
        let chain = chain_of(self.executor, &self.did).await?;
        let signer = self.unlocked.signer(now)?;
        let by = master_at_head(&chain, signer)?;
        let pre = pre_address(&device.sign, &device.r)?;
        let address = InboxAddress::Pre(pre.clone());
        let ticket = InboxBody::OfferTicket(Box::new(self.ticket()));
        let mut payload = sign_payload(device.sign.clone(), ticket, now, by.clone(), signer)?;
        let sealed = seal(&device.enc, &address, &payload, &mut RandomRng(self.random));
        if let InboxBody::OfferTicket(ticket) = &mut payload.body {
            zeroize::Zeroize::zeroize(&mut ticket.s);
        }
        let bind = InboxBind {
            scid: self.offer.scid.clone(),
            offer_id: self.offer.offer_id.clone(),
            sign: device.sign.clone(),
            r: device.r.clone(),
        };
        let write = InboxWrite {
            sealed: sealed?,
            bind: Some(bind.clone()),
        };
        let at = MasterPosition {
            did: &self.did,
            store: &self.store,
            by: &by,
        };
        let path = format!("/v1/inbox/pre/{pre}");
        let request = master_request(
            &at,
            signer,
            "PUT",
            &path,
            &body_bytes(&write)?,
            self.random,
            now,
        )?;
        self.executor
            .inbox_put(&address, &write.sealed, Some(&bind), &request)
            .await?;
        Ok(())
    }

    /// The offer link (`https://id.coasys.org/o#coasys:offer:1:…`): the QR code shows it.
    pub fn link(&self) -> String {
        self.offer_string.link()
    }

    /// The offer string alone (`coasys:offer:1:…`), for a server's CLI.
    pub fn offer_string(&self) -> String {
        self.offer_string.encode()
    }

    pub fn offer_id(&self) -> &str {
        &self.offer.offer_id
    }

    /// The offer as a ticket for the device's inbox (`InboxBody::OfferTicket`): the offer
    /// string's fields and the signed offer, which the device checks against the log.
    pub fn ticket(&self) -> OfferTicket {
        OfferTicket {
            store: self.offer_string.store.clone(),
            scid: self.offer_string.scid.clone(),
            offer_id: self.offer_string.offer_id.clone(),
            s: self.offer_string.s.clone(),
            signed_offer: self.signed.clone(),
        }
    }

    /// Whether the card may offer "Keep my master key on this device too" for `claimed`: a
    /// `device` offer whose claim says the device may keep it. The claim's co-signature covers
    /// `canKeepMaster` and the `add` carries it, so this is the log's rule
    /// ([`ad4m_identity_core::link::may_keep_master`]) before the add; the push after the add
    /// checks the log itself. The record's device kind never decides it.
    pub fn may_keep(&self, claimed: &Claimed) -> bool {
        self.offer.kind == OfferKind::Device && claimed.can_keep_master
    }

    /// A list request signed by the current master key at `chain`'s head.
    fn list_request(&self, chain: &KelChain) -> Result<SignedRequest, ClientError> {
        let now = self.clock.now();
        let signer = self.unlocked.signer(now)?;
        let by = master_at_head(chain, signer)?;
        let at = MasterPosition {
            did: &self.did,
            store: &self.store,
            by: &by,
        };
        let path = format!("/v1/offers/{}", self.offer.scid);
        master_request(&at, signer, "GET", &path, b"", self.random, now)
    }

    /// Waits until the offer is claimed; fails when it ends unclaimed (expired, cancelled,
    /// superseded) or expires. A claim whose co-signature does not cover the view the store
    /// answered (another `encKey` or `canKeepMaster`) is `ClientError::Link(LinkError::CoSig)`.
    pub async fn wait_for_claim(&self) -> Result<Claimed, ClientError> {
        let chain = chain_of(self.executor, &self.did).await?;
        // The store's wait answers at a change or at its timeout, so the first look takes the
        // state as it is and later ones wait for the claim.
        let mut wait = 0;
        loop {
            // Past the offer's expiry the offer ended, whatever the unlock says.
            if self.clock.now() >= self.offer.expires {
                return Err(ClientError::OfferEnded("expired".into()));
            }
            let request = self.list_request(&chain)?;
            let listings = self.executor.offers(&request, wait).await?;
            wait = LIST_WAIT_S;
            let ours = listings
                .into_iter()
                .find(|l| l.offer.offer.offer_id == self.offer.offer_id);
            match ours.map(|l| l.state) {
                Some(OfferState::Claimed { claim }) => {
                    let view: ClaimView = serde_json::from_value(json!(claim))
                        .map_err(|e| ClientError::Protocol(e.to_string()))?;
                    // The match code covers the device key only, and `fold` never reads the
                    // co-signature: check that the device signed this very view (its `encKey`
                    // and `canKeepMaster` too) before the master can sign it into the log.
                    link::verify_claim_view(&self.offer, &view)?;
                    return Ok(Claimed {
                        match_code: link::match_code(
                            &self.offer.scid,
                            &self.offer.offer_id,
                            &view.device_key,
                        ),
                        device_key: view.device_key.clone(),
                        hint: view.hint.clone(),
                        can_keep_master: view.can_keep_master,
                        claim: view,
                    });
                }
                Some(OfferState::Open) => {}
                Some(other) => {
                    return Err(ClientError::OfferEnded(
                        serde_json::to_value(&other)
                            .ok()
                            .and_then(|v| v["state"].as_str().map(str::to_string))
                            .unwrap_or_default(),
                    ))
                }
                // The list holds open and claimed offers only.
                None => return Err(ClientError::OfferEnded("closed".into())),
            }
        }
    }

    /// The person confirmed the same match code: appends the `add` that spends `claimed`;
    /// then, with `keep_master` (only where [`LinkFlow::may_keep`]), puts the master key into
    /// the device's inbox; then writes `device` (its name, kind, …) into the record, with
    /// `holdsMaster: "offered"` when the master key went and `false` otherwise.
    ///
    /// The flow stays: after a failed publish it can confirm again while the claim is
    /// `claimed`. The push and the record write run only after the publish succeeded, so a
    /// failed confirm did neither. When the store took the add but its answer (or the log read
    /// after it) was lost, the offer is `spent` and the confirm failed before both: confirming
    /// again finds, in the refreshed log, the `add` of this offer with this claim's device key
    /// and co-signature, publishes nothing, and runs the push and the record write once, under
    /// the flow's one unlock (a push past [`MasterUse::Link`]'s ttl is refused as expired).
    /// Once a confirm ran those steps, or when the log's add is another key's or another
    /// offer's, a repeat goes to the store, which refuses it before anything is pushed or
    /// written.
    pub async fn confirm(
        &self,
        claimed: &Claimed,
        device: DeviceEntry,
        keep_master: bool,
    ) -> Result<Linked, ClientError> {
        if keep_master && !self.may_keep(claimed) {
            return Err(ClientError::NotAKeeper(claimed.device_key.clone()));
        }
        let log = self.executor.log(&self.did, 0, true).await?;
        let events = log
            .events
            .iter()
            .map(Event::from_json)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| {
                ClientError::Protocol(format!("the log holds an unparsable event: {e}"))
            })?;
        // The store took this claim's add before, but its answer (or the read after it) was
        // lost, so this flow never ran the steps after it: they run now, with no publish. Any
        // other add (another key, another offer) leaves the publish to the store's refusal.
        let landed = !self.followed_up.get()
            && events.iter().any(|e| {
                matches!(&e.op, Op::Add { add, offer, co_sig }
                    if *offer == self.offer.offer_id
                        && *co_sig == claimed.claim.co_sig
                        && add.iter().any(|k| k.sign == claimed.device_key))
            });
        let (head, events, chain) = if landed {
            let head = log.head.ok_or_else(|| {
                ClientError::Protocol("the executor accepted no head for the log".into())
            })?;
            let chain = fold(&events)?;
            (head, events, chain)
        } else {
            let chain = fold(&events)?;
            let signer = self.unlocked.signer(self.clock.now())?;
            let event = add_claimed(&chain, signer, &self.offer, &claimed.claim)?;
            let head = self.executor.publish(std::slice::from_ref(&event)).await?;
            let (events, chain) = log_of(self.executor, &self.did).await?;
            (head, events, chain)
        };
        let key_id = chain
            .state()
            .key_id_of(&claimed.device_key)
            .cloned()
            .ok_or_else(|| ClientError::Protocol("the log does not list the new key".into()))?;
        let signer = if keep_master {
            Some(self.unlocked.signer(self.clock.now())?)
        } else {
            None
        };
        // From here on the steps after the add run (the push is on the retry list before its
        // put): a later confirm goes to the store again, which refuses the spent offer.
        self.followed_up.set(true);
        let master = if let Some(signer) = signer {
            Some(
                super::master::push(
                    self.executor,
                    self.vault,
                    &events,
                    &chain,
                    &self.did,
                    signer,
                    &key_id,
                    self.clock,
                    self.random,
                )
                .await,
            )
        } else {
            None
        };
        let holds_master = match master {
            Some(Ok(_)) => HoldsMaster::Offered,
            _ => HoldsMaster::No,
        };
        let record = record::write(
            self.executor,
            &self.did,
            &[Change::Device {
                key_id: key_id.clone(),
                entry: DeviceEntry {
                    holds_master,
                    ..device
                },
            }],
            self.random,
        )
        .await;
        Ok(Linked {
            head,
            key_id,
            master,
            record,
        })
    }
}
