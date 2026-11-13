//! `ClaimService`: the executor's side of a link. It answers an offer with one of
//! its wallet keys and that key's `enc` key, co-signs the claim, and follows the offer until the
//! holder's `add` lists the key.
//!
//! `claim_for_main` claims with the main wallet key and first runs the first-link rule
//! (`operator::first_link_check`): every path that claims for the main agent calls it, the
//! `identity.claimOffer` handler and the pre-join ticket path of `identity.prepareDevice`
//! (`inbox`), so the rule never depends on an RPC guard. On a desktop app's executor at its first
//! run (`--custody-channel`, no operator record, the main agent of no identity) a claim goes
//! ahead and records nothing; it is held (`JoinAccept::Awaiting`, `needsAccept` in the answer
//! and the `link-ticket` event) until the person at this computer accepts it
//! (`identity.acceptJoin`, local only). While held, `join_status` binds nothing and answers
//! `awaiting-accept`, even once the offer is spent: whoever sees the device string and delivers
//! a ticket first does not take the desktop. Once accepted, the identity the main agent joins
//! (`join_status` binds it once the holder's add lists the key) becomes the operator
//! (`OperatorSetBy::FirstRun`). Until then the desktop may claim again, for any identity.
//! A claim that sets no operator record needs no accept. Any claim not yet bound may be
//! cancelled (`identity.cancelJoin`): it never binds, even if the holder confirms it later.
//! The decision holds on every path that binds: a repeated claim of the offer keeps it (a
//! cancelled claim is refused `cancelled`, the offer named for another identity
//! `offer-conflict`), and `identity.publish` of the holder's add binds the main agent only as
//! `join_status` would (`IdentityService::bind_agent_if_listed`).
//!
//! `claim_for_agent` is the claim itself, for any local agent: `claim_for_main` runs it after
//! its rules and writes the main agent's join row (`add_join`, which keeps a repeated claim's
//! decision) just before the store call, and `bot.createAgent` runs it for a new bot agent
//! (`crate::bots`), whose offer the holder opened as a bot offer; a bot's claim writes no join
//! row and needs no accept. Claims for a hosted tenant's agent are not here yet.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::did_of;
use ad4m_identity_core::link::{self, OfferString};

use super::device::{device_key, enc_key_of, WalletSigner};
use super::operator::{first_link_check, FirstLink};
use super::service::{
    backend_error, cache_error, db, error, first_link_error, now, store_base, Binding,
    IdentityService,
};
use super::types::{convert, ClaimOfferResult, JoinAccepted, JoinCancelled, JoinStatus};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::db::{JoinAccept, JoinAdded, JoinCancel, JoinRow};

/// The claims of one executor.
pub struct ClaimService<'a> {
    identity: &'a IdentityService,
}

impl IdentityService {
    pub fn claims(&self) -> ClaimService<'_> {
        ClaimService { identity: self }
    }
}

impl ClaimService<'_> {
    /// Whether the main agent's key is valid in the log of the identity it speaks for, at the
    /// log's trusted seq (the accepted, store-signed head; after a refresh from the store).
    async fn main_is_linked(&self) -> Result<bool, WsRpcError> {
        let main = AgentContext::main_agent();
        let (agent_did, key) = device_key(&main).map_err(WsRpcError::internal)?;
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(false);
        };
        self.identity.refreshed_or_cached(&scid).await?;
        let trusted = self.identity.cache.trusted(&scid).await;
        Ok(trusted.map_err(cache_error)?.is_some_and(|(chain, seq)| {
            let state = chain.state();
            super::resolver::device_key_id(state, &key)
                .is_some_and(|id| state.key_valid_at_time(&id, seq, now()))
        }))
    }

    /// Claims `offer` with the main wallet key, on every path that claims for the main agent
    /// (`identity.claimOffer`, a pre-join ticket). Refuses `no-operator` with no operator
    /// record and `first-link` while the main key is in no log and the offer is not of the
    /// recorded operator; `already-linked` while the main key speaks for an
    /// identity (a revoked key needs a new main key: `identity.rekey`). With no record on a
    /// `--custody-channel` executor whose main agent speaks for no identity (a desktop app's
    /// first run) the claim goes ahead, records nothing and is held for accept
    /// (`needs_accept`): the operator is recorded only when the main agent joins (`join_status`)
    /// after `identity.acceptJoin`. `can_keep_master` goes into the claim the main key co-signs.
    pub async fn claim_for_main(
        &self,
        offer: &OfferString,
        can_keep_master: bool,
        hint: Option<String>,
    ) -> Result<ClaimOfferResult, WsRpcError> {
        self.identity.refuse_without_store_keys()?;
        let offer_did = did_of(&offer.scid);
        let record = db(|db| db.operator_record())?;
        if self.main_is_linked().await? {
            return Err(error(
                409,
                "already-linked: this executor's key speaks for an identity already",
            ));
        }
        let main = AgentContext::main_agent();
        let desktop_first_run = record.is_none() && crate::config::custody_channel_on() && {
            let (main_did, _) = device_key(&main).map_err(WsRpcError::internal)?;
            db(|db| db.agent_identity(&main_did))?.is_none()
        };
        let first_link = first_link_check(record.as_ref(), &offer_did, desktop_first_run)
            .map_err(first_link_error)?;
        // The row goes in before the store call: when the store commits the claim and its
        // answer is lost, a retry gets `gone` once the add spent the offer, and only this row
        // lets `joinStatus` follow the offer and record the binding. A repeated claim of the
        // offer keeps the row the first one wrote: the decision taken here (accepted,
        // cancelled) stays, a cancelled claim is not sent again, and the row names one
        // identity only.
        let first_run = first_link == FirstLink::FirstRun;
        let claimed = self
            .claim_for_agent(&main, offer, can_keep_master, hint, || {
                let added = db(|db| {
                    db.add_join(&JoinRow {
                        offer_id: offer.offer_id.clone(),
                        scid: offer.scid.clone(),
                        claimed_at: now(),
                        accept: if first_run {
                            JoinAccept::Awaiting
                        } else {
                            JoinAccept::NotNeeded
                        },
                    })
                })?;
                match added {
                    JoinAdded::Added => Ok(first_run),
                    JoinAdded::Kept(join) => Self::held(&join),
                    JoinAdded::Cancelled => Err(error(
                        409,
                        "cancelled: this executor's claim of the offer is cancelled; it is not made again",
                    )),
                    JoinAdded::OtherIdentity(scid) => Err(error(
                        409,
                        format!(
                            "offer-conflict: this executor claimed the offer for {}, not {offer_did}",
                            did_of(&scid)
                        ),
                    )),
                }
            })
            .await?;
        if claimed.needs_accept {
            log::info!("identity: this desktop executor claimed an offer of {offer_did}; it becomes the operator when the person here accepts (identity.acceptJoin), the holder confirms and the main agent joins");
        }
        Ok(claimed)
    }

    /// Claims `offer` with the wallet key of `agent` and that key's `enc` key: the steps every
    /// claim takes, whoever it is for (the main agent, a bot agent). `before_store` runs after
    /// the claim is signed and before the store sees it: the caller records what lets it
    /// follow the offer when the store's answer is lost, may refuse the claim there (nothing
    /// reaches the store), and answers whether the claim is held for accept (`needs_accept`).
    /// Refuses `no-store-keys`, an offer of another store than this executor's
    /// (`offer-store`), and a malformed secret.
    pub async fn claim_for_agent(
        &self,
        agent: &AgentContext,
        offer: &OfferString,
        can_keep_master: bool,
        hint: Option<String>,
        before_store: impl FnOnce() -> Result<bool, WsRpcError>,
    ) -> Result<ClaimOfferResult, WsRpcError> {
        self.identity.refuse_without_store_keys()?;
        let offer_did = did_of(&offer.scid);
        if let Some(ours) = &self.identity.store_url {
            if store_base(ours) != store_base(&offer.store) {
                return Err(WsRpcError::bad_request(format!(
                    "offer-store: the offer names the store {}, this executor uses {ours}",
                    offer.store
                )));
            }
        }
        let secret = link::decode_secret(&offer.s)
            .ok_or_else(|| WsRpcError::bad_request("invalid: the offer's secret is malformed"))?;
        let (_, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let enc = enc_key_of(&key).map_err(WsRpcError::internal)?;
        let signer = WalletSigner::new(agent).map_err(WsRpcError::internal)?;
        let claim = link::make_claim(
            &offer.scid,
            &offer.offer_id,
            secret,
            &signer,
            enc,
            can_keep_master,
            hint,
        )
        .map_err(|e| WsRpcError::bad_request(format!("invalid: {e}")))?;
        signer.check().map_err(WsRpcError::internal)?;
        let claim: ad4m_ldk::identity::Claim =
            convert(&claim).map_err(|e| WsRpcError::internal(e.to_string()))?;
        let needs_accept = before_store()?;
        let receipt = self
            .identity
            .backend
            .claim_offer(&offer_did, &offer.offer_id, &claim)
            .await
            .map_err(backend_error)?;
        Ok(ClaimOfferResult {
            did: offer_did,
            match_code: link::match_code(&offer.scid, &offer.offer_id, signer.key().as_str()),
            offer_id: offer.offer_id.clone(),
            kind: convert(&receipt.kind).map_err(|e| WsRpcError::internal(e.to_string()))?,
            expires: receipt.expires,
            needs_accept,
        })
    }

    /// `identity.joinStatus`: the state of a claim this executor made for its main agent (the
    /// only agent that claims today); on `spent`, takes the log and records that the main
    /// agent speaks for the identity, and on a desktop's first link that the identity is the
    /// operator. A cancelled claim answers `cancelled` and never binds. A held first-run claim
    /// (not yet accepted) answers `awaiting-accept` while the offer is claimed or spent, and
    /// binds nothing. A managed agent has no claims.
    pub async fn join_status(
        &self,
        agent: &AgentContext,
        offer_id: &str,
    ) -> Result<JoinStatus, WsRpcError> {
        let join = self.join_of(agent, offer_id)?;
        let did = did_of(&join.scid);
        if join.accept == JoinAccept::Cancelled {
            return Ok(JoinStatus {
                state: "cancelled".into(),
                did: Some(did),
                key_id: None,
                seq: None,
            });
        }
        let held = Self::held(&join)?;
        let (_, key) = device_key(agent).map_err(WsRpcError::internal)?;
        let progress = self
            .identity
            .backend
            .offer_state(&did, offer_id, &key, 0)
            .await
            .map_err(backend_error)?;
        let mut status = JoinStatus {
            state: progress.state.clone(),
            did: Some(did.clone()),
            key_id: None,
            seq: progress.seq,
        };
        if held && matches!(progress.state.as_str(), "claimed" | "spent") {
            status.state = "awaiting-accept".into();
            return Ok(status);
        }
        if progress.state == "spent" {
            self.identity
                .cache
                .refresh(self.identity.backend.as_ref(), &join.scid)
                .await
                .map_err(cache_error)?;
            // The binding, and on a desktop's first link the operator record, in one write
            // that reads the claim's accept: a claim cancelled meanwhile binds nothing. A
            // first-run claim gets here only once accepted, and a claim alone records nothing,
            // so a claim that never completes (the wrong code scanned, the holder said No, the
            // person here cancelled) leaves the desktop free to claim again.
            let binding = if join.accept == JoinAccept::Accepted {
                Binding::FirstRun(offer_id)
            } else {
                Binding::Claim(offer_id)
            };
            status.key_id = self
                .identity
                .bind_agent_if_listed(agent, &join.scid, binding)
                .await?;
        }
        Ok(status)
    }

    /// The main agent's claim on `offer_id`; `not-found` for any other agent or offer.
    fn join_of(&self, agent: &AgentContext, offer_id: &str) -> Result<JoinRow, WsRpcError> {
        db(|db| db.join(offer_id))?
            .filter(|_| agent.is_main_agent())
            .ok_or_else(|| WsRpcError::not_found("not-found: no claim of this agent"))
    }

    /// Whether `join` waits for `identity.acceptJoin`: a first-run claim not yet accepted. A
    /// row written before the accept column (`not-needed`) is held too while there is no
    /// operator record, as only a first-run claim is made without one.
    pub(super) fn held(join: &JoinRow) -> Result<bool, WsRpcError> {
        Ok(match join.accept {
            JoinAccept::Awaiting => true,
            JoinAccept::NotNeeded => db(|db| db.operator_record())?.is_none(),
            JoinAccept::Accepted | JoinAccept::Cancelled => false,
        })
    }

    /// `identity.acceptJoin`: the person at this computer accepts a held first-run claim, so
    /// the next `join_status` that reads `spent` binds the main agent and records the operator.
    /// Before or after the offer is spent; again once accepted is a no-op. `not-found` for no
    /// claim of the main agent, or one that needs no accept; `cancelled` once cancelled.
    pub fn accept_join(
        &self,
        agent: &AgentContext,
        offer_id: &str,
    ) -> Result<JoinAccepted, WsRpcError> {
        let join = self.join_of(agent, offer_id)?;
        if join.accept == JoinAccept::Accepted {
            return Ok(JoinAccepted { accepted: true });
        }
        if join.accept == JoinAccept::Cancelled {
            return Err(error(
                409,
                "cancelled: the claim of this offer is cancelled",
            ));
        }
        if !Self::held(&join)? {
            return Err(WsRpcError::not_found(
                "not-found: no claim of this offer awaits accept",
            ));
        }
        // A cancel is final: when one lands between the read and this write, nothing changes.
        if !db(|db| db.set_join_accept(offer_id, JoinAccept::Accepted))? {
            return Err(error(
                409,
                "cancelled: the claim of this offer is cancelled",
            ));
        }
        log::info!("identity: the claim of offer {offer_id} is accepted; the main agent joins once the holder confirms");
        Ok(JoinAccepted { accepted: true })
    }

    /// `identity.cancelJoin`: the claim never binds, even if the holder confirms it later
    /// (any claim, first-run or not). Again once cancelled is a no-op. `not-found` for no claim
    /// of the main agent; `already-joined` once the main agent speaks for the claim's identity.
    /// The executor does not tell the store: the holder's add may still list the key in the
    /// log, but this executor never binds to it through this claim.
    pub fn cancel_join(
        &self,
        agent: &AgentContext,
        offer_id: &str,
    ) -> Result<JoinCancelled, WsRpcError> {
        self.join_of(agent, offer_id)?;
        let (agent_did, _) = device_key(agent).map_err(WsRpcError::internal)?;
        match db(|db| db.cancel_join(offer_id, &agent_did))? {
            JoinCancel::Cancelled => {
                log::info!("identity: the claim of offer {offer_id} is cancelled");
                Ok(JoinCancelled { cancelled: true })
            }
            JoinCancel::NoClaim => Err(WsRpcError::not_found("not-found: no claim of this agent")),
            JoinCancel::AlreadyJoined => Err(error(
                409,
                "already-joined: the main agent speaks for this claim's identity",
            )),
        }
    }
}
