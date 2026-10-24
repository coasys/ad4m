//! `ClaimService` (AR-02 4.15): the executor's side of a link. It answers an offer with one of
//! its wallet keys and that key's `enc` key, co-signs the claim, and follows the offer until the
//! holder's `add` lists the key.
//!
//! `claim_for_main` claims with the main wallet key and first runs AR-04's first-link rule
//! (`operator::first_link_check`): every path that claims for the main agent calls it, the
//! `identity.claimOffer` handler today and the pre-join ticket path of a later slice, so the
//! rule never depends on an RPC guard. `claim_for_tenant` (AR-05) comes with hosts.

use ad4m_identity_core::build::Signer;
use ad4m_identity_core::did_of;
use ad4m_identity_core::link;

use super::device::{device_key, enc_key_of, WalletSigner};
use super::operator::first_link_check;
use super::service::{
    backend_error, cache_error, db, error, first_link_error, now, parse_offer, store_base,
    IdentityService,
};
use super::types::{convert, ClaimOfferParams, ClaimOfferResult, JoinStatus};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;
use crate::db::JoinRow;

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
    /// Whether the main agent's key is valid in the log of the identity it speaks for.
    async fn main_is_linked(&self) -> Result<bool, WsRpcError> {
        let main = AgentContext::main_agent();
        let (agent_did, key) = device_key(&main).map_err(WsRpcError::internal)?;
        let Some(scid) = db(|db| db.agent_identity(&agent_did))? else {
            return Ok(false);
        };
        Ok(match self.identity.refreshed_or_cached(&scid).await? {
            Some(chain) => chain
                .state()
                .key_valid_at_time(&key, chain.head().seq, now()),
            None => false,
        })
    }

    /// Claims `params.offer` with the main wallet key. Refuses `no-operator` with no operator
    /// record and `first-link` while the main key is in no log and the offer is not of the
    /// recorded operator (AR-04 4.9.3); `already-linked` while the main key speaks for an
    /// identity (a revoked key needs a new main key, AR-02's `identity.rekey`).
    pub async fn claim_for_main(
        &self,
        params: &ClaimOfferParams,
    ) -> Result<ClaimOfferResult, WsRpcError> {
        let offer = parse_offer(&params.offer)?;
        let offer_did = did_of(&offer.scid);
        let record = db(|db| db.operator_record())?;
        let linked = self.main_is_linked().await?;
        if linked {
            return Err(error(
                409,
                "already-linked: this executor's key speaks for an identity already",
            ));
        }
        first_link_check(record.as_ref(), linked, &offer_did).map_err(first_link_error)?;
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
        let main = AgentContext::main_agent();
        let (_, key) = device_key(&main).map_err(WsRpcError::internal)?;
        let enc = enc_key_of(&key).map_err(WsRpcError::internal)?;
        let signer = WalletSigner::new(&main).map_err(WsRpcError::internal)?;
        let claim = link::make_claim(
            &offer.scid,
            &offer.offer_id,
            secret,
            &signer,
            enc,
            params.can_keep_master.unwrap_or(false),
            params.hint.clone(),
        )
        .map_err(|e| WsRpcError::bad_request(format!("invalid: {e}")))?;
        signer.check().map_err(WsRpcError::internal)?;
        let claim: ad4m_ldk::identity::Claim =
            convert(&claim).map_err(|e| WsRpcError::internal(e.to_string()))?;
        let receipt = self
            .identity
            .backend
            .claim_offer(&offer_did, &offer.offer_id, &claim)
            .await
            .map_err(backend_error)?;
        db(|db| {
            db.add_join(&JoinRow {
                offer_id: offer.offer_id.clone(),
                scid: offer.scid.clone(),
                agent_did: None,
                claimed_at: now(),
            })
        })?;
        Ok(ClaimOfferResult {
            did: offer_did,
            match_code: link::match_code(&offer.scid, &offer.offer_id, signer.key().as_str()),
            offer_id: offer.offer_id,
            kind: convert(&receipt.kind).map_err(|e| WsRpcError::internal(e.to_string()))?,
            expires: receipt.expires,
        })
    }

    /// `identity.joinStatus`: the state of a claim this executor made for `agent`; on `spent`,
    /// takes the log and records that the agent speaks for the identity.
    pub async fn join_status(
        &self,
        agent: &AgentContext,
        offer_id: &str,
    ) -> Result<JoinStatus, WsRpcError> {
        let join = db(|db| db.join(offer_id))?
            .filter(|j| j.agent_did.as_deref() == agent.managed_did())
            .ok_or_else(|| WsRpcError::not_found("not-found: no claim of this agent"))?;
        let did = did_of(&join.scid);
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
        if progress.state == "spent" {
            self.identity
                .cache
                .refresh(self.identity.backend.as_ref(), &join.scid)
                .await
                .map_err(cache_error)?;
            status.key_id = self
                .identity
                .record_agent_if_listed(agent, &join.scid)
                .await?;
        }
        Ok(status)
    }
}
