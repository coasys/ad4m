//! `link` (AR-02 4.2, steps 1, 2, 4 and 5 on the holder's side): add a device or an executor to
//! the identity.
//!
//! 1. [`LinkFlow::open`]: one unlock of the vault for the whole flow; an offer signed by the
//!    current master key at the log's head (`identity.openOffer`); the offer link carries the
//!    secret `s` in its fragment only.
//! 2. The surface hands the link over (QR code, paste, `ad4m-executor claim <link>`).
//! 3. [`LinkFlow::wait_for_claim`]: master-signed list requests (`identity.offers`) until the
//!    offer is claimed; the claim's device key gives the match code both sides show.
//! 4. [`LinkFlow::confirm`] after the person sees the same code: the `add` that spends the
//!    claim, signed by the current master key (`identity.publish`).

use ad4m_hash_chain::{encode_proof_value, hash_bytes};
use ad4m_identity_core::build::add_claimed;
use ad4m_identity_core::link::{self, ClaimView, Offer, OfferKind, OfferString, SignedBy};
use ad4m_identity_core::{signing_bytes, KelChain};
use ad4m_ldk::identity::{OfferState, RequestBody, SignedHead, SignedRequest};
use serde_json::json;

use crate::executor::{ExecutorRpc, ViaExecutor};
use crate::secret::Secret;
use crate::vault::{MasterUse, Unlocked, Vault};
use crate::{fold, ClientError, Clock, Random};

/// The signing domain of requests to the store.
pub const REQUEST_DOMAIN: &str = "coasys:store-request";

/// How long a signed list request stays valid (the store takes at most 300 s).
const REQUEST_TTL_S: u64 = 60;

/// The seconds each list request waits for a change (the store's longest wait).
const LIST_WAIT_S: u8 = 8;

/// A claim that arrived: what the confirm card shows.
#[derive(Clone, Debug, PartialEq)]
pub struct Claimed {
    /// "R7K Q2M": the joining device shows the same code.
    pub match_code: String,
    pub device_key: String,
    pub hint: Option<String>,
    claim: ClaimView,
}

/// The holder's side of one link.
pub struct LinkFlow<'a, R: ExecutorRpc> {
    executor: &'a ViaExecutor<R>,
    clock: &'a dyn Clock,
    random: &'a dyn Random,
    unlocked: Unlocked,
    did: String,
    offer: Offer,
    offer_string: OfferString,
    _secret: Secret<[u8; 32]>,
}

fn b58_random(random: &dyn Random, len: usize) -> String {
    let mut bytes = vec![0u8; len];
    random.fill(&mut bytes);
    bs58::encode(bytes).into_string()
}

/// The identity's log through the executor, refreshed and folded here.
async fn chain_of<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    did: &str,
) -> Result<KelChain, ClientError> {
    fold(&executor.events(did).await?)
}

impl<'a, R: ExecutorRpc> LinkFlow<'a, R> {
    /// Opens an offer of `kind` (`Node` for an executor) for the identity `did`, whose store
    /// is at `store_url`, and makes its link.
    pub async fn open(
        executor: &'a ViaExecutor<R>,
        vault: &dyn Vault,
        did: &str,
        kind: OfferKind,
        store_url: &str,
        clock: &'a dyn Clock,
        random: &'a dyn Random,
    ) -> Result<LinkFlow<'a, R>, ClientError> {
        let scid = ad4m_identity_core::scid_of(did)
            .ok_or_else(|| ClientError::Protocol(format!("{did} is not a did:scid:coasys:1")))?
            .to_string();
        let chain = chain_of(executor, did).await?;
        let unlocked = vault.unlock(&scid, MasterUse::Link, clock.now())?;
        let mut s = [0u8; link::SECRET_LEN];
        random.fill(&mut s);
        let secret = Secret::new(s);
        s = [0; link::SECRET_LEN];
        let _ = s;
        let offer = Offer {
            scid: scid.clone(),
            offer_id: b58_random(random, link::OFFER_ID_LEN),
            kind,
            commit: link::offer_commit(secret.expose()),
            expires: clock.now() + link::OFFER_TTL_S,
            device_key: None,
            bot: None,
        };
        let signer = unlocked.signer(clock.now())?;
        let head = chain.head();
        let master = chain
            .state()
            .current_master()
            .ok_or_else(|| ClientError::Protocol("the identity has ended".into()))?;
        if master.sign != signer.key() {
            return Err(ClientError::Protocol(
                "the vault's master key is not the identity's current master".into(),
            ));
        }
        let by = SignedBy {
            key_id: master.id.clone(),
            kel_seq: head.seq,
            kel_hash: head.hash.clone(),
        };
        let signed = link::sign_offer(offer.clone(), by, signer)?;
        let wire: ad4m_ldk::identity::SignedOffer = serde_json::from_value(json!(signed))
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
            clock,
            random,
            unlocked,
            did: did.to_string(),
            offer,
            offer_string,
            _secret: secret,
        })
    }

    /// The offer link (`https://coasys.id/o#coasys:offer:1:…`): the QR code shows it.
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

    /// A list request signed by the current master key at `chain`'s head.
    fn list_request(&self, chain: &KelChain) -> Result<SignedRequest, ClientError> {
        let now = self.clock.now();
        let signer = self.unlocked.signer(now)?;
        let master = chain
            .state()
            .current_master()
            .ok_or_else(|| ClientError::Protocol("the identity has ended".into()))?;
        let head = chain.head();
        let req = RequestBody {
            did: self.did.clone(),
            key_id: master.id.clone(),
            kel_seq: head.seq,
            kel_hash: head.hash.clone(),
            method: "GET".into(),
            path: format!("/v1/offers/{}", self.offer.scid),
            body_hash: hash_bytes(b""),
            nonce: b58_random(self.random, 16),
            expires: now + REQUEST_TTL_S,
        };
        let bytes = signing_bytes(REQUEST_DOMAIN, &req)
            .map_err(|e| ClientError::Protocol(e.to_string()))?;
        let sig = encode_proof_value(&signer.sign(&bytes));
        Ok(SignedRequest { req, sig })
    }

    /// Waits until the offer is claimed; fails when it ends unclaimed (expired, cancelled,
    /// superseded) or expires.
    pub async fn wait_for_claim(&self) -> Result<Claimed, ClientError> {
        let chain = chain_of(self.executor, &self.did).await?;
        // The store's wait answers at a change or at its timeout, so the first look takes the
        // state as it is and later ones wait for the claim.
        let mut wait = 0;
        loop {
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
                    return Ok(Claimed {
                        match_code: link::match_code(
                            &self.offer.scid,
                            &self.offer.offer_id,
                            &view.device_key,
                        ),
                        device_key: view.device_key.clone(),
                        hint: view.hint.clone(),
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
            if self.clock.now() >= self.offer.expires {
                return Err(ClientError::OfferEnded("expired".into()));
            }
        }
    }

    /// The person confirmed the same match code: appends the `add` that spends `claimed`.
    pub async fn confirm(self, claimed: &Claimed) -> Result<SignedHead, ClientError> {
        let chain = chain_of(self.executor, &self.did).await?;
        let signer = self.unlocked.signer(self.clock.now())?;
        let event = add_claimed(&chain, signer, &self.offer, &claimed.claim, None)?;
        Ok(self
            .executor
            .publish(std::slice::from_ref(&event), None, None)
            .await?)
    }
}
