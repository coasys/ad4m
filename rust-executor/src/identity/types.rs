//! The `identity.*` RPC contracts and the `identity-fork` event.
//!
//! The agent language speaks `ad4m_ldk::identity`'s wire types; these mirror the ones an RPC
//! method carries, with `ts_rs` for the SDK, and serialise to the same JSON (camelCase,
//! absent fields of a signed offer left out, so its signature still covers its JCS form).
//! [`convert`] crosses between the two through JSON.

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use crate::types::Agent;

/// The same JSON read as another type: between these mirrors and `ad4m_ldk::identity`'s.
pub fn convert<A: Serialize, B: DeserializeOwned>(value: &A) -> Result<B, serde_json::Error> {
    serde_json::from_value(serde_json::to_value(value)?)
}

/// The store's signature over a log's head (`coasys:head`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SignedHead {
    pub scid: String,
    #[ts(type = "number")]
    pub seq: u64,
    pub hash: String,
    #[ts(type = "number")]
    pub time: u64,
    /// The store's `did:key`.
    pub store: String,
    pub sig: String,
}

/// Two different events at one seq of a log, as the cache recorded them. For a store that
/// answers a shorter log, `seq` is the cache's head, `ours` its hash and `theirs` the hash of the
/// head the store signed (a shorter log never truncates the cached one).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ForkRecord {
    #[ts(type = "number")]
    pub seq: u64,
    pub ours: String,
    pub theirs: String,
}

/// `identity.status`: what this agent's device key speaks for.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct IdentityStatus {
    /// The `did:scid` this agent speaks for, or `null` (a device-only identity).
    pub did: Option<String>,
    /// The agent's device key, a multikey (`z6Mk…`): its `did:key` without the prefix.
    pub device_key: String,
    /// The X25519 key `did_key` derives from the device key (`z6LS…`), the `enc` an `incept`
    /// or `add` lists for it.
    pub device_enc: String,
    /// The device key's id in the log (`d1`), once the log lists it.
    pub key_id: Option<String>,
    /// The seq of the event that revoked the device key, if one did.
    #[ts(type = "number | null")]
    pub revoked: Option<u64>,
    /// The last head of the log the cache accepted.
    pub head: Option<SignedHead>,
    /// The first fork the cache recorded for the log.
    pub fork: Option<ForkRecord>,
}

/// `identity.log` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct LogParams {
    /// A `did:scid:coasys:1:…`.
    pub did: String,
    /// The first seq to return (default 0).
    #[ts(type = "number", optional)]
    #[serde(default)]
    pub from: Option<u64>,
    /// Refresh from the agent language before answering.
    #[ts(optional)]
    #[serde(default)]
    pub fresh: Option<bool>,
}

/// `identity.log`: a log as the cache holds it. Every event passed `fold` here.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct IdentityLog {
    pub did: String,
    /// The events from `from` on, in seq order, as JSON.
    pub events: Vec<Value>,
    /// The last head of the log the cache accepted from the agent language.
    pub head: Option<SignedHead>,
    pub fork: Option<ForkRecord>,
}

/// `identity.cosignIncept` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct CosignInceptParams {
    /// An `incept` that lists this agent's device key, with the master's proof.
    pub event: Value,
}

/// `identity.publish` params. No vouching token or proof of work: the store has no brake yet
/// and admits a new log without one.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct PublishParams {
    /// Signed events that continue the log's head (or start a log), as JSON.
    pub events: Vec<Value>,
}

/// What an offer is for: `node` is an executor.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
pub enum OfferKind {
    Device,
    Node,
    Bot,
}

/// An offer. Absent fields are left out: the signature covers the JCS form.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct Offer {
    pub scid: String,
    pub offer_id: String,
    pub kind: OfferKind,
    pub commit: String,
    #[ts(type = "number")]
    pub expires: u64,
    #[ts(optional)]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub device_key: Option<String>,
    #[ts(optional)]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bot: Option<Value>,
}

/// The log position of the key that signed.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SignedBy {
    pub key_id: String,
    #[ts(type = "number")]
    pub kel_seq: u64,
    pub kel_hash: String,
}

/// An offer signed by the identity's current master key (`coasys:offer`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SignedOffer {
    pub offer: Offer,
    pub by: SignedBy,
    pub sig: String,
}

/// A request signed by a key of the identity (`X-Coasys-Request`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct SignedRequest {
    pub req: RequestBody,
    pub sig: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct RequestBody {
    pub did: String,
    pub key_id: String,
    #[ts(type = "number")]
    pub kel_seq: u64,
    pub kel_hash: String,
    pub method: String,
    pub path: String,
    pub body_hash: String,
    pub nonce: String,
    #[ts(type = "number")]
    pub expires: u64,
}

/// `identity.openOffer` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OpenOfferParams {
    pub signed_offer: SignedOffer,
}

/// `identity.openOffer`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OfferOpened {
    pub offer_id: String,
}

/// `identity.offers` params: a list request signed by the current master key.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OffersParams {
    pub request: SignedRequest,
    /// Seconds to wait for a change (0–8).
    #[ts(optional)]
    #[serde(default)]
    pub wait: Option<u8>,
}

/// What the holder sees of a claim; the secret is gone.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ClaimView {
    pub device_key: String,
    pub enc_key: String,
    pub can_keep_master: bool,
    #[serde(default)]
    pub hint: Option<String>,
    pub co_sig: String,
    #[ts(type = "number")]
    pub claimed_at: u64,
}

/// An offer's state.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "state", rename_all = "kebab-case")]
pub enum OfferState {
    Open,
    Claimed {
        claim: ClaimView,
    },
    Spent {
        #[ts(type = "number")]
        seq: u64,
    },
    Expired,
    Cancelled,
    Contested,
    Locked,
    Superseded,
}

/// One open or claimed offer of the identity.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OfferListing {
    pub offer: SignedOffer,
    pub state: OfferState,
}

/// `identity.claimOffer` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ClaimOfferParams {
    /// The offer string (`coasys:offer:1:…`) or the offer link (`https://coasys.id/o#…`).
    pub offer: String,
    /// A name for this device the holder sees on the confirm card (at most 64 characters).
    #[ts(optional)]
    #[serde(default)]
    pub hint: Option<String>,
}

/// `identity.claimOffer`: the claim is made; the holder confirms with the same match code.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct ClaimOfferResult {
    pub did: String,
    pub offer_id: String,
    /// "R7K Q2M": the holder shows the same code.
    pub match_code: String,
    pub kind: OfferKind,
    #[ts(type = "number")]
    pub expires: u64,
}

/// `identity.joinStatus` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct JoinStatusParams {
    pub offer_id: String,
}

/// `identity.joinStatus`: the offer's state for this executor's key. On `spent` the log lists
/// the key and the agent speaks for `did`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct JoinStatus {
    pub state: String,
    pub did: Option<String>,
    pub key_id: Option<String>,
    #[ts(type = "number | null")]
    pub seq: Option<u64>,
}

/// A bot key's scope.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct BotView {
    pub can: Vec<String>,
    #[ts(type = "number")]
    pub expires: u64,
}

/// One device key of the identity, from its log. Names, places and the master-key holders
/// will come from the private record, which the executor does not have yet.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct DeviceView {
    pub key_id: String,
    pub sign: String,
    pub bot: Option<BotView>,
    #[ts(type = "number | null")]
    pub expires: Option<u64>,
    #[ts(type = "number")]
    pub added_seq: u64,
    #[ts(type = "number | null")]
    pub revoked_seq: Option<u64>,
    /// The key is this agent's device key.
    pub this_device: bool,
}

/// `identity-fork`: the cache found two different events at one seq of a log, or a store that
/// answered a shorter log. It kept its history and appended nothing.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct IdentityForkEvent {
    pub did: String,
    #[ts(type = "number")]
    pub seq: u64,
    pub ours: String,
    pub theirs: String,
}

/// `agent.get`: the agent's profile, and the identity its device key speaks for (`did`
/// stays the device `did:key`).
#[derive(Clone, Debug, Serialize, Deserialize, TS)]
pub struct AgentWithIdentity {
    #[serde(flatten)]
    #[ts(flatten)]
    pub agent: Agent,
    /// The `did:scid` this agent speaks for, or `null`.
    pub identity: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// The mirrors read and write the agent language's JSON unchanged.
    #[test]
    fn the_mirrors_round_trip_the_ldk_wire_types() {
        let offer = ad4m_ldk::identity::SignedOffer {
            offer: ad4m_ldk::identity::Offer {
                scid: "QmScid".into(),
                offer_id: "offer".into(),
                kind: ad4m_ldk::identity::OfferKind::Node,
                commit: "QmCommit".into(),
                expires: 1_700_000_300,
                device_key: None,
                bot: None,
            },
            by: ad4m_ldk::identity::SignedBy {
                key_id: "m0".into(),
                kel_seq: 0,
                kel_hash: "QmHash".into(),
            },
            sig: "zSig".into(),
        };
        let mirror: SignedOffer = convert(&offer).unwrap();
        assert_eq!(
            serde_json::to_value(&mirror).unwrap(),
            serde_json::to_value(&offer).unwrap()
        );
        // Absent optional fields stay absent: the offer's signature covers its JCS form.
        assert!(serde_json::to_value(&mirror).unwrap()["offer"]
            .get("deviceKey")
            .is_none());
        let back: ad4m_ldk::identity::SignedOffer = convert(&mirror).unwrap();
        assert_eq!(back, offer);

        let state: OfferState = serde_json::from_value(json!({
            "state": "claimed",
            "claim": {"deviceKey": "z6Mk", "encKey": "z6LS", "canKeepMaster": false,
                      "hint": null, "coSig": "z", "claimedAt": 3}
        }))
        .unwrap();
        let ldk: ad4m_ldk::identity::OfferState = convert(&state).unwrap();
        assert!(matches!(
            ldk,
            ad4m_ldk::identity::OfferState::Claimed { .. }
        ));
    }
}
