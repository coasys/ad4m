//! The store client through the executor (`ViaExecutor`): every log read, append
//! and offer goes through the executor's `identity.*` calls, so the executor checks what it
//! passes and keeps its cache current. The transport is [`ExecutorRpc`]: the desktop custody
//! service calls its executor over the shell's loopback channel with the custody credential
//! (which the custody service writes as the first line of its executor's stdin at start; the
//! admin credential never opens the custody channel);
//! `ad4m-client`'s WebSocket client serves it natively (feature `ws`).

use ad4m_identity_core::inbox::InboxAddress;
use ad4m_identity_core::record::{SealedRecord, RECORD_KEY_LEN};
use ad4m_identity_core::Event;
use ad4m_ldk::identity::{
    DeviceRequest, DeviceRequestBody, InboxBind, InboxStored, OfferListing, OfferOpened,
    SignedHead, SignedOffer, SignedRequest,
};
use zeroize::Zeroize;

use crate::secret::Secret;
use async_trait::async_trait;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

/// An RPC refusal or failure: the executor's status code and message.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("{code}: {message}")]
pub struct RpcError {
    pub code: u16,
    pub message: String,
}

/// One executor RPC call: `method` with `params`, answering the result JSON.
#[async_trait(?Send)]
pub trait ExecutorRpc {
    async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError>;
}

/// `identity.status`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IdentityStatus {
    pub did: Option<String>,
    pub device_key: String,
    pub device_enc: String,
    pub key_id: Option<String>,
    pub revoked: Option<u64>,
    pub head: Option<SignedHead>,
    pub fork: Option<ForkRecord>,
    /// Set while the executor's configured store answers the log behind its cache.
    #[serde(default)]
    pub store_behind: Option<StoreBehind>,
    /// The identity store the executor uses (its config's, else its network seed's) and whether
    /// it trusts a key for it; `None` without a store.
    pub store: Option<StoreInfo>,
}

/// The executor's identity store (`identity.status.store`): its base URL, and whether the
/// executor trusts a key for it. Untrusted, the executor accepts no head from it, so no
/// `did:scid` record verifies and `identity.publish` / `identity.claimOffer` refuse
/// `no-store-keys`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StoreInfo {
    pub url: String,
    pub trusted: bool,
}

/// The executor's configured store answering a log behind its cache, since `since` (unix
/// seconds); `seq` is the store's head at its last answer, `None` when it answered no log.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StoreBehind {
    pub since: u64,
    pub seq: Option<u64>,
}

/// A fork the executor's cache recorded.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForkRecord {
    pub seq: u64,
    pub ours: String,
    pub theirs: String,
}

/// `identity.keyOwner`: the identity a key belongs to, by the store's key index.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct KeyOwner {
    pub did: String,
    /// `m0` for master #0, `d3` for a device key.
    pub key_id: String,
    pub added: u64,
    pub revoked: Option<u64>,
}

/// `identity.claimOffer`: the claim is made; the holder shows the same match code.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct OfferClaimed {
    pub did: String,
    pub offer_id: String,
    pub match_code: String,
    pub expires: u64,
}

/// `identity.log`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IdentityLog {
    pub did: String,
    pub events: Vec<Value>,
    pub head: Option<SignedHead>,
    pub fork: Option<ForkRecord>,
}

/// `identity.takeMaster`: a master key sealed to this device, opened by its executor for the
/// custody service. The secret is wiped when this drops.
pub struct TakenMaster {
    /// Acknowledge it (`identity.inboxAck`) once the vault keeps the key.
    pub message_id: String,
    pub did: String,
    pub index: u32,
    pub secret: Secret<[u8; 32]>,
}

impl std::fmt::Debug for TakenMaster {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TakenMaster")
            .field("message_id", &self.message_id)
            .field("did", &self.did)
            .field("index", &self.index)
            .finish_non_exhaustive()
    }
}

/// 32 bytes of base58btc in `value[field]`, taken out of the JSON and wiped there.
fn take_secret(value: &mut Value, method: &str, field: &str) -> Result<Secret<[u8; 32]>, RpcError> {
    let bad = || RpcError {
        code: 502,
        message: format!("{method} answered no 32-byte {field}"),
    };
    let Value::String(mut text) = value[field].take() else {
        return Err(bad());
    };
    let decoded = bs58::decode(&text).into_vec();
    text.zeroize();
    let mut bytes = decoded.map_err(|_| bad())?;
    let secret = <[u8; 32]>::try_from(bytes.as_slice()).map(Secret::new);
    bytes.zeroize();
    secret.map_err(|_| bad())
}

/// An inbox address as the executor's calls name it: `{pre}` or `{scid, keyId}`.
fn address_json(address: &InboxAddress) -> Value {
    match address {
        InboxAddress::Pre(pre) => json!({ "pre": pre }),
        InboxAddress::Joined { scid, key_id } => json!({ "scid": scid, "keyId": key_id }),
    }
}

/// The executor's identity calls, typed.
pub struct ViaExecutor<R: ExecutorRpc> {
    rpc: R,
}

fn read<T: DeserializeOwned>(method: &str, value: Value) -> Result<T, RpcError> {
    serde_json::from_value(value).map_err(|e| RpcError {
        code: 502,
        message: format!("{method} answered {e}"),
    })
}

fn json_of<T: Serialize>(value: &T) -> Result<Value, RpcError> {
    serde_json::to_value(value).map_err(|e| RpcError {
        code: 400,
        message: e.to_string(),
    })
}

impl<R: ExecutorRpc> ViaExecutor<R> {
    pub fn new(rpc: R) -> Self {
        ViaExecutor { rpc }
    }

    pub fn rpc(&self) -> &R {
        &self.rpc
    }

    async fn call<T: DeserializeOwned>(&self, method: &str, params: Value) -> Result<T, RpcError> {
        read(method, self.rpc.call(method, params).await?)
    }

    pub async fn status(&self) -> Result<IdentityStatus, RpcError> {
        self.call("identity.status", json!({})).await
    }

    /// The log of `did` through the executor's cache; `fresh` refreshes it first.
    pub async fn log(&self, did: &str, from: u64, fresh: bool) -> Result<IdentityLog, RpcError> {
        self.call(
            "identity.log",
            json!({ "did": did, "from": from, "fresh": fresh }),
        )
        .await
    }

    /// The events of a log, parsed with the identity core.
    pub async fn events(&self, did: &str) -> Result<Vec<Event>, RpcError> {
        let log = self.log(did, 0, true).await?;
        log.events
            .iter()
            .map(Event::from_json)
            .collect::<Result<_, _>>()
            .map_err(|e| RpcError {
                code: 502,
                message: format!("identity.log served an event that does not parse: {e}"),
            })
    }

    /// Adds the executor's device-key proof to `incept`.
    pub async fn cosign_incept(&self, incept: &Event) -> Result<Event, RpcError> {
        let value: Value = self
            .call(
                "identity.cosignIncept",
                json!({ "event": json_of(incept)? }),
            )
            .await?;
        Event::from_json(&value).map_err(|e| RpcError {
            code: 502,
            message: format!("identity.cosignIncept answered an event that does not parse: {e}"),
        })
    }

    /// Appends signed events through the executor (no vouching token or proof of work:
    /// the store has no brake yet and admits a new log without one).
    pub async fn publish(&self, events: &[Event]) -> Result<SignedHead, RpcError> {
        self.call("identity.publish", json!({ "events": json_of(&events)? }))
            .await
    }

    pub async fn open_offer(&self, offer: &SignedOffer) -> Result<OfferOpened, RpcError> {
        self.call(
            "identity.openOffer",
            json!({ "signedOffer": json_of(offer)? }),
        )
        .await
    }

    pub async fn offers(
        &self,
        request: &SignedRequest,
        wait: u8,
    ) -> Result<Vec<OfferListing>, RpcError> {
        self.call(
            "identity.offers",
            json!({ "request": json_of(request)?, "wait": wait }),
        )
        .await
    }

    /// Puts a sealed message into an inbox with the master-signed `request` of the write
    /// (`bind` names the bound offer of a pre-join address).
    pub async fn inbox_put(
        &self,
        address: &InboxAddress,
        sealed: &str,
        bind: Option<&InboxBind>,
        request: &SignedRequest,
    ) -> Result<InboxStored, RpcError> {
        let mut params = json!({
            "address": address_json(address),
            "sealed": sealed,
            "request": json_of(request)?,
        });
        if let Some(bind) = bind {
            params["bind"] = json_of(bind)?;
        }
        self.call("identity.inboxPut", params).await
    }

    /// The custody channel's call: the oldest current master key sealed to this device, opened
    /// by the executor; `None` when none waits.
    pub async fn take_master(&self) -> Result<Option<TakenMaster>, RpcError> {
        const METHOD: &str = "identity.takeMaster";
        let mut value = self.rpc.call(METHOD, json!({})).await?;
        if value.is_null() {
            return Ok(None);
        }
        let secret = take_secret(&mut value, METHOD, "secret")?;
        let field = |name: &str| {
            value[name].as_str().map(str::to_string).ok_or(RpcError {
                code: 502,
                message: format!("{METHOD} answered no {name}"),
            })
        };
        Ok(Some(TakenMaster {
            message_id: field("messageId")?,
            did: field("did")?,
            index: value["index"]
                .as_u64()
                .and_then(|i| u32::try_from(i).ok())
                .ok_or(RpcError {
                    code: 502,
                    message: format!("{METHOD} answered no index"),
                })?,
            secret,
        }))
    }

    /// Acknowledges message `id` of this device's inbox: the store deletes it.
    pub async fn inbox_ack(&self, id: &str) -> Result<(), RpcError> {
        let _: Value = self.call("identity.inboxAck", json!({ "id": id })).await?;
        Ok(())
    }

    /// The sealed private record of the executor's identity, read with its device key; `None`
    /// before the first write.
    pub async fn record(&self) -> Result<Option<SealedRecord>, RpcError> {
        self.call("identity.record", json!({})).await
    }

    /// Writes the next version of the record with the executor's device key; answers the
    /// version stored.
    pub async fn record_put(&self, record: &SealedRecord) -> Result<u64, RpcError> {
        let stored: Value = self
            .call("identity.recordPut", json!({ "record": json_of(record)? }))
            .await?;
        stored["version"].as_u64().ok_or(RpcError {
            code: 502,
            message: "identity.recordPut answered no version".into(),
        })
    }

    /// The sealed private record of an identity the executor may not speak for, read with the
    /// signed `request` of a `GET /v1/private/{scid}` (the current master key's, on a restore);
    /// `None` before the first write.
    pub async fn record_with(
        &self,
        request: &SignedRequest,
    ) -> Result<Option<SealedRecord>, RpcError> {
        self.call("identity.record", json!({ "request": json_of(request)? }))
            .await
    }

    /// Writes the next version of a record with the signed `request` of its
    /// `PUT /v1/private/{scid}`; answers the version stored.
    pub async fn record_put_with(
        &self,
        record: &SealedRecord,
        request: &SignedRequest,
    ) -> Result<u64, RpcError> {
        let stored: Value = self
            .call(
                "identity.recordPut",
                json!({ "record": json_of(record)?, "request": json_of(request)? }),
            )
            .await?;
        stored["version"].as_u64().ok_or(RpcError {
            code: 502,
            message: "identity.recordPut answered no version".into(),
        })
    }

    /// The identity the signing multikey `key` belongs to, by the store's key index (master
    /// keys included); `None` when it is in no log.
    pub async fn key_owner(&self, key: &str) -> Result<Option<KeyOwner>, RpcError> {
        self.call("identity.keyOwner", json!({ "key": key })).await
    }

    /// Claims the offer `offer` (its string or link) with this executor's device key, the
    /// operator's call. `can_keep_master` needs an executor with the custody channel.
    pub async fn claim_offer(
        &self,
        offer: &str,
        hint: Option<&str>,
        can_keep_master: bool,
    ) -> Result<OfferClaimed, RpcError> {
        let mut params = json!({ "offer": offer, "canKeepMaster": can_keep_master });
        if let Some(hint) = hint {
            params["hint"] = json!(hint);
        }
        self.call("identity.claimOffer", params).await
    }

    /// The record key that `wrapped` (the executor's entry of a record's `wrapped`) seals to
    /// the executor's device key, opened in its wallet.
    pub async fn unwrap_record_key(
        &self,
        wrapped: &str,
    ) -> Result<Secret<[u8; RECORD_KEY_LEN]>, RpcError> {
        const METHOD: &str = "identity.unwrapRecordKey";
        let mut value = self.rpc.call(METHOD, json!({ "wrapped": wrapped })).await?;
        take_secret(&mut value, METHOD, "key")
    }

    /// The request box since `since` (unix seconds), read with the executor's device key.
    pub async fn requests(&self, since: u64) -> Result<Vec<DeviceRequest>, RpcError> {
        self.call("identity.requests", json!({ "since": since }))
            .await
    }

    /// Posts `body` to the request box with the executor's device key; it lives until
    /// `expires` (default: the longest, 7 days). Answers the request's id.
    pub async fn post_request(
        &self,
        body: &DeviceRequestBody,
        expires: Option<u64>,
    ) -> Result<String, RpcError> {
        let mut params = json!({ "body": json_of(body)? });
        if let Some(expires) = expires {
            params["expires"] = json!(expires);
        }
        let posted: Value = self.call("identity.postRequest", params).await?;
        posted["id"].as_str().map(str::to_string).ok_or(RpcError {
            code: 502,
            message: "identity.postRequest answered no id".into(),
        })
    }

    /// Deletes request `id` with the master-signed `request`.
    pub async fn delete_request(&self, id: &str, request: &SignedRequest) -> Result<(), RpcError> {
        let _: Value = self
            .call(
                "identity.deleteRequest",
                json!({ "id": id, "request": json_of(request)? }),
            )
            .await?;
        Ok(())
    }
}

#[cfg(feature = "ws")]
mod ws {
    use super::*;

    /// "RPC error 403: message" (the WebSocket client's error text) as its code and message;
    /// any other failure (a closed socket) is 503.
    pub(crate) fn rpc_error(text: String) -> RpcError {
        let parsed = text.strip_prefix("RPC error ").and_then(|rest| {
            let (code, message) = rest.split_once(": ")?;
            Some(RpcError {
                code: code.parse().ok()?,
                message: message.to_string(),
            })
        });
        parsed.unwrap_or(RpcError {
            code: 503,
            message: text,
        })
    }

    #[async_trait(?Send)]
    impl ExecutorRpc for ad4m_client::ws_rpc::WsRpcClient {
        async fn call(&self, method: &str, params: Value) -> Result<Value, RpcError> {
            ad4m_client::ws_rpc::WsRpcClient::call::<Value>(self, method, params)
                .await
                .map_err(|e| rpc_error(e.to_string()))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn the_ws_clients_error_text_gives_the_code() {
            let e = rpc_error("RPC error 403: first-link: no".into());
            assert_eq!((e.code, e.message.as_str()), (403, "first-link: no"));
            assert_eq!(rpc_error("closed".into()).code, 503);
        }
    }
}
