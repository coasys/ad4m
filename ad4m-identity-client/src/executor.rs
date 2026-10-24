//! The store client through the executor (AR-03 4.3.3, `ViaExecutor`): every log read, append
//! and offer goes through the executor's `identity.*` calls, so the executor checks what it
//! passes and keeps its cache current. The transport is [`ExecutorRpc`]: the desktop custody
//! service calls its executor over the shell's loopback channel with the admin credential;
//! `ad4m-client`'s WebSocket client serves it natively (feature `ws`).

use ad4m_identity_core::Event;
use ad4m_ldk::identity::{
    OfferListing, OfferOpened, Pow, PowParams, SignedHead, SignedOffer, SignedRequest,
};
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
    pub vouched: bool,
    pub head: Option<SignedHead>,
    pub fork: Option<ForkRecord>,
}

/// A fork the executor's cache recorded.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForkRecord {
    pub seq: u64,
    pub ours: String,
    pub theirs: String,
}

/// `identity.log`.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IdentityLog {
    pub did: String,
    pub events: Vec<Value>,
    pub head: Option<SignedHead>,
    pub vouched: bool,
    pub fork: Option<ForkRecord>,
}

/// `identity.claimOffer`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClaimOfferResult {
    pub did: String,
    pub offer_id: String,
    pub match_code: String,
    pub kind: String,
    pub expires: u64,
}

/// `identity.joinStatus`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JoinStatus {
    pub state: String,
    pub did: Option<String>,
    pub key_id: Option<String>,
    pub seq: Option<u64>,
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

    pub async fn pow_params(&self) -> Result<PowParams, RpcError> {
        self.call("identity.powParams", json!({})).await
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

    pub async fn publish(
        &self,
        events: &[Event],
        vouch: Option<&str>,
        pow: Option<&Pow>,
    ) -> Result<SignedHead, RpcError> {
        let mut params = json!({ "events": json_of(&events)? });
        if let Some(vouch) = vouch {
            params["vouch"] = json!(vouch);
        }
        if let Some(pow) = pow {
            params["pow"] = json_of(pow)?;
        }
        self.call("identity.publish", params).await
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

    /// Claims an offer for the executor's main agent (the operator's call).
    pub async fn claim_offer(
        &self,
        offer: &str,
        hint: Option<&str>,
    ) -> Result<ClaimOfferResult, RpcError> {
        let mut params = json!({ "offer": offer });
        if let Some(hint) = hint {
            params["hint"] = json!(hint);
        }
        self.call("identity.claimOffer", params).await
    }

    pub async fn join_status(&self, offer_id: &str) -> Result<JoinStatus, RpcError> {
        self.call("identity.joinStatus", json!({ "offerId": offer_id }))
            .await
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
