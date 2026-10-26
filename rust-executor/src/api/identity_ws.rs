//! The `identity.*` WS RPC calls with their guards (the table in
//! `tests::every_identity_call_has_its_guard`). "The agent's own session" is `Cap(…)` with the
//! handler acting for `RequestContext::acts_as()`: the main agent for the operator and the main
//! agent's apps, the managed agent for a user. The work is in `identity::service`.

use std::sync::Arc;

use serde_json::Value;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::agent::capabilities::{
    AGENT_READ_CAPABILITY, AGENT_SIGN_CAPABILITY, AGENT_UPDATE_CAPABILITY,
};
use crate::identity::types::{
    ClaimOfferParams, ClaimOfferResult, CosignInceptParams, DeviceView, DidResolutionResult,
    IdentityLog, IdentityStatus, JoinStatus, JoinStatusParams, LogParams, OfferListing,
    OfferOpened, OffersParams, OpenOfferParams, PublishParams, ResetLogParams, ResolveParams,
    SignedHead,
};
use crate::types::RequestContext;

fn parse<T: serde::de::DeserializeOwned>(params: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

fn answer<T: serde::Serialize>(value: T) -> Result<Value, WsRpcError> {
    Ok(serde_json::to_value(value)?)
}

async fn status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(crate::identity::service()?.status(&ctx.acts_as()).await?)
}

async fn log(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: LogParams = parse(params)?;
    let log = crate::identity::service()?
        .log(&p.did, p.from.unwrap_or(0), p.fresh.unwrap_or(false))
        .await?;
    answer(log)
}

async fn reset_log(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: ResetLogParams = parse(params)?;
    answer(crate::identity::service()?.reset_log(&p.did).await?)
}

/// A DID resolution result in every case: a refusal is `didResolutionMetadata.error`, not an
/// RPC error.
async fn resolve(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: ResolveParams = parse(params)?;
    let result = crate::identity::service()?
        .resolver
        .resolve_as(&p.did_url, p.fresh.unwrap_or(false), p.accept.as_deref())
        .await;
    answer(result)
}

async fn cosign_incept(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: CosignInceptParams = parse(params)?;
    answer(
        crate::identity::service()?
            .cosign_incept(&ctx.acts_as(), &p.event)
            .await?,
    )
}

async fn publish(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: PublishParams = parse(params)?;
    answer(
        crate::identity::service()?
            .publish(&ctx.acts_as(), &p)
            .await?,
    )
}

async fn open_offer(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: OpenOfferParams = parse(params)?;
    answer(
        crate::identity::service()?
            .open_offer(&p.signed_offer)
            .await?,
    )
}

async fn offers(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: OffersParams = parse(params)?;
    answer(
        crate::identity::service()?
            .offers(&p.request, p.wait.unwrap_or(0))
            .await?,
    )
}

async fn claim_offer(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: ClaimOfferParams = parse(params)?;
    answer(
        crate::identity::service()?
            .claims()
            .claim_for_main(&p)
            .await?,
    )
}

async fn join_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: JoinStatusParams = parse(params)?;
    answer(
        crate::identity::service()?
            .claims()
            .join_status(&ctx.acts_as(), &p.offer_id)
            .await?,
    )
}

async fn devices(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(crate::identity::service()?.devices(&ctx.acts_as()).await?)
}

/// The `identity.*` methods and their guards.
pub fn register_ws_handlers(map: &mut HandlerMap) {
    // This agent's identity: a key log is public, the agent's own status is its session's.
    map.method::<NoParams, IdentityStatus>("identity.status", status)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    // Any identity's log: a key log is public, so the handler serves any identity.
    map.method::<LogParams, IdentityLog>("identity.log", log)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    // Any DID's document: public data derived from public logs.
    map.method::<ResolveParams, DidResolutionResult>("identity.resolve", resolve)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    // Drops this executor's copy of a log (the way out of a fork): the operator only.
    map.method::<ResetLogParams, bool>("identity.resetLog", reset_log)
        .guard(Guard::Operator);
    // Signs with the agent's own device key, and publishes what the custody service signed.
    // Both can bind the agent to an identity for good, so for the main agent only the
    // operator (the custody service holds the admin credential): every app holds these
    // capabilities by default. A user acts for their own agent.
    map.method::<CosignInceptParams, Value>("identity.cosignIncept", cosign_incept)
        .guard(Guard::UserOrOperator(AGENT_SIGN_CAPABILITY.clone()));
    map.method::<PublishParams, SignedHead>("identity.publish", publish)
        .guard(Guard::UserOrOperator(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<OpenOfferParams, OfferOpened>("identity.openOffer", open_offer)
        .guard(Guard::Cap(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<OffersParams, Vec<OfferListing>>("identity.offers", offers)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    // Claims for the main agent with its wallet key: the operator only. The first-link rule
    // runs inside `claim_for_main`, so it never depends on this guard.
    map.method::<ClaimOfferParams, ClaimOfferResult>("identity.claimOffer", claim_offer)
        .guard(Guard::Operator);
    map.method::<JoinStatusParams, JoinStatus>("identity.joinStatus", join_status)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<NoParams, Vec<DeviceView>>("identity.devices", devices)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::Capability;

    /// Each call carries its guard: reads the agent read capability; co-signing and publishing
    /// the agent sign and update capabilities, and for the main agent the operator; other writes
    /// the agent update capability; a claim for the main agent and a log reset the operator.
    #[test]
    fn every_identity_call_has_its_guard() {
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let cap = |c: &Capability| format!("{:?}", Guard::Cap(c.clone()));
        let expected = [
            ("identity.claimOffer", format!("{:?}", Guard::Operator)),
            (
                "identity.cosignIncept",
                format!("{:?}", Guard::UserOrOperator(AGENT_SIGN_CAPABILITY.clone())),
            ),
            ("identity.devices", cap(&AGENT_READ_CAPABILITY)),
            ("identity.joinStatus", cap(&AGENT_READ_CAPABILITY)),
            ("identity.log", cap(&AGENT_READ_CAPABILITY)),
            ("identity.offers", cap(&AGENT_READ_CAPABILITY)),
            ("identity.openOffer", cap(&AGENT_UPDATE_CAPABILITY)),
            (
                "identity.publish",
                format!(
                    "{:?}",
                    Guard::UserOrOperator(AGENT_UPDATE_CAPABILITY.clone())
                ),
            ),
            ("identity.resetLog", format!("{:?}", Guard::Operator)),
            ("identity.resolve", cap(&AGENT_READ_CAPABILITY)),
            ("identity.status", cap(&AGENT_READ_CAPABILITY)),
        ];
        let got: Vec<(String, String)> = map
            .guards()
            .into_iter()
            .map(|(name, guard)| (name.to_string(), format!("{:?}", guard.expect("a guard"))))
            .collect();
        let want: Vec<(String, String)> = expected
            .iter()
            .map(|(name, guard)| (name.to_string(), guard.clone()))
            .collect();
        assert_eq!(got, want);
    }
}
