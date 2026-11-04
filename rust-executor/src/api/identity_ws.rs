//! The `identity.*` WS RPC calls with their guards (the table in
//! `tests::every_identity_call_has_its_guard`). "The agent's own session" is `Cap(…)` with the
//! handler acting for `RequestContext::acts_as()`: the main agent for the operator and the main
//! agent's apps, the managed agent for a user. The work is in `identity::service`.

use std::sync::Arc;

use ad4m_identity_core::{Event, FoldError, FormatError};
use serde_json::value::RawValue;
use serde_json::Value;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::agent::capabilities::{
    AGENT_READ_CAPABILITY, AGENT_SIGN_CAPABILITY, AGENT_UPDATE_CAPABILITY,
};
use crate::agent::AgentContext;
use crate::identity::types::{
    ClaimOfferParams, ClaimOfferResult, CosignInceptParams, DeleteRequestParams, DeviceKeys,
    DeviceRequest, DeviceView, DidResolutionResult, IdentityLog, IdentityStatus, InboxAckParams,
    InboxEntry, InboxPutParams, InboxStored, JoinStatus, JoinStatusParams, LogParams, OfferListing,
    OfferOpened, OffersParams, OpenOfferParams, PostRequestParams, PreparedDevice, PublishParams,
    RecordKey, RecordParams, RecordPutParams, RecordStored, RequestPosted, RequestsParams,
    ResetLogParams, ResolveParams, SealedRecord, SignedHead, TakenMaster, UnwrapRecordKeyParams,
};
use crate::types::RequestContext;

fn parse<T: serde::de::DeserializeOwned>(params: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

fn answer<T: serde::Serialize>(value: T) -> Result<Value, WsRpcError> {
    Ok(serde_json::to_value(value)?)
}

/// The refusal of an `identity.publish` or `identity.cosignIncept` frame whose event (in
/// `params.events`, `params.event`) names a member twice, or whose frame repeats `params`,
/// `events` or `event`: `invalid: DuplicateMember (…)`, the
/// rule's name as the store gives it. The socket reads a frame into a `Value`, which keeps only the
/// last of the two, so the frame's own text is read here; every other rule of the event format
/// the handler applies to the values. `None` for another method, or a frame this does not read
/// (its params outside the contract, which dispatch refuses).
pub(crate) fn duplicate_member_refusal(method: &str, frame: &str) -> Option<WsRpcError> {
    #[derive(serde::Deserialize)]
    struct Frame<'a> {
        #[serde(borrow, default)]
        params: Option<Params<'a>>,
    }
    #[derive(serde::Deserialize)]
    struct Params<'a> {
        #[serde(borrow, default)]
        events: Option<Vec<&'a RawValue>>,
        #[serde(borrow, default)]
        event: Option<&'a RawValue>,
    }
    if !matches!(method, "identity.publish" | "identity.cosignIncept") {
        return None;
    }
    let refusal = |e: FormatError| WsRpcError::bad_request(format!("invalid: {} ({e})", e.name()));
    // A repeated `params`, `events` or `event` is a duplicate member too: the frame's `Value`
    // would carry only the last copy. The derived `Deserialize` refuses it as a duplicate field.
    let params = match serde_json::from_str::<Frame>(frame) {
        Ok(frame) => frame.params?,
        Err(e) => {
            let text = e.to_string();
            let name = text.strip_prefix("duplicate field `")?.split('`').next()?;
            return Some(refusal(FormatError::DuplicateMember(name.to_string())));
        }
    };
    let texts = match method {
        "identity.publish" => params.events.unwrap_or_default(),
        _ => params.event.into_iter().collect(),
    };
    texts
        .iter()
        .find_map(|text| match Event::from_json_text(text.get()) {
            Err(FoldError::Format(e @ FormatError::DuplicateMember(_))) => Some(e),
            _ => None,
        })
        .map(refusal)
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

async fn claim_offer(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: ClaimOfferParams = parse(params)?;
    let can_keep_master = p.can_keep_master.unwrap_or(false);
    // Only the custody service may say this executor keeps the master key: it is the one
    // caller of `identity.takeMaster`.
    if can_keep_master {
        crate::identity::custody::admit(&ctx.session, crate::config::custody_channel_on())?;
    }
    let offer = crate::identity::service::parse_offer(&p.offer)?;
    answer(
        crate::identity::service()?
            .claims()
            .claim_for_main(&offer, can_keep_master, p.hint)
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

/// `identity.prepareDevice`: this executor's device string; its pre-join inbox is read until
/// the string expires, and a valid ticket is claimed (`identity::inbox`).
async fn prepare_device(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let service = crate::identity::service()?;
    answer(
        crate::identity::inbox::prepare_device(&service, crate::config::custody_channel_on())
            .await?,
    )
}

async fn inbox_put(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: InboxPutParams = parse(params)?;
    answer(crate::identity::service()?.inbox_put(&p).await?)
}

/// `identity.inbox`: the main agent's waiting messages, by id and type only.
async fn inbox(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(
        crate::identity::service()?
            .pull_inbox(
                &AgentContext::main_agent(),
                crate::config::custody_channel_on(),
            )
            .await?,
    )
}

/// `identity.takeMaster`: the custody channel's call; the master key is in this answer only.
async fn take_master(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    crate::identity::custody::admit(&ctx.session, crate::config::custody_channel_on())?;
    answer(
        crate::identity::service()?
            .take_master(&AgentContext::main_agent())
            .await?,
    )
}

async fn inbox_ack(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: InboxAckParams = parse(params)?;
    answer(
        crate::identity::service()?
            .inbox_ack(&AgentContext::main_agent(), &p.id)
            .await?,
    )
}

async fn record(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: RecordParams = parse(params)?;
    answer(
        crate::identity::service()?
            .record(&ctx.acts_as(), &p)
            .await?,
    )
}

async fn record_put(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: RecordPutParams = parse(params)?;
    answer(
        crate::identity::service()?
            .record_put(&ctx.acts_as(), &p)
            .await?,
    )
}

/// `identity.unwrapRecordKey`: the main agent's record key of one version, for the app on this
/// machine that opens the record (the desktop's custody service).
async fn unwrap_record_key(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: UnwrapRecordKeyParams = parse(params)?;
    answer(
        crate::identity::service()?
            .unwrap_record_key(&AgentContext::main_agent(), &p)
            .await?,
    )
}

async fn requests(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: RequestsParams = parse(params)?;
    answer(
        crate::identity::service()?
            .requests(&ctx.acts_as(), &p)
            .await?,
    )
}

async fn post_request(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: PostRequestParams = parse(params)?;
    answer(
        crate::identity::service()?
            .post_request(&ctx.acts_as(), &p)
            .await?,
    )
}

async fn delete_request(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DeleteRequestParams = parse(params)?;
    answer(
        crate::identity::service()?
            .delete_request(&ctx.acts_as(), &p)
            .await?,
    )
}

/// `identity.rekey`: a new main wallet key; every place that holds the main agent's DID moves
/// to it (`identity::rekey`).
async fn rekey(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(crate::identity::rekey::rekey_main().await?)
}

/// `identity.custodyStatus`: the custody channel is open, and the device keys a master key
/// for this computer is sealed to.
async fn custody_status(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    crate::identity::custody::admit(&ctx.session, crate::config::custody_channel_on())?;
    answer(crate::identity::custody::main_device_keys()?)
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
    // The device inbox of this executor's main key: the operator on this machine only. Its
    // device string, its messages and the claim of a ticket are the main agent's.
    map.method::<NoParams, PreparedDevice>("identity.prepareDevice", prepare_device)
        .local_only()
        .guard(Guard::Operator);
    map.method::<NoParams, Vec<InboxEntry>>("identity.inbox", inbox)
        .read()
        .local_only()
        .guard(Guard::Operator);
    map.method::<InboxAckParams, bool>("identity.inboxAck", inbox_ack)
        .local_only()
        .guard(Guard::Operator);
    // The one call that carries a master key: the custody channel only
    // (`identity::custody::admit`); every other caller gets 403.
    map.method::<NoParams, Option<TakenMaster>>("identity.takeMaster", take_master)
        .local_only()
        .guard(Guard::Operator);
    // Passes a master-signed inbox write: the signature is the authority, as for openOffer.
    map.method::<InboxPutParams, InboxStored>("identity.inboxPut", inbox_put)
        .guard(Guard::Cap(AGENT_UPDATE_CAPABILITY.clone()));
    // The private record and the request box of the agent's identity: reads and writes
    // signed by a key of the identity, or by the agent's own device key.
    map.method::<RecordParams, Option<SealedRecord>>("identity.record", record)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<RecordPutParams, RecordStored>("identity.recordPut", record_put)
        .guard(Guard::Cap(AGENT_UPDATE_CAPABILITY.clone()));
    // The record key of the main agent's entry, opened in the wallet for the app on this
    // machine that reads the record through this executor: the operator here only.
    map.method::<UnwrapRecordKeyParams, RecordKey>("identity.unwrapRecordKey", unwrap_record_key)
        .local_only()
        .guard(Guard::Operator);
    map.method::<RequestsParams, Vec<DeviceRequest>>("identity.requests", requests)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<PostRequestParams, RequestPosted>("identity.postRequest", post_request)
        .guard(Guard::Cap(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<DeleteRequestParams, bool>("identity.deleteRequest", delete_request)
        .guard(Guard::Cap(AGENT_UPDATE_CAPABILITY.clone()));
    // A new main key for this executor: only on this machine, never from a network session.
    map.method::<NoParams, DeviceKeys>("identity.rekey", rekey)
        .local_only()
        .guard(Guard::Operator);
    // The custody channel: the admin credential on a loopback listener of an executor started
    // with --custody-channel (`identity::custody::admit`); any other caller gets 403.
    map.method::<NoParams, DeviceKeys>("identity.custodyStatus", custody_status)
        .read()
        .local_only()
        .guard(Guard::Operator);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::Capability;

    /// Only an event's duplicate member is refused from the frame; anything else is the
    /// handler's (and dispatch's) to refuse.
    #[test]
    fn a_frame_event_that_names_a_member_twice_is_refused() {
        let twice = r#"{"seq":0,"op":"add","seq":1}"#;
        let publish = |events: &str| {
            duplicate_member_refusal(
                "identity.publish",
                &format!(
                    r#"{{"id":"1","type":"identity.publish","params":{{"events":{events}}}}}"#
                ),
            )
        };
        let refused = publish(&format!("[{{\"seq\":0}},{twice}]")).expect("refused");
        assert_eq!(refused.code, 400);
        assert!(
            refused.message.starts_with("invalid: DuplicateMember")
                && refused.message.contains("\"seq\""),
            "{}",
            refused.message
        );
        let cosign = duplicate_member_refusal(
            "identity.cosignIncept",
            &format!(r#"{{"type":"identity.cosignIncept","params":{{"event":{twice}}}}}"#),
        );
        assert!(cosign.is_some());
        // A nested object counts too.
        assert!(publish(r#"[{"proof":[{"a":1,"a":2}]}]"#).is_some());
        // So does a frame that repeats `params`, `events` or `event`: its `Value` keeps the last.
        let repeated = |frame: &str, method: &str, member: &str| {
            let refused = duplicate_member_refusal(method, frame).expect("refused");
            assert_eq!(refused.code, 400);
            assert!(
                refused.message.starts_with("invalid: DuplicateMember")
                    && refused.message.contains(member),
                "{}",
                refused.message
            );
        };
        repeated(
            r#"{"type":"identity.publish","params":{"events":[]},"params":{"events":[{"seq":0}]}}"#,
            "identity.publish",
            "params",
        );
        repeated(
            r#"{"type":"identity.publish","params":{"events":[],"events":[{"seq":0}]}}"#,
            "identity.publish",
            "events",
        );
        repeated(
            r#"{"type":"identity.cosignIncept","params":{"event":{},"event":{"seq":0}}}"#,
            "identity.cosignIncept",
            "event",
        );
        // Another format error, params outside the contract, another method: not refused here.
        assert!(publish(r#"[{"seq":0}]"#).is_none());
        assert!(publish(r#""not a list""#).is_none());
        assert!(duplicate_member_refusal(
            "identity.log",
            &format!(r#"{{"params":{{"events":[{twice}]}}}}"#)
        )
        .is_none());
    }

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
            ("identity.custodyStatus", format!("{:?}", Guard::Operator)),
            ("identity.deleteRequest", cap(&AGENT_UPDATE_CAPABILITY)),
            ("identity.devices", cap(&AGENT_READ_CAPABILITY)),
            ("identity.inbox", format!("{:?}", Guard::Operator)),
            ("identity.inboxAck", format!("{:?}", Guard::Operator)),
            ("identity.inboxPut", cap(&AGENT_UPDATE_CAPABILITY)),
            ("identity.joinStatus", cap(&AGENT_READ_CAPABILITY)),
            ("identity.log", cap(&AGENT_READ_CAPABILITY)),
            ("identity.offers", cap(&AGENT_READ_CAPABILITY)),
            ("identity.openOffer", cap(&AGENT_UPDATE_CAPABILITY)),
            ("identity.postRequest", cap(&AGENT_UPDATE_CAPABILITY)),
            ("identity.prepareDevice", format!("{:?}", Guard::Operator)),
            (
                "identity.publish",
                format!(
                    "{:?}",
                    Guard::UserOrOperator(AGENT_UPDATE_CAPABILITY.clone())
                ),
            ),
            ("identity.record", cap(&AGENT_READ_CAPABILITY)),
            ("identity.recordPut", cap(&AGENT_UPDATE_CAPABILITY)),
            ("identity.rekey", format!("{:?}", Guard::Operator)),
            ("identity.requests", cap(&AGENT_READ_CAPABILITY)),
            ("identity.resetLog", format!("{:?}", Guard::Operator)),
            ("identity.resolve", cap(&AGENT_READ_CAPABILITY)),
            ("identity.status", cap(&AGENT_READ_CAPABILITY)),
            ("identity.takeMaster", format!("{:?}", Guard::Operator)),
            ("identity.unwrapRecordKey", format!("{:?}", Guard::Operator)),
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

    /// `identity.rekey`, the custody channel and the device inbox stay on this machine: the
    /// admin credential on a network listener and an operator from a key login get 403
    /// `local-only`, an app with every capability 403. The custody channel then also needs
    /// `--custody-channel`: without it the operator on this machine gets 403
    /// `no-custody-channel`.
    #[tokio::test]
    async fn rekey_and_the_custody_channel_refuse_every_caller_off_this_machine() {
        use crate::agent::session::{Principal, Session};
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let network = || {
            Arc::new(RequestContext::for_session(
                Session::for_test(Principal::Operator).on_network(),
            ))
        };
        let app = || {
            Arc::new(RequestContext::app_for_test(vec![
                crate::agent::capabilities::ALL_CAPABILITY.clone(),
            ]))
        };
        let key_login = || {
            Arc::new(RequestContext::for_session(
                Session::for_test(Principal::Operator).from_key_login(),
            ))
        };
        for (method, params) in [
            ("identity.rekey", serde_json::json!({})),
            ("identity.custodyStatus", serde_json::json!({})),
            ("identity.takeMaster", serde_json::json!({})),
            ("identity.prepareDevice", serde_json::json!({})),
            ("identity.inbox", serde_json::json!({})),
            ("identity.inboxAck", serde_json::json!({ "id": "m1" })),
            (
                "identity.unwrapRecordKey",
                serde_json::json!({ "wrapped": "x" }),
            ),
        ] {
            for caller in [network(), key_login()] {
                let refused = map
                    .dispatch(method, params.clone(), caller)
                    .await
                    .unwrap_err();
                assert_eq!(refused.code, 403, "{method}");
                assert!(
                    refused.message.starts_with("local-only"),
                    "{method}: {}",
                    refused.message
                );
            }
            let refused = map.dispatch(method, params, app()).await.unwrap_err();
            assert_eq!(refused.code, 403, "{method}");
        }
        let saved = crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(crate::Ad4mConfig::default());
        let mut no_channel = vec![];
        for method in ["identity.custodyStatus", "identity.takeMaster"] {
            no_channel.push(
                map.dispatch(
                    method,
                    serde_json::json!({}),
                    Arc::new(RequestContext::operator_for_test()),
                )
                .await
                .unwrap_err(),
            );
        }
        // With the flag, a tokenless operator on loopback is still not the custody service.
        crate::config::set_global_config(crate::Ad4mConfig {
            custody_channel: Some(true),
            ..Default::default()
        });
        no_channel.push(
            map.dispatch(
                "identity.takeMaster",
                serde_json::json!({}),
                Arc::new(RequestContext::for_session(
                    Session::for_test(Principal::Operator).without_credential(),
                )),
            )
            .await
            .unwrap_err(),
        );
        *crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner()) = saved;
        for refused in no_channel {
            assert_eq!(refused.code, 403);
            assert!(
                refused
                    .message
                    .starts_with(crate::identity::custody::NO_CUSTODY_CHANNEL),
                "{}",
                refused.message
            );
        }
    }

    /// A claim that says it keeps the master key needs the custody channel's caller: an
    /// executor without the channel would delete every master key sent to it, and with the
    /// channel only the admin credential on loopback (the custody service) takes one.
    #[tokio::test]
    async fn a_claim_keeps_the_master_key_only_through_the_custody_channel() {
        use crate::agent::session::{Principal, Session};
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let saved = crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        let params = serde_json::json!({ "offer": "coasys:offer:1:x", "canKeepMaster": true });
        crate::config::set_global_config(crate::Ad4mConfig::default());
        let mut refused = vec![map
            .dispatch(
                "identity.claimOffer",
                params.clone(),
                Arc::new(RequestContext::operator_for_test()),
            )
            .await
            .unwrap_err()];
        crate::config::set_global_config(crate::Ad4mConfig {
            custody_channel: Some(true),
            ..Default::default()
        });
        for session in [
            Session::for_test(Principal::Operator).from_key_login(),
            Session::for_test(Principal::Operator).on_network(),
            Session::for_test(Principal::Operator).without_credential(),
        ] {
            refused.push(
                map.dispatch(
                    "identity.claimOffer",
                    params.clone(),
                    Arc::new(RequestContext::for_session(session)),
                )
                .await
                .unwrap_err(),
            );
        }
        *crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner()) = saved;
        for refused in refused {
            assert_eq!(refused.code, 403);
            assert!(
                refused
                    .message
                    .starts_with(crate::identity::custody::NO_CUSTODY_CHANNEL),
                "{}",
                refused.message
            );
        }
    }

    /// With `--custody-channel`, the admin credential on a loopback listener gets this
    /// computer's device keys.
    #[tokio::test]
    async fn the_custody_channel_answers_the_admin_credential_on_loopback() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        crate::config::set_global_config(crate::Ad4mConfig {
            custody_channel: Some(true),
            ..Default::default()
        });
        let answer = map
            .dispatch(
                "identity.custodyStatus",
                serde_json::json!({}),
                Arc::new(RequestContext::operator_for_test()),
            )
            .await;
        crate::config::set_global_config(crate::Ad4mConfig::default());
        let keys: DeviceKeys = serde_json::from_value(answer.unwrap()).unwrap();
        assert_eq!(keys.sign, crate::agent::did());
        assert!(keys.enc.starts_with("z6LS"), "{}", keys.enc);
    }
}
