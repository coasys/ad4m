//! The `bot.*` WS RPC calls with their guards. All act on the executor's bots, so the operator
//! or an app on the main agent calls them (`Guard::Executor`), never a managed session: a bot
//! never sets its own grant. An app on the main agent sees only the bots of the main agent's
//! identity. The work is in `bots::service`.

use std::sync::Arc;

use serde_json::Value;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::agent::capabilities::{AGENT_READ_CAPABILITY, AGENT_UPDATE_CAPABILITY};
use crate::bots::service::{
    self, BotActivity, BotAgent, BotLogParams, BotParams, CreateBotParams, CreatedBot,
    SetGrantParams,
};
use crate::types::RequestContext;

fn parse<T: serde::de::DeserializeOwned>(params: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

fn answer<T: serde::Serialize>(value: T) -> Result<Value, WsRpcError> {
    Ok(serde_json::to_value(value)?)
}

async fn create_agent(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(service::create(parse(params)?).await?)
}

async fn set_grant(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(service::set_grant(parse(params)?, ctx).await?)
}

async fn grant(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(service::grant(parse(params)?, ctx).await?)
}

async fn list(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(service::list(ctx).await?)
}

async fn log(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    answer(service::activity(parse(params)?, ctx).await?)
}

/// The `bot.*` methods and their guards.
pub fn register_ws_handlers(map: &mut HandlerMap) {
    // Makes a managed agent and claims the owner's bot offer with its key. The bots act for
    // the person this executor speaks for, so they are the person's (`agent:UPDATE`, as
    // `identity.openOffer`), never a sign-up's: `user.create`'s capability is the anonymous
    // caller's too.
    map.method::<CreateBotParams, CreatedBot>("bot.createAgent", create_agent)
        .long()
        .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<SetGrantParams, BotAgent>("bot.setGrant", set_grant)
        .guard(Guard::Executor(AGENT_UPDATE_CAPABILITY.clone()));
    map.method::<BotParams, BotAgent>("bot.grant", grant)
        .read()
        .guard(Guard::Executor(AGENT_READ_CAPABILITY.clone()));
    map.method::<NoParams, Vec<BotAgent>>("bot.list", list)
        .read()
        .guard(Guard::Executor(AGENT_READ_CAPABILITY.clone()));
    map.method::<BotLogParams, Vec<BotActivity>>("bot.log", log)
        .read()
        .guard(Guard::Executor(AGENT_READ_CAPABILITY.clone()));
}
