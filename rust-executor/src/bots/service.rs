//! The `bot.*` calls: make a bot agent that claims its owner's bot offer, set its grant, read
//! it and its activity log. The RPC registration and guards are in `api/bots_ws.rs`.

use std::sync::Arc;

use ad4m_identity_core::did_of;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::{now, remember, BotAction, GrantState, NOT_IN_CAN};
use crate::agent::capabilities::{
    get_user_default_capabilities, AuthInfo, DEFAULT_TOKEN_VALID_PERIOD,
};
use crate::agent::grants::Grants;
use crate::agent::{did_for_context, AgentContext};
use crate::api::ws_handler::WsRpcError;
use crate::db::{BotActivityRow, BotGrantRow, BotRow};
use crate::db_backend::db_backend;
use crate::identity::bot_key::BotKey;
use crate::identity::types::OfferKind;
use crate::identity::verdict::KeyVerdict;
use crate::types::RequestContext;

/// The most spaces a grant names.
pub const MAX_SPACES: usize = 100;
/// The most activity entries one `bot.log` answers, and how many without `limit`.
pub const LOG_PAGE_MAX: usize = 200;
pub const LOG_PAGE_DEFAULT: usize = 50;
/// The longest bot name.
pub const NAME_MAX: usize = 64;

/// `bot.createAgent` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct CreateBotParams {
    /// The bot offer its owner opened (`ad4m-offer:…` or the offer's JSON).
    pub offer: String,
    /// What its owner calls it.
    pub name: String,
}

/// `bot.createAgent`: the bot agent made, its claim sent; the holder confirms with the same
/// match code, then sets the grant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct CreatedBot {
    /// The bot's agent, a `did:key`.
    pub did: String,
    /// Its device key, the multikey the owner's log lists once the holder confirms.
    pub key: String,
    /// The owner, `did:scid:coasys:1:…`.
    pub owner: String,
    pub offer_id: String,
    /// "R7K Q2M": the holder shows the same code.
    pub match_code: String,
    /// When the offer expires, unix seconds.
    #[ts(type = "number")]
    pub expires: u64,
    /// The bot's session token: the harness that runs the bot calls with it.
    pub token: String,
}

/// `bot.setGrant` params: the whole grant, and the version it replaces.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct SetGrantParams {
    /// The bot's agent DID.
    pub bot: String,
    /// The neighbourhood URLs and perspective UUIDs it may act in.
    pub spaces: Vec<String>,
    pub can: Vec<BotAction>,
    /// Unix seconds; absent for no end but the key's in the owner's log.
    #[ts(type = "number | null", optional)]
    #[serde(default)]
    pub expires: Option<u64>,
    pub state: GrantState,
    /// The version read; a grant changed since answers 409.
    #[ts(type = "number")]
    pub version: u64,
}

/// `bot.grant` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BotParams {
    pub bot: String,
}

/// `bot.log` params.
#[derive(Clone, Debug, Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BotLogParams {
    pub bot: String,
    /// Entries older than this entry id; absent for the newest.
    #[ts(type = "number | null", optional)]
    #[serde(default)]
    pub before: Option<i64>,
    /// At most this many (default 50, at most 200).
    #[ts(type = "number | null", optional)]
    #[serde(default)]
    pub limit: Option<u32>,
}

/// A bot's grant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct BotGrant {
    pub spaces: Vec<String>,
    pub can: Vec<BotAction>,
    #[ts(type = "number | null")]
    pub expires: Option<u64>,
    pub state: GrantState,
    #[ts(type = "number")]
    pub version: u64,
    #[ts(type = "number")]
    pub updated_at: u64,
}

/// What the owner's log says about the bot's key: the ceiling of every grant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct BotKeyState {
    /// Its id in the log (`d3`).
    pub key_id: String,
    /// `valid`, `revoked`, `expired` or `notInLog`.
    pub verdict: String,
    /// The actions the bot mark allows; empty for a key without it.
    pub can: Vec<String>,
    /// The bot mark's expiry, unix seconds; `null` for a key without it.
    #[ts(type = "number | null")]
    pub expires: Option<u64>,
}

/// One bot of this executor.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct BotAgent {
    pub did: String,
    pub name: String,
    /// `did:scid:coasys:1:…`.
    pub owner: String,
    pub offer_id: String,
    #[ts(type = "number")]
    pub created_at: u64,
    pub grant: BotGrant,
    /// `null` until the holder confirms the bot's claim, or when the log is not readable.
    pub key: Option<BotKeyState>,
}

/// One entry of a bot's activity log.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct BotActivity {
    #[ts(type = "number")]
    pub id: i64,
    #[ts(type = "number")]
    pub at: u64,
    pub method: String,
    /// `null` for a call outside every action.
    pub action: Option<BotAction>,
    pub space: Option<String>,
    pub reason: Option<String>,
    /// Written once the call answered: `allowed` (it ran and succeeded), `failed:<status>`
    /// (the grant let it through, then the method's guard or handler answered that error
    /// status), or the bot check's refusal code (`bot-grant`, `bot-paused`, `not-in-can`, …).
    pub outcome: String,
}

fn db_error(e: impl std::fmt::Display) -> WsRpcError {
    WsRpcError::internal(e.to_string())
}

fn grant_of(row: &BotGrantRow) -> BotGrant {
    BotGrant {
        spaces: row.spaces.clone(),
        can: row.can.iter().filter_map(|a| BotAction::parse(a)).collect(),
        expires: row.expires,
        state: GrantState::parse(&row.state).unwrap_or(GrantState::Revoked),
        version: row.version,
        updated_at: row.updated_at,
    }
}

fn key_state(key: BotKey) -> BotKeyState {
    BotKeyState {
        key_id: key.key_id,
        verdict: match key.verdict {
            KeyVerdict::Valid => "valid",
            KeyVerdict::Revoked => "revoked",
            KeyVerdict::Expired => "expired",
            KeyVerdict::NotInLog => "notInLog",
        }
        .to_string(),
        can: key
            .scope
            .as_ref()
            .map(|s| s.can.clone())
            .unwrap_or_default(),
        expires: key.scope.as_ref().map(|s| s.expires),
    }
}

/// What the owner's log says about the key of `bot`, when it can be read.
async fn read_key(bot: &BotRow) -> Option<BotKey> {
    let identity = crate::identity::service().ok()?;
    let key = bot.did.strip_prefix("did:key:")?;
    identity.bot_key(&bot.owner, key).await.ok()
}

async fn view(bot: &BotRow) -> BotAgent {
    BotAgent {
        did: bot.did.clone(),
        name: bot.name.clone(),
        owner: did_of(&bot.owner),
        offer_id: bot.offer_id.clone(),
        created_at: bot.created_at,
        grant: grant_of(&bot.grant),
        key: read_key(bot).await.map(key_state),
    }
}

/// The identity of the main agent, for a caller that is not the operator: an app on the main
/// agent manages only the bots of that identity.
async fn caller_identity(ctx: &RequestContext) -> Option<String> {
    if ctx.is_operator() {
        return None;
    }
    let main = did_for_context(&AgentContext::main_agent()).ok()?;
    crate::identity::service()
        .ok()?
        .identity_of(&main)
        .await
        .or_else(|| Some(String::new()))
}

fn visible(bot: &BotRow, identity: &Option<String>) -> bool {
    identity
        .as_ref()
        .is_none_or(|identity| *identity == did_of(&bot.owner))
}

/// The bot `did`, if the caller may manage it: missing and not the caller's are both 404.
async fn find(did: &str, ctx: &RequestContext) -> Result<BotRow, WsRpcError> {
    let identity = caller_identity(ctx).await;
    db_backend()
        .bot(did)
        .map_err(db_error)?
        .filter(|bot| visible(bot, &identity))
        .ok_or_else(|| WsRpcError::not_found(format!("not-found: no bot {did}")))
}

/// `bot.createAgent`.
pub async fn create(params: CreateBotParams) -> Result<CreatedBot, WsRpcError> {
    crate::config::may_add_managed_user(
        crate::config::UserKind::Bot,
        crate::config::custody_channel_on(),
        crate::user_management::is_multi_user_enabled(),
    )
    .map_err(WsRpcError::forbidden)?;
    let name = params.name.trim().to_string();
    if name.is_empty() || name.chars().count() > NAME_MAX {
        return Err(WsRpcError::bad_request(format!(
            "invalid: a bot's name has 1 to {NAME_MAX} characters"
        )));
    }
    let offer = crate::identity::service::parse_offer(&params.offer)?;
    let identity = crate::identity::service()?;
    identity.refuse_without_store_keys()?;

    let username = format!("bot-{}@bots.ad4m", uuid::Uuid::new_v4());
    let password = format!("{}{}", uuid::Uuid::new_v4(), uuid::Uuid::new_v4());
    let did =
        crate::user_management::create_user(&username, &password).map_err(WsRpcError::internal)?;
    if let Err(e) = crate::user_management::init_user_profile(&did).await {
        ::log::warn!("bots: the profile of {did}: {e}");
    }
    let at = now();
    let mut bot = BotRow {
        did: did.clone(),
        username: username.clone(),
        name: name.clone(),
        owner: offer.scid.clone(),
        offer_id: offer.offer_id.clone(),
        created_at: at,
        grant: BotGrantRow {
            spaces: vec![],
            can: vec![],
            expires: None,
            state: GrantState::Active.as_str().to_string(),
            version: 1,
            updated_at: at,
        },
    };
    db_backend().add_bot(&bot).map_err(db_error)?;
    remember(bot.clone());

    let agent = AgentContext::managed(did.clone());
    let claimed = identity
        .claims()
        .claim_for_agent(&agent, &offer, false, Some(name.clone()), || Ok(()))
        .await;
    let refusal = match &claimed {
        Ok(result) if result.kind == OfferKind::Bot => None,
        Ok(_) => Some(WsRpcError::bad_request(
            "not-a-bot-offer: the holder opened this offer for a device, not a bot",
        )),
        Err(e) => Some(WsRpcError {
            code: e.code,
            message: e.message.clone(),
        }),
    };
    if let Some(refusal) = refusal {
        // The agent stays (its key may be in a claim), as a bot that may do nothing.
        bot.grant.state = GrantState::Revoked.as_str().to_string();
        bot.grant.version += 1;
        if db_backend()
            .set_bot_grant(&did, &bot.grant, 1)
            .map_err(db_error)?
        {
            remember(bot);
        }
        return Err(refusal);
    }
    let claimed = claimed?;
    // The bot's links anchor in its owner's log (`identity::anchor_for`): keep that log in the
    // key map, as the startup load does for the owners of the bots already here.
    if let Err(e) = identity.cache.keep_local(&offer.scid).await {
        ::log::warn!(
            "bots: the log of {} is unreadable ({e}); the bot's links carry no anchor until its next accepted head",
            offer.scid
        );
    }
    let auth = AuthInfo {
        app_name: name.clone(),
        app_desc: format!("The bot {name}"),
        app_domain: None,
        app_url: None,
        app_icon_path: None,
        capabilities: Some(get_user_default_capabilities()),
        user_email: Some(username),
    };
    let (token, _) = crate::agent::grants::mint(agent, auth, None, DEFAULT_TOKEN_VALID_PERIOD)
        .map_err(WsRpcError::internal)?;
    Ok(CreatedBot {
        key: did.strip_prefix("did:key:").unwrap_or(&did).to_string(),
        did,
        owner: claimed.did,
        offer_id: claimed.offer_id,
        match_code: claimed.match_code,
        expires: claimed.expires,
        token,
    })
}

/// `bot.setGrant`.
pub async fn set_grant(
    params: SetGrantParams,
    ctx: Arc<RequestContext>,
) -> Result<BotAgent, WsRpcError> {
    let bot = find(&params.bot, &ctx).await?;
    if bot.grant.state == GrantState::Revoked.as_str() {
        return Err(WsRpcError {
            code: 409,
            message: "bot-revoked: a revoked grant never changes".to_string(),
        });
    }
    if params.version != bot.grant.version {
        return Err(version_conflict(bot.grant.version));
    }
    if params.spaces.len() > MAX_SPACES {
        return Err(WsRpcError::bad_request(format!(
            "invalid: a grant names at most {MAX_SPACES} spaces"
        )));
    }
    if params.spaces.iter().any(|s| s.trim().is_empty()) {
        return Err(WsRpcError::bad_request("invalid: an empty space"));
    }
    if let Some(scope) = read_key(&bot).await.and_then(|key| key.scope) {
        if let Some(action) = params
            .can
            .iter()
            .find(|a| !scope.can.iter().any(|c| c == a.as_str()))
        {
            return Err(WsRpcError::bad_request(format!(
                "{NOT_IN_CAN}: the owner's log does not allow {} for this bot",
                action.as_str()
            )));
        }
    }
    let mut can: Vec<String> = Vec::new();
    for action in &params.can {
        if !can.iter().any(|a| a == action.as_str()) {
            can.push(action.as_str().to_string());
        }
    }
    let mut spaces: Vec<String> = Vec::new();
    for space in &params.spaces {
        if !spaces.contains(space) {
            spaces.push(space.clone());
        }
    }
    let grant = BotGrantRow {
        spaces,
        can,
        expires: params.expires,
        state: params.state.as_str().to_string(),
        version: bot.grant.version + 1,
        updated_at: now(),
    };
    if !db_backend()
        .set_bot_grant(&bot.did, &grant, bot.grant.version)
        .map_err(db_error)?
    {
        let current = db_backend()
            .bot(&bot.did)
            .map_err(db_error)?
            .map(|b| b.grant.version)
            .unwrap_or(0);
        return Err(version_conflict(current));
    }
    let bot = BotRow { grant, ..bot };
    remember(bot.clone());
    if params.state == GrantState::Revoked {
        // Its sessions end too: a revoked bot holds no token.
        if let Err(e) = Grants::global().revoke_agent(&AgentContext::managed(bot.did.clone())) {
            ::log::warn!("bots: ending the sessions of {}: {e}", bot.did);
        }
    }
    Ok(view(&bot).await)
}

fn version_conflict(current: u64) -> WsRpcError {
    WsRpcError {
        code: 409,
        message: format!("version: the grant changed, its version is now {current}"),
    }
}

/// `bot.grant`.
pub async fn grant(params: BotParams, ctx: Arc<RequestContext>) -> Result<BotAgent, WsRpcError> {
    let bot = find(&params.bot, &ctx).await?;
    Ok(view(&bot).await)
}

/// `bot.list`: the bots the caller manages, oldest first.
pub async fn list(ctx: Arc<RequestContext>) -> Result<Vec<BotAgent>, WsRpcError> {
    let identity = caller_identity(&ctx).await;
    let mut views = Vec::new();
    for bot in db_backend().bots().map_err(db_error)? {
        if visible(&bot, &identity) {
            views.push(view(&bot).await);
        }
    }
    Ok(views)
}

/// `bot.log`: the bot's activity, newest first.
pub async fn activity(
    params: BotLogParams,
    ctx: Arc<RequestContext>,
) -> Result<Vec<BotActivity>, WsRpcError> {
    let bot = find(&params.bot, &ctx).await?;
    let limit = params
        .limit
        .map(|l| (l as usize).clamp(1, LOG_PAGE_MAX))
        .unwrap_or(LOG_PAGE_DEFAULT);
    let rows = db_backend()
        .bot_activity(&bot.did, params.before, limit)
        .map_err(db_error)?;
    Ok(rows.into_iter().map(activity_of).collect())
}

fn activity_of(row: BotActivityRow) -> BotActivity {
    BotActivity {
        id: row.id,
        at: row.at,
        method: row.method,
        action: row.action.as_deref().and_then(BotAction::parse),
        space: row.space,
        reason: row.reason,
        outcome: row.outcome,
    }
}
