//! Bots: agents that act for a person, under a grant the person controls.
//!
//! A bot is a managed agent of this executor (a `users` row, a wallet key) whose key is in its
//! owner's key log with the bot mark (`KeyLife::Bot`, with the actions it may take and until
//! when). `bot.createAgent` makes the agent and claims the owner's bot offer with its key
//! ([`service`]); once the holder confirms, the log lists the key. The owner's grant, kept
//! here (`db/bots.rs`), says where and what the bot may do now: spaces, actions, expiry, and
//! whether it is active, paused or revoked.
//!
//! Every call from a bot's session passes [`admit`] before its guard (`api/ws_handler.rs`):
//!
//! 1. [`reach`]: what the call does. Its own agent and session ([`Reach::Own`]) is always
//!    allowed. Reading or writing one perspective, or joining one neighbourhood, is an action
//!    in a space ([`Reach::Action`]). Every other method is outside every grant: `403 bot-grant`.
//! 2. [`decide`]: the grant (revoked, paused, expired, the action, the space), then the log
//!    (the key listed with the bot mark and valid, the action in its scope), then the reason
//!    a write or a join must give (`meta.reason` of the request, `400 reason-required`).
//!
//! [`log`] keeps every write and join the bot made and every call refused, with the reason the
//! bot gave; reads are not kept. The owner reads it with `bot.log`; a revoked bot's log stays.

pub mod log;
pub mod service;

use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};

use ad4m_identity_core::BotScope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use crate::api::guards::{Guard, Owner};
use crate::api::ws_handler::WsRpcError;
use crate::db::BotRow;
use crate::identity::bot_key::{BotKey, BotKeyError};
use crate::identity::verdict::KeyVerdict;
use crate::types::RequestContext;

/// A call outside the bot's grant: a method no grant covers, or a space not in it.
pub const BOT_GRANT: &str = "bot-grant";
/// The owner paused the bot.
pub const BOT_PAUSED: &str = "bot-paused";
/// The action is not in the grant's `can`, or not in the scope the log gives the key.
pub const NOT_IN_CAN: &str = "not-in-can";
/// The grant or the bot's key expired.
pub const BOT_EXPIRED: &str = "bot-expired";
/// The owner revoked the grant, or the log revoked the key.
pub const BOT_REVOKED: &str = "bot-revoked";
/// The owner's log does not list the bot's key with the bot mark (yet).
pub const BOT_UNBOUND: &str = "bot-unbound";
/// A write or a join without `meta.reason`.
pub const REASON_REQUIRED: &str = "reason-required";
/// The longest reason kept, in characters.
pub const REASON_MAX: usize = 280;

/// What a grant lets a bot do in a space.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
pub enum BotAction {
    Read,
    Write,
    Join,
}

impl BotAction {
    pub fn as_str(self) -> &'static str {
        match self {
            BotAction::Read => "read",
            BotAction::Write => "write",
            BotAction::Join => "join",
        }
    }

    pub fn parse(text: &str) -> Option<Self> {
        match text {
            "read" => Some(BotAction::Read),
            "write" => Some(BotAction::Write),
            "join" => Some(BotAction::Join),
            _ => None,
        }
    }
}

/// Whether a grant is in force.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "lowercase")]
pub enum GrantState {
    Active,
    Paused,
    /// For good: no change brings the grant back.
    Revoked,
}

impl GrantState {
    pub fn as_str(self) -> &'static str {
        match self {
            GrantState::Active => "active",
            GrantState::Paused => "paused",
            GrantState::Revoked => "revoked",
        }
    }

    pub fn parse(text: &str) -> Option<Self> {
        match text {
            "active" => Some(GrantState::Active),
            "paused" => Some(GrantState::Paused),
            "revoked" => Some(GrantState::Revoked),
            _ => None,
        }
    }
}

/// What a call does, for a bot's grant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Reach {
    /// The bot's own agent and session: always allowed, never logged.
    Own,
    /// An action, in the space `space` (a perspective's UUID, or a neighbourhood URL to
    /// join); `None` for a read of public data that names no space (an expression).
    Action {
        action: BotAction,
        space: Option<Space>,
    },
    /// Outside every grant.
    Outside,
}

/// The space a call names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Space {
    /// A perspective, by UUID: in the grant when its UUID or its neighbourhood URL is.
    Perspective(String),
    /// A neighbourhood URL (a join).
    Url(String),
}

impl Space {
    fn text(&self) -> &str {
        match self {
            Space::Perspective(uuid) => uuid,
            Space::Url(url) => url,
        }
    }
}

/// Methods on the bot's own agent and session, beside the [`Guard::Open`] ones.
const OWN: &[&str] = &["agent.get", "agent.status"];

/// Methods that read public data and name no space: allowed with `read`.
const READS_WITHOUT_SPACE: &[&str] = &[
    "expression.get",
    "expression.getMany",
    "expression.getRaw",
    "expression.interactions",
];

/// Joins: the method and the param that holds the neighbourhood URL.
const JOINS: &[(&str, &str)] = &[("neighbourhood.join", "url")];

/// Methods on one perspective that change what it is or run work on it, not its content: the
/// perspective's owner calls them, never a bot.
const PERSPECTIVE_ADMIN: &[&str] = &[
    "perspective.update",
    "perspective.remove",
    "perspective.runAgent",
    "perspective.runInterpretation",
    "perspective.runInterpretationWithHarness",
    "perspective.addAutoProcessor",
    "perspective.removeAutoProcessor",
    "perspective.publishSnapshot",
    "neighbourhood.publish",
];

/// The MCP tools a bot may call: each runs RPC methods through the handler map, so
/// [`admit`] sees every call. The auth tools run for every caller.
pub const BOT_MCP_TOOLS: &[&str] = &[
    "add_link",
    "query_links",
    "execute_commands",
    "neighbourhood_join_from_url",
    "get_my_did",
    "get_agent_profile",
    "get_flows",
    "flow_valid_outputs",
];

/// What `method`, guarded by `guard`, does with `params`, for a bot's grant.
pub fn reach(method: &str, guard: &Guard, params: &Value) -> Reach {
    if matches!(guard, Guard::Open) || OWN.contains(&method) {
        return Reach::Own;
    }
    if READS_WITHOUT_SPACE.contains(&method) {
        return Reach::Action {
            action: BotAction::Read,
            space: None,
        };
    }
    if let Some((_, param)) = JOINS.iter().find(|(name, _)| *name == method) {
        return match params.get(*param).and_then(Value::as_str) {
            Some(url) => Reach::Action {
                action: BotAction::Join,
                space: Some(Space::Url(url.to_string())),
            },
            None => Reach::Outside,
        };
    }
    if PERSPECTIVE_ADMIN.contains(&method) {
        return Reach::Outside;
    }
    let Guard::Owns {
        cap,
        id,
        owner: Owner::Perspective,
    } = guard
    else {
        return Reach::Outside;
    };
    let Some(uuid) = id.read(params) else {
        return Reach::Outside;
    };
    let capability = cap(vec![uuid.clone()]);
    let can = |c: &str| capability.can.iter().any(|x| x == c);
    let action = match capability.with.domain.as_str() {
        crate::agent::capabilities::PERSPECTIVE if can("READ") || can("SUBSCRIBE") => {
            Some(BotAction::Read)
        }
        crate::agent::capabilities::PERSPECTIVE if can("UPDATE") || can("CREATE") => {
            Some(BotAction::Write)
        }
        crate::agent::capabilities::NEIGHBOURHOOD if can("READ") => Some(BotAction::Read),
        crate::agent::capabilities::NEIGHBOURHOOD if can("UPDATE") => Some(BotAction::Write),
        _ => None,
    };
    match action {
        Some(action) => Reach::Action {
            action,
            space: Some(Space::Perspective(uuid)),
        },
        None => Reach::Outside,
    }
}

/// A refused bot call: its code and the error the caller gets.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Refusal {
    pub code: &'static str,
    pub status: u16,
    pub message: String,
}

impl Refusal {
    fn new(code: &'static str, status: u16, detail: impl std::fmt::Display) -> Self {
        Refusal {
            code,
            status,
            message: format!("{code}: {detail}"),
        }
    }

    fn forbidden(code: &'static str, detail: impl std::fmt::Display) -> Self {
        Self::new(code, 403, detail)
    }

    pub fn error(&self) -> WsRpcError {
        WsRpcError {
            code: self.status,
            message: self.message.clone(),
        }
    }
}

/// Unix seconds.
pub fn now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// The grant's half of the decision: revoked, the method, paused, expired, the action, the
/// space. `space_url` is the neighbourhood URL of a perspective the call names, if it has one.
pub fn decide_grant(
    bot: &BotRow,
    reach: &Reach,
    space_url: Option<&str>,
    now: u64,
) -> Result<(), Refusal> {
    let grant = &bot.grant;
    if grant.state == GrantState::Revoked.as_str() {
        return Err(Refusal::forbidden(
            BOT_REVOKED,
            "the owner revoked this bot",
        ));
    }
    let (action, space) = match reach {
        Reach::Own => return Ok(()),
        Reach::Outside => {
            return Err(Refusal::forbidden(
                BOT_GRANT,
                "the method is outside every bot grant",
            ))
        }
        Reach::Action { action, space } => (*action, space),
    };
    if grant.state == GrantState::Paused.as_str() {
        return Err(Refusal::forbidden(BOT_PAUSED, "the owner paused this bot"));
    }
    if grant.expires.is_some_and(|expires| now >= expires) {
        return Err(Refusal::forbidden(BOT_EXPIRED, "the grant expired"));
    }
    if !grant.can.iter().any(|a| a == action.as_str()) {
        return Err(Refusal::forbidden(
            NOT_IN_CAN,
            format!("the grant does not allow {}", action.as_str()),
        ));
    }
    if let Some(space) = space {
        let listed = |s: &str| grant.spaces.iter().any(|x| x == s);
        if !listed(space.text()) && !space_url.is_some_and(listed) {
            return Err(Refusal::forbidden(
                BOT_GRANT,
                format!("{} is not a space of the grant", space.text()),
            ));
        }
    }
    Ok(())
}

/// The log's half: the bot's key listed with the bot mark, valid, and `action` in the scope.
pub fn decide_key(
    key: Result<BotKey, BotKeyError>,
    action: BotAction,
    now: u64,
) -> Result<(), Refusal> {
    let key = match key {
        Ok(key) => key,
        Err(BotKeyError::NotListed) => {
            return Err(Refusal::forbidden(
                BOT_UNBOUND,
                "the owner's log does not list this bot's key",
            ))
        }
        Err(BotKeyError::StoreUnavailable) => {
            return Err(Refusal::new(
                "store-unavailable",
                503,
                "the owner's log could not be read",
            ))
        }
    };
    let Some(scope) = key.scope else {
        return Err(Refusal::forbidden(
            BOT_UNBOUND,
            "the owner's log lists this key without the bot mark",
        ));
    };
    match key.verdict {
        KeyVerdict::Valid => {}
        KeyVerdict::Revoked | KeyVerdict::NotInLog => {
            return Err(Refusal::forbidden(
                BOT_REVOKED,
                "the owner's log revoked this bot's key",
            ))
        }
        KeyVerdict::Expired => {
            return Err(Refusal::forbidden(BOT_EXPIRED, "the bot's key expired"));
        }
    }
    check_scope(&scope, action, now)
}

fn check_scope(scope: &BotScope, action: BotAction, now: u64) -> Result<(), Refusal> {
    if now >= scope.expires {
        return Err(Refusal::forbidden(BOT_EXPIRED, "the bot's key expired"));
    }
    if !scope.can.iter().any(|a| a == action.as_str()) {
        return Err(Refusal::forbidden(
            NOT_IN_CAN,
            format!("the owner's log does not allow {}", action.as_str()),
        ));
    }
    Ok(())
}

/// The reason a write or a join must give.
pub fn decide_reason(action: BotAction, reason: Option<&str>) -> Result<(), Refusal> {
    if action != BotAction::Read && reason.is_none_or(|r| r.trim().is_empty()) {
        return Err(Refusal::new(
            REASON_REQUIRED,
            400,
            "a bot's write gives meta.reason",
        ));
    }
    Ok(())
}

/// The bots of this executor, by agent DID: read on every call of a managed session, so
/// kept in memory and written through to the database.
static BOTS: LazyLock<RwLock<HashMap<String, BotRow>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// The bot whose agent is `did`, if it is one.
pub fn registered(did: &str) -> Option<BotRow> {
    BOTS.read().ok()?.get(did).cloned()
}

/// Whether [`init`] has loaded the registry. Until it has, a managed agent's session cannot be
/// told a bot from a user, so [`classify`] refuses it rather than serving a bot with a user's
/// authority (fail closed). Tests start loaded: they put bots in with [`remember`].
static LOADED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(cfg!(test));

/// The registry did not load, so no managed agent's session is served.
pub const BOTS_UNAVAILABLE: &str = "bots-unavailable";

/// What a managed agent `did` is: a bot (its row), or not one. `Err` for every managed agent
/// while the registry has not loaded: a bot whose row cannot be read must not pass as a user.
pub fn classify(did: &str) -> Result<Option<BotRow>, WsRpcError> {
    if !LOADED.load(std::sync::atomic::Ordering::SeqCst) {
        return Err(WsRpcError {
            code: 503,
            message: format!(
                "{BOTS_UNAVAILABLE}: the bot registry did not load, so no managed agent's \
                 session is served until the executor restarts"
            ),
        });
    }
    Ok(registered(did))
}

#[cfg(test)]
pub(crate) fn set_loaded(loaded: bool) {
    LOADED.store(loaded, std::sync::atomic::Ordering::SeqCst);
}

/// Keeps `bot` (new, or with a changed grant) in the registry.
pub(crate) fn remember(bot: BotRow) {
    if let Ok(mut bots) = BOTS.write() {
        bots.insert(bot.did.clone(), bot);
    }
}

/// Loads the bots from the database and starts the activity log's sweep. Runs once the
/// database is open, before the API serves a call.
pub fn init() -> Result<(), String> {
    let rows = crate::db_backend::db_backend()
        .bots()
        .map_err(|e| format!("reading the bots: {e}"))?;
    let count = rows.len();
    for row in rows {
        remember(row);
    }
    LOADED.store(true, std::sync::atomic::Ordering::SeqCst);
    if count > 0 {
        ::log::info!("bots: {count} bot agent(s)");
    }
    tokio::spawn(log::run_sweep());
    Ok(())
}

tokio::task_local! {
    /// The `reason` argument of the MCP tool call running in this task.
    pub static MCP_REASON: Option<String>;
}

/// `meta.reason` of a request frame, cut to [`REASON_MAX`] characters.
pub fn reason_of(frame: &Value) -> Option<String> {
    frame
        .get("meta")
        .and_then(|meta| meta.get("reason"))
        .and_then(Value::as_str)
        .map(cut_reason)
}

/// `reason` cut to [`REASON_MAX`] characters.
pub fn cut_reason(reason: &str) -> String {
    reason.chars().take(REASON_MAX).collect()
}

/// The check of every call from a bot's session (`ctx` acts as the bot `bot`): `Ok` or the
/// refusal. With `record`, a write, a join and a refusal go to the activity log (the
/// dispatcher records; a check without the call does not).
pub async fn admit(
    bot: &BotRow,
    method: &str,
    guard: &Guard,
    params: &Value,
    ctx: &RequestContext,
    record: bool,
) -> Result<(), WsRpcError> {
    let reach = reach(method, guard, params);
    let decided = decide(bot, &reach, ctx.reason.as_deref()).await;
    if record {
        // Allowed reads and calls on its own session are not logged; writes, joins and
        // refusals are.
        let logged = !matches!(
            (&reach, &decided),
            (Reach::Own, Ok(()))
                | (
                    Reach::Action {
                        action: BotAction::Read,
                        ..
                    },
                    Ok(()),
                )
        );
        if logged {
            log::record(bot, method, &reach, ctx.reason.as_deref(), &decided);
        }
    }
    decided.map_err(|refusal| refusal.error())
}

/// The check of `events.watch` / `events.unwatch` from a bot's session, which the connection
/// answers before the handler map sees it (`api/ws_rpc.rs`), so [`admit`] never does. Each
/// perspective a watch names is a read in that space, decided like a call and logged when
/// refused. A watch of every perspective (`null`) is refused: a grant names its spaces, and
/// `link-added` of a space outside it is a read there. `events.unwatch` is always allowed.
/// What this does not do: close a stream already open when the grant later changes.
pub async fn admit_watch(
    bot: &BotRow,
    method: &str,
    params: &Value,
    ctx: &RequestContext,
) -> Result<(), WsRpcError> {
    if method != crate::api::event_interest::WATCH {
        return Ok(());
    }
    // A frame that is no object gets the handler's 400.
    let Some(types) = params.as_object() else {
        return Ok(());
    };
    let reason = ctx.reason.as_deref();
    for wanted in types.values() {
        let items: Vec<Option<&str>> = match wanted {
            Value::Null => vec![None],
            Value::Array(items) => items.iter().map(Value::as_str).collect(),
            _ => continue,
        };
        for item in items {
            let reach = Reach::Action {
                action: BotAction::Read,
                space: item.map(|uuid| Space::Perspective(uuid.to_string())),
            };
            let decided = match item {
                None => Err(Refusal::forbidden(
                    BOT_GRANT,
                    "a bot watches the perspectives of its grant, not every perspective",
                )),
                Some(_) => decide(bot, &reach, reason).await,
            };
            if let Err(refusal) = decided {
                log::record(bot, method, &reach, reason, &Err(refusal.clone()));
                return Err(refusal.error());
            }
        }
    }
    Ok(())
}

async fn decide(bot: &BotRow, reach: &Reach, reason: Option<&str>) -> Result<(), Refusal> {
    let now = now();
    let space_url = match reach {
        Reach::Action {
            space: Some(Space::Perspective(uuid)),
            ..
        } => match crate::perspectives::get_perspective(uuid) {
            Some(perspective) => perspective.persisted.lock().await.shared_url.clone(),
            None => None,
        },
        _ => None,
    };
    decide_grant(bot, reach, space_url.as_deref(), now)?;
    let Reach::Action { action, .. } = reach else {
        return Ok(());
    };
    let key = match crate::identity::service() {
        Ok(identity) => {
            let key = bot.did.strip_prefix("did:key:").unwrap_or(&bot.did);
            identity.bot_key(&bot.owner, key).await
        }
        Err(_) => Err(BotKeyError::StoreUnavailable),
    };
    decide_key(key, *action, now)?;
    decide_reason(*action, reason)
}

#[cfg(test)]
mod tests;
