//! Spaces by person (slice 3.B part A8): making a space, the link language it syncs through,
//! and the spaces a person holds.
//!
//! - **`neighbourhood.create`** ([`create`]) makes a space of a perspective: it templates the
//!   server-link language with `SERVER_URL` (the chosen link server), `UID` (a random salt)
//!   and `CREATOR` (the caller's person), loads it (loading it first claims the room: the
//!   creator's genesis makes it), writes the creator's `rules` link (`join: open` unless the
//!   caller says otherwise, R56; it makes the creator the first admin, so no `admin` link is
//!   written: [`membership::links::genesis`]), then publishes the neighbourhood. The
//!   commit after publishing carries the membership links to the room's `/membership` with the
//!   genesis. The [`LinkLanguageChoice`] is recorded once (R3): a second `create` on the same
//!   perspective fails. Holochain is not offered yet: `coming-soon`.
//! - **`neighbourhood.publish`** refuses the server-link template and every language made
//!   from it ([`refuse_creator_template`]): its `CREATOR` must be the caller's person, which
//!   only `create` fills in.
//! - **`neighbourhood.linkLanguageChoice`** ([`link_language_choice`]): the recorded choice,
//!   or, for a space this executor joined, the one its link language's template parameters
//!   name, recorded on first read.
//! - **`spaces.list`** ([`list`]): the caller's holdings, each `made` (the caller's person is
//!   the creator) or `joined`. `joinedAsGuest` is not produced yet.

use std::collections::HashSet;
use std::sync::{LazyLock, Mutex, PoisonError};

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use sha2::{Digest, Sha256};
use ts_rs::TS;

use crate::agent::AgentContext;
use crate::languages::LanguageController;
use crate::link_servers::{self, LinkServerDirectory};
use crate::membership::{self, JoinMode, MemberRole};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{LanguageMeta, LinkStatus, Perspective, PerspectiveHandle, PerspectiveState};

/// The template parameter that names the space's creator (a person's DID).
pub const CREATOR_PARAM: &str = "CREATOR";
/// The template parameter that names the link server.
pub const SERVER_URL_PARAM: &str = "SERVER_URL";
/// The template parameter that salts the room id.
pub const UID_PARAM: &str = "UID";

/// How a new space syncs.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(tag = "kind", rename_all = "camelCase")]
#[ts(export)]
pub enum SyncChoice {
    /// The first link server of the directory (`runtime.linkServers`).
    Default,
    /// A link server the person names; checked like `link_server.url` (protocol 2 or later).
    OwnLinkServer { url: String },
    /// Holochain: not offered yet (`coming-soon`).
    Holochain,
}

/// What a space's link language syncs through.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum ChoiceKind {
    LinkServer,
    Holochain,
}

/// The link language a space syncs through, fixed when the space is made (R3).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkLanguageChoice {
    pub kind: ChoiceKind,
    /// The address of the space's link language.
    pub link_language: String,
    /// The link server (`linkServer` only).
    pub server_url: Option<String>,
    /// The room on the link server: `base58(sha256(CREATOR ‖ 0x00 ‖ UID))` (`linkServer` only).
    pub room_id: Option<String>,
    /// The person who made the space (a Holochain space: the neighbourhood's author).
    pub creator: String,
    /// When the choice was fixed (ISO 8601).
    pub fixed_at: String,
}

/// Who adds members to a new space.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum SpaceAddRule {
    /// Only admins add members.
    #[default]
    Admins,
    /// Any member adds members.
    Members,
}

impl From<SpaceAddRule> for ad4m_identity_core::membership::AddRule {
    fn from(rule: SpaceAddRule) -> Self {
        match rule {
            SpaceAddRule::Admins => ad4m_identity_core::membership::AddRule::Admins,
            SpaceAddRule::Members => ad4m_identity_core::membership::AddRule::Members,
        }
    }
}

/// The rules a new space starts with: its `rules` link.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RulesInput {
    pub add: SpaceAddRule,
    pub join: JoinMode,
}

impl Default for RulesInput {
    /// Admins add members; anyone may join (R56).
    fn default() -> Self {
        RulesInput {
            add: SpaceAddRule::Admins,
            join: JoinMode::Open,
        }
    }
}

/// `neighbourhood.create` params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateNeighbourhoodParams {
    /// The perspective that becomes the space.
    pub perspective_uuid: String,
    pub sync: SyncChoice,
    pub rules: RulesInput,
}

/// `neighbourhood.create` result.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateNeighbourhoodResult {
    /// The neighbourhood URL (`neighbourhood://…`).
    pub url: String,
    pub choice: LinkLanguageChoice,
}

/// `neighbourhood.linkLanguageChoice` params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct LinkLanguageChoiceParams {
    pub perspective_uuid: String,
}

/// How the caller holds a space.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum HoldingHow {
    /// The caller's person made it.
    Made,
    /// The caller joined it.
    Joined,
    /// The caller joined as a guest (not produced yet).
    JoinedAsGuest,
}

/// One space the caller holds (`spaces.list`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SpaceHolding {
    pub perspective_uuid: String,
    pub neighbourhood_url: String,
    /// The perspective's name, else the neighbourhood URL.
    pub name: String,
    pub how: HoldingHow,
    /// Made: when the choice was fixed. Joined: when the caller was admitted, else when the
    /// neighbourhood was published (ISO 8601).
    pub since: String,
    pub choice: LinkLanguageChoice,
    pub sync_state: PerspectiveState,
    /// The caller's role in the space; `null` when the space does not name the caller.
    pub my_role: Option<MemberRole>,
}

/// Why a space call failed. Each message starts with its code (`coming-soon: …`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpaceError {
    /// The sync choice is not offered yet (Holochain).
    ComingSoon(String),
    /// The caller speaks for no person (a bot key, or a key without an identity).
    NeedsIdentity(String),
    /// The perspective is a space already, or another `create` is making it one.
    AlreadyCreated(String),
    NotFound(String),
    /// The caller's params cannot be used (an own link server that is not one).
    Invalid(String),
    /// This executor cannot make the space now (no link server, no template).
    Unavailable(String),
    Failed(String),
}

impl std::fmt::Display for SpaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (code, message) = match self {
            SpaceError::ComingSoon(m) => ("coming-soon", m),
            SpaceError::NeedsIdentity(m) => ("needs-identity", m),
            SpaceError::AlreadyCreated(m) => ("already-created", m),
            SpaceError::NotFound(m) => ("not-found", m),
            SpaceError::Invalid(m) => ("invalid", m),
            SpaceError::Unavailable(m) => ("unavailable", m),
            SpaceError::Failed(m) => ("failed", m),
        };
        write!(f, "{code}: {message}")
    }
}

impl std::error::Error for SpaceError {}

/// The room a server-link space syncs through: `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`, as the
/// language and the link server compute it.
pub fn room_id_of(creator: &str, uid: &str) -> String {
    let mut hash = Sha256::new();
    hash.update(creator.as_bytes());
    hash.update([0u8]);
    hash.update(uid.as_bytes());
    bs58::encode(hash.finalize()).into_string()
}

/// The parameters a templated language was made with (its meta's `templateAppliedParams`).
fn applied_params(meta: &LanguageMeta) -> Map<String, Value> {
    meta.template_applied_params
        .as_deref()
        .and_then(|params| serde_json::from_str::<Map<String, Value>>(params).ok())
        .unwrap_or_default()
}

fn declares(meta: &LanguageMeta, param: &str) -> bool {
    meta.possible_template_params
        .as_ref()
        .is_some_and(|params| params.iter().any(|p| p == param))
}

/// Whether `meta` is the server-link template or a language made from it: one whose
/// `CREATOR` names a space's creator, so only `neighbourhood.create` may publish it.
pub fn is_creator_template(meta: &LanguageMeta) -> bool {
    declares(meta, CREATOR_PARAM) || applied_params(meta).contains_key(CREATOR_PARAM)
}

/// Whether `meta` is the server-link template itself (not templated yet).
fn is_server_link_template(meta: &LanguageMeta) -> bool {
    meta.template_applied_params.is_none()
        && [CREATOR_PARAM, SERVER_URL_PARAM, UID_PARAM]
            .iter()
            .all(|param| declares(meta, param))
}

/// `neighbourhood.publish`'s refusal: `link_language` is the server-link template or made from
/// it. A language whose meta cannot be read is not refused here (installing it fails instead).
pub async fn refuse_creator_template(link_language: &str) -> Result<(), String> {
    match LanguageController::global_instance()
        .get_language_expression(link_language)
        .await
    {
        Ok(meta) if is_creator_template(&meta) => Err(format!(
            "{link_language} is a server-link language: use neighbourhood.create, which fills in \
             its creator"
        )),
        Ok(_) => Ok(()),
        Err(e) => {
            log::debug!("publish: no meta for {link_language} ({e}); not refused here");
            Ok(())
        }
    }
}

/// The choice a space's link language names: a server-link language's template parameters
/// name its server, room and creator; any other is Holochain's, made by the neighbourhood's
/// `author`.
pub fn choice_from_meta(
    meta: &LanguageMeta,
    link_language: &str,
    author: &str,
    fixed_at: &str,
) -> LinkLanguageChoice {
    let params = applied_params(meta);
    let text = |key: &str| params.get(key).and_then(Value::as_str).map(str::to_string);
    match text(SERVER_URL_PARAM) {
        Some(server_url) => {
            let creator = text(CREATOR_PARAM).unwrap_or_else(|| author.to_string());
            let room_id = text(CREATOR_PARAM)
                .zip(text(UID_PARAM))
                .map(|(creator, uid)| room_id_of(&creator, &uid));
            LinkLanguageChoice {
                kind: ChoiceKind::LinkServer,
                link_language: link_language.to_string(),
                server_url: Some(server_url),
                room_id,
                creator,
                fixed_at: fixed_at.to_string(),
            }
        }
        None => LinkLanguageChoice {
            kind: ChoiceKind::Holochain,
            link_language: link_language.to_string(),
            server_url: None,
            room_id: None,
            creator: author.to_string(),
            fixed_at: fixed_at.to_string(),
        },
    }
}

fn now_iso() -> String {
    membership::iso(chrono::Utc::now().timestamp_millis().max(0) as u64)
}

fn recorded_choice(uuid: &str) -> Result<Option<LinkLanguageChoice>, SpaceError> {
    let stored = crate::db_backend::db_backend()
        .space_choice(uuid)
        .map_err(|e| SpaceError::Failed(format!("reading the space's choice: {e}")))?;
    stored
        .map(|json| {
            serde_json::from_str(&json)
                .map_err(|e| SpaceError::Failed(format!("the recorded choice of {uuid}: {e}")))
        })
        .transpose()
}

/// Records `choice` for `uuid` unless one is recorded: `false` when one was.
fn record_choice(uuid: &str, choice: &LinkLanguageChoice) -> Result<bool, SpaceError> {
    let json = serde_json::to_string(choice).map_err(|e| SpaceError::Failed(e.to_string()))?;
    crate::db_backend::db_backend()
        .record_space_choice(uuid, &json)
        .map_err(|e| SpaceError::Failed(format!("recording the space's choice: {e}")))
}

/// The perspectives a `create` is making a space of now: a second `create` on one of them
/// fails at once instead of making a second language and room.
static CREATING: LazyLock<Mutex<HashSet<String>>> = LazyLock::new(Default::default);

/// One perspective's place in [`CREATING`], given back when dropped (on success, failure or a
/// cancelled request).
struct CreateClaim(String);

impl CreateClaim {
    fn take(uuid: &str) -> Option<Self> {
        let mut creating = CREATING.lock().unwrap_or_else(PoisonError::into_inner);
        creating
            .insert(uuid.to_string())
            .then(|| CreateClaim(uuid.to_string()))
    }
}

impl Drop for CreateClaim {
    fn drop(&mut self) {
        CREATING
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .remove(&self.0);
    }
}

/// The server-link template among the link languages this executor knows (the seed's and
/// those added since).
async fn server_link_template() -> Result<String, SpaceError> {
    let known = {
        let service = crate::runtime_service::RuntimeService::global_instance();
        let service = service.lock().unwrap_or_else(PoisonError::into_inner);
        service
            .as_ref()
            .map(|s| s.get_know_link_languages())
            .unwrap_or_default()
    };
    let controller = LanguageController::global_instance();
    let metas = futures::future::join_all(
        known
            .iter()
            .map(|address| controller.get_language_expression(address)),
    )
    .await;
    known
        .into_iter()
        .zip(metas)
        .find_map(|(address, meta)| match meta {
            Ok(meta) if is_server_link_template(&meta) => Some(address),
            Ok(_) => None,
            Err(e) => {
                log::warn!("No meta for link language {address}: {e}");
                None
            }
        })
        .ok_or_else(|| {
            SpaceError::Unavailable(
                "this executor knows no server-link language template (its seed's \
                 knownLinkLanguages)"
                    .to_string(),
            )
        })
}

/// The link server a new space syncs through.
async fn chosen_server(sync: &SyncChoice) -> Result<String, SpaceError> {
    match sync {
        SyncChoice::Holochain => Err(coming_soon()),
        SyncChoice::Default => LinkServerDirectory::global()
            .default_entry()
            .map(|entry| entry.url.clone())
            .ok_or_else(|| {
                SpaceError::Unavailable(
                    "no link server: set link_server.url, or use a seed that names linkServers"
                        .to_string(),
                )
            }),
        SyncChoice::OwnLinkServer { url } => {
            link_servers::check_operator_url(url)
                .await
                .map_err(SpaceError::Invalid)?;
            link_servers::base_url(url).map_err(SpaceError::Invalid)
        }
    }
}

fn coming_soon() -> SpaceError {
    SpaceError::ComingSoon("Holochain spaces are not offered yet; choose a link server".to_string())
}

fn perspective_of(uuid: &str) -> Result<PerspectiveInstance, SpaceError> {
    crate::perspectives::get_perspective(uuid)
        .ok_or_else(|| SpaceError::NotFound(format!("Perspective not found: {uuid}")))
}

/// `neighbourhood.create`: makes a space of the perspective `params.perspective_uuid` as
/// `ctx`. See the module doc for the steps.
pub async fn create(
    params: CreateNeighbourhoodParams,
    ctx: &AgentContext,
) -> Result<CreateNeighbourhoodResult, SpaceError> {
    if params.sync == SyncChoice::Holochain {
        return Err(coming_soon());
    }
    let uuid = params.perspective_uuid.as_str();
    let mut perspective = perspective_of(uuid)?;
    let _claim = CreateClaim::take(uuid)
        .ok_or_else(|| SpaceError::AlreadyCreated(format!("{uuid} is being made a space now")))?;
    if recorded_choice(uuid)?.is_some() {
        return Err(SpaceError::AlreadyCreated(format!(
            "{uuid} is a space already: its link language is fixed"
        )));
    }
    if let Some(url) = perspective.persisted.lock().await.shared_url.clone() {
        return Err(SpaceError::AlreadyCreated(format!(
            "{uuid} is shared as {url} already"
        )));
    }

    let caller = membership::caller(ctx)
        .await
        .map_err(|e| SpaceError::Failed(e.to_string()))?;
    let person = membership::person_of(&caller)
        .map_err(|refusal| {
            SpaceError::NeedsIdentity(format!(
                "a space is made by a person: {refusal}; create an identity first"
            ))
        })?
        .to_string();

    let server_url = chosen_server(&params.sync).await?;
    let template = server_link_template().await?;
    let uid = bs58::encode(rand::random::<[u8; 32]>()).into_string();
    let mut template_data = Map::new();
    template_data.insert(SERVER_URL_PARAM.into(), Value::String(server_url.clone()));
    template_data.insert(UID_PARAM.into(), Value::String(uid.clone()));
    template_data.insert(CREATOR_PARAM.into(), Value::String(person.clone()));

    let controller = LanguageController::global_instance();
    let (link_language, _name) = controller
        .publish_templated(&template, template_data, ctx)
        .await
        .map_err(|e| SpaceError::Failed(format!("making the space's link language: {e}")))?;
    // Loaded before anything is shared: loading it is what claims the room.
    LanguageController::install_language(link_language.clone())
        .await
        .map_err(|e| SpaceError::Failed(format!("loading the space's link language: {e}")))?;

    perspective
        .add_links(
            membership::links::genesis(params.rules.add.into(), params.rules.join),
            LinkStatus::Shared,
            None,
            ctx,
        )
        .await
        .map_err(|e| SpaceError::Failed(format!("writing the space's rules: {e}")))?;

    let url = crate::neighbourhoods::neighbourhood_publish_from_perspective_with_context(
        uuid,
        link_language.clone(),
        Perspective::default(),
        ctx,
    )
    .await
    .map_err(|e| SpaceError::Failed(format!("publishing the neighbourhood: {e}")))?;

    let choice = LinkLanguageChoice {
        kind: ChoiceKind::LinkServer,
        link_language,
        server_url: Some(server_url),
        room_id: Some(room_id_of(&person, &uid)),
        creator: person,
        fixed_at: now_iso(),
    };
    record_choice(uuid, &choice)?;
    Ok(CreateNeighbourhoodResult { url, choice })
}

/// The choice of a handle's neighbourhood, from its link language's meta; `None` when the
/// perspective is not a neighbourhood or the meta cannot be read.
async fn derived_choice(handle: &PerspectiveHandle) -> Option<LinkLanguageChoice> {
    let neighbourhood = handle.neighbourhood.as_ref()?;
    let link_language = &neighbourhood.data.link_language;
    let meta = LanguageController::global_instance()
        .get_language_expression(link_language)
        .await
        .map_err(|e| log::warn!("No meta for link language {link_language}: {e}"))
        .ok()?;
    Some(choice_from_meta(
        &meta,
        link_language,
        &neighbourhood.author,
        &neighbourhood.timestamp,
    ))
}

/// The choice of `handle`'s space: the recorded one, else the one its link language names
/// (recorded now).
async fn choice_of(handle: &PerspectiveHandle) -> Result<Option<LinkLanguageChoice>, SpaceError> {
    if let Some(choice) = recorded_choice(&handle.uuid)? {
        return Ok(Some(choice));
    }
    let Some(choice) = derived_choice(handle).await else {
        return Ok(None);
    };
    record_choice(&handle.uuid, &choice)?;
    Ok(Some(choice))
}

/// `neighbourhood.linkLanguageChoice`: the choice of the space `uuid`; `None` for a
/// perspective that is not a space, or one whose link language's meta cannot be read.
pub async fn link_language_choice(uuid: &str) -> Result<Option<LinkLanguageChoice>, SpaceError> {
    let handle = perspective_of(uuid)?.persisted.lock().await.clone();
    choice_of(&handle).await
}

/// Who the caller is in a space: its person (when it has one) and its DID.
pub struct Holder {
    pub person: Option<String>,
    pub did: Option<String>,
}

impl Holder {
    pub async fn of(ctx: &AgentContext) -> Self {
        let person = match membership::caller(ctx).await {
            Ok(caller) => membership::person_of(&caller).ok().map(str::to_string),
            Err(e) => {
                log::warn!("spaces.list: cannot judge the caller's key: {e}");
                None
            }
        };
        Holder {
            person,
            did: crate::agent::did_for_context(ctx).ok(),
        }
    }

    fn is(&self, who: &str) -> bool {
        self.person.as_deref() == Some(who) || self.did.as_deref() == Some(who)
    }
}

/// The holding `handle` is for `holder`, given the space's `choice` and `members`.
pub fn holding(
    handle: &PerspectiveHandle,
    choice: LinkLanguageChoice,
    members: &[membership::MemberInfo],
    holder: &Holder,
) -> Option<SpaceHolding> {
    let neighbourhood_url = handle.shared_url.clone()?;
    let mine = members.iter().find(|m| holder.is(&m.person));
    let how = if holder.is(&choice.creator) {
        HoldingHow::Made
    } else {
        HoldingHow::Joined
    };
    let since = match (how, mine) {
        (HoldingHow::Made, _) => choice.fixed_at.clone(),
        (_, Some(member)) => member.since.clone(),
        (_, None) => handle
            .neighbourhood
            .as_ref()
            .map(|n| n.timestamp.clone())
            .unwrap_or_else(|| choice.fixed_at.clone()),
    };
    Some(SpaceHolding {
        perspective_uuid: handle.uuid.clone(),
        name: handle
            .name
            .clone()
            .filter(|n| !n.is_empty())
            .unwrap_or_else(|| neighbourhood_url.clone()),
        neighbourhood_url,
        how,
        since,
        choice,
        sync_state: handle.state.clone(),
        my_role: mine.map(|m| m.role),
    })
}

/// `spaces.list`: the spaces among `perspectives` (the ones the caller may read), as
/// `holder` holds them. A space whose choice cannot be read is left out, with a warning.
pub async fn list(perspectives: Vec<PerspectiveInstance>, holder: &Holder) -> Vec<SpaceHolding> {
    let mut holdings = Vec::new();
    for perspective in perspectives {
        let handle = perspective.persisted.lock().await.clone();
        if handle.shared_url.is_none() || handle.neighbourhood.is_none() {
            continue;
        }
        let choice = match choice_of(&handle).await {
            Ok(Some(choice)) => choice,
            Ok(None) => {
                log::warn!(
                    "spaces.list: {} has no readable link-language choice",
                    handle.uuid
                );
                continue;
            }
            Err(e) => {
                log::warn!("spaces.list: {}: {e}", handle.uuid);
                continue;
            }
        };
        let members = perspective.sparql_store.membership().members();
        holdings.extend(holding(&handle, choice, &members, holder));
    }
    holdings
}

#[cfg(test)]
#[path = "spaces_tests.rs"]
mod tests;
