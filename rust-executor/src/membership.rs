//! The membership of a space on this executor (AR-08 section 4.5.1, slice 3.B part A3).
//!
//! Each perspective's store holds a [`SpaceMembership`]: the replay
//! ([`ad4m_identity_core::membership::MembershipGraph`]) of the perspective's membership links
//! (`coasys://membership/…` in the default graph), rebuilt from the store when it opens.
//!
//! - **Owner gate (R18).** A local write of a membership link goes through [`SpaceMembership::gate`]
//!   first ([`crate::perspectives::perspective_instance::PerspectiveInstance`]'s `write_link_diff`):
//!   the link is judged as the store judges it and applied to a copy of the graph; a refused
//!   link refuses the whole diff with the replay's [`Refusal`]. A diff that passes installs the
//!   new graph and queues what changed.
//! - **Read filter (R15).** In a governed space (one whose `rules` link counts), a Shared link
//!   that is not a membership link and whose verdict is `valid` is stored as `notMember` unless
//!   its person (a bot's link: its bot key) is a current member ([`SpaceMembership::overlay`]).
//!   `notMember` is neither shown nor counted. After each change the perspective judges again
//!   the links of the persons it names and every `notMember` link
//!   (`perspectives::Recheck::Members`), so a removal hides a person's links and a rejoin shows
//!   them again.
//! - Membership links never end by a diff (R12, `perspectives::monotonic`), and one in a
//!   remote diff is dropped (R13, `perspectives::ingest`): until the membership sync (A7) they
//!   reach a space only by a local write.
//!
//! The creator, until the space template names one (A8), is the person of the first counted
//! `rules` link this store holds.

use std::collections::BTreeSet;
use std::sync::{Mutex, PoisonError};

use ad4m_identity_core::membership::{
    is_membership_predicate, predicate, AddRule, InviteState, Join, JudgedLink, Member,
    MembershipChange, MembershipGraph, Refusal, Role, INVITE_PREFIX, SPACE,
};
use chrono::{DateTime, SecondsFormat, Utc};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::identity::resolver::CheckedLink;
use crate::identity::verdict::LinkVerdict;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkStatus};

/// An expiry far enough away to stand for none: an invite link needs one, and
/// `neighbourhood.createInvite` without `expiresAt` writes this (JavaScript's
/// `Number.MAX_SAFE_INTEGER`). [`InviteInfo`] leaves it out.
pub const NO_EXPIRY: u64 = 9_007_199_254_740_991;

/// The role of a member, as the SDK reads it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum MemberRole {
    Admin,
    Member,
    Bot,
}

impl From<Role> for MemberRole {
    fn from(role: Role) -> Self {
        match role {
            Role::Admin => MemberRole::Admin,
            Role::Member => MemberRole::Member,
            Role::Bot => MemberRole::Bot,
        }
    }
}

/// How a person who is not yet named gets in: `open` (anyone may join) or `invite`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum JoinMode {
    Open,
    Invite,
}

impl From<Join> for JoinMode {
    fn from(join: Join) -> Self {
        match join {
            Join::Open => JoinMode::Open,
            Join::Invite => JoinMode::Invite,
        }
    }
}

impl JoinMode {
    /// The target of a `join` link that sets this mode.
    fn target(self) -> &'static str {
        match self {
            JoinMode::Open => "literal:string:open",
            JoinMode::Invite => "literal:string:invite",
        }
    }
}

/// A current member (`neighbourhood.members`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct MemberInfo {
    /// The person's DID, or the bot's `did:key`.
    pub person: String,
    pub role: MemberRole,
    /// The owner of a bot.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub bot_of: Option<String>,
    /// When the link that admitted them was signed (ISO 8601).
    pub since: String,
}

impl From<&Member> for MemberInfo {
    fn from(member: &Member) -> Self {
        MemberInfo {
            person: member.person.clone(),
            role: member.role.into(),
            bot_of: member.bot_of.clone(),
            since: iso(member.since),
        }
    }
}

/// An invite of the space (`neighbourhood.invites`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InviteInfo {
    /// `invite://<id>`.
    pub invite: String,
    /// When it expires (ISO 8601); absent for an invite made without an expiry.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub expires_at: Option<String>,
    /// The person whose join used it.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub used_by: Option<String>,
}

impl From<&InviteState> for InviteInfo {
    fn from(invite: &InviteState) -> Self {
        InviteInfo {
            invite: invite.invite.clone(),
            expires_at: (invite.expires_at != NO_EXPIRY).then(|| iso(invite.expires_at)),
            used_by: invite.used_by.clone(),
        }
    }
}

/// What one counted membership link changed, as `neighbourhood-membership-changed` carries it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(tag = "kind", rename_all = "camelCase")]
#[ts(export)]
pub enum MembershipChangeInfo {
    /// A person or bot became a member.
    #[serde(rename_all = "camelCase")]
    Joined { member: MemberInfo },
    /// A membership ended: `by` is whoever's link ended it, `at` when (ISO 8601).
    #[serde(rename_all = "camelCase")]
    Removed {
        person: String,
        by: String,
        at: String,
    },
    /// A current member's role changed.
    #[serde(rename_all = "camelCase")]
    RoleChanged { member: MemberInfo },
    /// The join mode changed.
    #[serde(rename_all = "camelCase")]
    JoinChanged { join: JoinMode },
}

impl From<&MembershipChange> for MembershipChangeInfo {
    fn from(change: &MembershipChange) -> Self {
        match change {
            MembershipChange::Joined(member) => MembershipChangeInfo::Joined {
                member: member.into(),
            },
            MembershipChange::Removed { person, by, at } => MembershipChangeInfo::Removed {
                person: person.clone(),
                by: by.clone(),
                at: iso(*at),
            },
            MembershipChange::RoleChanged(member) => MembershipChangeInfo::RoleChanged {
                member: member.into(),
            },
            MembershipChange::JoinChanged { join } => MembershipChangeInfo::JoinChanged {
                join: (*join).into(),
            },
        }
    }
}

/// `neighbourhood-membership-changed`: what the membership links a write counted changed.
#[derive(Clone, Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodMembershipChangedEvent {
    pub perspective_uuid: String,
    pub changes: Vec<MembershipChangeInfo>,
}

/// Why `neighbourhood.join` did not let the caller in.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(export)]
pub enum JoinRefusalReason {
    /// The space does not admit the person (an admin removed them, or the join counts for
    /// nothing).
    NotAMember,
    /// The space is invite only and the join names no invite that exists.
    InviteOnly,
    /// The joining key is a device's or a bot's, with no identity of its own.
    NeedsIdentity,
    InviteExpired,
    InviteUsed,
    /// The neighbourhood's server refused the join.
    ServerRefused,
}

impl From<Refusal> for JoinRefusalReason {
    fn from(refusal: Refusal) -> Self {
        match refusal {
            Refusal::NeedsIdentity => JoinRefusalReason::NeedsIdentity,
            Refusal::InviteOnly => JoinRefusalReason::InviteOnly,
            Refusal::InviteExpired => JoinRefusalReason::InviteExpired,
            Refusal::InviteUsed => JoinRefusalReason::InviteUsed,
            _ => JoinRefusalReason::NotAMember,
        }
    }
}

/// `neighbourhood-join-refused`: the join of `url` left no perspective behind.
#[derive(Clone, Debug, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodJoinRefusedEvent {
    pub url: String,
    pub reason: JoinRefusalReason,
}

/// A membership link refused by the owner gate: the replay's [`Refusal`], as an error a write
/// returns.
#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
#[error("membership link refused: {0}")]
pub struct MembershipRefused(pub Refusal);

/// `ms` since the epoch as ISO 8601 (an out-of-range value reads as the epoch).
pub fn iso(ms: u64) -> String {
    i64::try_from(ms)
        .ok()
        .and_then(DateTime::<Utc>::from_timestamp_millis)
        .unwrap_or_default()
        .to_rfc3339_opts(SecondsFormat::Millis, true)
}

/// An ISO 8601 / RFC 3339 instant in ms since the epoch; `None` for one before it or unparsable.
pub fn ms_of(timestamp: &str) -> Option<u64> {
    DateTime::parse_from_rfc3339(timestamp)
        .ok()
        .and_then(|t| u64::try_from(t.timestamp_millis()).ok())
}

/// `link` with its judgement, as the replay takes it; `None` when its timestamp does not parse.
pub fn judged(link: &LinkExpression, checked: &CheckedLink) -> Option<JudgedLink> {
    Some(JudgedLink {
        hash: crate::perspectives::sparql_store::link_hash(link),
        source: link.data.source.clone(),
        predicate: link.data.predicate.clone().unwrap_or_default(),
        target: link.data.target.clone(),
        author_key: link.author.clone(),
        timestamp: ms_of(&link.timestamp)?,
        verdict: checked.verdict.clone(),
        person: checked.person.clone(),
        by_bot: checked.by_bot,
    })
}

/// The person whose rights `link` exercises: none for a bot's key or a key without one.
fn speaker(link: &JudgedLink) -> Option<&str> {
    (!link.by_bot).then_some(link.person.as_deref()).flatten()
}

#[derive(Default)]
struct State {
    /// Every membership link this store holds that has a judgement to replay, in no order.
    links: Vec<JudgedLink>,
    /// Their replay; `None` until a `rules` link names the creator.
    graph: Option<MembershipGraph>,
    /// Changes not yet published ([`SpaceMembership::take_changes`]).
    pending: Vec<MembershipChange>,
    /// The space turned governed (its first `rules` link) since the last take: every valid
    /// link is judged again.
    governed_now: bool,
}

/// The creator a set of membership links names: the speaker of their first counted `rules`
/// link, in replay order.
fn creator_of(links: &[JudgedLink]) -> Option<String> {
    links
        .iter()
        .filter(|l| l.predicate == predicate::RULES && l.verdict.counts())
        .filter_map(|l| speaker(l).map(|s| ((l.timestamp, l.hash), s)))
        .min_by_key(|(order, _)| *order)
        .map(|(_, s)| s.to_string())
}

/// The replay of `links` under the creator they name.
fn replay(links: &[JudgedLink]) -> Option<MembershipGraph> {
    let creator = creator_of(links)?;
    Some(MembershipGraph::replay(&creator, links).0)
}

/// The membership of one space: shared by a perspective's store and the perspective.
#[derive(Default)]
pub struct SpaceMembership {
    state: Mutex<State>,
}

impl SpaceMembership {
    pub fn new() -> Self {
        Self::default()
    }

    fn state(&self) -> std::sync::MutexGuard<'_, State> {
        self.state.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// The graph now; `None` before a `rules` link counts.
    pub fn graph(&self) -> Option<MembershipGraph> {
        self.state().graph.clone()
    }

    /// Whether the space's membership governs who its links count for: its `rules` counted.
    pub fn governed(&self) -> bool {
        self.state()
            .graph
            .as_ref()
            .is_some_and(|g| g.rules().is_some())
    }

    /// Replaces the state with the replay of the membership links `stored` holds, with the
    /// verdicts and persons stored on them. No change is queued: nothing new happened.
    pub fn rebuild(&self, stored: &[DecoratedLinkExpression]) {
        let links: Vec<JudgedLink> = stored
            .iter()
            .filter(|l| {
                l.data
                    .predicate
                    .as_deref()
                    .is_some_and(is_membership_predicate)
            })
            .filter_map(|l| {
                let checked = CheckedLink {
                    verdict: l.verdict.clone().unwrap_or(LinkVerdict::CantCheck),
                    person: l.person.clone(),
                    by_bot: l.by_bot.unwrap_or(false),
                };
                judged(&LinkExpression::from(l.clone()), &checked)
            })
            .collect();
        let graph = replay(&links);
        let mut state = self.state();
        state.links = links;
        state.graph = graph;
        state.pending.clear();
        state.governed_now = false;
    }

    /// The owner gate (R18): judges each membership link among `additions` (`own` as the store
    /// takes it) and applies it, in order, to a copy of the graph. The first one the replay
    /// refuses refuses them all, and nothing changes. Otherwise the copy becomes the graph and
    /// what changed is queued. Additions that are not membership links pass untouched.
    ///
    /// Before the space has rules, the first `rules` link names the creator: its speaker.
    pub fn gate(&self, additions: &[LinkExpression], own: bool) -> Result<(), MembershipRefused> {
        let incoming: Vec<&LinkExpression> = additions
            .iter()
            .filter(|l| {
                l.data
                    .predicate
                    .as_deref()
                    .is_some_and(is_membership_predicate)
            })
            .collect();
        if incoming.is_empty() {
            return Ok(());
        }
        let mut state = self.state();
        let mut links = state.links.clone();
        let mut graph = state.graph.clone();
        let was_governed = graph.as_ref().is_some_and(|g| g.rules().is_some());
        let mut changes = Vec::new();
        for link in incoming {
            let checked = if own {
                link.check_own_entered(None)
            } else {
                link.check_entered(None)
            };
            let judged = judged(link, &checked).ok_or(MembershipRefused(Refusal::Malformed))?;
            let applying = match graph.as_mut() {
                Some(graph) => graph,
                None => {
                    // No rules yet: the replay of a space without a creator, which a `rules`
                    // link's speaker becomes.
                    let creator = match judged.predicate.as_str() {
                        predicate::RULES => speaker(&judged).unwrap_or_default(),
                        _ => "",
                    };
                    graph.insert(MembershipGraph::new(creator))
                }
            };
            changes.extend(applying.apply(&judged).map_err(MembershipRefused)?);
            links.push(judged);
        }
        let graph = graph.filter(|g| g.rules().is_some());
        let governed = graph.is_some();
        state.links = links;
        state.graph = graph;
        state.pending.extend(changes);
        state.governed_now |= governed && !was_governed;
        Ok(())
    }

    /// The changes queued since the last call, and whether the space turned governed since.
    pub fn take_changes(&self) -> (Vec<MembershipChange>, bool) {
        let mut state = self.state();
        let governed_now = std::mem::take(&mut state.governed_now);
        (std::mem::take(&mut state.pending), governed_now)
    }

    /// The read filter (R15): `checked` as the store writes it for `link` under `status`. A
    /// Shared `valid` link that is not a membership link, in a governed space, by a person who
    /// is not a current member (a bot's link: by a bot key that is not), is `notMember`, with
    /// no person.
    pub fn overlay(
        &self,
        link: &LinkExpression,
        status: Option<&LinkStatus>,
        checked: CheckedLink,
    ) -> CheckedLink {
        if checked.verdict != LinkVerdict::Valid
            || status != Some(&LinkStatus::Shared)
            || link
                .data
                .predicate
                .as_deref()
                .is_some_and(is_membership_predicate)
        {
            return checked;
        }
        let state = self.state();
        let Some(graph) = state.graph.as_ref().filter(|g| g.rules().is_some()) else {
            return checked;
        };
        let member = if checked.by_bot {
            graph.is_member(&link.author)
        } else {
            checked
                .person
                .as_deref()
                .is_some_and(|p| graph.is_member(p))
        };
        if member {
            checked
        } else {
            CheckedLink {
                verdict: LinkVerdict::NotMember,
                person: None,
                by_bot: false,
            }
        }
    }

    /// The current members.
    pub fn members(&self) -> Vec<MemberInfo> {
        self.state()
            .graph
            .as_ref()
            .map(|g| {
                g.members()
                    .iter()
                    .filter(|m| m.until.is_none())
                    .map(MemberInfo::from)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The counted invites.
    pub fn invites(&self) -> Vec<InviteInfo> {
        self.state()
            .graph
            .as_ref()
            .map(|g| g.invites().iter().map(InviteInfo::from).collect())
            .unwrap_or_default()
    }
}

/// The persons (and bot keys) whose links a change can turn on or off.
pub fn changed_persons(changes: &[MembershipChange]) -> BTreeSet<String> {
    changes
        .iter()
        .filter_map(|c| match c {
            MembershipChange::Joined(m) | MembershipChange::RoleChanged(m) => {
                Some(m.person.clone())
            }
            MembershipChange::Removed { person, .. } => Some(person.clone()),
            MembershipChange::JoinChanged { .. } => None,
        })
        .collect()
}

/// The membership links each `neighbourhood.*` call writes. Each is signed by the caller.
pub mod links {
    use super::*;

    fn link(source: &str, predicate: &str, target: &str) -> Link {
        Link {
            source: source.to_string(),
            predicate: Some(predicate.to_string()),
            target: target.to_string(),
        }
    }

    /// `literal:json:<RFC 3986-encoded JSON>`, as the SDK's `Literal` writes it.
    fn json_literal(value: &serde_json::Value) -> String {
        format!("literal:json:{}", urlencoding::encode(&value.to_string()))
    }

    /// The space's rules: who adds members, and the join mode it starts with.
    pub fn rules(add: AddRule, join: JoinMode) -> Link {
        let add = match add {
            AddRule::Admins => "admins",
            AddRule::Members => "members",
        };
        let join = match join {
            JoinMode::Open => "open",
            JoinMode::Invite => "invite",
        };
        link(
            SPACE,
            predicate::RULES,
            &json_literal(&serde_json::json!({ "add": add, "join": join })),
        )
    }

    /// `neighbourhood.addMember`: `admin` for an admin, else `member`.
    pub fn add_member(person: &str, role: MemberRole) -> Result<Link, String> {
        match role {
            MemberRole::Admin => Ok(link(person, predicate::ADMIN, SPACE)),
            MemberRole::Member => Ok(link(person, predicate::MEMBER, SPACE)),
            MemberRole::Bot => Err("a bot is added with neighbourhood.addBot".to_string()),
        }
    }

    /// A self-join: through `invite` when it names one.
    pub fn self_join(person: &str, invite: Option<&str>) -> Link {
        link(person, predicate::MEMBER, invite.unwrap_or(SPACE))
    }

    /// `neighbourhood.removeMember`, `leave` and `removeBot`.
    pub fn removed(person: &str) -> Link {
        link(person, predicate::REMOVED, SPACE)
    }

    /// `neighbourhood.createInvite`: a new `invite://<id>` and the link that makes it.
    pub fn invite(expires_at: u64) -> (String, Link) {
        let invite = format!("{INVITE_PREFIX}{}", uuid::Uuid::new_v4());
        let target = json_literal(&serde_json::json!({ "expiresAt": expires_at }));
        let made = link(&invite, predicate::INVITE, &target);
        (invite, made)
    }

    /// `neighbourhood.setJoinMode`.
    pub fn join_mode(join: JoinMode) -> Link {
        link(SPACE, predicate::JOIN, join.target())
    }

    /// `neighbourhood.addBot`: the `bot` link and the owner's `approved-by`.
    pub fn add_bot(bot: &str, owner: &str) -> [Link; 2] {
        [
            link(bot, predicate::BOT, SPACE),
            link(bot, predicate::APPROVED_BY, owner),
        ]
    }
}

/// The deep link of an invite: `coasys://join?url=<neighbourhood url>&invite=<invite>`.
pub fn invite_url(neighbourhood_url: &str, invite: &str) -> String {
    format!(
        "coasys://join?url={}&invite={}",
        urlencoding::encode(neighbourhood_url),
        urlencoding::encode(invite)
    )
}

/// The judgement a link the caller signs now gets ([`crate::identity::check_signed_own`]):
/// whose person its links speak for, and whether its key is a bot's.
pub async fn caller(
    ctx: &crate::agent::AgentContext,
) -> Result<CheckedLink, deno_core::error::AnyError> {
    let probe = Link {
        source: SPACE.to_string(),
        predicate: None,
        target: SPACE.to_string(),
    };
    let link = LinkExpression::from(crate::agent::create_signed_link(probe, ctx)?);
    crate::identity::prepare_link_signers(std::slice::from_ref(&link)).await;
    Ok(link.check_own_entered(None))
}

/// The person `caller` speaks for: [`Refusal::NeedsIdentity`] for a bot key or a key with
/// no identity.
pub fn person_of(caller: &CheckedLink) -> Result<&str, Refusal> {
    match (&caller.person, caller.by_bot) {
        (Some(person), false) => Ok(person),
        _ => Err(Refusal::NeedsIdentity),
    }
}

/// What `neighbourhood.join` does in a space it just installed, for `caller` naming `invite`
/// at `now` (ms): the self-join link to write, `None` when there is none to write (the
/// caller is a member already, or the space has no rules here yet), or why the space does
/// not admit the caller.
pub fn join_link(
    membership: &SpaceMembership,
    caller: &CheckedLink,
    invite: Option<&str>,
    now: u64,
) -> Result<Option<Link>, Refusal> {
    let Some(graph) = membership.graph() else {
        return Ok(None);
    };
    let person = person_of(caller)?;
    graph.admits(Some(person), false, invite, now)?;
    if graph.is_member(person) {
        return Ok(None);
    }
    Ok(Some(links::self_join(person, invite)))
}

/// The refusal behind a write the owner gate refused, if that is why it failed.
pub fn refusal_of(error: &deno_core::error::AnyError) -> Option<Refusal> {
    error.downcast_ref::<MembershipRefused>().map(|r| r.0)
}

#[cfg(test)]
mod tests;
