//! The membership graph of a space (AR-08 section 4.5.1): who is a member and with which role,
//! and how a person who is not yet named gets in.
//!
//! Membership is a set of ordinary links in the space's default graph, each with a predicate
//! under [`MEMBERSHIP_PREFIX`]. The caller judges every link first (its [`LinkVerdict`], see
//! [`crate::judgement`]) and resolves its signing key to the derived person; a [`JudgedLink`]
//! carries both. [`MembershipGraph`] replays those links in `(timestamp, hash)` order and holds
//! the result. It does no I/O and reads no clock.
//!
//! The rows, with the link's source, target and who may write it:
//!
//! | Predicate | Source | Target | Counts when written by |
//! |---|---|---|---|
//! | `rules` | [`SPACE`] | `literal:json:{"add","guests","join"}` | the creator; the first only |
//! | `join` | [`SPACE`] | `literal:string:open` or `…:invite` | an admin |
//! | `admin` | person DID | [`SPACE`] | an admin |
//! | `member` | person DID | [`SPACE`] or `invite://<id>` | a holder of the add right, or the person (self-join) |
//! | `invite` | `invite://<id>` | `literal:json:{"expiresAt": ms}` | an admin |
//! | `removed` | person DID or bot `did:key` | [`SPACE`] | an admin, the person, or the bot's owner |
//! | `bot` | bot `did:key` | [`SPACE`] | a member, with a key that is not a bot's |
//! | `approved-by` | bot `did:key` | owner person DID | the owner, with a key that is not a bot's |
//!
//! The counted `rules` link makes the creator the first admin. A `rules` literal without `join`
//! reads as [`Join::Invite`], and so does a space before its `rules` link. A self-join counts in
//! an open space, or with an invite that exists, has not expired at the link's timestamp and no
//! other person used; it needs a person's own key (a device-only key or a bot's key never
//! joins). After an admin removed a person, the open rule does not admit that person again; a
//! person who left (wrote their own `removed`) may. A bot counts as a member once both its
//! `bot` and its `approved-by` link count; its `removed` clears both. A link signed by a bot
//! key never exercises its owner's rights. The `guest` row is slice 4: the replay reads no
//! predicate other than the eight above.

use std::collections::{BTreeMap, HashSet};

use percent_encoding::percent_decode_str;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::judgement::LinkVerdict;
use crate::DID_PREFIX;

/// The prefix of every membership predicate.
pub const MEMBERSHIP_PREFIX: &str = "coasys://membership/";
/// The node that stands for the space itself: the source of `rules` and `join`, the target of
/// `admin`, `member`, `removed` and `bot`.
pub const SPACE: &str = "coasys://membership/space";
/// The prefix of an invite: `invite://<id>`.
pub const INVITE_PREFIX: &str = "invite://";
/// The prefix of a device or bot key.
const DID_KEY_PREFIX: &str = "did:key:";

/// The membership predicates the replay reads.
pub mod predicate {
    /// The space's rules: who adds members and how a person not yet named gets in.
    pub const RULES: &str = "coasys://membership/rules";
    /// A change of the join mode.
    pub const JOIN: &str = "coasys://membership/join";
    /// A person made an admin.
    pub const ADMIN: &str = "coasys://membership/admin";
    /// A person added as a member, by another or by themselves.
    pub const MEMBER: &str = "coasys://membership/member";
    /// An invite and its expiry.
    pub const INVITE: &str = "coasys://membership/invite";
    /// A membership ended.
    pub const REMOVED: &str = "coasys://membership/removed";
    /// A bot proposed as a member.
    pub const BOT: &str = "coasys://membership/bot";
    /// The owner's approval of a bot.
    pub const APPROVED_BY: &str = "coasys://membership/approved-by";
}

/// Whether `p` is a membership predicate (any predicate under [`MEMBERSHIP_PREFIX`], including
/// those the replay does not read yet).
pub fn is_membership_predicate(p: &str) -> bool {
    p.starts_with(MEMBERSHIP_PREFIX)
}

/// The role of a member.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Role {
    /// Writes the join mode, admins and invites, and removes anyone.
    Admin,
    /// A member; adds others when the rules say [`AddRule::Members`].
    Member,
    /// A bot key, approved by its owner.
    Bot,
}

/// How a person who is not yet named gets in.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Join {
    /// Any person may join by writing their own `member` link.
    Open,
    /// Only with an invite, or when a holder of the add right adds them.
    Invite,
}

/// Who may add a member.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum AddRule {
    /// Admins only.
    Admins,
    /// Every member (not a bot).
    Members,
}

/// The rules of the creator's first `rules` link. An absent `add` reads as
/// [`AddRule::Admins`], an absent `join` as [`Join::Invite`]; `guests` is not read in this slice.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Rules {
    /// Who may add a member.
    pub add: AddRule,
    /// The join mode the space starts with.
    pub join: Join,
}

/// One membership: current while `until` is `None`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Member {
    /// The person's DID, or the bot's `did:key`.
    pub person: String,
    /// The role held.
    pub role: Role,
    /// The owner of a bot.
    pub bot_of: Option<String>,
    /// The timestamp (ms) of the link that admitted it.
    pub since: u64,
    /// The timestamp (ms) of the `removed` link that ended it.
    pub until: Option<u64>,
}

/// What one counted link changed.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum MembershipChange {
    /// A person or bot became a member.
    Joined(Member),
    /// A membership ended. `by` is the person (or bot key) whose link ended it, `at` its
    /// timestamp.
    Removed {
        /// The person or bot whose membership ended.
        person: String,
        /// Who ended it.
        by: String,
        /// When.
        at: u64,
    },
    /// A current member's role changed.
    RoleChanged(Member),
    /// The join mode changed.
    JoinChanged {
        /// The join mode from now on.
        join: Join,
    },
}

/// Why a link counts for nothing. A refused link changes nothing.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, thiserror::Error)]
#[serde(rename_all = "camelCase")]
pub enum Refusal {
    /// The link's verdict does not count ([`LinkVerdict::counts`]).
    #[error("the link's verdict does not count")]
    NotCounted,
    /// The source or target does not fit the row.
    #[error("the source or target does not fit the row")]
    Malformed,
    /// Set once already: the same link again, a second `rules` link or an invite id in use.
    #[error("already set")]
    Repeated,
    /// A `rules` link not written by the creator's person.
    #[error("only the creator writes the rules")]
    NotCreator,
    /// The author is not an admin (or, under [`AddRule::Admins`], has no add right).
    #[error("the author is not an admin")]
    NotAdmin,
    /// The author, the person named or the owner is not a current member.
    #[error("not a member")]
    NotMember,
    /// An `approved-by` link not written by the owner it names.
    #[error("only the owner approves a bot")]
    NotOwner,
    /// The link needs a person's own key, and its key is device-only or a bot's.
    #[error("needs an identity")]
    NeedsIdentity,
    /// A self-join in an invite-only space without an invite that exists.
    #[error("the space is invite only")]
    InviteOnly,
    /// The invite expired before the self-join.
    #[error("the invite expired")]
    InviteExpired,
    /// Another person used the invite.
    #[error("the invite was used")]
    InviteUsed,
    /// An admin removed this person; the open rule does not admit them again.
    #[error("removed by an admin")]
    Removed,
    /// Timestamped before its author's removal and seen after it.
    #[error("timestamped before its author's removal")]
    Backdated,
}

/// A membership link the caller already judged and resolved to a person.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JudgedLink {
    /// The link's hash; with `timestamp`, the replay order. Hex in JSON.
    #[serde(with = "hex32")]
    pub hash: [u8; 32],
    /// The link's source.
    pub source: String,
    /// The link's predicate.
    pub predicate: String,
    /// The link's target.
    pub target: String,
    /// The signing key's `did:key`.
    pub author_key: String,
    /// The link's timestamp in ms.
    pub timestamp: u64,
    /// The link's verdict.
    pub verdict: LinkVerdict,
    /// The signing key's derived person; `None` for a device-only key or a key with no log. A
    /// bot key's derived person is its owner.
    #[serde(default)]
    pub person: Option<String>,
    /// Whether the signing key carries the bot mark.
    #[serde(default)]
    pub by_bot: bool,
}

impl JudgedLink {
    fn order(&self) -> (u64, &[u8; 32]) {
        (self.timestamp, &self.hash)
    }

    /// The person whose rights the link exercises: none for a bot key or a key without one.
    fn speaker(&self) -> Option<&str> {
        if self.by_bot {
            None
        } else {
            self.person.as_deref()
        }
    }

    /// Whom a removal of this author bars from backdating: the bot key for a bot, else the
    /// person (or the key, when it has none).
    fn author_id(&self) -> &str {
        match (self.by_bot, &self.person) {
            (false, Some(person)) => person,
            _ => &self.author_key,
        }
    }
}

/// The 32 bytes of a hash as lowercase hex.
mod hex32 {
    use super::*;

    pub fn serialize<S: Serializer>(hash: &[u8; 32], s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&hex::encode(hash))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<[u8; 32], D::Error> {
        let text = String::deserialize(d)?;
        let mut hash = [0u8; 32];
        hex::decode_to_slice(&text, &mut hash).map_err(serde::de::Error::custom)?;
        Ok(hash)
    }
}

/// The last counted removal of a person or bot.
#[derive(Clone, Debug)]
struct Removal {
    at: u64,
    /// An admin (not the person, not the bot's owner) removed them, and no one admitted them
    /// since.
    barred: bool,
}

#[derive(Clone, Debug)]
struct Invite {
    expires_at: u64,
    used_by: Option<String>,
}

/// The two rows a bot needs, by owner person.
#[derive(Clone, Debug, Default)]
struct BotRows {
    proposed_by: Option<String>,
    approved_by: Option<String>,
}

/// What the counted links add up to.
#[derive(Clone, Debug)]
struct State {
    rules: Option<Rules>,
    join: Join,
    members: BTreeMap<String, Member>,
    removals: BTreeMap<String, Removal>,
    invites: BTreeMap<String, Invite>,
    bots: BTreeMap<String, BotRows>,
}

/// The membership of one space: the replay of its membership links.
#[derive(Clone, Debug)]
pub struct MembershipGraph {
    creator: String,
    state: State,
    /// Every link that is not backdated, not repeated and has a counting verdict, counted or
    /// not, in `(timestamp, hash)` order. A link that arrives out of order replays them.
    kept: Vec<JudgedLink>,
    seen: HashSet<[u8; 32]>,
}

impl MembershipGraph {
    /// The empty graph of a space whose creator is the person `creator`.
    pub fn new(creator: &str) -> Self {
        MembershipGraph {
            creator: creator.to_string(),
            state: State::new(),
            kept: Vec::new(),
            seen: HashSet::new(),
        }
    }

    /// Applies one link, in any order of arrival: `Ok` with what it changed, or `Err` with
    /// why it counts for nothing. A link that sorts before one already applied replays the
    /// kept links in order, and the changes are those of the whole graph. A link timestamped
    /// before its author's counted removal is [`Refusal::Backdated`] and is not kept; a link
    /// whose predicate the replay does not read changes nothing.
    pub fn apply(&mut self, link: &JudgedLink) -> Result<Vec<MembershipChange>, Refusal> {
        if !link.verdict.counts() {
            return Err(Refusal::NotCounted);
        }
        if !reads(&link.predicate) {
            return Ok(Vec::new());
        }
        if self.seen.contains(&link.hash) {
            return Err(Refusal::Repeated);
        }
        if let Some(removal) = self.state.removals.get(link.author_id()) {
            if link.timestamp < removal.at {
                return Err(Refusal::Backdated);
            }
        }
        self.seen.insert(link.hash);
        let at = self.kept.partition_point(|k| k.order() < link.order());
        self.kept.insert(at, link.clone());
        if at + 1 == self.kept.len() {
            return self.state.step(&self.creator, link);
        }
        let mut state = State::new();
        let mut outcome = Ok(Vec::new());
        for (i, kept) in self.kept.iter().enumerate() {
            let result = state.step(&self.creator, kept);
            if i == at {
                outcome = result;
            }
        }
        let changes = self.state.changes_to(&state, link);
        self.state = state;
        outcome.map(|_| changes)
    }

    /// Sorts `links` by `(timestamp, hash)` and applies each: the graph and the refused links
    /// by their index in `links`. The result does not depend on the order of `links`; since
    /// every link meets the others in order, none is backdated, so a caller replaying a stored
    /// history leaves out the links [`apply`](Self::apply) refused as backdated when they came.
    pub fn replay(creator: &str, links: &[JudgedLink]) -> (Self, Vec<(usize, Refusal)>) {
        let mut order: Vec<usize> = (0..links.len()).collect();
        order.sort_by(|&a, &b| links[a].order().cmp(&links[b].order()));
        let mut graph = Self::new(creator);
        let mut refused = Vec::new();
        for i in order {
            if let Err(refusal) = graph.apply(&links[i]) {
                refused.push((i, refusal));
            }
        }
        refused.sort_by_key(|(i, _)| *i);
        (graph, refused)
    }

    /// The creator's person.
    pub fn creator(&self) -> &str {
        &self.creator
    }

    /// The counted rules; `None` before the creator's `rules` link.
    pub fn rules(&self) -> Option<&Rules> {
        self.state.rules.as_ref()
    }

    /// The join mode now.
    pub fn join(&self) -> Join {
        self.state.join
    }

    /// Whether `person` (a person DID or a bot key) is a current member.
    pub fn is_member(&self, person: &str) -> bool {
        self.state.current(person).is_some()
    }

    /// The role of a current member.
    pub fn role_of(&self, person: &str) -> Option<Role> {
        self.state.current(person).map(|m| m.role)
    }

    /// Every membership, current or ended (`until` set), by person.
    pub fn members(&self) -> Vec<Member> {
        self.state.members.values().cloned().collect()
    }

    /// What a self-join by `person` would meet at `now` (ms): `Ok` when it would count or the
    /// person is a member already. `by_bot` is whether the joining key carries the bot mark;
    /// `invite` is the invite the join names (`invite://<id>`).
    pub fn admits(
        &self,
        person: Option<&str>,
        by_bot: bool,
        invite: Option<&str>,
        now: u64,
    ) -> Result<(), Refusal> {
        let person = match person {
            Some(person) if !by_bot => person,
            _ => return Err(Refusal::NeedsIdentity),
        };
        if self.is_member(person) {
            return Ok(());
        }
        self.state.self_join(person, invite, now).map(|_| ())
    }
}

/// Whether the replay reads predicate `p`.
fn reads(p: &str) -> bool {
    use predicate::*;
    [
        RULES,
        JOIN,
        ADMIN,
        MEMBER,
        INVITE,
        REMOVED,
        BOT,
        APPROVED_BY,
    ]
    .contains(&p)
}

/// The decoded body of a `literal:<kind>:<percent-encoded>` URL.
fn literal(url: &str, kind: &str) -> Option<String> {
    let body = url
        .strip_prefix("literal:")?
        .strip_prefix(kind)?
        .strip_prefix(':')?;
    percent_decode_str(body)
        .decode_utf8()
        .ok()
        .map(|text| text.into_owned())
}

fn parse_rules(target: &str) -> Option<Rules> {
    #[derive(Deserialize)]
    struct Literal {
        add: Option<AddRule>,
        join: Option<Join>,
    }
    let literal: Literal = serde_json::from_str(&literal(target, "json")?).ok()?;
    Some(Rules {
        add: literal.add.unwrap_or(AddRule::Admins),
        join: literal.join.unwrap_or(Join::Invite),
    })
}

fn parse_join(target: &str) -> Option<Join> {
    match literal(target, "string")?.as_str() {
        "open" => Some(Join::Open),
        "invite" => Some(Join::Invite),
        _ => None,
    }
}

fn parse_expiry(target: &str) -> Option<u64> {
    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase")]
    struct Literal {
        expires_at: u64,
    }
    serde_json::from_str::<Literal>(&literal(target, "json")?)
        .ok()
        .map(|l| l.expires_at)
}

fn is_person(did: &str) -> bool {
    did.len() > DID_PREFIX.len() && did.starts_with(DID_PREFIX)
}

fn is_key(did: &str) -> bool {
    did.len() > DID_KEY_PREFIX.len() && did.starts_with(DID_KEY_PREFIX)
}

fn is_invite(uri: &str) -> bool {
    uri.len() > INVITE_PREFIX.len() && uri.starts_with(INVITE_PREFIX)
}

impl State {
    fn new() -> Self {
        State {
            rules: None,
            join: Join::Invite,
            members: BTreeMap::new(),
            removals: BTreeMap::new(),
            invites: BTreeMap::new(),
            bots: BTreeMap::new(),
        }
    }

    fn current(&self, person: &str) -> Option<&Member> {
        self.members.get(person).filter(|m| m.until.is_none())
    }

    fn is_admin(&self, person: Option<&str>) -> bool {
        person
            .and_then(|p| self.current(p))
            .is_some_and(|m| m.role == Role::Admin)
    }

    fn may_add(&self, person: Option<&str>) -> Result<(), Refusal> {
        let members_add = self.rules.is_some_and(|r| r.add == AddRule::Members);
        match person.and_then(|p| self.current(p)).map(|m| m.role) {
            Some(Role::Admin) => Ok(()),
            Some(Role::Member) if members_add => Ok(()),
            _ if members_add => Err(Refusal::NotMember),
            _ => Err(Refusal::NotAdmin),
        }
    }

    /// Makes `person` a current member with `role`: the change, if any.
    fn admit(
        &mut self,
        person: &str,
        role: Role,
        bot_of: Option<String>,
        at: u64,
    ) -> Option<MembershipChange> {
        if let Some(removal) = self.removals.get_mut(person) {
            removal.barred = false;
        }
        if let Some(member) = self.members.get_mut(person) {
            if member.until.is_none() {
                if member.role == role && member.bot_of == bot_of {
                    return None;
                }
                member.role = role;
                member.bot_of = bot_of;
                return Some(MembershipChange::RoleChanged(member.clone()));
            }
        }
        let member = Member {
            person: person.to_string(),
            role,
            bot_of,
            since: at,
            until: None,
        };
        self.members.insert(person.to_string(), member.clone());
        Some(MembershipChange::Joined(member))
    }

    /// Checks a self-join of `person` (who is not a member) at `at`, naming `invite`: the
    /// invite it uses, if it counts through one.
    fn self_join(
        &self,
        person: &str,
        invite: Option<&str>,
        at: u64,
    ) -> Result<Option<String>, Refusal> {
        let barred = self.removals.get(person).is_some_and(|r| r.barred);
        let open = || match (self.join, barred) {
            (Join::Open, false) => Ok(None),
            (Join::Open, true) => Err(Refusal::Removed),
            (Join::Invite, _) => Err(Refusal::InviteOnly),
        };
        let Some(uri) = invite else {
            return open();
        };
        let held = match self.invites.get(uri) {
            None => Err(Refusal::InviteOnly),
            Some(i) if at >= i.expires_at => Err(Refusal::InviteExpired),
            Some(i) if i.used_by.as_deref().is_some_and(|u| u != person) => {
                Err(Refusal::InviteUsed)
            }
            Some(_) => Ok(Some(uri.to_string())),
        };
        held.or_else(|refusal| open().map_err(|_| refusal))
    }

    /// Applies one link in order.
    fn step(&mut self, creator: &str, link: &JudgedLink) -> Result<Vec<MembershipChange>, Refusal> {
        let speaker = link.speaker();
        let at = link.timestamp;
        let (source, target) = (link.source.as_str(), link.target.as_str());
        let mut changes = Vec::new();
        match link.predicate.as_str() {
            predicate::RULES => {
                let rules = (source == SPACE)
                    .then(|| parse_rules(target))
                    .flatten()
                    .ok_or(Refusal::Malformed)?;
                if speaker != Some(creator) {
                    return Err(Refusal::NotCreator);
                }
                if self.rules.is_some() {
                    return Err(Refusal::Repeated);
                }
                self.rules = Some(rules);
                changes.extend(self.admit(creator, Role::Admin, None, at));
                changes.extend(self.set_join(rules.join));
            }
            predicate::JOIN => {
                let join = (source == SPACE)
                    .then(|| parse_join(target))
                    .flatten()
                    .ok_or(Refusal::Malformed)?;
                if !self.is_admin(speaker) {
                    return Err(Refusal::NotAdmin);
                }
                changes.extend(self.set_join(join));
            }
            predicate::ADMIN => {
                if !is_person(source) || target != SPACE {
                    return Err(Refusal::Malformed);
                }
                if !self.is_admin(speaker) {
                    return Err(Refusal::NotAdmin);
                }
                changes.extend(self.admit(source, Role::Admin, None, at));
            }
            predicate::MEMBER => {
                if !is_person(source) || !(target == SPACE || is_invite(target)) {
                    return Err(Refusal::Malformed);
                }
                if let Err(refusal) = self.may_add(speaker) {
                    let person = speaker.ok_or(Refusal::NeedsIdentity)?;
                    if person != source {
                        return Err(refusal);
                    }
                    if self.current(person).is_some() {
                        return Ok(changes);
                    }
                    let invite = is_invite(target).then_some(target);
                    if let Some(uri) = self.self_join(person, invite, at)? {
                        if let Some(held) = self.invites.get_mut(&uri) {
                            held.used_by = Some(person.to_string());
                        }
                    }
                }
                if self.current(source).is_none() {
                    changes.extend(self.admit(source, Role::Member, None, at));
                }
            }
            predicate::INVITE => {
                let expires_at = is_invite(source)
                    .then(|| parse_expiry(target))
                    .flatten()
                    .ok_or(Refusal::Malformed)?;
                if !self.is_admin(speaker) {
                    return Err(Refusal::NotAdmin);
                }
                if self.invites.contains_key(source) {
                    return Err(Refusal::Repeated);
                }
                self.invites.insert(
                    source.to_string(),
                    Invite {
                        expires_at,
                        used_by: None,
                    },
                );
            }
            predicate::REMOVED => {
                if !(is_person(source) || is_key(source)) || target != SPACE {
                    return Err(Refusal::Malformed);
                }
                let member = self.current(source).ok_or(Refusal::NotMember)?;
                let leaving = if link.by_bot {
                    link.author_key == source
                } else {
                    speaker == Some(source)
                };
                let owner = member.role == Role::Bot
                    && speaker.is_some()
                    && member.bot_of.as_deref() == speaker;
                if !(leaving || owner || self.is_admin(speaker)) {
                    return Err(Refusal::NotAdmin);
                }
                let by = link.author_id().to_string();
                if let Some(member) = self.members.get_mut(source) {
                    member.until = Some(at);
                }
                self.bots.remove(source);
                self.removals.insert(
                    source.to_string(),
                    Removal {
                        at,
                        barred: !(leaving || owner),
                    },
                );
                changes.push(MembershipChange::Removed {
                    person: source.to_string(),
                    by,
                    at,
                });
            }
            predicate::BOT => {
                if !is_key(source) || target != SPACE {
                    return Err(Refusal::Malformed);
                }
                let person = speaker.ok_or(Refusal::NeedsIdentity)?;
                if self.current(person).is_none() {
                    return Err(Refusal::NotMember);
                }
                self.bots.entry(source.to_string()).or_default().proposed_by =
                    Some(person.to_string());
                changes.extend(self.admit_bot(source, at));
            }
            predicate::APPROVED_BY => {
                if !is_key(source) || !is_person(target) {
                    return Err(Refusal::Malformed);
                }
                let person = speaker.ok_or(Refusal::NeedsIdentity)?;
                if person != target {
                    return Err(Refusal::NotOwner);
                }
                if self.current(person).is_none() {
                    return Err(Refusal::NotMember);
                }
                self.bots.entry(source.to_string()).or_default().approved_by =
                    Some(person.to_string());
                changes.extend(self.admit_bot(source, at));
            }
            _ => {}
        }
        Ok(changes)
    }

    fn set_join(&mut self, join: Join) -> Option<MembershipChange> {
        (self.join != join).then(|| {
            self.join = join;
            MembershipChange::JoinChanged { join }
        })
    }

    /// Admits a bot once both its rows count and its owner is a current member.
    fn admit_bot(&mut self, bot: &str, at: u64) -> Option<MembershipChange> {
        let rows = self.bots.get(bot)?;
        rows.proposed_by.as_ref()?;
        let owner = rows.approved_by.clone()?;
        self.current(&owner)?;
        self.admit(bot, Role::Bot, Some(owner), at)
    }

    /// The changes from `self` to `after`, for a replay that `cause` set off: the join mode,
    /// then by person. A membership the replay no longer has ends at `cause`.
    fn changes_to(&self, after: &State, cause: &JudgedLink) -> Vec<MembershipChange> {
        let mut changes = Vec::new();
        if self.join != after.join {
            changes.push(MembershipChange::JoinChanged { join: after.join });
        }
        let mut people: Vec<&String> = self.members.keys().chain(after.members.keys()).collect();
        people.sort();
        people.dedup();
        for person in people {
            match (self.current(person), after.current(person)) {
                (None, Some(now)) => changes.push(MembershipChange::Joined(now.clone())),
                (Some(_), None) => {
                    let ended = after.members.get(person).and_then(|m| m.until);
                    changes.push(MembershipChange::Removed {
                        person: person.clone(),
                        by: cause.author_id().to_string(),
                        at: ended.unwrap_or(cause.timestamp),
                    })
                }
                (Some(was), Some(now)) if was.role != now.role || was.bot_of != now.bot_of => {
                    changes.push(MembershipChange::RoleChanged(now.clone()))
                }
                _ => {}
            }
        }
        changes
    }
}
