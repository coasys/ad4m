//! `neighbourhood.countByPerson` (AR-08 section 4.11.2, slice 3.B part A9): the links of one
//! source and predicate, counted once per person.
//!
//! Each link is read with the verdict, person and bot flag the store holds for it (the
//! resolver's judgement under the read filter), so a count reads no log:
//!
//! - Only a `valid` link counts. `notMember` (the read filter turned it off), `cantCheck`
//!   (the author's log could not be read: `unavailable`) and every other verdict go to their
//!   skip bucket.
//! - A bot's link never counts (`bots`); a key that no log lists has no person (`noLog`).
//! - In a governed space the person must hold the role `member` or `admin` at `at` (now
//!   without it); a person who does not goes to `nonMembers`.
//! - A person counts once: their latest link (by timestamp, then target) wins and counts for
//!   its target.
//! - `eligible` is the number of members or admins whose person is a `did:scid` at `at`. A
//!   space without rules names no members: there it is the number of persons counted.
//! - `guests` stays 0: no link is stored as a guest's yet.

use std::collections::{BTreeMap, HashMap};

use ad4m_identity_core::membership::{Member, Role};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use super::{ms_of, SpaceMembership};
use crate::identity::verdict::LinkVerdict;
use crate::types::DecoratedLinkExpression;

/// What `neighbourhood.countByPerson` counts: the links `source -[predicate]-> *` of a
/// space, at `at` (ISO 8601; now without it).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PersonCountQuery {
    pub perspective_uuid: String,
    /// Without it, the links of every source.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub source: Option<String>,
    pub predicate: String,
    /// Count as of this time: later links are left out, and membership is as it was then.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[ts(optional)]
    pub at: Option<String>,
}

/// The links a count left out, by reason.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct SkipCounts {
    /// Always 0 for now.
    pub guests: u32,
    /// Links a bot signed.
    pub bots: u32,
    /// Valid links by a key no log lists: no person.
    pub no_log: u32,
    /// Links by a person who is not a member or admin (stored `notMember`, or not one at
    /// `at`).
    pub non_members: u32,
    /// Links whose author's log could not be read (`cantCheck`).
    pub unavailable: u32,
    /// Links that are not valid (invalid, flagged, unjudged or with an unreadable time).
    pub not_valid: u32,
}

/// The count of `neighbourhood.countByPerson`.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct PersonCount {
    /// Persons per target: each person's latest link counts.
    pub by_target: BTreeMap<String, u32>,
    /// The persons counted.
    pub counted: u32,
    /// The persons who could have been counted (see the module doc).
    pub eligible: u32,
    pub skipped: SkipCounts,
}

/// Whether `member` holds a role that counts at `at` (now without it).
fn counts_at(member: &Member, at: Option<u64>) -> bool {
    let role = matches!(member.role, Role::Admin | Role::Member);
    match at {
        None => role && member.until.is_none(),
        Some(at) => role && member.since <= at && member.until.is_none_or(|until| until > at),
    }
}

/// Counts `links` once per person under `membership`, at `at` (ms; now without it).
pub fn count_by_person(
    links: &[DecoratedLinkExpression],
    membership: &SpaceMembership,
    at: Option<u64>,
) -> PersonCount {
    let members: Option<HashMap<String, Member>> =
        membership.graph().filter(|g| g.rules().is_some()).map(|g| {
            g.members()
                .into_iter()
                .map(|m| (m.person.clone(), m))
                .collect()
        });
    let mut skipped = SkipCounts::default();
    // Each person's latest link: (timestamp, target).
    let mut latest: HashMap<&str, (u64, &str)> = HashMap::new();
    for link in links {
        let Some(timestamp) = ms_of(&link.timestamp) else {
            skipped.not_valid += 1;
            continue;
        };
        if at.is_some_and(|at| timestamp > at) {
            continue;
        }
        match link.verdict {
            Some(LinkVerdict::Valid) => {}
            Some(LinkVerdict::NotMember) => {
                skipped.non_members += 1;
                continue;
            }
            Some(LinkVerdict::CantCheck) => {
                skipped.unavailable += 1;
                continue;
            }
            _ => {
                skipped.not_valid += 1;
                continue;
            }
        }
        if link.by_bot == Some(true) {
            skipped.bots += 1;
            continue;
        }
        let Some(person) = link.person.as_deref() else {
            skipped.no_log += 1;
            continue;
        };
        if let Some(members) = &members {
            if !members.get(person).is_some_and(|m| counts_at(m, at)) {
                skipped.non_members += 1;
                continue;
            }
        }
        let this = (timestamp, link.data.target.as_str());
        latest
            .entry(person)
            .and_modify(|held| *held = (*held).max(this))
            .or_insert(this);
    }
    let mut by_target: BTreeMap<String, u32> = BTreeMap::new();
    for (_, target) in latest.values() {
        *by_target.entry(target.to_string()).or_default() += 1;
    }
    let counted = latest.len() as u32;
    let eligible = match &members {
        Some(members) => members
            .values()
            .filter(|m| m.person.starts_with("did:scid:") && counts_at(m, at))
            .count() as u32,
        None => counted,
    };
    PersonCount {
        by_target,
        counted,
        eligible,
        skipped,
    }
}
