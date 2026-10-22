//! Monotonic links (#1176): a Shared link under a monotonic predicate is
//! never removed by a diff. The only way to end one is a new signed link.
//!
//! The mechanism knows nothing about the code that writes such links. One
//! table, [`MONOTONIC_PREDICATES`], names the predicates (exact or by
//! prefix) and, for each, how a link under it may end ([`Ending`]):
//!
//! - [`Ending::Tombstone`]: by its author's signed tombstone (see
//!   *Retraction*). Flows use this: everything under `ad4m://flow/` and
//!   `ad4m://acceptedBy`.
//! - [`Ending::Revocation`]: only by a dedicated revocation link, which the
//!   predicate's reader interprets; the link itself stays, and so does the
//!   history it is part of.
//! - [`Ending::Never`]: tombstones themselves (`ad4m://retracted`,
//!   `ad4m://flow/role_grant_revoked`) and the `ad4m://monotonic` flag.
//!   Ending a revocation would reopen a grant, and a declaration stays once
//!   made.
//!
//! Another feature makes its predicates monotonic by adding an entry. The
//! rule is enforced where links cross into this replica's store:
//!
//! ```text
//!  peer diff ──► diff_from_link_language ──► drop_monotonic_removals (warn)
//!                                                   │
//!  local write ─► remove_link / remove_links /      │
//!                 update_link / link_mutations /    │
//!                 commit_batch                      │
//!                   └─ refuse_monotonic_removal ─► error, nothing persisted
//!                                                   ▼
//!                                         persist_link_diff
//!                                           └─ retraction_effects
//! ```
//!
//! A local write is refused, not rewritten into a tombstone: callers and
//! subscribers would otherwise see a removal event for a link that still
//! exists. Local links (such as the `ad4m://flow/current_state` cache and
//! `resolved_as` marks older executors wrote, before both moved to
//! `replica_state`) are private to this replica and are not affected.
//! The stored link's status decides, not the caller's label (a removal of a
//! link the store does not hold keeps the caller's, so Shared is refused),
//! and a removal of a Local monotonic link is never committed
//! ([`committable`]).
//!
//! # Retraction
//!
//! `<link.source> --ad4m://retracted--> literal:string:<link.proof.signature>`,
//! Shared and signed by the retracted link's own author. It ends only links
//! whose entry is [`Ending::Tombstone`]. [`retraction_effects`]
//! runs inside every store write, so every replica applies it the same way
//! whatever path the tombstone or the link arrives by: a stored link it names
//! (same source, same author, that signature) is removed from the store, and
//! an addition a stored tombstone already names is not written, so the two
//! can arrive in either order. The removal is this replica's store effect
//! only; it is never committed to the link language. Only a tombstone whose
//! signature verifies counts.
//!
//! # Old peers
//!
//! An executor without this module still applies and sends removals, and
//! stores tombstones as ordinary links. This one drops removals from it, and
//! never sends one. Until everyone upgrades the two disagree about exactly
//! these links, in a direction set by the operation: after a removal the old
//! node drops the link and the new node keeps it; after a valid tombstone the
//! new node ends the link while the old node keeps it (and stores the
//! tombstone as an ordinary link). There is no compatibility switch, since
//! accepting tombstone-less removals is the hole this closes. Pending diffs from before an upgrade go through the same
//! ingest.
//!
//! [`retraction_effects`]: PerspectiveInstance::retraction_effects

use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{
    DecoratedLinkExpression, DecoratedPerspectiveDiff, Link, LinkExpression, LinkStatus,
    PerspectiveDiff,
};
use ad4m_client::literal::Literal;
use deno_core::error::AnyError;

// ---------------------------------------------------------------------------
// Vocabulary. Defined here, not in the code that writes it, so this module
// depends on nothing above it; the flow engine imports these from here.
// ---------------------------------------------------------------------------

/// Tombstone: link source → `literal:string:<signature of the ended link>`.
/// Generic: it ends a link under any [`Ending::Tombstone`] predicate.
pub const RETRACTED_PREDICATE: &str = "ad4m://retracted";
/// The property-shape flag that declares an app predicate monotonic (#1176
/// PR B reads it). The flag link is itself monotonic, so a declaration
/// cannot be withdrawn.
pub const MONOTONIC_FLAG_PREDICATE: &str = "ad4m://monotonic";
/// Namespace of the flow engine's shared vocabulary; every predicate under it
/// is monotonic.
pub const FLOW_NAMESPACE: &str = "ad4m://flow/";
/// Proposal → a voting DID. A vote counts only when the link's author IS the
/// DID it names, with a valid signature (see
/// `flow_instance::atom::valid_votes`).
pub const ACCEPTED_BY_PREDICATE: &str = "ad4m://acceptedBy";
/// Tombstone: role instance → revoked DID. Written instead of deleting the
/// grant; the link's author-asserted timestamp is the revocation time. Source =
/// role-instance URI, target = the DID — literal-encoded or raw, the reader
/// accepts both (`flow_evaluator::target_names_did`). Authority mirrors the
/// grant's own rule (`roles::revocation_authorised`): whoever the role
/// query's `author` condition accepts as granter may revoke, which makes
/// admin-gated roles admin-revoked, `$did` roles self-revoked, and
/// open-authorship roles revocable by anyone. Stays in the graph forever so
/// newcomers can reconstruct the full role-membership history.
pub const ROLE_GRANT_REVOKED_PREDICATE: &str = "ad4m://flow/role_grant_revoked";

// ---------------------------------------------------------------------------
// The table
// ---------------------------------------------------------------------------

/// How a Shared link under a monotonic predicate may end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ending {
    /// By its author's signed [`RETRACTED_PREDICATE`] tombstone, which this
    /// module applies to the store.
    Tombstone,
    /// Only by a dedicated revocation link under the given predicate, which
    /// keeps the history the link was part of. The link itself stays; the
    /// reader of that predicate decides what the revocation means.
    Revocation(&'static str),
    /// Never: tombstones and declarations, whose ending would undo them.
    Never,
}

/// Which predicates an entry of [`MONOTONIC_PREDICATES`] covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PredicateMatch {
    Exact(&'static str),
    Prefix(&'static str),
}

impl PredicateMatch {
    pub fn matches(&self, predicate: &str) -> bool {
        match self {
            PredicateMatch::Exact(p) => predicate == *p,
            PredicateMatch::Prefix(p) => predicate.starts_with(p),
        }
    }
}

/// One monotonic predicate (or namespace) and how a link under it may end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonotonicPredicate {
    pub predicate: PredicateMatch,
    pub ending: Ending,
}

/// Every monotonic predicate this executor knows. The first matching entry
/// decides, so exact entries come before a prefix that also covers them.
/// A predicate in no entry is ordinary: removable by any diff.
pub const MONOTONIC_PREDICATES: &[MonotonicPredicate] = &[
    MonotonicPredicate {
        predicate: PredicateMatch::Exact(RETRACTED_PREDICATE),
        ending: Ending::Never,
    },
    MonotonicPredicate {
        predicate: PredicateMatch::Exact(MONOTONIC_FLAG_PREDICATE),
        ending: Ending::Never,
    },
    // A revocation is itself a tombstone; ending it would reopen the grant.
    MonotonicPredicate {
        predicate: PredicateMatch::Exact(ROLE_GRANT_REVOKED_PREDICATE),
        ending: Ending::Never,
    },
    // Flows (#1176): votes and the flow vocabulary.
    MonotonicPredicate {
        predicate: PredicateMatch::Exact(ACCEPTED_BY_PREDICATE),
        ending: Ending::Tombstone,
    },
    MonotonicPredicate {
        predicate: PredicateMatch::Prefix(FLOW_NAMESPACE),
        ending: Ending::Tombstone,
    },
];

/// How a Shared link under `predicate` may end, per `table`; `None` if it
/// is not monotonic.
pub fn ending_in(table: &[MonotonicPredicate], predicate: Option<&str>) -> Option<Ending> {
    let predicate = predicate?;
    table
        .iter()
        .find(|entry| entry.predicate.matches(predicate))
        .map(|entry| entry.ending)
}

/// How a Shared link under `predicate` may end; `None` if it is not
/// monotonic.
pub fn ending(predicate: Option<&str>) -> Option<Ending> {
    ending_in(MONOTONIC_PREDICATES, predicate)
}

/// Whether a Shared link under `predicate` is never removed by a diff.
pub fn is_monotonic(predicate: Option<&str>) -> bool {
    ending(predicate).is_some()
}

/// Whether a [`RETRACTED_PREDICATE`] tombstone can end a link under
/// `predicate`: its entry says [`Ending::Tombstone`].
pub fn is_retractable(predicate: Option<&str>) -> bool {
    ending(predicate) == Some(Ending::Tombstone)
}

/// What the refusal of a removal under a predicate ending `ending` tells
/// the caller to do instead.
fn instead(ending: Ending) -> String {
    match ending {
        Ending::Tombstone => format!("retract with a tombstone ({RETRACTED_PREDICATE})"),
        Ending::Revocation(by) => format!("end it with a revocation ({by})"),
        Ending::Never => "it cannot be ended".to_string(),
    }
}

/// The local-write refusal: `Err` for a Shared removal under a monotonic
/// predicate. Called before anything is persisted or committed.
pub fn refuse_monotonic_removal(
    link: &LinkExpression,
    status: &LinkStatus,
) -> Result<(), AnyError> {
    let predicate = link.data.predicate.as_deref();
    match ending(predicate) {
        Some(ending) if *status == LinkStatus::Shared => Err(anyhow::anyhow!(
            "{} is monotonic; {} instead of removing {} -> {}",
            predicate.unwrap_or_default(),
            instead(ending),
            link.data.source,
            link.data.target
        )),
        _ => Ok(()),
    }
}

/// Ingest side: the removals of a peer's diff that may be applied. Every
/// removal under a monotonic predicate is dropped with a warning, whatever
/// status the sender claims; everything arriving by sync is Shared.
pub fn drop_monotonic_removals(removals: Vec<LinkExpression>) -> Vec<LinkExpression> {
    removals
        .into_iter()
        .filter(|link| {
            let keep = !is_monotonic(link.data.predicate.as_deref());
            if !keep {
                log::warn!(
                    "dropping a peer's removal of a monotonic link ({} -[{}]-> {} by {}); only its author's tombstone ends it",
                    link.data.source,
                    link.data.predicate.as_deref().unwrap_or_default(),
                    link.data.target,
                    link.author
                );
            }
            keep
        })
        .collect()
}

/// The part of a local diff that may be committed to the link language: no
/// removal under a monotonic predicate. One that got this far passed
/// [`refuse_monotonic_removal`] on its stored status, so it ends a Local link
/// that peers never held; sending it would break "never sends one".
pub fn committable(diff: &PerspectiveDiff) -> PerspectiveDiff {
    PerspectiveDiff {
        additions: diff.additions.clone(),
        removals: diff
            .removals
            .iter()
            .filter(|l| !is_monotonic(l.data.predicate.as_deref()))
            .cloned()
            .collect(),
    }
}

/// The tombstone that ends `link`. Its author must write it: a tombstone signed
/// by anyone else has no effect.
pub fn retraction_for(link: &LinkExpression) -> Result<Link, AnyError> {
    let target = Literal::from_string(link.proof.signature.clone()).to_url()?;
    Ok(Link {
        source: link.data.source.clone(),
        predicate: Some(RETRACTED_PREDICATE.to_string()),
        target,
    })
}

/// The signature a tombstone names, if `link` is a tombstone that counts:
/// Shared, and its own signature verifies. `None` for anything else.
fn retracted_signature(link: &LinkExpression) -> Option<String> {
    if link.data.predicate.as_deref() != Some(RETRACTED_PREDICATE)
        || link.status != Some(LinkStatus::Shared)
        || !link.compute_proof_valid()
    {
        return None;
    }
    match parse_literal_value(&link.data.target) {
        serde_json::Value::String(signature) if !signature.is_empty() => Some(signature),
        _ => None,
    }
}

/// Whether `tombstone` (already checked by [`retracted_signature`] to name
/// `signature`) ends `link`.
fn ends(tombstone: &LinkExpression, signature: &str, link: &LinkExpression) -> bool {
    is_retractable(link.data.predicate.as_deref())
        && link.data.source == tombstone.data.source
        && link.author == tombstone.author
        && link.proof.signature == signature
}

/// What the tombstones in, and already under, a diff's additions do to the
/// store.
#[derive(Debug, Default)]
pub(crate) struct RetractionEffects {
    /// Additions an already stored tombstone (or one in the same diff) ends:
    /// not written.
    pub covered: Vec<LinkExpression>,
    /// Stored links a tombstone in the diff ends: removed from this store,
    /// never committed.
    pub retracted: Vec<DecoratedLinkExpression>,
}

impl RetractionEffects {
    pub fn covers(&self, link: &LinkExpression) -> bool {
        self.covered.iter().any(|c| same_link(c, link))
    }

    /// Bring the diff a write path publishes (Prolog, pubsub, flow trigger)
    /// in line with what the store now holds.
    pub fn fold_into(&self, diff: &mut DecoratedPerspectiveDiff) {
        if self.covered.is_empty() && self.retracted.is_empty() {
            return;
        }
        diff.additions.retain(|added| {
            !self
                .covered
                .iter()
                .any(|c| same_link(c, &LinkExpression::from(added.clone())))
        });
        diff.removals.extend(self.retracted.iter().cloned());
    }
}

fn same_link(a: &LinkExpression, b: &LinkExpression) -> bool {
    a.author == b.author
        && a.timestamp == b.timestamp
        && a.data.source == b.data.source
        && a.data.predicate == b.data.predicate
        && a.data.target == b.data.target
}

impl PerspectiveInstance {
    /// The retraction effects of writing `additions`: see the module doc.
    /// Reads the store, writes nothing.
    pub(crate) fn retraction_effects(
        &self,
        additions: &[LinkExpression],
    ) -> Result<RetractionEffects, AnyError> {
        let mut effects = RetractionEffects::default();
        let tombstones: Vec<(&LinkExpression, String)> = additions
            .iter()
            .filter_map(|l| retracted_signature(l).map(|sig| (l, sig)))
            .collect();

        for (tombstone, signature) in &tombstones {
            for stored in self
                .sparql_store
                .get_links_by_source(&tombstone.data.source)?
            {
                let link = LinkExpression::from(stored.clone());
                if ends(tombstone, signature, &link) {
                    effects.retracted.push(stored);
                }
            }
        }

        for link in additions {
            if !is_retractable(link.data.predicate.as_deref()) {
                continue;
            }
            let in_diff = tombstones.iter().any(|(t, sig)| ends(t, sig, link));
            if in_diff || self.stored_tombstone_ends(link)? {
                effects.covered.push(link.clone());
            }
        }
        Ok(effects)
    }

    fn stored_tombstone_ends(&self, link: &LinkExpression) -> Result<bool, AnyError> {
        let stored = self.sparql_store.query_links(
            Some(&link.data.source),
            Some(RETRACTED_PREDICATE),
            None,
            None,
            None,
            None,
        )?;
        Ok(stored.into_iter().any(|t| {
            let tombstone = LinkExpression::from(t);
            retracted_signature(&tombstone).is_some_and(|sig| ends(&tombstone, &sig, link))
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A predicate an entry covers, for table-driven assertions.
    fn sample(entry: &MonotonicPredicate) -> String {
        match entry.predicate {
            PredicateMatch::Exact(p) => p.to_string(),
            PredicateMatch::Prefix(p) => format!("{p}sample"),
        }
    }

    /// Every entry of the table is monotonic and ends the way the entry
    /// says; a tombstone ends only `Tombstone` entries.
    #[test]
    fn every_table_entry_is_monotonic_and_ends_as_listed() {
        for entry in MONOTONIC_PREDICATES {
            let p = sample(entry);
            assert!(is_monotonic(Some(&p)), "{p}");
            assert_eq!(ending(Some(&p)), Some(entry.ending), "{p}");
            assert_eq!(
                is_retractable(Some(&p)),
                entry.ending == Ending::Tombstone,
                "{p}"
            );
        }
    }

    #[test]
    fn the_table_is_monotonic_and_nothing_else() {
        for p in [
            "ad4m://flow/to_state",
            "ad4m://flow/role_grant_revoked",
            "ad4m://retracted",
            "ad4m://acceptedBy",
            "ad4m://monotonic",
        ] {
            assert!(is_monotonic(Some(p)), "{p}");
        }
        for p in [
            "ad4m://flowName",
            "ad4m://flow",
            "ad4m://acceptedByX",
            "ad4m://retractedX",
            "test://likes",
        ] {
            assert!(!is_monotonic(Some(p)), "{p}");
        }
        assert!(!is_monotonic(None));
    }

    #[test]
    fn tombstones_and_the_flag_are_not_retractable() {
        assert!(is_retractable(Some("ad4m://acceptedBy")));
        assert!(is_retractable(Some("ad4m://flow/to_state")));
        for p in [
            RETRACTED_PREDICATE,
            ROLE_GRANT_REVOKED_PREDICATE,
            MONOTONIC_FLAG_PREDICATE,
        ] {
            assert_eq!(ending(Some(p)), Some(Ending::Never), "{p}");
            assert!(!is_retractable(Some(p)), "{p}");
        }
        assert!(!is_retractable(Some("test://likes")));
    }

    /// The lookup reads whatever table it is given: a new entry is all a
    /// feature needs, and the first match decides.
    #[test]
    fn ending_in_reads_the_given_table() {
        let table = [
            MonotonicPredicate {
                predicate: PredicateMatch::Exact("test://grant"),
                ending: Ending::Revocation("test://grant_revoked"),
            },
            MonotonicPredicate {
                predicate: PredicateMatch::Prefix("test://log/"),
                ending: Ending::Tombstone,
            },
            MonotonicPredicate {
                predicate: PredicateMatch::Exact("test://log/seal"),
                ending: Ending::Never,
            },
        ];
        assert_eq!(
            ending_in(&table, Some("test://grant")),
            Some(Ending::Revocation("test://grant_revoked"))
        );
        assert_eq!(
            ending_in(&table, Some("test://log/entry")),
            Some(Ending::Tombstone)
        );
        assert_eq!(
            ending_in(&table, Some("test://log/seal")),
            Some(Ending::Tombstone),
            "shadowed by the earlier prefix"
        );
        assert_eq!(ending_in(&table, Some("test://grants")), None);
        assert_eq!(ending_in(&table, Some("ad4m://flow/to_state")), None);
        assert_eq!(ending_in(&table, None), None);
    }

    #[test]
    fn a_refusal_names_how_the_link_ends() {
        let link = |predicate: &str| LinkExpression {
            data: Link {
                source: "s".into(),
                predicate: Some(predicate.into()),
                target: "t".into(),
            },
            author: "did:key:a".into(),
            timestamp: "t0".into(),
            proof: Default::default(),
            status: Some(LinkStatus::Shared),
        };
        let refusal = |predicate: &str| {
            format!(
                "{:#}",
                refuse_monotonic_removal(&link(predicate), &LinkStatus::Shared).unwrap_err()
            )
        };
        assert!(refusal("ad4m://flow/to_state").contains("tombstone (ad4m://retracted)"));
        assert!(refusal(MONOTONIC_FLAG_PREDICATE).contains("cannot be ended"));
        assert!(refuse_monotonic_removal(&link("test://likes"), &LinkStatus::Shared).is_ok());
        assert!(refuse_monotonic_removal(&link("ad4m://flow/to_state"), &LinkStatus::Local).is_ok());
    }

    #[test]
    fn monotonic_removals_are_never_committed() {
        let link = |predicate: &str| LinkExpression {
            data: Link {
                source: "s".into(),
                predicate: Some(predicate.into()),
                target: "t".into(),
            },
            author: "did:key:a".into(),
            timestamp: "t0".into(),
            proof: Default::default(),
            status: Some(LinkStatus::Shared),
        };
        let diff = PerspectiveDiff {
            additions: vec![link("ad4m://flow/current_state")],
            removals: vec![link("ad4m://flow/current_state"), link("test://likes")],
        };

        let committed = committable(&diff);

        assert_eq!(committed.additions.len(), 1);
        assert_eq!(committed.removals.len(), 1);
        assert_eq!(
            committed.removals[0].data.predicate.as_deref(),
            Some("test://likes")
        );
    }
}
