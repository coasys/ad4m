//! Monotonic flow state (#1176): under the flow vocabulary, a Shared link is
//! never removed by a diff. The only way to end one is a new signed link.
//!
//! [`is_monotonic`] names the predicates: everything under `ad4m://flow/`,
//! `ad4m://acceptedBy` and `ad4m://monotonic`. The rule is enforced where
//! links cross into this replica's store:
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
//! exists. Local links (the engine's `ad4m://flow/current_state` cache, the
//! `resolved_as` marks) are private to this replica and are not affected.
//! The stored link's status decides, not the caller's label (see
//! [`PerspectiveInstance::removal_status`]), and a removal of a Local flow
//! link is never committed ([`committable`]).
//!
//! # Retraction
//!
//! `<link.source> --ad4m://flow/retracted--> literal:string:<link.proof.signature>`,
//! Shared and signed by the retracted link's own author. [`retraction_effects`]
//! runs inside every store write, so every replica applies it the same way
//! whatever path the tombstone or the link arrives by: a stored link it names
//! (same source, same author, that signature) is removed from the store, and
//! an addition a stored tombstone already names is not written, so the two
//! can arrive in either order. The removal is this replica's store effect
//! only; it is never committed to the link language. Only a tombstone whose
//! signature verifies counts. Tombstones (`retracted`, `role_grant_revoked`)
//! and the `ad4m://monotonic` flag cannot themselves be retracted: ending a
//! revocation would reopen a grant, and a declaration stays once made.
//!
//! # Old peers
//!
//! An executor without this module still applies and sends removals, and
//! stores tombstones as ordinary links. This one drops removals from it, and
//! never sends one. Until everyone upgrades the two disagree about exactly
//! these links, always in one direction: the new node keeps the link. There
//! is no compatibility switch, since accepting tombstone-less removals is the
//! hole this closes. Pending diffs from before an upgrade go through the same
//! ingest.
//!
//! [`retraction_effects`]: PerspectiveInstance::retraction_effects

use crate::perspectives::flow_instance::atom::{
    ACCEPTED_BY_PREDICATE, ROLE_GRANT_REVOKED_PREDICATE,
};
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{
    DecoratedLinkExpression, DecoratedPerspectiveDiff, Link, LinkExpression, LinkStatus,
    PerspectiveDiff,
};
use ad4m_client::literal::Literal;
use deno_core::error::AnyError;

/// Namespace of the flow engine's shared vocabulary; every predicate under it
/// is monotonic.
pub const FLOW_NAMESPACE: &str = "ad4m://flow/";
/// The property-shape flag that declares an app predicate monotonic (#1176
/// PR B reads it). The flag link is itself monotonic, so a declaration
/// cannot be withdrawn.
pub const MONOTONIC_FLAG_PREDICATE: &str = "ad4m://monotonic";
/// Tombstone: link source → `literal:string:<signature of the ended link>`.
pub const RETRACTED_PREDICATE: &str = "ad4m://flow/retracted";

/// Whether a Shared link under `predicate` may only be ended by a tombstone.
pub fn is_monotonic(predicate: Option<&str>) -> bool {
    match predicate {
        Some(p) => {
            p.starts_with(FLOW_NAMESPACE)
                || p == ACCEPTED_BY_PREDICATE
                || p == MONOTONIC_FLAG_PREDICATE
        }
        None => false,
    }
}

/// Whether an `ad4m://flow/retracted` tombstone can end a link under
/// `predicate`: monotonic, and not itself a tombstone or the flag.
pub fn is_retractable(predicate: Option<&str>) -> bool {
    is_monotonic(predicate)
        && !matches!(
            predicate,
            Some(RETRACTED_PREDICATE | ROLE_GRANT_REVOKED_PREDICATE | MONOTONIC_FLAG_PREDICATE)
        )
}

/// The local-write refusal: `Err` for a Shared removal under a monotonic
/// predicate. Called before anything is persisted or committed.
pub fn refuse_monotonic_removal(
    link: &LinkExpression,
    status: &LinkStatus,
) -> Result<(), AnyError> {
    let predicate = link.data.predicate.as_deref();
    if *status == LinkStatus::Shared && is_monotonic(predicate) {
        return Err(anyhow::anyhow!(
            "{} is monotonic; retract with a tombstone ({RETRACTED_PREDICATE}) instead of removing {} -> {}",
            predicate.unwrap_or_default(),
            link.data.source,
            link.data.target
        ));
    }
    Ok(())
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

    /// The status a removal of `link` would take from this store: the stored
    /// link's own, else `requested`. `link_mutations` carries its caller's
    /// status rather than the stored one, so under a monotonic predicate the
    /// store decides in both directions: a Local-labelled removal of a Shared
    /// link is refused, a Shared-labelled removal of a Local link goes. A link
    /// this store does not hold keeps `requested`, so Shared stays refused.
    pub(crate) fn removal_status(
        &self,
        link: &LinkExpression,
        requested: &LinkStatus,
    ) -> Result<LinkStatus, AnyError> {
        if !is_monotonic(link.data.predicate.as_deref()) {
            return Ok(requested.clone());
        }
        let stored = self.sparql_store.get_link(
            &link.data.source,
            link.data.predicate.as_deref(),
            &link.data.target,
            &link.author,
            &link.timestamp,
        )?;
        Ok(stored
            .and_then(|l| l.status)
            .unwrap_or_else(|| requested.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_fixed_core_is_monotonic_and_nothing_else() {
        for p in [
            "ad4m://flow/to_state",
            "ad4m://flow/role_grant_revoked",
            "ad4m://flow/retracted",
            "ad4m://acceptedBy",
            "ad4m://monotonic",
        ] {
            assert!(is_monotonic(Some(p)), "{p}");
        }
        for p in [
            "ad4m://flowName",
            "ad4m://flow",
            "ad4m://acceptedByX",
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
        assert!(!is_retractable(Some(RETRACTED_PREDICATE)));
        assert!(!is_retractable(Some(ROLE_GRANT_REVOKED_PREDICATE)));
        assert!(!is_retractable(Some(MONOTONIC_FLAG_PREDICATE)));
        assert!(!is_retractable(Some("test://likes")));
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
