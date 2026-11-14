//! What a link language's diff may change in this perspective (#1146).
//!
//! A diff from a neighbourhood is untrusted: the link language does not bind
//! `LinkExpression.author` to whoever committed it. `diff_from_link_language`
//! passes every diff through [`retain_ingestible`] and then writes it with
//! [`SparqlStore::add_remote_link`] / [`SparqlStore::remove_remote_link`],
//! and decorates and publishes only what those applied, so no reader
//! (model_query, Prolog, raw SPARQL, `query_links`, flows) sees what is
//! dropped:
//!
//! ```text
//! link language ─► dedup ─► retain_ingestible ─► add/remove_remote_link ─► prolog / pubsub
//!                           └ addition: verdict   └ addition or removal: link not
//!                             not `invalid`         stored as Local (checked and
//!                                                   written under one store lock)
//! ```
//!
//! Removals of Shared links are not checked for authority here.
//!
//! Membership links never ride a diff: one in a diff is dropped. The link language hands
//! them over apart (`membershipLinks`, slice 3.B A7), and
//! `PerspectiveInstance::persist_membership_links` judges each through the owner gate as a
//! peer's link before it writes it.

use super::monotonic::RetractionEffects;
use super::perspective_instance::PerspectiveInstance;
#[cfg(doc)]
use super::sparql_store::SparqlStore;
use crate::identity::verdict::LinkVerdict;
use crate::membership::{MembershipLinkResult, MembershipRefused};
use crate::types::{LinkExpression, LinkStatus, PerspectiveDiff};
use ad4m_identity_core::membership::{is_membership_predicate, Refusal};
use deno_core::error::AnyError;
use std::sync::PoisonError;

/// Drop from a remote diff every addition whose verdict is `invalid`: its
/// proof does not verify against its own `author`, or the signer's key log
/// rules the key out. A `cantCheck` link stays (its signer's log is not
/// known yet; the store re-checks it once it is), and so does a `flagged`
/// one: both are stored with that verdict, and logic reads count neither.
/// A membership link (`coasys://membership/…`) is dropped however it is
/// signed (R13): membership reaches a space only through the membership
/// sync, never in an ordinary diff.
/// The caller fills the key map first ([`crate::identity::prepare_link_signers`]).
pub(crate) fn retain_ingestible(additions: &mut Vec<LinkExpression>) {
    additions.retain(|link| {
        let predicate = link.data.predicate.as_deref().unwrap_or("");
        if ad4m_identity_core::membership::is_membership_predicate(predicate) {
            log::warn!(
                "Dropping membership link from a remote diff: {} -[{predicate}]-> {} by {}",
                link.data.source,
                link.data.target,
                link.author
            );
            return false;
        }
        let verdict = link.check().verdict;
        let invalid = matches!(verdict, LinkVerdict::Invalid(_));
        if invalid {
            log::warn!(
                "Dropping remote link whose verdict is {}: {} -[{}]-> {} by {}",
                verdict.stored(),
                link.data.source,
                predicate,
                link.data.target,
                link.author
            );
        }
        !invalid
    });
}

impl PerspectiveInstance {
    /// Write the membership links a link language synced (A7, `membershipLinks`), one at a
    /// time under `persist_lock`, each through the owner gate as a peer's link
    /// ([`crate::membership::SpaceMembership::gate`] with `own` false): a link the gate refuses
    /// is not written and refuses only itself. A link this store holds already counts again; a
    /// link that is not a membership link is refused as malformed (it belongs in a diff).
    /// Returns the result per link, in order, and the links written. The caller fills the key
    /// map for their signers first and publishes after (`membership_links_from_link_language`).
    pub(super) fn persist_membership_links(
        &self,
        links: Vec<LinkExpression>,
    ) -> (Vec<MembershipLinkResult>, Vec<LinkExpression>) {
        let _persisting = self
            .persist_lock
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let membership = self.sparql_store.membership();
        let mut results = Vec::with_capacity(links.len());
        let mut written = Vec::new();
        for mut link in links {
            link.status = Some(LinkStatus::Shared);
            let hash = super::sparql_store::link_hash(&link);
            let result = |counted: bool, reason: Option<Refusal>| MembershipLinkResult {
                hash: hex::encode(hash),
                counted,
                reason,
            };
            let predicate = link.data.predicate.as_deref().unwrap_or("");
            if !is_membership_predicate(predicate) {
                results.push(result(false, Some(Refusal::Malformed)));
                continue;
            }
            if membership.holds(&hash) {
                results.push(result(true, None));
                continue;
            }
            if let Err(MembershipRefused(refusal)) =
                membership.gate(std::slice::from_ref(&link), false)
            {
                results.push(result(false, Some(refusal)));
                continue;
            }
            match self.sparql_store.add_remote_link(&link) {
                Ok(true) => {
                    results.push(result(true, None));
                    written.push(link);
                }
                outcome => {
                    // The gate took the link and the store did not: replay what the store
                    // holds, so the membership is the store's again.
                    log::warn!(
                        "Membership link {} -[{predicate}]-> {} not stored: {outcome:?}",
                        link.data.source,
                        link.data.target
                    );
                    match self.sparql_store.membership_links() {
                        Ok(stored) => membership.rebuild(&stored),
                        Err(e) => log::error!("Membership links unreadable: {e:?}"),
                    }
                    results.push(result(false, Some(Refusal::Malformed)));
                }
            }
        }
        (results, written)
    }

    /// Write a link language's diff, removals first like
    /// [`Self::persist_link_diff`] and under the same `persist_lock`,
    /// skipping every link this store holds as `Local` (#1146). Returns what
    /// was applied, and what the diff's `ad4m://retracted` tombstones did
    /// (`monotonic::RetractionEffects`): the links they end are removed, and
    /// additions they cover are not written. A link whose write fails, or
    /// whose stored status cannot be read, is dropped rather than applied
    /// blind.
    pub(super) fn persist_remote_diff(
        &self,
        diff: PerspectiveDiff,
    ) -> Result<(PerspectiveDiff, RetractionEffects), AnyError> {
        let _persisting = self
            .persist_lock
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let effects = self.retraction_effects(&diff.additions)?;
        let applied = |kind: &str, link: &LinkExpression, result: Result<bool, AnyError>| {
            result.unwrap_or_else(|e| {
                log::warn!(
                    "Dropping remote {kind} of {} -[{}]-> {}: {e:?}",
                    link.data.source,
                    link.data.predicate.as_deref().unwrap_or(""),
                    link.data.target
                );
                false
            })
        };
        // A tombstone ends only a Shared link (`monotonic::ends`), so its
        // removals need no Local check; `fold_into` publishes them.
        for retracted in &effects.retracted {
            if let Err(e) = self
                .sparql_store
                .remove_link(&LinkExpression::from(retracted.clone()))
            {
                log::warn!("Failed to remove retracted link from SPARQL store: {:?}", e);
            }
        }
        let removals = diff
            .removals
            .into_iter()
            .filter(|l| applied("removal", l, self.sparql_store.remove_remote_link(l)))
            .collect();
        let additions = diff
            .additions
            .into_iter()
            .filter(|l| !effects.covers(l))
            .filter(|l| applied("addition", l, self.sparql_store.add_remote_link(l)))
            .collect();
        Ok((
            PerspectiveDiff {
                additions,
                removals,
            },
            effects,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::retain_ingestible;
    use crate::agent::signatures::TestSigner;
    use crate::types::{Link, LinkExpression};

    /// R13: a membership link that arrives in an ordinary diff is dropped, however it is
    /// signed; an ordinary link beside it stays.
    #[test]
    fn a_membership_link_in_a_diff_is_dropped() {
        let signer = TestSigner::generate();
        let link = |predicate: &str| -> LinkExpression {
            signer
                .sign(Link {
                    source: "did:scid:coasys:1:Ada".into(),
                    predicate: Some(predicate.into()),
                    target: "coasys://membership/space".into(),
                })
                .into()
        };
        let mut additions = vec![link("coasys://membership/member"), link("test://likes")];
        retain_ingestible(&mut additions);
        let kept: Vec<_> = additions
            .iter()
            .map(|l| l.data.predicate.clone().unwrap())
            .collect();
        assert_eq!(kept, vec!["test://likes".to_string()]);
    }
}
