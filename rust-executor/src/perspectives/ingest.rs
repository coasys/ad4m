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
//!                           └ addition: proof     └ addition or removal: link not
//!                             verifies against      stored as Local (checked and
//!                             its author            written under one store lock)
//! ```
//!
//! Removals of Shared links are not checked for authority here.

use super::monotonic::RetractionEffects;
use super::perspective_instance::PerspectiveInstance;
#[cfg(doc)]
use super::sparql_store::SparqlStore;
use crate::types::{LinkExpression, PerspectiveDiff};
use deno_core::error::AnyError;
use std::sync::PoisonError;

/// Drop from a remote diff every addition whose proof does not verify
/// against its own `author`.
pub(crate) fn retain_ingestible(additions: &mut Vec<LinkExpression>) {
    additions.retain(|link| {
        let valid = link.compute_proof_valid();
        if !valid {
            log::warn!(
                "Dropping remote link whose proof does not verify against its author {}: {} -[{}]-> {}",
                link.author,
                link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                link.data.target
            );
        }
        valid
    });
}

impl PerspectiveInstance {
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
