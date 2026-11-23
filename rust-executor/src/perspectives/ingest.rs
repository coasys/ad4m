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
use super::perspective_instance::{touches_shape, PerspectiveInstance};
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

/// What `persist_remote_diff` does with one link's store result: the
/// generation an applied write took goes to the feed entry's coverage,
/// `Some`; a skipped (stored `Local`) or failed write takes none. Returns
/// whether the link was applied.
fn applied(
    kind: &str,
    link: &LinkExpression,
    result: Result<Option<u64>, AnyError>,
    generations: &mut Vec<u64>,
) -> bool {
    match result {
        Ok(generation) => {
            generations.extend(generation);
            generation.is_some()
        }
        Err(e) => {
            log::warn!(
                "Dropping remote {kind} of {} -[{}]-> {}: {e:?}",
                link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                link.data.target
            );
            false
        }
    }
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
        // The generations this diff's own writes took: one per applied
        // write, since the remote path has no single-transaction step.
        let mut generations = Vec::new();
        // A tombstone ends only a Shared link (`monotonic::ends`), so its
        // removals need no Local check; `fold_into` publishes them.
        for retracted in &effects.retracted {
            match self
                .sparql_store
                .remove_link(&LinkExpression::from(retracted.clone()))
            {
                Ok(g) => generations.push(g),
                Err(e) => {
                    log::warn!("Failed to remove retracted link from SPARQL store: {:?}", e)
                }
            }
        }
        let removals = diff
            .removals
            .into_iter()
            .filter(|l| {
                applied(
                    "removal",
                    l,
                    self.sparql_store.remove_remote_link(l),
                    &mut generations,
                )
            })
            .collect::<Vec<_>>();
        let additions = diff
            .additions
            .into_iter()
            .filter(|l| !effects.covers(l))
            .filter(|l| {
                applied(
                    "addition",
                    l,
                    self.sparql_store.add_remote_link(l),
                    &mut generations,
                )
            })
            .collect::<Vec<_>>();
        let applied = PerspectiveDiff {
            additions,
            removals,
        };
        // The same rule as `write_link_diff`: a write that touches a shape
        // own link drops the cached shapes before it returns, so a peer that
        // never registered a class still reads it the moment its SHACL
        // arrives, and no await sits between the write and the clear.
        if touches_shape(
            applied
                .additions
                .iter()
                .chain(applied.removals.iter())
                .map(|l| &l.data)
                .chain(effects.retracted.iter().map(|l| &l.data)),
        ) {
            self.clear_shape_cache();
        }

        // The feed entry right after the writes — what was applied, plus the
        // retracted links the published diff reports — tagged with the
        // generations the writes took, so a draining tick can prove the feed
        // covers the store's movement here too.
        self.broadcast_commit_write(
            crate::perspectives::perspective_instance::subscription_triggers::writes_of_links(
                applied
                    .additions
                    .iter()
                    .map(|l| &l.data)
                    .chain(applied.removals.iter().map(|l| &l.data))
                    .chain(effects.retracted.iter().map(|l| &l.data)),
            ),
            generations,
        );

        Ok((applied, effects))
    }
}
