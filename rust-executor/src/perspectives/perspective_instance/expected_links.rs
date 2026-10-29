//! Committing a batch only if the links it expects still stand ([`ExpectedLinks`]):
//! a write that read a record checks, under `persist_lock`, that the record is still what it
//! read, so a write that landed in between is not overwritten.

use super::PerspectiveInstance;
use crate::agent::{create_signed_expression, AgentContext};
use crate::types::{
    DecoratedLinkExpression, DecoratedPerspectiveDiff, LinkExpression, LinkStatus, PerspectiveDiff,
};
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
#[cfg(test)]
use std::sync::atomic::Ordering;
use std::sync::PoisonError;

/// One stored link as [`ExpectedLinks`] compares it: author, timestamp, target.
type HeldLink = (String, String, String);

/// What a batch expects the store to hold when it commits
/// ([`PerspectiveInstance::commit_batch_if`]): the links from one source along one predicate, as
/// [`PerspectiveInstance::expect_links`] found them. A link written again with the same target is
/// a change too: it has a new timestamp.
#[derive(Debug, Clone)]
pub struct ExpectedLinks {
    source: String,
    predicate: String,
    links: Vec<HeldLink>,
}

/// [`PerspectiveInstance::commit_batch_if`] found a link it expected changed, and wrote nothing.
#[derive(Debug)]
pub struct LinksChanged;

impl std::fmt::Display for LinksChanged {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the links the batch expected changed before it was written")
    }
}

impl std::error::Error for LinksChanged {}

impl PerspectiveInstance {
    /// What the store holds from `source` along `predicate` now, for [`Self::commit_batch_if`]
    /// to expect.
    pub fn expect_links(&self, source: &str, predicate: &str) -> Result<ExpectedLinks, AnyError> {
        Ok(ExpectedLinks {
            source: source.to_string(),
            predicate: predicate.to_string(),
            links: self.links_held(source, predicate)?,
        })
    }

    fn links_held(&self, source: &str, predicate: &str) -> Result<Vec<HeldLink>, AnyError> {
        let mut links: Vec<HeldLink> = self
            .sparql_store
            .query_links(Some(source), Some(predicate), None, None, None, None)?
            .into_iter()
            .map(|l| (l.author, l.timestamp, l.data.target))
            .collect();
        links.sort();
        Ok(links)
    }

    /// [`Self::commit_batch`], if the store still holds each of `expected` when the batch is
    /// written: checked and written under `persist_lock`, so no write lands in between. If one
    /// changed, the batch is dropped unwritten, nothing is sent to the link language, and the
    /// error is [`LinksChanged`].
    pub async fn commit_batch_if(
        &mut self,
        batch_uuid: String,
        context: &AgentContext,
        expected: &[ExpectedLinks],
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        //let commit_start = std::time::Instant::now();
        //log::info!("🔄 BATCH COMMIT: Starting batch commit for batch_uuid: {}", batch_uuid);
        //let batch_retrieval_start = std::time::Instant::now();

        // Get the diff without holding lock during the entire operation
        let diff = {
            let mut batch_store = self.batch_store.write().await;

            match batch_store.remove(&batch_uuid) {
                Some(batch) => batch.diff,
                None => return Err(anyhow!("No batch found with given UUID")),
            }
        };
        #[cfg(test)]
        if self.fail_next_commit.swap(false, Ordering::SeqCst) {
            drop(diff);
            return Err(anyhow!("injected fault (test seam): commit_batch failed"));
        }
        // Sign the additions first: a flag queued in this batch counts for its
        // removals, as it will on every peer that receives both in one diff,
        // and a flag counts only once its signature verifies.
        let signed_additions = diff
            .additions
            .into_iter()
            .map(|link| {
                let status = link.status.unwrap_or(LinkStatus::Shared);
                let signed_expr = create_signed_expression(link.data.normalize(), context)?;
                let mut stored = LinkExpression::from(signed_expr);
                stored.status = Some(status);
                Ok(stored)
            })
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let shared_additions: Vec<LinkExpression> = signed_additions
            .iter()
            .filter(|l| l.status == Some(LinkStatus::Shared))
            .cloned()
            .collect();
        // Backstop for a removal queued past the refusing entry points; the
        // batch is dropped whole, as on any other commit error.
        let stored_declared = self.monotonic_declared().await?;
        let declared = stored_declared.with_incoming(&shared_additions);
        for link in &diff.removals {
            let status = link.status.as_ref().unwrap_or(&LinkStatus::Shared);
            crate::perspectives::monotonic::refuse_monotonic_removal(link, status, &declared)?;
        }

        //log::info!("🔄 BATCH COMMIT: Retrieved batch diff in {:?} - {} additions, {} removals",
        //    batch_retrieval_start.elapsed(), diff.additions.len(), diff.removals.len());

        //let processing_start = std::time::Instant::now();
        let mut shared_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };
        let mut local_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };

        let mut persist_diff = PerspectiveDiff::empty();

        // Process additions
        for stored in signed_additions {
            let status = stored.status.clone().unwrap_or(LinkStatus::Shared);
            persist_diff.additions.push(stored.clone());
            let decorated = DecoratedLinkExpression::from((stored, status.clone()));

            match status {
                LinkStatus::Shared => shared_diff.additions.push(decorated),
                LinkStatus::Local => local_diff.additions.push(decorated),
            }
        }

        // Process removals
        for link in diff.removals {
            let status = link.status.clone().unwrap_or(LinkStatus::Shared);
            persist_diff.removals.push(link.clone());
            let decorated = DecoratedLinkExpression::from((link, status.clone()));
            match status {
                LinkStatus::Shared => shared_diff.removals.push(decorated),
                LinkStatus::Local => local_diff.removals.push(decorated),
            }
        }

        //log::info!("🔄 BATCH COMMIT: Link processing took {:?} - shared: {} add/{} rem, local: {} add/{} rem",
        //    processing_start.elapsed(),
        //    shared_diff.additions.len(), shared_diff.removals.len(),
        //    local_diff.additions.len(), local_diff.removals.len());

        // The shared changes go to the link language, if there is one.
        let shared_changed = !shared_diff.additions.is_empty() || !shared_diff.removals.is_empty();
        let link_language_diff = if shared_changed && self.has_link_language().await {
            Some(PerspectiveDiff {
                additions: shared_diff
                    .additions
                    .iter()
                    .map(|l| l.clone().into())
                    .collect(),
                removals: shared_diff
                    .removals
                    .iter()
                    .map(|l| l.clone().into())
                    .collect(),
            })
        } else {
            None
        };

        // Create combined diff for prolog update, SPARQL store update, and return value
        let mut combined_diff = DecoratedPerspectiveDiff {
            additions: [shared_diff.additions.clone(), local_diff.additions.clone()].concat(),
            removals: [shared_diff.removals.clone(), local_diff.removals.clone()].concat(),
        };
        let changed = !combined_diff.additions.is_empty() || !combined_diff.removals.is_empty();

        {
            let _persisting = self
                .persist_lock
                .lock()
                .unwrap_or_else(PoisonError::into_inner);
            for e in expected {
                if self.links_held(&e.source, &e.predicate)? != e.links {
                    return Err(LinksChanged.into());
                }
            }
            // Commit to link language (SPARQL store is updated right after).
            // Tombstone removals stay out of this diff: they are this replica's
            // store effect only.
            if let Some(diff) = &link_language_diff {
                self.spawn_commit_and_handle_error(diff);
            }
            // The SPARQL store must be updated first so any subscription re-checks
            // triggered by update_prolog_engines() see the committed batch state.
            if changed {
                self.write_link_diff(&persist_diff, &stored_declared)?
                    .fold_into(&mut combined_diff);
            }
        }

        // Only update subscription engines when there are changes.
        if changed {
            // Queue the Prolog fact update and the notification + live-query checks
            self.update_prolog_engines(combined_diff.clone()).await;

            // Publish link-added/-removed/-updated to WS subscribers.
            // The single-mutation paths (add_link / link_mutations /
            // diff_from_link_language) publish their diff synchronously before
            // spawning the prolog update, and spawn_prolog_facts_update
            // deliberately skips publishing to avoid double-delivery on those
            // paths. commit_batch never went through that publish step, so
            // batched diffs were never reaching WS subscribers — that's what
            // this call restores.
            self.pubsub_publish_diff(&combined_diff).await;
        }

        //log::info!("🔄 BATCH COMMIT: Total batch commit took {:?}", commit_start.elapsed());

        // Return combined diff
        Ok(combined_diff)
    }
}
