//! Committing a batch only if the links it expects still stand ([`ExpectedLinks`]):
//! a write that read a record checks, under `persist_lock`, that the record is still what it
//! read, so a write that landed in between is not overwritten.

use super::{PerspectiveInstance, TimestampedBatch};
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

        // Take the batch out of the store without holding the lock for the commit
        let TimestampedBatch {
            mut diff, updates, ..
        } = {
            let mut batch_store = self.batch_store.write().await;

            match batch_store.remove(&batch_uuid) {
                Some(batch) => batch,
                None => return Err(anyhow!("No batch found with given UUID")),
            }
        };
        #[cfg(test)]
        if self.fail_next_commit.swap(false, Ordering::SeqCst) {
            drop(diff);
            return Err(anyhow!("injected fault (test seam): commit_batch failed"));
        }
        // Backstop for a removal queued past the refusing entry points; the
        // batch is dropped whole, as on any other commit error.
        for link in &diff.removals {
            crate::perspectives::monotonic::refuse_monotonic_removal(
                link,
                link.status.as_ref().unwrap_or(&LinkStatus::Shared),
            )?;
        }

        // An update whose halves are both still staged commits as an update.
        // Later work in the batch may have dropped one half; then the other
        // stays a plain addition or removal.
        let updates: Vec<(LinkExpression, LinkExpression)> = updates
            .into_iter()
            .filter(|(old, new)| {
                let (Some(r), Some(a)) = (
                    diff.removals.iter().position(|l| l == old),
                    diff.additions.iter().position(|l| l == new),
                ) else {
                    return false;
                };
                diff.removals.remove(r);
                diff.additions.remove(a);
                true
            })
            .collect();

        let mut persist_diff = PerspectiveDiff::empty();
        // Only Shared links reach the link language.
        let mut shared_diff = PerspectiveDiff::empty();
        // Additions and removals, published as ADDED / REMOVED.
        let mut published = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };

        for link in diff.additions {
            let status = link.status.unwrap_or(LinkStatus::Shared);
            let signed_expr = create_signed_expression(link.data.normalize(), context)?;
            let mut stored = LinkExpression::from(signed_expr);
            stored.status = Some(status.clone());
            persist_diff.additions.push(stored.clone());
            if status == LinkStatus::Shared {
                shared_diff.additions.push(stored.clone());
            }
            published
                .additions
                .push(DecoratedLinkExpression::from((stored, status)));
        }

        for link in diff.removals {
            let status = link.status.clone().unwrap_or(LinkStatus::Shared);
            persist_diff.removals.push(link.clone());
            if status == LinkStatus::Shared {
                shared_diff.removals.push(link.clone());
            }
            published
                .removals
                .push(DecoratedLinkExpression::from((link, status)));
        }

        // Both halves of an update carry the stored status `update_link` gave them.
        let mut combined_diff = published.clone();
        let mut updated = Vec::new();
        for (old, new) in updates {
            let status = new.status.clone().unwrap_or_default();
            persist_diff.removals.push(old.clone());
            persist_diff.additions.push(new.clone());
            if status == LinkStatus::Shared {
                shared_diff.removals.push(old.clone());
                shared_diff.additions.push(new.clone());
            }
            let old = DecoratedLinkExpression::from((old, status.clone()));
            let new = DecoratedLinkExpression::from((new, status));
            combined_diff.removals.push(old.clone());
            combined_diff.additions.push(new.clone());
            updated.push((old, new));
        }

        // The shared changes go to the link language, if there is one.
        let shared_changed = !shared_diff.additions.is_empty() || !shared_diff.removals.is_empty();
        let link_language_diff = if shared_changed && self.has_link_language().await {
            Some(shared_diff)
        } else {
            None
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
                let effects = self.write_link_diff(&persist_diff)?;
                effects.fold_into(&mut combined_diff);
                effects.fold_into(&mut published);
            }
        }

        // Only update subscription engines when there are changes.
        if changed {
            // Queue the Prolog fact update and the notification + live-query checks
            self.update_prolog_engines(combined_diff.clone()).await;

            // Publish link-added/-removed/-updated to WS subscribers. The
            // single-mutation paths publish their own diff, and
            // spawn_prolog_facts_update deliberately does not (no double
            // delivery there), so a batch must publish here or its links never
            // reach WS subscribers. Updates go out as `link-updated`, not as a
            // removal plus an addition.
            self.pubsub_publish_diff(&published).await;
            self.publish_link_updates(&updated).await;
        }

        //log::info!("🔄 BATCH COMMIT: Total batch commit took {:?}", commit_start.elapsed());

        // Return combined diff
        Ok(combined_diff)
    }
}
