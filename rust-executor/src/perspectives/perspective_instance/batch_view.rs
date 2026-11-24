//! A batch's effective view of the graph: the committed links plus the batch's staged
//! additions, minus its staged removals. [`PerspectiveInstance::get_links`] reads the store
//! only, so inside an uncommitted batch it still answers as the batch began; reads that must
//! see the batch's own staged work go through [`PerspectiveInstance::get_links_effective`]
//! instead — the validated write core (`perspectives::instances`) does, for collection
//! membership checks and the remove cascade.
//!
//! Removals in a batch's diff always name the exact stored expression they drop (batched
//! `remove_link` / `remove_links` / `update_link` resolve the caller's expression against the
//! store first), so a staged removal hides exactly one committed link here — matched by
//! [`crate::db::link_identity`], the same identity `merge_pending_diffs` dedups by.
//!
//! A link that exists only inside its batch is "removed" by dropping the staged addition:
//! [`PerspectiveInstance::remove_link`] insists on a stored expression, so
//! [`PerspectiveInstance::cancel_batch_addition`] is the way — the same pruning
//! `execute_commands` does for `RemoveLink` and `SetSingleTarget` in `perspective_instance.rs`.
//! The same signed expression can sit in both places at once, though:
//! `perspective.addLinkExpression` stages the caller's expression verbatim, including one the
//! store already holds, or one it already staged. Cancelling the staged copies then does not
//! remove the link — the committed one still needs its removal staged — and two staged
//! copies are still one link. [`EffectiveLink`] folds one identity to one entry and keeps
//! the two presences apart.

use super::{PerspectiveInstance, TimestampedBatch};
use crate::db::link_identity;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery, PerspectiveDiff};
use deno_core::error::AnyError;
use std::collections::{HashMap, HashSet};

/// A link on a batch's effective graph — one entry per [`link_identity`], however many
/// copies of it were staged. `committed` and `staged` are not exclusive:
/// `perspective.addLinkExpression` can stage the exact signed expression the store holds,
/// so removing the link can mean dropping staged copies AND staging the committed one's
/// removal.
pub(crate) struct EffectiveLink {
    /// The expression; the committed spelling when the store holds it.
    pub link: DecoratedLinkExpression,
    /// The store holds this expression: its removal is staged through `remove_link`.
    pub committed: bool,
    /// The batch holds staged copies of it: they drop through `cancel_batch_addition`.
    pub staged: bool,
}

/// An open batch's staged work at one moment: what [`PerspectiveInstance::fork_batch`] copied,
/// for [`PerspectiveInstance::merge_batch`] to tell what the fork changed.
pub(crate) struct BatchSnapshot {
    diff: PerspectiveDiff,
    updates: Vec<(LinkExpression, LinkExpression)>,
}

/// How [`PerspectiveInstance::merge_batch`] ended.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum MergeOutcome {
    /// The fork's work is now the target's.
    Merged,
    /// The target batch is gone (committed, discarded or swept).
    Gone,
    /// The target batch changed since the fork: nothing was merged.
    Conflict,
}

/// Whether `now` holds the same items as `base`, by `key`, counting copies.
fn same_items<T, K: std::hash::Hash + Eq>(base: &[T], now: &[T], key: impl Fn(&T) -> K) -> bool {
    let mut counts: HashMap<K, isize> = HashMap::new();
    for item in base {
        *counts.entry(key(item)).or_default() += 1;
    }
    for item in now {
        *counts.entry(key(item)).or_default() -= 1;
    }
    counts.values().all(|n| *n == 0)
}

impl EffectiveLink {
    fn committed(link: DecoratedLinkExpression) -> Self {
        EffectiveLink {
            link,
            committed: true,
            staged: false,
        }
    }
}

impl PerspectiveInstance {
    /// What `query` matches on the batch's effective graph: its `get_links` answer minus the
    /// links the batch has staged for removal, plus the batch's staged additions that match
    /// the query's `source`/`predicate`/`target` (dates and `limit` are the caller's
    /// business — the write core's queries carry none), deduplicated to one
    /// [`EffectiveLink`] per link identity.
    ///
    /// The persisted answer as it is when `batch_id` is absent or names no open batch — every
    /// caller outside a transaction.
    pub(crate) async fn get_links_effective(
        &self,
        query: &LinkQuery,
        batch_id: Option<&str>,
    ) -> Result<Vec<EffectiveLink>, AnyError> {
        let persisted = self.get_links(query).await?;
        let Some(bid) = batch_id else {
            return Ok(persisted
                .into_iter()
                .map(EffectiveLink::committed)
                .collect());
        };
        let batches = self.batch_store.read().await;
        let Some(batch) = batches.get(bid) else {
            return Ok(persisted
                .into_iter()
                .map(EffectiveLink::committed)
                .collect());
        };

        let relevant = |link: &Link| {
            query.source.as_ref().is_none_or(|s| *s == link.source)
                && query
                    .predicate
                    .as_ref()
                    .is_none_or(|p| link.predicate.as_deref() == Some(p.as_str()))
                && query.target.as_ref().is_none_or(|t| *t == link.target)
        };

        // The exact stored expressions this batch has already dropped.
        let removed: HashSet<_> = batch.diff.removals.iter().map(link_identity).collect();

        // One entry per identity — a restaged committed link counts once, flagged both
        // ways — with committed entries first and staged additions in their own order.
        let mut out: Vec<EffectiveLink> = Vec::new();
        let mut index: HashMap<_, usize> = HashMap::new();
        for link in persisted {
            let identity = link_identity(&link.clone().into());
            if removed.contains(&identity) {
                continue;
            }
            index.insert(identity, out.len());
            out.push(EffectiveLink::committed(link));
        }
        for expr in batch.diff.additions.iter().filter(|e| relevant(&e.data)) {
            match index.get(&link_identity(expr)) {
                Some(&i) => out[i].staged = true,
                None => {
                    index.insert(link_identity(expr), out.len());
                    out.push(EffectiveLink {
                        link: DecoratedLinkExpression::from((
                            expr.clone(),
                            expr.status.clone().unwrap_or_default(),
                        )),
                        committed: false,
                        staged: true,
                    });
                }
            }
        }
        Ok(out)
    }

    /// Open a private batch seeded with a copy of `batch_id`'s staged work, so the reads of a
    /// `perspective.mutate` call into `batch_id` see the caller's work plus its own, and the
    /// call's writes touch only the private batch. Answers the private batch's id and the
    /// copy it was seeded with; `None` when `batch_id` is not open.
    pub(crate) async fn fork_batch(&self, batch_id: &str) -> Option<(String, BatchSnapshot)> {
        let mut batches = self.batch_store.write().await;
        let source = batches.get(batch_id)?;
        let base = BatchSnapshot {
            diff: source.diff.clone(),
            updates: source.updates.clone(),
        };
        let fork = TimestampedBatch {
            diff: base.diff.clone(),
            updates: base.updates.clone(),
            created_at: source.created_at,
        };
        let fork_id = uuid::Uuid::new_v4().to_string();
        batches.insert(fork_id.clone(), fork);
        Some((fork_id, base))
    }

    /// Merge what the private batch `fork_id` staged since `base` into `into`, and drop the
    /// private batch: one step under the batch store's lock. The merge is conditional: when
    /// `into` is not exactly `base` any more (its additions, removals or updates changed since
    /// the fork), the fork's result may no longer follow from it (two calls replacing the same
    /// single-valued relation, a raw update of a link the fork replaced), so nothing is merged
    /// and the fork is dropped.
    pub(crate) async fn merge_batch(
        &self,
        into: &str,
        fork_id: &str,
        base: BatchSnapshot,
    ) -> MergeOutcome {
        let mut batches = self.batch_store.write().await;
        let Some(fork) = batches.remove(fork_id) else {
            return MergeOutcome::Gone;
        };
        let Some(target) = batches.get_mut(into) else {
            return MergeOutcome::Gone;
        };
        let pair = |(old, new): &(LinkExpression, LinkExpression)| {
            (link_identity(old), link_identity(new))
        };
        if !(same_items(&base.diff.additions, &target.diff.additions, link_identity)
            && same_items(&base.diff.removals, &target.diff.removals, link_identity)
            && same_items(&base.updates, &target.updates, pair))
        {
            return MergeOutcome::Conflict;
        }
        // The target is `base`: the fork's staged work replaces it.
        target.diff = fork.diff;
        target.updates = fork.updates;
        MergeOutcome::Merged
    }

    /// Drop the staged additions that are `link` (by `link_identity`), returning whether any
    /// were staged. "Removing" a link that exists only inside its batch is cancelling the
    /// addition: the store has nothing to remove, so `remove_link` cannot take it.
    ///
    /// This only ever touches the batch's staged copies — a committed link with the same
    /// identity is the store's business: when the caller restaged a stored expression
    /// (`perspective.addLinkExpression` takes it verbatim), the committed copy still needs
    /// `remove_link`, and that is the caller's check, not this one's.
    pub(crate) async fn cancel_batch_addition(
        &self,
        batch_id: &str,
        link: &LinkExpression,
    ) -> bool {
        let mut batches = self.batch_store.write().await;
        let Some(batch) = batches.get_mut(batch_id) else {
            return false;
        };
        let staged = batch.diff.additions.len();
        batch
            .diff
            .additions
            .retain(|addition| link_identity(addition) != link_identity(link));
        staged != batch.diff.additions.len()
    }
}
