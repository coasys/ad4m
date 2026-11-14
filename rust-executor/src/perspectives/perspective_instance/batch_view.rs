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
//! store already holds. Cancelling the staged copy then does not remove the link — the
//! committed one still needs its removal staged. [`EffectiveLink`] keeps the two apart.

use super::PerspectiveInstance;
use crate::db::link_identity;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery};
use deno_core::error::AnyError;
use std::collections::HashSet;

/// A link on a batch's effective graph. `committed` says whether the store holds this
/// expression — if it does, removing the link means staging its removal through
/// `remove_link`; a staged-only link leaves through `cancel_batch_addition` instead.
///
/// The committed and the staged copies of one expression are separate entries here, so a
/// committed link the batch restaged shows up twice: the entries are ordered persisted
/// first, which lets a caller that dedups by [`link_identity`] take the committed one.
pub(crate) struct EffectiveLink {
    /// The expression as the store or the batch holds it.
    pub link: DecoratedLinkExpression,
    /// The store holds this expression.
    pub committed: bool,
}

impl EffectiveLink {
    fn committed(link: DecoratedLinkExpression) -> Self {
        EffectiveLink {
            link,
            committed: true,
        }
    }

    fn staged(link: DecoratedLinkExpression) -> Self {
        EffectiveLink {
            link,
            committed: false,
        }
    }
}

impl PerspectiveInstance {
    /// What `query` matches on the batch's effective graph: its `get_links` answer minus the
    /// links the batch has staged for removal, plus the batch's staged additions that match
    /// the query's `source`/`predicate`/`target` (dates and `limit` are the caller's
    /// business — the write core's queries carry none).
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

        // Committed first: a link the batch restaged over its own committed copy then has
        // its persisted entry ahead of the staged one.
        let mut out: Vec<EffectiveLink> = persisted
            .into_iter()
            .filter(|link| !removed.contains(&link_identity(&link.clone().into())))
            .map(EffectiveLink::committed)
            .collect();
        out.extend(
            batch
                .diff
                .additions
                .iter()
                .filter(|e| relevant(&e.data))
                .map(|expr| {
                    EffectiveLink::staged(DecoratedLinkExpression::from((
                        expr.clone(),
                        expr.status.clone().unwrap_or_default(),
                    )))
                }),
        );
        Ok(out)
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
