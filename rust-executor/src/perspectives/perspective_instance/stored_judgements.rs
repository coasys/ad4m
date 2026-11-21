//! The judgement the store holds for a link a write path just persisted.
//!
//! `persist_link_diff` fills the key map for a link's signer before the store
//! judges it, and a link this executor writes itself is checked as its own —
//! so a verdict derived at decoration time, before that discovery ran, can be
//! stale: a `cantCheck` with no person where the store wrote `valid` and the
//! person. The RPC reply and the link-added event carry the judgement the
//! store actually wrote.

use super::PerspectiveInstance;
use crate::perspectives::sparql_store::EvalStop;
use crate::types::DecoratedLinkExpression;

impl PerspectiveInstance {
    /// `links` with the judgement the store holds for each — the read-back for
    /// a write path's reply and link-added event after a persist. A link the
    /// store does not hold keeps the decoration it had.
    pub(crate) fn stored_judgements(
        &self,
        links: Vec<DecoratedLinkExpression>,
    ) -> Vec<DecoratedLinkExpression> {
        links
            .into_iter()
            .map(|link| {
                self.sparql_store
                    .get_link(
                        &EvalStop::none(),
                        &link.data.source,
                        link.data.predicate.as_deref(),
                        &link.data.target,
                        &link.author,
                        &link.timestamp,
                    )
                    .ok()
                    .flatten()
                    .unwrap_or(link)
            })
            .collect()
    }
}
