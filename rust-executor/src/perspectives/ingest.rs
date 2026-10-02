//! What a link language's diff may change in this perspective (#1146).
//!
//! A diff from a neighbourhood is untrusted: the link language does not bind
//! `LinkExpression.author` to whoever committed it. `diff_from_link_language`
//! passes every diff through [`retain_ingestible`] before it is persisted,
//! decorated or published, so no reader (model_query, Prolog, raw SPARQL,
//! `query_links`, flows) sees what it drops:
//!
//! ```text
//! link language ─► dedup ─► retain_ingestible ─► persist_link_diff ─► prolog / pubsub
//!                           ├ addition: proof verifies against its author
//!                           └ addition or removal: link not stored as Local
//! ```
//!
//! Removals of Shared links are not checked for authority here.

use super::sparql_store::SparqlStore;
use crate::types::LinkExpression;

/// Drop from a remote diff every addition whose proof does not verify
/// against its own `author`, and every addition or removal of a link this
/// store holds as `Local`. A status that cannot be read counts as `Local`:
/// the remote change is dropped rather than applied blind.
pub(crate) fn retain_ingestible(
    store: &SparqlStore,
    additions: &mut Vec<LinkExpression>,
    removals: &mut Vec<LinkExpression>,
) {
    let not_local = |link: &LinkExpression| match store.is_stored_local(link) {
        Ok(local) => !local,
        Err(e) => {
            log::warn!(
                "Dropping remote change to {} -[{}]-> {}: stored status unreadable: {e:?}",
                link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                link.data.target
            );
            false
        }
    };
    additions.retain(|link| {
        if !link.compute_proof_valid() {
            log::warn!(
                "Dropping remote link whose proof does not verify against its author {}: {} -[{}]-> {}",
                link.author,
                link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                link.data.target
            );
            return false;
        }
        not_local(link)
    });
    removals.retain(|link| not_local(link));
}
