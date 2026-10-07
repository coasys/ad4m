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

#[cfg(doc)]
use super::sparql_store::SparqlStore;
use crate::types::LinkExpression;

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
