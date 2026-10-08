//! Which predicates a perspective treats as containment.
//!
//! `ad4m://has_child` is containment everywhere. A perspective's installed
//! schema may declare more, as standard RDF:
//!
//! ```text
//! <app://child> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://has_child>
//! ```
//!
//! An app can have its own containment predicate, or more than one kind of
//! containment that it must keep apart — a document's ordered parts, and the
//! posts in a feed — and one predicate cannot hold both. Declaring each as a
//! sub-property of `has_child` lets the parts of the
//! executor that read containment for context (the waker's parent lookup, the
//! MCP `parent=` reads, child subscriptions) follow every kind, while the app's
//! own data keeps them distinct.
//!
//! ## Only declarations the perspective's authors made
//!
//! In a neighbourhood any member can write a link, and one `subPropertyOf`
//! reclassifies every link of a predicate at once. So a declaration counts only
//! when it is signed by the perspective's owner or the neighbourhood's author —
//! the same rule that decides which SDNA is installed (`sdna::get_sdna_facts`).
//! A stray declaration from anybody else is ignored.
//!
//! ## Resolved once, as concrete IRIs
//!
//! The set is resolved when first asked for, cached on the perspective, and
//! dropped whenever a declaration is written or removed. Callers write the
//! concrete IRIs into their queries rather than a variable predicate, so a
//! subscription still narrows to the predicates it reads.
//!
//! ## Only containment, and only reads
//!
//! A declaration means containment and nothing else: it does not extend the
//! flow default edge, the ORM's default relation, or the shared query scope
//! (`Scope::Raw`), each of which reads `has_child` by name on purpose. Writes
//! (`add_child`, `parent=` on create) still write `has_child`.

use super::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery, PerspectiveDiff};
use std::sync::Arc;

/// The containment predicate every perspective has.
pub const HAS_CHILD: &str = "ad4m://has_child";

/// How a perspective declares another predicate to be containment.
pub const SUB_PROPERTY_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subPropertyOf";

/// The containment predicates from a perspective's declaration links:
/// `has_child` first, then each declared predicate once, in a stable order.
///
/// `links` are the `subPropertyOf → has_child` links; `trusted` the DIDs whose
/// declarations count. A declaration with an invalid proof, from anybody else,
/// or naming something that is not a usable IRI is ignored.
pub fn resolve_containment(links: &[DecoratedLinkExpression], trusted: &[String]) -> Vec<String> {
    let mut declared: Vec<String> = links
        .iter()
        .filter(|link| link.proof.valid.unwrap_or(false) && trusted.contains(&link.author))
        .filter(|link| {
            link.data.predicate.as_deref() == Some(SUB_PROPERTY_OF) && link.data.target == HAS_CHILD
        })
        .map(|link| link.data.source.clone())
        .filter(|predicate| {
            predicate != HAS_CHILD && super::model_query::is_safe_iri_target(predicate)
        })
        .collect();
    declared.sort();
    declared.dedup();
    let mut all = vec![HAS_CHILD.to_string()];
    all.extend(declared);
    all
}

/// Whether a link declares (or withdraws) containment — the writes that make
/// a cached set stale.
pub fn declares_containment(predicate: Option<&str>, target: &str) -> bool {
    predicate == Some(SUB_PROPERTY_OF) && target == HAS_CHILD
}

impl PerspectiveInstance {
    /// The predicates this perspective treats as containment: `has_child`,
    /// then each one its authors declared. Resolved once and cached.
    pub async fn containment_predicates(&self) -> Arc<Vec<String>> {
        if let Some(cached) = self.containment_cache.read().unwrap().clone() {
            return cached;
        }
        let trusted = {
            let persisted = self.persisted.lock().await;
            let mut trusted = vec![persisted
                .get_primary_owner()
                .unwrap_or_else(crate::agent::did)];
            if let Some(author) = persisted.neighbourhood.as_ref().map(|n| n.author.clone()) {
                trusted.push(author);
            }
            trusted
        };
        let links = self
            .get_links(&LinkQuery {
                predicate: Some(SUB_PROPERTY_OF.to_string()),
                target: Some(HAS_CHILD.to_string()),
                ..Default::default()
            })
            .await
            .unwrap_or_default();
        let resolved = Arc::new(resolve_containment(&links, &trusted));
        *self.containment_cache.write().unwrap() = Some(resolved.clone());
        resolved
    }

    /// A model query over what one item contains, through every containment
    /// predicate this perspective has — the executor's own `parent=` reads.
    /// The query is the client's JSON; the parent scope is set here, because
    /// it is one no client can spell.
    pub async fn model_query_contained_by(
        &self,
        class_name: &str,
        query_json: &str,
        parent_id: &str,
    ) -> Result<String, deno_core::anyhow::Error> {
        let predicates = self.containment_predicates().await.as_ref().clone();
        let scope = super::model_query::Scope::Contained {
            id: parent_id.to_string(),
            predicates,
        };
        self.model_query_within(
            class_name,
            query_json,
            super::perspective_instance::MODEL_QUERY_SHAPE_WAIT,
            Some(scope),
        )
        .await
    }

    /// Drop the cached containment set, so the next read resolves it again.
    pub fn invalidate_containment(&self) {
        *self.containment_cache.write().unwrap() = None;
    }

    /// Drop the cached set if a diff adds or removes a declaration.
    pub(super) fn note_containment_writes(&self, diff: &PerspectiveDiff) {
        let touches = diff
            .additions
            .iter()
            .chain(diff.removals.iter())
            .any(|link| declares_containment(link.data.predicate.as_deref(), &link.data.target));
        if touches {
            self.invalidate_containment();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{ExpressionProof, Link, LinkExpression};

    fn declaration(source: &str, author: &str, valid: bool) -> DecoratedLinkExpression {
        let mut link = DecoratedLinkExpression::from((
            LinkExpression {
                author: author.to_string(),
                timestamp: "2026-10-08T10:00:00.000Z".to_string(),
                data: Link {
                    source: source.to_string(),
                    predicate: Some(SUB_PROPERTY_OF.to_string()),
                    target: HAS_CHILD.to_string(),
                },
                proof: ExpressionProof::default(),
                status: None,
            },
            crate::types::LinkStatus::Shared,
        ));
        link.proof.valid = Some(valid);
        link
    }

    #[test]
    fn has_child_alone_without_declarations() {
        assert_eq!(
            resolve_containment(&[], &["did:owner".to_string()]),
            vec![HAS_CHILD.to_string()]
        );
    }

    #[test]
    fn counts_declarations_from_the_perspectives_authors_only() {
        let trusted = vec!["did:owner".to_string(), "did:nh-author".to_string()];
        let links = vec![
            declaration("app://child", "did:owner", true),
            declaration("app://comment", "did:nh-author", true),
            declaration("app://stray", "did:member", true),
            declaration("app://forged", "did:owner", false),
            declaration("app://child", "did:nh-author", true),
            declaration("not an iri", "did:owner", true),
        ];
        assert_eq!(
            resolve_containment(&links, &trusted),
            vec![
                HAS_CHILD.to_string(),
                "app://child".to_string(),
                "app://comment".to_string()
            ]
        );
    }

    #[test]
    fn recognises_a_declaration_link() {
        assert!(declares_containment(Some(SUB_PROPERTY_OF), HAS_CHILD));
        assert!(!declares_containment(Some(SUB_PROPERTY_OF), "app://other"));
        assert!(!declares_containment(Some(HAS_CHILD), HAS_CHILD));
    }
}
