//! Roles a perspective declares its own predicates to play.
//!
//! The executor reads a few things by role rather than by an app's predicate:
//! containment (`containment.rs`), message text (`message_text.rs`). Each role
//! has a base IRI, and a perspective's installed schema says which of its
//! predicates play it, as standard RDF:
//!
//! ```text
//! <app://child> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <ad4m://has_child>
//! ```
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
//! Each role's set is resolved when first asked for, cached on the perspective,
//! and every set is dropped whenever any `subPropertyOf` link is written or
//! removed. Callers write the concrete IRIs into their queries rather than a
//! variable predicate, so a subscription still narrows to the predicates it
//! reads.

use super::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, LinkQuery, PerspectiveDiff};
use std::sync::Arc;

/// How a perspective declares a predicate to play a role.
pub const SUB_PROPERTY_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subPropertyOf";

/// The predicates declared to play `role`, from a perspective's declaration
/// links: each once, in a stable order, never `role` itself.
///
/// `links` are the `subPropertyOf → role` links; `trusted` the DIDs whose
/// declarations count. A declaration with an invalid proof, from anybody else,
/// or naming something that is not a usable IRI is ignored.
pub fn resolve_declared(
    links: &[DecoratedLinkExpression],
    trusted: &[String],
    role: &str,
) -> Vec<String> {
    let mut declared: Vec<String> = links
        .iter()
        .filter(|link| link.proof.valid.unwrap_or(false) && trusted.contains(&link.author))
        .filter(|link| {
            link.data.predicate.as_deref() == Some(SUB_PROPERTY_OF) && link.data.target == role
        })
        .map(|link| link.data.source.clone())
        .filter(|predicate| predicate != role && super::model_query::is_safe_iri_target(predicate))
        .collect();
    declared.sort();
    declared.dedup();
    declared
}

/// The predicate a write in `base`'s role uses: the one predicate declared to
/// play it, when there is exactly one, else `base` itself.
///
/// Reads follow every declared predicate; a write has to pick one. With a
/// single declaration the app has said which predicate the role is. With
/// several it has said there are distinctions the executor cannot choose
/// between, so a write falls back to the role itself rather than guess.
pub fn write_predicate(base: &str, declared: &[String]) -> String {
    match declared {
        [only] => only.clone(),
        _ => base.to_string(),
    }
}

/// Whether a link declares (or withdraws) a role — the writes that make the
/// cached sets stale.
pub fn declares_role(predicate: Option<&str>) -> bool {
    predicate == Some(SUB_PROPERTY_OF)
}

impl PerspectiveInstance {
    /// The predicates this perspective's authors declared to play `role`.
    /// Resolved once per role and cached.
    pub async fn declared_for(&self, role: &str) -> Arc<Vec<String>> {
        if let Some(cached) = self.declared_roles.read().unwrap().get(role).cloned() {
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
                target: Some(role.to_string()),
                ..Default::default()
            })
            .await
            .unwrap_or_default();
        let resolved = Arc::new(resolve_declared(&links, &trusted, role));
        self.declared_roles
            .write()
            .unwrap()
            .insert(role.to_string(), resolved.clone());
        resolved
    }

    /// Drop every cached role set, so the next read resolves it again.
    pub fn invalidate_declared_roles(&self) {
        self.declared_roles.write().unwrap().clear();
    }

    /// Drop the cached sets if a diff adds or removes a declaration.
    pub(super) fn note_role_writes(&self, diff: &PerspectiveDiff) {
        let touches = diff
            .additions
            .iter()
            .chain(diff.removals.iter())
            .any(|link| declares_role(link.data.predicate.as_deref()));
        if touches {
            self.invalidate_declared_roles();
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::types::{ExpressionProof, Link, LinkExpression};

    pub(crate) fn declaration(
        source: &str,
        role: &str,
        author: &str,
        valid: bool,
    ) -> DecoratedLinkExpression {
        let mut link = DecoratedLinkExpression::from((
            LinkExpression {
                author: author.to_string(),
                timestamp: "2026-10-08T10:00:00.000Z".to_string(),
                data: Link {
                    source: source.to_string(),
                    predicate: Some(SUB_PROPERTY_OF.to_string()),
                    target: role.to_string(),
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
    fn counts_declarations_from_the_perspectives_authors_only() {
        let trusted = vec!["did:owner".to_string(), "did:nh-author".to_string()];
        let role = "ad4m://some_role";
        let links = vec![
            declaration("app://b", role, "did:owner", true),
            declaration("app://a", role, "did:nh-author", true),
            declaration("app://stray", role, "did:member", true),
            declaration("app://forged", role, "did:owner", false),
            declaration("app://b", role, "did:nh-author", true),
            declaration("not an iri", role, "did:owner", true),
            declaration(role, role, "did:owner", true),
            declaration("app://other-role", "ad4m://another", "did:owner", true),
        ];
        assert_eq!(
            resolve_declared(&links, &trusted, role),
            vec!["app://a".to_string(), "app://b".to_string()]
        );
    }

    #[test]
    fn writes_use_the_single_declared_predicate_else_the_role() {
        let role = "ad4m://some_role";
        assert_eq!(write_predicate(role, &[]), role);
        assert_eq!(
            write_predicate(role, &["app://one".to_string()]),
            "app://one"
        );
        assert_eq!(
            write_predicate(role, &["app://a".to_string(), "app://b".to_string()]),
            role
        );
    }

    #[test]
    fn recognises_a_declaration_link() {
        assert!(declares_role(Some(SUB_PROPERTY_OF)));
        assert!(!declares_role(Some("ad4m://has_child")));
        assert!(!declares_role(None));
    }
}
