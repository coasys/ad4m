//! Which predicates a perspective treats as containment.
//!
//! `ad4m://has_child` is containment everywhere. A perspective's installed
//! schema may declare more, as a role (`declared_roles.rs`):
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
//! own data keeps them distinct. Who may declare, and how the set is cached,
//! is `declared_roles.rs`.
//!
//! ## Only containment
//!
//! A declaration means containment and nothing else: it does not extend the
//! flow default edge, the ORM's default relation, or the shared query scope
//! (`Scope::Raw`), each of which reads `has_child` by name on purpose.
//!
//! The executor's own writes (`add_child`, `parent=` on create) use the
//! declared predicate when exactly one is declared, so what a bot writes is
//! what the app reads; with none, or several, they write `has_child`
//! (`declared_roles::write_predicate`).

use super::perspective_instance::PerspectiveInstance;
use crate::types::DecoratedLinkExpression;
use std::sync::Arc;

pub use super::declared_roles::SUB_PROPERTY_OF;

/// The containment predicate every perspective has.
pub const HAS_CHILD: &str = "ad4m://has_child";

/// The containment predicates from a perspective's declaration links:
/// `has_child` first, then each declared predicate once, in a stable order.
pub fn resolve_containment(links: &[DecoratedLinkExpression], trusted: &[String]) -> Vec<String> {
    with_has_child(super::declared_roles::resolve_declared(
        links, trusted, HAS_CHILD,
    ))
}

fn with_has_child(declared: Vec<String>) -> Vec<String> {
    let mut all = vec![HAS_CHILD.to_string()];
    all.extend(declared);
    all
}

impl PerspectiveInstance {
    /// The predicates this perspective treats as containment: `has_child`,
    /// then each one its authors declared.
    pub async fn containment_predicates(&self) -> Arc<Vec<String>> {
        let declared = self.declared_for(HAS_CHILD).await;
        Arc::new(with_has_child(declared.as_ref().clone()))
    }

    /// The predicate the executor's own containment writes use: the one
    /// declared, when there is exactly one, else `has_child`.
    pub async fn containment_write_predicate(&self) -> String {
        super::declared_roles::write_predicate(HAS_CHILD, &self.declared_for(HAS_CHILD).await)
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::declared_roles::tests::declaration;

    #[test]
    fn has_child_alone_without_declarations() {
        assert_eq!(
            resolve_containment(&[], &["did:owner".to_string()]),
            vec![HAS_CHILD.to_string()]
        );
    }

    #[test]
    fn has_child_first_then_trusted_declarations() {
        let trusted = vec!["did:owner".to_string()];
        let links = vec![
            declaration("app://comment", HAS_CHILD, "did:owner", true),
            declaration("app://child", HAS_CHILD, "did:owner", true),
            declaration("app://stray", HAS_CHILD, "did:member", true),
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
}
