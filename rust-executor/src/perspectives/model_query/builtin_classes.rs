//! Built-in abstract classes (#1446).
//!
//! A built-in abstract class is declared here, as SHACL JSON beside the flow
//! classes in `hardwired_sdna/`, and is never written into a perspective. A
//! query names it by its URI (`ad4m://TextContent`); its shape comes from
//! this module, so an app class registered under the same name cannot take
//! it over. Aliases name it by its local name (`alias: { TextContent: "text" }`),
//! the same key an app's own abstract class would use.
//!
//! The SDK mirrors each one as an `@Model({ abstract: true })` class that
//! queries by the URI and is never registered (`core/src/perspectives/TextContent.ts`;
//! a test compares the two).

use super::shape::load_shape_from_store;
use super::types::ModelShape;
use crate::perspectives::shacl_parser::parse_shacl_to_links;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{ExpressionProof, LinkExpression, LinkStatus};
use std::collections::HashMap;
use std::sync::{Arc, OnceLock};

/// `TextContent { text }`: text a view or an LLM can read. `text` may contain
/// HTML or Markdown.
pub const TEXT_CONTENT_URI: &str = "ad4m://TextContent";
pub(crate) const TEXT_CONTENT_SDNA: &str = include_str!("../hardwired_sdna/text_content.json");

/// `Container { children }`: what holds other records. A record is contained
/// in P when `P ad4m://has_child X` exists, whatever P is, or when P is an
/// instance of a class that conforms to `Container` and links X through that
/// class's `children` relation (see [`super::containment`]).
pub const CONTAINER_URI: &str = "ad4m://Container";
pub(crate) const CONTAINER_SDNA: &str = include_str!("../hardwired_sdna/container.json");

/// URI, name aliases use, SHACL JSON, whether it reads its own rows (see
/// [`Builtin::member`]).
const BUILTINS: &[(&str, &str, &str, bool)] = &[
    (TEXT_CONTENT_URI, "TextContent", TEXT_CONTENT_SDNA, false),
    (CONTAINER_URI, "Container", CONTAINER_SDNA, true),
];

/// A built-in abstract class.
#[derive(Debug)]
pub struct Builtin {
    /// The name aliases key it by.
    pub name: &'static str,
    pub shape: Arc<ModelShape>,
    /// The shape a query over the built-in reads beside its conforming
    /// classes, under the built-in's URI: `Container`'s holders that no class
    /// describes (`ad4m://self`, a neighbourhood root, a plain-string id).
    /// Its relation is required, so a row is a node with at least one link
    /// on its predicate; required rather than structural, because the
    /// structural tier is skipped under a parent scope, where every node in
    /// scope would then be a row. `None` for a built-in that has no rows of
    /// its own.
    pub member: Option<Arc<ModelShape>>,
}

/// The built-in abstract class with this URI, or `None` for any other name.
pub fn builtin(uri: &str) -> Option<&'static Builtin> {
    static BUILT: OnceLock<HashMap<&'static str, Builtin>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            BUILTINS
                .iter()
                .map(|(uri, name, sdna, reads_itself)| {
                    let shape = read_back(name, sdna);
                    let member = reads_itself.then(|| {
                        let mut member = shape.clone();
                        for p in &mut member.properties {
                            p.is_required = true;
                        }
                        Arc::new(member)
                    });
                    (
                        *uri,
                        Builtin {
                            name,
                            shape: Arc::new(shape),
                            member,
                        },
                    )
                })
                .collect()
        })
        .get(uri)
}

/// The shape a perspective would hold for `sdna`: written by the SHACL
/// writer into a scratch store and read back by the shape loader, so a
/// built-in is read exactly as a registered class is. The JSON is compiled
/// in and tested, so a failure here is a build defect.
fn read_back(name: &str, sdna: &str) -> ModelShape {
    let store = SparqlStore::new(None).expect("an in-memory store for a built-in class");
    for link in parse_shacl_to_links(sdna, name)
        .unwrap_or_else(|e| panic!("built-in class {name}: invalid SHACL: {e}"))
    {
        store
            .add_link(&LinkExpression {
                author: String::new(),
                timestamp: String::new(),
                data: link,
                proof: ExpressionProof {
                    key: String::new(),
                    signature: String::new(),
                },
                status: Some(LinkStatus::Shared),
            })
            .unwrap_or_else(|e| panic!("built-in class {name}: {e}"));
    }
    load_shape_from_store(&store, name)
        .unwrap_or_else(|e| panic!("built-in class {name}: shape did not load: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_content_is_an_abstract_class_with_text() {
        let text_content = builtin(TEXT_CONTENT_URI).expect("TextContent is built in");
        assert_eq!(text_content.name, "TextContent");
        assert_eq!(text_content.shape.target_class, TEXT_CONTENT_URI);
        let names: Vec<&str> = text_content
            .shape
            .properties
            .iter()
            .map(|p| p.name.as_str())
            .collect();
        assert_eq!(names, vec!["text"]);
        let sdna: serde_json::Value = serde_json::from_str(TEXT_CONTENT_SDNA).unwrap();
        assert_eq!(sdna["abstract"], true);
    }

    #[test]
    fn container_is_an_abstract_class_with_children_over_has_child() {
        let container = builtin(CONTAINER_URI).expect("Container is built in");
        assert_eq!(container.name, "Container");
        assert_eq!(container.shape.target_class, CONTAINER_URI);
        let children = &container.shape.include_relations;
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "children");
        assert_eq!(children[0].predicate, "ad4m://has_child");
        assert_eq!(children[0].direction, "forward");
        // Its own rows: a node with a has_child link, whatever its class.
        let member = container
            .member
            .as_ref()
            .expect("Container reads its own rows");
        assert!(member.properties.iter().all(|p| p.is_required));
        assert!(builtin(TEXT_CONTENT_URI).unwrap().member.is_none());
    }

    #[test]
    fn only_the_uri_names_a_builtin() {
        assert!(builtin("TextContent").is_none());
        assert!(builtin("app://TextContent").is_none());
        assert!(builtin("Container").is_none());
    }
}
