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

/// URI, name aliases use, SHACL JSON.
const BUILTINS: &[(&str, &str, &str)] = &[(TEXT_CONTENT_URI, "TextContent", TEXT_CONTENT_SDNA)];

/// A built-in abstract class.
#[derive(Debug)]
pub struct Builtin {
    /// The name aliases key it by.
    pub name: &'static str,
    pub shape: Arc<ModelShape>,
}

/// The built-in abstract class with this URI, or `None` for any other name.
pub fn builtin(uri: &str) -> Option<&'static Builtin> {
    static BUILT: OnceLock<HashMap<&'static str, Builtin>> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            BUILTINS
                .iter()
                .map(|(uri, name, sdna)| {
                    (
                        *uri,
                        Builtin {
                            name,
                            shape: Arc::new(read_back(name, sdna)),
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
    fn only_the_uri_names_a_builtin() {
        assert!(builtin("TextContent").is_none());
        assert!(builtin("app://TextContent").is_none());
    }
}
