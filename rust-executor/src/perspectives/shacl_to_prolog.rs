//! SHACL to Prolog backward compatibility module
//!
//! This module generates Prolog facts from SHACL links, enabling backward compatibility
//! with existing Prolog-based SDNA queries (like infer() and template matching) when
//! classes are defined using SHACL-only (without Prolog code).
//!
//! This is a transitional feature - once all consumers are updated to use SHACL queries
//! directly, this module can be deprecated.

use crate::types::DecoratedLinkExpression;
use std::collections::{HashMap, HashSet};

/// Generate Prolog facts from SHACL links for backward compatibility.
///
/// This allows infer() queries and template matching to work with SHACL-only classes.
/// Only generates facts for classes that don't have original Prolog code.
///
/// # Arguments
/// * `all_links` - All links in the perspective
/// * `seen_subject_classes` - Map of class names to their properties (including "code" for Prolog)
///
/// # Returns
/// Vector of Prolog fact strings
pub fn generate_prolog_facts_from_shacl(
    all_links: &[DecoratedLinkExpression],
    seen_subject_classes: &HashMap<String, HashMap<String, String>>,
) -> Vec<String> {
    let mut lines = Vec::new();

    // First pass: collect shape → class mappings and class info
    let mut shape_to_class: HashMap<String, String> = HashMap::new();
    let mut class_shapes: HashMap<String, String> = HashMap::new();

    for link_expression in all_links {
        let link = &link_expression.data;
        // sh://targetClass links map shapes to classes
        if link.predicate == Some("sh://targetClass".to_string()) {
            let shape_uri = &link.source;
            let class_uri = &link.target;
            let class_name = extract_local_name(class_uri);

            if !class_name.is_empty() {
                // Only generate SHACL→Prolog facts for classes WITHOUT original Prolog code
                // Classes with original Prolog use their own predicates and class identifiers
                let has_original_prolog = seen_subject_classes
                    .get(&class_name)
                    .and_then(|props| props.get("code"))
                    .map(|code| !code.trim().is_empty())
                    .unwrap_or(false);

                if !has_original_prolog {
                    shape_to_class.insert(shape_uri.clone(), class_name.clone());
                    class_shapes.insert(class_name, shape_uri.clone());
                }
            }
        }
    }

    // Second pass: collect properties for each shape
    let mut shape_properties: HashMap<String, Vec<String>> = HashMap::new();
    let mut property_to_shape: HashMap<String, String> = HashMap::new();

    for link_expression in all_links {
        let link = &link_expression.data;
        // sh://property links connect shapes to property shapes
        if link.predicate == Some("sh://property".to_string()) {
            let shape_uri = &link.source;
            let prop_shape_uri = &link.target;

            if shape_to_class.contains_key(shape_uri) {
                shape_properties
                    .entry(shape_uri.clone())
                    .or_default()
                    .push(prop_shape_uri.clone());
                property_to_shape.insert(prop_shape_uri.clone(), shape_uri.clone());
            }
        }
    }

    // Third pass: collect property names and setters
    let mut prop_shape_to_name: HashMap<String, String> = HashMap::new();
    let mut prop_has_setter: HashSet<String> = HashSet::new();
    let mut prop_is_collection: HashSet<String> = HashSet::new();
    let mut shape_has_constructor: HashSet<String> = HashSet::new();

    for link_expression in all_links {
        let link = &link_expression.data;

        // sh://path links give property names
        if link.predicate == Some("sh://path".to_string()) {
            let prop_shape_uri = &link.source;
            let path_uri = &link.target;
            let prop_name = extract_local_name(path_uri);

            if !prop_name.is_empty() {
                prop_shape_to_name.insert(prop_shape_uri.clone(), prop_name);
            }
        }

        // ad4m://setter links indicate writable properties
        if link.predicate == Some("ad4m://setter".to_string()) {
            prop_has_setter.insert(link.source.clone());
        }

        // ad4m://CollectionShape type indicates a collection
        if link.predicate == Some("rdf://type".to_string())
            && link.target == "ad4m://CollectionShape"
        {
            prop_is_collection.insert(link.source.clone());
        }

        // ad4m://constructor links indicate the shape has a constructor
        if link.predicate == Some("ad4m://constructor".to_string()) {
            shape_has_constructor.insert(link.source.clone());
        }
    }

    // Generate Prolog facts for each SHACL class
    for (class_name, shape_uri) in &class_shapes {
        let shape_id = generate_prolog_safe_id(class_name);

        // subject_class/2 fact
        lines.push(format!("subject_class(\"{}\", {}).", class_name, shape_id));

        // Generate property facts
        if let Some(prop_shapes) = shape_properties.get(shape_uri) {
            for prop_shape in prop_shapes {
                if let Some(prop_name) = prop_shape_to_name.get(prop_shape) {
                    if prop_is_collection.contains(prop_shape) {
                        // collection/2 fact
                        lines.push(format!("collection({}, \"{}\").", shape_id, prop_name));

                        // Collection operations - always generate adder, remover, and setter
                        // These are required for template object matching queries
                        lines.push(format!(
                            "collection_adder({}, \"{}\", _).",
                            shape_id, prop_name
                        ));
                        lines.push(format!(
                            "collection_remover({}, \"{}\", _).",
                            shape_id, prop_name
                        ));
                        lines.push(format!(
                            "collection_setter({}, \"{}\", _).",
                            shape_id, prop_name
                        ));
                    } else {
                        // property/2 fact
                        lines.push(format!("property({}, \"{}\").", shape_id, prop_name));

                        // property_setter/3 if writable
                        if prop_has_setter.contains(prop_shape) {
                            lines.push(format!(
                                "property_setter({}, \"{}\", _).",
                                shape_id, prop_name
                            ));
                        }
                    }
                }
            }
        }

        // constructor/2 if shape has constructor
        if shape_has_constructor.contains(shape_uri) {
            lines.push(format!("constructor({}, _).", shape_id));
        }

        // Generate instance/2 rule for SHACL-based classes
        let instance_conditions = collect_instance_conditions(
            shape_uri,
            &shape_properties,
            &prop_shape_to_name,
            all_links,
        );

        if !instance_conditions.is_empty() {
            // Use OR (;) to check if ANY of the properties exist
            let condition_str = instance_conditions.join("; ");
            lines.push(format!(
                "instance({}, Base) :- {}.",
                shape_id, condition_str
            ));
        } else {
            // No properties found - generate a permissive rule that matches any base
            lines.push(format!("instance({}, _).", shape_id));
        }
    }

    lines
}

/// Extract the local name from a URI.
/// Examples:
/// - "recipe://name" -> "name"
/// - "https://example.com/vocab#term" -> "term"
/// - "flux://Channel" -> "Channel"
fn extract_local_name(uri: &str) -> String {
    uri.split("://")
        .last()
        .unwrap_or(uri)
        .split('/')
        .last()
        .unwrap_or(uri)
        .split('#')
        .last()
        .unwrap_or(uri)
        .to_string()
}

/// Generate a Prolog-safe identifier from a class name.
/// Sanitizes: lowercase, replace non-alphanumeric with '_', collapse underscores.
fn generate_prolog_safe_id(class_name: &str) -> String {
    let sanitized_name = class_name
        .to_lowercase()
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>()
        .split('_')
        .filter(|s| !s.is_empty())
        .collect::<Vec<&str>>()
        .join("_");
    format!("shacl_{}", sanitized_name)
}

/// Collect instance conditions for a shape (predicates that can identify an instance).
fn collect_instance_conditions(
    shape_uri: &str,
    shape_properties: &HashMap<String, Vec<String>>,
    prop_shape_to_name: &HashMap<String, String>,
    all_links: &[DecoratedLinkExpression],
) -> Vec<String> {
    let mut conditions = Vec::new();

    if let Some(prop_shapes) = shape_properties.get(shape_uri) {
        for prop_shape in prop_shapes {
            if prop_shape_to_name.contains_key(prop_shape) {
                // Find the path predicate for this property
                for link_expression in all_links {
                    let link = &link_expression.data;
                    if link.predicate == Some("sh://path".to_string()) && &link.source == prop_shape
                    {
                        let predicate = &link.target;
                        conditions.push(format!("triple(Base, \"{}\", _)", predicate));
                        break;
                    }
                }
            }
        }
    }

    conditions
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    fn make_link(source: &str, predicate: &str, target: &str) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: "did:key:test".to_string(),
            timestamp: "2026-01-01T00:00:00Z".to_string(),
            data: Link {
                source: source.to_string(),
                predicate: Some(predicate.to_string()),
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                signature: "sig".to_string(),
                key: "key".to_string(),
                valid: Some(true),
                invalid: None,
            },
            status: None,
        }
    }

    #[test]
    fn test_extract_local_name() {
        assert_eq!(extract_local_name("recipe://name"), "name");
        assert_eq!(extract_local_name("flux://Channel"), "Channel");
        assert_eq!(extract_local_name("https://example.com/vocab#term"), "term");
        assert_eq!(extract_local_name("simple"), "simple");
    }

    #[test]
    fn test_generate_prolog_safe_id() {
        assert_eq!(generate_prolog_safe_id("Recipe"), "shacl_recipe");
        assert_eq!(generate_prolog_safe_id("MyClass"), "shacl_myclass");
        assert_eq!(
            generate_prolog_safe_id("Some-Class-Name"),
            "shacl_some_class_name"
        );
    }

    #[test]
    fn test_generate_prolog_facts_empty_input() {
        let links: Vec<DecoratedLinkExpression> = vec![];
        let seen_classes: HashMap<String, HashMap<String, String>> = HashMap::new();

        let facts = generate_prolog_facts_from_shacl(&links, &seen_classes);
        assert!(facts.is_empty());
    }

    #[test]
    fn test_generate_prolog_facts_basic_class() {
        let links = vec![
            // Shape definition
            make_link("recipe://RecipeShape", "rdf://type", "sh://NodeShape"),
            make_link(
                "recipe://RecipeShape",
                "sh://targetClass",
                "recipe://Recipe",
            ),
            // Property
            make_link(
                "recipe://RecipeShape",
                "sh://property",
                "recipe://Recipe.name",
            ),
            make_link("recipe://Recipe.name", "sh://path", "recipe://name"),
            make_link("recipe://Recipe.name", "ad4m://setter", "literal://..."),
            // Constructor
            make_link(
                "recipe://RecipeShape",
                "ad4m://constructor",
                "literal://...",
            ),
        ];

        let seen_classes: HashMap<String, HashMap<String, String>> = HashMap::new();
        let facts = generate_prolog_facts_from_shacl(&links, &seen_classes);

        // Should generate subject_class, property, property_setter, constructor, instance
        assert!(facts
            .iter()
            .any(|f| f.contains("subject_class(\"Recipe\",")));
        assert!(facts.iter().any(|f| f.contains("property(shacl_recipe,")));
        assert!(facts
            .iter()
            .any(|f| f.contains("property_setter(shacl_recipe,")));
        assert!(facts
            .iter()
            .any(|f| f.contains("constructor(shacl_recipe,")));
        assert!(facts.iter().any(|f| f.contains("instance(shacl_recipe,")));
    }

    #[test]
    fn test_skips_classes_with_prolog_code() {
        let links = vec![make_link(
            "recipe://RecipeShape",
            "sh://targetClass",
            "recipe://Recipe",
        )];

        // Class has existing Prolog code - should skip
        let mut seen_classes: HashMap<String, HashMap<String, String>> = HashMap::new();
        let mut recipe_props = HashMap::new();
        recipe_props.insert("code".to_string(), "subject_class(...)".to_string());
        seen_classes.insert("Recipe".to_string(), recipe_props);

        let facts = generate_prolog_facts_from_shacl(&links, &seen_classes);

        // Should NOT generate facts for Recipe since it has Prolog code
        assert!(!facts.iter().any(|f| f.contains("Recipe")));
    }

    #[test]
    fn test_collection_facts() {
        let links = vec![
            make_link(
                "recipe://RecipeShape",
                "sh://targetClass",
                "recipe://Recipe",
            ),
            make_link(
                "recipe://RecipeShape",
                "sh://property",
                "recipe://Recipe.items",
            ),
            make_link("recipe://Recipe.items", "sh://path", "recipe://items"),
            make_link(
                "recipe://Recipe.items",
                "rdf://type",
                "ad4m://CollectionShape",
            ),
        ];

        let seen_classes: HashMap<String, HashMap<String, String>> = HashMap::new();
        let facts = generate_prolog_facts_from_shacl(&links, &seen_classes);

        // Should generate collection facts
        assert!(facts.iter().any(|f| f.contains("collection(shacl_recipe,")));
        assert!(facts
            .iter()
            .any(|f| f.contains("collection_adder(shacl_recipe,")));
        assert!(facts
            .iter()
            .any(|f| f.contains("collection_remover(shacl_recipe,")));
        assert!(facts
            .iter()
            .any(|f| f.contains("collection_setter(shacl_recipe,")));
    }
}
