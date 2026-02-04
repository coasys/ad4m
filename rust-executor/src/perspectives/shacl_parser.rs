use crate::types::Link;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

/// AD4M Action - represents a link operation (e.g., addLink, removeLink, setSingleTarget)
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AD4MAction {
    pub action: String,
    pub source: String,
    pub predicate: String,
    pub target: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub local: Option<bool>,
}

/// SHACL Shape structure (from TypeScript)
#[derive(Debug, Deserialize, Serialize)]
pub struct SHACLShape {
    pub target_class: String,
    pub properties: Vec<PropertyShape>,
    /// Constructor actions for creating instances
    #[serde(default)]
    pub constructor_actions: Vec<AD4MAction>,
    /// Destructor actions for removing instances
    #[serde(default)]
    pub destructor_actions: Vec<AD4MAction>,
}

/// SHACL Property Shape structure
#[derive(Debug, Deserialize, Serialize)]
pub struct PropertyShape {
    pub path: String,
    pub name: Option<String>,
    pub datatype: Option<String>,
    pub min_count: Option<u32>,
    pub max_count: Option<u32>,
    pub writable: Option<bool>,
    pub local: Option<bool>,
    pub resolve_language: Option<String>,
    pub node_kind: Option<String>,
    pub collection: Option<bool>,
    /// Setter action for single-valued properties
    #[serde(default)]
    pub setter: Vec<AD4MAction>,
    /// Adder action for collection properties
    #[serde(default)]
    pub adder: Vec<AD4MAction>,
    /// Remover action for collection properties
    #[serde(default)]
    pub remover: Vec<AD4MAction>,
}

/// Parse SHACL JSON to RDF links (Option 3: Named Property Shapes)
pub fn parse_shacl_to_links(shacl_json: &str, class_name: &str) -> Result<Vec<Link>, AnyError> {
    let shape: SHACLShape = serde_json::from_str(shacl_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse SHACL JSON: {}", e))?;

    let mut links = Vec::new();

    // Extract namespace from target_class (e.g., "recipe://Recipe" -> "recipe://")
    let namespace = extract_namespace(&shape.target_class);
    let shape_uri = format!("{}{}Shape", namespace, class_name);

    // Class definition links
    // Note: The ad4m://has_subject_class link is created by add_sdna(), not here,
    // to avoid duplication since add_sdna() always creates that link

    links.push(Link {
        source: shape.target_class.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://SubjectClass".to_string(),
    });

    links.push(Link {
        source: shape.target_class.clone(),
        predicate: Some("ad4m://shape".to_string()),
        target: shape_uri.clone(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "sh://NodeShape".to_string(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("sh://targetClass".to_string()),
        target: shape.target_class.clone(),
    });

    // Constructor actions (stored as JSON in literal)
    if !shape.constructor_actions.is_empty() {
        let constructor_json = serde_json::to_string(&shape.constructor_actions)
            .unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://constructor".to_string()),
            target: format!("literal://string:{}", constructor_json),
        });
    }

    // Destructor actions (stored as JSON in literal)
    if !shape.destructor_actions.is_empty() {
        let destructor_json = serde_json::to_string(&shape.destructor_actions)
            .unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://destructor".to_string()),
            target: format!("literal://string:{}", destructor_json),
        });
    }

    // Property shape links (Option 3: Named Property Shapes)
    for prop in shape.properties.iter() {
        // Use name field if provided, otherwise extract from path
        let prop_name = prop.name.as_ref()
            .map(|n| n.clone())
            .unwrap_or_else(|| extract_local_name(&prop.path));
        
        let prop_shape_uri = format!("{}{}.{}", namespace, class_name, prop_name);

        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("sh://property".to_string()),
            target: prop_shape_uri.clone(),
        });

        // Determine type based on collection flag
        let shape_type = if prop.collection.unwrap_or(false) {
            "ad4m://CollectionShape"
        } else {
            "sh://PropertyShape"
        };

        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: shape_type.to_string(),
        });

        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("sh://path".to_string()),
            target: prop.path.clone(),
        });

        // Optional constraints
        if let Some(datatype) = &prop.datatype {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://datatype".to_string()),
                target: datatype.clone(),
            });
        }

        if let Some(min_count) = prop.min_count {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://minCount".to_string()),
                target: format!("literal://number:{}", min_count),
            });
        }

        if let Some(max_count) = prop.max_count {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://maxCount".to_string()),
                target: format!("literal://number:{}", max_count),
            });
        }

        if let Some(writable) = prop.writable {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://writable".to_string()),
                target: format!("literal://boolean:{}", writable),
            });
        }

        if let Some(resolve_lang) = &prop.resolve_language {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://resolveLanguage".to_string()),
                target: format!("literal://string:{}", resolve_lang),
            });
        }

        if let Some(node_kind) = &prop.node_kind {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://nodeKind".to_string()),
                target: node_kind.clone(),
            });
        }

        if let Some(local) = prop.local {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://local".to_string()),
                target: format!("literal://boolean:{}", local),
            });
        }

        // Property-level actions (setter, adder, remover)
        if !prop.setter.is_empty() {
            let setter_json = serde_json::to_string(&prop.setter)
                .unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://setter".to_string()),
                target: format!("literal://string:{}", setter_json),
            });
        }

        if !prop.adder.is_empty() {
            let adder_json = serde_json::to_string(&prop.adder)
                .unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://adder".to_string()),
                target: format!("literal://string:{}", adder_json),
            });
        }

        if !prop.remover.is_empty() {
            let remover_json = serde_json::to_string(&prop.remover)
                .unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://remover".to_string()),
                target: format!("literal://string:{}", remover_json),
            });
        }
    }

    Ok(links)
}

/// Parse Prolog SDNA code and generate SHACL links for the class
/// This enables backward compatibility with Prolog-only SDNA definitions
pub fn parse_prolog_sdna_to_shacl_links(prolog_sdna: &str, class_name: &str) -> Result<Vec<Link>, AnyError> {
    use regex::Regex;

    let mut links = Vec::new();

    // Extract namespace from a predicate in the prolog code
    // Look for patterns like triple(Base, "todo://state", ...) to find the namespace
    let predicate_regex = Regex::new(r#"triple\([^,]+,\s*"([a-zA-Z][a-zA-Z0-9+.-]*://)[^"]*""#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    let namespace = predicate_regex.captures(prolog_sdna)
        .map(|c| c.get(1).map(|m| m.as_str().to_string()))
        .flatten()
        .unwrap_or_else(|| "ad4m://".to_string());

    let target_class = format!("{}{}", namespace, class_name);
    let shape_uri = format!("{}{}Shape", namespace, class_name);

    // Basic class definition links
    links.push(Link {
        source: target_class.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://SubjectClass".to_string(),
    });

    links.push(Link {
        source: target_class.clone(),
        predicate: Some("ad4m://shape".to_string()),
        target: shape_uri.clone(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "sh://NodeShape".to_string(),
    });

    links.push(Link {
        source: shape_uri.clone(),
        predicate: Some("sh://targetClass".to_string()),
        target: target_class.clone(),
    });

    // Parse constructor: constructor(c, '[{action: ...}]').
    // Note: Prolog uses single quotes for JSON-like content with unquoted keys
    let constructor_regex = Regex::new(r#"constructor\([^,]+,\s*'(\[.*?\])'\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    if let Some(caps) = constructor_regex.captures(prolog_sdna) {
        if let Some(actions_str) = caps.get(1) {
            // Convert Prolog-style JSON to valid JSON (add quotes to keys)
            let json_str = convert_prolog_json_to_json(actions_str.as_str());
            links.push(Link {
                source: shape_uri.clone(),
                predicate: Some("ad4m://constructor".to_string()),
                target: format!("literal://string:{}", json_str),
            });
        }
    }

    // Parse destructor: destructor(c, '[{action: ...}]').
    let destructor_regex = Regex::new(r#"destructor\([^,]+,\s*'(\[.*?\])'\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    if let Some(caps) = destructor_regex.captures(prolog_sdna) {
        if let Some(actions_str) = caps.get(1) {
            let json_str = convert_prolog_json_to_json(actions_str.as_str());
            links.push(Link {
                source: shape_uri.clone(),
                predicate: Some("ad4m://destructor".to_string()),
                target: format!("literal://string:{}", json_str),
            });
        }
    }

    // Parse properties and their getters to extract predicates
    // property(c, "name").
    let property_regex = Regex::new(r#"property\([^,]+,\s*"([^"]+)"\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // property_getter(c, Base, "name", Value) :- triple(Base, "predicate://path", Value).
    let getter_regex = Regex::new(r#"property_getter\([^,]+,\s*[^,]+,\s*"([^"]+)",\s*[^)]+\)\s*:-\s*triple\([^,]+,\s*"([^"]+)""#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // property_setter(c, "name", '[{action: ...}]').
    let setter_regex = Regex::new(r#"property_setter\([^,]+,\s*"([^"]+)",\s*'(\[.*?\])'\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // property_resolve_language(c, "name", "literal").
    let resolve_lang_regex = Regex::new(r#"property_resolve_language\([^,]+,\s*"([^"]+)",\s*"([^"]+)"\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // Collect properties
    let mut properties: std::collections::HashMap<String, (Option<String>, Option<String>, Option<String>)> =
        std::collections::HashMap::new();

    for caps in property_regex.captures_iter(prolog_sdna) {
        if let Some(prop_name) = caps.get(1) {
            properties.entry(prop_name.as_str().to_string())
                .or_insert((None, None, None));
        }
    }

    // Extract predicate paths from getters
    for caps in getter_regex.captures_iter(prolog_sdna) {
        if let (Some(prop_name), Some(predicate)) = (caps.get(1), caps.get(2)) {
            if let Some(entry) = properties.get_mut(prop_name.as_str()) {
                entry.0 = Some(predicate.as_str().to_string());
            }
        }
    }

    // Extract setters
    for caps in setter_regex.captures_iter(prolog_sdna) {
        if let (Some(prop_name), Some(actions)) = (caps.get(1), caps.get(2)) {
            if let Some(entry) = properties.get_mut(prop_name.as_str()) {
                entry.1 = Some(convert_prolog_json_to_json(actions.as_str()));
            }
        }
    }

    // Extract resolve languages
    for caps in resolve_lang_regex.captures_iter(prolog_sdna) {
        if let (Some(prop_name), Some(lang)) = (caps.get(1), caps.get(2)) {
            if let Some(entry) = properties.get_mut(prop_name.as_str()) {
                entry.2 = Some(lang.as_str().to_string());
            }
        }
    }

    // Generate property shape links
    for (prop_name, (path, setter, resolve_lang)) in properties.iter() {
        let prop_shape_uri = format!("{}{}.{}", namespace, class_name, prop_name);

        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("sh://property".to_string()),
            target: prop_shape_uri.clone(),
        });

        links.push(Link {
            source: prop_shape_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "sh://PropertyShape".to_string(),
        });

        if let Some(path) = path {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://path".to_string()),
                target: path.clone(),
            });
        }

        if let Some(setter_json) = setter {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://setter".to_string()),
                target: format!("literal://string:{}", setter_json),
            });
        }

        if let Some(lang) = resolve_lang {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://resolveLanguage".to_string()),
                target: format!("literal://string:{}", lang),
            });
        }
    }

    // Parse collections
    // collection(c, "comments").
    let collection_regex = Regex::new(r#"collection\([^,]+,\s*"([^"]+)"\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // collection_getter(c, Base, "comments", List) :- findall(C, triple(Base, "predicate://path", C), List).
    let coll_getter_regex = Regex::new(r#"collection_getter\([^,]+,\s*[^,]+,\s*"([^"]+)"[^)]*\)\s*:-.*triple\([^,]+,\s*"([^"]+)""#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // collection_adder(c, "commentss", '[{action: ...}]').
    let coll_adder_regex = Regex::new(r#"collection_adder\([^,]+,\s*"([^"]+)",\s*'(\[.*?\])'\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // collection_remover(c, "commentss", '[{action: ...}]').
    let coll_remover_regex = Regex::new(r#"collection_remover\([^,]+,\s*"([^"]+)",\s*'(\[.*?\])'\)"#)
        .map_err(|e| anyhow::anyhow!("Regex error: {}", e))?;

    // Collect collections: name -> (path, adder, remover)
    let mut collections: std::collections::HashMap<String, (Option<String>, Option<String>, Option<String>)> =
        std::collections::HashMap::new();

    for caps in collection_regex.captures_iter(prolog_sdna) {
        if let Some(coll_name) = caps.get(1) {
            collections.entry(coll_name.as_str().to_string())
                .or_insert((None, None, None));
        }
    }

    // Extract collection paths from getters
    for caps in coll_getter_regex.captures_iter(prolog_sdna) {
        if let (Some(coll_name), Some(predicate)) = (caps.get(1), caps.get(2)) {
            if let Some(entry) = collections.get_mut(coll_name.as_str()) {
                entry.0 = Some(predicate.as_str().to_string());
            }
        }
    }

    // Extract adders (note: adder name might have extra 's' like "commentss")
    for caps in coll_adder_regex.captures_iter(prolog_sdna) {
        if let (Some(coll_name_with_s), Some(actions)) = (caps.get(1), caps.get(2)) {
            // Try to match to a collection by removing trailing 's'
            let coll_name = coll_name_with_s.as_str().trim_end_matches('s');
            if let Some(entry) = collections.get_mut(coll_name) {
                entry.1 = Some(convert_prolog_json_to_json(actions.as_str()));
            }
        }
    }

    // Extract removers
    for caps in coll_remover_regex.captures_iter(prolog_sdna) {
        if let (Some(coll_name_with_s), Some(actions)) = (caps.get(1), caps.get(2)) {
            let coll_name = coll_name_with_s.as_str().trim_end_matches('s');
            if let Some(entry) = collections.get_mut(coll_name) {
                entry.2 = Some(convert_prolog_json_to_json(actions.as_str()));
            }
        }
    }

    // Generate collection shape links
    for (coll_name, (path, adder, remover)) in collections.iter() {
        let coll_shape_uri = format!("{}{}.{}", namespace, class_name, coll_name);

        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("sh://property".to_string()),
            target: coll_shape_uri.clone(),
        });

        links.push(Link {
            source: coll_shape_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://CollectionShape".to_string(),
        });

        if let Some(path) = path {
            links.push(Link {
                source: coll_shape_uri.clone(),
                predicate: Some("sh://path".to_string()),
                target: path.clone(),
            });
        }

        if let Some(adder_json) = adder {
            links.push(Link {
                source: coll_shape_uri.clone(),
                predicate: Some("ad4m://adder".to_string()),
                target: format!("literal://string:{}", adder_json),
            });
        }

        if let Some(remover_json) = remover {
            links.push(Link {
                source: coll_shape_uri.clone(),
                predicate: Some("ad4m://remover".to_string()),
                target: format!("literal://string:{}", remover_json),
            });
        }
    }

    Ok(links)
}

/// Convert Prolog-style JSON (with unquoted keys) to valid JSON
/// e.g., '{action: "addLink", source: "this"}' -> '{"action":"addLink","source":"this"}'
fn convert_prolog_json_to_json(prolog_json: &str) -> String {
    use regex::Regex;

    // Add quotes around unquoted keys: word: -> "word":
    let key_regex = Regex::new(r#"(\{|\s|,)([a-zA-Z_][a-zA-Z0-9_]*):"#).unwrap();
    let result = key_regex.replace_all(prolog_json, r#"$1"$2":"#);

    result.to_string()
}

/// Extract namespace from URI (e.g., "recipe://Recipe" -> "recipe://")
/// Matches TypeScript SHACLShape.ts extractNamespace() behavior
pub fn extract_namespace(uri: &str) -> String {
    // Handle protocol-style URIs (://ending) - for AD4M-style URIs like "recipe://Recipe"
    // We want just the scheme + "://" part
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];

        // If nothing after scheme or only simple local name (no / or #), return just scheme://
        if !after_scheme.contains('/') && !after_scheme.contains('#') {
            return uri[..scheme_pos + 3].to_string();
        }
    }

    // Handle hash fragments (e.g., "http://example.com/ns#Recipe" -> "http://example.com/ns#")
    if let Some(hash_pos) = uri.rfind('#') {
        return uri[..hash_pos + 1].to_string();
    }

    // Handle slash-based paths (e.g., "http://example.com/ns/Recipe" -> "http://example.com/ns/")
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];
        if let Some(last_slash) = after_scheme.rfind('/') {
            return uri[..scheme_pos + 3 + last_slash + 1].to_string();
        }
    }

    // Fallback: return as-is with trailing separator
    String::new()
}

/// Extract local name from URI (e.g., "recipe://name" -> "name")
fn extract_local_name(uri: &str) -> String {
    uri.split('/').last()
        .filter(|s| !s.is_empty())
        .unwrap_or("unknown")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_namespace() {
        // AD4M-style URIs (scheme://LocalName) -> just scheme://
        assert_eq!(extract_namespace("recipe://Recipe"), "recipe://");
        assert_eq!(extract_namespace("simple://Test"), "simple://");

        // W3C-style URIs with hash fragments -> include the hash
        assert_eq!(extract_namespace("http://example.com/ns#Recipe"), "http://example.com/ns#");

        // W3C-style URIs with slash paths -> include trailing slash
        assert_eq!(extract_namespace("http://example.com/ns/Recipe"), "http://example.com/ns/");
    }

    #[test]
    fn test_extract_local_name() {
        assert_eq!(extract_local_name("recipe://name"), "name");
        assert_eq!(extract_local_name("http://example.com/property"), "property");
        assert_eq!(extract_local_name("simple://test/path/item"), "item");
    }

    #[test]
    fn test_parse_shacl_basic() {
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "properties": [
                {
                    "path": "recipe://name",
                    "name": "name",
                    "datatype": "xsd://string",
                    "min_count": 1,
                    "max_count": 1,
                    "writable": true,
                    "resolve_language": "literal"
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Recipe").unwrap();

        // Should have: class definition (4) + property shape (7) = 11 links minimum
        // Note: ad4m://has_subject_class link is NOT created here - it's created by add_sdna()
        assert!(links.len() >= 11);

        // Check for key links (note: ad4m://self -> literal://string:Recipe is NOT here)
        assert!(links.iter().any(|l| l.source == "recipe://RecipeShape" && l.predicate == Some("sh://targetClass".to_string())));
        assert!(links.iter().any(|l| l.source == "recipe://Recipe.name" && l.predicate == Some("sh://path".to_string())));
    }

    #[test]
    fn test_parse_shacl_with_actions() {
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "recipe://name", "target": "literal://string:uninitialized"}
            ],
            "destructor_actions": [
                {"action": "removeLink", "source": "this", "predicate": "recipe://name", "target": "*"}
            ],
            "properties": [
                {
                    "path": "recipe://name",
                    "name": "name",
                    "datatype": "xsd://string",
                    "min_count": 1,
                    "max_count": 1,
                    "writable": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "recipe://name", "target": "value"}]
                },
                {
                    "path": "recipe://ingredient",
                    "name": "ingredients",
                    "node_kind": "IRI",
                    "adder": [{"action": "addLink", "source": "this", "predicate": "recipe://ingredient", "target": "value"}],
                    "remover": [{"action": "removeLink", "source": "this", "predicate": "recipe://ingredient", "target": "value"}]
                }
            ]
        }"#;

        let links = parse_shacl_to_links(shacl_json, "Recipe").unwrap();

        // Check for constructor action link
        assert!(links.iter().any(|l|
            l.source == "recipe://RecipeShape" &&
            l.predicate == Some("ad4m://constructor".to_string()) &&
            l.target.starts_with("literal://string:")
        ), "Missing constructor action link");

        // Check for destructor action link
        assert!(links.iter().any(|l|
            l.source == "recipe://RecipeShape" &&
            l.predicate == Some("ad4m://destructor".to_string()) &&
            l.target.starts_with("literal://string:")
        ), "Missing destructor action link");

        // Check for property setter action link
        assert!(links.iter().any(|l|
            l.source == "recipe://Recipe.name" &&
            l.predicate == Some("ad4m://setter".to_string()) &&
            l.target.starts_with("literal://string:")
        ), "Missing setter action link");

        // Check for collection adder action link
        assert!(links.iter().any(|l|
            l.source == "recipe://Recipe.ingredients" &&
            l.predicate == Some("ad4m://adder".to_string()) &&
            l.target.starts_with("literal://string:")
        ), "Missing adder action link");

        // Check for collection remover action link
        assert!(links.iter().any(|l|
            l.source == "recipe://Recipe.ingredients" &&
            l.predicate == Some("ad4m://remover".to_string()) &&
            l.target.starts_with("literal://string:")
        ), "Missing remover action link");
    }
}
