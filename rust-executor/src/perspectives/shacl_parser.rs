use crate::types::Link;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};

/// SHACL Shape structure (from TypeScript)
#[derive(Debug, Deserialize, Serialize)]
pub struct SHACLShape {
    pub target_class: String,
    pub properties: Vec<PropertyShape>,
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
    pub resolve_language: Option<String>,
    pub node_kind: Option<String>,
    pub collection: Option<bool>,
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
    links.push(Link {
        source: "ad4m://self".to_string(),
        predicate: Some("ad4m://has_subject_class".to_string()),
        target: format!("literal://string:{}", class_name),
    });

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
    }

    Ok(links)
}

/// Extract namespace from URI (e.g., "recipe://Recipe" -> "recipe://")
fn extract_namespace(uri: &str) -> String {
    // Handle fragment separator (#) if present
    let base_uri = if let Some(hash_pos) = uri.rfind('#') {
        &uri[..hash_pos + 1]
    } else {
        uri
    };
    
    // Find scheme separator
    if let Some(scheme_pos) = base_uri.find("://") {
        let after_scheme = &base_uri[scheme_pos + 3..];
        
        // Find last slash in the authority/path
        if let Some(last_slash) = after_scheme.rfind('/') {
            base_uri[..scheme_pos + 3 + last_slash + 1].to_string()
        } else {
            // No path, just return scheme + "://"
            base_uri[..scheme_pos + 3].to_string()
        }
    } else {
        // No scheme, fallback
        format!("{}/", uri)
    }
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
        assert_eq!(extract_namespace("recipe://Recipe"), "recipe://");
        assert_eq!(extract_namespace("http://example.com/ns#Recipe"), "http://example.com/ns/");
        assert_eq!(extract_namespace("simple://Test"), "simple://");
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
        
        // Should have: class definition (5) + property shape (7) = 12 links minimum
        assert!(links.len() >= 12);
        
        // Check for key links
        assert!(links.iter().any(|l| l.source == "ad4m://self" && l.target == "literal://string:Recipe"));
        assert!(links.iter().any(|l| l.source == "recipe://RecipeShape" && l.predicate == Some("sh://targetClass".to_string())));
        assert!(links.iter().any(|l| l.source == "recipe://Recipe.name" && l.predicate == Some("sh://path".to_string())));
    }
}
