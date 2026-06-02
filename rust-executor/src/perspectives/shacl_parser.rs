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

/// A single structured conformance condition for relation filtering.
/// DB-agnostic representation that can be translated to any query language.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ConformanceCondition {
    /// Type of check: "flag" (predicate + value) or "required" (predicate exists)
    #[serde(rename = "type")]
    pub condition_type: String,
    /// The predicate URI to check on the target node
    pub predicate: String,
    /// For "flag" conditions: the expected value
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
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
    /// Pre-computed getter expression for reading this relation/property.
    /// For relations with a target model, this encodes conformance filtering.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub getter: Option<String>,
    /// Structured conformance conditions (DB-agnostic).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub conformance_conditions: Vec<ConformanceCondition>,
    /// Target SHACL node shape URI (sh:class). When present, linked nodes
    /// must conform to this shape, enabling typed construction.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub class: Option<String>,
    /// Kind of relation this property describes. One of "hasMany", "hasOne",
    /// "belongsToOne", "belongsToMany".  Drives direction (forward/reverse)
    /// and scalar-vs-collection rendering.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relation_kind: Option<String>,
    /// Bare target class name for a relation property — used by the executor
    /// to look up the target shape through its in-memory cache.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_class_name: Option<String>,
    /// Post-getter where-clause filter for relation properties.  Keys are
    /// property names on the target class; values follow the where-clause DSL.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub where_filter: Option<serde_json::Value>,
    /// Predicate IRI lookup for `where_filter` keys (property name → predicate).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub where_predicates: Option<std::collections::HashMap<String, String>>,
    /// Whether conformance/type filtering is enabled for this relation.
    /// Omitted (defaulting to true) when not explicitly disabled.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub filter: Option<bool>,
    /// Fixed value constraint (sh:hasValue).  When combined with min_count >= 1
    /// the property is interpreted as a `@Flag` — its presence + value mark
    /// the instance as belonging to the class.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub has_value: Option<String>,
}

// ============================================================================
// SHACL Flow structures (state machines without Prolog)
// ============================================================================

/// Link pattern for state detection
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LinkPattern {
    /// Optional source pattern (if omitted, uses the expression address)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    /// Required predicate to match
    pub predicate: String,
    /// Required target value to match
    pub target: String,
}

/// Flow State definition
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FlowState {
    /// State name (e.g., "ready", "doing", "done")
    pub name: String,
    /// Numeric state value for ordering (e.g., 0, 0.5, 1)
    pub value: f64,
    /// Link pattern that indicates this state
    pub state_check: LinkPattern,
}

/// Flow Transition definition
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FlowTransition {
    /// Name of this action (shown to users, e.g., "Start", "Finish")
    pub action_name: String,
    /// State to transition from
    pub from_state: String,
    /// State to transition to
    pub to_state: String,
    /// Actions to execute for this transition
    pub actions: Vec<AD4MAction>,
}

/// SHACL Flow structure - state machine definition
#[derive(Debug, Deserialize, Serialize)]
pub struct SHACLFlow {
    /// Flow name (e.g., "TODO")
    pub name: String,
    /// Namespace for URIs (e.g., "todo://")
    pub namespace: String,
    /// Flowable condition - "any" or a LinkPattern
    #[serde(default = "default_flowable")]
    pub flowable: serde_json::Value,
    /// Actions to execute when starting the flow
    #[serde(default)]
    pub start_action: Vec<AD4MAction>,
    /// States in this flow
    #[serde(default)]
    pub states: Vec<FlowState>,
    /// Transitions between states
    #[serde(default)]
    pub transitions: Vec<FlowTransition>,
}

fn default_flowable() -> serde_json::Value {
    serde_json::Value::String("any".to_string())
}

/// Parse Flow JSON to RDF links
pub fn parse_flow_to_links(flow_json: &str, flow_name: &str) -> Result<Vec<Link>, AnyError> {
    let flow: SHACLFlow = serde_json::from_str(flow_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse Flow JSON: {}", e))?;

    let mut links = Vec::new();

    let flow_uri = format!("{}{}Flow", flow.namespace, flow_name);

    // Flow type
    links.push(Link {
        source: flow_uri.clone(),
        predicate: Some("rdf://type".to_string()),
        target: "ad4m://Flow".to_string(),
    });

    // Flow name
    links.push(Link {
        source: flow_uri.clone(),
        predicate: Some("ad4m://flowName".to_string()),
        target: format!("literal:string:{}", urlencoding::encode(flow_name)),
    });

    // Flowable condition
    let flowable_target = if flow.flowable == serde_json::Value::String("any".to_string()) {
        "ad4m://any".to_string()
    } else {
        format!(
            "literal:string:{}",
            urlencoding::encode(&flow.flowable.to_string())
        )
    };
    links.push(Link {
        source: flow_uri.clone(),
        predicate: Some("ad4m://flowable".to_string()),
        target: flowable_target,
    });

    // Start action
    if !flow.start_action.is_empty() {
        let actions_json = serde_json::to_string(&flow.start_action)
            .map_err(|e| anyhow::anyhow!("Failed to serialize start actions: {}", e))?;
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://startAction".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&actions_json)),
        });
    }

    // States
    for state in &flow.states {
        let state_uri = format!("{}{}.{}", flow.namespace, flow_name, state.name);

        // Link flow to state
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://hasState".to_string()),
            target: state_uri.clone(),
        });

        // State type
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://FlowState".to_string(),
        });

        // State name
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("ad4m://stateName".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&state.name)),
        });

        // State value
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("ad4m://stateValue".to_string()),
            target: format!("literal:number:{}", state.value),
        });

        // State check pattern
        let check_json = serde_json::to_string(&state.state_check)
            .map_err(|e| anyhow::anyhow!("Failed to serialize state check: {}", e))?;
        links.push(Link {
            source: state_uri.clone(),
            predicate: Some("ad4m://stateCheck".to_string()),
            target: format!("literal:string:{}", urlencoding::encode(&check_json)),
        });
    }

    // Transitions
    for transition in &flow.transitions {
        let transition_uri = format!(
            "{}{}.{}To{}",
            flow.namespace, flow_name, transition.from_state, transition.to_state
        );
        let from_state_uri = format!("{}{}.{}", flow.namespace, flow_name, transition.from_state);
        let to_state_uri = format!("{}{}.{}", flow.namespace, flow_name, transition.to_state);

        // Link flow to transition
        links.push(Link {
            source: flow_uri.clone(),
            predicate: Some("ad4m://hasTransition".to_string()),
            target: transition_uri.clone(),
        });

        // Transition type
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("rdf://type".to_string()),
            target: "ad4m://FlowTransition".to_string(),
        });

        // Action name
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://actionName".to_string()),
            target: format!(
                "literal:string:{}",
                urlencoding::encode(&transition.action_name)
            ),
        });

        // From state
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://fromState".to_string()),
            target: from_state_uri,
        });

        // To state
        links.push(Link {
            source: transition_uri.clone(),
            predicate: Some("ad4m://toState".to_string()),
            target: to_state_uri,
        });

        // Transition actions
        if !transition.actions.is_empty() {
            let actions_json = serde_json::to_string(&transition.actions)
                .map_err(|e| anyhow::anyhow!("Failed to serialize transition actions: {}", e))?;
            links.push(Link {
                source: transition_uri.clone(),
                predicate: Some("ad4m://transitionActions".to_string()),
                target: format!("literal:string:{}", urlencoding::encode(&actions_json)),
            });
        }
    }

    Ok(links)
}

/// Parse SHACL JSON to RDF links (Option 3: Named Property Shapes)
pub fn parse_shacl_to_links(shacl_json: &str, class_name: &str) -> Result<Vec<Link>, AnyError> {
    let shape: SHACLShape = serde_json::from_str(shacl_json)
        .map_err(|e| anyhow::anyhow!("Failed to parse SHACL JSON: {}", e))?;

    let mut links = Vec::new();

    // Extract namespace from target_class (e.g., "recipe://Recipe" -> "recipe://")
    let namespace = extract_namespace(&shape.target_class)?;
    let shape_uri = format!("{}{}Shape", namespace, class_name);

    // Create name mapping for class lookup (needed by isSubjectInstance)
    let name_mapping = format!("literal:string:shacl://{}", class_name);

    links.push(Link {
        source: "ad4m://self".to_string(),
        predicate: Some("ad4m://has_shacl".to_string()),
        target: name_mapping.clone(),
    });

    links.push(Link {
        source: name_mapping,
        predicate: Some("ad4m://shacl_shape_uri".to_string()),
        target: shape_uri.clone(),
    });

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
        let constructor_json =
            serde_json::to_string(&shape.constructor_actions).unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://constructor".to_string()),
            target: format!("literal:string:{}", constructor_json),
        });
    }

    // Destructor actions (stored as JSON in literal)
    if !shape.destructor_actions.is_empty() {
        let destructor_json =
            serde_json::to_string(&shape.destructor_actions).unwrap_or_else(|_| "[]".to_string());
        links.push(Link {
            source: shape_uri.clone(),
            predicate: Some("ad4m://destructor".to_string()),
            target: format!("literal:string:{}", destructor_json),
        });
    }

    // Property shape links (Option 3: Named Property Shapes)
    for prop in shape.properties.iter() {
        // Use name field if provided, otherwise extract from path
        let prop_name = prop
            .name
            .as_ref()
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
                target: format!("literal:{}^^xsd:integer", min_count),
            });
        }

        if let Some(max_count) = prop.max_count {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://maxCount".to_string()),
                target: format!("literal:{}^^xsd:integer", max_count),
            });
        }

        if let Some(writable) = prop.writable {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://writable".to_string()),
                target: format!("literal:{}", writable),
            });
        }

        if let Some(resolve_lang) = &prop.resolve_language {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://resolveLanguage".to_string()),
                target: format!("literal:string:{}", resolve_lang),
            });
        }

        if let Some(node_kind) = &prop.node_kind {
            // Ensure node_kind is a valid URI - prefix bare names with sh://
            // e.g. "IRI" -> "sh://IRI", "Literal" -> "sh://Literal"
            let node_kind_uri = if node_kind.contains("://") {
                node_kind.clone()
            } else {
                format!("sh://{}", node_kind)
            };
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://nodeKind".to_string()),
                target: node_kind_uri,
            });
        }

        if let Some(local) = prop.local {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://local".to_string()),
                target: format!("literal:{}", local),
            });
        }

        // Property-level actions (setter, adder, remover)
        if !prop.setter.is_empty() {
            let setter_json =
                serde_json::to_string(&prop.setter).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://setter".to_string()),
                target: format!("literal:string:{}", setter_json),
            });
        }

        if !prop.adder.is_empty() {
            let adder_json =
                serde_json::to_string(&prop.adder).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://adder".to_string()),
                target: format!("literal:string:{}", adder_json),
            });
        }

        if !prop.remover.is_empty() {
            let remover_json =
                serde_json::to_string(&prop.remover).unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://remover".to_string()),
                target: format!("literal:string:{}", remover_json),
            });
        }

        // sh:class — target SHACL node shape URI for typed relation resolution
        if let Some(class_uri) = &prop.class {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://class".to_string()),
                target: class_uri.clone(),
            });
        }

        // Pre-computed getter expression for conformance-filtered relation traversal
        if let Some(getter) = &prop.getter {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://getter".to_string()),
                target: format!("literal:string:{}", getter),
            });
        }

        // Structured conformance conditions for DB-agnostic type filtering
        if !prop.conformance_conditions.is_empty() {
            let conditions_json = serde_json::to_string(&prop.conformance_conditions)
                .unwrap_or_else(|_| "[]".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://conformanceConditions".to_string()),
                target: format!("literal:string:{}", conditions_json),
            });
        }

        // Relation kind — drives direction and scalar-vs-collection rendering.
        if let Some(kind) = &prop.relation_kind {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://relationKind".to_string()),
                target: format!("literal:string:{}", kind),
            });
        }

        // Bare target class name for cache-based include resolution.
        if let Some(target_name) = &prop.target_class_name {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://targetClassName".to_string()),
                target: format!("literal:string:{}", target_name),
            });
        }

        // Post-getter where-clause filter for relations.
        if let Some(where_filter) = &prop.where_filter {
            let filter_json =
                serde_json::to_string(where_filter).unwrap_or_else(|_| "{}".to_string());
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://whereFilter".to_string()),
                target: format!("literal:string:{}", filter_json),
            });
        }

        // Predicate IRI lookup for where_filter keys.
        if let Some(where_predicates) = &prop.where_predicates {
            if !where_predicates.is_empty() {
                let map_json =
                    serde_json::to_string(where_predicates).unwrap_or_else(|_| "{}".to_string());
                links.push(Link {
                    source: prop_shape_uri.clone(),
                    predicate: Some("ad4m://wherePredicates".to_string()),
                    target: format!("literal:string:{}", map_json),
                });
            }
        }

        // Conformance/type filtering enable flag (only emitted when false).
        if let Some(filter_enabled) = prop.filter {
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("ad4m://filter".to_string()),
                target: format!("literal:{}", filter_enabled),
            });
        }

        // sh:hasValue marks @Flag properties.  The target is stored as either
        // a URI (typical for ad4m://type-style flags) or as a literal value.
        if let Some(has_value) = &prop.has_value {
            let target = if has_value.contains("://") || has_value.starts_with("literal:") {
                has_value.clone()
            } else {
                format!("literal:string:{}", urlencoding::encode(has_value))
            };
            links.push(Link {
                source: prop_shape_uri.clone(),
                predicate: Some("sh://hasValue".to_string()),
                target,
            });
        }
    }

    Ok(links)
}
/// Extract namespace from URI (e.g., "recipe://Recipe" -> "recipe://")
/// Matches TypeScript SHACLShape.ts extractNamespace() behavior
pub fn extract_namespace(uri: &str) -> Result<String, AnyError> {
    // Handle protocol-style URIs (://ending) - for AD4M-style URIs like "recipe://Recipe"
    // We want just the scheme + "://" part
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];

        // If nothing after scheme or only simple local name (no / or #), return just scheme://
        if !after_scheme.contains('/') && !after_scheme.contains('#') {
            return Ok(uri[..scheme_pos + 3].to_string());
        }
    }

    // Handle hash fragments (e.g., "http://example.com/ns#Recipe" -> "http://example.com/ns#")
    if let Some(hash_pos) = uri.rfind('#') {
        return Ok(uri[..hash_pos + 1].to_string());
    }

    // Handle slash-based paths (e.g., "http://example.com/ns/Recipe" -> "http://example.com/ns/")
    if let Some(scheme_pos) = uri.find("://") {
        let after_scheme = &uri[scheme_pos + 3..];
        if let Some(last_slash) = after_scheme.rfind('/') {
            return Ok(uri[..scheme_pos + 3 + last_slash + 1].to_string());
        }
    }

    // Error: malformed URI without proper namespace structure
    Err(anyhow::anyhow!(
        "Cannot extract namespace from malformed URI: '{}'",
        uri
    ))
}

/// Extract local name from URI (e.g., "recipe://name" -> "name")
fn extract_local_name(uri: &str) -> String {
    // Find the last occurrence of namespace delimiters: '#', ':', or '/'
    // This handles URIs like "http://example.com/ns#name" or "prefix:name"
    let last_hash = uri.rfind('#');
    let last_colon = uri.rfind(':');
    let last_slash = uri.rfind('/');

    // Find the rightmost delimiter position
    let delimiter_pos = [last_hash, last_colon, last_slash]
        .iter()
        .filter_map(|&pos| pos)
        .max();

    match delimiter_pos {
        Some(pos) => {
            let local_name = &uri[pos + 1..];
            if local_name.is_empty() {
                "unknown".to_string()
            } else {
                local_name.to_string()
            }
        }
        None => {
            // No delimiter found, return the whole URI if non-empty
            if uri.is_empty() {
                "unknown".to_string()
            } else {
                uri.to_string()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_namespace() {
        // AD4M-style URIs (scheme://LocalName) -> just scheme://
        assert_eq!(extract_namespace("recipe://Recipe").unwrap(), "recipe://");
        assert_eq!(extract_namespace("simple://Test").unwrap(), "simple://");

        // W3C-style URIs with hash fragments -> include the hash
        assert_eq!(
            extract_namespace("http://example.com/ns#Recipe").unwrap(),
            "http://example.com/ns#"
        );

        // W3C-style URIs with slash paths -> include trailing slash
        assert_eq!(
            extract_namespace("http://example.com/ns/Recipe").unwrap(),
            "http://example.com/ns/"
        );
    }

    #[test]
    fn test_extract_local_name() {
        assert_eq!(extract_local_name("recipe://name"), "name");
        assert_eq!(
            extract_local_name("http://example.com/property"),
            "property"
        );
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
        assert!(links.iter().any(|l| l.source == "recipe://RecipeShape"
            && l.predicate == Some("sh://targetClass".to_string())));
        assert!(links
            .iter()
            .any(|l| l.source == "recipe://Recipe.name"
                && l.predicate == Some("sh://path".to_string())));
    }

    #[test]
    fn test_parse_shacl_with_actions() {
        let shacl_json = r#"{
            "target_class": "recipe://Recipe",
            "constructor_actions": [
                {"action": "addLink", "source": "this", "predicate": "recipe://name", "target": "literal:string:uninitialized"}
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
        assert!(
            links.iter().any(|l| l.source == "recipe://RecipeShape"
                && l.predicate == Some("ad4m://constructor".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing constructor action link"
        );

        // Check for destructor action link
        assert!(
            links.iter().any(|l| l.source == "recipe://RecipeShape"
                && l.predicate == Some("ad4m://destructor".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing destructor action link"
        );

        // Check for property setter action link
        assert!(
            links.iter().any(|l| l.source == "recipe://Recipe.name"
                && l.predicate == Some("ad4m://setter".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing setter action link"
        );

        // Check for collection adder action link
        assert!(
            links
                .iter()
                .any(|l| l.source == "recipe://Recipe.ingredients"
                    && l.predicate == Some("ad4m://adder".to_string())
                    && l.target.starts_with("literal://string:")
                    || l.target.starts_with("literal:string:")),
            "Missing adder action link"
        );

        // Check for collection remover action link
        assert!(
            links
                .iter()
                .any(|l| l.source == "recipe://Recipe.ingredients"
                    && l.predicate == Some("ad4m://remover".to_string())
                    && l.target.starts_with("literal://string:")
                    || l.target.starts_with("literal:string:")),
            "Missing remover action link"
        );
    }

    #[test]
    fn test_parse_flow_basic() {
        let flow_json = r#"{
            "name": "TODO",
            "namespace": "todo://",
            "flowable": "any",
            "start_action": [
                {"action": "addLink", "source": "this", "predicate": "todo://state", "target": "todo://ready"}
            ],
            "states": [
                {
                    "name": "ready",
                    "value": 0.0,
                    "state_check": {"predicate": "todo://state", "target": "todo://ready"}
                },
                {
                    "name": "done",
                    "value": 1.0,
                    "state_check": {"predicate": "todo://state", "target": "todo://done"}
                }
            ],
            "transitions": [
                {
                    "action_name": "Complete",
                    "from_state": "ready",
                    "to_state": "done",
                    "actions": [
                        {"action": "addLink", "source": "this", "predicate": "todo://state", "target": "todo://done"},
                        {"action": "removeLink", "source": "this", "predicate": "todo://state", "target": "todo://ready"}
                    ]
                }
            ]
        }"#;

        let links = parse_flow_to_links(flow_json, "TODO").unwrap();

        // Check for flow type link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("rdf://type".to_string())
                && l.target == "ad4m://Flow"),
            "Missing flow type link"
        );

        // Check for flowable link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://flowable".to_string())
                && l.target == "ad4m://any"),
            "Missing flowable link"
        );

        // Check for start action link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://startAction".to_string())
                && l.target.starts_with("literal://string:")
                || l.target.starts_with("literal:string:")),
            "Missing start action link"
        );

        // Check for state links
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasState".to_string())
                && l.target == "todo://TODO.ready"),
            "Missing ready state link"
        );

        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasState".to_string())
                && l.target == "todo://TODO.done"),
            "Missing done state link"
        );

        // Check for transition link
        assert!(
            links.iter().any(|l| l.source == "todo://TODOFlow"
                && l.predicate == Some("ad4m://hasTransition".to_string())
                && l.target == "todo://TODO.readyTodone"),
            "Missing transition link"
        );

        // Check for transition action name
        assert!(
            links.iter().any(|l| l.source == "todo://TODO.readyTodone"
                && l.predicate == Some("ad4m://actionName".to_string())),
            "Missing action name link"
        );
    }

    #[test]
    fn test_parse_flow_with_link_pattern_flowable() {
        let flow_json = r#"{
            "name": "Approval",
            "namespace": "approval://",
            "flowable": {"predicate": "rdf://type", "target": "approval://Document"},
            "states": [],
            "transitions": []
        }"#;

        let links = parse_flow_to_links(flow_json, "Approval").unwrap();

        // Check for flowable link with pattern (not "any")
        let flowable_link = links
            .iter()
            .find(|l| {
                l.source == "approval://ApprovalFlow"
                    && l.predicate == Some("ad4m://flowable".to_string())
            })
            .expect("Missing flowable link");

        // Should be a literal with JSON, not "ad4m://any"
        assert!(
            flowable_link.target.starts_with("literal://string:")
                || flowable_link.target.starts_with("literal:string:"),
            "Flowable should be encoded as literal JSON"
        );
        assert!(
            flowable_link.target.contains("predicate"),
            "Flowable literal should contain predicate"
        );
    }
}
