//! SHACL class parsing for MCP tool generation
//!
//! Extracts subject class definitions from SHACL links in perspectives,
//! providing typed structures for generating dynamic MCP tools.

use crate::graphql::graphql_types::LinkQuery;
use crate::perspectives::perspective_instance::PerspectiveInstance;

/// A SHACL subject class with its properties
#[derive(Debug, Clone)]
pub struct ShaclClass {
    /// Class name (e.g., "Channel", "Message", "Task")
    pub name: String,
    /// Lowercase class name for tool naming
    pub name_lower: String,
    /// Properties defined on this class
    pub properties: Vec<ShaclProperty>,
}

/// A property on a SHACL subject class
#[derive(Debug, Clone)]
pub struct ShaclProperty {
    /// Property name (e.g., "name", "content", "status")
    pub name: String,
    /// Whether this is a collection (max_count > 1) or a scalar property
    pub is_collection: bool,
    /// The predicate URI used in links for this property (from sh://path)
    pub predicate: Option<String>,
    /// SHACL datatype constraint (e.g., "xsd://string", "xsd://dateTime")
    pub datatype: Option<String>,
    /// Minimum cardinality (sh://minCount)
    pub min_count: Option<u32>,
    /// Maximum cardinality (sh://maxCount)
    pub max_count: Option<u32>,
    /// Node kind (e.g., "sh://IRI", "sh://Literal")
    pub node_kind: Option<String>,
}

impl ShaclClass {
    /// Get scalar (non-collection) properties
    pub fn scalar_properties(&self) -> Vec<&ShaclProperty> {
        self.properties
            .iter()
            .filter(|p| !p.is_collection)
            .collect()
    }

    /// Get collection properties
    pub fn collection_properties(&self) -> Vec<&ShaclProperty> {
        self.properties.iter().filter(|p| p.is_collection).collect()
    }

    /// Get required properties (min_count >= 1)
    pub fn required_properties(&self) -> Vec<&ShaclProperty> {
        self.properties
            .iter()
            .filter(|p| p.min_count.unwrap_or(0) >= 1)
            .collect()
    }
}

impl ShaclProperty {
    /// Whether this property is required (min_count >= 1)
    pub fn is_required(&self) -> bool {
        self.min_count.unwrap_or(0) >= 1
    }

    /// Human-readable type description for tool documentation
    pub fn type_description(&self) -> String {
        let base = if let Some(ref dt) = self.datatype {
            dt.replace("xsd://", "").replace("xsd:", "")
        } else if let Some(ref nk) = self.node_kind {
            if nk.contains("IRI") {
                "URI reference".to_string()
            } else {
                "value".to_string()
            }
        } else {
            "string".to_string()
        };

        if self.is_collection {
            format!("{} (collection)", base)
        } else {
            base
        }
    }
}

/// Load all SHACL subject classes from a perspective.
/// Reuses PerspectiveInstance::get_subject_classes_from_shacl() for class discovery,
/// then enriches with property metadata from SHACL links.
pub async fn load_classes(perspective: &PerspectiveInstance) -> Vec<ShaclClass> {
    let class_names = match perspective.get_subject_classes_from_shacl().await {
        Ok(names) => names,
        Err(_) => return vec![],
    };

    let mut classes = Vec::new();
    for class_name in class_names {
        let properties = load_class_properties(perspective, &class_name).await;
        classes.push(ShaclClass {
            name_lower: class_name.to_lowercase(),
            name: class_name,
            properties,
        });
    }

    classes
}

/// Load a single class by name from a perspective
pub async fn load_class(perspective: &PerspectiveInstance, class_name: &str) -> Option<ShaclClass> {
    let classes = load_classes(perspective).await;
    classes
        .into_iter()
        .find(|c| c.name.to_lowercase() == class_name.to_lowercase())
}

/// Extract property information from a SHACL shape
pub async fn load_class_properties(
    perspective: &PerspectiveInstance,
    class_name: &str,
) -> Vec<ShaclProperty> {
    let name_literal =
        ad4m_client::literal::Literal::from_string(format!("shacl://{}", class_name))
            .to_url()
            .unwrap_or_else(|_| format!("literal://string:shacl://{}", class_name));
    let shape_links = match perspective
        .get_links(&LinkQuery {
            source: Some(name_literal),
            predicate: Some("ad4m://shacl_shape_uri".to_string()),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(_) => return vec![],
    };

    if shape_links.is_empty() {
        return vec![];
    }

    let shape_uri = &shape_links[0].data.target;
    let prop_links = match perspective
        .get_links(&LinkQuery {
            source: Some(shape_uri.clone()),
            predicate: Some("sh://property".to_string()),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(_) => return vec![],
    };

    let mut properties = Vec::new();
    for prop_link in &prop_links {
        let prop_uri = &prop_link.data.target;
        let prop_name = prop_uri
            .rsplit_once('.')
            .map(|(_, name)| name.to_string())
            .unwrap_or_else(|| prop_uri.clone());

        let is_collection = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://CollectionShape".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) => !links.is_empty(),
            Err(_) => false,
        };

        // Get the predicate URI (sh://path link)
        let predicate = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("sh://path".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) if !links.is_empty() => Some(links[0].data.target.clone()),
            _ => None,
        };

        // Get datatype constraint (sh://datatype)
        let datatype = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("sh://datatype".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) if !links.is_empty() => Some(links[0].data.target.clone()),
            _ => None,
        };

        // Get min/max count (sh://minCount, sh://maxCount)
        let min_count = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("sh://minCount".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) if !links.is_empty() => {
                ad4m_client::literal::Literal::from_url(links[0].data.target.clone())
                    .ok()
                    .and_then(|l| l.get().ok())
                    .and_then(|v| v.to_string().parse::<u32>().ok())
            }
            _ => None,
        };

        let max_count = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("sh://maxCount".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) if !links.is_empty() => {
                ad4m_client::literal::Literal::from_url(links[0].data.target.clone())
                    .ok()
                    .and_then(|l| l.get().ok())
                    .and_then(|v| v.to_string().parse::<u32>().ok())
            }
            _ => None,
        };

        // Get node kind (sh://nodeKind)
        let node_kind = match perspective
            .get_links(&LinkQuery {
                source: Some(prop_uri.clone()),
                predicate: Some("sh://nodeKind".to_string()),
                ..Default::default()
            })
            .await
        {
            Ok(links) if !links.is_empty() => Some(links[0].data.target.clone()),
            _ => None,
        };

        properties.push(ShaclProperty {
            name: prop_name,
            is_collection,
            predicate,
            datatype,
            min_count,
            max_count,
            node_kind,
        });
    }

    properties
}

/// Find the original-cased class name from a lowercase name
pub async fn find_class_name(
    perspective: &PerspectiveInstance,
    class_name_lower: &str,
) -> Option<String> {
    let class_names = match perspective.get_subject_classes_from_shacl().await {
        Ok(names) => names,
        Err(_) => return None,
    };

    for name in &class_names {
        if name.to_lowercase() == class_name_lower {
            return Some(name.clone());
        }
    }

    // Fallback: direct link query for non-standard URIs
    let class_links = match perspective
        .get_links(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(_) => return None,
    };

    for link in &class_links {
        let name = link
            .data
            .source
            .split("://")
            .last()
            .unwrap_or(&link.data.source);
        if name.to_lowercase() == class_name_lower {
            return Some(name.to_string());
        }
    }

    None
}

/// Resolve a property name to its predicate URI for a given class.
/// This is the common operation needed by MCP tools to read/write properties.
pub async fn resolve_property_predicate(
    perspective: &PerspectiveInstance,
    class_name: &str,
    property_name: &str,
) -> Result<String, String> {
    let properties = load_class_properties(perspective, class_name).await;
    if properties.is_empty() {
        return Err(format!("No SHACL shape found for class '{}'", class_name));
    }
    for prop in &properties {
        if prop.name == property_name {
            return prop.predicate.clone().ok_or_else(|| {
                format!(
                    "Property '{}' on class '{}' has no predicate URI",
                    property_name, class_name
                )
            });
        }
    }
    let available: Vec<&str> = properties.iter().map(|p| p.name.as_str()).collect();
    Err(format!(
        "Property '{}' not found on class '{}'. Available: {}",
        property_name,
        class_name,
        available.join(", ")
    ))
}
