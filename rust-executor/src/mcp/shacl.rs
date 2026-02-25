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
}

/// Load all SHACL subject classes from a perspective
pub async fn load_classes(perspective: &PerspectiveInstance) -> Vec<ShaclClass> {
    let class_links = match perspective
        .get_links(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })
        .await
    {
        Ok(links) => links,
        Err(_) => return vec![],
    };

    let mut classes = Vec::new();
    for class_link in &class_links {
        let class_uri = &class_link.data.source;
        let class_name = class_uri
            .split("://")
            .last()
            .unwrap_or(class_uri)
            .to_string();

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
    let name_literal = format!("literal://string:shacl://{}", class_name);
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

        properties.push(ShaclProperty {
            name: prop_name,
            is_collection,
        });
    }

    properties
}

/// Find the original-cased class name from a lowercase name
pub async fn find_class_name(
    perspective: &PerspectiveInstance,
    class_name_lower: &str,
) -> Option<String> {
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
