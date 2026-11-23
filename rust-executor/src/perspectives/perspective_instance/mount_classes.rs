//! Which mount owns which class (design §2.2: one mount per class set).
//! A mount claims every class it installs with links it writes outside
//! its own address space, so they outlive an unmount and a restart:
//!
//! ```text
//! ad4m://mounted_class/<name> -[ad4m://mounted_by]->     <lang>://project
//! ad4m://mounted_class/<name> -[ad4m://mounted_target]-> <target class IRI>
//! ad4m://mounted_class/<name> -[ad4m://mounted_node]->   <SHACL node>
//! ```
//!
//! A class resolves by its declared name, by the basename of its target
//! class (`load_shape`) and by its class node, and its SHACL writes its
//! shape node and the property and action nodes under it. Before any
//! install (the first mount and every version refresh)
//! `check_class_ownership` refuses a shape set that occupies a name,
//! class IRI or SHACL node of a class this language does not own: another
//! mount's, or an SDNA class no mount installed. Only the owner may
//! replace a class. Claims count only when the claiming mount wrote them
//! (`mount_links`).

use std::collections::{HashMap, HashSet};

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;

use super::PerspectiveInstance;
use crate::languages::mount_types::{MountShape, MountShapes};
use crate::languages::LanguageController;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::perspectives::shacl_parser::parse_shacl_to_links;
use crate::types::{Link, LinkQuery};

const CLAIM_SOURCE_PREFIX: &str = "ad4m://mounted_class/";
const CLAIM_OWNER: &str = "ad4m://mounted_by";
const CLAIM_TARGET: &str = "ad4m://mounted_target";
const CLAIM_NODE: &str = "ad4m://mounted_node";

/// One class a mount claimed.
struct ClassClaim {
    name: String,
    /// The language that installed it.
    owner: String,
    /// Every target class IRI it was installed with (a refresh that moved
    /// the class to a new IRI leaves the old class node installed too).
    targets: Vec<String>,
    /// Every node its SHACL wrote: the shape node (a `node_shape_uri` may
    /// name any URI with the class's suffix), its property and action
    /// nodes, the class node and the name mapping.
    nodes: Vec<String>,
}

/// Every node the SHACL of `shape` writes as `parse_shacl_to_links` would:
/// each source of its links but `ad4m://self`.
fn shape_nodes(shape: &MountShape) -> Result<Vec<String>, AnyError> {
    let mut nodes: Vec<String> = parse_shacl_to_links(&shape.shape.to_string(), &shape.name)?
        .into_iter()
        .map(|l| l.source)
        .filter(|source| source != "ad4m://self")
        .collect();
    nodes.sort();
    nodes.dedup();
    Ok(nodes)
}

/// The name a class node resolves by, as `add_sdna` matches it: the last
/// path segment after the scheme (`mounttest://Thing` → `Thing`,
/// `literal:string:shacl://MountThing` → `MountThing`).
fn class_basename(node: &str) -> &str {
    let after_scheme = node.rsplit("://").next().unwrap_or(node);
    after_scheme.rsplit('/').next().unwrap_or(after_scheme)
}

/// The node `alias_mount_class_names` types as the class `name`.
fn alias_node(name: &str) -> String {
    format!("literal:string:shacl://{name}")
}

impl PerspectiveInstance {
    /// Refuse `shapes` for `lang_address` when one of its classes takes a
    /// name or class IRI that is in use and not `lang_address`'s own: a
    /// class another mount claimed, or an installed SDNA class no mount
    /// claimed. Also refuses a shape set that names one class twice.
    pub(super) async fn check_class_ownership(
        &self,
        lang_address: &str,
        shapes: &MountShapes,
    ) -> Result<(), AnyError> {
        let refuse = |what: String, owner: Option<&str>| {
            let held = match owner {
                Some(owner) => format!("it belongs to mounted language {owner}"),
                None => "it is a class no mount installed".to_string(),
            };
            anyhow!("cannot mount {lang_address}: {what} is already in this perspective and {held}")
        };

        // Every name, class IRI and shape node the shape set occupies, by
        // the shape that declares it.
        let mut declared: HashSet<&str> = HashSet::new();
        let mut names: HashMap<&str, &str> = HashMap::new();
        let mut targets: HashMap<&str, &str> = HashMap::new();
        let mut nodes: HashMap<String, &str> = HashMap::new();
        for shape in &shapes.shapes {
            if !declared.insert(&shape.name) {
                return Err(anyhow!(
                    "cannot mount {lang_address}: it declares class {} twice",
                    shape.name
                ));
            }
            for node in shape_nodes(shape)? {
                if let Some(other) = nodes.insert(node.clone(), &shape.name) {
                    return Err(anyhow!(
                        "cannot mount {lang_address}: classes {other} and {} both write shape node {node}",
                        shape.name
                    ));
                }
            }
            let target = shape.shape["target_class"].as_str().ok_or_else(|| {
                anyhow!(
                    "cannot mount {lang_address}: class {} has no target_class",
                    shape.name
                )
            })?;
            for name in [shape.name.as_str(), class_basename(target)] {
                if let Some(other) = names.insert(name, &shape.name) {
                    if other != shape.name {
                        return Err(anyhow!(
                            "cannot mount {lang_address}: classes {other} and {} both resolve as {name}",
                            shape.name
                        ));
                    }
                }
            }
            if let Some(other) = targets.insert(target, &shape.name) {
                return Err(anyhow!(
                    "cannot mount {lang_address}: classes {other} and {} both target {target}",
                    shape.name
                ));
            }
        }

        let claims = self.class_claims().await?;
        for claim in claims.iter().filter(|c| c.owner != lang_address) {
            let owner = Some(claim.owner.as_str());
            if names.contains_key(claim.name.as_str()) {
                return Err(refuse(format!("class {}", claim.name), owner));
            }
            if let Some(node) = claim.nodes.iter().find(|n| nodes.contains_key(n.as_str())) {
                return Err(refuse(format!("shape node {node}"), owner));
            }
            for target in &claim.targets {
                if targets.contains_key(target.as_str()) {
                    return Err(refuse(format!("class IRI {target}"), owner));
                }
                if names.contains_key(class_basename(target)) {
                    return Err(refuse(format!("class {}", class_basename(target)), owner));
                }
            }
        }

        // A node this language has not claimed must not be in the store
        // yet: it would be another class's SHACL (a non-mount class's, or
        // one a claim no longer names).
        let own_nodes: HashSet<&str> = claims
            .iter()
            .filter(|c| c.owner == lang_address)
            .flat_map(|c| c.nodes.iter().map(String::as_str))
            .collect();
        for node in nodes.keys().filter(|n| !own_nodes.contains(n.as_str())) {
            let in_use = !self
                .get_links(&LinkQuery {
                    source: Some(node.clone()),
                    ..Default::default()
                })
                .await?
                .is_empty();
            if in_use {
                return Err(refuse(format!("shape node {node}"), None));
            }
        }

        let owner_of_node = |node: &str| {
            claims
                .iter()
                .find(|c| c.targets.iter().any(|t| t == node) || alias_node(&c.name) == node)
                .map(|c| c.owner.as_str())
        };
        for node in self.class_nodes().await? {
            let name = class_basename(&node);
            let occupied = targets.contains_key(node.as_str()) || names.contains_key(name);
            let owner = owner_of_node(&node);
            if occupied && owner != Some(lang_address) {
                return Err(refuse(format!("class {name} ({node})"), owner));
            }
        }

        let owner_of_name = |name: &str| {
            claims
                .iter()
                .find(|c| c.name == name || c.targets.iter().any(|t| class_basename(t) == name))
                .map(|c| c.owner.as_str())
        };
        for name in self.subject_class_names().await? {
            let owner = owner_of_name(&name);
            if names.contains_key(name.as_str()) && owner != Some(lang_address) {
                return Err(refuse(format!("class {name}"), owner));
            }
        }
        Ok(())
    }

    /// Claim every class of `shapes` for `lang_address` (the links this
    /// language does not hold yet).
    ///
    /// Spike limit: claims are permanent. Neither `unmount` nor a refresh
    /// that drops a class or moves it to a new IRI or node releases
    /// anything, and a failed install keeps what it claimed. So the names,
    /// IRIs and nodes a language ever installed stay its own in this
    /// perspective: the same language address may remount and refresh,
    /// but no other language — a new version of it at a new address
    /// included — can take them. Releasing a claim safely needs the
    /// retained SDNA removed with it, which the spike does not do.
    pub(super) async fn claim_mount_classes(
        &mut self,
        lang_address: &str,
        shapes: &MountShapes,
    ) -> Result<(), AnyError> {
        let held: Vec<ClassClaim> = self
            .class_claims()
            .await?
            .into_iter()
            .filter(|c| c.owner == lang_address)
            .collect();
        let mut links = Vec::new();
        for shape in &shapes.shapes {
            let source = format!("{CLAIM_SOURCE_PREFIX}{}", shape.name);
            let claim = held.iter().find(|c| c.name == shape.name);
            if claim.is_none() {
                links.push(Link {
                    source: source.clone(),
                    predicate: Some(CLAIM_OWNER.to_string()),
                    target: format!("{lang_address}://project"),
                });
            }
            if let Some(target) = shape.shape["target_class"].as_str() {
                if !claim.is_some_and(|c| c.targets.iter().any(|t| t == target)) {
                    links.push(Link {
                        source: source.clone(),
                        predicate: Some(CLAIM_TARGET.to_string()),
                        target: target.to_string(),
                    });
                }
            }
            for node in shape_nodes(shape)? {
                if !claim.is_some_and(|c| c.nodes.contains(&node)) {
                    links.push(Link {
                        source: source.clone(),
                        predicate: Some(CLAIM_NODE.to_string()),
                        target: node,
                    });
                }
            }
        }
        if !links.is_empty() {
            self.add_mount_links(lang_address, links).await?;
        }
        Ok(())
    }

    /// The names and target class IRIs of every class `lang_address`
    /// claimed: the classes its records may name.
    pub(super) async fn mount_own_classes(
        &self,
        lang_address: &str,
    ) -> Result<HashSet<String>, AnyError> {
        Ok(self
            .class_claims()
            .await?
            .into_iter()
            .filter(|c| c.owner == lang_address)
            .flat_map(|c| std::iter::once(c.name).chain(c.targets))
            .collect())
    }

    /// Every class claim in the store.
    async fn class_claims(&self) -> Result<Vec<ClassClaim>, AnyError> {
        let by_predicate = |predicate: &str| LinkQuery {
            predicate: Some(predicate.to_string()),
            ..Default::default()
        };
        // A claim counts only when the mount it names wrote it.
        let mut written = self.mount_written_by_language();
        let mut claims: Vec<ClassClaim> = Vec::new();
        for link in self.get_links(&by_predicate(CLAIM_OWNER)).await? {
            let Some(name) = link.data.source.strip_prefix(CLAIM_SOURCE_PREFIX) else {
                continue;
            };
            let Ok((owner, _)) = LanguageController::parse_expr_url(&link.data.target) else {
                continue;
            };
            if written.wrote(&owner, &link)? {
                claims.push(ClassClaim {
                    name: name.to_string(),
                    owner,
                    targets: Vec::new(),
                    nodes: Vec::new(),
                });
            }
        }
        for predicate in [CLAIM_TARGET, CLAIM_NODE] {
            for link in self.get_links(&by_predicate(predicate)).await? {
                let Some(name) = link.data.source.strip_prefix(CLAIM_SOURCE_PREFIX) else {
                    continue;
                };
                let Some(claim) = claims.iter_mut().find(|c| c.name == name) else {
                    continue;
                };
                if written.wrote(&claim.owner, &link)? {
                    if predicate == CLAIM_TARGET {
                        claim.targets.push(link.data.target);
                    } else {
                        claim.nodes.push(link.data.target);
                    }
                }
            }
        }
        Ok(claims)
    }

    /// Every class node of the installed SHACL (`rdf://type
    /// ad4m://SubjectClass`).
    async fn class_nodes(&self) -> Result<Vec<String>, AnyError> {
        let mut nodes: Vec<String> = self
            .get_links(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })
            .await?
            .into_iter()
            .map(|l| l.data.source)
            .collect();
        nodes.sort();
        nodes.dedup();
        Ok(nodes)
    }

    /// Every class name `add_sdna` registered (`ad4m://self
    /// -[ad4m://has_subject_class]-> literal:string:<name>`).
    async fn subject_class_names(&self) -> Result<Vec<String>, AnyError> {
        Ok(self
            .get_links(&LinkQuery {
                source: Some("ad4m://self".to_string()),
                predicate: Some("ad4m://has_subject_class".to_string()),
                ..Default::default()
            })
            .await?
            .into_iter()
            .filter_map(|l| match parse_literal_value(&l.data.target) {
                Value::String(name) => Some(name),
                _ => None,
            })
            .collect())
    }
}
