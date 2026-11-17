//! Which mount owns which class (design §2.2: one mount per class set).
//! A mount claims every class it installs with links it writes outside
//! its own address space, so they outlive an unmount and a restart:
//!
//! ```text
//! ad4m://mounted_class/<name> -[ad4m://mounted_by]->     <lang>://project
//! ad4m://mounted_class/<name> -[ad4m://mounted_target]-> <target class IRI>
//! ```
//!
//! A class resolves by its declared name, by the basename of its target
//! class (`load_shape`) and by its class node, so a shape set occupies all
//! three. Before any install (the first mount and every version refresh)
//! `check_class_ownership` refuses a shape set that occupies a name or
//! class IRI of a class this language does not own: another mount's, or
//! an SDNA class no mount installed. Only the owner may replace a class.

use std::collections::HashMap;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::Value;

use super::PerspectiveInstance;
use crate::languages::mount_types::MountShapes;
use crate::languages::LanguageController;
use crate::perspectives::model_query::utils::parse_literal_value;
use crate::types::{Link, LinkQuery};

const CLAIM_SOURCE_PREFIX: &str = "ad4m://mounted_class/";
const CLAIM_OWNER: &str = "ad4m://mounted_by";
const CLAIM_TARGET: &str = "ad4m://mounted_target";

/// One class a mount claimed.
struct ClassClaim {
    name: String,
    /// The language that installed it.
    owner: String,
    /// Every target class IRI it was installed with (a refresh that moved
    /// the class to a new IRI leaves the old class node installed too).
    targets: Vec<String>,
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

        // Every name and class IRI the shape set occupies, by the shape
        // that declares it.
        let mut names: HashMap<&str, &str> = HashMap::new();
        let mut targets: HashMap<&str, &str> = HashMap::new();
        for shape in &shapes.shapes {
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
            for target in &claim.targets {
                if targets.contains_key(target.as_str()) {
                    return Err(refuse(format!("class IRI {target}"), owner));
                }
                if names.contains_key(class_basename(target)) {
                    return Err(refuse(format!("class {}", class_basename(target)), owner));
                }
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
                        source,
                        predicate: Some(CLAIM_TARGET.to_string()),
                        target: target.to_string(),
                    });
                }
            }
        }
        if !links.is_empty() {
            self.add_mount_links(lang_address, links).await?;
        }
        Ok(())
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
                });
            }
        }
        for link in self.get_links(&by_predicate(CLAIM_TARGET)).await? {
            let Some(name) = link.data.source.strip_prefix(CLAIM_SOURCE_PREFIX) else {
                continue;
            };
            let Some(claim) = claims.iter_mut().find(|c| c.name == name) else {
                continue;
            };
            if written.wrote(&claim.owner, &link)? {
                claim.targets.push(link.data.target);
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
