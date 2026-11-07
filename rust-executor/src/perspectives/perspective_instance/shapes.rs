//! Class registration: `register_shapes` writes the shapes of model classes
//! and `shapes` reads them back, both against one cached registry.
//!
//! A class is its target class IRI. Its links are the two index links on
//! that IRI (`rdf://type ad4m://SubjectClass`, `ad4m://shape`), its shape
//! node and one node per property, as `shacl_parser::shape_links` writes
//! them. Those are the class's *own* links (`shacl_parser::is_own_link`);
//! any other link on the same nodes is an annotation and is never touched.
//!
//! Registering compares the own links a shape gives with the own links
//! stored, as triples (source, predicate, target as the store reads it
//! back). Equal sets write nothing. Otherwise one diff adds the missing
//! triples and removes every copy of each stale one. Copies of an expected
//! triple stay: two peers that registered the same shape agree.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::sync::Arc;
use ts_rs::TS;

use super::PerspectiveInstance;
use crate::agent::AgentContext;
use crate::perspectives::shacl_parser::{
    canonical_json, class_name_of, is_own_link, shape_from_links, shape_links, SHACLShape,
    ShapeNode, SHAPE_PREDICATE, SUBJECT_CLASS,
};
use crate::perspectives::sparql_store::canonical_target;
use crate::types::{DecoratedLinkExpression, Link, LinkMutations, LinkStatus};

/// What `register_shapes` did, by class name.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RegisterResult {
    /// Classes whose stored shape already was the one given: nothing written.
    pub unchanged: Vec<String>,
    /// Classes written or rewritten.
    pub written: Vec<String>,
    /// The shapes generation once the registry holds the new shapes. It
    /// grows with every change to the installed shapes.
    #[ts(type = "number")]
    pub generation: u64,
}

/// A registered class: its name, its shape, and the SHA-256 (hex) of the
/// shape's canonical JSON.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ShapeSummary {
    pub name: String,
    pub shape: SHACLShape,
    pub hash: String,
}

/// The identity of a stored triple: the target as the store reads it back,
/// so `literal:string:a b` and `literal:string:a%20b` are one triple.
type Triple = (String, String, String);

fn triple(link: &Link) -> Triple {
    (
        link.source.clone(),
        link.predicate.clone().unwrap_or_default(),
        canonical_target(&link.target),
    )
}

/// The SHA-256 (hex) of `shape`'s canonical JSON.
pub(crate) fn shape_hash(shape: &SHACLShape) -> String {
    hex::encode(Sha256::digest(canonical_json(shape).as_bytes()))
}

impl PerspectiveInstance {
    /// The own links stored for the class `target_class`: its index links,
    /// those of every shape it points to, and of every property those shapes
    /// list.
    fn own_shape_links(
        &self,
        target_class: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let own_of = |node: ShapeNode, source: &str| -> Result<_, AnyError> {
            Ok(self
                .sparql_store
                .get_links_by_source(source)?
                .into_iter()
                .filter(|l| {
                    l.data
                        .predicate
                        .as_deref()
                        .is_some_and(|p| is_own_link(node, p, &l.data.target))
                })
                .collect::<Vec<DecoratedLinkExpression>>())
        };
        let targets_of = |links: &[DecoratedLinkExpression], predicate: &str| -> BTreeSet<String> {
            links
                .iter()
                .filter(|l| l.data.predicate.as_deref() == Some(predicate))
                .map(|l| l.data.target.clone())
                .collect()
        };
        let mut own = own_of(ShapeNode::Class, target_class)?;
        for shape in targets_of(&own, SHAPE_PREDICATE) {
            let shape_links = own_of(ShapeNode::Shape, &shape)?;
            for property in targets_of(&shape_links, "sh://property") {
                own.extend(own_of(ShapeNode::Property, &property)?);
            }
            own.extend(shape_links);
        }
        Ok(own)
    }

    /// Every registered class with a readable shape, sorted by name, then by
    /// target class. Cached until the installed shapes change.
    pub(crate) fn shape_registry(&self) -> Result<Arc<Vec<ShapeSummary>>, AnyError> {
        let generation = {
            let cache = self.abstract_cache.read().unwrap();
            if let Some(registry) = &cache.registry {
                return Ok(registry.clone());
            }
            cache.generation
        };
        let classes: BTreeSet<String> = self
            .sparql_store
            .query_links(
                None,
                Some("rdf://type"),
                Some(SUBJECT_CLASS),
                None,
                None,
                None,
            )?
            .into_iter()
            .map(|l| l.data.source)
            .collect();
        let mut registry = Vec::new();
        for target_class in classes {
            let links: Vec<Link> = self
                .own_shape_links(&target_class)?
                .into_iter()
                .map(|l| l.data)
                .collect();
            if let Some(shape) = shape_from_links(&target_class, &links) {
                registry.push(ShapeSummary {
                    name: class_name_of(&target_class),
                    hash: shape_hash(&shape),
                    shape,
                });
            }
        }
        registry.sort_by(|a, b| {
            (&a.name, &a.shape.target_class).cmp(&(&b.name, &b.shape.target_class))
        });
        let registry = Arc::new(registry);
        let mut cache = self.abstract_cache.write().unwrap();
        // Read against shapes that changed meanwhile: answer, don't keep.
        if cache.generation == generation {
            cache.registry = Some(registry.clone());
        }
        Ok(registry)
    }

    /// The registered classes named in `names` (all of them for `None`).
    pub fn shapes(&self, names: Option<Vec<String>>) -> Result<Vec<ShapeSummary>, AnyError> {
        let registry = self.shape_registry()?;
        Ok(match names {
            None => registry.as_ref().clone(),
            Some(names) => {
                let names: HashSet<String> = names.into_iter().collect();
                registry
                    .iter()
                    .filter(|s| names.contains(&s.name))
                    .cloned()
                    .collect()
            }
        })
    }

    /// The names of the registered classes, sorted, each once.
    pub fn class_names(&self) -> Result<Vec<String>, AnyError> {
        let mut names: Vec<String> = self
            .shape_registry()?
            .iter()
            .map(|s| s.name.clone())
            .collect();
        names.dedup();
        Ok(names)
    }

    /// Register `shapes`, each as the class of its target class IRI.
    ///
    /// A class whose stored own links already are the ones its shape gives
    /// is left alone. For the others, one diff (Shared) adds the missing
    /// links and removes the stale own links; links other apps put on the
    /// class, shape or property nodes stay. A shape given a new
    /// `node_shape_uri` moves: the class points to the new shape node and
    /// the old one's own links go.
    ///
    /// Answers once the registry and the shape cache hold the new shapes.
    /// Refuses, before writing anything: a shape the encoder refuses, two
    /// shapes for one target class, and a shape URI another class holds.
    pub async fn register_shapes(
        &mut self,
        shapes: Vec<SHACLShape>,
        context: &AgentContext,
    ) -> Result<RegisterResult, AnyError> {
        let mut expected: BTreeMap<String, (String, Vec<Link>)> = BTreeMap::new();
        for shape in shapes {
            let target_class = shape.target_class.clone();
            let name = class_name_of(&target_class);
            let links = shape_links(shape, &name)?;
            if expected
                .insert(target_class.clone(), (name, links))
                .is_some()
            {
                return Err(anyhow!(
                    "Two shapes for class '{target_class}' in one call: give each class once."
                ));
            }
        }
        let shape_uris: BTreeMap<&str, &str> = expected
            .iter()
            .flat_map(|(target_class, (_, links))| {
                links
                    .iter()
                    .filter(|l| {
                        l.source == *target_class && l.predicate.as_deref() == Some(SHAPE_PREDICATE)
                    })
                    .map(move |l| (l.target.as_str(), target_class.as_str()))
            })
            .collect();
        if shape_uris.len() < expected.len() {
            return Err(anyhow!(
                "Two classes in one call share a shape URI: give each class its own."
            ));
        }

        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;

        for (shape_uri, target_class) in &shape_uris {
            let holders = self.sparql_store.query_links(
                None,
                Some(SHAPE_PREDICATE),
                Some(shape_uri),
                None,
                None,
                None,
            )?;
            if let Some(other) = holders.iter().find(|l| l.data.source != *target_class) {
                return Err(anyhow!(
                    "Shape '{shape_uri}' already belongs to class '{}': class '{target_class}' \
                     needs its own shape URI.",
                    other.data.source
                ));
            }
        }

        let mut unchanged = Vec::new();
        let mut written = Vec::new();
        let mut mutations = LinkMutations::default();
        for (target_class, (name, links)) in expected {
            let wanted: BTreeSet<Triple> = links.iter().map(triple).collect();
            let stored = self.own_shape_links(&target_class)?;
            let held: BTreeSet<Triple> = stored.iter().map(|l| triple(&l.data)).collect();
            if wanted == held {
                unchanged.push(name);
                continue;
            }
            let mut added = BTreeSet::new();
            mutations.additions.extend(
                links
                    .into_iter()
                    .filter(|l| {
                        let t = triple(l);
                        !held.contains(&t) && added.insert(t)
                    })
                    .map(Into::into),
            );
            mutations.removals.extend(
                stored
                    .into_iter()
                    .filter(|l| !wanted.contains(&triple(&l.data)))
                    .map(Into::into),
            );
            written.push(name);
        }

        if !written.is_empty() {
            self.link_mutations(mutations, LinkStatus::Shared, context)
                .await?;
            for name in &written {
                self.invalidate_shape(name);
            }
        }
        self.shape_registry()?;
        for name in &written {
            if let Err(e) = self.get_shape(name) {
                log::warn!("register_shapes: '{name}' is stored but does not load: {e:#}");
            }
        }
        unchanged.sort();
        written.sort();
        Ok(RegisterResult {
            unchanged,
            written,
            generation: self.shapes_generation(),
        })
    }

    /// [`Self::register_shapes`] for one shape given as JSON.
    pub async fn register_shape_json(
        &mut self,
        shape_json: &str,
        context: &AgentContext,
    ) -> Result<RegisterResult, AnyError> {
        let shape: SHACLShape = serde_json::from_str(shape_json)
            .map_err(|e| anyhow!("Failed to parse SHACL JSON: {e}"))?;
        self.register_shapes(vec![shape], context).await
    }
}

#[cfg(test)]
#[path = "shapes_tests.rs"]
mod tests;
