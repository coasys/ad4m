//! Containment through the built-in `Container` on `PerspectiveInstance`
//! (#1446): the cached edge set, and the `children` of `Container` rows. The
//! rule lives in `model_query::containment`; this module caches and wires it,
//! as a child of `perspective_instance` to reach its private state.

use deno_core::error::AnyError;
use std::sync::Arc;

use super::PerspectiveInstance;
use crate::perspectives::model_query::builtin_classes::CONTAINER_URI;
use crate::perspectives::model_query::containment::{self, ContainmentEdges};
use crate::perspectives::model_query::{ModelQueryInput, ModelQueryResult, QueryClasses};

impl PerspectiveInstance {
    /// The containment edge set E: `has_child` on any node, and one edge per
    /// installed class whose `children` (by name or alias) is another
    /// predicate. Cached with the abstract-class resolution, until the
    /// installed shapes change.
    pub(crate) fn containment_edges(&self) -> Result<ContainmentEdges, AnyError> {
        let generation = {
            let cache = self.abstract_cache.read().unwrap();
            if let Some(hit) = &cache.containment {
                return Ok(hit.clone());
            }
            cache.generation
        };
        let resolution = self.resolve_abstract(CONTAINER_URI)?;
        let edges: ContainmentEdges =
            Arc::new(containment::edges(&resolution.classes, &resolution.views));
        let mut cache = self.abstract_cache.write().unwrap();
        // Computed against shapes that changed meanwhile: answer, don't keep.
        if cache.generation == generation {
            cache.containment = Some(edges.clone());
        }
        Ok(edges)
    }

    /// A query over `Container` reads each row through one of its classes,
    /// so a row's `children` would be that class's only. Replace it, and
    /// `__links.children`, with everything the edge set says the row holds.
    pub(super) async fn finish_container_rows(
        &self,
        classes: &QueryClasses,
        query: &ModelQueryInput,
        result: &mut ModelQueryResult,
    ) -> Result<(), AnyError> {
        if !matches!(classes, QueryClasses::One(name) if name == CONTAINER_URI) {
            return Ok(());
        }
        let edges = self.containment_edges()?;
        containment::fill_container_rows(
            &self.sparql_store,
            &mut result.instances,
            &edges,
            query.link_status.as_ref(),
            query.include_unverified,
        )
        .await
    }
}

#[cfg(test)]
#[path = "containment_tests.rs"]
mod tests;
