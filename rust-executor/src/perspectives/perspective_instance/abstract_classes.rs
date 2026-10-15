//! Abstract model classes on `PerspectiveInstance` (#1435): which concrete
//! classes an abstract class reads, cached until the installed shapes change;
//! the query and subscription wiring; and the refusal to create one.
//!
//! The rule itself lives in `model_query::abstract_classes`; this module only
//! caches and wires it. A child module of `perspective_instance` so it can
//! reach the shape cache and the subscription registry without widening
//! their visibility.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::PerspectiveInstance;
use crate::perspectives::model_query::abstract_classes::{
    self, AbstractIndex, Resolution, ViewResolver,
};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::{ModelQueryInput, QueryClasses};

/// What the installed shapes say about abstract classes, and how many times
/// they have changed. A subscription over an abstract class keeps the
/// generation it resolved at and resolves again when it moves.
#[derive(Default)]
pub(super) struct AbstractCache {
    generation: u64,
    index: Option<Arc<AbstractIndex>>,
    resolved: HashMap<String, Arc<Resolution>>,
}

/// The concrete classes a model query or subscription reads: the classes it
/// names, or those an abstract class resolves to, with their views.
pub(super) struct ReadTargets {
    pub(super) classes: QueryClasses,
    pub(super) views: HashMap<String, Arc<ModelShape>>,
    /// Set when the caller named an abstract class.
    pub(super) resolution: Option<Arc<Resolution>>,
}

impl PerspectiveInstance {
    /// Drop every cached shape, and what was derived from them.
    pub(super) fn clear_shape_cache(&self) {
        self.shape_cache.write().unwrap().clear();
        self.forget_derived_shapes();
    }

    /// The installed shapes changed: a class was added, refreshed or synced
    /// in. Any of these can change which classes are abstract, which aliases
    /// exist and which classes an abstract class reads.
    pub(super) fn forget_derived_shapes(&self) {
        let mut cache = self.abstract_cache.write().unwrap();
        cache.generation += 1;
        cache.index = None;
        cache.resolved.clear();
    }

    /// How many times the installed shapes have changed. Read it *before*
    /// resolving: a change that lands mid-resolution moves it, so the result
    /// is never mistaken for current.
    pub(super) fn shapes_generation(&self) -> u64 {
        self.abstract_cache.read().unwrap().generation
    }

    /// The abstract classes and aliases installed here, cached.
    pub(super) fn abstract_index(&self) -> Result<Arc<AbstractIndex>, AnyError> {
        let generation = {
            let cache = self.abstract_cache.read().unwrap();
            if let Some(index) = &cache.index {
                return Ok(index.clone());
            }
            cache.generation
        };
        let index = Arc::new(AbstractIndex::load(&self.sparql_store)?);
        let mut cache = self.abstract_cache.write().unwrap();
        // Computed against shapes that changed meanwhile: answer, don't keep.
        if cache.generation == generation {
            cache.index = Some(index.clone());
        }
        Ok(index)
    }

    /// Every installed concrete class `abstract_class` reads, with its view.
    /// Cached until the installed shapes change.
    pub(super) fn resolve_abstract(
        &self,
        abstract_class: &str,
    ) -> Result<Arc<Resolution>, AnyError> {
        let generation = {
            let cache = self.abstract_cache.read().unwrap();
            if let Some(hit) = cache.resolved.get(abstract_class) {
                return Ok(hit.clone());
            }
            cache.generation
        };
        let index = self.abstract_index()?;
        let resolution = Arc::new(abstract_classes::resolve(
            &self.sparql_store,
            &self.shape_resolver(),
            &index,
            abstract_class,
        )?);
        let mut cache = self.abstract_cache.write().unwrap();
        if cache.generation == generation {
            cache
                .resolved
                .insert(abstract_class.to_string(), resolution.clone());
        }
        Ok(resolution)
    }

    /// The concrete classes `classes` reads. An abstract class is read on
    /// its own: in a union of several classes its views would have to
    /// apply to some rows and not others.
    ///
    /// What a union refuses is refused for an abstract class too, before it
    /// is resolved, so the answer does not depend on which classes are
    /// installed.
    pub(super) fn read_targets(
        &self,
        classes: &QueryClasses,
        query: &ModelQueryInput,
    ) -> Result<ReadTargets, AnyError> {
        let index = self.abstract_index()?;
        let abstract_class = match classes {
            QueryClasses::One(name) if index.is_abstract(name) => name,
            QueryClasses::Union(names) => {
                if let Some(name) = names.iter().find(|n| index.is_abstract(n)) {
                    return Err(anyhow!(
                        "classNames: '{name}' is an abstract class. Query it on its own: it \
                         already reads every class that has its properties."
                    ));
                }
                return Ok(ReadTargets::named(classes));
            }
            QueryClasses::One(_) => return Ok(ReadTargets::named(classes)),
        };
        crate::perspectives::model_query::refuse_union_reads(query)?;
        let resolution = self.resolve_abstract(abstract_class)?;
        Ok(ReadTargets {
            classes: QueryClasses::union(resolution.classes.clone()),
            views: resolution.views.clone(),
            resolution: Some(resolution),
        })
    }

    /// [`Self::read_targets`] for a subscription's trigger, which is built
    /// from shapes alone: a query that does not parse is answered by the
    /// query run, not here.
    pub(super) fn trigger_targets(&self, classes: &QueryClasses, query_json: &str) -> ReadTargets {
        let query: ModelQueryInput = serde_json::from_str(query_json).unwrap_or_default();
        self.read_targets(classes, &query)
            .unwrap_or_else(|_| ReadTargets::named(classes))
    }

    /// `create_subject` on an abstract class: refused, it names what records
    /// have, not what they are.
    pub(super) fn refuse_abstract_create(&self, class_name: &str) -> Result<(), AnyError> {
        if self.abstract_index()?.is_abstract(class_name) {
            return Err(anyhow!(
                "'{class_name}' is an abstract class and cannot be created. Create an instance \
                 of a class that has its properties; querying '{class_name}' reads it."
            ));
        }
        Ok(())
    }

    /// Resolve every subscription over an abstract class (#1435) again whose
    /// classes were resolved before the installed shapes last changed.
    /// Returns the ids whose class list changed: those re-run this pass
    /// whatever was written, because their old trigger knows nothing of the
    /// new classes.
    pub(super) async fn refresh_abstract_subscriptions(&self) -> HashSet<String> {
        let generation = self.shapes_generation();
        let stale: Vec<(String, QueryClasses, String)> = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .filter_map(|(id, q)| {
                    let params = q.model_query_params.as_ref()?;
                    match &params.abstract_read {
                        Some((at, _)) if *at != generation => Some((
                            id.clone(),
                            params.classes.clone(),
                            params.query_json.clone(),
                        )),
                        _ => None,
                    }
                })
                .collect()
        };
        let mut changed = HashSet::new();
        for (id, classes, query_json) in stale {
            let targets = self.trigger_targets(&classes, &query_json);
            let Some(resolution) = targets.resolution.as_ref() else {
                // No longer abstract (re-registered as a concrete class): the
                // re-run reads it as named; watch it as named.
                let (trigger_sparql, predicates) =
                    self.model_trigger_query(&targets, &query_json);
                let mut queries = self.subscribed_queries.lock().await;
                let Some(query) = queries.get_mut(&id) else {
                    continue;
                };
                let trigger = self.build_model_trigger_for(&targets, &query_json, &query.last_result);
                if let Some(params) = query.model_query_params.as_mut() {
                    params.abstract_read = None;
                    params.trigger = trigger;
                }
                query.query = trigger_sparql;
                query.predicates = predicates;
                changed.insert(id);
                continue;
            };
            let mut queries = self.subscribed_queries.lock().await;
            let Some(query) = queries.get_mut(&id) else {
                continue;
            };
            let same = query
                .model_query_params
                .as_ref()
                .and_then(|p| p.abstract_read.as_ref())
                .is_some_and(|(_, read)| *read == resolution.classes);
            let rebuilt = (!same).then(|| {
                (
                    self.model_trigger_query(&targets, &query_json),
                    self.build_model_trigger_for(&targets, &query_json, &query.last_result),
                )
            });
            let Some(params) = query.model_query_params.as_mut() else {
                continue;
            };
            params.abstract_read = Some((generation, resolution.classes.clone()));
            if let Some(((trigger_sparql, predicates), trigger)) = rebuilt {
                params.trigger = trigger;
                query.query = trigger_sparql;
                query.predicates = predicates;
                changed.insert(id);
            }
        }
        changed
    }

    /// [`Self::build_model_trigger`] over `targets`, reading the views of an
    /// abstract class's classes.
    pub(super) fn build_model_trigger_for(
        &self,
        targets: &ReadTargets,
        query_json: &str,
        result: &str,
    ) -> super::subscription_triggers::ModelTrigger {
        let base = self.shape_resolver();
        let resolver = ViewResolver::new(&base, &targets.views);
        self.build_model_trigger_with(
            &resolver,
            targets.classes.names().iter().map(String::as_str),
            query_json,
            result,
        )
    }
}

impl ReadTargets {
    fn named(classes: &QueryClasses) -> Self {
        ReadTargets {
            classes: classes.clone(),
            views: HashMap::new(),
            resolution: None,
        }
    }
}
