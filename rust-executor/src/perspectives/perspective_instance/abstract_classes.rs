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
use crate::perspectives::model_query::builtin_classes::builtin;
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
    /// The classes could not be resolved (a store error): a subscription
    /// tries again on its next pass rather than keeping this answer.
    pub(super) unresolved: bool,
}

/// The generation a subscription that could not be resolved records: never a
/// real one, so the next pass resolves it again.
pub(super) const UNRESOLVED: u64 = u64::MAX;

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
        #[cfg(test)]
        if faults::take(&self.uuid) {
            return Err(anyhow!("injected store error"));
        }
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
    /// Cached until the installed shapes change. `abstract_class` is a stored
    /// abstract class's name or a built-in's URI (`ad4m://TextContent`).
    pub fn resolve_abstract(&self, abstract_class: &str) -> Result<Arc<Resolution>, AnyError> {
        let generation = {
            let cache = self.abstract_cache.read().unwrap();
            if let Some(hit) = cache.resolved.get(abstract_class) {
                return Ok(hit.clone());
            }
            cache.generation
        };
        let index = self.abstract_index()?;
        // A built-in by its URI, from `builtin_classes`, never from the store:
        // an app class of the same name does not change what it reads.
        let (alias_key, shape) = match builtin(abstract_class) {
            Some(b) => (b.name, b.shape.clone()),
            None => (abstract_class, self.get_shape(abstract_class)?),
        };
        let resolution = Arc::new(abstract_classes::resolve(
            &self.sparql_store,
            &self.shape_resolver(),
            &index,
            alias_key,
            &shape,
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
        let is_abstract = |name: &str| builtin(name).is_some() || index.is_abstract(name);
        let abstract_class = match classes {
            QueryClasses::One(name) if is_abstract(name) => name,
            QueryClasses::Union(names) => {
                if let Some(name) = names.iter().find(|n| is_abstract(n)) {
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
            unresolved: false,
        })
    }

    /// [`Self::read_targets`] for a subscription's trigger, which is built
    /// from shapes alone: a query that does not parse is answered by the
    /// query run, not here.
    ///
    /// On an error the classes are watched as named and the targets are
    /// marked `unresolved`, so the subscription records [`UNRESOLVED`] and
    /// is resolved again on its next pass: one transient store error must
    /// not leave it watching predicates no class writes.
    pub(super) fn trigger_targets(&self, classes: &QueryClasses, query_json: &str) -> ReadTargets {
        let query: ModelQueryInput = serde_json::from_str(query_json).unwrap_or_default();
        self.read_targets(classes, &query).unwrap_or_else(|e| {
            log::warn!(
                "subscription over {:?}: its classes did not resolve, trying again on the \
                 next pass: {e}",
                classes.names()
            );
            ReadTargets {
                unresolved: true,
                ..ReadTargets::named(classes)
            }
        })
    }

    /// What a subscription over `targets` records as read (see
    /// `ModelSubscriptionParams::abstract_read`).
    pub(super) fn abstract_read(targets: &ReadTargets, generation: u64) -> Option<u64> {
        if targets.unresolved {
            return Some(UNRESOLVED);
        }
        targets.resolution.as_ref().map(|_| generation)
    }

    /// Whether `class_name` is an abstract class here: a built-in's URI
    /// (`ad4m://TextContent`), or a stored class marked abstract.
    pub(crate) fn is_abstract_class(&self, class_name: &str) -> Result<bool, AnyError> {
        Ok(builtin(class_name).is_some() || self.abstract_index()?.is_abstract(class_name))
    }

    /// A write through an abstract class: refused, it names what records
    /// have, not what they are. Its predicates are not the ones a record's
    /// own class writes, so a write through it lands where no read looks.
    /// Every subject write (create, update, delete) checks here first.
    pub(crate) fn refuse_abstract_write(
        &self,
        class_name: &str,
        write: AbstractWrite,
    ) -> Result<(), AnyError> {
        if !self.is_abstract_class(class_name)? {
            return Ok(());
        }
        Err(match write {
            AbstractWrite::Create => anyhow!(
                "'{class_name}' is an abstract class and cannot be created. Create an instance \
                 of a class that has its properties; querying '{class_name}' reads it."
            ),
            AbstractWrite::Update | AbstractWrite::Delete => anyhow!(
                "'{class_name}' is an abstract class: a record cannot be {} through it. Write \
                 it through its own class, the `__subjectClass` a query on '{class_name}' \
                 returns.",
                write.done()
            ),
        })
    }

    /// Resolve every subscription over an abstract class (#1435) again whose
    /// classes were resolved before the installed shapes last changed, and
    /// rebuild its trigger. Returns their ids: those re-run this pass
    /// whatever was written. A change can move the classes, or only a view
    /// (an alias moved from `body` to `content`, same class list); either
    /// way the old trigger watches the wrong predicates and the last result
    /// may read the wrong ones. One still unresolved is left for the next
    /// pass.
    pub(super) async fn refresh_abstract_subscriptions(&self) -> HashSet<String> {
        let generation = self.shapes_generation();
        let stale: Vec<(String, QueryClasses, String)> = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .filter_map(|(id, q)| {
                    let params = q.model_query_params.as_ref()?;
                    match &params.abstract_read {
                        Some(at) if *at != generation => Some((
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
            if targets.unresolved {
                continue;
            }
            // No resolution: no longer abstract (re-registered as a concrete
            // class). The re-run reads it as named; watch it as named.
            let (trigger_sparql, predicates) = self.model_trigger_query(&targets, &query_json);
            let mut queries = self.subscribed_queries.lock().await;
            let Some(query) = queries.get_mut(&id) else {
                continue;
            };
            let trigger = self.build_model_trigger_for(&targets, &query_json, &query.last_result);
            let Some(params) = query.model_query_params.as_mut() else {
                continue;
            };
            params.abstract_read = Self::abstract_read(&targets, generation);
            params.trigger = trigger;
            query.query = trigger_sparql;
            query.predicates = predicates;
            changed.insert(id);
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

/// The subject write [`PerspectiveInstance::refuse_abstract_write`] checks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AbstractWrite {
    Create,
    Update,
    Delete,
}

impl AbstractWrite {
    fn done(self) -> &'static str {
        match self {
            AbstractWrite::Create => "created",
            AbstractWrite::Update => "updated",
            AbstractWrite::Delete => "deleted",
        }
    }
}

impl PerspectiveInstance {
    /// The predicates the classes `abstract_class` reads store `property`
    /// under, deduplicated: what a raw SPARQL scan over "every class's
    /// `text`" matches on (the waker's mention scan, #1446).
    pub fn predicates_through(
        &self,
        abstract_class: &str,
        property: &str,
    ) -> Result<Vec<String>, AnyError> {
        let resolution = self.resolve_abstract(abstract_class)?;
        let mut predicates: Vec<String> = resolution
            .views
            .values()
            .filter_map(|view| view.properties.iter().find(|p| p.name == property))
            .map(|p| p.predicate.clone())
            .filter(|p| !p.is_empty())
            .collect();
        predicates.sort();
        predicates.dedup();
        Ok(predicates)
    }

    /// The name of `class_name`'s property that answers to `property` when
    /// read through `abstract_class`: its own `property`, or the one aliased
    /// to it. `None` when the class does not conform.
    pub fn property_through(
        &self,
        abstract_class: &str,
        class_name: &str,
        property: &str,
    ) -> Result<Option<String>, AnyError> {
        let resolution = self.resolve_abstract(abstract_class)?;
        let Some(view) = resolution.views.get(class_name) else {
            return Ok(None);
        };
        let own = self.get_shape(class_name)?;
        // A view is its class's shape with properties renamed in place.
        Ok(view
            .properties
            .iter()
            .zip(&own.properties)
            .find(|(v, _)| v.name == property)
            .map(|(_, o)| o.name.clone()))
    }
}

impl ReadTargets {
    fn named(classes: &QueryClasses) -> Self {
        ReadTargets {
            classes: classes.clone(),
            views: HashMap::new(),
            resolution: None,
            unresolved: false,
        }
    }
}

#[cfg(test)]
pub(super) mod faults {
    //! Test hook: fail the next `abstract_index` read of a perspective, as a
    //! store error would.
    use std::collections::HashSet;
    use std::sync::{LazyLock, Mutex};

    static FAIL_NEXT: LazyLock<Mutex<HashSet<String>>> = LazyLock::new(Default::default);

    pub(in crate::perspectives) fn fail_next_index(perspective_uuid: &str) {
        FAIL_NEXT
            .lock()
            .unwrap()
            .insert(perspective_uuid.to_string());
    }

    pub(super) fn take(perspective_uuid: &str) -> bool {
        FAIL_NEXT.lock().unwrap().remove(perspective_uuid)
    }
}
