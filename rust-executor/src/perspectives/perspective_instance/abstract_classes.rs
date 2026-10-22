//! Abstract model classes on `PerspectiveInstance` (#1435): the cached
//! resolution, the query and subscription wiring, and writes through one.
//! The rule lives in `model_query::abstract_classes`; this module caches and
//! wires it, as a child of `perspective_instance` to reach its private state.
//!
//! A write through an abstract class dispatches to the record's own classes
//! among those the abstract class reads (`subject_classes_of` ∩ the
//! resolution). An update runs each class's setter for the property that
//! answers to the abstract name; a delete runs each class's destructor.
//! Commands two classes share run once. Values are not converted: what one
//! class stores as HTML another gets as is. Creating one is refused.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::{Action, Command, Parameter, PerspectiveInstance};
use crate::agent::AgentContext;
use crate::perspectives::model_query::abstract_classes::{
    self, AbstractIndex, Resolution, ViewResolver,
};
use crate::perspectives::model_query::builtin_classes::{builtin, CONTAINER_URI};
use crate::perspectives::model_query::containment::{self, ContainmentEdges};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::{ModelQueryInput, QueryClasses};
use crate::perspectives::perspective_instance::SubjectClassOption;
use serde_json::Value;

/// What the installed shapes say about abstract classes, and how many times
/// they have changed. A subscription over an abstract class keeps the
/// generation it resolved at and resolves again when it moves.
#[derive(Default)]
pub(super) struct AbstractCache {
    pub(super) generation: u64,
    index: Option<Arc<AbstractIndex>>,
    resolved: HashMap<String, Arc<Resolution>>,
    /// The containment edge set (see `super::containment`).
    pub(super) containment: Option<crate::perspectives::model_query::containment::ContainmentEdges>,
}

/// The classes a model query or subscription reads: those it names, or an
/// abstract class's, with their views.
pub(super) struct ReadTargets {
    pub(super) classes: QueryClasses,
    pub(super) views: HashMap<String, Arc<ModelShape>>,
    /// Set when the caller named an abstract class.
    pub(super) resolution: Option<Arc<Resolution>>,
    /// Not resolved (a store error): a subscription tries again next pass.
    pub(super) unresolved: bool,
    /// The containment edge set, when the query reads containment (a
    /// containment scope, or a query over `Container`).
    pub(super) containment: Option<ContainmentEdges>,
}

/// The generation an unresolved subscription records: never a real one.
pub(super) const UNRESOLVED: u64 = u64::MAX;

impl PerspectiveInstance {
    /// Drop every cached shape, and what was derived from them.
    pub(super) fn clear_shape_cache(&self) {
        self.shape_cache.write().unwrap().clear();
        self.forget_derived_shapes();
    }

    /// The installed shapes changed (added, refreshed or synced in): which
    /// classes are abstract, their aliases and resolutions may have too.
    pub(super) fn forget_derived_shapes(&self) {
        let mut cache = self.abstract_cache.write().unwrap();
        cache.generation += 1;
        cache.index = None;
        cache.resolved.clear();
        cache.containment = None;
    }

    /// How many times the installed shapes have changed. Read it *before*
    /// resolving, so a change landing mid-resolution is seen.
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

    /// The name aliases key `abstract_class` by, and its shape. A built-in
    /// by its URI, from `builtin_classes`, never from the store: an app class
    /// of the same name does not change what it reads or writes.
    fn abstract_shape<'a>(
        &self,
        abstract_class: &'a str,
    ) -> Result<(&'a str, Arc<ModelShape>), AnyError> {
        Ok(match builtin(abstract_class) {
            Some(b) => (b.name, b.shape.clone()),
            None => (abstract_class, self.get_shape(abstract_class)?),
        })
    }

    /// Every installed concrete class `abstract_class` reads, with its view.
    /// Cached until the installed shapes change. `abstract_class` is a stored
    /// abstract class's name or a built-in's URI (`ad4m://TextContent`).
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
        let (alias_key, shape) = self.abstract_shape(abstract_class)?;
        let mut resolution = abstract_classes::resolve(
            &self.sparql_store,
            &self.shape_resolver(),
            &index,
            alias_key,
            &shape,
        )?;
        // A built-in that reads its own rows (`Container`'s class-less
        // holders) is a member of its own union, under its URI.
        if let Some(member) = builtin(abstract_class).and_then(|b| b.member.clone()) {
            resolution.classes.push(abstract_class.to_string());
            resolution.views.insert(abstract_class.to_string(), member);
        }
        let resolution = Arc::new(resolution);
        let mut cache = self.abstract_cache.write().unwrap();
        if cache.generation == generation {
            cache
                .resolved
                .insert(abstract_class.to_string(), resolution.clone());
        }
        Ok(resolution)
    }

    /// The concrete classes `classes` reads. An abstract class is read on its
    /// own (in a union its views would apply to some rows only), and what a
    /// union refuses is refused before it resolves, whatever is installed.
    pub(super) fn read_targets(
        &self,
        classes: &QueryClasses,
        query: &ModelQueryInput,
    ) -> Result<ReadTargets, AnyError> {
        let mut targets = self.read_classes(classes, query)?;
        let over_container = matches!(classes, QueryClasses::One(name) if name == CONTAINER_URI);
        if over_container || containment::reads_containment(query) {
            targets.containment = Some(self.containment_edges()?);
        }
        Ok(targets)
    }

    /// The classes of [`Self::read_targets`].
    fn read_classes(
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
            containment: None,
        })
    }

    /// [`Self::read_targets`] for a subscription's trigger. On an error the
    /// classes are watched as named and marked `unresolved`, so the next pass
    /// resolves them again: a transient store error must not leave it
    /// watching predicates no class writes.
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
    /// `ModelSubscriptionParams::abstract_read`): the generation, when what
    /// it reads depends on the installed shapes. An abstract class's classes
    /// do, and so does a containment read: a class installed later can add
    /// a containment edge.
    pub(super) fn abstract_read(targets: &ReadTargets, generation: u64) -> Option<u64> {
        if targets.unresolved {
            return Some(UNRESOLVED);
        }
        (targets.resolution.is_some() || targets.containment.is_some()).then_some(generation)
    }

    /// Whether `class_name` is an abstract class here: a built-in's URI
    /// (`ad4m://TextContent`), or a stored class marked abstract.
    pub(crate) fn is_abstract_class(&self, class_name: &str) -> Result<bool, AnyError> {
        Ok(builtin(class_name).is_some() || self.abstract_index()?.is_abstract(class_name))
    }

    /// Creating an abstract class is refused: it names what records have,
    /// not what they are.
    pub(crate) fn refuse_abstract_create(&self, class_name: &str) -> Result<(), AnyError> {
        if self.is_abstract_class(class_name)? {
            return Err(anyhow!(
                "'{class_name}' is an abstract class and cannot be created. Create an instance \
                 of a class that has its properties; querying '{class_name}' reads it."
            ));
        }
        Ok(())
    }

    /// The classes of `base` a write through `abstract_class` reaches: those
    /// it is, among those the abstract class reads. Never empty.
    fn write_targets(&self, abstract_class: &str, base: &str) -> Result<Vec<String>, AnyError> {
        let resolution = self.resolve_abstract(abstract_class)?;
        let own = self
            .subject_classes_of(&[base.to_string()])?
            .remove(base)
            .unwrap_or_default();
        let targets: Vec<String> = resolution
            .classes
            .iter()
            .filter(|c| own.contains(c))
            .cloned()
            .collect();
        if targets.is_empty() {
            return Err(anyhow!(
                "'{base}' is not a record of any class '{abstract_class}' reads ({}): there is \
                 nothing to write through it.",
                resolution.classes.join(", ")
            ));
        }
        Ok(targets)
    }

    /// `update_subject` through `abstract_class`: each value is written by
    /// every target class's setter for the property answering to its name.
    /// A command another class already wrote for this name (same action,
    /// source and predicate) is dropped, so a shared predicate is written,
    /// or a shared collection appended to, once.
    ///
    /// A relation or collection name also takes `{"action": "adder" |
    /// "remover" | "setter", "value": [...]}` (the SDK's `addX` / `removeX`
    /// / `setX`): each class's adder or remover runs once per member, a
    /// setter as for a plain value. A class with no setter for it sets the
    /// members with a `collectionSetter` on its adder's predicate, as the
    /// SDK does for a concrete class. A `collectionSetter` gets the members
    /// as its parameters, in a run of its own.
    pub(super) async fn update_through_abstract(
        &self,
        abstract_class: &str,
        base: &str,
        values: &Value,
        context: &AgentContext,
    ) -> Result<Vec<Run>, AnyError> {
        let mut has_child = Vec::new();
        if abstract_class == CONTAINER_URI {
            if let Some(write) = self.has_child_write(base, values).await? {
                if !write.beside_own {
                    return Ok(vec![Run {
                        commands: write.commands,
                        parameters: vec![],
                    }]);
                }
                has_child = write.commands;
            }
        }
        let Value::Object(values) = values else {
            return Ok(vec![]);
        };
        let (alias_key, shape) = self.abstract_shape(abstract_class)?;
        let names: HashSet<&str> = shape
            .properties
            .iter()
            .map(|p| p.name.as_str())
            .chain(shape.include_relations.iter().map(|r| r.name.as_str()))
            .collect();
        if let Some(key) = values.keys().find(|k| !names.contains(k.as_str())) {
            let mut names: Vec<_> = names.into_iter().collect();
            names.sort();
            return Err(anyhow!(
                "'{key}' is not a name of the abstract class '{abstract_class}' ({}). Write it \
                 through the record's own class.",
                names.join(", ")
            ));
        }
        let memberships: HashSet<&str> = shape
            .properties
            .iter()
            .filter(|p| p.is_collection)
            .map(|p| p.name.as_str())
            .chain(shape.include_relations.iter().map(|r| r.name.as_str()))
            .collect();
        let index = self.abstract_index()?;
        let targets = self.write_targets(abstract_class, base)?;
        let mut commands = Vec::new();
        let mut runs = Vec::new();
        for (name, value) in values {
            let membership = memberships.contains(name.as_str());
            let (op, value) = match membership {
                true => Op::of(value),
                false => (Op::Set, value),
            };
            let mut written: Vec<(Action, Option<String>, Option<String>)> = Vec::new();
            let mut has_action = false;
            for class in &targets {
                let own_shape = self.get_shape(class)?;
                let Some(own) = index.own_name(class, &own_shape, name, alias_key) else {
                    continue;
                };
                let Some(actions) = self.actions_for(class, own, op, membership).await? else {
                    continue;
                };
                has_action = true;
                let actions: Vec<Command> = actions
                    .into_iter()
                    .filter(|c| {
                        let key = (c.action.clone(), c.source.clone(), c.predicate.clone());
                        !written.contains(&key) && {
                            written.push(key);
                            true
                        }
                    })
                    .collect();
                if op != Op::Set {
                    for member in self.members(class, own, value, context).await? {
                        commands.extend(actions.iter().map(|c| Command {
                            target: Some(member.clone()),
                            ..c.clone()
                        }));
                    }
                    continue;
                }
                let (collection, setter): (Vec<_>, Vec<_>) = actions
                    .into_iter()
                    .partition(|c| c.action == Action::CollectionSetter);
                if !setter.is_empty() {
                    commands.extend(
                        self.setter_commands(class, own, &setter, value, context)
                            .await?,
                    );
                }
                if !collection.is_empty() {
                    runs.extend(
                        self.collection_setter_runs(class, own, collection, value, context)
                            .await?,
                    );
                }
            }
            if !has_action {
                return Err(anyhow!(
                    "'{name}' cannot be written through '{abstract_class}': none of the \
                     record's classes ({}) has {} for it.",
                    targets.join(", "),
                    op.what()
                ));
            }
        }
        for command in has_child {
            if !commands.contains(&command) {
                commands.push(command);
            }
        }
        if !commands.is_empty() {
            runs.insert(
                0,
                Run {
                    commands,
                    parameters: vec![],
                },
            );
        }
        Ok(runs)
    }

    /// The actions of `class` that do `op` to its property `own`. Setting a
    /// relation or collection (`membership`) with no setter falls back to
    /// a `collectionSetter` on each predicate its adder appends to.
    async fn actions_for(
        &self,
        class: &str,
        own: &str,
        op: Op,
        membership: bool,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        let predicate = match op {
            Op::Set => "ad4m://setter",
            Op::Add => "ad4m://adder",
            Op::Remove => "ad4m://remover",
        };
        let actions = self
            .get_property_actions_from_shacl(class, own, predicate)
            .await?;
        if actions.is_some() || op != Op::Set || !membership {
            return Ok(actions);
        }
        let adder = self
            .get_property_actions_from_shacl(class, own, "ad4m://adder")
            .await?
            .filter(|a| !a.is_empty() && a.iter().all(|c| c.action == Action::AddLink));
        Ok(adder.map(|adder| {
            adder
                .into_iter()
                .map(|c| Command {
                    action: Action::CollectionSetter,
                    ..c
                })
                .collect()
        }))
    }

    /// The members `value` names for `own` of `class`, each encoded as that
    /// property stores it: an array's elements, or a single value. `null`
    /// is refused: it names no members, and read as `[]` it would clear.
    pub(super) async fn members(
        &self,
        class: &str,
        own: &str,
        value: &Value,
        context: &AgentContext,
    ) -> Result<Vec<String>, AnyError> {
        let items = match value {
            Value::Array(items) => items.clone(),
            Value::Null => {
                return Err(anyhow!(
                    "'{own}' of '{class}': null names no members. Pass [] to clear it."
                ))
            }
            single => vec![single.clone()],
        };
        let mut members = Vec::with_capacity(items.len());
        for item in &items {
            members.push(
                self.resolve_property_value(class, own, item, context)
                    .await?,
            );
        }
        Ok(members)
    }

    /// One run per `collectionSetter` in `collection`, with the members
    /// `value` names for `own` of `class` as its parameters. They have no
    /// name: nothing in the commands is replaced by a member;
    /// `collectionSetter` reads only their values.
    pub(super) async fn collection_setter_runs(
        &self,
        class: &str,
        own: &str,
        collection: Vec<Command>,
        value: &Value,
        context: &AgentContext,
    ) -> Result<Vec<Run>, AnyError> {
        let parameters: Vec<Parameter> = self
            .members(class, own, value, context)
            .await?
            .into_iter()
            .map(|m| Parameter {
                name: String::new(),
                value: Value::String(m),
            })
            .collect();
        Ok(collection
            .into_iter()
            .map(|c| Run {
                commands: vec![c],
                parameters: parameters.clone(),
            })
            .collect())
    }

    /// Run what [`Self::update_through_abstract`] (or a write on a concrete
    /// class with a `collectionSetter`) returned, in one batch: the
    /// caller's, or one made here when there are several runs.
    pub(super) async fn execute_runs(
        &mut self,
        runs: Vec<Run>,
        base: String,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        let own_batch = match (&batch_id, runs.len()) {
            (None, n) if n > 1 => Some(self.create_batch().await),
            _ => None,
        };
        let batch = own_batch.clone().or(batch_id);
        for run in runs {
            let ran = self
                .execute_commands(
                    run.commands,
                    base.clone(),
                    run.parameters,
                    batch.clone(),
                    context,
                )
                .await;
            if let Err(e) = ran {
                if let Some(own) = &own_batch {
                    self.discard_batch(own).await;
                }
                return Err(e);
            }
        }
        if let Some(own) = own_batch {
            self.commit_batch(own, context).await?;
        }
        Ok(())
    }

    /// Delete the instance of `subject_class` at `base`: run the class's
    /// destructor. Through an abstract class, run the destructor of each
    /// target class, each command once; the record's other classes are left
    /// as they are.
    pub async fn remove_subject(
        &mut self,
        subject_class: SubjectClassOption,
        base: String,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        let class_name = self
            .subject_class_option_to_class_name(subject_class, context)
            .await?;
        let classes = if self.is_abstract_class(&class_name)? {
            self.write_targets(&class_name, &base)?
        } else {
            vec![class_name]
        };
        let mut commands: Vec<Command> = Vec::new();
        for class in &classes {
            let destructor = self
                .get_shape_actions_from_shacl(class, "ad4m://destructor")
                .await?
                .ok_or_else(|| anyhow!("No destructor found for subject class: {class}."))?;
            for command in destructor {
                if !commands.contains(&command) {
                    commands.push(command);
                }
            }
        }
        if commands.is_empty() {
            return Ok(());
        }
        self.execute_commands(commands, base, vec![], batch_id, context)
            .await
    }

    /// Put `child` under `parent` through `Container` (#1446): what MCP's
    /// `add_child` and `instance_create(parent)` do, and what the SDK's
    /// `create({ parent: { id } })` asks `updateSubject` for. See
    /// [`Self::has_child_write`] for which link it writes.
    pub(crate) async fn add_to_container(
        &mut self,
        parent: &str,
        child: &str,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        let value = serde_json::json!({ "children": { "action": "adder", "value": [child] } });
        let runs = self
            .update_through_abstract(CONTAINER_URI, parent, &value, context)
            .await?;
        self.execute_runs(runs, parent.to_string(), batch_id, context)
            .await
    }

    /// The `ad4m://has_child` links an add or remove through `Container`'s
    /// `children` writes. `has_child` holds on any node (`ad4m://self`, a
    /// plain-string id, a Flux channel whose relation is not `children`), so
    /// on a parent none of whose classes has the action for the relation
    /// answering to `children`, these links are the whole write.
    ///
    /// When one of its classes has an adder, `None`: each such class runs its
    /// own (a Block's adder writes `we://child`, open question 5), as for any
    /// abstract class. When one has a remover, the removals are still
    /// returned, `beside_own`: the child may be held through `has_child` as
    /// well as, or instead of, the class's own relation, and a remove takes
    /// it out of every edge (#1461 review).
    async fn has_child_write(
        &self,
        base: &str,
        values: &Value,
    ) -> Result<Option<HasChildWrite>, AnyError> {
        let Some(children) = values.get("children") else {
            return Ok(None);
        };
        let (op, value) = Op::of(children);
        let action = match op {
            Op::Add => Action::AddLink,
            Op::Remove => Action::RemoveLink,
            Op::Set => return Ok(None),
        };
        if values.as_object().is_some_and(|o| o.len() != 1) {
            return Ok(None);
        }
        let resolution = self.resolve_abstract(CONTAINER_URI)?;
        let own = self
            .subject_classes_of(&[base.to_string()])?
            .remove(base)
            .unwrap_or_default();
        let index = self.abstract_index()?;
        let alias_key = builtin(CONTAINER_URI)
            .map(|b| b.name)
            .unwrap_or("Container");
        let mut beside_own = false;
        for class in resolution.classes.iter().filter(|c| own.contains(c)) {
            let shape = self.get_shape(class)?;
            let Some(own_name) = index.own_name(class, &shape, "children", alias_key) else {
                continue;
            };
            if self.actions_for(class, own_name, op, true).await?.is_some() {
                if op == Op::Add {
                    return Ok(None);
                }
                beside_own = true;
                break;
            }
        }
        let members: Vec<String> = match value {
            Value::Array(items) => items
                .iter()
                .map(|i| i.as_str().map(str::to_string))
                .collect::<Option<_>>()
                .ok_or_else(|| anyhow!("'children' of a Container: members are node ids"))?,
            Value::String(one) => vec![one.clone()],
            _ => return Err(anyhow!("'children' of a Container: members are node ids")),
        };
        let commands = members
            .into_iter()
            .map(|member| Command {
                source: Some("this".to_string()),
                predicate: Some(containment::HAS_CHILD.to_string()),
                target: Some(member),
                local: None,
                action: action.clone(),
            })
            .collect();
        Ok(Some(HasChildWrite {
            commands,
            beside_own,
        }))
    }

    /// Resolve again every subscription over an abstract class, or reading
    /// containment, resolved before the installed shapes last changed, and
    /// rebuild its trigger: a change can move its classes, only a view (an
    /// alias moved), or the containment edge set. Returns
    /// their ids, which re-run this pass whatever was written. One still
    /// unresolved waits for the next pass.
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
            targets.containment.as_deref().map(Vec::as_slice),
        )
    }
}

/// One `execute_commands` run of a write that sets a collection: a
/// `collectionSetter` gets its members as parameters, in a run of its own.
pub(super) struct Run {
    pub(super) commands: Vec<Command>,
    /// The members a `collectionSetter` sets; empty otherwise.
    pub(super) parameters: Vec<Parameter>,
}

/// The `has_child` links a `children` write through `Container` makes (see
/// `PerspectiveInstance::has_child_write`).
struct HasChildWrite {
    commands: Vec<Command>,
    /// One of the parent's classes runs its own remover too: these commands
    /// join its run instead of being the whole write.
    beside_own: bool,
}

/// What an update through an abstract class does to one name.
#[derive(Clone, Copy, PartialEq)]
enum Op {
    Set,
    Add,
    Remove,
}

impl Op {
    /// `{"action": "adder" | "remover" | "setter", "value": v}` is that op
    /// with `v`; anything else is a value to set.
    fn of(value: &Value) -> (Op, &Value) {
        let Value::Object(fields) = value else {
            return (Op::Set, value);
        };
        let (Some(Value::String(action)), Some(inner), 2) =
            (fields.get("action"), fields.get("value"), fields.len())
        else {
            return (Op::Set, value);
        };
        match action.as_str() {
            "adder" => (Op::Add, inner),
            "remover" => (Op::Remove, inner),
            "setter" => (Op::Set, inner),
            _ => (Op::Set, value),
        }
    }

    fn what(self) -> &'static str {
        match self {
            Op::Set => "a setter",
            Op::Add => "an adder",
            Op::Remove => "a remover",
        }
    }
}

impl ReadTargets {
    fn named(classes: &QueryClasses) -> Self {
        ReadTargets {
            classes: classes.clone(),
            views: HashMap::new(),
            resolution: None,
            unresolved: false,
            containment: None,
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

    pub(crate) fn fail_next_index(perspective_uuid: &str) {
        FAIL_NEXT
            .lock()
            .unwrap()
            .insert(perspective_uuid.to_string());
    }

    pub(super) fn take(perspective_uuid: &str) -> bool {
        FAIL_NEXT.lock().unwrap().remove(perspective_uuid)
    }
}
