//! Which writes re-run a model subscription (#1237).
//!
//! A model subscription used to re-run whenever a batch wrote any predicate
//! its class declares. That was wrong in both directions:
//!
//! - **Too narrow.** Only the subscribed class's own predicates counted, so an
//!   edit to a record the query reaches through `include` (a comment's text
//!   under `include: { comments: true }`) re-ran nothing and the update was
//!   lost.
//! - **Too wide.** Classes share predicates. Every class with a flag on one
//!   shared flag predicate re-ran on every create, whatever class was created.
//!
//! [`TriggerRules`] widens what is watched (the classes reached through
//! `include`, projections, `where` quantifiers and the parent scope) and
//! matches each written link on more than its predicate. A write re-runs the
//! subscription when one of these holds, checked in this order:
//!
//! 1. **`any`**: the predicate alone decides, as before. Used where the
//!    query's reads cannot be told apart by node: `links`, `producedByFlow`,
//!    property getters, transitive reads, and classes with no flag.
//! 2. **`joins`**: a flag predicate written (or removed) with one of the
//!    flag values of a class the subscription scans. This is how a new
//!    record enters, and how a deleted one leaves the count.
//! 3. **`near`**: a predicate read on records in the result, written on a
//!    node that is in the last result or is a parent-scope anchor. For a node
//!    in the last result this is a fast path: `scan` (a top-level record
//!    carries its flag) or `via` (an included record is linked to its parent
//!    in the result) matches the same write, after a store read.
//! 4. **`scan`**: a predicate of a scanned class (the subscribed class and
//!    any class a `where` quantifier reads), written on a node that is an
//!    instance of that class: it has one of the class's flag links. This is
//!    how a record that is not in the result yet moves into a filter or a page.
//! 5. **`via`**: a `near` predicate written on a node linked, through a
//!    relation the query includes or projects, to a node in the last result.
//!    This is how an included record that was filtered out, or did not conform
//!    yet, enters the include. It is also what lets a polymorphic include be
//!    watched without knowing its members' classes. A typed relation counts
//!    as included even without `include`: its conformance getter decides its
//!    ids on every query, from the target's flag and required properties.
//!
//! Wherever the query reads something these rules cannot describe, the rules
//! fall back to `every_write` or to `any` for the predicates involved. Missing
//! a write loses an update; an extra re-run costs one query. So every
//! uncertain case falls back to re-running.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use serde_json::Value;

use crate::perspectives::model_query::types::ShapeProperty;
use crate::perspectives::model_query::{
    conformance_predicates, IncludeValue, ModelQueryInput, ModelShape, Scope, ShapeResolver,
    WhereCondition,
};
use crate::perspectives::sparql_store::{canonical_target, SparqlStore};
use crate::types::DecoratedLinkExpression;

/// Past this many distinct links in one batch, a batch keeps only its
/// predicates and every model subscription falls back to predicate matching.
/// A bulk import must not hold every triple it wrote until the next check.
const MAX_TRACKED_LINKS: usize = 10_000;

/// How many links touching one node the `via` rule reads before it stops
/// and re-runs. A node with more neighbours than this is a hub (a shared
/// literal, a large container), and scanning it on every write would cost more
/// than the query it might save.
const MAX_NEIGHBOURS: usize = 2_000;

/// The writes of one subscription-check batch.
#[derive(Debug, Clone, Default)]
pub(super) struct Writes {
    /// Every predicate written or removed. Raw SPARQL subscriptions match on
    /// these alone.
    pub(super) predicates: HashSet<String>,
    /// `(source, predicate, target)` of each link written or removed, or
    /// `None` once the batch passed [`MAX_TRACKED_LINKS`].
    links: Option<HashSet<(String, String, String)>>,
}

impl Writes {
    pub(super) fn new() -> Self {
        Writes {
            predicates: HashSet::new(),
            links: Some(HashSet::new()),
        }
    }

    /// Record one written or removed link that has a predicate.
    pub(super) fn record(&mut self, source: &str, predicate: &str, target: &str) {
        self.predicates.insert(predicate.to_string());
        if let Some(links) = &mut self.links {
            if links.len() >= MAX_TRACKED_LINKS {
                self.links = None;
            } else {
                links.insert((source.into(), predicate.into(), target.into()));
            }
        }
    }

    /// Fold another batch's writes into this one.
    pub(super) fn extend(&mut self, other: Writes) {
        self.predicates.extend(other.predicates);
        match (&mut self.links, other.links) {
            (Some(mine), Some(theirs)) if mine.len() + theirs.len() <= MAX_TRACKED_LINKS => {
                mine.extend(theirs)
            }
            _ => self.links = None,
        }
    }
}

impl From<HashSet<String>> for Writes {
    /// Predicates without their links: model subscriptions fall back to
    /// predicate matching. For tests that only care about predicates.
    fn from(predicates: HashSet<String>) -> Self {
        Writes {
            predicates,
            links: None,
        }
    }
}

/// Collect a diff's links into [`Writes`]. `None` when a link has no
/// predicate, which leaves nothing to match on: check every subscription.
pub(super) fn writes_of<'a>(
    links: impl IntoIterator<Item = &'a DecoratedLinkExpression>,
) -> Option<Writes> {
    let mut writes = Writes::new();
    for link in links {
        let predicate = link.data.predicate.as_deref()?;
        writes.record(&link.data.source, predicate, &link.data.target);
    }
    Some(writes)
}

/// A set of predicates, or all of them.
#[derive(Debug, Clone, Default, PartialEq)]
enum Predicates {
    #[default]
    None,
    Some(HashSet<String>),
    All,
}

impl Predicates {
    fn add(&mut self, predicate: &str) {
        if predicate.is_empty() {
            return;
        }
        match self {
            Predicates::All => {}
            Predicates::Some(set) => {
                set.insert(predicate.to_string());
            }
            Predicates::None => *self = Predicates::Some([predicate.to_string()].into()),
        }
    }

    fn contains(&self, predicate: &str) -> bool {
        match self {
            Predicates::None => false,
            Predicates::Some(set) => set.contains(predicate),
            Predicates::All => true,
        }
    }

    fn is_none(&self) -> bool {
        matches!(self, Predicates::None)
    }
}

/// One flag link that marks a node as an instance of a class.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Flag {
    predicate: String,
    value: String,
}

/// What a model subscription watches, built once from its class and query
/// at subscribe time. See the module docs for the rules.
#[derive(Debug, Default)]
pub(super) struct TriggerRules {
    every_write: bool,
    any: HashSet<String>,
    /// Flag predicate → the values (and their canonical spellings) that
    /// re-run on their own.
    joins: HashMap<String, HashSet<String>>,
    near: Predicates,
    /// Predicate → the flags of the scanned classes that declare it. A
    /// write re-runs when either end carries one of these flags.
    scan: HashMap<String, Vec<Flag>>,
    via: Predicates,
    /// Parent-scope anchors. They count as result nodes for `near`.
    anchors: HashSet<String>,
    /// A parent scope whose predicate is not known (`Scope::Model` without a
    /// `field`): any write touching an anchor re-runs.
    anchors_any_predicate: bool,
}

impl TriggerRules {
    fn every_write() -> Self {
        TriggerRules {
            every_write: true,
            ..Default::default()
        }
    }

    /// Every predicate these rules can re-run on, or `None` for any. The
    /// predicate-only fallback for a batch too large to keep its links.
    fn predicates(&self) -> Option<HashSet<&str>> {
        if self.every_write
            || self.anchors_any_predicate
            || matches!(self.near, Predicates::All)
            || matches!(self.via, Predicates::All)
        {
            return None;
        }
        let mut out: HashSet<&str> = self.any.iter().map(String::as_str).collect();
        out.extend(self.joins.keys().map(String::as_str));
        out.extend(self.scan.keys().map(String::as_str));
        for set in [&self.near, &self.via] {
            if let Predicates::Some(set) = set {
                out.extend(set.iter().map(String::as_str));
            }
        }
        Some(out)
    }

    /// Fold `other` in: the result re-runs on every write either would.
    /// Matching is a disjunction over the rules, so the union never misses a
    /// write one of the two matches; it can only match a few more.
    fn merge(&mut self, other: TriggerRules) {
        self.every_write |= other.every_write;
        self.any.extend(other.any);
        for (predicate, values) in other.joins {
            self.joins.entry(predicate).or_default().extend(values);
        }
        for set in [(&mut self.near, other.near), (&mut self.via, other.via)] {
            match set {
                (mine, Predicates::All) => *mine = Predicates::All,
                (_, Predicates::None) => {}
                (mine, Predicates::Some(theirs)) => theirs.iter().for_each(|p| mine.add(p)),
            }
        }
        for (predicate, flags) in other.scan {
            let entry = self.scan.entry(predicate).or_default();
            for flag in flags {
                if !entry.contains(&flag) {
                    entry.push(flag);
                }
            }
        }
        self.anchors.extend(other.anchors);
        self.anchors_any_predicate |= other.anchors_any_predicate;
    }
}

impl super::PerspectiveInstance {
    /// The trigger for a model subscription over `class_names` with
    /// `query_json`, watching the nodes of its first `result`. Each class
    /// contributes its own rules; a query over several classes (#1238)
    /// re-runs on a write any of them matches.
    pub(super) fn build_model_trigger<'a>(
        &self,
        class_names: impl IntoIterator<Item = &'a str>,
        query_json: &str,
        result: &str,
    ) -> ModelTrigger {
        let mut rules: Option<TriggerRules> = None;
        for class_name in class_names {
            let shape = self.get_shape(class_name).ok();
            let side = self.model_query_side_predicates(shape.as_deref(), query_json);
            let class_rules = build_rules(
                class_name,
                query_json,
                &self.shape_resolver(),
                side,
                &super::extract_predicates_from_sparql,
            );
            match &mut rules {
                Some(rules) => rules.merge(class_rules),
                None => rules = Some(class_rules),
            }
        }
        // No class to watch: nothing to tell writes apart by.
        ModelTrigger::new(rules.unwrap_or_else(TriggerRules::every_write), result)
    }
}

/// Build the rules for a subscription on `class_name` with `query_json`.
///
/// `extra_any` are predicates the query reads beside its classes' shapes
/// (`links`, `producedByFlow`; see `model_query_side_predicates`), matched on
/// the predicate alone. `getter_predicates` names the predicates a property
/// getter's SPARQL reads.
pub(super) fn build_rules(
    class_name: &str,
    query_json: &str,
    resolver: &dyn ShapeResolver,
    extra_any: impl IntoIterator<Item = String>,
    getter_predicates: &dyn Fn(&str) -> HashSet<String>,
) -> TriggerRules {
    let Ok(shape) = resolver.get_shape(class_name) else {
        return TriggerRules::every_write();
    };
    let Ok(query) = serde_json::from_str::<ModelQueryInput>(query_json) else {
        return TriggerRules::every_write();
    };
    let mut walk = Walk {
        rules: TriggerRules::default(),
        resolver,
        getter_predicates,
    };
    walk.rules.any.extend(extra_any);
    walk.scan_class(&shape, query.where_clause.as_ref());
    walk.query(&shape, &query);
    if let Some(parent) = &query.parent {
        walk.parent(parent);
    }
    walk.rules
}

struct Walk<'a> {
    rules: TriggerRules,
    resolver: &'a dyn ShapeResolver,
    getter_predicates: &'a dyn Fn(&str) -> HashSet<String>,
}

impl Walk<'_> {
    /// Watch `shape`'s predicates on result nodes (`near`), and what its
    /// getters read.
    fn watch_class(&mut self, shape: &ModelShape) {
        for p in &shape.properties {
            self.rules.near.add(&p.predicate);
            match (p.getter.as_deref(), p.direction.is_some()) {
                (Some(getter), true) => self.relation_getter(p, getter),
                (Some(getter), false) => self.any_getter(p, getter),
                (None, _) => {}
            }
        }
        for r in &shape.include_relations {
            self.rules.near.add(&r.predicate);
        }
    }

    /// A relation with a getter is filled by it on every query, `include` or
    /// not: a typed relation's conformance getter lists only the targets that
    /// carry the target class's flag and required properties, and its
    /// `where_filter` drops the ones whose values fail it. So a write on a
    /// linked target changes the relation. Watch it as an `include: true`
    /// one level down: the predicates read on the target in `near`, the
    /// relation in `via`. Without recursing, since only the ids are read.
    fn relation_getter(&mut self, p: &ShapeProperty, getter: &str) {
        let Some(on_target) = conformance_predicates(getter, &p.predicate) else {
            // The model author's own SPARQL: its reads cannot be placed on
            // the target, so match them by predicate.
            self.any_getter(p, getter);
            self.rules
                .any
                .extend(p.where_predicates.iter().flat_map(|w| w.values().cloned()));
            return;
        };
        self.rules.via.add(&p.predicate);
        let filtered = p.where_predicates.iter().flat_map(|w| w.values());
        for predicate in on_target.iter().chain(filtered) {
            self.rules.near.add(predicate);
        }
    }

    /// A getter that reads triples the shape does not name, from nodes other
    /// than the record: re-run on the property's predicate and on every
    /// predicate the getter names. A getter with a variable predicate names
    /// none, and may read any.
    fn any_getter(&mut self, p: &ShapeProperty, getter: &str) {
        if !p.predicate.is_empty() {
            self.rules.any.insert(p.predicate.clone());
        }
        let read = (self.getter_predicates)(getter);
        if read.is_empty() {
            self.rules.every_write = true;
        }
        self.rules.any.extend(read);
    }

    /// A class whose records can enter the result through a write on the
    /// record itself: the subscribed class, and every class a `where`
    /// quantifier reads.
    fn scan_class(
        &mut self,
        shape: &ModelShape,
        where_clause: Option<&std::collections::BTreeMap<String, WhereCondition>>,
    ) {
        self.watch_class(shape);
        let flags: Vec<Flag> = shape
            .properties
            .iter()
            .filter(|p| p.is_flag && !p.predicate.is_empty())
            .filter_map(|p| {
                Some(Flag {
                    predicate: p.predicate.clone(),
                    value: p.initial_value.clone()?,
                })
            })
            .collect();
        if flags.is_empty() {
            // No flag, so instance-ness is not a one-triple lookup: any write
            // to the class's predicates re-runs, as before #1237.
            self.rules.any.extend(shape.predicates());
        } else {
            for flag in &flags {
                let values = self.rules.joins.entry(flag.predicate.clone()).or_default();
                values.insert(flag.value.clone());
                values.insert(canonical_target(&flag.value));
            }
            for predicate in shape.predicates() {
                let entry = self.rules.scan.entry(predicate).or_default();
                for flag in &flags {
                    if !entry.contains(flag) {
                        entry.push(flag.clone());
                    }
                }
            }
        }
        if let Some(clause) = where_clause {
            self.quantifiers(shape, clause);
        }
    }

    /// Scan the target class of every relation quantifier (`some` / `none`)
    /// in `clause`, which is a `where` on `shape`.
    fn quantifiers(
        &mut self,
        shape: &ModelShape,
        clause: &std::collections::BTreeMap<String, WhereCondition>,
    ) {
        for (key, condition) in clause {
            match condition {
                WhereCondition::SubClauses(branches) => {
                    for branch in branches {
                        self.quantifiers(shape, branch);
                    }
                }
                WhereCondition::SubClause(inner) => self.quantifiers(shape, inner),
                WhereCondition::Ops(ops) if ops.some.is_some() || ops.none.is_some() => {
                    let target = shape
                        .include_relations
                        .iter()
                        .find(|r| r.name == *key && !r.target_class_name.is_empty())
                        .and_then(|r| self.resolver.get_shape(&r.target_class_name).ok());
                    let Some(target) = target else {
                        // The quantified records' class is unknown, so there is
                        // no flag to recognise one by.
                        self.rules.every_write = true;
                        return;
                    };
                    for inner in [&ops.some, &ops.none].into_iter().flatten() {
                        self.scan_class(&target, Some(inner));
                    }
                }
                _ => {}
            }
        }
    }

    /// Walk `query`'s includes and projections, which read from `shape`.
    /// Recurses over the query, never over the shapes, so it ends.
    fn query(&mut self, shape: &ModelShape, query: &ModelQueryInput) {
        for (name, value) in query.include.iter().flatten() {
            let sub = match value {
                IncludeValue::Bool(false) => continue,
                IncludeValue::Bool(true) => ModelQueryInput::default(),
                IncludeValue::SubQuery(sub) => (**sub).clone(),
            };
            // The query skips a relation the shape does not declare.
            let Some(rel) = shape.include_relations.iter().find(|r| r.name == *name) else {
                continue;
            };
            self.rules.via.add(&rel.predicate);
            let target = (!sub.polymorphic.unwrap_or(false))
                .then_some(())
                .filter(|_| !rel.target_class_name.is_empty())
                .and_then(|_| self.resolver.get_shape(&rel.target_class_name).ok());
            match target {
                Some(target) => {
                    self.watch_class(&target);
                    if let Some(clause) = &sub.where_clause {
                        self.quantifiers(&target, clause);
                    }
                    self.query(&target, &sub);
                }
                None => self.unknown_members(&sub),
            }
        }
        for proj in query.projections.iter().flat_map(|p| p.values()) {
            let declared = shape
                .properties
                .iter()
                .map(|p| (&p.name, &p.predicate))
                .chain(
                    shape
                        .include_relations
                        .iter()
                        .map(|r| (&r.name, &r.predicate)),
                );
            let Some(predicate) = declared
                .filter(|(name, predicate)| **name == proj.from && !predicate.is_empty())
                .map(|(_, predicate)| predicate.clone())
                .next()
            else {
                continue;
            };
            let target_name = proj.target_class_name.clone().or_else(|| {
                shape
                    .include_relations
                    .iter()
                    .find(|r| r.name == proj.from && !r.target_class_name.is_empty())
                    .map(|r| r.target_class_name.clone())
            });
            let target = target_name.and_then(|n| self.resolver.get_shape(&n).ok());
            if proj.transitive {
                // Reachable at any depth: a link far below the result changes
                // the projection, and its nodes are in no result.
                self.rules.any.insert(predicate);
                match &target {
                    Some(target) if proj.where_clause.is_some() => {
                        self.rules.any.extend(target.predicates())
                    }
                    Some(_) => {}
                    None if proj.where_clause.is_some() => self.rules.every_write = true,
                    None => {}
                }
                continue;
            }
            self.rules.via.add(&predicate);
            self.rules.near.add(&predicate);
            match &target {
                Some(target) => {
                    self.watch_class(target);
                    if let Some(clause) = &proj.where_clause {
                        self.quantifiers(target, clause);
                    }
                }
                None => {
                    self.rules.near = Predicates::All;
                    if proj.where_clause.as_ref().is_some_and(has_quantifier) {
                        self.rules.every_write = true;
                    }
                }
            }
        }
    }

    /// An include whose members' class is not known (polymorphic, or a
    /// relation that declares no class): any predicate on a member, and any
    /// relation from it, may be read.
    fn unknown_members(&mut self, sub: &ModelQueryInput) {
        self.rules.near = Predicates::All;
        if sub.include.as_ref().is_some_and(|i| !i.is_empty())
            || sub.projections.as_ref().is_some_and(|p| !p.is_empty())
        {
            self.rules.via = Predicates::All;
        }
        if sub.where_clause.as_ref().is_some_and(has_quantifier) {
            self.rules.every_write = true;
        }
    }

    fn parent(&mut self, parent: &Scope) {
        match parent {
            Scope::Raw { id, predicate } => {
                self.rules.anchors.insert(id.clone());
                self.rules.near.add(predicate);
            }
            Scope::Model { id, field, .. } => {
                self.rules.anchors.insert(id.clone());
                match field {
                    Some(field) => self.rules.near.add(field),
                    None => self.rules.anchors_any_predicate = true,
                }
            }
            Scope::Traverse {
                ids,
                predicate,
                transitive,
                levels,
                ..
            } => {
                self.rules.anchors.extend(ids.iter().cloned());
                if *transitive || levels.is_some() {
                    // Below the first level, a link's ends are neither anchors
                    // nor (when a level limit cut them) result nodes.
                    self.rules.any.insert(predicate.clone());
                } else {
                    self.rules.near.add(predicate);
                }
            }
        }
    }
}

fn has_quantifier(clause: &std::collections::BTreeMap<String, WhereCondition>) -> bool {
    clause.values().any(|c| match c {
        WhereCondition::Ops(ops) => ops.some.is_some() || ops.none.is_some(),
        WhereCondition::SubClauses(branches) => branches.iter().any(has_quantifier),
        WhereCondition::SubClause(inner) => has_quantifier(inner),
        _ => false,
    })
}

/// A model subscription's trigger: its rules, and the nodes of its last
/// result. Cheap to clone; [`ModelTrigger::with_result`] replaces the nodes.
#[derive(Debug, Clone)]
pub(super) struct ModelTrigger {
    rules: Arc<TriggerRules>,
    /// Every `id` in the last result, at any depth, in both the spelling the
    /// result used and its canonical link-target spelling.
    nodes: Arc<HashSet<String>>,
}

impl ModelTrigger {
    pub(super) fn new(rules: TriggerRules, result: &str) -> Self {
        ModelTrigger {
            rules: Arc::new(rules),
            nodes: Arc::new(result_nodes(result)),
        }
    }

    /// The same rules, watching the nodes of `result`.
    pub(super) fn with_result(&self, result: &str) -> Self {
        ModelTrigger {
            rules: self.rules.clone(),
            nodes: Arc::new(result_nodes(result)),
        }
    }

    /// Whether `writes` may change this subscription's result.
    pub(super) fn matches(&self, writes: &Writes, store: &mut StoreLookups) -> bool {
        let rules = &self.rules;
        if rules.every_write {
            return true;
        }
        let Some(links) = &writes.links else {
            return match rules.predicates() {
                None => true,
                Some(watched) => writes
                    .predicates
                    .iter()
                    .any(|p| watched.contains(p.as_str())),
            };
        };
        links
            .iter()
            .any(|(s, p, t)| self.matches_link(s, p, t, store))
    }

    fn matches_link(
        &self,
        source: &str,
        predicate: &str,
        target: &str,
        store: &mut StoreLookups,
    ) -> bool {
        let rules = &self.rules;
        if rules.any.contains(predicate) {
            return true;
        }
        // A `literal:` target can be spelled more than one way (see
        // `canonical_target`); compare both spellings.
        let canonical = canonical_target(target);
        let spellings = [source, target, canonical.as_str()];
        if rules
            .joins
            .get(predicate)
            .is_some_and(|values| values.contains(target) || values.contains(&canonical))
        {
            return true;
        }
        if rules.anchors_any_predicate && spellings.iter().any(|e| rules.anchors.contains(*e)) {
            return true;
        }
        let near = rules.near.contains(predicate);
        if near
            && spellings
                .iter()
                .any(|e| self.nodes.contains(*e) || rules.anchors.contains(*e))
        {
            return true;
        }
        let ends = [source, target];
        if let Some(flags) = rules.scan.get(predicate) {
            if ends
                .iter()
                .any(|e| flags.iter().any(|f| store.has_flag(e, f)))
            {
                return true;
            }
        }
        if near && !rules.via.is_none() {
            return ends.iter().any(|e| {
                store.linked_to(e, |p, other| {
                    rules.via.contains(p) && self.nodes.contains(other)
                })
            });
        }
        false
    }
}

/// Every `id` in a model result, at any depth. Included records carry their
/// `id` like top-level ones do.
fn result_nodes(result: &str) -> HashSet<String> {
    fn walk(v: &Value, out: &mut HashSet<String>) {
        match v {
            Value::Object(map) => {
                for (k, v) in map {
                    match (k.as_str(), v) {
                        ("id", Value::String(id)) => {
                            out.insert(canonical_target(id));
                            out.insert(id.clone());
                        }
                        _ => walk(v, out),
                    }
                }
            }
            Value::Array(items) => items.iter().for_each(|v| walk(v, out)),
            _ => {}
        }
    }
    let mut out = HashSet::new();
    if let Ok(v) = serde_json::from_str::<Value>(result) {
        walk(&v, &mut out);
    }
    out
}

/// Store reads for one subscription check, memoised across subscriptions:
/// forty subscriptions asking whether the same node carries the same flag
/// read it once.
pub(super) struct StoreLookups<'a> {
    store: &'a SparqlStore,
    flags: HashMap<(String, Flag), bool>,
    /// Node → `(predicate, other end)` of each link touching it, or `None`
    /// past [`MAX_NEIGHBOURS`].
    neighbours: HashMap<String, Option<Vec<(String, String)>>>,
}

impl<'a> StoreLookups<'a> {
    pub(super) fn new(store: &'a SparqlStore) -> Self {
        StoreLookups {
            store,
            flags: HashMap::new(),
            neighbours: HashMap::new(),
        }
    }

    fn has_flag(&mut self, node: &str, flag: &Flag) -> bool {
        let key = (node.to_string(), flag.clone());
        if let Some(hit) = self.flags.get(&key) {
            return *hit;
        }
        // A failed read counts as an instance: re-run rather than miss.
        let hit = self
            .store
            .has_triple(node, &flag.predicate, &flag.value)
            .unwrap_or(true);
        self.flags.insert(key, hit);
        hit
    }

    /// Whether a link touching `node` satisfies `wanted(predicate, other
    /// end)`. A hub past [`MAX_NEIGHBOURS`], or a failed read, answers yes.
    fn linked_to(&mut self, node: &str, wanted: impl Fn(&str, &str) -> bool) -> bool {
        let store = self.store;
        let neighbours = self
            .neighbours
            .entry(node.to_string())
            .or_insert_with(|| store.neighbours(node, MAX_NEIGHBOURS).ok().flatten());
        match neighbours {
            None => true,
            Some(links) => links.iter().any(|(p, other)| wanted(p, other)),
        }
    }
}

#[cfg(test)]
pub(super) mod reruns {
    //! Test hook: how many times each subscription was re-run.
    use std::collections::HashMap;
    use std::sync::{LazyLock, Mutex};

    static RERUNS: LazyLock<Mutex<HashMap<String, usize>>> = LazyLock::new(Default::default);

    pub(in crate::perspectives) fn note(subscription_id: &str) {
        *RERUNS
            .lock()
            .unwrap()
            .entry(subscription_id.to_string())
            .or_default() += 1;
    }

    pub(in crate::perspectives) fn count(subscription_id: &str) -> usize {
        RERUNS
            .lock()
            .unwrap()
            .get(subscription_id)
            .copied()
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod tests;
