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
//! `include`, projections, dotted `order` keys, `where` quantifiers and the
//! parent scope) and
//! matches each written link on more than its predicate. A write re-runs the
//! subscription when one of these holds, checked in this order:
//!
//! 1. **`any`**: the predicate alone decides, as before. Used where the
//!    query's reads cannot be told apart by node: `links`, `producedByFlow`,
//!    property getters, transitive reads, classes with no flag, and, when the
//!    query has a window, the property a dotted `order` key sorts by (the
//!    sort reads it on every linked target, flagged or not).
//! 2. **`joins`**: a flag predicate written (or removed) with one of the
//!    flag values of a class the subscription scans. This is how a new
//!    record enters, and how a deleted one leaves the count. Under a
//!    one-hop parent scope the flagged node must also sit inside it —
//!    linked to an anchor through the scope's edge — since a record under
//!    another parent can never be in the result (#1795).
//! 3. **`near`**: a predicate read on records in the result, written on a
//!    node that is in the last result or is a parent-scope anchor. For a node
//!    in the last result this is a fast path: `scan` (a top-level record
//!    carries its flag) or `via` (an included record is linked to its parent
//!    in the result) matches the same write, after a store read.
//! 4. **`holders`**: under a containment step in (`parent: { ids,
//!    direction: "in" }` without a predicate), a predicate a holder class is
//!    recognised by, written on a node that links an anchor through that
//!    class's edge. This is how a node becomes a holder of an anchor when the
//!    subscribed class does not read the holder's flag (#1461 review).
//! 5. **`scan`**: a predicate of a scanned class (the subscribed class and
//!    any class a `where` quantifier reads), written on a node that is an
//!    instance of that class: it has one of the class's flag links. This is
//!    how a record that is not in the result yet moves into a filter or a page.
//! 6. **`via`**: a `near` predicate written on a node linked, through a
//!    relation the query includes, projects or orders by (`comments.text`),
//!    to a node in the last result.
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

use crate::perspectives::model_query::containment::{watched_predicates, ContainmentEdge};
use crate::perspectives::model_query::types::{ScopeDirection, ShapeProperty};
use crate::perspectives::model_query::utils::validate_iri;
use crate::perspectives::model_query::{
    conformance_predicates, IncludeValue, ModelQueryInput, ModelShape, Scope, ShapeResolver,
    WhereCondition,
};
use crate::perspectives::sparql_store::{canonical_target, stored_as_literal, SparqlStore};
use crate::types::{DecoratedLinkExpression, Link};

/// Past this many distinct links in one batch, a batch keeps only its
/// predicates and every model subscription falls back to predicate matching.
/// A bulk import must not hold every triple it wrote until the next check.
const MAX_TRACKED_LINKS: usize = 10_000;

/// How many links touching one node, through one `via` relation, the `via`
/// rule reads before it stops and re-runs. A node with more than this is a
/// large container, and scanning it on every write would cost more than the
/// query it might save.
const MAX_NEIGHBOURS: usize = 2_000;

/// The writes of one subscription-check batch. `pub(crate)`: the
/// auto-processor watch loops name it in their wake-feed receiver
/// (#1794); its internals stay `pub(super)`.
#[derive(Debug, Clone, Default)]
pub(crate) struct Writes {
    /// Every predicate written or removed. Raw SPARQL subscriptions match on
    /// these alone.
    pub(super) predicates: HashSet<String>,
    /// Each link written or removed, or `None` once the batch passed
    /// [`MAX_TRACKED_LINKS`].
    links: Option<HashSet<Written>>,
    /// The write generations this batch's own store writes took: one for a
    /// diff committed in a single transaction, one per link on the
    /// link-by-link fallback. The feed entry it rides lets a draining tick
    /// prove the feed covers the store's movement — every generation since
    /// the last proven one must have an entry — so it is filled by the
    /// commit path, not by `writes_of`. Empty for a `Writes` built outside
    /// a commit (subscription checks, tests).
    pub(crate) generations: Vec<u64>,
}

/// One written or removed link, with what every subscription's check needs
/// to know about its target worked out once.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Written {
    source: String,
    predicate: String,
    target: String,
    /// `target` as it reads back from the store (see [`canonical_target`]).
    canonical: String,
    /// The target is stored as an RDF literal: the link is a value on its
    /// source and never links into a record (see [`stored_as_literal`]).
    literal: bool,
}

impl Writes {
    pub(super) fn new() -> Self {
        Writes {
            predicates: HashSet::new(),
            links: Some(HashSet::new()),
            generations: Vec::new(),
        }
    }

    /// Record one written or removed link that has a predicate.
    pub(super) fn record(&mut self, source: &str, predicate: &str, target: &str) {
        self.predicates.insert(predicate.to_string());
        if let Some(links) = &mut self.links {
            if links.len() >= MAX_TRACKED_LINKS {
                self.links = None;
            } else {
                links.insert(Written {
                    source: source.into(),
                    predicate: predicate.into(),
                    target: target.into(),
                    canonical: canonical_target(target),
                    literal: stored_as_literal(target),
                });
            }
        }
    }

    /// Fold another batch's writes into this one.
    pub(super) fn extend(&mut self, other: Writes) {
        self.predicates.extend(other.predicates);
        self.generations.extend(other.generations);
        match (&mut self.links, other.links) {
            (Some(mine), Some(theirs)) if mine.len() + theirs.len() <= MAX_TRACKED_LINKS => {
                mine.extend(theirs)
            }
            _ => self.links = None,
        }
    }

    /// Whether the batch wrote `predicate`, for tests that must check the
    /// matcher saw one specific batch — not whatever the feed carried first.
    #[cfg(test)]
    pub(crate) fn wrote(&self, predicate: &str) -> bool {
        self.predicates.contains(predicate)
    }

    /// Whether the batch is a schema write — the classification
    /// `inbound_touches_shacl` makes over a diff for the shape cache, made
    /// over these links. A kept transcript read under the old shape is stale
    /// on such a batch whatever its data predicates are, so the
    /// auto-processor marks every processor stale (`auto_processor_mark_stale`).
    /// With per-link detail gone (a batch past [`MAX_TRACKED_LINKS`]) a
    /// predicate whose link *could* be a schema write reads as one —
    /// `&|_| true` answers the source question yes there:
    /// over-marking costs a re-read, under-marking keeps a stale read.
    /// `is_property_shape` tells whether a link's source is a node an
    /// existing `sh://property` link names; `super::shacl_write_link`
    /// needs it for the predicates instance data also carries.
    pub(super) fn touches_shacl(&self, is_property_shape: &dyn Fn(&str) -> bool) -> bool {
        match &self.links {
            Some(links) => links.iter().any(|l| {
                super::shacl_write_link(&l.source, &l.predicate, &l.target, is_property_shape)
            }),
            None => self
                .predicates
                .iter()
                .any(|p| super::shacl_write_link("", p, "ad4m://SubjectClass", &|_| true)),
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
            generations: Vec::new(),
        }
    }
}

/// Collect a diff's links into [`Writes`]. `None` when a link has no
/// predicate, which leaves nothing to match on: check every subscription.
pub(crate) fn writes_of<'a>(
    links: impl IntoIterator<Item = &'a DecoratedLinkExpression>,
) -> Option<Writes> {
    writes_of_links(links.into_iter().map(|l| &l.data))
}

/// [`writes_of`] over `Link`s, the store's plain form: a `PerspectiveDiff`
/// (`LinkExpression`) or a mixed applied-plus-retracted set maps to them by
/// `&l.data` and feeds the feed entry the same terms.
pub(crate) fn writes_of_links<'a>(links: impl IntoIterator<Item = &'a Link>) -> Option<Writes> {
    let mut writes = Writes::new();
    for link in links {
        let predicate = link.predicate.as_deref()?;
        writes.record(&link.source, predicate, &link.target);
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

    /// Every predicate either set holds.
    fn extend(&mut self, other: Predicates) {
        match other {
            Predicates::All => *self = Predicates::All,
            Predicates::None => {}
            Predicates::Some(theirs) => theirs.iter().for_each(|p| self.add(p)),
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
    /// A one-hop parent scope narrows `joins` (a flag write) to nodes
    /// already linked to an anchor through the scope's edge: a record under
    /// another parent can never be in the result, so its create or delete
    /// cannot change it. The edge predicates the scope follows; `All` when
    /// it follows any (`Scope::Model` without a `field`). `None` when the
    /// query has no parent — or when the scope's membership is deeper than
    /// one link (transitive / `levels`) and one neighbour read cannot decide
    /// it, so flag writes stay ungated.
    scoped: Option<Predicates>,
    /// A containment step in from the anchors: predicate a holder class is
    /// recognised by → the edge predicates that class holds through. A write
    /// of one re-runs when its source links an anchor through one of them:
    /// the node just became (or stopped being) a holder of an anchor.
    holders: HashMap<String, Predicates>,
    /// The subscribed class's own flag links, in both the flag value's and
    /// its canonical spelling. Only these are gated by `scoped` (`joins`,
    /// `scan`): a flag of a class the query scans through a `where`
    /// quantifier belongs to records that are not scope members themselves —
    /// one enters the result by linking to a member, not to an anchor, so
    /// the parent-edge gate would miss it (#1795).
    direct_flags: HashSet<Flag>,
    /// The flags of the classes a `where` quantifier scans, in both
    /// spellings. A flag here is never gated, even when the subscribed
    /// class carries it too: a self-referential quantifier (`Comment` with
    /// `replies: { some: … }`) reads records of the same flag that enter
    /// the result by linking to a member, not by sitting under an anchor.
    quantified_flags: HashSet<Flag>,
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
        out.extend(self.holders.keys().map(String::as_str));
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
        self.near.extend(other.near);
        self.via.extend(other.via);
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
        // The merged trigger gates flag writes only when every class's
        // scope does; a class without one admits them from anywhere.
        self.scoped = match (self.scoped.take(), other.scoped) {
            (Some(mut a), Some(b)) => {
                a.extend(b);
                Some(a)
            }
            _ => None,
        };
        for (predicate, edges) in other.holders {
            self.holders.entry(predicate).or_default().extend(edges);
        }
        self.direct_flags.extend(other.direct_flags);
        self.quantified_flags.extend(other.quantified_flags);
    }

    /// Whether the parent-scope gate applies to a write of `flag`: it is
    /// the subscribed class's own, and no quantified class shares it.
    fn gated(&self, flag: &Flag) -> bool {
        self.direct_flags.contains(flag) && !self.quantified_flags.contains(flag)
    }
}

impl super::PerspectiveInstance {
    /// The trigger for a model subscription over `class_names` with
    /// `query_json`, watching the nodes of its first `result`. Each class
    /// contributes its own rules; a query over several classes (#1238)
    /// re-runs on a write any of them matches.
    #[cfg(test)]
    pub(super) fn build_model_trigger<'a>(
        &self,
        class_names: impl IntoIterator<Item = &'a str>,
        query_json: &str,
        result: &str,
    ) -> ModelTrigger {
        let query: ModelQueryInput = serde_json::from_str(query_json).unwrap_or_default();
        let edges = crate::perspectives::model_query::containment::reads_containment(&query)
            .then(|| self.containment_edges().expect("containment edges"));
        self.build_model_trigger_with(
            &self.shape_resolver(),
            class_names,
            query_json,
            result,
            edges.as_deref().map(Vec::as_slice),
        )
    }

    /// [`Self::build_model_trigger`] reading shapes through `resolver`: the
    /// views of an abstract class's classes (#1435), whose `where` keys are
    /// the abstract class's names.
    pub(super) fn build_model_trigger_with<'a>(
        &self,
        resolver: &dyn ShapeResolver,
        class_names: impl IntoIterator<Item = &'a str>,
        query_json: &str,
        result: &str,
        containment: Option<&[ContainmentEdge]>,
    ) -> ModelTrigger {
        let mut rules: Option<TriggerRules> = None;
        for class_name in class_names {
            let shape = resolver.get_shape(class_name).ok();
            let side = self.model_query_side_predicates(shape.as_deref(), query_json);
            let class_rules = build_rules(
                class_name,
                query_json,
                resolver,
                side,
                &super::extract_predicates_from_sparql,
                containment.unwrap_or_default(),
            );
            match &mut rules {
                Some(rules) => rules.merge(class_rules),
                None => rules = Some(class_rules),
            }
        }
        // No class to watch: nothing to tell writes apart by.
        ModelTrigger::new(rules.unwrap_or_else(TriggerRules::every_write), result)
    }

    /// The trigger SPARQL and predicate set of a model subscription over
    /// `targets`: the union of each class's trigger set, matched on the
    /// predicate alone.
    pub(super) fn model_trigger_query(
        &self,
        targets: &super::abstract_classes::ReadTargets,
        query_json: &str,
    ) -> (String, HashSet<String>) {
        // Each class contributes its own trigger set; the matcher is
        // predicate-only, so the sets meet as their union. A view renames
        // properties, never their predicates, so the classes' own shapes
        // give the same set.
        let mut trigger_predicates: Vec<String> = targets
            .classes
            .names()
            .iter()
            .flat_map(|class_name| {
                let mut predicates =
                    self.build_model_trigger_predicates(class_name, Some(query_json));
                // A built-in's own rows (`Container`) have no stored shape.
                if let Some(view) = targets.views.get(class_name) {
                    predicates.extend(view.predicates());
                }
                predicates
            })
            .collect();
        if let Some(edges) = &targets.containment {
            trigger_predicates.extend(watched_predicates(edges));
        }
        trigger_predicates.sort();
        trigger_predicates.dedup();

        // A predicate is interpolated as `<{p}>`: one that is not a valid IRI
        // can never match a stored link — and must not be spliced into the
        // SPARQL text either.
        let patterns: Vec<String> = trigger_predicates
            .iter()
            .enumerate()
            .filter(|(_, p)| validate_iri(p).is_ok())
            .map(|(i, p)| format!("{{ ?s <{}> ?o{} . }}", p, i))
            .collect();
        let trigger_sparql = if patterns.is_empty() {
            // Fallback: match any triple (always re-check)
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o . } LIMIT 1".to_string()
        } else {
            format!(
                "SELECT ?s ?p ?o WHERE {{ {} }} LIMIT 1",
                patterns.join(" UNION ")
            )
        };
        (trigger_sparql, trigger_predicates.into_iter().collect())
    }

    /// Predicates a model query reads beside its classes' shapes, which a
    /// subscription matches on the predicate alone.
    pub(super) fn model_query_side_predicates(
        &self,
        shape: Option<&ModelShape>,
        query_json: &str,
    ) -> Vec<String> {
        let mut predicates = Vec::new();

        // `links` reaches predicates the shape does not declare (a revocation
        // tombstone); a subscription asking for one must re-run when it lands.
        if let Some(shape) = shape {
            if let Ok(query) = serde_json::from_str::<
                crate::perspectives::model_query::ModelQueryInput,
            >(query_json)
            {
                predicates.extend(crate::perspectives::model_query::links_trigger_predicates(
                    shape,
                    &query,
                    &self.shape_resolver(),
                ));
            }
        }

        if let Ok(query) = serde_json::from_str::<serde_json::Value>(query_json) {
            // `producedByFlow` resolves through the flow's receipts and
            // the flow catalogue, not the shape: re-run when a receipt's
            // index entry or body lands (sync may deliver them apart) —
            // the reads `load_flow_receipts` makes — or a flow is
            // registered, whose definition is written with its
            // `rdf://type ad4m://Flow` link.
            if query
                .get("where")
                .and_then(|w| w.get("producedByFlow"))
                .is_some()
            {
                use crate::perspectives::flow_instance::{produced, receipt};
                predicates.push(produced::FLOW_RECEIPT_INDEX_PREDICATE.to_string());
                predicates.push(receipt::FLOW_RECEIPT_CONTENT_PREDICATE.to_string());
                predicates.push("rdf://type".to_string());
            }
        }

        predicates
    }
}

/// Build the rules for a subscription on `class_name` with `query_json`.
///
/// `extra_any` are predicates the query reads beside its classes' shapes
/// (`links`, `producedByFlow`; see `model_query_side_predicates`), matched on
/// the predicate alone. `getter_predicates` names the predicates a property
/// getter's SPARQL reads. `containment` is the edge set a containment scope
/// reads.
pub(super) fn build_rules(
    class_name: &str,
    query_json: &str,
    resolver: &dyn ShapeResolver,
    extra_any: impl IntoIterator<Item = String>,
    getter_predicates: &dyn Fn(&str) -> HashSet<String>,
    containment: &[ContainmentEdge],
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
        containment,
    };
    walk.rules.any.extend(extra_any);
    walk.scan_class(&shape, query.where_clause.as_ref(), true);
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
    containment: &'a [ContainmentEdge],
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
    /// record itself: the subscribed class (`direct`), and every class a
    /// `where` quantifier reads. Only the subscribed class's flags are
    /// remembered in `direct_flags`: the parent-scope gate proves a member
    /// out of scope, and a quantified class's records are not members.
    fn scan_class(
        &mut self,
        shape: &ModelShape,
        where_clause: Option<&std::collections::BTreeMap<String, WhereCondition>>,
        direct: bool,
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
        let recorded = if direct {
            &mut self.rules.direct_flags
        } else {
            &mut self.rules.quantified_flags
        };
        for flag in &flags {
            recorded.insert(flag.clone());
            recorded.insert(Flag {
                predicate: flag.predicate.clone(),
                value: canonical_target(&flag.value),
            });
        }
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
                        self.scan_class(&target, Some(inner), false);
                    }
                }
                _ => {}
            }
        }
    }

    /// Walk `query`'s includes, projections and order keys, which read from
    /// `shape`. Recurses over the query, never over the shapes, so it ends.
    fn query(&mut self, shape: &ModelShape, query: &ModelQueryInput) {
        // A dotted order key (`comments.text`) sorts by a property of the
        // relation's targets, `include` or not: the page query joins them
        // through the class's declared relation (`SortKey::RelationProperty`).
        // Watch the target as an include one level down, without recursing:
        // only its own properties are read. A key naming no declared relation
        // sorts by nothing (the query falls back to the timestamp).
        //
        // Under a window (`limit` / `offset`) the last result holds only the
        // page, so a target linked to a record off the page is near nothing
        // in it, and `near` / `via` miss the edit that moves that record onto
        // the page. Re-run on any write of the sorted property then: the sort
        // reads it on every linked target, whether or not the target carries
        // its class's flag, so `scan` would miss a target without one.
        // Without a window every match is in the result already.
        let windowed = query.limit.is_some() || query.offset.is_some();
        for key in query.order.iter().flatten().map(|(key, _)| key.as_str()) {
            let Some((rel_name, prop_name)) = key.split_once('.') else {
                continue;
            };
            let Some(rel) = shape.include_relations.iter().find(|r| r.name == rel_name) else {
                continue;
            };
            self.rules.via.add(&rel.predicate);
            let target = (!rel.target_class_name.is_empty())
                .then(|| self.resolver.get_shape(&rel.target_class_name).ok())
                .flatten();
            match target {
                Some(target) => {
                    self.watch_class(&target);
                    if windowed {
                        match target.properties.iter().find(|p| p.name == prop_name) {
                            Some(p) => self.rules.any.extend([p.predicate.clone()]),
                            None => self.rules.any.extend(target.predicates()),
                        }
                    }
                }
                // The targets' class is not known, so neither is the sorted
                // property's predicate: any write on a target may move it.
                None => self.rules.near = Predicates::All,
            }
        }
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
    /// relation from it, may be read. A member's typed relations are read
    /// even without a sub-include: `hydrate_polymorphic` runs each member
    /// class's query, which fills them through their conformance getters.
    /// So a write on a node linked to a member by any predicate may change
    /// the result.
    ///
    /// The cost: a write on a node outside the result reads all its links.
    /// A class marker (a flag's value) is linked to every instance, so a
    /// create re-runs when an instance of its class is in the result, or
    /// when the class has more than [`MAX_NEIGHBOURS`] instances. That is
    /// about `dev`'s behaviour for these subscriptions. Narrowing it to
    /// the relation predicates of the registered classes would miss a
    /// class registered after subscribing.
    fn unknown_members(&mut self, sub: &ModelQueryInput) {
        self.rules.near = Predicates::All;
        self.rules.via = Predicates::All;
        if sub.where_clause.as_ref().is_some_and(has_quantifier) {
            self.rules.every_write = true;
        }
    }

    fn parent(&mut self, parent: &Scope) {
        match parent {
            Scope::Raw { id, predicate } => {
                self.rules.anchors.insert(id.clone());
                self.rules.near.add(predicate);
                self.rules.scoped = Some(Predicates::Some([predicate.clone()].into()));
            }
            Scope::Model { id, field, .. } => {
                self.rules.anchors.insert(id.clone());
                match field {
                    Some(field) => {
                        self.rules.near.add(field);
                        self.rules.scoped = Some(Predicates::Some([field.clone()].into()));
                    }
                    None => {
                        self.rules.anchors_any_predicate = true;
                        self.rules.scoped = Some(Predicates::All);
                    }
                }
            }
            Scope::Traverse {
                ids,
                predicate,
                transitive,
                levels,
                direction,
                path,
                ..
            } => {
                // One step in: the rows hold an anchor. A node that becomes a
                // holder is not an anchor and may be in no result yet (the
                // subscribed class does not read the holder's flag), so its
                // flag write is related back to the anchors (`holders`).
                if predicate.is_none()
                    && !*transitive
                    && levels.is_none()
                    && path.is_none()
                    && *direction == ScopeDirection::In
                {
                    for edge in self.containment {
                        let Some(holder) = &edge.holder else {
                            continue;
                        };
                        for recognised_by in holder.recognised_by() {
                            self.rules
                                .holders
                                .entry(recognised_by)
                                .or_default()
                                .add(&edge.predicate);
                        }
                    }
                }
                self.rules.anchors.extend(ids.iter().cloned());
                // A containment traversal follows every edge's predicate, and
                // a node that becomes a holder changes it too (see
                // `Scope::Contained` below). The scope gate below takes only
                // the edges — a `recognised_by` flag links a holder to its
                // flag value, not a member to an anchor.
                let edges: Vec<String> = self
                    .containment
                    .iter()
                    .map(|e| e.predicate.clone())
                    .collect();
                let followed: Vec<String> = match (predicate, path) {
                    (Some(predicate), _) => vec![predicate.clone()],
                    (None, Some(path)) => path.clone(),
                    // A containment scope's edges are same-level alternatives
                    // — every one is the scope's one edge read, not a deeper
                    // hop, so the `hop > 0` below must not see them as a
                    // path. And they are not `watched_predicates`: a holder
                    // flag's predicate is no edge.
                    (None, None) => edges.clone(),
                };
                for (hop, predicate) in followed.iter().enumerate() {
                    if *transitive || levels.is_some() || (path.is_some() && hop > 0) {
                        // Below the first level of a `path` — and anywhere
                        // under a scope deeper than one link — a link's ends
                        // are neither anchors nor (when a level limit cut
                        // them) result nodes.
                        self.rules.any.insert(predicate.clone());
                    } else {
                        self.rules.near.add(predicate);
                    }
                }
                // A holder class's flag is not an edge the scope follows,
                // but a write of one still moves the result: on the anchor
                // (a holder under `direction: "out"`), on a result node, or —
                // `direction: "in"`, one hop out — on a node linked to an
                // anchor through that class's edge (the `holders` rule
                // above). Deeper than one hop, none of those reads decides
                // it, so it re-runs outright.
                if predicate.is_none() && path.is_none() {
                    for edge in self.containment {
                        let Some(holder) = &edge.holder else {
                            continue;
                        };
                        for recognised_by in holder.recognised_by() {
                            if *transitive || levels.is_some() {
                                self.rules.any.insert(recognised_by);
                            } else {
                                self.rules.near.add(&recognised_by);
                            }
                        }
                    }
                }
                // A single hop: scope membership is one neighbour read.
                if !*transitive && levels.is_none() {
                    let edges = match (predicate, path) {
                        (Some(predicate), _) => vec![predicate.clone()],
                        // Only a one-hop path is provable by one lookup; a
                        // longer one's flags could sit on unseen intermediates.
                        (None, Some(path)) if path.len() == 1 => path.clone(),
                        (None, Some(_)) => Vec::new(),
                        (None, None) => edges,
                    };
                    if !edges.is_empty() {
                        self.rules.scoped = Some(Predicates::Some(edges.into_iter().collect()));
                    }
                }
            }
            // Every edge's predicate on the anchor, and the predicates its
            // holder classes are recognised by: a flag written on the anchor
            // turns its existing links on that class's predicate into
            // containment. The gate takes only the edge predicates.
            Scope::Contained(c) => {
                self.rules.anchors.insert(c.id.clone());
                for predicate in watched_predicates(self.containment) {
                    self.rules.near.add(&predicate);
                }
                self.rules.scoped = Some(Predicates::Some(
                    self.containment
                        .iter()
                        .map(|e| e.predicate.clone())
                        .collect(),
                ));
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
/// `pub(crate)`: the auto-processor watch loops keep one per processor,
/// built exactly as a model subscription's is (#1794).
#[derive(Debug, Clone)]
pub(crate) struct ModelTrigger {
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
    pub(crate) fn with_result(&self, result: &str) -> Self {
        ModelTrigger {
            rules: self.rules.clone(),
            nodes: Arc::new(result_nodes(result)),
        }
    }

    /// [`Self::with_result`] for a result's instances already parsed.
    pub(crate) fn with_instances(&self, instances: &[Value]) -> Self {
        let mut nodes = HashSet::new();
        for instance in instances {
            nodes.extend(value_nodes(instance));
        }
        ModelTrigger {
            rules: self.rules.clone(),
            nodes: Arc::new(nodes),
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
        links.iter().any(|link| self.matches_link(link, store))
    }

    fn matches_link(&self, link: &Written, store: &mut StoreLookups) -> bool {
        let rules = &self.rules;
        let Written {
            source,
            predicate,
            target,
            canonical,
            literal,
        } = link;
        if rules.any.contains(predicate) {
            return true;
        }
        // A `literal:` target can be spelled more than one way (see
        // `canonical_target`); compare both spellings.
        let spellings = [source, target, canonical];
        if rules
            .joins
            .get(predicate)
            .is_some_and(|values| values.contains(target) || values.contains(canonical))
        {
            // Under a one-hop parent scope the flag counts only when the
            // node is already in it: a class flag under another parent
            // cannot change a result that reads only this scope (#1795).
            // Its edge write is `near` the anchor and wakes on its own.
            // The gate proves a scope MEMBER out of scope, so it applies
            // only to the subscribed class's own flags: a quantified class's
            // record enters the result by linking to a member, not by
            // sitting under an anchor.
            let direct = [target, canonical].iter().any(|value| {
                rules.gated(&Flag {
                    predicate: predicate.clone(),
                    value: (*value).clone(),
                })
            });
            match &rules.scoped {
                None => return true,
                _ if !direct || self.nodes.contains(source) || rules.anchors.contains(source) => {
                    return true;
                }
                Some(edges) if store.linked_through(source, edges, &rules.anchors) => {
                    return true;
                }
                Some(_) => {}
            }
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
        // A holder's flag, on a node that links an anchor through that
        // holder's edge. Read around the source only: the target is a flag
        // value, which every instance links.
        if let Some(edges) = rules.holders.get(predicate) {
            if store.linked_through(source, edges, &rules.anchors) {
                return true;
            }
        }
        // The ends a write can change a record at. A target stored as an RDF
        // literal is a value on the source: no record carries a flag on it,
        // and no relation reaches it. Shared values (`true`, a status, an
        // emoji) are hubs, so reading around one would cost the most.
        let both = [source, target];
        let ends = if *literal { &both[..1] } else { &both[..] };
        if let Some(flags) = rules.scan.get(predicate) {
            // `scan` reads "a node of the queried class" — under a one-hop
            // scope that still has to mean in scope: a flagged node under
            // another parent is a Post, but not one this result reads. The
            // gate decides membership of the QUERIED class only; a
            // quantifier-scanned class's flag skips it for the reason
            // `joins` gives above.
            if ends.iter().any(|e| {
                flags.iter().any(|f| {
                    store.has_flag(e, f)
                        && (self.nodes.contains(*e)
                            || rules.anchors.contains(*e)
                            || !rules.gated(f)
                            || match &rules.scoped {
                                None => true,
                                Some(edges) => store.linked_through(e, edges, &rules.anchors),
                            })
                })
            }) {
                return true;
            }
        }
        if near && !rules.via.is_none() {
            return ends
                .iter()
                .any(|e| store.linked_through(e, &rules.via, &self.nodes));
        }
        false
    }
}

/// Every `id` in a model result, at any depth. Included records carry their
/// `id` like top-level ones do.
fn result_nodes(result: &str) -> HashSet<String> {
    serde_json::from_str::<Value>(result)
        .map(|v| value_nodes(&v))
        .unwrap_or_default()
}

/// [`result_nodes`] over a result that is already parsed.
fn value_nodes(result: &Value) -> HashSet<String> {
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
    walk(result, &mut out);
    out
}

/// Store reads for one subscription check, memoised across subscriptions:
/// forty subscriptions asking whether the same node carries the same flag
/// read it once.
pub(super) struct StoreLookups<'a> {
    store: &'a SparqlStore,
    flags: HashMap<(String, Flag), bool>,
    /// `(node, predicate)` → `(predicate, other end)` of each link touching
    /// the node, with that predicate (`None`: with any), or `None` past
    /// [`MAX_NEIGHBOURS`].
    neighbours: HashMap<(String, Option<String>), Neighbours>,
}

/// `(predicate, other end)` of the links read around a node, or `None` past
/// [`MAX_NEIGHBOURS`].
type Neighbours = Option<Vec<(String, String)>>;

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

    /// Whether `node` is linked, through one of `via`, to one of `nodes`.
    /// Reads only the links with a `via` predicate when `via` names them, so
    /// a node with many other links (a flag value every instance points at)
    /// costs nothing. More than [`MAX_NEIGHBOURS`] such links, or a failed
    /// read, answers yes.
    fn linked_through(&mut self, node: &str, via: &Predicates, nodes: &HashSet<String>) -> bool {
        let predicates: Vec<Option<&str>> = match via {
            Predicates::None => return false,
            Predicates::Some(set) => set.iter().map(|p| Some(p.as_str())).collect(),
            Predicates::All => vec![None],
        };
        predicates.into_iter().any(|predicate| {
            let store = self.store;
            let neighbours = self
                .neighbours
                .entry((node.to_string(), predicate.map(str::to_string)))
                .or_insert_with(|| {
                    store
                        .neighbours(node, predicate, MAX_NEIGHBOURS)
                        .ok()
                        .flatten()
                });
            match neighbours {
                None => true,
                Some(links) => links.iter().any(|(_, other)| nodes.contains(other)),
            }
        })
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
