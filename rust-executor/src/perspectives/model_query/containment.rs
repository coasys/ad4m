//! Containment through the built-in `Container` (#1446).
//!
//! `contains(P, X)` holds when at least one of these does:
//!
//! - **(a)** `P ad4m://has_child X` exists, whatever P is: an instance of any
//!   class, or no instance at all (`ad4m://self`, a neighbourhood root, a
//!   plain-string id). `has_child` is `Container`'s own predicate and means
//!   nothing else.
//! - **(b)** P is an instance of an installed class that conforms to
//!   `Container`, and `P p X` exists for `p` the predicate of that class's
//!   relation answering to `children` (its own name, or an alias
//!   `{ Container: "children" }`). On any other node `p` may mean something
//!   else, so it counts only on instances of that class.
//!
//! The child's class never decides containment: `children` is untyped, and a
//! query's own class filters the children it reads.
//!
//! The executor resolves the installed classes once per shapes generation into
//! an **edge set** E ([`edges`]): rule (a) is one edge with no holder check,
//! each conforming class whose `children` is not `has_child` adds one edge with
//! its class as the holder check. A class whose `children` is `has_child` adds
//! nothing, because (a) covers it. Every containment read is one `UNION`
//! branch per edge ([`contains_patterns`]): a `parent: { id }` scope, a
//! `Traverse` with no predicate (a step, or a walk one depth at a time), the
//! `children` of a `Container` row ([`fill_container_rows`]), and a `where`
//! on them ([`resolve_children_where`]).

use super::builtin_classes::CONTAINER_URI;
use super::links::{decorated_link, sort_link_rows, LINKS_KEY};
use super::sparql_builder::{
    link_status_filter, proof_valid_filter, rebase_into_subquery, shape_conformance_patterns,
    LinkGuard,
};
use super::types::{IncludeValue, ModelQueryInput, ModelShape, Scope, WhereCondition};
use super::utils::{validate_iri, values_or_str_filter};
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::LinkStatus;
use deno_core::anyhow::{anyhow, Error};
use serde_json::Value;
use std::collections::BTreeMap;
use std::sync::Arc;

/// `Container`'s own predicate: containment on any node (rule a).
pub const HAS_CHILD: &str = "ad4m://has_child";

/// One way a node holds another: a link on `predicate`, from a holder that
/// must be an instance of `holder` when that is set.
#[derive(Debug, Clone)]
pub struct ContainmentEdge {
    pub predicate: String,
    /// The class whose instances hold through `predicate`; `None` for
    /// `has_child`, which holds on any node.
    pub holder: Option<Holder>,
}

/// The class a holder must be an instance of, with the shape that says so.
#[derive(Debug, Clone)]
pub struct Holder {
    pub class: String,
    pub shape: Arc<ModelShape>,
}

/// The edge set E, shared by every query of one shapes generation.
pub type ContainmentEdges = Arc<Vec<ContainmentEdge>>;

impl ContainmentEdge {
    /// Rule (a): `has_child`, on any node.
    pub fn has_child() -> Self {
        ContainmentEdge {
            predicate: HAS_CHILD.to_string(),
            holder: None,
        }
    }
}

/// E from the classes `Container` reads, each with its view (its `children`
/// under that name). The `has_child` edge comes first; then one edge per
/// class, sorted by class name, whose forward `children` relation is another
/// predicate.
///
/// A class with nothing that tells its instances apart (no required property,
/// flag or initial value) adds no edge: its holder check would be empty, and
/// its predicate would then count on every node, which is rule 2's whole
/// point to prevent. It is logged; its `has_child` links still count.
pub fn edges(
    classes: &[String],
    views: &std::collections::HashMap<String, Arc<ModelShape>>,
) -> Vec<ContainmentEdge> {
    let mut out = vec![ContainmentEdge::has_child()];
    let mut sorted: Vec<&String> = classes.iter().collect();
    sorted.sort();
    for class in sorted {
        if class == CONTAINER_URI {
            continue;
        }
        let Some(view) = views.get(class) else {
            continue;
        };
        let Some(children) = view
            .include_relations
            .iter()
            .find(|r| r.name == "children" && r.direction == "forward")
        else {
            continue;
        };
        if children.predicate == HAS_CHILD || validate_iri(&children.predicate).is_err() {
            continue;
        }
        if shape_conformance_patterns(view, false, LinkGuard::default()).is_empty() {
            log::warn!(
                "Container: class '{class}' holds its children through '{}', but nothing marks \
                 its instances (no required property, flag or initial value); those links are \
                 not read as containment. Its has_child links still are.",
                children.predicate
            );
            continue;
        }
        out.push(ContainmentEdge {
            predicate: children.predicate.clone(),
            holder: Some(Holder {
                class: class.clone(),
                shape: view.clone(),
            }),
        });
    }
    out
}

/// One edge's patterns: `holder <p> child`, the link guarded, and when the edge
/// has a holder class, that class's conformance on `holder`. `holder` and
/// `child` are terms (`<iri>` or a variable); `n` keeps the helper variables
/// of two edges apart.
pub(super) fn edge_patterns(
    edge: &ContainmentEdge,
    n: usize,
    holder: &str,
    child: &str,
    guard: LinkGuard,
) -> String {
    let pred = format!("<{}>", edge.predicate);
    format!(
        "{holder} {pred} {child} .{}{}",
        guard.join(&format!("?_ce{n}r"), holder, &pred, child),
        holder_check(edge, n, holder, guard)
    )
}

/// The edge's holder class's conformance on `holder`, its helper variables
/// namespaced by `n`; empty for an edge any node holds through.
fn holder_check(edge: &ContainmentEdge, n: usize, holder: &str, guard: LinkGuard) -> String {
    let Some(h) = &edge.holder else {
        return String::new();
    };
    let conformance = shape_conformance_patterns(&h.shape, false, guard);
    rebase_into_subquery(&conformance, &format!("ce{n}h"), holder)
        .iter()
        .map(|p| format!(" {}", p.trim()))
        .collect()
}

/// "`holder` contains `child`" over every edge of `edges`: one `UNION` branch
/// per edge, or the one edge's patterns alone.
pub(super) fn contains_patterns(
    edges: &[ContainmentEdge],
    holder: &str,
    child: &str,
    guard: LinkGuard,
) -> String {
    let branches: Vec<String> = edges
        .iter()
        .enumerate()
        .map(|(n, e)| edge_patterns(e, n, holder, child, guard))
        .collect();
    match branches.as_slice() {
        [one] => format!("    {one}"),
        _ => format!("    {{ {} }}", branches.join(" } UNION { ")),
    }
}

/// Whether `scope` reads containment: `{ id }`, or a `Traverse` that names no
/// predicate.
pub fn is_containment_scope(scope: &Scope) -> bool {
    matches!(
        scope,
        Scope::Contained(_)
            | Scope::Traverse {
                predicate: None,
                ..
            }
    )
}

/// Whether `query`, or an `include` sub-query of it, reads containment.
pub fn reads_containment(query: &ModelQueryInput) -> bool {
    query.parent.as_ref().is_some_and(is_containment_scope)
        || query.include.iter().flatten().any(|(_, v)| match v {
            IncludeValue::SubQuery(sub) => reads_containment(sub),
            IncludeValue::Bool(_) => false,
        })
}

/// Hand `edges` to `query` and every `include` sub-query: the builder reads
/// containment from them (see `ModelQueryInput::containment`).
pub fn set_containment(query: &mut ModelQueryInput, edges: &ContainmentEdges) {
    query.containment = Some(edges.clone());
    for value in query.include.iter_mut().flat_map(|m| m.values_mut()) {
        if let IncludeValue::SubQuery(sub) = value {
            set_containment(sub, edges);
        }
    }
}

/// E with no class installed: rule (a) alone. What a query gets when no
/// perspective resolved E for it (a caller running the pipeline on a bare
/// store), so a containment read never matches less than `has_child` does.
pub fn has_child_only() -> ContainmentEdges {
    Arc::new(vec![ContainmentEdge::has_child()])
}

/// Every predicate a containment read follows, and the predicates the holder
/// classes are recognised by: a node that becomes a holder turns its existing
/// links into containment, so those writes change the read too.
pub fn watched_predicates(edges: &[ContainmentEdge]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for edge in edges {
        out.push(edge.predicate.clone());
        if let Some(h) = &edge.holder {
            out.extend(h.recognised_by());
        }
    }
    out.sort();
    out.dedup();
    out
}

impl Holder {
    /// The predicates the holder check reads: its class's required
    /// properties, flags and initial values.
    pub fn recognised_by(&self) -> impl Iterator<Item = String> + '_ {
        self.shape
            .properties
            .iter()
            .filter(|p| p.is_required || p.is_flag || p.initial_value.is_some())
            .filter(|p| !p.predicate.is_empty())
            .map(|p| p.predicate.clone())
    }
}

/// What one `children` condition of a `Container` `where` asks of a row.
enum Holds {
    /// It holds one of these.
    AnyOf(Vec<String>),
    /// It holds something (`some: {}`), or nothing (`none: {}`).
    Anything { negated: bool },
}

impl Holds {
    /// `condition` on `children`, or the error naming what is answered.
    fn of(condition: &WhereCondition) -> Result<Holds, Error> {
        let refused = |what: &str| {
            anyhow!(
                "Container: `where` on `children` does not support {what}. A Container's \
                 children come from several relations (`has_child`, and each class's own \
                 `children`), and a condition on them is answered over all of them: a node \
                 id, a list of ids (holds any of them), `{{ eq }}`, `{{ contains: id }}`, \
                 `{{ some: {{}} }}` (holds something) or `{{ none: {{}} }}` (holds nothing), \
                 combined with AND, OR and NOT. Children have no class, so a clause on their \
                 properties has nothing to read: query their class with `parent: {{ id }}` \
                 instead."
            )
        };
        let ops = match condition.eq_normalized() {
            WhereCondition::String(id) => return Ok(Holds::AnyOf(vec![id.clone()])),
            WhereCondition::StringArray(ids) => return Ok(Holds::AnyOf(ids.clone())),
            WhereCondition::Ops(ops) => ops,
            _ => return Err(refused("a number, a boolean or a nested clause")),
        };
        let named = [
            (ops.not.is_some(), "`not`"),
            (ops.between.is_some(), "`between`"),
            (ops.lt.is_some(), "`lt`"),
            (ops.lte.is_some(), "`lte`"),
            (ops.gt.is_some(), "`gt`"),
            (ops.gte.is_some(), "`gte`"),
            (ops.contains.is_some(), "`contains`"),
            (ops.some.is_some(), "`some`"),
            (ops.none.is_some(), "`none`"),
            (ops.eq.is_some(), "`eq`"),
            (ops.author.is_some(), "`author`"),
        ];
        let set: Vec<&str> = named.iter().filter(|(s, _)| *s).map(|(_, n)| *n).collect();
        match (&ops.contains, &ops.some, &ops.none, set.as_slice()) {
            (Some(Value::String(id)), _, _, [_]) => Ok(Holds::AnyOf(vec![id.clone()])),
            (_, Some(clause), _, [_]) if clause.is_empty() => {
                Ok(Holds::Anything { negated: false })
            }
            (_, _, Some(clause), [_]) if clause.is_empty() => Ok(Holds::Anything { negated: true }),
            (_, Some(_), _, [_]) | (_, _, Some(_), [_]) => {
                Err(refused("`some` / `none` with a clause"))
            }
            (Some(_), _, _, [_]) => Err(refused("`contains` with anything but one id")),
            _ => Err(refused(&set.join(" + "))),
        }
    }
}

/// One `where` clause: property (or `AND` / `OR` / `NOT`) → condition.
type Clause = BTreeMap<String, WhereCondition>;

/// Visit every `children` condition of `clause`, at the top and under
/// `AND`, `OR` and `NOT`, in one fixed order. `answer` sees each and may
/// return the clause that replaces it: the condition is removed, and the
/// clause joins the same level's `AND`.
fn visit_children(
    clause: &mut Clause,
    answer: &mut dyn FnMut(&WhereCondition) -> Result<Option<Clause>, Error>,
) -> Result<(), Error> {
    if let Some(key) = clause.keys().find(|k| k.starts_with("children.")) {
        return Err(anyhow!(
            "Container: `where` key '{key}' reads a property of the children. Children have \
             no class, so there is nothing to read it from: query their class with \
             `parent: {{ id }}` instead."
        ));
    }
    for (key, condition) in clause.iter_mut() {
        match (key.as_str(), condition) {
            ("AND" | "OR", WhereCondition::SubClauses(branches)) => {
                for branch in branches {
                    visit_children(branch, answer)?;
                }
            }
            ("NOT", WhereCondition::SubClause(branch)) => visit_children(branch, answer)?,
            _ => {}
        }
    }
    let Some(condition) = clause.get("children") else {
        return Ok(());
    };
    let Some(replacement) = answer(condition)? else {
        return Ok(());
    };
    clause.remove("children");
    match clause
        .entry("AND".to_string())
        .or_insert_with(|| WhereCondition::SubClauses(vec![]))
    {
        WhereCondition::SubClauses(branches) => branches.push(replacement),
        _ => return Err(anyhow!("where: `AND` takes a list of clauses")),
    }
    Ok(())
}

/// The nodes that hold one of `children` (anything, when `None`) over
/// `edges`, under `guard`.
async fn holders_of(
    store: &SparqlStore,
    edges: &[ContainmentEdge],
    children: Option<&[String]>,
    guard: LinkGuard<'_>,
) -> Result<Vec<String>, Error> {
    if children.is_some_and(|c| c.is_empty()) {
        return Ok(vec![]);
    }
    let sparql = format!(
        "SELECT DISTINCT ?_holder WHERE {{\n{}\n    {}\n}}",
        contains_patterns(edges, "?_holder", "?_held", guard),
        children
            .map(|c| values_or_str_filter("_held", c))
            .unwrap_or_default(),
    );
    let rows: Vec<Value> = serde_json::from_str(&store.query_async(&sparql).await?)?;
    Ok(rows
        .iter()
        .filter_map(|r| r["_holder"].as_str().map(str::to_string))
        .collect())
}

/// Answer the `children` conditions of a `Container` query's `where` over
/// `edges`, before the query runs.
///
/// A `Container` query reads each row through each class it is an instance
/// of, and each reading's `children` is that class's relation alone: a Block
/// holding `a` by `we://child` and `b` by `has_child` is two readings, one
/// with `[a]`, one with `[b]`. Left to the readings, a `where` on `children`
/// filtered each on its own before [`fill_container_rows`] merged them, so
/// `NOT contains a` passed through the `has_child` reading, an `AND` over `a`
/// and `b` passed through neither, and the count and page were cut on those
/// rows (#1461 review).
///
/// So each condition becomes an `id` condition on the nodes that satisfy it
/// over every edge: "holds one of S" is `id` in the holders of S, "holds
/// something" `id` in all holders, "holds nothing" `NOT` that. The query then
/// filters, counts, orders and pages on it like on any `id` condition, in
/// every reading alike. What has no meaning over untyped children (a clause
/// on their properties, a per-link author, a comparison, a dotted key) is
/// refused rather than handed to one reading, and so are a projection and an
/// `include` over `children`, which would count or hydrate one reading's, and
/// an `order` on it, which would sort and page on one reading's (#1461
/// review).
pub async fn resolve_children_where(
    store: &SparqlStore,
    query: &mut ModelQueryInput,
    edges: &[ContainmentEdge],
) -> Result<(), Error> {
    if let Some(key) = query
        .projections
        .iter()
        .flatten()
        .find(|(_, p)| p.from == "children")
        .map(|(k, _)| k)
    {
        return Err(anyhow!(
            "Container: projection '{key}' over `children` is not supported: it would count \
             the children of one of a row's readings, not everything the row holds. Read \
             `children` itself, or count with `parent: {{ id }}` and `limit: 0`."
        ));
    }
    if query.include.iter().flatten().any(|(k, _)| k == "children") {
        return Err(anyhow!(
            "Container: `include` on `children` is not supported: children have no class to \
             hydrate them as. Query them with `parent: {{ id }}` on their class."
        ));
    }
    if query
        .order
        .iter()
        .flatten()
        .any(|(key, _)| key == "children")
    {
        return Err(anyhow!(
            "Container: `order` on `children` is not supported: rows would be sorted and paged \
             on the children of one of a row's readings, before everything the row holds is \
             merged. Order by `id` or leave `order` out, and sort the returned `children` \
             yourself."
        ));
    }
    let Some(clause) = query.where_clause.as_mut() else {
        return Ok(());
    };
    let mut asked: Vec<Holds> = Vec::new();
    visit_children(clause, &mut |condition| {
        asked.push(Holds::of(condition)?);
        Ok(None)
    })?;
    if asked.is_empty() {
        return Ok(());
    }
    let guard = LinkGuard {
        status: query.link_status.as_ref(),
        include_unverified: query.include_unverified,
    };
    let mut replacements: Vec<Clause> = Vec::new();
    for holds in &asked {
        let (children, negated) = match holds {
            Holds::AnyOf(ids) => (Some(ids.as_slice()), false),
            Holds::Anything { negated } => (None, *negated),
        };
        let ids = WhereCondition::StringArray(holders_of(store, edges, children, guard).await?);
        let in_holders = BTreeMap::from([("id".to_string(), ids)]);
        replacements.push(if negated {
            BTreeMap::from([("NOT".to_string(), WhereCondition::SubClause(in_holders))])
        } else {
            in_holders
        });
    }
    let mut replacements = replacements.into_iter();
    let clause = query.where_clause.as_mut().expect("checked above");
    visit_children(clause, &mut |_| Ok(replacements.next()))
}

/// The `children` of `Container` rows, and their `__links` rows when asked
/// for: everything E says each row holds, merged across the classes it is an
/// instance of. A row read as one class carries only that class's `children`;
/// a `Block` that also holds through `has_child` holds both, so `children`,
/// `links: ["children"]` and `parent: { id }` agree (rule 9).
///
/// Rows without a `children` key (a `properties` selection left it out) keep
/// that; `__links` is replaced only where it has a `children` entry.
/// `children` is ordered by each child's first link, then by id.
pub async fn fill_container_rows(
    store: &SparqlStore,
    rows: &mut [Value],
    edges: &[ContainmentEdge],
    link_status: Option<&LinkStatus>,
    include_unverified: Option<bool>,
) -> Result<(), Error> {
    let wants =
        |row: &Value| row.get("children").is_some() || row[LINKS_KEY].get("children").is_some();
    let ids: Vec<String> = rows
        .iter()
        .filter(|r| wants(r))
        .filter_map(|r| r["id"].as_str().map(str::to_string))
        .collect();
    if ids.is_empty() {
        return Ok(());
    }
    let guard = LinkGuard {
        status: link_status,
        include_unverified,
    };
    let branches: Vec<String> = edges
        .iter()
        .enumerate()
        .map(|(n, edge)| {
            format!(
                "VALUES ?predicate {{ <{}> }} ?source ?predicate ?target .{}",
                edge.predicate,
                holder_check(edge, n, "?source", guard)
            )
        })
        .collect();
    let link_status = link_status_filter(link_status);
    let proof_valid = proof_valid_filter(include_unverified);
    let sparql = format!(
        r#"SELECT ?source ?predicate ?target ?wireTarget ?author ?timestamp ?proofKey ?proofSig ?proofValid WHERE {{
    {{ {} }}
    {}
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofKey> ?proofKey . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofSignature> ?proofSig . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/proofValid> ?proofValid . }}
    OPTIONAL {{ ?_reifier <ad4m://ontology/wireTarget> ?wireTarget . }}
{link_status}{proof_valid}}}"#,
        branches.join(" } UNION { "),
        values_or_str_filter("source", &ids),
    );
    let found: Vec<Value> = serde_json::from_str(&store.query_async(&sparql).await?)?;
    // source -> (children by first timestamp, link rows)
    let mut by_source: BTreeMap<String, (BTreeMap<String, String>, Vec<Value>)> = BTreeMap::new();
    for row in &found {
        let (Some(source), Some(target), Some(timestamp)) = (
            row["source"].as_str(),
            row["target"].as_str(),
            row["timestamp"].as_str(),
        ) else {
            continue;
        };
        let entry = by_source.entry(source.to_string()).or_default();
        let first = entry
            .0
            .entry(target.to_string())
            .or_insert_with(|| timestamp.to_string());
        if timestamp < first.as_str() {
            *first = timestamp.to_string();
        }
        entry.1.push(decorated_link(row));
    }
    for row in rows.iter_mut() {
        let Some(id) = row["id"].as_str().map(str::to_string) else {
            continue;
        };
        let (children, mut links) = by_source.remove(&id).unwrap_or_default();
        let mut children: Vec<(String, String)> =
            children.into_iter().map(|(t, ts)| (ts, t)).collect();
        children.sort();
        sort_link_rows(&mut links);
        let Value::Object(obj) = row else {
            continue;
        };
        if obj.contains_key("children") {
            obj.insert(
                "children".to_string(),
                Value::Array(
                    children
                        .into_iter()
                        .map(|(_, t)| Value::String(t))
                        .collect(),
                ),
            );
        }
        if let Some(Value::Object(row_links)) = obj.get_mut(LINKS_KEY) {
            if row_links.contains_key("children") {
                row_links.insert("children".to_string(), Value::Array(links));
            }
        }
    }
    Ok(())
}
