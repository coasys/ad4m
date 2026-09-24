//! Data types for the model query DSL and internal query execution.
//!
//! This module mirrors the TypeScript query types (`Query`, `WhereCondition`,
//! `IncludeProjection`, etc.) as Rust structs with serde deserialization.  It
//! also defines the internal shape metadata types ([`ModelShape`],
//! [`ShapeProperty`], [`ShapeRelation`]) and the query execution plan
//! ([`InstanceQueryPlan`]).

use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap};

// ---------------------------------------------------------------------------
// Query DSL types (mirrors TS types.ts)
// ---------------------------------------------------------------------------

/// Comparison operators for where-clause conditions.
///
/// Used inside [`WhereCondition::Ops`] to express range queries, negation,
/// and substring matching.  Multiple fields can be combined (e.g. `gt` + `lt`
/// for an open range).
///
/// `deny_unknown_fields` ensures that when `#[serde(untagged)]` tries this
/// variant, objects that contain non-WhereOps keys (e.g. a nested where clause
/// like `{"name": "Alice"}`) are rejected and fall through to `SubClause`.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WhereOps {
    #[serde(default)]
    pub not: Option<Value>,
    #[serde(default)]
    pub between: Option<(f64, f64)>,
    #[serde(default)]
    pub lt: Option<f64>,
    #[serde(default)]
    pub lte: Option<f64>,
    #[serde(default)]
    pub gt: Option<f64>,
    #[serde(default)]
    pub gte: Option<f64>,
    #[serde(default)]
    pub contains: Option<Value>,
    /// Relation quantifier: at least one linked target satisfies the nested
    /// clause. `{ comments: { some: { author: "did:…" } } }`. An empty clause
    /// means "has at least one".
    #[serde(default)]
    pub some: Option<BTreeMap<String, WhereCondition>>,
    /// Relation quantifier: no linked target satisfies the nested clause.
    /// `{ comments: { none: {} } }` is "has none at all".
    #[serde(default)]
    pub none: Option<BTreeMap<String, WhereCondition>>,
    /// Equality spelled as an operator: `{ eq: X }` means the same as the bare
    /// value `X` (a scalar, or an array for "any of"). It exists so a value can
    /// sit beside `author` in one operator object, and cannot be combined with
    /// the other value operators.
    #[serde(default)]
    pub eq: Option<Box<WhereCondition>>,
    /// Per-link author: the link that satisfies this property's value
    /// condition must have been written by an author meeting this condition
    /// (a DID, a DID array, `{ not }` or `{ contains }`). With no value
    /// condition beside it, some link on the property must have been. See
    /// [`super::link_author`].
    #[serde(default)]
    pub author: Option<Box<WhereCondition>>,
}

impl Default for WhereOps {
    fn default() -> Self {
        WhereOps {
            not: None,
            between: None,
            lt: None,
            lte: None,
            gt: None,
            gte: None,
            contains: None,
            some: None,
            none: None,
            eq: None,
            author: None,
        }
    }
}

/// A single where-clause condition.
///
/// Deserialized from JSON with `#[serde(untagged)]` — the variant is inferred
/// from the JSON value's type:
/// - `"active"` → [`String`](WhereCondition::String)
/// - `42.0` → [`Number`](WhereCondition::Number)
/// - `true` → [`Bool`](WhereCondition::Bool)
/// - `["a","b"]` → [`StringArray`](WhereCondition::StringArray) (IN operator)
/// - `[1,2,3]` → [`NumberArray`](WhereCondition::NumberArray) (IN operator)
/// - `{"gt": 5, "lt": 10}` → [`Ops`](WhereCondition::Ops)
/// - `[{"name":"Alice"},{"name":"Bob"}]` → [`SubClauses`](WhereCondition::SubClauses) (OR / AND)
/// - `{"name":"Alice","status":"active"}` → [`SubClause`](WhereCondition::SubClause) (NOT)
///
/// **Variant ordering** is significant for `#[serde(untagged)]`:
/// - `Bool` before `Number` so JSON booleans don't coerce to `f64`.
/// - `StringArray` / `NumberArray` before `SubClauses` so empty arrays `[]`
///   become `StringArray([])` rather than `SubClauses([])`.
/// - `Ops` (with `deny_unknown_fields`) before `SubClause` so operator objects
///   like `{"gt": 5}` match `Ops` and where-clause objects like `{"name":"Alice"}`
///   fall through to `SubClause`.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum WhereCondition {
    Bool(bool),
    Number(f64),
    String(String),
    StringArray(Vec<String>),
    NumberArray(Vec<f64>),
    /// Array of nested where clauses used for OR and AND combinators.
    /// The key name in the parent `BTreeMap` determines the semantic:
    /// `"OR"` → any branch must pass; `"AND"` → all branches must pass.
    SubClauses(Vec<BTreeMap<String, WhereCondition>>),
    /// Operator-based condition (`gt`, `lt`, `contains`, `not`, etc.).
    /// `WhereOps` uses `deny_unknown_fields` so objects whose keys are not
    /// WhereOps fields are rejected here and fall through to `SubClause`.
    Ops(WhereOps),
    /// Single nested where clause used for the NOT combinator.
    SubClause(BTreeMap<String, WhereCondition>),
}

impl WhereCondition {
    /// `{ eq: X }` on its own is the bare value `X`; anything else is returned
    /// as it is. For the readers that match on the bare shapes.
    pub fn eq_normalized(&self) -> &WhereCondition {
        match self {
            WhereCondition::Ops(o) if o.author.is_none() => match o.eq.as_deref() {
                Some(eq)
                    if o.not.is_none()
                        && o.between.is_none()
                        && o.lt.is_none()
                        && o.lte.is_none()
                        && o.gt.is_none()
                        && o.gte.is_none()
                        && o.contains.is_none()
                        && o.some.is_none()
                        && o.none.is_none() =>
                {
                    eq
                }
                _ => self,
            },
            _ => self,
        }
    }

    /// The where clause a `NOT` holds.
    ///
    /// Untagged deserialisation cannot see the key a value sits under, so a
    /// `NOT` clause whose keys are all operator names, `NOT: { author: A }`
    /// being the one that matters, arrives as [`Ops`](WhereCondition::Ops).
    /// This reads it back as the clause it was written as. `None` for a value
    /// that is no clause at all.
    pub fn as_not_clause(&self) -> Option<std::borrow::Cow<'_, BTreeMap<String, WhereCondition>>> {
        use std::borrow::Cow;
        let o = match self {
            WhereCondition::SubClause(branch) => return Some(Cow::Borrowed(branch)),
            WhereCondition::Ops(o) => o,
            _ => return None,
        };
        let mut clause = BTreeMap::new();
        let mut put = |key: &str, cond: WhereCondition| {
            clause.insert(key.to_string(), cond);
        };
        let from = |v: &Value| serde_json::from_value::<WhereCondition>(v.clone()).ok();
        if let Some(ref v) = o.not {
            put("not", from(v)?);
        }
        if let Some((lo, hi)) = o.between {
            put("between", WhereCondition::NumberArray(vec![lo, hi]));
        }
        for (key, n) in [("lt", o.lt), ("lte", o.lte), ("gt", o.gt), ("gte", o.gte)] {
            if let Some(n) = n {
                put(key, WhereCondition::Number(n));
            }
        }
        if let Some(ref v) = o.contains {
            put("contains", from(v)?);
        }
        if let Some(ref c) = o.some {
            put("some", WhereCondition::SubClause(c.clone()));
        }
        if let Some(ref c) = o.none {
            put("none", WhereCondition::SubClause(c.clone()));
        }
        if let Some(ref c) = o.eq {
            put("eq", (**c).clone());
        }
        if let Some(ref c) = o.author {
            put("author", (**c).clone());
        }
        Some(Cow::Owned(clause))
    }
}

/// Sort direction for ORDER BY clauses.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq)]
pub enum OrderDirection {
    ASC,
    DESC,
}

/// Custom serde deserializer for the `order` field.
///
/// Accepts two JSON shapes that the TS client may send:
/// - Tuple array: `[["name", "ASC"], ["age", "DESC"]]`
/// - Object map: `{"name": "ASC", "age": "DESC"}`
///
/// Top-level queries typically send the tuple form; sub-queries inside
/// `include` may send the object form.
fn deserialize_order_flex<'de, D>(
    deserializer: D,
) -> Result<Option<Vec<(String, OrderDirection)>>, D::Error>
where
    D: Deserializer<'de>,
{
    let val: Option<Value> = Option::deserialize(deserializer)?;
    let val = match val {
        Some(v) => v,
        None => return Ok(None),
    };
    match val {
        // Array of [key, direction] tuples
        Value::Array(arr) => {
            let mut out = Vec::new();
            for item in arr {
                if let Value::Array(pair) = item {
                    if pair.len() != 2 {
                        return Err(serde::de::Error::custom(
                            "order entry must be a [key, direction] pair",
                        ));
                    }
                    let key = pair[0]
                        .as_str()
                        .ok_or_else(|| serde::de::Error::custom("order key must be a string"))?
                        .to_string();
                    if key.is_empty() {
                        return Err(serde::de::Error::custom("order key must not be empty"));
                    }
                    let dir_str = pair[1].as_str().unwrap_or("ASC");
                    let dir = if dir_str.eq_ignore_ascii_case("desc") {
                        OrderDirection::DESC
                    } else {
                        OrderDirection::ASC
                    };
                    out.push((key, dir));
                } else {
                    return Err(serde::de::Error::custom("order entry must be an array"));
                }
            }
            Ok(Some(out))
        }
        // Object map { key: direction }
        Value::Object(map) => {
            let mut out = Vec::new();
            for (key, dir_val) in map {
                if key.is_empty() {
                    return Err(serde::de::Error::custom("order key must not be empty"));
                }
                let dir_str = dir_val
                    .as_str()
                    .ok_or_else(|| serde::de::Error::custom("order direction must be a string"))?;
                let dir = if dir_str.eq_ignore_ascii_case("desc") {
                    OrderDirection::DESC
                } else {
                    OrderDirection::ASC
                };
                out.push((key, dir));
            }
            Ok(Some(out))
        }
        _ => Ok(None),
    }
}

/// A named subgraph scope: a parent node + linking predicate.
///
/// Reusable across contexts that need to identify "the subtree under node X
/// linked via predicate P": query-time filtering (e.g. "all Messages belonging
/// to Channel X" as a `parent` filter on `ModelQueryInput`) AND write-time
/// scoping (e.g. AutoProcessor's `existing_scope` / `mint_scope` fields — the
/// former constrains dedup lookups; the latter turns each new mint into a
/// child link under the given node).
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Scope {
    Model {
        model: String,
        id: String,
        field: Option<String>,
    },
    Raw {
        id: String,
        predicate: String,
    },
    /// Bounded traversal from one or more anchors — see [`Traverse`](Scope::Traverse).
    ///
    /// A third variant rather than options on the two above, because those are
    /// constructed as struct literals throughout `auto_processor`,
    /// `interpretation` and `flow_context`, none of which traverse anything.
    /// Adding fields there would have meant editing every write-scope call site
    /// to say "and don't traverse", which is noise at each one and a much wider
    /// blast radius than the feature deserves. The split is also honest: the
    /// variants above identify *an* anchor, this one says how to walk from
    /// *several*.
    ///
    /// `rename_all` because the TS `TraverseScope` is spread verbatim into the
    /// wire JSON, spelling this variant's multi-word fields in camelCase — and
    /// serde ignores unknown fields on an untagged variant, so a missed
    /// spelling here is not an error but a silently dropped limit. The other
    /// variants' fields are all single words, which is why the enum never
    /// needed this before. The snake_case alias keeps Rust-side spellings
    /// working.
    #[serde(rename_all = "camelCase")]
    Traverse {
        /// The anchors to walk from. One query answers for all of them, which
        /// is what keeps a level of a tree to a single round trip (and a single
        /// subscription) rather than one per parent.
        #[serde(deserialize_with = "deserialize_ids_flex")]
        ids: Vec<String>,
        predicate: String,
        /// Follow the predicate as far as it goes, rather than one step.
        ///
        /// Note the path reports only its endpoints: SPARQL property paths bind
        /// no intermediate variables, so a transitive read says *that* a node is
        /// under the anchor and never *where*. Reconstructing the shape needs
        /// the inverse relation read separately (`@BelongsTo`).
        ///
        /// Every named anchor is excluded from the result, as with `levels` —
        /// an anchor is where the read started, not something it found. The `+`
        /// path gives that for free only in a tree; in a cycle the anchor is
        /// one-or-more steps from itself, so the exclusion is stated in the
        /// query rather than left to the path operator.
        #[serde(default)]
        transitive: bool,
        #[serde(default)]
        direction: ScopeDirection,
        /// Keep at most this many results *per anchor*, applied after ordering
        /// and before hydration.
        ///
        /// SPARQL cannot express a per-group limit — it has no window functions,
        /// and its sub-SELECTs are uncorrelated — so this is applied in the
        /// executor between the two phases of the query. That placement is the
        /// whole point: the id phase over-fetches rows of one string, and the
        /// hydration phase, which is the expensive half, sees only the survivors.
        #[serde(default, alias = "limit_per_anchor")]
        limit_per_anchor: Option<usize>,
        /// Walk the relation level by level, keeping this many results per anchor at each depth —
        /// `[10, 5, 3]` is "ten replies, five under each of those, three under each of *those*".
        ///
        /// The walk happens here rather than in the caller, and that is the whole point. A caller
        /// driving it pays a network round trip per level, and a client that renders as each answer
        /// arrives shows the tree assembling itself a level at a time. In here the levels are
        /// sequential SPARQL against a local store with no serialisation between them, and the
        /// records are hydrated once for the union — so three levels cost one request and one
        /// hydration instead of three of each.
        ///
        /// Like `transitive`, the anchors are excluded from their own result. A walk that reaches
        /// an anchor again — through a cycle, or because one anchor was named below another —
        /// reports it once, at neither place: it is where the walk started, not something the walk
        /// found.
        ///
        /// *Refused* alongside `transitive`, which is the unbounded form of the same walk: a path
        /// expression reaches everything below the anchor and can be told nothing about depth.
        /// Refused alongside `limit_per_anchor` too, which cannot express this on its own — with
        /// one anchor at the top there is one group, so it caps the total rather than the breadth
        /// at each level, and the walk sets its own per-level limit at every depth regardless.
        /// Both are errors rather than a silent resolution in the walk's favour, which would be a
        /// wrong answer wearing the shape of a right one.
        #[serde(default)]
        levels: Option<Vec<usize>>,
    },
}

/// Which way a traversal follows its predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ScopeDirection {
    /// `anchor --predicate--> result`: the anchor owns the link. The default,
    /// and the only direction the other `Scope` variants have ever had.
    #[default]
    Out,
    /// `result --predicate--> anchor`: the result owns the link and points back.
    ///
    /// Answers "what points at this" for a *search* — ordered, filtered,
    /// limited. Distinct from a reverse `include`, which answers the same
    /// question for rows already in hand and cannot narrow them.
    In,
}

/// Accept `ids` as either a bare string or a list of them.
///
/// The single-anchor spelling is the common one (one branch of a thread, one
/// node of a graph) and requiring `["x"]` for it would be a papercut at every
/// call site.
fn deserialize_ids_flex<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let value = Value::deserialize(deserializer)?;
    match value {
        Value::String(s) => Ok(vec![s]),
        Value::Array(items) => items
            .into_iter()
            .map(|item| match item {
                Value::String(s) => Ok(s),
                other => Err(serde::de::Error::custom(format!(
                    "scope ids must be strings, got {other}"
                ))),
            })
            .collect(),
        other => Err(serde::de::Error::custom(format!(
            "scope ids must be a string or a list of strings, got {other}"
        ))),
    }
}

/// Value in the `include` map for eager-loading relations.
///
/// - `Bool(true)` — include with default sub-query
/// - `SubQuery(...)` — include with a custom nested query (supports where,
///   order, limit, and further nested includes)
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum IncludeValue {
    Bool(bool),
    SubQuery(Box<ModelQueryInput>),
}

/// Configuration for a single projection key (mirrors TS `IncludeProjection`).
///
/// Projections are lightweight aggregations that begin with `$` in the query
/// object.  They compute either a count or a filtered list of related IRIs
/// using a single grouped SPARQL query per key.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProjectionInput {
    /// The relation name on the parent model to project over.
    pub from: String,
    /// When true, attach a count (integer) instead of a list.
    #[serde(default)]
    pub count: bool,
    /// Count (or list) everything reachable through `from`, not just one step.
    ///
    /// What "42 replies" on a collapsed branch means: people read that as the
    /// whole conversation below it, and a direct-child count says 3. The
    /// projection query is already grouped per parent and already asked of
    /// every row at once, so this costs one token in the emitted path and no
    /// extra round trip.
    ///
    /// Refused alongside a link-author or link-timestamp filter: those read the
    /// reification of *one* link, and a path is a reachability test with no
    /// single link to point at.
    #[serde(default)]
    pub transitive: bool,
    /// Bare class name of the projection target.  The executor resolves the
    /// target shape from this through its in-memory cache when projection
    /// where-clauses reference target properties by name.
    #[serde(default)]
    pub target_class_name: Option<String>,
    /// Optional where filter applied against target instance properties.
    #[serde(default, rename = "where")]
    pub where_clause: Option<BTreeMap<String, WhereCondition>>,
    /// Limit the number of linked results (when 1, value is unwrapped to scalar).
    pub limit: Option<usize>,
    /// Order results before limiting.
    #[serde(default, deserialize_with = "deserialize_order_flex")]
    pub order: Option<Vec<(String, OrderDirection)>>,
}

/// The structured query input (mirrors the TS `Query<T>` type).
///
/// This is the top-level request object deserialized from the JSON that
/// the TypeScript client sends.  It supports filtering (`where`), sorting
/// (`order`), pagination (`limit`/`offset`), eager-loading (`include`),
/// projections, and property selection.
#[derive(Debug, Clone, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ModelQueryInput {
    #[serde(default)]
    pub parent: Option<Scope>,
    #[serde(default)]
    pub properties: Option<Vec<String>>,
    #[serde(default)]
    pub include: Option<HashMap<String, IncludeValue>>,
    /// Projection keys (begin with `$`): lightweight aggregations/lists that
    /// are computed Rust-side with a single grouped SPARQL per key.
    #[serde(default)]
    pub projections: Option<HashMap<String, ProjectionInput>>,
    #[serde(default, rename = "where")]
    pub where_clause: Option<BTreeMap<String, WhereCondition>>,
    #[serde(default, deserialize_with = "deserialize_order_flex")]
    pub order: Option<Vec<(String, OrderDirection)>>,
    #[serde(default)]
    pub offset: Option<usize>,
    #[serde(default)]
    pub limit: Option<usize>,
    #[serde(default)]
    pub count: Option<bool>,
    /// When true, evaluate **property** getters (@Property with `getter`) during
    /// hydration. Relation conformance getters always run regardless.
    /// Defaults to true — property getters are evaluated post-pagination via
    /// batched VALUES queries (O(M) cost).  Set to false to skip them.
    #[serde(default, rename = "deepQuery")]
    pub deep_query: Option<bool>,
    /// Hydrate this relation's targets as the class each one actually *is*,
    /// rather than as the class the relation declares.
    ///
    /// Only meaningful on an `include` sub-query. A heterogeneous relation —
    /// `CollectionBlock.children`, a reified edge's endpoints — either declares
    /// a base class, in which case every subclass property is dropped on
    /// hydration, or declares nothing, in which case the include cannot resolve
    /// a shape at all.
    #[serde(default)]
    pub polymorphic: Option<bool>,
    /// Classes the caller would rather have, most wanted first.
    ///
    /// Membership is not exclusive, so a target can satisfy several classes and
    /// hydration has to read it through one of them. Without this the choice is
    /// made by specificity — how many required triples each class matched — which
    /// answers "which class demanded most of this node", a question nobody asked.
    /// Naming classes here answers the one they did: read it as this, if it is
    /// one.
    ///
    /// **Ranks, never excludes.** Classification is unaffected: a target is still
    /// tested against every registered class, and this only reorders the ones it
    /// matched. So a target conforming to nothing named here still arrives,
    /// hydrated against whichever class it *does* match, chosen by specificity as
    /// it would have been anyway. The relation is heterogeneous by definition and
    /// a caller listing what it can use must not thereby narrow what the
    /// collection contains.
    ///
    /// (A target matching no registered class at all is a different case, and is
    /// skipped — there is no shape to read it through.)
    ///
    /// Only meaningful alongside `polymorphic`.
    #[serde(default)]
    pub prefer_classes: Option<Vec<String>>,
    /// Read the instance as it exists in links of this status only (#1116).
    ///
    /// `Some(Shared)` hydrates from Shared links only, which is what a
    /// multi-user read needs (#1024): a Local link is executor-private and must
    /// not reach another user. `Some(Local)` is the converse. The restriction
    /// applies to every predicate, not only those a class declares `local`.
    /// `None` (the default) reads both, subject to the class's `local` flags.
    /// Combines with `include_unverified` on the same link: a link is read
    /// only when it has the status and verified, or the query opted in.
    ///
    /// See [`link_status_filter`](super::sparql_builder::link_status_filter).
    #[serde(default)]
    pub link_status: Option<crate::types::LinkStatus>,
    /// Hydrate from links whose signature did not verify as well.
    ///
    /// By default a row whose link does not carry a stored `proofValid` of
    /// `"true"` is withheld (see
    /// [`proof_valid_filter`](super::sparql_builder::proof_valid_filter)). This
    /// is the opt-out for a caller that wants to *display* an unverified claim —
    /// a UI marking a value "unverified". Anything that acts on the data (a vote
    /// counter, a role check) must leave it off.
    #[serde(default)]
    pub include_unverified: Option<bool>,
    /// Return the individual links behind these properties, relations or
    /// predicate IRIs under the additive `__links` key — see
    /// [`super::links`]. Reaches predicates the shape does not declare
    /// (annotation links such as revocation tombstones), and dates and
    /// attributes each link separately rather than the instance as a whole.
    #[serde(default)]
    pub links: Option<Vec<String>>,
}

/// Result returned by the model query endpoint.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelQueryResult {
    pub instances: Vec<Value>,
    pub total_count: usize,
}

/// The `where: { producedByFlow: { flow, state? } }` filter, extracted from
/// a query before the pipeline sees it.
///
/// It is not a property filter: whether an instance is a valid output of a
/// flow is decided by signature verification over carried receipts
/// (`flow_instance::produced`), which SPARQL cannot express and
/// `matches_where` has no store to answer. So the key is **removed** from
/// the where clause here and resolved by the caller
/// (`PerspectiveInstance::model_query`) into a concrete id set *before* the
/// query runs — which is what makes it apply before pagination: the ids go
/// into the store as a `VALUES ?source` constraint, so `limit` counts only
/// instances that passed (the same ordering guarantee `limitPerAnchor`
/// gives its slice).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProducedByFlowFilter {
    /// The flow's URI, as the perspective's catalogue keys it.
    pub flow: String,
    /// Only runs settled into this terminal state; absent = any.
    pub state: Option<String>,
}

/// Remove and parse the top-level `producedByFlow` where key, if present.
///
/// **Malformed is an error, never a skip.** A filter that silently dropped
/// would return every instance — the exact opposite of a fail-closed
/// verification filter. The same reasoning refuses the key anywhere it
/// cannot act: nested in `OR`/`AND`/`NOT` branches, under a `some`/`none`
/// quantifier, or inside an include/projection sub-query. Those positions
/// would need per-branch verification semantics nobody has defined; a query
/// naming them gets an error instead of an answer that looks filtered and
/// is not.
pub fn take_produced_by_flow(
    input: &mut ModelQueryInput,
) -> Result<Option<ProducedByFlowFilter>, String> {
    const KEY: &str = "producedByFlow";

    let filter = match input.where_clause.as_mut().and_then(|wc| wc.remove(KEY)) {
        None => None,
        Some(WhereCondition::SubClause(map)) => {
            let mut flow = None;
            let mut state = None;
            for (k, v) in &map {
                match (k.as_str(), v) {
                    ("flow", WhereCondition::String(s)) => flow = Some(s.clone()),
                    ("state", WhereCondition::String(s)) => state = Some(s.clone()),
                    _ => {
                        return Err(format!(
                            "`{KEY}` takes `{{ flow, state? }}` with string values; `{k}` is \
                             not part of it"
                        ))
                    }
                }
            }
            let flow = flow.ok_or_else(|| {
                format!("`{KEY}` needs a `flow`: which flow's outputs is the question")
            })?;
            Some(ProducedByFlowFilter { flow, state })
        }
        Some(other) => {
            return Err(format!(
                "`{KEY}` must be an object `{{ flow, state? }}`, got {other:?}"
            ))
        }
    };

    // The key must not survive anywhere the resolver cannot see it.
    if let Some(position) = find_stray_produced_by_flow(input) {
        return Err(format!(
            "`{KEY}` is only supported as a top-level where key on the queried class \
             (found {position}); it resolves to a verified id set and has no per-branch \
             semantics"
        ));
    }
    Ok(filter)
}

/// Where, other than the (already-extracted) top level, does a
/// `producedByFlow` key still sit? `None` when the query is clean.
fn find_stray_produced_by_flow(input: &ModelQueryInput) -> Option<String> {
    fn in_clause(wc: &BTreeMap<String, WhereCondition>, top: bool) -> Option<String> {
        for (key, cond) in wc {
            if key == "producedByFlow" && !top {
                return Some("nested in a where branch".to_string());
            }
            match cond {
                WhereCondition::SubClause(map) => {
                    if let Some(hit) = in_clause(map, false) {
                        return Some(hit);
                    }
                }
                WhereCondition::SubClauses(branches) => {
                    for branch in branches {
                        if let Some(hit) = in_clause(branch, false) {
                            return Some(hit);
                        }
                    }
                }
                WhereCondition::Ops(ops) => {
                    for nested in [&ops.some, &ops.none].into_iter().flatten() {
                        if let Some(hit) = in_clause(nested, false) {
                            return Some(hit);
                        }
                    }
                }
                _ => {}
            }
        }
        None
    }

    if let Some(wc) = &input.where_clause {
        // The caller extracted the top-level key before scanning; one still
        // here at the top means the caller never extracted (include
        // sub-queries), and that position cannot act either.
        if let Some(hit) = in_clause(wc, true) {
            return Some(hit);
        }
    }
    if let Some(includes) = &input.include {
        for value in includes.values() {
            if let IncludeValue::SubQuery(sub) = value {
                if sub
                    .where_clause
                    .as_ref()
                    .is_some_and(|wc| wc.contains_key("producedByFlow"))
                {
                    return Some("in an include sub-query".to_string());
                }
                if let Some(hit) = find_stray_produced_by_flow(sub) {
                    return Some(hit);
                }
            }
        }
    }
    if let Some(projections) = &input.projections {
        for proj in projections.values() {
            if let Some(wc) = &proj.where_clause {
                if wc.contains_key("producedByFlow") {
                    return Some("in a projection where clause".to_string());
                }
                if let Some(hit) = in_clause(wc, false) {
                    return Some(hit);
                }
            }
        }
    }
    None
}

/// Constrain the query to `allowed` ids, intersecting with any `id`
/// condition the caller already wrote. Returns `false` when the resulting
/// set is empty — the caller should answer with no instances rather than
/// send the store an empty `VALUES`.
///
/// An existing `id` condition that is not a plain string or string array is
/// refused: intersecting a verified id set with operator or combinator
/// semantics has no defined meaning, and approximating one would return an
/// answer to a question the caller did not ask.
pub fn constrain_ids(
    input: &mut ModelQueryInput,
    allowed: std::collections::BTreeSet<String>,
) -> Result<bool, String> {
    let wc = input.where_clause.get_or_insert_with(BTreeMap::new);
    let intersected: Vec<String> = match wc.get("id") {
        None => allowed.into_iter().collect(),
        Some(WhereCondition::String(one)) => allowed.into_iter().filter(|id| id == one).collect(),
        Some(WhereCondition::StringArray(several)) => allowed
            .into_iter()
            .filter(|id| several.contains(id))
            .collect(),
        Some(other) => {
            return Err(format!(
                "`producedByFlow` cannot be combined with this `id` condition ({other:?}); \
                 use a plain id or an id list"
            ))
        }
    };
    let any = !intersected.is_empty();
    wc.insert("id".to_string(), WhereCondition::StringArray(intersected));
    Ok(any)
}

/// Parameters for SPARQL-side pagination (pushed ORDER BY + LIMIT + OFFSET).
pub(super) struct SparqlPagination {
    pub(super) sort_key: SortKey,
    pub(super) direction: OrderDirection,
    pub(super) offset: Option<usize>,
    pub(super) limit: Option<usize>,
}

/// What to sort by when pagination is pushed to SPARQL.
pub(super) enum SortKey {
    /// Sort by reifier timestamp (MIN(?_ts) per source).
    Timestamp,
    /// Sort by a property value extracted from its literal IRI.
    Property(String), // predicate IRI
    /// Sort by a projection count — uses `COUNT(DISTINCT ?_proj_t)` in the
    /// GROUP BY pagination subquery.  The string is the relation predicate IRI
    /// used to reach the counted items.
    Projection(String), // relation predicate IRI
    /// Sort by a scalar property on a directly-related model instance.
    /// Emits a double-OPTIONAL join in the pagination subquery:
    /// `?source <rel_pred> ?_rel . ?_rel <prop_pred> ?_sort_val`.
    RelationProperty { rel_pred: String, prop_pred: String },
}

// ---------------------------------------------------------------------------
// Internal shape metadata (derived from SHACL links or client JSON)
// ---------------------------------------------------------------------------

/// Transform expression for property values (SHACL-AF Node Expression).
/// Applied in the Rust executor during hydration for resolveLanguage properties.
#[derive(Debug, Clone, Deserialize, PartialEq)]
#[serde(tag = "type", rename_all = "camelCase")]
pub(crate) enum TransformExpression {
    Focus,
    Literal {
        value: serde_json::Value,
    },
    Path {
        predicate: String,
    },
    Exists {
        expr: Box<TransformExpression>,
    },
    If {
        cond: Box<TransformExpression>,
        then: Box<TransformExpression>,
        #[serde(rename = "else")]
        else_expr: Option<Box<TransformExpression>>,
    },
    Concat {
        args: Vec<TransformExpression>,
    },
    Coalesce {
        args: Vec<TransformExpression>,
    },
    Function {
        iri: String,
        args: Vec<TransformExpression>,
    },
}

/// Default file decode transform: converts FileData objects to data: URIs.
///
/// Represents: `if (exists(path('data_base64'))) then concat(...) else focus()`
pub(crate) fn default_file_decode() -> TransformExpression {
    TransformExpression::If {
        cond: Box::new(TransformExpression::Exists {
            expr: Box::new(TransformExpression::Path {
                predicate: "data_base64".to_string(),
            }),
        }),
        then: Box::new(TransformExpression::Concat {
            args: vec![
                TransformExpression::Literal {
                    value: Value::String("data:".to_string()),
                },
                TransformExpression::Coalesce {
                    args: vec![
                        TransformExpression::Path {
                            predicate: "file_type".to_string(),
                        },
                        TransformExpression::Literal {
                            value: Value::String("image/png".to_string()),
                        },
                    ],
                },
                TransformExpression::Literal {
                    value: Value::String(";base64,".to_string()),
                },
                TransformExpression::Path {
                    predicate: "data_base64".to_string(),
                },
            ],
        }),
        else_expr: Some(Box::new(TransformExpression::Focus)),
    }
}

/// A single property or relation declared in a model class's shape.
///
/// Constructed by reading SHACL triples from the store
/// ([`super::shape::load_shape`]).
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ShapeProperty {
    pub(crate) name: String,
    pub(crate) predicate: String,
    pub(crate) is_collection: bool,
    pub(crate) is_flag: bool,
    pub(crate) is_required: bool,
    pub(crate) initial_value: Option<String>,
    /// Language address used to resolve property values, and the sole
    /// selector of storage mode:
    ///   - `None`              → deterministic typed literal (POS-index
    ///                           fast path — the default for a plain
    ///                           `@Property()`).
    ///   - `Some("literal")`   → signed-envelope on the built-in literal
    ///                           language (`expression_create` produces a
    ///                           `{author, timestamp, data, proof}` URI).
    ///   - `Some(<addr>)`      → `expression_create` on that custom
    ///                           language.
    pub(crate) resolve_language: Option<String>,
    pub(crate) datatype: Option<String>,
    pub(crate) direction: Option<String>, // "forward" or "reverse" for relation properties
    pub(crate) is_scalar_relation: bool, // true for hasOne/belongsToOne (render as scalar, not array)
    /// SPARQL getter expression (e.g. `SELECT ?value WHERE { ... }` or `ASK WHERE { ... }`).
    /// For properties: returns a scalar value.
    /// For relations: returns target IDs (conformance-filtered).
    pub(crate) getter: Option<String>,
    /// Post-getter where-clause filter for relations.  Used to apply
    /// where conditions on related instances after the getter runs,
    /// by fetching the target property values and comparing the parsed data.
    pub(crate) where_filter: Option<BTreeMap<String, WhereCondition>>,
    /// Predicate mappings for `where_filter` (property name → predicate IRI).
    pub(crate) where_predicates: Option<HashMap<String, String>>,
    /// Transform expression (SHACL-AF Node Expression).
    /// Applied in hydration for resolveLanguage properties.
    pub(super) transform: Option<TransformExpression>,
    /// Natural-language hint describing this property's meaning, read back from
    /// the `ad4m://interpretation_hint` link on the property node.  Surfaced so the
    /// generic LLM extractor (and MCP tool-schema generation) can inject it as
    /// semantic guidance.  `None` when the SDNA declared no hint.
    pub(crate) interpretation_hint: Option<String>,
    /// CRDT ordering strategy for this collection, from `ad4m://ordering` on the
    /// property shape. `None` leaves the relation an unordered set sorted by
    /// link timestamp, which is what every existing relation is.
    pub(crate) ordering: Option<String>,
    /// Whether this property is the class's dedup identity (its title-like
    /// interpretation key), read back from the `ad4m://identity` link on the
    /// property node.  `false` when the SDNA declared no identity — a class
    /// with no identity property is never deduplicated.
    pub(crate) identity: bool,
    /// Whether this property's links are written with `LinkStatus::Local`,
    /// read back from the `ad4m://local` link on the property node.  Local
    /// links stay in the executor's own store: they are never gossiped to the
    /// neighbourhood, so remote agents cannot read this property at all and
    /// its values do not survive a re-sync from the network.  `false` is the
    /// default (shared) for every property that does not declare it.
    ///
    /// Surfaced through `describe_perspective` so an agent can tell which
    /// properties of a class are executor-private before writing to them.
    pub(crate) local: bool,
}

impl ShapeProperty {
    /// True when the property's values are stored as deterministic typed
    /// literals (POS-index friendly) rather than signed expression
    /// envelopes or custom-language expressions. Derived from
    /// `resolve_language` alone:
    ///   - `None`             → deterministic (default fast path)
    ///   - `Some("literal")`  → envelope (per-value provenance)
    ///   - `Some(<other>)`    → custom-language expression (never
    ///                          deterministic)
    pub(crate) fn is_deterministic_literal(&self) -> bool {
        self.resolve_language.is_none()
    }
}

/// Enriched relation metadata for include (eager-loading) resolution.
/// Target shapes are resolved at recursion time through a [`ShapeResolver`]
/// against the perspective's in-memory shape cache.
#[derive(Debug, Clone)]
pub struct ShapeRelation {
    pub(crate) name: String,
    pub(crate) predicate: String,
    pub(crate) direction: String, // "forward" or "reverse"
    pub(crate) kind: String,      // "hasMany", "hasOne", "belongsToOne", "belongsToMany"
    pub(crate) max_count: Option<usize>,
    pub(crate) target_class_name: String,
    /// Original `sh:class` URI as emitted by the SHACL writer.  Preserved
    /// alongside `target_class_name` so the MCP layer (which exposes the
    /// node-shape URI to clients) can return the full IRI rather than the
    /// bare class name.  Empty when no `sh:class` was set on the property.
    pub(crate) target_class_uri: String,
}

/// Complete shape of a model class — the set of all properties, relations,
/// and include metadata needed to query, hydrate, and enrich instances.
///
/// This is the central metadata object threaded through the entire query
/// pipeline.  Built once from SHACL triples in the perspective's store and
/// memoized in `PerspectiveInstance::shape_cache`.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ModelShape {
    pub(crate) target_class: String,
    #[allow(dead_code)]
    pub(crate) shape_uri: String,
    pub(crate) properties: Vec<ShapeProperty>,
    /// Enriched relation metadata for include resolution, populated
    /// directly from the perspective's SHACL triples.
    pub(crate) include_relations: Vec<ShapeRelation>,
    /// Class-level natural-language hint, read back from the
    /// `ad4m://interpretation_hint` link on the shape node.  Steers the generic
    /// LLM extractor toward what instances of this class represent.  `None`
    /// when the SDNA declared no class hint.
    pub(crate) interpretation_hint: Option<String>,
}

impl ModelShape {
    /// Returns all predicate IRIs declared in this shape (properties + relations + flags).
    pub fn predicates(&self) -> Vec<String> {
        let mut preds: Vec<String> = self
            .properties
            .iter()
            .filter(|p| !p.predicate.is_empty())
            .map(|p| p.predicate.clone())
            .collect();
        for r in &self.include_relations {
            if !r.predicate.is_empty() {
                preds.push(r.predicate.clone());
            }
        }
        preds.sort();
        preds.dedup();
        preds
    }
}

/// Resolves shapes by class name.  Implementations memoize results so that
/// repeated lookups (including recursive include traversal across the same
/// class) parse SHACL at most once per process lifetime.
///
/// The concrete production implementation lives on `PerspectiveInstance`
/// and reads from the perspective's `shape_cache` + SHACL store.  Tests
/// typically supply a static implementation pre-populated with `ModelShape`
/// instances.
pub trait ShapeResolver: Send + Sync {
    fn get_shape(
        &self,
        class_name: &str,
    ) -> Result<std::sync::Arc<ModelShape>, deno_core::anyhow::Error>;
}

/// SPARQL execution plan for an instance query.
///
/// For non-paginated queries, a single SPARQL `SELECT` fetches all matching
/// rows.  For paginated queries we use a **two-phase** approach because
/// Oxigraph's query planner doesn't push nested sub-queries with `ORDER BY`
/// + `LIMIT` down efficiently (O(N * total_triples) vs O(page_size) lookups):
///
/// 1. **Phase 1** — A lightweight pagination sub-query retrieves just the
///    source IRIs in sorted/limited order.
/// 2. **Phase 2** — A `VALUES ?source { ... }` property query fetches all
///    triples for those specific instances.
pub(super) enum InstanceQueryPlan {
    /// Single query -- no pagination or non-paginated query.
    Single(String),
    /// Two-phase: (pagination_subquery, predicate_filter, conformance, where_extra).
    /// Phase 1: execute pagination_subquery -> get source IRIs.
    /// Phase 2: build property query with VALUES ?source { ... }.
    TwoPhase {
        pagination_subquery: String,
        predicate_filter: String,
    },
}

impl InstanceQueryPlan {
    /// Extract the SPARQL string for non-paginated queries (Single variant).
    /// Panics for TwoPhase. Used only in unit tests.
    #[cfg(test)]
    pub(super) fn into_single(self) -> String {
        match self {
            InstanceQueryPlan::Single(s) => s,
            InstanceQueryPlan::TwoPhase { .. } => {
                panic!("Expected Single query plan, got TwoPhase")
            }
        }
    }
}

#[cfg(test)]
mod where_condition_deser_tests {
    use super::*;
    use serde_json::from_value;

    #[test]
    fn deser_string() {
        let v: WhereCondition = from_value(serde_json::json!("hello")).unwrap();
        assert!(matches!(v, WhereCondition::String(s) if s == "hello"));
    }

    #[test]
    fn deser_number() {
        let v: WhereCondition = from_value(serde_json::json!(42.0)).unwrap();
        assert!(matches!(v, WhereCondition::Number(n) if (n - 42.0).abs() < f64::EPSILON));
    }

    #[test]
    fn deser_bool() {
        let v: WhereCondition = from_value(serde_json::json!(true)).unwrap();
        assert!(matches!(v, WhereCondition::Bool(true)));
    }

    #[test]
    fn deser_string_array() {
        let v: WhereCondition = from_value(serde_json::json!(["a", "b", "c"])).unwrap();
        assert!(matches!(v, WhereCondition::StringArray(arr) if arr == vec!["a", "b", "c"]));
    }

    #[test]
    fn deser_number_array() {
        let v: WhereCondition = from_value(serde_json::json!([1.0, 2.0, 3.0])).unwrap();
        assert!(matches!(v, WhereCondition::NumberArray(_)));
    }

    #[test]
    fn deser_empty_array_is_string_array() {
        // Empty arrays land on StringArray (first Vec variant), not SubClauses.
        let v: WhereCondition = from_value(serde_json::json!([])).unwrap();
        assert!(matches!(v, WhereCondition::StringArray(arr) if arr.is_empty()));
    }

    #[test]
    fn deser_ops_with_known_fields() {
        let v: WhereCondition = from_value(serde_json::json!({ "gt": 5, "lt": 10 })).unwrap();
        assert!(
            matches!(v, WhereCondition::Ops(ops) if ops.gt == Some(5.0) && ops.lt == Some(10.0))
        );
    }

    #[test]
    fn deser_sub_clauses_for_or_and() {
        // Array of objects → SubClauses (used as OR/AND value)
        let v: WhereCondition =
            from_value(serde_json::json!([{"name": "Alice"}, {"name": "Bob"}])).unwrap();
        match v {
            WhereCondition::SubClauses(branches) => {
                assert_eq!(branches.len(), 2);
                assert!(branches[0].contains_key("name"));
            }
            other => panic!("expected SubClauses, got {other:?}"),
        }
    }

    #[test]
    fn deser_sub_clause_for_not() {
        // Object with non-WhereOps keys → SubClause (used as NOT value)
        let v: WhereCondition =
            from_value(serde_json::json!({"name": "Alice", "status": "active"})).unwrap();
        match v {
            WhereCondition::SubClause(map) => {
                assert_eq!(map.len(), 2);
                assert!(map.contains_key("name"));
                assert!(map.contains_key("status"));
            }
            other => panic!("expected SubClause, got {other:?}"),
        }
    }

    #[test]
    fn deser_where_clause_with_or() {
        // Full where-clause map with OR key
        let json = serde_json::json!({
            "OR": [
                { "name": { "contains": "foo" } },
                { "description": { "contains": "foo" } }
            ]
        });
        let wc: BTreeMap<String, WhereCondition> = serde_json::from_value(json).unwrap();
        assert!(wc.contains_key("OR"));
        let or_val = wc.get("OR").unwrap();
        assert!(matches!(or_val, WhereCondition::SubClauses(b) if b.len() == 2));
    }

    #[test]
    fn deser_where_clause_with_not() {
        let json = serde_json::json!({ "NOT": { "status": "deleted" } });
        let wc: BTreeMap<String, WhereCondition> = serde_json::from_value(json).unwrap();
        assert!(wc.contains_key("NOT"));
        let not_val = wc.get("NOT").unwrap();
        assert!(matches!(not_val, WhereCondition::SubClause(_)));
    }

    #[test]
    fn deser_model_query_with_or_where() {
        let json = serde_json::json!({
            "where": {
                "OR": [
                    { "name": "Alice" },
                    { "name": "Bob" }
                ]
            }
        });
        let mq: ModelQueryInput = serde_json::from_value(json).unwrap();
        let wc = mq.where_clause.unwrap();
        assert!(matches!(wc.get("OR"), Some(WhereCondition::SubClauses(b)) if b.len() == 2));
    }
}

#[cfg(test)]
mod produced_by_flow_filter_tests {
    use super::*;
    use std::collections::BTreeSet;

    fn input(json: serde_json::Value) -> ModelQueryInput {
        serde_json::from_value(json).expect("query parses")
    }

    fn ids(list: &[&str]) -> BTreeSet<String> {
        list.iter().map(|s| s.to_string()).collect()
    }

    /// The filter comes out parsed and the where clause no longer carries the
    /// key — `matches_where` must never see it as a property named
    /// `producedByFlow`, which no instance has and every instance would fail.
    #[test]
    fn the_filter_is_extracted_and_removed_from_the_where_clause() {
        let mut q = input(serde_json::json!({
            "where": {
                "producedByFlow": { "flow": "coasys://DeliveryFlow", "state": "done" },
                "title": "Ship it",
            }
        }));
        let filter = take_produced_by_flow(&mut q)
            .expect("well-formed")
            .expect("present");
        assert_eq!(
            filter,
            ProducedByFlowFilter {
                flow: "coasys://DeliveryFlow".into(),
                state: Some("done".into()),
            }
        );
        let wc = q
            .where_clause
            .as_ref()
            .expect("the rest of the clause stays");
        assert!(!wc.contains_key("producedByFlow"));
        assert!(wc.contains_key("title"), "sibling conditions are untouched");

        let mut none = input(serde_json::json!({ "where": { "title": "x" } }));
        assert_eq!(take_produced_by_flow(&mut none).expect("fine"), None);

        let mut stateless = input(serde_json::json!({
            "where": { "producedByFlow": { "flow": "coasys://DeliveryFlow" } }
        }));
        let filter = take_produced_by_flow(&mut stateless).expect("state is optional");
        assert_eq!(filter.expect("present").state, None);
    }

    /// A verification filter that silently dropped would admit everything.
    /// Malformed shapes are errors, whatever else the query says.
    ///
    /// Red if `take_produced_by_flow` skips a shape it cannot parse.
    #[test]
    fn a_malformed_filter_is_an_error_not_a_skip() {
        for bad in [
            serde_json::json!({ "where": { "producedByFlow": "coasys://DeliveryFlow" } }),
            serde_json::json!({ "where": { "producedByFlow": { "state": "done" } } }),
            serde_json::json!({ "where": { "producedByFlow": { "flow": "f", "extra": "x" } } }),
            serde_json::json!({ "where": { "producedByFlow": { "flow": 42 } } }),
        ] {
            let mut q = input(bad.clone());
            assert!(
                take_produced_by_flow(&mut q).is_err(),
                "must refuse rather than silently admit everything: {bad}"
            );
        }
    }

    /// Nowhere the resolver cannot act may carry the key: nested branches,
    /// quantifiers, include sub-queries and projections all refuse. Each
    /// would otherwise read as an ordinary (never-matching or, worse,
    /// always-matching) property condition.
    #[test]
    fn the_key_is_refused_anywhere_but_the_top_level() {
        for (label, bad) in [
            (
                "OR branch",
                serde_json::json!({ "where": { "OR": [
                    { "producedByFlow": { "flow": "f" } },
                    { "title": "x" },
                ]}}),
            ),
            (
                "some quantifier",
                serde_json::json!({ "where": { "comments": { "some": {
                    "producedByFlow": { "flow": "f" }
                }}}}),
            ),
            (
                "include sub-query",
                serde_json::json!({ "include": { "comments": {
                    "where": { "producedByFlow": { "flow": "f" } }
                }}}),
            ),
            (
                "projection where",
                serde_json::json!({ "projections": { "$n": {
                    "from": "comments",
                    "where": { "producedByFlow": { "flow": "f" } }
                }}}),
            ),
        ] {
            let mut q = input(bad);
            assert!(
                take_produced_by_flow(&mut q).is_err(),
                "{label}: a position the resolver cannot see must refuse"
            );
        }
    }

    /// The resolved id set lands as a store-pushable `id` IN-condition and
    /// intersects with whatever `id` condition the caller already wrote —
    /// both conditions hold, never either alone.
    #[test]
    fn the_allow_set_intersects_an_existing_id_condition() {
        let mut fresh = input(serde_json::json!({ "where": { "title": "x" } }));
        assert!(constrain_ids(&mut fresh, ids(&["a", "b"])).expect("ok"));
        assert!(matches!(
            fresh.where_clause.as_ref().unwrap().get("id"),
            Some(WhereCondition::StringArray(v)) if v == &vec!["a".to_string(), "b".to_string()]
        ));

        let mut narrowed = input(serde_json::json!({ "where": { "id": ["b", "c"] } }));
        assert!(constrain_ids(&mut narrowed, ids(&["a", "b"])).expect("ok"));
        assert!(matches!(
            narrowed.where_clause.as_ref().unwrap().get("id"),
            Some(WhereCondition::StringArray(v)) if v == &vec!["b".to_string()]
        ));

        let mut disjoint = input(serde_json::json!({ "where": { "id": "c" } }));
        assert!(
            !constrain_ids(&mut disjoint, ids(&["a", "b"])).expect("ok"),
            "a disjoint intersection reports empty so the caller answers with no rows"
        );

        let mut weird = input(serde_json::json!({ "where": { "id": { "contains": "a" } } }));
        assert!(
            constrain_ids(&mut weird, ids(&["a"])).is_err(),
            "an id condition with operator semantics has no defined intersection"
        );
    }

    /// An empty allow set must yield `false`, not an empty `VALUES` block
    /// handed to the store.
    #[test]
    fn an_empty_allow_set_short_circuits() {
        let mut q = input(serde_json::json!({}));
        assert!(!constrain_ids(&mut q, BTreeSet::new()).expect("ok"));
    }
}

/// Intermediate representation of all RDF links belonging to one instance.
///
/// Produced by [`super::hydration::group_results_by_source`] from raw SPARQL
/// result rows, then consumed by [`super::hydration::hydrate_one`] to build
/// a fully typed JSON object.
#[derive(Debug)]
pub(super) struct InstanceLinks {
    pub(super) source: String,
    /// (predicate, target, author, timestamp) for each link
    pub(super) links: Vec<(String, String, String, String)>,
}
