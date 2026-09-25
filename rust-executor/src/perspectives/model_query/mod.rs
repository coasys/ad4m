//! Structured model queries over the AD4M perspective graph.
//!
//! This module implements the Rust-side query engine for AD4M's `@Subject`-class
//! model system.  TypeScript client code declares model classes with decorators
//! (`@Property`, `@HasMany`, `@Flag`, etc.) that map to RDF triples stored in an
//! Oxigraph SPARQL 1.2 store.  When a client calls `Model.query(...)`, the
//! request is serialised as a [`ModelQueryInput`] and executed here.
//!
//! # Query pipeline
//!
//! [`query::execute_model_query`] orchestrates the full pipeline:
//!
//! ```text
//! ┌─────────────┐     ┌──────────────────┐     ┌────────────┐
//! │  1. Shape    │────▶│  2. SPARQL build  │────▶│  3. Store  │
//! │  resolution  │     │  (conformance +   │     │  execution │
//! │  (shape.rs)  │     │   where clauses)  │     │            │
//! └─────────────┘     │  (sparql_builder)  │     └─────┬──────┘
//!                      └──────────────────┘           │
//!                              ┌──────────────────────┘
//!                              ▼
//! ┌─────────────┐     ┌──────────────────┐     ┌────────────────┐
//! │ 4. Hydration │────▶│  5. Filtering &  │────▶│  6. Getters &  │
//! │  (raw rows → │     │     sorting      │     │    relations   │
//! │   instances) │     │  (filtering.rs)  │     │  (getters.rs,  │
//! │ (hydration)  │     └──────────────────┘     │  relations.rs) │
//! └─────────────┘                               └───────┬────────┘
//!                                                       │
//!                              ┌────────────────────────┘
//!                              ▼
//!                      ┌──────────────────┐
//!                      │  7. Projections   │
//!                      │  (projection.rs)  │
//!                      └──────────────────┘
//! ```
//!
//! 1. **Shape resolution** ([`shape`]) — Load the model's SHACL shape from
//!    the store, or parse the JSON metadata sent by the TS client.  The shape
//!    describes properties, predicates, relations, flags, and getters.
//!
//! 2. **SPARQL construction** ([`sparql_builder`]) — Build conformance patterns
//!    (which triples make an instance "conform" to the model class) and
//!    where-clause filters.  Supports a two-phase pagination plan for large
//!    result sets.
//!
//! 3. **Store execution** — The generated SPARQL is run against the Oxigraph
//!    [`SparqlStore`](crate::perspectives::sparql_store::SparqlStore), returning
//!    `?source ?predicate ?target ?author ?timestamp` rows.
//!
//! 4. **Hydration** ([`hydration`]) — Raw SPARQL rows are grouped by `?source`
//!    (instance ID) and assembled into JSON objects with typed property values
//!    (using `literal:` URI parsing), collection arrays, timestamps, and author.
//!
//! 5. **Filtering & sorting** ([`filtering`]) — Where-clause conditions that
//!    could not be pushed into SPARQL (e.g. complex `Ops` on getter-computed
//!    properties) are evaluated in Rust.  Instances are then sorted according
//!    to the query's `order` specification.
//!
//! 6. **Getters & relations** ([`getters`], [`relations`]) — SPARQL getter
//!    expressions (`ASK` / `SELECT`) defined on properties or relations are
//!    evaluated in batched `VALUES` queries.  Reverse relations
//!    (`@BelongsTo`) and recursive eager-loading (`include`) are resolved.
//!
//! 7. **Projections** ([`projection`]) — Lightweight aggregations (count or
//!    list) over relation edges, computed with a single grouped SPARQL per
//!    projection key.
//!
//! # Sub-modules
//!
//! | Module | Responsibility |
//! |--------|---------------|
//! | [`types`] | All data structures: query DSL input types, shape metadata, internal query plans |
//! | [`utils`] | SPARQL injection prevention, `literal:` URI parsing, IRI validation |
//! | [`shape`] | Loading model shapes from the SHACL store or from client-provided JSON |
//! | [`sparql_builder`] | Generating SPARQL query strings (conformance, where-clauses, pagination) |
//! | [`hydration`] | Converting raw SPARQL result rows into hydrated JSON instances |
//! | [`filtering`] | Post-hydration where-clause matching and multi-key sorting |
//! | [`link_author`] | `where` `author`: nested per link (`{ p: { eq, author } }`), bare per instance, side by side both |
//! | [`getters`] | Evaluating `ASK`/`SELECT` getter expressions in batched queries |
//! | [`relations`] | Resolving reverse relations and recursive `include` eager-loading |
//! | [`projection`] | Computing projection aggregations (counts and filtered lists) |
//! | [`links`] | Per-link rows on request (`links` → `__links`), including undeclared predicates |
//! | [`query`] | Top-level orchestrator that wires the whole pipeline together |

#[cfg(test)]
mod collection_provenance_tests;
mod eval_transform;
mod filtering;
mod getters;
mod hydration;
#[cfg(test)]
mod integration_tests;
mod link_author;
#[cfg(test)]
mod link_author_tests;
#[cfg(test)]
mod link_status_relation_tests;
#[cfg(test)]
mod link_status_tests;
mod links;
mod projection;
#[cfg(test)]
mod proof_valid_tests;
mod query;
mod relations;
#[cfg(test)]
mod round_trip_tests;
#[cfg(test)]
mod selection_guard_tests;
pub(crate) mod shape;
mod sparql_builder;
#[cfg(test)]
mod test_helpers;
pub(crate) mod types;
pub(crate) mod utils;

/// The per-value condition evaluator, shared with the flow engine so a
/// `fromRole` rule's `author` condition means the same thing whether it is
/// applied to a grant row here or to a revocation tombstone there.
pub(crate) use filtering::matches_condition;
pub use getters::evaluate_getters_batch;
pub(crate) use links::links_trigger_predicates;
pub use query::execute_model_query;
pub use relations::resolve_reverse_relations;
pub(crate) use shape::load_shape_from_store;
pub use types::{
    constrain_ids, take_produced_by_flow, IncludeValue, ModelQueryInput, ModelQueryResult,
    ModelShape, OrderDirection, ProducedByFlowFilter, ProjectionInput, Scope, ShapeResolver,
    WhereCondition, WhereOps,
};
/// Re-export the shared IRI-safety predicate so write-side callers
/// (e.g. `perspective_instance::resolve_property_value`,
/// `mcp::tools::create_property_expression`) can gate raw-`NamedNode`
/// storage on the same rule the SPARQL query builder uses.  See
/// [`utils::is_safe_iri_target`] for the full contract.
pub use utils::is_safe_iri_target;
