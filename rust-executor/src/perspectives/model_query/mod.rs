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
//! | [`getters`] | Evaluating `ASK`/`SELECT` getter expressions in batched queries |
//! | [`relations`] | Resolving reverse relations and recursive `include` eager-loading |
//! | [`projection`] | Computing projection aggregations (counts and filtered lists) |
//! | [`query`] | Top-level orchestrator that wires the whole pipeline together |

mod filtering;
mod getters;
mod hydration;
#[cfg(test)]
mod integration_tests;
mod projection;
mod query;
mod relations;
#[cfg(test)]
mod round_trip_tests;
mod shape;
mod sparql_builder;
#[cfg(test)]
mod test_helpers;
pub(crate) mod types;
mod utils;

pub use getters::evaluate_getters_batch;
pub use query::execute_model_query;
pub use relations::resolve_reverse_relations;
pub(crate) use shape::load_shape_from_store;
pub use types::{
    IncludeValue, ModelQueryInput, ModelQueryResult, ModelShape, OrderDirection, ParentScope,
    ProjectionInput, ShapeResolver, WhereCondition, WhereOps,
};
