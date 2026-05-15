mod filtering;
mod getters;
mod hydration;
#[cfg(test)]
mod integration_tests;
mod projection;
mod query;
mod relations;
mod shape;
mod sparql_builder;
#[cfg(test)]
mod test_helpers;
pub(crate) mod types;
mod utils;

// Public re-exports: external consumers (perspective_instance.rs) use these paths.
pub use getters::evaluate_getters_batch;
pub use query::execute_model_query;
pub use relations::resolve_reverse_relations;
pub(crate) use shape::load_shape_from_store;
pub use types::{
    IncludeValue, ModelQueryInput, ModelQueryResult, OrderDirection, ParentScope, ProjectionInput,
    WhereCondition, WhereOps,
};
