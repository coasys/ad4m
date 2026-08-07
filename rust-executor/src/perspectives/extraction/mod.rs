//! Generic LLM extraction: turn conversation text into typed subject-class
//! instances, steered by the natural-language `extraction_hint` declared on each
//! class/property.
//!
//! Data flow of one extraction run (`run_extraction` wires it end to end):
//!   1. Snapshot the existing instances per class (`existing_instance_context`
//!      returns `{id, title, class}` rows) so the model can either skip
//!      known items or upsert them via their `id`.
//!   2. Render those, plus each class's `extraction_hint` + fields and the
//!      transcript, into a single prompt (`build_extraction_input`).
//!   3. Ask the configured LLM (`AIService::prompt` on the task registered by
//!      `ensure_extraction_task`), retrying the parse a few times because local
//!      models emit half-valid JSON (`retry_extraction_parse` +
//!      `parse_extraction_response` -> `ProposedInstance`s).
//!   4. Drop anything already present (`filter_already_present`), then turn each
//!      surviving instance into shape-driven perspective links anchored at a
//!      freshly-minted base URI (`place_instances` -> `instance_links`, scalar
//!      values encoded via `value_to_literal_uri`).
//!   5. Write them all in one perspective diff (`add_links`).
//!
//! `apply_extraction_raw` exposes the pure parse+link steps (no LLM, no store)
//! for callers and tests. Those pure/DB-only units are unit-tested in-file; the
//! real-LLM end-to-end suite lives in `extraction_e2e.rs`, with shared
//! fixtures/harness in `extraction_test_support.rs`.

use serde::Deserialize;
use std::collections::HashMap;

mod graph;
mod parse;
mod prompt;
mod run;

pub use graph::*;
pub use parse::*;
pub use prompt::*;
pub use run::*;

/// One instance the LLM proposes. Normally a **create**: the target class name
/// plus a flat map of field-name -> value. When `id` is present it is an
/// **upsert/update** — the base URI of an existing instance whose scalar fields
/// should be patched instead of minting a new node (this is how the extractor
/// grows/refines an existing tree node à la Flux "grouping"). Extra/unknown
/// fields are tolerated (kept in `props`); `instance_links` filters them against
/// the class shape. `id` is a reserved field name for this reason.
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct ProposedInstance {
    pub class: String,
    /// Present only for updates: the existing instance's base URI to patch.
    #[serde(default)]
    pub id: Option<String>,
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}

#[cfg(test)]
mod tests;
