//! Generic LLM extraction: turn conversation text into typed subject-class
//! instances, steered by the natural-language `extraction_hint` declared on each
//! class/property.
//!
//! Data flow of one extraction run (`run_extraction` wires it end to end):
//!   1. Read the titles already present per class (`existing_instance_titles`)
//!      so the model can be steered away from re-proposing known items.
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

/// One instance the LLM proposes creating: the target class name plus a flat
/// map of field-name -> value. Extra/unknown fields are tolerated (kept in
/// `props`); `instance_links` filters them against the class shape.
#[derive(Debug, Clone, Deserialize, PartialEq)]
pub struct ProposedInstance {
    pub class: String,
    #[serde(flatten)]
    pub props: HashMap<String, serde_json::Value>,
}

#[cfg(test)]
mod tests;
