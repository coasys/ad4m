//! `crate::perspectives::interpretation` on wasm32: no LLM interpretation in the web
//! executor.
//!
//! Real: `types` (the native `interpretation/types.rs` by path), and two pure helpers that
//! ring 2 uses outside interpretation, copied from native (`class_local_name` from
//! `graph/mod.rs`, `decode_literal_string` from `graph/read.rs`); keep them the same.
//! Stubbed: every interpretation run, transcript read and overlay call is an error
//! ([`NotAvailable`](crate::web::stubs::NotAvailable)).

#[path = "../../perspectives/interpretation/types.rs"]
mod types;
pub use types::*;

use crate::agent::AgentContext;
use crate::perspectives::model_query::types::{ModelShape, Scope};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use std::collections::HashSet;
use std::sync::Arc;

const WHAT: &str = "AI interpretation";

fn unavailable() -> anyhow::Error {
    crate::web::stubs::not_available(WHAT)
}

/// Copy of native `graph::class_local_name`.
pub(crate) fn class_local_name(target_class: &str) -> &str {
    target_class
        .rsplit(|c| c == '/' || c == ':' || c == '#')
        .find(|seg| !seg.is_empty())
        .unwrap_or(target_class)
}

/// Copy of native `graph::read::decode_literal_string`.
pub(crate) fn decode_literal_string(uri: &str) -> Option<String> {
    use ad4m_literal::{Literal, LiteralValue};
    match Literal::from_url(uri.to_string()).ok()?.get().ok()? {
        LiteralValue::String(s) => Some(s),
        _ => None,
    }
}

#[derive(Debug, Clone, Default)]
pub enum DedupStrategy {
    #[default]
    NormalizedString,
    Semantic {
        model: String,
        threshold: f32,
    },
}

pub struct InterpretationRunCursor {
    pub processor: String,
    pub sources: Vec<String>,
}

pub(crate) struct SliceScopeLink<'a> {
    pub mint_scope: &'a Scope,
    pub pre_existing: &'a HashSet<String>,
    pub processor_id: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterpretationDebug {
    pub prompt: String,
    pub response: String,
}

#[derive(Debug, Clone)]
pub struct InterpretationOutcome {
    pub bases: Vec<String>,
    pub debug: Option<InterpretationDebug>,
    pub flow_proposals: Vec<String>,
}

pub async fn gather_transcript_sparql(
    _perspective: &PerspectiveInstance,
    _sparql: &str,
) -> anyhow::Result<Vec<TranscriptTurn>> {
    Err(unavailable())
}

pub async fn existing_instance_context(
    _perspective: &PerspectiveInstance,
    _shapes: &[ModelShape],
    _scope: Option<&Scope>,
) -> anyhow::Result<ExistingInstances> {
    Err(unavailable())
}

#[allow(clippy::too_many_arguments)]
pub(crate) async fn run_interpretation_observed_linked(
    _perspective: &mut PerspectiveInstance,
    _shapes: &[ModelShape],
    _transcript: &[TranscriptTurn],
    _base_prefix: &str,
    _context: &AgentContext,
    _scope: Option<&Scope>,
    _emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
    _slice_link: Option<&SliceScopeLink<'_>>,
) -> anyhow::Result<Vec<String>> {
    Err(unavailable())
}

#[allow(clippy::too_many_arguments)]
pub(crate) async fn run_interpretation_with_strategy_and_model_linked(
    _perspective: &mut PerspectiveInstance,
    _shapes: &[ModelShape],
    _transcript: &[TranscriptTurn],
    _base_prefix: &str,
    _context: &AgentContext,
    _dedup_strategy: &DedupStrategy,
    _model_override: Option<&str>,
    _scope: Option<&Scope>,
    _cursor: Option<&InterpretationRunCursor>,
    _emit_debug_events: bool,
    _emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
    _flow_filter: Option<&[String]>,
    _slice_link: Option<&SliceScopeLink<'_>>,
) -> anyhow::Result<InterpretationOutcome> {
    Err(unavailable())
}

#[allow(clippy::too_many_arguments)]
pub async fn run_interpretation_with_harness_and_model(
    _perspective: &mut PerspectiveInstance,
    _shapes: &[ModelShape],
    _transcript: &[TranscriptTurn],
    _base_prefix: &str,
    _context: &AgentContext,
    _model_override: Option<&str>,
    _scope: Option<&Scope>,
    _cursor: Option<&InterpretationRunCursor>,
    _max_tool_calls: u32,
    _caller: Option<Arc<crate::agent::session::Session>>,
    _emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
    _dedup_on_drain: bool,
    _credit_gate: Option<Arc<dyn crate::ai_service::harness::CreditGate>>,
    _flow_filter: Option<&[String]>,
) -> anyhow::Result<InterpretationOutcome> {
    Err(unavailable())
}

pub mod overlay {
    use super::unavailable;
    use crate::agent::AgentContext;
    use crate::perspectives::perspective_instance::PerspectiveInstance;

    /// Same shape as native `overlay::OverlayView` (the RPC contract names it).
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, ts_rs::TS)]
    pub(crate) struct OverlayView {
        pub base: String,
        pub kind: String,
        pub run: Option<String>,
        pub inferred: Vec<(String, serde_json::Value)>,
    }

    pub(crate) async fn list_overlays(
        _perspective: &PerspectiveInstance,
    ) -> anyhow::Result<Vec<OverlayView>> {
        Err(unavailable())
    }

    pub(crate) async fn accept_interpretation(
        _perspective: &mut PerspectiveInstance,
        _base: &str,
        _property: Option<&str>,
        _context: &AgentContext,
    ) -> anyhow::Result<()> {
        Err(unavailable())
    }

    pub(crate) async fn reject_interpretation(
        _perspective: &mut PerspectiveInstance,
        _base: &str,
        _property: Option<&str>,
        _context: &AgentContext,
    ) -> anyhow::Result<()> {
        Err(unavailable())
    }
}
