//! Writer stage + composed entry point.
//!
//! [`write_engine_proposal`] turns one `SatisfiedTransition` into an
//! on-graph `FlowTransitionProposal`. [`run_engine_proposal_pass`]
//! composes load → evaluate → (optional semantic-check) → write into the
//! single call the extraction pass invokes after `apply_with_overlay`.

#![allow(dead_code, clippy::too_many_arguments)]

use super::primitives::SatisfiedTransition;
use super::queryable::evaluate_flow_transitions;

/// Convenience over
/// [`crate::perspectives::flow_classes::write_flow_transition_proposal`]
/// for the engine-generated path.
///
/// Consumes a [`SatisfiedTransition`] (the deterministic
/// requires-satisfied record produced by [`evaluate_flow_transitions`])
/// and threads its fields into the primitive writer. Kept in the
/// evaluator module — not `flow_classes` — so the classes layer stays a
/// leaf: it doesn't need to know about `SatisfiedTransition` to mint
/// proposals, and this wrapper is only compiled on the evaluator's dep
/// path.
///
/// `proposal_id` / `batch_id` are caller-supplied to stay consistent
/// with `mint_flow_instance` — the auto-processor generates the id and
/// threads its own batch so the whole extraction pass commits atomically.
/// Propose-time is synthesised on-graph by `Ad4mModel`'s `createdAt`
/// (earliest link timestamp on the proposal URI), so no timestamp param
/// is threaded.
///
/// Returns the freshly-minted proposal URI.
#[allow(clippy::too_many_arguments)]
pub async fn write_engine_proposal(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    proposal_id: &str,
    proposer_did: &str,
    transition: &SatisfiedTransition,
    rationale: Option<&str>,
    batch_id: Option<String>,
    context: &crate::agent::AgentContext,
) -> anyhow::Result<String> {
    crate::perspectives::flow_classes::write_flow_transition_proposal(
        perspective,
        proposal_id,
        proposer_did,
        &transition.instance_uri,
        &transition.from_state,
        &transition.to_state,
        &transition.evidence_ids,
        &transition.evidence_hash,
        rationale,
        batch_id,
        context,
    )
    .await
}

/// An LLM-emitted "proposal to advance this flow" that the engine may
/// honour when the deterministic `requires` guard also fires.
///
/// This is the boundary type between the interpretation layer (which parses
/// [`crate::perspectives::interpretation::types::LlmFlowProposal`] from the
/// LLM's JSON) and the flow-post-processing engine. Keeping the boundary
/// type here (rather than importing `LlmFlowProposal` directly) avoids the
/// `flow_evaluator` ← `interpretation` ← `flow_evaluator` module cycle.
///
/// - `instance_uri` — the FlowInstance URI the LLM cited (verbatim from
///   the prompt's `active_flows[i].instance`).
/// - `to_state` — one of that FlowInstance's `nextStates[j].name` values.
/// - `reason` — optional short attribution. Written as the proposal's
///   `rationale` field only when `Some(text)` with non-empty text.
///
/// Match semantics inside [`run_engine_proposal_pass`]: a hint matches a
/// [`SatisfiedTransition`] iff BOTH `instance_uri` and `to_state` match.
/// Unmatched hints (no satisfied transition for that pair) are silently
/// discarded — the LLM cannot bypass the deterministic guard, and the
/// prompt already documents this behavior.
#[derive(Debug, Clone, PartialEq)]
pub struct LlmProposalHint {
    pub instance_uri: String,
    pub to_state: String,
    pub reason: Option<String>,
}

/// Compose the load → evaluate → write pipeline into one call that the
/// extraction pass (`interpretation::run`) invokes AFTER
/// `apply_with_overlay` has committed the LLM-derived writes. At that
/// point the graph state on which `requires` model-queries run is what
/// the pass just produced, so a transition satisfied by fresh evidence
/// is immediately turned into a proposal on behalf of the acting DID.
///
/// Silent-fail throughout — the extraction pass MUST NOT break because
/// the flow layer stumbled. Loader errors, unknown-DID errors, and
/// individual `write_engine_proposal` failures are logged (`warn!` for
/// the loader path, `debug!` for per-transition writes) and downgraded
/// to an empty result / a partial list.
///
/// `scope`, when `Some`, narrows the FlowInstance load to the pass's
/// anchor URI (same policy as
/// [`crate::perspectives::flow_context::gather_active_flow_contexts`]).
///
/// `semantic_check`, when `Some((llm, model_id))`, wires the 2nd-pass LLM
/// confirmation between the deterministic evaluator and the on-graph
/// write. For each `SatisfiedTransition` whose target state carries a
/// `semantic_check` hint,
/// [`crate::perspectives::flow_semantic_check::run_semantic_check`] is
/// invoked and only a `Pass` verdict advances the transition to the
/// write stage; `Fail` and `Ambiguous` discard the transition (fail-safe:
/// an uncertain LLM must not silently advance a flow). Transitions
/// without a per-state `semantic_check` hint are auto-passed without an
/// LLM call. LLM I/O errors are treated as `discard` — the flow layer
/// must never break the extraction pass. When `semantic_check` is `None`
/// the gate is skipped entirely.
///
/// `llm_hints` carries the LLM's own `flow_proposals` output as a slice
/// of [`LlmProposalHint`]s. When a hint matches a satisfied transition
/// by `(instance_uri, to_state)`, the LLM's `reason` (if any) is written
/// as the proposal's `rationale` field — attribution flows from the LLM
/// to the on-graph proposal. Hints WITHOUT a matching satisfied
/// transition are silently discarded: the LLM cannot bypass the
/// deterministic `requires` guard. Satisfied transitions without a
/// matching hint still get an engine-emitted proposal with
/// `rationale = None`. Pass `&[]` to opt out entirely.
///
/// Returns the URIs of every `FlowTransitionProposal` this pass minted.
/// The extraction pass threads these into
/// [`crate::perspectives::interpretation::run::InterpretationOutcome::flow_proposals`]
/// so tests / callers can observe.
pub async fn run_engine_proposal_pass(
    perspective: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    scope: Option<&crate::perspectives::model_query::types::Scope>,
    context: &crate::agent::AgentContext,
    semantic_check: Option<(
        &dyn crate::perspectives::flow_semantic_check::SemanticCheckLlm,
        &str,
    )>,
    llm_hints: &[LlmProposalHint],
) -> Vec<String> {
    // Load the flow catalogue. Empty on I/O failure — same policy as
    // `gather_active_flow_contexts`. An empty perspective has zero flows,
    // and there's nothing to post-process.
    let flows_by_uri = match crate::perspectives::flow_context::load_shacl_flows(perspective).await
    {
        Ok(m) => m,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: load_shacl_flows failed: {e:#}");
            return Vec::new();
        }
    };
    if flows_by_uri.is_empty() {
        return Vec::new();
    }

    // Load active FlowInstances. When the pass carries a `scope`
    // anchor (extraction-pass path from `interpretation::run`), narrow
    // to that anchor's base URI — J#1's bounded default. When `scope`
    // is `None` (engine-only sweep entry point + all e2e tests), load
    // every live FlowInstance on the perspective; the sweep's bound is
    // the perspective's own flow count, not the extraction batch.
    // Same silent-fallback on load failure as the pre-pass loader.
    let records = match scope {
        Some(s) => {
            let subject = crate::perspectives::flow_context::scope_subject(s).to_string();
            crate::perspectives::flow_context::load_flow_instances(perspective, &[subject]).await
        }
        None => crate::perspectives::flow_context::load_all_flow_instances(perspective).await,
    };
    let records = match records {
        Ok(r) => r,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: load_flow_instances failed: {e:#}");
            return Vec::new();
        }
    };
    if records.is_empty() {
        return Vec::new();
    }

    // The proposer of an engine-generated proposal is the acting DID of
    // the extraction pass — same identity that owns the InterpretationRun
    // for the committed writes. Silent-fallback on lookup failure: no DID
    // → no proposals; the transitions remain latent for the next pass.
    let acting_did = match crate::agent::did_for_context(context) {
        Ok(d) => d,
        Err(e) => {
            log::warn!("run_engine_proposal_pass: did_for_context failed: {e:#}");
            return Vec::new();
        }
    };

    let satisfied =
        evaluate_flow_transitions(perspective, &records, &flows_by_uri, &acting_did).await;
    if satisfied.is_empty() {
        return Vec::new();
    }

    // Index FlowContext by instance_uri so the semantic-check gate can
    // look up the flow's overall interpretationHint + next-state
    // summaries when composing its confirmation prompt. Computed once per
    // pass (not per transition) since multiple SatisfiedTransitions can
    // share the same active FlowInstance. Only needed when
    // `semantic_check` is `Some` — an empty HashMap when the gate is off
    // costs one allocation and keeps the per-transition loop uniform.
    let flow_ctx_by_uri: std::collections::HashMap<
        String,
        crate::perspectives::flow_context::FlowContext,
    > = if semantic_check.is_some() {
        crate::perspectives::flow_context::build_flow_contexts(&records, &flows_by_uri)
            .into_iter()
            .map(|c| (c.instance_uri.clone(), c))
            .collect()
    } else {
        std::collections::HashMap::new()
    };

    // Each proposal writes inside its own batch — same
    // create_batch / commit_batch discipline `apply_with_overlay` uses
    // for `mint_interpretation_run`. `write_flow_transition_proposal`
    // internally does `create_subject` + N-1 `update_subject` for the
    // evidence bag, so wrapping them in a batch is what makes the
    // proposal land atomically on-graph (readers never see a
    // half-populated proposal). Per-transition batches — not one batch
    // for the whole pass — so a single failure only rolls back that
    // proposal and the others still ship.
    //
    // Propose-time is synthesised on-graph by `Ad4mModel`'s built-in
    // `createdAt` (earliest link timestamp on the proposal URI) — the
    // writer no longer takes a `proposed_at` param.
    let mut minted = Vec::with_capacity(satisfied.len());
    for transition in &satisfied {
        // Semantic-check gate. Runs BEFORE the write so a
        // rejected/uncertain transition never lands as a proposal. The
        // gate is skipped entirely when the caller passes `None`. When
        // `Some((llm, model_id))`:
        //   - transition has no `semantic_check` hint → `run_semantic_check`
        //     short-circuits to `Pass` without an LLM call (see the
        //     `build_semantic_check_prompt` contract).
        //   - hint present → LLM is called; only `Pass` fires. `Fail` and
        //     `Ambiguous` discard (fail-safe: uncertain LLM must not
        //     silently advance a flow).
        //   - LLM I/O error → discard the transition, log at `debug!`. The
        //     flow layer must never break the extraction pass.
        //   - FlowContext lookup miss (unexpected: every SatisfiedTransition
        //     came from a record we built contexts for) → discard + log,
        //     same fail-safe philosophy.
        if let Some((llm, model_id)) = semantic_check {
            let Some(flow_ctx) = flow_ctx_by_uri.get(&transition.instance_uri) else {
                log::debug!(
                    "run_engine_proposal_pass: no FlowContext for {}.{}→{} (instance {}); discarding",
                    transition.flow_name,
                    transition.from_state,
                    transition.to_state,
                    transition.instance_uri,
                );
                continue;
            };
            match crate::perspectives::flow_semantic_check::run_semantic_check(
                llm, model_id, transition, flow_ctx,
            )
            .await
            {
                Ok(verdict) => {
                    if !crate::perspectives::flow_semantic_check::should_fire_proposal(verdict) {
                        log::debug!(
                            "run_engine_proposal_pass: semantic-check {verdict:?} on {}.{}→{}; discarding",
                            transition.flow_name,
                            transition.from_state,
                            transition.to_state,
                        );
                        continue;
                    }
                }
                Err(e) => {
                    log::debug!(
                        "run_engine_proposal_pass: semantic-check LLM error on {}.{}→{}: {e:#}; discarding",
                        transition.flow_name,
                        transition.from_state,
                        transition.to_state,
                    );
                    continue;
                }
            }
        }

        // Match LLM hints by (instance_uri, to_state). The first matching
        // hint wins if the LLM emitted several for the same pair (the
        // prompt caps at one per instance per pass, but this is a
        // fail-safe against a chatty small model). An unmatched satisfied
        // transition still writes — just without a rationale.
        let rationale = llm_hints
            .iter()
            .find(|h| {
                h.instance_uri == transition.instance_uri && h.to_state == transition.to_state
            })
            .and_then(|h| h.reason.as_deref())
            .filter(|s| !s.is_empty());

        let proposal_id = uuid::Uuid::new_v4().to_string();
        let batch_id = perspective.create_batch().await;
        let write_res = write_engine_proposal(
            perspective,
            &proposal_id,
            &acting_did,
            transition,
            rationale,
            Some(batch_id.clone()),
            context,
        )
        .await;
        match write_res {
            Ok(uri) => match perspective.commit_batch(batch_id.clone(), context).await {
                Ok(_) => minted.push(uri),
                Err(e) => {
                    let _ = perspective.discard_batch(&batch_id).await;
                    log::debug!(
                        "run_engine_proposal_pass: commit_batch for {}.{}→{} failed: {e:#}",
                        transition.flow_name,
                        transition.from_state,
                        transition.to_state,
                    );
                }
            },
            Err(e) => {
                // One bad write must not sink the rest — mirror the
                // per-transition silent-skip policy `evaluate_flow_transitions`
                // uses for query errors.
                let _ = perspective.discard_batch(&batch_id).await;
                log::debug!(
                    "run_engine_proposal_pass: write_engine_proposal for {}.{}→{} failed: {e:#}",
                    transition.flow_name,
                    transition.from_state,
                    transition.to_state,
                );
            }
        }
    }
    minted
}
