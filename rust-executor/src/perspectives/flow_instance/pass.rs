//! The background sweep: derive every instance in scope, then record what
//! the fold already decided.
//!
//! This module observes; it does not decide. It writes exactly two things —
//! the `currentState` cache, so a reader without a perspective sees the
//! fold's answer, and `resolved_as → "fired"` marks, so UIs can list history
//! and the mint side can tell a new consensus event from one it has already
//! seen. Neither is ever read back as authority.
//!
//! **It deletes nothing.** A replica's view is partial by construction: a
//! proposal can arrive before the evidence it cites, and the settling links
//! of an earlier transition can arrive after a later one. Deleting on the
//! strength of a partial view destroys honest history that sync was about to
//! vindicate, so the right response to a disagreement is to wait for links,
//! never to remove them. Superseded and unverifiable proposals therefore
//! linger until a follow-up marks them (never deletes them); explicit
//! rejection is a human act and lives elsewhere.

use super::{fold_read_set, FlowInstance};
use crate::agent::AgentContext;
use crate::perspectives::flow_classes::advance_flow_instance_state;
use crate::perspectives::flow_context::{
    load_all_flow_instances, load_flow_instances, load_shacl_flows, retain_selected_flows,
    scope_subject,
};
use crate::perspectives::flow_instance::atom::{FIRED_MARK, RESOLVED_AS_PREDICATE};
use crate::perspectives::model_query::types::Scope;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{Link, LinkStatus};

/// One consensus event this replica recorded for the first time: the atoms
/// that settled an edge, now marked, with the cache advanced to match.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FireOutcome {
    pub instance_uri: String,
    pub from_state: String,
    pub to_state: String,
    /// The distinct DIDs whose votes made up the quorum.
    pub voters: Vec<String>,
    pub contributing_proposal_uris: Vec<String>,
}

/// One sweep over the `FlowInstance`s in scope: derive each, and where the
/// graph lags the fold, write the cache and the marks in a single batch.
///
/// Never fails: any error is logged and skips the narrowest safe unit
/// (instance < pass). `instance_filter` narrows the sweep to one instance,
/// so a single accept does not re-derive every flow on the perspective.
pub async fn run_flow_consensus_pass(
    perspective: &mut PerspectiveInstance,
    scope: Option<&Scope>,
    context: &AgentContext,
    flow_filter: Option<&[String]>,
    instance_filter: Option<&str>,
) -> Vec<FireOutcome> {
    let loaded = async {
        let mut flows_by_uri = load_shacl_flows(perspective).await?;
        retain_selected_flows(&mut flows_by_uri, flow_filter);
        let records = match scope {
            Some(s) => load_flow_instances(perspective, &[scope_subject(s).to_string()]).await?,
            None => load_all_flow_instances(perspective).await?,
        };
        anyhow::Ok((flows_by_uri, records))
    }
    .await;
    let (flows_by_uri, mut records) = match loaded {
        Ok(l) => l,
        Err(e) => {
            log::warn!("run_flow_consensus_pass: load failed: {e:#}");
            return Vec::new();
        }
    };
    if let Some(only) = instance_filter {
        records.retain(|r| r.instance_uri == only);
    }
    records.sort_by(|a, b| a.instance_uri.cmp(&b.instance_uri));

    let mut outcomes = Vec::new();
    for record in &records {
        let Some(flow) = flows_by_uri.get(&record.flow_uri) else {
            continue;
        };
        let instance = FlowInstance::from_record(record, flow);
        let read_set = match instance.read_set(perspective).await {
            Ok(rs) => rs,
            Err(e) => {
                log::warn!(
                    "run_flow_consensus_pass: reading {} failed; skipping instance this pass: {e:#}",
                    record.instance_uri
                );
                continue;
            }
        };
        let derived = fold_read_set(flow, &read_set);
        let already_marked = read_set.marked_proposals();

        // An edge is new to this replica when some atom that settled it is
        // not yet marked. The mark is bookkeeping, so this comparison can
        // never change the state — only how much of it we still have to note.
        let mut to_mark: Vec<String> = Vec::new();
        let mut fresh: Vec<FireOutcome> = Vec::new();
        for edge in &derived.settled {
            let unmarked: Vec<String> = edge
                .atom_uris
                .iter()
                .filter(|uri| !already_marked.contains(*uri))
                .cloned()
                .collect();
            if unmarked.is_empty() {
                continue;
            }
            to_mark.extend(unmarked);
            fresh.push(FireOutcome {
                instance_uri: record.instance_uri.clone(),
                from_state: edge.from_state.clone(),
                to_state: edge.to_state.clone(),
                voters: edge.voters.clone(),
                contributing_proposal_uris: edge.atom_uris.clone(),
            });
        }
        let stale_cache = record.current_state != derived.state;
        if !stale_cache && to_mark.is_empty() {
            continue;
        }
        if stale_cache {
            log::debug!(
                "run_flow_consensus_pass: healing {} — cached `{}`, derived `{}`",
                record.instance_uri,
                record.current_state,
                derived.state
            );
        }
        match write_state_and_marks(
            perspective,
            &record.instance_uri,
            stale_cache.then_some(derived.state.as_str()),
            &to_mark,
            context,
        )
        .await
        {
            Ok(()) => outcomes.extend(fresh),
            Err(e) => log::warn!(
                "run_flow_consensus_pass: recording {} rolled back (re-runs next pass): {e:#}",
                record.instance_uri
            ),
        }
    }
    outcomes
}

/// Write the cache and the marks in one batch, so a crash between them can
/// never leave a marked history with an un-advanced cache. Both are
/// bookkeeping, so a rollback costs nothing but a repeat next pass.
async fn write_state_and_marks(
    perspective: &mut PerspectiveInstance,
    instance_uri: &str,
    advance_to: Option<&str>,
    mark_fired: &[String],
    context: &AgentContext,
) -> anyhow::Result<()> {
    let batch_id = perspective.create_batch().await;
    let written = async {
        if let Some(state) = advance_to {
            advance_flow_instance_state(
                perspective,
                instance_uri,
                state,
                Some(batch_id.clone()),
                context,
            )
            .await?;
        }
        for uri in mark_fired {
            perspective
                .add_link(
                    Link {
                        source: uri.clone(),
                        predicate: Some(RESOLVED_AS_PREDICATE.to_string()),
                        target: format!("literal:string:{}", urlencoding::encode(FIRED_MARK)),
                    },
                    LinkStatus::Shared,
                    Some(batch_id.clone()),
                    context,
                )
                .await
                .map_err(|e| anyhow::anyhow!("marking {uri} fired failed: {e:#}"))?;
        }
        anyhow::Ok(())
    }
    .await;
    if let Err(e) = written {
        perspective.discard_batch(&batch_id).await;
        return Err(e);
    }
    match perspective.commit_batch(batch_id.clone(), context).await {
        Ok(_) => Ok(()),
        Err(e) => {
            perspective.discard_batch(&batch_id).await;
            Err(anyhow::anyhow!("commit_batch failed: {e:#}"))
        }
    }
}
