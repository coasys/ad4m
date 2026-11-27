//! Why the executor refuses a flow move, as data.
//!
//! `propose_flow_transition`, `accept_flow_proposal` and `reject_flow_proposal`
//! refuse on purpose when the caller's input or the flow's state forbids the
//! move. A refusal is not a fault. [`FlowRefusal`] names it with a `kind` and the
//! facts behind it, so a client does not parse the sentence. The WS-RPC
//! handlers send it as the `data` of a 409 reply.
//!
//! A real failure (a store, signing or serialisation error) stays
//! [`FlowError::Failed`] and replies 500.
//!
//! [`Display`](std::fmt::Display) gives the English sentence. It is the
//! `message` of the reply, for logs and people.

use super::atom::{OutputRef, OutputsRefusal, FLOW_INSTANCE_PREDICATE};
use serde::{Deserialize, Serialize};
use std::fmt;
use ts_rs::TS;

/// A refused flow move. Serialised as `{ "kind": "<kebab-case>", ...camelCase fields }`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(
    tag = "kind",
    rename_all = "kebab-case",
    rename_all_fields = "camelCase"
)]
#[ts(export)]
pub enum FlowRefusal {
    /// The perspective has no `FlowInstance` at `instance`.
    NoInstance { instance: String },
    /// The perspective has no proposal at `proposal`.
    NoProposal { proposal: String },
    /// `to` is not a state `instance` can reach from `from`, its derived state.
    NotReachable { from: String, to: String },
    /// The `requires` guard of `to` is not satisfied on this replica.
    GuardUnmet { to: String },
    /// Two edges out of `from` already carry quorum, so `instance` is stalled.
    Contested { instance: String, from: String },
    /// Outputs were named for `to`, and a run does not end in `to`.
    NotTerminal { to: String },
    /// `output` is not an instance of its class on this replica. `proposal` is
    /// the proposal a voter refuses to sign. It is `null` when the caller is
    /// proposing.
    OutputNotInstance {
        output: OutputRef,
        proposal: Option<String>,
    },
    /// `proposal` enters a terminal state and carries no outputs commitment.
    OutputsUncommitted { proposal: String },
    /// The outputs `proposal` commits to hash to `recomputed` on this replica,
    /// not to `committed`.
    OutputsMismatch {
        proposal: String,
        committed: String,
        recomputed: String,
    },
    /// Open proposals on the edge `instance` to `to` name different outputs.
    DifferentOutputs {
        instance: String,
        to: String,
        proposals: Vec<String>,
    },
    /// `proposal` leaves `leaves`, and `instance` is in `current`.
    StaleProposal {
        proposal: String,
        leaves: String,
        instance: String,
        current: String,
    },
    /// `proposal` is not an identity-checked proposal. `reason` says why. It is
    /// `null` when the proposal has no flow-instance link.
    NotEngineVisible {
        proposal: String,
        reason: Option<String>,
    },
    /// `proposal` cites evidence this replica cannot reproduce.
    EvidenceUnreproducible { proposal: String },
    /// `agent` signed no link of `proposal`, and an agent retracts only its own.
    NotYourProposal { proposal: String, agent: String },
}

impl fmt::Display for FlowRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoInstance { instance } => write!(f, "no FlowInstance at {instance}"),
            Self::NoProposal { proposal } => {
                write!(f, "no FlowTransitionProposal at {proposal}")
            }
            Self::NotReachable { from, to } => write!(
                f,
                "`{to}` is not reachable from `{from}` — no proposal written"
            ),
            Self::GuardUnmet { to } => write!(
                f,
                "guard for `{to}` is not satisfied on this replica — proposal not written"
            ),
            Self::Contested { instance, from } => write!(
                f,
                "{instance} is contested in `{from}` — two edges out of it already carry \
                 quorum, so a further proposal cannot resolve it; no proposal written"
            ),
            Self::NotTerminal { to } => write!(
                f,
                "`{to}` is not terminal, so a run does not end there and has no outputs to \
                 name — no proposal written"
            ),
            Self::OutputNotInstance {
                output,
                proposal: None,
            } => write!(
                f,
                "a voter would refuse this proposal: {} — no proposal written",
                OutputsRefusal::OutputNotInstance {
                    output: output.clone()
                }
            ),
            Self::OutputNotInstance {
                output,
                proposal: Some(proposal),
            } => co_sign_refused(
                f,
                proposal,
                &OutputsRefusal::OutputNotInstance {
                    output: output.clone(),
                },
            ),
            Self::OutputsUncommitted { proposal } => {
                co_sign_refused(f, proposal, &OutputsRefusal::Uncommitted)
            }
            Self::OutputsMismatch {
                proposal,
                committed,
                recomputed,
            } => co_sign_refused(
                f,
                proposal,
                &OutputsRefusal::HashMismatch {
                    committed: committed.clone(),
                    recomputed: recomputed.clone(),
                },
            ),
            Self::DifferentOutputs {
                instance,
                to,
                proposals,
            } => write!(
                f,
                "{instance} → {to}: open proposal(s) {} on this edge name different outputs; \
                 co-sign one or reject it — no proposal written",
                proposals.join(", ")
            ),
            Self::StaleProposal {
                proposal,
                leaves,
                instance,
                current,
            } => write!(
                f,
                "proposal {proposal} is stale: it leaves `{leaves}` but {instance} is in \
                 `{current}` — accept not recorded"
            ),
            Self::NotEngineVisible {
                proposal,
                reason: Some(reason),
            } => write!(
                f,
                "proposal {proposal} is not engine-visible ({reason} — a mid-sync proposal \
                 may still complete) — accept not recorded"
            ),
            Self::NotEngineVisible {
                proposal,
                reason: None,
            } => write!(
                f,
                "proposal {proposal} carries no {FLOW_INSTANCE_PREDICATE} link — not \
                 engine-visible, accept not recorded"
            ),
            Self::EvidenceUnreproducible { proposal } => write!(
                f,
                "proposal {proposal} cites evidence this replica cannot reproduce — refusing \
                 to co-sign; the proposal is left untouched"
            ),
            Self::NotYourProposal { proposal, agent } => write!(
                f,
                "proposal {proposal} carries no link signed by {agent} — cannot reject \
                 another agent's proposal"
            ),
        }
    }
}

/// The sentence of a voter that declines to co-sign `proposal` over `refusal`.
fn co_sign_refused(
    f: &mut fmt::Formatter<'_>,
    proposal: &str,
    refusal: &OutputsRefusal,
) -> fmt::Result {
    write!(
        f,
        "proposal {proposal} is refused: {refusal} — refusing to co-sign; the proposal is left \
         untouched"
    )
}

/// The error of a flow call: a refusal the caller can act on, or a failure.
#[derive(Debug)]
pub enum FlowError {
    /// The caller's input or the flow's state forbids the move. Replies 409.
    Refused(FlowRefusal),
    /// Something broke (a store, signing or serialisation error). Replies 500.
    Failed(anyhow::Error),
}

impl From<anyhow::Error> for FlowError {
    fn from(error: anyhow::Error) -> Self {
        Self::Failed(error)
    }
}

impl From<FlowRefusal> for FlowError {
    fn from(refusal: FlowRefusal) -> Self {
        Self::Refused(refusal)
    }
}

impl fmt::Display for FlowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Refused(refusal) => refusal.fmt(f),
            Self::Failed(error) => write!(f, "{error:#}"),
        }
    }
}
