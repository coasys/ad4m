/**
 * Consensus firing for `FlowInstance` — advance a flow's `currentState`
 * once `aggregateFlowVotes` reports a tally that meets its `ConsensusRule`.
 *
 * Split into two entry points so callers can compose:
 *   - {@link selectFireCandidate} (pure) — walks an
 *     {@link AggregateFlowVotesResult} against a live instance snapshot and
 *     returns the tally that should fire *right now* (or `undefined`,
 *     including the important "stale vote" case where the picked tally's
 *     `fromState` no longer matches the instance's `currentState` because
 *     the flow has already advanced past it).
 *   - {@link fireFlowConsensus} (async) — actually mutates the on-graph
 *     `FlowInstance`: sets `currentState = firedTally.toState` and calls
 *     `.save()`, which routes through `Ad4mModel.innerUpdate` (writes the
 *     new `ad4m://flow/current_state` link + retires the old one).
 *
 * A convenience {@link fireIfConsensus} bundles the two for the common
 * "aggregate → maybe fire" caller flow.
 *
 * Design notes:
 *   - v1 fires ONLY the state advance. Recording an on-graph
 *     `FlowInstanceAdvance` event (evidence pointers + rationale
 *     provenance) is a separate slice — the returned {@link FireOutcome}
 *     surface already carries what a future writer would need.
 *   - No `fromRole` resolution happens here — the aggregator consumed the
 *     pre-resolved DID set from the caller. Firing is agnostic to how
 *     eligibility was decided.
 *   - The stale-`fromState` guard prevents a well-known concurrency hazard:
 *     several votes for `A → B` clear consensus while another firing pass
 *     has already advanced the instance to `B`; a second call must NOT
 *     interpret those votes as authority to re-advance `B → A` (there's
 *     no such transition on record) or "re-fire" `B` (no-op but confusing).
 */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import type { FlowInstance, FlowTransitionProposal } from "./FlowModels";
import type {
  AggregateFlowVotesResult,
  FlowVoteTally,
} from "./FlowVoteAggregator";

/**
 * Snapshot of what {@link fireFlowConsensus} actually wrote. Callers use
 * this for logging, UI updates, and — once the FlowInstanceAdvance event
 * class lands — projecting proposal evidence into an on-graph audit trail.
 */
export interface FireOutcome {
  /** The FlowInstance URI whose `currentState` was advanced. */
  instanceUri: string;
  /** Value of `currentState` before the advance. */
  fromState: string;
  /** Value of `currentState` after the advance. */
  toState: string;
  /**
   * Distinct DIDs whose proposals were counted toward this consensus.
   * Sorted lexicographically (mirrors {@link FlowVoteTally.eligibleProposers}).
   */
  firedByProposers: string[];
  /**
   * URIs of the proposals that contributed to the tally. Order preserved
   * from {@link FlowVoteTally.contributing}, which is oldest-first.
   */
  contributingProposalUris: string[];
}

/**
 * Choose which tally (if any) is safe to fire against a given
 * `FlowInstance` snapshot right now.
 *
 * Returns `undefined` when:
 *   - the aggregate has no `fires` tally (no target met consensus), OR
 *   - the fires tally's `fromState` differs from `instance.currentState`
 *     (a firing pass has already advanced the flow past this transition,
 *     so the votes are stale relative to the current state).
 *
 * The second guard is the reason this helper exists as a separate function:
 * `AggregateFlowVotesResult.fires` is computed without reference to the
 * live instance, so callers MUST re-check against the current snapshot
 * before firing.
 */
export function selectFireCandidate(
  instance: Pick<FlowInstance, "currentState">,
  aggregate: AggregateFlowVotesResult,
): FlowVoteTally | undefined {
  const candidate = aggregate.fires;
  if (!candidate) return undefined;
  if (candidate.fromState !== instance.currentState) return undefined;
  return candidate;
}

/**
 * Advance an on-graph `FlowInstance` to `firedTally.toState`.
 *
 * Preconditions (all enforced — a violation throws before touching the
 * perspective):
 *   - `firedTally.consensusReached === true`
 *   - `firedTally.fromState === instance.currentState`
 *   - `firedTally.toState !== instance.currentState`
 *
 * Mutation path: sets `instance.currentState`, then calls
 * `instance.save()`. `save()` routes to `Ad4mModel.innerUpdate` when the
 * instance already has a snapshot (i.e. it was hydrated via `findAll` or
 * `getData`), which diffs the current property values against the
 * snapshot and writes exactly the changed links. Callers therefore MUST
 * pass a hydrated instance — a fresh `new FlowInstance(...)` without
 * prior hydration would route into the create path and fail.
 *
 * @throws When a precondition is violated. The perspective is not
 *   touched in that case.
 */
export async function fireFlowConsensus(
  _perspective: PerspectiveProxy,
  instance: FlowInstance,
  firedTally: FlowVoteTally,
): Promise<FireOutcome> {
  if (!firedTally.consensusReached) {
    throw new Error(
      `fireFlowConsensus: refusing to fire — tally has not reached consensus (${firedTally.eligibleProposers.length}/${firedTally.requiredCount} for ${firedTally.fromState} → ${firedTally.toState})`,
    );
  }
  if (firedTally.fromState !== instance.currentState) {
    throw new Error(
      `fireFlowConsensus: stale tally — fromState=${firedTally.fromState} does not match instance.currentState=${instance.currentState} (flow already advanced?)`,
    );
  }
  if (firedTally.toState === instance.currentState) {
    throw new Error(
      `fireFlowConsensus: refusing to fire a no-op — toState=${firedTally.toState} equals instance.currentState`,
    );
  }

  const fromState = instance.currentState;
  const toState = firedTally.toState;

  instance.currentState = toState;
  await instance.save();

  return {
    instanceUri: instance.id,
    fromState,
    toState,
    firedByProposers: [...firedTally.eligibleProposers],
    contributingProposalUris: firedTally.contributing.map(
      (p: FlowTransitionProposal) => p.id,
    ),
  };
}

/**
 * Convenience: aggregate → maybe fire, in one call.
 *
 * Runs {@link selectFireCandidate} first; if it returns a tally,
 * delegates to {@link fireFlowConsensus} and returns its outcome.
 * Otherwise returns `undefined` (no consensus, or stale vote for a
 * prior state).
 *
 * Split kept: callers who want to *report* the winning tally without
 * firing (e.g. UI preview) should use {@link selectFireCandidate}
 * directly.
 */
export async function fireIfConsensus(
  perspective: PerspectiveProxy,
  instance: FlowInstance,
  aggregate: AggregateFlowVotesResult,
): Promise<FireOutcome | undefined> {
  const tally = selectFireCandidate(instance, aggregate);
  if (!tally) return undefined;
  return fireFlowConsensus(perspective, instance, tally);
}
