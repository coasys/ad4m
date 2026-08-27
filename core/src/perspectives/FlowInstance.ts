/**
 * `FlowInstance` — object-oriented wrapper for a running flow.
 *
 * Design authority: `docs/flow-interpretation-hints-design.md` §4.3.
 *
 * Wraps two on-graph objects:
 *
 *   - `FlowInstanceRecord` — the raw @Model row minted by the engine on
 *     `PerspectiveProxy.startFlowInstance(...)`; carries `flow`,
 *     `subject`, `currentState`, and Ad4mModel's synthesised `createdAt`.
 *   - `SHACLFlow` — the flow definition the record is bound to. Held so
 *     `availableTransitions` / `currentState` object lookups don't require
 *     a per-call round-trip.
 *
 * Constructed by `PerspectiveProxy.startFlowInstance` (mint path) and
 * `PerspectiveProxy.getFlowInstances` (read path). Direct construction
 * outside those paths is possible via `FlowInstance.wrap(record, shape,
 * perspective)` but should be rare — the perspective helpers already
 * do the right thing.
 *
 * # What's here vs. what's coming in §7 / slice 10.6
 *
 * Currently implemented (2026-08-27, PR #929 review R2):
 * - `uri`, `subject`, `flowName`, `currentStateName`, `startedAtMillis`
 *   accessors — the read surface every UI needs today.
 * - `currentState` — resolves the `FlowState` on the shape whose name
 *   matches `record.currentState`.
 * - `availableTransitions` — filters `shape.transitions` by `fromState`.
 * - `proposals()` — queries `FlowTransitionProposal` records that
 *   target this instance's URI.
 *
 * Deferred to a follow-up PR (§4.3, matches consensus-engine slice 10.6):
 * - `history`, `evidence` accessors
 * - `proposeTransition(toState, evidence, rationale?)`, `accept`,
 *   `reject`, `fireAction(actionName)` mutators — need engine wiring
 * - Subscription hooks `onStateChange` / `onProposalAdded` /
 *   `onProposalResolved` — need the `flow-state-changed` event stream
 *   the consensus engine emits when it fires transitions.
 *
 * These are marked as `throw new Error("…coming in slice 10.6")` stubs so
 * TypeScript surfaces them at call-sites today but a caller reaching
 * for one gets a clear "not yet" rather than silent no-op.
 */

import { PerspectiveProxy } from "./PerspectiveProxy";
import { FlowInstanceRecord, FlowTransitionProposal } from "./FlowModels";
import { SHACLFlow, FlowState, FlowTransition } from "../shacl/SHACLFlow";

/** Unsubscribe handle returned by the deferred subscription APIs. */
export type Unsubscribe = () => void;

export class FlowInstance {
  /**
   * Private constructor — construct via `PerspectiveProxy.startFlowInstance`
   * or `PerspectiveProxy.getFlowInstances`. `FlowInstance.wrap(...)` is the
   * escape hatch for tests / direct construction cases.
   */
  private constructor(
    private readonly perspective: PerspectiveProxy,
    /** The flow definition this instance was minted against. */
    public readonly shape: SHACLFlow,
    /**
     * The on-graph record — carries `flow`, `subject`, `currentState`
     * and Ad4mModel's synthesised `createdAt`. Callers can reach into it
     * for anything the wrapper doesn't expose directly, but should prefer
     * the wrapper accessors so future refactors of the underlying shape
     * don't ripple through their code.
     */
    public readonly record: FlowInstanceRecord,
  ) {}

  /**
   * Escape hatch used by `PerspectiveProxy.startFlowInstance` and
   * `getFlowInstances` — not meant for general use, but public so tests
   * that already own a `FlowInstanceRecord + SHACLFlow` pair (e.g. from
   * pre-populated fixtures) can build a wrapper without going through
   * the mint path.
   */
  static wrap(
    perspective: PerspectiveProxy,
    shape: SHACLFlow,
    record: FlowInstanceRecord,
  ): FlowInstance {
    return new FlowInstance(perspective, shape, record);
  }

  // ── Read accessors ────────────────────────────────────────────────────

  /**
   * URI of the on-graph `FlowInstance` node — `ad4m://flow/instance/{id}`.
   *
   * Sourced from `Ad4mModel`'s synthesised `baseExpression`, which for
   * `FlowInstanceRecord` *is* the instance's own URI (the value
   * `FlowTransitionProposal.flowInstance` references).
   */
  get uri(): string {
    return (this.record as any).baseExpression as string;
  }

  /**
   * URI of the base expression this flow runs on — matches the
   * `SHACLFlow.inputTypes` a `PerspectiveProxy.availableFlows(exprAddr)`
   * check would greenlight.
   */
  get subject(): string {
    return this.record.subject;
  }

  /** Flow name — matches `shape.name` and the on-graph discriminator. */
  get flowName(): string {
    return this.record.flow;
  }

  /** Name of the state this instance is currently in. */
  get currentStateName(): string {
    return this.record.currentState;
  }

  /**
   * ISO-8601 (epoch millis after Ad4mModel hydration) start time.
   * Returns `undefined` when hydration didn't produce a `createdAt`
   * — rare, but not fatal.
   */
  get startedAtMillis(): number | undefined {
    const v = (this.record as any).createdAt;
    return typeof v === "number" ? v : undefined;
  }

  /**
   * The `FlowState` object on the shape matching `currentStateName`.
   * Throws when the record's state name is not declared on the flow —
   * that's a "stale FlowInstance whose flow was edited under it" bug,
   * not a silent-fail case.
   */
  get currentState(): FlowState {
    const s = this.shape.states.find((x) => x.name === this.currentStateName);
    if (!s) {
      throw new Error(
        `FlowInstance ${this.uri}: currentState "${this.currentStateName}" is not declared on flow "${this.flowName}". ` +
          `Known states: ${this.shape.states.map((x) => x.name).join(", ") || "(none)"}`,
      );
    }
    return s;
  }

  /**
   * Every `FlowTransition` on the shape whose `fromState` matches
   * `currentStateName`. Order preserved from the shape.
   *
   * Empty when the current state is terminal (no outgoing transitions).
   */
  get availableTransitions(): FlowTransition[] {
    return this.shape.transitions.filter((t) => t.fromState === this.currentStateName);
  }

  /**
   * All `FlowTransitionProposal` records targeting this instance's URI.
   *
   * Uses the SDNA identity discriminator (`ad4m://flow/instance` predicate)
   * for the where-filter — one SPARQL round-trip, no client-side
   * filtering.
   *
   * Note: the consensus engine (slice 10.6) will start writing these; today
   * they only exist when a client wrote one directly (or a test seeded one).
   */
  async proposals(): Promise<FlowTransitionProposal[]> {
    return FlowTransitionProposal.findAll(this.perspective, {
      where: { flowInstance: this.uri },
    });
  }

  // ── Mutations — deferred to slice 10.6 (§4.3 stubs) ───────────────────

  /**
   * @internal Stub for design §4.3 — landing with the consensus engine
   * loop in slice 10.6.
   */
  async proposeTransition(
    _toState: string,
    _evidence: string[],
    _rationale?: string,
  ): Promise<FlowTransitionProposal> {
    throw new Error(
      "FlowInstance.proposeTransition: not yet implemented (design §4.3, slice 10.6 — consensus engine loop)",
    );
  }

  /**
   * @internal Stub for design §4.3 — landing with the consensus engine
   * loop in slice 10.6.
   */
  async accept(_proposalUri: string): Promise<void> {
    throw new Error(
      "FlowInstance.accept: not yet implemented (design §4.3, slice 10.6 — consensus engine loop)",
    );
  }

  /**
   * @internal Stub for design §4.3 — landing with the consensus engine
   * loop in slice 10.6.
   */
  async reject(_proposalUri: string): Promise<void> {
    throw new Error(
      "FlowInstance.reject: not yet implemented (design §4.3, slice 10.6 — consensus engine loop)",
    );
  }

  /**
   * @internal Stub for design §4.3 (§6.3 fireAction) — landing with the
   * zero-state action-flow work.
   */
  async fireAction(_actionName: string): Promise<void> {
    throw new Error(
      "FlowInstance.fireAction: not yet implemented (design §6.3 — zero-state action flows)",
    );
  }

  /**
   * @internal Stub — subscription APIs land with the `flow-state-changed`
   * event stream in the consensus-engine PR (design §4.3).
   */
  onStateChange(_handler: (newState: FlowState, oldState: FlowState) => void): Unsubscribe {
    throw new Error(
      "FlowInstance.onStateChange: not yet implemented (design §4.3, slice 10.6 — subscription hooks)",
    );
  }

  /**
   * @internal Stub — subscription APIs land with the `flow-state-changed`
   * event stream in the consensus-engine PR (design §4.3).
   */
  onProposalAdded(_handler: (p: FlowTransitionProposal) => void): Unsubscribe {
    throw new Error(
      "FlowInstance.onProposalAdded: not yet implemented (design §4.3, slice 10.6 — subscription hooks)",
    );
  }

  /**
   * @internal Stub — subscription APIs land with the `flow-state-changed`
   * event stream in the consensus-engine PR (design §4.3).
   */
  onProposalResolved(
    _handler: (p: FlowTransitionProposal, outcome: "fired" | "rejected") => void,
  ): Unsubscribe {
    throw new Error(
      "FlowInstance.onProposalResolved: not yet implemented (design §4.3, slice 10.6 — subscription hooks)",
    );
  }
}
