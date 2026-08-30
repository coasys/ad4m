/**
 * TypeScript @Model classes mirroring the Rust-side hardwired SDNA for the
 * flow runtime. Apps can import these to query the perspective graph for
 * pending flow-transition proposals without knowing the raw link predicates.
 *
 * Registration:
 *   await FlowTransitionProposal.register(perspective);
 *
 * Querying:
 *   const pending = await FlowTransitionProposal.findAll(perspective);
 *
 * Slice 5/7 of the flow-implementation arc (design doc §4.2). The Rust
 * engine does not yet mint these — that lands with the consensus firing
 * work in a later PR arc. This class defines the on-graph shape so:
 *   - clients can build UI over proposals immediately (Deliberation demo);
 *   - the SDNA-parity test locks TS/Rust drift the moment the JSON exists
 *     (`rust-executor/src/perspectives/hardwired_sdna/flow_transition_proposal.json`);
 *   - the engine can start writing into this shape when consensus lands
 *     without touching the client-side surface.
 */

import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Optional, Property } from "../model/decorators";
import { Model } from "../model/decorators";
import type { PerspectiveProxy } from "./PerspectiveProxy";
import { computeFlowEvidenceHash } from "./FlowEvidenceHash";

// ── FlowTransitionProposal ──────────────────────────────────────────────────
// Mirrors the Rust hardwired `flow_transition_proposal.json` SDNA (once wired
// on the engine side; see file header). One node per proposed transition; the
// URI is `ad4m://flow/proposal/<uuid>` when the engine mints it, but the class
// carries no URI-format constraint — apps constructing proposals directly may
// choose any URI they can defend as unique.

@Model({ name: "FlowTransitionProposal" })
export class FlowTransitionProposal extends Ad4mModel {
  /**
   * URI of the running `FlowInstance` this proposal targets. First-declared
   * property so `buildSHACL` derives the shape namespace from its `through`
   * prefix — `ad4m://flow/instance` → `ad4m://` → target_class
   * `ad4m://FlowTransitionProposal`. Also the identity predicate: the
   * discriminator `findAll` uses to isolate proposal nodes.
   */
  @Property({ through: "ad4m://flow/instance", required: true, identity: true })
  flowInstance: string = "";

  /** Name of the state the flow instance is currently in. */
  @Property({ through: "ad4m://flow/from_state", required: true })
  fromState: string = "";

  /** Name of the state the proposal wants to transition to. */
  @Property({ through: "ad4m://flow/to_state", required: true })
  toState: string = "";

  /** DID of the agent that issued the proposal. */
  @Property({ through: "ad4m://flow/proposer", required: true })
  proposer: string = "";

  /**
   * URIs of instances offered as evidence that `toState.requires` is now
   * satisfied. Multiple links, one per cited instance; ordering is not
   * significant.
   */
  @HasMany({ through: "ad4m://flow/evidence", datatype: "xsd:string" })
  evidence: string[] = [];

  /**
   * Scalar SHA-256 hex digest (lowercase, 64 chars) of the canonicalized
   * evidence bag. Byte-for-byte parity with Rust
   * `rust-executor/src/perspectives/flow_evaluator.rs::evidence_hash` and
   * TS {@link computeFlowEvidenceHash} — see that helper's docstring for
   * the exact byte layout. Named `evidenceHashes` (plural) because the
   * digest covers the whole bag; not a per-URI JSON map.
   * Downstream consensus verification (design §7) recomputes this hash
   * to check the evidence list hasn't been tampered with after the
   * proposal was written.
   */
  @Property({ through: "ad4m://flow/evidence_hashes", required: true })
  evidenceHashes: string = "";

  /**
   * URI of the `InterpretationRun` node that produced this proposal, when
   * the proposal came from an LLM extraction pass. Absent when a human
   * clicked "propose" in the UI or an external tool wrote the proposal
   * directly.
   */
  @Optional({ through: "ad4m://flow/run_uri" })
  runUri?: string;

  /**
   * Free-form human-readable justification. Optional — the LLM includes it
   * when it wants to explain *why* the cited evidence satisfies the state's
   * `requires`; the UI shows it to reviewers.
   */
  @Optional({ through: "ad4m://flow/rationale" })
  rationale?: string;

  /**
   * RFC3339 timestamp when the proposal was written.
   * Named `proposedAt` (not `createdAt`) because `Ad4mModel` reserves
   * `createdAt` / `updatedAt` / `baseExpression` as synthetic hydration
   * fields — a subclass property with those names is silently overwritten
   * by the earliest / latest link timestamp on hydration, and the TS-side
   * `jsonToModelInstance` converts any `createdAt` value to epoch millis.
   */
  @Property({ through: "ad4m://flow/created_at", required: true })
  proposedAt: string = "";

  /**
   * Client-side factory: mint a new `FlowTransitionProposal` on-graph.
   *
   * Symmetric with what the Rust engine's auto-processor writes when a
   * satisfied `requires` clause fires (see
   * `rust-executor/src/perspectives/flow_classes.rs::write_flow_transition_proposal`)
   * — same predicate set, same evidence-hash algorithm — so downstream
   * consensus verification (design §7) cannot tell engine-minted and
   * client-minted proposals apart from the on-graph shape.
   *
   * Called by the OO wrapper `FlowInstance.proposeTransition(...)` (lands
   * with the wrapper on PR #929's branch) and by any client that already
   * knows the instance URI + fromState (e.g. bots writing proposals
   * directly, UI "propose" buttons).
   *
   * @throws When `toState` is empty (must be a real declared state name).
   * @throws When `proposer` is empty (must be a DID).
   * @returns The freshly-minted `FlowTransitionProposal` (hydrated).
   */
  static async propose(
    perspective: PerspectiveProxy,
    opts: BuildFlowTransitionProposalOpts,
  ): Promise<FlowTransitionProposal> {
    const fields = buildFlowTransitionProposalFields(opts);
    return FlowTransitionProposal.create(perspective, fields);
  }

  /**
   * List all proposals on-graph that target a given `FlowInstance` URI,
   * oldest first.
   *
   * Thin, well-typed wrapper over `findAll({ where: { flowInstance } })` —
   * exists so:
   *   - the ordering is a documented contract (vote aggregation and
   *     consensus firing iterate proposals deterministically);
   *   - the empty-`flowInstanceUri` boundary is defended at the entry
   *     point (raw `findAll({ where: { flowInstance: "" } })` would return
   *     nothing silently — misleading to callers who meant "for this
   *     instance" and passed a stale value);
   *   - consensus/vote code has a grep target that names its intent
   *     ("list proposals for instance"), not a generic `findAll` call.
   *
   * @throws When `flowInstanceUri` is empty.
   */
  static async listForInstance(
    perspective: PerspectiveProxy,
    flowInstanceUri: string,
  ): Promise<FlowTransitionProposal[]> {
    if (!flowInstanceUri) {
      throw new Error(
        "FlowTransitionProposal.listForInstance: flowInstanceUri is required",
      );
    }
    const results = await FlowTransitionProposal.findAll(perspective, {
      where: { flowInstance: flowInstanceUri },
    });
    return [...results].sort((a, b) => {
      if (a.proposedAt < b.proposedAt) return -1;
      if (a.proposedAt > b.proposedAt) return 1;
      return 0;
    });
  }
}

/**
 * Input contract for {@link FlowTransitionProposal.propose} and its pure
 * companion {@link buildFlowTransitionProposalFields}. Fields mirror the
 * @Model properties on {@link FlowTransitionProposal}, with two
 * consensus-verifier-relevant additions:
 *
 * - `classNames`: the ordered class-URI list from the flow's `requires`
 *   clauses. Feeds the evidence hash. Load-bearing ordering — the flow
 *   definition author controls it and consensus verifiers reproduce it
 *   from the on-graph `SHACLFlow` at verification time.
 * - `proposedAt`: RFC3339 timestamp. Optional; defaults to
 *   `new Date().toISOString()`. Tests inject a fixed value to lock
 *   deterministic on-graph output.
 */
export interface BuildFlowTransitionProposalOpts {
  flowInstance: string;
  fromState: string;
  toState: string;
  proposer: string;
  evidence: readonly string[];
  classNames: readonly string[];
  rationale?: string;
  runUri?: string;
  proposedAt?: string;
}

/**
 * Pure helper: assemble the `{ [field]: value }` record that
 * {@link FlowTransitionProposal.propose} passes to `Ad4mModel.create`.
 * Split out so the field-derivation logic (evidence hashing, timestamp
 * stamping, optional-field pruning) is unit-testable without a live
 * perspective. `propose()` = this + `create()`, nothing else.
 *
 * @throws When `toState` or `proposer` are empty strings.
 */
export function buildFlowTransitionProposalFields(
  opts: BuildFlowTransitionProposalOpts,
): Record<string, unknown> {
  if (!opts.toState) {
    throw new Error("FlowTransitionProposal.propose: toState is required");
  }
  if (!opts.proposer) {
    throw new Error("FlowTransitionProposal.propose: proposer is required");
  }
  const evidenceHashes = computeFlowEvidenceHash(opts.classNames, opts.evidence);
  const fields: Record<string, unknown> = {
    flowInstance: opts.flowInstance,
    fromState: opts.fromState,
    toState: opts.toState,
    proposer: opts.proposer,
    evidence: [...opts.evidence],
    evidenceHashes,
    proposedAt: opts.proposedAt ?? new Date().toISOString(),
  };
  if (opts.rationale !== undefined) fields.rationale = opts.rationale;
  if (opts.runUri !== undefined) fields.runUri = opts.runUri;
  return fields;
}

// ── FlowInstance ────────────────────────────────────────────────────────────
// On-graph node minted by the engine when a flow is started on a specific
// base expression. Its URI is the value that `FlowTransitionProposal.flowInstance`
// references. Mirrors `rust-executor/src/perspectives/hardwired_sdna/flow_instance.json`.

@Model({ name: "FlowInstance" })
export class FlowInstance extends Ad4mModel {
  /**
   * Name of the `SHACLFlow` this instance runs. First-declared property so
   * `buildSHACL` derives the shape namespace from its `through` prefix —
   * `ad4m://flow/flow_name` → `ad4m://` → target_class `ad4m://FlowInstance`.
   * Also the discriminator predicate `findAll` uses to isolate instance nodes.
   * (Presence-based discrimination — the same value can appear on many
   * `FlowInstance` nodes when a flow is running on multiple bases.)
   */
  @Property({ through: "ad4m://flow/flow_name", required: true, identity: true })
  flow: string = "";

  /**
   * URI of the subject expression this flow runs on.
   * Named `subject` (not `baseExpression`) because `Ad4mModel` reserves
   * `baseExpression` as a synthetic hydration field always set to the
   * instance's own URI — a subclass property with that name is shadowed
   * on read.
   */
  @Property({ through: "ad4m://flow/base", required: true })
  subject: string = "";

  /** Name of the state the flow is currently in. */
  @Property({ through: "ad4m://flow/current_state", required: true })
  currentState: string = "";

  /**
   * RFC3339 timestamp when the flow was started on this base.
   * Named `startedAt` (not `createdAt`) because `Ad4mModel` reserves
   * `createdAt` / `updatedAt` as synthetic hydration fields — a subclass
   * property with those names is silently overwritten by the earliest /
   * latest link timestamp, and the TS-side `jsonToModelInstance` converts
   * any `createdAt` value to epoch millis.
   */
  @Property({ through: "ad4m://flow/created_at", required: true })
  startedAt: string = "";

  /**
   * OO wrapper for {@link FlowTransitionProposal.propose}: mint a
   * `FlowTransitionProposal` targeting *this* instance and its current
   * state, without the caller having to thread `flowInstance` /
   * `fromState` through the opts.
   *
   * Two derived fields, everything else passes through untouched:
   *   - `flowInstance` = `this.id` (the on-graph URI of this FlowInstance
   *     node, set by Ad4mModel hydration or Ad4mModel.create).
   *   - `fromState` = `this.currentState` (the state the instance is in
   *     right now — a proposal on any other value would be stale by
   *     construction).
   *
   * The pass-through preserves the design property that `.propose()` is
   * the only place proposal fields get validated / hashed / stamped, so
   * an OO-wrapper call and a static-factory call write the *same*
   * on-graph shape (evidence hash, timestamp, optional-field pruning).
   * Consensus verification cannot tell the two paths apart.
   *
   * @throws When `this.id` is empty (unhydrated / unsaved instance).
   * @throws When `this.currentState` is empty (would allow a proposal
   *   whose `fromState` is silently `""`, matching no tally bucket).
   * @throws (via `.propose()`) When `opts.toState` or `opts.proposer`
   *   are empty.
   */
  async proposeTransition(
    opts: Omit<BuildFlowTransitionProposalOpts, "flowInstance" | "fromState">,
  ): Promise<FlowTransitionProposal> {
    if (!this.id) {
      throw new Error(
        "FlowInstance.proposeTransition: instance has no id (call on a hydrated / saved instance)",
      );
    }
    if (!this.currentState) {
      throw new Error(
        "FlowInstance.proposeTransition: instance.currentState is empty",
      );
    }
    return FlowTransitionProposal.propose(this.perspective, {
      ...opts,
      flowInstance: this.id,
      fromState: this.currentState,
    });
  }
}
