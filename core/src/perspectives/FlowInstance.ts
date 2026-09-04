/**
 * `FlowInstance` — object-oriented wrapper for a running flow.
 *
 * Design authority: `docs/flow-interpretation-hints-design.md` §4.3.
 *
 * Wraps two on-graph objects:
 *
 *   - `FlowInstanceRecord` — the raw @Model row minted by
 *     {@link FlowInstance.start}; carries `flow`, `subject`, `currentState`,
 *     and Ad4mModel's synthesised `createdAt`.
 *   - `SHACLFlow` — the flow definition the record is bound to. Held so
 *     `availableTransitions` / `currentState` object lookups don't require
 *     a per-call round-trip.
 *
 * Construct via the class factories — {@link FlowInstance.start} for the
 * mint path and {@link FlowInstance.findAll} for the read path — rather
 * than the `wrap` escape hatch, which exists for tests that already own
 * a matched (record, shape) pair.
 *
 * # What's here vs. what's coming in §7 / slice 10.6
 *
 * Read surface (implemented today, PR #929):
 * - `uri`, `subject`, `flowName`, `currentStateName`, `startedAtMillis`
 *   accessors.
 * - `currentState` — resolves the `FlowState` on the shape whose name
 *   matches `record.currentState`.
 * - `availableTransitions` — filters `shape.transitions` by `fromState`.
 * - `proposals()` — queries `FlowTransitionProposal` records that
 *   target this instance's URI.
 *
 * - `acceptProposal()` / `rejectProposal()` — the consensus write API
 *   (slice 10.6): accept counts your DID toward the flow's consensusRule
 *   and fires the transition at quorum; reject hard-deletes.
 *
 * Still absent (rather than shipped as `throw new Error("not yet")`
 * stubs): `proposeTransition`, `fireAction`, and the subscriptions
 * (`onStateChange`, `onProposalAdded`, `onProposalResolved`) — they land
 * with the manual-proposal and subscription-topic slices.
 */

import { PerspectiveProxy } from "./PerspectiveProxy";
import { FlowFireOutcome } from "./PerspectiveClient";
import { Ad4mModel } from "../model/Ad4mModel";
import { FlowInstanceRecord, FlowTransitionProposal } from "./FlowModels";
import { SHACLFlow, FlowState, FlowTransition } from "../shacl/SHACLFlow";

/**
 * Extract the flow's human-readable name from its canonical URI.
 *
 * `SHACLFlow.flowUri` is `${namespace}${name}Flow` (e.g.
 * `coasys://DeliveryFlow`). This function strips the `Flow` suffix and
 * everything before the last URI-segment separator (`/` or `#`), leaving
 * the bare name. Returns `undefined` for URIs that don't match the
 * `…{name}Flow` shape — a stale FlowInstanceRecord whose flow URI came
 * from an older writer, for instance.
 */
function flowNameFromUri(flowUri: string): string | undefined {
  if (!flowUri.endsWith("Flow")) return undefined;
  const withoutSuffix = flowUri.slice(0, -"Flow".length);
  const sepIdx = Math.max(
    withoutSuffix.lastIndexOf("/"),
    withoutSuffix.lastIndexOf("#"),
  );
  if (sepIdx < 0) return withoutSuffix || undefined;
  const name = withoutSuffix.slice(sepIdx + 1);
  return name || undefined;
}

export class FlowInstance {
  /**
   * Private constructor — construct via {@link FlowInstance.start} or
   * {@link FlowInstance.findAll}. {@link FlowInstance.wrap} is the
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
   * Escape hatch used by {@link FlowInstance.start} and
   * {@link FlowInstance.findAll} — not meant for general use, but public
   * so tests that already own a `FlowInstanceRecord + SHACLFlow` pair
   * (e.g. from pre-populated fixtures) can build a wrapper without
   * going through the mint path.
   */
  static wrap(
    perspective: PerspectiveProxy,
    shape: SHACLFlow,
    record: FlowInstanceRecord,
  ): FlowInstance {
    return new FlowInstance(perspective, shape, record);
  }

  /**
   * Mint a new `FlowInstance` on the given perspective (design doc §4.3).
   *
   * Idempotently registers the hardwired `FlowInstanceRecord` +
   * `FlowTransitionProposal` @Model classes on first call — the on-graph
   * shape matches the Rust-side hardwired SDNA (parity-locked in
   * `flow-instance.test.ts` / `flow-transition-proposal.test.ts`).
   *
   * The returned wrapper carries the parsed `SHACLFlow` alongside the
   * on-graph record, so `currentState` / `availableTransitions` /
   * `proposals` accessors work without further round-trips. The consensus
   * engine (slice 10.6) will read the record's `currentState` when it
   * fires transitions.
   *
   * @param perspective - The perspective the flow instance lives on
   * @param flowName - Name of a `SHACLFlow` already registered on the perspective
   * @param baseExpression - URI of the subject expression the flow runs on
   * @throws When the flow is unknown or has zero declared states (zero-state
   *   flows fire via the forthcoming atomic-action path, §6.3).
   */
  static async start(
    perspective: PerspectiveProxy,
    flowName: string,
    baseExpression: string,
  ): Promise<FlowInstance> {
    const flow = await perspective.getFlow(flowName);
    if (!flow) throw `Flow "${flowName}" not found`;
    if (flow.states.length === 0) {
      throw `Flow "${flowName}" has no states — FlowInstance.start is for stateful flows only; zero-state flows fire via the forthcoming atomic-action path (§6.3)`;
    }
    // Register the hardwired runtime classes if this is the first flow
    // instance on the perspective. registerAll is a single batched RPC and
    // no-ops when both classes are already present.
    await Ad4mModel.registerAll(perspective, [FlowInstanceRecord, FlowTransitionProposal]);
    // Property keys must be the FlowInstanceRecord @Model field names —
    // `subject` (not `baseExpression`, which collides with Ad4mModel's
    // synthetic hydration field and would be silently shadowed on read).
    // No explicit start-time field: Ad4mModel synthesises `createdAt` on
    // hydration from the earliest link timestamp on the instance's URI.
    // Convention: `SHACLFlow.states` is stored sorted ascending by `value`
    // (enforced by `fromLinks`), so `states[0]` is the initial state. A
    // flow author who wants a specific state as the entry point must give
    // it the lowest `value` in the set.
    //
    // Store the flow's URI, not the bare name — see FlowInstanceRecord's
    // docstring for the collision-across-modules argument (James PR #929 R5).
    const record = await FlowInstanceRecord.create(perspective, {
      flowUri: flow.flowUri,
      subject: baseExpression,
      currentState: flow.states[0].name,
    });
    return FlowInstance.wrap(perspective, flow, record);
  }

  /**
   * Return every live `FlowInstance` on the perspective (design doc §4.3).
   *
   * Read-only path: never registers classes. On a perspective that has
   * never minted a flow instance, the `FlowInstanceRecord` SHACL shape
   * isn't installed yet — this returns `[]` in that case rather than
   * mutating the perspective. Registration is the responsibility of
   * {@link FlowInstance.start}, which is the write path. (Registering on
   * a read would sync a write to every peer in the neighbourhood —
   * exactly what a query must not do — James PR #929 R7.)
   *
   * Filter surface (all optional, all combinable):
   * - **`flowName`** — narrows by flow-name discriminator (e.g. "Delivery")
   * - **`subject`** — narrows by base-expression URI, i.e. "give me every
   *   flow running on THIS expression"
   *
   * Both filters translate to a single SHACL `where`-filter round-trip
   * (no client-side filtering); combining them AND-joins server-side.
   * The string-arg shape (`FlowInstance.findAll(p, "Delivery")`) is
   * shorthand for `{ flowName: "Delivery" }`.
   *
   * Records whose `flow` value has no matching `SHACLFlow` on the
   * perspective (e.g. the flow was unregistered) are silently skipped —
   * the wrapper can't answer `currentState` / `availableTransitions`
   * without the shape, and callers routinely iterate the returned array
   * without null-checks.
   *
   * @example
   * ```typescript
   * const all = await FlowInstance.findAll(perspective);
   * const deliveries = await FlowInstance.findAll(perspective, "Delivery");
   * const onThisTask = await FlowInstance.findAll(perspective, {
   *   subject: "ad4m://task/1",
   * });
   * const deliveriesOnTask = await FlowInstance.findAll(perspective, {
   *   flowName: "Delivery",
   *   subject: "ad4m://task/1",
   * });
   * ```
   */
  static async findAll(
    perspective: PerspectiveProxy,
    filter?: string | { flowName?: string; subject?: string },
  ): Promise<FlowInstance[]> {
    const { flowName, subject } =
      typeof filter === "string"
        ? { flowName: filter, subject: undefined }
        : { flowName: filter?.flowName, subject: filter?.subject };

    // Filter surface accepts a flow *name* for ergonomics — resolve it to
    // the canonical URI before the SHACL query, since the record stores
    // the URI (James PR #929 R5). Unknown name → no matches (short-circuit
    // with an empty result rather than issuing a bare-name query that
    // would silently return nothing anyway).
    const where: Record<string, string> = {};
    if (flowName !== undefined) {
      const flow = await perspective.getFlow(flowName);
      if (!flow) return [];
      where.flowUri = flow.flowUri;
    }
    if (subject !== undefined) where.subject = subject;

    // "Shape not found" on a perspective that has never minted a flow
    // instance is a no-op read, not an error — the executor throws for a
    // missing SHACL shape and there is no side-effect-free way to ask
    // "does this class exist". Return `[]` on that specific case; rethrow
    // anything else so real infra failures don't get swallowed.
    //
    // Regex covers every message the executor + client stack raises for
    // an unregistered class:
    //   "No SHACL shape stored for class 'FlowInstance'." (executor RPC)
    //   "Shape not found" (older Rust path)
    //   "class not registered" / "not registered" (TS Ad4mModel guard)
    let records: FlowInstanceRecord[];
    try {
      records =
        Object.keys(where).length > 0
          ? await FlowInstanceRecord.findAll(perspective, { where })
          : await FlowInstanceRecord.findAll(perspective);
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (
        /no shacl shape stored|shape not found|class not registered|not registered/i.test(
          msg,
        )
      ) {
        return [];
      }
      throw e;
    }

    // Pair each record with its parsed SHACLFlow. Cache lookups by
    // flow-URI so `getFlow` fires at most once per distinct flow —
    // matters when a perspective has hundreds of instances against
    // one shape.
    //
    // The URI stored on the record is `${namespace}${name}Flow`
    // (see SHACLFlow.flowUri) — recovering the name for `getFlow`
    // means stripping the namespace + the `Flow` suffix. Records that
    // don't parse this way are treated as stale and skipped.
    const shapesByUri = new Map<string, SHACLFlow>();
    const wrappers: FlowInstance[] = [];
    for (const record of records) {
      let shape = shapesByUri.get(record.flowUri);
      if (!shape) {
        const name = flowNameFromUri(record.flowUri);
        if (!name) continue;
        const loaded = await perspective.getFlow(name);
        if (!loaded || loaded.flowUri !== record.flowUri) {
          // Stale record — the flow it references was unregistered,
          // or a different flow now owns that name.
          continue;
        }
        shape = loaded;
        shapesByUri.set(record.flowUri, shape);
      }
      wrappers.push(FlowInstance.wrap(perspective, shape, record));
    }
    return wrappers;
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
    // `id` is Ad4mModel's getter over the private `_baseExpression` — the
    // only accessor populated on BOTH construction paths (create/save and
    // findAll hydration). The previous read of a raw `.baseExpression`
    // property returned undefined on every path: hydration deliberately
    // skips assigning that key, and create never sets it. First caught by
    // the accept/reject wire tests; `proposals()` inherited the same bug
    // (its where-filter matched on undefined).
    return this.record.id;
  }

  /**
   * URI of the base expression this flow runs on — matches the
   * `SHACLFlow.inputTypes` that {@link PerspectiveProxy.availableFlows}
   * would greenlight for this expression.
   */
  get subject(): string {
    return this.record.subject;
  }

  /**
   * Human-readable flow name — the display label. Sourced from the paired
   * `SHACLFlow.name`, not from the record (which stores the URI as its
   * canonical identity — see FlowInstanceRecord's docstring for why).
   */
  get flowName(): string {
    return this.shape.name;
  }

  /** Flow URI — the on-graph discriminator, collision-free across modules. */
  get flowUri(): string {
    return this.record.flowUri;
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

  /**
   * Accept a proposal targeting this instance: your DID is added to its
   * acceptors (idempotent) and the consensus pass runs immediately — when
   * the flow's `consensusRule` threshold is met the transition fires and
   * the fired outcomes are returned. Accepts a proposal object or its URI.
   */
  async acceptProposal(proposal: FlowTransitionProposal | string): Promise<FlowFireOutcome[]> {
    const uri = typeof proposal === "string" ? proposal : proposal.id;
    return this.perspective.acceptFlowProposal(uri);
  }

  /**
   * Reject a proposal targeting this instance: it is hard-deleted.
   * Already-fired proposals are the kept flow record and cannot be
   * rejected. Accepts a proposal object or its URI.
   */
  async rejectProposal(proposal: FlowTransitionProposal | string): Promise<boolean> {
    const uri = typeof proposal === "string" ? proposal : proposal.id;
    return this.perspective.rejectFlowProposal(uri);
  }
}
