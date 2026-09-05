import { Link } from "../links/Links";
import { Literal } from "../Literal";
import { AD4MAction } from "./SHACLShape";

// Re-export AD4MAction for consumers who import from SHACLFlow
export { AD4MAction };

/**
 * Link pattern for state detection
 * Used to check if an expression is in a particular state
 */
export interface LinkPattern {
  /** Optional source pattern (if omitted, uses the expression address) */
  source?: string;
  /** Required predicate to match */
  predicate: string;
  /** Required target value to match */
  target: string;
}

/**
 * Per-property condition for a `ModelQuery`. String / number / boolean
 * shorthands compile to an equality match; the object forms are the full
 * shape a `ModelQuery` engine consumer will evaluate against a class
 * instance's decoded property value.
 *
 * Only the shorthand + `equals` / `in` / `exists` / `matches` operators are
 * declared in v1 (see design doc §4.1). More operators can land as
 * additional variants without a schema break.
 */
export type PropertyCondition =
  | string
  | number
  | boolean
  | { equals: unknown }
  | { in: unknown[] }
  | { exists: true }
  | { matches: string };

/**
 * A model-level query — the shape a flow's `requires` guard evaluates
 * against the perspective's current class-instance graph. Deliberately
 * higher-level than raw links so guards talk about subject classes, not
 * the underlying triples.
 *
 * See design doc §3 principle 2 ("Guards talk about models, not raw graph")
 * and §4.1 for the full field spec. Template variables allowed inside
 * `where` string values: `$flow.base`, `$flow.uri` (alias:
 * `$flow.instance`), `$did` — the engine substitutes at evaluation time.
 */
export interface ModelQuery {
  /** Subject-class URI to search for. */
  className: string;
  /**
   * Per-property conditions (all must match — AND semantics inside one
   * ModelQuery, same as AND semantics across an array of ModelQueries).
   */
  where?: { [prop: string]: PropertyCondition | string };
  /**
   * Cardinality constraint on matching instances. Default `{ min: 1 }` —
   * at least one match required to satisfy the guard.
   */
  count?: { min?: number; max?: number };
  /**
   * Scope the query to instances linked from a flow anchor. The anchor
   * (base subject or flow instance URI) is the **source** of the link and
   * the matched item is the **target**: `linkedTo: "base"` finds items
   * where the base subject links TO them via the default predicate
   * `ad4m://has_child`.
   *
   * `"base"` / `"flow"` use `ad4m://has_child`; the object form
   * `{ via, to }` names a custom predicate (e.g.
   * `{ via: "ns://about", to: "flow" }`).
   *
   * For the reverse direction (matched instance → anchor), use
   * `where: { about: "$flow.base" }` instead — `linkedTo` cannot
   * express that direction.
   */
  linkedTo?: "flow" | "base" | { via: string; to: "flow" | "base" };
  /**
   * When this query is used as a `ConsensusRule.fromRole`, names the
   * property on the matched instance that carries a DID. Design doc §7.2
   * Shape 1 (instance-carries-DID): engine runs the query, then extracts
   * `didProperty` from each result to build the eligible-DID set.
   *
   * Shape 2 (instance-is-per-DID, using `$did` templating in `where`)
   * omits `didProperty` — the engine iterates candidate DIDs and treats
   * a non-empty result set as "this DID matches."
   *
   * Ignored when the query is used as a state guard (`requires`) or
   * background `context`.
   */
  didProperty?: string;
  /**
   * OR-compose a role expression across multiple ModelQueries. Semantics:
   * a DID counts if it appears in the result of ANY branch (design §7.3
   * multi-role hybrid example — "either reviewer OR admin"). Branches
   * can carry their own `didProperty` / `where`, but NOT `linkedTo`
   * (which must be on the outer query — `linkedTo` on a branch is
   * rejected at translation time).
   *
   * Every branch must use the same `className` as the outer query and
   * cannot declare its own `count` — the outer query's class and
   * cardinality apply to the combined result set.
   *
   * A ModelQuery with a non-empty `or` array acts as a composition node;
   * its own `className` / `where` etc. still count as an additional
   * branch (union with the array). Empty or unset = no composition.
   *
   * Intentionally scoped to `or` for v1 — `and` / `not` fall out of the
   * same mechanism and can land later without a schema break (design
   * §7.4). Ignored when the query is used as a state guard or context.
   */
  or?: ModelQuery[];
}

/**
 * Consensus rule for a flow-state transition (or a top-level zero-state
 * flow's completion). Distinct DID counting with optional role
 * restriction.
 *
 * Design doc §7:
 * - `{ n: 1 }` — any agent's proposal advances (default).
 * - `{ n: 2 }` — two distinct agents must agree.
 * - `{ n, fromRole }` — only agents matching the role query count.
 *
 * `fromRole` reuses `ModelQuery` (design §7.2) so role definitions live
 * entirely in the ontology — communities can invent roles like
 * "Reviewer" / "Guardian" without any code change.
 */
export interface ConsensusRule {
  /** Required count of distinct qualifying DIDs. */
  n: number;
  /**
   * Optional role restriction. When set, only DIDs returned by the role
   * query are counted. Two evaluation shapes (design §7.2):
   *   Shape 1 — `didProperty` present: run once, extract DIDs from matches.
   *   Shape 2 — `$did` templated in `where`: run per candidate.
   */
  fromRole?: ModelQuery;
}

/**
 * Flow State definition
 * Represents a single state in the flow state machine
 */
export interface FlowState {
  /** State name (e.g., "ready", "doing", "done") */
  name: string;
  /** Numeric state value for ordering (e.g., 0, 0.5, 1) */
  value: number;
  /**
   * Natural-language description of what puts a flow instance IN this state.
   * Used by AI-driven state suggestion (`suggestFlowState`) — the same
   * pattern subject classes use for `interpretationHint`. When set, the LLM
   * reads it alongside conversation content to suggest transitions.
   *
   * Example (Deliberation Flow, "Tension" state):
   *   "Participants have voiced opposing views or objections — there is a
   *   clear disagreement or unresolved conflict on the table."
   */
  interpretationHint?: string;
  /**
   * Model-level guard: the state is satisfied when every ModelQuery in
   * the array returns at least one match on committed graph state
   * (overlays do NOT count as evidence — design §3 principle 5).
   *
   * Array semantics: AND — every entry must match. Empty / unset =
   * no model-level guard.
   *
   * Design doc §4.1: "an array of queries, ALL must match; each query's
   * matches become evidence."
   */
  requires?: ModelQuery[];
  /**
   * Optional English hint for a targeted LLM confirmation before the
   * engine declares the state entered. Distinct from
   * `interpretationHint`, which frames the state semantically for the
   * up-front extraction pass — `semanticCheck` is the "am I still
   * confident?" pass the engine runs after `requires` matches, for
   * states where structural presence isn't enough (e.g. tension
   * resolution).
   *
   * Design doc §5 covers the LLM confirmation pass shape. Unset = no
   * confirmation, `requires` matches directly imply state entered.
   */
  semanticCheck?: string;
  /**
   * Per-state consensus rule override. When present, transitions INTO
   * this state require the specified number of distinct qualifying DIDs
   * (see `ConsensusRule`). Overrides the flow-level `consensusRule` if
   * both are set; when neither is set, the runtime engine's default of
   * `{ n: 1 }` applies (any single agent's action fires the transition).
   *
   * Design doc §4.1 (v3) — per-state override enables mixed-consensus
   * flows where e.g. "Perspective" states need only one voice but the
   * "Resolution" state requires a quorum.
   */
  consensusRule?: ConsensusRule;
}

/**
 * Runtime shape check for a decoded `ModelQuery`. Deserialization paths use
 * this to guard against malformed guard arrays leaking into flow metadata
 * (an untyped `Array.isArray` would happily accept `[null]` or `[{}]`).
 * Only the mandatory `className: string` field is enforced — richer
 * validation stays with the runtime engine that will actually execute the
 * query.
 */
function isModelQueryShape(v: unknown): v is ModelQuery {
  if (
    typeof v !== "object" ||
    v === null ||
    typeof (v as { className?: unknown }).className !== "string" ||
    (v as { className: string }).className.length === 0
  ) {
    return false;
  }
  const or = (v as { or?: unknown }).or;
  if (or !== undefined) {
    if (!Array.isArray(or) || !or.every(isModelQueryShape)) return false;
  }
  return true;
}

/**
 * Runtime shape check for a decoded `ConsensusRule`. Enforces the one
 * mandatory field (`n: number` with a sane range) and, when `fromRole`
 * is present, that it's a valid ModelQuery. A malformed literal (e.g.
 * `{ n: "many" }` or `{ n: 1, fromRole: null }`) leaves the field unset
 * rather than becoming corrupt flow metadata that downstream engines
 * would trip over.
 */
function isConsensusRuleShape(v: unknown): v is ConsensusRule {
  if (typeof v !== "object" || v === null) return false;
  const n = (v as { n?: unknown }).n;
  if (typeof n !== "number" || !Number.isFinite(n) || n < 1 || !Number.isInteger(n)) {
    return false;
  }
  const fromRole = (v as { fromRole?: unknown }).fromRole;
  if (fromRole !== undefined && !isModelQueryShape(fromRole)) return false;
  return true;
}

/**
 * Flow Transition definition
 * Represents a transition between two states
 */
export interface FlowTransition {
  /** Name of this action (shown to users, e.g., "Start", "Finish") */
  actionName: string;
  /** State to transition from */
  fromState: string;
  /** State to transition to */
  toState: string;
  /** Actions to execute for this transition */
  actions: AD4MAction[];
}

/**
 * SHACL Flow - represents a state machine for AD4M expressions
 *
 * Flows define:
 * - Which expressions can enter the flow (via `inputTypes`)
 * - What states exist and their per-state guards (via `requires`)
 * - How to transition between states (via transitions + consensus rules)
 *
 * State lives on the on-graph `FlowInstanceRecord.currentState` and is
 * read/written through the `FlowInstance` wrapper class
 * ({@link FlowInstance.start} / {@link FlowInstance.findAll}) — see design
 * doc §4.3/§5.
 *
 * @example
 * ```typescript
 * const todoFlow = new SHACLFlow('todo://TODO', 'todo://');
 *
 * // Any expression can become a TODO
 * todoFlow.inputTypes = ['any'];
 *
 * todoFlow.addState({ name: 'ready', value: 0 });
 * todoFlow.addState({ name: 'doing', value: 0.5 });
 * todoFlow.addState({ name: 'done',  value: 1 });
 *
 * todoFlow.addTransition({
 *   actionName: 'Start',
 *   fromState: 'ready',
 *   toState: 'doing',
 *   actions: [],
 * });
 *
 * // Register on the perspective and mint an instance on a subject.
 * await perspective.addFlow('TODO', todoFlow);
 * const instance = await FlowInstance.start(perspective, 'TODO', 'expr://123');
 * ```
 */
export class SHACLFlow {
  /** Flow name (e.g., "TODO") */
  public name: string;

  /** Namespace for generated URIs */
  public namespace: string;

  /**
   * Top-level natural-language description of what the flow is about — used
   * by AI-driven state suggestion (`suggestFlowState`) to frame the state
   * hints. Optional; states' own `interpretationHint`s are what actually
   * discriminate. Example (Deliberation Flow):
   *   "Tracks a group deliberation from an initial proposal to a shared
   *   understanding — through voicing perspectives, surfacing tension, and
   *   finding overlap."
   */
  public interpretationHint?: string;

  /**
   * Subject-class URIs the flow accepts as its base expression.
   *
   * Design doc §4.1: a flow's *typed input*. When an
   * expression whose class matches any URI in this array enters scope,
   * the flow-spawn engine considers it a candidate base. Superseded
   * the older link-level `flowable: LinkPattern` primitive — model-level
   * matching by subject class instead of link matching. Empty array =
   * flow declares no typed input. The `"any"` sentinel is accepted as
   * a wildcard for tests and legacy flows.
   */
  public inputTypes: string[] = [];

  /**
   * Subject-class URIs the flow must produce (linked to the base) for a
   * run to count as complete.
   *
   * Design doc §4.1: a flow's *typed output*. Two jobs:
   * (a) documentation for stateful flows — records what a completed run
   * leaves in the graph; (b) **terminal condition** for zero-state flows —
   * a flow with no `FlowState`s completes when at least one instance of
   * each `outputTypes` entry exists in scope and is linked to the base,
   * and `consensusRule` fires. This is what makes "Like" a first-class
   * flow with no state machine.
   */
  public outputTypes: string[] = [];

  /**
   * Optional English hint for AI-driven flow-spawn: when should the
   * interpretation engine mint a new instance of this flow (as opposed
   * to advancing an existing one)?
   *
   * Design doc §4.1: analogous to `interpretationHint` on subject
   * classes — steers the LLM's up-front extraction pass. Distinct from
   * `interpretationHint` on the flow, which describes what the flow *is*.
   * Example (Delivery Flow): "Spawn when someone commits to a concrete,
   * actionable task."
   */
  public creationHint?: string;

  /**
   * Optional background queries pulled into the LLM prompt as CONTEXT
   * (not evidence).
   *
   * Design doc §4.1: engine evaluates each `ModelQuery` and threads
   * matches into the prompt so the LLM can reason with them, but
   * matches do NOT count toward `requires` guards on states. Same
   * `ModelQuery[]` shape and template-variable rules as `FlowState.requires`.
   * Empty / unset = no context queries.
   */
  public context?: ModelQuery[];

  /**
   * Flow-level consensus rule. Two roles:
   *  - **Default for state transitions** — when a `FlowState` doesn't
   *    declare its own `consensusRule`, transitions into it fall back
   *    to this rule.
   *  - **Zero-state flow completion** — for actions-as-flows (a flow
   *    with no `FlowState`s), this is the rule the engine checks the
   *    moment `outputTypes` are satisfied. E.g. a Like flow declares
   *    `{ n: 1 }` and completes on the first author.
   *
   * Design doc §4.1: terminal condition for stateless flows.
   * Unset = engine default of `{ n: 1 }` applies at both layers.
   */
  public consensusRule?: ConsensusRule;

  /**
   * States in this flow.
   *
   * **Ordering convention (James PR #929 R9):** the array is stored sorted
   * ascending by `value`. `states[0]` is the initial state — the one
   * {@link FlowInstance.start} mints a fresh instance into. A flow author
   * who wants a specific state as the entry point must give it the lowest
   * `value` in the set. `fromLinks` enforces this sort, so link-order on
   * the graph never dictates the initial state.
   */
  private _states: FlowState[] = [];

  /** Transitions between states */
  private _transitions: FlowTransition[] = [];

  /**
   * Create a new SHACL Flow
   * @param name - Flow name (e.g., "TODO")
   * @param namespace - Namespace for URIs (e.g., "todo://")
   */
  constructor(name: string, namespace: string) {
    this.name = name;
    this.namespace = namespace;
  }

  /** Get all states */
  get states(): FlowState[] {
    return [...this._states];
  }

  /** Get all transitions */
  get transitions(): FlowTransition[] {
    return [...this._transitions];
  }

  /**
   * Add a state to the flow
   * @param state - State definition
   */
  addState(state: FlowState): void {
    this._states.push(state);
  }

  /**
   * Add a transition to the flow
   * @param transition - Transition definition
   */
  addTransition(transition: FlowTransition): void {
    this._transitions.push(transition);
  }

  /**
   * Get the flow shape URI
   */
  get flowUri(): string {
    return `${this.namespace}${this.name}Flow`;
  }

  /**
   * Get a state URI
   */
  stateUri(stateName: string): string {
    return `${this.namespace}${this.name}.${stateName}`;
  }

  /**
   * Get a transition URI
   */
  transitionUri(fromState: string, toState: string): string {
    return `${this.namespace}${this.name}.${fromState}To${toState}`;
  }

  /**
   * Serialize the flow to AD4M links
   * These links can be stored in a perspective and queried via SPARQL
   * 
   * @returns Array of Link objects representing the flow
   */
  toLinks(): Link[] {
    const links: Link[] = [];
    const flowUri = this.flowUri;

    // Flow type
    links.push({
      source: flowUri,
      predicate: "rdf://type",
      target: "ad4m://Flow"
    });

    // Flow name
    links.push({
      source: flowUri,
      predicate: "ad4m://flowName",
      target: Literal.from(this.name).toUrl()
    });

    // Top-level interpretation hint (optional, drives AI state suggestion).
    // Empty strings are treated as "unset" so a round-trip through the graph
    // doesn't materialise an empty-hint predicate that consumers would then
    // read back as a meaningful value.
    if (this.interpretationHint) {
      links.push({
        source: flowUri,
        predicate: "ad4m://interpretationHint",
        target: Literal.from(this.interpretationHint).toUrl()
      });
    }

    // Typed I/O (flow-level, design §4.1). Both arrays are only
    // serialized when non-empty — an empty list is indistinguishable
    // from "unset" for a round-trip, and emitting one would give
    // downstream consumers a meaningless predicate to interpret.
    if (this.inputTypes.length > 0) {
      links.push({
        source: flowUri,
        predicate: "ad4m://inputTypes",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.inputTypes))}`
      });
    }
    if (this.outputTypes.length > 0) {
      links.push({
        source: flowUri,
        predicate: "ad4m://outputTypes",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.outputTypes))}`
      });
    }

    // Flow-level `creationHint` — same empty-string-is-unset guard as
    // `interpretationHint`, same reason (round-trip fidelity).
    if (this.creationHint) {
      links.push({
        source: flowUri,
        predicate: "ad4m://creationHint",
        target: Literal.from(this.creationHint).toUrl()
      });
    }

    // Flow-level `context` — background queries the LLM sees but not
    // guard evidence. Single JSON literal, consumers parse back.
    if (this.context && this.context.length > 0) {
      links.push({
        source: flowUri,
        predicate: "ad4m://context",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.context))}`
      });
    }

    // Flow-level `consensusRule` — default for state transitions when a
    // FlowState omits its own rule; also the terminal-firing rule for
    // zero-state flows. Serialized as one JSON literal (the shape is
    // fixed and cheap to parse; consistent with `context` / `requires`
    // so consumers only need one parser).
    if (this.consensusRule) {
      links.push({
        source: flowUri,
        predicate: "ad4m://consensusRule",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.consensusRule))}`
      });
    }

    // States
    for (const state of this._states) {
      const stateUri = this.stateUri(state.name);

      // Link flow to state
      links.push({
        source: flowUri,
        predicate: "ad4m://hasState",
        target: stateUri
      });

      // State type
      links.push({
        source: stateUri,
        predicate: "rdf://type",
        target: "ad4m://FlowState"
      });

      // State name
      links.push({
        source: stateUri,
        predicate: "ad4m://stateName",
        target: Literal.from(state.name).toUrl()
      });

      // State value
      links.push({
        source: stateUri,
        predicate: "ad4m://stateValue",
        target: Literal.from(state.value).toUrl()
      });

      // Per-state interpretation hint (optional, drives AI state suggestion).
      // Empty strings treated as unset — same rationale as the flow-level hint.
      if (state.interpretationHint) {
        links.push({
          source: stateUri,
          predicate: "ad4m://interpretationHint",
          target: Literal.from(state.interpretationHint).toUrl()
        });
      }

      // Per-state `requires` guard — array of ModelQueries, AND-combined.
      // Serialized as one JSON string on `ad4m://requires`. Consumers
      // (state-transition engine, `flow.availableActions`) parse back.
      if (state.requires && state.requires.length > 0) {
        links.push({
          source: stateUri,
          predicate: "ad4m://requires",
          target: `literal:string:${encodeURIComponent(JSON.stringify(state.requires))}`
        });
      }

      // Per-state `semanticCheck` hint — optional English confirmation
      // that runs after `requires` matches, for states where structural
      // presence isn't sufficient evidence. Empty string = unset.
      if (state.semanticCheck) {
        links.push({
          source: stateUri,
          predicate: "ad4m://semanticCheck",
          target: Literal.from(state.semanticCheck).toUrl()
        });
      }

      // Per-state `consensusRule` override — one JSON literal on the
      // state, same predicate name as the flow-level rule but scoped by
      // source URI so the reader can tell them apart.
      if (state.consensusRule) {
        links.push({
          source: stateUri,
          predicate: "ad4m://consensusRule",
          target: `literal:string:${encodeURIComponent(JSON.stringify(state.consensusRule))}`
        });
      }
    }

    // Transitions
    for (const transition of this._transitions) {
      const transitionUri = this.transitionUri(transition.fromState, transition.toState);
      const fromStateUri = this.stateUri(transition.fromState);
      const toStateUri = this.stateUri(transition.toState);

      // Link flow to transition
      links.push({
        source: flowUri,
        predicate: "ad4m://hasTransition",
        target: transitionUri
      });

      // Transition type
      links.push({
        source: transitionUri,
        predicate: "rdf://type",
        target: "ad4m://FlowTransition"
      });

      // Action name
      links.push({
        source: transitionUri,
        predicate: "ad4m://actionName",
        target: Literal.from(transition.actionName).toUrl()
      });

      // From state
      links.push({
        source: transitionUri,
        predicate: "ad4m://fromState",
        target: fromStateUri
      });

      // To state
      links.push({
        source: transitionUri,
        predicate: "ad4m://toState",
        target: toStateUri
      });

      // Transition actions
      links.push({
        source: transitionUri,
        predicate: "ad4m://transitionActions",
        target: `literal:string:${encodeURIComponent(JSON.stringify(transition.actions))}`
      });
    }

    return links;
  }

  /**
   * Reconstruct a SHACLFlow from links
   * 
   * @param links - Array of links containing the flow definition
   * @param flowUri - The URI of the flow to reconstruct
   * @returns Reconstructed SHACLFlow
   */
  static fromLinks(links: Link[], flowUri: string): SHACLFlow {
    // Extract namespace and name from flowUri
    // Format: {namespace}{Name}Flow
    const flowSuffix = "Flow";
    if (!flowUri.endsWith(flowSuffix)) {
      throw new Error(`Invalid flow URI: ${flowUri} (must end with 'Flow')`);
    }
    
    const withoutSuffix = flowUri.slice(0, -flowSuffix.length);
    const lastSlashOrColon = Math.max(
      withoutSuffix.lastIndexOf('/'),
      withoutSuffix.lastIndexOf(':')
    );
    
    const namespace = withoutSuffix.slice(0, lastSlashOrColon + 1);
    const name = withoutSuffix.slice(lastSlashOrColon + 1);
    
    const flow = new SHACLFlow(name, namespace);

    // Find top-level interpretation hint. Only accept a decoded value that
    // is a non-empty string — malformed literals (Literal.get() can return
    // an object, number, etc.) leave the field unset rather than becoming
    // corrupt flow metadata.
    const flowHintLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://interpretationHint"
    );
    if (flowHintLink) {
      try {
        const decoded = Literal.fromUrl(flowHintLink.target).get();
        if (typeof decoded === "string" && decoded.length > 0) {
          flow.interpretationHint = decoded;
        }
      } catch {
        // Ignore parse errors — leave unset.
      }
    }

    // Find typed I/O (flow-level, design §4.1). Both arrays validated
    // element-wise — only accept string[] payloads. A malformed literal
    // (`[null]`, `[42]`, non-array) leaves the field as its default
    // empty array rather than corrupting flow metadata.
    const inputTypesLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://inputTypes"
    );
    if (inputTypesLink) {
      try {
        const jsonStr = inputTypesLink.target.replace(
          /^literal:\/\/string:|^literal:string:/,
          ""
        );
        const parsed = JSON.parse(decodeURIComponent(jsonStr));
        if (Array.isArray(parsed) && parsed.every(v => typeof v === "string" && v.length > 0)) {
          flow.inputTypes = parsed as string[];
        }
      } catch {
        // Ignore parse errors — leave as default empty array.
      }
    }

    const outputTypesLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://outputTypes"
    );
    if (outputTypesLink) {
      try {
        const jsonStr = outputTypesLink.target.replace(
          /^literal:\/\/string:|^literal:string:/,
          ""
        );
        const parsed = JSON.parse(decodeURIComponent(jsonStr));
        if (Array.isArray(parsed) && parsed.every(v => typeof v === "string" && v.length > 0)) {
          flow.outputTypes = parsed as string[];
        }
      } catch {
        // Ignore parse errors — leave as default empty array.
      }
    }

    // Find flow-level `creationHint`. Non-empty-string guard — same
    // rationale as `interpretationHint`.
    const creationHintLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://creationHint"
    );
    if (creationHintLink) {
      try {
        const decoded = Literal.fromUrl(creationHintLink.target).get();
        if (typeof decoded === "string" && decoded.length > 0) {
          flow.creationHint = decoded;
        }
      } catch {
        // Ignore parse errors — leave unset.
      }
    }

    // Find flow-level `context` — same ModelQuery-shape guard as
    // FlowState.requires (rejects `[null]`, `[{}]`, non-string
    // `className`).
    const contextLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://context"
    );
    if (contextLink) {
      try {
        const jsonStr = contextLink.target.replace(
          /^literal:\/\/string:|^literal:string:/,
          ""
        );
        const parsed = JSON.parse(decodeURIComponent(jsonStr));
        if (Array.isArray(parsed) && parsed.every(isModelQueryShape)) {
          flow.context = parsed as ModelQuery[];
        }
      } catch {
        // Ignore parse errors — leave unset.
      }
    }

    // Find flow-level `consensusRule`. Reject malformed literals
    // (missing `n`, non-integer `n`, invalid `fromRole`) so the runtime
    // engine never has to defensively re-validate a stored rule.
    const consensusRuleLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://consensusRule"
    );
    if (consensusRuleLink) {
      try {
        const jsonStr = consensusRuleLink.target.replace(
          /^literal:\/\/string:|^literal:string:/,
          ""
        );
        const parsed = JSON.parse(decodeURIComponent(jsonStr));
        if (isConsensusRuleShape(parsed)) {
          flow.consensusRule = parsed;
        }
      } catch {
        // Ignore parse errors — leave unset.
      }
    }


    // Find states
    const stateLinks = links.filter(l =>
      l.source === flowUri && l.predicate === "ad4m://hasState"
    );
    
    // Build a map from state URI to state name for later lookup
    const stateUriToName = new Map<string, string>();
    
    for (const stateLink of stateLinks) {
      const stateUri = stateLink.target;
      
      // Get state name
      const nameLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://stateName"
      );
      const stateName = nameLink ? Literal.fromUrl(nameLink.target).get() as string : "";
      
      // Store mapping for transition lookup
      stateUriToName.set(stateUri, stateName);
      
      // Get state value
      const valueLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://stateValue"
      );
      const stateValue = valueLink ? Literal.fromUrl(valueLink.target).get() as number : 0;
      
      // Get per-state interpretation hint (optional). Same non-empty-string
      // guard as the flow-level hint.
      const hintLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://interpretationHint"
      );
      let interpretationHint: string | undefined;
      if (hintLink) {
        try {
          const decoded = Literal.fromUrl(hintLink.target).get();
          if (typeof decoded === "string" && decoded.length > 0) {
            interpretationHint = decoded;
          }
        } catch {
          // Ignore parse errors — leave unset.
        }
      }

      // Get per-state `requires` guard (optional) — one link carrying a
      // JSON-encoded ModelQuery[]. Reject arrays whose entries are not
      // ModelQuery-shaped (missing/non-string `className`, `null`, primitives).
      const requiresLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://requires"
      );
      let requires: ModelQuery[] | undefined;
      if (requiresLink) {
        try {
          const jsonStr = requiresLink.target.replace(
            /^literal:\/\/string:|^literal:string:/,
            ""
          );
          const parsed = JSON.parse(decodeURIComponent(jsonStr));
          if (Array.isArray(parsed) && parsed.every(isModelQueryShape)) {
            requires = parsed as ModelQuery[];
          }
        } catch {
          // Ignore parse errors — leave requires unset rather than crash
        }
      }

      // Get per-state `semanticCheck` hint (optional). Non-empty-string guard.
      const semanticCheckLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://semanticCheck"
      );
      let semanticCheck: string | undefined;
      if (semanticCheckLink) {
        try {
          const decoded = Literal.fromUrl(semanticCheckLink.target).get();
          if (typeof decoded === "string" && decoded.length > 0) {
            semanticCheck = decoded;
          }
        } catch {
          // Ignore parse errors — leave unset.
        }
      }

      // Get per-state `consensusRule` override (optional). Same shape
      // guard as the flow-level rule; a malformed literal (missing `n`,
      // non-integer, invalid `fromRole`) leaves the field unset.
      const stateConsensusLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://consensusRule"
      );
      let consensusRule: ConsensusRule | undefined;
      if (stateConsensusLink) {
        try {
          const jsonStr = stateConsensusLink.target.replace(
            /^literal:\/\/string:|^literal:string:/,
            ""
          );
          const parsed = JSON.parse(decodeURIComponent(jsonStr));
          if (isConsensusRuleShape(parsed)) {
            consensusRule = parsed;
          }
        } catch {
          // Ignore parse errors — leave unset.
        }
      }

      flow.addState({
        name: stateName,
        value: stateValue,
        ...(interpretationHint !== undefined ? { interpretationHint } : {}),
        ...(requires !== undefined ? { requires } : {}),
        ...(semanticCheck !== undefined ? { semanticCheck } : {}),
        ...(consensusRule !== undefined ? { consensusRule } : {}),
      });
    }

    // Perspective link storage does not preserve insertion order — `hasState`
    // links come back in arbitrary order from the graph, so `flow.states[0]`
    // was randomly one of the round-tripped states. Sort by `value` ascending
    // so the "initial state = states[0]" convention (used by
    // `FlowInstance.start`) survives a fromGraph round trip.
    // The `states` getter returns a defensive copy, so mutate `_states` directly.
    flow._states.sort((a, b) => a.value - b.value);

    // Find transitions
    const transitionLinks = links.filter(l =>
      l.source === flowUri && l.predicate === "ad4m://hasTransition"
    );
    
    for (const transitionLink of transitionLinks) {
      const transitionUri = transitionLink.target;
      
      // Get action name
      const actionNameLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://actionName"
      );
      const actionName = actionNameLink ? Literal.fromUrl(actionNameLink.target).get() as string : "";
      
      // Get from state
      const fromStateLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://fromState"
      );
      const fromStateUri = fromStateLink?.target || "";
      const fromState = stateUriToName.get(fromStateUri) || "";
      
      // Get to state
      const toStateLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://toState"
      );
      const toStateUri = toStateLink?.target || "";
      const toState = stateUriToName.get(toStateUri) || "";
      
      // Get actions
      const actionsLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://transitionActions"
      );
      let actions: AD4MAction[] = [];
      if (actionsLink) {
        try {
          const jsonStr = actionsLink.target.replace(/^literal:\/\/string:|^literal:string:/, '');
          actions = JSON.parse(decodeURIComponent(jsonStr));
        } catch {
          // Ignore parse errors
        }
      }
      
      flow.addTransition({ actionName, fromState, toState, actions });
    }

    return flow;
  }

  /**
   * Convert to JSON representation
   */
  toJSON(): object {
    // Per-state metadata is optional; strip empty strings / empty arrays so
    // json.states[i] agrees with what toLinks() would materialise (a state
    // constructed with `interpretationHint: ""` produces no predicate, so
    // JSON must not carry the empty field either).
    const sanitizedStates = this._states.map(s => ({
      name: s.name,
      value: s.value,
      ...(s.interpretationHint ? { interpretationHint: s.interpretationHint } : {}),
      ...(s.requires && s.requires.length > 0 ? { requires: s.requires } : {}),
      ...(s.semanticCheck ? { semanticCheck: s.semanticCheck } : {}),
      ...(s.consensusRule ? { consensusRule: s.consensusRule } : {})
    }));
    return {
      name: this.name,
      namespace: this.namespace,
      states: sanitizedStates,
      transitions: this._transitions,
      // Empty strings / empty arrays treated as unset — same semantics
      // as the toLinks path, so JSON <-> links round-trips agree.
      ...(this.interpretationHint ? { interpretationHint: this.interpretationHint } : {}),
      ...(this.inputTypes.length > 0 ? { inputTypes: this.inputTypes } : {}),
      ...(this.outputTypes.length > 0 ? { outputTypes: this.outputTypes } : {}),
      ...(this.creationHint ? { creationHint: this.creationHint } : {}),
      ...(this.context && this.context.length > 0 ? { context: this.context } : {}),
      ...(this.consensusRule ? { consensusRule: this.consensusRule } : {})
    };
  }

  /**
   * Create from JSON representation
   */
  static fromJSON(json: any): SHACLFlow {
    const flow = new SHACLFlow(json.name, json.namespace);
    // Non-empty-string guard — mirrors the toLinks-side reader.
    if (typeof json.interpretationHint === "string" && json.interpretationHint.length > 0) {
      flow.interpretationHint = json.interpretationHint;
    }
    // Typed I/O (design §4.1). Same string[]/ModelQuery[] shape guards
    // as the fromLinks path — malformed payloads leave defaults untouched.
    if (
      Array.isArray(json.inputTypes) &&
      json.inputTypes.every((v: unknown) => typeof v === "string" && v.length > 0)
    ) {
      flow.inputTypes = json.inputTypes as string[];
    }
    if (
      Array.isArray(json.outputTypes) &&
      json.outputTypes.every((v: unknown) => typeof v === "string" && v.length > 0)
    ) {
      flow.outputTypes = json.outputTypes as string[];
    }
    if (typeof json.creationHint === "string" && json.creationHint.length > 0) {
      flow.creationHint = json.creationHint;
    }
    if (Array.isArray(json.context) && json.context.every(isModelQueryShape)) {
      flow.context = json.context as ModelQuery[];
    }
    if (isConsensusRuleShape(json.consensusRule)) {
      flow.consensusRule = json.consensusRule as ConsensusRule;
    }
    for (const state of json.states || []) {
      // Same validation as the toLinks path: reject malformed optional fields
      // rather than let them poison downstream consumers.
      const sanitized: FlowState = {
        name: state.name,
        value: state.value,
      };
      if (typeof state.interpretationHint === "string" && state.interpretationHint.length > 0) {
        sanitized.interpretationHint = state.interpretationHint;
      }
      if (Array.isArray(state.requires) && state.requires.every(isModelQueryShape)) {
        sanitized.requires = state.requires as ModelQuery[];
      }
      if (typeof state.semanticCheck === "string" && state.semanticCheck.length > 0) {
        sanitized.semanticCheck = state.semanticCheck;
      }
      if (isConsensusRuleShape(state.consensusRule)) {
        sanitized.consensusRule = state.consensusRule as ConsensusRule;
      }
      flow.addState(sanitized);
    }
    for (const transition of json.transitions || []) {
      flow.addTransition(transition);
    }
    return flow;
  }
}
