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
 * `where` string values: `$flow.base`, `$flow.uri`, `$did` — the engine
 * substitutes at evaluation time.
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
   * How the matched instance connects back to the flow — `"flow"` /
   * `"base"` for the two canonical anchors; the object form names a
   * predicate `via` and one of the two anchors as `to`.
   */
  linkedTo?: "flow" | "base" | { via: string; to: "flow" | "base" };
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
   * Link pattern that indicates this state.
   *
   * Legacy field kept for existing consumers; v1+ flows should express
   * "am I in this state?" via `requires` (design §4.1), which talks
   * about subject-class instances rather than raw links. `stateCheck`
   * will be retired once no production perspective still writes it.
   */
  stateCheck: LinkPattern;
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
   * no model-level guard (falls back to `stateCheck`).
   *
   * Design doc §4.1: "replaces stateCheck+guardQuery; array of queries,
   * ALL must match; each query's matches become evidence."
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
 * Flowable condition - determines which expressions can enter this flow
 * "any" means all expressions can start this flow
 * Otherwise, a link pattern to check
 */
export type FlowableCondition = "any" | LinkPattern;

/**
 * SHACL Flow - represents a state machine for AD4M expressions
 * 
 * Flows define:
 * - Which expressions can enter the flow (flowable condition)
 * - What states exist and how to detect them (via link patterns)
 * - How to transition between states (via actions)
 * 
 * @example
 * ```typescript
 * const todoFlow = new SHACLFlow('todo://TODO', 'todo://');
 * 
 * // Any expression can become a TODO
 * todoFlow.flowable = 'any';
 * 
 * // Define states
 * todoFlow.addState({
 *   name: 'ready',
 *   value: 0,
 *   stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
 * });
 * todoFlow.addState({
 *   name: 'doing', 
 *   value: 0.5,
 *   stateCheck: { predicate: 'todo://state', target: 'todo://doing' }
 * });
 * todoFlow.addState({
 *   name: 'done',
 *   value: 1,
 *   stateCheck: { predicate: 'todo://state', target: 'todo://done' }
 * });
 * 
 * // Define start action
 * todoFlow.startAction = [{
 *   action: 'addLink',
 *   source: 'this',
 *   predicate: 'todo://state',
 *   target: 'todo://ready'
 * }];
 * 
 * // Define transitions
 * todoFlow.addTransition({
 *   actionName: 'Start',
 *   fromState: 'ready',
 *   toState: 'doing',
 *   actions: [
 *     { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' },
 *     { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
 *   ]
 * });
 * 
 * // Store in perspective
 * await perspective.addFlow('TODO', todoFlow);
 * ```
 */
export class SHACLFlow {
  /** Flow name (e.g., "TODO") */
  public name: string;

  /** Namespace for generated URIs */
  public namespace: string;

  /** Condition for which expressions can start this flow */
  public flowable: FlowableCondition = "any";

  /** Actions to execute when starting the flow */
  public startAction: AD4MAction[] = [];

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
  
  /** States in this flow */
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

    // Top-level interpretation hint (optional, drives AI state suggestion)
    if (this.interpretationHint !== undefined) {
      links.push({
        source: flowUri,
        predicate: "ad4m://interpretationHint",
        target: Literal.from(this.interpretationHint).toUrl()
      });
    }

    // Flowable condition
    if (this.flowable === "any") {
      links.push({
        source: flowUri,
        predicate: "ad4m://flowable",
        target: "ad4m://any"
      });
    } else {
      links.push({
        source: flowUri,
        predicate: "ad4m://flowable",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.flowable))}`
      });
    }

    // Start action
    if (this.startAction.length > 0) {
      links.push({
        source: flowUri,
        predicate: "ad4m://startAction",
        target: `literal:string:${encodeURIComponent(JSON.stringify(this.startAction))}`
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

      // State check pattern
      links.push({
        source: stateUri,
        predicate: "ad4m://stateCheck",
        target: `literal:string:${encodeURIComponent(JSON.stringify(state.stateCheck))}`
      });

      // Per-state interpretation hint (optional, drives AI state suggestion)
      if (state.interpretationHint !== undefined) {
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
      // presence isn't sufficient evidence.
      if (state.semanticCheck !== undefined) {
        links.push({
          source: stateUri,
          predicate: "ad4m://semanticCheck",
          target: Literal.from(state.semanticCheck).toUrl()
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

    // Find top-level interpretation hint
    const flowHintLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://interpretationHint"
    );
    if (flowHintLink) {
      try {
        flow.interpretationHint = Literal.fromUrl(flowHintLink.target).get() as string;
      } catch {
        // Ignore parse errors
      }
    }

    // Find flowable condition
    const flowableLink = links.find(l => 
      l.source === flowUri && l.predicate === "ad4m://flowable"
    );
    if (flowableLink) {
      if (flowableLink.target === "ad4m://any") {
        flow.flowable = "any";
      } else {
        try {
          const jsonStr = flowableLink.target.replace(/^literal:\/\/string:|^literal:string:/, '');
          flow.flowable = JSON.parse(decodeURIComponent(jsonStr));
        } catch {
          flow.flowable = "any";
        }
      }
    }

    // Find start action
    const startActionLink = links.find(l =>
      l.source === flowUri && l.predicate === "ad4m://startAction"
    );
    if (startActionLink) {
      try {
        const jsonStr = startActionLink.target.replace(/^literal:\/\/string:|^literal:string:/, '');
        flow.startAction = JSON.parse(decodeURIComponent(jsonStr));
      } catch {
        // Ignore parse errors
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
      
      // Get state check
      const checkLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://stateCheck"
      );
      let stateCheck: LinkPattern = { predicate: "", target: "" };
      if (checkLink) {
        try {
          const jsonStr = checkLink.target.replace(/^literal:\/\/string:|^literal:string:/, '');
          stateCheck = JSON.parse(decodeURIComponent(jsonStr));
        } catch {
          // Ignore parse errors
        }
      }
      
      // Get per-state interpretation hint (optional)
      const hintLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://interpretationHint"
      );
      let interpretationHint: string | undefined;
      if (hintLink) {
        try {
          interpretationHint = Literal.fromUrl(hintLink.target).get() as string;
        } catch {
          // Ignore parse errors
        }
      }

      // Get per-state `requires` guard (optional) — one link carrying a
      // JSON-encoded ModelQuery[].
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
          if (Array.isArray(parsed)) {
            requires = parsed;
          }
        } catch {
          // Ignore parse errors — leave requires unset rather than crash
        }
      }

      // Get per-state `semanticCheck` hint (optional)
      const semanticCheckLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://semanticCheck"
      );
      let semanticCheck: string | undefined;
      if (semanticCheckLink) {
        try {
          semanticCheck = Literal.fromUrl(semanticCheckLink.target).get() as string;
        } catch {
          // Ignore parse errors
        }
      }

      flow.addState({
        name: stateName,
        value: stateValue,
        stateCheck,
        ...(interpretationHint !== undefined ? { interpretationHint } : {}),
        ...(requires !== undefined ? { requires } : {}),
        ...(semanticCheck !== undefined ? { semanticCheck } : {}),
      });
    }

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
    return {
      name: this.name,
      namespace: this.namespace,
      flowable: this.flowable,
      startAction: this.startAction,
      states: this._states,
      transitions: this._transitions,
      ...(this.interpretationHint !== undefined ? { interpretationHint: this.interpretationHint } : {})
    };
  }

  /**
   * Create from JSON representation
   */
  static fromJSON(json: any): SHACLFlow {
    const flow = new SHACLFlow(json.name, json.namespace);
    flow.flowable = json.flowable || "any";
    flow.startAction = json.startAction || [];
    if (typeof json.interpretationHint === "string") {
      flow.interpretationHint = json.interpretationHint;
    }
    for (const state of json.states || []) {
      flow.addState(state);
    }
    for (const transition of json.transitions || []) {
      flow.addTransition(transition);
    }
    return flow;
  }
}
