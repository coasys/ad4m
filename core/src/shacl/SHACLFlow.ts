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
 * Flow State definition
 * Represents a single state in the flow state machine
 */
export interface FlowState {
  /** State name (e.g., "ready", "doing", "done") */
  name: string;
  /** Numeric state value for ordering (e.g., 0, 0.5, 1) */
  value: number;
  /** Link pattern that indicates this state */
  stateCheck: LinkPattern;
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
   * These links can be stored in a perspective and queried via SurrealDB
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
        target: `literal://string:${encodeURIComponent(JSON.stringify(this.flowable))}`
      });
    }

    // Start action
    if (this.startAction.length > 0) {
      links.push({
        source: flowUri,
        predicate: "ad4m://startAction",
        target: `literal://string:${encodeURIComponent(JSON.stringify(this.startAction))}`
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
        target: `literal://string:${encodeURIComponent(JSON.stringify(state.stateCheck))}`
      });
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
        target: `literal://string:${encodeURIComponent(JSON.stringify(transition.actions))}`
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

    // Find flowable condition
    const flowableLink = links.find(l => 
      l.source === flowUri && l.predicate === "ad4m://flowable"
    );
    if (flowableLink) {
      if (flowableLink.target === "ad4m://any") {
        flow.flowable = "any";
      } else {
        try {
          const jsonStr = flowableLink.target.replace('literal://string:', '');
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
        const jsonStr = startActionLink.target.replace('literal://string:', '');
        flow.startAction = JSON.parse(decodeURIComponent(jsonStr));
      } catch {
        // Ignore parse errors
      }
    }

    // Find states
    const stateLinks = links.filter(l =>
      l.source === flowUri && l.predicate === "ad4m://hasState"
    );
    
    for (const stateLink of stateLinks) {
      const stateUri = stateLink.target;
      
      // Get state name
      const nameLink = links.find(l =>
        l.source === stateUri && l.predicate === "ad4m://stateName"
      );
      const stateName = nameLink ? Literal.fromUrl(nameLink.target).get() as string : "";
      
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
          const jsonStr = checkLink.target.replace('literal://string:', '');
          stateCheck = JSON.parse(decodeURIComponent(jsonStr));
        } catch {
          // Ignore parse errors
        }
      }
      
      flow.addState({ name: stateName, value: stateValue, stateCheck });
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
      const fromState = fromStateUri.split('.').pop() || "";
      
      // Get to state
      const toStateLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://toState"
      );
      const toStateUri = toStateLink?.target || "";
      const toState = toStateUri.split('.').pop() || "";
      
      // Get actions
      const actionsLink = links.find(l =>
        l.source === transitionUri && l.predicate === "ad4m://transitionActions"
      );
      let actions: AD4MAction[] = [];
      if (actionsLink) {
        try {
          const jsonStr = actionsLink.target.replace('literal://string:', '');
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
      transitions: this._transitions
    };
  }

  /**
   * Create from JSON representation
   */
  static fromJSON(json: any): SHACLFlow {
    const flow = new SHACLFlow(json.name, json.namespace);
    flow.flowable = json.flowable || "any";
    flow.startAction = json.startAction || [];
    for (const state of json.states || []) {
      flow.addState(state);
    }
    for (const transition of json.transitions || []) {
      flow.addTransition(transition);
    }
    return flow;
  }
}
