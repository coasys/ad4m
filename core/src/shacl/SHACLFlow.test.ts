import { SHACLFlow, FlowState, FlowTransition, AD4MAction } from './SHACLFlow';

describe('SHACLFlow', () => {
  describe('basic construction', () => {
    it('creates a flow with name and namespace', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      expect(flow.name).toBe('TODO');
      expect(flow.namespace).toBe('todo://');
      expect(flow.flowUri).toBe('todo://TODOFlow');
    });

    it('generates correct state URIs', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      expect(flow.stateUri('ready')).toBe('todo://TODO.ready');
      expect(flow.stateUri('done')).toBe('todo://TODO.done');
    });

    it('generates correct transition URIs', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      expect(flow.transitionUri('ready', 'doing')).toBe('todo://TODO.readyTodoing');
    });
  });

  describe('state management', () => {
    it('adds and retrieves states', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      
      flow.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });
      
      flow.addState({
        name: 'done',
        value: 1,
        stateCheck: { predicate: 'todo://state', target: 'todo://done' }
      });
      
      expect(flow.states.length).toBe(2);
      expect(flow.states[0].name).toBe('ready');
      expect(flow.states[1].name).toBe('done');
    });
  });

  describe('transition management', () => {
    it('adds and retrieves transitions', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      
      flow.addTransition({
        actionName: 'Complete',
        fromState: 'ready',
        toState: 'done',
        actions: [
          { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://done' },
          { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
        ]
      });
      
      expect(flow.transitions.length).toBe(1);
      expect(flow.transitions[0].actionName).toBe('Complete');
      expect(flow.transitions[0].actions.length).toBe(2);
    });
  });

  describe('toLinks()', () => {
    it('serializes flow to links', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.flowable = 'any';
      flow.startAction = [
        { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
      ];
      
      flow.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });
      
      flow.addTransition({
        actionName: 'Start',
        fromState: 'ready',
        toState: 'doing',
        actions: [{ action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' }]
      });
      
      const links = flow.toLinks();
      
      // Check flow type link
      const typeLink = links.find(l => l.predicate === 'rdf://type' && l.target === 'ad4m://Flow');
      expect(typeLink).toBeDefined();
      expect(typeLink!.source).toBe('todo://TODOFlow');
      
      // Check flowable link
      const flowableLink = links.find(l => l.predicate === 'ad4m://flowable');
      expect(flowableLink).toBeDefined();
      expect(flowableLink!.target).toBe('ad4m://any');
      
      // Check start action link
      const startActionLink = links.find(l => l.predicate === 'ad4m://startAction');
      expect(startActionLink).toBeDefined();
      expect(startActionLink!.target).toContain('addLink');
      
      // Check state link
      const stateLink = links.find(l => l.predicate === 'ad4m://hasState');
      expect(stateLink).toBeDefined();
      expect(stateLink!.target).toBe('todo://TODO.ready');
      
      // Check transition link
      const transitionLink = links.find(l => l.predicate === 'ad4m://hasTransition');
      expect(transitionLink).toBeDefined();
    });
  });

  describe('fromLinks()', () => {
    it('reconstructs flow from links', () => {
      const original = new SHACLFlow('TODO', 'todo://');
      original.flowable = 'any';
      original.startAction = [
        { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
      ];
      original.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });
      original.addState({
        name: 'done',
        value: 1,
        stateCheck: { predicate: 'todo://state', target: 'todo://done' }
      });
      original.addTransition({
        actionName: 'Complete',
        fromState: 'ready',
        toState: 'done',
        actions: [{ action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://done' }]
      });
      
      const links = original.toLinks();
      const reconstructed = SHACLFlow.fromLinks(links, 'todo://TODOFlow');
      
      expect(reconstructed.name).toBe('TODO');
      expect(reconstructed.namespace).toBe('todo://');
      expect(reconstructed.flowable).toBe('any');
      expect(reconstructed.startAction.length).toBe(1);
      expect(reconstructed.states.length).toBe(2);
      expect(reconstructed.transitions.length).toBe(1);
      expect(reconstructed.transitions[0].actionName).toBe('Complete');
    });
  });

  describe('JSON serialization', () => {
    it('converts to and from JSON', () => {
      const original = new SHACLFlow('TODO', 'todo://');
      original.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });
      original.addTransition({
        actionName: 'Start',
        fromState: 'ready',
        toState: 'doing',
        actions: []
      });
      
      const json = original.toJSON();
      const reconstructed = SHACLFlow.fromJSON(json);
      
      expect(reconstructed.name).toBe('TODO');
      expect(reconstructed.states.length).toBe(1);
      expect(reconstructed.transitions.length).toBe(1);
    });
  });

  describe('full TODO example', () => {
    it('creates complete TODO flow matching Prolog example', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.flowable = 'any';
      
      // Start action - renders expression as TODO in 'ready' state
      flow.startAction = [
        { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
      ];
      
      // Three states
      flow.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });
      flow.addState({
        name: 'doing',
        value: 0.5,
        stateCheck: { predicate: 'todo://state', target: 'todo://doing' }
      });
      flow.addState({
        name: 'done',
        value: 1,
        stateCheck: { predicate: 'todo://state', target: 'todo://done' }
      });
      
      // Transitions
      flow.addTransition({
        actionName: 'Start',
        fromState: 'ready',
        toState: 'doing',
        actions: [
          { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' },
          { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
        ]
      });
      flow.addTransition({
        actionName: 'Finish',
        fromState: 'doing',
        toState: 'done',
        actions: [
          { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://done' },
          { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://doing' }
        ]
      });
      
      // Verify structure
      expect(flow.states.length).toBe(3);
      expect(flow.transitions.length).toBe(2);
      
      // Verify links generation
      const links = flow.toLinks();
      expect(links.length).toBeGreaterThan(15); // Flow + 3 states + 2 transitions = many links
      
      // Verify round-trip
      const reconstructed = SHACLFlow.fromLinks(links, flow.flowUri);
      expect(reconstructed.states.length).toBe(3);
      expect(reconstructed.transitions.length).toBe(2);
    });
  });
});
