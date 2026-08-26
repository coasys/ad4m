import { SHACLFlow, FlowState, FlowTransition, AD4MAction } from './SHACLFlow';
import { Link } from '../links/Links';
import { Literal } from '../Literal';

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

  describe('interpretationHint (AI-driven state suggestion)', () => {
    it('round-trips top-level and per-state interpretation hints via toLinks/fromLinks', () => {
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.flowable = 'any';
      flow.interpretationHint =
        'Tracks a group deliberation from initial proposal to shared understanding.';

      flow.addState({
        name: 'Proposal',
        value: 0,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/proposal' },
        interpretationHint:
          'The initial proposal or question has been raised. No distinct perspective or objection has been voiced yet.'
      });
      flow.addState({
        name: 'Tension',
        value: 1,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/tension' },
        interpretationHint:
          'Participants have expressed opposing views or objections — a clear disagreement is on the table.'
      });
      // A state deliberately without an interpretationHint stays hint-free after round-trip.
      flow.addState({
        name: 'Resolution',
        value: 2,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/resolution' }
      });

      const links = flow.toLinks();
      const roundTripped = SHACLFlow.fromLinks(links, flow.flowUri);

      expect(roundTripped.interpretationHint).toBe(flow.interpretationHint);
      expect(roundTripped.states.find(s => s.name === 'Proposal')?.interpretationHint)
        .toBe(flow.states.find(s => s.name === 'Proposal')?.interpretationHint);
      expect(roundTripped.states.find(s => s.name === 'Tension')?.interpretationHint)
        .toBe(flow.states.find(s => s.name === 'Tension')?.interpretationHint);
      expect(roundTripped.states.find(s => s.name === 'Resolution')?.interpretationHint)
        .toBeUndefined();
    });

    it('round-trips interpretation hints via toJSON/fromJSON', () => {
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.interpretationHint = 'Top-level hint.';
      flow.addState({
        name: 'Proposal',
        value: 0,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/proposal' },
        interpretationHint: 'Per-state hint.'
      });

      const json = flow.toJSON() as any;
      expect(json.interpretationHint).toBe('Top-level hint.');
      expect(json.states[0].interpretationHint).toBe('Per-state hint.');

      const roundTripped = SHACLFlow.fromJSON(json);
      expect(roundTripped.interpretationHint).toBe('Top-level hint.');
      expect(roundTripped.states[0].interpretationHint).toBe('Per-state hint.');
    });

    it('omits interpretationHint from toJSON when unset (backwards-compatible)', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' }
      });

      const json = flow.toJSON() as any;
      expect('interpretationHint' in json).toBe(false);
      // States without a hint don't gain one — existing schema is untouched.
      expect('interpretationHint' in json.states[0]).toBe(false);
    });
  });

  describe('requires + semanticCheck on FlowState (v1 flow guards)', () => {
    it('round-trips a state with `requires` and `semanticCheck` via toLinks/fromLinks', () => {
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.addState({
        name: 'Tension',
        value: 1,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/tension' },
        interpretationHint:
          'Participants have expressed opposing views or objections.',
        // Design §4.1: model-level guard replacing raw-link stateCheck.
        // AND-combined; every entry must match on committed graph state.
        requires: [
          {
            className: 'ns://Objection',
            where: { about: '$flow.base' },
            count: { min: 1 },
            linkedTo: 'base',
          },
          {
            className: 'ns://Perspective',
            where: { about: '$flow.base', stance: { in: ['for', 'against'] } },
            count: { min: 2 },
          },
        ],
        // Design §5: LLM confirmation after `requires` structurally matches.
        semanticCheck:
          'Confirm the objection is a genuine disagreement, not a clarifying question.',
      });

      const links = flow.toLinks();
      const roundTripped = SHACLFlow.fromLinks(links, flow.flowUri);

      const tension = roundTripped.states.find(s => s.name === 'Tension');
      expect(tension).toBeDefined();
      // requires: array preserved, condition shorthands + object forms both survive.
      expect(tension?.requires).toHaveLength(2);
      expect(tension?.requires?.[0].className).toBe('ns://Objection');
      expect(tension?.requires?.[0].where).toEqual({ about: '$flow.base' });
      expect(tension?.requires?.[0].count).toEqual({ min: 1 });
      expect(tension?.requires?.[0].linkedTo).toBe('base');
      expect(tension?.requires?.[1].where).toEqual({
        about: '$flow.base',
        stance: { in: ['for', 'against'] },
      });
      expect(tension?.requires?.[1].count).toEqual({ min: 2 });
      // semanticCheck string round-trips through Literal serialization.
      expect(tension?.semanticCheck).toBe(
        'Confirm the objection is a genuine disagreement, not a clarifying question.'
      );
    });

    it('round-trips requires + semanticCheck via toJSON/fromJSON', () => {
      const flow = new SHACLFlow('Delivery', 'ns://delivery/');
      flow.addState({
        name: 'Done',
        value: 1,
        stateCheck: { predicate: 'ns://delivery/state', target: 'ns://delivery/done' },
        requires: [
          {
            className: 'ns://CompletionEvidence',
            where: { forTask: '$flow.base' },
            count: { min: 1 },
          },
        ],
        semanticCheck: 'Confirm the artifact matches what was asked for.',
      });

      const json = flow.toJSON() as any;
      const done = json.states[0];
      expect(done.requires).toHaveLength(1);
      expect(done.requires[0].className).toBe('ns://CompletionEvidence');
      expect(done.semanticCheck).toBe('Confirm the artifact matches what was asked for.');

      const roundTripped = SHACLFlow.fromJSON(json);
      const rtDone = roundTripped.states.find(s => s.name === 'Done');
      expect(rtDone?.requires?.[0].className).toBe('ns://CompletionEvidence');
      expect(rtDone?.requires?.[0].where).toEqual({ forTask: '$flow.base' });
      expect(rtDone?.semanticCheck).toBe('Confirm the artifact matches what was asked for.');
    });

    it('omits requires and semanticCheck from toLinks when unset (backwards-compatible)', () => {
      // Legacy state — only stateCheck. No new predicates land on the state URI.
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.addState({
        name: 'ready',
        value: 0,
        stateCheck: { predicate: 'todo://state', target: 'todo://ready' },
      });

      const links = flow.toLinks();
      const stateUri = flow.stateUri('ready');
      const requiresLinks = links.filter(
        l => l.source === stateUri && l.predicate === 'ad4m://requires'
      );
      const semanticCheckLinks = links.filter(
        l => l.source === stateUri && l.predicate === 'ad4m://semanticCheck'
      );
      expect(requiresLinks).toHaveLength(0);
      expect(semanticCheckLinks).toHaveLength(0);

      // Round-trip: the state's fields stay undefined, not empty objects.
      const roundTripped = SHACLFlow.fromLinks(links, flow.flowUri);
      const ready = roundTripped.states.find(s => s.name === 'ready');
      expect(ready?.requires).toBeUndefined();
      expect(ready?.semanticCheck).toBeUndefined();
    });

    it('an empty `requires` array is treated as no guard (no link emitted)', () => {
      const flow = new SHACLFlow('Empty', 'ns://empty/');
      flow.addState({
        name: 'x',
        value: 0,
        stateCheck: { predicate: 'ns://empty/state', target: 'ns://empty/x' },
        requires: [],
      });

      const links = flow.toLinks();
      const stateUri = flow.stateUri('x');
      const requiresLinks = links.filter(
        l => l.source === stateUri && l.predicate === 'ad4m://requires'
      );
      expect(requiresLinks).toHaveLength(0);
    });
  });

  describe('CodeRabbit hardening: empty-string / malformed-value tolerance', () => {
    it('empty-string interpretationHint / semanticCheck emit zero predicates', () => {
      // Empty strings must be treated as "unset" so we don't materialise an
      // empty-hint predicate that a consumer would read back as a meaningful
      // value (CodeRabbit PR #929 comment on lines 316-323 / 388-395 / 408-416).
      const flow = new SHACLFlow('Empty', 'ns://empty/');
      flow.interpretationHint = '';
      flow.addState({
        name: 'x',
        value: 0,
        stateCheck: { predicate: 'ns://empty/state', target: 'ns://empty/x' },
        interpretationHint: '',
        semanticCheck: '',
      });

      const links = flow.toLinks();
      const hintLinks = links.filter(l => l.predicate === 'ad4m://interpretationHint');
      const semanticCheckLinks = links.filter(l => l.predicate === 'ad4m://semanticCheck');
      expect(hintLinks).toHaveLength(0);
      expect(semanticCheckLinks).toHaveLength(0);

      const json = flow.toJSON() as any;
      expect('interpretationHint' in json).toBe(false);
    });

    it('malformed decoded literals leave the field unset (fromLinks)', () => {
      // Simulate a broken graph: the ad4m://interpretationHint link points at
      // a target that decodes to a non-string via Literal.get(). The reader
      // must not blindly `as string`-cast it into flow metadata.
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.addState({
        name: 'Proposal',
        value: 0,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/proposal' },
      });

      const links = flow.toLinks();
      const flowUri = flow.flowUri;
      const stateUri = flow.stateUri('Proposal');

      // Non-string literals: literal:number decodes to a real number,
      // literal:json to a real object. `Literal.get()` returns the decoded
      // typed value, so a naive `as string` cast would leak them into flow
      // metadata unless the reader validates.
      const nonStringNumber = `literal:number:${encodeURIComponent('42')}`;
      const nonStringObject = `literal:json:${encodeURIComponent(JSON.stringify({ evil: true }))}`;

      links.push({
        source: flowUri,
        predicate: 'ad4m://interpretationHint',
        target: nonStringNumber,
      });
      links.push({
        source: stateUri,
        predicate: 'ad4m://interpretationHint',
        target: nonStringObject,
      });
      links.push({
        source: stateUri,
        predicate: 'ad4m://semanticCheck',
        target: nonStringNumber,
      });

      const roundTripped = SHACLFlow.fromLinks(links, flowUri);
      const proposal = roundTripped.states.find(s => s.name === 'Proposal');
      expect(roundTripped.interpretationHint).toBeUndefined();
      expect(proposal?.interpretationHint).toBeUndefined();
      expect(proposal?.semanticCheck).toBeUndefined();
    });

    it('malformed `requires` array entries leave the guard unset (fromLinks)', () => {
      // Array.isArray alone accepts `[null]` / `[{}]` / `[42]` — a broken
      // graph should not materialise those as ModelQuery entries.
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.addState({
        name: 'Tension',
        value: 1,
        stateCheck: { predicate: 'ns://deliberation/state', target: 'ns://deliberation/tension' },
      });

      const links = flow.toLinks();
      const stateUri = flow.stateUri('Tension');

      // Case 1: array with null entry.
      const bogus1 = `literal:string:${encodeURIComponent(JSON.stringify([null]))}`;
      // Case 2: array with empty object (no className).
      const bogus2 = `literal:string:${encodeURIComponent(JSON.stringify([{}]))}`;
      // Case 3: array with entry whose className is not a string.
      const bogus3 = `literal:string:${encodeURIComponent(JSON.stringify([{ className: 42 }]))}`;
      // Case 4: array with entry whose className is empty string.
      const bogus4 = `literal:string:${encodeURIComponent(JSON.stringify([{ className: '' }]))}`;

      for (const bogus of [bogus1, bogus2, bogus3, bogus4]) {
        const brokenLinks = [
          ...links,
          { source: stateUri, predicate: 'ad4m://requires', target: bogus },
        ];
        const rt = SHACLFlow.fromLinks(brokenLinks, flow.flowUri);
        expect(rt.states.find(s => s.name === 'Tension')?.requires).toBeUndefined();
      }

      // Sanity: a well-shaped entry mixed in a valid array still round-trips.
      const good = `literal:string:${encodeURIComponent(
        JSON.stringify([{ className: 'ns://Objection', where: { about: '$flow.base' } }])
      )}`;
      const goodLinks = [
        ...links,
        { source: stateUri, predicate: 'ad4m://requires', target: good },
      ];
      const rtGood = SHACLFlow.fromLinks(goodLinks, flow.flowUri);
      expect(rtGood.states.find(s => s.name === 'Tension')?.requires).toHaveLength(1);
      expect(rtGood.states.find(s => s.name === 'Tension')?.requires?.[0].className).toBe(
        'ns://Objection'
      );
    });

    it('malformed JSON payload sanitisation (fromJSON)', () => {
      // A downstream caller reconstructing from an untrusted JSON blob must
      // get the same non-empty-string / ModelQuery-shape guard as fromLinks.
      const dodgyJson = {
        name: 'Bad',
        namespace: 'ns://bad/',
        flowable: 'any',
        startAction: [],
        interpretationHint: '',
        states: [
          {
            name: 's',
            value: 0,
            stateCheck: { predicate: 'ns://bad/state', target: 'ns://bad/s' },
            interpretationHint: '',
            requires: [null, {}, { className: 42 }],
            semanticCheck: '',
          },
        ],
        transitions: [],
      };

      const flow = SHACLFlow.fromJSON(dodgyJson);
      expect(flow.interpretationHint).toBeUndefined();
      const s = flow.states.find(x => x.name === 's');
      expect(s?.interpretationHint).toBeUndefined();
      expect(s?.requires).toBeUndefined();
      expect(s?.semanticCheck).toBeUndefined();
    });
  });

  describe('flow-level typed I/O + creationHint + context (design §4.1)', () => {
    it('round-trips inputTypes / outputTypes / creationHint / context via toLinks/fromLinks', () => {
      const flow = new SHACLFlow('Delivery', 'coasys://');
      flow.inputTypes = ['coasys://Task'];
      flow.outputTypes = ['coasys://Delivery'];
      flow.creationHint =
        'Spawn when someone commits to a concrete, actionable task.';
      flow.context = [
        {
          className: 'coasys://Actor',
          where: { did: '$did' },
          count: { min: 1 },
        },
      ];

      const links = flow.toLinks();
      const roundTripped = SHACLFlow.fromLinks(links, flow.flowUri);
      expect(roundTripped.inputTypes).toEqual(['coasys://Task']);
      expect(roundTripped.outputTypes).toEqual(['coasys://Delivery']);
      expect(roundTripped.creationHint).toBe(
        'Spawn when someone commits to a concrete, actionable task.'
      );
      expect(roundTripped.context).toEqual([
        {
          className: 'coasys://Actor',
          where: { did: '$did' },
          count: { min: 1 },
        },
      ]);
    });

    it('round-trips a zero-state action flow (Like example, §6.3)', () => {
      const like = new SHACLFlow('Like', 'we://');
      like.inputTypes = ['we://Post'];
      like.outputTypes = ['we://Like'];
      like.creationHint = 'Spawn when a user endorses or approves a post.';

      const links = like.toLinks();
      const roundTripped = SHACLFlow.fromLinks(links, like.flowUri);
      expect(roundTripped.states).toEqual([]);
      expect(roundTripped.transitions).toEqual([]);
      expect(roundTripped.inputTypes).toEqual(['we://Post']);
      expect(roundTripped.outputTypes).toEqual(['we://Like']);
      expect(roundTripped.creationHint).toBe(
        'Spawn when a user endorses or approves a post.'
      );
    });

    it('round-trips inputTypes / outputTypes / creationHint / context via toJSON/fromJSON', () => {
      const flow = new SHACLFlow('Delib', 'coasys://');
      flow.inputTypes = ['coasys://Proposal'];
      flow.outputTypes = ['coasys://Resolution'];
      flow.creationHint = 'Spawn when a proposal is put forward for discussion.';
      flow.context = [
        { className: 'coasys://Group', linkedTo: 'base' },
      ];

      const json = JSON.parse(JSON.stringify(flow.toJSON()));
      const roundTripped = SHACLFlow.fromJSON(json);
      expect(roundTripped.inputTypes).toEqual(['coasys://Proposal']);
      expect(roundTripped.outputTypes).toEqual(['coasys://Resolution']);
      expect(roundTripped.creationHint).toBe(
        'Spawn when a proposal is put forward for discussion.'
      );
      expect(roundTripped.context).toEqual([
        { className: 'coasys://Group', linkedTo: 'base' },
      ]);
    });

    it('omits the new fields from toLinks / toJSON when unset (backwards-compatible)', () => {
      const flow = new SHACLFlow('Bare', 'ns://');
      const links = flow.toLinks();
      expect(
        links.find(l => l.predicate === 'ad4m://inputTypes')
      ).toBeUndefined();
      expect(
        links.find(l => l.predicate === 'ad4m://outputTypes')
      ).toBeUndefined();
      expect(
        links.find(l => l.predicate === 'ad4m://creationHint')
      ).toBeUndefined();
      expect(
        links.find(l => l.predicate === 'ad4m://context')
      ).toBeUndefined();

      const json = flow.toJSON() as Record<string, unknown>;
      expect(json.inputTypes).toBeUndefined();
      expect(json.outputTypes).toBeUndefined();
      expect(json.creationHint).toBeUndefined();
      expect(json.context).toBeUndefined();
    });

    it('malformed inputTypes / outputTypes / context leave defaults untouched (fromLinks)', () => {
      const flowUri = 'ns://BadFlow';
      const badLinks: Link[] = [
        { source: flowUri, predicate: 'rdf://type', target: 'ad4m://Flow' },
        {
          source: flowUri,
          predicate: 'ad4m://flowName',
          target: Literal.from('BadFlow').toUrl(),
        },
        {
          source: flowUri,
          predicate: 'ad4m://inputTypes',
          target: `literal:string:${encodeURIComponent(JSON.stringify([null, 42, {}]))}`,
        },
        {
          source: flowUri,
          predicate: 'ad4m://outputTypes',
          target: `literal:string:${encodeURIComponent(JSON.stringify(['', 'coasys://Delivery']))}`,
        },
        {
          source: flowUri,
          predicate: 'ad4m://context',
          target: `literal:string:${encodeURIComponent(JSON.stringify([{ notAClassName: 'x' }]))}`,
        },
      ];

      const flow = SHACLFlow.fromLinks(badLinks, flowUri);
      expect(flow.inputTypes).toEqual([]);
      expect(flow.outputTypes).toEqual([]);
      expect(flow.context).toBeUndefined();
    });

    it('malformed inputTypes / outputTypes / creationHint / context sanitised (fromJSON)', () => {
      const dodgyJson = {
        name: 'BadFlow',
        namespace: 'ns://',
        flowable: 'any',
        startAction: [],
        inputTypes: ['coasys://Task', null, 42],
        outputTypes: 'not-an-array',
        creationHint: '',
        context: [{ className: 42 }, null],
        states: [],
        transitions: [],
      };

      const flow = SHACLFlow.fromJSON(dodgyJson);
      expect(flow.inputTypes).toEqual([]);
      expect(flow.outputTypes).toEqual([]);
      expect(flow.creationHint).toBeUndefined();
      expect(flow.context).toBeUndefined();
    });
  });
});
