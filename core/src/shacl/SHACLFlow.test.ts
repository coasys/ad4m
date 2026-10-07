import { SHACLFlow, FlowState, FlowTransition, AD4MAction, ConsensusRule } from './SHACLFlow';
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
      expect(flow.transitionUri('ready', 'doing', 'Start')).toBe('todo://TODO.transition/ready/doing/Start');
    });

    it('encodes transition URI parts like the executor flow writer', () => {
      // Same fixture as `transition_uri_encodes_parts_like_the_sdk` in
      // rust-executor/src/perspectives/shacl_parser.rs.
      const flow = new SHACLFlow('TODO', 'todo://');
      expect(flow.transitionUri("in review", "a/b", "Fast-track!*'()~._"))
        .toBe("todo://TODO.transition/in%20review/a%2Fb/Fast-track%21%2A%27%28%29~._");
    });
  });

  describe('state management', () => {
    it('adds and retrieves states', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      
      flow.addState({
        name: 'ready',
        value: 0,
      });
      
      flow.addState({
        name: 'done',
        value: 1,
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
      flow.addState({
        name: 'ready',
        value: 0,
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
      
      // `flowable` retired (design §4.1) — the flow serializer must not emit
      // any `ad4m://flowable` link. `inputTypes` is the successor field and
      // gets exercised separately.
      const flowableLink = links.find(l => l.predicate === 'ad4m://flowable');
      expect(flowableLink).toBeUndefined();
      
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
      original.addState({
        name: 'ready',
        value: 0,
      });
      original.addState({
        name: 'done',
        value: 1,
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
      expect(reconstructed.states.length).toBe(2);
      expect(reconstructed.transitions.length).toBe(1);
      expect(reconstructed.transitions[0].actionName).toBe('Complete');
    });

    it('sorts states by value so states[0] is the lowest-value (initial) state after a graph round-trip', () => {
      // Perspective link storage does not preserve insertion order — `hasState`
      // links come back arbitrarily, which used to break the "initial state
      // = states[0]" convention that `FlowInstance.start`
      // relies on to mint the first state of a fresh instance.
      const original = new SHACLFlow('Delivery', 'ns://');
      original.addState({ name: 'Identified', value: 0 });
      original.addState({ name: 'InProgress', value: 1 });
      original.addState({ name: 'Done', value: 2 });

      const links = original.toLinks();
      // Simulate perspective returning `hasState` links out of order (reverse).
      const hasStateLinks = links.filter(l => l.predicate === 'ad4m://hasState');
      const otherLinks = links.filter(l => l.predicate !== 'ad4m://hasState');
      const shuffled = [...otherLinks, ...hasStateLinks.slice().reverse()];

      const reconstructed = SHACLFlow.fromLinks(shuffled, 'ns://DeliveryFlow');
      expect(reconstructed.states.length).toBe(3);
      expect(reconstructed.states[0].name).toBe('Identified');
      expect(reconstructed.states[1].name).toBe('InProgress');
      expect(reconstructed.states[2].name).toBe('Done');
      expect(reconstructed.states[0].value).toBe(0);
    });

    // #1202 — two states tied on the lowest `value` must yield the same
    // initial state no matter which `hasState` link the graph hands back
    // first. A stable sort on `value` alone keeps discovery order for ties,
    // so two replicas could mint instances in different genesis states.
    // The tie-break key is the state name (ascending, code-point order) —
    // the same key `parse_flow_from_links` uses in rust-executor, so the
    // concrete winner is pinned, not just "some fixed state".
    it('breaks a lowest-value tie by state name regardless of hasState link order (#1202)', () => {
      const fromLinksWithStatesInOrder = (order: Array<[string, number]>) => {
        const flow = new SHACLFlow('Tie', 'tie://');
        for (const [name, value] of order) flow.addState({ name, value });
        // `toLinks` emits `hasState` links in `addState` order, and
        // `fromLinks` discovers states in `hasState` link order.
        return SHACLFlow.fromLinks(flow.toLinks(), flow.flowUri);
      };

      // `review` and `draft` tie at the lowest value; `done` is above both.
      const reviewFirst = fromLinksWithStatesInOrder([['review', 0], ['draft', 0], ['done', 1]]);
      const draftFirst = fromLinksWithStatesInOrder([['draft', 0], ['review', 0], ['done', 1]]);

      expect(reviewFirst.states[0].name).toBe(draftFirst.states[0].name);
      expect(reviewFirst.states[0].name).toBe('draft');
      expect(reviewFirst.states.map(s => s.name)).toEqual(['draft', 'review', 'done']);
      expect(draftFirst.states.map(s => s.name)).toEqual(['draft', 'review', 'done']);
    });

    // Mirrors `parse_flow_from_links_sorts_nan_state_values_last` in
    // rust-executor. `toLinks` writes a NaN value as `number:NaN`, which
    // `decodeStateValue` reads back as NaN (it is outside the grammar).
    // `a.value - b.value` is NaN for such a pair, which
    // `Array.prototype.sort` treats as engine-defined rather than "last",
    // so the finite states could land in any relative order too — and
    // `states[0]` is the initial state.
    it('sorts a NaN-valued state last so it never becomes the initial state', () => {
      const flow = new SHACLFlow('Nan', 'nan://');
      flow.addState({ name: 'done', value: 1 });
      flow.addState({ name: 'broken', value: NaN });
      flow.addState({ name: 'identified', value: 0 });

      const reconstructed = SHACLFlow.fromLinks(flow.toLinks(), flow.flowUri);

      expect(reconstructed.states.some(s => Number.isNaN(s.value))).toBe(true);
      expect(reconstructed.states.map(s => s.name)).toEqual(['identified', 'done', 'broken']);
    });

    // Builds a flow's links by hand so the `stateValue` target can be any
    // string (`Literal.from(-0).toUrl()` writes `0`, and `toLinks` cannot
    // write `-inf` or `abc` at all). `target === undefined` is a state with
    // no `stateValue` link.
    const flowLinksWithStates = (
      flowUri: string,
      states: Array<[string, string | undefined]>,
      reverse: boolean,
    ) => {
      const links: Array<{ source: string; predicate: string; target: string }> = [
        { source: flowUri, predicate: 'rdf://type', target: 'ad4m://Flow' },
        { source: flowUri, predicate: 'ad4m://flowName', target: Literal.from('X').toUrl() },
        { source: flowUri, predicate: 'ad4m://namespace', target: Literal.from('xrt://').toUrl() },
      ];
      const ordered = states.map((s, i) => [i, s] as const);
      if (reverse) ordered.reverse();
      for (const [i, [name, target]] of ordered) {
        const stateUri = `xrt://X.s${i}`;
        links.push({ source: flowUri, predicate: 'ad4m://hasState', target: stateUri });
        links.push({ source: stateUri, predicate: 'ad4m://stateName', target: Literal.from(name).toUrl() });
        if (target !== undefined) {
          links.push({ source: stateUri, predicate: 'ad4m://stateValue', target });
        }
      }
      return links;
    };

    // The `stateValue` decoder, payload by payload. **Same rows, same
    // expectations as `state_value_decoder_matches_ts` in
    // `rust-executor/src/perspectives/shacl_parser.rs`**; a row added here
    // is added there. `parseFloat` (the old decoder) would answer 1 on
    // `1abc` and `1e%2B21`, and NaN on `inf`; Rust's `str::parse` the
    // reverse — the grammar is explicit so neither parser's quirks leak
    // into the initial-state choice. Observed through `fromLinks` on a
    // one-state flow because the decoder is module-private.
    it('state value decoder matches rust', () => {
      const rows: Array<[string, number]> = [
        // plain decimals, both parsers agree on the value
        ['literal:number:0', 0],
        ['literal:number:-0', -0],
        ['literal:number:1', 1],
        ['literal:number:1.0', 1],
        ['literal:number:01', 1],
        ['literal:number:1.', 1],
        ['literal:number:.5', 0.5],
        ['literal:number:+1', 1],
        ['literal:number:-2.5', -2.5],
        ['literal:number:1e5', 100000],
        ['literal:number:1E5', 100000],
        ['literal:number:1e-7', 1e-7],
        ['literal:number:1e21', 1e21],
        // what `Literal.toUrl` writes for 1e21, and what Rust writes
        ['literal:number:1e%2B21', 1e21],
        ['literal:number:1000000000000000000000', 1e21],
        // legacy prefix
        ['literal://number:2', 2],
        // infinities: TS spelling, Rust spelling, any ASCII case
        ['literal:number:Infinity', Infinity],
        ['literal:number:-Infinity', -Infinity],
        ['literal:number:inf', Infinity],
        ['literal:number:-inf', -Infinity],
        ['literal:number:INF', Infinity],
        ['literal:number:+infinity', Infinity],
        // outside the grammar: NaN, never 0
        ['literal:number:NaN', NaN],
        ['literal:number:nan', NaN],
        ['literal:number:', NaN],
        ['literal:number:abc', NaN],
        ['literal:number:1abc', NaN],
        ['literal:number:%201', NaN],
        ['literal:number:1%20', NaN],
        ['literal:number:0x10', NaN],
        ['literal:number:1_000', NaN],
        ['literal:number:1e', NaN],
        ['literal:number:.', NaN],
        ['literal:number:\u0661', NaN], // ARABIC-INDIC DIGIT ONE
        ['literal:number:%zz', NaN],
        ['literal:number:%E2%82', NaN], // truncated UTF-8 sequence
        // not a number literal at all
        ['literal:string:1', NaN],
        ['ad4m://x', NaN],
      ];
      for (const [target, want] of rows) {
        const flow = SHACLFlow.fromLinks(flowLinksWithStates('xrt://XFlow', [['s', target]], false), 'xrt://XFlow');
        const got = flow.states[0].value;
        // `Object.is` separates -0 from 0 and NaN from NaN-as-equal, which
        // `toBe` (also `Object.is`) does too; the message names the row.
        expect([target, got]).toEqual([target, want]);
        expect(Object.is(got, want)).toBe(true);
      }
    });

    // Cross-runtime initial-state table (lifted from Marvin's #1204 review
    // scratch and extended). **Same rows, same winners as
    // `initial_state_table_matches_ts` in
    // `rust-executor/src/perspectives/shacl_parser.rs`**; a row added here
    // is added there. Each row is parsed with the `hasState` links in both
    // orders and must give the same initial state either way. Rows that
    // exist to kill a specific comparator mutation say so.
    it('initial state table matches rust', () => {
      const n = (payload: string) => `literal:number:${payload}`;
      const rows: Array<[string, Array<[string, string | undefined]>, string]> = [
        ['tie', [['review', n('0')], ['draft', n('0')], ['done', n('1')]], 'draft'],
        // A `<`/`>` value compare with `Object.is` semantics, or Rust
        // `total_cmp`, orders -0 before 0 and would pick `b`.
        ['negzero', [['b', n('-0')], ['a', n('0')]], 'a'],
        ['nan_nan', [['z', n('NaN')], ['y', n('NaN')]], 'y'],
        ['nan_finite', [['a', n('NaN')], ['b', n('5')]], 'b'],
        ['neg_infinity_ts_spelling', [['b', n('0')], ['a', n('-Infinity')]], 'a'],
        ['neg_infinity_rust_spelling', [['start', n('0')], ['sink', n('-inf')]], 'sink'],
        ['pos_infinity', [['a', n('Infinity')], ['b', n('0')]], 'b'],
        ['forms', [['x', n('1')], ['w', n('1.0')], ['v', n('01')]], 'v'],
        // U+1F600 is above the BMP; U+FFFD is in it but above U+D800. Code
        // point order (`compareCodePoints`, Rust bytes) puts U+FFFD first;
        // UTF-16 code-unit order (plain `<`) would pick the emoji.
        ['non_bmp', [['\u{1F600}', n('0')], ['\u{FFFD}', n('0')]], '\u{FFFD}'],
        ['non_numeric', [['junk', n('abc')], ['start', n('1')]], 'start'],
        ['trailing_garbage', [['start', n('0.5')], ['x', n('1abc')]], 'start'],
        ['empty_payload', [['a', n('1')], ['b', n('')]], 'a'],
        ['missing_link', [['b', n('1')], ['a', undefined]], 'a'],
        // `1e%2B21` must decode to a finite 1e21 (not NaN) to beat NaN.
        ['encoded_exponent', [['a', n('1e%2B21')], ['b', n('NaN')]], 'a'],
        ['hex', [['a', n('0x10')], ['b', n('1')]], 'b'],
        ['wrong_literal_type', [['a', 'literal:string:0'], ['b', n('1')]], 'b'],
      ];
      for (const [name, states, want] of rows) {
        for (const reverse of [false, true]) {
          const flow = SHACLFlow.fromLinks(flowLinksWithStates('xrt://XFlow', states, reverse), 'xrt://XFlow');
          expect([name, reverse, flow.states[0].name]).toEqual([name, reverse, want]);
        }
      }
    });
  });

  describe('JSON serialization', () => {
    it('converts to and from JSON', () => {
      const original = new SHACLFlow('TODO', 'todo://');
      original.addState({
        name: 'ready',
        value: 0,
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

      // Start action - renders expression as TODO in 'ready' state
      // Three states
      flow.addState({
        name: 'ready',
        value: 0,
      });
      flow.addState({
        name: 'doing',
        value: 0.5,
      });
      flow.addState({
        name: 'done',
        value: 1,
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
      flow.interpretationHint =
        'Tracks a group deliberation from initial proposal to shared understanding.';

      flow.addState({
        name: 'Proposal',
        value: 0,
        interpretationHint:
          'The initial proposal or question has been raised. No distinct perspective or objection has been voiced yet.'
      });
      flow.addState({
        name: 'Tension',
        value: 1,
        interpretationHint:
          'Participants have expressed opposing views or objections — a clear disagreement is on the table.'
      });
      // A state deliberately without an interpretationHint stays hint-free after round-trip.
      flow.addState({
        name: 'Resolution',
        value: 2,
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
      // Minimal state — no v1 predicates land on the state URI.
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.addState({
        name: 'ready',
        value: 0,
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
      // Same rule must apply to json.states[i] so JSON and toLinks agree on
      // "unset" (CodeRabbit PR #929 follow-up on lines 891-907).
      const flow = new SHACLFlow('Empty', 'ns://empty/');
      flow.interpretationHint = '';
      flow.addState({
        name: 'x',
        value: 0,
        interpretationHint: '',
        semanticCheck: '',
        requires: [],
      });

      const links = flow.toLinks();
      const hintLinks = links.filter(l => l.predicate === 'ad4m://interpretationHint');
      const semanticCheckLinks = links.filter(l => l.predicate === 'ad4m://semanticCheck');
      const requiresLinks = links.filter(l => l.predicate === 'ad4m://requires');
      expect(hintLinks).toHaveLength(0);
      expect(semanticCheckLinks).toHaveLength(0);
      expect(requiresLinks).toHaveLength(0);

      const json = flow.toJSON() as any;
      expect('interpretationHint' in json).toBe(false);
      // json.states[0] must strip the same optional fields as toLinks does.
      expect('interpretationHint' in json.states[0]).toBe(false);
      expect('semanticCheck' in json.states[0]).toBe(false);
      expect('requires' in json.states[0]).toBe(false);
      // Required fields still present.
      expect(json.states[0].name).toBe('x');
      expect(json.states[0].value).toBe(0);
    });

    it('malformed decoded literals leave the field unset (fromLinks)', () => {
      // Simulate a broken graph: the ad4m://interpretationHint link points at
      // a target that decodes to a non-string via Literal.get(). The reader
      // must not blindly `as string`-cast it into flow metadata.
      const flow = new SHACLFlow('Deliberation', 'ns://deliberation/');
      flow.addState({
        name: 'Proposal',
        value: 0,
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
        interpretationHint: '',
        states: [
          {
            name: 's',
            value: 0,
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

  describe('consensusRule (design §7)', () => {
    it('round-trips a flow-level consensusRule via toLinks/fromLinks', () => {
      const like = new SHACLFlow('Like', 'we://');
      like.inputTypes = ['we://Post'];
      like.outputTypes = ['we://Like'];
      like.consensusRule = { n: 1 };

      const links = like.toLinks();
      const consensusLink = links.find(
        l => l.source === like.flowUri && l.predicate === 'ad4m://consensusRule'
      );
      expect(consensusLink).toBeDefined();

      const roundTripped = SHACLFlow.fromLinks(links, like.flowUri);
      expect(roundTripped.consensusRule).toEqual({ n: 1 });
    });

    it('round-trips a per-state consensusRule override via toLinks/fromLinks', () => {
      const delib = new SHACLFlow('Delib', 'coasys://');
      delib.addState({
        name: 'perspective',
        value: 0.25,
        consensusRule: { n: 1 },
      });
      delib.addState({
        name: 'resolution',
        value: 1,
        consensusRule: {
          n: 2,
          fromRole: {
            className: 'coasys://Reviewer',
            where: { forTask: '$flow.base' },
            didProperty: 'agent',
          },
        },
      });

      const links = delib.toLinks();
      const roundTripped = SHACLFlow.fromLinks(links, delib.flowUri);
      expect(roundTripped.states[0].consensusRule).toEqual({ n: 1 });
      expect(roundTripped.states[1].consensusRule).toEqual({
        n: 2,
        fromRole: {
          className: 'coasys://Reviewer',
          where: { forTask: '$flow.base' },
          didProperty: 'agent',
        },
      });
    });

    it('round-trips flow-level + per-state consensusRule via toJSON/fromJSON', () => {
      const flow = new SHACLFlow('Delib', 'coasys://');
      flow.consensusRule = { n: 3 };
      flow.addState({
        name: 'overlap',
        value: 0.75,
        consensusRule: {
          n: 1,
          fromRole: {
            or: [
              { className: 'coasys://Reviewer', didProperty: 'agent' },
              { className: 'coasys://Admin', didProperty: 'agent' },
            ],
            className: 'coasys://Facilitator',
            didProperty: 'agent',
          },
        },
      });

      const json = JSON.parse(JSON.stringify(flow.toJSON()));
      const roundTripped = SHACLFlow.fromJSON(json);
      expect(roundTripped.consensusRule).toEqual({ n: 3 });
      expect(roundTripped.states[0].consensusRule?.n).toBe(1);
      expect(roundTripped.states[0].consensusRule?.fromRole?.or?.length).toBe(2);
      expect(roundTripped.states[0].consensusRule?.fromRole?.didProperty).toBe('agent');
    });

    it('omits consensusRule from toLinks / toJSON when unset (backwards-compatible)', () => {
      const flow = new SHACLFlow('Bare', 'ns://');
      flow.addState({
        name: 'ready',
        value: 0,
      });

      const links = flow.toLinks();
      expect(
        links.find(l => l.predicate === 'ad4m://consensusRule')
      ).toBeUndefined();

      const json = flow.toJSON() as Record<string, unknown>;
      expect(json.consensusRule).toBeUndefined();
      const states = json.states as Array<Record<string, unknown>>;
      expect(states[0].consensusRule).toBeUndefined();
    });

    it('rejects malformed consensusRule (missing n, non-integer, bad fromRole) — fromLinks', () => {
      const flowUri = 'ns://BadFlow';
      const stateUri = 'ns://BadFlow.bad';
      const badLinks: Link[] = [
        { source: flowUri, predicate: 'rdf://type', target: 'ad4m://Flow' },
        { source: flowUri, predicate: 'ad4m://flowName', target: Literal.from('BadFlow').toUrl() },
        // Flow-level: missing `n`
        {
          source: flowUri,
          predicate: 'ad4m://consensusRule',
          target: `literal:string:${encodeURIComponent(JSON.stringify({ fromRole: { className: 'ns://R' } }))}`,
        },
        { source: flowUri, predicate: 'ad4m://hasState', target: stateUri },
        { source: stateUri, predicate: 'rdf://type', target: 'ad4m://FlowState' },
        { source: stateUri, predicate: 'ad4m://stateName', target: Literal.from('bad').toUrl() },
        { source: stateUri, predicate: 'ad4m://stateValue', target: Literal.from(0).toUrl() },
        {
          source: stateUri,
          predicate: 'ad4m://stateCheck',
          target: `literal:string:${encodeURIComponent(JSON.stringify({ predicate: 'p', target: 't' }))}`,
        },
        // State-level: non-integer n + junk fromRole
        {
          source: stateUri,
          predicate: 'ad4m://consensusRule',
          target: `literal:string:${encodeURIComponent(JSON.stringify({ n: 'many', fromRole: null }))}`,
        },
      ];

      const flow = SHACLFlow.fromLinks(badLinks, flowUri);
      expect(flow.consensusRule).toBeUndefined();
      expect(flow.states[0].consensusRule).toBeUndefined();
    });

    it('rejects malformed consensusRule via fromJSON (n as string, fractional n, fromRole missing className)', () => {
      const dodgyJson = {
        name: 'BadFlow',
        namespace: 'ns://',
        consensusRule: { n: 'many' },
        states: [
          {
            name: 'a',
            value: 0,
            consensusRule: { n: 1.5 },
          },
          {
            name: 'b',
            value: 1,
            consensusRule: { n: 1, fromRole: { where: { x: 1 } } },
          },
        ],
        transitions: [],
      };

      const flow = SHACLFlow.fromJSON(dodgyJson as any);
      expect(flow.consensusRule).toBeUndefined();
      expect(flow.states[0].consensusRule).toBeUndefined();
      expect(flow.states[1].consensusRule).toBeUndefined();
    });

    it('rejects malformed `or` branches inside a role query (recursive isModelQueryShape) — fromLinks', () => {
      const flowUri = 'ns://BadOrRoleFlow';
      const stateUri = 'ns://BadOrRoleFlow.gate';
      const badLinks: Link[] = [
        { source: flowUri, predicate: 'rdf://type', target: 'ad4m://Flow' },
        { source: flowUri, predicate: 'ad4m://flowName', target: Literal.from('BadOrRoleFlow').toUrl() },
        { source: flowUri, predicate: 'ad4m://hasState', target: stateUri },
        { source: stateUri, predicate: 'rdf://type', target: 'ad4m://FlowState' },
        { source: stateUri, predicate: 'ad4m://stateName', target: Literal.from('gate').toUrl() },
        { source: stateUri, predicate: 'ad4m://stateValue', target: Literal.from(0).toUrl() },
        {
          source: stateUri,
          predicate: 'ad4m://stateCheck',
          target: `literal:string:${encodeURIComponent(JSON.stringify({ predicate: 'p', target: 't' }))}`,
        },
        {
          source: stateUri,
          predicate: 'ad4m://consensusRule',
          target: `literal:string:${encodeURIComponent(
            JSON.stringify({
              n: 1,
              fromRole: {
                className: 'ns://Composed',
                or: [{ className: 'ns://Ok' }, null, { didProperty: 'agent' }],
              },
            }),
          )}`,
        },
      ];

      const flow = SHACLFlow.fromLinks(badLinks, flowUri);
      expect(flow.states[0].consensusRule).toBeUndefined();
    });

    it('rejects malformed `or` branches inside a role query (recursive isModelQueryShape) — fromJSON', () => {
      const flow = SHACLFlow.fromJSON({
        name: 'BadOrRole',
        namespace: 'ns://',
        states: [
          {
            name: 'Gate',
            value: 0,
            consensusRule: {
              n: 1,
              fromRole: {
                className: 'ns://Composed',
                or: [{ className: 'ns://Ok' }, null, { didProperty: 'agent' }],
              },
            },
          },
        ],
        transitions: [],
      } as any);
      expect(flow.states[0].consensusRule).toBeUndefined();
    });

    it('rejects a `requires` ModelQuery whose nested `or` branch is malformed', () => {
      const flow = SHACLFlow.fromJSON({
        name: 'BadRequiresOr',
        namespace: 'ns://',
        states: [
          {
            name: 'Gate',
            value: 0,
            requires: [{ className: 'ns://Root', or: [{ className: 'ns://Ok' }, null] }],
          },
        ],
        transitions: [],
      } as any);
      expect(flow.states[0].requires).toBeUndefined();
    });

    it('accepts a role query in `$did`-templated Shape 2 form (no didProperty)', () => {
      const flow = new SHACLFlow('Gated', 'ns://');
      const rule: ConsensusRule = {
        n: 1,
        fromRole: {
          className: 'ns://Reputation',
          where: { agent: '$did', score: { equals: 100 } as any },
        },
      };
      flow.consensusRule = rule;

      const roundTripped = SHACLFlow.fromLinks(flow.toLinks(), flow.flowUri);
      expect(roundTripped.consensusRule?.fromRole?.didProperty).toBeUndefined();
      expect(roundTripped.consensusRule?.fromRole?.where?.agent).toBe('$did');
    });
  });

  describe('transition URIs', () => {
    const byKey = (ts: FlowTransition[]) =>
      ts.map(t => `${t.fromState}->${t.toState}:${t.actionName}:${JSON.stringify(t.actions)}`).sort();

    const roundTrip = (flow: SHACLFlow) => SHACLFlow.fromLinks(flow.toLinks(), flow.flowUri);

    it('keeps two transitions between the same states after toLinks -> fromLinks', () => {
      const flow = new SHACLFlow('Review', 'review://');
      flow.addState({ name: 'review', value: 0 });
      flow.addState({ name: 'done', value: 1 });
      flow.addTransition({ actionName: 'Approve', fromState: 'review', toState: 'done', actions: [{ action: 'addLink', source: 'this', predicate: 'review://by', target: 'approver' }] });
      flow.addTransition({ actionName: 'Fast-track', fromState: 'review', toState: 'done', actions: [] });

      expect(byKey(roundTrip(flow).transitions)).toEqual(byKey(flow.transitions));
    });

    it('does not collide "a" -> "Tob" with "aTo" -> "b"', () => {
      const flow = new SHACLFlow('F', 'f://');
      for (const [name, value] of [['a', 0], ['Tob', 1], ['aTo', 2], ['b', 3]] as const) {
        flow.addState({ name, value });
      }
      flow.addTransition({ actionName: 'Go', fromState: 'a', toState: 'Tob', actions: [] });
      flow.addTransition({ actionName: 'Go', fromState: 'aTo', toState: 'b', actions: [] });

      expect(byKey(roundTrip(flow).transitions)).toEqual(byKey(flow.transitions));
    });
  });

  describe('initial state ordering', () => {
    it('fromJSON sorts states by value', () => {
      const flow = new SHACLFlow('TODO', 'todo://');
      flow.addState({ name: 'done', value: 1 });
      flow.addState({ name: 'ready', value: 0 });
      flow.addState({ name: 'doing', value: 0.5 });

      const fromJSON = SHACLFlow.fromJSON(flow.toJSON());
      expect(fromJSON.states.map(s => s.name)).toEqual(['ready', 'doing', 'done']);
    });
  });
});
