import { WakerSubscriptionManager, hintFor } from './WakerSubscriptionManager';

/**
 * Regression guard for the fork that used to exist between this file and
 * plugins/ad4m/wakerSubscriptionManager.ts: the plugin copy learned to report
 * a rejected subscription and keep re-attempting it, while this copy — the one
 * `@coasys/ad4m` actually exports — still swallowed the failure and returned as
 * if the subscription were live. The plugin now re-exports this module, so
 * these tests cover both.
 */
describe('WakerSubscriptionManager', () => {
  const noopLogger = () => ({
    info: () => {},
    warn: () => {},
    error: () => {},
    debug: () => {},
  });

  const perspectiveClient = {
    querySparql: () => Promise.resolve([]),
    queryLinks: () => Promise.resolve([]),
  };

  const sub = {
    id: 'mention-locked',
    type: 'mention' as const,
    perspective: 'fake-uuid',
    channel: '',
    query: 'SELECT * WHERE { ?s ?p ?o }',
  };

  /** A QuerySubscriptionProxy stand-in whose registration the executor rejects. */
  function rejectingProxyClass(error: Error, disposed: { count: number }) {
    return function () {
      return {
        initialized: Promise.reject(error),
        subscribe: () => Promise.reject(error),
        dispose: () => {
          disposed.count += 1;
        },
        onResult: () => {},
      };
    };
  }

  it('rejects when the executor refuses the subscription, and keeps it pending', async () => {
    const disposed = { count: 0 };
    const persisted: { subs: any[] } = { subs: [{ placeholder: true }] };
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: rejectingProxyClass(
        new Error('RPC error 403: main key not found'),
        disposed,
      ),
      debounceMs: 10,
      // Stay pending for the assertion instead of racing a re-attempt.
      retryPendingMs: 60_000,
      onWake: () => {},
      onPersist: (subs) => {
        persisted.subs = subs;
      },
    });

    // Must not resolve: a silent resolve is what let the subscribe tools reply
    // "Subscribed..." to a subscription that was never registered.
    await expect(manager.subscribe(sub)).rejects.toThrow(/main key not found/);

    expect(manager.has(sub.id)).toBe(false);
    expect(manager.getActive()).toHaveLength(0);
    expect(persisted.subs).toHaveLength(0);
    expect(disposed.count).toBeGreaterThan(0);
    // ...but it is enrolled for re-attempt, because the caller asked for it.
    expect(manager.getPending().map((s) => s.id)).toEqual([sub.id]);

    manager.disposeAll();
    expect(manager.getPending()).toHaveLength(0);
  });

  /**
   * A QuerySubscriptionProxy stand-in whose handshake never answers: the
   * executor accepted the socket but nothing ever comes back (#1016).
   */
  function hangingProxyClass(
    disposed: { count: number },
    hangOn: 'subscribe' | 'initialized' = 'subscribe',
  ) {
    return function () {
      return {
        initialized:
          hangOn === 'initialized' ? new Promise<boolean>(() => {}) : Promise.resolve(true),
        subscribe: () =>
          hangOn === 'subscribe' ? new Promise<void>(() => {}) : Promise.resolve(),
        dispose: () => {
          disposed.count += 1;
        },
        onResult: () => {},
      };
    };
  }

  async function waitUntil(cond: () => boolean, ms = 2000): Promise<void> {
    const deadline = Date.now() + ms;
    while (!cond() && Date.now() < deadline) {
      await new Promise((r) => setTimeout(r, 10));
    }
  }

  it('times out a subscribe handshake the executor never answers, and keeps it pending', async () => {
    const disposed = { count: 0 };
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: hangingProxyClass(disposed),
      debounceMs: 10,
      subscribeTimeoutMs: 50,
      retryPendingMs: 60_000,
      onWake: () => {},
    });

    // Before the deadline existed this await never settled: the subscription
    // was recorded neither as active nor as pending, so the 30s re-attempt
    // never saw it and the caller's tool invocation died at the harness
    // timeout instead (#1016).
    await expect(manager.subscribe(sub)).rejects.toThrow(/timed out after 50ms/);

    expect(manager.has(sub.id)).toBe(false);
    expect(manager.getActive()).toHaveLength(0);
    expect(manager.getPending().map((s) => s.id)).toEqual([sub.id]);
    // The proxy is dropped so a late answer backs out instead of registering.
    expect(disposed.count).toBeGreaterThan(0);

    manager.disposeAll();
  });

  it('times out when subscribe() answers but initialized never settles', async () => {
    const disposed = { count: 0 };
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: hangingProxyClass(disposed, 'initialized'),
      debounceMs: 10,
      subscribeTimeoutMs: 50,
      retryPendingMs: 60_000,
      onWake: () => {},
    });

    await expect(manager.subscribe(sub)).rejects.toThrow(/timed out after 50ms/);
    expect(manager.getActive()).toHaveLength(0);
    expect(manager.getPending().map((s) => s.id)).toEqual([sub.id]);

    manager.disposeAll();
  });

  it('re-attempts a timed-out subscription and enrolls it once the executor answers', async () => {
    let attempt = 0;
    const ProxyClass = function () {
      attempt += 1;
      const answers = attempt >= 2;
      return {
        initialized: Promise.resolve(true),
        subscribe: () => (answers ? Promise.resolve() : new Promise<void>(() => {})),
        dispose: () => {},
        onResult: () => {},
      };
    };
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: ProxyClass,
      debounceMs: 10,
      subscribeTimeoutMs: 30,
      retryPendingMs: 30,
      onWake: () => {},
    });

    await expect(manager.subscribe(sub)).rejects.toThrow(/re-attempting every/);
    expect(manager.getPending()).toHaveLength(1);

    await waitUntil(() => manager.has(sub.id));
    expect(manager.has(sub.id)).toBe(true);
    expect(manager.getPending()).toHaveLength(0);
    expect(attempt).toBe(2);

    manager.disposeAll();
  });

  /** One link as `queryLinks` returns it: the triple under `data`, plus the expression fields. */
  function link(source: string, predicate: string, target = 'test://message', author = 'did:key:alice') {
    return { author, timestamp: '2026-10-09T00:00:00.000Z', data: { source, predicate, target } };
  }

  /** Delivers one new mention and returns the parents it woke with, the link queries made and any warnings. */
  async function wakeWithParentRows(rows: unknown, address = 'test://message') {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const ProxyClass = function () {
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const queries: any[] = [];
    const warnings: string[] = [];
    const client = {
      queryLinks: (_uuid: string, query: any) => {
        queries.push(query);
        return Promise.resolve(rows);
      },
    };
    const wakes: any[] = [];
    const manager = new WakerSubscriptionManager({
      perspectiveClient: client,
      logger: { ...noopLogger(), warn: (msg: string) => { warnings.push(msg); } },
      QuerySubscriptionProxy: ProxyClass,
      debounceMs: 10,
      retryPendingMs: 60_000,
      onWake: (_sub, _result, mentions) => { wakes.push(mentions); },
    });

    await manager.subscribe({ ...sub, id: 'mention-parents' });
    await deliver!([{ source: address }]);
    await waitUntil(() => wakes.length > 0);
    manager.disposeAll();
    return { wakes, queries, warnings };
  }

  it('resolves the parents of a new mention from the links queryLinks returns', async () => {
    const { wakes } = await wakeWithParentRows([link('test://parent', 'ad4m://has_child')]);
    expect(wakes).toEqual([[{
      address: 'test://message',
      parents: ['test://parent'],
      parentLinks: [{ address: 'test://parent', predicate: 'ad4m://has_child' }],
    }]]);
  });

  /**
   * Containment is the space's vocabulary: WE hangs an utterance under its call
   * through `we://children` and a reply under its post through `we://comment`.
   * A lookup pinned to `ad4m://has_child` woke the agent on every WE mention
   * with no parent at all, so it could not find the conversation it was in.
   */
  it('asks for every link pointing at the item, not only ad4m://has_child', async () => {
    const { wakes, queries } = await wakeWithParentRows([
      link('test://call', 'we://children'),
      link('test://post', 'we://comment'),
    ]);
    expect(queries).toHaveLength(1);
    expect(queries[0].target).toBe('test://message');
    expect(queries[0].source).toBeUndefined();
    expect(queries[0].predicate).toBeUndefined();
    expect(wakes[0][0].parentLinks).toEqual([
      { address: 'test://call', predicate: 'we://children' },
      { address: 'test://post', predicate: 'we://comment' },
    ]);
  });

  /**
   * The address is handed to `queryLinks` as data, never spliced into query
   * text, so an address no IRI could carry is looked up like any other.
   */
  it('looks up an address SPARQL could not hold in an IRI', async () => {
    const odd = 'test://x> . ?s ?p ?o } #';
    const { wakes, queries, warnings } = await wakeWithParentRows([link('test://parent', 'test://p', odd)], odd);
    expect(queries[0].target).toBe(odd);
    expect(wakes[0][0].parents).toEqual(['test://parent']);
    expect(warnings).toEqual([]);
  });

  it('lists a parent linked through two predicates once in parents and twice in parentLinks', async () => {
    const { wakes } = await wakeWithParentRows([
      link('test://call', 'we://children'),
      link('test://call', 'ad4m://has_child'),
    ]);
    expect(wakes[0][0].parents).toEqual(['test://call']);
    expect(wakes[0][0].parentLinks).toHaveLength(2);
  });

  /** `queryLinks` returns a row per link; two authors adding the same triple are one parent link. */
  it('keeps one parent link per (source, predicate) when several authors added it', async () => {
    const { wakes } = await wakeWithParentRows([
      link('test://call', 'we://children', 'test://message', 'did:key:alice'),
      link('test://call', 'we://children', 'test://message', 'did:key:bob'),
    ]);
    expect(wakes[0][0].parentLinks).toEqual([{ address: 'test://call', predicate: 'we://children' }]);
  });

  /**
   * Links the executor writes at an item that are not containment: the
   * interpretation overlay's shadow of a real predicate, a FlowInstance's base,
   * a proposal's evidence and output, and the store's own metadata.
   */
  it('drops ad4m://ontology/, ad4m://interp/ and ad4m://flow/ links', async () => {
    const { wakes } = await wakeWithParentRows([
      link('test://channel', 'ad4m://has_child'),
      link('test://channel', 'ad4m://interp/inferred/ad4m://has_child'),
      link('test://flow-instance', 'ad4m://flow/base'),
      link('test://proposal', 'ad4m://flow/evidence'),
      link('test://proposal', 'ad4m://flow/output'),
      link('test://meta', 'ad4m://ontology/author'),
    ]);
    expect(wakes[0][0].parentLinks).toEqual([{ address: 'test://channel', predicate: 'ad4m://has_child' }]);
  });

  it('does not list the item as its own parent', async () => {
    const { wakes } = await wakeWithParentRows([
      link('test://message', 'test://self'),
      link('test://parent', 'ad4m://has_child'),
    ]);
    expect(wakes[0][0].parents).toEqual(['test://parent']);
  });

  it('keeps only links with a string source and predicate', async () => {
    const { wakes } = await wakeWithParentRows([
      link('test://a', 'test://p'),
      {},
      { data: { source: 'test://orphan' } },
      link('test://b', 'test://p'),
    ]);
    expect(wakes[0][0].parents).toEqual(['test://a', 'test://b']);
  });

  it('reports no parents, without a warning, when queryLinks returns a non-array', async () => {
    const { wakes, warnings } = await wakeWithParentRows(true);
    expect(wakes).toEqual([[{ address: 'test://message', parents: [], parentLinks: [] }]]);
    expect(warnings).toEqual([]);
  });

  it('names the operator as the fix for a locked executor, on both message shapes', () => {
    // Current executor message (post-#973) and the older internal error must
    // both map to the same hint, and the hint must point at the node operator —
    // a remote agent cannot unlock someone else's node.
    const current = hintFor('RPC error 403: Executor is locked: the wallet has not been unlocked since the last restart.');
    const legacy = hintFor('RPC error 403: main key not found');
    for (const hint of [current, legacy]) {
      expect(hint).toContain('executor is locked');
      expect(hint).toContain('operator');
    }
    expect(hintFor('connection refused')).toBe('');
  });
});
