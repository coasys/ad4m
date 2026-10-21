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

  /**
   * Delivers one new mention (or the `delivered` rows) and returns what it woke with, every model query the manager made
   * (`[className, parsed query]`) and any warnings. `answer` gets each query's `parent` scope and
   * returns the rows' ids, or a raw result to hand back as is.
   */
  async function wakeWith(
    answer: (scope: any) => unknown,
    ancestorDepth?: number,
    address = 'test://message',
    delivered: any[] = [{ source: address }],
  ) {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const ProxyClass = function () {
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const queries: [string, any][] = [];
    const warnings: string[] = [];
    const client = {
      querySparql: () => Promise.reject(new Error('the waker resolves containment through Container')),
      modelQuery: (_uuid: string, className: string, queryJson: string) => {
        const query = JSON.parse(queryJson);
        queries.push([className, query]);
        const rows = answer(query.parent);
        return Promise.resolve(
          Array.isArray(rows) ? { instances: rows.map((id) => ({ id })), totalCount: rows.length } : rows,
        );
      },
    };
    const wakes: any[] = [];
    const manager = new WakerSubscriptionManager({
      perspectiveClient: client,
      logger: { ...noopLogger(), warn: (msg: string) => { warnings.push(msg); } },
      QuerySubscriptionProxy: ProxyClass,
      debounceMs: 10,
      retryPendingMs: 60_000,
      ...(ancestorDepth !== undefined ? { ancestorDepth } : {}),
      onWake: (_sub, _result, mentions) => { wakes.push(mentions); },
    });

    await manager.subscribe({ ...sub, id: 'mention-parents' });
    await deliver!(delivered);
    await waitUntil(() => wakes.length > 0);
    manager.disposeAll();
    return { wakes, queries, warnings };
  }

  /**
   * Containment as a map of child → holders, answered as the executor's `Container` answers: a
   * step in returns each id's holders, a transitive walk in returns everything above the ids,
   * breadth first, each once, never an anchor, up to `maxDepth`.
   */
  const tree = (holders: Record<string, string[]>) => (scope: any) => {
    const anchors: string[] = scope.ids;
    if (!scope.transitive) return [...new Set(anchors.flatMap((id) => holders[id] ?? []))];
    const out: string[] = [];
    const seen = new Set(anchors);
    let level = anchors;
    for (let depth = 0; level.length && depth < (scope.maxDepth ?? Infinity); depth++) {
      const next = level.flatMap((id) => holders[id] ?? []).filter((h) => !seen.has(h) && (seen.add(h), true));
      out.push(...next);
      level = next;
    }
    return out;
  };

  it('asks Container for what holds a new mention, one step in', async () => {
    const { wakes, queries } = await wakeWith(() => ['test://parent']);
    expect(queries[0]).toEqual([
      'ad4m://Container',
      { parent: { ids: ['test://message'], direction: 'in' }, properties: [] },
    ]);
    expect(wakes).toEqual([[{ address: 'test://message', parents: ['test://parent'], ancestors: [] }]]);
  });

  it('reports no parents, without a warning, when the executor returns no instances', async () => {
    const { wakes, warnings } = await wakeWith(() => true);
    expect(wakes).toEqual([[{ address: 'test://message', parents: [], ancestors: [] }]]);
    expect(warnings).toEqual([]);
  });

  it('deduplicates two result rows for the same source in one mention pass', async () => {
    const { wakes, queries } = await wakeWith(() => [], undefined, 'addr://msg-1', [
      { source: 'addr://msg-1' },
      { source: 'addr://msg-1' },
    ]);
    expect(wakes).toEqual([[{ address: 'addr://msg-1', parents: [], ancestors: [] }]]);
    // The parent lookup runs once, not once per row.
    expect(queries).toHaveLength(1);
  });

  /**
   * Test 11 (#1446): a mention under a message held by a We block. The block is a parent beside
   * the channel that holds the message by has_child; the chain above both comes back nearest
   * first, from one bounded walk in from the parents.
   */
  it('reports every container holding a mention, and the chain above, nearest first', async () => {
    const { wakes, queries } = await wakeWith(
      tree({
        'test://message': ['test://channel', 'test://block'],
        'test://block': ['test://call'],
        'test://call': ['test://space'],
        'test://space': ['ad4m://self'],
      }),
    );
    expect(wakes).toEqual([
      [
        {
          address: 'test://message',
          parents: ['test://channel', 'test://block'],
          ancestors: ['test://call', 'test://space', 'ad4m://self'],
        },
      ],
    ]);
    expect(queries).toHaveLength(2);
    expect(queries[1]).toEqual([
      'ad4m://Container',
      {
        parent: { ids: ['test://channel', 'test://block'], direction: 'in', transitive: true, maxDepth: 3 },
        properties: [],
      },
    ]);
  });

  it('bounds the walk by ancestorDepth, and never reports the mention itself', async () => {
    const holders = {
      'test://message': ['test://a'],
      'test://a': ['test://b'],
      'test://b': ['test://c'],
      'test://c': ['test://message'],
    };
    const { wakes } = await wakeWith(tree(holders), 5);
    // c leads back to the message: it is the mention, not something above it.
    expect(wakes[0][0].ancestors).toEqual(['test://b', 'test://c']);
    const shallow = await wakeWith(tree(holders), 1);
    expect(shallow.wakes[0][0].ancestors).toEqual(['test://b']);
    const none = await wakeWith(tree(holders), 0);
    expect(none.wakes[0][0].ancestors).toEqual([]);
    expect(none.queries).toHaveLength(1);
  });

  it('sends an address with query syntax in it as data', async () => {
    const address = 'test://bad> . ?s ?p ?o . #';
    const { wakes, queries } = await wakeWith(() => [], 3, address);
    expect(queries[0][1].parent.ids).toEqual([address]);
    expect(wakes[0][0]).toEqual({ address, parents: [], ancestors: [] });
  });

  it('reads a model subscription by row id', async () => {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const constructed: any[][] = [];
    const ProxyClass = function (...args: any[]) {
      constructed.push(args);
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const wakes: any[] = [];
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: ProxyClass,
      debounceMs: 10,
      onWake: (_sub, result) => { wakes.push(result); },
    });
    const query = JSON.stringify({ where: { id: 'test://call' }, properties: ['children'] });
    await manager.subscribe({
      id: 'children-of-call',
      type: 'channel-messages',
      perspective: 'fake-uuid',
      channel: 'test://call',
      query,
      className: 'ad4m://Container',
    });
    expect(constructed[0]).toEqual(['fake-uuid', query, perspectiveClient, 'ad4m://Container']);
    await deliver!({ instances: [{ id: 'test://call', children: ['test://block'] }], totalCount: 1 });
    await waitUntil(() => wakes.length > 0);
    expect(wakes[0].instances[0].children).toEqual(['test://block']);
    // Not a model result: ignored.
    await deliver!([{ source: 'x' }]);
    manager.disposeAll();
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
