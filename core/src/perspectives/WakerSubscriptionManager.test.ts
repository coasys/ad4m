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

  /** Delivers one new mention and returns the parents it woke with, the parent query and any warnings. */
  async function wakeWithParentRows(rows: unknown) {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const ProxyClass = function () {
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const queries: string[] = [];
    const warnings: string[] = [];
    const client = {
      querySparql: (_uuid: string, query: string) => {
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
    await deliver!([{ source: 'test://message' }]);
    await waitUntil(() => wakes.length > 0);
    manager.disposeAll();
    return { wakes, queries, warnings };
  }

  it('resolves the parents of a new mention from the flat rows querySparql returns', async () => {
    const { wakes, queries } = await wakeWithParentRows([{ source: 'test://parent' }]);
    expect(queries[0]).toContain('<ad4m://has_child> <test://message>');
    expect(wakes).toEqual([[{ address: 'test://message', parents: ['test://parent'] }]]);
  });

  it('keeps only string parents when a row leaves ?source unbound', async () => {
    const { wakes } = await wakeWithParentRows([{ source: 'test://a' }, {}, { source: 'test://b' }]);
    expect(wakes).toEqual([[{ address: 'test://message', parents: ['test://a', 'test://b'] }]]);
  });

  it('reports no parents, without a warning, when querySparql returns a non-array', async () => {
    const { wakes, warnings } = await wakeWithParentRows(true);
    expect(wakes).toEqual([[{ address: 'test://message', parents: [] }]]);
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
