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
    querySparql: () => Promise.resolve({ results: { bindings: [] } }),
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

  it('names the locked wallet as the cause of a main-key 403 only', () => {
    expect(hintFor('RPC error 403: main key not found')).toContain('wallet is locked');
    expect(hintFor('connection refused')).toBe('');
  });
});
