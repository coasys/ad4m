import { WakerSubscriptionManager, WakerSubscription, hintFor, holdsAWholeWordTerm } from './WakerSubscriptionManager';
import { Container } from './Container';
import { TextContent } from './TextContent';

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
    modelQuery: () => Promise.resolve({ instances: [], totalCount: 0 }),
  };

  const sub = {
    id: 'mention-locked',
    type: 'mention' as const,
    perspective: 'fake-uuid',
    channel: '',
    className: TextContent.builtinUri,
    query: '{"where":{"OR":[{"text":{"contains":"bot"}}]}}',
    terms: ['bot'],
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
   * Delivers model results to a mention subscription and returns what it woke with, the
   * subscription's constructor arguments, the parent queries (class and parsed JSON) and warnings.
   * `parents` answers each parent query: instances, or a function of the query.
   */
  async function wakeWith(parents: unknown, deliveries: any[] = [{ instances: [{ id: 'test://message', text: 'hi bot' }] }], ancestorDepth?: number) {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const opened: any[][] = [];
    const ProxyClass = function (...args: any[]) {
      opened.push(args);
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const queries: { className: string; query: any }[] = [];
    const warnings: string[] = [];
    const client = {
      modelQuery: (_uuid: string, className: string, queryJson: string) => {
        const query = JSON.parse(queryJson);
        queries.push({ className, query });
        return Promise.resolve(typeof parents === 'function' ? parents(query) : parents);
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
    for (const result of deliveries) {
      await deliver!(result);
      await new Promise((resolve) => setTimeout(resolve, 30));
    }
    manager.disposeAll();
    return { wakes, opened, queries, warnings };
  }

  /** Containment as a map of child → parents, answered per `where: { children }` query. */
  const tree = (holders: Record<string, string[]>) => (query: any) => ({
    instances: [...new Set((query.where.children as string[]).flatMap((child) => holders[child] ?? []))].map((id) => ({ id })),
  });

  it('opens a mention subscription as a model query over its class', async () => {
    const { opened } = await wakeWith({ instances: [] }, []);
    expect(opened).toEqual([[sub.perspective, sub.query, expect.anything(), TextContent.builtinUri]]);
  });

  it('wakes once per new mentioned id in the model result', async () => {
    const { wakes } = await wakeWith({ instances: [] }, [
      { instances: [{ id: 'test://a', text: 'bot?' }], totalCount: 1 },
      { instances: [{ id: 'test://b', text: '@bot' }, { id: 'test://a', text: 'bot?' }], totalCount: 2 },
      { instances: [{ id: 'test://b', text: '@bot' }, { id: 'test://a', text: 'bot?' }], totalCount: 2 },
      // SPARQL rows are not a model result.
      [{ source: 'test://c' }],
    ]);
    expect(wakes.map((mentions) => mentions.map((m: any) => m.address))).toEqual([['test://a'], ['test://b']]);
  });

  // #1459's table (`mention_query_matches_the_name_as_a_whole_word_only` and the regex-characters
  // case), now the waker's check: the query returns these as substring candidates
  // (`the_mention_query_returns_substring_candidates_with_their_text` in Rust).
  it('holds a term only as a whole word, case-insensitively and literally', () => {
    const holds = (text: string, terms = ['Data']) => holdsAWholeWordTerm(text, terms);
    expect(['hi Data!', 'DATA, look', '@data', 'data', "Data's turn"].filter((t) => !holds(t))).toEqual([]);
    expect(['the metadata is stale', 'a database row', 'datas', '_data', 'dataé'].filter((t) => holds(t))).toEqual([]);
    expect(holds('ask J.R. (bot) please', ['J.R. (bot)'])).toBe(true);
    expect(holds('ask JxRx (bot) please', ['J.R. (bot)'])).toBe(false);
    expect(holds('hi a"b\\c!', ['a"b\\c'])).toBe(true);
    expect(holds('hi a"bc!', ['a"b\\c'])).toBe(false);
    // The DID, inside a mention's HTML as apps write it.
    expect(holds('<span data-id="did:key:zBot">@bot</span>', ['did:key:zBot'])).toBe(true);
    expect(holds('did:key:zBotX', ['did:key:zBot'])).toBe(false);
    expect(holds('hi Data', ['', 'Bob'])).toBe(false);
  });

  it('reads HTML as its visible text, and a mention by the DID in its target attribute', () => {
    const bot = ['Data', 'did:key:zBot'];
    const holds = (text: string) => holdsAWholeWordTerm(text, bot);
    // Markup is not words: attribute names and values never wake "Data".
    expect(holds('<div data-x="1">hi</div>')).toBe(false);
    expect(holds('<p class="data">see the <a href="https://x.dev/data">notes</a></p>')).toBe(false);
    // Another user's mention, in each app's markup.
    expect(holds('<p>hi <a data-type="mention" href="did:key:zOther" data-label="Marvin">@Marvin</a> </p>')).toBe(false);
    expect(holds('<span data-id="did:key:zOther">@Marvin</span> can you look?')).toBe(false);
    expect(holds('<span class="we-mention" data-did="did:key:zOther">@Marvin</span> can you look?')).toBe(false);
    // Visible words still count, inside and between elements.
    expect(holds('<p>hey Data</p>')).toBe(true);
    expect(holds('<p>hi</p><p>Data, look</p>')).toBe(true);
    expect(holds('<p>hey <strong>@Data</strong>&nbsp;&amp; Bob</p>')).toBe(true);
    expect(holds('meta<b>data</b>')).toBe(false);
    // Flux's mention (tiptap `mention` node), by name and by DID alone.
    expect(holds('<p>hi <a data-type="mention" href="did:key:zBot" data-label="Data">@Data</a> </p>')).toBe(true);
    expect(holds('<p>hi <a data-type="mention" href="did:key:zBot" data-label="Bot">@Bot</a> </p>')).toBe(true);
    expect(holds('<span data-id="did:key:zBot">@bot</span>')).toBe(true);
    // We stores a block as plain `text` with the DID in `marks`, so the name is what is read;
    // its editor's chip carries the DID in `data-did`.
    expect(holds('hey @Data can you look?')).toBe(true);
    expect(holds('<span class="we-mention" data-did="did:key:zBot">@Bot</span> can you look?')).toBe(true);
    // Plain text with a bare "<" is not markup.
    expect(holds('a < b and Data > c')).toBe(true);
  });

  it('wakes only on a whole-word mention, and still wakes when one arrives later', async () => {
    const { wakes } = await wakeWith({ instances: [] }, [
      // Substring candidates the query returns: no wake.
      // So is "bot" in the markup of someone else's mention.
      { instances: [{ id: 'test://meta', text: 'the robots are fine' }, { id: 'test://none' }], totalCount: 2 },
      {
        instances: [
          { id: 'test://real', text: 'hey Bot,' },
          { id: 'test://meta', text: 'the robots are fine' },
          { id: 'test://markup', text: '<p><span class="mention" data-bot-hint="1" data-id="did:key:zOther">@Marvin</span> hi</p>' },
        ],
        totalCount: 3,
      },
    ]);
    expect(wakes.map((mentions) => mentions.map((m: any) => m.address))).toEqual([['test://real']]);
  });

  it('refuses a mention subscription without its terms', async () => {
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: function () { throw new Error('must not open'); },
      onWake: () => {},
    });
    const { terms: _terms, ...withoutTerms } = sub;
    await expect(manager.subscribe(withoutTerms)).rejects.toThrow(/needs its terms/);
    await expect(manager.subscribe({ ...sub, terms: [] })).rejects.toThrow(/needs its terms/);
    expect(manager.getActive()).toEqual([]);
    expect(manager.getPending()).toEqual([]);
  });

  it('resolves the parents of a new mention with a model query on Container', async () => {
    const { wakes, queries } = await wakeWith({ instances: [{ id: 'test://call', children: ['test://message'] }] }, undefined, 0);
    expect(queries).toEqual([
      { className: Container.builtinUri, query: { where: { children: ['test://message'] }, properties: [] } },
    ]);
    expect(wakes).toEqual([[{ address: 'test://message', parents: ['test://call'], ancestors: [] }]]);
  });

  it('keeps only string ids, and reports no parents without a warning when the result has no instances', async () => {
    const some = await wakeWith({ instances: [{ id: 'test://a' }, {}, { id: 'test://b' }] }, undefined, 0);
    expect(some.wakes[0][0].parents).toEqual(['test://a', 'test://b']);
    const none = await wakeWith(true);
    expect(none.wakes).toEqual([[{ address: 'test://message', parents: [], ancestors: [] }]]);
    expect(none.warnings).toEqual([]);
  });

  it('reports the chain above the parents, nearest first, each address once', async () => {
    // A message in a thread in a call in a space — and the call is also in a second channel, so two
    // paths reach the space.
    const { wakes } = await wakeWith(
      tree({
        'test://message': ['test://thread'],
        'test://thread': ['test://call'],
        'test://call': ['test://space', 'test://channel'],
        'test://channel': ['test://space'],
      }),
    );
    expect(wakes).toEqual([
      [
        {
          address: 'test://message',
          parents: ['test://thread'],
          ancestors: ['test://call', 'test://space', 'test://channel'],
        },
      ],
    ]);
  });

  it('stops at its depth, and at a cycle, rather than walking forever', async () => {
    const { wakes } = await wakeWith(
      tree({
        'test://message': ['test://a'],
        'test://a': ['test://b'],
        'test://b': ['test://c'],
        'test://c': ['test://message'],
      }),
      undefined,
      5,
    );
    // c leads back to the message: seen already, so the walk ends there rather than going round.
    expect(wakes[0][0].ancestors).toEqual(['test://b', 'test://c']);
    const shallow = await wakeWith(tree({ 'test://message': ['test://a'], 'test://a': ['test://b'], 'test://b': ['test://c'] }), undefined, 1);
    expect(shallow.wakes[0][0].ancestors).toEqual(['test://b']);
  });

  it('looks up a branching ancestor level in one query', async () => {
    const { wakes, queries } = await wakeWith(tree({
      'test://message': ['test://left', 'test://right'],
      'test://left': ['test://root'],
      'test://right': ['test://root'],
    }));
    expect(wakes[0][0].ancestors).toEqual(['test://root']);
    expect(queries).toHaveLength(3);
    expect(queries[1].query.where.children).toEqual(['test://left', 'test://right']);
  });

  it('hands any address to the executor as a value, not as query text', async () => {
    // The model query escapes it; the waker no longer builds query text, so it has nothing to skip.
    const address = 'test://bad> . ?s ?p ?o . #';
    const { wakes, queries, warnings } = await wakeWith(tree({ [address]: ['test://call'] }), [{ instances: [{ id: address, text: 'hi bot' }] }], 0);
    expect(queries[0].query.where.children).toEqual([address]);
    expect(wakes[0][0]).toEqual({ address, parents: ['test://call'], ancestors: [] });
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
