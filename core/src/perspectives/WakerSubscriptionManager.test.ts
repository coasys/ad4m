import { MENTION_TEXT_LIMIT, WakerSubscriptionManager, hintFor, holdsAWholeWordTerm, mentionOf } from './WakerSubscriptionManager';

/** The agent the mention subscriptions below are for, and another one. */
const ME = 'did:key:zAgent';
const OTHER = 'did:key:zOther';

/**
 * A row of the executor's mention query (a model query over TextContent): its `text`, and its
 * mention and text links with their provenance under `__links`, as `links: ["mentions", "text"]`
 * returns them.
 */
function row(id: string, text: string, links: { mentions?: any[]; text?: any[] } = {}) {
  return { id, text, __links: { mentions: links.mentions ?? [], text: links.text ?? [] } };
}

/** A link as `__links` carries it. */
function link(target: string, author: string, timestamp: string, valid = true) {
  return { author, timestamp, data: { target }, proof: { key: '', signature: '', valid, invalid: !valid } };
}

/** A record found through its text alone, which names the agent as a whole word. */
const named = (id: string) => row(id, 'hi Data!');

/** A model subscription's result. */
const result = (...rows: any[]) => ({ instances: rows, totalCount: rows.length });

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
    // What `get_mention_waker_config` returns: a model query over TextContent, its terms and DID.
    className: 'ad4m://TextContent',
    query: JSON.stringify({ where: { OR: [{ mentions: { eq: ME, author: { not: ME } } }] }, links: ['mentions', 'text'] }),
    terms: ['Data', ME],
    did: ME,
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
   * Delivers one new mention (or each of the `deliveries` in turn) and returns what it woke with, every model query the
   * manager made (`[className, parsed query]`), any warnings and the subscriptions it opened. `answer` gets each
   * query's `parent` scope and returns the rows' ids, or a raw result to hand back as is.
   */
  async function wakeWith(
    answer: (scope: any) => unknown,
    ancestorDepth?: number,
    address = 'test://message',
    deliveries: any[] = [result(named(address))],
  ) {
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
    for (const result of deliveries) {
      await deliver!(result);
      await new Promise((resolve) => setTimeout(resolve, 30));
    }
    await waitUntil(() => wakes.length > 0);
    manager.disposeAll();
    return { wakes, queries, warnings, opened };
  }

  it('opens a mention subscription as a model query over TextContent', async () => {
    const { opened } = await wakeWith(() => []);
    expect(opened).toEqual([[sub.perspective, sub.query, expect.anything(), 'ad4m://TextContent']]);
  });

  it('refuses a mention subscription without its TextContent class, terms or DID', async () => {
    const manager = new WakerSubscriptionManager({
      perspectiveClient,
      logger: noopLogger(),
      QuerySubscriptionProxy: function () { throw new Error('never opened'); },
      onWake: () => {},
    });
    // What an older executor saved: SPARQL over `we://mention` links, or a TextContent query
    // over the text alone.
    const sparql = { ...sub, className: undefined, terms: undefined, did: undefined, query: 'SELECT ?source WHERE { }' };
    const textOnly = { ...sub, did: undefined };
    const noTerms = { ...sub, terms: [] };
    for (const old of [sparql, textOnly, noTerms]) {
      await expect(manager.subscribe(old)).rejects.toThrow('needs its TextContent class, terms and DID');
    }
    expect(manager.getActive()).toEqual([]);
    expect(manager.getPending()).toEqual([]);
  });

  it('ignores a result that is not instances, and a row without an id', async () => {
    const { wakes, queries } = await wakeWith(() => [], 0, 'test://message', [
      [{ source: 'test://sparql-row' }],
      result(named('test://message'), { text: 'hi Data, no id' }),
    ]);
    expect(wakes).toEqual([[{ address: 'test://message', parents: [], ancestors: [], verified: false }]]);
    expect(queries).toHaveLength(1);
  });

  /**
   * #1557: one query, two paths. A row whose mention link names the agent, in a verified link
   * another agent wrote, wakes as it is: WE's chip label ran into the next word ("@Datai") and
   * the whole-word check dropped it. A row found only through its text wakes on a whole word
   * only. A link the agent wrote, one that did not verify, or one to someone else does not
   * count as a mention; such a row falls back to its text.
   */
  it('wakes through the mention link whatever the text, and through the text on a whole word only', async () => {
    const at = '2026-10-27T00:41:23.437Z';
    const { wakes } = await wakeWith(() => [], 0, 'unused', [
      result(
        row('test://chip', '@Datai have taken a dump of prod', { mentions: [link(ME, OTHER, at)] }),
        row('test://spoken', 'Hey, data!', { text: [link('literal:string:Hey', OTHER, at)] }),
        row('test://metadata', 'the metadata is stale'),
        row('test://own-link', '@Datai', { mentions: [link(ME, ME, at)] }),
        row('test://forged', '@Datai', { mentions: [link(ME, OTHER, at, false)] }),
        row('test://someone-else', '@Bobi', { mentions: [link('did:key:zBob', OTHER, at)] }),
        row('test://html-chip', `<p>hey <span class="we-mention" data-did="${ME}">@someone</span></p>`),
        row('test://html-attr', '<p data-type="mention">for someone else</p>'),
      ),
    ]);
    expect(wakes).toHaveLength(1);
    expect(wakes[0].map((m: any) => m.address)).toEqual(['test://chip', 'test://spoken', 'test://html-chip']);
  });

  /**
   * #1563: a mention wake says who wrote the link the mention is in, when, and whether its proof
   * verified: the mention link's (the newest of the agent's) on the relation path, the newest text
   * link another agent wrote on the text path. A record that matches both wakes once, with the
   * mention link's.
   */
  it('carries the provenance of the link the mention is in, once per record', async () => {
    const { wakes } = await wakeWith(() => [], 0, 'unused', [
      result(
        row('test://both', 'hey Data', {
          mentions: [
            link(ME, 'did:key:zWriter', '2026-10-27T00:41:22.242Z'),
            link(ME, 'did:key:zEditor', '2026-10-27T00:41:23.437Z'),
            link('did:key:zBob', 'did:key:zLater', '2026-10-27T00:42:00.000Z'),
          ],
          text: [link('literal:string:hey', 'did:key:zWriter', '2026-10-27T00:41:22.242Z')],
        }),
        row('test://named', 'Data, can you look?', {
          text: [
            link('literal:string:old', 'did:key:zWriter', '2026-10-27T00:40:00.000Z'),
            link('literal:string:new', 'did:key:zEditor', '2026-10-27T00:40:30.000Z'),
            link('literal:string:mine', ME, '2026-10-27T00:40:40.000Z'),
          ],
        }),
        row('test://both', 'hey Data', { mentions: [link(ME, 'did:key:zWriter', '2026-10-27T00:41:22.242Z')] }),
      ),
    ]);
    expect(wakes).toEqual([
      [
        {
          address: 'test://both',
          parents: [],
          ancestors: [],
          author: 'did:key:zEditor',
          timestamp: '2026-10-27T00:41:23.437Z',
          verified: true,
        },
        {
          address: 'test://named',
          parents: [],
          ancestors: [],
          author: 'did:key:zEditor',
          timestamp: '2026-10-27T00:40:30.000Z',
          verified: true,
        },
      ],
    ]);
  });

  it('wakes on a record once its mention link arrives, after its text alone did not', async () => {
    const h = await mentionHarness(10);
    await h.deliver(result(row('test://late-link', '@Datai have news')));
    await new Promise((r) => setTimeout(r, 30));
    expect(h.wakes).toEqual([]);
    await h.deliver(
      result(row('test://late-link', '@Datai have news', { mentions: [link(ME, OTHER, '2026-10-27T00:41:23.437Z')] })),
    );
    await waitUntil(() => h.wakes.length > 0, 1000);
    h.manager.disposeAll();
    expect(h.wakes.map((w: any) => w.map((m: any) => m.address))).toEqual([['test://late-link']]);
  });

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
    expect(wakes).toEqual([[{ address: 'test://message', parents: ['test://parent'], ancestors: [], verified: false }]]);
  });

  it('reports no parents, without a warning, when the executor returns no instances', async () => {
    const { wakes, warnings } = await wakeWith(() => true);
    expect(wakes).toEqual([[{ address: 'test://message', parents: [], ancestors: [], verified: false }]]);
    expect(warnings).toEqual([]);
  });

  it('deduplicates two result rows for the same record in one mention pass', async () => {
    const { wakes, queries } = await wakeWith(() => [], undefined, 'addr://msg-1', [
      result(named('addr://msg-1'), named('addr://msg-1')),
    ]);
    expect(wakes).toEqual([[{ address: 'addr://msg-1', parents: [], ancestors: [], verified: false }]]);
    // The parent lookup runs once, not once per row.
    expect(queries).toHaveLength(1);
  });

  /** A mention subscription whose results the test pushes by hand, with the wakes it produced. */
  async function mentionHarness(
    debounceMs: number,
    onWake?: (mentions: any) => unknown,
    // What the subscription holds when it opens (the proxy's `result`), and what a restart restored.
    opening?: {
      initial?: any;
      previousSeen?: Record<string, string[]>;
      modelQuery?: (uuid: string, className: string, queryJson: string) => Promise<any>;
    },
  ) {
    let deliver: ((result: any) => Promise<void>) | undefined;
    const ProxyClass = function () {
      return {
        initialized: Promise.resolve(true),
        subscribe: () => Promise.resolve(),
        dispose: () => {},
        result: opening?.initial,
        onResult: (cb: (result: any) => Promise<void>) => { deliver = cb; },
      };
    };
    const wakes: any[] = [];
    const persistedSeen: string[][] = [];
    const manager = new WakerSubscriptionManager({
      perspectiveClient: {
        querySparql: () => Promise.resolve([]),
        modelQuery: opening?.modelQuery ?? (() => Promise.resolve({ instances: [], totalCount: 0 })),
      },
      logger: noopLogger(),
      QuerySubscriptionProxy: ProxyClass,
      debounceMs,
      retryPendingMs: 60_000,
      retryWakeMs: 20,
      onWake: (_sub, _result, mentions) => {
        wakes.push(mentions);
        return onWake?.(mentions);
      },
      onPersist: (_subs, seen) => { persistedSeen.push(seen['mention-harness'] ?? []); },
      previousSeenMessages: opening?.previousSeen,
    } as any);
    await manager.subscribe({ ...sub, id: 'mention-harness' });
    const msg = (...ids: string[]) => result(...ids.map(named));
    return { manager, deliver: (r: any) => deliver!(r), msg, wakes, persistedSeen };
  }

  /**
   * Live test 2026-10-21 (T13b): a mention written while the gateway was down. The subscription
   * opened after the restart already holds it, but the waker only listened for later changes.
   */
  it('wakes on a mention the restored subscription already holds when it opens', async () => {
    const msg = (...ids: string[]) => result(...ids.map(named));
    const h = await mentionHarness(10, undefined, {
      initial: msg('test://during-downtime', 'test://old'),
      previousSeen: { 'mention-harness': ['test://old'] },
    });
    await waitUntil(() => h.wakes.length > 0, 1000);
    h.manager.disposeAll();
    expect(h.wakes.map((w: any) => w.map((m: any) => m.address))).toEqual([['test://during-downtime']]);
  });

  it('takes the mentions a new subscription opens on as history, and wakes only on later ones', async () => {
    const h = await mentionHarness(10, undefined, {
      initial: result(named('test://old')),
    });
    await h.deliver(h.msg('test://new', 'test://old'));
    await waitUntil(() => h.wakes.length > 0, 1000);
    await new Promise((r) => setTimeout(r, 30));
    h.manager.disposeAll();
    expect(h.wakes.map((w: any) => w.map((m: any) => m.address))).toEqual([['test://new']]);
  });

  /** Review of #1512 (Lal): B was persisted as seen while its parents were still resolving. */
  it('does not persist a mention still resolving its parents when an earlier wake lands', async () => {
    let landA: (delivered: boolean) => void = () => {};
    let resolveB: () => void = () => {};
    let bLookupStarted = false;
    const h = await mentionHarness(
      10,
      () => (h.wakes.length === 1 ? new Promise((r) => { landA = r; }) : true),
      {
        // A's lookups answer at once; B's waits until the test lets it go.
        modelQuery: (_uuid: string, _className: string, queryJson: string) => {
          const empty = { instances: [], totalCount: 0 };
          if (!JSON.parse(queryJson).parent?.ids?.includes('test://b')) return Promise.resolve(empty);
          bLookupStarted = true;
          return new Promise((r) => { resolveB = () => r(empty); });
        },
      },
    );
    await h.deliver(h.msg('test://a'));
    await waitUntil(() => h.wakes.length === 1, 1000);
    const bHandled = h.deliver(h.msg('test://b', 'test://a'));
    await waitUntil(() => bLookupStarted, 1000);
    expect(bLookupStarted).toBe(true);
    landA(true);
    await waitUntil(() => h.persistedSeen.some((seen) => seen.includes('test://a')), 1000);
    expect(h.persistedSeen.at(-1)).toEqual(['test://a']);
    resolveB();
    await bHandled;
    await waitUntil(() => h.persistedSeen.some((seen) => seen.includes('test://b')), 1000);
    h.manager.disposeAll();
    expect(h.wakes.map((w: any) => w.map((m: any) => m.address))).toEqual([['test://a'], ['test://b']]);
  });

  it('names every mention that arrived inside one debounce window in the wake', async () => {
    const h = await mentionHarness(100);
    await h.deliver(h.msg('test://a'));
    await new Promise((r) => setTimeout(r, 20));
    await h.deliver(h.msg('test://b', 'test://a'));
    await waitUntil(() => h.wakes.length > 0, 1000);
    await new Promise((r) => setTimeout(r, 150));
    h.manager.disposeAll();
    expect(h.wakes).toHaveLength(1);
    expect(h.wakes[0].map((m: any) => m.address).sort()).toEqual(['test://a', 'test://b']);
  });

  /**
   * Live test 2026-10-21 (T13): a mention reached the waker while the gateway drained for a
   * restart; the wake POST answered 503, the mention was persisted as seen and never woke anyone.
   */
  it('does not persist a mention as seen when its wake was not delivered, and retries it', async () => {
    let accept = false;
    const h = await mentionHarness(10, () => Promise.resolve(accept));
    await h.deliver(h.msg('test://lost'));
    await waitUntil(() => h.wakes.length > 0, 1000);
    await new Promise((r) => setTimeout(r, 30));
    expect(h.persistedSeen.every((seen) => !seen.includes('test://lost'))).toBe(true);
    accept = true;
    await waitUntil(() => h.persistedSeen.some((seen) => seen.includes('test://lost')), 5000);
    h.manager.disposeAll();
    expect(h.wakes.length).toBeGreaterThan(1);
    expect(h.wakes.at(-1).map((m: any) => m.address)).toEqual(['test://lost']);
    expect(h.persistedSeen.some((seen) => seen.includes('test://lost'))).toBe(true);
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
          verified: false,
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
    expect(wakes[0][0]).toEqual({ address, parents: [], ancestors: [], verified: false });
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
    // both map to the same hint, and the hint must point at the executor's operator —
    // a remote agent cannot unlock someone else's executor.
    const current = hintFor('RPC error 403: Executor is locked: the wallet has not been unlocked since the last restart.');
    const legacy = hintFor('RPC error 403: main key not found');
    for (const hint of [current, legacy]) {
      expect(hint).toContain('executor is locked');
      expect(hint).toContain('operator');
    }
    expect(hintFor('connection refused')).toBe('');
  });
});

/** Ported from #1459 through dev's whole-word check, which the text path keeps. */
describe('holdsAWholeWordTerm', () => {
  it.each([
    ['hi Data!', true],
    ['the metadata is stale', false],
    ['DATA, look', true],
    ['a database row', false],
    ['@data', true],
    ["Data's turn", true],
    ['@Datai have taken a dump', false],
    ['<p>ask <b>Data</b></p>', true],
    ['<p data-type="mention">someone</p>', false],
    [`<a data-type="mention" href="${ME}">@x</a>`, true],
  ])('%s -> %s', (text, expected) => {
    expect(holdsAWholeWordTerm(text, ['Data', ME])).toBe(expected);
  });

  it('reads a bounded prefix, so a long run of unclosed openers cannot stall the waker', () => {
    const bot = ['Data', 'did:key:zBot'];
    // Unbounded, each of these took seconds: every unclosed opener scans to the end.
    for (const opener of ['<script>', '<style>', '<!--']) {
      const started = Date.now();
      expect(holdsAWholeWordTerm('the metadata ' + opener.repeat(125_000), bot)).toBe(false);
      expect(holdsAWholeWordTerm('hey Data ' + opener.repeat(125_000), bot)).toBe(true);
      expect(Date.now() - started).toBeLessThan(1000);
    }
    // Within the limit a long message still wakes, by name and by DID target.
    const filler = 'lorem ipsum '.repeat(1000);
    expect(holdsAWholeWordTerm(filler + 'ask Data', bot)).toBe(true);
    expect(holdsAWholeWordTerm(`${filler}<a href="did:key:zBot">@Bot</a>`, bot)).toBe(true);
    // Past it, it does not.
    expect(holdsAWholeWordTerm('x'.repeat(MENTION_TEXT_LIMIT) + ' Data', bot)).toBe(false);
  });
});

describe('mentionOf', () => {
  const at = '2026-10-27T00:41:23.437Z';
  const sub = { did: ME, terms: ['Data', ME] };

  it('takes a verified mention link to the agent by someone else, whatever the text', () => {
    expect(mentionOf(row('x', 'nothing', { mentions: [link(ME, OTHER, at)] }), sub)).toEqual({
      author: OTHER,
      timestamp: at,
      verified: true,
    });
  });

  it('falls back to a whole-word text mention when the link is the agent\'s own, unverified or someone else\'s', () => {
    for (const mention of [link(ME, ME, at), link(ME, OTHER, at, false), link('did:key:zBob', OTHER, at)]) {
      expect(mentionOf(row('x', '@Datai', { mentions: [mention] }), sub)).toBeNull();
      expect(mentionOf(row('x', 'hi Data', { mentions: [mention] }), sub)).toEqual({ verified: false });
    }
  });

  it('needs the DID for the link path', () => {
    expect(mentionOf(row('x', 'nothing', { mentions: [link(ME, OTHER, at)] }), { terms: ['Data'] })).toBeNull();
  });
});
