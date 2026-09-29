import { PerspectiveProxy } from './PerspectiveProxy';
import { SHACLShape } from '../shacl/SHACLShape';
import { SHACLFlow } from '../shacl/SHACLFlow';

// ── Helpers ──────────────────────────────────────────────────────────────────

function createMockClient(overrides: Record<string, jest.Mock> = {}): any {
  return {
    addPerspectiveLinkAddedListener: jest.fn(),
    addPerspectiveLinkRemovedListener: jest.fn(),
    addPerspectiveLinkUpdatedListener: jest.fn(),
    addPerspectiveSyncStateChangeListener: jest.fn(),
    getShaclNames: jest.fn().mockResolvedValue([]),
    // Client contract (post r3897752023): getShaclTargetClass returns
    // `undefined` on "not found", not `null`. Mock the same shape.
    getShaclTargetClass: jest.fn().mockResolvedValue(undefined),
    getShacl: jest.fn().mockResolvedValue(null),
    getAllShacl: jest.fn().mockResolvedValue([]),
    ...overrides,
  };
}

function createProxy(client?: any): PerspectiveProxy {
  const mockClient = client ?? createMockClient();
  return new PerspectiveProxy(
    {
      uuid: 'test-uuid',
      name: 'test',
      owners: [],
      sharedUrl: null,
      neighbourhood: null,
      state: 'Synced',
    } as any,
    mockClient,
  );
}

/** Build the link triples that SHACLShape.fromLinks expects for a minimal shape. */
function buildShapeLinks(
  shapeUri: string,
  targetClass: string,
  properties: Array<{ name: string; path: string; datatype?: string }>,
): Array<{ source: string; predicate: string; target: string }> {
  const links: Array<{ source: string; predicate: string; target: string }> = [
    { source: shapeUri, predicate: 'sh://targetClass', target: targetClass },
  ];
  for (const prop of properties) {
    const propUri = `${shapeUri}.${prop.name}`;
    links.push({ source: shapeUri, predicate: 'sh://property', target: propUri });
    links.push({ source: propUri, predicate: 'sh://path', target: prop.path });
    if (prop.datatype) {
      links.push({ source: propUri, predicate: 'sh://datatype', target: prop.datatype });
    }
  }
  return links;
}

// ── Tests ────────────────────────────────────────────────────────────────────

describe('PerspectiveProxy SHACL RPC delegation', () => {
  describe('getShaclNames', () => {
    it('delegates to PerspectiveClient.getShaclNames with the perspective UUID', async () => {
      const client = createMockClient({
        getShaclNames: jest.fn().mockResolvedValue(['Message', 'Channel']),
      });
      const proxy = createProxy(client);

      const names = await proxy.getShaclNames();

      expect(client.getShaclNames).toHaveBeenCalledWith('test-uuid');
      expect(names).toEqual(['Message', 'Channel']);
    });

    it('returns an empty array when no shapes exist', async () => {
      const proxy = createProxy();
      const names = await proxy.getShaclNames();
      expect(names).toEqual([]);
    });
  });

  describe('getShaclTargetClass', () => {
    it('delegates to PerspectiveClient and returns the target class', async () => {
      const client = createMockClient({
        getShaclTargetClass: jest.fn().mockResolvedValue('flux://Message'),
      });
      const proxy = createProxy(client);

      const tc = await proxy.getShaclTargetClass('Message');

      expect(client.getShaclTargetClass).toHaveBeenCalledWith('test-uuid', 'Message');
      expect(tc).toBe('flux://Message');
    });

    it('returns undefined when the shape does not exist', async () => {
      const client = createMockClient({
        // Post r3897752023 the client itself returns undefined on "not
        // found", so the proxy passthrough must preserve that.
        getShaclTargetClass: jest.fn().mockResolvedValue(undefined),
      });
      const proxy = createProxy(client);

      const tc = await proxy.getShaclTargetClass('NonExistent');
      expect(tc).toBeUndefined();
    });
  });

  describe('getShacl', () => {
    it('delegates to PerspectiveClient and reconstructs a SHACLShape', async () => {
      const shapeUri = 'flux://MessageShape';
      const links = buildShapeLinks(shapeUri, 'flux://Message', [
        { name: 'body', path: 'flux://body', datatype: 'xsd:string' },
        { name: 'timestamp', path: 'flux://timestamp', datatype: 'xsd:dateTime' },
      ]);

      const client = createMockClient({
        getShacl: jest.fn().mockResolvedValue({ shapeUri, links }),
      });
      const proxy = createProxy(client);

      const shape = await proxy.getShacl('Message');

      expect(client.getShacl).toHaveBeenCalledWith('test-uuid', 'Message');
      expect(shape).not.toBeNull();
      expect(shape!.nodeShapeUri).toBe(shapeUri);
      expect(shape!.targetClass).toBe('flux://Message');
      expect(shape!.properties.length).toBe(2);
      expect(shape!.properties.map(p => p.name).sort()).toEqual(['body', 'timestamp']);
    });

    it('returns null when the shape does not exist', async () => {
      const proxy = createProxy();
      const shape = await proxy.getShacl('NonExistent');
      expect(shape).toBeNull();
    });
  });

  describe('getAllShacl', () => {
    it('delegates to PerspectiveClient and reconstructs all shapes', async () => {
      const msgLinks = buildShapeLinks('flux://MessageShape', 'flux://Message', [
        { name: 'body', path: 'flux://body' },
      ]);
      const chanLinks = buildShapeLinks('flux://ChannelShape', 'flux://Channel', [
        { name: 'name', path: 'flux://name' },
      ]);

      const client = createMockClient({
        getAllShacl: jest.fn().mockResolvedValue([
          { name: 'Message', shapeUri: 'flux://MessageShape', links: msgLinks },
          { name: 'Channel', shapeUri: 'flux://ChannelShape', links: chanLinks },
        ]),
      });
      const proxy = createProxy(client);

      const shapes = await proxy.getAllShacl();

      expect(client.getAllShacl).toHaveBeenCalledWith('test-uuid');
      expect(shapes.length).toBe(2);
      expect(shapes[0].name).toBe('Message');
      expect(shapes[0].shape.targetClass).toBe('flux://Message');
      expect(shapes[1].name).toBe('Channel');
      expect(shapes[1].shape.targetClass).toBe('flux://Channel');
    });

    it('returns an empty array when no shapes exist', async () => {
      const proxy = createProxy();
      const shapes = await proxy.getAllShacl();
      expect(shapes).toEqual([]);
    });

    it('passes every entry through when SHACLShape.fromLinks succeeds', async () => {
      // Named for what it actually asserts: `SHACLShape.fromLinks` always
      // returns a shape (even for the empty-links case — an empty shape
      // with no targetClass is still a shape), so the proxy's `s !== null`
      // filter is only reachable if `fromLinks` itself is mocked to return
      // null. The original name ("filters out shapes that fail
      // reconstruction") described a case the production path can't
      // produce; that was dead coverage.
      const client = createMockClient({
        getAllShacl: jest.fn().mockResolvedValue([
          { name: 'Good', shapeUri: 'app://GoodShape', links: buildShapeLinks('app://GoodShape', 'app://Good', [{ name: 'x', path: 'app://x' }]) },
          { name: 'Empty', shapeUri: 'app://EmptyShape', links: [] },
        ]),
      });
      const proxy = createProxy(client);

      const shapes = await proxy.getAllShacl();

      expect(shapes.length).toBe(2);
      expect(shapes.map(s => s.name).sort()).toEqual(['Empty', 'Good']);
    });

    it('skips a shape that cannot be decoded and returns the others', async () => {
      // An unknown transform variant makes SHACLShape.fromLinks throw for that one shape.
      const brokenLinks = [
        ...buildShapeLinks('app://BrokenShape', 'app://Broken', [{ name: 'img', path: 'app://img' }]),
        { source: 'app://BrokenShape.img', predicate: 'ad4m://transform', target: 'literal:string:{"type":"fromTheFuture"}' },
      ];
      const good = (n: string) => ({
        name: n,
        shapeUri: `app://${n}Shape`,
        links: buildShapeLinks(`app://${n}Shape`, `app://${n}`, [{ name: 'x', path: 'app://x' }]),
      });

      const client = createMockClient({
        getAllShacl: jest.fn().mockResolvedValue([
          good('A'),
          { name: 'Broken', shapeUri: 'app://BrokenShape', links: brokenLinks },
          good('B'),
          good('C'),
        ]),
      });
      const proxy = createProxy(client);
      const warn = jest.spyOn(console, 'warn').mockImplementation(() => {});

      try {
        const shapes = await proxy.getAllShacl();
        expect(shapes.map(s => s.name)).toEqual(['A', 'B', 'C']);
        expect(shapes.map(s => s.shape.targetClass)).toEqual(['app://A', 'app://B', 'app://C']);
        expect(warn).toHaveBeenCalledTimes(1);
        expect(warn.mock.calls[0][0]).toContain('"Broken"');
        expect(warn.mock.calls[0][1]).toBeInstanceOf(Error);
      } finally {
        warn.mockRestore();
      }
    });
  });
});

describe('addShacl', () => {
  it('registers the shape through the executor writer, like @Model classes', async () => {
    const addSdna = jest.fn().mockResolvedValue(true);
    const proxy = createProxy(createMockClient({ addSdna }));

    const shape = new SHACLShape('todo://Todo');
    shape.addProperty({ name: 'title', path: 'todo://title', maxCount: 1 });
    await proxy.addShacl('Todo', shape);

    expect(addSdna).toHaveBeenCalledWith('test-uuid', 'Todo', '', 'subject_class', JSON.stringify(shape.toJSON()));
  });

  it("sends the shape's own URI, which the executor keeps", async () => {
    const addSdna = jest.fn().mockResolvedValue(true);
    const proxy = createProxy(createMockClient({ addSdna }));

    await proxy.addShacl('Todo', new SHACLShape('shapes://TodoShape', 'todo://Todo'));

    expect(JSON.parse(addSdna.mock.calls[0][4]).node_shape_uri).toBe('shapes://TodoShape');
  });
});

describe('addFlow', () => {
  /** A client backed by an in-memory link store. */
  function storeClient() {
    let links: any[] = [];
    const matches = (l: any, q: any) =>
      (!q.source || l.data.source === q.source) && (!q.predicate || l.data.predicate === q.predicate);
    const add = (ls: any[]) => { links.push(...ls.map((data: any, i: number) => ({ data: { ...data }, timestamp: `${links.length + i}` }))); };
    return {
      links: () => links,
      add,
      client: createMockClient({
        queryLinks: jest.fn(async (_: string, q: any) => links.filter(l => matches(l, q))),
        addLinks: jest.fn(async (_: string, ls: any[]) => { add(ls); return []; }),
        linkMutations: jest.fn(async (_: string, m: any) => {
          links = links.filter(l => !m.removals.includes(l));
          add(m.additions);
          return { additions: [], removals: [] };
        }),
      }),
    };
  }

  const todoFlow = () => {
    const flow = new SHACLFlow('Todo', 'todo://');
    flow.addState({ name: 'ready', value: 0 });
    flow.addState({ name: 'done', value: 1 });
    flow.addTransition({ actionName: 'Complete', fromState: 'ready', toState: 'done', actions: [] });
    return flow;
  };
  const transitions = (flow: SHACLFlow | null) =>
    flow!.transitions.map(t => `${t.fromState}->${t.toState}:${t.actionName}`);

  it('replaces the stored transitions when a flow is re-added', async () => {
    const { client, add } = storeClient();
    const proxy = createProxy(client);

    // A flow stored with the old `{from}To{to}` transition URIs.
    const legacy = todoFlow().toLinks().map(l => ({ ...l,
      source: l.source.replace(/\.transition\/.*$/, '.readyTodone'),
      target: l.target.replace(/\.transition\/.*$/, '.readyTodone') }));
    add(legacy);
    await proxy.addFlow('Todo', todoFlow());
    expect(transitions(await proxy.getFlow('Todo'))).toEqual(['ready->done:Complete']);

    await proxy.addFlow('Todo', todoFlow());
    expect(transitions(await proxy.getFlow('Todo'))).toEqual(['ready->done:Complete']);
  });
});

describe('SHACLShape.fromLinks round-trip with RPC link format', () => {
  it('reconstructs property details from simplified link triples', () => {
    const shapeUri = 'recipe://RecipeShape';
    const links = [
      { source: shapeUri, predicate: 'sh://targetClass', target: 'recipe://Recipe' },
      { source: shapeUri, predicate: 'sh://property', target: `${shapeUri}.title` },
      { source: `${shapeUri}.title`, predicate: 'sh://path', target: 'recipe://title' },
      { source: `${shapeUri}.title`, predicate: 'sh://datatype', target: 'xsd:string' },
      { source: `${shapeUri}.title`, predicate: 'sh://minCount', target: 'literal:1^^xsd:integer' },
      { source: `${shapeUri}.title`, predicate: 'sh://maxCount', target: 'literal:1^^xsd:integer' },
      { source: shapeUri, predicate: 'sh://property', target: `${shapeUri}.servings` },
      { source: `${shapeUri}.servings`, predicate: 'sh://path', target: 'recipe://servings' },
      { source: `${shapeUri}.servings`, predicate: 'sh://datatype', target: 'xsd:integer' },
    ];

    const shape = SHACLShape.fromLinks(links as any, shapeUri);

    expect(shape.nodeShapeUri).toBe(shapeUri);
    expect(shape.targetClass).toBe('recipe://Recipe');
    expect(shape.properties.length).toBe(2);

    const title = shape.properties.find(p => p.name === 'title')!;
    expect(title.path).toBe('recipe://title');
    expect(title.datatype).toBe('xsd:string');
    expect(title.minCount).toBe(1);
    expect(title.maxCount).toBe(1);

    const servings = shape.properties.find(p => p.name === 'servings')!;
    expect(servings.path).toBe('recipe://servings');
    expect(servings.datatype).toBe('xsd:integer');
  });
});
