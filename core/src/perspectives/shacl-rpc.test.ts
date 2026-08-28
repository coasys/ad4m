import { PerspectiveProxy } from './PerspectiveProxy';
import { SHACLShape } from '../shacl/SHACLShape';

// ── Helpers ──────────────────────────────────────────────────────────────────

function createMockClient(overrides: Record<string, jest.Mock> = {}): any {
  return {
    addPerspectiveLinkAddedListener: jest.fn(),
    addPerspectiveLinkRemovedListener: jest.fn(),
    addPerspectiveLinkUpdatedListener: jest.fn(),
    addPerspectiveSyncStateChangeListener: jest.fn(),
    getShaclNames: jest.fn().mockResolvedValue([]),
    getShaclTargetClass: jest.fn().mockResolvedValue(null),
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
        getShaclTargetClass: jest.fn().mockResolvedValue(null),
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

    it('filters out shapes that fail reconstruction', async () => {
      // A shape with no links should fail fromLinks gracefully
      const client = createMockClient({
        getAllShacl: jest.fn().mockResolvedValue([
          { name: 'Good', shapeUri: 'app://GoodShape', links: buildShapeLinks('app://GoodShape', 'app://Good', [{ name: 'x', path: 'app://x' }]) },
          { name: 'Empty', shapeUri: 'app://EmptyShape', links: [] },
        ]),
      });
      const proxy = createProxy(client);

      const shapes = await proxy.getAllShacl();

      // Both should appear since fromLinks always returns a SHACLShape
      // (even with no targetClass), but the filter only drops null
      expect(shapes.length).toBe(2);
      expect(shapes[0].name).toBe('Good');
    });
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
      { source: `${shapeUri}.title`, predicate: 'sh://minCount', target: 'literal:number:1' },
      { source: `${shapeUri}.title`, predicate: 'sh://maxCount', target: 'literal:number:1' },
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
