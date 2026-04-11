/**
 * Tests for batched hydrateRelations() — verifies the N+1 → 1 SPARQL optimisation.
 *
 * We mock PerspectiveProxy to count querySparql / get calls and verify
 * that the batched path issues 1 SPARQL query instead of N perspective.get() calls.
 */

import { hydrateRelations } from './hydration';

// ── Helpers to build minimal mocks ──────────────────────────────────────

function makeMockPerspective(opts: {
  sparqlResults?: { source: string; target: string }[];
  sparqlError?: boolean;
  getResults?: Map<string, { data: { source: string; target: string; predicate: string } }[]>;
}) {
  const callCounts = { querySparql: 0, get: 0, findAll: 0 };

  const perspective = {
    querySparql: jest.fn(async (_q: string) => {
      callCounts.querySparql++;
      if (opts.sparqlError) throw new Error('SPARQL unavailable');
      return {
        results: {
          bindings: (opts.sparqlResults ?? []).map(r => ({
            source: { value: r.source },
            target: { value: r.target },
          })),
        },
      };
    }),
    get: jest.fn(async (query: any) => {
      callCounts.get++;
      const targetId = query.target;
      return opts.getResults?.get(targetId) ?? [];
    }),
  };

  return { perspective, callCounts };
}

function makeMockTargetClass(knownInstances: Map<string, any>) {
  class MockTarget {
    static __metadata = { className: 'MockTarget' };
    id: string;
    perspective: any;
    constructor(perspective: any, id: string) {
      this.id = id;
      this.perspective = perspective;
    }
    async get() {
      const data = knownInstances.get(this.id);
      if (data) Object.assign(this, data);
      return this;
    }
    static async findAll(_perspective: any, query: any) {
      const ids: string[] = query?.where?.id ?? [];
      return ids
        .filter(id => knownInstances.has(id))
        .map(id => {
          const inst = { id, ...knownInstances.get(id) };
          return inst;
        });
    }
  }
  return MockTarget;
}

// Minimal decorator metadata mock
jest.mock('./decorators', () => ({
  getPropertiesMetadata: () => ({}),
  getRelationsMetadata: (cls: any) => cls.__relMeta ?? {},
  buildConformanceFilter: () => () => true,
}));

jest.mock('./query-utils', () => ({
  compileWhereClause: () => '',
}));

jest.mock('../utils', () => ({
  escapeQueryString: (s: string) => s,
}));

// ── Tests ───────────────────────────────────────────────────────────────

describe('hydrateRelations batching', () => {
  const PREDICATE = 'flux://belongs_to';

  it('issues 1 SPARQL query instead of N perspective.get() calls for belongsToMany', async () => {
    const N = 50;
    // Create N parent instances
    const instances = Array.from({ length: N }, (_, i) => ({
      id: `parent-${i}`,
      children: undefined as any,
    }));

    // Each parent has 2 children pointing to it
    const sparqlResults: { source: string; target: string }[] = [];
    const knownChildren = new Map<string, any>();
    for (let i = 0; i < N; i++) {
      for (let j = 0; j < 2; j++) {
        const childId = `child-${i}-${j}`;
        sparqlResults.push({ source: childId, target: `parent-${i}` });
        knownChildren.set(childId, { name: `Child ${i}-${j}` });
      }
    }

    const { perspective, callCounts } = makeMockPerspective({ sparqlResults });
    const TargetClass = makeMockTargetClass(knownChildren);

    // Wire up relation metadata
    const modelClass = {
      __relMeta: {
        children: {
          kind: 'belongsToMany',
          predicate: PREDICATE,
          target: () => TargetClass,
          maxCount: undefined,
        },
      },
    };

    // Mock getRelationsMetadata to return our metadata
    const decorators = require('./decorators');
    decorators.getRelationsMetadata = () => modelClass.__relMeta;

    await hydrateRelations(
      modelClass,
      instances,
      perspective as any,
      { children: true },
    );

    // Key assertion: 1 SPARQL query, 0 perspective.get() calls
    expect(callCounts.querySparql).toBe(1);
    expect(callCounts.get).toBe(0);

    // Verify data correctness
    for (let i = 0; i < N; i++) {
      expect(instances[i].children).toHaveLength(2);
      expect(instances[i].children[0].id).toMatch(/^child-/);
    }

    console.log(`✅ Batched: ${N} instances hydrated with 1 SPARQL query + 1 findAll (was ${N} sequential perspective.get calls)`);
  });

  it('issues 1 SPARQL query for belongsToOne', async () => {
    const N = 20;
    const instances = Array.from({ length: N }, (_, i) => ({
      id: `parent-${i}`,
      owner: undefined as any,
    }));

    const sparqlResults: { source: string; target: string }[] = [];
    const knownOwners = new Map<string, any>();
    for (let i = 0; i < N; i++) {
      const ownerId = `owner-${i}`;
      sparqlResults.push({ source: ownerId, target: `parent-${i}` });
      knownOwners.set(ownerId, { name: `Owner ${i}` });
    }

    const { perspective, callCounts } = makeMockPerspective({ sparqlResults });
    const TargetClass = makeMockTargetClass(knownOwners);

    const modelClass = {
      __relMeta: {
        owner: {
          kind: 'belongsToOne',
          predicate: PREDICATE,
          target: () => TargetClass,
          maxCount: 1,
        },
      },
    };

    const decorators = require('./decorators');
    decorators.getRelationsMetadata = () => modelClass.__relMeta;

    await hydrateRelations(modelClass, instances, perspective as any, { owner: true });

    expect(callCounts.querySparql).toBe(1);
    expect(callCounts.get).toBe(0);

    for (let i = 0; i < N; i++) {
      expect(instances[i].owner).toBeTruthy();
      expect(instances[i].owner.id).toBe(`owner-${i}`);
    }

    console.log(`✅ Batched belongsToOne: ${N} instances, 1 SPARQL query (was ${N} perspective.get calls)`);
  });

  it('falls back to sequential on SPARQL failure', async () => {
    const instances = [{ id: 'p-0', children: undefined as any }];

    const getResults = new Map<string, any[]>();
    getResults.set('p-0', [
      { data: { source: 'c-0', target: 'p-0', predicate: PREDICATE } },
    ]);

    const { perspective, callCounts } = makeMockPerspective({
      sparqlError: true,
      getResults,
    });
    const TargetClass = makeMockTargetClass(new Map([['c-0', { name: 'Child' }]]));

    const modelClass = {
      __relMeta: {
        children: {
          kind: 'belongsToMany',
          predicate: PREDICATE,
          target: () => TargetClass,
          maxCount: undefined,
        },
      },
    };

    const decorators = require('./decorators');
    decorators.getRelationsMetadata = () => modelClass.__relMeta;

    await hydrateRelations(modelClass, instances, perspective as any, { children: true });

    // Should have fallen back to perspective.get
    expect(callCounts.querySparql).toBe(1); // attempted
    expect(callCounts.get).toBe(1); // fallback

    expect(instances[0].children).toHaveLength(1);
    console.log('✅ Fallback to sequential works when SPARQL fails');
  });
});
