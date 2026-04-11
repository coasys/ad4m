import { buildSPARQLOrderLimitOffset, buildSPARQLQuery, hasJsOnlyWhereFilters } from './query-sparql';

// Minimal stubs for ModelMetadata — buildSPARQLOrderLimitOffset only uses the query arg
const emptyMetadata: any = { properties: {}, relations: {} };

// Metadata with a required (non-literal) property and a literal-stored property
const richMetadata: any = {
  properties: {
    name: {
      name: 'name',
      predicate: 'flux://name',
      required: true,
      resolveLanguage: 'literal',
    },
    category: {
      name: 'category',
      predicate: 'flux://category',
      required: true,
      resolveLanguage: 'did:lang:abc',  // non-literal → URI stored
    },
    description: {
      name: 'description',
      predicate: 'flux://description',
      initial: 'literal://string:empty',
      resolveLanguage: 'literal',
    },
  },
  relations: {},
};

const emptyRelations: any = {};

describe('hasJsOnlyWhereFilters', () => {
  it('returns false when no where clause', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, undefined)).toBe(false);
  });

  it('returns true when where has literal-stored property filter', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { name: 'Pasta' })).toBe(true);
  });

  it('returns false when where only has non-literal property filter', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { category: 'some://uri' })).toBe(false);
  });

  it('returns true when where has author filter', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { author: 'did:key:abc' })).toBe(true);
  });

  it('returns true when where has timestamp filter', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { timestamp: { gt: 100 } })).toBe(true);
  });

  it('returns true for belongsToOne reverse relation', () => {
    const relMeta: any = { parent: { kind: 'belongsToOne', predicate: 'flux://parent' } };
    const meta: any = { properties: { parent: { name: 'parent', predicate: 'flux://parent' } }, relations: {} };
    expect(hasJsOnlyWhereFilters(meta, relMeta, { parent: 'some://id' })).toBe(true);
  });
});

describe('buildSPARQLQuery — pagination disabled with JS-only filters', () => {
  const modelClass: any = {};

  it('does NOT include subquery pagination when JS-only where filters exist', () => {
    const query = { limit: 10, offset: 0, where: { name: 'Pasta' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Should NOT have the inner SELECT DISTINCT ?source ... LIMIT pattern
    // because name is literal-stored → JS-only filter
    expect(sparql).not.toMatch(/SELECT DISTINCT \?source.*LIMIT/s);
    // The outer query should still exist
    expect(sparql).toContain('SELECT ?source ?predicate ?target');
  });

  it('DOES include subquery pagination when all filters are SPARQL-capable', () => {
    const query = { limit: 10, offset: 0, where: { category: 'some://uri' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Should have the paginated subquery form
    expect(sparql).toMatch(/SELECT DISTINCT \?source/);
    expect(sparql).toContain('LIMIT 10');
    expect(sparql).toContain('OFFSET 0');
  });
});

describe('buildSPARQLOrderLimitOffset', () => {
  it('returns empty string when no pagination options', () => {
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, {})).toBe('');
  });

  it('returns LIMIT clause', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { limit: 50 });
    expect(result).toBe('LIMIT 50');
  });

  it('returns LIMIT + OFFSET clauses', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { limit: 50, offset: 100 });
    expect(result).toContain('LIMIT 50');
    expect(result).toContain('OFFSET 100');
  });

  it('returns ORDER BY DESC for timestamp', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { order: { timestamp: 'DESC' } });
    expect(result).toBe('ORDER BY DESC(?timestamp)');
  });

  it('returns ORDER BY ASC for a property', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { order: { name: 'ASC' } });
    expect(result).toBe('ORDER BY ASC(?wTarget_name)');
  });

  it('returns combined ORDER BY + LIMIT + OFFSET', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, {
      order: { timestamp: 'DESC' },
      limit: 50,
      offset: 100,
    });
    expect(result).toContain('ORDER BY DESC(?timestamp)');
    expect(result).toContain('LIMIT 50');
    expect(result).toContain('OFFSET 100');
  });

  it('handles multiple order fields', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, {
      order: { timestamp: 'DESC', name: 'ASC' },
    });
    expect(result).toContain('DESC(?timestamp)');
    expect(result).toContain('ASC(?wTarget_name)');
  });

  it('handles offset: 0', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { limit: 10, offset: 0 });
    expect(result).toContain('LIMIT 10');
    expect(result).toContain('OFFSET 0');
  });

  it('ORDER BY maps required property to cfTarget_ variable', () => {
    const meta: any = {
      properties: {
        name: { name: 'name', predicate: 'flux://name', required: true },
      },
      relations: {},
    };
    const result = buildSPARQLOrderLimitOffset(meta, { order: { name: 'ASC' } });
    expect(result).toBe('ORDER BY ASC(?cfTarget_name)');
  });

  it('ORDER BY maps optional initial-value property to cfInitTarget_ variable', () => {
    const meta: any = {
      properties: {
        description: { name: 'description', predicate: 'flux://desc', initial: 'literal://string:empty' },
      },
      relations: {},
    };
    const result = buildSPARQLOrderLimitOffset(meta, { order: { description: 'DESC' } });
    expect(result).toBe('ORDER BY DESC(?cfInitTarget_description)');
  });

  it('ORDER BY keeps timestamp as ?timestamp (special case)', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { order: { timestamp: 'DESC' } });
    expect(result).toBe('ORDER BY DESC(?timestamp)');
    expect(result).not.toContain('cfTarget_timestamp');
  });

  it('ORDER BY falls back to wTarget_ for unknown properties', () => {
    const result = buildSPARQLOrderLimitOffset(emptyMetadata, { order: { foo: 'ASC' } });
    expect(result).toBe('ORDER BY ASC(?wTarget_foo)');
  });
});
