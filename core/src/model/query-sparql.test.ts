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

  it('returns false when where has literal-stored property with equality filter (pushed to SPARQL)', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { name: 'Pasta' })).toBe(false);
  });

  it('returns true when where has literal-stored property with comparison operator', () => {
    // gt/lt/gte/lte/between/contains remain JS-only
    const meta: any = { properties: { rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' } }, relations: {} };
    expect(hasJsOnlyWhereFilters(meta, emptyRelations, { rating: { gt: 5 } })).toBe(true);
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

describe('buildSPARQLQuery — pagination handled in JS, not SPARQL', () => {
  const modelClass: any = {};

  it('does NOT include LIMIT/OFFSET in SPARQL even when query has them', () => {
    const query = { limit: 10, offset: 0, where: { category: 'some://uri' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // SPARQL should NOT contain LIMIT/OFFSET — pagination is done in JS
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
    // The outer query should still exist
    expect(sparql).toContain('SELECT ?source ?predicate ?target');
  });

  it('does NOT include LIMIT/OFFSET even with JS-only where filters', () => {
    const query = { limit: 10, offset: 0, where: { name: 'Pasta' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
  });
});

describe('buildSPARQLOrderLimitOffset', () => {
  it('always returns empty string (pagination is handled in JS)', () => {
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, {})).toBe('');
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, { limit: 50 })).toBe('');
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, { limit: 50, offset: 100 })).toBe('');
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, { order: { timestamp: 'DESC' } })).toBe('');
    expect(buildSPARQLOrderLimitOffset(emptyMetadata, {
      order: { timestamp: 'DESC' },
      limit: 50,
      offset: 100,
    })).toBe('');
  });
});

describe('buildSPARQLQuery — parse_literal push-down filters', () => {
  const modelClass: any = {};

  it('generates parse_literal FILTER for equality on literal property', () => {
    const query = { where: { name: 'Alice' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('<ad4m://fn/parse_literal>');
    expect(sparql).toContain('STR(<ad4m://fn/parse_literal>(?wTarget_name)) = "Alice"');
  });

  it('generates parse_literal IN FILTER for array on literal property', () => {
    const query = { where: { name: ['Alice', 'Bob'] } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('STR(<ad4m://fn/parse_literal>(?wTarget_name)) IN ("Alice", "Bob")');
  });

  it('generates parse_literal NOT FILTER for not condition', () => {
    const query = { where: { name: { not: 'deleted' } } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('STR(<ad4m://fn/parse_literal>(?wTarget_name)) != "deleted"');
  });

  it('does NOT use parse_literal for comparison operators (gt/lt)', () => {
    const meta: any = {
      properties: {
        rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' },
      },
      relations: {},
    };
    const query = { where: { rating: { gt: 5 } } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('parse_literal');
    expect(sparql).toContain('?wTarget_cmp_rating');
  });

  it('does NOT use parse_literal for non-literal properties', () => {
    const query = { where: { category: 'some://uri' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('parse_literal');
    expect(sparql).toContain('<some://uri>');
  });
});

describe('buildSPARQLQuery — structural correctness', () => {
  const modelClass: any = {};

  it('generates valid SPARQL with parent filter', () => {
    const query = {
      limit: 50,
      offset: 0,
      parent: { id: 'flux://channel-123', predicate: 'flux://has_message' },
      where: { category: 'some://uri' },
    };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('<flux://channel-123>');
    // No SPARQL-level pagination
    expect(sparql).not.toContain('LIMIT');
  });

  it('does not allow injection through where clause IRI values', () => {
    const query = {
      where: { category: 'some://uri"> . } UNION { SELECT * WHERE { ?s ?p ?o' },
    };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Value should be wrapped in angle brackets as an IRI
    expect(sparql).toContain('<some://uri');
  });
});
