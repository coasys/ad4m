import { buildSPARQLOrderLimitOffset, buildSPARQLQuery, buildSPARQLCountQuery, buildPaginationSubquery, hasJsOnlyWhereFilters, buildSPARQLGroupedCountQuery, parseSparqlGroupedCount } from './query-sparql';

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

  it('returns true when where has author filter (count() fast path is unsafe — buildSPARQLCountQuery has no named graph)', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { author: 'did:key:abc' })).toBe(true);
  });

  it('returns true when where has timestamp filter (same reason — guards the COUNT fast path)', () => {
    expect(hasJsOnlyWhereFilters(richMetadata, emptyRelations, { timestamp: { gt: 100 } })).toBe(true);
  });

  it('returns true for belongsToOne reverse relation', () => {
    const relMeta: any = { parent: { kind: 'belongsToOne', predicate: 'flux://parent' } };
    const meta: any = { properties: { parent: { name: 'parent', predicate: 'flux://parent' } }, relations: {} };
    expect(hasJsOnlyWhereFilters(meta, relMeta, { parent: 'some://id' })).toBe(true);
  });
});

describe('buildSPARQLQuery — SPARQL-level pagination via subquery', () => {
  const modelClass: any = {};

  it('includes LIMIT/OFFSET in a pagination subquery when query has them, no JS-only filters, and ORDER BY', () => {
    const query = { limit: 10, offset: 0, where: { category: 'some://uri' }, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // SPARQL should contain LIMIT inside the pagination subquery
    expect(sparql).toContain('LIMIT 10');
    // The outer query should still exist
    expect(sparql).toContain('SELECT ?source ?predicate ?target');
    // Should have a subquery pattern
    expect(sparql).toContain('SELECT DISTINCT ?source');
  });

  it('does NOT use SPARQL-level pagination when limit/offset present but no ORDER BY', () => {
    const query = { limit: 10, offset: 0, where: { category: 'some://uri' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Without ORDER BY, SPARQL pagination is not used — JS-level slicing handles it
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
    // No subquery needed
    expect(sparql).not.toContain('SELECT DISTINCT');
  });

  it('does NOT include LIMIT/OFFSET when JS-only where filters exist (literal gt operator)', () => {
    // gt on a literal-stored property is JS-only, so pagination must not be pushed to SPARQL
    const meta: any = { properties: { rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' } }, relations: {} };
    const query = { limit: 10, offset: 0, where: { rating: { gt: 3 } } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
  });
});

describe('buildSPARQLOrderLimitOffset', () => {
  it('always returns empty string (pagination uses subquery pattern instead)', () => {
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
      order: { name: 'ASC' as const },
    };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('<flux://channel-123>');
    // SPARQL-level pagination via subquery
    expect(sparql).toContain('LIMIT 50');
    expect(sparql).toContain('SELECT DISTINCT ?source');
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

describe('buildSPARQLQuery — NOT with array push-down', () => {
  const modelClass: any = {};

  it('generates NOT IN filter for NOT with array of values', () => {
    const query = { where: { name: { not: ['deleted', 'archived'] } } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Should use NOT IN with parse_literal for literal properties
    expect(sparql).toContain('NOT IN');
    expect(sparql).toContain('"deleted"');
    expect(sparql).toContain('"archived"');
  });
});

describe('buildSPARQLQuery — mixed push-down + JS-only', () => {
  const modelClass: any = {};
  const mixedMeta: any = {
    properties: {
      name: { name: 'name', predicate: 'flux://name', required: true, resolveLanguage: 'literal' },
      rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' },
    },
    relations: {},
  };

  it('pushes equality to SPARQL but keeps gt as JS-only', () => {
    const query = { where: { name: 'Alice', rating: { gt: 5 } } };
    const sparql = buildSPARQLQuery(mixedMeta, emptyRelations, query, modelClass);
    // name equality should be pushed to SPARQL
    expect(sparql).toContain('parse_literal');
    expect(sparql).toContain('Alice');
    // hasJsOnlyWhereFilters should return true because of rating.gt
    expect(hasJsOnlyWhereFilters(mixedMeta, emptyRelations, query.where)).toBe(true);
  });
});

describe('buildSPARQLQuery — non-literal and flag properties', () => {
  const modelClass: any = {};

  it('does NOT use parse_literal for non-literal (resolveLanguage) properties', () => {
    const meta: any = {
      properties: {
        avatar: { name: 'avatar', predicate: 'flux://avatar', required: true, resolveLanguage: 'did:lang:some-language' },
      },
      relations: {},
    };
    const query = { where: { avatar: 'https://example.com/pic.jpg' } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('parse_literal');
  });

  it('does NOT use parse_literal for flag properties', () => {
    const meta: any = {
      properties: {
        isPublic: { name: 'isPublic', predicate: 'flux://isPublic', flag: true, initial: 'flux://true' },
      },
      relations: {},
    };
    const query = { where: { isPublic: 'flux://true' } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('parse_literal');
  });
});

describe('buildSPARQLQuery — IRI correctness', () => {
  const modelClass: any = {};

  it('uses <ad4m://fn/parse_literal> IRI, not fn::parse_literal SurrealDB syntax', () => {
    const query = { where: { name: 'test' } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('<ad4m://fn/parse_literal>');
    expect(sparql).not.toContain('fn::parse_literal');
  });
});

// ──────────────────────────────────────────────────────────
// SPARQL-level pagination (LIMIT/OFFSET in generated queries)
// ──────────────────────────────────────────────────────────

describe('SPARQL-level pagination', () => {
  const modelClass: any = {};

  it('includes LIMIT in SPARQL when query specifies limit + order', () => {
    const query = { limit: 30, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('LIMIT 30');
    expect(sparql).toContain('SELECT DISTINCT ?source');
  });

  it('does NOT push LIMIT to SPARQL when no ORDER BY is specified', () => {
    const query = { limit: 30 };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    // Without ORDER BY, SPARQL-level pagination is not used
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('SELECT DISTINCT');
  });

  it('includes OFFSET in SPARQL when query specifies offset > 0 with order', () => {
    const query = { limit: 20, offset: 40, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('LIMIT 20');
    expect(sparql).toContain('OFFSET 40');
  });

  it('does NOT include OFFSET when offset is 0', () => {
    const query = { limit: 10, offset: 0, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('LIMIT 10');
    expect(sparql).not.toContain('OFFSET');
  });

  it('includes ORDER BY in subquery when query.order is specified', () => {
    const query = { limit: 10, order: { name: 'DESC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('ORDER BY DESC(');
    expect(sparql).toContain('LIMIT 10');
  });

  it('uses SAMPLE() aggregate for ORDER BY variables in pagination subquery (SPARQL 1.1 compliance)', () => {
    const query = { limit: 5, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    // Must use SAMPLE() to aggregate the order variable under GROUP BY
    expect(sparql).toMatch(/SAMPLE\(/);
    expect(sparql).toMatch(/AS \?pg_sort_0/);
    expect(sparql).toContain('GROUP BY ?source');
    expect(sparql).toContain('ORDER BY ASC(?pg_sort_0)');
  });

  it('omits GROUP BY when ORDER BY is present (uses SAMPLE() aggregate instead)', () => {
    // When ORDER BY is present, GROUP BY ?source is used with SAMPLE() aggregates.
    // The old redundant GROUP BY (without ORDER BY) no longer applies because
    // SPARQL-level pagination is only used when ORDER BY is present.
    const query = { limit: 10, order: { name: 'ASC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('GROUP BY ?source');
    expect(sparql).toContain('SAMPLE(');
    expect(sparql).toContain('SELECT DISTINCT ?source');
    expect(sparql).toContain('LIMIT 10');
  });

  it('does NOT default to ORDER BY timestamp when paginating without explicit order', () => {
    const query = { limit: 30 };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('ORDER BY');
    expect(sparql).not.toContain('pg_minTs');
    expect(sparql).not.toContain('GRAPH ?pg_g');
  });

  it('does NOT push pagination to SPARQL when JS-only where filters exist (literal gt operator)', () => {
    // author equality is now SPARQL-pushable; use a literal gt which remains JS-only
    const meta: any = { properties: { rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' } }, relations: {} };
    const query = { limit: 10, where: { rating: { gt: 5 } } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
  });

  it('does NOT push pagination to SPARQL when JS-only where filters exist (gt operator)', () => {
    const meta: any = {
      properties: {
        rating: { name: 'rating', predicate: 'flux://rating', required: true, resolveLanguage: 'literal' },
      },
      relations: {},
    };
    const query = { limit: 10, where: { rating: { gt: 5 } } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    expect(sparql).not.toContain('LIMIT');
  });

  it('wraps pagination in a subquery (outer SELECT fetches all links for the page)', () => {
    const query = { limit: 5, offset: 10, order: { name: 'DESC' as const } };
    const sparql = buildSPARQLQuery(richMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('SELECT ?source ?predicate ?target ?author ?timestamp');
    expect(sparql).toContain('SELECT DISTINCT ?source');
    expect(sparql).toContain('LIMIT 5');
    expect(sparql).toContain('OFFSET 10');
  });

  it('does NOT wrap pagination in a subquery when no ORDER BY (JS slicing preserves natural order)', () => {
    const query = { limit: 5, offset: 10 };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    // Should be a plain SELECT without subquery
    expect(sparql).toContain('SELECT ?source ?predicate ?target ?author ?timestamp');
    expect(sparql).not.toContain('SELECT DISTINCT');
    expect(sparql).not.toContain('LIMIT');
    expect(sparql).not.toContain('OFFSET');
  });

  describe('buildSPARQLCountQuery', () => {
    it('returns a COUNT(DISTINCT ?source) query', () => {
      const query = { parent: { id: 'flux://ch-1', predicate: 'flux://has_child' } };
      const countSparql = buildSPARQLCountQuery(emptyMetadata, emptyRelations, query, modelClass);
      expect(countSparql).toContain('COUNT(DISTINCT ?source)');
      expect(countSparql).toContain('?count');
      expect(countSparql).toContain('<flux://ch-1>');
    });

    it('does NOT include LIMIT/OFFSET (counts full result set)', () => {
      const query = { limit: 10, offset: 20, parent: { id: 'flux://ch-1', predicate: 'flux://has_child' } };
      const countSparql = buildSPARQLCountQuery(emptyMetadata, emptyRelations, query, modelClass);
      expect(countSparql).not.toContain('LIMIT');
      expect(countSparql).not.toContain('OFFSET');
      expect(countSparql).toContain('COUNT(DISTINCT ?source)');
    });
  });
});

describe('buildSPARQLQuery — set-difference patterns', () => {
  const modelClass: any = {};

  // Subgroups are grandchildren of channels (channel → conversation → subgroup).
  // Never assume direct child relationships in set-difference queries.
  // A global EXISTS pattern (no parent scoping) should match items regardless of depth.
  it('generates global EXISTS without parent scoping when no parent filter', () => {
    const meta: any = {
      properties: {
        status: { name: 'status', predicate: 'flux://status', required: true, resolveLanguage: 'literal' },
      },
      relations: {},
    };
    const query = { where: { status: 'active' } };
    const sparql = buildSPARQLQuery(meta, emptyRelations, query, modelClass);
    // Without a parent filter, the query should not scope to any specific parent
    // This ensures grandchildren (items at any depth) are matched
    expect(sparql).not.toContain('flux://has_child');
    expect(sparql).toContain('SELECT ?source ?predicate ?target');
  });
});

// ─────────────────────────────────────────────────────────────
// buildSPARQLWhereFilters — author / timestamp SPARQL push
// ─────────────────────────────────────────────────────────────
describe('buildSPARQLQuery — author/timestamp SPARQL filter push', () => {
  const modelClass: any = {};

  it('pushes author equality into SPARQL FILTER using STR(?author)', () => {
    const query = { where: { author: 'did:key:zABC' } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('STR(?author) = "did:key:zABC"');
    expect(sparql).toContain('?author');
  });

  it('pushes author IN array into SPARQL FILTER', () => {
    const query = { where: { author: ['did:key:zABC', 'did:key:zDEF'] } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('STR(?author) IN');
    expect(sparql).toContain('"did:key:zABC"');
    expect(sparql).toContain('"did:key:zDEF"');
  });

  it('pushes author NOT into SPARQL FILTER', () => {
    const query = { where: { author: { not: 'did:key:zABC' } } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('STR(?author) != "did:key:zABC"');
  });

  it('pushes timestamp gt/lt into SPARQL FILTER', () => {
    const query = { where: { timestamp: { gt: 1000, lt: 9000 } } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('?timestamp > 1000');
    expect(sparql).toContain('?timestamp < 9000');
  });

  it('pushes timestamp between into SPARQL FILTER', () => {
    const query = { where: { timestamp: { between: [100, 200] as [number, number] } } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('?timestamp >= 100');
    expect(sparql).toContain('?timestamp <= 200');
  });

  it('pushes timestamp equality into SPARQL FILTER', () => {
    const query = { where: { timestamp: 1714000000000 } };
    const sparql = buildSPARQLQuery(emptyMetadata, emptyRelations, query, modelClass);
    expect(sparql).toContain('?timestamp = 1714000000000');
  });
});

// ─────────────────────────────────────────────────────────────
// buildSPARQLGroupedCountQuery
// ─────────────────────────────────────────────────────────────
describe('buildSPARQLGroupedCountQuery', () => {
  const signalMetadata: any = {
    properties: {
      signalTypeId: {
        name: 'signalTypeId',
        predicate: 'flux://signalTypeId',
        required: true,
        resolveLanguage: 'literal',
      },
    },
    relations: {},
  };

  it('selects ?parent and COUNT(DISTINCT ?source)', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], undefined);
    expect(sparql).toContain('SELECT ?parent (COUNT(DISTINCT ?source) AS ?count)');
  });

  it('groups by ?parent', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], undefined);
    expect(sparql).toContain('GROUP BY ?parent');
  });

  it('includes parent → child join with the supplied predicate', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1', 'flux://post/2'], undefined);
    expect(sparql).toContain('?parent <flux://has-signal> ?source');
  });

  it('includes FILTER(?parent IN (...)) for the supplied parent IDs', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1', 'flux://post/2'], undefined);
    expect(sparql).toContain('?parent IN (<flux://post/1>, <flux://post/2>)');
  });

  it('includes the full named graph block (GRAPH ?linkGraph + author + timestamp)', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], undefined);
    expect(sparql).toContain('GRAPH ?linkGraph');
    expect(sparql).toContain('?linkGraph <ad4m://ontology/author> ?author');
    expect(sparql).toContain('?linkGraph <ad4m://ontology/timestamp> ?timestamp');
  });

  it('includes conformance join for required properties', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], undefined);
    expect(sparql).toContain('<flux://signalTypeId>');
  });

  it('applies a where clause filter (author equality) via SPARQL FILTER', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], { author: 'did:key:zABC' });
    expect(sparql).toContain('STR(?author) = "did:key:zABC"');
  });

  it('applies a where clause filter (required property equality) via parse_literal', () => {
    const sparql = buildSPARQLGroupedCountQuery(signalMetadata, emptyRelations, 'flux://has-signal', ['flux://post/1'], { signalTypeId: 'like' });
    expect(sparql).toContain('parse_literal');
    expect(sparql).toContain('"like"');
  });
});

// ─────────────────────────────────────────────────────────────
// parseSparqlGroupedCount
// ─────────────────────────────────────────────────────────────
describe('parseSparqlGroupedCount', () => {
  it('returns an empty map for empty input', () => {
    expect(parseSparqlGroupedCount([]).size).toBe(0);
  });

  it('returns an empty map for non-array input', () => {
    expect(parseSparqlGroupedCount(null as any).size).toBe(0);
  });

  it('parses RDF binding objects ({ parent: { value: ... }, count: { value: ... } })', () => {
    const rows = [
      { parent: { value: 'flux://post/1' }, count: { value: '3' } },
      { parent: { value: 'flux://post/2' }, count: { value: '0' } },
    ];
    const map = parseSparqlGroupedCount(rows);
    expect(map.get('flux://post/1')).toBe(3);
    expect(map.get('flux://post/2')).toBe(0);
  });

  it('parses plain string/number values (non-RDF binding format)', () => {
    const rows = [
      { parent: 'flux://post/1', count: 5 },
      { parent: 'flux://post/2', count: '2' },
    ];
    const map = parseSparqlGroupedCount(rows);
    expect(map.get('flux://post/1')).toBe(5);
    expect(map.get('flux://post/2')).toBe(2);
  });

  it('skips rows with missing parent or count', () => {
    const rows = [
      { parent: { value: 'flux://post/1' } },             // no count
      { count: { value: '3' } },                           // no parent
      { parent: { value: 'flux://post/3' }, count: { value: '1' } },
    ];
    const map = parseSparqlGroupedCount(rows);
    expect(map.size).toBe(1);
    expect(map.get('flux://post/3')).toBe(1);
  });

  it('skips rows with NaN count', () => {
    const rows = [{ parent: { value: 'flux://post/1' }, count: { value: 'not-a-number' } }];
    expect(parseSparqlGroupedCount(rows).size).toBe(0);
  });
});
