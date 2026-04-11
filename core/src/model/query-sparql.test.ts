import { buildSPARQLOrderLimitOffset } from './query-sparql';

// Minimal stubs for ModelMetadata — buildSPARQLOrderLimitOffset only uses the query arg
const emptyMetadata: any = { properties: {}, relations: {} };

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
});
