/**
 * Tests for the delta fast-path logic used in subscription-pool.ts.
 *
 * The delta optimisation in pooledSubscribe's onResult handler:
 * 1. Checks if new rows are a strict superset of previous rows
 * 2. Extracts added rows (new triples not in prev set)
 * 3. Verifies ALL added rows have source URIs not already in previous results
 * 4. If condition 3 fails (existing source got new links), falls back to full rehydration
 *
 * These tests validate the detection logic in isolation.
 */

type Row = { source: string; predicate: string; target: string };

/**
 * Replicates the delta-path eligibility check from subscription-pool.ts
 */
function detectDeltaPath(prevArr: Row[], newArr: Row[]): {
  useDelta: boolean;
  addedRows: Row[];
  reason?: string;
} {
  if (newArr.length <= prevArr.length) {
    return { useDelta: false, addedRows: [], reason: 'no growth' };
  }
  if (newArr.length - prevArr.length > 5) {
    return { useDelta: false, addedRows: [], reason: 'delta too large' };
  }

  const prevSources = new Set(prevArr.map(r => `${r.source}|${r.predicate}|${r.target}`));
  const addedRows = newArr.filter(r => !prevSources.has(`${r.source}|${r.predicate}|${r.target}`));

  const newSources = new Set(newArr.map(r => `${r.source}|${r.predicate}|${r.target}`));
  const removedRows = prevArr.filter(r => !newSources.has(`${r.source}|${r.predicate}|${r.target}`));

  if (removedRows.length > 0) {
    return { useDelta: false, addedRows, reason: 'rows removed' };
  }

  // Key check: all added rows must have NEW source URIs
  const prevSourceIds = new Set(prevArr.map(r => r.source));
  const allNewSources = addedRows.every(r => !prevSourceIds.has(r.source));

  if (!allNewSources) {
    return { useDelta: false, addedRows, reason: 'existing source got new links' };
  }

  return { useDelta: true, addedRows };
}

describe('delta fast-path detection', () => {
  const prev: Row[] = [
    { source: 'a', predicate: 'p1', target: 't1' },
    { source: 'a', predicate: 'p2', target: 't2' },
    { source: 'b', predicate: 'p1', target: 't3' },
  ];

  it('appends genuinely new items via delta path', () => {
    const next: Row[] = [
      ...prev,
      { source: 'c', predicate: 'p1', target: 't4' },
    ];
    const result = detectDeltaPath(prev, next);
    expect(result.useDelta).toBe(true);
    expect(result.addedRows).toHaveLength(1);
    expect(result.addedRows[0].source).toBe('c');
  });

  it('falls back when existing source gets new links', () => {
    const next: Row[] = [
      ...prev,
      { source: 'a', predicate: 'p3', target: 't5' }, // source 'a' already exists
    ];
    const result = detectDeltaPath(prev, next);
    expect(result.useDelta).toBe(false);
    expect(result.reason).toBe('existing source got new links');
  });

  it('falls back when rows are removed (even if new ones added)', () => {
    const next: Row[] = [
      { source: 'a', predicate: 'p1', target: 't1' },
      { source: 'a', predicate: 'p2', target: 't2' },
      // source 'b' row removed, two new sources added so length grows
      { source: 'c', predicate: 'p1', target: 't4' },
      { source: 'd', predicate: 'p1', target: 't5' },
    ];
    const result = detectDeltaPath(prev, next);
    expect(result.useDelta).toBe(false);
    expect(result.reason).toBe('rows removed');
  });

  it('falls back when delta is too large (>5 new rows)', () => {
    const next: Row[] = [
      ...prev,
      ...Array.from({ length: 6 }, (_, i) => ({
        source: `new${i}`, predicate: 'p1', target: `t${10 + i}`,
      })),
    ];
    const result = detectDeltaPath(prev, next);
    expect(result.useDelta).toBe(false);
    expect(result.reason).toBe('delta too large');
  });

  it('falls back when no growth', () => {
    const result = detectDeltaPath(prev, prev);
    expect(result.useDelta).toBe(false);
    expect(result.reason).toBe('no growth');
  });

  it('handles multiple new sources in one delta', () => {
    const next: Row[] = [
      ...prev,
      { source: 'c', predicate: 'p1', target: 't4' },
      { source: 'd', predicate: 'p1', target: 't5' },
    ];
    const result = detectDeltaPath(prev, next);
    expect(result.useDelta).toBe(true);
    expect(result.addedRows).toHaveLength(2);
  });
});
