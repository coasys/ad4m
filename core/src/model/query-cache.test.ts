import { getCachedResult, setCachedResult, invalidatePerspectiveCache, clearQueryCache, queryCacheSize } from './query-cache';

describe('query-cache', () => {
    beforeEach(() => {
        clearQueryCache();
    });

    describe('lazy eviction', () => {
        it('should remove expired entries when setting a new one', () => {
            // Insert with a very short TTL
            setCachedResult('p1', 'SELECT ?x', 'result1', 1);
            expect(queryCacheSize()).toBe(1);

            // Wait for expiry
            const start = Date.now();
            while (Date.now() - start < 5) { /* busy wait 5ms */ }

            // Expired entry should still be in the map (not yet evicted)
            // but getCachedResult should return undefined
            expect(getCachedResult('p1', 'SELECT ?x')).toBeUndefined();

            // Setting a new entry should evict the expired one
            setCachedResult('p2', 'SELECT ?y', 'result2', 10000);
            // Only the new entry should remain
            expect(queryCacheSize()).toBe(1);
            expect(getCachedResult('p2', 'SELECT ?y')).toBe('result2');
        });
    });

    describe('invalidatePerspectiveCache', () => {
        it('should remove all entries for a specific perspective', () => {
            setCachedResult('p1', 'query1', 'r1', 10000);
            setCachedResult('p1', 'query2', 'r2', 10000);
            setCachedResult('p2', 'query1', 'r3', 10000);
            expect(queryCacheSize()).toBe(3);

            invalidatePerspectiveCache('p1');

            expect(queryCacheSize()).toBe(1);
            expect(getCachedResult('p1', 'query1')).toBeUndefined();
            expect(getCachedResult('p1', 'query2')).toBeUndefined();
            expect(getCachedResult('p2', 'query1')).toBe('r3');
        });

        it('should not affect other perspectives', () => {
            setCachedResult('p1', 'q', 'r1', 10000);
            setCachedResult('p2', 'q', 'r2', 10000);

            invalidatePerspectiveCache('p3');

            expect(queryCacheSize()).toBe(2);
        });
    });
});
