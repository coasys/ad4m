/**
 * Query Result TTL Cache
 * 
 * Short-lived cache (200ms TTL) for SPARQL query results.
 * Prevents redundant queries during rapid render cycles where
 * multiple components issue the same query within milliseconds.
 * 
 * Key: `perspectiveUUID + queryText`
 */

interface CacheEntry {
    result: any;
    expiry: number;
}

const cache = new Map<string, CacheEntry>();

const DEFAULT_TTL_MS = 200;

function makeKey(perspectiveUUID: string, queryText: string): string {
    return `${perspectiveUUID}:${queryText}`;
}

/**
 * Get a cached query result if it exists and hasn't expired.
 */
export function getCachedResult(perspectiveUUID: string, queryText: string): any | undefined {
    const key = makeKey(perspectiveUUID, queryText);
    const entry = cache.get(key);
    if (!entry) return undefined;
    if (Date.now() > entry.expiry) {
        cache.delete(key);
        return undefined;
    }
    return entry.result;
}

/**
 * Store a query result in the cache with TTL.
 */
export function setCachedResult(perspectiveUUID: string, queryText: string, result: any, ttlMs: number = DEFAULT_TTL_MS): void {
    const key = makeKey(perspectiveUUID, queryText);
    cache.set(key, { result, expiry: Date.now() + ttlMs });
}

/**
 * Clear all cache entries (useful for testing).
 */
export function clearQueryCache(): void {
    cache.clear();
}

/**
 * Get the current size of the cache (useful for testing).
 */
export function queryCacheSize(): number {
    return cache.size;
}
