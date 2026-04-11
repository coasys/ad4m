/**
 * Subscription Pool — deduplicates identical SPARQL subscriptions.
 *
 * When multiple UI components subscribe to the same SPARQL query on the same
 * perspective, the pool shares a single `subscribeQuery()` call and distributes
 * results to all registered callbacks. Hydration happens once per result set,
 * then is shared across all subscribers.
 *
 * Key: `perspectiveUUID + sparqlQuery`
 */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";

type HydrateCallback = (rawResult: any) => Promise<any>;
type ResultCallback = (hydratedResult: any) => void;

interface PoolEntry {
    /** The underlying subscription from perspective.subscribeQuery() */
    subscription: any;
    /** All registered callbacks */
    callbacks: Set<{ onResult: ResultCallback; hydrate: HydrateCallback }>;
    /** Latest raw result (for new subscribers joining late) */
    latestRaw: any;
    /** Latest hydrated result */
    latestHydrated: any;
    /** Whether hydration is in flight */
    hydrating: boolean;
    /** Reference count for disposal */
    refCount: number;
}

const pool = new Map<string, PoolEntry>();

function makeKey(perspectiveUUID: string, sparqlQuery: string): string {
    return `${perspectiveUUID}::${sparqlQuery}`;
}

export interface PooledSubscription {
    /** The initial (hydrated) result */
    initialResult: any;
    /** Unsubscribe this particular callback from the pool */
    dispose: () => void;
}

/**
 * Subscribe to a SPARQL query, sharing the underlying subscription
 * with any other subscribers using the same perspective + query.
 *
 * @param perspective - The perspective proxy
 * @param sparqlQuery - The SPARQL query text
 * @param hydrate     - Async function that takes raw SPARQL results and returns hydrated results
 * @param onResult    - Callback for subsequent result updates (hydrated)
 * @returns PooledSubscription with initial result and dispose function
 */
export async function pooledSubscribe(
    perspective: PerspectiveProxy,
    sparqlQuery: string,
    hydrate: HydrateCallback,
    onResult: ResultCallback,
): Promise<PooledSubscription> {
    const uuid = (perspective as any).uuid || (perspective as any).handle?.uuid || '';
    const key = makeKey(uuid, sparqlQuery);
    
    const entry = pool.get(key);
    const subscriber = { onResult, hydrate };

    if (entry) {
        // Join existing subscription
        entry.callbacks.add(subscriber);
        entry.refCount++;

        // Hydrate latest result for this new subscriber
        let initialResult: any;
        if (entry.latestHydrated !== undefined) {
            initialResult = entry.latestHydrated;
        } else if (entry.latestRaw !== undefined) {
            initialResult = await hydrate(entry.latestRaw);
        }

        return {
            initialResult,
            dispose: () => disposeSubscriber(key, subscriber),
        };
    }

    // Create new subscription
    const subscription = await perspective.subscribeQuery(sparqlQuery);
    
    const newEntry: PoolEntry = {
        subscription,
        callbacks: new Set([subscriber]),
        latestRaw: subscription.result,
        latestHydrated: undefined,
        hydrating: false,
        refCount: 1,
    };

    pool.set(key, newEntry);

    // Set up shared result handler with delta optimization
    subscription.onResult(async (rawResult: any) => {
        const prevRaw = newEntry.latestRaw;
        newEntry.latestRaw = rawResult;
        newEntry.hydrating = true;
        try {
            const firstSub = newEntry.callbacks.values().next().value;
            if (firstSub) {
                // Delta optimization: if the new result is strictly an addition
                // (all previous rows still present + new rows), only hydrate the
                // new rows and append them to the cached hydrated result.
                const prevArr = Array.isArray(prevRaw) ? prevRaw : [];
                const newArr = Array.isArray(rawResult) ? rawResult : [];
                const prevHydrated = newEntry.latestHydrated;

                if (
                    prevHydrated &&
                    Array.isArray(prevHydrated) &&
                    newArr.length > prevArr.length &&
                    newArr.length - prevArr.length <= 5 // small delta — worth optimizing
                ) {
                    // Build a set of source URIs from previous results for quick lookup
                    const prevSources = new Set(prevArr.map((r: any) => `${r.source}|${r.predicate}|${r.target}`));
                    const addedRows = newArr.filter((r: any) => !prevSources.has(`${r.source}|${r.predicate}|${r.target}`));

                    // Check that no rows were removed (pure addition)
                    const newSources = new Set(newArr.map((r: any) => `${r.source}|${r.predicate}|${r.target}`));
                    const removedRows = prevArr.filter((r: any) => !newSources.has(`${r.source}|${r.predicate}|${r.target}`));

                    if (removedRows.length === 0 && addedRows.length > 0) {
                        // Pure addition — hydrate only the new rows
                        try {
                            const deltaHydrated = await firstSub.hydrate(addedRows);
                            if (Array.isArray(deltaHydrated)) {
                                const merged = [...prevHydrated, ...deltaHydrated];
                                newEntry.latestHydrated = merged;
                                for (const cb of newEntry.callbacks) {
                                    try { cb.onResult(merged); } catch (e) { /* subscriber error */ }
                                }
                                return; // Delta path succeeded
                            }
                        } catch {
                            // Delta hydration failed — fall through to full hydration
                        }
                    }
                }

                // Full hydration fallback
                const hydrated = await firstSub.hydrate(rawResult);
                newEntry.latestHydrated = hydrated;
                // Distribute to all callbacks
                for (const cb of newEntry.callbacks) {
                    try { cb.onResult(hydrated); } catch (e) { /* subscriber error */ }
                }
            }
        } finally {
            newEntry.hydrating = false;
        }
    });

    // Hydrate initial result
    const initialResult = await hydrate(subscription.result);
    newEntry.latestHydrated = initialResult;

    return {
        initialResult,
        dispose: () => disposeSubscriber(key, subscriber),
    };
}

function disposeSubscriber(key: string, subscriber: { onResult: ResultCallback; hydrate: HydrateCallback }): void {
    const entry = pool.get(key);
    if (!entry) return;

    entry.callbacks.delete(subscriber);
    entry.refCount--;

    if (entry.refCount <= 0) {
        // Last subscriber — tear down the underlying subscription
        if (entry.subscription?.dispose) {
            entry.subscription.dispose();
        }
        pool.delete(key);
    }
}

/**
 * Clear the entire pool (for testing).
 */
export function clearSubscriptionPool(): void {
    for (const entry of pool.values()) {
        if (entry.subscription?.dispose) {
            entry.subscription.dispose();
        }
    }
    pool.clear();
}

/**
 * Get the current pool size (for testing).
 */
export function subscriptionPoolSize(): number {
    return pool.size;
}
