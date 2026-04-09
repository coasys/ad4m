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

    // Set up shared result handler
    subscription.onResult(async (rawResult: any) => {
        newEntry.latestRaw = rawResult;
        newEntry.hydrating = true;
        try {
            // Hydrate ONCE using the first subscriber's hydrate function
            const firstSub = newEntry.callbacks.values().next().value;
            if (firstSub) {
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
