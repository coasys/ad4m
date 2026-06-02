import { ApiClient } from "../apiClient";
import { InteractionCall, InteractionMeta } from "../language/Language";
import { ExpressionRendered } from "./Expression";
import { Literal } from "../Literal";
import { PersistentCache, createPersistentCache } from "../cache/PersistentCache";
import type { CreateExpressionRequest, ExpressionManyRequest } from "../generated/api";

interface CachedExpression {
    data: ExpressionRendered;
    size: number;
}

export class ExpressionClient {
    #apiClient: ApiClient

    // L1: in-memory LRU cache (hot path, sub-ms)
    #memCache = new Map<string, CachedExpression>();
    #inflight = new Map<string, Promise<ExpressionRendered>>();
    /** Maximum number of entries in the L1 memory cache. */
    static MAX_MEM_ENTRIES = 200;
    /** Maximum total size (in bytes) of L1 memory cache values. */
    static MAX_MEM_BYTES = 5 * 1024 * 1024; // 5 MB
    #totalMemBytes = 0;

    // L2: persistent cache (IndexedDB in browser, NullCache in Node)
    #persistent: PersistentCache<ExpressionRendered>;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
        this.#persistent = createPersistentCache<ExpressionRendered>('ad4m-expression-cache', 'expressions');
    }

    async get(url: string, alwaysGet: boolean = false): Promise<ExpressionRendered> {
        // Existing literal fast-path
        if(!alwaysGet){
            try {
                let literalValue = Literal.fromUrl(url).get();
                if (typeof literalValue === 'object' && literalValue !== null) {
                    if ('author' in literalValue && 'timestamp' in literalValue && 'data' in literalValue && 'proof' in literalValue) {
                        return literalValue;
                    }
                }
            } catch(e) {}
        }

        // L1: memory hit
        if (!alwaysGet) {
            const memHit = this.#memCache.get(url);
            if (memHit) return memHit.data;
        }

        // Deduplicate in-flight requests
        if (!alwaysGet) {
            const existing = this.#inflight.get(url);
            if (existing) return existing;
        }

        // L2 check + L3 network
        const promise = (async () => {
            if (!alwaysGet) {
                const persisted = await this.#persistent.get(url);
                if (persisted) {
                    this.#memCacheResult(url, persisted); // promote to L1
                    return persisted;
                }
            }

            // L3: network fetch
            const result = await this.#apiClient.call<ExpressionRendered>('expression.get', { url });
            this.#memCacheResult(url, result);
            this.#persistent.put(url, result); // fire-and-forget write to L2
            return result;
        })();

        this.#inflight.set(url, promise);

        try {
            return await promise;
        } finally {
            this.#inflight.delete(url);
        }
    }

    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        // Check L1 first
        const results: (ExpressionRendered | null)[] = urls.map(
            u => this.#memCache.get(u)?.data ?? null
        );
        let uncachedIndices = results
            .map((r, i) => r === null ? i : -1)
            .filter(i => i >= 0);

        if (uncachedIndices.length === 0) return results as ExpressionRendered[];

        // Check L2 for remaining misses
        const l2Results = await Promise.all(
            uncachedIndices.map(i => this.#persistent.get(urls[i]))
        );
        const stillMissing: number[] = [];
        for (let j = 0; j < uncachedIndices.length; j++) {
            const idx = uncachedIndices[j];
            if (l2Results[j]) {
                results[idx] = l2Results[j]!;
                this.#memCacheResult(urls[idx], l2Results[j]!); // promote to L1
            } else {
                stillMissing.push(idx);
            }
        }

        if (stillMissing.length === 0) return results as ExpressionRendered[];

        // L3: fetch only truly uncached URLs
        const uncachedUrls = stillMissing.map(i => urls[i]);
        const fetched = await this.#apiClient.call<ExpressionRendered[]>(
            'expression.getMany', { urls: uncachedUrls }
        );

        for (let i = 0; i < stillMissing.length; i++) {
            const idx = stillMissing[i];
            results[idx] = fetched[i];
            if (fetched[i]) {
                this.#memCacheResult(uncachedUrls[i], fetched[i]);
                this.#persistent.put(uncachedUrls[i], fetched[i]); // fire-and-forget
            }
        }

        return results as ExpressionRendered[];
    }

    /**
     * Insert a result into L1 memory cache with LRU eviction.
     */
    #memCacheResult(url: string, data: ExpressionRendered): void {
        const size = JSON.stringify(data).length;

        // Evict oldest entries if we'd exceed limits
        while (
            this.#memCache.size >= ExpressionClient.MAX_MEM_ENTRIES ||
            this.#totalMemBytes + size > ExpressionClient.MAX_MEM_BYTES
        ) {
            const oldest = this.#memCache.keys().next().value;
            if (!oldest) break;
            const entry = this.#memCache.get(oldest)!;
            this.#totalMemBytes -= entry.size;
            this.#memCache.delete(oldest);
        }

        this.#memCache.set(url, { data, size });
        this.#totalMemBytes += size;
    }

    /**
     * Clear both L1 (memory) and L2 (persistent) expression caches.
     */
    clearCache(): void {
        this.#memCache.clear();
        this.#inflight.clear();
        this.#totalMemBytes = 0;
        this.#persistent.clear(); // fire-and-forget
    }

    async getRaw(url: string): Promise<string> {
        return this.#apiClient.call<string>('expression.get', { url, raw: true })
    }

    async create(content: unknown, languageAddress: string): Promise<string> {
        const serialized = JSON.stringify(content)
        return this.#apiClient.call<string>('expression.create', { content: serialized, languageAddress })
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        return this.#apiClient.call<InteractionMeta[]>('expression.interactions', { url })
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        const result = await this.#apiClient.call<string|null>('expression.interact', { url, interactionCall });
        // Interactions can mutate the expression — invalidate cache so next get() fetches fresh
        this.#memCache.delete(url);
        this.#inflight.delete(url);
        this.#persistent.delete(url); // fire-and-forget
        return result;
    }
}
