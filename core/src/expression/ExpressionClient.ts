import { ApiClient, RpcError } from "../apiClient";
import { InteractionCall, InteractionMeta } from "../language/Language";
import { ExpressionRendered } from "./Expression";
import { Literal } from "../Literal";
import { createPersistentCache } from "../cache/PersistentCache";
import { KeyedCache, StoredEntry } from "../cache/KeyedCache";

export class ExpressionClient {
    #apiClient: ApiClient

    /** Maximum number of expressions kept in memory. */
    static MAX_MEM_ENTRIES = 200;
    /** Maximum total size (in bytes of JSON) of the expressions kept in memory. */
    static MAX_MEM_BYTES = 5 * 1024 * 1024; // 5 MB

    // The misses of the `get` and `getMany` calls made in one tick go out in
    // one `expression.getMany`, which answers per URL what `expression.get` would.
    #expressions: KeyedCache<ExpressionRendered>;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
        this.#expressions = new KeyedCache<ExpressionRendered>({
            loadMany: async (urls) => (await this.#apiClient.call('expression.getMany', { urls })).map(entry =>
                entry && 'code' in entry ? new RpcError(entry.code, entry.message) : entry as ExpressionRendered | null),
            persistent: createPersistentCache<StoredEntry<ExpressionRendered>>('ad4m-expression-cache', 'expressions'),
            limits: () => ({ entries: ExpressionClient.MAX_MEM_ENTRIES, bytes: ExpressionClient.MAX_MEM_BYTES }),
        });
    }

    /** The expression at `url`; `alwaysGet` skips the literal fast path and the cache. */
    async get(url: string, alwaysGet: boolean = false): Promise<ExpressionRendered> {
        if (alwaysGet) {
            const result = await this.#apiClient.call('expression.get', { url });
            if (result) this.#expressions.set(url, result);
            return result;
        }
        try {
            let literalValue = Literal.fromUrl(url).get();
            if (typeof literalValue === 'object' && literalValue !== null) {
                if ('author' in literalValue && 'timestamp' in literalValue && 'data' in literalValue && 'proof' in literalValue) {
                    return literalValue;
                }
            }
        } catch(e) {}
        return this.#expressions.get(url);
    }

    /**
     * The expressions at `urls`, in order (`null` when not found); the misses go
     * out in one call. Rejects with the `RpcError` `get` gives when a URL's read fails.
     */
    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        return this.#expressions.getMany(urls);
    }

    /**
     * Clear both the memory and the persistent expression caches.
     */
    clearCache(): void {
        this.#expressions.clear(true);
    }

    async getRaw(url: string): Promise<string> {
        return this.#apiClient.call('expression.getRaw', { url })
    }

    async create(content: unknown, languageAddress: string): Promise<string> {
        const serialized = JSON.stringify(content)
        return this.#apiClient.call('expression.create', { content: serialized, languageAddress })
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        return this.#apiClient.call('expression.interactions', { url })
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        const result = await this.#apiClient.call('expression.interact', { url, interactionCall });
        // Interactions can mutate the expression — invalidate cache so next get() fetches fresh
        this.#expressions.delete(url);
        return result;
    }
}
