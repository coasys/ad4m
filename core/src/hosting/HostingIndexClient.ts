import { ApiClient } from "../apiClient";
import type { IndexConfig } from "../generated/api/IndexConfig";
import type { IndexStatus } from "../generated/api/IndexStatus";
import type { ListingText } from "../generated/api/ListingText";
import type { OwnListing } from "../generated/api/OwnListing";

/**
 * This executor's listing in the Coasys hosting index (the operator's calls).
 *
 * The executor, not the caller, signs in to the index with its main device key and makes the
 * HTTP calls; these methods only start them. Every `index.*` call needs the operator.
 * AI models and per-operation rates in the listing come from the executor, not from the caller.
 */
export class HostingIndexClient {
    #apiClient: ApiClient;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /**
     * Sets the index this executor lists itself in (`url`, `https`) and whether it may call it.
     * Off by default: no call leaves for an index until this is `enabled`.
     */
    async configure(config: IndexConfig): Promise<IndexConfig> {
        return this.#apiClient.call("index.configure", config);
    }

    /**
     * The config, the operator identity and device key, and (index on) whether the index admits
     * the operator and the listing as the index shows it, with its state (`listed`,
     * `unreachable`, `delisted`, `withdrawn`).
     */
    async status(): Promise<IndexStatus> {
        return this.#apiClient.call("index.status", {});
    }

    /**
     * Lists this executor, or updates its listing (one per executor). The index reaches the
     * executor's `host.info` (`ad4m.host.info()`) at `url` before it lists it.
     */
    async publish(listing: ListingText): Promise<OwnListing> {
        return this.#apiClient.call("index.publish", listing);
    }

    /** Withdraws the listing: it is hidden at once. */
    async withdraw(): Promise<boolean> {
        return this.#apiClient.call("index.withdraw", {});
    }

    /**
     * `handler` gets the listing each time its state changes: after a publish or a withdraw, and
     * when the index answers another state (`unreachable`, `delisted`). Returns the unsubscribe
     * function. Needs `events.watch` on the socket, which `ApiClient.on` sends.
     */
    onListingChanged(handler: (listing: OwnListing) => void): () => void {
        return this.#apiClient.on("index-listing-changed", handler);
    }
}
