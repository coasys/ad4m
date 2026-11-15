import { ApiClient } from '../apiClient'
import type { SpaceHolding } from "../generated/api/SpaceHolding"

/** The spaces this session's person holds. */
export class SpacesClient {
    #apiClient: ApiClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
    }

    /** Every space the session may read, each `made` (its person made it) or `joined`, with
     *  the link language it syncs through, its sync state and the person's role. */
    async list(): Promise<SpaceHolding[]> {
        return this.#apiClient.call('spaces.list', {})
    }
}
