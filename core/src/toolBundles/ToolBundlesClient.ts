import { ApiClient, CallOptions } from '../apiClient'
import type { InstalledBundle } from '../generated/api/InstalledBundle'

/**
 * Tool bundles: JavaScript an app installs on the executor so agents can use its tools on the
 * app's own records. The executor runs each bundle in a sandbox that can compute and nothing else,
 * and offers its tools to every agent through `list_record_tools` / `call_record_tool`.
 *
 * Installing and removing act on the whole node, like installing a Language: a user session may
 * list bundles but not change them.
 */
export class ToolBundlesClient {
    #apiClient: ApiClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
    }

    /**
     * Install a bundle from its source: one self-contained ES module exporting `toolsets`.
     * Replaces any installed bundle that exports a toolset of the same name, so installing a new
     * release is how an app updates its tools — and a release must export every toolset of the
     * bundle it replaces, or it is refused. Installing the same source again changes nothing.
     *
     * Only the node's operator may install: a managed user's session is refused, as for Languages.
     */
    async install(source: string, options?: CallOptions): Promise<InstalledBundle> {
        return this.#apiClient.call('toolBundles.install', { source }, options)
    }

    async list(): Promise<InstalledBundle[]> {
        return this.#apiClient.call('toolBundles.list', {})
    }

    /** Uninstall a bundle by its address. Answers whether it was installed. */
    async remove(address: string): Promise<boolean> {
        return this.#apiClient.call('toolBundles.remove', { address })
    }
}
