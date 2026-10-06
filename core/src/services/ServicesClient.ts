import type { ApiClient, CallOptions } from '../apiClient'
import type { ServicesDescription } from '../generated/api/ServicesDescription'
import { ServiceClient, type ServiceClientOptions, type ServiceDefinition, type ServiceEventTable, type ServiceMethodTable } from './ServiceClient'

/**
 * The executor's service registry: what is
 * installed, the interface documents, and which implementation to prefer.
 */
export class ServicesClient {
    readonly #api: ApiClient

    constructor(api: ApiClient) {
        this.#api = api
    }

    /** A typed client for one interface version, from `ad4m service-gen` output. */
    use<M extends ServiceMethodTable, E extends ServiceEventTable>(def: ServiceDefinition<M, E>, options?: ServiceClientOptions): ServiceClient<M, E> {
        return new ServiceClient(this.#api, def, options)
    }

    /** Installed interfaces and implementations, with the caller's granted actions. */
    describe(target?: string, options?: CallOptions): Promise<ServicesDescription> {
        return this.#api.call('services.describe', target === undefined ? {} : { target }, options)
    }

    /** The interface document with this hash. */
    interface(hash: string, options?: CallOptions): Promise<unknown> {
        return this.#api.call('services.interface', { hash }, options)
    }

    /**
     * Prefer the Service Language `module` (`service://<hash>`) for `interface`'s compatible line:
     * for the caller, or as the executor default (`forAllUsers`, admin only).
     */
    setPreference(iface: string, module: string, forAllUsers?: boolean, options?: CallOptions): Promise<boolean> {
        return this.#api.call('services.setPreference', { interface: iface, module, ...(forAllUsers ? { forAllUsers } : {}) }, options)
    }
}
