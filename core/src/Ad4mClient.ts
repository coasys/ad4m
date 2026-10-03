import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'
import { AIClient } from './ai/AIClient'
import { ApiClient, EventFilter } from './apiClient'
import type { ClientEventMap, ClientEventName } from './apiClient'
import { Ad4mModel } from './model/Ad4mModel'
import { ServicesClient } from './services/ServicesClient'
import type { ServiceClient, ServiceClientOptions, ServiceDefinition, ServiceEventTable, ServiceMethodTable } from './services/ServiceClient'

/**
 * Client for the Ad4m interface wrapping WebSocket RPC calls
 * for convenient use in user facing code.
 * 
 * Aggregates the six sub-clients:
 * AgentClient, ExpressionClient, LanguageClient,
 * NeighbourhoodClient, PerspectiveClient and RuntimeClient
 * for the respective functionality.
 *
 * {@link Ad4mClient.on} receives executor events from the moment of registration.
 */
export class Ad4mClient {
    #baseUrl: string
    #token?: string
    #apiClient: ApiClient
    #agentClient: AgentClient
    #expressionClient: ExpressionClient
    #languageClient: LanguageClient
    #neighbourhoodClient: NeighbourhoodClient
    #perspectiveClient: PerspectiveClient
    #runtimeClient: RuntimeClient
    #aiClient: AIClient
    #servicesClient: ServicesClient

    constructor(
        baseUrl: string,
        token?: string,
        options?: { webSocketImpl?: new (url: string) => WebSocket; fetchImpl?: typeof fetch }
    ) {
        this.#baseUrl = baseUrl
        this.#token = token
        this.#apiClient = new ApiClient(baseUrl, token, options?.webSocketImpl, options?.fetchImpl)
        this.#agentClient = new AgentClient(baseUrl, token, this.#apiClient)
        this.#expressionClient = new ExpressionClient(baseUrl, token, this.#apiClient)
        this.#languageClient = new LanguageClient(baseUrl, token, this.#apiClient)
        this.#neighbourhoodClient = new NeighbourhoodClient(baseUrl, token, this.#apiClient)
        this.#aiClient = new AIClient(baseUrl, token, this.#apiClient)
        this.#servicesClient = new ServicesClient(this.#apiClient)
        this.#perspectiveClient = new PerspectiveClient(baseUrl, token, this.#apiClient)
        this.#perspectiveClient.setExpressionClient(this.#expressionClient)
        this.#perspectiveClient.setNeighbourhoodClient(this.#neighbourhoodClient)
        this.#perspectiveClient.setAIClient(this.#aiClient)
        this.#runtimeClient = new RuntimeClient(baseUrl, token, this.#apiClient)

        // Register with AD4M DevTools if the bridge is installed (e.g. browser extension)
        try {
            const dt = (globalThis as any).__AD4M_DEVTOOLS__;
            if (dt) {
                dt._client = this;
                dt._Ad4mModel = Ad4mModel;
            }
        } catch {}
    }

    get agent(): AgentClient {
        return this.#agentClient
    }

    get expression(): ExpressionClient {
        return this.#expressionClient
    }

    get languages(): LanguageClient {
        return this.#languageClient
    }

    get neighbourhood(): NeighbourhoodClient {
        return this.#neighbourhoodClient
    }

    get perspective(): PerspectiveClient {
        return this.#perspectiveClient
    }

    get runtime(): RuntimeClient {
        return this.#runtimeClient
    }

    get ai(): AIClient {
        return this.#aiClient
    }

    /** The executor's service registry. */
    get services(): ServicesClient {
        return this.#servicesClient
    }

    /**
     * A typed client for one service interface version, e.g.
     * `ad4m.service(AiInference_1_0_0).call('prompt', { messages })`.
     */
    service<M extends ServiceMethodTable, E extends ServiceEventTable>(def: ServiceDefinition<M, E>, options?: ServiceClientOptions): ServiceClient<M, E> {
        return this.#servicesClient.use(def, options)
    }

    /**
     * Call `handler` with every `type` event, or only those about
     * `filter.perspective`. The payload is typed by the executor's event
     * table (`generated/api/Events.ts`). Returns a function that removes the
     * handler.
     */
    on<K extends ClientEventName>(type: K, handler: (event: ClientEventMap[K]) => void, filter?: EventFilter): () => void {
        return this.#apiClient.on(type, handler, filter)
    }

    /** Close all event connections and clear in-memory caches */
    close(): void {
        this.#agentClient.clearByDidCache()
        this.#apiClient.closeAll()
    }
}
