import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'
import { AIClient } from './ai/AIClient'
import { ApiClient } from './apiClient'
import { Ad4mModel } from './model/Ad4mModel'

/**
 * Client for the Ad4m interface wrapping WebSocket RPC calls
 * for convenient use in user facing code.
 * 
 * Aggregates the six sub-clients:
 * AgentClient, ExpressionClient, LanguageClient,
 * NeighbourhoodClient, PerspectiveClient and RuntimeClient
 * for the respective functionality.
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

    constructor(baseUrl: string, token?: string, subscribe: boolean = true) {
        this.#baseUrl = baseUrl
        this.#token = token
        this.#apiClient = new ApiClient(baseUrl, token)
        this.#agentClient = new AgentClient(baseUrl, token, subscribe, this.#apiClient)
        this.#expressionClient = new ExpressionClient(baseUrl, token, this.#apiClient)
        this.#languageClient = new LanguageClient(baseUrl, token, this.#apiClient)
        this.#neighbourhoodClient = new NeighbourhoodClient(baseUrl, token, this.#apiClient)
        this.#aiClient = new AIClient(baseUrl, token, subscribe, this.#apiClient)
        this.#perspectiveClient = new PerspectiveClient(baseUrl, token, subscribe, this.#apiClient)
        this.#perspectiveClient.setExpressionClient(this.#expressionClient)
        this.#perspectiveClient.setNeighbourhoodClient(this.#neighbourhoodClient)
        this.#perspectiveClient.setAIClient(this.#aiClient)
        this.#runtimeClient = new RuntimeClient(baseUrl, token, subscribe, this.#apiClient)

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

    /** Start event subscriptions (agent-updated, agent-status-changed, apps-changed).
     *  Safe to call if subscriptions were deferred at construction (subscribe=false). */
    startSubscriptions(): void {
        this.#agentClient.subscribeAgentUpdated()
        this.#agentClient.subscribeAgentStatusChanged()
        this.#agentClient.subscribeAppsChanged()
    }

    /** Close all event connections and clear in-memory caches */
    close(): void {
        this.#agentClient.clearByDidCache()
        this.#apiClient.closeAll()
    }
}
