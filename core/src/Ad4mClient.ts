import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'
import { AIClient } from './ai/AIClient'
import { RestClient } from './restClient'
import { initDevToolsBridge } from './devtools/bridge';

/**
 * Client for the Ad4m interface wrapping REST API calls
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
    #restClient: RestClient
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
        this.#restClient = new RestClient(baseUrl, token)
        this.#agentClient = new AgentClient(baseUrl, token, subscribe, this.#restClient)
        this.#expressionClient = new ExpressionClient(baseUrl, token, this.#restClient)
        this.#languageClient = new LanguageClient(baseUrl, token, this.#restClient)
        this.#neighbourhoodClient = new NeighbourhoodClient(baseUrl, token, this.#restClient)
        this.#aiClient = new AIClient(baseUrl, token, subscribe, this.#restClient)
        this.#perspectiveClient = new PerspectiveClient(baseUrl, token, subscribe, this.#restClient)
        this.#perspectiveClient.setExpressionClient(this.#expressionClient)
        this.#perspectiveClient.setNeighbourhoodClient(this.#neighbourhoodClient)
        this.#perspectiveClient.setAIClient(this.#aiClient)
        this.#runtimeClient = new RuntimeClient(baseUrl, token, subscribe, this.#restClient)

        // Initialize DevTools bridge in non-production browser environments
        if (typeof globalThis !== "undefined" && typeof (globalThis as any).window !== "undefined" && (
          typeof process === 'undefined' || process.env?.NODE_ENV !== 'production'
        )) {
          try {
            initDevToolsBridge(this);
          } catch {}
        }
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

    /** Close all SSE connections */
    close(): void {
        this.#restClient.closeAll()
    }
}
