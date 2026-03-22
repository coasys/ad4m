import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'
import { AIClient } from './ai/AIClient'

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
        this.#agentClient = new AgentClient(baseUrl, token, subscribe)
        this.#expressionClient = new ExpressionClient(baseUrl, token)
        this.#languageClient = new LanguageClient(baseUrl, token)
        this.#neighbourhoodClient = new NeighbourhoodClient(baseUrl, token)
        this.#aiClient = new AIClient(baseUrl, token, subscribe)
        this.#perspectiveClient = new PerspectiveClient(baseUrl, token, subscribe)
        this.#perspectiveClient.setExpressionClient(this.#expressionClient)
        this.#perspectiveClient.setNeighbourhoodClient(this.#neighbourhoodClient)
        this.#perspectiveClient.setAIClient(this.#aiClient)
        this.#runtimeClient = new RuntimeClient(baseUrl, token, subscribe)
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
}
