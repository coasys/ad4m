import { ApolloClient } from '@apollo/client/core'
import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'
import { AIClient } from './ai/AIClient'

/**
 * Client for the Ad4m interface wrapping GraphQL queryies
 * for convenient use in user facing code.
 * 
 * Aggregates the six sub-clients:
 * AgentClient, ExpressionClient, LanguageClient,
 * NeighbourhoodClient, PerspectiveClient and RuntimeClient
 * for the respective functionality.
 */
export class Ad4mClient {
    #apolloClient: ApolloClient<any>
    #agentClient: AgentClient
    #expressionClient: ExpressionClient
    #languageClient: LanguageClient
    #neighbourhoodClient: NeighbourhoodClient
    #perspectiveClient: PerspectiveClient
    #runtimeClient: RuntimeClient
    #aiClient: AIClient


    constructor(client: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = client
        this.#agentClient = new AgentClient(this.#apolloClient, subscribe)
        this.#expressionClient = new ExpressionClient(this.#apolloClient)
        this.#languageClient = new LanguageClient(this.#apolloClient)
        this.#neighbourhoodClient = new NeighbourhoodClient(this.#apolloClient)
        this.#aiClient = new AIClient(this.#apolloClient, subscribe)
        this.#perspectiveClient = new PerspectiveClient(this.#apolloClient, subscribe)
        this.#perspectiveClient.setExpressionClient(this.#expressionClient)
        this.#perspectiveClient.setNeighbourhoodClient(this.#neighbourhoodClient)
        this.#perspectiveClient.setAIClient(this.#aiClient)
        this.#runtimeClient = new RuntimeClient(this.#apolloClient, subscribe)
        
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