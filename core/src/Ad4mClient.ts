import 'reflect-metadata'
import { ApolloClient } from '@apollo/client/core'
import { AgentClient } from './agent/AgentClient'
import { LanguageClient } from './language/LanguageClient'
import { NeighbourhoodClient } from './neighbourhood/NeighbourhoodClient'
import { PerspectiveClient } from './perspectives/PerspectiveClient'
import { RuntimeClient } from './runtime/RuntimeClient'
import { ExpressionClient } from './expression/ExpressionClient'

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


    constructor(client: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = client
        this.#agentClient = new AgentClient(client, subscribe)
        this.#expressionClient = new ExpressionClient(client)
        this.#languageClient = new LanguageClient(client)
        this.#neighbourhoodClient = new NeighbourhoodClient(client)
        this.#perspectiveClient = new PerspectiveClient(client, subscribe)
        this.#runtimeClient = new RuntimeClient(client, subscribe)
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
}