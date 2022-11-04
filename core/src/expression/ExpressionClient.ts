import { ApolloClient, gql } from "@apollo/client/core";
import { InteractionCall, InteractionMeta } from "../language/Language";
import unwrapApolloResult from "../unwrapApolloResult";
import { ExpressionRendered } from "./Expression";

export class ExpressionClient {
    #apolloClient: ApolloClient<any>

    constructor(client: ApolloClient<any>) {
        this.#apolloClient = client
    }

    async get(url: string): Promise<ExpressionRendered> {
        const { expression } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query expression($url: String!) {
                expression(url: $url) {
                    author
                    timestamp
                    data
                    language {
                        address
                    }
                    proof {
                        valid
                        invalid
                    }
                }
            }`,
            variables: { url }
        }))
        return expression
    }

    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        const { expressionMany } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query expressionMany($urls: [String!]!) {
                expressionMany(urls: $urls) {
                    author
                    timestamp
                    data
                    language {
                        address
                    }
                    proof {
                        valid
                        invalid
                    }
                }
            }`,
            variables: { urls }
        }))
        return expressionMany
    }

    async getRaw(url: string): Promise<string> {
        const { expressionRaw } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query expressionRaw($url: String!) {
                expressionRaw(url: $url)
            }`,
            variables: { url }
        }))
        return expressionRaw
    }

    async create(content: any, languageAddress: string): Promise<string> {
        content = JSON.stringify(content)
        const { expressionCreate } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation expressionCreate($content: String!, $languageAddress: String!){
                expressionCreate(content: $content, languageAddress: $languageAddress)
            }`,
            variables: { content, languageAddress }
        }))
        return expressionCreate
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        const { expressionInteractions } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query expressionInteractions($url: String!) {
                expressionInteractions(url: $url) { 
                    label
                    name
                    parameters { name, type }
                }
            }`,
            variables: { url }
        }))
        return expressionInteractions
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        const { expressionInteract } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation expressionInteract($url: String!, $interactionCall: InteractionCall!){
                expressionInteract(url: $url, interactionCall: $interactionCall)
            }`,
            variables: { url, interactionCall }
        }))
        return expressionInteract
    }
}