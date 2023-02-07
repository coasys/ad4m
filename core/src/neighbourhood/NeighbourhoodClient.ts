import { ApolloClient, ApolloQueryResult, gql } from "@apollo/client/core"
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective } from "../perspectives/Perspective"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import unwrapApolloResult from "../unwrapApolloResult"

export class NeighbourhoodClient {
    #apolloClient: ApolloClient<any>

    constructor(client: ApolloClient<any>) {
        this.#apolloClient = client
    }

    async publishFromPerspective(
        perspectiveUUID: string, 
        linkLanguage: Address,
        meta: Perspective
    ): Promise<string> {
        const { neighbourhoodPublishFromPerspective } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation neighbourhoodPublishFromPerspective(
                $linkLanguage: String!,
                $meta: PerspectiveInput!,
                $perspectiveUUID: String!
            ) {
                neighbourhoodPublishFromPerspective(
                    linkLanguage: $linkLanguage,
                    meta: $meta,
                    perspectiveUUID: $perspectiveUUID
                )
            }`,
            variables: { perspectiveUUID, linkLanguage, meta: meta}
        }))
        return neighbourhoodPublishFromPerspective
    }

    async joinFromUrl(url: string): Promise<PerspectiveHandle> {
        const { neighbourhoodJoinFromUrl } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`    mutation neighbourhoodJoinFromUrl($url: String!) {
                neighbourhoodJoinFromUrl(url: $url) {
                    uuid
                    name
                    sharedUrl
                    neighbourhood { 
                        linkLanguage 
                        meta { 
                            links
                                {
                                    author
                                    timestamp
                                    data { source, predicate, target }
                                    proof { valid, invalid, signature, key }
                                }  
                        } 
                    }
                }
            }`,
            variables: { url }
        }))
        return neighbourhoodJoinFromUrl
    }

    async otherAgents(perspectiveUUID: string): Promise<DID[]> {
        const { neighbourhoodOtherAgents } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query neighbourhoodOtherAgents($perspectiveUUID: String!) {
                neighbourhoodOtherAgents(perspectiveUUID: $perspectiveUUID)
            }`,
            variables: { perspectiveUUID }
        }))
        return neighbourhoodOtherAgents
    }

    async hasTelepresenceAdapter(perspectiveUUID: string): Promise<boolean> {
        const { neighbourhoodHasTelepresenceAdapter } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query neighbourhoodHasTelepresenceAdapter($perspectiveUUID: String!) {
                neighbourhoodHasTelepresenceAdapter(perspectiveUUID: $perspectiveUUID)
            }`,
            variables: { perspectiveUUID }
        }))
        return neighbourhoodHasTelepresenceAdapter
    }

    async onlineAgents(perspectiveUUID: string): Promise<OnlineAgent[]> {
        const { neighbourhoodOnlineAgents } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query neighbourhoodOnlineAgents($perspectiveUUID: String!) {
                neighbourhoodOnlineAgents(perspectiveUUID: $perspectiveUUID) {
                    did
                    status {
                        author
                        timestamp
                        data { 
                            links {
                                author
                                timestamp
                                data { source, predicate, target }
                                proof { valid, invalid, signature, key }
                            }
                        }
                        proof { valid, invalid, signature, key }
                    }
                }
            }`,
            variables: { perspectiveUUID }
        }))
        return neighbourhoodOnlineAgents
    }

    async setOnlineStatus(perspectiveUUID: string, status: Perspective): Promise<void> {
        const { neighbourhoodSetOnlineStatus } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation neighbourhoodSetOnlineStatus(
                $perspectiveUUID: String!,
                $status: PerspectiveInput!
            ) {
                neighbourhoodSetOnlineStatus(
                    perspectiveUUID: $perspectiveUUID,
                    status: $status
                )
            }`,
            variables: { perspectiveUUID, status }
        }))

        return neighbourhoodSetOnlineStatus
    }

    async sendSignal(perspectiveUUID: string, remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        const { neighbourhoodSendSignal } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation neighbourhoodSendSignal(
                $perspectiveUUID: String!,
                $remoteAgentDid: String!,
                $payload: PerspectiveInput!
            ) {
                neighbourhoodSendSignal(
                    perspectiveUUID: $perspectiveUUID,
                    remoteAgentDid: $remoteAgentDid,
                    payload: $payload
                )
            }`,
            variables: { perspectiveUUID, remoteAgentDid, payload }
        }))

        return neighbourhoodSendSignal
    }

    async sendBroadcast(perspectiveUUID: string, payload: Perspective): Promise<boolean> {
        const { neighbourhoodSendBroadcast } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation neighbourhoodSendBroadcast(
                $perspectiveUUID: String!,
                $payload: PerspectiveInput!
            ) {
                neighbourhoodSendBroadcast(
                    perspectiveUUID: $perspectiveUUID,
                    payload: $payload
                )
            }`,
            variables: { perspectiveUUID, payload }
        }))

        return neighbourhoodSendBroadcast
    }

    async addSignalHandler(perspectiveUUID: string, handler: TelepresenceSignalCallback): Promise<void> {
        await this.#apolloClient.subscribe({
            query: gql`subscription neighbourhoodSignal($perspectiveUUID: String!) {
                neighbourhoodSignal(perspectiveUUID: $perspectiveUUID) {
                    author
                    timestamp
                    data {
                        links
                            {
                                author
                                timestamp
                                data { source, predicate, target }
                                proof { valid, invalid, signature, key }
                            }  
                    }
                    proof { valid, invalid, signature, key }
                }
            }`,
            variables: { perspectiveUUID }
        }).subscribe({
            next: (result: ApolloQueryResult<any>) => {
                const { neighbourhoodSignal } = unwrapApolloResult(result)
                handler(neighbourhoodSignal)
            }
        })
    }
}