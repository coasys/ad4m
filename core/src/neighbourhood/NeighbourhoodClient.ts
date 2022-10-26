import { ApolloClient, ApolloQueryResult, gql } from "@apollo/client/core"
import { Address } from "../Address"
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
}