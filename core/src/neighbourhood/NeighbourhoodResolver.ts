import { Arg, Mutation, Resolver, PubSub } from "type-graphql";
import { PerspectiveHandle, PerspectiveState } from "../perspectives/PerspectiveHandle";
import { PerspectiveInput } from "../perspectives/Perspective";
import { PERSPECTIVE_UPDATED_TOPIC } from "../PubSub";

/**
 * Resolver classes are used here to define the GraphQL schema 
 * (through the type-graphql annotations)
 * and are spawned in the client tests in Ad4mClient.test.ts.
 * For the latter, they return test fixtures.
 */
@Resolver()
export default class NeighbourhoodResolver {
    @Mutation(returns => String)
    neighbourhoodPublishFromPerspective(
        @Arg('perspectiveUUID') perspectiveUUID: string, 
        @Arg('linkLanguage') linkLanguage: string,
        @Arg('meta') meta: PerspectiveInput
    ): string {
        return "neighbourhood://neighbourhoodAddress"
    }

    @Mutation(returns => PerspectiveHandle)
    neighbourhoodJoinFromUrl(@Arg('url') url: string, @PubSub() pubSub: any): PerspectiveHandle {
        const perspective = new PerspectiveHandle
        perspective.name = "test-perspective"
        perspective.sharedUrl = url
        perspective.uuid = "234234234"
        perspective.state = PerspectiveState.Synced
        pubSub.publish(PERSPECTIVE_UPDATED_TOPIC, { perspective});
        return perspective
    }
}