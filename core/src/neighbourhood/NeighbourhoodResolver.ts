import { Arg, Mutation, Query, Resolver, Subscription, PubSub } from "type-graphql";
import { PerspectiveHandle, PerspectiveState } from "../perspectives/PerspectiveHandle";
import { Perspective, PerspectiveExpression, PerspectiveInput, PerspectiveUnsignedInput } from "../perspectives/Perspective";
import { DID } from "../DID";
import { TEST_AGENT_DID } from "../agent/AgentResolver";
import { OnlineAgent } from "../language/Language";
import { testLink } from "../perspectives/PerspectiveResolver";
import { NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC, PERSPECTIVE_UPDATED_TOPIC } from "../PubSub";

const testPerspectiveExpression = new PerspectiveExpression()
testPerspectiveExpression.author = TEST_AGENT_DID
testPerspectiveExpression.timestamp = new Date().toISOString()
testPerspectiveExpression.data = new Perspective([testLink])
testPerspectiveExpression.proof = {
    signature: '',
    key: '',
    valid: true
}

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

    @Query(returns => [String])
    neighbourhoodOtherAgents(@Arg('perspectiveUUID') perspectiveUUID: string): DID[] {
        return ['did:test:other']
    }

    @Query()
    neighbourhoodHasTelepresenceAdapter(@Arg('perspectiveUUID') perspectiveUUID: string): boolean {
        return true
    }

    @Query(returns => [OnlineAgent])
    neighbourhoodOnlineAgents(@Arg('perspectiveUUID') perspectiveUUID: string): OnlineAgent[] {
        const onlineAgent = new OnlineAgent()
        onlineAgent.did = 'did:test:online'
        onlineAgent.status = testPerspectiveExpression
        return [onlineAgent]
    }

    @Mutation()
    neighbourhoodSetOnlineStatus(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('status') status: PerspectiveInput): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSetOnlineStatusU(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('status') status: PerspectiveUnsignedInput): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSendSignal(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('remoteAgentDid') recipient: DID, @Arg('payload') signal: PerspectiveInput): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSendSignalU(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('remoteAgentDid') recipient: DID, @Arg('payload') signal: PerspectiveUnsignedInput): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSendBroadcast(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('payload') signal: PerspectiveInput, @Arg('loopback', { nullable: true }) loopback?: boolean): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSendBroadcastU(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('payload') signal: PerspectiveUnsignedInput, @Arg('loopback', { nullable: true }) loopback?: boolean): boolean {
        return true
    }

    @Subscription({topics: NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC, nullable: true})
    neighbourhoodSignal(@Arg('perspectiveUUID') pID: string): PerspectiveExpression {
        return testPerspectiveExpression
    }
}