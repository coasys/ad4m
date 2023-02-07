import { Arg, Mutation, Query, Resolver, Subscription } from "type-graphql";
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle";
import { Perspective, PerspectiveExpression, PerspectiveInput } from "../perspectives/Perspective";
import { DID } from "../DID";
import { TEST_AGENT_DID } from "../agent/AgentResolver";
import { OnlineAgent } from "../language/Language";
import { testLink } from "../perspectives/PerspectiveResolver";
import { NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC } from "../PubSub";

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
    neighbourhoodJoinFromUrl(@Arg('url') url: string): PerspectiveHandle {
        const perspective = new PerspectiveHandle
        perspective.name = "test-perspective"
        perspective.sharedUrl = url
        perspective.uuid = "234234234"
        return perspective
    }

    @Query(returns => [String])
    neighbourhoodOtherAgents(@Arg('perspectiveUUID') perspectiveUUID: string): DID[] {
        return ['did:test:other']
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
    neighbourhoodSendSignal(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('remoteAgentDid') recipient: DID, @Arg('payload') signal: PerspectiveInput): boolean {
        return true
    }

    @Mutation()
    neighbourhoodSendBroadcast(@Arg('perspectiveUUID') perspectiveUUID: string, @Arg('payload') signal: PerspectiveInput): boolean {
        return true
    }

    @Subscription({topics: NEIGHBOURHOOD_SIGNAL_RECEIVED_TOPIC, nullable: true})
    neighbourhoodSignal(): PerspectiveExpression {
        return testPerspectiveExpression
    }
}