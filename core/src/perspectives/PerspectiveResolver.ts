import { Arg, Mutation, PubSub, Query, Resolver, Root, Subscription, ObjectType, Field } from "type-graphql";
import { LinkExpression, LinkExpressionInput, LinkExpressionMutations, LinkExpressionUpdated, LinkInput, LinkMutations } from "../links/Links";
import { Neighbourhood, NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { LinkStatus } from "./PerspectiveProxy";
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle";
import { LINK_ADDED_TOPIC, LINK_REMOVED_TOPIC, LINK_UDATED_TOPIC, PERSPECTIVE_ADDED_TOPIC, PERSPECTIVE_REMOVED_TOPIC, PERSPECTIVE_UPDATED_TOPIC, PERSPECTIVE_SYNC_STATE_CHANGE } from '../PubSub'

export const testLink = new LinkExpression()
testLink.author = "did:ad4m:test"
testLink.timestamp = Date.now()
testLink.data = {
    source: 'root',
    target: 'neighbourhood://Qm12345'
}
testLink.proof = {
    signature: '',
    key: '',
    valid: true
}

export const PERSPECTIVE_QUERY_SUBSCRIPTION = "PERSPECTIVE_QUERY_SUBSCRIPTION"

@ObjectType()
export class QuerySubscription {
    @Field()
    subscriptionId: string;

    @Field()
    result: string;
}

/**
 * Resolver classes are used here to define the GraphQL schema
 * (through the type-graphql annotations)
 * and are spawned in the client tests in Ad4mClient.test.ts.
 * For the latter, they return test fixtures.
 */
@Resolver()
export default class PerspectiveResolver {
    @Query(returns => [PerspectiveHandle])
    perspectives(): PerspectiveHandle[] {
        const p1 = new PerspectiveHandle()
        p1.name = 'test-perspective-1'
        p1.uuid = '00001'
        const p2 = new PerspectiveHandle()
        p2.name = 'test-perspective-2'
        p2.uuid = '00002'
        p2.sharedUrl = 'neighbourhood://Qm12345'
        const neighbourhood = new NeighbourhoodExpression();
        neighbourhood.data = new Neighbourhood("language://Qm12345", new Perspective());
        neighbourhood.author = "did:ad4m:test"
        neighbourhood.timestamp = Date.now()
        neighbourhood.proof = {
            signature: '',
            key: '',
            valid: true
        }
        p2.neighbourhood = neighbourhood
        p2.state = PerspectiveState.Synced
        return [p1, p2]
    }

    @Query(returns => PerspectiveHandle, {nullable: true})
    perspective(@Arg('uuid') uuid: string): PerspectiveHandle|null {
        return new PerspectiveHandle(uuid, 'test-perspective-1')
    }

    @Query(returns => Perspective, {nullable: true})
    perspectiveSnapshot(@Arg('uuid') uuid: string): Perspective|null {
        return new Perspective([testLink])
    }

    @Mutation(returns => String, {nullable: true})
    perspectivePublishSnapshot(@Arg('uuid') uuid: string): String|null {
        return 'perspective://Qm12345'
    }

    @Query(returns => [LinkExpression], {nullable: true})
    perspectiveQueryLinks(@Arg('uuid') uuid: string, @Arg('query') query: LinkQuery): LinkExpression[] {
        return [testLink]
    }

    @Query(returns => String)
    async perspectiveQueryProlog(
        @Arg('uuid') uuid: string, 
        @Arg('query') query: string
    ): Promise<string> {
        return `[{"X": 1}]`
    }

    @Query(returns => String)
    async perspectiveQuerySurrealDB(
        @Arg('uuid') uuid: string, 
        @Arg('query') query: string
    ): Promise<string> {
        return `[]`
    }

    @Mutation(returns => QuerySubscription)
    async perspectiveSubscribeQuery(
        @Arg('uuid') uuid: string,
        @Arg('query') query: string
    ): Promise<QuerySubscription> {
        const result = `[{"X": 1}]`
        return {
            subscriptionId: "test-subscription-id",
            result: result
        }
    }

    @Mutation(returns => PerspectiveHandle)
    perspectiveAdd(@Arg('name') name: string, @PubSub() pubSub: any): PerspectiveHandle {
        const perspective = new PerspectiveHandle('00006', name);
        pubSub.publish(PERSPECTIVE_ADDED_TOPIC, { perspective })
        return new PerspectiveHandle('00006', name)
    }

    @Mutation(returns => PerspectiveHandle, {nullable: true})
    perspectiveUpdate(@Arg('uuid') uuid: string, @Arg('name') name: string, @PubSub() pubSub: any): PerspectiveHandle {
        const perspective = new PerspectiveHandle(uuid, name);
        pubSub.publish(PERSPECTIVE_UPDATED_TOPIC, { perspective })
        return new PerspectiveHandle(uuid, name)
    }

    @Mutation(returns => Boolean)
    perspectiveRemove(@Arg('uuid') uuid: string, @PubSub() pubSub: any): boolean {
        const perspective = new PerspectiveHandle(uuid);
        pubSub.publish(PERSPECTIVE_REMOVED_TOPIC, { perspective })
        return true
    }

    @Mutation(returns => String)
    async perspectiveCreateBatch(@Arg('uuid') uuid: string): Promise<string> {
        const perspective = new PerspectiveHandle(uuid)
        return uuid + '-batch-' + Date.now()
    }

    @Mutation(returns => LinkExpressionMutations)
    async perspectiveCommitBatch(
        @Arg('uuid') uuid: string,
        @Arg('batchId') batchId: string
    ): Promise<LinkExpressionMutations> {
        return new LinkExpressionMutations([], [])
    }

    @Mutation(returns => LinkExpression)
    perspectiveAddLink(
        @Arg('uuid') uuid: string, 
        @Arg('link') link: LinkInput, 
        @Arg('status', { nullable: true, defaultValue: 'shared' }) status: LinkStatus,
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): LinkExpression {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = link
        l.status = status

        pubSub.publish(LINK_ADDED_TOPIC, { link: l })
        pubSub.publish(PERSPECTIVE_SYNC_STATE_CHANGE, PerspectiveState.LinkLanguageInstalledButNotSynced)
        return l
    }

    @Mutation(returns => [LinkExpression])
    perspectiveAddLinks(
        @Arg('uuid') uuid: string, 
        @Arg('links', type => [LinkInput]) links: LinkInput[], 
        @Arg('status', { nullable: true, defaultValue: 'shared' }) status: LinkStatus,
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): LinkExpression[] {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = links[0]
        l.status = status

        const l2 = new LinkExpression()
        l2.author = 'did:ad4m:test'
        l2.timestamp = Date.now()
        l2.proof = testLink.proof
        l2.data = links[0]
        l2.status = status

        pubSub.publish(LINK_ADDED_TOPIC, { link: l })
        pubSub.publish(LINK_ADDED_TOPIC, { link: l2 })
        return [l, l2]
    }

    @Mutation(returns => Boolean)
    perspectiveRemoveLink(
        @Arg('uuid') uuid: string, 
        @Arg('link') link: LinkExpressionInput,
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): Boolean {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = link.data
        pubSub.publish(LINK_REMOVED_TOPIC, { link: l })
        return true
    }

    @Mutation(returns => [LinkExpression])
    perspectiveRemoveLinks(
        @Arg('uuid') uuid: string, 
        @Arg('links', type => [LinkExpressionInput]) links: LinkExpressionInput[],
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): LinkExpression[] {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = links[0].data

        const l2 = new LinkExpression()
        l2.author = 'did:ad4m:test'
        l2.timestamp = Date.now()
        l2.proof = testLink.proof
        l2.data = links[0].data

        pubSub.publish(LINK_REMOVED_TOPIC, { link: l })
        pubSub.publish(LINK_ADDED_TOPIC, { link: l2 })
        return [l, l2]
    }

    @Mutation(returns => LinkExpressionMutations)
    perspectiveLinkMutations(@Arg('uuid') uuid: string, @Arg('mutations') mutations: LinkMutations, @Arg('status', { nullable: true}) status: LinkStatus, @PubSub() pubSub: any): LinkExpressionMutations {
        const perspectiveAddLinks = this.perspectiveAddLinks(uuid, mutations.additions, status, null, pubSub);
        const perspectiveRemoveLinks = this.perspectiveRemoveLinks(uuid, mutations.removals, null, pubSub);
        return new LinkExpressionMutations(perspectiveAddLinks, perspectiveRemoveLinks)
    }

    @Mutation(returns => LinkExpression)
    perspectiveUpdateLink(
        @Arg('uuid') uuid: string,
        @Arg('oldLink') oldLink: LinkExpressionInput,
        @Arg('newLink') newLink: LinkInput,
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): LinkExpression {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = newLink

        pubSub.publish(LINK_REMOVED_TOPIC, { link: l })
        return l
    }

    @Mutation(returns => LinkExpression)
    perspectiveAddLinkExpression(
        @Arg('uuid') uuid: string,
        @Arg('link') link: LinkExpressionInput,
        @Arg('status', { nullable: true, defaultValue: 'shared' }) status: LinkStatus,
        @Arg('batchId', { nullable: true }) batchId: string,
        @PubSub() pubSub: any
    ): LinkExpression {
        pubSub.publish(LINK_ADDED_TOPIC, { link })
        link.status = status;
        return link as LinkExpression
    }

    @Mutation(returns => Boolean)
    perspectiveAddSdna(@Arg('uuid') uuid: string, @Arg('name') name: string, @Arg('sdnaCode') sdnaCode: string, @Arg('sdnaType') sdnaType: string, @PubSub() pubSub: any): Boolean {
        return true
    }

    @Mutation(returns => Boolean)
    perspectiveExecuteCommands(
        @Arg('uuid') uuid: string,
        @Arg('commands') commands: string,
        @Arg('expression') expression: string,
        @Arg('parameters', type => String, {nullable: true}) parameters: string,
        @Arg('batchId', { nullable: true }) batchId?: string,
    ): Boolean {
        return true
    }

    @Mutation(returns => Boolean)
    perspectiveCreateSubject(
        @Arg('uuid') uuid: string,
        @Arg('subjectClass') SubjectClass: string,
        @Arg('expressionAddress') expressionAddress: string,
        @Arg('initialValues', { nullable: true }) initialValues?: string,
        @Arg('batchId', { nullable: true }) batchId?: string,
    ): Boolean {
        return true
    }

    @Mutation(returns => String)
    perspectiveGetSubjectData(
        @Arg('uuid') uuid: string,
        @Arg('subjectClass') SubjectClass: string,
        @Arg('expressionAddress') expressionAddress: string
    ): String {
        return ""
    }

    @Subscription({topics: PERSPECTIVE_ADDED_TOPIC, nullable: true})
    perspectiveAdded(): PerspectiveHandle {
        const perspective = new PerspectiveHandle('00001', 'New Perspective');
        return perspective
    }

    @Subscription({topics: PERSPECTIVE_UPDATED_TOPIC, nullable: true})
    perspectiveUpdated(): PerspectiveHandle {
        return new PerspectiveHandle('00001', 'New Perspective')
    }

    @Subscription({topics: PERSPECTIVE_REMOVED_TOPIC, nullable: true})
    perspectiveRemoved(): string {
        return '00006'
    }

    @Subscription({topics: LINK_ADDED_TOPIC, nullable: true})
    perspectiveLinkAdded(@Arg('uuid') uuid: string): LinkExpression {
        return testLink
    }

    @Subscription({topics: LINK_REMOVED_TOPIC, nullable: true})
    perspectiveLinkRemoved(@Arg('uuid') uuid: string): LinkExpression {
        return testLink
    }

    @Subscription({topics: LINK_UDATED_TOPIC, nullable: true})
    perspectiveLinkUpdated(@Arg('uuid') uuid: string): LinkExpressionUpdated {
        return {oldLink: testLink, newLink: testLink}
    }

    @Subscription({topics: PERSPECTIVE_SYNC_STATE_CHANGE, nullable: false})
    perspectiveSyncStateChange(@Arg('uuid') uuid: string): PerspectiveState {
        return PerspectiveState.Synced
    }

    @Mutation(returns => Boolean)
    perspectiveKeepAliveQuery(
        @Arg('uuid') uuid: string,
        @Arg('subscriptionId') subscriptionId: string
    ): boolean {
        return true
    }

    @Subscription({
        topics: PERSPECTIVE_QUERY_SUBSCRIPTION,
        filter: ({ payload, args }) => 
            payload.subscriptionId === args.subscriptionId
    })
    perspectiveQuerySubscription(
        @Arg('subscriptionId') subscriptionId: string,
        @Root() payload: { subscriptionId: string, uuid: string, result: string }
    ): string {
        return payload.result
    }

    @Mutation(returns => Boolean)
    async perspectiveDisposeQuerySubscription(
        @Arg('uuid') uuid: string,
        @Arg('subscriptionId') subscriptionId: string
    ): Promise<boolean> {
        return true
    }
}