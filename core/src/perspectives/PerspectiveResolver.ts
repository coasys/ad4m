import { Arg, Mutation, PubSub, PubSubEngine, Query, Resolver, Subscription } from "type-graphql";
import { LinkExpression, LinkExpressionInput, LinkInput } from "../links/Links";
import { Neighbourhood } from "../neighbourhood/Neighbourhood";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { PerspectiveHandle } from "./PerspectiveHandle";
import { LINK_ADDED_TOPIC, LINK_REMOVED_TOPIC, PERSPECTIVE_ADDED_TOPIC, PERSPECTIVE_REMOVED_TOPIC, PERSPECTIVE_UPDATED_TOPIC } from '../PubSub'

const testLink = new LinkExpression()
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
        p2.neighbourhood = new Neighbourhood("language://Qm12345", new Perspective())
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
    perspectiveQueryProlog(@Arg('uuid') uuid: string, @Arg('query') query: String): string {
        return `[{"X": 1}]`
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

    @Mutation(returns => LinkExpression)
    perspectiveAddLink(@Arg('uuid') uuid: string, @Arg('link') link: LinkInput, @PubSub() pubSub: any): LinkExpression {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = link

        pubSub.publish(LINK_ADDED_TOPIC, { link: l })
        return l
    }

    @Mutation(returns => LinkExpression)
    perspectiveAddLinkExpression(@Arg('uuid') uuid: string, @Arg('link') link: LinkExpressionInput, @PubSub() pubSub: any): LinkExpression {
        pubSub.publish(LINK_ADDED_TOPIC, { link })
        return link as LinkExpression
    }
 
    @Mutation(returns => LinkExpression)
    perspectiveUpdateLink(@Arg('uuid') uuid: string, @Arg('oldLink') oldlink: LinkExpressionInput, @Arg('newLink') newlink: LinkInput, @PubSub() pubSub: any): LinkExpression {
        const l = new LinkExpression()
        l.author = 'did:ad4m:test'
        l.timestamp = Date.now()
        l.proof = testLink.proof
        l.data = newlink

        pubSub.publish(LINK_REMOVED_TOPIC, { link: l })

        return l    
    }

    @Mutation(returns => Boolean)
    perspectiveRemoveLink(@Arg('uuid') uuid: string, @Arg('link') link: LinkExpressionInput, @PubSub() pubSub: any): Boolean {
        pubSub.publish(LINK_REMOVED_TOPIC)
        return true
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
}