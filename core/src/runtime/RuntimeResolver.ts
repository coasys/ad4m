import 'reflect-metadata'
import { Arg, Mutation, Resolver, Query, Subscription, ObjectType, Field } from "type-graphql";
import { Perspective, PerspectiveExpression, PerspectiveInput } from "../perspectives/Perspective";
import { ExpressionProof } from "../expression/Expression";
import { LinkExpression } from "../links/Links";
import { ExceptionType } from "../Exception";
import { DIRECT_MESSAGE_RECEIVED, EXCEPTION_OCCURRED_TOPIC } from '../PubSub';

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

const testPerspectiveExpression = new PerspectiveExpression()
testPerspectiveExpression.author = 'did:ad4m:test'
testPerspectiveExpression.timestamp = Date.now().toString()
testPerspectiveExpression.proof = new ExpressionProof('', '')
testPerspectiveExpression.data = new Perspective([testLink])

@ObjectType()
export class SentMessage {
    @Field()
    recipient: string;
    @Field()
    message: PerspectiveExpression;
}

@ObjectType()
export class RuntimeInfo {
    @Field()
    ad4mExecutorVersion: string;
    @Field()
    isInitialized: Boolean;
    @Field()
    isUnlocked: Boolean;
}

@ObjectType()
export class ExceptionInfo {
    @Field()
    title: string;
    @Field()
    message: string;
    @Field()
    type: ExceptionType;
    @Field({ nullable: true })
    addon?: string;
}

/**
 * Resolver classes are used here to define the GraphQL schema 
 * (through the type-graphql annotations)
 * and are spawned in the client tests in Ad4mClient.test.ts.
 * For the latter, they return test fixtures.
 */
@Resolver()
export default class RuntimeResolver {
    @Mutation(returns => Boolean)
    runtimeQuit(): Boolean {
        return true
    }

    @Mutation(returns => Boolean)
    runtimeOpenLink(@Arg('url') url: string): Boolean {
        return true
    }

    @Query(returns => RuntimeInfo)
    runtimeInfo(): RuntimeInfo {
        return {
            ad4mExecutorVersion: "x.x.x",
            isInitialized: true,
            isUnlocked: true
        } as RuntimeInfo
    }

    @Mutation(returns => [String])
    addTrustedAgents(@Arg("agents", type => [String]) agents: string[]): string[] {
        return agents
    }

    @Mutation(returns => [String])
    deleteTrustedAgents(@Arg("agents", type => [String]) agents: string[]): string[] {
        return []
    }

    @Query(returns => [String])
    getTrustedAgents(): string[] {
        return ["agentPubKey"]
    }

    @Query(returns => [String])
    runtimeKnownLinkLanguageTemplates(): string[] {
        return ["Qm12345abcdef"]
    }

    @Mutation(returns => [String])
    runtimeAddKnownLinkLanguageTemplates(@Arg("addresses", type => [String]) addresses: string[]): string[] {
        return addresses
    }

    @Mutation(returns => [String])
    runtimeRemoveKnownLinkLanguageTemplates(@Arg("addresses", type => [String]) addresses: string[]): string[] {
        return []
    }

    @Query(returns => [String])
    runtimeFriends(): string[] {
        return ["did:test:friend"]
    }

    @Mutation(returns => [String])
    runtimeAddFriends(@Arg("dids", type => [String]) dids: string[]): string[] {
        return dids
    }

    @Mutation(returns => [String])
    runtimeRemoveFriends(@Arg("dids", type => [String]) dids: string[]): string[] {
        return []
    }

    @Query()
    runtimeHcAgentInfos(): String {
        return JSON.stringify([{"agent":{"type":"Buffer","data":[9,191,231,58,255,107,202,55,206,57,9,103,17,34,206,195,207,114,5,73,77,198,56,136,17,227,242,231,194,135,128,48,170,189,119,186]},"signature":{"type":"Buffer","data":[195,143,170,36,234,123,255,85,188,138,124,2,91,18,53,231,44,41,240,8,80,131,100,150,165,125,146,90,17,200,190,129,114,211,11,146,150,128,198,199,79,118,157,101,194,68,58,245,98,182,79,139,140,41,143,129,8,136,87,77,180,231,218,11]},"agent_info":{"type":"Buffer","data":[134,165,115,112,97,99,101,196,36,203,165,212,173,24,215,165,196,25,145,248,113,246,139,205,182,241,138,57,0,26,8,217,179,23,126,59,136,128,213,110,225,173,72,197,95,165,97,103,101,110,116,196,36,9,191,231,58,255,107,202,55,206,57,9,103,17,34,206,195,207,114,5,73,77,198,56,136,17,227,242,231,194,135,128,48,170,189,119,186,164,117,114,108,115,145,217,99,107,105,116,115,117,110,101,45,112,114,111,120,121,58,47,47,101,83,52,86,112,54,109,118,80,48,122,116,85,101,104,114,117,113,89,52,102,66,53,101,69,98,121,48,69,66,100,89,84,50,95,67,48,69,112,77,111,76,111,47,107,105,116,115,117,110,101,45,113,117,105,99,47,104,47,49,57,50,46,49,54,56,46,49,55,56,46,54,48,47,112,47,55,55,52,52,47,45,45,172,115,105,103,110,101,100,95,97,116,95,109,115,207,0,0,1,123,233,104,189,50,176,101,120,112,105,114,101,115,95,97,102,116,101,114,95,109,115,206,0,18,79,128,169,109,101,116,97,95,105,110,102,111,196,34,129,187,100,104,116,95,115,116,111,114,97,103,101,95,97,114,99,95,104,97,108,102,95,108,101,110,103,116,104,206,128,0,0,1]}},{"agent":{"type":"Buffer","data":[98,187,145,48,115,209,94,143,31,153,102,69,138,29,133,213,34,52,39,164,157,139,178,111,23,33,118,250,28,155,78,246,128,49,179,38]},"signature":{"type":"Buffer","data":[233,192,38,3,59,248,124,231,57,255,40,154,50,60,119,252,68,198,154,109,175,155,106,217,211,155,109,223,249,91,221,210,17,132,72,230,11,230,247,119,72,244,145,91,75,7,67,5,130,151,44,231,52,220,28,154,212,82,58,90,203,211,236,4]},"agent_info":{"type":"Buffer","data":[134,165,115,112,97,99,101,196,36,203,165,212,173,24,215,165,196,25,145,248,113,246,139,205,182,241,138,57,0,26,8,217,179,23,126,59,136,128,213,110,225,173,72,197,95,165,97,103,101,110,116,196,36,98,187,145,48,115,209,94,143,31,153,102,69,138,29,133,213,34,52,39,164,157,139,178,111,23,33,118,250,28,155,78,246,128,49,179,38,164,117,114,108,115,145,217,99,107,105,116,115,117,110,101,45,112,114,111,120,121,58,47,47,55,101,77,106,48,73,83,72,56,56,81,56,45,89,51,74,89,70,53,76,72,57,83,54,68,119,71,73,49,88,45,115,87,53,68,104,66,87,121,71,119,78,115,47,107,105,116,115,117,110,101,45,113,117,105,99,47,104,47,49,57,50,46,49,54,56,46,49,55,56,46,54,48,47,112,47,55,55,52,52,47,45,45,172,115,105,103,110,101,100,95,97,116,95,109,115,207,0,0,1,123,233,103,45,197,176,101,120,112,105,114,101,115,95,97,102,116,101,114,95,109,115,206,0,18,79,128,169,109,101,116,97,95,105,110,102,111,196,34,129,187,100,104,116,95,115,116,111,114,97,103,101,95,97,114,99,95,104,97,108,102,95,108,101,110,103,116,104,206,128,0,0,1]}},{"agent":{"type":"Buffer","data":[181,101,115,45,214,127,198,153,159,184,30,87,67,224,208,184,203,176,130,158,236,127,153,125,243,183,188,167,154,25,118,254,101,145,210,109]},"signature":{"type":"Buffer","data":[88,189,229,122,44,171,194,156,90,79,148,49,207,224,34,199,219,88,24,243,103,127,123,41,87,171,127,92,6,216,198,171,26,226,237,217,122,78,98,146,55,255,68,240,202,83,58,140,147,185,3,66,15,216,210,22,99,197,73,234,120,17,74,1]},"agent_info":{"type":"Buffer","data":[134,165,115,112,97,99,101,196,36,203,165,212,173,24,215,165,196,25,145,248,113,246,139,205,182,241,138,57,0,26,8,217,179,23,126,59,136,128,213,110,225,173,72,197,95,165,97,103,101,110,116,196,36,181,101,115,45,214,127,198,153,159,184,30,87,67,224,208,184,203,176,130,158,236,127,153,125,243,183,188,167,154,25,118,254,101,145,210,109,164,117,114,108,115,145,217,99,107,105,116,115,117,110,101,45,112,114,111,120,121,58,47,47,114,70,71,83,100,113,104,101,68,107,70,70,56,69,102,109,45,69,116,55,119,81,101,120,83,88,55,65,112,69,51,89,86,99,45,120,102,52,104,106,77,115,77,47,107,105,116,115,117,110,101,45,113,117,105,99,47,104,47,49,57,50,46,49,54,56,46,49,55,56,46,54,48,47,112,47,55,55,52,52,47,45,45,172,115,105,103,110,101,100,95,97,116,95,109,115,207,0,0,1,123,233,113,23,220,176,101,120,112,105,114,101,115,95,97,102,116,101,114,95,109,115,206,0,18,79,128,169,109,101,116,97,95,105,110,102,111,196,34,129,187,100,104,116,95,115,116,111,114,97,103,101,95,97,114,99,95,104,97,108,102,95,108,101,110,103,116,104,206,128,0,0,1]}},{"agent":{"type":"Buffer","data":[3,171,237,107,186,245,165,47,237,235,211,49,245,62,113,53,255,252,223,226,75,118,148,187,23,53,70,174,160,184,64,63,94,210,227,56]},"signature":{"type":"Buffer","data":[129,12,55,104,239,121,138,2,86,106,136,51,219,79,170,8,195,69,81,188,225,192,247,196,54,39,164,110,75,39,240,56,245,189,154,77,72,11,97,250,202,149,242,97,128,28,47,164,236,104,136,82,212,246,44,143,132,119,255,135,112,11,20,13]},"agent_info":{"type":"Buffer","data":[134,165,115,112,97,99,101,196,36,203,165,212,173,24,215,165,196,25,145,248,113,246,139,205,182,241,138,57,0,26,8,217,179,23,126,59,136,128,213,110,225,173,72,197,95,165,97,103,101,110,116,196,36,3,171,237,107,186,245,165,47,237,235,211,49,245,62,113,53,255,252,223,226,75,118,148,187,23,53,70,174,160,184,64,63,94,210,227,56,164,117,114,108,115,145,217,97,107,105,116,115,117,110,101,45,112,114,111,120,121,58,47,47,104,121,112,115,86,121,103,117,80,84,115,53,118,73,80,65,102,97,112,90,88,113,117,84,115,79,80,100,67,81,79,79,117,103,57,51,82,103,106,95,114,85,73,47,107,105,116,115,117,110,101,45,113,117,105,99,47,104,47,49,54,53,46,50,50,46,51,50,46,49,49,47,112,47,53,55,55,57,47,45,45,172,115,105,103,110,101,100,95,97,116,95,109,115,207,0,0,1,123,233,113,188,123,176,101,120,112,105,114,101,115,95,97,102,116,101,114,95,109,115,206,0,18,79,128,169,109,101,116,97,95,105,110,102,111,196,34,129,187,100,104,116,95,115,116,111,114,97,103,101,95,97,114,99,95,104,97,108,102,95,108,101,110,103,116,104,206,128,0,0,1]}}])
    }

    @Mutation(returns => Boolean)
    runtimeHcAddAgentInfos(@Arg("agentInfos", type => String) agentInfos): boolean {
        return true
    }

    @Query(returns => Boolean)
    runtimeVerifyStringSignedByDid(
        @Arg("did", type => String) did: string, 
        @Arg("didSigningKeyId", type => String) didSigningKeyId: string, 
        @Arg("data", type => String) data: string, 
        @Arg("signedData", type => String) signedData: string) {
        return true
    }
    
    @Mutation()
    runtimeSetStatus(@Arg("status", type => PerspectiveInput) status: Perspective): boolean {
        return true
    }

    @Query({nullable: true})
    runtimeFriendStatus(@Arg("did", type => String) did: string): PerspectiveExpression {
        return testPerspectiveExpression
    }

    @Mutation()
    runtimeFriendSendMessage(
        @Arg("did", type => String) did: string, 
        @Arg("message", type => PerspectiveInput) message: PerspectiveInput
    ): boolean {
        return true
    }

    @Query(returns => [PerspectiveExpression])
    runtimeMessageInbox(@Arg("filter", type => String, {nullable: true }) filter?: string): PerspectiveExpression[] {
        return [testPerspectiveExpression]
    }

    @Query(returns => [SentMessage])
    runtimeMessageOutbox(@Arg("filter", type => String, {nullable: true }) filter?: string): SentMessage[] {
        return [{
            recipient: "did:test:recipient", 
            message: testPerspectiveExpression
        } as SentMessage]
    }

 
    @Subscription({topics: DIRECT_MESSAGE_RECEIVED, nullable: true})
    runtimeMessageReceived(): PerspectiveExpression {
        return testPerspectiveExpression
    }

    @Subscription({topics: EXCEPTION_OCCURRED_TOPIC, nullable: true})
    exceptionOccurred(): ExceptionInfo {
        return {
            title: "Test title",
            message: "Test message",
            type: ExceptionType.LanguageIsNotLoaded,
        }
    }
}