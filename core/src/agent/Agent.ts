import { Field, ObjectType, InputType } from "type-graphql";
import { Perspective } from "../perspectives/Perspective";
import { ExpressionGeneric } from "../expression/Expression";

/**  AD4M's representation of an Agent
* 
* AD4M Agents are build around DIDs, which are used to identify and authenticate the Agent.
* Conceptually, an Agent is regarded as something that can speak and that can listen.
*
* Agents speak by creating Expressions in AD4M Languages which are signed by the Agent's DID key,
* And they also speak (broadcast) by putting semantic statements into their public "Agent Perspective".
* They listen (can receive messages) through their "direct message Language".
* 
* These three aspects are represented by the three fields of this class.
*
* This class is used as format for the Expressions in the Agent language.
* Since AD4M treats DID URIs as addresses for the Agent Language,
* DIDs are resolved to Expressions that are objects of this class.
* Thus, this is how agents see (other) agents.
*/
@ObjectType()
export class Agent {
    /** The DID of the Agent
     * All epxressions authored by them are signed with the keys mentioned
     * in the DID document behind this DID URI.
     */
    @Field()
    did: string;

    /** The Perspective that holds the public-facing semantics/statements of the Agent 
     * Holds and shares a Perspective that links all information
     * this agent wants to offer as public-facing semantics.
     * This should be used for any kind of user profile information.
    */
    @Field(type => Perspective, {nullable: true})
    perspective?: Perspective;

    /** Address of the Language by which the Agent will receive DMs */
    @Field({nullable: true})
    directMessageLanguage?: string;

    constructor(did: string, perspective?: Perspective) {
        this.did = did
        if(perspective) {
            this.perspective = perspective
        } else {
            this.perspective = new Perspective()
        }
    }
}

@ObjectType()
export class AgentExpression extends ExpressionGeneric(Agent) {};

@ObjectType()
export class EntanglementProof {
    @Field()
    did: string;

    @Field()
    didSigningKeyId: string;

    @Field()
    deviceKeyType: string;

    @Field()
    deviceKey: string;

    @Field()
    deviceKeySignedByDid: string;

    @Field({nullable: true})
    didSignedByDeviceKey?: string;

    constructor(did: string, didSigningKeyId: string, deviceKeyType: string, deviceKey: string, deviceKeySignedByDid: string, didSignedByDeviceKey?: string) {
        this.did = did;
        this.didSigningKeyId = didSigningKeyId;
        this.deviceKeyType = deviceKeyType;
        this.deviceKey = deviceKey;
        this.deviceKeySignedByDid = deviceKeySignedByDid;
        this.didSignedByDeviceKey = didSignedByDeviceKey;
    }
}

@InputType()
export class EntanglementProofInput {
    @Field()
    did: string;

    @Field()
    didSigningKeyId: string;

    @Field()
    deviceKeyType: string;

    @Field()
    deviceKey: string;

    @Field()
    deviceKeySignedByDid: string;

    @Field()
    didSignedByDeviceKey: string;

    constructor(did: string, didSigningKeyId: string, deviceKeyType: string, deviceKey: string, deviceKeySignedByDid: string, didSignedByDeviceKey: string) {
        this.did = did;
        this.didSigningKeyId = didSigningKeyId;
        this.deviceKeyType = deviceKeyType;
        this.deviceKey = deviceKey;
        this.deviceKeySignedByDid = deviceKeySignedByDid;
        this.didSignedByDeviceKey = didSignedByDeviceKey;
    }
}