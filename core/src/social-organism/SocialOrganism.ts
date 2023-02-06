import { Field, InputType, ObjectType } from "type-graphql"
import { ExpressionGeneric } from "../expression/Expression"
import { Perspective } from "../perspectives/Perspective"

@ObjectType()
export class SocialOrganism {
    @Field()
    meta: Perspective

    @Field()
    members: string[]

    @Field()
    nucleusNeighbourhood: string

    @Field()
    outputsNeighbourhood: string

    @Field()
    inputsDMLanguage: string
}

@InputType()
export class SocialOrganismInput {
    @Field(type => Perspective)
    meta: Perspective
    
    @Field()
    members: string[]

    @Field()
    nucleusNeighbourhood: string

    @Field()
    outputsNeighbourhood: string

    @Field()
    inputsDMLanguage: string
}


export class SocialOrganismExpression extends ExpressionGeneric(SocialOrganism) {}


export class SocialOrganismProof {
    expressionURL: string
    did: string
    signature: string
    key: string
}