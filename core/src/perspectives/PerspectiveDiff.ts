import { Field, ObjectType } from "type-graphql";
import { ExpressionGeneric } from "../expression/Expression";
import { LinkExpression } from "../links/Links";

@ObjectType()
export class PerspectiveDiff {
    @Field(type => [LinkExpression])
    additions: LinkExpression[]
    
    @Field(type => [LinkExpression])
    removals: LinkExpression[]
}

@ObjectType()
export class PerspectiveDiffExpression extends ExpressionGeneric(PerspectiveDiff) {};
