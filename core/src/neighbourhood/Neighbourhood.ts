import { Field, ObjectType } from "type-graphql";
import { ExpressionGeneric } from "../expression/Expression";
import { LanguageRef } from "../language/LanguageRef";
import { Perspective } from "../perspectives/Perspective";


@ObjectType()
export class Neighbourhood {
    @Field()
    linkLanguage: string

    @Field()
    meta: Perspective

    constructor(linkLanguage: string, meta: Perspective) {
        this.linkLanguage = linkLanguage;
        this.meta = meta;
    }
}

@ObjectType()
export class NeighbourhoodExpression extends ExpressionGeneric(Neighbourhood) {};
