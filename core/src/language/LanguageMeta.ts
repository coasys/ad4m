import { Field, InputType, ObjectType } from "type-graphql";
import { ExpressionGeneric } from "../expression/Expression";

@ObjectType()
export class LanguageMeta {
    @Field()
    name: string;

    @Field()
    address: string;

    @Field({nullable: true})
    description: string;

    @Field()
    author: string;

    @Field({nullable: true})
    templated: boolean

    @Field({nullable: true})
    templateSourceLanguageAddress?: string;

    @Field({nullable: true})
    templateAppliedParams?: string;

    @Field(type => [String], {nullable: true})
    possibleTemplateParams?: string[];

    @Field({nullable: true})
    sourceCodeLink?: string;
}

@InputType()
export class LanguageMetaInput {
    @Field()
    name: string;

    @Field()
    description: string;

    @Field(type => [String], {nullable: true})
    possibleTemplateParams?: string[];

    @Field({nullable: true})
    sourceCodeLink?: string;

    constructor(name?: string, description?: string) {
        this.name = name
        this.description = description
        if(!this.description) this.description = ""
    }
}

export class LanguageMetaInternal {
    name: string;
    address: string;
    description: string;
    templateSourceLanguageAddress?: string;
    templateAppliedParams?: string;
    possibleTemplateParams?: string[];
    sourceCodeLink?: string;
}

export class LanguageExpression extends ExpressionGeneric(LanguageMetaInternal) {};

export class LanguageLanguageInput {
    bundle: string;
    meta: LanguageMetaInternal;
}