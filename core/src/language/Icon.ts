import { Field, ObjectType } from "type-graphql";

@ObjectType()
export class Icon {
    @Field()
    code: string

    constructor(code: string) {
        this.code = code
    }
}