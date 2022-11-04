import { Field, ObjectType } from "type-graphql";
import { Neighbourhood } from "../neighbourhood/Neighbourhood";

// This type is used in the GraphQL interface to reference a mutable
// prespective that is implemented locally by the Ad4m runtime.
// The UUID is used in mutations to identify the perspective that gets mutated.
@ObjectType()
export class PerspectiveHandle {
    @Field()
    uuid: string
    @Field()
    name: string

    @Field(type => String, {nullable: true})
    sharedUrl?: string

    @Field(type => Neighbourhood, {nullable: true})
    neighbourhood?: Neighbourhood

    constructor(uuid?: string, name?: string) {
        this.uuid = uuid
        this.name = name
    }
}
