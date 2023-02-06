import { Field, ObjectType } from "type-graphql";
import { Neighbourhood } from "../neighbourhood/Neighbourhood";

export enum PerspectiveState {
    Private,
    NeighbourhoodJoinInitiated,
    LinkLanguageFailedToInstall,
    LinkLanguageInstalledButNotSynced,
    Synced,
}

// This type is used in the GraphQL interface to reference a mutable
// prespective that is implemented locally by the Ad4m runtime.
// The UUID is used in mutations to identify the perspective that gets mutated.
@ObjectType()
export class PerspectiveHandle {
    @Field()
    uuid: string
    @Field()
    name: string
    @Field()
    state: PerspectiveState

    @Field(type => String, {nullable: true})
    sharedUrl?: string

    @Field(type => Neighbourhood, {nullable: true})
    neighbourhood?: Neighbourhood

    constructor(uuid?: string, name?: string, state?: PerspectiveState) {
        this.uuid = uuid
        this.name = name
        if (state) {
            this.state = state
        } else {
            this.state = PerspectiveState.Private
        }
    }
}
