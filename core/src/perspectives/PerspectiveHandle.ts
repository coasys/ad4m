import { Field, ObjectType } from "type-graphql";
import { NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";

export enum PerspectiveState {
    Private = "Private",
    NeighboudhoodCreationInitiated = "NeighboudhoodCreationInitiated",
    NeighbourhoodJoinInitiated = "NeighbourhoodJoinInitiated",
    LinkLanguageFailedToInstall = "LinkLanguageFailedToInstall",
    LinkLanguageInstalledButNotSynced = "LinkLanguageInstalledButNotSynced",
    Synced = "Synced",
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

    @Field(type => NeighbourhoodExpression, {nullable: true})
    neighbourhood?: NeighbourhoodExpression

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
