import { Field, ObjectType } from "type-graphql";
import { NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";

export enum PerspectiveState {
    Private = "PRIVATE",
    NeighboudhoodCreationInitiated = "NEIGHBOURHOOD_CREATION_INITIATED",
    NeighbourhoodJoinInitiated = "NEIGHBOURHOOD_JOIN_INITIATED",
    LinkLanguageFailedToInstall = "LINK_LANGUAGE_FAILED_TO_INSTALL",
    LinkLanguageInstalledButNotSynced = "LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED",
    Synced = "SYNCED",
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

    @Field(type => [String], {nullable: true})
    owners?: string[]

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
