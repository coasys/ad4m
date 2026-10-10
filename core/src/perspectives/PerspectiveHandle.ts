import { Neighbourhood, NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";
import { LinkExpression } from "../links/Links";
import { Perspective } from "./Perspective";
import type { PerspectiveHandle as WirePerspectiveHandle } from "../generated/api/PerspectiveHandle";
import type { DecoratedNeighbourhoodExpression } from "../generated/api/DecoratedNeighbourhoodExpression";

export const PerspectiveState = {
    Private: "PRIVATE",
    NeighboudhoodCreationInitiated: "NEIGHBOURHOOD_CREATION_INITIATED",
    NeighbourhoodJoinInitiated: "NEIGHBOURHOOD_JOIN_INITIATED",
    LinkLanguageFailedToInstall: "LINK_LANGUAGE_FAILED_TO_INSTALL",
    LinkLanguageInstalledButNotSynced: "LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED",
    Synced: "SYNCED",
} as const
export type PerspectiveState = typeof PerspectiveState[keyof typeof PerspectiveState]
// This type is used in the REST interface to reference a mutable
// prespective that is implemented locally by the Ad4m runtime.
// The UUID is used in mutations to identify the perspective that gets mutated.
export class PerspectiveHandle {
    uuid: string
    name: string
    state: PerspectiveState
    sharedUrl?: string
    neighbourhood?: NeighbourhoodExpression
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

    /** Build a PerspectiveHandle from the executor's wire shape. */
    static fromWire(wire: WirePerspectiveHandle): PerspectiveHandle {
        // Keep the wire's nulls: callers compare these fields with null.
        const handle = new PerspectiveHandle(wire.uuid, wire.name, wire.state)
        handle.sharedUrl = wire.sharedUrl
        handle.neighbourhood = wire.neighbourhood ? neighbourhoodExpressionFromWire(wire.neighbourhood) : null
        handle.owners = wire.owners
        return handle
    }
}

/** Build a NeighbourhoodExpression, with a `Perspective` as its meta, from the wire shape. */
export function neighbourhoodExpressionFromWire(wire: DecoratedNeighbourhoodExpression): NeighbourhoodExpression {
    const meta = new Perspective(wire.data.meta.links.map(LinkExpression.fromWire))
    return new NeighbourhoodExpression(
        wire.author, wire.timestamp, new Neighbourhood(wire.data.linkLanguage, meta), wire.proof,
    )
}
