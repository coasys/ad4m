import { ExpressionGeneric } from "../expression/Expression";
import { Link, LinkExpression, LinkExpressionInput, LinkInput, linkExpressionToWire } from "../links/Links";
import { LinkQuery } from "./LinkQuery";
import type { DecoratedPerspective } from "../generated/api/DecoratedPerspective";
import type { Perspective as WirePerspective } from "../generated/api/Perspective";
import type { PerspectiveExpression as WirePerspectiveExpression } from "../generated/api/PerspectiveExpression";

/** A Perspective represents subjective meaning, encoded through
* associations between expressions, a.k.a. Links, that is a graph
* over the objective Expressions of any subset of Languages.
*
* This type represents the clean onotological concept of a Perspective.
* An instance of this class can be regarded as an immutable snapshot of 
* a mutable perspective.
*
* The types PerspectiveProxy and PerspectiveHandle are used when dealing 
* with an instantiated mutable perspective as is done through most of 
* the AD4M API.
*/
export class Perspective {
    /** The content of the perspective, a list/graph of links */
    links: LinkExpression[]

    constructor(links?: LinkExpression[]) {
        if(links) {
            this.links = links
        } else {
            this.links = []
        }
    }

    /** Build a Perspective (with its query helpers) from the executor's wire shape. */
    static fromWire(wire: DecoratedPerspective | null): Perspective {
        return new Perspective((wire?.links ?? []).map(LinkExpression.fromWire))
    }

    static toWire(perspective: Perspective): WirePerspective {
        return { links: perspective.links.map(linkExpressionToWire) }
    }
    
    /** Convenience function for filtering links just like with PerspectiveProxy.
     *  Applies every filter of `query`; returns copies, so changing a result
     *  does not change this snapshot. */
    get(query: LinkQuery): LinkExpression[] {
        const { source, predicate, target, fromDate, untilDate, limit } = query ?? {} as LinkQuery
        // Dates may arrive as strings (a plain-object query), as PerspectiveProxy accepts.
        const from = fromDate ? new Date(fromDate).getTime() : undefined
        const until = untilDate ? new Date(untilDate).getTime() : undefined
        let result = this.links.filter(l =>
            (!source || l.data.source === source) &&
            (!predicate || l.data.predicate === predicate) &&
            (!target || l.data.target === target) &&
            (from === undefined || new Date(l.timestamp).getTime() >= from) &&
            (until === undefined || new Date(l.timestamp).getTime() <= until))
        if (limit) result = result.slice(0, limit)
        // A JSON copy, not structuredClone: a framework may hold this snapshot
        // in a reactive Proxy (Vue reactive(), Svelte 5 $state), and
        // structuredClone throws DataCloneError on a Proxy.
        return result.map(l => Object.setPrototypeOf(JSON.parse(JSON.stringify(l)), Object.getPrototypeOf(l)))
    }

    /** Convenience function to get the target of the first link that matches the given query
     * This makes sense when the query is expected to return only one link
     * and the target of that link is what you are looking for.
     */
    getSingleTarget(query: LinkQuery): string|void {
        const foundLinks = this.get(new LinkQuery({ ...query, target: undefined }))
        if(foundLinks.length)
            return foundLinks[0].data.target
        else
            return null
    }
    
}
export class PerspectiveInput {
    links: LinkExpressionInput[]
}
export class PerspectiveUnsignedInput {
    links: LinkInput[]

    constructor(links?: LinkInput[]) {
        if(links)
            this.links = links
        else
            this.links = []
    }

    static fromLink(link: Link): PerspectiveUnsignedInput {
        let obj = new PerspectiveUnsignedInput()
        obj.links.push(link)
        return obj
    }
}
export class PerspectiveExpression extends ExpressionGeneric(Perspective) {
    static fromWire(wire: WirePerspectiveExpression): PerspectiveExpression {
        return new PerspectiveExpression(wire.author, wire.timestamp, Perspective.fromWire(wire.data), wire.proof)
    }
};
