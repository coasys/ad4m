import { Field, ObjectType, InputType } from "type-graphql";
import { ExpressionGeneric } from "../expression/Expression";
import { LinkExpression, LinkExpressionInput } from "../links/Links";
import { LinkQuery } from "./LinkQuery";

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
* the GraphQL mutations.
*/
@ObjectType()
export class Perspective {
    /** The content of the perspective, a list/graph of links */
    @Field(type => [LinkExpression])
    links: LinkExpression[]

    constructor(links?: LinkExpression[]) {
        if(links) {
            this.links = links
        } else {
            this.links = []
        }
    }
    
    /** Convenience function for filtering links just like with PerspectiveProxy */
    get(query: LinkQuery): LinkExpression[] {
        if(!query || !query.source && !query.predicate && !query.target) {
            return this.links
        }
        
        if(query.source) {
            let result = JSON.parse(JSON.stringify(this.links))
            // @ts-ignore
            if(query.target) result = result.filter(l => l.data.target === query.target)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(l => new Date(l.timestamp) >= query.fromDate!)
            //@ts-ignore
            if (query.untilDate) result = result.filter(l => new Date(l.timestamp) <= query.untilDate!)
            // console.debug("result", result)
            if (query.limit) result = result.slice(0, query.limit);
            return result
        }

        // console.debug("getLinks 3")

        if(query.target) {
            //@ts-ignore
            let result = JSON.parse(JSON.stringify(this.links))
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(l => new Date(l.timestamp) >= query.fromDate!)
            //@ts-ignore
            if (query.untilDate) result = result.filter(l => new Date(l.timestamp) <= query.untilDate!)
            if (query.limit) result = result.slice(0, query.limit);
            return result
        }

        // console.debug("getLinks 4")

        //@ts-ignore
        let result = JSON.parse(JSON.stringify(this.links))
        result = result.filter(link => link.data.predicate === query.predicate)
        if (query.limit) result = result.slice(0, query.limit)
        return result
    }

    /** Convenience function to get the target of the first link that matches the given query
     * This makes sense when the query is expected to return only one link
     * and the target of that link is what you are looking for.
     */
    getSingleTarget(query: LinkQuery): string|void {
        delete query.target
        const foundLinks = this.get(query)
        if(foundLinks.length)
            return foundLinks[0].data.target
        else
            return null
    }
    
}

@InputType()
export class PerspectiveInput {
    @Field(type => [LinkExpressionInput])
    links: LinkExpressionInput[]
}

@ObjectType()
export class PerspectiveExpression extends ExpressionGeneric(Perspective) {};
