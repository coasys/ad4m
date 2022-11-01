import { Field, InputType, ObjectType } from "type-graphql";
import { ExpressionGeneric, ExpressionGenericInput } from '../expression/Expression';

@ObjectType()
export class Link {
    @Field()
    source: string;
    
    @Field()
    target: string;
    
    @Field({nullable: true})
    predicate?: string;

    constructor(obj) {
        this.source = obj.source ? obj.source : ''
        this.target = obj.target ? obj.target : ''
        this.predicate = obj.predicate ? obj.predicate : ''
    }
}

@InputType()
export class LinkMutations {
    @Field(type => [LinkInput])
    additions: LinkInput[];

    @Field(type => [LinkExpression])
    removals: LinkExpression[];
}

@InputType()
export class LinkInput {
    @Field()
    source: string;
    
    @Field()
    target: string;
    
    @Field({nullable: true})
    predicate?: string;
}

@ObjectType()
export class LinkExpression extends ExpressionGeneric(Link) {
    hash(): number {
        const mash = JSON.stringify(this.data, Object.keys(this.data).sort()) +
        JSON.stringify(this.author) + this.timestamp
        let hash = 0, i, chr;
        for (i = 0; i < mash.length; i++) {
        chr   = mash.charCodeAt(i);
        hash  = ((hash << 5) - hash) + chr;
        hash |= 0; // Convert to 32bit integer
        }
        return hash;
    }
};

@InputType()
export class LinkExpressionInput extends ExpressionGenericInput(LinkInput) {};

export function linkEqual(l1: LinkExpression, l2: LinkExpression): boolean {
    return l1.author == l2.author &&
        l1.timestamp == l2.timestamp &&
        l1.data.source == l2.data.source &&
        l1.data.predicate == l2.data.predicate &&
        l1.data.target == l2.data.target
}

export function isLink(l: any): boolean {
    return l && l.source && l.target
}
