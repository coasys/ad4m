import { ExpressionGeneric, ExpressionGenericInput } from '../expression/Expression';
import { LinkStatus } from "../perspectives/PerspectiveProxy";
export class Link {
    source: string;
    target: string;
    predicate?: string;

    constructor(obj) {
        this.source = obj.source ? obj.source : ''
        this.target = obj.target ? obj.target : ''
        this.predicate = obj.predicate ? obj.predicate : ''
    }
}
export class LinkMutations {
    additions: LinkInput[];
    removals: LinkExpressionInput[];
}
export class LinkExpressionMutations {
    additions: LinkExpression[];
    removals: LinkExpression[];

    constructor(additions: LinkExpression[], removals: LinkExpression[]) {
        this.additions = additions
        this.removals = removals
    }
}
export class LinkInput {
    source: string;
    target: string;
    predicate?: string;
}
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
    status?: LinkStatus;
};
export class LinkExpressionInput extends ExpressionGenericInput(LinkInput) {
    hash: () => number;
    status?: LinkStatus;
};

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
export class LinkExpressionUpdated {
    oldLink: LinkExpression;
    newLink: LinkExpression;

    constructor(oldLink: LinkExpression, newLink: LinkExpression) {
        this.oldLink = oldLink
        this.newLink = newLink
    }
}
