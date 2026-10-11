import { ExpressionGeneric, ExpressionGenericInput } from '../expression/Expression';
import type { DecoratedLinkExpression } from "../generated/api/DecoratedLinkExpression";
import type { LinkExpression as WireLinkExpression } from "../generated/api/LinkExpression";
import type { LinkExpressionInput as WireLinkExpressionInput } from "../generated/api/LinkExpressionInput";
import type { LinkMutations as WireLinkMutations } from "../generated/api/LinkMutations";
import type { LinkStatus as WireLinkStatus } from "../generated/api/LinkStatus";
import type { PerspectiveLinkDiff } from "../generated/api/PerspectiveLinkDiff";
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

    static fromWire(diff: PerspectiveLinkDiff): LinkExpressionMutations {
        return new LinkExpressionMutations(
            diff.additions.map(LinkExpression.fromWire),
            diff.removals.map(LinkExpression.fromWire),
        )
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
    status?: WireLinkStatus;

    /** Build a LinkExpression (with `hash()`) from the executor's wire shape. */
    static fromWire(wire: WireLinkExpression | DecoratedLinkExpression): LinkExpression {
        const link = new LinkExpression(wire.author, wire.timestamp, wire.data, wire.proof)
        if (wire.status) link.status = wire.status
        return link
    }
};
export class LinkExpressionInput extends ExpressionGenericInput(LinkInput) {
    hash: () => number;
    status?: WireLinkStatus;
};

export function linkExpressionToWire(link: LinkExpression): WireLinkExpression {
    return {
        author: link.author,
        timestamp: link.timestamp,
        data: { source: link.data.source, target: link.data.target, predicate: link.data.predicate ?? null },
        proof: { key: link.proof.key, signature: link.proof.signature },
        status: link.status ?? null,
    }
}

export function linkExpressionInputToWire(link: LinkExpressionInput): WireLinkExpressionInput {
    return {
        author: link.author,
        timestamp: link.timestamp,
        data: { source: link.data.source, target: link.data.target, predicate: link.data.predicate },
        proof: { key: link.proof.key, signature: link.proof.signature, valid: link.proof.valid, invalid: link.proof.invalid },
        status: link.status,
    }
}

export function linkMutationsToWire(mutations: LinkMutations): WireLinkMutations {
    return {
        additions: mutations.additions,
        removals: mutations.removals.map(linkExpressionInputToWire),
    }
}

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
