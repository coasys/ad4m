import Agent from './Agent'

export default class Expression {
    author: Agent;
    timestamp: string;
    data: object;
    proof: ExpressionProof;

    constructor() {
        this.author = new Agent("anonymous")
        this.timestamp = "never"
        this.data = {}
    }
}

export function isExpression(e: any): boolean {
    return e && e.author && e.timestamp && e.data
}

export class ExpressionProof {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;

    constructor(sig: string, k: string) {
        this.key = k
        this.signature = sig
    }
}