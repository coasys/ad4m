/**
 * Flat-export test language for AD4M-executor integration tests.
 *
 * Exercises the interactions + expression capability surface with a
 * simple in-memory expression store.
 */
import type { Address, Interaction, Expression } from "https://esm.sh/v135/@coasys/ad4m@0.5.0";

export const name = "test-language";
export const version = "0.0.1";

const expressions: Expression[] = [];
let agent: any = null;

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
}

export function interactions(expressionAddress: Address): Interaction[] {
    return [{
        label: "Modify an expression",
        name: "modify",
        parameters: [{ name: "newValue", type: "object" }],
        execute: async (parameters: object) => {
            const addr = parseInt(expressionAddress);
            if (addr > expressions.length) return "Non-existant expression";
            //@ts-ignore
            const content = parameters["newValue"];
            const expr = agent.createSignedExpression(content);
            expressions[addr] = expr;
            return "ok";
        }
    }];
}

export async function expressionGet(address: Address): Promise<Expression | null> {
    return expressions[parseInt(address)] ?? null;
}

export async function expressionCreate(content: object): Promise<Address> {
    const expr = agent.createSignedExpression(content);
    const addr = expressions.length;
    expressions[addr] = expr;
    return addr.toString();
}

export async function teardown(): Promise<void> {}
