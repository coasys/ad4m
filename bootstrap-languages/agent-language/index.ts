/**
 * # Agent Expression Store — Flat Export Language
 *
 * Expression language that stores agent expressions via Holochain DNA.
 * Flat-export migration of the legacy create()-factory version.
 */

import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "agent-expression-store";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

let hc: any = null;
let agent: any = null;

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
    hc = (globalThis as any).__holochainDelegate__;

    const dnaBundle = Buffer.from(BUNDLE, "base64");
    await hc.registerDNAs([{
        file: dnaBundle,
        nick: DNA_ROLE,
        zomeCalls: [
            [ZOME_NAME, "create_agent_expression"],
            [ZOME_NAME, "get_agent_expression"],
        ],
    }]);
}

export async function teardown(): Promise<void> {
    hc = null;
    agent = null;
}

export function interactions(): any[] {
    return [];
}

// =============================================================================
// Expression capability
// =============================================================================

export async function expressionCreate(content: any): Promise<string> {
    if (!content["did"] || !content["perspective"] || !content["perspective"].links)
        throw "Content must be an Agent object";

    const agentObj = content;
    if (agentObj.did != agent.did)
        throw "Can't set Agent Expression for foreign DID - only for self";

    if (!agentObj.directMessageLanguage) agentObj.directMessageLanguage = undefined;

    agentObj.perspective!.links.forEach((link: any) => {
        delete link.proof.valid;
        delete link.proof.invalid;
        delete link.status;
    });

    const expression = agent.createSignedExpression(agentObj);

    await hc.call(DNA_ROLE, ZOME_NAME, "create_agent_expression", expression);

    return agentObj.did;
}

export async function expressionGet(did: string): Promise<any> {
    console.log("Getting expression with did", did);
    const expression = await hc.call(DNA_ROLE, ZOME_NAME, "get_agent_expression", did);
    return expression;
}
