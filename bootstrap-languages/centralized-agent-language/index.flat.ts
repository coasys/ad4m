/**
 * # Centralized Agent Expression Store — Flat Export Language
 *
 * Expression language that stores agent expressions via a centralized
 * server (socket.ad4m.dev). Flat-export migration of the legacy
 * create()-factory version.
 */

import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "centralized-agent-expression-store";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

let agent: any = null;

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    agent = (globalThis as any).__agentProxy__;
}

export async function teardown(): Promise<void> {
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
    });

    const expression = agent.createSignedExpression(agentObj);

    await axiod.post("https://socket.ad4m.dev/agent", {
        data: {
            did: agentObj.did,
            expression,
        },
    });

    return agentObj.did;
}

export async function expressionGet(did: string): Promise<any> {
    console.log("Getting expression with did", did);

    const data = await axiod.get("https://socket.ad4m.dev/agent", {
        params: { did },
    });

    return data.data.expression;
}
