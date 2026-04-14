/**
 * # Neighbourhood Store — Flat Export Language
 *
 * Expression language that stores neighbourhood metadata via a centralized
 * proxy (Cloudflare Workers KV). Flat-export migration of the legacy
 * create()-factory version.
 */

import axiod from "https://deno.land/x/axiod/mod.ts";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "neighbourhood-store";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev/";

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

export async function expressionCreate(neighbourhood: object): Promise<string> {
    // @ts-ignore — UTILS is injected by the runtime
    const hash = UTILS.hash(JSON.stringify(neighbourhood));
    const expression = agent.createSignedExpression(neighbourhood);

    const neighbourhoodPostData = {
        key: hash,
        value: JSON.stringify(expression),
    };
    const neighbourhoodPostResult = await axiod.post(PROXY_URL, neighbourhoodPostData);
    if (neighbourhoodPostResult.status != 200) {
        console.error("Upload neighbourhood data gets error: ", neighbourhoodPostResult);
    }

    return hash;
}

export async function expressionGet(address: string): Promise<any> {
    const cid = address.toString();

    let presignedUrl;
    try {
        const getPresignedUrl = await axiod.get(PROXY_URL + `?key=${cid}`);
        presignedUrl = getPresignedUrl.data.url;
    } catch (e) {
        console.error("Get neighbourhood failed at getting presigned url", e);
        return null;
    }

    let neighbourhoodObject;
    try {
        const getNeighbourhoodObject = await axiod.get(presignedUrl);
        neighbourhoodObject = getNeighbourhoodObject.data;
    } catch (e) {
        console.error("Get meta information failed at getting meta information", e);
        return null;
    }

    return neighbourhoodObject;
}
