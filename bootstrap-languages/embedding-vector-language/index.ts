/**
 * # Embedding Vector Language — Flat Export Language
 *
 * Expression language that compresses expressions into base64 URIs.
 * The "address" IS the compressed expression — no external storage.
 * Flat-export migration of the legacy create()-factory version.
 */

import pako from "https://esm.sh/v135/pako@2.0.4";
import base64js from "https://esm.sh/v135/base64-js@1.5.1";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "embedding-vector-language";
export const version = "0.1.0";

// =============================================================================
// Module-level state
// =============================================================================

let agent: any = null;

// =============================================================================
// Helpers
// =============================================================================

function compressUri(uri: string): string {
    const compressed = pako.deflate(uri);
    return base64js.fromByteArray(compressed);
}

function decompressUri(compressedString: string): string {
    const compressed = base64js.toByteArray(compressedString);
    const decompressed = pako.inflate(compressed);
    return new TextDecoder().decode(decompressed);
}

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

export async function expressionCreate(content: object): Promise<string> {
    try {
        const expr = agent.createSignedExpression(content);
        const exprString = JSON.stringify(expr);
        return compressUri(exprString);
    } catch (e) {
        console.error("caught error", e);
        return null;
    }
}

export async function expressionGet(address: string): Promise<any> {
    try {
        const decompressedAddress = decompressUri(address);
        const expr = JSON.parse(decompressedAddress);
        return expr;
    } catch (e) {
        console.error("caught error", e);
        return null;
    }
}
