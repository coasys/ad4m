/**
 * Flat-export version of note-store language.
 * Tests the new Language v1 module pattern.
 *
 * Context accessed via imports from "ad4m:host":
 * - languageAddress() -- returns this language's address
 * - languageSettings() -- returns settings JSON string
 * - storageGet/storagePut -- per-language KV store
 * - agentCreateSignedExpression -- sign data
 */
import {
    languageAddress,
    languageSettings,
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "ad4m:host";

// Direct exports (flat pattern)
export const name = "note-store-flat";
export const version = "0.1.0";

export async function init(): Promise<void> {
    const addr = languageAddress();
    const settingsJson = languageSettings();
    const settings = settingsJson ? JSON.parse(settingsJson) : {};

    console.log("[note-store-flat] init() called, address:", addr);
}

export function interactions(expressionAddress: string): any[] {
    return [];
}

// Expression capability
export async function expressionGet(address: string): Promise<any | null> {
    console.log("[note-store-flat] expressionGet:", address);
    const data = storageGet(address);
    if (data === null) return null;
    try {
        return JSON.parse(data);
    } catch (e) {
        console.error("[note-store-flat] expressionGet error:", e);
        return null;
    }
}

export async function expressionCreate(content: object): Promise<string> {
    const expr = agentCreateSignedExpression(content);
    const exprString = JSON.stringify(expr);
    const address = hash(exprString);
    storagePut(address, exprString);
    console.log("[note-store-flat] expressionCreate:", address);
    return address;
}

// Teardown
export async function teardown(): Promise<void> {
    console.log("[note-store-flat] teardown() called");
}
