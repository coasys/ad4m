/**
 * Flat-export version of note-store language.
 * Tests the new flat export pattern (no create() factory).
 * 
 * NEW INTERFACE: init() takes NO arguments. Context accessed via:
 * - globalThis.languageStorageDirectory() — returns storage directory path
 * - globalThis.languageAddress() — returns this language's address
 * - globalThis.languageSettings() — returns settings JSON string
 * Delegates (agent, holochain) available via globalThis.
 */
import { exists } from "https://deno.land/std@0.184.0/fs/mod.ts";
import { join } from "https://deno.land/std@0.184.0/path/mod.ts";

// Direct exports (flat pattern)
export const name = "note-store-flat";
export const version = "0.1.0";

// Module-level state (set in init)
let storagePath = "";
let agent: any = null;

export async function init(): Promise<void> {
    // NEW: Get language context via flat import functions
    storagePath = languageStorageDirectory();
    const languageAddress = languageAddress();
    const settingsJson = languageSettings();
    const settings = settingsJson ? JSON.parse(settingsJson) : {};
    
    // Agent available via globalThis (set by bootstrap before init)
    agent = globalThis.__agentProxy__;
    
    console.log("[note-store-flat] init() called, storage:", storagePath, "address:", languageAddress);
}

export function interactions(expressionAddress: string): any[] {
    return [];
}

// Expression capability
export async function expressionGet(address: string): Promise<any | null> {
    const path = join(storagePath, `${address}.txt`);
    console.log("[note-store-flat] expressionGet:", path);
    try {
        await exists(path);
        return JSON.parse(Deno.readTextFileSync(path));
    } catch (e) {
        console.error("[note-store-flat] expressionGet error:", e);
        return null;
    }
}

export async function expressionCreate(content: object): Promise<string> {
    const expr = agent.createSignedExpression(content);
    const exprString = JSON.stringify(expr);
    // @ts-ignore
    const hash = UTILS.hash(exprString);
    Deno.writeTextFileSync(join(storagePath, `${hash}.txt`), exprString);
    console.log("[note-store-flat] expressionCreate:", hash);
    return hash;
}

// Note: addressOf is handled via mod.expressionAddressOf in the bootstrap
// The bootstrap maps it to putAdapter.addressOf when present

// Teardown
export async function teardown(): Promise<void> {
    console.log("[note-store-flat] teardown() called");
}
