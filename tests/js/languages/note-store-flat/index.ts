/**
 * Flat-export version of note-store language.
 * Tests the new Language v1 module pattern.
 *
 * Context accessed via imports from "ad4m:host":
 * - languageStorageDirectory() -- returns storage directory path
 * - languageAddress() -- returns this language's address
 * - languageSettings() -- returns settings JSON string
 */
import { exists } from "https://deno.land/std@0.184.0/fs/mod.ts";
import { join } from "https://deno.land/std@0.184.0/path/mod.ts";
import {
    languageStorageDirectory,
    languageAddress,
    languageSettings,
    agentCreateSignedExpression,
} from "ad4m:host";

// Direct exports (flat pattern)
export const name = "note-store-flat";
export const version = "0.1.0";

// Module-level state (set in init)
let storagePath = "";

export async function init(): Promise<void> {
    storagePath = languageStorageDirectory();
    const addr = languageAddress();
    const settingsJson = languageSettings();
    const settings = settingsJson ? JSON.parse(settingsJson) : {};

    console.log("[note-store-flat] init() called, storage:", storagePath, "address:", addr);
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
    const expr = agentCreateSignedExpression(content);
    const exprString = JSON.stringify(expr);
    // @ts-ignore
    const hash = UTILS.hash(exprString);
    Deno.writeTextFileSync(join(storagePath, `${hash}.txt`), exprString);
    console.log("[note-store-flat] expressionCreate:", hash);
    return hash;
}

// Teardown
export async function teardown(): Promise<void> {
    console.log("[note-store-flat] teardown() called");
}
