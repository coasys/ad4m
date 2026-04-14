/**
 * Flat-export note-store test language for AD4M-executor integration tests.
 *
 * Persists expressions to individual files in the language's storage
 * directory, keyed by their content hash.
 */
import { exists } from "https://deno.land/std@0.184.0/fs/mod.ts";
import { join } from "https://deno.land/std@0.184.0/path/mod.ts";
import type { Address, Interaction, Expression } from "https://esm.sh/v135/@coasys/ad4m@0.5.0";

export const name = "note-store";
export const version = "0.0.1";

let storagePath = "";
let agent: any = null;

export async function init(): Promise<void> {
    storagePath = (globalThis as any).languageStorageDirectory();
    agent = (globalThis as any).__agentProxy__;
}

export function interactions(_expressionAddress: Address): Interaction[] {
    return [];
}

export async function expressionGet(address: Address): Promise<Expression | null> {
    const path = join(storagePath, `${address}.txt`);
    console.log("note-store language trying to get at path:", path);
    try {
        await exists(path);
        return JSON.parse(Deno.readTextFileSync(path));
    } catch (e) {
        console.error("caught error", e);
        return null;
    }
}

export async function expressionCreate(content: object): Promise<Address> {
    const expr = agent.createSignedExpression(content);
    const exprString = JSON.stringify(expr);
    // @ts-ignore
    const hash = UTILS.hash(exprString);
    Deno.writeTextFileSync(join(storagePath, `${hash}.txt`), exprString);
    return hash;
}

export async function teardown(): Promise<void> {}
