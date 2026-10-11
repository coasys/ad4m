/**
 * Flat-export note-store test language for AD4M-executor integration tests.
 *
 * Persists expressions via the ad4m:host KV store, keyed by content hash.
 * No direct filesystem access — works in any runtime that implements
 * the ad4m:host storage API.
 */
import type { Address, Interaction, Expression } from "https://esm.sh/v135/@coasys/ad4m@0.5.0";
import { agentCreateSignedExpression, hash, storageGet, storagePut } from "ad4m:host";

export const name = "note-store";
export const version = "0.0.1";

export async function init(): Promise<void> {}

export function interactions(_expressionAddress: Address): Interaction[] {
    return [];
}

export async function expressionGet(address: Address): Promise<Expression | null> {
    console.log("note-store language trying to get:", address);
    const data = storageGet(address);
    if (data === null) return null;
    try {
        return JSON.parse(data);
    } catch (e) {
        console.error("caught error", e);
        return null;
    }
}

export async function expressionCreate(content: object): Promise<Address> {
    const expr = agentCreateSignedExpression(content);
    const exprString = JSON.stringify(expr);
    const address = hash(exprString);
    storagePut(address, exprString);
    return address;
}

export async function teardown(): Promise<void> {}
