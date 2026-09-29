// Local perspective-language — stores perspective snapshot expressions locally.
//
// Uses ad4m:host storageGet/storagePut for persistence. The production
// perspective-language has never had a working implementation (stub
// since inception). This local version provides a functional store.
import {
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "ad4m:host";

export const name = "local-perspective-store";
export const version = "0.1.0";

export async function init() {}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(perspective) {
    const address = hash(JSON.stringify(perspective));
    const expression = agentCreateSignedExpression(perspective);
    storagePut("perspective-" + address, JSON.stringify(expression));
    return address;
}

export async function expressionGet(address) {
    try {
        const raw = storageGet("perspective-" + address);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
