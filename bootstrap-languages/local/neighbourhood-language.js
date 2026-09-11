// Local neighbourhood-language — stores neighbourhood expressions locally.
//
// Uses ad4m:host storageGet/storagePut for persistence. Neighbourhood
// expressions contain link language addresses, meta, and membership data.
import {
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "ad4m:host";

export const name = "local-neighbourhood-store";
export const version = "0.1.0";

export async function init() {}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(neighbourhood) {
    const address = hash(JSON.stringify(neighbourhood));
    const expression = agentCreateSignedExpression(neighbourhood);
    storagePut("neighbourhood-" + address, JSON.stringify(expression));
    return address;
}

export async function expressionGet(address) {
    try {
        const raw = storageGet("neighbourhood-" + address);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
