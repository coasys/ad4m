// Local language-language — stores language meta + bundles locally.
//
// Uses ad4m:host storageGet/storagePut for persistence within the
// executor's per-language storage directory. For local/standalone
// deployments, bundles are pre-populated on disk during startup, so
// the language-language never needs network access.
import {
    agentCreateSignedExpression,
    hash,
    storageGet,
    storagePut,
} from "ad4m:host";

export const name = "local-language-store";
export const version = "0.1.0";

export async function init() {}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(language) {
    const computed = hash(language.bundle.toString());
    if (computed !== language.meta.address) {
        throw new Error(
            "Language store: address mismatch.\n" +
            "Expected: " + language.meta.address + "\n" +
            "Got: " + computed
        );
    }
    const expression = agentCreateSignedExpression(language.meta);
    storagePut("meta-" + computed, JSON.stringify(expression));
    storagePut("bundle-" + computed, language.bundle.toString());
    return computed;
}

export async function expressionGet(address) {
    try {
        const raw = storageGet("meta-" + address);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}

export async function languageGetSource(address) {
    const raw = storageGet("bundle-" + address);
    if (!raw) {
        throw new Error("No language source found for address: " + address);
    }
    return raw;
}
