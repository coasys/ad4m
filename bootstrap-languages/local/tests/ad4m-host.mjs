// `ad4m:host` for the tests: an in-memory store, no executor.
import { registerHooks } from "node:module";

const store = new Map();

export const agentDid = "did:key:test";
export function storageGet(key) { return store.get(key) ?? null; }
export function storagePut(key, value) { store.set(key, value); }
export function emitPerspectiveDiff() {}

const self = import.meta.url;
registerHooks({
    resolve(specifier, context, next) {
        if (specifier === "ad4m:host") return { url: self, shortCircuit: true };
        return next(specifier, context);
    },
});
