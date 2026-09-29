// Local language-language — stores language meta + bundles locally.
//
// Two storage modes, chosen once in init() from the language settings:
//
// - Default (no `storagePath` setting): ad4m:host storageGet/storagePut.
//   That KV store is scoped to this executor and this language, so
//   languages published here are only visible to this executor. For
//   local/standalone deployments, bundles are pre-populated on disk
//   during startup (see generate-seed.mjs), so the language-language
//   never needs network access.
//
// - Shared (`{"storagePath": "<dir>"}` in the language settings): files
//   `meta-<address>.json` and `bundle-<address>.js` in <dir>, written and
//   read through the optional File I/O extension (readStorageFile /
//   writeStorageFile). Several executors pointed at the same directory
//   see each other's published languages; the integration tests use this
//   in place of a network. The executor only grants file access outside
//   the language's own storage directory to system languages, and then
//   only to its working directory, so <dir> must lie inside it. The
//   directory must exist: writeStorageFile does not create it.
import {
    agentCreateSignedExpression,
    hash,
    languageSettings,
    readStorageFile,
    storageGet,
    storagePut,
    writeStorageFile,
} from "ad4m:host";

export const name = "local-language-store";
export const version = "0.2.0";

// Shared directory, or "" for KV mode.
let storagePath = "";

function readSettings() {
    try {
        const parsed = JSON.parse(languageSettings() || "null");
        return parsed && typeof parsed === "object" ? parsed : {};
    } catch (_) {
        return {};
    }
}

function filePath(key) {
    const ext = key.startsWith("meta-") ? ".json" : ".js";
    return (storagePath + "/" + key + ext).replace(/\/+/g, "/");
}

// Returns the stored string, or null when there is none.
function load(key) {
    if (!storagePath) return storageGet(key) || null;
    try {
        return readStorageFile(filePath(key));
    } catch (_) {
        return null;
    }
}

function store(key, value) {
    if (storagePath) writeStorageFile(filePath(key), value);
    else storagePut(key, value);
}

export async function init() {
    const settings = readSettings();
    storagePath = typeof settings.storagePath === "string" ? settings.storagePath : "";
}
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
    store("meta-" + computed, JSON.stringify(expression));
    store("bundle-" + computed, language.bundle.toString());
    return computed;
}

export async function expressionGet(address) {
    try {
        const raw = load("meta-" + address);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}

export async function languageGetSource(address) {
    const raw = load("bundle-" + address);
    if (!raw) {
        throw new Error("No language source found for address: " + address);
    }
    return raw;
}
