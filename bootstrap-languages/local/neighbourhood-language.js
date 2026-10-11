// Local neighbourhood-language — stores neighbourhood expressions locally.
//
// Neighbourhood expressions contain link language addresses, meta, and
// membership data. Two storage modes, chosen once in init() from the
// language settings, like local/language-language.js:
//
// - Default (no `storagePath` setting): ad4m:host storageGet/storagePut,
//   visible to this executor only.
// - Shared (`{"storagePath": "<dir>"}`): files `neighbourhood-<address>.json`
//   in <dir>, through the optional File I/O extension (readStorageFile /
//   writeStorageFile), so executors pointed at the same directory can join
//   each other's neighbourhoods. <dir> must exist and lie inside the
//   executor's working directory (see language-language.js).
import {
    agentCreateSignedExpression,
    hash,
    languageSettings,
    readStorageFile,
    storageGet,
    storagePut,
    writeStorageFile,
} from "ad4m:host";

export const name = "local-neighbourhood-store";
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
    return (storagePath + "/" + key + ".json").replace(/\/+/g, "/");
}

export async function init() {
    const settings = readSettings();
    storagePath = typeof settings.storagePath === "string" ? settings.storagePath : "";
}
export function interactions() { return []; }
export async function teardown() {}

export async function expressionCreate(neighbourhood) {
    const address = hash(JSON.stringify(neighbourhood));
    const expression = JSON.stringify(agentCreateSignedExpression(neighbourhood));
    const key = "neighbourhood-" + address;
    if (storagePath) writeStorageFile(filePath(key), expression);
    else storagePut(key, expression);
    return address;
}

export async function expressionGet(address) {
    const key = "neighbourhood-" + address;
    try {
        const raw = storagePath ? readStorageFile(filePath(key)) : storageGet(key);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
