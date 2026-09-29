// Local agent-language — stores agent expressions locally.
//
// Each agent expression holds a DID, public perspective links, and
// optional direct-message language reference. Two storage modes, chosen
// once in init() from the language settings, like local/language-language.js:
//
// - Default (no `storagePath` setting): ad4m:host storageGet/storagePut,
//   so an executor can only resolve its own agents' profiles.
// - Shared (`{"storagePath": "<dir>"}`): files `agent-<did>.json` in <dir>,
//   through the optional File I/O extension (readStorageFile /
//   writeStorageFile), so executors pointed at the same directory resolve
//   each other's profiles (agent.byDID, did:// expressions). <dir> must
//   exist and lie inside the executor's working directory.
import {
    agentDid,
    agentCreateSignedExpression,
    languageSettings,
    readStorageFile,
    storageGet,
    storagePut,
    writeStorageFile,
} from "ad4m:host";

export const name = "local-agent-store";
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

export async function expressionCreate(content) {
    if (!content || !content.did || !content.perspective) {
        throw new Error("Agent expression requires did and perspective fields");
    }

    const myDid = agentDid();
    if (content.did !== myDid) {
        throw new Error("Can only create agent expressions for own DID");
    }

    // Strip proof.valid/invalid from perspective links (match production behaviour)
    if (content.perspective && content.perspective.links) {
        content.perspective.links = content.perspective.links.map(function(link) {
            if (link.proof) {
                delete link.proof.valid;
                delete link.proof.invalid;
            }
            return link;
        });
    }

    const expression = JSON.stringify(agentCreateSignedExpression(content));
    const key = "agent-" + content.did;
    if (storagePath) writeStorageFile(filePath(key), expression);
    else storagePut(key, expression);
    return content.did;
}

export async function expressionGet(did) {
    const key = "agent-" + did;
    try {
        const raw = storagePath ? readStorageFile(filePath(key)) : storageGet(key);
        if (!raw) return null;
        return JSON.parse(raw);
    } catch (_) {
        return null;
    }
}
