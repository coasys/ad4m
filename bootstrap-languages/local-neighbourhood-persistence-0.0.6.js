// local-neighbourhood-persistence -- test fixture for AD4M integration tests.
//
// Persists neighbourhood expressions via the OPTIONAL Storage File I/O
// extension (ad4m:host spec §7.6). Uses filesystem rather than the core
// KV for the same reason as local-language-persistence: the test
// harness needs the data to survive across per-agent data-path wipes,
// which the KV's per-agent scoping cannot provide.
// See local-language-persistence-0.0.9.js for the longer rationale.
import {
    agentCreateSignedExpression,
    hash,
    languageSettings,
    readStorageFile,
    writeStorageFile,
} from "ad4m:host";

export const name = "neighbourhood-store";
export const version = "0.0.6";

let storagePath = "";

function join(...parts) {
    return parts.join("/").replace(/\/+/g, "/");
}

export async function init() {
    const settingsJson = languageSettings();
    let settings = {};
    try {
        const parsed = settingsJson ? JSON.parse(settingsJson) : null;
        if (parsed && typeof parsed === "object") settings = parsed;
    } catch (_) {}
    storagePath = settings.storagePath || "./tst-tmp/";
}

export function interactions(_expression) {
    return [];
}

export async function expressionGet(address) {
    const neighbourhoodPath = join(storagePath, `neighbourhood-${address}.json`);
    try {
        const neighbourhood = JSON.parse(readStorageFile(neighbourhoodPath));
        console.log("Found neighbourhood: ", neighbourhood);
        return neighbourhood;
    } catch {
        return null;
    }
}

export async function expressionCreate(neighbourhood) {
    const expression = agentCreateSignedExpression(neighbourhood);
    const content = JSON.stringify(expression);
    const address = hash(content);
    const neighbourhoodPath = join(storagePath, `neighbourhood-${address}.json`);
    console.log("Writing neighbourhood with path: ", neighbourhoodPath);
    writeStorageFile(neighbourhoodPath, content);
    return address;
}

export async function teardown() {}
