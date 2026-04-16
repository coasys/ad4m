// local-neighbourhood-persistence -- test fixture for AD4M integration tests.
//
// A minimal language that persists neighbourhood expression files to a
// local directory. Used by the test setup to stand in for the
// "neighbourhoodLanguage" slot in the bootstrap seed.
import { languageSettings, agentCreateSignedExpression } from "ad4m:host";

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
        const neighbourhood = JSON.parse(Deno.readTextFileSync(neighbourhoodPath).toString());
        console.log("Found neighbourhood: ", neighbourhood);
        return neighbourhood;
    } catch {
        return null;
    }
}

export async function expressionCreate(neighbourhood) {
    const expression = agentCreateSignedExpression(neighbourhood);
    const content = JSON.stringify(expression);
    // @ts-ignore - UTILS is a runtime global provided by the executor
    const address = UTILS.hash(content);
    const neighbourhoodPath = join(storagePath, `neighbourhood-${address}.json`);
    console.log("Writing neighbourhood with path: ", neighbourhoodPath);
    Deno.writeTextFileSync(neighbourhoodPath, content);
    return address;
}

export async function teardown() {}
