// local-language-persistence — test fixture for AD4M integration tests.
//
// A minimal flat-pattern language that persists language meta + bundle
// files to a local directory. Used by the test setup to stand in for
// the "languages" (language-language) slot in the bootstrap seed.

export const name = "languages";
export const version = "0.0.9";

let storagePath = "";
let agent = null;

function join(...parts) {
    return parts.join("/").replace(/\/+/g, "/");
}

export async function init() {
    const settingsJson = globalThis.languageSettings();
    let settings = {};
    try {
        const parsed = settingsJson ? JSON.parse(settingsJson) : null;
        if (parsed && typeof parsed === "object") settings = parsed;
    } catch (_) {}
    storagePath = settings.storagePath || "./tst-tmp/languages";
    agent = globalThis.__agentProxy__;
}

export function interactions(_expression) {
    return [];
}

export async function expressionGet(address) {
    const metaPath = join(storagePath, `meta-${address}.json`);
    try {
        const text = Deno.readTextFileSync(metaPath);
        return JSON.parse(text);
    } catch (e) {
        console.log("Did not find meta file for given address:" + address, e);
        return null;
    }
}

export async function expressionCreate(language) {
    // @ts-ignore - UTILS is a runtime global provided by the executor
    const hash = UTILS.hash(language.bundle.toString());
    if (hash !== language.meta.address) {
        throw new Error(
            `Language Persistence: Can't store language. Address stated in meta differs from actual file\n` +
            `Wanted: ${language.meta.address}\nGot: ${hash}`
        );
    }
    const expression = agent.createSignedExpression(language.meta);
    const metaPath = join(storagePath, `meta-${hash}.json`);
    const bundlePath = join(storagePath, `bundle-${hash}.js`);
    console.log("Writing meta & bundle path: ", metaPath, bundlePath);
    Deno.writeTextFileSync(metaPath, JSON.stringify(expression));
    Deno.writeTextFileSync(bundlePath, language.bundle.toString());
    return hash;
}

export async function languageGetSource(address) {
    const bundlePath = join(storagePath, `bundle-${address}.js`);
    try {
        return Deno.readTextFileSync(bundlePath);
    } catch {
        throw new Error("Did not find language source for given address:" + address);
    }
}

export async function teardown() {}
