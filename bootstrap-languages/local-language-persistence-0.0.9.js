// local-language-persistence -- test fixture for AD4M integration tests.
//
// A minimal language-language that persists language meta + bundle data
// via the OPTIONAL Storage File I/O extension (ad4m:host spec §7.6).
//
// This language deliberately does NOT use the core KV store: the test
// harness publishes languages in one executor run and then reads them
// back from a DIFFERENT per-agent data path in subsequent test runs.
// The KV is per-agent by design and would be lost across that boundary.
// This language instead writes to a shared path relative to the test
// harness's CWD (permitted because the language-language is a system
// language with CWD access), which survives the per-agent wipe.
//
// That cross-agent shared-path behaviour is exactly what the File I/O
// extension is for. Production languages should prefer the KV API
// (storageGet/storagePut) -- this fixture is an exception.
//
// Only runs on runtimes that install the File I/O extension (Deno
// executor does; a pure browser runtime without IndexedDB mapping
// would not).
import {
    agentCreateSignedExpression,
    languageSettings,
    readStorageFile,
    writeStorageFile,
} from "ad4m:host";

export const name = "languages";
export const version = "0.0.9";

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
    // CWD-relative default matches the test harness layout: tests run
    // from tests/js/, so this resolves to tests/js/tst-tmp/languages/
    // which sits OUTSIDE any per-agent data path.
    storagePath = settings.storagePath || "./tst-tmp/languages";
}

export function interactions(_expression) {
    return [];
}

export async function expressionGet(address) {
    const metaPath = join(storagePath, `meta-${address}.json`);
    try {
        return JSON.parse(readStorageFile(metaPath));
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
    const expression = agentCreateSignedExpression(language.meta);
    const metaPath = join(storagePath, `meta-${hash}.json`);
    const bundlePath = join(storagePath, `bundle-${hash}.js`);
    console.log("Writing meta & bundle path: ", metaPath, bundlePath);
    writeStorageFile(metaPath, JSON.stringify(expression));
    writeStorageFile(bundlePath, language.bundle.toString());
    return hash;
}

export async function languageGetSource(address) {
    const bundlePath = join(storagePath, `bundle-${address}.js`);
    try {
        return readStorageFile(bundlePath);
    } catch {
        throw new Error("Did not find language source for given address:" + address);
    }
}

export async function teardown() {}
