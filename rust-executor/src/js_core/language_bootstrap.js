// Language Bootstrap JS
// Runs inside every per-language Deno runtime.
// Bridges raw Deno ops (from registered extensions) and the LanguageContext
// shape that language bundles expect.

// Polyfill Node.js Buffer on globalThis – many language bundles depend on it.
import { Buffer } from "node:buffer";
globalThis.Buffer = Buffer;

// Minimal DOM stubs – language bundles that include Svelte Icon components
// reference HTMLElement, document, and customElements at module load time.
// These stubs prevent "Class extends value undefined" errors without
// needing a full DOM implementation (the Icon is never actually rendered).
if (typeof globalThis.HTMLElement === "undefined") {
    globalThis.HTMLElement = class HTMLElement {
        constructor() { this.shadowRoot = { appendChild() {} }; }
        attachShadow() { return this.shadowRoot; }
        connectedCallback() {}
        disconnectedCallback() {}
        attributeChangedCallback() {}
    };
}
if (typeof globalThis.document === "undefined") {
    const noop = () => ({
        textContent: "",
        appendChild() {},
        removeChild() {},
        insertBefore() { return this; },
        setAttribute() {},
        removeAttribute() {},
        addEventListener() {},
        removeEventListener() {},
        classList: { add() {}, remove() {}, toggle() {} },
        style: {},
        childNodes: [],
        firstChild: null,
        nextSibling: null,
        parentNode: null,
    });
    globalThis.document = {
        createElement: () => noop(),
        createTextNode: (t) => ({ ...noop(), textContent: t }),
        createElementNS: () => noop(),
        createComment: () => noop(),
        head: noop(),
        body: noop(),
        querySelector: () => null,
        querySelectorAll: () => [],
    };
}
if (typeof globalThis.customElements === "undefined") {
    globalThis.customElements = {
        define() {},
        get() { return undefined; },
    };
}
if (typeof globalThis.window === "undefined") {
    globalThis.window = globalThis;
}

// Map of serialised cell_id key → signalCallback for Holochain signal routing.
// Key format: `${hex(dnaHash)}:${hex(agentPubkey)}`
globalThis.__holochainSignalCallbacks__ = new Map();

/**
 * Convert a Uint8Array / number[] to a hex string for use as a map key.
 */
function toHex(arr) {
    return Array.from(arr, b => b.toString(16).padStart(2, "0")).join("");
}

/**
 * Build a lookup key from a cell_id pair [dnaHash, agentPubkey].
 */
function cellIdKey(cellId) {
    return `${toHex(cellId[0])}:${toHex(cellId[1])}`;
}

/**
 * Extract every cell_id from a Holochain AppInfo and register the signal
 * callback for each one — both in the JS-side map (for dispatch) and in the
 * Rust-side registry (so the central signal loop knows which language to target).
 */
function registerSignalCallbacksForApp(appInfo, signalCallback, languageAddress) {
    if (!signalCallback || !appInfo || !appInfo.cell_info) return;

    for (const roleName of Object.keys(appInfo.cell_info)) {
        const cellInfos = appInfo.cell_info[roleName];
        for (const cellInfo of cellInfos) {
            // cellInfo is an enum wrapper: { provisioned: {...} } | { cloned: {...} } | { stem: {...} }
            const inner = cellInfo.provisioned || cellInfo.cloned || cellInfo.stem || cellInfo.value;
            if (!inner || !inner.cell_id) continue;
            const key = cellIdKey(inner.cell_id);
            globalThis.__holochainSignalCallbacks__.set(key, signalCallback);
            // Also notify Rust so the central signal loop can route to this language
            LANGUAGE_CONTROLLER.registerHolochainSignalHandler(key, languageAddress);
        }
    }
}

/**
 * Handle a Holochain signal dispatched from Rust.
 * Rust calls this with the decoded signal object: { cell_id, zome_name, payload }.
 * We match cell_id against registered callbacks and invoke the right one.
 */
globalThis.__handleHolochainSignal__ = async function(signal) {
    if (!signal || !signal.cell_id) return;
    const key = cellIdKey(signal.cell_id);
    const callback = globalThis.__holochainSignalCallbacks__.get(key);
    if (callback) {
        try {
            await callback(signal);
        } catch (e) {
            console.error("Error in Holochain signal callback:", e);
        }
    }
};

/**
 * Dynamically imports the language bundle from a file:// URL and captures
 * the default export as globalThis.languageConstructor.
 */
async function loadLanguageBundle(path) {
    const url = path.startsWith("file://") ? path : `file://${path}`;
    const module = await import(url);
    // Handle module.default.default, module.default, or bare module
    if (module.default && module.default.default) {
        globalThis.languageConstructor = module.default.default;
    } else if (module.default) {
        globalThis.languageConstructor = module.default;
    } else {
        globalThis.languageConstructor = module;
    }
}

/**
 * Creates a Holochain delegate object for a given language address.
 * Provides registerDNAs, call, and callAsync methods.
 */
function createHolochainDelegate(languageAddress) {
    return {
        async registerDNAs(dnas, signalCallback) {
            const results = [];
            for (const dna of dnas) {
                const appId = `${languageAddress}-${dna.nick}`;

                // Normalize source into the tagged enum format Rust expects:
                //   { type: "path", value: "/path/to/happ" }
                //   { type: "bytes", value: Uint8Array }
                // Language bundles may pass various formats:
                //   { file: Uint8Array, nick: ... } — raw hApp bytes
                //   { path: "..." } — file path
                //   { source: { path: "..." } } or { source: { bundle: ... } }
                let source;
                if (dna.source && dna.source.type) {
                    // Already in correct tagged format
                    source = dna.source;
                } else if (dna.source && dna.source.path) {
                    source = { type: "path", value: dna.source.path };
                } else if (dna.source && dna.source.bundle) {
                    source = { type: "bundle", value: dna.source.bundle };
                } else if (dna.file) {
                    // Raw hApp bytes (e.g. perspective-diff-sync passes { file: Uint8Array })
                    source = { type: "bytes", value: new Uint8Array(dna.file) };
                } else if (dna.path) {
                    source = { type: "path", value: dna.path };
                } else {
                    source = dna.source;
                }

                const installPayload = {
                    installed_app_id: appId,
                    agent_key: await HOLOCHAIN_SERVICE.getAgentKey(),
                    membrane_proofs: {},
                    existing_cells: {},
                    network_seed: dna.network_seed || undefined,
                    source: source,
                };
                let appInfo;
                try {
                    appInfo = await HOLOCHAIN_SERVICE.installApp(installPayload);
                } catch (e) {
                    // App may already be installed (possibly under a different app_id
                    // but with the same DNA+agent cell, e.g. when templating languages)
                    appInfo = await HOLOCHAIN_SERVICE.getAppInfo(appId);
                    if (!appInfo) {
                        console.warn(`[registerDNAs] Failed to install app ${appId}: ${e.message || e}. Continuing without this DNA.`);
                        continue;
                    }
                }
                results.push(appInfo);

                // Wire the signalCallback to every cell_id in this app
                registerSignalCallbacksForApp(appInfo, signalCallback, languageAddress);
            }
            return results;
        },

        async call(dnaNick, zomeName, fnName, params) {
            const appId = `${languageAddress}-${dnaNick}`;
            return await HOLOCHAIN_SERVICE.callZomeFunction(
                appId, dnaNick, zomeName, fnName, params
            );
        },

        async callAsync(calls, timeoutMs) {
            const promises = calls.map(call =>
                HOLOCHAIN_SERVICE.callZomeFunction(
                    `${languageAddress}-${call.dnaNick}`,
                    call.dnaNick,
                    call.zomeName,
                    call.fnName,
                    call.params
                )
            );
            if (timeoutMs) {
                const timeoutPromise = new Promise((_, reject) =>
                    setTimeout(() => reject(new Error("callAsync timeout")), timeoutMs)
                );
                return await Promise.race([Promise.all(promises), timeoutPromise]);
            }
            return await Promise.all(promises);
        }
    };
}

/**
 * Creates an ad4mSignal function for a given language address.
 * When called, publishes the signal via the LANGUAGE_CONTROLLER op.
 */
function createAd4mSignal(languageAddress) {
    return function(signal) {
        LANGUAGE_CONTROLLER.ad4mSignalEmitted(signal, languageAddress);
    };
}

/**
 * Initializes the language by parsing the context JSON, building the full
 * LanguageContext with Holochain delegate and ad4mSignal, calling the
 * language constructor, and storing the result.
 */
async function initLanguage(contextJson) {
    const context = typeof contextJson === "string" ? JSON.parse(contextJson) : contextJson;

    const languageAddress = context.Holochain.__languageAddress;

    const holochainDelegate = createHolochainDelegate(languageAddress);
    const ad4mSignal = createAd4mSignal(languageAddress);

    // Build an agent proxy that exposes static properties (did, signingKeyId)
    // and delegates method calls to the global AGENT ops provided by agent_extension.
    const agentProxy = {
        did: context.agent.did,
        signingKeyId: context.agent.signingKeyId,
        createSignedExpression: (data) => AGENT.createSignedExpression(data),
        sign: (payload) => AGENT.sign(payload),
        signStringHex: (payload) => AGENT.signStringHex(payload),
        getAllLocalUserDIDs: () => AGENT.getAllLocalUserDIDs(),
        createSignedExpressionForUser: (userEmail, data) => AGENT.createSignedExpressionForUser(userEmail, data),
        didForUser: (userEmail) => AGENT.didForUser(userEmail),
    };

    const fullContext = {
        agent: agentProxy,
        customSettings: context.customSettings,
        storageDirectory: context.storageDirectory,
        Holochain: holochainDelegate,
        ad4mSignal: ad4mSignal,
    };

    const language = await globalThis.languageConstructor(fullContext);
    globalThis.__ad4m_language_instance__ = language;
    globalThis.language = language;
    return language;
}

globalThis.loadLanguageBundle = loadLanguageBundle;
globalThis.initLanguage = initLanguage;
globalThis.createHolochainDelegate = createHolochainDelegate;
globalThis.createAd4mSignal = createAd4mSignal;
