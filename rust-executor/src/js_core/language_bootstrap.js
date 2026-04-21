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

// OPTIONAL EXTENSION: Storage File I/O (host-contract.md §"Extension:
// Storage File I/O"). This is where the Deno executor opts into the
// extension by attaching readStorageFile / writeStorageFile to
// LANGUAGE_CONTROLLER. A runtime that doesn't want to expose raw file
// access (e.g. a pure-browser runtime without IndexedDB) just omits
// this block and languages that try to import readStorageFile will
// get a clear error at call time.
//
// Kept in the bootstrap (not the extension snapshot) so this opt-in is
// plainly visible at runtime and easy to audit or swap out.
if (typeof globalThis.LANGUAGE_CONTROLLER === "object" && globalThis.LANGUAGE_CONTROLLER) {
    globalThis.LANGUAGE_CONTROLLER.readStorageFile = function(path) {
        return Deno.readTextFileSync(path);
    };
    globalThis.LANGUAGE_CONTROLLER.writeStorageFile = function(path, content) {
        Deno.writeTextFileSync(path, content);
    };
}

// Map of serialised cell_id key -> signalCallback for Holochain signal routing.
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
    if (!appInfo || !appInfo.cell_info) return;

    // The Rust-side cell_id → languageAddress mapping is registered for
    // EVERY app, regardless of whether the language passed a JS callback.
    // Flat languages don't pass a callback (they expose handleHolochainSignal
    // and rely on the globalThis.__handleHolochainSignal__ bridge installed
    // by initLanguage); legacy factory languages pass a callback that goes
    // into the JS-side dispatch map.
    for (const roleName of Object.keys(appInfo.cell_info)) {
        const cellInfos = appInfo.cell_info[roleName];
        for (const cellInfo of cellInfos) {
            // cellInfo is an enum wrapper: { provisioned: {...} } | { cloned: {...} } | { stem: {...} }
            const inner = cellInfo.provisioned || cellInfo.cloned || cellInfo.stem || cellInfo.value;
            if (!inner || !inner.cell_id) continue;
            const key = cellIdKey(inner.cell_id);
            if (signalCallback) {
                globalThis.__holochainSignalCallbacks__.set(key, signalCallback);
            }
            // Notify Rust so the central signal loop can route to this language
            LANGUAGE_CONTROLLER.registerHolochainSignalHandler(key, languageAddress);
        }
    }
}

/**
 * Recursively convert {__binary: [...]} markers (produced by Rust's
 * msgpack_value_to_json for Binary values) back into Uint8Array so that
 * language bundles receive the same types as from zome call responses.
 */
function convertBinaryMarkers(val) {
    if (val === null || val === undefined) return val;
    if (Array.isArray(val)) {
        return val.map(convertBinaryMarkers);
    }
    if (typeof val === 'object') {
        const keys = Object.keys(val);
        if (keys.length === 1 && keys[0] === '__binary' && Array.isArray(val.__binary)) {
            return new Uint8Array(val.__binary);
        }
        const result = {};
        for (const key of keys) {
            result[key] = convertBinaryMarkers(val[key]);
        }
        return result;
    }
    return val;
}

/**
 * Handle a Holochain signal dispatched from Rust.
 * Rust calls this with the decoded signal object: { cell_id, zome_name, payload }.
 * We match cell_id against registered callbacks and invoke the right one.
 */
globalThis.__handleHolochainSignal__ = async function(signal) {
    if (!signal || !signal.cell_id) return;
    // Convert __binary markers in the payload to Uint8Array, matching
    // the conversion that callZomeFunction already does for call responses.
    if (signal.payload) {
        signal.payload = convertBinaryMarkers(signal.payload);
    }
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
                } else if (dna.bundle) {
                    // Rust ALDK DnaSpec: { nick: string, bundle: Vec<u8> }
                    // serializes to { nick, bundle: Uint8Array | number[] }
                    source = { type: "bytes", value: new Uint8Array(dna.bundle) };
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

    // Build an agent proxy that delegates to the global AGENT ops.
    // `did` and `signingKeyId` are getters so they reflect the current
    // thread-local AgentContext (which may be a managed user's context
    // when expression_create runs with a user agent context).
    const agentProxy = {
        get did() { return AGENT.did(); },
        get signingKeyId() { return AGENT.signingKeyId(); },
        createSignedExpression: (data) => AGENT.createSignedExpression(data),
        sign: (payload) => AGENT.sign(payload),
        signStringHex: (payload) => AGENT.signStringHex(payload),
        getAllLocalUserDIDs: () => AGENT.getAllLocalUserDIDs(),
        createSignedExpressionForUser: (userEmail, data) => AGENT.createSignedExpressionForUser(userEmail, data),
        didForUser: (userEmail) => AGENT.didForUser(userEmail),
    };

    const mod = globalThis.languageModule;

    // Set globals for non-serializable delegates (WASM languages access these via globalThis)
    globalThis.__holochainDelegate__ = holochainDelegate;
    globalThis.__ad4mSignal__ = ad4mSignal;
    globalThis.__agentProxy__ = agentProxy;

    // init() takes NO arguments — context is accessed via the host
    // import functions installed above. The language calls
    // languageStorageDirectory(), languageAddress(), languageSettings()
    // to get its storage dir, address, and settings from the runtime.
    await mod.init();

    // Build language instance from the module's named exports.
    //
    // Explicitly assign each top-level flat export onto the `language`
    // object. `Object.assign({}, mod)` on a module namespace exotic
    // object is observably flaky in Deno — the spread succeeds without
    // error but cross-peer p-diff-sync stops propagating diffs, which
    // points at a failed live-binding lookup on the namespace. Walking
    // `mod` with a plain `in` check avoids the spread path entirely.
    //
    // Name/version are normalized because languages may expose them as
    // plain string constants (the JS authoring style) or as zero-arg
    // accessor functions (the Rust ALDK style — wasm-bindgen cannot
    // emit string constants).
    const readMaybeFn = (v, fallback) => {
        if (typeof v === "function") return v();
        return v ?? fallback;
    };
    const language = {};
    const flatExportNames = [
        // Lifecycle
        "init", "teardown", "interactions", "isPublic",
        // Expression capability
        "expressionGet", "expressionCreate", "expressionAddressOf",
        "isImmutableExpression", "expressionIcon", "expressionConstructorIcon",
        "expressionInteract",
        // Perspective-commit
        "perspectiveCommit",
        // Perspective-sync
        "perspectiveSyncSync", "perspectiveSyncRender", "perspectiveSyncCurrentRevision",
        // Perspective-query
        "perspectiveQuerySupportedKinds", "perspectiveQueryRun",
        // Peers
        "peersSetLocal", "peersRemote",
        // Telepresence
        "telepresenceSetOnlineStatus", "telepresenceGetOnlineAgents",
        "telepresenceSendSignal", "telepresenceSendBroadcast",
        "telepresenceRegisterSignalCallback",
        // Legacy callback-setter shape — still used by current bootstrap
        // languages (p-diff-sync, centralized-p-diff-sync) until they are
        // ported to call emitPerspectiveDiff() directly. The runtime
        // wires LANGUAGE_CONTROLLER sinks into these in
        // register_callbacks().
        "linkSyncAddCallback", "linkSyncRemoveCallback",
        "linkSyncAddSyncStateChangeCallback",
        // Other optional adapters
        "languageGetSource", "getByAuthor", "getAll", "settingsIcon",
        // Holochain signal routing — also installed on globalThis below
        "handleHolochainSignal",
    ];
    for (const key of flatExportNames) {
        const v = mod[key];
        if (v !== undefined) {
            language[key] = v;
        }
    }
    language.name = readMaybeFn(mod.name, "unknown");
    language.version = readMaybeFn(mod.version, undefined);

    // Holochain signal bridge — `rust-executor/src/lib.rs` dispatches
    // signals by evaluating `await globalThis.__handleHolochainSignal__(args)`
    // in the language's isolate scope. Install the bridge here so
    // languages declaring a `handleHolochainSignal` export actually
    // receive signals. The bridge accepts the runtime's
    // `{ cell_id, zome_name, payload }` shape and forwards it.
    if (typeof mod.handleHolochainSignal === "function") {
        language.handleHolochainSignal = mod.handleHolochainSignal;
        globalThis.__handleHolochainSignal__ = async (signal) => {
            // Convert __binary markers to Uint8Array, same as the default
            // signal handler does above. Without this, languages receive
            // raw {__binary:[...]} objects instead of Uint8Array for
            // Holochain hashes and binary data.
            if (signal && signal.payload) {
                signal.payload = convertBinaryMarkers(signal.payload);
            }
            return await mod.handleHolochainSignal(signal);
        };
    }

    // Teardown — wrap the language's own teardown (if any) in an async
    // function so the runtime can always `await language.teardown()`.
    if (mod.teardown) {
        const originalTeardown = mod.teardown;
        language.teardown = async () => {
            await originalTeardown();
        };
    } else {
        language.teardown = async () => {};
    }

    globalThis.__ad4m_language_instance__ = language;
    globalThis.language = language;
    return language;
}

globalThis.initLanguage = initLanguage;
globalThis.createHolochainDelegate = createHolochainDelegate;
globalThis.createAd4mSignal = createAd4mSignal;
