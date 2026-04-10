// Language Bootstrap JS
// Runs inside every per-language Deno runtime.
// Bridges raw Deno ops (from registered extensions) and the LanguageContext
// shape that language bundles expect.

// Polyfill Node.js Buffer on globalThis – many language bundles depend on it.
import { Buffer } from "node:buffer";
import { setupFlatWasmImports, teardownFlatWasmImports } from "./flat_wasm_imports.ts";
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

    const fullContext = {
        agent: agentProxy,
        customSettings: context.customSettings,
        storageDirectory: context.storageDirectory,
        Holochain: holochainDelegate,
        ad4mSignal: ad4mSignal,
    };

    let language;

    if (globalThis.__language_pattern__ === "flat") {
        const mod = globalThis.languageModule;

        // Set globals for non-serializable delegates (WASM languages access these via globalThis)
        globalThis.__holochainDelegate__ = holochainDelegate;
        globalThis.__ad4mSignal__ = ad4mSignal;
        globalThis.__agentProxy__ = agentProxy;

        // Set up flat WASM import functions on globalThis (needed before init())
        // These provide: languageStorageDirectory(), languageAddress(), languageSettings(),
        // plus agentDid(), agentSign(), holochainCall(), signalEmit(), etc.
        setupFlatWasmImports();

        // NEW INTERFACE: init() takes NO arguments — context is accessed via flat import functions
        // The language calls languageStorageDirectory(), languageAddress(), languageSettings()
        // to get its storage dir, address, and settings from the runtime.
        await mod.init();

        // Build language instance from flat exports.
        //
        // Languages may expose name/version either as plain string constants
        // (the JS authoring style: `export const name = "..."`) or as
        // zero-arg accessor functions (the Rust ALDK style: wasm-bindgen
        // emits `export function name(): string` because WASM exports cannot
        // be string constants). Normalize both shapes to a string here so
        // the rest of the runtime sees a uniform `language.name` field.
        const readMaybeFn = (v, fallback) => {
            if (typeof v === "function") return v();
            return v ?? fallback;
        };
        language = {
            name: readMaybeFn(mod.name, "unknown"),
            version: readMaybeFn(mod.version, undefined),
        };

        // Map adapter exports to adapter slots.
        //
        // ReadOnlyLanguage style: a language may expose only
        // `expressionAddressOf` (no `expressionCreate`). The runtime's
        // createExpression dispatcher inspects
        // `putAdapter.createPublic` first and falls back to
        // `putAdapter.addressOf` — BUT it reads `putAdapter.createPublic`
        // unconditionally, so `putAdapter` MUST exist even in the
        // addressOf-only case. The previous guard only built `putAdapter`
        // when `expressionCreate` was present, leaving it undefined for
        // read-only languages and crashing the dispatcher with
        // "cannot read properties of undefined".
        if (mod.expressionCreate || mod.expressionGet || mod.expressionAddressOf || mod.addressOf) {
            // Support both expressionAddressOf (design name) and addressOf (TypeScript name)
            const addressOf = mod.expressionAddressOf || mod.addressOf;
            let putAdapter;
            if (mod.expressionCreate) {
                putAdapter = { createPublic: mod.expressionCreate };
                if (addressOf) putAdapter.addressOf = addressOf;
            } else if (addressOf) {
                // ReadOnlyLanguage — only addressOf. The dispatcher's
                // `createPublic ? ... : addressOf(...)` ternary needs
                // `createPublic` to be a readable (falsy) value, so
                // leave it undefined on an otherwise-populated object.
                putAdapter = { addressOf };
            }
            language.expressionAdapter = {
                putAdapter,
                get: mod.expressionGet,
            };
        }

        // Map perspective-commit / perspective-sync / peers exports to the
        // linksAdapter slot the Rust-side LanguageController consumes. Spec
        // §5.2 splits link-sync into three independent capabilities
        // (commit / sync / peers); we collapse them back into the single
        // linksAdapter shape here because the Rust consumer has not been
        // restructured around the three-way split yet.
        //
        // Legacy linkSync* export names are still accepted as a fallback to
        // ease bootstrap-language migration; new code should emit perspective*
        // / peers* names via `defineLanguage`.
        const syncFn = mod.perspectiveSyncSync || mod.linkSyncSync;
        const commitFn = mod.perspectiveCommit || mod.linkSyncCommit;
        const renderFn = mod.perspectiveSyncRender || mod.linkSyncRender;
        const currentRevisionFn = mod.perspectiveSyncCurrentRevision || mod.linkSyncCurrentRevision;
        const peersRemoteFn = mod.peersRemote || mod.linkSyncOthers;
        const peersSetLocalFn = mod.peersSetLocal || mod.linkSyncSetLocalAgents;

        if (syncFn || commitFn) {
            // Spec §5.2 splits the legacy single `commit()` (which returned
            // the new revision string) into two independent capabilities:
            // `perspective-commit` (write a diff, return nothing) and
            // `perspective-sync` (read current revision). Rust ALDK and any
            // new flat language therefore returns `undefined` from
            // `perspectiveCommit`. The Rust-side `PerspectiveInstance::commit`
            // path now treats `Ok(None)` as the normal success signal (no
            // revision to report), so we pass the raw commit function
            // through without the earlier "poll currentRevision after
            // commit" bridge. Pre-fix, that bridge was the only thing
            // keeping commit-only flat languages out of a perpetual
            // pending-diff retry loop; post-fix, it was unnecessary glue
            // that could hide real commit failures behind a stale
            // revision read.
            language.linksAdapter = {
                sync: syncFn,
                commit: commitFn,
                render: renderFn,
                currentRevision: currentRevisionFn,
                others: peersRemoteFn,
                // writable / public are legacy hints. `public` collapses into
                // the lifecycle-level isPublic() (set below); `writable` is
                // gone — the runtime infers writability from the presence of
                // a perspectiveCommit export.
                writable: mod.linkSyncWritable,
                public: mod.linkSyncPublic || (mod.isPublic ? mod.isPublic : undefined),
                addCallback: mod.linkSyncAddCallback,
                removeCallback: mod.linkSyncRemoveCallback,
                addSyncStateChangeCallback: mod.linkSyncAddSyncStateChangeCallback,
                setLocalAgents: peersSetLocalFn,
            };
        }

        // Lifecycle-level isPublic hint (spec §5). Attached to the language
        // object so the Rust-side LanguageController can consult it.
        if (typeof mod.isPublic === "function") {
            language.isPublic = mod.isPublic;
        }

        // Perspective-query capability (spec §5.2). Raw exports are stored
        // on the language object; the Rust-side query path reads them here.
        if (typeof mod.perspectiveQueryRun === "function") {
            language.perspectiveQueryAdapter = {
                supportedKinds: mod.perspectiveQuerySupportedKinds,
                run: mod.perspectiveQueryRun,
            };
        }

        // Map Telepresence functions. Spec §5 marks every telepresence
        // method as optional, so detect on ANY telepresence* export rather
        // than gating on setOnlineStatus alone — a language that only
        // implements sendSignal/sendBroadcast still has telepresence.
        if (
            mod.telepresenceSetOnlineStatus ||
            mod.telepresenceGetOnlineAgents ||
            mod.telepresenceSendSignal ||
            mod.telepresenceSendBroadcast ||
            mod.telepresenceRegisterSignalCallback
        ) {
            language.telepresenceAdapter = {
                setOnlineStatus: mod.telepresenceSetOnlineStatus,
                getOnlineAgents: mod.telepresenceGetOnlineAgents,
                sendSignal: mod.telepresenceSendSignal,
                sendBroadcast: mod.telepresenceSendBroadcast,
                registerSignalCallback: mod.telepresenceRegisterSignalCallback,
            };
        }

        // Map DirectMessage functions
        if (mod.directMessageRecipient) {
            language.directMessageAdapter = {
                recipient: mod.directMessageRecipient,
                status: mod.directMessageStatus,
                sendP2P: mod.directMessageSendP2P,
                sendInbox: mod.directMessageSendInbox,
                setStatus: mod.directMessageSetStatus,
                inbox: mod.directMessageInbox,
                addMessageCallback: mod.directMessageAddMessageCallback,
            };
        }

        // Other optional adapters
        if (mod.languageGetSource) {
            language.languageAdapter = { getLanguageSource: mod.languageGetSource };
        }
        if (mod.getByAuthor) {
            language.getByAuthorAdapter = { getByAuthor: mod.getByAuthor };
        }
        if (mod.getAll) {
            language.getAllAdapter = { getAll: mod.getAll };
        }
        if (mod.expressionIcon) {
            language.expressionUI = {
                icon: mod.expressionIcon,
                constructorIcon: mod.expressionConstructorIcon,
            };
        }
        if (mod.settingsIcon) {
            language.settingsUI = { settingsIcon: mod.settingsIcon };
        }
        if (mod.isImmutableExpression) {
            language.isImmutableExpression = mod.isImmutableExpression;
        }

        // Interactions
        if (mod.interactions) {
            language.interactions = mod.interactions;
        }
        // expression_interact fallback path (spec §5.7). Rust ALDK
        // languages cannot attach callable `execute` functions onto
        // interaction descriptors because the descriptor list crosses
        // the wasm-bindgen boundary as plain JSON — callable members
        // are lost on the way across. They therefore export a top-level
        // `expressionInteract(address, name, parametersJson)` function
        // that the dispatcher in languages/mod.rs falls back to when
        // `interaction.execute` is absent. Without this mapping, the
        // dispatcher reads `language.expressionInteract` as undefined
        // and throws "Interaction X is not executable" for every Rust
        // ALDK interaction, even when the language correctly implements
        // the export.
        if (typeof mod.expressionInteract === "function") {
            language.expressionInteract = mod.expressionInteract;
        }

        // Holochain signal bridge — `rust-executor/src/lib.rs` dispatches
        // signals by evaluating `await globalThis.__handleHolochainSignal__(args)`
        // in the language's isolate scope. Install the bridge here so flat
        // languages declaring a `handleHolochainSignal` export actually
        // receive signals. The bridge accepts the runtime's
        // `{ cell_id, zome_name, payload }` shape and forwards it.
        if (typeof mod.handleHolochainSignal === "function") {
            language.handleHolochainSignal = mod.handleHolochainSignal;
            globalThis.__handleHolochainSignal__ = async (signal) => {
                return await mod.handleHolochainSignal(signal);
            };
        }
        // Teardown — order matters: run the language's own teardown FIRST
        // (while host imports like agentDid/storagePut/emitSignal are still
        // installed on globalThis), THEN tear the imports down. The previous
        // order crashed any language whose teardown logged via emit_signal
        // or persisted final state via storage_put, because those globals
        // had already been deleted.
        if (mod.teardown) {
            const originalTeardown = mod.teardown;
            language.teardown = async () => {
                try {
                    await originalTeardown();
                } finally {
                    teardownFlatWasmImports();
                }
            };
        } else {
            // No language-level teardown — still need to clean up imports.
            language.teardown = async () => { teardownFlatWasmImports(); };
        }

    } else {
        // Legacy: create() factory pattern
        language = await globalThis.languageConstructor(fullContext);
    }

    globalThis.__ad4m_language_instance__ = language;
    globalThis.language = language;
    return language;
}

globalThis.initLanguage = initLanguage;
globalThis.createHolochainDelegate = createHolochainDelegate;
globalThis.createAd4mSignal = createAd4mSignal;
