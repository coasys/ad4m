// Kept in the emitted lib/imports.d.ts, so a consumer of the published
// package gets the `ad4m:host` declaration (shipped in src/) too.
/// <reference path="./host.d.ts" preserve="true" />

/**
 * Typed wrappers around the AD4M host import surface.
 *
 * At runtime, `ad4m:host` is a synthetic Deno module registered by
 * the executor's StringModuleLoader. Language bundles (both JS and
 * Rust/WASM) import from it as a standard ES module. esbuild marks
 * `ad4m:host` as external so the import survives bundling.
 *
 * This file exports one wrapper per host function so that
 * `@coasys/ad4m-ldk` consumers can write
 * `import { agentDid } from "@coasys/ad4m-ldk"` and get full type safety.
 *
 * The host module is imported as a namespace and each function is looked up
 * when it is called, not when the bundle is linked. A named import of a
 * function the executor does not export fails the whole module at load time
 * ("does not provide an export named ..."), so a Language built with this
 * LDK would not load on any executor older than the newest host function,
 * even if it never calls that function. With the lookup at call time the
 * Language loads, and only a call to a missing function throws
 * `HostFunctionMissing`.
 *
 * Spec section 7 (`docs-src/language-interface-spec.md`).
 */

import * as host from "ad4m:host";

type Host = typeof host;
type HostFunctionName = {
    [K in keyof Host]: Host[K] extends (...args: any[]) => any ? K : never;
}[keyof Host];

/** Thrown when a Language calls a host function its executor does not provide. */
export class HostFunctionMissing extends Error {
    functionName: string;
    constructor(functionName: string) {
        super(
            `[ad4m:host] this executor does not provide \`${functionName}\`; ` +
                `it needs a newer AD4M executor`,
        );
        this.name = "HostFunctionMissing";
        this.functionName = functionName;
    }
}

function bind<K extends HostFunctionName>(name: K): Host[K] {
    const call = (...args: unknown[]) => {
        const fn = (host as Record<string, unknown>)[name];
        if (typeof fn !== "function") throw new HostFunctionMissing(name);
        return fn(...args);
    };
    return call as Host[K];
}

// Agent (Spec section 7.1)
export const agentDid = bind("agentDid");
export const agentSigningKeyId = bind("agentSigningKeyId");
export const agentSign = bind("agentSign");
export const agentSignStringHex = bind("agentSignStringHex");
export const agentOpenSealed = bind("agentOpenSealed");
export const agentCreateSignedExpression = bind("agentCreateSignedExpression");
export const agentGetAllLocalUserDids = bind("agentGetAllLocalUserDids");
export const agentCreateSignedExpressionForUser = bind("agentCreateSignedExpressionForUser");
export const agentDidForUser = bind("agentDidForUser");

// Holochain (Spec section 7.2)
export const holochainRegisterDnas = bind("holochainRegisterDnas");
export const holochainCall = bind("holochainCall");
export const holochainCallAsync = bind("holochainCallAsync");

// HTTP fetch (Spec section 7.2b)
export const httpFetch = bind("httpFetch");

// Runtime utilities (Spec section 7.7)
export const hash = bind("hash");

// Language context (Spec section 7.3)
export const languageStorageDirectory = bind("languageStorageDirectory");
export const languageAddress = bind("languageAddress");
export const languageSettings = bind("languageSettings");

// Event emission (Spec section 7.5)
export const emitPerspectiveDiff = bind("emitPerspectiveDiff");
export const emitSyncStateChange = bind("emitSyncStateChange");
export const emitTelepresenceSignal = bind("emitTelepresenceSignal");
export const emitSignal = bind("emitSignal");

// Storage KV -- CORE (Spec section 7.4)
export const storageGet = bind("storageGet");
export const storagePut = bind("storagePut");
export const storageDelete = bind("storageDelete");
export const storageListKeys = bind("storageListKeys");

// Storage File I/O -- OPTIONAL EXTENSION (Spec section 7.6)
// Throws "[ad4m:host] Storage File I/O extension is not installed ..."
// on runtimes that don't install the extension. Prefer the KV API
// above unless you specifically need filesystem-like semantics.
export const readStorageFile = bind("readStorageFile");
export const writeStorageFile = bind("writeStorageFile");
