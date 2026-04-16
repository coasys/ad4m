/**
 * Typed re-exports of the AD4M host import surface.
 *
 * At runtime, `ad4m:host` is a synthetic Deno module registered by
 * the executor's StringModuleLoader. Language bundles (both JS and
 * Rust/WASM) import from it as a standard ES module. esbuild marks
 * `ad4m:host` as external so the import survives bundling.
 *
 * This file re-exports everything so that `@coasys/ad4m-ldk` consumers
 * can write `import { agentDid } from "@coasys/ad4m-ldk"` and get
 * full type safety.
 *
 * Spec section 7 (`docs-src/language-interface-spec.md`).
 */

// Re-export everything from the host module.
// The ambient declaration in `host.d.ts` provides types for tsc;
// the actual module is resolved at runtime by the executor.
export {
    // Agent (Spec section 7.1)
    agentDid,
    agentSigningKeyId,
    agentSign,
    agentSignStringHex,
    agentCreateSignedExpression,
    agentGetAllLocalUserDids,
    agentCreateSignedExpressionForUser,
    agentDidForUser,

    // Holochain (Spec section 7.2)
    holochainRegisterDnas,
    holochainCall,
    holochainCallAsync,

    // HTTP fetch (Spec section 7.2b)
    httpFetch,

    // Language context (Spec section 7.3)
    languageStorageDirectory,
    languageAddress,
    languageSettings,

    // Event emission (Spec section 7.5)
    emitPerspectiveDiff,
    emitSyncStateChange,
    emitTelepresenceSignal,
    emitSignal,

    // Storage KV -- CORE (Spec section 7.4)
    storageGet,
    storagePut,
    storageDelete,
    storageListKeys,

    // Storage File I/O -- OPTIONAL EXTENSION (Spec section 7.6)
    // Throws "[ad4m:host] Storage File I/O extension is not installed ..."
    // on runtimes that don't install the extension. Prefer the KV API
    // above unless you specifically need filesystem-like semantics.
    readStorageFile,
    writeStorageFile,
} from "ad4m:host";
