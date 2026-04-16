# AD4M Language Host Contract

This document specifies the runtime globals that a host environment must
provide before any AD4M Language can be loaded. The `ad4m:host` module
(`rust-executor/src/js_core/host.js`) delegates all platform-specific
work to these globals, making it fully runtime-agnostic.

A host that installs all three globals correctly can run AD4M Languages
in any JavaScript environment: Deno (the current executor), a browser,
Node.js, or an embedded engine.

## Overview

```
Language bundle (JS or WASM)
    |  import { agentDid, holochainCall, ... } from "ad4m:host"
    v
host.js  (runtime-agnostic, no platform APIs)
    |  accesses three globals on globalThis
    v
+------------------+  +----------------------+  +----------------------+
|  globalThis.     |  |  globalThis.         |  |  globalThis.         |
|  AGENT           |  |  LANGUAGE_CONTROLLER |  |  __holochainDelegate__|
+------------------+  +----------------------+  +----------------------+
    |                      |                         |
    v                      v                         v
Host-specific backend (Deno ops, HTTP API, Web Crypto, IndexedDB, ...)
```

## `globalThis.AGENT`

Provides agent identity and signing operations. Must be installed before
any language is loaded.

**Reference implementation:** `rust-executor/src/js_core/agent_extension.js`

### Required methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `did()` | `() => string` | Returns the agent's DID (e.g. `did:key:z6Mk...`). |
| `signingKeyId()` | `() => string` | Returns the signing key identifier. |
| `sign(payload)` | `(Uint8Array) => Uint8Array` | Signs raw bytes, returns the signature bytes. |
| `signStringHex(payload)` | `(string) => string` | Signs a string payload and returns the hex-encoded signature. |
| `createSignedExpression(data)` | `(any) => object` | Wraps `data` in an Expression envelope with `author`, `timestamp`, `data`, and `proof` fields. Returns the signed expression object. |
| `getAllLocalUserDIDs()` | `() => string[]` | Returns DIDs of all local managed users. |
| `createSignedExpressionForUser(email, data)` | `(string, any) => object` | Like `createSignedExpression` but signs with a specific managed user's key. |
| `didForUser(email)` | `(string) => string` | Returns the DID for a managed user identified by email. |

### Notes

- In the Deno executor, these delegate to `deno_core` ops backed by the
  Rust `AgentService`.
- A browser implementation could back these with Web Crypto API for
  signing and an HTTP/WebSocket API for DID resolution.
- The `createSignedExpression` return value must have the shape:
  ```json
  {
    "author": "did:key:...",
    "timestamp": "2024-01-01T00:00:00.000Z",
    "data": <the input data>,
    "proof": {
      "key": "did:key:...#...",
      "signature": "<hex string>"
    }
  }
  ```

## `globalThis.LANGUAGE_CONTROLLER`

Provides language context, event dispatch, and storage I/O. Must be
installed before any language is loaded.

**Reference implementation:** `rust-executor/src/js_core/languages_extension.js`

### Required methods

#### Language context (Spec section 7.3)

| Method | Signature | Description |
|--------|-----------|-------------|
| `languageStorageDirectory()` | `() => string` | Returns the filesystem path (or logical identifier) for this language's persistent storage. |
| `languageAddress()` | `() => string` | Returns the content-addressed hash that identifies this language. |
| `languageSettings()` | `() => string` | Returns a JSON string of language-specific settings, or empty string. |

#### Event dispatch (Spec section 7.5)

| Method | Signature | Description |
|--------|-----------|-------------|
| `perspectiveDiffReceived(diff, langAddr)` | `(any, string) => void` | Dispatches a perspective diff event to the runtime. |
| `syncStateChanged(state, langAddr)` | `(any, string) => void` | Notifies the runtime that sync state changed. |
| `telepresenceSignalReceived(signal, langAddr, recipientDid?)` | `(any, string, string?) => void` | Routes a telepresence signal to the runtime. |
| `ad4mSignalEmitted(signal, langAddr)` | `(any, string) => void` | Emits a generic signal to the runtime. |

#### Holochain signal routing

| Method | Signature | Description |
|--------|-----------|-------------|
| `registerHolochainSignalHandler(cellIdKey, langAddr)` | `(string, string) => void` | Registers this language as the handler for signals from a Holochain cell. |

#### Storage file I/O (used by KV persistence in `ad4m:host`)

| Method | Signature | Description |
|--------|-----------|-------------|
| `readStorageFile(path)` | `(string) => string` | Reads a file from the language's storage directory. Throws if not found (error message should contain "NotFound" or "No such file" for the KV layer to handle gracefully). |
| `writeStorageFile(path, content)` | `(string, string) => void` | Writes a file to the language's storage directory. |

### Notes

- `readStorageFile` / `writeStorageFile` are the **only** I/O methods in
  the entire host contract. All other methods are pure data passing.
- **Deno executor:** implements these via `Deno.readTextFileSync` /
  `Deno.writeTextFileSync`.
- **Browser runtime:** could implement via `localStorage`, `IndexedDB`,
  or an HTTP API to a remote storage backend.
- If storage I/O is unavailable, the `ad4m:host` KV layer degrades
  gracefully to in-memory-only storage.

## `globalThis.__holochainDelegate__`

Provides Holochain DNA management and zome function calls. Unlike the
other two globals, this is installed **per-language** by the runtime's
bootstrap process, just before calling the language's `init()`.

Languages that don't use Holochain never call these methods, so a
runtime without Holochain support can simply not install this global.
The `ad4m:host` module throws a descriptive error if a language tries
to call a Holochain import without the delegate present.

**Reference implementation:** `rust-executor/src/js_core/language_bootstrap.js`
(the `createHolochainDelegate()` function)

### Required methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `registerDNAs(dnas, signalCallback)` | `(object[], any) => Promise<object[]>` | Installs Holochain DNAs and returns the resulting app info. `dnas` is an array of `{ path, nick }` objects (or `{ bundle, nick }` for inline bundles). `signalCallback` is currently unused (pass `undefined`). |
| `call(dnaNick, zome, fnName, params)` | `(string, string, string, any) => Promise<any>` | Calls a Holochain zome function. Returns the deserialized result. |
| `callAsync(calls, callback)` | `(object[], any) => Promise<any>` | Batch zome call. `calls` is an array of `{ dnaNick, zomeName, fnName, params }`. Returns array of results. |

### Notes

- All methods return Promises because Holochain operations are
  inherently asynchronous.
- A browser runtime could implement this as a WebSocket bridge to a
  remote Holochain conductor.
- If Holochain is not available, simply don't install the global.
  Languages that require it will fail with a clear error at `init()` time.

## Module registration

The host must register the `ad4m:host` module so that
`import { ... } from "ad4m:host"` resolves at runtime.

- **Deno executor:** uses `StringModuleLoader` to register the module
  contents under the `"ad4m:host"` specifier
  (`rust-executor/src/js_core/options.rs`).
- **Browser runtime:** could use an import map
  (`{ "imports": { "ad4m:host": "./host.js" } }`) or a Service Worker
  that intercepts the import.
- **Node.js:** could use a custom loader hook or an import map via
  `--experimental-import-meta-resolve`.

## Initialization order

1. Host installs `globalThis.AGENT` and `globalThis.LANGUAGE_CONTROLLER`.
2. Host registers the `ad4m:host` module.
3. Host loads the language bundle (which imports from `ad4m:host`).
4. Host installs `globalThis.__holochainDelegate__` (if the language
   uses Holochain).
5. Host calls the language's `init()` function.
6. Language is ready for use.

## Minimal browser implementation sketch

```javascript
// 1. Install AGENT (backed by HTTP API to a remote executor)
globalThis.AGENT = {
    did: () => fetch("/api/agent/did").then(r => r.text()),
    // ... or use cached DID + Web Crypto for local signing
};

// 2. Install LANGUAGE_CONTROLLER
globalThis.LANGUAGE_CONTROLLER = {
    languageStorageDirectory: () => "indexeddb://lang-storage/" + langAddr,
    languageAddress: () => langAddr,
    languageSettings: () => settingsJson,

    // Storage via localStorage
    readStorageFile: (path) => {
        const data = localStorage.getItem("ad4m-storage:" + path);
        if (data === null) throw new Error("NotFound: " + path);
        return data;
    },
    writeStorageFile: (path, content) => {
        localStorage.setItem("ad4m-storage:" + path, content);
    },

    // Events via postMessage or EventTarget
    perspectiveDiffReceived: (diff, addr) => { /* dispatch */ },
    syncStateChanged: (state, addr) => { /* dispatch */ },
    telepresenceSignalReceived: (sig, addr, did) => { /* dispatch */ },
    ad4mSignalEmitted: (sig, addr) => { /* dispatch */ },
    registerHolochainSignalHandler: (cellId, addr) => { /* no-op or WebSocket */ },
};

// 3. Register the ad4m:host module (via import map or bundler alias)
// 4. Load language bundle
// 5. Optionally install __holochainDelegate__ via WebSocket to conductor
// 6. Call language.init()
```
