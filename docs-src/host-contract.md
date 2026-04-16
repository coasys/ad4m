# AD4M Language Host Contract

This document specifies the runtime globals that a host environment must
provide before any AD4M Language can be loaded. The `ad4m:host` module
(`rust-executor/src/js_core/host.js`) delegates all platform-specific
work to these globals, making it fully runtime-agnostic.

The contract is split into:

- **Core** — two globals every host must install (`AGENT`,
  `LANGUAGE_CONTROLLER`). Covers agent identity, language context,
  events, and the KV store. A language using only core functions runs
  on any compliant runtime.
- **Extensions** — additional globals a host *may* install to expose
  capabilities beyond the core. Each extension has its own opt-in
  contract; languages that use an extension must tolerate runtimes
  that don't provide it (the `ad4m:host` wrappers throw clear errors
  on call). Current extensions:
  - Holochain (`__holochainDelegate__`)
  - Storage File I/O (methods on `LANGUAGE_CONTROLLER`)

## Overview

```
Language bundle (JS or WASM)
    |  import { agentDid, holochainCall, ... } from "ad4m:host"
    v
host.js  (runtime-agnostic, no platform APIs)
    |  accesses core globals and optional extensions
    v
+------------------+  +----------------------+   optional:
|  globalThis.     |  |  globalThis.         |  +----------------------+
|  AGENT           |  |  LANGUAGE_CONTROLLER |  |  globalThis.         |
|  (core)          |  |  (core + extensions) |  |  __holochainDelegate__|
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

### Notes

- The methods above are **all pure data passing** — no I/O. Any
  filesystem-like operations belong to the optional Storage File I/O
  extension (see below), not the core contract.
- The KV store (`storageGet` / `storagePut` / etc. on `ad4m:host`) is
  core. In the reference implementation it *reuses* the File I/O
  extension methods for persistence when available, but degrades
  gracefully to in-memory-only if the extension is not installed —
  so a runtime can ship a working (non-persistent) KV with no
  filesystem at all.

# Optional extensions

Extensions are capabilities a runtime *may* provide beyond the core
contract. A language that uses an extension must be written knowing
the runtime might not implement it: the `ad4m:host` wrappers throw a
clear error at call time if the extension is missing. Runtimes that
don't implement an extension simply omit installing its global (for
Holochain) or its methods (for File I/O on `LANGUAGE_CONTROLLER`).

## Extension: Holochain (`globalThis.__holochainDelegate__`)

Provides Holochain DNA management and zome function calls. Unlike the
core globals, this is installed **per-language** by the runtime's
bootstrap process, just before calling the language's `init()`.

Languages that don't use Holochain never call these methods, so a
runtime without Holochain support can simply not install this global.
The `ad4m:host` module throws a descriptive error if a language tries
to call a Holochain import without the delegate present.

**Reference implementation:** `rust-executor/src/js_core/language_bootstrap.js`
(the `createHolochainDelegate()` function)

### Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `registerDNAs(dnas, signalCallback)` | `(object[], any) => Promise<object[]>` | Installs Holochain DNAs and returns the resulting app info. `dnas` is an array of `{ path, nick }` objects (or `{ bundle, nick }` for inline bundles). `signalCallback` is currently unused (pass `undefined`). |
| `call(dnaNick, zome, fnName, params)` | `(string, string, string, any) => Promise<any>` | Calls a Holochain zome function. Returns the deserialized result. |
| `callAsync(calls, callback)` | `(object[], any) => Promise<any>` | Batch zome call. `calls` is an array of `{ dnaNick, zomeName, fnName, params }`. Returns array of results. |

### `ad4m:host` exports (call-side)

- `holochainRegisterDnas(dnas)`
- `holochainCall(dnaNick, zome, fnName, params)`
- `holochainCallAsync(dnaNick, zome, fnName, params)`

### Notes

- All methods return Promises because Holochain operations are
  inherently asynchronous.
- A browser runtime could implement this as a WebSocket bridge to a
  remote Holochain conductor.
- If Holochain is not available, simply don't install the global.
  Languages that require it will fail with a clear error at `init()` time.

## Extension: Storage File I/O (methods on `LANGUAGE_CONTROLLER`)

Provides raw read/write access to a filesystem-like storage layer, at
paths chosen by the language. Unlike the Holochain extension this
attaches directly to the `LANGUAGE_CONTROLLER` global — it has no
per-language lifecycle, so a runtime that supports File I/O installs
the two methods once, alongside the core `LANGUAGE_CONTROLLER` setup.

This extension exists for languages that need storage semantics the
KV API can't express:
- Custom storage layouts (one file per expression, nested dirs, …)
- Large blobs where the KV's full-rewrite-on-put model is a problem
- Shared paths outside the per-language storage scope (e.g. test
  fixtures storing language bundles in a directory that must survive
  across agents)

**The KV store is core and always works**; this extension is only for
languages that explicitly opt into filesystem-like semantics.

### Methods (on `LANGUAGE_CONTROLLER`)

| Method | Signature | Description |
|--------|-----------|-------------|
| `readStorageFile(path)` | `(string) => string` | Reads a file/blob at `path` as a UTF-8 string. Throws if not found (error message should contain "NotFound" or "No such file" for the KV layer to handle gracefully). |
| `writeStorageFile(path, content)` | `(string, string) => void` | Writes a file/blob at `path` with the given UTF-8 content. Should create parent directories as needed. |

### `ad4m:host` exports (call-side)

- `readStorageFile(path)`
- `writeStorageFile(path, content)`

Both throw `"[ad4m:host] Storage File I/O extension is not installed …"`
on runtimes that don't install the methods.

### Notes

- Deno executor: `Deno.readTextFileSync` / `Deno.writeTextFileSync`
  (sandboxed by Deno's filesystem permission allow-list).
- Browser runtime: could wrap IndexedDB with a synthetic path scheme
  (e.g. `idb:/lang-x/bundle-y.js`), use the Origin Private File
  System, or route to an HTTP API. "Path" is opaque to the contract.
- The paths passed in must be reachable by the runtime's permission
  model. Languages that pick a path outside the language's allowed
  scope will get a permission error, not a contract violation.
- The KV store (`storageGet` / `storagePut` / etc.) is deliberately
  decoupled from this extension: it *uses* the same methods when
  available, but falls back to an in-memory-only mode when they are
  not, so a runtime can ship a functional KV without implementing
  File I/O at all.

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

1. Host installs `globalThis.AGENT` and `globalThis.LANGUAGE_CONTROLLER`
   with the core methods. If the host supports the File I/O extension,
   it also attaches `readStorageFile` / `writeStorageFile` to
   `LANGUAGE_CONTROLLER` at this step.
2. Host registers the `ad4m:host` module.
3. Host loads the language bundle (which imports from `ad4m:host`).
4. If the host supports the Holochain extension and the language
   declares it needs Holochain, host installs
   `globalThis.__holochainDelegate__`.
5. Host calls the language's `init()` function.
6. Language is ready for use.

## Minimal browser implementation sketch

```javascript
// 1. Install AGENT (backed by HTTP API to a remote executor)
globalThis.AGENT = {
    did: () => fetch("/api/agent/did").then(r => r.text()),
    // ... or use cached DID + Web Crypto for local signing
};

// 2. Install LANGUAGE_CONTROLLER (core methods)
globalThis.LANGUAGE_CONTROLLER = {
    languageStorageDirectory: () => "indexeddb://lang-storage/" + langAddr,
    languageAddress: () => langAddr,
    languageSettings: () => settingsJson,

    // OPTIONAL: File I/O extension. Omit these entirely if this runtime
    // doesn't support raw file I/O — the KV will work in-memory-only and
    // languages importing readStorageFile will throw a clear error on call.
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
