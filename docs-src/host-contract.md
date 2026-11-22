# AD4M Language Host Contract

This document specifies the runtime globals that a host environment must
provide before any AD4M Language can be loaded. The `ad4m:host` module
(`rust-executor/src/js_core/host.js`) delegates platform-specific
work to these globals and to platform builtins (`fetch` and friends),
making it fully runtime-agnostic. The TypeScript declaration of the
module is `ad4m-ldk/js/src/host.d.ts`.

The contract is split into:

- **Core** — three globals every host must install (`AGENT`,
  `LANGUAGE_CONTROLLER`, `UTILS`). Covers agent identity, language
  context, events, KV storage, and the canonical content-address hash
  function. A language using only core functions runs on any compliant
  runtime.
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
host.js  (runtime-agnostic; web-platform builtins only: fetch, URL, ...)
    |  accesses core globals and optional extensions
    v
+------------+  +----------------------+  +----------+   optional:
| globalThis.|  | globalThis.          |  |globalThis|  +----------------------+
| AGENT      |  | LANGUAGE_CONTROLLER  |  | .UTILS   |  |  globalThis.         |
| (core)     |  | (core + extensions)  |  | (core)   |  |  __holochainDelegate__|
+------------+  +----------------------+  +----------+  +----------------------+
    |                    |                      |                |
    v                    v                      v                v
Host-specific backend (Deno ops, HTTP API, Wasm SHA-256, OPFS sync handles, ...)
```

## The `ad4m:host` surface

`host.d.ts` declares the 27 functions a language can import. Each is
marked **SYNC** or **ASYNC**: SYNC resolves inside the isolate turn
(backed by a plain `#[op2]` op or by `host.js` itself), ASYNC returns a
Promise (backed by `#[op2(async)]` ops or by `fetch`).

| Function | Mode | Backed by |
|----------|------|-----------|
| **Agent / identity** | | |
| `agentDid()` | SYNC | `AGENT.did` |
| `agentSigningKeyId()` | SYNC | `AGENT.signingKeyId` |
| `agentGetAllLocalUserDids()` | SYNC | `AGENT.getAllLocalUserDIDs` |
| `agentDidForUser(email)` | SYNC | `AGENT.didForUser` |
| `agentCreateSignedExpressionForUser(email, data)` | SYNC | `AGENT.createSignedExpressionForUser` |
| **Signing** | | |
| `agentSign(payload)` | SYNC | `AGENT.sign` |
| `agentSignStringHex(payload)` | SYNC | `AGENT.signStringHex` |
| `agentCreateSignedExpression(data)` | SYNC | `AGENT.createSignedExpression` (with a stringified fallback op) |
| `agentOpenSealed(sealed)` | SYNC | `AGENT.openSealed` |
| **Language controller — context** | | |
| `languageAddress()` | SYNC | `LANGUAGE_CONTROLLER.languageAddress` |
| `languageStorageDirectory()` | SYNC | `LANGUAGE_CONTROLLER.languageStorageDirectory` (legacy) |
| `languageSettings()` | SYNC | `LANGUAGE_CONTROLLER.languageSettings` |
| **Language controller — events** | | |
| `emitPerspectiveDiff(diff)` | SYNC | `LANGUAGE_CONTROLLER.perspectiveDiffReceived` |
| `emitSyncStateChange(state)` | SYNC | `LANGUAGE_CONTROLLER.syncStateChanged` |
| `emitTelepresenceSignal(payload, recipientDid?)` | SYNC | `LANGUAGE_CONTROLLER.telepresenceSignalReceived` |
| `emitSignal(data)` | SYNC | `LANGUAGE_CONTROLLER.ad4mSignalEmitted` |
| **Language controller — KV storage** | | |
| `storageGet(key)` | SYNC | `host.js` in-isolate Map; persists through the File I/O extension |
| `storagePut(key, value)` | SYNC | same |
| `storageDelete(key)` | SYNC | same |
| `storageListKeys(prefix?)` | SYNC | same |
| **Utils / hash** | | |
| `hash(data)` | SYNC | `UTILS.hash` |
| **HTTP fetch** | | |
| `httpFetch(url, method, headersJson, body, maxBytes?)` | ASYNC | `host.js` over `globalThis.fetch`; 10 s timeout, 32 MiB reply cap |
| **File I/O extension** | | |
| `readStorageFile(path)` | SYNC | `LANGUAGE_CONTROLLER.readStorageFile` |
| `writeStorageFile(path, content)` | SYNC | `LANGUAGE_CONTROLLER.writeStorageFile` |
| **Holochain extension** | | |
| `holochainRegisterDnas(dnas)` | ASYNC | `__holochainDelegate__.registerDNAs` → `HOLOCHAIN_SERVICE` ops |
| `holochainCall(dnaNick, zome, fnName, params)` | ASYNC | `__holochainDelegate__.call` |
| `holochainCallAsync(dnaNick, zome, fnName, params)` | ASYNC | `__holochainDelegate__.callAsync` (single-call wrapper) |

## `globalThis.AGENT`

Provides agent identity and signing operations. Must be installed before
any language is loaded.

**Reference implementation:** `rust-executor/src/js_core/agent_extension.js`

### Required methods

All `AGENT` methods are SYNC in the reference implementation
(`#[op2]` ops in `agent_extension.rs`).

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `did()` | SYNC | `() => string` | Returns the agent's DID (e.g. `did:key:z6Mk...`). |
| `signingKeyId()` | SYNC | `() => string` | Returns the signing key identifier. |
| `sign(payload)` | SYNC | `(Uint8Array) => Uint8Array` | Signs raw bytes, returns the signature bytes. The reference actually returns a plain number array, not `Uint8Array` — see Notes. |
| `signStringHex(payload)` | SYNC | `(string) => string` | Signs a string payload and returns the hex-encoded signature. |
| `openSealed(sealed)` | SYNC | `(SealedBox) => string` | Opens a box sealed to the acting agent (the node, or the managed user the call runs for) and returns the plaintext as hex. Throws for a box sealed to any other DID, or an altered one. See "Sealed boxes" below. |
| `createSignedExpression(data)` | SYNC | `(any) => object` | Wraps `data` in an Expression envelope with `author`, `timestamp`, `data`, and `proof` fields. Returns the signed expression object. |
| `getAllLocalUserDIDs()` | SYNC | `() => string[]` | Returns the main agent's DID plus the DIDs of all local managed users. |
| `createSignedExpressionForUser(email, data)` | SYNC | `(string, any) => object` | Like `createSignedExpression` but signs with a managed user's key. Only the user that the current call runs for: any other email, and any call that runs for the main agent, throws. |
| `didForUser(email)` | SYNC | `(string) => string` | Returns the DID for a managed user identified by email. |

The Deno implementation installs six more methods on `AGENT` that are
not part of the contract — see "Globals outside the contract".

#### Sealed boxes

Anyone can seal to an agent from its DID alone, so a language seals itself;
only opening needs the host. The recipient's X25519 key is the Montgomery form
of the Ed25519 key in its `did:key` (RFC 7748 birational map, libsodium
`crypto_sign_ed25519_pk_to_curve25519`); the host opens with the X25519 form of
the agent's Ed25519 secret (`crypto_sign_ed25519_sk_to_curve25519`). A box is an
ephemeral X25519 key `ephemeralPublicKey`, the AES-256-GCM key
`HKDF-SHA256(ECDH, salt = ephemeralPublicKey || recipient X25519 key,
info = "ad4m:sealed-box:v1")`, a 12-byte `nonce` and the `ciphertext` with its
tag, all hex. The executor opens in `rust-executor/src/agent/sealed_box.rs`; the
server link language seals in `bootstrap-languages/server-link-language/src/encryption.ts`.

### Notes

- In the Deno executor, these delegate to `deno_core` ops backed by the
  Rust `AgentService`.
- `sign`'s declared return type is `Uint8Array`, but the Deno op
  serializes `Vec<u8>` (`agent_extension.rs:140-144`) and the JS
  forwards it unconverted (`agent_extension.js:47-49`, `host.js:82`),
  so the reference hands back a `number[]`. A host should match the
  declared shape; language code should tolerate either.
- A browser implementation must keep every method synchronous: carry
  the DID as a preloaded string (resolved during startup, before
  languages load — remote DID resolution cannot answer a SYNC call),
  and sign with a synchronous JS/Wasm implementation or a deliberately
  designed synchronous bridge — Web Crypto's `sign` returns a Promise.
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

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `languageStorageDirectory()` | SYNC | `() => string` | Returns the filesystem path (or logical identifier) for this language's persistent storage. Legacy: spec §7.3 marks it a removal candidate — new languages use the KV or the File I/O extension instead. |
| `languageAddress()` | SYNC | `() => string` | Returns the content-addressed hash that identifies this language. |
| `languageSettings()` | SYNC | `() => string` | Returns a JSON string of language-specific settings, or empty string. |

#### Event dispatch (Spec section 7.5)

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `perspectiveDiffReceived(diff, langAddr)` | SYNC | `(any, string) => void` | Dispatches a perspective diff event to the runtime. |
| `syncStateChanged(state, langAddr)` | SYNC | `(any, string) => void` | Notifies the runtime that sync state changed. |
| `telepresenceSignalReceived(signal, langAddr, recipientDid?)` | SYNC | `(any, string, string?) => void` | Routes a telepresence signal to the runtime. |
| `ad4mSignalEmitted(signal, langAddr)` | SYNC | `(any, string) => void` | Emits a generic signal to the runtime. |

#### Holochain signal routing

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `registerHolochainSignalHandler(cellIdKey, langAddr)` | SYNC | `(string, string) => void` | Registers this language as the handler for signals from a Holochain cell. |

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

## `globalThis.UTILS`

Runtime utilities. Must be installed before any language is loaded.
Currently a small surface; primarily the canonical content-address
hash function that every content-addressed Language uses to compute
Expression addresses.

**Reference implementation:** `rust-executor/src/js_core/utils_extension.js`

### Required methods

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `hash(data)` | SYNC | `(string) => string` | Canonical AD4M content-address hash. Computes **SHA-256 multihash → CIDv1 with codec `0x00` → base58btc multibase** (which starts with `z`), then prefixes the result with `"Qm"`, so addresses start with `Qmz`. The comment in `utils_extension.rs` says DAG-Protobuf, but DAG-Protobuf is codec `0x70`; the code uses `0x00`. Runtimes MUST implement this identically — two runtimes must produce the same address for the same content, or Expression URLs become non-portable. |

Legacy log helpers (`consoleLog`, `consoleDebug`, `consoleError`,
`consoleWarn`) also live on `UTILS` in the reference implementation
but are not part of the public contract; new Languages should use
`console.log` / `console.error` directly instead.

### Notes

- Exposed to Languages via `ad4m:host`'s `hash()` export (spec §7.7).
  Languages should import that rather than reaching for `UTILS.hash`
  directly — the global is an implementation detail.
- `hash()` is SYNC, so a browser runtime needs a synchronous SHA-256 —
  a JS/Wasm implementation plus a CIDv1 encoder; Web Crypto's `digest`
  returns a Promise and cannot back it. Node.js runtimes use `crypto` +
  `multiformats`. The output format (base58btc CIDv1 Qm-prefixed) is
  the contract, not the library.

# Core functions implemented inside `ad4m:host`

Two parts of the core surface are not backed by a contract global at
all: `host.js` implements them itself, on top of platform builtins and
the globals above. A host needs no extra globals for them, but must
provide the platform builtins they use.

## `httpFetch(url, method, headersJson, body, maxBytes?)` — ASYNC

Declared in `host.d.ts` under the label "Spec section 7.2b" (the
interface spec does not number it yet). `host.js` runs it over
`globalThis.fetch` (`host.js:199-259`):

- 10 s timeout (`AbortSignal.timeout`) covering headers and body
  together; the timeout error names the URL.
- The reply body is read as a stream and refused as soon as it passes
  32 MiB — or the caller's lower `maxBytes` — with an error whose
  `code` is `"too-large"`.
- Returns `{ status, body }` for every completed HTTP response,
  whatever the status. Rejects on network failure, timeout, the size
  cap — and on request/argument errors: `method`, `headers` and `url`
  are passed to `fetch` unvalidated (`host.js:211-216`), so a malformed
  URL, an invalid method or header, or a forbidden combination fails
  before any network access. A lowercase `get` with a nonempty body
  slips past the case-sensitive body guard (`:212`) and is rejected by
  `fetch` after method normalization. Timeout and other failures are
  plain `Error`s; only the size cap carries `code: "too-large"`. Error
  messages redact credentials and query strings.
- `headersJson` is a JSON object string; malformed JSON is silently
  treated as empty headers (`:202-208`), not rejected. `body` is a
  string sent only for non-GET/HEAD methods.

**What the host must provide:** a standard `fetch` (Deno, browsers and
Web Workers all have one) plus `AbortSignal.timeout`, `TextDecoder`,
`URL`. On a browser host the calls are normal cross-origin fetches, so
the target server's CORS policy decides what a language can reach —
see the central-backends table in the web-executor notes.

## Storage KV — `storageGet` / `storagePut` / `storageDelete` / `storageListKeys` — SYNC

Per-language key/value storage, always available (spec §7.4). `host.js`
keeps it in an in-isolate `Map` (`host.js:409-530`):

- Every key is namespaced `languageAddress() + "::" + key`, so two
  languages cannot see each other's values. `storageListKeys` strips
  the namespace before returning.
- Persistence is best-effort: the first storage call loads
  `<languageStorageDirectory>/ad4m-language-kv.json` through the File
  I/O extension, and a put/delete schedules one full-file rewrite at
  the end of the turn. If the extension is absent the KV silently runs
  in-memory-only — that is normal, not an error (`host.js:435-439`). A
  missing file is likewise normal. It logs a warning only when a load
  fails for another reason (`:450-459`) or when a flush fails
  (`:476-484`).
- `storageGet` returns `null` (not `undefined`) for a missing key.

A browser host that wants a persistent KV has to work within the SYNC
contract: IndexedDB is asynchronous and cannot back the SYNC File I/O
methods directly. The options are a preloaded in-memory file cache
filled during startup, with an explicit durability policy for when
cached writes are flushed back, or — inside a Worker — prepared OPFS
synchronous access handles for the served paths. With no extension at
all the KV still works for the isolate's lifetime.

# Optional extensions

Extensions are capabilities a runtime *may* provide beyond the core
contract. A language that uses an extension must be written knowing
the runtime might not implement it: the `ad4m:host` wrappers fail
clearly at call time if the extension is missing — thrown
synchronously by the plain-function wrappers, surfaced as a Promise
rejection by the `async` ones. Runtimes that
don't implement an extension simply omit installing its global (for
Holochain) or its methods (for File I/O on `LANGUAGE_CONTROLLER`).

## Extension: Holochain (`globalThis.__holochainDelegate__`)

Provides Holochain DNA management and zome function calls. Unlike the
core globals, this is installed **per-language** by the runtime's
bootstrap process, just before calling the language's `init()`.

Languages that don't use Holochain never call these methods, so a
runtime without Holochain support can simply not install this global.
Without the delegate, `holochainRegisterDnas` and `holochainCall`
throw a descriptive error synchronously — the missing-delegate check
runs before any Promise exists (`host.js:64-72,98-102`) — while the
`async` `holochainCallAsync` surfaces the same failure as a Promise
rejection (`:104`).

**Reference implementation:** `rust-executor/src/js_core/language_bootstrap.js`
(the `createHolochainDelegate()` function)

### Methods

All delegate methods are ASYNC: they sit on `#[op2(async)]`
`HOLOCHAIN_SERVICE` ops in the Deno executor.

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `registerDNAs(dnas, signalCallback)` | ASYNC | `(object[], any?) => Promise<object[]>` | Installs Holochain DNAs and returns the resulting app infos. Each `dna` is `{ nick, ... }` where the source may be `path` (file path), `file` or `bundle` (hApp bytes), or `source` (`{ path }` / `{ bundle }` / a `{ type, value }` tag); `network_seed` is an optional override. `signalCallback` is a legacy hook: when a caller passes one it is registered for every cell of the app (`language_bootstrap.js:103-126`) and fires through `__handleHolochainSignal__`. `ad4m:host` always passes `undefined` — contract languages get signals via `handleHolochainSignal` instead, so pass `undefined`. |
| `call(dnaNick, zome, fnName, params)` | ASYNC | `(string, string, string, any) => Promise<any>` | Calls a Holochain zome function. Returns the deserialized result. |
| `callAsync(calls, timeoutMs?)` | ASYNC | `(object[], number?) => Promise<any[]>` | Batch zome call. `calls` is an array of `{ dnaNick, zomeName, fnName, params }`. Returns the array of results; `timeoutMs` optionally rejects with `"callAsync timeout"`. |

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

| Method | Mode | Signature | Description |
|--------|------|-----------|-------------|
| `readStorageFile(path)` | SYNC | `(string) => string` | Reads a file/blob at `path` as a UTF-8 string. Throws if not found (error message should contain "NotFound" or "No such file" for the KV layer to handle gracefully). |
| `writeStorageFile(path, content)` | SYNC | `(string, string) => void` | Writes a file/blob at `path` with the given UTF-8 content. Creating parent directories as needed is a host recommendation; the Deno reference calls `Deno.writeTextFileSync` directly (`language_bootstrap.js:75-77`) and does not create them. |

### `ad4m:host` exports (call-side)

- `readStorageFile(path)`
- `writeStorageFile(path, content)`

Both throw `"[ad4m:host] Storage File I/O extension is not installed …"`
on runtimes that don't install the methods.

### The Deno executor's storage contract

The Deno executor implements both methods with its own storage calls,
not with Deno's file API, and seals what a language keeps:

- **Where.** A relative path is inside the language's storage folder
  (`languageStorageDirectory()`); an absolute path inside that folder is
  the same file. Sub-folders are made as needed. A path with `..`, the
  folder itself, and the executor's own files at the top of the folder
  (`bundle.js`, `meta.json`, `settings.json`) are refused.
- **Sealed.** Once the executor's storage is unlocked, every file a
  language writes in its folder is stored encrypted under a key the
  executor holds: the key of the neighbourhood's perspective for the
  neighbourhood's link language, the executor's own key for every other
  language. The file on disk starts with `AD4MSEAL`; its content is
  XChaCha20-Poly1305 ciphertext bound to the language's address, the
  file's path inside the folder and the key's store. A file copied into
  another language's folder, or renamed, does not open. A language reads
  and writes plain strings; the sealing is invisible to it.
- **Atomic.** A write replaces the whole file at once: a reader sees the
  old content or the new, never part of either.
- **Missing files.** Reading a file that does not exist throws an error
  whose message starts `NotFound: No such file`.
- **No direct file access.** A non-system language's isolate has no
  filesystem permission: `Deno.readTextFileSync`,
  `Deno.writeTextFileSync` and every other file API fail with a
  permission error, its own storage folder included. Use
  `readStorageFile` / `writeStorageFile` or the KV store.
- **System languages** (the bootstrap set) may also name absolute paths
  under the executor's working directory. Those files are written in the
  clear (the executor logs a warning once per language), and these
  languages keep Deno read/write permission on their storage folder and
  the working directory.
- **From older executors.** Plaintext files already in a storage folder
  are sealed in place when the executor starts. Until every one is
  sealed, a plaintext file a language reads is accepted and sealed on
  the spot; after that, a plaintext file in a storage folder is refused.
- **Deleting a neighbourhood** destroys its perspective's key, and with
  it the link language's sealed files.

### Notes

- These methods are SYNC, so browser storage APIs that return Promises
  cannot back them directly. IndexedDB is asynchronous; an HTTP wrapper
  would hand `JSON.parse` a Promise on load, and its unawaited writes
  could never report a persistence failure. A browser host needs a
  preloaded in-memory file cache with an explicit durability policy,
  or — inside a Worker — prepared OPFS synchronous access handles.
  "Path" is opaque to the contract.
- The paths passed in must be reachable by the runtime's permission
  model. Languages that pick a path outside the language's allowed
  scope will get a permission error, not a contract violation.
- The KV store (`storageGet` / `storagePut` / etc.) is deliberately
  decoupled from this extension: it *uses* the same methods when
  available, but falls back to an in-memory-only mode when they are
  not, so a runtime can ship a functional KV without implementing
  File I/O at all.

## Globals outside the contract

A compliant host needs only the globals above. The Deno executor
installs more — legacy surfaces kept for old bundles and bootstrap
plumbing — and the table also lists the platform builtins `host.js`
itself calls. None of these are additional direct `ad4m:host` exports.
The platform builtins are required on every runtime; the rest are
optional legacy globals that new languages should not use and a new
host does not have to install (except where noted).

| Global | Where installed | What it is |
|--------|-----------------|------------|
| `SIGNATURE` | `rust-executor/src/js_core/signature_extension.js:7` | `verifyStringSignedByDID`, `verify` — sync signature-check ops. Legacy; no `ad4m:host` export. |
| `PUBSUB` | `rust-executor/src/js_core/pubsub_extension.js:6` | `publish(topic, data)` — an async op that always fails: language code may not publish executor events (`pubsub_extension.rs`, #1371). Kept so the op table stays unchanged. |
| `RUNTIME_SERVICE` | `rust-executor/src/runtime_service/runtime_service_extension.js:6` | `friends`, `addMessageOutbox`, `getTrustedAgents` — sync `#[op2]` ops behind `async` JS wrappers. Legacy runtime surface. |
| `HOLOCHAIN_SERVICE` | `rust-executor/src/holochain_service/holochain_service_extension.js:53` | 14 raw conductor ops, all async: conductor lifecycle, `installApp`, `callZomeFunction`, agent infos, dna/happ pack tools. The contract's `__holochainDelegate__` is built on it by `createHolochainDelegate` (`language_bootstrap.js:179-281`); language code should never call it directly. |
| Extra `AGENT` methods | `rust-executor/src/js_core/agent_extension.js` | Sync ops beyond the contract's nine: `didDocument` (:13), `agent` (:56), `isInitialized` (:59), `isUnlocked` (:62), `agentForUser` (:90), `listUserEmails` (:102). Executor-internal surface, not exported by `ad4m:host`. |
| `__ad4mSignal__` | `rust-executor/src/js_core/language_bootstrap.js:287-291, 326` | Per-language signal emitter installed at `init()`; how WASM-era languages reach `ad4mSignalEmitted` without `ad4m:host`. |
| `__agentProxy__` | `rust-executor/src/js_core/language_bootstrap.js:310-320, 327` | LanguageContext-shaped agent object (`did`/`signingKeyId` getters plus the contract signing methods) for languages that expect the old context global. |
| `__handleHolochainSignal__`, `__holochainSignalCallbacks__` | `rust-executor/src/js_core/language_bootstrap.js:82, 157-173, 407-419` | Inbound Holochain signal dispatch: the executor calls `__handleHolochainSignal__` in the isolate; the map holds per-cell JS callbacks. Installed by the bootstrap inside the isolate, not by the host. |
| `languageModule`, `initLanguage`, `__ad4m_language_instance__`, `language` | `rust-executor/src/js_core/language_bootstrap.js:298, 322, 432-439` | The bootstrap's own plumbing: `languageModule` holds the imported bundle, `initLanguage` builds the instance and stores it for the runtime to call. |
| `Buffer` | `rust-executor/src/js_core/language_bootstrap.js:7-8` | `node:buffer` polyfill for bundles that call `Buffer.from`. A non-Deno host must supply it too if its languages need it. |
| DOM stubs (`HTMLElement`, `document`, `customElements`, `window`) | `rust-executor/src/js_core/language_bootstrap.js:14-59` | No-op shims so Svelte-compiled icon modules don't crash at import. A browser tab has real ones (harmless); a bare worker loading those bundles needs comparable stubs. |
| Platform builtins | used by `rust-executor/src/js_core/host.js` | Not installed by the executor — `fetch` (:216), `AbortSignal.timeout` (:210), `TextDecoder` (:180), `URL` (:187), `queueMicrotask` (:493), `setTimeout` (`language_bootstrap.js:274`). Deno, browsers and workers all provide them. |

## Module registration

The host must register the `ad4m:host` module so that
`import { ... } from "ad4m:host"` resolves at runtime.

- **Deno executor:** uses `StringModuleLoader` to register the module
  contents under the `"ad4m:host"` specifier
  (`rust-executor/src/js_core/options.rs`).
- **Browser runtime:** an import map
  (`{ "imports": { "ad4m:host": "./host.js" } }`) works in a tab, but
  import maps do not apply to imports inside Workers, and a Service
  Worker fetch interception cannot resolve a bare specifier —
  resolution happens before the request reaches it. The concrete
  approach is a bundler alias, a custom module loader, or rewriting
  the imports to a real path.
- **Node.js:** a custom loader hook. `--experimental-import-meta-resolve`
  does not supply import maps.

## Initialization order

1. Host installs `globalThis.AGENT`, `globalThis.LANGUAGE_CONTROLLER`,
   and `globalThis.UTILS` with the core methods. If the host supports
   the File I/O extension, it also attaches `readStorageFile` /
   `writeStorageFile` to `LANGUAGE_CONTROLLER` at this step.
2. Host registers the `ad4m:host` module.
3. Host loads the language bundle (which imports from `ad4m:host`).
4. If the host supports the Holochain extension and the language
   declares it needs Holochain, host installs
   `globalThis.__holochainDelegate__`.
5. Host calls the language's `init()` function.
6. Language is ready for use.

## Minimal browser implementation sketch

```javascript
// 1. Install AGENT — every method is SYNC, so none may return a
//    Promise. Resolve the DID during startup and keep it as a string;
//    sign with a synchronous JS/Wasm implementation (Web Crypto's
//    sign is async and cannot back a SYNC method).
globalThis.AGENT = {
    did: () => cachedDid,               // preloaded DID string
    sign: (payload) => syncSign(payload), // JS/Wasm signer, not Web Crypto
    // ...
};

// 2. Install LANGUAGE_CONTROLLER (core methods)
globalThis.LANGUAGE_CONTROLLER = {
    languageStorageDirectory: () => "/lang-storage/" + langAddr,
    languageAddress: () => langAddr,
    languageSettings: () => settingsJson,

    // OPTIONAL: File I/O extension. This sketch is tab-only —
    // localStorage does not exist inside Workers (a Worker needs a
    // preloaded cache or OPFS sync handles instead). Omit these
    // entirely if this runtime doesn't support raw file I/O — the KV
    // will work in-memory-only and languages importing
    // readStorageFile will throw a clear error on call.
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

// 3. Install UTILS (canonical content-address hash)
//    Output must be the AD4M-canonical format: SHA-256 -> CIDv1 -> base58btc,
//    prefixed with "Qm". hash() is SYNC, so this needs a synchronous
//    SHA-256 (JS or Wasm) — Web Crypto's digest is async.
globalThis.UTILS = {
    hash: (data) => canonicalAd4mHash(data),  // SHA-256 -> CIDv1 -> "Qm..."
};

// 4. Register the ad4m:host module (bundler alias, custom module
//    loader or rewritten imports — import maps don't apply in Workers)
// 5. Load language bundle
// 6. Optionally install __holochainDelegate__ via WebSocket to conductor
// 7. Call language.init()
```
