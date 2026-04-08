# AD4M Language Development Kit — Interface Spec

**Version:** 0.2.0-draft  
**Date:** 2026-04-08  
**Status:** Draft — for discussion

> **Change log v0.2:** Everything is a flat function import/export. No JSON parameters to `init()`.
> Context is accessed via `language_*()` function imports. Delegates are flat functions, not objects.
> Same function names in JavaScript and Rust/WASM.

---

## 1. Design Decisions

| Decision | Choice |
|----------|--------|
| Runtime → Language | Flat function imports (same in JS and Rust/WASM) |
| Language → Runtime | Flat function exports (single namespace) |
| Context access | Flat function imports (`language_*()`) — no JSON params to `init()` |
| JS language authoring | Class or `defineLanguage()` with defaults |
| Rust language authoring | Trait + `#[ad4m_language]` proc macro |
| Required functions | Only `name`, `version`, `init`, `teardown` |
| Capability completeness | Enforced at load time (throws if partial) |
| WASM export mapping | Trait methods → `extern "C"` flat functions |

---

## 2. Function Imports — What the Runtime Provides to the Language

All function imports are **identical in JavaScript and Rust/WASM**. Both get the same flat function names.
Import from `ad4m:runtime` in JavaScript. In Rust/WASM they become `extern "C"` declarations that the WASM module links against.

### 2.1 Agent Identity & Signing

| Function | Returns | Description |
|---------|---------|-------------|
| `agent_did()` | `string` | Current agent's DID |
| `agent_signing_key_id()` | `string` | Current signing key ID |
| `agent_sign(data: Uint8Array)` | `Uint8Array` | Sign arbitrary bytes |
| `agent_sign_string_hex(data: string)` | `string` | Sign a hex string |
| `agent_create_signed_expression(data)` | `object` | Create a signed expression |
| `agent_get_all_local_user_dids()` | `string[]` | All local user DIDs (main + managed) |
| `agent_did_for_user(email: string)` | `string` | Get DID for a specific user |
| `agent_create_signed_expression_for_user(email, data)` | `object` | Signed expression for a specific user |

### 2.2 Holochain

| Function | Returns | Description |
|---------|---------|-------------|
| `holochain_register_dnas(dnas)` | `void` | Register DNA bundles with the conductor |
| `holochain_call(dnaNick, zome, fnName, params)` | `unknown` | Sync zome call |
| `holochain_call_async(calls, timeoutMs?)` | `unknown[]` | Async batch zome calls |

### 2.3 Signals

| Function | Returns | Description |
|---------|---------|-------------|
| `signal_emit(data)` | `void` | Emit a signal to the AD4M signal bus |

### 2.4 Language Context

These give the language access to its own context. Call them inside `init()`.

| Function | Returns | Description |
|---------|---------|-------------|
| `language_storage_directory()` | `string` | Persistent storage path for this language instance |
| `language_address()` | `string` | This language's address on the network |
| `language_settings()` | `string` | User's custom settings for this language (raw JSON — caller parses) |

### 2.5 JavaScript Usage

```javascript
import {
    agent_did, agent_sign, holochain_call, holochain_register_dnas,
    signal_emit, language_storage_directory, language_address, language_settings
} from 'ad4m:runtime';

export async function init() {
    const storage = language_storage_directory();
    const langAddr = language_address();
    const settings = JSON.parse(language_settings());  // custom settings as JSON
    const did = agent_did();

    await holochain_register_dnas([{ nick: "my-dna", bundle: myDnaBundle }]);
}

export async function teardown() { }

export async function expressionCreate(content) {
    const sig = await agent_sign(content);
    // ...
}
```

### 2.6 Rust/WASM Usage

```rust
// These are extern "C" imports — the WASM module links against them at runtime.
extern "C" {
    fn agent_did() -> *mut c_char;
    fn agent_sign(payload: *const u8, len: usize) -> *mut c_char;
    fn holochain_call(dna_nick: *const c_char, zome: *const c_char, fn_name: *const c_char, params: *const c_char) -> *mut c_char;
    fn holochain_register_dnas(dnas_json: *const c_char);
    fn signal_emit(data: *const c_char);
    fn language_storage_directory() -> *mut c_char;
    fn language_address() -> *mut c_char;
    fn language_settings() -> *mut c_char;
}

fn example_init() {
    let storage = unsafe { ptr_to_string(language_storage_directory()) };
    let lang_addr = unsafe { ptr_to_string(language_address()) };
    let settings_json = unsafe { ptr_to_string(language_settings()) };
    let did = unsafe { ptr_to_string(agent_did()) };
}
```

> **Note on ownership:** Rust functions that return strings return `*mut c_char` (owned pointer).
> The WASM module must free the pointer after use. The ALDK provides a utility (`ptr_to_string`)
> that copies the string and frees the pointer.

---

## 3. Language Export Interface — What the Language Provides to the Runtime

### 3.1 Required Exports

Every language must export these:

```javascript
// JavaScript
export const name = "@coasys/my-language";     // string — unique name
export const version = "1.0.0";                 // string — semver
export async function init() { }               // no parameters — use language_*() imports inside
export async function teardown() { }           // no parameters
```

```rust
// Rust WASM (via #[ad4m_language] macro)
impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "1.0.0";

    fn init(&mut self) { }      // no parameters — use language_*() imports inside
    fn teardown(&mut self) { }
}
```

### 3.2 Getting Context Inside `init()`

Call the `language_*()` function imports to get context. Call them inside `init()` and store what you need in the language's state.

```javascript
export const name = "@coasys/my-language";
const myStorage = language_storage_directory();  // captured at init time
const myAddress = language_address();
const mySettings = JSON.parse(language_settings());

export async function init() {
    // Call language_*() imports here — they work from init() onward
}
```

```rust
struct MyLanguage {
    storage: String,
    address: String,
}

impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "1.0.0";

    fn init(&mut self) {
        self.storage = language_storage_directory();
        self.address = language_address();
    }

    fn teardown(&mut self) { }
}
```

### 3.3 Capability Functions

All other exports are **optional capabilities**. If a language implements one function of a capability group, it must implement all functions of that group. This is validated at load time.

#### Expression Capability (storage & retrieval)

Required if the language stores/retrieves expressions.

```javascript
export async function expressionCreate(content: unknown): Promise<string>;
// → returns the address of the created expression

export async function expressionGet(address: string): Promise<Expression | null>;
// → returns the expression or null

export async function expressionAddressOf(data: unknown): Promise<string>;
// → deterministic address for given data
```

```rust
fn expression_create(&mut self, content: &JsValue) -> String;
fn expression_get(&mut self, address: &str) -> JsValue;  // returns JsValue::NULL for not found
fn expression_address_of(&mut self, data: &JsValue) -> String;
```

#### Link Sync Capability

Required if the language syncs links between agents via Holochain.

```javascript
export async function linkSyncSync(): Promise<PerspectiveDiff>;
// → sync with network, returns diff (additions + removals since last sync)

export async function linkSyncCommit(diff: PerspectiveDiff): Promise<string>;
// → commit a diff, returns new revision hash

export async function linkSyncRender(): Promise<{ links: LinkExpression[] }>;
// → full snapshot of all links (for rendering)

export function linkSyncCurrentRevision(): string | null;
// → current revision hash

export async function linkSyncOthers(): Promise<string[]>;
// → list of other agents in the sync group

export function linkSyncWritable(): boolean;
export function linkSyncPublic(): boolean;

export function linkSyncAddCallback(callback: (diff: PerspectiveDiff) => void): number;
export function linkSyncRemoveCallback(callback: (diff: PerspectiveDiff) => void): number;
export function linkSyncAddSyncStateChangeCallback(callback: (state: string) => void): number;
export function linkSyncSetLocalAgents(): Promise<void>;
```

```rust
fn link_sync_sync(&mut self) -> JsValue;
fn link_sync_commit(&mut self, diff: &JsValue) -> String;
fn link_sync_render(&mut self) -> JsValue;
fn link_sync_current_revision(&self) -> String;
fn link_sync_others(&mut self) -> Vec<String>;
fn link_sync_writable(&self) -> bool;
fn link_sync_public(&self) -> bool;
fn link_sync_add_callback(&mut self, callback: &JsValue) -> u32;
fn link_sync_remove_callback(&mut self, callback: &JsValue) -> u32;
fn link_sync_add_sync_state_change_callback(&mut self, callback: &JsValue) -> u32;
fn link_sync_set_local_agents(&mut self) -> JsValue;
```

#### Telepresence Capability

Required if the language handles online status and agent signaling.

```javascript
export async function telepresenceSetOnlineStatus(status: unknown): Promise<void>;
export async function telepresenceGetOnlineAgents(): Promise<unknown[]>;
export async function telepresenceSendSignal(remoteAgentDid: string, payload: unknown): Promise<unknown>;
export async function telepresenceSendBroadcast(payload: unknown): Promise<unknown>;
export async function telepresenceRegisterSignalCallback(callback: unknown): Promise<void>;
```

```rust
fn telepresence_set_online_status(&mut self, status: &JsValue) -> JsValue;
fn telepresence_get_online_agents(&mut self) -> Vec<JsValue>;
fn telepresence_send_signal(&mut self, remote_agent_did: &str, payload: &JsValue) -> JsValue;
fn telepresence_send_broadcast(&mut self, payload: &JsValue) -> JsValue;
fn telepresence_register_signal_callback(&mut self, callback: &JsValue) -> JsValue;
```

#### Direct Message Capability

```javascript
export function directMessageRecipient(): string;
export async function directMessageStatus(): Promise<DMStatus>;
export async function directMessageSendP2P(recipient: string, data: unknown): Promise<void>;
export async function directMessageSendInbox(recipient: string, data: unknown): Promise<void>;
export function directMessageSetStatus(status: DMStatus): Promise<void>;
export async function directMessageInbox(): Promise<DMMessage[]>;
export function directMessageAddMessageCallback(cb: (msg: DMMessage) => void): number;
```

```rust
fn direct_message_recipient(&self) -> String;
fn direct_message_status(&mut self) -> JsValue;
fn direct_message_send_p2p(&mut self, recipient: &str, data: &JsValue) -> JsValue;
fn direct_message_send_inbox(&mut self, recipient: &str, data: &JsValue) -> JsValue;
fn direct_message_set_status(&mut self, status: &JsValue) -> JsValue;
fn direct_message_inbox(&mut self) -> Vec<JsValue>;
fn direct_message_add_message_callback(&mut self, cb: &JsValue) -> u32;
```

#### UI / Icon Capability

```javascript
export function expressionIcon(): string;        // base64 SVG or URL
export function expressionConstructorIcon(): string;
export function settingsIcon(): string;
```

```rust
fn expression_icon(&self) -> String;
fn expression_constructor_icon(&self) -> String;
fn settings_icon(&self) -> String;
```

#### Language Source Capability

```javascript
export async function languageGetSource(): Promise<LanguageSource>;
```

```rust
fn language_get_source(&mut self) -> JsValue;
```

#### Query Adapters

```javascript
export async function getByAuthor(author: string): Promise<Expression[]>;
export async function getAll(): Promise<Expression[]>;
export function isImmutableExpression(address: string): boolean;
```

```rust
fn get_by_author(&mut self, author: &str) -> Vec<JsValue>;
fn get_all(&mut self) -> Vec<JsValue>;
fn is_immutable_expression(&self, address: &str) -> bool;
```

#### Interactions

```javascript
export function interactions(): Interaction[];
// → returns the list of interactions this language can perform
```

```rust
fn interactions(&self) -> Vec<JsValue>;
```

---

## 4. JavaScript ALDK — `@ad4m/ldk`

### 4.1 Package

```
@ad4m/ldk
├── index.js           — exports Language, defineLanguage, capability
├── types.d.ts         — TypeScript type definitions
├── runtime.js         — runtime mock / types for ad4m:runtime
└── validation.js      — capability completeness checker
```

### 4.2 API

```javascript
import { Language, defineLanguage } from '@ad4m/ldk';

// --- Option A: extend the base class ---
class MyLanguage extends Language {
    static name = "@coasys/my-language";
    static version = "1.0.0";

    async init() {
        // Use language_*() imports from 'ad4m:runtime'
    }

    async teardown() { }

    async expressionCreate(content) { /* ... */ }
    async expressionGet(address) { /* ... */ }
}

export default MyLanguage;

// --- Option B: defineLanguage() object shorthand ---
import { defineLanguage } from '@ad4m/ldk';

export default defineLanguage({
    name: "@coasys/my-language",
    version: "1.0.0",

    async init() {
        // Use language_*() imports to get context
        const storage = language_storage_directory();
    },

    async expressionCreate(content) { /* ... */ }
    // expressionGet, teardown, etc. use defaults if not provided
});
```

### 4.3 Base `Language` class

```javascript
// Core class — all defaults provided, override what you need
class Language {
    static name = "unnamed-language";
    static version = "0.0.0";

    async init() { }  // no-op — use language_*() imports to get context
    async teardown() { }         // no-op

    // Expression capability — default throws "not implemented"
    async expressionCreate(content) {
        throw new Error("expressionCreate not implemented");
    }
    async expressionGet(address) {
        throw new Error("expressionGet not implemented");
    }
    async expressionAddressOf(data) {
        throw new Error("expressionAddressOf not implemented");
    }

    // Link sync — safe defaults
    async linkSyncSync() {
        return new PerspectiveDiff();
    }
    async linkSyncCommit(diff) {
        throw new Error("linkSyncCommit not implemented");
    }
    async linkSyncRender() {
        return { links: [] };
    }
    linkSyncCurrentRevision() { return null; }
    async linkSyncOthers() { return []; }
    linkSyncWritable() { return false; }
    linkSyncPublic() { return false; }
    linkSyncAddCallback(cb) { return 0; }
    linkSyncRemoveCallback(cb) { return 0; }
    linkSyncAddSyncStateChangeCallback(cb) { return 0; }
    async linkSyncSetLocalAgents() { }

    // Telepresence — default throws
    async telepresenceSetOnlineStatus(status) {
        throw new Error("telepresenceSetOnlineStatus not implemented");
    }
    async telepresenceGetOnlineAgents() {
        throw new Error("telepresenceGetOnlineAgents not implemented");
    }
    async telepresenceSendSignal(agent, payload) {
        throw new Error("telepresenceSendSignal not implemented");
    }
    async telepresenceSendBroadcast(payload) {
        throw new Error("telepresenceSendBroadcast not implemented");
    }
    async telepresenceRegisterSignalCallback(cb) { }

    // Direct message — default throws
    directMessageRecipient() { throw new Error("not implemented"); }
    async directMessageStatus() { throw new Error("not implemented"); }
    async directMessageSendP2P(recipient, data) {
        throw new Error("not implemented");
    }
    async directMessageSendInbox(recipient, data) {
        throw new Error("not implemented");
    }
    async directMessageSetStatus(status) { throw new Error("not implemented"); }
    async directMessageInbox() { throw new Error("not implemented"); }
    directMessageAddMessageCallback(cb) { return 0; }

    // UI
    expressionIcon() { return ""; }
    expressionConstructorIcon() { return ""; }
    settingsIcon() { return ""; }

    // Source
    async languageGetSource() {
        throw new Error("languageGetSource not implemented");
    }

    // Queries
    async getByAuthor(author) { return []; }
    async getAll() { return []; }
    isImmutableExpression(address) { return false; }

    // Interactions
    interactions() { return []; }
}
```

### 4.4 `defineLanguage()` — validation and defaults

```javascript
function defineLanguage(partial) {
    // 1. Merge with defaults
    const defaults = new Language.prototype;
    const merged = Object.assign({}, defaults, partial);

    // 2. Validate capability completeness
    const capabilityGroups = [
        ['expressionCreate', 'expressionGet', 'expressionAddressOf'],
        ['linkSyncSync', 'linkSyncCommit', 'linkSyncRender',
         'linkSyncCurrentRevision', 'linkSyncOthers',
         'linkSyncWritable', 'linkSyncPublic',
         'linkSyncAddCallback', 'linkSyncRemoveCallback',
         'linkSyncAddSyncStateChangeCallback', 'linkSyncSetLocalAgents'],
        ['telepresenceSetOnlineStatus', 'telepresenceGetOnlineAgents',
         'telepresenceSendSignal', 'telepresenceSendBroadcast',
         'telepresenceRegisterSignalCallback'],
        ['directMessageRecipient', 'directMessageStatus',
         'directMessageSendP2P', 'directMessageSendInbox',
         'directMessageSetStatus', 'directMessageInbox',
         'directMessageAddMessageCallback'],
    ];

    for (const group of capabilityGroups) {
        const implemented = group.filter(fn => partial[fn] !== undefined);
        const missing = group.filter(fn => partial[fn] === undefined);

        // If ANY function in a group is implemented, ALL must be implemented
        if (implemented.length > 0 && missing.length > 0) {
            throw new Error(
                `Capability group incomplete: implemented [${implemented.join(', ')}] ` +
                `but missing [${missing.join(', ')}]`
            );
        }
    }

    // 3. Return an object that the runtime can use
    return merged;
}
```

---

## 5. Rust ALDK — `ad4m-ldk` Crate

### 5.1 Crate Structure

```
ad4m-ldk/
├── Cargo.toml
├── src/
│   ├── lib.rs              — re-exports
│   ├── runtime.rs          — extern "C" function declarations (agent_*, holochain_*, signal_emit)
│   ├── context.rs          — LanguageContext deserialization
│   ├── language.rs         — Language trait
│   ├── capabilities.rs     — per-capability traits (LinkSyncCapability, etc.)
│   ├── errors.rs           — LdkError type
│   ├── js_value.rs         — JsValue utilities
│   └── macro.rs            — #[ad4m_language] proc macro
└── macro/src/lib.rs        — proc macro implementation
```

### 5.2 Core Trait

```rust
use wasm_bindgen::JsValue;

#[derive(Debug)]
pub struct LdkError {
    pub message: String,
}

impl LdkError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self { message: msg.into() }
    }
}

impl From<String> for LdkError {
    fn from(s: String) -> Self { Self::new(s) }
}

pub trait Language: Sized {
    const NAME: &'static str;
    const VERSION: &'static str;

    /// Initialize the language. Call language_*() imports inside this function
    /// to get storage directory, language address, and settings.
    fn init(&mut self) { }

    fn teardown(&mut self) { }
}
```

### 5.3 Capability Traits

```rust
use wasm_bindgen::JsValue;
use super::{Language, LdkError};

// ============================================================================
// Expression Capability
// ============================================================================

pub trait ExpressionCapability: Language {
    fn expression_create(&mut self, content: &JsValue) -> Result<String, LdkError>;
    fn expression_get(&mut self, address: &str) -> JsValue;
    fn expression_address_of(&mut self, data: &JsValue) -> String;
}

// Default implementations (throws "not implemented")
impl<T: Language> ExpressionCapability for T {
    default fn expression_create(&mut self, _: &JsValue) -> Result<String, LdkError> {
        Err(LdkError::new("expressionCreate not implemented"))
    }
    default fn expression_get(&mut self, _: &str) -> JsValue {
        JsValue::NULL
    }
    default fn expression_address_of(&mut self, _: &JsValue) -> String {
        String::new()
    }
}

// ============================================================================
// Link Sync Capability
// ============================================================================

#[derive(Default)]
pub struct PerspectiveDiff {
    pub additions: Vec<JsValue>,
    pub removals: Vec<JsValue>,
}

pub trait LinkSyncCapability: Language {
    fn link_sync_sync(&mut self) -> JsValue;
    fn link_sync_commit(&mut self, diff: &JsValue) -> Result<String, LdkError>;
    fn link_sync_render(&mut self) -> JsValue;
    fn link_sync_current_revision(&self) -> String;
    fn link_sync_others(&mut self) -> Vec<String>;
    fn link_sync_writable(&self) -> bool;
    fn link_sync_public(&self) -> bool;
    fn link_sync_add_callback(&mut self, callback: &JsValue) -> u32;
    fn link_sync_remove_callback(&mut self, callback: &JsValue) -> u32;
    fn link_sync_add_sync_state_change_callback(&mut self, callback: &JsValue) -> u32;
    fn link_sync_set_local_agents(&mut self) -> JsValue;
}

impl<T: Language> LinkSyncCapability for T {
    default fn link_sync_sync(&mut self) -> JsValue { JsValue::NULL }
    default fn link_sync_commit(&mut self, _: &JsValue) -> Result<String, LdkError> {
        Err(LdkError::new("linkSyncCommit not implemented"))
    }
    default fn link_sync_render(&mut self) -> JsValue {
        JsValue::from_serde(&serde_json::json!({ "links": [] })).unwrap()
    }
    default fn link_sync_current_revision(&self) -> String { String::new() }
    default fn link_sync_others(&mut self) -> Vec<String> { vec![] }
    default fn link_sync_writable(&self) -> bool { false }
    default fn link_sync_public(&self) -> bool { false }
    default fn link_sync_add_callback(&mut self, _: &JsValue) -> u32 { 0 }
    default fn link_sync_remove_callback(&mut self, _: &JsValue) -> u32 { 0 }
    default fn link_sync_add_sync_state_change_callback(&mut self, _: &JsValue) -> u32 { 0 }
    default fn link_sync_set_local_agents(&mut self) -> JsValue { JsValue::NULL }
}

// ============================================================================
// Telepresence Capability
// ============================================================================

pub trait TelepresenceCapability: Language {
    fn telepresence_set_online_status(&mut self, status: &JsValue) -> Result<(), LdkError>;
    fn telepresence_get_online_agents(&mut self) -> Result<Vec<JsValue>, LdkError>;
    fn telepresence_send_signal(&mut self, remote_agent_did: &str, payload: &JsValue) -> Result<JsValue, LdkError>;
    fn telepresence_send_broadcast(&mut self, payload: &JsValue) -> Result<JsValue, LdkError>;
    fn telepresence_register_signal_callback(&mut self, callback: &JsValue) -> Result<(), LdkError>;
}

impl<T: Language> TelepresenceCapability for T {
    default fn telepresence_set_online_status(&mut self, _: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("telepresenceSetOnlineStatus not implemented"))
    }
    default fn telepresence_get_online_agents(&mut self) -> Result<Vec<JsValue>, LdkError> {
        Err(LdkError::new("telepresenceGetOnlineAgents not implemented"))
    }
    default fn telepresence_send_signal(&mut self, _: &str, _: &JsValue) -> Result<JsValue, LdkError> {
        Err(LdkError::new("telepresenceSendSignal not implemented"))
    }
    default fn telepresence_send_broadcast(&mut self, _: &JsValue) -> Result<JsValue, LdkError> {
        Err(LdkError::new("telepresenceSendBroadcast not implemented"))
    }
    default fn telepresence_register_signal_callback(&mut self, _: &JsValue) -> Result<(), LdkError> {
        Ok(())
    }
}

// ============================================================================
// Direct Message Capability
// ============================================================================

pub trait DirectMessageCapability: Language {
    fn direct_message_recipient(&self) -> String;
    fn direct_message_status(&mut self) -> Result<JsValue, LdkError>;
    fn direct_message_send_p2p(&mut self, recipient: &str, data: &JsValue) -> Result<(), LdkError>;
    fn direct_message_send_inbox(&mut self, recipient: &str, data: &JsValue) -> Result<(), LdkError>;
    fn direct_message_set_status(&mut self, status: &JsValue) -> Result<(), LdkError>;
    fn direct_message_inbox(&mut self) -> Result<Vec<JsValue>, LdkError>;
    fn direct_message_add_message_callback(&mut self, cb: &JsValue) -> u32;
}

impl<T: Language> DirectMessageCapability for T {
    default fn direct_message_recipient(&self) -> String {
        String::new()
    }
    default fn direct_message_status(&mut self) -> Result<JsValue, LdkError> {
        Err(LdkError::new("directMessageStatus not implemented"))
    }
    default fn direct_message_send_p2p(&mut self, _: &str, _: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("directMessageSendP2P not implemented"))
    }
    default fn direct_message_send_inbox(&mut self, _: &str, _: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("directMessageSendInbox not implemented"))
    }
    default fn direct_message_set_status(&mut self, _: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("directMessageSetStatus not implemented"))
    }
    default fn direct_message_inbox(&mut self) -> Result<Vec<JsValue>, LdkError> {
        Err(LdkError::new("directMessageInbox not implemented"))
    }
    default fn direct_message_add_message_callback(&mut self, _: &JsValue) -> u32 { 0 }
}
```

### 5.4 Runtime Imports (what the WASM module imports from the host)

```rust
// These are provided by the AD4M runtime (Rust executor or Deno).
// The WASM module links against them at runtime.
// All return owned *mut c_char strings — caller must free the pointer.

extern "C" {
    // Agent
    fn agent_did() -> *mut c_char;
    fn agent_signing_key_id() -> *mut c_char;
    fn agent_sign(payload: *const u8, len: usize) -> *mut c_char;
    fn agent_sign_string_hex(payload: *const c_char) -> *mut c_char;
    fn agent_create_signed_expression(data: *const c_char) -> *mut c_char;
    fn agent_get_all_local_user_dids() -> *mut c_char;
    fn agent_did_for_user(email: *const c_char) -> *mut c_char;
    fn agent_create_signed_expression_for_user(email: *const c_char, data: *const c_char) -> *mut c_char;

    // Holochain
    fn holochain_register_dnas(dnas_json: *const c_char);
    fn holochain_call(dna_nick: *const c_char, zome: *const c_char, fn_name: *const c_char, params: *const c_char) -> *mut c_char;
    fn holochain_call_async(calls: *const c_char, timeout_ms: u32) -> *mut c_char;

    // Signals
    fn signal_emit(data: *const c_char);

    // Language context
    fn language_storage_directory() -> *mut c_char;
    fn language_address() -> *mut c_char;
    fn language_settings() -> *mut c_char;
}
```

### 5.5 `#[ad4m_language]` Proc Macro

```rust
// What the macro generates from this input:
//
// #[ad4m_language]
// impl Language for MyLanguage {
//     const NAME: &'static str = "@coasys/my-language";
//     const VERSION: &'static str = "1.0.0";
//     fn init(&mut self) { }
//     fn expression_create(&mut self, content: &JsValue) -> Result<String, LdkError> { ... }
//     fn link_sync_sync(&mut self) -> JsValue { ... }
// }
//

#[proc_macro_attribute]
pub fn ad4m_language(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parses the impl block, extracts NAME, VERSION, and trait methods,
    // generates flat extern "C" WASM exports:
    //
    // #[no_mangle]
    // pub extern "C" fn name() -> *mut c_char {
    //     Cow::Borrowed("my-language").into()
    // }
    //
    // #[no_mangle]
    // pub extern "C" fn version() -> *mut c_char {
    //     Cow::Borrowed("1.0.0").into()
    // }
    //
    // #[no_mangle]
    // pub extern "C" async fn init() {
    //     // calls MyLanguage::init(...)
    // }
    //
    // #[no_mangle]
    // pub extern "C" async fn expression_create(content: JsValue) -> JsValue {
    //     // calls MyLanguage::expression_create(...) and serializes Result
    // }
    //
    // ... etc for every trait method with underscore naming
}
```

### 5.6 Complete Example: Minimal Rust Language

```rust
use wasm_bindgen::prelude::*;
use ad4m_ldk::prelude::*;

#[wasm_bindgen]
pub struct MyLanguage {
    storage_dir: String,
    lang_address: String,
}

#[wasm_bindgen]
impl MyLanguage {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            storage_dir: String::new(),
            lang_address: String::new(),
        }
    }
}

impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "0.1.0";

    fn init(&mut self) {
        // Use language_*() imports to get context
        self.storage_dir = language_storage_directory();
        self.lang_address = language_address();
    }

    fn teardown(&mut self) { }
}

impl ExpressionCapability for MyLanguage {
    fn expression_create(&mut self, content: &JsValue) -> Result<String, LdkError> {
        let address = format!("address-for-{:?}", content);
        Ok(address)
    }

    fn expression_get(&mut self, address: &str) -> JsValue {
        JsValue::NULL
    }
}

#[ad4m_language]
impl Language for MyLanguage { }
```

The macro generates flat `extern "C"` WASM exports from the trait implementation. The developer writes clean trait methods with `&JsValue` parameters and `Result<T, LdkError>` returns; the macro handles the WASM ABI.

The macro generates flat `extern "C"` WASM exports from the trait implementation. The developer writes clean trait methods; the macro handles the WASM ABI.

---

## 6. Data Types

### PerspectiveDiff

```typescript
interface PerspectiveDiff {
    additions: LinkExpression[];
    removals: LinkExpression[];
}

interface LinkExpression {
    author: string;
    timestamp: number;
    data: {
        source: string | null;
        target: string | null;
        predicate: string | null;
    };
    proof?: {
        signature: string;
        key: string;
    };
}
```

### Expression

```typescript
interface Expression {
    author: string;
    timestamp: number;
    data: unknown;
    proof?: {
        signature: string;
        key: string;
    };
}
```

### SignedExpression

```typescript
interface SignedExpression {
    author: string;
    timestamp: number;
    data: unknown;
    proof: {
        signature: string;    // base64 signature
        key: string;         // signing key ID
    };
}
```

---

## 7. Open Questions

1. **Signal routing for WASM:** How does the runtime call `handleHolochainSignal()` on a WASM module? The signal callback needs to be registered during init, and the WASM module needs a way to receive async callbacks from the host. This needs more design.

2. **`directMessageStatus` type:** What is `DMStatus`? Needs concrete definition.

3. **`perspective-diff-sync` as reference:** The actual p-diff-sync language has many internal details (gossip protocol, mutex, peer tracking). A minimal link language would be much simpler. Should we write a minimal stub language as the reference instead?

4. **Adapter objects in Rust:** The `PerspectiveDiff`, `Expression` etc. types — should they live in the ALDK crate or in `core`? Currently in `core` as TypeScript types. Rust equivalents should probably be in `ad4m-ldk`.

5. **WASM memory management:** Who frees `*mut c_char` strings returned from WASM? The current spec says "caller must free the pointer" — the ALDK should provide a `ptr_to_string()` utility that copies and frees. Need to ensure this is safe in the macro-generated code.

6. **`holochain_call_async` return type in JS:** Should this return a `Promise` in JS (natural async) or a plain value (matching the WASM sync signature)? Currently spec shows it returns `unknown[]` in JS (treated as async by the runtime), but the WASM version is sync. Need to confirm the JS interface.
