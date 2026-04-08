# AD4M Language Development Kit — Interface Spec

**Version:** 0.1.0-draft  
**Date:** 2026-04-08  
**Status:** Draft — for discussion

---

## 1. Design Decisions

| Decision | Choice |
|----------|--------|
| Runtime → Language delegation | Explicit import from `ad4m:runtime` |
| Language → Runtime exports | Flat function exports (single namespace) |
| JS language authoring | Class or `defineLanguage()` with defaults |
| Rust language authoring | Trait + `#[ad4m_language]` proc macro |
| Init parameter | JSON string (`&str` in Rust) |
| Required functions | Only `name`, `version`, `init`, `teardown` |
| Capability completeness | Enforced at load time (throws if partial) |
| WASM export mapping | Trait methods → `extern "C"` flat functions |

---

## 2. Delegate API — What the Runtime Provides to the Language

### 2.1 Magic Module Path: `ad4m:runtime`

The runtime provides three delegate objects via an explicit import.
Both JavaScript and WASM languages import from the same magic module path.

```javascript
// JavaScript
import { agent, holochain, signals } from 'ad4m:runtime';
```

```rust
// Rust/WASM (compiled source — imports become extern "C" linkage)
use ad4m_ldk::prelude::*;

fn example() {
    let did = agent_did();
    let sig = agent_sign(&payload_bytes);
    holochain_call("my-dna", "zome", "fn", &params_json);
    signals_emit(&data_json);
}
```

### 2.2 `agent` — Agent Identity & Signing

```typescript
// TypeScript type
interface Agent {
  did: string;                                          // this agent's DID
  signingKeyId: string;                                  // current signing key ID

  sign(data: Uint8Array): Promise<Uint8Array>;         // sign arbitrary bytes
  signStringHex(data: string): Promise<string>;        // sign a hex string

  createSignedExpression(data: unknown): Promise<SignedExpression>;
  // Convenience: creates a signed expression with the given data

  getAllLocalUserDids(): string[];
  // All local user DIDs (main agent + managed users)

  didForUser(email: string): string;
  // Get DID for a specific user by email

  createSignedExpressionForUser(email: string, data: unknown): Promise<SignedExpression>;
}
```

```rust
// Rust WASM imports
fn agent_did() -> String;
fn agent_signing_key_id() -> String;
fn agent_sign(payload: &[u8]) -> Vec<u8>;
fn agent_sign_string_hex(payload: &str) -> String;
fn agent_create_signed_expression(data: &JsValue) -> JsValue;
fn agent_get_all_local_user_dids() -> Vec<String>;
fn agent_did_for_user(email: &str) -> String;
fn agent_create_signed_expression_for_user(email: &str, data: &JsValue) -> JsValue;
```

### 2.3 `holochain` — Holochain DNA Registration & Zome Calls

```typescript
interface HolochainDelegate {
  /**
   * Register one or more DNA bundles with the Holochain conductor.
   * Must be called before any zome calls for that DNA.
   */
  registerDNAs(dnas: { nick: string; bundle: Uint8Array }[]): Promise<void>;

  /**
   * Synchronous call to a zome function.
   */
  call(dnaNick: string, zome: string, fnName: string, params: unknown): Promise<unknown>;

  /**
   * Asynchronous batch call to multiple zome functions.
   */
  callAsync(calls: ZomeCall[], timeoutMs?: number): Promise<unknown[]>;
}

interface ZomeCall {
  dnaNick: string;
  zomeName: string;
  fnName: string;
  params: unknown;
}
```

```rust
// Rust WASM imports
fn holochain_register_dnas(dnas: &JsValue) -> JsValue;  // returns Vec<RegisteredDna>
fn holochain_call(dna_nick: &str, zome: &str, fn_name: &str, params: &JsValue) -> JsValue;
fn holochain_call_async(calls: &JsValue, timeout_ms: u32) -> JsValue;  // returns JsFuture
```

### 2.4 `signals` — Signal Emission

```typescript
interface Signals {
  /**
   * Emit a signal to the AD4M signal bus.
   * Forwarded to the runtime's signal handler.
   */
  emit(data: unknown): void;
}
```

```rust
// Rust WASM import
fn signal_emit(data: &JsValue);
```

---

## 3. Language Export Interface — What the Language Provides to the Runtime

### 3.1 Required Exports

Every language must export these:

```javascript
// JavaScript
export const name = "@coasys/my-language";     // string — unique name
export const version = "1.0.0";                 // string — semver
export async function init(contextJson) { }     // string parameter
export async function teardown() { }            // no parameters
```

```rust
// Rust WASM (via #[ad4m_language] macro)
impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "1.0.0";

    fn init(&mut self, context_json: &str) { }
    fn teardown(&mut self) { }
}
```

### 3.2 Context Passed to `init`

```typescript
// The JSON string passed to init() contains:
interface LanguageContext {
  storageDirectory: string;    // persistent storage path for this language instance
  customSettings: object;      // user's custom settings for this language
  languageAddress: string;      // this language's address on the network
}
```

Example:
```javascript
export async function init(contextJson) {
    const ctx = JSON.parse(contextJson);
    console.log("Language installed at:", ctx.storageDirectory);
    console.log("Language address:", ctx.languageAddress);
}
```

```rust
fn init(&mut self, context_json: &str) {
    let ctx: LanguageContext = serde_json::from_str(context_json).unwrap();
    println!("Storage: {}", ctx.storage_directory);
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

    async init(contextJson) {
        // Use this.agent, this.holochain, this.signals
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

    async init(contextJson) {
        const { agent, holochain, signals } = await import('ad4m:runtime');
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

    async init(contextJson) { }  // no-op
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
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct LanguageContext {
    #[serde(rename = "storageDirectory")]
    pub storage_directory: String,
    pub custom_settings: JsValue,
    #[serde(rename = "languageAddress")]
    pub language_address: String,
}

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

    fn init(&mut self, context_json: &str) -> Result<(), LdkError>;

    fn teardown(&mut self) {}
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

extern "C" {
    // Agent
    pub fn agent_did() -> String;
    pub fn agent_signing_key_id() -> String;
    pub fn agent_sign(payload: &[u8]) -> Vec<u8>;
    pub fn agent_sign_string_hex(payload: &str) -> String;
    pub fn agent_create_signed_expression(data: &JsValue) -> JsValue;
    pub fn agent_get_all_local_user_dids() -> Vec<String>;
    pub fn agent_did_for_user(email: &str) -> String;
    pub fn agent_create_signed_expression_for_user(email: &str, data: &JsValue) -> JsValue;

    // Holochain
    pub fn holochain_register_dnas(dnas_json: &JsValue) -> JsValue;
    pub fn holochain_call(dna_nick: &str, zome: &str, fn_name: &str, params: &JsValue) -> JsValue;
    pub fn holochain_call_async(calls: &JsValue, timeout_ms: u32) -> JsValue;

    // Signals
    pub fn signal_emit(data: &JsValue);
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
//     fn init(&mut self, ctx: &str) { }
//     fn expression_create(&mut self, content: &JsValue) -> Result<String, LdkError> { ... }
//     fn link_sync_sync(&mut self) -> JsValue { ... }
// }
//

#[proc_macro_attribute]
pub fn ad4m_language(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parses the impl block, extracts trait methods, generates flat extern "C" exports:
    //
    // #[no_mangle]
    // pub extern "C" fn name() -> *mut std::ffi::c_char {
    //     Cow::Borrowed("my-language").into()
    // }
    //
    // #[no_mangle]
    // pub extern "C" fn version() -> *mut std::ffi::c_char {
    //     Cow::Borrowed("1.0.0").into()
    // }
    //
    // #[no_mangle]
    // pub async fn init(context_json: &str) {
    //     // calls LanguageImpl::init(...)
    // }
    //
    // #[no_mangle]
    // pub async fn expression_create(content: JsValue) -> JsValue {
    //     // calls LanguageImpl::expression_create(...)
    // }
    //
    // ... etc for every trait method
}
```

### 5.6 Complete Example: Minimal Rust Language

```rust
use wasm_bindgen::prelude::*;
use ad4m_ldk::prelude::*;

#[wasm_bindgen]
pub struct MyLanguage {
    storage_dir: String,
    // ... state
}

#[wasm_bindgen]
impl MyLanguage {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            storage_dir: String::new(),
        }
    }
}

impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "0.1.0";

    fn init(&mut self, context_json: &str) -> Result<(), LdkError> {
        let ctx: LanguageContext = serde_json::from_str(context_json)
            .map_err(|e| LdkError::new(e.to_string()))?;
        self.storage_dir = ctx.storage_directory;
        Ok(())
    }

    fn teardown(&mut self) { }
}

impl ExpressionCapability for MyLanguage {
    fn expression_create(&mut self, content: &JsValue) -> Result<String, LdkError> {
        // ... implementation
        Ok("generated-address".to_string())
    }

    fn expression_get(&mut self, address: &str) -> JsValue {
        // ... implementation
        JsValue::NULL
    }
}

#[ad4m_language]
impl Language for MyLanguage { }
```

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

1. **`defineLanguage()` merging:** Should `name` and `version` be static properties on the class (Option A) or properties of the passed object (Option B)? Current spec uses Option B (object property).

2. **Signal routing for WASM:** How does the runtime call `handleHolochainSignal()` on a WASM module? The signal callback needs to be registered during init, and the WASM module needs a way to receive async callbacks. This needs more design.

3. **`directMessageStatus` type:** What is `DMStatus`? Needs definition.

4. **`perspective-diff-sync` as reference:** The actual p-diff-sync language has many internal details (gossip protocol, mutex, peer tracking). A minimal link language would be much simpler. Should we write a minimal stub language as the reference instead?

5. **Adapter objects in Rust:** The `PerspectiveDiff`, `Expression` etc. types — should they live in the ALDK crate or in `core`? Currently in `core` as TypeScript types. Rust equivalents should probably be in `ad4m-ldk`.

6. **WASM memory management:** Who frees JSValue strings returned from WASM? Need to define ownership rules for `String` return types in the macro.
