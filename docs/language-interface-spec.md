# AD4M Language Development Kit — Interface Spec

**Version:** 0.4.0-draft  
**Date:** 2026-04-09  
**Status:** Draft — for discussion

> **Change log v0.4:**
> - Unified two-direction naming: `linksReceiveDiff`, `linksPublish`, `dmReceiveMessage`, `dmPublish`
> - Callbacks are just runtime→language exports (the language calls back via imports)
> - No callback registration IDs — runtime knows the caller's language instance
> - Holochain signal: `handleHolochainSignal(dnaNick, data)` — needs dnaNick to route to correct language
> - `linksPublish(diff)` / `dmPublish(msg)` / `signalEmit(data)` for language→runtime data pushes
> - Clean separation: exports = runtime calls language, imports = language calls runtime

---

## 1. Design: Two Directions

Every cross-boundary call is either:

```
RUNTIME → LANGUAGE     (runtime calls an exported function)
    Example: handleHolochainSignal(dnaNick, data)
    The runtime has received an event from outside and calls the language.

LANGUAGE → RUNTIME     (language calls an imported function)
    Example: linksPublish(diff)
    The language has new data and pushes it to the runtime.
```

**There are no special "callback" mechanisms.** The language stores the callback function from the runtime in its own state (JS) or calls a runtime trigger function (WASM). Both are just function calls.

---

## 2. Runtime → Language: What the Language Exports

The runtime calls these when external events arrive.

### 2.1 Holochain Signal

```
Holochain DNA emits a signal
        ↓
  AD4M runtime receives it
        ↓
  calls language.handleHolochainSignal(dnaNick, signalData)
```

The `dnaNick` identifies which DNA sent the signal (since a language can manage multiple DNAs).

| Export | Parameters | Description |
|--------|-----------|-------------|
| `handleHolochainSignal(dnaNick, signalData)` | `string, unknown` | Holochain DNA emitted a signal |

### 2.2 Link Diff Received

When the runtime has received link diff data from the network through a non-signal channel (e.g. runtime-level gossip, cache invalidation), it can push it to the language via this export.

| Export | Parameters | Description |
|--------|-----------|-------------|
| `linksReceiveDiff(diff)` | `PerspectiveDiff` | Runtime delivers a diff to the language |

> **Note:** In many languages (e.g. p-diff-sync), link data arrives exclusively through `handleHolochainSignal`. The `linksReceiveDiff` export exists for cases where the runtime receives link data through other means and needs to deliver it to the language.

### 2.3 Direct Message Received

| Export | Parameters | Description |
|--------|-----------|-------------|
| `dmReceiveMessage(message)` | `PerspectiveExpression` | Runtime delivers a DM to the language |

---

## 3. Language → Runtime: What the Language Imports

The language calls these to push data to the runtime.

### 3.1 Link Diff Published

The language has a new diff (from local commit, sync, or Holochain signal processing) and pushes it to the runtime.

| Import | Parameters | Description |
|--------|-----------|-------------|
| `linksPublish(diff)` | `PerspectiveDiff` | Push a diff to the runtime |
| `linksPublishSyncState(state)` | `string` | Notify sync state change ("Synced", "NotSynced", etc.) |

The runtime stores the diff and invokes any registered JS callback (set via `linksSetCallback`).

### 3.2 Signal Emitted

| Import | Parameters | Description |
|--------|-----------|-------------|
| `signalEmit(data)` | `unknown` | Emit a signal to the AD4M signal bus |

### 3.3 Direct Message Published

| Import | Parameters | Description |
|--------|-----------|-------------|
| `dmPublish(message)` | `PerspectiveExpression` | Push a DM to the runtime |

---

## 4. Callback Registration (Runtime → Language Direction)

The runtime registers a callback with the language. The language stores it and calls the appropriate runtime import to fire it.

### 4.1 JavaScript

```javascript
// Runtime registers a callback with the language:
export function linksSetCallback(callback) {
    this._linkCallback = callback;  // store the JS function
}

// When links change inside the language:
if (this._linkCallback) {
    this._linkCallback(diff);  // call it directly
}
```

### 4.2 Rust/WASM

The language can't store `JsValue`. Instead, the runtime registers a callback and the language fires it via an import.

```
Runtime                        WASM Language                     Callback map
    │                                  │                                │
    │──── linksSetCallback(cbID) ──────►│  runtime stores cbID           │
    │                                  │  language stores cbID internally │
    │                                  │                                │
    │  [links change in WASM]          │                                │
    │                                  │──── linksTriggerCallback(cbID, diffJson) ──►│  runtime looks up cbID → calls JS fn
```

| Import | Parameters | Description |
|--------|-----------|-------------|
| `linksSetCallback(callbackId)` | `i32` | Register a callback by ID (runtime assigns ID) |
| `linksTriggerCallback(callbackId, diffJson)` | `i32, string` | Fire the registered callback with diff |
| `linksPublishSyncState(callbackId, state)` | `i32, string` | Fire sync state callback |
| `dmSetCallback(callbackId)` | `i32` | Register a DM callback by ID |
| `dmTriggerCallback(callbackId, msgJson)` | `i32, string` | Fire the registered DM callback |

In Rust, the language stores `i32` callback IDs. When it needs to fire, it calls `linksTriggerCallback(id, json)` and the WASM host looks up the actual JS function.

---

## 5. Complete Import List (Runtime → Language / Language Calls)

All are flat function imports, same names in JS and Rust/WASM.

### Agent Identity

| Import | Returns | Description |
|--------|---------|-------------|
| `agentDid()` | `string` | Current agent's DID |
| `agentSigningKeyId()` | `string` | Current signing key ID |
| `agentSign(data: Uint8Array)` | `Uint8Array` | Sign bytes |
| `agentSignStringHex(data: string)` | `string` | Sign hex string |
| `agentCreateSignedExpression(data)` | `object` | Create a signed expression |
| `agentGetAllLocalUserDids()` | `string[]` | All local user DIDs |
| `agentDidForUser(email: string)` | `string` | Get DID for a user |
| `agentCreateSignedExpressionForUser(email, data)` | `object` | Signed expression for a user |

### Holochain

| Import | Returns | Description |
|--------|---------|-------------|
| `holochainRegisterDnas(dnas)` | `void` | Register DNA bundles |
| `holochainCall(dnaNick, zome, fnName, params)` | `unknown` | Sync zome call |
| `holochainCallAsync(calls, timeoutMs?)` | `unknown[]` | Async batch zome calls |

### Language Context

| Import | Returns | Description |
|--------|---------|-------------|
| `languageStorageDirectory()` | `string` | Persistent storage path |
| `languageAddress()` | `string` | This language's network address |
| `languageSettings()` | `string` | Custom settings (raw JSON) |

### Data Pushes (Language → Runtime)

| Import | Parameters | Description |
|--------|-----------|-------------|
| `linksPublish(diff)` | `PerspectiveDiff` | Push a link diff |
| `linksPublishSyncState(state)` | `string` | Push sync state |
| `dmPublish(message)` | `PerspectiveExpression` | Push a DM |
| `signalEmit(data)` | `unknown` | Emit to the AD4M signal bus |

### Callback Triggers (Language → Runtime, for WASM)

| Import | Parameters | Description |
|--------|-----------|-------------|
| `linksSetCallback(callbackId)` | `i32` | Register link callback ID |
| `linksTriggerCallback(callbackId, diffJson)` | `i32, string` | Fire link callback |
| `linksSetSyncStateCallback(callbackId)` | `i32` | Register sync state callback ID |
| `linksTriggerSyncStateCallback(callbackId, state)` | `i32, string` | Fire sync state callback |
| `dmSetCallback(callbackId)` | `i32` | Register DM callback ID |
| `dmTriggerCallback(callbackId, msgJson)` | `i32, string` | Fire DM callback |
| `signalSetCallback(callbackId)` | `i32` | Register signal callback ID |
| `signalTriggerCallback(callbackId, dataJson)` | `i32, string` | Fire signal callback |

---

## 6. Complete Export List (Language → Runtime / Runtime Calls)

### Required

| Export | Type | Description |
|--------|------|-------------|
| `name` | `string` | Language name |
| `version` | `string` | Semver version |
| `init()` | `Promise<void>` | Initialize (use `language_*()` imports inside) |
| `teardown()` | `Promise<void>` | Clean up |

### External Event Handlers (Runtime → Language)

| Export | Parameters | Description |
|--------|-----------|-------------|
| `handleHolochainSignal(dnaNick, signalData)` | `string, unknown` | Holochain DNA emitted a signal |
| `linksReceiveDiff(diff)` | `PerspectiveDiff` | Runtime delivered a diff from external source |
| `dmReceiveMessage(message)` | `PerspectiveExpression` | Runtime delivered a DM |

### Expression Capability

| Export | Returns | Notes |
|--------|---------|-------|
| `expressionCreate(content)` | `Promise<string>` | Store expression |
| `expressionGet(address)` | `Promise<Expression\|null>` | Retrieve |
| `expressionAddressOf(data)` | `string` | Deterministic address |
| `expressionIcon()` | `string` | Icon (base64 SVG or URL) |
| `expressionConstructorIcon()` | `string` | Constructor icon |
| `settingsIcon()` | `string` | Settings icon |

### Link Sync Capability

| Export | Returns | Notes |
|--------|---------|-------|
| `linkSyncSync()` | `Promise<PerspectiveDiff>` | Sync with network |
| `linkSyncCommit(diff)` | `Promise<string>` | Commit a diff |
| `linkSyncRender()` | `Promise<{links: LinkExpression[]}>` | Full link snapshot |
| `linkSyncCurrentRevision()` | `string\|null` | Current revision |
| `linkSyncOthers()` | `Promise<string[]>` | Other synced agents |
| `linkSyncWritable()` | `boolean` | Is writable |
| `linkSyncPublic()` | `boolean` | Is public |
| `linkSyncSetLocalAgents(agents)` | `Promise<void>` | Register local agents |

### Telepresence Capability

| Export | Returns | Notes |
|--------|---------|-------|
| `telepresenceSetOnlineStatus(status)` | `Promise<void>` | Set online status |
| `telepresenceGetOnlineAgents()` | `Promise<OnlineAgent[]>` | Get online agents |
| `telepresenceSendSignal(agentDid, payload)` | `Promise<object>` | Send signal to agent |
| `telepresenceSendBroadcast(payload)` | `Promise<object>` | Broadcast |

### Direct Message Capability

| Export | Returns | Notes |
|--------|---------|-------|
| `directMessageRecipient()` | `string` | This agent's DID |
| `directMessageStatus()` | `Promise<PerspectiveExpression\|void>` | Get DM status |
| `directMessageSendP2P(recipient, message)` | `Promise<PerspectiveExpression\|void>` | Send P2P DM |
| `directMessageSendInbox(recipient, message)` | `Promise<PerspectiveExpression\|void>` | Send inbox DM |
| `directMessageSetStatus(status)` | `Promise<void>` | Set DM status |
| `directMessageInbox(filter?)` | `Promise<PerspectiveExpression[]>` | Get inbox |

### Language Source

| Export | Returns |
|--------|---------|
| `languageGetSource()` | `Promise<LanguageSource>` |

### Query

| Export | Returns |
|--------|---------|
| `getByAuthor(author, count, page)` | `Promise<Expression[]\|null>` |
| `getAll(filter?, count, page)` | `Promise<Expression[]\|null>` |
| `isImmutableExpression(address)` | `boolean` |

### Interactions

| Export | Returns |
|--------|---------|
| `interactions(address)` | `Interaction[]` |

---

## 7. Callback Registration Detail

The runtime registers callbacks with the language. The language stores the callback and fires it via the runtime trigger imports.

### JavaScript — Direct Function Storage

```javascript
class MyLanguage {
    _linkCallback = null;
    _syncStateCallback = null;
    _dmCallback = null;
    _signalCallback = null;

    // Runtime registers
    linksSetCallback(callback) { this._linkCallback = callback; }
    linksSetSyncStateCallback(callback) { this._syncStateCallback = callback; }
    dmSetCallback(callback) { this._dmCallback = callback; }
    signalSetCallback(callback) { this._signalCallback = callback; }

    // Language fires — calls the stored function directly
    _fireLinkCallback(diff) {
        if (this._linkCallback) this._linkCallback(diff);
    }
    _fireSyncStateCallback(state) {
        if (this._syncStateCallback) this._syncStateCallback(state);
    }
}
```

### Rust/WASM — i32 ID + Trigger Import

```rust
struct MyLanguage {
    link_cb_id: Option<i32>,
    sync_state_cb_id: Option<i32>,
    dm_cb_id: Option<i32>,
    signal_cb_id: Option<i32>,
}

impl MyLanguage {
    // Runtime registers — language stores the i32 ID
    fn links_set_callback(&mut self, callback_id: i32) {
        self.link_cb_id = Some(callback_id);
    }

    // Language fires — calls trigger import, passes ID so runtime can look up JS fn
    fn fire_link_callback(&self, diff: &PerspectiveDiff) {
        let json = serde_json::to_string(diff).unwrap();
        unsafe {
            let c_str = std::ffi::CString::new(json).unwrap();
            links_trigger_callback(
                self.link_cb_id.unwrap(),
                c_str.as_ptr()
            );
            std::mem::forget(c_str); // Don't free — host owns it now
        }
    }
}
```

The WASM host implements `links_trigger_callback(id, json)` by looking up `id` in its callback map and calling the stored JS function.

---

## 8. Data Flow Examples

### Example 1: Holochain DNA emits a link signal (p-diff-sync pattern)

```
1. Holochain DNA emits signal via WebSocket
2. AD4M runtime (Rust) receives it
3. Runtime calls: language.handleHolochainSignal("perspective-diff-sync", signalData)
4. Language processes signal, has new links
5. Language calls: linksPublish(diff)  ← pushes diff to runtime
6. Runtime calls registered JS callback with diff
7. AD4M runtime updates perspectives, emits signals to UI
```

### Example 2: Periodic sync (p-diff-sync pattern)

```
1. Runtime timer fires, calls: language.linkSyncSync()
2. Language calls holochainCall("sync", myDid) → Holochain
3. Holochain returns diff
4. Language processes diff, calls: linksPublish(diff)
5. Runtime invokes registered JS callback
```

### Example 3: DM sent (p2p messaging)

```
1. User calls runtime.sendDM(recipient, content)
2. Runtime calls: language.directMessageSendP2P(recipient, message)
3. Language calls holochainCall("send_dm", ...) → Holochain
4. Recipient's Holochain DNA signals
5. Recipient's runtime calls: language.handleHolochainSignal(dnaNick, signalData)
6. Recipient's language processes, calls: dmPublish(message)
7. Recipient's runtime invokes registered DM callback
```

---

## 9. Open Questions

1. **`linksReceiveDiff` vs `handleHolochainSignal`:** In p-diff-sync, link data always arrives through `handleHolochainSignal`. Is `linksReceiveDiff` ever needed? Or can all link data flow through `handleHolochainSignal`? If yes, remove `linksReceiveDiff`.

2. **`dmReceiveMessage` vs `handleHolochainSignal`:** Same question for DMs. Can all DM data arrive through `handleHolochainSignal`?

3. **`TelepresenceAdapter` signals:** The current `TelepresenceAdapter.registerSignalCallback` registers a callback for telepresence signals from OTHER agents. How does this map to the new design? Does the language receive these via `handleHolochainSignal` or via a separate `telepresenceReceiveSignal`?

4. **`DMStatus` type:** Not yet concretely defined.

5. **Sync state callback:** Separate trigger (`linksTriggerSyncStateCallback`) or folded into `linksTriggerCallback` with a different JSON structure? Current thinking: separate is cleaner.

6. **Multiple DNAs per language:** If one language manages multiple DNAs, `handleHolochainSignal(dnaNick, data)` identifies which DNA. But for `linksPublish` / `dmPublish` — does the language need to specify which DNA the data is for? Probably not — the runtime knows which language instance is calling.
