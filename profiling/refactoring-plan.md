# AD4M Executor Memory Leak Analysis & Refactoring Plan

> **Note:** This plan was written against the pre-#693 architecture. Some details may be outdated.

**Date:** 2026-02-21  
**Author:** Hex (Agent), based on memory profiling results  
**For:** Nico (lucksus)

## Executive Summary

Memory profiling revealed three categories of leaks:

| Issue | Severity | Memory per instance | Recovery on remove |
|-------|----------|--------------------|--------------------|
| Neighbourhood teardown | **CRITICAL** | ~139 MB | **0%** |
| Bare perspective create/remove | Medium | ~2.4 MB | Partial |
| Language cloning (template apply) | Medium | ~4.2 MB | **0%** (permanent) |

**Root cause:** `perspectiveRemove` sets a teardown flag but performs **zero resource cleanup**. No Holochain hApps are uninstalled, no Prolog engine pools are freed, no SurrealDB instances are dropped, and no languages are unloaded from the JS runtime.

---

## 1. CRITICAL: Neighbourhood Teardown Leaks 100% of Memory

### The Call Chain

```text
GraphQL perspectiveRemove
  → rust-executor/src/graphql/mutation_resolvers.rs:804-815
    → perspectives::remove_perspective(uuid)
      → rust-executor/src/perspectives/mod.rs:143-166
        → instance.teardown_background_tasks()
          → perspective_instance.rs:243-245  ← THIS IS THE ENTIRE TEARDOWN
```

### What `teardown_background_tasks` Actually Does

**File:** `rust-executor/src/perspectives/perspective_instance.rs`, lines 243-245

```rust
pub async fn teardown_background_tasks(&self) {
    *self.is_teardown.lock().await = true;
}
```

That's it. It sets a boolean flag. The background task loops (7 of them, started at line 231-241) check this flag and eventually stop looping, but **no resources are freed**.

### What `remove_perspective` Does

**File:** `rust-executor/src/perspectives/mod.rs`, lines 143-166

```rust
pub async fn remove_perspective(uuid: &str) -> Option<PerspectiveInstance> {
    // 1. Remove from SQLite DB (links, diffs, handle)
    Ad4mDb::remove_perspective(uuid);   // line 145-152
    
    // 2. Remove from in-memory HashMap
    let removed_instance = PERSPECTIVES.write().unwrap().remove(uuid);  // line 154-157
    
    // 3. Set teardown flag (that's all teardown_background_tasks does)
    instance.teardown_background_tasks().await;  // line 160
    
    // 4. Publish removal event
    pubsub.publish(PERSPECTIVE_REMOVED_TOPIC, uuid);  // line 163-165
    
    return removed_instance;  // PerspectiveInstance is dropped here (but Arcs keep resources alive)
}
```

### Resources Allocated But Never Freed

#### 1.1 Holochain hApps (~100-130 MB per neighbourhood)

**Allocated at:** `executor/src/core/storage-services/Holochain/HolochainService.ts`, line 195-234  
(`ensureInstallDNAforLanguage` → `HOLOCHAIN_SERVICE.installApp()`)

**Freed by:** `HolochainService.removeDnaForLang()` at line 241-243, which calls `HOLOCHAIN_SERVICE.removeApp(lang)`

**The problem:** `removeDnaForLang` is only called from `LanguageController.languageRemove()` (line 491 of LanguageController.ts), which is only triggered by the `languageRemove` GraphQL mutation. **`perspectiveRemove` never calls `languageRemove`.** It doesn't even know which languages a perspective/neighbourhood uses.

Additionally, the Rust-side Holochain conductor maintains:
- WASM runtimes for each installed hApp
- DHT state and network connections
- Signal broadcast streams (added at `holochain_service/mod.rs:126`, never removed)

**Estimated memory:** Each Holochain hApp with WASM runtime: 50-130 MB depending on DNA complexity. This is the single biggest leak.

#### 1.2 Prolog Engine Pools (~10-20 MB per perspective)

**Allocated at:** `perspectives/perspective_instance.rs`, lines 1390-1420  
(`ensure_prolog_engine_pool` → `PrologService::ensure_perspective_pool`)

Each perspective creates **two** Prolog pools:
- Main pool: `uuid` (line 1392, with 2-5 engines depending on link count)
- Notification pool: `notification_{uuid}` (line 1412, with 1 engine)

**File:** `rust-executor/src/prolog_service/mod.rs`, lines 50-74

```rust
pub async fn ensure_perspective_pool(&self, perspective_id: String, pool_size: Option<usize>) {
    // Creates PrologEnginePool with N Scryer Prolog engine processes
    let pool = PrologEnginePool::new();
    pool.initialize(pool_size.unwrap_or(DEFAULT_POOL_SIZE)).await?;
    pools.insert(perspective_id, pool);
}
```

Each `PrologEnginePool` also creates:
- Filtered sub-pools (engine_pool.rs, line 556+) with their own Prolog engines
- SDNA pools (separate set of engines)
- Cleanup tasks (tokio tasks, line 672)
- State logging tasks (tokio tasks, line 905)

**Removal method exists but is never called:** `_remove_perspective_pool()` at `prolog_service/mod.rs:69-74`. Note the underscore prefix — Rust convention for "intentionally unused." It's only called in tests (line 438).

**Estimated memory:** 5-10 MB per Prolog engine × (2-5 main engines + 1 notification engine + filtered pools) = 10-40 MB per perspective.

#### 1.3 SurrealDB In-Memory Database (~5-10 MB per perspective)

**Allocated at:** `perspectives/mod.rs`, lines 50-52 (init) and 86-88 (add_perspective)

```rust
let surreal_service = SurrealDBService::new("ad4m", &handle.uuid).await?;
```

**File:** `rust-executor/src/surreal_service/mod.rs`, lines 250-310

Each perspective gets its own in-memory SurrealDB instance (`Surreal<Mem>`) with:
- Node table (all URIs)
- Link table (graph edges)
- Multiple indexes (7 indexes defined)
- JavaScript function definitions (for `fn::parse_literal`)
- Schema definitions

The `SurrealDBService` is stored in the `PerspectiveInstance` as `Arc<SurrealDBService>`. When the perspective is removed, the `PerspectiveInstance` is dropped from the HashMap, but if any background tasks still hold `Arc` clones, the SurrealDB instance stays alive.

**No cleanup method exists.** There's `clear_perspective()` (line 412) which deletes data but keeps the DB instance alive. The DB should be fully dropped.

#### 1.4 Link Language Reference and JS Objects

**Allocated at:** `perspective_instance.rs`, lines 281-310 (`ensure_link_language`)

```rust
link_language: Arc<RwLock<Option<Language>>>,
```

The `Language` struct holds a `JsCoreHandle` reference. The JS-side language object (created by `LanguageController.loadLanguage()`, LanguageController.ts:218-301) includes:
- The language module itself (loaded via Deno `loadModule`)
- Registered callbacks: `linksAdapter.addCallback` (line 271), `addSyncStateChangeCallback` (line 276), `telepresenceAdapter.registerSignalCallback` (line 285)
- Holochain delegate reference
- Storage directory handle

**These callbacks create circular references:** The language holds callbacks that reference the LanguageController's observer arrays, which reference the language.

#### 1.5 Background Tokio Tasks (7 per perspective)

**Spawned at:** `perspectives/mod.rs`, line 91 and `perspective_instance.rs`, lines 231-241

```rust
pub async fn start_background_tasks(self) {
    let _ = join!(
        self.ensure_link_language(),       // polls every 5s
        self.notification_check_loop(),     // polls on trigger
        self.nh_sync_loop(),               // polls every 10-60s  
        self.pending_diffs_loop(),         // polls every 3s
        self.subscribed_queries_loop(),    // polls every 200ms
        self.surreal_subscription_cleanup_loop(),  // polls
        self.fallback_sync_loop()          // polls every 30s+
    );
}
```

The `tokio::spawn(p.clone().start_background_tasks())` at mod.rs:91 creates a tokio task that **clones the entire PerspectiveInstance** (which contains Arcs to all the resources above). Even after `is_teardown` is set to `true`, the loops need to wake up and check the flag — they sleep for up to 60 seconds between checks (nh_sync_loop). During that window, all Arcs are still held.

**More critically:** The task itself holds the cloned PerspectiveInstance until it exits. If any loop gets stuck (e.g., waiting on a zome call that times out after 90 seconds), the resources are held indefinitely.

---

## 2. Bare Perspective Leak (~2.4 MB per create/remove)

Even without a neighbourhood (no Holochain), creating and removing a perspective leaks:

### Resources not cleaned up:

| Resource | Allocated | Size estimate |
|----------|-----------|---------------|
| Prolog engine pools (2 pools) | perspective_instance.rs:1390-1420 | ~1.5 MB |
| SurrealDB instance | mod.rs:86-88 | ~0.5 MB |
| SQLite link data | db.rs — **IS cleaned up** (line 725-741) | 0 |
| Tokio task handles | mod.rs:91 | ~0.1 MB |
| Arc-held state (subscribed queries, batch store, mutexes) | perspective_instance.rs:197-230 | ~0.3 MB |

The 2.4 MB figure matches: 2 Prolog pools (main with 5 engines + notification with 1 engine) + SurrealDB + miscellaneous Arc state.

---

## 3. Language Cloning Leak (~4.2 MB per clone)

### The Flow

```text
languageApplyTemplateAndPublish (Ad4mCore.ts:190)
  → languageApplyTemplateOnSource (LanguageController.ts:810)
    → readAndTemplateHolochainDNA (LanguageController.ts:604)
      → unPackHapp, unPackDna (creates temp files)
      → Modifies DNA properties (UIDs, etc.)  
      → packDna, packHapp (creates new bundle)
    → constructLanguageLanguageInput (bundles JS + hApp)
  → publish (creates expression in Language Language)
  → The new language is then installed via languageByRef
    → installLanguage (LanguageController.ts:382)
      → loadLanguage (LanguageController.ts:218)
        → Loads JS module into Deno runtime
        → Creates Holochain delegate
        → Registers callbacks
        → Adds to #languages Map
```

### What Accumulates:

1. **JS modules loaded into Deno**: Each `loadModule()` call (LanguageController.ts:66-70) loads a new JavaScript module into the Deno runtime. These modules are **never unloaded** from V8's module map. Even if `#languages.delete(hash)` is called, the V8 module remains in memory.

2. **Holochain DNA hApp bundles on disk**: `readAndTemplateHolochainDNA` (LanguageController.ts:604-700) creates temporary directories for unpacking/repacking but some intermediate files may persist.

3. **Language constructor closures**: `#languageConstructors` Map (LanguageController.ts:79) stores the constructor function for each language. These are never removed unless `languageRemove` is explicitly called.

4. **The installed language stays in `#languages` Map forever**: Once a templated language is published and installed, it lives in `#languages` Map permanently. There's no mechanism to know when it's no longer needed.

### Why This Matters for Neighbourhoods:

When a neighbourhood is created via `neighbourhoodPublishFromPerspective`, it calls `languageApplyTemplateAndPublish` to clone a link language. This cloned language is installed permanently. If the neighbourhood's perspective is later removed, the cloned link language remains installed — its Holochain hApp stays running, its JS module stays loaded, and its Prolog state stays allocated.

---

## 4. Proposed Fixes (Priority Order)

### Fix 1: CRITICAL — Implement `teardown_background_tasks` properly

**File:** `rust-executor/src/perspectives/perspective_instance.rs`

Replace lines 243-245 with a proper teardown:

```rust
pub async fn teardown_background_tasks(&self) {
    // Signal all background loops to stop
    *self.is_teardown.lock().await = true;
    
    let uuid = self.persisted.lock().await.uuid.clone();
    
    // 1. Remove Prolog engine pools
    let prolog_service = get_prolog_service().await;
    if let Err(e) = prolog_service._remove_perspective_pool(uuid.clone()).await {
        log::error!("Error removing Prolog pool for perspective {}: {:?}", uuid, e);
    }
    // Also remove the notification pool
    let notification_pool = notification_pool_name(&uuid);
    if let Err(e) = prolog_service._remove_perspective_pool(notification_pool).await {
        log::error!("Error removing notification Prolog pool for perspective {}: {:?}", uuid, e);
    }
    
    // 2. Clear SurrealDB data (the Arc will be dropped when all refs are gone)
    if let Err(e) = self.surreal_service.clear_perspective(&uuid).await {
        log::error!("Error clearing SurrealDB for perspective {}: {:?}", uuid, e);
    }
    
    // 3. If this is a neighbourhood, uninstall the link language's Holochain hApp
    let handle = self.persisted.lock().await.clone();
    if let Some(ref nh) = handle.neighbourhood {
        let link_language_address = nh.data.link_language.clone();
        // Call into JS to remove the language (which calls removeDnaForLang)
        if let Err(e) = Self::unload_language_for_perspective(link_language_address).await {
            log::error!("Error unloading link language for perspective {}: {:?}", uuid, e);
        }
    }
    
    // 4. Clear subscribed queries
    self.subscribed_queries.lock().await.clear();
    self.surreal_subscribed_queries.lock().await.clear();
    
    // 5. Clear batch store
    self.batch_store.write().await.clear();
}
```

**Prerequisite:** Rename `_remove_perspective_pool` to `remove_perspective_pool` in `prolog_service/mod.rs:69` (remove the underscore prefix).

### Fix 2: CRITICAL — Add language unloading path from Rust to JS

**File:** `rust-executor/src/languages/mod.rs`

Add a new method:

```rust
impl LanguageController {
    pub async fn remove_language(address: Address) -> Result<(), AnyError> {
        Self::global_instance()
            .js_core
            .execute("await core.waitForLanguages()".into())
            .await?;

        let script = format!(
            r#"await core.languageController.languageRemove("{}")"#,
            address,
        );
        Self::global_instance().js_core.execute(script).await?;
        Ok(())
    }
}
```

**Then use it from teardown** (as `Self::unload_language_for_perspective` in Fix 1 above).

### Fix 3: CRITICAL — Clean up Holochain signal streams on app removal

**File:** `rust-executor/src/holochain_service/mod.rs`

In the signal forwarding task (line 100-135), add handling for `RemoveApp`:

```rust
// Add a channel for removed app IDs
let (removed_app_ids_sender, mut removed_app_ids_receiver) = mpsc::unbounded_channel::<String>();
```

In the `RemoveApp` handler (line 156-168), after removing the app, send the app_id through the channel:

```rust
HolochainServiceRequest::RemoveApp(app_id, response_tx) => {
    let result = service.remove_app(app_id.clone()).await;
    if result.is_ok() {
        let _ = removed_app_ids_sender.send(app_id);
    }
    let _ = response_tx.send(HolochainServiceResponse::RemoveApp(result));
}
```

In the signal stream select loop, handle removals:

```rust
Some(removed_id) = removed_app_ids_receiver.recv() => {
    streams.remove(&removed_id);
}
```

**Also fix JS side:** In `HolochainService.ts`, add cleanup of `#signalCallbacks`:

```typescript
async removeDnaForLang(lang: string) {
    // Remove signal callbacks for this language
    this.#signalCallbacks = this.#signalCallbacks.filter(e => e[2] !== lang);
    await HOLOCHAIN_SERVICE.removeApp(lang);
}
```

### Fix 4: MEDIUM — Add reference counting for languages

Languages can be shared across multiple perspectives/neighbourhoods. A language should only be uninstalled when no perspective references it.

**File:** `executor/src/core/LanguageController.ts`

Add a reference counter:

```typescript
#languageRefCounts: Map<string, number>  // language address → active perspective count

languageAddRef(address: string) {
    const count = this.#languageRefCounts.get(address) || 0;
    this.#languageRefCounts.set(address, count + 1);
}

languageReleaseRef(address: string) {
    const count = this.#languageRefCounts.get(address) || 0;
    if (count <= 1) {
        this.#languageRefCounts.delete(address);
        // Safe to remove — no perspectives using this language
        this.languageRemove(address);
    } else {
        this.#languageRefCounts.set(address, count - 1);
    }
}
```

Call `languageAddRef` when a perspective installs/uses a link language, and `languageReleaseRef` in teardown.

### Fix 5: MEDIUM — Ensure SurrealDB instance is fully dropped

**File:** `rust-executor/src/surreal_service/mod.rs`

Add a `shutdown` method:

```rust
impl SurrealDBService {
    pub async fn shutdown(&self) -> Result<(), Error> {
        // Drop all data
        self.db.query("REMOVE DATABASE IF EXISTS current").await.ok();
        // The Surreal<Db> will be dropped when all Arc references are released
        Ok(())
    }
}
```

**File:** `rust-executor/src/perspectives/perspective_instance.rs`

In teardown, explicitly call shutdown and ensure no lingering Arc references:

```rust
// In teardown_background_tasks:
self.surreal_service.shutdown().await.ok();
```

### Fix 6: LOW — Bound the background task shutdown window

**File:** `rust-executor/src/perspectives/perspective_instance.rs`

The background tasks check `is_teardown` on each loop iteration, but some loops sleep for up to 60 seconds. Add a `tokio::select!` with a shutdown signal:

```rust
// Instead of:
while !*self.is_teardown.lock().await {
    interval.tick().await;
    // ... work ...
}

// Use a Notify or watch channel:
tokio::select! {
    _ = self.shutdown_notify.notified() => { break; }
    _ = interval.tick() => { /* ... work ... */ }
}
```

This would require adding a `tokio::sync::Notify` to `PerspectiveInstance` and triggering it in teardown. This ensures tasks exit promptly rather than waiting up to 60 seconds.

### Fix 7: LOW — Clean up Deno module cache on language removal

**File:** `executor/src/core/LanguageController.ts`, in `languageRemove` (line 471-492)

The current `languageRemove` deletes from `#languages` and `#languageConstructors`, calls `removeDnaForLang`, and deletes files. But the Deno/V8 module cache still holds the loaded module.

This is harder to fix — V8 doesn't support module unloading. Options:
1. Accept this as a known limitation
2. Use Deno workers (each language in its own worker, killed on unload)
3. Track and avoid re-loading the same module hash

---

## 5. Architecture Notes

### 5.1 The Missing Lifecycle Contract

The fundamental architectural issue is that **there's no lifecycle contract for perspectives**. Resources are allocated eagerly across multiple systems (Holochain, Prolog, SurrealDB, JS runtime) but there's no corresponding deallocation phase.

**What should exist:** A `PerspectiveLifecycle` trait/interface:

```rust
trait PerspectiveLifecycle {
    async fn on_create(&self);       // allocate resources
    async fn on_activate(&self);     // start background tasks
    async fn on_deactivate(&self);   // stop background tasks
    async fn on_destroy(&self);      // free ALL resources
}
```

Currently, `new()` + `start_background_tasks()` covers create/activate, and `teardown_background_tasks()` is supposed to cover deactivate/destroy but only does the flag-setting part of deactivate.

### 5.2 Cross-System Resource Ownership

Resources are allocated by one system but never communicated to the teardown path:

| Resource | Allocated by | Teardown knows about? |
|----------|-------------|----------------------|
| Holochain hApp | LanguageController (JS) | ❌ No |
| Prolog pools | PerspectiveInstance (Rust) | ❌ No (pool name not stored) |
| SurrealDB instance | mod.rs (Rust) | ✅ Yes (in struct) |
| JS language modules | LanguageController (JS) | ❌ No |
| Signal callbacks | HolochainService (JS) | ❌ No |
| Link/sync callbacks | LanguageController (JS) | ❌ No |

**Recommendation:** The `PerspectiveInstance` should maintain a list of all language addresses it uses, so teardown can iterate them and release references.

### 5.3 Arc Reference Cycle Risk

The `PerspectiveInstance` is cloned via `Arc` across:
- The `PERSPECTIVES` HashMap (mod.rs:22)
- The background task (spawned at mod.rs:91)
- Any in-flight GraphQL request handlers

When `remove_perspective` removes from the HashMap, the instance still lives in the background task clone. If Fix 6 isn't applied, the instance (and all its Arc'd resources) can live for up to 60 seconds after removal.

### 5.4 Language Reference Counting is Essential

Right now, languages are installed once and live forever. With neighbourhoods:
1. Joining NH installs a link language
2. The link language installs a Holochain hApp
3. Removing the perspective doesn't touch either

Since multiple perspectives could reference the same language (e.g., two neighbourhoods using the same link language template), **reference counting is the right approach**. Simple "remove on perspective delete" could break other perspectives.

### 5.5 Holochain Conductor Memory

The Holochain conductor runs in its own thread (`std::thread::spawn` at mod.rs:100) with its own Tokio runtime. Each installed hApp adds:
- WASM modules (compiled and cached)
- DHT data structures
- Network connections (WebRTC peers, signal connections)
- Lair keystore entries

`conductor.uninstall_app()` (used in `remove_app` at mod.rs:395) does clean up these resources, but it's **never called** during perspective removal. This is the single biggest memory saving opportunity.

---

## 6. Testing the Fixes

After implementing the fixes, re-run the memory profiling tests:

1. **Neighbourhood teardown test**: Create 3 neighbourhoods, remove all 3, verify memory returns to within ~20 MB of baseline (allowing for some permanent allocations like the conductor itself).

2. **Bare perspective churn test**: Create/remove 100 perspectives, verify total memory growth < 10 MB (vs current ~240 MB).

3. **Language clone test**: Clone 10 languages, verify memory growth is bounded. With reference counting, removing all perspectives using cloned languages should recover the memory.

4. **Long-running test**: Run for 1 hour with periodic create/remove cycles, verify no unbounded growth.

---

## 7. Summary of Changes by File

| File | Changes needed |
|------|---------------|
| `rust-executor/src/perspectives/perspective_instance.rs:243` | Implement full teardown (Fix 1) |
| `rust-executor/src/perspectives/mod.rs:143` | Await full teardown, ensure Arc cleanup |
| `rust-executor/src/prolog_service/mod.rs:69` | Rename `_remove_perspective_pool` → `remove_perspective_pool` |
| `rust-executor/src/languages/mod.rs` | Add `remove_language()` method (Fix 2) |
| `rust-executor/src/holochain_service/mod.rs:100-135` | Add stream removal on app uninstall (Fix 3) |
| `executor/src/core/storage-services/Holochain/HolochainService.ts:241` | Clean up `#signalCallbacks` in `removeDnaForLang` (Fix 3) |
| `executor/src/core/LanguageController.ts` | Add reference counting (Fix 4) |
| `rust-executor/src/surreal_service/mod.rs` | Add `shutdown()` method (Fix 5) |

**Estimated effort:** Fix 1-3 (critical path) = 1-2 days. Fix 4-7 = 2-3 additional days.

**Estimated memory savings:** Fix 1-3 should recover ~90% of leaked memory from neighbourhood teardown. Fix 4 handles the remaining edge cases with shared languages.
