use crate::agent::AgentContext;
use crate::js_core::agent_extension::{forget_call_agent, register_call_agent};
use crate::js_core::JsCore;
use crate::languages::capability::{detection_script, parse_capability_list, Capability};
use crate::languages::LanguageContext;
use futures::future::LocalBoxFuture;
use futures::stream::{FuturesUnordered, StreamExt};
use log::{debug, error, info, warn};
use serde_json::Value as JsonValue;
use std::cell::Cell;
use std::collections::{HashSet, VecDeque};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::{mpsc::UnboundedReceiver, oneshot};

// ---------------------------------------------------------------------------
// The agent a call acts as
// ---------------------------------------------------------------------------
//
// One language runtime serves every user of the executor. A call names the
// agent it acts as (`AgentContext`, e.g. a managed user publishing their
// profile), and reads run beside other calls, so that agent cannot be a
// runtime-wide setting: a call that resumed after an await would act as
// whichever call set it last. The runtime registers the call's agent under the
// call's id (`register_call_agent`) and runs the script inside
// `__ad4mRunCall(id, ...)`, which takes the agent out once as an unforgeable
// token and keeps it in a V8 async-context variable. V8 carries that variable
// with every continuation the call creates (awaits, promise reactions, timers),
// and the agent ops (`agent_did`, `agent_sign`, ...) sign as the token they are
// given. With no token they refuse. The host's own entries into the language
// (module evaluation, init, callback registration, teardown) run the same way
// with a main-agent token (`execute_as_host`), and so does every timer, socket
// handler and promise they start; a Holochain signal arrives as a call as the
// main agent. See `js_core/agent_extension.rs`.

/// Request sent to a LanguageRuntime via its channel
#[derive(Debug)]
pub(crate) struct LanguageRuntimeRequest {
    pub operation: LanguageOperation,
    pub response_tx: oneshot::Sender<Result<String, String>>,
    /// When the handle sent it; the runtime logs the time in queue on dequeue (#1133).
    pub queued_at: Instant,
    /// Names the request in `RunningRequest`.
    pub id: u64,
}

/// The requests a runtime is running. The runtime writes it; the handle reads it to tell a
/// caller whose call runs from one still queued.
#[derive(Debug, Default)]
pub(crate) struct Running {
    /// The ordered request running, if any: its id and when it started.
    pub ordered: Option<(u64, Instant)>,
    /// The concurrent calls started (first polled, their script begun) and not yet ended.
    /// One admitted but not started is not here: its caller's start deadline still runs.
    pub concurrent: HashSet<u64>,
}

pub(crate) type RunningRequest = Arc<Mutex<Running>>;

/// How many concurrent calls one language runs beside its ordered stream. Beyond it a
/// concurrent call waits (in send order behind lifecycle requests, first-come behind
/// other concurrent calls) without holding up ordered calls, and its caller's start
/// deadline runs, so a burst cannot grow one runtime's futures without bound.
pub(crate) const MAX_CONCURRENT_CALLS: usize = 16;

/// How a script call runs beside the language's other calls.
///
/// The ordering this gives is what callers rely on. A perspective's commits must land
/// one at a time in the order they were sent, so a later diff is never applied before
/// the one it builds on; the same holds for sync, expression creates, interactions and
/// every other call that writes state a later call depends on. A concurrent call is
/// for work whose effect needs no position in the send order: a read (an expression
/// get, a rendered revision, a peer or presence list), or an emit whose order the
/// protocol tolerates (a telepresence signal or broadcast).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CallKind {
    /// Runs alone among the ordered calls, in send order: commits, sync, expression
    /// creates, interactions, signal delivery, and every call not marked concurrent.
    Ordered,
    /// Starts at once and runs beside the other calls, up to [`MAX_CONCURRENT_CALLS`]
    /// in flight. Only for a call that changes no state a later call depends on and
    /// needs no position in the send order.
    Concurrent,
}

/// Operations that can be sent to a LanguageRuntime
#[derive(Debug)]
pub(crate) enum LanguageOperation {
    Execute(String, AgentContext, CallKind),
    LoadModule(String, LanguageContext),
    LoadLanguage(JsonValue),
    RegisterCallbacks,
    Teardown,
}

impl LanguageOperation {
    /// Variant name for log lines (`{:?}` would print the whole script).
    pub fn name(&self) -> &'static str {
        match self {
            LanguageOperation::Execute(..) => "Execute",
            LanguageOperation::LoadModule(..) => "LoadModule",
            LanguageOperation::LoadLanguage(..) => "LoadLanguage",
            LanguageOperation::RegisterCallbacks => "RegisterCallbacks",
            LanguageOperation::Teardown => "Teardown",
        }
    }

    pub fn is_concurrent(&self) -> bool {
        matches!(self, LanguageOperation::Execute(_, _, CallKind::Concurrent))
    }

    /// Loading, callback registration and teardown: they run with no call beside them.
    pub fn is_lifecycle(&self) -> bool {
        !matches!(self, LanguageOperation::Execute(..))
    }
}

/// Per-language Deno runtime that encapsulates a single language instance.
/// Each LanguageRuntime has its own isolated JsCore/Deno worker.
///
/// This is NOT Send (contains V8/MainWorker). It lives in a dedicated thread
/// and processes requests from a channel. Mirrors the JsCore pattern -
/// communication happens via LanguageRuntimeHandle.
pub struct LanguageRuntime {
    language_address: String,
    js_core: JsCore,
    /// Set by `load_module`: the bundle is wasm-bindgen output (a Rust/ALDK
    /// language), so every call into it runs ordered, `Concurrent` included.
    wasm: Cell<bool>,
}

impl LanguageRuntime {
    pub fn new(
        language_address: String,
        storage_directory: PathBuf,
        is_system_language: bool,
    ) -> Self {
        info!(
            "Creating LanguageRuntime for: {} (system={})",
            language_address, is_system_language
        );
        Self {
            language_address,
            js_core: JsCore::new_for_language(storage_directory, is_system_language),
            wasm: Cell::new(false),
        }
    }

    /// Whether the bundle is wasm-bindgen output (a Rust/ALDK language).
    /// wasm-bindgen's `__wbindgen`/`__wbg_` names are the wasm module's own
    /// import/export names, so bundling cannot rename them.
    fn is_wasm_bundle(source: &str) -> bool {
        source.contains("__wbindgen") || source.contains("__wbg_")
    }

    /// Initialize the language runtime by executing the bootstrap module.
    /// Must be called before processing requests to make Deno ops available.
    pub async fn init(&self) -> Result<(), String> {
        self.js_core.init_for_language().await.map_err(|e| {
            format!(
                "Bootstrap failed for language {}: {}",
                self.language_address, e
            )
        })
    }

    /// Load a language bundle from source code directly (no file I/O in JS).
    /// The bundle is loaded as an ES module via Deno's runtime API and its
    /// full module namespace is captured as `globalThis.languageModule`.
    pub async fn load_module(
        &self,
        id: u64,
        source: &str,
        language_context: &LanguageContext,
    ) -> Result<(), String> {
        // A WASM language runs every call ordered: its futures are polled from
        // wasm-bindgen-futures' single module-wide microtask queue, which does
        // not restore each task's V8 async context, so a call resumed beside
        // another would act as the other call's agent (`is_wasm_bundle`).
        self.wasm.set(Self::is_wasm_bundle(source));

        // Use a synthetic URL unique to this language so Deno's module map
        // doesn't collide if multiple languages are loaded in the same runtime.
        let specifier = format!("https://ad4m.language/{}/bundle.js", self.language_address);

        // Set the language context into the thread-local BEFORE loading the module
        // This makes language_*() ops available to the language's init()
        self.js_core.set_language_context(
            language_context
                .storage_directory
                .to_string_lossy()
                .to_string(),
            language_context.language_address.clone(),
            language_context
                .custom_settings
                .as_ref()
                .map(|s| s.to_string())
                .unwrap_or_default(),
        );

        // Loaded, not evaluated: the `import()` below evaluates it, under the main agent's
        // token, so the bundle's top-level code and the timers it starts act as the main agent.
        self.js_core
            .load_module_from_source_unevaluated(&specifier, source.to_string())
            .await
            .map_err(|e| format!("Failed to load language bundle: {}", e))?;

        // Capture the module namespace as globalThis.languageModule. The
        // Language v1 module format exports `init()` plus a flat set of
        // named capability functions (perspectiveSyncSync, perspectiveCommit,
        // etc.) which the bootstrap dispatcher reads off the namespace.
        // `init` may live on the namespace itself or on the default export
        // (depending on whether the language uses `export function init`
        // vs `export default { init }`); both shapes are normalized to a
        // single namespace object here.
        //
        // Note: execute() wraps scripts in `return (expr)`, so this must be
        // a single expression, not statements.
        //
        // JSON-encode the specifier so it embeds as a well-formed JS
        // string literal. The specifier embeds `self.language_address`,
        // which is technically attacker-controllable via language
        // resolution paths; a bare `"{}"` splice would let any `"`,
        // `\`, or newline in the address break the import expression
        // or inject JS inside the language isolate.
        let specifier_lit = serde_json::to_string(&specifier)
            .map_err(|e| format!("failed to encode bundle specifier: {}", e))?;
        let capture_script = format!(
            r#"import({}).then(m => {{
                if (typeof m.init === "function") {{
                    globalThis.languageModule = m;
                }} else if (m.default && typeof m.default.init === "function") {{
                    globalThis.languageModule = m.default;
                }} else {{
                    throw new Error(
                        "Language bundle does not export init() — expected a " +
                        "Language v1 module with a top-level init function."
                    );
                }}
            }})"#,
            specifier_lit
        );
        self.execute_as_host(id, &capture_script)
            .await
            .map_err(|e| format!("Failed to capture language constructor: {}", e))?;
        Ok(())
    }

    /// Initialize the language with context via the bootstrap's initLanguage()
    pub async fn load_language(&self, id: u64, language_context: JsonValue) -> Result<(), String> {
        info!("Initializing language: {}", self.language_address);

        let context_json = serde_json::to_string(&language_context)
            .map_err(|e| format!("Failed to serialize language context: {}", e))?;

        let script = format!("await initLanguage({})", context_json);

        match self.execute_as_host(id, &script).await {
            Ok(result) => {
                info!(
                    "Language constructor completed for {}: {}",
                    self.language_address,
                    &result[..result.len().min(200)]
                );
                Ok(())
            }
            Err(e) => {
                error!(
                    "Failed to call language constructor for {}: {}",
                    self.language_address, e
                );
                Err(format!(
                    "Failed to call language constructor for {}: {}",
                    self.language_address, e
                ))
            }
        }
    }

    /// Execute a script in this language's runtime, with no agent token: the agent ops refuse.
    /// Language code runs only through `execute_call` and `execute_as_host`.
    pub async fn execute(&self, script: &str) -> Result<String, String> {
        self.js_core.execute(script).await
    }

    /// Runs a host entry into the language (loading, init, callback registration, teardown)
    /// as the main agent: the language code it runs, and every timer and promise that code
    /// starts, act as the main agent. `id` is the lifecycle request's.
    async fn execute_as_host(&self, id: u64, script: &str) -> Result<String, String> {
        self.execute_call(id, script, AgentContext::main_agent())
            .await
    }

    /// Wire the runtime's diff / sync-state / telepresence-signal sinks
    /// into the language, then return the set of capabilities it exports.
    ///
    /// Two independent jobs live under this one v8 round-trip:
    ///
    /// 1. **Callback registration.** Language v1 still ships two push
    ///    channels as explicit exports — `linkSyncAddCallback` and
    ///    `linkSyncAddSyncStateChangeCallback` — and the telepresence
    ///    callback via `telepresenceRegisterSignalCallback`. They are
    ///    effectively setters: the runtime hands in a closure that forwards
    ///    to `LANGUAGE_CONTROLLER.{perspectiveDiffReceived,syncStateChanged,
    ///    telepresenceSignalReceived}`, and the language calls it from
    ///    `handleHolochainSignal` (or wherever) when new data arrives.
    ///    Without this step no cross-peer diff ever lands in the runtime's
    ///    perspective store. (The ALDK also exposes direct host imports —
    ///    `emitPerspectiveDiff` etc. — but current bootstrap languages
    ///    still use the callback-setter shape, and the capability
    ///    registration step below doesn't care which mechanism a language
    ///    picks.)
    ///
    /// 2. **Capability probing.** Returns the set of capabilities the
    ///    language actually exports. The caller stores the set in the
    ///    global capability registry so `Language::new(address)` can later
    ///    answer `has(Capability::…)` without another v8 round-trip.
    pub async fn register_callbacks(&self, id: u64) -> Result<HashSet<Capability>, String> {
        // JSON-encode the language address so it embeds as a proper JS
        // string literal — defensive against addresses containing quotes,
        // backslashes, or newlines.
        let addr_lit = serde_json::to_string(&self.language_address)
            .map_err(|e| format!("failed to encode language address: {}", e))?;

        // Wire the runtime's sinks. Each setter is optional; a language
        // that uses `emitPerspectiveDiff` host imports directly simply
        // won't export these setters, and the guards below no-op.
        let callback_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (!language) return "ok";
                if (typeof language.linkSyncAddCallback === "function") {{
                    language.linkSyncAddCallback((diff) => {{
                        LANGUAGE_CONTROLLER.perspectiveDiffReceived(diff, {addr_lit});
                    }});
                }}
                if (typeof language.linkSyncAddSyncStateChangeCallback === "function") {{
                    language.linkSyncAddSyncStateChangeCallback((state) => {{
                        LANGUAGE_CONTROLLER.syncStateChanged(state, {addr_lit});
                    }});
                }}
                if (typeof language.telepresenceRegisterSignalCallback === "function") {{
                    language.telepresenceRegisterSignalCallback((signal, recipientDid) => {{
                        LANGUAGE_CONTROLLER.telepresenceSignalReceived(signal, {addr_lit}, recipientDid);
                    }});
                }}
                return "ok";
            }})()
            "#,
        );

        let _ = self.execute_as_host(id, &callback_script).await?;

        // Detect which capabilities the language actually exports. Returns
        // a JSON array of kebab-case capability names that parse_capability_list
        // converts to the typed enum set. Kept as one v8 round-trip per
        // language load, not per call. The script checks one flat export
        // per capability (`Capability::export_name`).
        let raw = self.execute_as_host(id, &detection_script()).await?;
        let caps = parse_capability_list(raw.trim());

        info!(
            "Detected capabilities for language {}: {:?}",
            self.language_address, caps
        );

        Ok(caps)
    }

    /// Teardown and cleanup this language runtime
    pub async fn teardown(&self, id: u64) -> Result<(), String> {
        info!("Tearing down language runtime: {}", self.language_address);

        // Run the language's teardown hook. The bootstrap shim wraps
        // the language's own teardown() in an async function so the
        // runtime can always `await language.teardown()`.
        let cleanup_script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (!language) return;
                if (typeof language.teardown === "function") {
                    await language.teardown();
                }
            })()
        "#;

        if let Err(e) = self.execute_as_host(id, cleanup_script).await {
            error!(
                "Error during language cleanup for {}: {}",
                self.language_address, e
            );
        }

        // execute() wraps scripts in `return (expr)`, so a trailing
        // semicolon on a statement-style expression produces a syntax
        // error (`return (delete X;);`). Drop the semicolon and let the
        // delete operator evaluate as the single expression inside the
        // wrapper. The previous form silently failed at the JS layer
        // (swallowed by `let _ =`), leaving the instance global dangling
        // on the isolate — harmless today because the isolate tears
        // down immediately after, but wrong on its face.
        let _ = self
            .execute_as_host(id, "delete globalThis.__ad4m_language_instance__")
            .await;

        info!("Tore down language runtime: {}", self.language_address);
        Ok(())
    }
}

/// A request the runtime has run, with what its caller gets.
struct Finished {
    id: u64,
    response_tx: oneshot::Sender<Result<String, String>>,
    result: Result<String, String>,
    is_teardown: bool,
}

impl LanguageRuntime {
    /// Returns the future that runs `request`. The request counts as started (in `running`, as
    /// a read or as the ordered request) on the future's first poll, just before its script
    /// starts, not when the loop admits it: reads admitted together start one after the other,
    /// and one that holds the thread keeps the others from starting, so until then their
    /// callers' start deadline still runs. A script call whose caller has gone by its first
    /// poll is answered with an error and not run. The caller gives up under the same lock, so
    /// a call either runs with its caller waiting or not at all.
    fn begin<'a>(
        &'a self,
        request: LanguageRuntimeRequest,
        running: &RunningRequest,
        as_concurrent: bool,
    ) -> LocalBoxFuture<'a, Finished> {
        let running = running.clone();
        Box::pin(async move {
            let LanguageRuntimeRequest {
                operation,
                response_tx,
                queued_at,
                id,
            } = request;
            let addr = &self.language_address;
            debug!("[lang:{}] Processing operation: {:?}", addr, operation);
            // Time in this runtime's queue (#1133 symptom ①): the counterpart of the
            // `[hc-actor] … waited` line.
            let waited = queued_at.elapsed();
            if waited > Duration::from_secs(1) {
                warn!(
                    "⚠️ 📚 [lang:{}] {} waited {} ms in queue",
                    addr,
                    operation.name(),
                    waited.as_millis()
                );
            } else {
                debug!(
                    "📚 [lang:{}] {} waited {} ms in queue",
                    addr,
                    operation.name(),
                    waited.as_millis()
                );
            }

            // An `Execute` whose caller has gone (the call deadline, or a dropped future) is
            // not run: nobody would get the result, and the caller may have queued the work
            // again (a commit goes to the pending diffs).
            let skip = {
                let mut running = running.lock().unwrap();
                let skip =
                    matches!(operation, LanguageOperation::Execute(..)) && response_tx.is_closed();
                if !skip {
                    if as_concurrent {
                        running.concurrent.insert(id);
                    } else {
                        running.ordered = Some((id, Instant::now()));
                    }
                }
                skip
            };
            if skip {
                return Finished {
                    id,
                    response_tx,
                    result: Err(format!(
                        "Call to language {} not started: its caller stopped waiting",
                        addr
                    )),
                    is_teardown: false,
                };
            }
            let is_teardown = matches!(operation, LanguageOperation::Teardown);
            let result = self.run(operation, id).await;
            Finished {
                id,
                response_tx,
                result,
                is_teardown,
            }
        })
    }

    async fn run(&self, operation: LanguageOperation, id: u64) -> Result<String, String> {
        match operation {
            LanguageOperation::Execute(script, agent_ctx, _) => {
                self.execute_call(id, &script, agent_ctx).await
            }
            LanguageOperation::LoadModule(path, context) => self
                .load_module(id, &path, &context)
                .await
                .map(|_| String::new()),
            LanguageOperation::LoadLanguage(context) => {
                self.load_language(id, context).await.map(|_| String::new())
            }
            LanguageOperation::RegisterCallbacks => {
                self.register_callbacks(id).await.and_then(|caps| {
                    // Serialize the capability set as a JSON array of kebab-case wire names
                    // so the handle side can parse it back into the same enum.
                    let names: Vec<&'static str> = caps.iter().map(|c| c.wire_name()).collect();
                    serde_json::to_string(&names).map_err(|e| e.to_string())
                })
            }
            LanguageOperation::Teardown => self.teardown(id).await.map(|_| String::new()),
        }
    }

    /// Runs a script call as `agent_ctx`: see "The agent a call acts as" above.
    async fn execute_call(
        &self,
        id: u64,
        script: &str,
        agent_ctx: AgentContext,
    ) -> Result<String, String> {
        register_call_agent(id, agent_ctx);
        // Newlines: a script that ends in a line comment still closes the arrow function.
        let script = format!("globalThis.__ad4mRunCall({id}, async () => (\n{script}\n))");
        let result = self.execute(&script).await;
        // The script takes the agent out first thing; one that did not compile never did.
        forget_call_agent(id);
        result
    }

    /// Clears `done` from `running` and answers its caller. Returns whether it was the
    /// teardown.
    fn finish(&self, done: Finished, running: &RunningRequest, as_concurrent: bool) -> bool {
        let addr = &self.language_address;
        if let Err(ref e) = done.result {
            error!("[lang:{}] Operation failed: {}", addr, e);
        } else {
            debug!("[lang:{}] Operation completed successfully", addr);
        }
        {
            let mut running = running.lock().unwrap();
            if as_concurrent {
                running.concurrent.remove(&done.id);
            } else {
                running.ordered = None;
            }
        }
        let _ = done.response_tx.send(done.result);
        done.is_teardown
    }

    /// Main event loop: process requests from the channel until Teardown.
    ///
    /// Concurrent calls (`CallKind::Concurrent`) start as they arrive and run beside every
    /// other call, up to [`MAX_CONCURRENT_CALLS`] in flight; past the bound the next ones
    /// wait. Ordered calls and lifecycle requests run one at a time, in send order. Loading,
    /// callback registration and teardown also wait for the running calls to end, and a
    /// call sent after one of them waits for it.
    ///
    /// Everything runs on this one task: the V8 event loop has one waker, and the call that
    /// polled it last owns it, so a call on another task that ended would take the wake-ups
    /// of the others with it. In a poll that ends pending the event-loop branch polls last,
    /// with this task's waker.
    pub(crate) async fn process_requests(
        self,
        mut rx: UnboundedReceiver<LanguageRuntimeRequest>,
        running: RunningRequest,
    ) {
        let local_set = tokio::task::LocalSet::new();
        let js_core = self.js_core.clone();
        let this = &self;
        let addr = self.language_address.clone();

        local_set
            .run_until(async move {
                // Requests not started yet, in send order: ordered calls, lifecycle
                // requests, and concurrent calls held back by the bound or a lifecycle
                // request.
                let mut queue: VecDeque<LanguageRuntimeRequest> = VecDeque::new();
                let mut ordered: Option<LocalBoxFuture<'_, Finished>> = None;
                let mut ordered_is_lifecycle = false;
                let mut concurrent: FuturesUnordered<LocalBoxFuture<'_, Finished>> =
                    FuturesUnordered::new();
                let mut closed = false;
                loop {
                    // Start what may start. Ordered and lifecycle requests keep send order:
                    // nothing later starts while one ahead waits, and no call sits beside a
                    // lifecycle request. A concurrent call has no order to keep, so it is
                    // skipped over when it cannot start — held only by the bound or a
                    // lifecycle request ahead of it — and never delays the calls behind it.
                    let mut i = 0;
                    while i < queue.len() {
                        // A call sent concurrent counts as ordered on a WASM
                        // language (`load_module`, `is_wasm_bundle`): nothing
                        // beside it ever runs on the isolate.
                        let is_concurrent = queue[i].operation.is_concurrent() && !this.wasm.get();
                        let is_lifecycle = queue[i].operation.is_lifecycle();
                        let lifecycle_ahead =
                            queue.iter().take(i).any(|r| r.operation.is_lifecycle());
                        let may_start = if is_concurrent {
                            !ordered_is_lifecycle
                                && !lifecycle_ahead
                                && concurrent.len() < MAX_CONCURRENT_CALLS
                        } else {
                            ordered.is_none()
                                && !lifecycle_ahead
                                && (!is_lifecycle || concurrent.is_empty())
                        };
                        if !may_start {
                            i += 1;
                            continue;
                        }
                        let request = queue.remove(i).unwrap();
                        let call = this.begin(request, &running, is_concurrent);
                        if is_concurrent {
                            concurrent.push(call);
                        } else {
                            ordered = Some(call);
                            ordered_is_lifecycle = is_lifecycle;
                        }
                    }
                    if closed && queue.is_empty() && ordered.is_none() && concurrent.is_empty() {
                        // Channel closed (all senders dropped) and every request answered.
                        info!("[lang:{}] Request channel closed, shutting down", addr);
                        return;
                    }

                    tokio::select! {
                        biased;

                        maybe_request = rx.recv(), if !closed => {
                            match maybe_request {
                                Some(request) => {
                                    // A concurrent call starts at once unless a lifecycle
                                    // request runs or waits before it, or the bound holds.
                                    // On a WASM language it waits for the ordered slot like
                                    // an ordered call (`is_wasm_bundle`).
                                    let start_now = request.operation.is_concurrent()
                                        && !this.wasm.get()
                                        && !ordered_is_lifecycle
                                        && concurrent.len() < MAX_CONCURRENT_CALLS
                                        && !queue.iter().any(|r| r.operation.is_lifecycle());
                                    if start_now {
                                        concurrent.push(this.begin(request, &running, true));
                                    } else {
                                        queue.push_back(request);
                                    }
                                }
                                None => closed = true,
                            }
                        }

                        done = async { ordered.as_mut().unwrap().await }, if ordered.is_some() => {
                            ordered = None;
                            ordered_is_lifecycle = false;
                            if this.finish(done, &running, false) {
                                debug!("Teardown complete for language: {}", addr);
                                return;
                            }
                        }

                        Some(done) = concurrent.next(), if !concurrent.is_empty() => {
                            this.finish(done, &running, true);
                        }

                        // Continuously poll V8 event loop for pending JS tasks/ops.
                        // The single place isolate-level errors are reported: an
                        // error a call's future found while driving the loop
                        // arrives through the shared sink (`js_core/futures.rs`).
                        event_loop_result = js_core.event_loop() => {
                            if let Err(err) = event_loop_result {
                                warn!("[lang:{}] Event loop error: {}", addr, err);
                            }
                        }
                    }
                }
            })
            .await;
    }
}
