use crate::agent::AgentContext;
use crate::js_core::agent_extension::{forget_call_agent, register_call_agent};
use crate::js_core::JsCore;
use crate::languages::capability::{detection_script, parse_capability_list, Capability};
use crate::languages::LanguageContext;
use futures::future::LocalBoxFuture;
use futures::stream::{FuturesUnordered, StreamExt};
use log::{debug, error, info, warn};
use serde_json::Value as JsonValue;
use std::collections::{HashSet, VecDeque};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::sync::{mpsc::UnboundedReceiver, oneshot};
use tokio::time::Instant;

// ---------------------------------------------------------------------------
// The agent a call acts as
// ---------------------------------------------------------------------------
//
// One language runtime serves every user of the executor, and calls into one
// language overlap, so the agent a call acts as travels with the call, not
// with the thread: see `js_core/agent_extension.rs`, "The agent a call acts
// as".

/// Request sent to a LanguageRuntime via its channel
#[derive(Debug)]
pub(crate) struct LanguageRuntimeRequest {
    pub operation: LanguageOperation,
    pub response_tx: oneshot::Sender<Result<String, String>>,
    /// When the handle sent it; the runtime logs the time in queue on dequeue (#1133).
    pub queued_at: Instant,
    /// Names the request in `RunningRequest`.
    pub id: u64,
    /// Set by the runtime when it starts the call: the caller splits its wait into time
    /// queued and time running (`call_diag::phase`).
    pub started: Arc<std::sync::OnceLock<Instant>>,
}

impl LanguageRuntimeRequest {
    /// An `Execute` whose caller stopped waiting: `begin` will not run it and a
    /// queued one is pruned. A lifecycle request is never gone — its caller
    /// cannot stop waiting for it.
    fn caller_gone(&self) -> bool {
        matches!(self.operation, LanguageOperation::Execute(..)) && self.response_tx.is_closed()
    }
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

/// The calls sent to a runtime that it has not started yet: id → when sent.
/// For the queue report (`queue_report`).
pub(crate) type QueuedRequests = Arc<Mutex<std::collections::BTreeMap<u64, Instant>>>;

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
        }
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
    /// The bundle is registered as an ES module and evaluated under the main
    /// agent's token (`JsCore::load_module_from_source`), then its full module
    /// namespace is captured as `globalThis.languageModule`.
    pub async fn load_module(
        &self,
        id: u64,
        source: &str,
        language_context: &LanguageContext,
    ) -> Result<(), String> {
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

        // Evaluated as the main agent, so the bundle's top-level code and the
        // timers it starts act as the main agent. A bundle whose top-level
        // await never settles fails the load, and the isolate keeps reporting
        // the stall: `LanguageController` drops the handle on the error, the
        // channel closes and this runtime's thread ends.
        self.js_core
            .load_module_from_source(
                &specifier,
                source.to_string(),
                id,
                AgentContext::main_agent(),
            )
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
    /// `begin` did not run it — the caller had already gone — so the error is
    /// a skip, not a failure (logged at debug, LOGGING.md).
    skipped: bool,
    /// `begin` ran it as a concurrent call (the operation's `is_concurrent`):
    /// `finish` clears its marker from `running.concurrent`, not `ordered`.
    concurrent: bool,
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
        queued: &QueuedRequests,
    ) -> LocalBoxFuture<'a, Finished> {
        let running = running.clone();
        let queued = queued.clone();
        Box::pin(async move {
            let is_concurrent = request.operation.is_concurrent();
            queued.lock().unwrap().remove(&request.id);
            // An `Execute` whose caller has gone (the call deadline, or a dropped future) is
            // not run: nobody would get the result, and the caller may have queued the work
            // again (a commit goes to the pending diffs).
            let skip = {
                let mut running = running.lock().unwrap();
                let skip = request.caller_gone();
                if !skip {
                    let now = Instant::now();
                    if is_concurrent {
                        running.concurrent.insert(request.id);
                    } else {
                        running.ordered = Some((request.id, now));
                    }
                    let _ = request.started.set(now);
                }
                skip
            };
            let LanguageRuntimeRequest {
                operation,
                response_tx,
                queued_at,
                id,
                started: _,
            } = request;
            let addr = &self.language_address;
            debug!("[lang:{}] Processing operation: {}", addr, operation.name());
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

            if skip {
                return Finished {
                    id,
                    response_tx,
                    result: Err(format!(
                        "Call to language {} not started: its caller stopped waiting",
                        addr
                    )),
                    is_teardown: false,
                    skipped: true,
                    concurrent: is_concurrent,
                };
            }
            let is_teardown = matches!(operation, LanguageOperation::Teardown);
            let result = self.run(operation, id).await;
            Finished {
                id,
                response_tx,
                result,
                is_teardown,
                skipped: false,
                concurrent: is_concurrent,
            }
        })
    }

    async fn run(&self, operation: LanguageOperation, id: u64) -> Result<String, String> {
        match operation {
            LanguageOperation::Execute(script, agent_ctx, _) => {
                self.execute_call(id, &script, agent_ctx).await
            }
            LanguageOperation::LoadModule(source, context) => self
                .load_module(id, &source, &context)
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

    /// Clears `done` from `running` and answers its caller — under the one lock, so a
    /// caller whose wait passes its deadline just then finds the result already sent
    /// rather than a stale "not started". Returns whether it was the teardown.
    fn finish(&self, done: Finished, running: &RunningRequest) -> bool {
        let addr = &self.language_address;
        match &done.result {
            Err(e) if done.skipped => debug!("[lang:{}] {}", addr, e),
            Err(e) => error!("[lang:{}] Operation failed: {}", addr, e),
            Ok(_) => debug!("[lang:{}] Operation completed successfully", addr),
        }
        {
            let mut running = running.lock().unwrap();
            if done.concurrent {
                running.concurrent.remove(&done.id);
            } else {
                running.ordered = None;
            }
            let _ = done.response_tx.send(done.result);
        }
        done.is_teardown
    }

    /// Main event loop: process requests from the channel until Teardown, or
    /// until the channel closes (every handle dropped: no caller is left, so
    /// work still in flight is dropped with the isolate rather than drained).
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
        queued: QueuedRequests,
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
                    // A call whose caller has gone would be skipped by `begin`
                    // anyway; prune it here so it never occupies a queue slot.
                    // A lifecycle request always stays: its caller cannot stop
                    // waiting for it.
                    queue.retain(|r| !r.caller_gone());
                    // Start what may start. Ordered and lifecycle requests keep send order:
                    // nothing later starts while one ahead waits, and no call sits beside a
                    // lifecycle request. A concurrent call has no order to keep, so it is
                    // skipped over when it cannot start — held only by the bound or a
                    // lifecycle request ahead of it — and never delays the calls behind it.
                    // `lifecycle_ahead` follows the scan: it counts the requests still
                    // queued ahead of the candidate.
                    let mut lifecycle_ahead = false;
                    let mut i = 0;
                    while i < queue.len() {
                        let is_concurrent = queue[i].operation.is_concurrent();
                        let is_lifecycle = queue[i].operation.is_lifecycle();
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
                            lifecycle_ahead |= is_lifecycle;
                            i += 1;
                            continue;
                        }
                        let request = queue.remove(i).unwrap();
                        let call = this.begin(request, &running, &queued);
                        if is_concurrent {
                            concurrent.push(call);
                        } else {
                            ordered = Some(call);
                            ordered_is_lifecycle = is_lifecycle;
                        }
                    }
                    if closed && queue.is_empty() {
                        // Channel closed: every handle is gone, so nothing is
                        // left that could wait on a call still in flight —
                        // return rather than drain. A call that can never end
                        // (a promise nothing settles, a stalled import())
                        // otherwise keeps this thread alive for ever.
                        info!("[lang:{}] Request channel closed, shutting down", addr);
                        return;
                    }

                    tokio::select! {
                        biased;

                        maybe_request = rx.recv(), if !closed => {
                            match maybe_request {
                                // Queued: the next pass of the loop starts it — or
                                // not — by the same rule as the requests already
                                // waiting.
                                Some(request) => queue.push_back(request),
                                None => closed = true,
                            }
                        }

                        done = async { ordered.as_mut().unwrap().await }, if ordered.is_some() => {
                            ordered = None;
                            ordered_is_lifecycle = false;
                            if this.finish(done, &running) {
                                debug!("Teardown complete for language: {}", addr);
                                return;
                            }
                        }

                        Some(done) = concurrent.next(), if !concurrent.is_empty() => {
                            this.finish(done, &running);
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
