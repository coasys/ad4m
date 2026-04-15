use crate::agent::AgentContext;
use crate::js_core::JsCore;
use crate::languages::capability::{parse_capability_list, Capability};
use crate::languages::LanguageContext;
use log::{debug, error, info, warn};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use std::collections::HashSet;
use std::path::PathBuf;
use tokio::sync::{mpsc::UnboundedReceiver, oneshot};

// ---------------------------------------------------------------------------
// Per-thread agent context for language runtimes
// ---------------------------------------------------------------------------
//
// CURRENT DESIGN (one language runtime per language, shared across users):
//
// Each language runtime runs in its own dedicated thread and processes
// requests sequentially. When an operation needs to run as a specific user
// (e.g. publishing a managed user's agent profile), the caller passes an
// AgentContext through the channel. Before executing the script, the runtime
// sets this thread-local to the caller's context. The Deno ops (agent_did,
// agent_sign, agent_create_signed_expression, agent_signing_key_id) read
// from this thread-local, so the JS code sees the correct DID and signs
// with the correct key. After execution, the context resets to main_agent().
//
// The JS-side `agentProxy` in language_bootstrap.js uses getters for `did`
// and `signingKeyId` that call back into these ops, so they dynamically
// reflect the current thread-local context rather than caching the init-time
// values.
//
// FUTURE REFACTOR (one language runtime per user per language):
//
// When languages are spawned per user, each runtime would have a fixed
// AgentContext set once at construction time (no need for thread-local
// switching). The changes needed:
//   1. Remove the thread-local and set/get helpers below
//   2. Store AgentContext as a field on LanguageRuntime
//   3. Pass it to JsCore or set it once in process_requests() init
//   4. The agentProxy getters in language_bootstrap.js would still work
//      (they call ops which would read from the runtime's fixed context)
//   5. LanguageController would manage runtimes keyed by (address, user)
//      instead of just address
//   6. execute_on_language_with_context() could route to the correct
//      per-user runtime instead of overriding the thread-local
// ---------------------------------------------------------------------------
thread_local! {
    static CURRENT_AGENT_CONTEXT: RefCell<AgentContext> = RefCell::new(AgentContext::main_agent());
}

/// Set the agent context for the current language runtime thread.
pub fn set_runtime_agent_context(ctx: &AgentContext) {
    CURRENT_AGENT_CONTEXT.with(|c| {
        *c.borrow_mut() = ctx.clone();
    });
}

/// Get the agent context for the current language runtime thread.
pub fn get_runtime_agent_context() -> AgentContext {
    CURRENT_AGENT_CONTEXT.with(|c| c.borrow().clone())
}

/// Request sent to a LanguageRuntime via its channel
#[derive(Debug)]
pub(crate) struct LanguageRuntimeRequest {
    pub operation: LanguageOperation,
    pub response_tx: oneshot::Sender<Result<String, String>>,
}

/// Operations that can be sent to a LanguageRuntime
#[derive(Debug)]
pub(crate) enum LanguageOperation {
    Execute(String, AgentContext),
    LoadModule(String, LanguageContext),
    LoadLanguage(JsonValue),
    RegisterCallbacks,
    Teardown,
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
    /// The bundle is loaded as an ES module via Deno's runtime API and its
    /// full module namespace is captured as `globalThis.languageModule`.
    pub async fn load_module(
        &self,
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

        self.js_core
            .load_module_from_source(&specifier, source.to_string())
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
        self.js_core
            .execute(&capture_script)
            .await
            .map_err(|e| format!("Failed to capture language constructor: {}", e))?;
        Ok(())
    }

    /// Initialize the language with context via the bootstrap's initLanguage()
    pub async fn load_language(&self, language_context: JsonValue) -> Result<(), String> {
        info!("Initializing language: {}", self.language_address);

        let context_json = serde_json::to_string(&language_context)
            .map_err(|e| format!("Failed to serialize language context: {}", e))?;

        let script = format!("await initLanguage({})", context_json);

        match self.js_core.execute(&script).await {
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

    /// Execute a script in this language's runtime
    pub async fn execute(&self, script: &str) -> Result<String, String> {
        self.js_core.execute(script).await
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
    pub async fn register_callbacks(&self) -> Result<HashSet<Capability>, String> {
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

        let _ = self.execute(&callback_script).await?;

        // Detect which capabilities the language actually exports. Returns
        // a JSON array of kebab-case capability names that parse_capability_list
        // converts to the typed enum set. Kept as one v8 round-trip per
        // language load, not per call.
        let detect_script = r#"
            (function() {
                const language = globalThis.__ad4m_language_instance__;
                const caps = [];
                if (!language) return JSON.stringify(caps);
                if (typeof language.expressionCreate === "function"
                    || typeof language.expressionAddressOf === "function"
                    || typeof language.addressOf === "function") {
                    caps.push("expression-create");
                }
                if (typeof language.expressionGet === "function") caps.push("expression-get");
                if (typeof language.perspectiveCommit === "function") caps.push("perspective-commit");
                if (typeof language.perspectiveSyncSync === "function") caps.push("perspective-sync");
                if (typeof language.perspectiveSyncRender === "function") caps.push("perspective-render");
                if (typeof language.perspectiveSyncCurrentRevision === "function") caps.push("perspective-current-revision");
                if (typeof language.peersSetLocal === "function") caps.push("peers-local");
                if (typeof language.peersRemote === "function") caps.push("peers-remote");
                if (typeof language.perspectiveQueryRun === "function") caps.push("perspective-query");
                if (typeof language.telepresenceSetOnlineStatus === "function") caps.push("telepresence-set-status");
                if (typeof language.telepresenceGetOnlineAgents === "function") caps.push("telepresence-get-agents");
                if (typeof language.telepresenceSendSignal === "function") caps.push("telepresence-send-signal");
                if (typeof language.telepresenceSendBroadcast === "function") caps.push("telepresence-send-broadcast");
                if (typeof language.handleHolochainSignal === "function") caps.push("holochain-signal");
                return JSON.stringify(caps);
            })()
        "#;

        let raw = self.execute(detect_script).await?;
        let caps = parse_capability_list(raw.trim());

        info!(
            "Detected capabilities for language {}: {:?}",
            self.language_address, caps
        );

        Ok(caps)
    }

    /// Teardown and cleanup this language runtime
    pub async fn teardown(&self) -> Result<(), String> {
        info!("Tearing down language runtime: {}", self.language_address);

        // Run the language's teardown hook. The bootstrap shim attaches
        // `teardown()` which also runs `teardownWasmImports()`, so invoking
        // it ensures both the language's own cleanup runs and the globalThis
        // host imports are removed.
        let cleanup_script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (!language) return;
                if (typeof language.teardown === "function") {
                    await language.teardown();
                }
            })()
        "#;

        if let Err(e) = self.execute(cleanup_script).await {
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
            .execute("delete globalThis.__ad4m_language_instance__")
            .await;

        info!("Tore down language runtime: {}", self.language_address);
        Ok(())
    }

    /// Main event loop: process requests from the channel until Teardown.
    ///
    /// Uses `tokio::select!` to concurrently poll the V8 event loop and
    /// receive requests, mirroring the pattern in `JsCore::start()`.
    /// Without event loop polling, JS async operations (timers, promises,
    /// pending ops) would stall between requests.
    pub(crate) async fn process_requests(self, mut rx: UnboundedReceiver<LanguageRuntimeRequest>) {
        let local_set = tokio::task::LocalSet::new();
        let js_core = self.js_core.clone();
        let addr = self.language_address.clone();

        local_set.run_until(async move {
            loop {
                tokio::select! {
                    biased;

                    // Poll requests from the channel
                    maybe_request = rx.recv() => {
                        match maybe_request {
                            Some(request) => {
                                let is_teardown = matches!(request.operation, LanguageOperation::Teardown);
                                debug!("[lang:{}] Processing operation: {:?}", addr, request.operation);

                                let result = match request.operation {
                                    LanguageOperation::Execute(script, ref agent_ctx) => {
                                        set_runtime_agent_context(agent_ctx);
                                        let r = self.execute(&script).await;
                                        set_runtime_agent_context(&AgentContext::main_agent());
                                        r
                                    }
                                    LanguageOperation::LoadModule(path, context) => {
                                        self.load_module(&path, &context).await.map(|_| String::new())
                                    }
                                    LanguageOperation::LoadLanguage(context) => {
                                        self.load_language(context).await.map(|_| String::new())
                                    }
                                    LanguageOperation::RegisterCallbacks => {
                                        self.register_callbacks().await.and_then(|caps| {
                                            // Serialize the capability set as a JSON array of
                                            // kebab-case wire names so the handle side can
                                            // parse it back into the same enum.
                                            let names: Vec<&'static str> = caps.iter().map(|c| match c {
                                                Capability::ExpressionCreate => "expression-create",
                                                Capability::ExpressionGet => "expression-get",
                                                Capability::PerspectiveCommit => "perspective-commit",
                                                Capability::PerspectiveSync => "perspective-sync",
                                                Capability::PerspectiveRender => "perspective-render",
                                                Capability::PerspectiveCurrentRevision => "perspective-current-revision",
                                                Capability::PerspectiveQuery => "perspective-query",
                                                Capability::PeersLocal => "peers-local",
                                                Capability::PeersRemote => "peers-remote",
                                                Capability::TelepresenceSetStatus => "telepresence-set-status",
                                                Capability::TelepresenceGetAgents => "telepresence-get-agents",
                                                Capability::TelepresenceSendSignal => "telepresence-send-signal",
                                                Capability::TelepresenceSendBroadcast => "telepresence-send-broadcast",
                                                Capability::HolochainSignal => "holochain-signal",
                                            }).collect();
                                            serde_json::to_string(&names).map_err(|e| e.to_string())
                                        })
                                    }
                                    LanguageOperation::Teardown => {
                                        self.teardown().await.map(|_| String::new())
                                    }
                                };

                                if let Err(ref e) = result {
                                    error!("[lang:{}] Operation failed: {}", addr, e);
                                } else {
                                    debug!("[lang:{}] Operation completed successfully", addr);
                                }

                                let _ = request.response_tx.send(result);

                                if is_teardown {
                                    debug!("Teardown complete for language: {}", addr);
                                    return;
                                }
                            }
                            None => {
                                // Channel closed (all senders dropped), exit
                                info!("[lang:{}] Request channel closed, shutting down", addr);
                                return;
                            }
                        }
                    }

                    // Continuously poll V8 event loop for pending JS tasks/ops
                    event_loop_result = js_core.event_loop() => {
                        match event_loop_result {
                            Ok(_) => {}
                            Err(err) => {
                                warn!("[lang:{}] Event loop error: {}", addr, err);
                            }
                        }
                    }
                }
            }
        }).await;
    }
}
