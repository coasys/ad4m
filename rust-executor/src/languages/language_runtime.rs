use crate::agent::AgentContext;
use crate::js_core::JsCore;
use crate::languages::LanguageContext;
use log::{debug, error, info, warn};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
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
    /// default export is captured as `globalThis.languageConstructor`.
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

        // Capture the default export as globalThis.languageConstructor.
        // Note: execute() wraps scripts in `return (expr)`, so this must be
        // a single expression, not statements.
        let capture_script = format!(
            r#"import("{}").then(m => {{
                const mod = m.default && m.default.default ? m.default.default : m.default || m;
                if (typeof mod === "function") {{
                    // Legacy: default export is create(context) function
                    globalThis.languageConstructor = mod;
                    globalThis.__language_pattern__ = "legacy";
                }} else if (mod && typeof mod.init === "function") {{
                    // New flat export pattern
                    globalThis.languageModule = mod;
                    globalThis.__language_pattern__ = "flat";
                }} else {{
                    // Fallback: assume legacy (object with callable default)
                    globalThis.languageConstructor = mod;
                    globalThis.__language_pattern__ = "legacy";
                }}
            }})"#,
            specifier
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

    /// Register callbacks for links and telepresence adapters
    pub async fn register_callbacks(&self) -> Result<(bool, bool), String> {
        let addr = &self.language_address;

        // Both scripts must tolerate two language shapes:
        //   * Legacy factory languages — register callbacks via
        //     addCallback / addSyncStateChangeCallback / registerSignalCallback.
        //   * Flat languages (spec v1.0) — emit via the host imports
        //     emitPerspectiveDiff / emitSyncStateChange / emitTelepresenceSignal,
        //     which fan out through LANGUAGE_CONTROLLER directly. They
        //     do NOT expose addCallback etc., so calling those methods
        //     unconditionally TypeErrors and the whole register step fails.
        // Guard each callback registration on `typeof === "function"`.
        let links_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (!(language && language.linksAdapter)) return false;
                if (typeof language.linksAdapter.addCallback === "function") {{
                    language.linksAdapter.addCallback((diff) => {{
                        LANGUAGE_CONTROLLER.perspectiveDiffReceived(diff, "{addr}");
                    }});
                }}
                if (typeof language.linksAdapter.addSyncStateChangeCallback === "function") {{
                    language.linksAdapter.addSyncStateChangeCallback((state) => {{
                        LANGUAGE_CONTROLLER.syncStateChanged(state, "{addr}");
                    }});
                }}
                return true;
            }})()
            "#,
        );

        let has_links = self.execute(&links_script).await?.trim() == "true";

        let telepresence_script = format!(
            r#"
            (function() {{
                const language = globalThis.__ad4m_language_instance__;
                if (!(language && language.telepresenceAdapter)) return false;
                if (typeof language.telepresenceAdapter.registerSignalCallback === "function") {{
                    language.telepresenceAdapter.registerSignalCallback((signal, recipientDid) => {{
                        LANGUAGE_CONTROLLER.telepresenceSignalReceived(signal, "{addr}", recipientDid);
                    }});
                }}
                return true;
            }})()
            "#,
        );

        let has_telepresence = self.execute(&telepresence_script).await?.trim() == "true";

        info!(
            "Registered callbacks for language {}: links={}, telepresence={}",
            self.language_address, has_links, has_telepresence
        );

        Ok((has_links, has_telepresence))
    }

    /// Teardown and cleanup this language runtime
    pub async fn teardown(&self) -> Result<(), String> {
        info!("Tearing down language runtime: {}", self.language_address);

        let cleanup_script = r#"
            (async function() {
                const language = globalThis.__ad4m_language_instance__;
                if (language && language.cleanup) {
                    await language.cleanup();
                }
            })()
        "#;

        if let Err(e) = self.execute(cleanup_script).await {
            error!(
                "Error during language cleanup for {}: {}",
                self.language_address, e
            );
        }

        let _ = self
            .execute("delete globalThis.__ad4m_language_instance__;")
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
                                        self.register_callbacks().await.map(|(links, tp)| {
                                            format!("{{\"links\":{},\"telepresence\":{}}}", links, tp)
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
