use log::{debug, error, info};
use serde_json::Value as JsonValue;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tokio::runtime::Builder;
use tokio::sync::{
    mpsc::{self, UnboundedSender},
    oneshot,
};

use crate::agent::AgentContext;
use crate::languages::capability::{parse_capability_list, Capability};
use crate::languages::LanguageContext;

use super::language_runtime::{
    CallKind, LanguageOperation, LanguageRuntime, LanguageRuntimeRequest, RunningRequest,
};

/// How long a script call into a language (`Execute`) may run before the calls behind it stop
/// waiting for it.
///
/// A language serves its ordered calls one at a time, and nothing interrupts a call once it
/// runs: an ordered call that never returns blocks every later ordered call to it. Once the
/// running ordered call has run for the deadline, a caller whose ordered call is still queued
/// gets an error, and a new ordered call fails at once instead of being queued. A read runs
/// beside them; one that has not started by the deadline after it was sent (it waits only
/// behind loading, callback registration or teardown, or a JS CPU loop that holds the thread)
/// gets an error. The runtime never starts a call whose caller has gone, so a
/// caller that got the error can retry without doing the work twice. The running call's own
/// caller waits until it ends: a call that keeps making progress (a file upload with one zome
/// call per chunk) has no total bound, and each of its waits has its own (a zome call 90 s, an
/// HTTP fetch 10 s). The deadline is for hangs, not slow calls: a large encrypted commit can
/// take over 30 s.
pub(crate) const LANGUAGE_CALL_DEADLINE: Duration = Duration::from_secs(60);

/// Ids for `LanguageRuntimeRequest::id`.
static NEXT_REQUEST_ID: AtomicU64 = AtomicU64::new(0);

/// Handle to a per-language runtime running in its own thread.
/// Thin communication layer that sends requests to a LanguageRuntime
/// via a channel.
#[derive(Clone)]
pub struct LanguageRuntimeHandle {
    pub language_address: String,
    pub language_name: Option<String>,
    tx: UnboundedSender<LanguageRuntimeRequest>,
    /// `LANGUAGE_CALL_DEADLINE`.
    call_deadline: Duration,
    /// The request the runtime is running.
    running: RunningRequest,
}

impl LanguageRuntimeHandle {
    /// Spawn a new LanguageRuntime in a dedicated thread.
    /// The storage_directory is the only filesystem path accessible to this sandbox.
    /// System/bootstrap languages additionally get CWD access.
    pub fn spawn(
        language_address: String,
        storage_directory: PathBuf,
        is_system_language: bool,
    ) -> Result<Self, String> {
        info!(
            "Spawning LanguageRuntime for: {} (system={})",
            language_address, is_system_language
        );

        let (tx, rx) = mpsc::unbounded_channel::<LanguageRuntimeRequest>();
        let addr = language_address.clone();
        let prefix_len = 8.min(addr.len());
        let running = RunningRequest::default();
        let runtime_running = running.clone();

        std::thread::Builder::new()
            .name(format!("lang-{}", &addr[..prefix_len]))
            .spawn(move || {
                let rt = Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("Failed to create Tokio runtime for language");

                rt.block_on(async {
                    let runtime =
                        LanguageRuntime::new(addr.clone(), storage_directory, is_system_language);

                    // Execute bootstrap module to make Deno ops available
                    if let Err(e) = runtime.init().await {
                        error!("[lang:{}] Bootstrap failed: {}", addr, e);
                        return;
                    }

                    runtime.process_requests(rx, runtime_running).await;
                });
            })
            .map_err(|e| format!("Failed to spawn language thread: {}", e))?;

        info!("LanguageRuntimeHandle spawned for: {}", language_address);
        Ok(Self {
            language_address,
            language_name: None,
            tx,
            call_deadline: LANGUAGE_CALL_DEADLINE,
            running,
        })
    }

    /// Test-only: a handle with no runtime behind it. The test answers the
    /// requests that arrive on `tx`'s receiver, in whatever order it likes.
    #[cfg(test)]
    pub(crate) fn from_channel(
        language_address: String,
        tx: UnboundedSender<LanguageRuntimeRequest>,
    ) -> Self {
        Self {
            language_address,
            language_name: None,
            tx,
            call_deadline: LANGUAGE_CALL_DEADLINE,
            running: RunningRequest::default(),
        }
    }

    /// Test-only: a shorter deadline, so a test of it does not wait a minute.
    #[cfg(test)]
    pub(crate) fn with_call_deadline(mut self, deadline: Duration) -> Self {
        self.call_deadline = deadline;
        self
    }

    /// Test-only: the running request a `from_channel` test plays the runtime for.
    #[cfg(test)]
    pub(crate) fn running_request(&self) -> RunningRequest {
        self.running.clone()
    }

    /// The error for a call that will not run because the language's running call has run for
    /// the deadline.
    fn busy_error(&self, running_since: Instant) -> String {
        format!(
            "Language {} is busy: its running call has run for {:?}, past the call deadline of {:?}",
            self.language_address,
            running_since.elapsed(),
            self.call_deadline
        )
    }

    /// Waits for the result of call `id`, sent at `queued_at`. While the call is queued, its
    /// caller gets an error at the deadline: an ordered call's once the ordered call running
    /// before it has run for the deadline, a read's (or an ordered call's with none running
    /// before it, its thread held by a read) once it has waited for the deadline. Once the call
    /// runs, its caller waits until it ends.
    async fn wait_for_call(
        &self,
        id: u64,
        kind: CallKind,
        queued_at: Instant,
        mut response_rx: oneshot::Receiver<Result<String, String>>,
    ) -> Result<Result<String, String>, oneshot::error::RecvError> {
        loop {
            let check_at = {
                let running = self.running.lock().unwrap();
                if running.ordered.map(|(running_id, _)| running_id) == Some(id)
                    || running.reads.contains(&id)
                {
                    break;
                }
                let (error, limit) = match (kind, running.ordered) {
                    (CallKind::Ordered, Some((_, since))) => {
                        (self.busy_error(since), since + self.call_deadline)
                    }
                    _ => (
                        format!(
                            "Language {} did not start the call within the call deadline of {:?}",
                            self.language_address, self.call_deadline
                        ),
                        queued_at + self.call_deadline,
                    ),
                };
                if Instant::now() >= limit {
                    // Dropped under the lock the runtime starts a call under, so the runtime
                    // either has started this call (and the check above broke out) or will
                    // skip it.
                    drop(response_rx);
                    return Ok(Err(error));
                }
                limit
            };
            tokio::select! {
                biased;
                response = &mut response_rx => return response,
                _ = tokio::time::sleep_until(check_at.into()) => {}
            }
        }
        response_rx.await
    }

    /// Send an operation and wait for the result.
    async fn send(&self, operation: LanguageOperation) -> Result<String, String> {
        let op_name = format!("{:?}", operation);
        let (response_tx, response_rx) = oneshot::channel();

        debug!(
            "[handle:{}] Sending operation: {}",
            self.language_address,
            &op_name[..op_name.len().min(100)]
        );

        // Only script calls give up: loading a language and its teardown wait for it.
        let kind = match &operation {
            LanguageOperation::Execute(_, _, kind) => Some(*kind),
            _ => None,
        };
        if kind == Some(CallKind::Ordered) {
            // Not queued behind a call that has run past the deadline: retries during a hang
            // would pile up in the channel. A read does not wait for that call.
            if let Some((_, since)) = self.running.lock().unwrap().ordered {
                if since.elapsed() >= self.call_deadline {
                    return Err(self.busy_error(since));
                }
            }
        }
        let id = NEXT_REQUEST_ID.fetch_add(1, Ordering::Relaxed);
        let queued_at = Instant::now();
        self.tx
            .send(LanguageRuntimeRequest {
                operation,
                response_tx,
                queued_at,
                id,
            })
            .map_err(|e| format!("Language runtime channel closed: {}", e))?;

        let response = if let Some(kind) = kind {
            self.wait_for_call(id, kind, queued_at, response_rx).await
        } else {
            response_rx.await
        };
        let result =
            response.map_err(|e| format!("Language runtime dropped without responding: {}", e))?;

        match &result {
            Ok(_) => debug!("[handle:{}] Operation completed OK", self.language_address),
            Err(e) => error!("[handle:{}] Operation failed: {}", self.language_address, e),
        }

        result
    }

    pub async fn execute(&self, script: String) -> Result<String, String> {
        self.execute_with_context(script, AgentContext::main_agent())
            .await
    }

    /// An ordered call as `agent_context`: it starts once the ordered calls sent before it
    /// have ended (`CallKind::Ordered`).
    pub async fn execute_with_context(
        &self,
        script: String,
        agent_context: AgentContext,
    ) -> Result<String, String> {
        self.send(LanguageOperation::Execute(
            script,
            agent_context,
            CallKind::Ordered,
        ))
        .await
    }

    pub async fn execute_read(&self, script: String) -> Result<String, String> {
        self.execute_read_with_context(script, AgentContext::main_agent())
            .await
    }

    /// A read as `agent_context`: it starts at once and runs beside the language's other calls
    /// (`CallKind::Read`). Only for a call that changes no state a later call depends on.
    pub async fn execute_read_with_context(
        &self,
        script: String,
        agent_context: AgentContext,
    ) -> Result<String, String> {
        self.send(LanguageOperation::Execute(
            script,
            agent_context,
            CallKind::Read,
        ))
        .await
    }

    pub async fn load_module(&self, path: String, context: LanguageContext) -> Result<(), String> {
        self.send(LanguageOperation::LoadModule(path, context))
            .await
            .map(|_| ())
    }

    pub async fn load_language(&self, context: JsonValue) -> Result<(), String> {
        self.send(LanguageOperation::LoadLanguage(context))
            .await
            .map(|_| ())
    }

    pub async fn register_callbacks(&self) -> Result<HashSet<Capability>, String> {
        let result_str = self.send(LanguageOperation::RegisterCallbacks).await?;
        Ok(parse_capability_list(result_str.trim()))
    }

    pub async fn teardown(&self) -> Result<(), String> {
        self.send(LanguageOperation::Teardown).await.map(|_| ())
    }

    /// Query the language name from the JS runtime after initLanguage has run.
    pub async fn query_language_name(&mut self) -> Option<String> {
        // Wrap in JSON.stringify so the result round-trips through a
        // well-formed JSON string, not a raw v8 `to_rust_string_lossy`
        // capture. Also tolerate the Rust ALDK shape where `name` is an
        // exported zero-arg function (wasm-bindgen cannot export string
        // constants): call it when it's a function, read the value when
        // it's a string. The previous `trim().trim_matches('"')` path
        // mangled any name containing a `"` or leading/trailing
        // whitespace — rare for language names but trivial to fix
        // correctly via `serde_json::from_str`.
        let script = r#"
            JSON.stringify(
                (() => {
                    const l = globalThis.__ad4m_language_instance__;
                    if (!l) return "";
                    const n = l.name;
                    if (typeof n === "function") return String(n() ?? "");
                    return String(n ?? "");
                })()
            )
        "#
        .to_string();
        match self.execute(script).await {
            Ok(result) => {
                let name: String = serde_json::from_str(result.trim()).ok()?;
                if name.is_empty() {
                    None
                } else {
                    self.language_name = Some(name.clone());
                    Some(name)
                }
            }
            Err(_) => None,
        }
    }

    /// Return a human-readable label: "name (address)" or just "address"
    pub fn label(&self) -> String {
        match &self.language_name {
            Some(name) => format!("{} ({})", name, self.language_address),
            None => self.language_address.clone(),
        }
    }
}
