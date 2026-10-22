//! A sandbox for a tool bundle: one V8 isolate on its own thread, with nothing to reach but its
//! own code.
//!
//! A Language runs in a full `MainWorker` with network, a storage directory and the executor's
//! extensions, because it has to talk to peers and sign. A tool bundle only computes: it is given
//! JSON and answers with JSON. So it gets a bare `JsRuntime` with one extension of two ops —
//! random bytes and a log line — and the globals `bootstrap.js` builds on them. There is no
//! filesystem, network, timer, agent or service op registered in the isolate, so there is nothing
//! a bundle could reach by finding a way round the globals.
//!
//! Every call has a time limit, and so does loading the bundle. The isolate has a memory cap, on
//! its V8 heap and on the ArrayBuffer memory it holds outside it. Either limit being hit terminates
//! the isolate; the call that hit it fails, and the next call starts a fresh one from the same
//! source. A bundle holds no state a caller may rely on between calls, so nothing is lost but
//! warm-up.
//!
//! The time limit belongs to the call, not to whoever waits on it: a call runs as a task of its
//! own, so a caller that stops waiting does not take the limit with it.

use deno_core::{op2, v8, JsRuntime, ModuleSpecifier, PollEventLoopOptions, RuntimeOptions};
use log::{debug, error, info, warn};
use rand::RngCore;
use serde_json::Value;
use std::alloc::Layout;
use std::cell::Cell;
use std::ffi::c_void;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tokio::sync::{mpsc, oneshot, Mutex};

const BOOTSTRAP: &str = include_str!("bootstrap.js");

/// How much a bundle may use before it is stopped.
#[derive(Debug, Clone, Copy)]
pub struct SandboxLimits {
    /// How long one call may run, from being sent to being answered, and how long the bundle
    /// may take to load.
    pub call_timeout: Duration,
    /// The most V8 heap the isolate may hold, and separately the most ArrayBuffer memory.
    pub max_heap_bytes: usize,
}

impl Default for SandboxLimits {
    /// Generous for the bundles in use: the heaviest call measured so far, an edit to a 3,600-node
    /// record, peaks at a few hundred megabytes and under a second.
    fn default() -> Self {
        Self {
            call_timeout: Duration::from_secs(30),
            max_heap_bytes: 1024 * 1024 * 1024,
        }
    }
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum SandboxError {
    /// The source did not load: a syntax error, an import, or a throw while it was evaluated.
    #[error("the bundle could not be loaded: {0}")]
    Load(String),
    /// The bundle threw, rejected, or has nothing callable at the path asked for.
    #[error("{0}")]
    Js(String),
    #[error("the call took longer than {0:?} and was stopped")]
    Timeout(Duration),
    #[error("the bundle used more than {0} bytes of memory and was stopped")]
    OutOfMemory(usize),
    /// The isolate's thread is gone, or could not be started.
    #[error("the bundle's sandbox is not running: {0}")]
    Closed(String),
}

/// One request to the isolate: read what is at an export path (`args` absent), or call it.
struct Request {
    path: Vec<String>,
    args: Option<String>,
    reply: oneshot::Sender<Result<String, SandboxError>>,
}

/// A running isolate, as the handle holds it.
struct Live {
    tx: mpsc::UnboundedSender<Request>,
    /// What stops the isolate from outside its thread.
    isolate: v8::IsolateHandle,
}

/// A bundle in a sandbox: cheap to clone, safe to share, and the only way to reach the isolate.
#[derive(Clone)]
pub struct BundleSandbox {
    name: String,
    source: Arc<str>,
    limits: SandboxLimits,
    live: Arc<Mutex<Option<Live>>>,
    /// One per isolate thread started and maybe not yet ended: closed when that thread ends.
    threads: Arc<std::sync::Mutex<Vec<oneshot::Receiver<()>>>>,
}

impl BundleSandbox {
    /// Start a sandbox and load `source`, an ES module, into it. Fails if the module does not
    /// load. `name` is for logs and the module's URL, not an identity.
    pub async fn start(
        name: impl Into<String>,
        source: impl Into<Arc<str>>,
        limits: SandboxLimits,
    ) -> Result<Self, SandboxError> {
        let sandbox = Self {
            name: name.into(),
            source: source.into(),
            limits,
            live: Arc::new(Mutex::new(None)),
            threads: Arc::default(),
        };
        let live = spawn_isolate(
            &sandbox.name,
            sandbox.source.clone(),
            limits,
            &sandbox.threads,
        )
        .await?;
        *sandbox.live.lock().await = Some(live);
        Ok(sandbox)
    }

    /// Call the function the bundle exports at `path` (`["toolsets", "template", "call"]`) with
    /// `args`, as the object it was found on, and await what it returns. Answers with the result
    /// as JSON; `undefined` comes back as null.
    pub async fn call(&self, path: &[&str], args: Vec<Value>) -> Result<Value, SandboxError> {
        let args = serde_json::to_string(&args).map_err(|e| SandboxError::Js(e.to_string()))?;
        self.send(path, Some(args)).await
    }

    /// The value the bundle exports at `path`, as JSON. Functions inside it are left out, as
    /// `JSON.stringify` leaves them.
    pub async fn read(&self, path: &[&str]) -> Result<Value, SandboxError> {
        self.send(path, None).await
    }

    /// Stop the isolate and wait until every isolate thread this sandbox started has ended, so
    /// the memory they held is released. Waits for a call still running to finish or be stopped
    /// at its time limit. A call made afterwards starts a fresh isolate.
    pub async fn close(&self) {
        // Without its queue, an isolate's thread ends once it has answered what it was given.
        self.live.lock().await.take();
        let threads = std::mem::take(&mut *self.threads.lock().expect("never poisoned"));
        for ended in threads {
            let _ = ended.await;
        }
    }

    async fn send(&self, path: &[&str], args: Option<String>) -> Result<Value, SandboxError> {
        let path: Vec<String> = path.iter().map(|p| p.to_string()).collect();
        // Dropped with this future: how the call learns, before it reaches the isolate, that
        // nobody is waiting for it any more.
        let (_waiting, caller) = oneshot::channel::<()>();
        let sandbox = self.clone();
        tokio::spawn(async move { sandbox.run_call(path, args, caller).await })
            .await
            .unwrap_or_else(|e| Err(SandboxError::Closed(format!("the call's task failed: {e}"))))
    }

    /// One call, start to end, on a task of its own: its time limit is enforced even if the
    /// caller is dropped while it runs.
    async fn run_call(
        &self,
        path: Vec<String>,
        args: Option<String>,
        mut caller: oneshot::Receiver<()>,
    ) -> Result<Value, SandboxError> {
        // Held for the whole call: the isolate runs one call at a time anyway, and a call that is
        // terminated must not let the next one reach the dying isolate.
        let mut live = self.live.lock().await;
        if matches!(caller.try_recv(), Err(oneshot::error::TryRecvError::Closed)) {
            return Err(SandboxError::Closed(
                "the caller stopped waiting before the call began".into(),
            ));
        }
        if live.is_none() {
            info!("[tool-bundle:{}] starting a fresh isolate", self.name);
            *live = Some(
                spawn_isolate(&self.name, self.source.clone(), self.limits, &self.threads).await?,
            );
        }
        let current = live.as_ref().expect("set above");

        let (reply, answer) = oneshot::channel();
        let request = Request {
            path: path.clone(),
            args,
            reply,
        };
        if current.tx.send(request).is_err() {
            *live = None;
            return Err(SandboxError::Closed("the isolate stopped".into()));
        }

        let outcome = match tokio::time::timeout(self.limits.call_timeout, answer).await {
            Ok(Ok(outcome)) => outcome,
            Ok(Err(_)) => Err(SandboxError::Closed("the isolate stopped mid-call".into())),
            Err(_) => {
                warn!(
                    "[tool-bundle:{}] call to {} ran past {:?}; terminating",
                    self.name,
                    path.join("."),
                    self.limits.call_timeout
                );
                current.isolate.terminate_execution();
                Err(SandboxError::Timeout(self.limits.call_timeout))
            }
        };

        // An isolate that was terminated, ran out of memory or lost its thread is not used again.
        if matches!(
            outcome,
            Err(SandboxError::Timeout(_) | SandboxError::OutOfMemory(_) | SandboxError::Closed(_))
        ) {
            *live = None;
        }
        let text = outcome?;
        serde_json::from_str(&text).map_err(|e| SandboxError::Js(format!("unreadable answer: {e}")))
    }
}

/// Start an isolate on a thread of its own and load the bundle into it. Answers once the bundle
/// has loaded, or why it did not. Loading has the call time limit, and an isolate whose start is
/// given up on is terminated, so bundle code never runs without a limit on it.
async fn spawn_isolate(
    name: &str,
    source: Arc<str>,
    limits: SandboxLimits,
    threads: &std::sync::Mutex<Vec<oneshot::Receiver<()>>>,
) -> Result<Live, SandboxError> {
    let (ended_tx, ended_rx) = oneshot::channel::<()>();
    {
        let mut threads = threads.lock().expect("never poisoned");
        // Forget the threads that have ended; a sender is only ever dropped, never used.
        threads.retain_mut(|t| matches!(t.try_recv(), Err(oneshot::error::TryRecvError::Empty)));
        threads.push(ended_rx);
    }
    let (tx, rx) = mpsc::unbounded_channel::<Request>();
    let (handle_tx, handle_rx) = oneshot::channel::<v8::IsolateHandle>();
    let (ready_tx, ready_rx) = oneshot::channel::<Result<(), SandboxError>>();
    let thread_name = format!("bundle-{}", name.chars().take(12).collect::<String>());
    let thread_bundle = name.to_string();

    std::thread::Builder::new()
        .name(thread_name)
        .spawn(move || {
            // Dropped last, when everything else on this thread is gone: how `close` learns the
            // thread has ended.
            let _ended = ended_tx;
            #[cfg(test)]
            let _running = running::Thread::start(&thread_bundle);
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(e) => {
                    let _ = ready_tx.send(Err(SandboxError::Closed(e.to_string())));
                    return;
                }
            };
            rt.block_on(run_isolate(
                thread_bundle,
                source,
                limits,
                handle_tx,
                ready_tx,
                rx,
            ));
        })
        .map_err(|e| SandboxError::Closed(format!("could not start a thread: {e}")))?;

    let ended = || SandboxError::Closed("the isolate's thread ended while loading".into());
    // Published before any bundle code runs, and held so that from here on a dropped start
    // terminates the load.
    let isolate = TerminateOnDrop(Some(handle_rx.await.map_err(|_| ended())?));
    match tokio::time::timeout(limits.call_timeout, ready_rx).await {
        Ok(Ok(Ok(()))) => {}
        Ok(Ok(Err(e))) => return Err(e),
        Ok(Err(_)) => return Err(ended()),
        Err(_) => {
            warn!(
                "[tool-bundle:{}] loading ran past {:?}; terminating",
                name, limits.call_timeout
            );
            return Err(SandboxError::Timeout(limits.call_timeout));
        }
    }
    Ok(Live {
        tx,
        isolate: isolate.disarm(),
    })
}

/// Under test: how many isolate threads are running per sandbox name, counted by the threads
/// themselves, so a test can see a worker end on any platform.
#[cfg(test)]
pub(super) mod running {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};

    fn threads() -> &'static Mutex<HashMap<String, usize>> {
        static THREADS: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();
        THREADS.get_or_init(Default::default)
    }

    /// The isolate threads of the sandbox called `name` that have not ended yet.
    pub fn count(name: &str) -> usize {
        threads().lock().unwrap().get(name).copied().unwrap_or(0)
    }

    /// Counts its thread as running until dropped, which is when the thread ends.
    pub(super) struct Thread(String);

    impl Thread {
        pub(super) fn start(name: &str) -> Self {
            *threads()
                .lock()
                .unwrap()
                .entry(name.to_string())
                .or_default() += 1;
            Self(name.to_string())
        }
    }

    impl Drop for Thread {
        fn drop(&mut self) {
            if let Some(n) = threads().lock().unwrap().get_mut(&self.0) {
                *n -= 1;
            }
        }
    }
}

/// Terminates the isolate when dropped, unless disarmed.
struct TerminateOnDrop(Option<v8::IsolateHandle>);

impl TerminateOnDrop {
    fn disarm(mut self) -> v8::IsolateHandle {
        self.0.take().expect("armed until disarmed")
    }
}

impl Drop for TerminateOnDrop {
    fn drop(&mut self) {
        if let Some(isolate) = self.0.take() {
            isolate.terminate_execution();
        }
    }
}

async fn run_isolate(
    name: String,
    source: Arc<str>,
    limits: SandboxLimits,
    handle: oneshot::Sender<v8::IsolateHandle>,
    ready: oneshot::Sender<Result<(), SandboxError>>,
    mut rx: mpsc::UnboundedReceiver<Request>,
) {
    let buffers = Arc::new(BufferBudget::new(limits.max_heap_bytes));
    let mut runtime = JsRuntime::new(RuntimeOptions {
        extensions: vec![tool_bundle_host::init()],
        create_params: Some(
            v8::Isolate::create_params()
                .heap_limits(0, limits.max_heap_bytes)
                .array_buffer_allocator(buffers.allocator()),
        ),
        ..Default::default()
    });
    let _ = buffers
        .isolate
        .set(runtime.v8_isolate().thread_safe_handle());

    // Out of memory is a termination too, but reported as itself. The headroom granted is what
    // lets V8 unwind the terminated script instead of aborting the process.
    let out_of_memory = Rc::new(Cell::new(false));
    {
        let handle = runtime.v8_isolate().thread_safe_handle();
        let flag = out_of_memory.clone();
        runtime.add_near_heap_limit_callback(move |current, _initial| {
            flag.set(true);
            handle.terminate_execution();
            current * 2
        });
    }

    // Before any bundle code runs, so its load can be stopped too. Nobody to stop it for means
    // nobody waiting for it: then it is not loaded at all.
    if handle
        .send(runtime.v8_isolate().thread_safe_handle())
        .is_err()
    {
        return;
    }

    let over_cap = || out_of_memory.get() || buffers.exceeded.load(Ordering::SeqCst);
    let loaded = match load(&mut runtime, &name, &source).await {
        _ if over_cap() => Err(SandboxError::OutOfMemory(limits.max_heap_bytes)),
        loaded => loaded,
    };
    let (invoker, namespace) = match loaded {
        Ok(loaded) => loaded,
        Err(e) => {
            error!("[tool-bundle:{}] {}", name, e);
            let _ = ready.send(Err(e));
            return;
        }
    };
    if ready.send(Ok(())).is_err() {
        return;
    }
    debug!("[tool-bundle:{}] loaded", name);

    while let Some(request) = rx.recv().await {
        let outcome = invoke(&mut runtime, &invoker, &namespace, &request).await;
        let terminated = runtime.v8_isolate().is_execution_terminating() || over_cap();
        // Over the cap is a failure even if the bundle caught the error it got and answered.
        let outcome = match outcome {
            _ if over_cap() => Err(SandboxError::OutOfMemory(limits.max_heap_bytes)),
            other => other,
        };
        let _ = request.reply.send(outcome);
        if terminated {
            // A terminated isolate is not trusted with another call; the handle starts a new one.
            debug!("[tool-bundle:{}] isolate terminated; thread ending", name);
            return;
        }
    }
}

/// Run the bootstrap, then load and evaluate the bundle. Answers with the invoker `bootstrap.js`
/// evaluates to, and the bundle's module namespace.
async fn load(
    runtime: &mut JsRuntime,
    name: &str,
    source: &str,
) -> Result<(v8::Global<v8::Function>, v8::Global<v8::Value>), SandboxError> {
    let invoker = runtime
        .execute_script("tool-bundle:bootstrap", BOOTSTRAP.to_string())
        .map_err(|e| SandboxError::Load(format!("bootstrap: {e}")))?;
    let invoker = {
        deno_core::scope!(scope, runtime);
        let local = v8::Local::new(scope, invoker);
        let function = v8::Local::<v8::Function>::try_from(local)
            .map_err(|_| SandboxError::Load("bootstrap did not evaluate to a function".into()))?;
        v8::Global::new(scope, function)
    };

    let specifier = ModuleSpecifier::parse(&format!("tool-bundle:///{name}.js"))
        .map_err(|e| SandboxError::Load(e.to_string()))?;
    let module = runtime
        .load_main_es_module_from_code(&specifier, source.to_string())
        .await
        .map_err(|e| SandboxError::Load(e.to_string()))?;
    let evaluated = runtime.mod_evaluate(module);
    runtime
        .run_event_loop(PollEventLoopOptions::default())
        .await
        .map_err(|e| SandboxError::Load(e.to_string()))?;
    evaluated
        .await
        .map_err(|e| SandboxError::Load(e.to_string()))?;

    let namespace = runtime
        .get_module_namespace(module)
        .map_err(|e| SandboxError::Load(e.to_string()))?;
    let namespace = {
        deno_core::scope!(scope, runtime);
        let local: v8::Local<v8::Value> = v8::Local::new(scope, namespace).into();
        v8::Global::new(scope, local)
    };
    Ok((invoker, namespace))
}

/// Pass one request through the invoker and run the event loop until it settles.
async fn invoke(
    runtime: &mut JsRuntime,
    invoker: &v8::Global<v8::Function>,
    namespace: &v8::Global<v8::Value>,
    request: &Request,
) -> Result<String, SandboxError> {
    let args = {
        deno_core::scope!(scope, runtime);
        let keys: Vec<v8::Local<v8::Value>> = request
            .path
            .iter()
            .map(|key| v8::String::new(scope, key).expect("path key").into())
            .collect();
        let path: v8::Local<v8::Value> = v8::Array::new_with_elements(scope, &keys).into();
        let mut args = vec![namespace.clone(), v8::Global::new(scope, path)];
        if let Some(json) = &request.args {
            let json: v8::Local<v8::Value> = v8::String::new(scope, json)
                .ok_or_else(|| SandboxError::Js("arguments too large".into()))?
                .into();
            args.push(v8::Global::new(scope, json));
        }
        args
    };

    let call = runtime.call_with_args(invoker, &args);
    let value = runtime
        .with_event_loop_promise(call, PollEventLoopOptions::default())
        .await
        .map_err(|e| SandboxError::Js(e.to_string()))?;

    deno_core::scope!(scope, runtime);
    let local = v8::Local::new(scope, value);
    Ok(local.to_rust_string_lossy(scope))
}

#[op2(fast)]
fn op_tool_bundle_random_values(#[buffer] buffer: &mut [u8]) {
    rand::thread_rng().fill_bytes(buffer);
}

#[op2(fast)]
fn op_tool_bundle_log(#[smi] level: u8, #[string] message: &str) {
    let (level, line) = log_line(level, message);
    log::log!(level, "{}", line);
}

/// The most of one log message a bundle gets into the node's log.
const LOG_MESSAGE_CHARS: usize = 2000;

/// What a bundle's `console` call writes to the node's log. A bundle does not choose how loud the
/// node's log is: `console.debug` is a trace line and every other level a debug line naming it,
/// cut to [`LOG_MESSAGE_CHARS`].
pub(super) fn log_line(level: u8, message: &str) -> (log::Level, String) {
    let message = match message.char_indices().nth(LOG_MESSAGE_CHARS) {
        Some((cut, _)) => format!("{}…", &message[..cut]),
        None => message.to_string(),
    };
    match level {
        0 => (log::Level::Trace, format!("[tool-bundle] {message}")),
        1 => (log::Level::Debug, format!("[tool-bundle] info: {message}")),
        2 => (log::Level::Debug, format!("[tool-bundle] warn: {message}")),
        _ => (log::Level::Debug, format!("[tool-bundle] error: {message}")),
    }
}

/// The ArrayBuffer memory an isolate holds, which its heap limit does not count: every backing
/// store V8 asks the isolate's allocator for comes from here, and past the limit it gets none and
/// the isolate is terminated.
struct BufferBudget {
    limit: usize,
    used: AtomicUsize,
    exceeded: AtomicBool,
    /// The isolate to terminate, set once it exists.
    isolate: OnceLock<v8::IsolateHandle>,
}

impl BufferBudget {
    fn new(limit: usize) -> Self {
        Self {
            limit,
            used: AtomicUsize::new(0),
            exceeded: AtomicBool::new(false),
            isolate: OnceLock::new(),
        }
    }

    /// The allocator to give the isolate. It holds the budget until V8 drops it.
    fn allocator(self: &Arc<Self>) -> v8::UniqueRef<v8::Allocator> {
        static VTABLE: v8::RustAllocatorVtable<BufferBudget> = v8::RustAllocatorVtable {
            allocate: budget_allocate,
            allocate_uninitialized: budget_allocate_uninitialized,
            free: budget_free,
            drop: budget_drop,
        };
        // SAFETY: the pointer is a strong count of this Arc, released by `budget_drop` when V8
        // drops the allocator, so the budget outlives every call V8 makes through it.
        unsafe { v8::new_rust_allocator(Arc::into_raw(self.clone()), &VTABLE) }
    }

    fn reserve(&self, len: usize) -> bool {
        let reserved = self
            .used
            .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |used| {
                used.checked_add(len).filter(|total| *total <= self.limit)
            })
            .is_ok();
        if !reserved {
            self.exceeded.store(true, Ordering::SeqCst);
            if let Some(isolate) = self.isolate.get() {
                isolate.terminate_execution();
            }
        }
        reserved
    }

    fn release(&self, len: usize) {
        self.used.fetch_sub(len, Ordering::SeqCst);
    }
}

/// Every backing store is allocated with this layout and freed with the same, since V8 frees
/// with the length it allocated.
fn buffer_layout(len: usize) -> Option<Layout> {
    Layout::from_size_align(len.max(1), 16).ok()
}

unsafe extern "C" fn budget_allocate(budget: &BufferBudget, len: usize) -> *mut c_void {
    budget_alloc(budget, len, true)
}

unsafe extern "C" fn budget_allocate_uninitialized(
    budget: &BufferBudget,
    len: usize,
) -> *mut c_void {
    budget_alloc(budget, len, false)
}

fn budget_alloc(budget: &BufferBudget, len: usize, zeroed: bool) -> *mut c_void {
    let Some(layout) = buffer_layout(len) else {
        return std::ptr::null_mut();
    };
    if !budget.reserve(len) {
        return std::ptr::null_mut();
    }
    // SAFETY: the layout has a non-zero size.
    let data = unsafe {
        if zeroed {
            std::alloc::alloc_zeroed(layout)
        } else {
            std::alloc::alloc(layout)
        }
    };
    if data.is_null() {
        budget.release(len);
    }
    data.cast()
}

unsafe extern "C" fn budget_free(budget: &BufferBudget, data: *mut c_void, len: usize) {
    if data.is_null() {
        return;
    }
    if let Some(layout) = buffer_layout(len) {
        // SAFETY: `data` came from `budget_alloc` with this same length, so this same layout.
        unsafe { std::alloc::dealloc(data.cast(), layout) };
        budget.release(len);
    }
}

unsafe extern "C" fn budget_drop(budget: *const BufferBudget) {
    // SAFETY: the strong count `allocator` handed to V8.
    drop(unsafe { Arc::from_raw(budget) });
}

deno_core::extension!(
    tool_bundle_host,
    ops = [op_tool_bundle_random_values, op_tool_bundle_log],
);
