//! The small runtime seam between this crate and its async executor.
//!
//! Native targets run on tokio (`spawn`, `sleep`, `timeout` are the real
//! ones). `wasm32-unknown-unknown` has no tokio runtime — a `spawn` or
//! `tokio::time` call panics there — so the browser's event loop drives
//! tasks (`wasm_bindgen_futures::spawn_local`) and `gloo_timers` drives
//! timers. Everything above the transport — the RPC pending map, the
//! reconnect supervisor, live queries — is written once against these
//! functions.
//!
//! `tokio::sync` primitives (broadcast/watch/oneshot/Mutex/OnceCell/Notify)
//! and `tokio::select!` compile for wasm32 and need no runtime — they are
//! used directly on both targets. `std::time::Instant` never appears:
//! `Duration` is all the timers need, and `Instant` is not the browser's
//! clock.

use std::future::Future;
use std::time::Duration;

/// A spawned task the owner can stop, and wait for.
pub(crate) struct Task {
    #[cfg(not(target_arch = "wasm32"))]
    inner: tokio::task::JoinHandle<()>,
    #[cfg(target_arch = "wasm32")]
    abort: futures_util::future::AbortHandle,
    /// Flips to `true` once the task finished or was aborted and dropped.
    #[cfg(target_arch = "wasm32")]
    done: tokio::sync::watch::Receiver<bool>,
}

impl Task {
    /// Stop the task at its next yield point — the same contract as
    /// `tokio::task::JoinHandle::abort` (the task's output is dropped).
    pub(crate) fn abort(&self) {
        #[cfg(not(target_arch = "wasm32"))]
        self.inner.abort();
        #[cfg(target_arch = "wasm32")]
        self.abort.abort();
    }

    /// Whether the task ended (ran out or was aborted).
    pub(crate) fn is_finished(&self) -> bool {
        #[cfg(not(target_arch = "wasm32"))]
        return self.inner.is_finished();
        #[cfg(target_arch = "wasm32")]
        return *self.done.borrow();
    }

    /// Await the task's end, like awaiting a tokio `JoinHandle` (an
    /// aborted task reports `is_cancelled`).
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) async fn join(self) -> Result<(), tokio::task::JoinError> {
        self.inner.await
    }

    /// Await the task's end: its future was dropped, finished or aborted.
    #[cfg(target_arch = "wasm32")]
    pub(crate) async fn join(mut self) {
        let _ = self.done.wait_for(|d| *d).await;
    }
}

/// `tokio::spawn` natively; `wasm_bindgen_futures::spawn_local` in the
/// browser (single-threaded — no `Send` needed there). The returned
/// [`Task`] aborts the work on either target.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn spawn<F>(fut: F) -> Task
where
    F: Future<Output = ()> + Send + 'static,
{
    Task {
        inner: tokio::spawn(fut),
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn spawn<F>(fut: F) -> Task
where
    F: Future<Output = ()> + 'static,
{
    // `spawn_local` returns no handle, so wrap the future in an `Abortable`:
    // `Task::abort` ends it at its next poll like a tokio abort.
    let (abort, reg) = futures_util::future::AbortHandle::new_pair();
    let (done_tx, done) = tokio::sync::watch::channel(false);
    wasm_bindgen_futures::spawn_local(async move {
        let _ = futures_util::future::Abortable::new(fut, reg).await;
        // The future is dropped by now: what it owned is released.
        done_tx.send_replace(true);
    });
    Task { abort, done }
}

/// Spawn work that runs to its end even if the caller stops awaiting, and
/// hand the caller its output: `tokio::spawn(..).await` natively; in the
/// browser a `spawn_local` that sends the output down a oneshot. The error
/// is the task ending without an output (a native panic or abort).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) async fn spawn_detached<T, F>(fut: F) -> Result<T, String>
where
    T: Send + 'static,
    F: Future<Output = T> + Send + 'static,
{
    tokio::spawn(fut).await.map_err(|e| e.to_string())
}

#[cfg(target_arch = "wasm32")]
pub(crate) async fn spawn_detached<T, F>(fut: F) -> Result<T, String>
where
    T: 'static,
    F: Future<Output = T> + 'static,
{
    let (tx, rx) = tokio::sync::oneshot::channel();
    wasm_bindgen_futures::spawn_local(async move {
        let _ = tx.send(fut.await);
    });
    rx.await
        .map_err(|_| "task ended without an output".to_string())
}

/// Fire-and-forget spawn for contexts that may run without an executor
/// (e.g. `Drop`). Native: skipped silently outside a tokio runtime — the
/// caller must await teardown itself there. Browser: `spawn_local` always
/// has the event loop.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn try_spawn<F>(fut: F)
where
    F: Future<Output = ()> + Send + 'static,
{
    if let Ok(rt) = tokio::runtime::Handle::try_current() {
        rt.spawn(fut);
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn try_spawn<F>(fut: F)
where
    F: Future<Output = ()> + 'static,
{
    wasm_bindgen_futures::spawn_local(fut);
}

/// `tokio::time::sleep` natively; a `setTimeout` future in the browser.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) async fn sleep(d: Duration) {
    tokio::time::sleep(d).await
}

#[cfg(target_arch = "wasm32")]
pub(crate) async fn sleep(d: Duration) {
    gloo_timers::future::sleep(d).await
}

/// Let other ready tasks run before continuing — `tokio::task::yield_now`
/// natively; in the browser a future that is pending exactly once (it wakes
/// itself, so the event loop polls it again after the queued tasks).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) async fn yield_now() {
    tokio::task::yield_now().await
}

#[cfg(target_arch = "wasm32")]
pub(crate) async fn yield_now() {
    let mut yielded = false;
    std::future::poll_fn(|cx| {
        if yielded {
            std::task::Poll::Ready(())
        } else {
            yielded = true;
            cx.waker().wake_by_ref();
            std::task::Poll::Pending
        }
    })
    .await
}

/// The deadline elapsed — `timeout`'s error on either target.
#[derive(Debug)]
pub(crate) struct Elapsed;

impl std::fmt::Display for Elapsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("deadline elapsed")
    }
}

impl std::error::Error for Elapsed {}

/// `tokio::time::timeout` natively; a race against `TimeoutFuture` in the
/// browser. The losing future is dropped, matching tokio's timeout.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) async fn timeout<F: Future>(d: Duration, fut: F) -> Result<F::Output, Elapsed> {
    tokio::time::timeout(d, fut).await.map_err(|_| Elapsed)
}

#[cfg(target_arch = "wasm32")]
pub(crate) async fn timeout<F: Future>(d: Duration, fut: F) -> Result<F::Output, Elapsed> {
    use futures_util::future::Either;
    let millis = d.as_millis().min(u32::MAX as u128) as u32;
    let timer = gloo_timers::future::TimeoutFuture::new(millis);
    match futures_util::future::select(std::pin::pin!(fut), timer).await {
        Either::Left((v, _)) => Ok(v),
        Either::Right(((), _)) => Err(Elapsed),
    }
}

/// One log line — `eprintln` natively, `console.warn` in the browser (wasm
/// stderr goes nowhere; the browser console is where failures surface).
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn log(line: &str) {
    eprintln!("{line}");
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn log(line: &str) {
    web_sys::console::warn_1(&line.into());
}
