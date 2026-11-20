//! Tasks on the JS event loop: the wasm32 half of `crate::rt`.
//!
//! The names and methods are tokio's (`tokio::task::{spawn, spawn_blocking, JoinHandle,
//! AbortHandle, JoinError}`), so code written against `crate::rt` builds on both targets.
//! What differs, because there is one thread:
//! - `spawn` takes a future that need not be `Send`; it runs on
//!   `wasm_bindgen_futures::spawn_local`.
//! - `spawn_blocking` runs the closure at once, on the caller's stack, and returns a handle
//!   that is already finished.
//! - A task cannot panic into its handle: a panic on wasm32 aborts the module.

use futures::channel::oneshot;
use futures::future::{AbortHandle as FutAbortHandle, Abortable};
use std::fmt;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};

/// Why awaiting a [`JoinHandle`] gave no value: the task was aborted, or it was dropped
/// without finishing.
#[derive(Debug)]
pub struct JoinError {
    _private: (),
}

impl JoinError {
    fn cancelled() -> Self {
        JoinError { _private: () }
    }

    pub fn is_cancelled(&self) -> bool {
        true
    }

    pub fn is_panic(&self) -> bool {
        false
    }
}

impl fmt::Display for JoinError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("task was cancelled")
    }
}

impl std::error::Error for JoinError {}

/// Aborts a task without owning its result.
#[derive(Clone, Debug)]
pub struct AbortHandle {
    abort: FutAbortHandle,
    finished: Arc<AtomicBool>,
}

impl AbortHandle {
    /// Stops the task at its next await; it never resumes, and its handle gives a
    /// cancelled [`JoinError`]. Aborting a finished task does nothing.
    pub fn abort(&self) {
        self.abort.abort();
    }

    /// The task returned, or it was aborted and has stopped.
    pub fn is_finished(&self) -> bool {
        self.finished.load(Ordering::SeqCst)
    }
}

/// The task's result, once it finishes. Dropping the handle detaches the task: it runs on.
pub struct JoinHandle<T> {
    result: oneshot::Receiver<T>,
    abort: AbortHandle,
}

impl<T> JoinHandle<T> {
    pub fn abort(&self) {
        self.abort.abort();
    }

    pub fn is_finished(&self) -> bool {
        self.abort.is_finished()
    }

    pub fn abort_handle(&self) -> AbortHandle {
        self.abort.clone()
    }
}

impl<T> fmt::Debug for JoinHandle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JoinHandle")
            .field("finished", &self.is_finished())
            .finish()
    }
}

impl<T> Future for JoinHandle<T> {
    type Output = Result<T, JoinError>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.result)
            .poll(cx)
            .map(|r| r.map_err(|oneshot::Canceled| JoinError::cancelled()))
    }
}

/// Runs `future` on the JS event loop.
pub fn spawn<F>(future: F) -> JoinHandle<F::Output>
where
    F: Future + 'static,
    F::Output: 'static,
{
    let (tx, result) = oneshot::channel();
    let (abort, registration) = FutAbortHandle::new_pair();
    let finished = Arc::new(AtomicBool::new(false));
    let task = Abortable::new(future, registration);
    let done = finished.clone();
    wasm_bindgen_futures::spawn_local(async move {
        if let Ok(output) = task.await {
            // The handle may be gone: then the output is dropped, as tokio drops it.
            let _ = tx.send(output);
        }
        done.store(true, Ordering::SeqCst);
    });
    JoinHandle {
        result,
        abort: AbortHandle { abort, finished },
    }
}

/// Runs `f` now, on the caller's stack (there is no other thread), and returns a finished
/// handle with its result.
pub fn spawn_blocking<F, R>(f: F) -> JoinHandle<R>
where
    F: FnOnce() -> R,
{
    let (tx, result) = oneshot::channel();
    let _ = tx.send(f());
    let (abort, _registration) = FutAbortHandle::new_pair();
    JoinHandle {
        result,
        abort: AbortHandle {
            abort,
            finished: Arc::new(AtomicBool::new(true)),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::rc::Rc;
    use wasm_bindgen_test::wasm_bindgen_test;

    /// Lets every task that is ready run once.
    async fn yield_to_tasks() {
        let (tx, rx) = oneshot::channel::<()>();
        wasm_bindgen_futures::spawn_local(async move {
            let _ = tx.send(());
        });
        let _ = rx.await;
    }

    #[wasm_bindgen_test]
    async fn a_spawned_task_runs_and_gives_its_output() {
        let ran = Rc::new(Cell::new(false));
        let flag = ran.clone();
        let handle = spawn(async move {
            flag.set(true);
            7
        });
        assert_eq!(handle.await.unwrap(), 7);
        assert!(ran.get());
    }

    #[wasm_bindgen_test]
    async fn an_aborted_task_does_not_finish() {
        let (_never, wait) = oneshot::channel::<()>();
        let reached_end = Rc::new(Cell::new(false));
        let flag = reached_end.clone();
        let handle = spawn(async move {
            let _ = wait.await;
            flag.set(true);
        });
        let abort = handle.abort_handle();
        yield_to_tasks().await;
        assert!(!handle.is_finished());
        handle.abort();
        let err = handle.await.unwrap_err();
        assert!(err.is_cancelled());
        assert!(!err.is_panic());
        assert!(abort.is_finished());
        assert!(!reached_end.get());
    }

    #[wasm_bindgen_test]
    async fn a_task_aborted_before_it_starts_never_runs() {
        let ran = Rc::new(Cell::new(false));
        let flag = ran.clone();
        let handle = spawn(async move { flag.set(true) });
        handle.abort();
        assert!(handle.await.unwrap_err().is_cancelled());
        assert!(!ran.get());
    }

    #[wasm_bindgen_test]
    async fn a_dropped_handle_detaches_the_task() {
        let (tx, rx) = oneshot::channel();
        drop(spawn(async move {
            let _ = tx.send(5);
        }));
        assert_eq!(rx.await.unwrap(), 5);
    }

    #[wasm_bindgen_test]
    async fn spawn_blocking_returns_its_value() {
        let handle = spawn_blocking(|| 2 + 2);
        assert!(handle.is_finished());
        assert_eq!(handle.await.unwrap(), 4);
    }
}
