use deno_core::error::{CoreError, JsError};
use deno_core::{v8, JsRuntime, PollEventLoopOptions};
use deno_runtime::worker::MainWorker;
use futures::Future;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::pin::Pin;
use std::rc::Rc;
use std::sync::Arc;
use std::task::{Context, Poll};
use tokio::sync::Mutex as TokioMutex;

/// Errors the isolate's event loop reported while a call's future was driving
/// it. An event-loop error is isolate-level (an unhandled rejection, a failed
/// dynamic import): it belongs to no call, so a call's future hands it to the
/// loop's owner (the `process_requests` event-loop branch, the only future
/// that reports them) instead of ending its own call on it.
pub(crate) type EventLoopErrors = Rc<RefCell<VecDeque<CoreError>>>;

/// Polls the isolate's event loop and hands every error it reports to
/// `errors`. Ready when the loop drained or reported an error.
///
/// A stalled module evaluation (an `import()` whose top-level await never
/// settles) is reported on every poll while the loop has no other work, and
/// deno registers no waker for it. A second poll that reports the same error
/// is that state: nothing can progress before the next request, so the poll
/// is Pending and the error is not reported again — a future that reported
/// it would be polled again at once, and the request loop would spin. Any
/// other error is reported once; the second poll registers the wakers of the
/// loop's remaining work.
fn poll_event_loop(
    runtime: &mut JsRuntime,
    cx: &mut Context<'_>,
    errors: &EventLoopErrors,
) -> Poll<()> {
    let options = PollEventLoopOptions::default();
    match runtime.poll_event_loop(cx, options) {
        Poll::Pending => Poll::Pending,
        Poll::Ready(Ok(())) => Poll::Ready(()),
        Poll::Ready(Err(first)) => match runtime.poll_event_loop(cx, options) {
            Poll::Ready(Err(again)) if again.to_string() == first.to_string() => Poll::Pending,
            second => {
                let mut errors = errors.borrow_mut();
                errors.push_back(first);
                if let Poll::Ready(Err(other)) = second {
                    errors.push_back(other);
                }
                Poll::Ready(())
            }
        },
    }
}

pub struct EventLoopFuture {
    worker: Arc<TokioMutex<MainWorker>>,
    errors: EventLoopErrors,
}

impl EventLoopFuture {
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, errors: EventLoopErrors) -> Self {
        EventLoopFuture { worker, errors }
    }
}

impl Future for EventLoopFuture {
    type Output = Result<(), CoreError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Report the errors the loop reported, whichever future's poll
        // surfaced them: once each, here.
        let worker = self.worker.try_lock();
        if let Ok(mut worker) = worker {
            // Drained, or pending work with registered wakers: deno_core's
            // internal wakers (I/O, timers) or the channel in the surrounding
            // select! (new RPC request) re-poll when needed.
            let _ = poll_event_loop(&mut worker.js_runtime, cx, &self.errors);
        }
        // On lock contention another task holds the worker mutex; the
        // channel-side waker in the surrounding select! re-polls when the
        // lock holder finishes.
        match self.errors.borrow_mut().pop_front() {
            Some(err) => Poll::Ready(Err(err)),
            None => Poll::Pending,
        }
    }
}

// deno v2.9's JsRuntime::resolve() returns futures resolving to
// `Result<Global<Value>, Box<JsError>>` (not CoreError like older deno_core).
// The trait bound and Output type here match that new shape; callers use
// `Box<JsError>` throughout the outer error chain.
pub struct SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, Box<JsError>>> + Unpin,
{
    worker: Arc<TokioMutex<MainWorker>>,
    errors: EventLoopErrors,
    value: F,
}

impl<F> SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, Box<JsError>>> + Unpin,
{
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, errors: EventLoopErrors, value: F) -> Self {
        SmartGlobalVariableFuture {
            worker,
            errors,
            value,
        }
    }
}

/// A settled call promise becomes its string result. deno v2.9: `handle_scope()`
/// is gone from JsRuntime — `v8::scope!` against the raw isolate + main_context.
/// The inner v8 resolve error (`Box<JsError>`) becomes `CoreError` here so the
/// outer future type stays the same.
fn into_result(
    result: Result<v8::Global<v8::Value>, Box<JsError>>,
    js_runtime: &mut JsRuntime,
) -> Result<String, CoreError> {
    match result {
        Ok(result) => {
            let main_context = js_runtime.main_context();
            let isolate = js_runtime.v8_isolate();
            deno_core::v8::scope!(let handle_scope, isolate);
            let ctx = deno_core::v8::Local::new(handle_scope, main_context);
            let scope = &mut deno_core::v8::ContextScope::new(handle_scope, ctx);
            Ok(result.open(scope).to_rust_string_lossy(scope))
        }
        Err(err) => Err(CoreError::from(err)),
    }
}

impl<F> Future for SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, Box<JsError>>> + Unpin,
{
    // Outer error stays CoreError so callers keep the same error handling.
    // The inner v8 resolve error (Box<JsError>) is converted at the point
    // the value is destructured below.
    type Output = Result<String, CoreError>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let worker = self.worker.clone();
        let mut worker = match worker.try_lock() {
            Ok(w) => w,
            Err(_) => return Poll::Pending,
        };

        // Clone the sink before `value_pin` borrows `self` mutably.
        let errors = self.errors.clone();
        let mut value_pin = Pin::new(&mut self.value);

        if let Poll::Ready(result) = value_pin.as_mut().poll(cx) {
            return Poll::Ready(into_result(result, &mut worker.js_runtime));
        }

        // An event-loop error is isolate-level, not this call's: an unhandled
        // rejection in any promise on the isolate lands here, whichever call
        // drove this poll. `poll_event_loop` hands it to the loop's owner and
        // this call keeps polling — it ends only when its own promise
        // settles, or it would free its slot (the ordered one included)
        // while its script still runs.
        if poll_event_loop(&mut worker.js_runtime, cx, &errors).is_ready() {
            if let Poll::Ready(result) = value_pin.poll(cx) {
                return Poll::Ready(into_result(result, &mut worker.js_runtime));
            }
        }

        Poll::Pending
    }
}
