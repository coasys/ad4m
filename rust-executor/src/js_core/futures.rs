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
        // Report an error a call's future collected while driving the loop:
        // reported once, here, no matter whose poll surfaced it.
        {
            let mut errors = self.errors.borrow_mut();
            if let Some(err) = errors.pop_front() {
                return Poll::Ready(Err(err));
            }
        }
        let worker = self.worker.try_lock();
        if let Ok(mut worker) = worker {
            // deno v2.9's poll_event_loop uses PollEventLoopOptions::default().
            // The old `pump_v8_message_loop` field was removed — upstream now
            // always pumps; `wait_for_inspector` stays available.
            match worker.js_runtime.poll_event_loop(
                cx,
                PollEventLoopOptions {
                    wait_for_inspector: false,
                    ..Default::default()
                },
            ) {
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                // Event loop drained or has pending work with registered
                // wakers. Return Pending — deno_core's internal wakers
                // (I/O, timers) or the channel in the surrounding select!
                // (new RPC request) will re-poll when needed.
                Poll::Ready(Ok(())) | Poll::Pending => Poll::Pending,
            }
        } else {
            // Lock contention — another task holds the worker mutex.
            // The channel-side waker in the surrounding select! re-polls
            // when the lock holder finishes.
            Poll::Pending
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

        if let Poll::Ready(event_loop_result) = worker
            .js_runtime
            .poll_event_loop(cx, deno_core::PollEventLoopOptions::default())
        {
            if let Err(err) = event_loop_result {
                // An event-loop error is isolate-level, not this call's: an
                // unhandled rejection in any promise on the isolate lands
                // here, whichever call drove this poll. Hand it to the
                // loop's owner and keep polling — this call ends only when
                // its own promise settles, or it would free its slot (the
                // ordered one included) while its script still runs.
                errors.borrow_mut().push_back(err);
            }

            if let Poll::Ready(result) = value_pin.poll(cx) {
                return Poll::Ready(into_result(result, &mut worker.js_runtime));
            }
        }

        Poll::Pending
    }
}
