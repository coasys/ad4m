use deno_core::error::{CoreError, JsError};
use deno_core::{v8, PollEventLoopOptions};
use deno_runtime::worker::MainWorker;
use futures::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use tokio::sync::Mutex as TokioMutex;

pub struct EventLoopFuture {
    worker: Arc<TokioMutex<MainWorker>>,
}

impl EventLoopFuture {
    pub fn new(worker: Arc<TokioMutex<MainWorker>>) -> Self {
        EventLoopFuture { worker }
    }
}

impl Future for EventLoopFuture {
    type Output = Result<(), CoreError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
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
    value: F,
}

impl<F> SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, Box<JsError>>> + Unpin,
{
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, value: F) -> Self {
        SmartGlobalVariableFuture { worker, value }
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

        let mut value_pin = Pin::new(&mut self.value);

        if let Poll::Ready(result) = value_pin.as_mut().poll(cx) {
            match result {
                Ok(result) => {
                    // deno v2.9: handle_scope() gone from JsRuntime. Use
                    // v8::scope! against the raw isolate + main_context.
                    let main_context = worker.js_runtime.main_context();
                    let isolate = worker.js_runtime.v8_isolate();
                    deno_core::v8::scope!(let handle_scope, isolate);
                    let ctx = deno_core::v8::Local::new(handle_scope, main_context);
                    let scope = &mut deno_core::v8::ContextScope::new(handle_scope, ctx);
                    let result = result.open(scope).to_rust_string_lossy(scope);
                    return Poll::Ready(Ok(result));
                }
                // deno v2.9: inner v8 resolve returns Box<JsError>; wrap
                // into CoreError so the outer Future type stays consistent.
                Err(err) => return Poll::Ready(Err(CoreError::from(err))),
            };
        }

        let event_loop_poll = worker
            .js_runtime
            .poll_event_loop(cx, deno_core::PollEventLoopOptions::default());
        if let Poll::Ready(event_loop_result) = event_loop_poll {
            if let Err(err) = event_loop_result {
                // Propagate the actual event-loop error so callers see
                // the real failure (permission denial, uncaught rejection,
                // module-eval error, etc.). event_loop returns CoreError
                // directly, no conversion needed.
                log::error!("Error in event loop: {:?}", err);
                return Poll::Ready(Err(err));
            }

            if let Poll::Ready(result) = value_pin.poll(cx) {
                match result {
                    Ok(result) => {
                        // deno v2.9: handle_scope() gone; use v8::scope! against
                        // raw isolate + main_context.
                        let main_context = worker.js_runtime.main_context();
                        let isolate = worker.js_runtime.v8_isolate();
                        deno_core::v8::scope!(let handle_scope, isolate);
                        let ctx = deno_core::v8::Local::new(handle_scope, main_context);
                        let scope = &mut deno_core::v8::ContextScope::new(handle_scope, ctx);
                        let result = result.open(scope).to_rust_string_lossy(scope);
                        return Poll::Ready(Ok(result));
                    }
                    // deno v2.9: Box<JsError> → CoreError conversion.
                    Err(err) => return Poll::Ready(Err(CoreError::from(err))),
                };
            }

            //return Poll::Ready(Err(anyhow::anyhow!(
            //    "Promise resolution is still pending but the event loop has already resolved."
            //)));
        }

        Poll::Pending
    }
}
