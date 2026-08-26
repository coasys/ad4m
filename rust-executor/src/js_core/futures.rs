use deno_core::error::CoreError;
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
            match worker.js_runtime.poll_event_loop(
                cx,
                PollEventLoopOptions {
                    pump_v8_message_loop: true,
                    wait_for_inspector: false,
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

pub struct SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, CoreError>> + Unpin,
{
    worker: Arc<TokioMutex<MainWorker>>,
    value: F,
}

impl<F> SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, CoreError>> + Unpin,
{
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, value: F) -> Self {
        SmartGlobalVariableFuture { worker, value }
    }
}

impl<F> Future for SmartGlobalVariableFuture<F>
where
    F: Future<Output = Result<v8::Global<v8::Value>, CoreError>> + Unpin,
{
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
                    let scope = &mut worker.js_runtime.handle_scope();
                    let result = result.open(scope).to_rust_string_lossy(scope);
                    return Poll::Ready(Ok(result));
                }
                Err(err) => return Poll::Ready(Err(err)),
            };
        }

        let event_loop_poll = worker
            .js_runtime
            .poll_event_loop(cx, deno_core::PollEventLoopOptions::default());
        if let Poll::Ready(event_loop_result) = event_loop_poll {
            if let Err(err) = event_loop_result {
                // Propagate the actual event-loop error so callers see
                // the real failure (permission denial, uncaught rejection,
                // module-eval error, etc.).
                log::error!("Error in event loop: {:?}", err);
                return Poll::Ready(Err(err));
            }

            if let Poll::Ready(result) = value_pin.poll(cx) {
                match result {
                    Ok(result) => {
                        let scope = &mut worker.js_runtime.handle_scope();
                        let result = result.open(scope).to_rust_string_lossy(scope);
                        return Poll::Ready(Ok(result));
                    }
                    Err(err) => return Poll::Ready(Err(err)),
                };
            }

            //return Poll::Ready(Err(anyhow::anyhow!(
            //    "Promise resolution is still pending but the event loop has already resolved."
            //)));
        }

        Poll::Pending
    }
}
