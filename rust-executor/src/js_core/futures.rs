use deno_core::error::CoreError;
use deno_core::{v8, PollEventLoopOptions};
use deno_runtime::worker::MainWorker;
use futures::Future;
// Import the JsRuntime struct.
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::thread::sleep;
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
    type Output = Result<(), CoreError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        sleep(std::time::Duration::from_millis(1));
        let worker = self.worker.try_lock();
        if let Ok(mut worker) = worker {
            let res = worker.js_runtime.poll_event_loop(
                cx,
                PollEventLoopOptions {
                    pump_v8_message_loop: true,
                    wait_for_inspector: false,
                },
            );
            cx.waker().wake_by_ref();
            res
        } else {
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

        if let Poll::Ready(event_loop_result) = &mut worker
            .js_runtime
            .poll_event_loop(cx, deno_core::PollEventLoopOptions::default())
        {
            if let Err(err) = event_loop_result {
                log::error!("Error in event loop: {:?}", err);
                return Poll::Ready(Err(CoreError::TLA));
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
