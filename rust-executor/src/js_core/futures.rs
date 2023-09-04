use deno_core::error::AnyError;
use deno_core::v8;
use deno_runtime::worker::MainWorker;
use std::future::Future;
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
    type Output = Result<(), AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let worker = self.worker.try_lock();
        if let Ok(mut worker) = worker {
            worker.poll_event_loop(cx, false)
        } else {
            Poll::Pending
        }
    }
}

pub struct SmartGlobalVariableFuture {
    worker: Arc<TokioMutex<MainWorker>>,
    value: v8::Global<v8::Value>,
}

impl SmartGlobalVariableFuture {
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, value: v8::Global<v8::Value>) -> Self {
        SmartGlobalVariableFuture { worker, value }
    }
}

impl Future for SmartGlobalVariableFuture {
    type Output = Result<String, AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        //println!("Trying to get the worker lock: {}", self.name);
        let mut worker = self.worker.try_lock().expect("Failed to lock worker");
        let poll_value = worker.js_runtime.poll_value(&self.value, cx);

        match poll_value {
            Poll::Pending => {
                cx.waker().wake_by_ref();
                Poll::Pending
            },
            Poll::Ready(value) => {
                match value {
                    Ok(value) => {
                        let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
                        let context = v8::Context::new(scope);
                        let scope = &mut v8::ContextScope::new(scope, context);
                        let value = value.open(scope).to_rust_string_lossy(scope);
                        Poll::Ready(Ok(value))
                    },
                    Err(err) => Poll::Ready(Err(err))
                }
            }
        }
    }
}
