use deno_core::error::AnyError;
use deno_core::v8;
use deno_runtime::worker::MainWorker;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
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
        let mut worker = match self.worker.try_lock() {
            Ok(worker) => worker,
            Err(_) => return Poll::Pending,
        };
        worker.poll_event_loop(cx, false)
    }
}

pub struct GlobalVariableFuture {
    worker: Arc<TokioMutex<MainWorker>>,
    name: String,
}

impl GlobalVariableFuture {
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, name: String) -> Self {
        GlobalVariableFuture { worker, name }
    }
}

impl Future for GlobalVariableFuture {
    type Output = Result<String, AnyError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        //println!("Trying to get the worker lock: {}", self.name);
        let mut worker = match self.worker.try_lock() {
            Ok(worker) => worker,
            Err(_) => {
                println!("Could not get the lock, returning pending: {}", self.name);
                return Poll::Pending
            }
        };
        //println!("Got the lock: {}", self.name);
        if let Ok(global_value) = worker.execute_script("global_var_future", self.name.clone().into()) {
            let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
            let context = v8::Context::new(scope);
            let scope = &mut v8::ContextScope::new(scope, context);
            let value = v8::Local::new(scope, global_value.clone());

            if value.is_promise() {
                let promise = v8::Local::<v8::Promise>::try_from(value).unwrap();
                if promise.state() == v8::PromiseState::Pending {
                    //cx.waker().wake_by_ref();
                    return Poll::Pending;
                } else {
                    //let result = promise.result();
                    let value = value.to_rust_string_lossy(scope);
                    return Poll::Ready(Ok(value));
                }
            } else if value.is_undefined() {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            } else {
                let value = value.to_rust_string_lossy(scope);
                return Poll::Ready(Ok(value));
            }
        } else {
            return Poll::Pending;
        }
    }
}
