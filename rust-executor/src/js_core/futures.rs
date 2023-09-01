use deno_core::error::AnyError;
use deno_core::v8;
use deno_runtime::worker::MainWorker;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};

pub struct EventLoopFuture {
    worker: Arc<Mutex<MainWorker>>,
}

impl EventLoopFuture {
    pub fn new(worker: Arc<Mutex<MainWorker>>) -> Self {
        EventLoopFuture { worker }
    }
}

impl Future for EventLoopFuture {
    type Output = Result<(), AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut worker = self.worker.lock().unwrap();
        worker.poll_event_loop(cx, false)
    }
}

pub struct GlobalVariableFuture {
    worker: Arc<Mutex<MainWorker>>,
    name: String,
}

impl GlobalVariableFuture {
    pub fn new(worker: Arc<Mutex<MainWorker>>, name: String) -> Self {
        GlobalVariableFuture { worker, name }
    }
}

impl Future for GlobalVariableFuture {
    type Output = Result<String, AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        println!("Trying to get the worker lock: {}", self.name);
        let mut worker = self.worker.lock().unwrap();
        println!("Got the lock: {}", self.name);
        if let Ok(global_value) = worker.execute_script("global_var_future", self.name.clone().into()) {
            let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
            let context = v8::Context::new(scope);
            let scope = &mut v8::ContextScope::new(scope, context);
            let value = v8::Local::new(scope, global_value.clone());

            if value.is_promise() {
                let promise = v8::Local::<v8::Promise>::try_from(value).unwrap();
                if promise.state() == v8::PromiseState::Pending {
                    return Poll::Pending;
                } else {
                    //let result = promise.result();
                    let value = value.to_rust_string_lossy(scope);
                    return Poll::Ready(Ok(value));
                }
            } else if value.is_undefined() {
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
