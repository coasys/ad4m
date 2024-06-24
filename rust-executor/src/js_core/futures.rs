use deno_core::error::AnyError;
use deno_core::v8::Handle;
use deno_core::{v8, PollEventLoopOptions, JsRuntime};
use deno_runtime::worker::MainWorker;
use log::info; // Import the JsRuntime struct.
use std::future::Future;
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
    type Output = Result<(), AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        sleep(std::time::Duration::from_millis(1));
        let worker = self.worker.try_lock();
        if let Ok(mut worker) = worker {
            let res = worker.js_runtime.poll_event_loop(cx, PollEventLoopOptions {
                pump_v8_message_loop: true,
                wait_for_inspector: false,
            });
            cx.waker().wake_by_ref();
            res
        } else {
            Poll::Pending
        }
    }
}

pub struct SmartGlobalVariableFuture {
    worker: Arc<TokioMutex<MainWorker>>,
    value: String,
}

impl SmartGlobalVariableFuture {
    pub fn new(worker: Arc<TokioMutex<MainWorker>>, value: String) -> Self {
        SmartGlobalVariableFuture { worker, value }
    }
}

impl Future for SmartGlobalVariableFuture {
    type Output = Result<String, AnyError>; // You can customize the output type.

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        //println!("Trying to get the worker lock: {}", self.name);
        let mut worker = self.worker.try_lock().expect("Failed to lock worker");
        let scope = &mut worker.js_runtime.handle_scope();
        let code = v8::String::new(scope, &self.value).unwrap();
        let script = v8::Script::compile(scope, code, None);
        match script {
            Some(script) => {
                let result = script.run(scope);
                match result {
                    Some(result) => {
                        info!("Result: {:?}", result.clone());
                        let result_str = result.open(scope).to_rust_string_lossy(scope);
                        // let result_str = result.to_string(scope).unwrap().to_rust_string_lossy(scope);
                        info!("Result: {}", result_str);
                        Poll::Ready(Ok(result_str))
                    }
                    None => Poll::Ready(Err(AnyError::msg("Failed to execute script"))),
                }
            },
            None => Poll::Ready(Err(AnyError::msg("Failed to compile script"))),
        }
    }
}