use deno_core::error::AnyError;
use deno_core::{v8, PollEventLoopOptions};
use deno_runtime::worker::MainWorker;
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
            let res = worker.js_runtime.poll_event_loop(cx, PollEventLoopOptions::default());
            cx.waker().wake_by_ref();
            res
        } else {
            Poll::Pending
        }
    }
}

