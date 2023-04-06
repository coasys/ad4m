use actix::prelude::*;

use deno_core::error::AnyError;
use deno_core::v8;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use std::sync::{Arc, Mutex};

mod futures;
mod options;
mod string_module_loader;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use options::{main_module_url, main_worker_options};

/// Define message
#[derive(Message)]
#[rtype(result = "Result<String, AnyError>")]
struct Execute {
    script: String,
}

pub struct JsCore {
    worker: Arc<Mutex<MainWorker>>,
}

impl JsCore {
    pub fn new() -> Self {
        JsCore {
            worker: Arc::new(Mutex::new(MainWorker::from_options(
                main_module_url(),
                PermissionsContainer::allow_all(),
                main_worker_options(),
            ))),
        }
    }

    pub async fn init_engine(&self) {
        let mut worker = self.worker.lock().unwrap();
        worker.bootstrap(&BootstrapOptions::default());
        worker
            .execute_main_module(&main_module_url())
            .await
            .unwrap();
    }

    pub fn event_loop(&self) -> EventLoopFuture {
        let event_loop = EventLoopFuture::new(self.worker.clone());
        event_loop
    }

    pub fn init_core(&self) -> Result<GlobalVariableFuture, AnyError> {
        let mut worker = self.worker.lock().unwrap();
        let _init_core = worker.execute_script("js_core", "initCore()")?;
        Ok(GlobalVariableFuture::new(
            self.worker.clone(),
            "core".to_string(),
        ))
    }

    pub async fn run() -> Result<((), ()), AnyError> {
        let js_core = JsCore::new();
        js_core.init_engine().await;
        let core_init = async {
            let result = js_core.init_core().expect("core init failed").await;
            result
        };
        tokio::try_join!(core_init, js_core.event_loop())
    }
}

// Provide Actor implementation for our actor
impl Actor for JsCore {
    type Context = Context<Self>;

    fn started(&mut self, _: &mut Context<Self>) {
       println!("Actor is alive");
    }

    fn stopped(&mut self, _: &mut Context<Self>) {
       println!("Actor is stopped");
    }
}

/// Define handler for `Ping` message
impl Handler<Execute> for JsCore {
    type Result = Result<String, AnyError>;

    fn handle(&mut self, msg: Execute, _: &mut Context<Self>) -> Self::Result {
        let mut worker = self.worker.lock().unwrap();
        let result = worker.execute_script("js_core", format!("JSON.stringify({})", msg.script))?;
        let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
        let context = v8::Context::new(scope);
        let scope = &mut v8::ContextScope::new(scope, context);
        let value = v8::Local::new(scope, result);
        //let value: v8::Local<v8::String> = unsafe { v8::Local::cast(value) };
        let value = value.to_rust_string_lossy(scope);
        Ok(value)
    }
}