use deno_core::error::AnyError;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use std::sync::{Arc, Mutex};

mod futures;
mod options;
mod string_module_loader;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use options::{main_module_url, main_worker_options};
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

    pub async fn run() -> Result<(), AnyError> {
        let js_core = JsCore::new();
        js_core.init_engine().await;
        let core_init = async {
            let result = js_core.init_core().expect("core init failed").await;
            println!("core init done!");
            result
        };
        let (event, init) = tokio::join!(core_init, js_core.event_loop());
        event.and(init)
    }
}
