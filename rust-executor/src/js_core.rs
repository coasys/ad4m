use actix::prelude::*;
use deno_core::error::AnyError;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use tokio::sync::mpsc::{self, Receiver, Sender};
use tokio::task::LocalSet;
use std::sync::{Arc, Mutex};
use tokio::runtime::Builder;
mod futures;
mod options;
mod string_module_loader;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use options::{main_module_url, main_worker_options};

/// Define message
#[derive(Message)]
#[rtype(result = "Result<String, AnyError>")]
pub struct Execute {
    pub script: String,
}
pub struct JsCoreActor {
    rx: Receiver<()>,
    tx: Sender<()>,
}

impl JsCoreActor {
    pub async fn initialized(&mut self) {
        self.rx.recv().await.expect("couldn't receive on channel");
    }
}

pub struct JsCore {
    worker: Arc<Mutex<MainWorker>>,
}

impl JsCore {
    fn new() -> Self {
        JsCore {
            worker: Arc::new(Mutex::new(MainWorker::from_options(
                main_module_url(),
                PermissionsContainer::allow_all(),
                main_worker_options(),
            ))),
        }
    }

    async fn init_engine(&self) {
        let mut worker = self.worker.lock().unwrap();
        worker.bootstrap(&BootstrapOptions::default());
        worker
            .execute_main_module(&main_module_url())
            .await
            .unwrap();
    }

    fn event_loop(&self) -> EventLoopFuture {
        let event_loop = EventLoopFuture::new(self.worker.clone());
        event_loop
    }

    fn init_core(&self) -> Result<GlobalVariableFuture, AnyError> {
        let mut worker = self.worker.lock().unwrap();
        let _init_core = worker.execute_script("js_core", "initCore()")?;
        Ok(GlobalVariableFuture::new(
            self.worker.clone(),
            "core".to_string(),
        ))
    }

    pub fn start() -> JsCoreActor{
        let (tx_inside, rx_outside) = mpsc::channel::<()>(1);
        let (tx_outside, rx_inside) = mpsc::channel::<()>(1);
        std::thread::spawn(move || {
            let rt = Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            let js_core = JsCore::new();
            
            rt.block_on(js_core.init_engine());
            let local = LocalSet::new();
            local.spawn_local(js_core.init_core().expect("couldn't spawn JS initCore()"));
            match rt.block_on(js_core.event_loop()) {
                Ok(_) => println!("event loop finished"),
                Err(err) => println!("event loop failed: {}", err),
            };
            
            rt.block_on(tx_inside.send(())).expect("couldn't send on channel");
        });

        JsCoreActor {
            rx: rx_outside,
            tx: tx_outside,
        }
    }
}

/*
// Provide Actor implementation for our actor
impl Actor for JsCore {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
        println!("Starting JsCore actor...");

        let event_loop_fut = self.event_loop();
        ctx.spawn(async move {
            match event_loop_fut.await {
                Ok(_) => println!("event loop finished"),
                Err(err) => println!("event loop failed: {}", err),
            }
        }.into_actor(self));

        //let init_core_fut = self.init_core().expect("couldn't call JS initCore()");
        //actix_rt::Arbiter::spawn_blocking(init_core_fut);
        //tokio::runtime::
        //ctx.spawn(async move {
            //init_engine_fut.await;
        //    init_core_fut.await;
        //}.into_actor(self));

    

        
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
 */