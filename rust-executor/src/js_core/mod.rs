use actix::prelude::*;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use deno_core::v8;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use log::{error, info};
use std::sync::{Arc, Mutex};
use tokio::runtime::Builder;
use tokio::sync::broadcast;
use tokio::sync::{
    broadcast::{Receiver, Sender},
    mpsc::{self, UnboundedSender},
};
use tokio::task::LocalSet;

mod futures;
mod options;
mod string_module_loader;
mod wallet_extension;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use crate::Ad4mConfig;
use options::{main_module_url, main_worker_options};

/// Define message
#[derive(Message)]
#[rtype(result = "Result<String, AnyError>")]
pub struct Execute {
    pub script: String,
}

pub struct JsCoreHandle {
    rx: Receiver<JsCoreResponse>,
    tx: UnboundedSender<JsCoreRequest>,

    broadcast_tx: Sender<JsCoreResponse>,
}

impl Clone for JsCoreHandle {
    fn clone(&self) -> Self {
        JsCoreHandle {
            rx: self.broadcast_tx.subscribe(),
            tx: self.tx.clone(),
            broadcast_tx: self.broadcast_tx.clone(),
        }
    }
}

impl JsCoreHandle {
    pub async fn initialized(&mut self) {
        self.rx.recv().await.expect("couldn't receive on channel");
    }

    pub async fn execute(&mut self, script: String) -> Result<String, AnyError> {
        let id = uuid::Uuid::new_v4().to_string();
        self.tx
            .send(JsCoreRequest {
                script,
                id: id.clone(),
            })
            .expect("couldn't send on channel... it is likely that the main worker thread has crashed...");

        let mut response = None;
        while response.is_none() {
            match self.rx.recv().await {
                Ok(r) => {
                    if r.id == id {
                        response = Some(r);
                    }
                }
                Err(err) => {
                    error!("Error receiving on channel");
                    return Err(anyhow!(err));
                }
            }
        }

        response
            .expect("none case handle above")
            .result
            .map_err(|err| anyhow!(err))
    }
}

#[derive(Debug, Clone)]
struct JsCoreRequest {
    script: String,
    id: String,
}

#[derive(Debug, Clone)]
struct JsCoreResponse {
    result: Result<String, String>,
    id: String,
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

    async fn init_engine(&self) {
        let mut worker = self
            .worker
            .lock()
            .expect("init_engine(): couldn't lock worker");
        worker.bootstrap(&BootstrapOptions::default());
        worker
            .execute_main_module(&main_module_url())
            .await
            .expect("init_engine(): could not execute main module");
    }

    fn event_loop(&self) -> EventLoopFuture {
        let event_loop = EventLoopFuture::new(self.worker.clone());
        event_loop
    }

    fn init_core(&self, mut config: Ad4mConfig) -> Result<GlobalVariableFuture, AnyError> {
        config.prepare();
        let mut worker = self
            .worker
            .lock()
            .expect("init_core(): couldn't lock worker");
        let _init_core =
            worker.execute_script("js_core", format!("initCore({})", config.get_json()))?;
        Ok(GlobalVariableFuture::new(
            self.worker.clone(),
            "core".to_string(),
        ))
    }

    pub fn start(config: Ad4mConfig) -> JsCoreHandle {
        let (tx_inside, rx_outside) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside, mut rx_inside) = mpsc::unbounded_channel::<JsCoreRequest>();

        let tx_inside_clone = tx_inside.clone();
        std::thread::spawn(move || {
            let rt = Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            let js_core = JsCore::new();

            rt.block_on(js_core.init_engine());
            info!("AD4M JS engine init completed");

            rt.block_on(async {
                let local = LocalSet::new();
                let tx_cloned = tx_inside.clone();
                let init_core_future = js_core.init_core(config).expect("couldn't spawn JS initCore()");

                // Run the local task set.
                let run_until = local.run_until(async move {
                    match init_core_future.await {
                        Ok(_) => {}
                        Err(err) => error!("AD4M coreInit() failed with error: {}", err),
                    };
                    tx_cloned
                        .send(JsCoreResponse {
                            result: Ok(String::from("initialized")),
                            id: String::from("initialized"),
                        })
                        .expect("couldn't send on channel");
                });
                tokio::select! {
                    _init_core_result = run_until => {}
                    event_loop_result = js_core.event_loop() => {
                        match event_loop_result {
                            Ok(_) => info!("AD4M event loop finished"),
                            Err(err) => error!("AD4M event loop closed with error: {}", err)
                        }
                    }
                }

                // Until stop was request via message
                // wait for new messages, execute them
                // and concurrently run the event loop
                loop {
                    tokio::select! {
                        event_loop_result = js_core.event_loop() => {
                            match event_loop_result {
                                Ok(_) => info!("AD4M event loop finished"),
                                Err(err) => {
                                    error!("AD4M event loop closed with error: {}", err);
                                    break;
                                }
                            }
                        }
                        request = rx_inside.recv() => {
                            match request {
                                Some(request) => {
                                    let mut worker = js_core.worker.lock().expect("request loop: couldn't lock worker");
                                    match worker.execute_script("js_core", request.script) {
                                        Ok(result) => {
                                            // evaluate deno result into String
                                            let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
                                            let context = v8::Context::new(scope);
                                            let scope = &mut v8::ContextScope::new(scope, context);
                                            let value = v8::Local::new(scope, result);
                                            //let value: v8::Local<v8::String> = unsafe { v8::Local::cast(value) };
                                            let value = value.to_rust_string_lossy(scope);
                                            tx_inside
                                                .send(JsCoreResponse {
                                                    result: Ok(value),
                                                    id: request.id,
                                                })
                                                .expect("couldn't send on channel");
                                        },
                                        Err(err) => {
                                            tx_inside
                                                .send(JsCoreResponse {
                                                    result: Err(err.to_string()),
                                                    id: request.id,
                                                })
                                                .expect("couldn't send on channel");
                                        }
                                    }
                                }
                                None => {
                                    error!("AD4M event loop closed");
                                    break;
                                }
                            }
                        }
                    }
                }

            })
        });

        JsCoreHandle {
            rx: rx_outside,
            tx: tx_outside,
            broadcast_tx: tx_inside_clone,
        }
    }
}
