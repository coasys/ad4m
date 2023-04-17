use actix::prelude::*;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use deno_core::resolve_url_or_path;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use lazy_static::lazy_static;
use log::{error, info};
use std::env::current_dir;
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
mod utils_extension;
mod wallet_extension;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use crate::Ad4mConfig;
use options::{main_module_url, main_worker_options};

lazy_static! {
    pub static ref JS_CORE_HANDLE: Mutex<Option<JsCoreHandle>> = Mutex::new(None);
}

/// Define message
#[derive(Message)]
#[rtype(result = "Result<String, AnyError>")]
pub struct Execute {
    pub script: String,
}

pub struct JsCoreHandle {
    rx: Receiver<JsCoreResponse>,
    rx_module_load: Receiver<JsCoreResponse>,
    tx: UnboundedSender<JsCoreRequest>,
    tx_module_load: UnboundedSender<JsCoreRequest>,
    broadcast_tx: Sender<JsCoreResponse>,
    broadcast_loader_tx: Sender<JsCoreResponse>,
}

impl Clone for JsCoreHandle {
    fn clone(&self) -> Self {
        JsCoreHandle {
            rx: self.broadcast_tx.subscribe(),
            rx_module_load: self.broadcast_loader_tx.subscribe(),
            tx: self.tx.clone(),
            tx_module_load: self.tx_module_load.clone(),
            broadcast_tx: self.broadcast_tx.clone(),
            broadcast_loader_tx: self.broadcast_loader_tx.clone(),
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

    pub async fn load_module(&mut self, path: String) -> Result<String, AnyError> {
        let id = uuid::Uuid::new_v4().to_string();
        self.tx_module_load
            .send(JsCoreRequest {
                script: path,
                id: id.clone(),
            })
            .expect("couldn't send on channel... it is likely that the main worker thread has crashed...");

        let mut response = None;
        while response.is_none() {
            match self.rx_module_load.recv().await {
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

    async fn load_module(&self, file_path: String) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().unwrap();
        let url = resolve_url_or_path(&file_path, current_dir()?.as_path())?;
        let _module_id = worker.js_runtime.load_side_module(&url, None).await?;
        //TODO; this likely needs to be run (although might be handled by the import in the js code when import() is called)
        //worker.js_runtime.mod_evaluate(module_id);
        Ok(())
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

    fn execute_async(&self, script: String) -> Result<GlobalVariableFuture, AnyError> {
        let mut worker = self
            .worker
            .lock()
            .expect("execute_async(): couldn't lock worker");
        let wrapped_script = format!(
            r#"
        globalThis.asyncResult = undefined;
        (async () => {{ 
            globalThis.asyncResult = ({}); 
        }})();
        "#,
            script
        );
        let _execute_async = worker.execute_script("js_core", wrapped_script)?;
        Ok(GlobalVariableFuture::new(
            self.worker.clone(),
            "asyncResult".to_string(),
        ))
    }

    pub fn start(config: Ad4mConfig) -> JsCoreHandle {
        let (tx_inside, rx_outside) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside, mut rx_inside) = mpsc::unbounded_channel::<JsCoreRequest>();

        let (tx_inside_loader, rx_outside_loader) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside_loader, mut rx_inside_loader) = mpsc::unbounded_channel::<JsCoreRequest>();

        let tx_inside_clone = tx_inside.clone();
        let tx_inside_loader_clone = tx_inside_loader.clone();
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
                let init_core_future = js_core
                    .init_core(config)
                    .expect("couldn't spawn JS initCore()");

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
                    _init_core_result = run_until => {
                        info!("AD4M initCore() finished");
                    }
                    event_loop_result = js_core.event_loop() => {
                        match event_loop_result {
                            Ok(_) => info!("AD4M event loop finished"),
                            Err(err) => error!("AD4M event loop closed with error: {}", err)
                        }
                    }
                }

                info!("AD4M init complete, starting await loop waiting for requests");

                loop {
                    //Listener future for loading JS modules into runtime
                    let module_load_fut = async {
                        while let Some(request) = rx_inside_loader.recv().await {
                            let tx_loader_cloned = tx_inside_loader.clone();
                            let script = request.script;
                            let id = request.id;

                            match js_core.load_module(script).await {
                                Ok(()) => {
                                    info!("Module loaded!");
                                    tx_inside_loader
                                        .send(JsCoreResponse {
                                            result: Ok(String::from("")),
                                            id: id,
                                        })
                                        .expect("couldn't send on channel");
                                }
                                Err(err) => {
                                    error!("Error loading module: {:?}", err);
                                    tx_loader_cloned
                                        .send(JsCoreResponse {
                                            result: Err(err.to_string()),
                                            id,
                                        })
                                        .expect("couldn't send on channel");
                                }
                            }
                        }
                    };

                    //Listener future for receiving script execution calls
                    let receive_fut = async {
                        while let Some(request) = rx_inside.recv().await {
                            let tx_cloned = tx_inside.clone();
                            let script = request.script;
                            let id = request.id;
                            match js_core.execute_async(script) {
                                Ok(execute_async_future) => match execute_async_future.await {
                                    Ok(result) => {
                                        tx_inside
                                            .send(JsCoreResponse {
                                                result: Ok(result),
                                                id: id,
                                            })
                                            .expect("couldn't send on channel");
                                    }
                                    Err(err) => {
                                        tx_cloned
                                            .send(JsCoreResponse {
                                                result: Err(err.to_string()),
                                                id,
                                            })
                                            .expect("couldn't send on channel");
                                    }
                                },
                                Err(err) => {
                                    tx_cloned
                                        .send(JsCoreResponse {
                                            result: Err(err.to_string()),
                                            id,
                                        })
                                        .expect("couldn't send on channel");
                                    continue;
                                }
                            }
                        }
                    };

                    //NOTE: WARNING!!!!!: Its is possible that if we get a request such as generate agent and then another to load a module,
                    //since the load module would complete first it will cause the request future to be "lost" and thus the graphql will not get a response
                    //we should track this and if required change the behaviour here so that we ensure we continue to run a receive_fut even in the case where
                    //a module_load_fut completes
                    //Using join! here might work if we ensure that the receive fut & module load fut's contain loops and run forever
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
                        _request = receive_fut => {
                            info!("AD4M receive_fut completed");
                            break;
                        }
                        _module_load = module_load_fut => {
                            info!("AD4M module load completed");
                            break;
                        }
                    }
                }
            })
        });

        let handle = JsCoreHandle {
            rx: rx_outside,
            tx: tx_outside,
            rx_module_load: rx_outside_loader,
            tx_module_load: tx_outside_loader,
            broadcast_tx: tx_inside_clone,
            broadcast_loader_tx: tx_inside_loader_clone,
        };

        //Set the JsCoreHandle to a global object so we can use it inside of deno op calls
        let mut global_handle = JS_CORE_HANDLE
            .lock()
            .expect("Could not get lock for JS_CORE_HANDLE");
        *global_handle = Some(handle.clone());

        handle
    }
}
