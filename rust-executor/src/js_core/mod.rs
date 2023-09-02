use deno_core::v8::Global;
use ::futures::Future;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use deno_core::resolve_url_or_path;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use holochain::prelude::{ExternIO, Signal};
use log::debug;
use once_cell::sync::Lazy;
use std::env::current_dir;
use std::error::Error;
use std::sync::Arc;
use tokio::runtime::Builder;
use tokio::sync::broadcast;
use tokio::sync::Mutex as TokioMutex;
use tokio::sync::{
    broadcast::{Receiver, Sender},
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    oneshot
};
use tokio::task::LocalSet;
use tracing::{error, info};
use lazy_static::lazy_static;

mod futures;
mod jwt_extension;
mod options;
mod pubsub_extension;
mod string_module_loader;
mod utils_extension;
mod wallet_extension;

use self::futures::{EventLoopFuture, GlobalVariableFuture};
use crate::holochain_service::maybe_get_holochain_service;
use crate::Ad4mConfig;
use options::{main_module_url, main_worker_options};

static JS_CORE_HANDLE: Lazy<Arc<TokioMutex<Option<JsCoreHandle>>>> =
    Lazy::new(|| Arc::new(TokioMutex::new(None)));

fn uuid_to_valid_variable_name(uuid: &str) -> String {
    let valid_chars: String = uuid.chars().filter(|c| c.is_alphabetic()).collect();
    valid_chars
}

use std::collections::HashMap;

lazy_static! {
    static ref RESPONSES: TokioMutex<HashMap<String, JsCoreResponse>> = TokioMutex::new(HashMap::new());
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
        let (response_tx, response_rx) = oneshot::channel();

        self.tx
            .send(JsCoreRequest {
                script,
                id: id.clone(),
                response_tx
            })
            .expect("couldn't send on channel... it is likely that the main worker thread has crashed...");

        let response = response_rx.await.unwrap();

        //info!("Got response: {:?}", response);
        assert!(response.id == id);

        response
            .result
            .map_err(|err| anyhow!(err))
    }

    pub async fn load_module(&mut self, path: String) -> Result<String, AnyError> {
        let id = uuid::Uuid::new_v4().to_string();
        let (response_tx, response_rx) = oneshot::channel();
        self.tx_module_load
            .send(JsCoreRequest {
                script: path,
                id: id.clone(),
                response_tx
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

#[derive(Debug)]
struct JsCoreRequest {
    script: String,
    id: String,
    response_tx: oneshot::Sender<JsCoreResponse>
}

#[derive(Debug, Clone)]
struct JsCoreResponse {
    result: Result<String, String>,
    id: String,
}

#[derive(Clone)]
pub struct JsCore {
    worker: Arc<TokioMutex<MainWorker>>,
}

pub struct ExternWrapper(ExternIO);

impl std::fmt::Display for ExternWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //Write the bytes to string like: [0, 1, 3]
        let bytes = self.0.as_bytes();
        let mut bytes_str = String::from("[");
        for (i, byte) in bytes.iter().enumerate() {
            bytes_str.push_str(&format!("{}", byte));
            if i < bytes.len() - 1 {
                bytes_str.push_str(", ");
            }
        }
        bytes_str.push_str("]");
        write!(f, "{}", bytes_str).unwrap();
        Ok(())
    }
}

impl JsCore {
    pub fn new() -> Self {
        JsCore {
            worker: Arc::new(TokioMutex::new(MainWorker::from_options(
                main_module_url(),
                PermissionsContainer::allow_all(),
                main_worker_options(),
            ))),
        }
    }

    async fn load_module(&self, file_path: String) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().await;
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
            .await;
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

    async fn init_core(&self, config: Ad4mConfig) -> Result<GlobalVariableFuture, AnyError> {
        let mut worker = self
            .worker
            .lock()
            .await;
        let _init_core =
            worker.execute_script("js_core", format!("initCore({})", config.get_json()).into())?;
        Ok(GlobalVariableFuture::new(
            self.worker.clone(),
            "core".to_string(),
        ))
    }

    async fn execute_async(
        &self,
        name: String,
        script: String,
    ) -> Result<Global<deno_core::v8::Value>, AnyError> {
        let mut worker = self
            .worker
            .lock()
            .await;
        let wrapped_script = format!(
            r#"
        globalThis.{} = undefined;
        (async () => {{
            console.log("starting execution of script");
            globalThis.{} = ({});
            console.log("finished execution of script");
        }})();
        "#,
            name, name, script
        );
        let value = worker.execute_script("js_core", wrapped_script.into())?;
        Ok(value)
    }

    fn generate_execution_slot(
        rx: Arc<TokioMutex<UnboundedReceiver<JsCoreRequest>>>,
        tx: Sender<JsCoreResponse>,
        js_core: JsCore,
    ) -> impl Future {
        async move {
            loop {
                let mut maybe_request = rx.lock().await;
                //let maybe_request = rx.lock().as_mut().ok().map(|c| c.try_recv());
                if let Ok(request) = maybe_request.try_recv()  {
                    //info!("Got request: {:?}", request);
                    let script = request.script.clone();
                    let id = request.id.clone();
                    let js_core_cloned = js_core.clone();
                    let response_tx = request.response_tx;
                    let tx_cloned = tx.clone();

                    //global_req_id = Some(id.clone());

                    tokio::task::spawn_local(async move {
                        info!("Spawn local driving: {}", id);
                        let local_variable_name = uuid_to_valid_variable_name(&id);
                        let script_fut =
                            js_core_cloned.execute_async(local_variable_name.clone(), script).await;
                        info!("Script fut created: {}", id);
                        match script_fut {
                            Ok(value) => {
                                let mut worker = js_core_cloned.worker.lock().await;

                                let resolved_value = loop {
                                    match worker.js_runtime.resolve_value(value.clone()).await {
                                        Ok(value) => {
                                            let new_value = worker.execute_script("global_var_future", local_variable_name.clone().into()).unwrap();
                                                let scope = &mut deno_core::v8::HandleScope::new(worker.js_runtime.v8_isolate());
                                                if !new_value.open(scope).is_promise() {
                                                let context = deno_core::v8::Context::new(scope);
                                                let scope = &mut deno_core::v8::ContextScope::new(scope, context);
                                                let value = deno_core::v8::Local::new(scope, new_value.clone());
                                                let value = value.to_rust_string_lossy(scope);
                                                break value;
                                            } else {
                                                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                                                continue;
                                            }
                                        },
                                        Err(err) => {
                                            error!("Error resolving value: {:?}", err);
                                            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                                        }
                                    }
                                    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                                };
                                info!("Resolved value");
                                response_tx.send(JsCoreResponse {
                                    result: Ok(resolved_value),
                                    id: id,
                                }).expect("couldn't send on channel");
                            },
                            Err(err) => {
                                error!("Error executing script: {:?}", err);
                                response_tx
                                    .send(JsCoreResponse {
                                        result: Err(err.to_string()),
                                        id: id,
                                    })
                                    .expect("couldn't send on channel");
                            }
                        }
                        // match script_fut {
                        //     Ok(script_fut) => match script_fut.await {
                        //         Ok(res) => {
                        //             info!("Script execution completed Succesfully: {}", id);
                        //             response_tx
                        //                 .send(JsCoreResponse {
                        //                     result: Ok(res),
                        //                     id: id,
                        //                 })
                        //                 .expect("couldn't send on channel");
                        //         }
                        //         Err(err) => {
                        //             error!("Error executing script: {:?}", err);
                        //             response_tx
                        //                 .send(JsCoreResponse {
                        //                     result: Err(err.to_string()),
                        //                     id: id,
                        //                 })
                        //                 .expect("couldn't send on channel");
                        //         }
                        //     },
                        //     Err(err) => {
                        //         error!("Error executing script: {:?}", err);
                        //         response_tx
                        //             .send(JsCoreResponse {
                        //                 result: Err(err.to_string()),
                        //                 id: id,
                        //             })
                        //             .expect("couldn't send on channel");
                        //     }
                        // }
                    });
                }
                tokio::task::yield_now().await;
                //tokio::time::sleep(std::time::Duration::from_millis(10)).await
            }
            info!("generate_execution_slot loop completed");
        }
    }

    pub async fn start(config: Ad4mConfig) -> JsCoreHandle {
        let (tx_inside, rx_outside) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside, mut rx_inside) = mpsc::unbounded_channel::<JsCoreRequest>();
        let rx_inside = Arc::new(TokioMutex::new(rx_inside));

        let (tx_inside_loader, rx_outside_loader) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside_loader, mut rx_inside_loader) = mpsc::unbounded_channel::<JsCoreRequest>();

        let tx_inside_clone = tx_inside.clone();
        let tx_inside_loader_clone = tx_inside_loader.clone();
        std::thread::spawn(move || {
            let rt = Builder::new_multi_thread()
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
                    .await
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
                    info!("Main loop running");
                    //Listener future for loading JS modules into runtime
                    let module_load_fut = async {
                        loop {
                            if let Ok(request) = rx_inside_loader.try_recv() {
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
                            tokio::task::yield_now().await;
                            //tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                        }
                    };

                    let mut global_req_id = None;

                    let local_set = tokio::task::LocalSet::new();

                    //Listener future for receiving script execution calls
                    //let receive_fut_1 = Self::generate_execution_slot(rx_inside.clone(), tx_inside.clone(), js_core.clone());

                    let holochain_signal_receiver_fut = async {
                        loop {
                            if let Some(holochain_service) = maybe_get_holochain_service() {
                                let stream_receiver = holochain_service.stream_receiver.try_lock();
                                if let Ok(mut stream_receiver) = stream_receiver {
                                    if let Ok(signal) = stream_receiver.try_recv() {
                                        match signal.clone() {
                                            Signal::App {
                                                cell_id,
                                                zome_name,
                                                signal: payload,
                                            } => {
                                                // Handle the received signal here
                                                let script = format!(
                                                    "await core.holochainService.handleCallback({{cell_id: [{:?}, {:?}], zome_name: '{}', signal: {}}})",
                                                    cell_id.dna_hash().get_raw_39().to_vec(), cell_id.agent_pubkey().get_raw_39().to_vec(), zome_name, ExternWrapper(payload.into_inner())
                                                );
                                                match js_core.execute_async("hc_signal_fut".to_string(), script).await {
                                                    Ok(_res) => {
                                                        info!(
                                                            "Holochain Handle Callback Completed Succesfully",
                                                        );
                                                    }
                                                    Err(err) => {
                                                        error!("Error executing callback: {:?}", err);
                                                    }
                                                }
                                            }
                                            Signal::System(_) => {
                                                // Handle the received signal here
                                                info!("Received system signal");
                                            }
                                        }
                                    }
                                }
                            }
                            tokio::task::yield_now().await;
                            //tokio::time::sleep(std::time::Duration::from_millis(10)).await
                        }
                    };

                    tokio::select! {
                        biased;

                        event_loop_result = js_core.event_loop() => {
                            match event_loop_result {
                                Ok(_) => info!("AD4M event loop finished"),
                                Err(err) => {
                                    let tx_cloned = tx_inside.clone();
                                    error!("AD4M event loop closed with error: {}", err);
                                    if global_req_id.is_some() {
                                        //TODO: this error should also cause the graphql server to error since right now we are just killing
                                        //the event loop completely and this should be reflected in the main thread
                                        tx_cloned
                                            .send(JsCoreResponse {
                                                result: Err(err.to_string()),
                                                id: global_req_id.unwrap(),
                                            })
                                            .expect("couldn't send on channel");
                                    }
                                    break;
                                }
                            }
                        }
                        _drive_local_set = local_set.run_until(Self::generate_execution_slot(rx_inside.clone(), tx_inside.clone(), js_core.clone())) => {
                            info!("AD4M drive local set completed");
                        }
                        _module_load = module_load_fut => {
                            info!("AD4M module load completed");
                            //break;
                        }
                        _holochain_signal_receivers = holochain_signal_receiver_fut => {
                            info!("AD4M holochain signal receiver completed");
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
        let mut global_handle = JS_CORE_HANDLE.lock().await;
        *global_handle = Some(handle.clone());

        handle
    }
}
