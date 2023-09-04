use ::futures::Future;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use deno_core::resolve_url_or_path;
use deno_runtime::worker::MainWorker;
use deno_runtime::{permissions::PermissionsContainer, BootstrapOptions};
use holochain::prelude::{ExternIO, Signal};
use once_cell::sync::Lazy;
use tokio::time::sleep;
use std::env::current_dir;
use std::sync::Arc;
use tokio::runtime::Builder;
use tokio::sync::broadcast;
use tokio::sync::Mutex as TokioMutex;
use tokio::sync::{
    broadcast::{Receiver, Sender},
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    oneshot
};
use tracing::{error, info};
use options::{main_module_url, main_worker_options};

mod futures;
mod jwt_extension;
mod options;
mod pubsub_extension;
mod string_module_loader;
mod utils_extension;
mod wallet_extension;

use self::futures::{EventLoopFuture, SmartGlobalVariableFuture};
use crate::holochain_service::maybe_get_holochain_service_async;
use crate::Ad4mConfig;

static JS_CORE_HANDLE: Lazy<Arc<TokioMutex<Option<JsCoreHandle>>>> =
    Lazy::new(|| Arc::new(TokioMutex::new(None)));

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

        info!("Got response: {:?}", response);

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

    // async fn init_core(&self, config: Ad4mConfig) -> Result<GlobalVariableFuture, AnyError> {
    //     let mut worker = self
    //         .worker
    //         .lock()
    //         .await;
    //     let _init_core =
    //         worker.execute_script("js_core", format!("initCore({})", config.get_json()).into())?;
    //     Ok(GlobalVariableFuture::new(
    //         self.worker.clone(),
    //         "core".to_string(),
    //     ))
    // }

    async fn execute_async_smart(
        &self,
        script: String
    ) -> Result<SmartGlobalVariableFuture, AnyError> {
        let mut worker = self.worker.lock().await;
        let wrapped_script = format!(
            r#"
            (async () => {{
                return ({});
            }})();
            "#, script
        );
        info!("Sending script: {}", wrapped_script);
        let execute_async = worker.execute_script("js_core", wrapped_script.into())?;
        Ok(SmartGlobalVariableFuture::new(self.worker.clone(), execute_async))
    }

    fn generate_execution_slot(
        rx: Arc<TokioMutex<UnboundedReceiver<JsCoreRequest>>>,
        tx: Sender<JsCoreResponse>,
        js_core: JsCore,
    ) -> impl Future {
        async move {
            loop {
                //info!("Execution slot loop running");
                let mut maybe_request = rx.lock().await;
                //let maybe_request = rx.lock().as_mut().ok().map(|c| c.try_recv());
                if let Some(request) = maybe_request.recv().await  {
                    //info!("Got request: {:?}", request);
                    let script = request.script.clone();
                    let id = request.id.clone();
                    let js_core_cloned = js_core.clone();
                    let response_tx = request.response_tx;
                    let tx_cloned = tx.clone();

                    //global_req_id = Some(id.clone());

                    tokio::task::spawn_local(async move {
                        info!("Spawn local driving: {}", id);
                        //let local_variable_name = uuid_to_valid_variable_name(&id);
                        let script_fut =
                            js_core_cloned.execute_async_smart(script).await.unwrap();
                        info!("Script fut created: {}", id);
                        match script_fut.await {
                            Ok(res) => {
                                info!("Script execution completed Succesfully: {}", id);
                                response_tx
                                    .send(JsCoreResponse {
                                        result: Ok(res),
                                        id: id,
                                    })
                                    .expect("couldn't send on channel");
                            }
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
                    });
                }
                //sleep(std::time::Duration::from_millis(10)).await;
                tokio::task::yield_now().await;
            }
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

            rt.block_on(async {
                let result = js_core.init_engine().await;
                info!("AD4M JS engine init completed, with result: {:?}", result);

                let init_core_future = js_core
                    .execute_async_smart(format!("initCore({})", config.get_json()).into());
                let result = init_core_future.await.unwrap().await;

                match result {
                    Ok(res) => {
                        info!("AD4M coreInit() completed Succesfully: {}", res);
                        tx_inside
                            .send(JsCoreResponse {
                                result: Ok(String::from("initialized")),
                                id: String::from("initialized"),
                            })
                            .expect("couldn't send on channel");
                    }
                    Err(err) => {
                        error!("Error executing coreInit(): {:?}", err);
                        tx_inside
                            .send(JsCoreResponse {
                                result: Err(format!("Error executing coreInit(): {:?}", err)),
                                id: String::from("initialized"),
                            })
                            .expect("couldn't send on channel");
                    }
                }

                loop {
                    info!("Main loop running");
                    //Listener future for loading JS modules into runtime
                    let module_load_fut = async {
                        loop {
                            //info!("Module load loop running");
                            if let Some(request) = rx_inside_loader.recv().await {
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
                            //sleep(std::time::Duration::from_millis(10)).await;
                            tokio::task::yield_now().await;
                        }
                    };

                    let mut global_req_id = None;

                    let local_set = tokio::task::LocalSet::new();

                    let holochain_signal_receiver_fut = async {
                        loop {
                            //info!("Holochain service loop");
                            if let Some(holochain_service) = maybe_get_holochain_service_async().await {
                                let mut stream_receiver = holochain_service.stream_receiver.lock().await;
                                if let Some(signal) = stream_receiver.recv().await {
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
                                            match js_core.execute_async_smart(script).await {
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
                            //sleep(std::time::Duration::from_millis(10)).await;
                            tokio::task::yield_now().await;
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
