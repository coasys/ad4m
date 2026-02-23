use ::futures::Future;
use deno_core::anyhow::anyhow;
use deno_core::error::{AnyError, CoreError};
use deno_core::{resolve_url_or_path, v8, PollEventLoopOptions};
use deno_fs::RealFs;
use deno_resolver::npm::DenoInNpmPackageChecker;
use deno_resolver::npm::NpmResolver;
use deno_runtime::deno_permissions::PermissionsContainer;
use deno_runtime::permissions::RuntimePermissionDescriptorParser;
use deno_runtime::worker::{MainWorker, WorkerOptions, WorkerServiceOptions};
use holochain::prelude::{ExternIO, Signal};
use log::{error, info};
use once_cell::sync::Lazy;
use std::collections::HashSet;
use std::env::current_dir;
use std::rc::Rc;
use std::sync::Arc;
use tokio::runtime::Builder;
use tokio::sync::broadcast;
use tokio::sync::Mutex as TokioMutex;
use tokio::sync::{
    broadcast::{Receiver, Sender},
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    oneshot,
};
use url::Url;

pub mod agent_extension;
pub mod error;
mod futures;
pub mod languages_extension;
mod options;
pub mod pubsub_extension;
pub mod signature_extension;
mod string_module_loader;
mod utils;
pub mod utils_extension;
pub mod wallet_extension;

use self::futures::{EventLoopFuture, SmartGlobalVariableFuture};
use crate::holochain_service::maybe_get_holochain_service;
use crate::Ad4mConfig;

pub(crate) static JS_CORE_HANDLE: Lazy<Arc<TokioMutex<Option<JsCoreHandle>>>> =
    Lazy::new(|| Arc::new(TokioMutex::new(None)));

pub struct JsCoreHandle {
    rx: Receiver<JsCoreResponse>,
    tx: UnboundedSender<JsCoreRequest>,
    tx_module_load: UnboundedSender<JsCoreRequest>,
    broadcast_tx: Sender<JsCoreResponse>,
}

impl Clone for JsCoreHandle {
    fn clone(&self) -> Self {
        JsCoreHandle {
            rx: self.broadcast_tx.subscribe(),
            tx: self.tx.clone(),
            tx_module_load: self.tx_module_load.clone(),
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
        let (response_tx, response_rx) = oneshot::channel();

        self.tx
            .send(JsCoreRequest {
                script,
                id: id.clone(),
                response_tx
            })
            .expect("couldn't send on channel... it is likely that the main worker thread has crashed...");

        let response = response_rx.await?;

        // info!("Got response: {:?}", response);

        response.result.map_err(|err| anyhow!(err))
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

        let response = response_rx.await?;

        response.result.map_err(|err| anyhow!(err))
    }
}

#[derive(Debug)]
struct JsCoreRequest {
    script: String,
    #[allow(dead_code)]
    id: String,
    response_tx: oneshot::Sender<JsCoreResponse>,
}

#[derive(Debug, Clone)]
struct JsCoreResponse {
    result: Result<String, String>,
}

#[derive(Clone)]
pub struct JsCore {
    worker: Arc<TokioMutex<MainWorker>>,
    loaded_modules: Arc<TokioMutex<HashSet<String>>>,
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
        bytes_str.push(']');
        write!(f, "{}", bytes_str).unwrap();
        Ok(())
    }
}

impl Default for JsCore {
    fn default() -> Self {
        Self::new()
    }
}

impl JsCore {
    pub fn new() -> Self {
        Self::new_with_options(
            options::main_module_url(),
            options::module_loader(),
            options::main_worker_options(),
        )
    }

    /// Create a new language-specific JsCore instance
    /// This uses a minimal bootstrap and doesn't load main.js or executor
    pub fn new_for_language() -> Self {
        Self::new_with_options(
            options::language_main_module_url(),
            options::language_module_loader(),
            options::language_worker_options(),
        )
    }

    fn new_with_options(
        module_url: Url,
        module_loader: Rc<string_module_loader::StringModuleLoader>,
        worker_options: WorkerOptions,
    ) -> Self {
        deno_core::v8::V8::set_flags_from_string("--no-opt");
        let fs = Arc::new(RealFs);
        let permission_desc_parser = Arc::new(RuntimePermissionDescriptorParser::new(
            sys_traits::impls::RealSys,
        ));

        let worker = MainWorker::bootstrap_from_options(
            &module_url,
            WorkerServiceOptions::<
                DenoInNpmPackageChecker,
                NpmResolver<sys_traits::impls::RealSys>,
                sys_traits::impls::RealSys,
            > {
                deno_rt_native_addon_loader: None,
                module_loader,
                permissions: PermissionsContainer::allow_all(permission_desc_parser),
                blob_store: Default::default(),
                broadcast_channel: Default::default(),
                feature_checker: Default::default(),
                node_services: Default::default(),
                npm_process_state_provider: Default::default(),
                root_cert_store_provider: Default::default(),
                fetch_dns_resolver: Default::default(),
                shared_array_buffer_store: Default::default(),
                compiled_wasm_module_store: Default::default(),
                v8_code_cache: Default::default(),
                fs,
            },
            worker_options,
        );

        JsCore {
            #[allow(clippy::arc_with_non_send_sync)]
            worker: Arc::new(TokioMutex::new(worker)),
            loaded_modules: Arc::new(TokioMutex::new(HashSet::new())),
        }
    }

    pub async fn load_module(&self, file_path: &str) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().await;
        let mut loaded_modules = self.loaded_modules.lock().await;
        let url = resolve_url_or_path(file_path, current_dir()?.as_path())?;
        if loaded_modules.contains(url.clone().as_str()) {
            return Ok(());
        }

        let module_id = worker.js_runtime.load_side_es_module(&url).await?;
        loaded_modules.insert(url.clone().to_string());
        let evaluate_fut = worker.js_runtime.mod_evaluate(module_id);
        worker
            .js_runtime
            .with_event_loop_future(evaluate_fut, PollEventLoopOptions::default())
            .await?;
        Ok(())
    }

    pub async fn init_engine(&self) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().await;
        worker
            .execute_main_module(&options::main_module_url())
            .await
            .map_err(|e| anyhow!("init_engine(): could not execute main module: {}", e))?;
        Ok(())
    }

    /// Initialize a language-specific runtime by executing the minimal bootstrap module.
    /// This makes Deno ops available without loading the full executor/main.js.
    pub async fn init_for_language(&self) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().await;
        worker
            .execute_main_module(&options::language_main_module_url())
            .await
            .map_err(|e| {
                anyhow!(
                    "init_for_language(): could not execute bootstrap module: {}",
                    e
                )
            })?;
        Ok(())
    }

    /// Execute a script synchronously in this JsCore instance
    pub async fn execute(&self, script: &str) -> Result<String, String> {
        let script_fut = self
            .execute_async_smart(script.to_string())
            .await
            .map_err(|e| format!("Failed to create script future: {}", e))?;

        script_fut
            .await
            .map_err(|e| format!("Script execution failed: {}", e))
    }

    pub(crate) fn event_loop(&self) -> EventLoopFuture {
        EventLoopFuture::new(self.worker.clone())
    }

    async fn execute_async_smart(
        &self,
        script: String,
    ) -> Result<
        SmartGlobalVariableFuture<impl Future<Output = Result<v8::Global<v8::Value>, CoreError>>>,
        AnyError,
    > {
        let wrapped_script = format!(
            r#"
            (async () => {{
                return ({});
            }})();
            "#,
            script
        );

        let resolve_fut = {
            let mut worker = self.worker.lock().await;
            let execute_async = worker.execute_script("js_core", wrapped_script.into());
            worker.js_runtime.resolve(execute_async.unwrap())
        };

        Ok(SmartGlobalVariableFuture::new(
            self.worker.clone(),
            resolve_fut,
        ))
    }

    fn generate_execution_slot(
        rx: Arc<TokioMutex<UnboundedReceiver<JsCoreRequest>>>,
        js_core: JsCore,
    ) -> impl Future {
        async move {
            loop {
                //info!("Execution slot loop running");
                let mut maybe_request = rx.lock().await;
                if let Some(request) = maybe_request.recv().await {
                    //info!("Got request: {:?}", request);
                    let script = request.script.clone();
                    let js_core_cloned = js_core.clone();
                    let response_tx = request.response_tx;

                    //global_req_id = Some(id.clone());

                    tokio::task::spawn_local(async move {
                        // info!("Spawn local driving: {}", id);
                        //let local_variable_name = uuid_to_valid_variable_name(&id);
                        let script_fut = js_core_cloned
                            .execute_async_smart(script)
                            .await
                            .expect("Couldn't create execute_async_smart future");
                        //info!("Script fut created: {}", id);
                        match script_fut.await {
                            Ok(res) => {
                                //info!("Script execution completed Succesfully: {}", id);
                                response_tx
                                    .send(JsCoreResponse { result: Ok(res) })
                                    .expect("couldn't send on channel");
                            }
                            Err(err) => {
                                error!("Error executing script: {:?}", err);
                                response_tx
                                    .send(JsCoreResponse {
                                        result: Err(err.to_string()),
                                    })
                                    .expect("couldn't send on channel");
                            }
                        }
                    });
                } else {
                    // No more requests available, add a small delay to prevent busy-waiting
                    tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
                }
            }
        }
    }

    pub async fn start(config: Ad4mConfig) -> JsCoreHandle {
        let (tx_inside, rx_outside) = broadcast::channel::<JsCoreResponse>(50);
        let (tx_outside, rx_inside) = mpsc::unbounded_channel::<JsCoreRequest>();
        let rx_inside = Arc::new(TokioMutex::new(rx_inside));

        let (tx_outside_loader, mut rx_inside_loader) = mpsc::unbounded_channel::<JsCoreRequest>();

        let tx_inside_clone = tx_inside.clone();
        std::thread::spawn(move || {
            let rt = Builder::new_current_thread()
                .thread_name(String::from("js_core"))
                .enable_all()
                .build()
                .expect("Failed to create Tokio runtime");
            let _guard = rt.enter();

            let js_core = JsCore::new();

            rt.block_on(async {
                let result = js_core.init_engine().await;
                info!("AD4M JS engine init completed, with result: {:?}", result);

                let result = js_core
                    .execute_async_smart(format!("initCore({})", config.get_json()))
                    .await
                    .expect("to be able to create js execution future")
                    .await;

                match result {
                    Ok(res) => {
                        info!("AD4M coreInit() completed Succesfully: {:?}", res);
                        tx_inside
                            .send(JsCoreResponse {
                                result: Ok(String::from("initialized")),
                            })
                            .expect("couldn't send on channel");
                    }
                    Err(err) => {
                        error!("Error executing coreInit(): {:?}", err);
                        tx_inside
                            .send(JsCoreResponse {
                                result: Err(format!("Error executing coreInit(): {:?}", err)),
                            })
                            .expect("couldn't send on channel");
                    }
                }

                loop {
                    //info!("Main loop running");
                    //Listener future for loading JS modules into runtime
                    let module_load_fut = async {
                        loop {
                            //info!("Module load loop running");
                            if let Some(request) = rx_inside_loader.recv().await {
                                let script = request.script;
                                let js_core_cloned = js_core.clone();
                                let ts_response = request.response_tx;

                                tokio::task::spawn_local(async move {
                                    match js_core_cloned.load_module(&script).await {
                                        Ok(()) => {
                                            info!("Module loaded!");
                                            ts_response
                                                .send(JsCoreResponse {
                                                    result: Ok(String::from("")),
                                                })
                                                .expect("couldn't send on channel");
                                        }
                                        Err(err) => {
                                            error!("Error loading module: {:?}", err);
                                            ts_response
                                                .send(JsCoreResponse {
                                                    result: Err(err.to_string()),
                                                })
                                                .expect("couldn't send on channel");
                                        }
                                    }
                                });
                            } else {
                                // No more module load requests, add small delay to prevent busy-waiting
                                tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
                            }
                        }
                    };

                    let local_set = tokio::task::LocalSet::new();
                    let holochain_local_set = tokio::task::LocalSet::new();
                    let module_load_local_set = tokio::task::LocalSet::new();

                    let holochain_signal_receiver_fut = async {
                        loop {
                            //info!("Holochain service loop");
                            if let Some(holochain_service) = maybe_get_holochain_service().await {
                                let mut stream_receiver = holochain_service.stream_receiver.lock().await;
                                if let Some(signal) = stream_receiver.recv().await {
                                    match signal.clone() {
                                        Signal::App {
                                            cell_id,
                                            zome_name,
                                            signal: payload,
                                        } => {
                                            // Build cell_id hex key for per-language routing
                                            let dna_hash_raw = cell_id.dna_hash().get_raw_39().to_vec();
                                            let agent_pubkey_raw = cell_id.agent_pubkey().get_raw_39().to_vec();
                                            let cell_id_key = format!(
                                                "{}:{}",
                                                dna_hash_raw.iter().map(|b| format!("{:02x}", b)).collect::<String>(),
                                                agent_pubkey_raw.iter().map(|b| format!("{:02x}", b)).collect::<String>()
                                            );

                                            // Format the payload once (ExternWrapper is not Clone)
                                            let payload_str = format!("{}", ExternWrapper(payload.into_inner()));
                                            let dna_hash_dbg = format!("{:?}", dna_hash_raw);
                                            let agent_pubkey_dbg = format!("{:?}", agent_pubkey_raw);

                                            // Route to per-language runtime if a handler is registered
                                            let maybe_lang_address: Option<String> = {
                                                let handlers = crate::js_core::languages_extension::HOLOCHAIN_SIGNAL_HANDLERS.read().await;
                                                handlers.get(&cell_id_key).cloned()
                                            };
                                            if let Some(lang_address) = maybe_lang_address {
                                                // Route to per-language runtime only
                                                let signal_script = format!(
                                                    "await globalThis.__handleHolochainSignal__({{cell_id: [{}, {}], zome_name: '{}', payload: {}}})",
                                                    dna_hash_dbg, agent_pubkey_dbg, zome_name, payload_str
                                                );
                                                let lang_addr = lang_address.clone();
                                                tokio::spawn(async move {
                                                    let controller = crate::languages::LanguageController::global_instance();
                                                    if let Err(e) = controller.execute_on_language(&lang_addr, &signal_script).await {
                                                        log::warn!("Failed to route Holochain signal to language {}: {}", lang_addr, e);
                                                    }
                                                });
                                            } else {
                                                // No per-language runtime registered; fall back to legacy JS handler
                                                let js_core_cloned = js_core.clone();
                                                tokio::task::spawn_local(async move {
                                                    let script = format!(
                                                        "await core.holochainService.handleCallback({{cell_id: [{}, {}], zome_name: '{}', signal: {}}})",
                                                        dna_hash_dbg, agent_pubkey_dbg, zome_name, payload_str
                                                    );
                                                    match js_core_cloned.execute_async_smart(script).await {
                                                        Ok(_res) => {
                                                        }
                                                        Err(err) => {
                                                            error!("Error executing callback: {:?}", err);
                                                        }
                                                    }
                                                });
                                            }
                                        },
                                        Signal::System(_) => {
                                            // Handle the received signal here
                                            info!("Received system signal");
                                        }
                                    }
                                } else {
                                    // No signal received, add small delay to prevent busy-waiting
                                    tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
                                }
                            } else {
                                // Holochain service not available, add delay to prevent busy-waiting
                                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                            }
                        }
                    };

                    tokio::select! {
                        biased;

                        event_loop_result = js_core.event_loop() => {
                            match event_loop_result {
                                Ok(_) => {} //info!("AD4M event loop finished"),
                                Err(err) => {
                                    error!("AD4M event loop closed with error: {}", err);
                                    break;
                                }
                            }
                        }
                        _drive_local_set = local_set.run_until(Self::generate_execution_slot(rx_inside.clone(), js_core.clone())) => {
                            info!("AD4M drive local set completed");
                        }
                        _module_load = module_load_local_set.run_until(module_load_fut) => {
                            info!("AD4M module load completed");
                            //break;
                        }
                        _holochain_signal_receivers = holochain_local_set.run_until(holochain_signal_receiver_fut) => {
                            info!("AD4M holochain signal receiver completed");
                        }
                    }
                }
            })
        });

        let handle = JsCoreHandle {
            rx: rx_outside,
            tx: tx_outside,
            tx_module_load: tx_outside_loader,
            broadcast_tx: tx_inside_clone,
        };

        //Set the JsCoreHandle to a global object so we can use it inside of deno op calls
        let mut global_handle = JS_CORE_HANDLE.lock().await;
        *global_handle = Some(handle.clone());

        handle
    }
}
