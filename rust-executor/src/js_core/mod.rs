use ::futures::Future;
use deno_core::anyhow::anyhow;
use deno_core::error::{AnyError, CoreError};
use deno_core::{resolve_url_or_path, v8, PollEventLoopOptions};
use deno_fs::RealFs;
use deno_resolver::npm::DenoInNpmPackageChecker;
use deno_resolver::npm::NpmResolver;
use deno_runtime::deno_permissions::{Permissions, PermissionsContainer, PermissionsOptions};
use deno_runtime::permissions::RuntimePermissionDescriptorParser;
use deno_runtime::worker::{MainWorker, WorkerOptions, WorkerServiceOptions};
use holochain::prelude::ExternIO;
use log::info;
use std::collections::HashSet;
use std::env::current_dir;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use tokio::sync::Mutex as TokioMutex;
use url::Url;

pub mod agent_extension;
pub mod error;
mod futures;
pub mod languages_extension;
mod options;
pub mod pubsub_extension;
pub mod signature_extension;
mod string_module_loader;
pub mod utils;
pub mod utils_extension;
pub mod wallet_extension;

use self::futures::{EventLoopFuture, SmartGlobalVariableFuture};

#[derive(Clone)]
pub struct JsCore {
    worker: Arc<TokioMutex<MainWorker>>,
    loaded_modules: Arc<TokioMutex<HashSet<String>>>,
}

pub struct ExternWrapper(pub ExternIO);

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

impl JsCore {
    /// Create a new language-specific JsCore instance with sandboxed permissions.
    /// Filesystem access is scoped to the language's storage directory only.
    /// System/bootstrap languages additionally get access to the executor's CWD
    /// (they are bundled by the developer and trusted).
    /// Network access is allowed (languages need HTTP for peers/DHT).
    /// All other capabilities (env, run, ffi, sys) are denied.
    pub fn new_for_language(storage_directory: PathBuf, is_system_language: bool) -> Self {
        let permission_desc_parser = Arc::new(RuntimePermissionDescriptorParser::new(
            sys_traits::impls::RealSys,
        ));

        let storage_dir_str = storage_directory.to_string_lossy().to_string();
        info!(
            "Creating {} language runtime with storage: {}",
            if is_system_language {
                "system"
            } else {
                "sandboxed"
            },
            storage_dir_str
        );

        // Build the filesystem allow list
        let mut allowed_paths = vec![storage_dir_str.clone()];
        if is_system_language {
            // System/bootstrap languages get CWD access (for shared test storage, etc.)
            if let Ok(cwd) = std::env::current_dir() {
                let cwd_str = cwd.to_string_lossy().to_string();
                info!("System language also gets CWD access: {}", cwd_str);
                allowed_paths.push(cwd_str);
            }
        }

        let permissions_opts = PermissionsOptions {
            allow_all: false,
            // Filesystem: scoped to storage dir (+ CWD for system languages)
            allow_read: Some(allowed_paths.clone()),
            deny_read: None,
            allow_write: Some(allowed_paths),
            deny_write: None,
            // Network: allowed (languages make HTTP requests to peers, DHT nodes, etc.)
            allow_net: Some(vec![]),
            deny_net: None,
            // Module imports: only from our synthetic domain (for the bundle capture script)
            allow_import: Some(vec!["ad4m.language".to_string()]),
            // Everything else: denied
            allow_env: None,
            deny_env: None,
            allow_sys: None,
            deny_sys: None,
            allow_run: None,
            deny_run: None,
            allow_ffi: None,
            deny_ffi: None,
            prompt: false,
        };

        let permissions = Permissions::from_options(&*permission_desc_parser, &permissions_opts)
            .expect("Failed to create sandboxed permissions for language runtime");
        let container = PermissionsContainer::new(permission_desc_parser, permissions);

        Self::new_with_options(
            options::language_main_module_url(),
            options::language_module_loader(),
            options::language_worker_options(),
            container,
        )
    }

    fn new_with_options(
        module_url: Url,
        module_loader: Rc<string_module_loader::StringModuleLoader>,
        worker_options: WorkerOptions,
        permissions: PermissionsContainer,
    ) -> Self {
        let fs = Arc::new(RealFs);

        let worker = MainWorker::bootstrap_from_options(
            &module_url,
            WorkerServiceOptions::<
                DenoInNpmPackageChecker,
                NpmResolver<sys_traits::impls::RealSys>,
                sys_traits::impls::RealSys,
            > {
                deno_rt_native_addon_loader: None,
                module_loader,
                permissions,
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

    /// Load an ES module from source code directly (no file I/O).
    /// The module is registered under a synthetic URL so it can be evaluated.
    pub async fn load_module_from_source(
        &self,
        specifier: &str,
        source: String,
    ) -> Result<(), AnyError> {
        let mut worker = self.worker.lock().await;
        let mut loaded_modules = self.loaded_modules.lock().await;
        if loaded_modules.contains(specifier) {
            return Ok(());
        }

        let url = Url::parse(specifier)
            .map_err(|e| anyhow!("Invalid module specifier '{}': {}", specifier, e))?;
        let module_id = worker
            .js_runtime
            .load_side_es_module_from_code(&url, source)
            .await?;
        loaded_modules.insert(specifier.to_string());
        let evaluate_fut = worker.js_runtime.mod_evaluate(module_id);
        worker
            .js_runtime
            .with_event_loop_future(evaluate_fut, PollEventLoopOptions::default())
            .await?;
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
}
