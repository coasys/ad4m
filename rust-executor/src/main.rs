
use deno_core::ModuleSource;
use deno_core::ResolutionKind;
use deno_core::v8;
use deno_runtime::deno_core::include_js_files;
//use deno_runtime::deno_core::op;
use deno_runtime::deno_core::Extension;
//use deno_core::{JsRuntime, RuntimeOptions}
use deno_runtime::deno_web::BlobStore;
use deno_runtime::deno_broadcast_channel::InMemoryBroadcastChannel;
use url::Url;
use std::rc::Rc;
use std::sync::Arc;
use deno_runtime::deno_core::error::AnyError;
use deno_runtime::worker::MainWorker;
use deno_runtime::permissions::{PermissionsContainer};
use deno_runtime::worker::WorkerOptions;
use deno_runtime::BootstrapOptions;
use deno_core::{anyhow};
use deno_core::ModuleLoader;
use deno_core::ModuleSpecifier;
use std::collections::HashMap;
use std::pin::Pin;
use deno_core::ModuleSourceFuture;

/* 
#[op]
async fn op_read_file(path: String) -> Result<String, AnyError> {
    let contents = tokio::fs::read_to_string(path).await?;
    Ok(contents)
}

#[op]
async fn op_write_file(path: String, contents: String) -> Result<(), AnyError> {
    tokio::fs::write(path, contents).await?;
    Ok(())
}

#[op]
fn op_remove_file(path: String) -> Result<(), AnyError> {
    std::fs::remove_file(path)?;
    Ok(())
}
*/



pub struct StringModuleLoader {
    modules: HashMap<String, String>,
}

impl StringModuleLoader {
    pub fn new() -> Self {
        StringModuleLoader {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, specifier: &str, code: &str) {
        self.modules.insert(specifier.to_string(), code.to_string());
    }
}

impl ModuleLoader for StringModuleLoader {
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _kind: ResolutionKind,
    ) -> Result<ModuleSpecifier, AnyError> {
        let module_specifier = deno_core::resolve_import(specifier, referrer)?;
        Ok(module_specifier)
    }

    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<ModuleSpecifier>,
        _is_dyn_import: bool,
    ) -> Pin<Box<ModuleSourceFuture>> {
        let module_code = self.modules.get(module_specifier.as_str()).cloned();
        let module_specifier = module_specifier.clone();
        let fut = async move {
            match module_code {
                Some(code) => Ok(ModuleSource {
                    code: code.into(),
                    module_type: deno_core::ModuleType::JavaScript,
                    module_url_specified: module_specifier.clone().to_string(),
                    module_url_found: module_specifier.clone().to_string(),
                }),
                None => Err(anyhow::anyhow!("Module not found: {}", module_specifier)),
            }
        };
        Box::pin(fut)
    }
}

async fn run_js() -> Result<(), AnyError> {
    let main_module = Url::parse("https://ad4m.runtime/main").unwrap();
    let executor_module =  Url::parse("https://ad4m.runtime/executor").unwrap();
    let test_module =  Url::parse("https://ad4m.runtime/test").unwrap();

    let mut loader = StringModuleLoader::new();
    loader.add_module(executor_module.as_str(), include_str!("../../executor/lib/bundle.js"));
    loader.add_module(test_module.as_str(), include_str!("./testlib.js"));
    loader.add_module(main_module.as_str(), include_str!("main.js"));
    
    let _runtime_extension = Extension::builder("runtime")
        //.js(include_js_files!(
        //    executor "../../executor/lib/bundle.js",
        //))
        //.esm(include_js_files!(
        //    executor "../../executor/lib/bundle.js",
        //))
        .js(include_js_files!(
            runtime "runtime.js",
        )) 
        .ops(vec![
            //op_read_file::decl(),
            //op_write_file::decl(),
            //op_remove_file::decl(),
        ])
        .build();

    let options = WorkerOptions {
        bootstrap: BootstrapOptions::default(),
        extensions: vec![],
        startup_snapshot: Some(deno_runtime::js::deno_isolate_init()),
        unsafely_ignore_certificate_errors: None,
        root_cert_store: None,
        seed: None,
        format_js_error_fn: None,
        source_map_getter: None,
        web_worker_preload_module_cb: Arc::new(|_| unreachable!()),
        web_worker_pre_execute_module_cb: Arc::new(|_| unreachable!()),
        create_web_worker_cb: Arc::new(|_| unreachable!()),
        maybe_inspector_server: None,
        should_break_on_first_statement: false,
        should_wait_for_inspector_session: false,
        module_loader: Rc::new(loader),
        npm_resolver: None,
        get_error_class_fn: None,
        cache_storage_dir: None,
        origin_storage_dir: None,
        blob_store: BlobStore::default(),
        broadcast_channel: InMemoryBroadcastChannel::default(),
        shared_array_buffer_store: None,
        compiled_wasm_module_store: None,
        stdio: Default::default(),
    };

    let permissions = PermissionsContainer::allow_all();
    let mut worker = MainWorker::from_options(main_module.clone(), permissions, options);
    worker.bootstrap(&BootstrapOptions::default());
    worker.execute_main_module(&main_module).await.unwrap();


    {
        let n = worker.execute_script("test worker", "n")?;
        let init_core = worker.execute_script("test worker", "initCore")?;
        //let (context_scope, scope) = get_module_handle_scope_context(&mut worker, main_id)?;
        let scope = &mut v8::HandleScope::new(worker.js_runtime.v8_isolate());
        let n = v8::Local::new(scope, n);
        let init_core = v8::Local::new(scope, init_core);

        println!("n.is_number: {:?}", n.is_number());
        println!("n: {:?}", n);
        println!("init_core.is_function: {:?}", init_core.is_function());
        println!("init_core: {:?}", init_core);

        let function: v8::Local<v8::Function> = unsafe { v8::Local::cast(init_core) };
        // call that function with arguments
        let recv = v8::Integer::new(scope, 2).into();
        
        let context = v8::Context::new(scope);
        let scope = &mut v8::ContextScope::new(scope, context);
        let init_return = function.call(scope, recv, &[]).unwrap();
        let core_promise = v8::Local::<v8::Promise>::try_from(init_return).unwrap();
        println!("core_promise: {:?}", core_promise);
    }
    
    worker.run_event_loop(false).await?;
    Ok(())
  }

fn main() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let run = run_js();
    //let run = run_js("./example.js");
    if let Err(error) = runtime.block_on(run) {
        eprintln!("error: {}", error);
    }
}
