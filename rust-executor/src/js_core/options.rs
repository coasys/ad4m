use std::{collections::HashMap, sync::Arc, rc::Rc};
use deno_runtime::{worker::WorkerOptions, BootstrapOptions, deno_web::BlobStore, deno_broadcast_channel::InMemoryBroadcastChannel};
use url::Url;

use super::string_module_loader::StringModuleLoader;

pub fn main_module_url() -> Url {
    Url::parse("https://ad4m.runtime/main").unwrap()
}

pub fn module_map() -> HashMap<String, String> {
    let mut map = HashMap::new();
    map.insert("https://ad4m.runtime/main".to_string(), include_str!("main.js").to_string());
    map.insert("https://ad4m.runtime/executor".to_string(), include_str!("../../../executor/lib/bundle.js").to_string());
    map.insert("https://ad4m.runtime/test".to_string(), include_str!("testlib.js").to_string());
    map
}

pub fn main_worker_options() -> WorkerOptions {
    let mut loader = StringModuleLoader::new();
    for (specifier, code) in module_map() {
        loader.add_module(specifier.as_str(), code.as_str());
    }
    
    WorkerOptions {
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
    }
}