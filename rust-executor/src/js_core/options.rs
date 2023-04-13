use deno_runtime::worker::WorkerOptions;
use std::{collections::HashMap, rc::Rc};
use url::Url;

use super::{string_module_loader::StringModuleLoader, utils_extension, wallet_extension};

pub fn main_module_url() -> Url {
    Url::parse("https://ad4m.runtime/main").unwrap()
}

pub fn module_map() -> HashMap<String, String> {
    let mut map = HashMap::new();
    map.insert(
        "https://ad4m.runtime/main".to_string(),
        include_str!("main.js").to_string(),
    );
    map.insert(
        "https://ad4m.runtime/executor".to_string(),
        include_str!("../../../executor/lib/bundle.js").to_string(),
    );
    map.insert(
        "https://ad4m.runtime/test".to_string(),
        include_str!("testlib.js").to_string(),
    );
    map
}

pub fn main_worker_options() -> WorkerOptions {
    let mut loader = StringModuleLoader::new();
    for (specifier, code) in module_map() {
        loader.add_module(specifier.as_str(), code.as_str());
    }

    let wallet_ext = wallet_extension::build();
    let utils_ext = utils_extension::build();

    WorkerOptions {
        extensions: vec![wallet_ext, utils_ext],
        module_loader: Rc::new(loader),
        ..Default::default()
    }
}
