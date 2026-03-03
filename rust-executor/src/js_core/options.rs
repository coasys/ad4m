use deno_runtime::worker::WorkerOptions;
use std::{collections::HashMap, rc::Rc};
use url::Url;

use super::agent_extension::agent_service;
use super::languages_extension::language_service;
use super::pubsub_extension::pubsub_service;
use super::signature_extension::signature_service;
use super::string_module_loader::StringModuleLoader;
use super::utils_extension::utils_service;
use super::wallet_extension::wallet_service;
use crate::entanglement_service::entanglement_service_extension::entanglement_service;
use crate::holochain_service::holochain_service_extension::holochain_service;
use crate::runtime_service::runtime_service_extension::runtime_service;

pub fn main_module_url() -> Url {
    Url::parse("https://ad4m.runtime/main").unwrap()
}
#[cfg(not(target_os = "windows"))]
pub fn module_map() -> HashMap<String, String> {
    let mut map = HashMap::new();
    map.insert(
        "https://ad4m.runtime/main".to_string(),
        include_str!("main.js").to_string(),
    );

    map.insert(
        "https://ad4m.runtime/executor".to_string(),
        include_str!("../../executor/lib/bundle.js").to_string(),
    );
    map
}

#[cfg(target_os = "windows")]
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
    map
}

pub fn module_loader() -> Rc<StringModuleLoader> {
    let mut loader = StringModuleLoader::new();
    for (specifier, code) in module_map() {
        loader.add_module(specifier.as_str(), code.as_str());
    }
    Rc::new(loader)
}

pub fn main_worker_options() -> WorkerOptions {
    WorkerOptions {
        startup_snapshot: {
            #[cfg(feature = "generate_snapshot")]
            {
                None
            }
            #[cfg(not(feature = "generate_snapshot"))]
            {
                Some(include_bytes!("../../CUSTOM_DENO_SNAPSHOT.bin"))
            }
        },
        extensions: vec![
            wallet_service::init(),
            utils_service::init(),
            pubsub_service::init(),
            holochain_service::init(),
            signature_service::init(),
            agent_service::init(),
            entanglement_service::init(),
            runtime_service::init(),
            language_service::init(),
        ],
        ..Default::default()
    }
}
