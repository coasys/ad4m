use deno_runtime::worker::WorkerOptions;
use std::rc::Rc;
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

/// Create a minimal module loader for language runtimes.
/// Maps the bootstrap URL so MainWorker::bootstrap_from_options() can resolve it.
/// Languages load their bundles from file paths afterwards.
pub fn language_module_loader() -> Rc<StringModuleLoader> {
    let mut loader = StringModuleLoader::new();
    loader.add_module(
        "https://ad4m.language/bootstrap",
        include_str!("language_bootstrap.js"),
    );
    // `ad4m:host` is the canonical import specifier for host imports.
    // Both JS and Rust/WASM language bundles emit
    //   `import { agentDid, holochainCall, ... } from "ad4m:host"`
    // at the top of their output (JS via esbuild `external`, Rust via
    // wasm-bindgen `#[wasm_bindgen(module = "ad4m:host")]`).
    //
    // The file is plain JavaScript (no TypeScript) so no transpilation
    // is needed and any runtime can load it without a TS compiler.
    loader.add_module("ad4m:host", include_str!("host.js"));
    Rc::new(loader)
}

/// Get a minimal main module URL for language runtimes
pub fn language_main_module_url() -> Url {
    Url::parse("https://ad4m.language/bootstrap").unwrap()
}

/// Create worker options for language-specific runtimes.
/// These runtimes have the same Rust service extensions but minimal JS bootstrap.
pub fn language_worker_options() -> WorkerOptions {
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
        // deno 2.9: `lazy_loaded_esm` / `lazy_loaded_js` sources that were
        // NOT consumed at snapshot build time (e.g. `node:buffer` and its
        // deno_node polyfill graph) are NOT embedded in the V8 snapshot.
        // At runtime, `add_residual_lazy_loaded_sources` populates the
        // module map from these slices; if they're empty,
        // `take_lazy_esm_source("node:buffer")` returns None and the
        // fallback path hits our `StringModuleLoader`, which correctly
        // returns NotFound (it doesn't own `node:*`). Regenerate via
        // `cargo run --features generate_snapshot --bin generate_snapshot`.
        #[cfg(not(feature = "generate_snapshot"))]
        residual_lazy_esm_sources: super::residual_lazy::RESIDUAL_LAZY_ESM_SOURCES,
        #[cfg(not(feature = "generate_snapshot"))]
        residual_lazy_js_sources: super::residual_lazy::RESIDUAL_LAZY_JS_SOURCES,
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
