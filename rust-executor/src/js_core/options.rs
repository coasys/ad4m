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
    // language_bootstrap.js does `import ... from "./wasm_imports.ts"`,
    // which deno_core resolves relative to the bootstrap URL above. Without
    // this registration the lookup falls through to `to_file_path()` (fails
    // for https scheme) and then to the modules map (absent) → NotFound,
    // which would abort loading the bootstrap module and break every
    // language before init() ever runs.
    loader.add_module(
        "https://ad4m.language/wasm_imports.ts",
        include_str!("wasm_imports.ts"),
    );
    // `ad4m:host.ts` is the import specifier used by Rust-authored Languages
    // through the ALDK (`#[wasm_bindgen(module = "ad4m:host.ts")]`). The
    // wasm-bindgen glue emits `import { agentDid, holochainCall, ... }
    // from "ad4m:host.ts"` at the top of the generated language bundle —
    // we satisfy that import by registering the same wasm_imports.ts
    // source under the `ad4m:host.ts` specifier.
    //
    // The `.ts` suffix is deliberate: the StringModuleLoader uses the URL
    // path extension to decide whether to transpile TypeScript, and the
    // source we register still contains TS type annotations. Without the
    // suffix, `MediaType::from_path` reports Unknown and Deno loads the
    // file as raw JS, crashing on the first type annotation.
    //
    // The two specifiers (`https://ad4m.language/wasm_imports.ts` and
    // `ad4m:host.ts`) load as two distinct Deno module instances, but
    // both close over globalThis extension ops (AGENT, LANGUAGE_CONTROLLER,
    // __holochainDelegate__) installed by `setupWasmImports()`, so state
    // stays consistent across the boundary.
    loader.add_module("ad4m:host.ts", include_str!("wasm_imports.ts"));
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
