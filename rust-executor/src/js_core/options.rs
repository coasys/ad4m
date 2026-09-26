use deno_runtime::worker::WorkerOptions;
use std::rc::Rc;
use url::Url;

use super::agent_extension::agent_service;
use super::languages_extension::language_service;
use super::pubsub_extension::pubsub_service;
use super::signature_extension::signature_service;
use super::string_module_loader::StringModuleLoader;
use super::utils_extension::utils_service;
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
/// The AD4M service extensions available to the language runtime, in the order the
/// runtime and the snapshot builder both register them. One list keeps the two in step:
/// an extension present at snapshot build time but absent at runtime (or the reverse)
/// misaligns the op registry.
///
/// The wallet extension no longer appears here. It exposed `globalThis.WALLET` — the
/// node's main private key, keystore export and lock — to every language, which no
/// language uses. Languages sign through `signature_service` / `agent_service` instead.
pub fn ad4m_language_extensions() -> Vec<deno_core::Extension> {
    vec![
        utils_service::init(),
        pubsub_service::init(),
        holochain_service::init(),
        signature_service::init(),
        agent_service::init(),
        entanglement_service::init(),
        runtime_service::init(),
        language_service::init(),
    ]
}

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
        extensions: ad4m_language_extensions(),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::ad4m_language_extensions;

    /// The wallet extension exposed the node's main private key and keystore export to
    /// every language. No language uses it, so the runtime must not register it.
    #[test]
    fn the_language_runtime_does_not_expose_the_wallet() {
        let names: Vec<&str> = ad4m_language_extensions()
            .iter()
            .map(|ext| ext.name)
            .collect();
        assert!(
            !names.contains(&"wallet_service"),
            "the language runtime still registers the wallet extension: {names:?}"
        );
    }

    /// The wallet extension's JS installed `globalThis.WALLET`. A language runtime booted from
    /// the embedded snapshot must not have it. This also catches a snapshot generated before
    /// the extension went away.
    #[tokio::test]
    async fn a_language_runtime_has_no_wallet_global() {
        let dir = tempfile::tempdir().unwrap();
        let runtime = crate::languages::language_runtime::LanguageRuntime::new(
            "wallet-check".to_string(),
            dir.path().to_path_buf(),
            false,
        );
        runtime.init().await.unwrap();
        let wallet = runtime.execute("typeof globalThis.WALLET").await.unwrap();
        assert!(wallet.contains("undefined"), "WALLET is {wallet}");
        let signature = runtime
            .execute("typeof globalThis.SIGNATURE")
            .await
            .unwrap();
        assert!(signature.contains("object"), "SIGNATURE is {signature}");
    }

    /// The legitimate signing path stays: languages sign through signature_service and
    /// act as their agent through agent_service.
    #[test]
    fn the_language_runtime_keeps_the_signing_extensions() {
        let names: Vec<&str> = ad4m_language_extensions()
            .iter()
            .map(|ext| ext.name)
            .collect();
        assert!(names.contains(&"signature_service"), "{names:?}");
        assert!(names.contains(&"agent_service"), "{names:?}");
    }
}
