use deno_runtime::ops::bootstrap::SnapshotOptions;
use deno_runtime::snapshot::create_runtime_snapshot;
use deno_runtime::transpile::maybe_transpile_source;
use rust_executor::entanglement_service::entanglement_service_extension::entanglement_service;
use rust_executor::holochain_service::holochain_service_extension::holochain_service;
use rust_executor::js_core::agent_extension::agent_service;
use rust_executor::js_core::languages_extension::language_service;
use rust_executor::js_core::pubsub_extension::pubsub_service;
use rust_executor::js_core::signature_extension::signature_service;
use rust_executor::js_core::utils_extension::utils_service;
use rust_executor::js_core::wallet_extension::wallet_service;
use rust_executor::runtime_service::runtime_service_extension::runtime_service;
use std::collections::HashSet;
use std::path::Path;

fn main() {
    // Snapshot binary path (kept next to Cargo.toml for the existing symlink
    // wiring `rust-executor/CUSTOM_DENO_SNAPSHOT.bin -> ../CUSTOM_DENO_SNAPSHOT.bin`).
    let snapshot_path = Path::new("CUSTOM_DENO_SNAPSHOT.bin").to_path_buf();
    // Companion residual-lazy tables generated alongside the .bin. Consumed by
    // `rust-executor/src/js_core/residual_lazy.rs` via `include!(...)`.
    let residual_esm_path = Path::new("CUSTOM_DENO_SNAPSHOT.residual_esm.rs").to_path_buf();
    let residual_js_path = Path::new("CUSTOM_DENO_SNAPSHOT.residual_js.rs").to_path_buf();

    // AD4M's own extensions. `create_runtime_snapshot` prepends deno_runtime's
    // standard extension list (including `deno_node::lazy_init`) before ours,
    // so we must NOT list deno_node here — that would double-register and
    // panic at snapshot build time.
    let extensions = vec![
        wallet_service::init(),
        utils_service::init(),
        pubsub_service::init(),
        holochain_service::init(),
        signature_service::init(),
        agent_service::init(),
        entanglement_service::init(),
        runtime_service::init(),
        language_service::init(),
    ];

    let output = create_runtime_snapshot(snapshot_path, SnapshotOptions::default(), extensions);
    println!("Snapshot generated successfully!");

    // --- Residual lazy-loaded sources ---------------------------------------
    //
    // deno 2.9 does not embed unconsumed `lazy_loaded_esm` / `lazy_loaded_js`
    // sources into the V8 snapshot blob (see coasys/deno PR #36262). At
    // runtime, `add_residual_lazy_loaded_sources` populates the module map
    // from `WorkerOptions.residual_lazy_{esm,js}_sources`; if empty,
    // `take_lazy_esm_source("node:buffer")` returns None and the loader
    // fallback fails with `Module not found: node:buffer`.
    //
    // Every `lazy_loaded_*` file declared by any snapshotted extension that
    // was NOT consumed at snapshot build time needs to be shipped as a
    // residual so the runtime can evaluate it on first import.
    let consumed: HashSet<&str> = output
        .consumed_lazy_specifiers
        .iter()
        .map(|s| s.as_str())
        .collect();

    let mut esm_entries: Vec<(String, String)> = Vec::new();
    let mut js_entries: Vec<(String, String)> = Vec::new();
    for entry in &output.lazy_extension_files {
        if consumed.contains(entry.specifier.as_str()) {
            continue;
        }
        let raw_source = std::fs::read_to_string(&entry.path).unwrap_or_else(|e| {
            panic!(
                "failed to read residual lazy source {:?}: {}",
                entry.path, e
            )
        });
        // Run every residual through deno's own extension transpiler, exactly
        // as `create_runtime_snapshot` does for eager/consumed extension
        // sources. Roughly half of deno_node's residual polyfills are `.ts`
        // files with TypeScript type annotations that V8 cannot parse as
        // script (`SyntaxError: Missing initializer in const declaration`
        // etc.). Without transpiling, `loadExtScript("ext:deno_node/...")`
        // blows up on first `node:buffer` import.
        let transpiled = maybe_transpile_source(entry.specifier.clone().into(), raw_source.into())
            .unwrap_or_else(|e| {
                panic!(
                    "failed to transpile residual lazy source {} ({:?}): {}",
                    entry.specifier, entry.path, e
                )
            });
        let source = transpiled.0.to_string();
        match entry.kind {
            deno_runtime::snapshot::LazyExtensionFileKind::Esm => {
                esm_entries.push((entry.specifier.clone(), source))
            }
            deno_runtime::snapshot::LazyExtensionFileKind::Js => {
                js_entries.push((entry.specifier.clone(), source))
            }
        }
    }

    write_residual_table(
        &residual_esm_path,
        "RESIDUAL_LAZY_ESM_SOURCES",
        &esm_entries,
    );
    write_residual_table(&residual_js_path, "RESIDUAL_LAZY_JS_SOURCES", &js_entries);
    println!(
        "Residual lazy sources: {} ESM, {} JS (out of {} declared)",
        esm_entries.len(),
        js_entries.len(),
        output.lazy_extension_files.len()
    );
}

fn write_residual_table(path: &Path, name: &str, entries: &[(String, String)]) {
    let mut out = String::new();
    out.push_str("// AUTO-GENERATED by rust-executor/src/bin/generate_snapshot.rs.\n");
    out.push_str("// Do not edit by hand. Regenerate with `cargo run --features generate_snapshot --bin generate_snapshot`.\n");
    out.push_str(&format!("pub static {}: &[(&str, &str)] = &[\n", name));
    for (spec, src) in entries {
        // Escape via Rust raw string with a delimiter unlikely to appear in
        // JS polyfill source. Fall back to a debug-escaped string literal if
        // the delimiter collides (defensive; deno polyfills don't use `r###`).
        let raw_delim = pick_raw_delimiter(src);
        out.push_str(&format!(
            "    ({:?}, r{delim}\"{src}\"{delim}),\n",
            spec,
            delim = "#".repeat(raw_delim),
            src = src
        ));
    }
    out.push_str("];\n");
    std::fs::write(path, out)
        .unwrap_or_else(|e| panic!("failed to write residual table {:?}: {}", path, e));
}

/// Pick the smallest number of `#` characters that makes a Rust raw-string
/// terminator unambiguous for the given source (i.e. `"##..."` never appears
/// inside `source`).
fn pick_raw_delimiter(source: &str) -> usize {
    let mut n = 1usize;
    loop {
        let needle = format!("\"{}", "#".repeat(n));
        if !source.contains(&needle) {
            return n;
        }
        n += 1;
        if n > 16 {
            panic!("could not find a safe raw-string delimiter for residual source");
        }
    }
}
